000100 ID DIVISION.                                                             
000301 PROGRAM-ID.    W5127600.                                                 
000400*                                                                         
000501*    AUTHOR.        SARASWATHY.                                           
000601*    DATE-WRITTEN   JUN 2019.                                             
000700*                                                                         
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001010*               CREATES ALL BELOW LIST FOR INTERNAL GIT ONLY.             
001100*               SKAPAR LISTPOSTER KURANSGRUPPERADE EL. NEDAN:             
001200*             - ARTIKLAR MED KDERS > 20             FÅR KDKG = 6          
001300*             - ARTIKLAR MED PUBLICERINGSVECKA YNGRE ÄN                   
001400*               KÖRNINGSDATUM - 2 ÅR                FÅR KDKG = 1          
001500*             - ARTIKLAR MED PB = 0 OCH KDERS = 0   FÅR KDKG = 5          
001600*             - ARTIKLAR UTAN ORDERINGÅNG           FÅR KDKG = 5          
001700*             - ARTIKLAR DÄR ARTIKELREGISTRETS LAGERVÄRDE ÄR <            
001800*                  1 * LAGERVÄRDET PÅ ORDERINGÅNGEN FÅR KDKG = 1          
001900*                  3 * LAGERVÄRDET PÅ ORDERINGÅNGEN FÅR KDKG = 2          
002000*                  5 * LAGERVÄRDET PÅ ORDERINGÅNGEN FÅR KDKG = 3          
002100*                 10 * LAGERVÄRDET PÅ ORDERINGÅNGEN FÅR KDKG = 4          
002200*             - ÖVRIGA                              FÅR KDKG = 5          
002300*                                                                         
002400*               SKAPAR LISTA KURANSGRUPPERING MED SHELFLIFEMETODEN        
002500*               ÅRSBEHOV TAS FRAM M.H.A. ORDERINGÅNG (* STD).             
002600*               TOT. LAGERSALDO HÄMTAS FRÅN LAGERBANDET (* STD).          
002700*               ÅRSBEHOV STÄLLS I RELATION TILL TOT.LAGERSALDO.           
002800*               ALLT UPP TILL   1  ÅRSBEHOV HAMNAR I KURANS 1             
002900*               FRÅN 2 UPP TILL 3  ÅRSBEHOV      ''-''      2             
003000*                    4          5  ÅRSBEHOV      ''-''      3             
003100*                    6          10 ÅRSBEHOV      ''-''      4             
003200*                                    RESTEN      ''-''      5             
003300*                                                                         
003400*               INDATA - DLB=DAGLIGT LAGERBAND                            
003500*                      - OISTAT = ORDER-INGÅNGS-STATISTIK                 
003600*                                                                         
003700*               UTDATA - FIL FÖR ANALYSER (2 ST)                          
003800*                      - SHELF-LIFELISTA MOT ORDERINGÅNG                  
003900     EJECT                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100                                                                          
004200 INPUT-OUTPUT SECTION.                                                    
004300 FILE-CONTROL.                                                            
004310                                                                          
004320*          --- ARTICLE INFO                                               
004500     SELECT W51264  ASSIGN       TO W51276D1.                             
004601                                                                          
004602*          --- INTERNAL GIT INFO                                          
004610     SELECT W51266  ASSIGN       TO W51276D2.                             
004810                                                                          
004820*          --- VCCS RECORDS AND QUANTITY AS PER DC                        
004830     SELECT W51293B ASSIGN       TO W51276D3.                             
005110                                                                          
005200*          --- OBSOLESCENCE REPORT FILE1                                  
005300     SELECT W51276  ASSIGN       TO W51276D4.                             
005310                                                                          
005320*          --- OBSOLESCENCE REPORT FILE2                                  
005330     SELECT W51276A  ASSIGN       TO W51276D5.                            
005400     EJECT                                                                
005410                                                                          
005500 DATA DIVISION.                                                           
005600 FILE SECTION.                                                            
005700                                                                          
005800 FD  W51264                                                               
005900     RECORDING F                                                          
006000     BLOCK CONTAINS 0.                                                    
006100                                                                          
006200*01  -COPY W51264    -L                                                   
006300     SKIP2                                                                
006310 FD  W51266                                                               
006320     RECORDING F                                                          
006330     BLOCK CONTAINS 0.                                                    
006340                                                                          
006350*01  -COPY W51266  -L                                                     
006360     SKIP2                                                                
006393     SKIP2                                                                
006400 FD  W51293B                                                              
006500     RECORDING F                                                          
006600     BLOCK CONTAINS 0.                                                    
006700                                                                          
006900*01  -COPY  W51295   -L.                                                  
007000     SKIP2                                                                
008300 FD  W51276                                                               
008400     RECORDING V                                                          
008500     BLOCK CONTAINS 0.                                                    
008600                                                                          
008700 01  POST                    PIC X(185).                                  
008800     EJECT                                                                
008801                                                                          
008810 FD  W51276A                                                              
008820     RECORDING V                                                          
008830     BLOCK CONTAINS 0.                                                    
008840                                                                          
008850 01  POST1                   PIC X(185).                                  
008860     EJECT                                                                
008900 WORKING-STORAGE SECTION.                                                 
009000                                                                          
009100*    -COPY WY2000W2                                                       
009200     SKIP3                                                                
009300 77  IDPGM                   PIC X(8)      VALUE 'W5127600'.              
009400 77  JA                      PIC X         VALUE 'J'.                     
009500 77  NEJ                     PIC X         VALUE 'N'.                     
009510 77  FIRST-SW                PIC X         VALUE 'J'.                     
009520     88 FIRST-LINE                         VALUE 'J'.                     
009530     EJECT                                                                
009600 77  EOF-W51264-SW           PIC X         VALUE 'N'.                     
009601     88 EOF-W51264                         VALUE 'J'.                     
009602     EJECT                                                                
009610 77  EOF-W51266-SW           PIC X         VALUE 'N'.                     
009620     88 EOF-W51266                         VALUE 'J'.                     
009630     EJECT                                                                
009640 77  EOF-W51293B-SW          PIC X         VALUE 'N'.                     
009650     88 EOF-W51293B                        VALUE 'J'.                     
009660     EJECT                                                                
009693 77  WS-IDDC                 PIC X(2).                                    
009694     88 CDC                                VALUE '11'.                    
009695     88 GOOD-DDC                           VALUE  'SE' 'NO' 'FI'          
009696                                                  'DE' 'PL' 'KR'          
009697                                                  'BE' 'FR'.              
009698     EJECT                                                                
009699 01  WS-L6-IDDC-ARTNR.                                                    
009700     05 WS-L6-IDDC           PIC X(2)      VALUE SPACES.                  
009701     05 WS-L6-ARTNR          PIC 9(9)      VALUE ZERO.                    
009702                                                                          
009703 01  WS-SALES-IDDC-ARTNR.                                                 
009704     05 WS-SALES-IDDC           PIC X(2)      VALUE SPACES.               
009705     05 WS-SALES-ARTNR          PIC 9(9)      VALUE ZERO.                 
009706                                                                          
009707 01  WS-LB-IDDC-ARTNR.                                                    
009708     05 WS-LB-IDDC           PIC X(2)      VALUE SPACES.                  
009709     05 WS-LB-ARTNR          PIC 9(9)      VALUE ZERO.                    
009710                                                                          
009720 77  SALES-ARTNR-CDC         PIC S9(9)     COMP-3.                        
009730 77  WS-IDDC-PREV            PIC X(2)      VALUE SPACES.                  
009741 77  DC-IX                   PIC S9(4)     VALUE +0  COMP SYNC.           
009750 77  MAX-WEEK                PIC S9(3)     COMP-3 VALUE +53.              
009800                                                                          
009900 01  W-TIAAVVD               PIC S9(5).                                   
010000 01  W-TIAAVVD-P             PIC S9(5)     VALUE +0  COMP-3.              
010100 01  LAGERVARDE              PIC S9(9)     VALUE +0  COMP-3.              
010200 01  TOT-PRARTNTO            PIC S9(11)V99 VALUE +0  COMP-3.              
010210 01  TOT-SUARTSTD            PIC S9(11)V99 VALUE +0  COMP-3.              
010300 01  W-SUSTDOI-AR            PIC S9(11)V99 VALUE +0  COMP-3.              
010400 01  TOT-INKRES              PIC S9(11)V99 VALUE +0  COMP-3.              
010500 01  TOT-VARDE               PIC S9(11)V99 VALUE +0  COMP-3.              
010600 01  SUPERTOT-VARDE          PIC S9(11)V99 VALUE +0  COMP-3.              
010700 01  W-RADER                 PIC S9(3)     VALUE +99 COMP-3.              
010900 01  W-KVAVIS                PIC S9(7)     VALUE +0  COMP-3.              
011000 01  W-SULEVANT-TOT          PIC S9(9)     VALUE +0  COMP-3.              
011010 01  W-SULEVANT-CDC-TOT      PIC S9(9)     VALUE +0  COMP-3.              
011100                                                                          
011200 01  INKURANT-RESERV-TABELL.                                              
011300     03  FILLER              PIC S9V99   VALUE 0.00  COMP-3.              
011400     03  FILLER              PIC S9V99   VALUE 0.00  COMP-3.              
011500     03  FILLER              PIC S9V99   VALUE 0.18  COMP-3.              
011600     03  FILLER              PIC S9V99   VALUE 0.53  COMP-3.              
011700     03  FILLER              PIC S9V99   VALUE 0.81  COMP-3.              
011800     03  FILLER              PIC S9V99   VALUE 1.00  COMP-3.              
011900 01  FILLER REDEFINES INKURANT-RESERV-TABELL.                             
012000     03  W-INKRESDEL OCCURS 6 PIC S9V99              COMP-3.              
012100                                                                          
012200 01  VARIABLER.                                                           
012300     03  H-IND1              PIC S9(3)     VALUE +0  COMP SYNC.           
012400     03  H-IND2              PIC S9(3)     VALUE +0  COMP SYNC.           
012410     03  H-IND3              PIC S9(1)     VALUE +0  COMP SYNC.           
012500     03  IX                  PIC S9(3)     VALUE +0  COMP SYNC.           
012600     EJECT                                                                
012700 01  WDATUM                  PIC X(6)      VALUE 'WDATUM'.                
012800                                                                          
012900 01  SUBPROGRAM.                                                          
013000     03  DATKORT             PIC X(8)      VALUE 'DATKORT'.               
013200     03  POSTSUM             PIC X(8)      VALUE 'POSTSUM'.               
013210     03  ABEND               PIC X(8)      VALUE 'ABEND  '.               
013300                                                                          
013400 01  LB-TRANSID.                                                          
013500     03  FILLER              PIC X(6)      VALUE 'W51264'.                
013600     03  FILLER              PIC X(8)      VALUE 'W51276D1'.              
013700     03  FILLER              PIC X(4)      VALUE ' LB '.                  
013800                                                                          
013810 01  GIT-TRANSID.                                                         
013820     03  FILLER              PIC X(6)      VALUE 'W51266'.                
013830     03  FILLER              PIC X(8)      VALUE 'W51276D2'.              
013840     03  FILLER              PIC X(4)      VALUE ' GIT'.                  
013846                                                                          
013900 01  SALES-TRANSID.                                                       
014000     03  FILLER              PIC X(7)      VALUE 'W51293B'.               
014100     03  FILLER              PIC X(8)      VALUE 'W51276D4'.              
014200     03  FILLER              PIC X(5)      VALUE 'SALES'.                 
015300     EJECT                                                                
015400*   -COPY W0005  -PRE POSTSUM-                                            
015500     EJECT                                                                
015600*   -COPY WDATKORT                                                        
015700     EJECT                                                                
015800 01  FILLER                  PIC X(16)   VALUE 'LB-AREA    '.             
015900*01  LBAREA     -COPY W51264    -PRE LB-.                                 
016000     EJECT                                                                
016100 01  FILLER                  PIC X(16)   VALUE 'GIT-AREA    '.            
016200*01  GITAREA    -COPY W51266    -PRE GIT-.                                
016300     EJECT                                                                
016900 01  FILLER                  PIC X(16)   VALUE 'IN-AREA     '.            
017000*01  INAREA     -COPY W51295    -PRE SALES-.                              
017200     EJECT                                                                
017401                                                                          
017435 01  TEXT-AREA.                                                           
017436     03  HEAD1.                                                           
017437         05  FILLER          PIC X(2)    VALUE                            
017440            'DC'.                                                         
017460         05  FILLER          PIC X       VALUE   ';'.                     
017461         05  FILLER          PIC X(14)   VALUE                            
017470            'TURNOVER GROUP'.                                             
017490         05  FILLER          PIC X       VALUE   ';'.                     
017491         05  FILLER          PIC X(13)   VALUE                            
017492            'PRODUCT GROUP'.                                              
017494         05  FILLER          PIC X       VALUE   ';'.                     
017495         05  FILLER          PIC X(10)   VALUE                            
017496            'STOCKVALUE'.                                                 
017498         05  FILLER          PIC X       VALUE   ';'.                     
017499         05  FILLER          PIC X(12)   VALUE                            
017500            'OBSOLESCENCE'.                                               
017501         05  FILLER          PIC X       VALUE   ';'.                     
017503                                                                          
017504     03  DC-RAD.                                                          
017505         05  RAD-IDDC        PIC X(2)    VALUE SPACES.                    
017506         05  FILLER          PIC X       VALUE   ';'.                     
017508         05  RAD-KGR         PIC 9.                                       
017509         05  FILLER          PIC X       VALUE   ';'.                     
017511         05  RAD-PKOD        PIC 99.                                      
017512         05  FILLER          PIC X       VALUE   ';'.                     
017514         05  RAD-LVARDE      PIC Z(9)9.99-.                               
017515         05  FILLER          PIC X       VALUE   ';'.                     
017517         05  RAD-INKRES      PIC Z(9)9.99-.                               
017518         05  FILLER          PIC X       VALUE   ';'.                     
017519                                                                          
017520     03  DC-TOTAL.                                                        
017521         05  TOT-IDDC        PIC X(2)    VALUE SPACES.                    
017522         05  FILLER          PIC X       VALUE   ';'.                     
017523         05  FILLER          PIC X(5)    VALUE                            
017524            'TOTAL'.                                                      
017525         05  FILLER          PIC X       VALUE   ';'.                     
017526         05  FILLER          PIC X(3)    VALUE                            
017527            'ALL'.                                                        
017528         05  FILLER          PIC X       VALUE   ';'.                     
017529         05  DCT-LVARDE      PIC Z(9)9.99-.                               
017530         05  FILLER          PIC X       VALUE   ';'.                     
017531         05  DCT-INKRES      PIC Z(9)9.99-.                               
017532         05  FILLER          PIC X       VALUE   ';'.                     
017533                                                                          
017551     03  ALLDC-TOT.                                                       
017552         05  FILLER          PIC X(3)    VALUE                            
017553             'TOT'.                                                       
017554         05  FILLER          PIC X       VALUE   ';'.                     
017555         05  ALLDC-KGR       PIC 9.                                       
017556         05  FILLER          PIC X       VALUE   ';'.                     
017557         05  ALLDC-PKOD      PIC 99.                                      
017558         05  FILLER          PIC X       VALUE   ';'.                     
017559         05  ALLDC-LVARDE    PIC Z(9)9.99-.                               
017560         05  FILLER          PIC X       VALUE   ';'.                     
017561         05  ALLDC-INKRES    PIC Z(9)9.99-.                               
017562         05  FILLER          PIC X       VALUE   ';'.                     
017563                                                                          
034500     EJECT                                                                
034600                                                                          
034700 01  O-KGR-TABELL.                                                        
034800*                                    **  TABELL FÖR  OIREG**              
034900     03  O-CL OCCURS 100.                                                 
035000         05  O-KGR OCCURS 7.                                              
035100             07  O-VARDE        PIC S9(11)V99   COMP-3.                   
035200     SKIP3                                                                
035210 01  DC-TOT-TABELL.                                                       
035220*                                    **  TABELL FÖR  DC-TOT               
035230     03  TOT-CL OCCURS 100.                                               
035240         05  TOT-KGR OCCURS 7.                                            
035250             07  DC-VARDE       PIC S9(11)V99   COMP-3.                   
035260     SKIP3                                                                
035261 01  DC-TABEL.                                                            
035262*                                    **  TABELL FÖR  ALL DC-TOT           
035263     03  ALLDC OCCURS 100.                                                
035264         05  DC-IDDC            PIC X(2)  VALUE SPACES.                   
035265         05  DC-CL OCCURS 100.                                            
035266             07  DC-KGR OCCURS 7.                                         
035267                 09  2-VARDE    PIC S9(11)V99   COMP-3.                   
035268     SKIP3                                                                
035300 01  TABELLINDEX.                                                         
035400     03  KGR-NY              PIC S9         COMP-3.                       
035500     03  PS-IX               PIC S9(3)      COMP-3.                       
035600     SKIP2                                                                
035700 01  FILLER                  PIC X(16)      VALUE 'W-KURTAB   '.          
035800 01  W-KURTAB.                                                            
035900     03  W-KUR1              PIC S9(11)V99  COMP-3.                       
036000     03  W-KUR2              PIC S9(11)V99  COMP-3.                       
036100     03  W-KUR3              PIC S9(11)V99  COMP-3.                       
036200     03  W-KUR4              PIC S9(11)V99  COMP-3.                       
036300     03  W-KUR5              PIC S9(11)V99  COMP-3.                       
036400     03  W-KUR6              PIC S9(11)V99  COMP-3.                       
036500     03  W-KUR1-GRANS        PIC S9(11)V99  COMP-3.                       
036600     03  W-KUR2-GRANS        PIC S9(11)V99  COMP-3.                       
036700     03  W-KUR3-GRANS        PIC S9(11)V99  COMP-3.                       
036800     03  W-KUR4-GRANS        PIC S9(11)V99  COMP-3.                       
036900     03  W-KUR5-GRANS        PIC S9(11)V99  COMP-3.                       
037000                                                                          
037200     EJECT                                                                
037300 PROCEDURE DIVISION.                                                      
037400                                                                          
037500 MAIN SECTION.                                                            
037510                                                                          
037600     PERFORM A-INIT                                                       
037700                                                                          
037710     PERFORM S01-READ-W51264-LB                                           
037720     PERFORM S02-READ-ORDER-SALES                                         
037920     PERFORM S04-READ-W51266-WDL6                                         
038000                                                                          
038100     PERFORM UNTIL EOF-W51264                                             
038200         PERFORM B-READ-SALES                                             
038210         PERFORM C-CHECK-GIT-PART                                         
038300         PERFORM D-CHECK-KGR-GRP                                          
038400         PERFORM S01-READ-W51264-LB                                       
038910     END-PERFORM                                                          
038911                                                                          
038920     PERFORM F-CREATE-OBSOLESCENCE-REP                                    
039100     PERFORM Z-FINIT                                                      
039200                                                                          
039300     MOVE ZERO TO RETURN-CODE                                             
039400     GOBACK                                                               
039500     .                                                                    
039600     EJECT                                                                
039700                                                                          
039800 A-INIT SECTION.                                                          
039900     OPEN INPUT  W51264                                                   
039910                 W51266                                                   
040000                 W51293B                                                  
040100          OUTPUT W51276                                                   
040200                 W51276A                                                  
040400                                                                          
040500     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
040600                                                                          
040800     CALL DATKORT           USING IDPGM WDATUM DATUMKORT                  
040900     MOVE D-AAR                TO W-TIAAVVD(1:2)                          
041200     MOVE D-VECKA              TO W-TIAAVVD(3:2)                          
041300     MOVE 1                    TO W-TIAAVVD(5:1)                          
041500     MOVE W-TIAAVVD            TO W-TIAAVVD-P                             
041600                                                                          
041900     INITIALIZE O-KGR-TABELL                                              
041910     INITIALIZE DC-TOT-TABELL                                             
041920     INITIALIZE DC-TABEL                                                  
042000     .                                                                    
042100     EJECT                                                                
042200                                                                          
042301 B-READ-SALES SECTION.                                                    
042302                                                                          
042303     MOVE +0                   TO W-SULEVANT-CDC-TOT                      
042304                                  W-SULEVANT-TOT                          
042306                                  SALES-ARTNR-CDC                         
042307                                                                          
042308*    MOVE LB-IDDC  TO WS-IDDC                                             
042309     IF CDC                                                               
042310       PERFORM BA-READ-SALES                                              
042311     ELSE                                                                 
042312       PERFORM BB-READ-SALES                                              
042313     END-IF                                                               
042314     .                                                                    
042315     EJECT                                                                
042316                                                                          
042317 BA-READ-SALES SECTION.                                                   
042318                                                                          
042600     PERFORM UNTIL EOF-W51293B OR                                         
042610                   WS-SALES-IDDC-ARTNR > WS-LB-IDDC-ARTNR                 
042620       IF SALES-IDDC   =  LB-IDDC                                         
042700       AND SALES-IDARTNR = LB-IDARTNR                                     
042800         IF SALES-SULEVANT  < +0                                          
042900           MOVE +0               TO W-SULEVANT-CDC-TOT                    
043000         ELSE                                                             
043100           MOVE SALES-SULEVANT    TO W-SULEVANT-CDC-TOT                   
043200         END-IF                                                           
043300           MOVE LB-IDARTNR         TO SALES-ARTNR-CDC                     
043400       END-IF                                                             
043500       PERFORM S02-READ-ORDER-SALES                                       
043600     END-PERFORM                                                          
043604     .                                                                    
043605     EJECT                                                                
043606 BB-READ-SALES SECTION.                                                   
043607                                                                          
043611     PERFORM UNTIL EOF-W51293B                                            
043613                OR WS-SALES-IDDC-ARTNR  > WS-LB-IDDC-ARTNR                
043616         IF SALES-IDDC   =  LB-IDDC                                       
043617         AND  SALES-IDARTNR  = LB-IDARTNR                                 
043619           IF SALES-SULEVANT < +0                                         
043620             MOVE  0              TO W-SULEVANT-TOT                       
043622           ELSE                                                           
043624             MOVE  SALES-SULEVANT TO W-SULEVANT-TOT                       
043800           END-IF                                                         
043902         END-IF                                                           
043904       PERFORM S02-READ-ORDER-SALES                                       
043905     END-PERFORM                                                          
043906     .                                                                    
043907     EJECT                                                                
043910 C-CHECK-GIT-PART SECTION.                                                
043911                                                                          
043912     MOVE +0                   TO W-KVAVIS                                
043920     PERFORM UNTIL EOF-W51266                                             
043921                OR WS-L6-IDDC-ARTNR  > WS-LB-IDDC-ARTNR                   
043930         IF GIT-IDDC = LB-IDDC AND GIT-IDARTNR = LB-IDARTNR               
043940            ADD  GIT-KVAVIS     TO W-KVAVIS                               
043957         END-IF                                                           
043960         PERFORM S04-READ-W51266-WDL6                                     
043970     END-PERFORM                                                          
043980     .                                                                    
043990     EJECT                                                                
043991                                                                          
044001 D-CHECK-KGR-GRP SECTION.                                                 
044002                                                                          
044003*    MOVE LB-IDDC  TO WS-IDDC                                             
044004     IF CDC                                                               
044005       PERFORM DA-CHECK-KGR-GRP-CDC                                       
044006     ELSE                                                                 
044007       PERFORM DB-CHECK-KGR-GRP-SDC-NDC                                   
044008     END-IF                                                               
044009     .                                                                    
044010     EJECT                                                                
044011                                                                          
044012 DA-CHECK-KGR-GRP-CDC SECTION.                                            
044013                                                                          
044016     IF LB-IDDC = WS-IDDC-PREV                                            
044020       CONTINUE                                                           
044030     ELSE                                                                 
044031       IF FIRST-LINE                                                      
044035         WRITE POST          FROM HEAD1                                   
044040         MOVE  LB-IDDC         TO WS-IDDC-PREV                            
044041         MOVE  NEJ             TO FIRST-SW                                
044042       ELSE                                                               
044050         PERFORM FA-CREATE-OBSOLESCENCE-REP1                              
044051         MOVE LB-IDDC          TO WS-IDDC-PREV                            
044052       END-IF                                                             
044060     END-IF                                                               
044070                                                                          
044100     COMPUTE TOT-SUARTSTD ROUNDED = LB-PRARTSTD *                         
044210                           ( LB-KVANTAL + W-KVAVIS )                      
044240                                                                          
044300* 'LB-TIFINLV + 2001 > 'DAGENS DATUM' =>                                  
044400*  => PUBLICERINGSVECKA YNGRE ÄN TVÅ ÅR                                   
044500     MOVE LB-TIFINLV           TO TMP1-YYWWD                              
044600     MOVE W-TIAAVVD-P          TO TMP2-YYWWD                              
044700     PERFORM WY2000P2                                                     
044800     IF LB-KDERS > +20                                                    
044900       MOVE +6                 TO KGR-NY                                  
045100       PERFORM S05-ADD-GIVEN-KURANS                                       
045200     ELSE                                                                 
045300       IF TMP1-YYWWD > +0                                                 
045400         IF (TMP1-YYWWD + 2001) > TMP2-YYWWD                              
045500           MOVE +1             TO KGR-NY                                  
045700           PERFORM S05-ADD-GIVEN-KURANS                                   
045800         ELSE                                                             
045900           IF LB-KVPB = +0 AND LB-KDERS = +0                              
045910               IF W-SULEVANT-CDC-TOT = +0                                 
046000                  MOVE +5           TO KGR-NY                             
046100                  PERFORM S05-ADD-GIVEN-KURANS                            
046110               ELSE                                                       
046120                  PERFORM DAA-TEST-BEST-KGR                               
046200                  PERFORM S06-ADD-KUR-SHELF-LIFE-OI                       
046210               END-IF                                                     
046300           ELSE                                                           
046420             IF LB-IDARTNR NOT = SALES-ARTNR-CDC                          
046500               MOVE +5         TO KGR-NY                                  
046700               PERFORM S05-ADD-GIVEN-KURANS                               
046800             ELSE                                                         
046910               IF W-SULEVANT-CDC-TOT = +0 AND LB-KDKG NOT = +5            
047000                 MOVE +5       TO KGR-NY                                  
047200                 PERFORM S05-ADD-GIVEN-KURANS                             
047300               ELSE                                                       
047400                 PERFORM DAA-TEST-BEST-KGR                                
047600                 PERFORM S06-ADD-KUR-SHELF-LIFE-OI                        
047700               END-IF                                                     
047800             END-IF                                                       
047900           END-IF                                                         
048000         END-IF                                                           
048100       ELSE                                                               
048200         MOVE +1               TO KGR-NY                                  
048400         PERFORM S05-ADD-GIVEN-KURANS                                     
048500       END-IF                                                             
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048801                                                                          
048802 DAA-TEST-BEST-KGR SECTION.                                               
048803*****        TESTAR LAGERVÄRDE MOT FÖRBRUKNING               **           
048804     COMPUTE LAGERVARDE = ( LB-KVANTAL + W-KVAVIS)                        
048805     IF LAGERVARDE <= W-SULEVANT-CDC-TOT                                  
048806               MOVE +1         TO KGR-NY                                  
048807     ELSE                                                                 
048808       IF LAGERVARDE <= (W-SULEVANT-CDC-TOT * 3)                          
048809               MOVE +2         TO KGR-NY                                  
048810       ELSE                                                               
048811         IF LAGERVARDE <= (W-SULEVANT-CDC-TOT * 5)                        
048812               MOVE +3         TO KGR-NY                                  
048813         ELSE                                                             
048814           IF LAGERVARDE <= (W-SULEVANT-CDC-TOT * 10)                     
048815               MOVE +4         TO KGR-NY                                  
048816           ELSE                                                           
048817               MOVE +5         TO KGR-NY                                  
048818           END-IF                                                         
048819         END-IF                                                           
048820       END-IF                                                             
048821     END-IF                                                               
048823     .                                                                    
048824     EJECT                                                                
051200*                                                                         
051300 DB-CHECK-KGR-GRP-SDC-NDC SECTION.                                        
051400                                                                          
051500     IF LB-IDDC = WS-IDDC-PREV                                            
051600       CONTINUE                                                           
051700     ELSE                                                                 
051800       IF FIRST-LINE                                                      
052100         WRITE POST          FROM HEAD1                                   
052200         MOVE  LB-IDDC         TO WS-IDDC-PREV                            
052300         MOVE  NEJ             TO FIRST-SW                                
052400       ELSE                                                               
052500         PERFORM FA-CREATE-OBSOLESCENCE-REP1                              
052600         MOVE LB-IDDC          TO WS-IDDC-PREV                            
052700       END-IF                                                             
052800     END-IF                                                               
052900                                                                          
053000     COMPUTE TOT-SUARTSTD ROUNDED = LB-PRARTSTD *                         
053100                           ( LB-KVANTAL + W-KVAVIS )                      
053200                                                                          
053300     MOVE LB-TIFINLV           TO TMP1-YYWWD                              
053400     MOVE W-TIAAVVD-P          TO TMP2-YYWWD                              
053500     PERFORM WY2000P2                                                     
053600     IF LB-KDERS > +20                                                    
053610       MOVE +6               TO KGR-NY                                    
053620       PERFORM S05-ADD-GIVEN-KURANS                                       
053630     ELSE                                                                 
053640       IF TMP1-YYWWD > +0                                                 
053650         IF (TMP1-YYWWD + 2001) > TMP2-YYWWD                              
053660           MOVE +1             TO KGR-NY                                  
053670           PERFORM S05-ADD-GIVEN-KURANS                                   
053680         ELSE                                                             
053690           IF LB-KVPB = +0 AND LB-KDERS = +0                              
053691             IF W-SULEVANT-TOT = +0                                       
053692               MOVE +5         TO KGR-NY                                  
053693               PERFORM S05-ADD-GIVEN-KURANS                               
053694             ELSE                                                         
053695               PERFORM DBA-TEST-BEST-KGR                                  
053696               PERFORM S06-ADD-KUR-SHELF-LIFE-OI                          
053697             END-IF                                                       
053698           ELSE                                                           
053702             IF W-SULEVANT-TOT = +0                                       
053703               MOVE +5         TO KGR-NY                                  
053704               PERFORM S05-ADD-GIVEN-KURANS                               
053705             ELSE                                                         
053706               PERFORM DBA-TEST-BEST-KGR                                  
053707               PERFORM S06-ADD-KUR-SHELF-LIFE-OI                          
053708             END-IF                                                       
053709           END-IF                                                         
053710         END-IF                                                           
053711       ELSE                                                               
053712         MOVE +1               TO KGR-NY                                  
053713         PERFORM S05-ADD-GIVEN-KURANS                                     
053714       END-IF                                                             
053715     END-IF                                                               
053716     .                                                                    
053717     EJECT                                                                
053718                                                                          
053719 DBA-TEST-BEST-KGR SECTION.                                               
053720                                                                          
053721     COMPUTE LAGERVARDE = ( LB-KVANTAL + W-KVAVIS)                        
053722                                                                          
053725     IF LAGERVARDE <= W-SULEVANT-TOT                                      
053726               MOVE +1         TO KGR-NY                                  
053727     ELSE                                                                 
053729       IF LAGERVARDE <= (W-SULEVANT-TOT * 3)                              
053730               MOVE +2         TO KGR-NY                                  
053731       ELSE                                                               
053733         IF LAGERVARDE <= (W-SULEVANT-TOT * 5)                            
053734               MOVE +3         TO KGR-NY                                  
053735         ELSE                                                             
053737           IF LAGERVARDE <= (W-SULEVANT-TOT * 10)                         
053738               MOVE +4         TO KGR-NY                                  
053739           ELSE                                                           
053740               MOVE +5         TO KGR-NY                                  
053741           END-IF                                                         
053742         END-IF                                                           
053743       END-IF                                                             
053744     END-IF                                                               
053745     .                                                                    
053746     EJECT                                                                
053747                                                                          
053748 F-CREATE-OBSOLESCENCE-REP SECTION.                                       
053749                                                                          
053750     PERFORM FA-CREATE-OBSOLESCENCE-REP1                                  
053751     PERFORM FB-CREATE-OBSOLESCENCE-REP2                                  
053752     .                                                                    
053753     EJECT                                                                
053754                                                                          
053755 FA-CREATE-OBSOLESCENCE-REP1 SECTION.                                     
053758         DISPLAY 'FA-CREATE-OBSOLESCENCE-REP1:'                           
053759*****     SUMMERAR TILL TOTALER I SHELF-LIFE-  OI-TABELL  **              
053760     MOVE 1     TO H-IND2                                                 
053761     PERFORM 100 TIMES                                                    
053762       MOVE 1   TO H-IND3                                                 
053763       PERFORM 6 TIMES                                                    
053764         ADD O-VARDE(H-IND2 H-IND3) TO O-VARDE(H-IND2 7)                  
053770         ADD 1  TO H-IND3                                                 
053780       END-PERFORM                                                        
053790       ADD 1    TO H-IND2                                                 
053791     END-PERFORM                                                          
053800*****   REDIGERAR DETALJRAD FÖR SHELF-LIFE-OI    PER P-KOD                
053900     MOVE WS-IDDC-PREV                         TO RAD-IDDC                
054000                                                  TOT-IDDC                
054300     MOVE 99          TO W-RADER                                          
054400     MOVE 1           TO H-IND2                                           
054410     ADD  1           TO H-IND1                                           
054411     IF H-IND1 <= 100 AND H-IND1 > 0                                      
054420      MOVE RAD-IDDC    TO DC-IDDC(H-IND1)                                 
054500      PERFORM 99 TIMES                                                    
054600        IF O-VARDE(H-IND2 7) NOT ZERO                                     
054700         MOVE 1       TO H-IND3                                           
054800         PERFORM 6 TIMES                                                  
055100            MOVE H-IND3                        TO RAD-KGR                 
055200            MOVE H-IND2                        TO RAD-PKOD                
055210            DISPLAY 'PD-H-IND3:' H-IND3                                   
055220            DISPLAY 'PD-H-IND2:' H-IND2                                   
055300            MOVE O-VARDE(H-IND2 H-IND3)        TO RAD-LVARDE              
055400            DISPLAY 'O-VARDE(H-IND2 H-IND3):'                             
055500                       O-VARDE(H-IND2 H-IND3)                             
055510            DISPLAY 'RAD-LVARDE:' RAD-LVARDE                              
055600            COMPUTE RAD-INKRES =                                          
055700                   O-VARDE(H-IND2 H-IND3) * W-INKRESDEL(H-IND3)           
055710            DISPLAY 'RAD-INKRES:' RAD-INKRES                              
055800            COMPUTE TOT-INKRES = TOT-INKRES +                             
055900                   O-VARDE(H-IND2 H-IND3) * W-INKRESDEL(H-IND3)           
056000            WRITE POST  FROM DC-RAD                                       
056010            MOVE O-VARDE(H-IND2 H-IND3)        TO                         
056020                                2-VARDE(H-IND1 H-IND2 H-IND3)             
058100            ADD 1     TO H-IND3                                           
058200         END-PERFORM                                                      
058300        END-IF                                                            
058310       COMPUTE TOT-VARDE = TOT-VARDE + O-VARDE(H-IND2 7 )                 
058400       ADD 1          TO H-IND2                                           
058500      END-PERFORM                                                         
058600     END-IF                                                               
058610     MOVE TOT-INKRES                           TO DCT-INKRES              
058700     MOVE TOT-VARDE                            TO DCT-LVARDE              
058800     WRITE POST       FROM DC-TOTAL                                       
058901     INITIALIZE O-KGR-TABELL                                              
058902     MOVE ZERO                                 TO TOT-INKRES              
058903     MOVE ZERO                                 TO TOT-VARDE               
065800     .                                                                    
065900     EJECT                                                                
066000                                                                          
066010 FB-CREATE-OBSOLESCENCE-REP2 SECTION.                                     
066012*****     SUMMERAR TILL TOTALER I SHELF-LIFE-  OI-TABELL  **              
066014     MOVE 1      TO H-IND2                                                
066015     PERFORM 100 TIMES                                                    
066016       MOVE 1    TO H-IND3                                                
066017       PERFORM 6 TIMES                                                    
066018         ADD DC-VARDE(H-IND2 H-IND3)    TO DC-VARDE(H-IND2 7)             
066019         ADD 1   TO H-IND3                                                
066020       END-PERFORM                                                        
066021       ADD 1     TO H-IND2                                                
066022     END-PERFORM                                                          
066023                                                                          
066024     MOVE 1      TO H-IND2                                                
066025     PERFORM 99 TIMES                                                     
066026       MOVE 1    TO H-IND3                                                
066027       IF DC-VARDE(H-IND2 7) NOT ZERO                                     
066028         PERFORM 7 TIMES                                                  
066030           ADD DC-VARDE(H-IND2 H-IND3)  TO DC-VARDE(100 H-IND3)           
066031           ADD 1 TO H-IND3                                                
066032         END-PERFORM                                                      
066034       END-IF                                                             
066036       ADD 1     TO H-IND2                                                
066037     END-PERFORM                                                          
066038                                                                          
066039*****   REDIGERAR DETALJRAD FÖR SHELF-LIFE-OI    PER P-KOD                
066042     WRITE POST1  FROM HEAD1                                              
066044                                                                          
066045     MOVE 1      TO H-IND2                                                
066046     PERFORM 99  TIMES                                                    
066047       IF DC-VARDE(H-IND2 7) NOT ZERO                                     
066048         MOVE 1  TO H-IND3                                                
066049         PERFORM 6 TIMES                                                  
066050           MOVE H-IND3                         TO ALLDC-KGR               
066051           MOVE H-IND2                         TO ALLDC-PKOD              
066052           MOVE DC-VARDE(H-IND2 H-IND3)        TO ALLDC-LVARDE            
066053           COMPUTE ALLDC-INKRES =                                         
066054                 DC-VARDE(H-IND2 H-IND3) * W-INKRESDEL(H-IND3)            
066055           WRITE POST1  FROM ALLDC-TOT                                    
066056           ADD 1 TO H-IND3                                                
066057         END-PERFORM                                                      
066058       END-IF                                                             
066059       ADD 1     TO H-IND2                                                
066060     END-PERFORM                                                          
066062     .                                                                    
066063     EJECT                                                                
066064                                                                          
066231                                                                          
066232 Z-FINIT SECTION.                                                         
066233                                                                          
066240     CLOSE W51293B                                                        
066300           W51264                                                         
066400           W51276                                                         
066410           W51276A                                                        
066500           W51266                                                         
066800     MOVE 'S'                TO POSTSUM-OPKOD                             
066900     CALL POSTSUM         USING POSTSUM-PARM                              
067000     .                                                                    
067100     EJECT                                                                
067200                                                                          
068400 S01-READ-W51264-LB SECTION.                                              
068500     READ W51264           INTO LB-LBAREA                                 
068600     AT END                                                               
068700       MOVE JA               TO EOF-W51264-SW                             
068710       MOVE HIGH-VALUES      TO WS-LB-IDDC-ARTNR                          
068800     NOT AT END                                                           
068810       MOVE LB-IDARTNR       TO WS-LB-ARTNR                               
068820       MOVE LB-IDDC          TO WS-LB-IDDC                                
068830                                WS-IDDC                                   
068900       MOVE LB-TRANSID       TO POSTSUM-TRANSID                           
069000       CALL POSTSUM       USING POSTSUM-PARM                              
069100     END-READ                                                             
069200     .                                                                    
069300     EJECT                                                                
069410                                                                          
069411 S02-READ-ORDER-SALES SECTION.                                            
069412     READ W51293B          INTO SALES-INAREA                              
069413     AT END                                                               
069414       MOVE JA               TO EOF-W51293B-SW                            
069415       MOVE HIGH-VALUES      TO WS-SALES-IDDC-ARTNR                       
069416     NOT AT END                                                           
069417       MOVE SALES-IDARTNR  TO WS-SALES-ARTNR                              
069418       MOVE SALES-IDDC     TO WS-SALES-IDDC                               
069419       MOVE SALES-TRANSID     TO POSTSUM-TRANSID                          
069420       CALL POSTSUM       USING POSTSUM-PARM                              
069421     END-READ                                                             
069422     .                                                                    
069470     EJECT                                                                
069495 S04-READ-W51266-WDL6 SECTION.                                            
069496     READ W51266           INTO GIT-GITAREA                               
069497     AT END                                                               
069498       MOVE JA               TO EOF-W51266-SW                             
069499       MOVE HIGH-VALUES      TO WS-L6-IDDC-ARTNR                          
069500     NOT AT END                                                           
069504       MOVE GIT-IDARTNR      TO WS-L6-ARTNR                               
069505       MOVE GIT-IDDC         TO WS-L6-IDDC                                
069506       MOVE GIT-TRANSID      TO POSTSUM-TRANSID                           
069507       CALL POSTSUM       USING POSTSUM-PARM                              
069508     END-READ                                                             
069509     .                                                                    
069510     EJECT                                                                
069511                                                                          
069520 S05-ADD-GIVEN-KURANS SECTION.                                            
069600     MOVE LB-KDPRODSL         TO PS-IX                                    
069700     ADD TOT-SUARTSTD         TO O-VARDE(PS-IX KGR-NY)                    
071600                                 DC-VARDE(PS-IX KGR-NY)                   
071610     .                                                                    
071700     EJECT                                                                
071800                                                                          
071900 S06-ADD-KUR-SHELF-LIFE-OI SECTION.                                       
072100     MOVE LB-KDPRODSL         TO PS-IX                                    
072101     MOVE LB-IDDC             TO WS-IDDC                                  
072110     IF CDC                                                               
072120       COMPUTE W-SUSTDOI-AR = W-SULEVANT-CDC-TOT * LB-PRARTSTD            
072130     ELSE                                                                 
072140       COMPUTE W-SUSTDOI-AR = W-SULEVANT-TOT * LB-PRARTSTD                
072150     END-IF                                                               
072400     MULTIPLY W-SUSTDOI-AR BY +1  GIVING W-KUR1-GRANS W-KUR1              
072500     MULTIPLY W-SUSTDOI-AR BY +3  GIVING W-KUR2-GRANS                     
072600     MULTIPLY W-SUSTDOI-AR BY +5  GIVING W-KUR3-GRANS W-KUR4              
072700     MULTIPLY W-SUSTDOI-AR BY +10 GIVING W-KUR4-GRANS                     
072800     MULTIPLY W-SUSTDOI-AR BY +2  GIVING              W-KUR3              
072810                                                      W-KUR2              
072900     MOVE ZERO                        TO              W-KUR5              
073000                                                                          
073100     IF TOT-SUARTSTD > W-KUR4-GRANS                                       
073200             SUBTRACT W-KUR4-GRANS FROM TOT-SUARTSTD GIVING W-KUR5        
073300     ELSE                                                                 
073400       MOVE ZERO                                       TO W-KUR5          
073500       IF TOT-SUARTSTD > W-KUR3-GRANS                                     
073600             SUBTRACT W-KUR3-GRANS FROM TOT-SUARTSTD GIVING W-KUR4        
073700       ELSE                                                               
073800         MOVE ZERO                                     TO W-KUR4          
073900         IF TOT-SUARTSTD > W-KUR2-GRANS                                   
074000             SUBTRACT W-KUR2-GRANS FROM TOT-SUARTSTD GIVING W-KUR3        
074100         ELSE                                                             
074200           MOVE ZERO                                   TO W-KUR3          
074300           IF TOT-SUARTSTD > W-KUR1-GRANS                                 
074400             SUBTRACT W-KUR1-GRANS FROM TOT-SUARTSTD GIVING W-KUR2        
074500           ELSE                                                           
074600             MOVE ZERO                                 TO W-KUR2          
074700             MOVE TOT-SUARTSTD                         TO W-KUR1          
074900           END-IF                                                         
075000         END-IF                                                           
075100       END-IF                                                             
075200     END-IF                                                               
075210                                                                          
075300     ADD W-KUR1                      TO O-VARDE(PS-IX 1)                  
075310                                        DC-VARDE(PS-IX 1)                 
075400     ADD W-KUR2                      TO O-VARDE(PS-IX 2)                  
075410                                        DC-VARDE(PS-IX 2)                 
075500     ADD W-KUR3                      TO O-VARDE(PS-IX 3)                  
075510                                        DC-VARDE(PS-IX 3)                 
075600     ADD W-KUR4                      TO O-VARDE(PS-IX 4)                  
075610                                        DC-VARDE(PS-IX 4)                 
075700     ADD W-KUR5                      TO O-VARDE(PS-IX 5)                  
075800                                        DC-VARDE(PS-IX 5)                 
078300     .                                                                    
078400     EJECT                                                                
078500                                                                          
081100*    -COPY WY2000P2                                                       
