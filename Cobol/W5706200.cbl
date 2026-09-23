000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5706200.                                                
000301 AUTHOR.         ANDERS HENRIKSSON                                        
000401 DATE-WRITTEN.   20120201                                                 
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*                                                                         
000801*    FUNKTION:                                                            
000901*        LÄSER IN 1 FIL, POSTER IFRÅN WDR8-PEDALBASEN.                    
001001*                                                                         
001101*                                                                         
001201*        KONTROLLERAR EKONOMISKA HÄNDELSER OCH AVVISAR                    
001301*        FELAKTIGA POSTER                                                 
001401*                                                                         
001501                                                                          
001601     SKIP3                                                                
001701 ENVIRONMENT DIVISION.                                                    
001801     SKIP2                                                                
001901 INPUT-OUTPUT SECTION.                                                    
002001                                                                          
002101 FILE-CONTROL.                                                            
002201     SKIP2                                                                
002301*          --- INFIL1-HÄNDELSEPOSTER                                      
002401     SELECT W57062                     ASSIGN TO W57062D1.                
002501     SKIP2                                                                
002601*          --- UTFIL1-KORREKTAPOSTER                                      
002701     SELECT W57064                     ASSIGN TO W57062D2.                
002801     SKIP2                                                                
002901*          --- UTFIL2-FELPOSTER                                           
003001     SELECT W57065                     ASSIGN TO W57062D3.                
003101     EJECT                                                                
003201 DATA DIVISION.                                                           
003301     SKIP3                                                                
003401 FILE SECTION.                                                            
003501     SKIP3                                                                
003601 FD  W57062                                                               
003701     RECORDING       F                                                    
003801     BLOCK CONTAINS  0.                                                   
003901                                                                          
004001*01  -COPY WDR801      -L.                                                
004101     SKIP3                                                                
004201                                                                          
004301 FD  W57064                                                               
004401     RECORDING       F                                                    
004501     BLOCK CONTAINS  0.                                                   
004601                                                                          
004701*01  POST -COPY WDR801 -PRE  RATT- -L.                                    
004801     SKIP3                                                                
004901                                                                          
005001 FD  W57065                                                               
005101     RECORDING       F                                                    
005201     BLOCK CONTAINS  0.                                                   
005301                                                                          
005401*01  POST -COPY WDR801 -PRE  FEL-  -L.                                    
005501     EJECT                                                                
005601 WORKING-STORAGE SECTION.                                                 
005701                                                                          
005801*    -- CHECKED BY WY2000                                                 
005901 77  IDPGM                       PIC X(8)    VALUE 'W5706300'.            
006001 77  JA                          PIC X       VALUE 'J'.                   
006101 77  NEJ                         PIC X       VALUE 'N'.                   
006201 77  W-KDTRADP                   PIC X(4)    VALUE SPACES.                
006301 77  W-PRKURS                    PIC S9(5)V9(5)                           
006401                                             VALUE +0   COMP-3.           
006501 77  W-REVALUTA                  PIC S9(5)                                
006502                                             VALUE +0   COMP-3.           
006503 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
006504 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
007200 77  W57061-EOF-SW               PIC X       VALUE 'N'.                   
007300     88  END-OF-W57062                       VALUE 'J'.                   
007400                                                                          
007500 77  WS-TOT-AMOUNT-DDI           PIC S9(9)V99  COMP-3 VALUE ZERO.         
007600 77  WS-LINE-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
007700 77  WS-DIFF-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
007800 77  WS-IDVERGL                  PIC X(10)   VALUE '          '.          
007901 77  WS-IDKUNDNR                 PIC S9(7)   COMP-3 VALUE ZERO.           
008000     EJECT                                                                
008100                                                                          
008200 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
008300*01  -COPY WWIDFTG                                                        
008400     EJECT                                                                
008500                                                                          
008600 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
008700*01  FILLER  -COPY WWDIST19   -RED TEST-IDDISTR.                          
008800*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
008900     EJECT                                                                
009000                                                                          
009100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009200 01  FILLER REDEFINES DAGENS-DATUM.                                       
009300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009600 01  W-AAAAMMDD                  PIC 9(8).                                
009700 01  WS-DAGENS-DATUM             PIC 9(8).                                
009801 01  WS-TIREGDAT-TOT             PIC 9(8) VALUE ZERO.                     
009901 01  FILLER REDEFINES WS-TIREGDAT-TOT.                                    
010001     03  WS-YY                   PIC 9(2).                                
010101     03  WS-TIREGDAT             PIC 9(6).                                
010200                                                                          
010300 01  RETURKOD                    PIC S9(4) COMP SYNC VALUE +0.            
010400 01  SPAR-KDVALISO               PIC X(3)       VALUE SPACE.              
010500 01  SPAR-BEFEL                  PIC X(20)      VALUE SPACE.              
010600 01  WS-KDVALISO                 PIC X(3)       VALUE SPACE.              
010700                                                                          
010800     EJECT                                                                
010900 01  FEL-TEXTER.                                                          
011000     03 W-FEL-1                 PIC X(20)   VALUE                         
011100     'SYSTEM ERROR 1'.                                                    
011200     03 W-FEL-3                 PIC X(20)   VALUE                         
011300     'RECEIVING WAREHOUSE'.                                               
011400     03 W-FEL-4                 PIC X(20)   VALUE                         
011500     'SENDING WAREHOUSE'.                                                 
011600     03 W-FEL-5                 PIC X(20)   VALUE                         
011700     'STOCKUPD MANDATORY'.                                                
011800     03 W-FEL-7                 PIC X(20)   VALUE                         
011900     'REGISTER DATE'.                                                     
012000     03 W-FEL-8                 PIC X(20)   VALUE                         
012100     'VERIFICATION DATE 1'.                                               
012200     03 W-FEL-81                PIC X(20)   VALUE                         
012300     'VERIFICATION DATE 2'.                                               
012400     03 W-FEL-9                 PIC X(20)   VALUE                         
012500     'SUM AMOUNT IS ZERO'.                                                
012600     03 W-FEL-10                PIC X(20)   VALUE                         
012700     'QUANTITY NUMBER 1'.                                                 
012800     03 W-FEL-11                PIC X(20)   VALUE                         
012900     'PARTNUMBER IS ZERO 1'.                                              
013000     03 W-FEL-12                PIC X(20)   VALUE                         
013100     'SUM AMOUNT NOT ZERO'.                                               
013200     03 W-FEL-13                PIC X(20)   VALUE                         
013300     'QUANTITY NUMBER 2'.                                                 
013400     03 W-FEL-14                PIC X(20)   VALUE                         
013500     'PARTNUMBER NOT ZERO'.                                               
013600     03 W-FEL-15                PIC X(20)   VALUE                         
013700     'MAIN EVENT NOT ADDED'.                                              
013800     03 W-FEL-16                PIC X(20)   VALUE                         
013900     'LEVEL NOT ADDED'.                                                   
014000     03 W-FEL-17                PIC X(20)   VALUE                         
014100     'WRONG CURRENCY CODE'.                                               
014200     03 W-FEL-18                PIC X(20)   VALUE                         
014300     'SUB EVENT NOT ADDED'.                                               
014400     03 W-FEL-19                PIC X(20)   VALUE                         
014500     'NO PRODUCT GROUP'.                                                  
014600     03 W-FEL-20                PIC X(20)   VALUE                         
014700     'NO LOC PRODUCT GROUP'.                                              
014800     03 W-FEL-21                PIC X(20)   VALUE                         
014900     'NO NET PRICE'.                                                      
015000     03 W-FEL-22                PIC X(20)   VALUE                         
015100     'NO LANDING COST'.                                                   
015200     03 W-FEL-23                PIC X(20)   VALUE                         
015300     'NO COST OF SALES'.                                                  
015400     03 W-FEL-24                PIC X(20)   VALUE                         
015500     'NO STANDARD PRICE'.                                                 
015600     03 W-FEL-25                PIC X(20)   VALUE                         
015700     'NO PURCHASE PRICE'.                                                 
015800     03 W-FEL-26                PIC X(20)   VALUE                         
015900     'NO SURCHARGE COST'.                                                 
016000     03 W-FEL-27                PIC X(20)   VALUE                         
016100     'NO SURCHARGE PACKING'.                                              
016200     03 W-FEL-28                PIC X(20)   VALUE                         
016300     'NO OVERHEAD SURCHARG'.                                              
016400     03 W-FEL-29                PIC X(20)   VALUE                         
016500     'SYSTEM ERROR 2'.                                                    
016600     03 W-FEL-30                PIC X(20)   VALUE                         
016700     'LEVEL NOT ADDED'.                                                   
016800     03 W-FEL-31                PIC X(20)   VALUE                         
016900     'CLIENT NOT ADDED'.                                                  
017000     03 W-FEL-32                PIC X(20)   VALUE                         
017100     'CURRENCY RATE WRONG'.                                               
017201     03 W-FEL-34                PIC X(20)   VALUE                         
017301     'NET PRICE NE SUM'.                                                  
017401     03 W-FEL-35                PIC X(20)   VALUE                         
017501     'NO TRANSPORT COST'.                                                 
017601     03 W-FEL-36                PIC X(20)   VALUE                         
017701     'PARTNUMBER IS ZERO 2'.                                              
017801     03 W-FEL-37                PIC X(20)   VALUE                         
017901     'NO TRADING PARTNER'.                                                
018002     03 W-FEL-38                PIC X(20)   VALUE                         
018102     'NO NET AND STD PRICE'.                                              
018103     03 W-FEL-39                PIC X(20)   VALUE                         
018104     'NO AVERAGE COST'.                                                   
018201                                                                          
018300                                                                          
018400     EJECT                                                                
018500 01  TRANSAR                      PIC X(4).                               
018600     88 GODK-TRANS                           VALUE '5106' '5108'          
018700                                                   '5109' '5116'          
018800                                                   '5151' '6302'          
018900                                                   '6303' '6309'          
019000                                                   '6193' '6119'          
019100                                                   '6192' '6193'          
019200                                                   '611C' '611D'          
019300                                                   '6115' '6144'          
019400                                                   '4731' '4737'          
019500                                                   '5108' '6203'          
019600                                                   '6117' '6115'          
019700                                                   '6148' '6133'          
019800                                                   '6147'                 
019900                                                   '4738' '6100'.         
020000                                                                          
020100     EJECT                                                                
020200 01  FLLSBOK                     PIC X.                                   
020300     88 GODK-FLLSBOK                        VALUE 'Y' 'N' 'J' ' '.        
020400                                                                          
020500     EJECT                                                                
020600 01  POST-SW                     PIC X      VALUE 'J'.                    
020700     88 POST-OK                             VALUE 'J'.                    
020800     88 POST-FEL                            VALUE 'N'.                    
020900                                                                          
021000     EJECT                                                                
021100 01  DYNAMISKA-SUBPROGRAM.                                                
021200*                                                                         
021300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
021400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
021500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021810     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
021900     SKIP2                                                                
022000*    --- PARAMETRAR TILL ABEND                                            
022100                                                                          
022200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
022300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
022400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
022500     SKIP2                                                                
022600 01  FELTEXT.                                                             
022700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
022800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
022900     EJECT                                                                
023000*    --- PARAMETRAR TILL DATKORT                                          
023100*                                                                         
023200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W57062'.              
023300     SKIP2                                                                
023400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
023500     SKIP2                                                                
023600*01  -COPY WDATKORT                                                       
023700     EJECT                                                                
023800*    --- PARAMETRAR TILL POSTSUM                                          
023900*                                                                         
024000*01  -COPY W0005   -PRE  POSTSUM-                                         
024100     EJECT                                                                
024200*01  -COPY WDATAREA                                                       
024300     EJECT                                                                
024310*01  -COPY W510CURR                                                       
024320     EJECT                                                                
024400 01  FILLER                      PIC X(16)      VALUE 'IMS'.              
024500                                                                          
024600 01  NYCKLAR-TILL-DLI.                                                    
024700     03  W-WDH501KY-X.                                                    
024800         05  W-IDFTG             PIC 9(2)        VALUE ZERO.              
024900         05  W-KDEKHHT           PIC X(3)        VALUE SPACE.             
025000     03  W-KDEKSHT-X.                                                     
025100         05  W-KDEKSHT           PIC X(3)        VALUE SPACE.             
025200     03  W-KDEKNIVA-X.                                                    
025300         05  W-KDEKNIVA          PIC X(5)        VALUE SPACE.             
025400*        05  W-FLLSBOK           PIC X           VALUE SPACE.             
025500     03  W-IDSYSMOT-X.                                                    
025600         05  W-IDSYSMOT          PIC X(4)        VALUE SPACE.             
025700     03  W-KDSEGKY-X.                                                     
025800         05  W-KDSEGKEY          PIC X           VALUE SPACE.             
027101     03  W-IDDC-B6-X.                                                     
027201         05 W-IDDC-B6            PIC X(2).                                
027301                                                                          
027401 01  STATUS-WS                   PIC XX.                                  
027501     88  SEGMENT-FINNS                      VALUE '  '.                   
027601     88  SEGMENT-FINNS-REDAN                VALUE 'II'.                   
027701     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
027801     88  SEGMENT-SLUT                       VALUE 'GB'.                   
027901     SKIP2                                                                
028001 01  GODK-STATUSKODER.                                                    
028101     03 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
028201                                                                          
028301 01  SSA1                        PIC X(64).                               
028401 01  SSA2                        PIC X(64).                               
028501 01  SSA3                        PIC X(64).                               
028601                                                                          
028701* ---IMS FUNKTIONSKODER----                                               
028801*01  -COPY W0003                                                          
028901     EJECT                                                                
029001                                                                          
029101*----DLI INPUT OCH OUTPUT AREA ------                                     
029201                                                                          
029301                                                                          
029401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
029501 01  DLI-IO-WDH501.                                                       
029601*    03  -COPY WDH501                                                     
029701     EJECT                                                                
029801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
029901 01  DLI-IO-WDH511.                                                       
030001*    03  -COPY WDH511                                                     
030101     EJECT                                                                
030201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
030301 01  DLI-IO-WDH521.                                                       
030401*    03  -COPY WDH521                                                     
030501     EJECT                                                                
030601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
030701 01  DLI-IO-WDH531.                                                       
030801*    03  -COPY WDH531                                                     
030901     EJECT                                                                
031001                                                                          
031101 01  IN1-AREA-START              PIC X(24)   VALUE                        
031201                                 'IN1-AREA-START  '.                      
031301     SKIP2                                                                
031401                                                                          
031501*01  AREA -COPY WDR801   -PRE IN1-                                        
031601*    05   -COPY W510EKHA -PRE IN1- -RED IN1-FIL-WDR801-DATA               
031701     EJECT                                                                
031801 01  RATT-AREA-START             PIC X(24)   VALUE                        
031901                                 'RATT-AREA-START  '.                     
032001     SKIP2                                                                
032101                                                                          
032201*01  AREA -COPY WDR801   -PRE RATT-                                       
032301*    05   -COPY W510EKHA -PRE RATT- -RED RATT-FIL-WDR801-DATA             
032401     EJECT                                                                
032501 01  FEL-AREA-START              PIC X(24)   VALUE                        
032601                                 'FEL-AREA-START  '.                      
032701     SKIP2                                                                
032801                                                                          
032901*01  AREA -COPY WDR801   -PRE FEL-                                        
033001*    05   -COPY W510EKHA -PRE FEL- -RED FEL-FIL-WDR801-DATA               
033101                                                                          
033501 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033601 01   DLI-IO-AREA-B601.                                                   
033701*     03  -COPY WDB601                                                    
033801                                                                          
033901     EJECT                                                                
034001 LINKAGE SECTION.                                                         
034101                                                                          
034201*01  -COPY W0008     -PRE WDH5-                                           
034301     05 FILLER              PIC X.                                        
034401                                                                          
034501*01  -COPY W0008     -PRE 9305-                                           
034601     05 FILLER              PIC X.                                        
034701                                                                          
034801*01  -COPY W0008     -PRE WDB6-                                           
034901     05 FILLER              PIC X.                                        
035001                                                                          
035101 PROCEDURE DIVISION USING   WDH5-PCB 9305-PCB WDB6-PCB.                   
035201                                                                          
035301 MAIN SECTION.                                                            
035401     ENTRY 'DLITCBL' USING  WDH5-PCB 9305-PCB WDB6-PCB.                   
035501                                                                          
035601     PERFORM A-INIT                                                       
035701     PERFORM S01-LAES-W57062                                              
035801     PERFORM UNTIL END-OF-W57062                                          
035901       PERFORM B-KONTROLLERA-MED-REGELVERK                                
036001       PERFORM S01-LAES-W57062                                            
036101     END-PERFORM                                                          
036201                                                                          
036301     PERFORM Z-FINIT                                                      
036401                                                                          
036501     MOVE ZERO TO  RETURN-CODE                                            
036601     GOBACK                                                               
036701     .                                                                    
036801     EJECT                                                                
036901                                                                          
037001 A-INIT SECTION.                                                          
037101     OPEN INPUT  W57062                                                   
037201                                                                          
037301     OPEN OUTPUT W57064                                                   
037401                 W57065                                                   
037501     SKIP2                                                                
037601     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
037701     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
037801     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
037901     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
038001     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
038101                                                                          
038201     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
038301     .                                                                    
038401     EJECT                                                                
038501                                                                          
038601 B-KONTROLLERA-MED-REGELVERK SECTION.                                     
038701     IF IN1-EKH-IDVERGL NOT = WS-IDVERGL                                  
038801       MOVE IN1-EKH-IDVERGL  TO WS-IDVERGL                                
038901     END-IF                                                               
039001                                                                          
039101**** FIX PROBLEM MED SAMFAKTURERING DÄR KUNDNR 0 ANVÄNDS FÖR              
039201**** SUMMA OCH TILLÄGG                                                    
039301     IF  IN1-EKH-KDEKHHT  = '102'                                         
039401     AND IN1-EKH-KDEKSHT  = '120'                                         
039501     AND IN1-EKH-KDEKNIVA = 'DET'                                         
039601       MOVE IN1-EKH-IDKUNDNR TO WS-IDKUNDNR                               
039701     END-IF                                                               
039801     IF  IN1-EKH-KDEKHHT  = '102'                                         
039901     AND IN1-EKH-KDEKSHT  = '120'                                         
040001     AND IN1-EKH-KDEKNIVA = 'SUM'                                         
040101       MOVE WS-IDKUNDNR TO IN1-EKH-IDKUNDNR                               
040201     END-IF                                                               
040301****                                                                      
040401                                                                          
040501     MOVE JA TO POST-SW                                                   
040601     MOVE SPACE TO SPAR-BEFEL                                             
040701     MOVE IN1-EKH-KDEKHHT  TO W-KDEKHHT                                   
040801     MOVE IN1-EKH-KDEKSHT  TO W-KDEKSHT                                   
040901     MOVE IN1-EKH-KDEKNIVA TO W-KDEKNIVA                                  
041001                                                                          
041101     MOVE IN1-EKH-KDTRADP  TO W-KDTRADP                                   
041201     PERFORM IMS-GU-WDB601-TRADP                                          
041301     IF SEGMENT-FINNS                                                     
041401       MOVE DCS-IDFTG      TO W-IDFTG                                     
041501     ELSE                                                                 
041601       MOVE W-FEL-37       TO SPAR-BEFEL                                  
041701       MOVE NEJ            TO POST-SW                                     
041801     END-IF                                                               
041901                                                                          
042001     IF POST-OK                                                           
042101       PERFORM IMS-GET-WDH5-ALL                                           
042201       IF SEGMENT-FINNS                                                   
042301         IF NIVA-FLPRODSL = 'Y' AND IN1-EKH-KDPRODSL NOT > 0              
042401           MOVE W-FEL-19     TO SPAR-BEFEL                                
042501           MOVE NEJ TO POST-SW                                            
042601         END-IF                                                           
042701         IF NIVA-FLPSLLOC = 'Y' AND POST-OK                               
042801           IF IN1-EKH-KDPSLLOC NOT > 0                                    
042901             MOVE W-FEL-20   TO SPAR-BEFEL                                
043001             MOVE NEJ        TO POST-SW                                   
043101           END-IF                                                         
043201         END-IF                                                           
043301* INGA PRIS KONTROLLER PÅ HHT=501 SHT=501, NOLL KAN FÖREKOMMA I           
043401* PRISFÄLTEN                                                              
043501         IF (IN1-EKH-KDEKHHT = '501' AND IN1-EKH-KDEKSHT = '501')         
043601         OR (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '201')         
043701         OR (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '204')         
043801         OR (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '251')         
043802         OR (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '301')         
043901             CONTINUE                                                     
044001         ELSE                                                             
044101           IF NIVA-FLARTNTO = 'Y' AND POST-OK                             
044201             IF IN1-EKH-PRARTNTO NOT > 0                                  
044301               MOVE W-FEL-21   TO SPAR-BEFEL                              
044401               MOVE NEJ        TO POST-SW                                 
044501             END-IF                                                       
044601           END-IF                                                         
044701           IF NIVA-FLARTSJK = 'Y' AND POST-OK                             
044801             IF IN1-EKH-PRARTSJK = 0                                      
044901               MOVE W-FEL-23   TO SPAR-BEFEL                              
045001               MOVE NEJ        TO POST-SW                                 
045101             END-IF                                                       
045201           END-IF                                                         
045301           IF NIVA-FLARTSTD = 'Y' AND POST-OK                             
045401             IF IN1-EKH-PRARTSTD  = 0                                     
045501               MOVE W-FEL-24   TO SPAR-BEFEL                              
045601               MOVE NEJ        TO POST-SW                                 
045701             END-IF                                                       
045801           END-IF                                                         
045901         END-IF                                                           
046001         IF NIVA-FLAVCOST = 'Y' AND POST-OK                               
046101           IF IN1-EKH-PRLANDCO NOT > 0                                    
046201             MOVE W-FEL-22   TO SPAR-BEFEL                                
046301             MOVE NEJ        TO POST-SW                                   
046401           END-IF                                                         
046501         END-IF                                                           
046601         IF NIVA-FLINK    = 'Y' AND POST-OK                               
046701           IF IN1-EKH-PRINK NOT > 0                                       
046801             MOVE W-FEL-25   TO SPAR-BEFEL                                
046901             MOVE NEJ        TO POST-SW                                   
047001           END-IF                                                         
047101         END-IF                                                           
047201         IF NIVA-FLDIRLON = 'Y' AND POST-OK                               
047301           IF IN1-EKH-PRDIRLON NOT > 0                                    
047401             MOVE W-FEL-26   TO SPAR-BEFEL                                
047501             MOVE NEJ        TO POST-SW                                   
047601           END-IF                                                         
047701         END-IF                                                           
047801         IF NIVA-FLDMTRL  = 'Y' AND POST-OK                               
047901           IF IN1-EKH-PRDMTRL NOT > 0                                     
048001             MOVE W-FEL-27   TO SPAR-BEFEL                                
048101             MOVE NEJ        TO POST-SW                                   
048201           END-IF                                                         
048301         END-IF                                                           
048401         IF NIVA-FLOVRPAL = 'Y' AND POST-OK                               
048501           IF IN1-EKH-PROVRPAL NOT > 0                                    
048601               MOVE W-FEL-28   TO SPAR-BEFEL                              
048701               MOVE NEJ        TO POST-SW                                 
048801           END-IF                                                         
048901         END-IF                                                           
049001         IF NIVA-FLHEMTAG = 'Y' AND POST-OK                               
049101           IF IN1-EKH-PRHEMTAG NOT > 0                                    
049201               MOVE W-FEL-35   TO SPAR-BEFEL                              
049301               MOVE NEJ        TO POST-SW                                 
049401           END-IF                                                         
049501         END-IF                                                           
049601         IF IN1-EKH-KDEKNIVA = 'DET'                                      
049701           IF IN1-EKH-FLLSBOK = SPACE                                     
049801             IF NIVA-FLLSBOK = 'Y' AND POST-OK                            
049901               MOVE 'Y'      TO IN1-EKH-FLLSBOK                           
050001             END-IF                                                       
050101           END-IF                                                         
050201         END-IF                                                           
050301         IF POST-OK                                                       
050401           PERFORM IMS-GNP-WDH5                                           
050501           IF SEGMENT-SAKNAS                                              
050601             IF W-KDEKNIVA  = 'SUM' OR 'MOMS'                             
050701               CONTINUE                                                   
050801             ELSE                                                         
050901               MOVE W-FEL-30 TO SPAR-BEFEL                                
051001               MOVE NEJ TO POST-SW                                        
051101             END-IF                                                       
051201           ELSE                                                           
051301             IF SYST-IDSYSMOT = SPACE                                     
051401               MOVE W-FEL-31 TO SPAR-BEFEL                                
051501               MOVE NEJ TO POST-SW                                        
051601             END-IF                                                       
051701           END-IF                                                         
051801         END-IF                                                           
051901         IF POST-OK                                                       
052001         MOVE IN1-EKH-KDVALISO TO SPAR-KDVALISO                           
052101           MOVE IN1-EKH-KDVALISO TO SPAR-KDVALISO                         
052201           PERFORM S03-KONTROLLERA-KDVALISO                               
052301           IF WS-KDVALISO = SPACE                                         
052401             MOVE W-FEL-17 TO SPAR-BEFEL                                  
052501           ELSE                                                           
052601             PERFORM BA-KONTROLLERA-POST                                  
052701           END-IF                                                         
052801         END-IF                                                           
052901       ELSE                                                               
053001         PERFORM IMS-GET-WDH5-HHT                                         
053101         IF SEGMENT-SAKNAS                                                
053201           MOVE W-FEL-15        TO SPAR-BEFEL                             
053301         ELSE                                                             
053401           PERFORM IMS-GET-WDH5-SHT                                       
053501           IF SEGMENT-SAKNAS                                              
053601             MOVE W-FEL-18      TO SPAR-BEFEL                             
053701           ELSE                                                           
053801             PERFORM IMS-GET-WDH5-NIVA                                    
053901             IF SEGMENT-SAKNAS                                            
054001                 MOVE W-FEL-16    TO SPAR-BEFEL                           
054101             END-IF                                                       
054201           END-IF                                                         
054301         END-IF                                                           
054401       END-IF                                                             
054501     END-IF                                                               
054601     IF SPAR-BEFEL NOT = SPACE                                            
054701       PERFORM BC-SKICKA-FELPOST                                          
054801     END-IF                                                               
054901     .                                                                    
055001     EJECT                                                                
055101                                                                          
055201 BA-KONTROLLERA-POST SECTION.                                             
055301     IF IN1-FIL-IDPGM(1:1) NOT = 'W'                                      
055401       MOVE W-FEL-1 TO SPAR-BEFEL                                         
055501     ELSE                                                                 
055601       IF SPAR-BEFEL = SPACE                                              
055701         MOVE IN1-EKH-IDDC-SEND TO W-IDDC-B6                              
055801         PERFORM IMS-GU-WDB601                                            
055901         IF DCS-KDDC = SPACE AND IN1-EKH-IDDC-SEND NOT = SPACE            
056001           MOVE W-FEL-4 TO SPAR-BEFEL                                     
056101         ELSE                                                             
056201           MOVE IN1-EKH-IDDC-REC TO W-IDDC-B6                             
056301           PERFORM IMS-GU-WDB601                                          
056401           IF DCS-KDDC = SPACE AND IN1-EKH-IDDC-REC NOT = SPACE           
056501             MOVE W-FEL-3 TO SPAR-BEFEL                                   
056601           ELSE                                                           
056701             MOVE IN1-EKH-FLLSBOK TO FLLSBOK                              
056801             IF IN1-EKH-FLLSBOK = 'J'                                     
056901               MOVE 'Y' TO IN1-EKH-FLLSBOK                                
057001             END-IF                                                       
057101             IF NOT GODK-FLLSBOK                                          
057201               MOVE W-FEL-5 TO SPAR-BEFEL                                 
057301             ELSE                                                         
057401                 MOVE 'AAMMDD' TO DAT-KDDATFORM                           
057501                 MOVE DAGENS-DATUM TO DAT-I-TIDATUM                       
057601                                                                          
057701                 CALL WDATKONV USING DAT-KDDATFORM                        
057801                                     DAT-I-TIDATUM                        
057901                                     DAT-O-TIDATUM                        
058001                                     DAT-KDSVAR                           
058101                                                                          
058201                 IF DAT-KDSVAR-OK                                         
058301                   MOVE DAT-TIAAMMDD TO W-AAAAMMDD                        
058401                   MOVE DAT-TISEKEL  TO W-AAAAMMDD(1:2)                   
058501                 ELSE                                                     
058601                   MOVE +1000        TO RETURKOD                          
058701                   CALL ABEND USING RETURKOD                              
058801                 END-IF                                                   
058901                 MOVE IN1-FIL-TIREGDAT TO WS-TIREGDAT                     
059001                 MOVE 20               TO WS-YY                           
059101                 IF WS-TIREGDAT-TOT > WS-DAGENS-DATUM                     
059201                   MOVE W-FEL-7 TO SPAR-BEFEL                             
059301                 ELSE                                                     
059401                   IF IN1-EKH-DAVERDAT >  WS-DAGENS-DATUM                 
059501                     MOVE W-FEL-8 TO SPAR-BEFEL                           
059601                   ELSE                                                   
059701                     IF IN1-EKH-DAVERDAT > WS-TIREGDAT-TOT                
059801                       MOVE W-FEL-81 TO SPAR-BEFEL                        
059901                     ELSE                                                 
060001                       IF IN1-EKH-KDEKNIVA NOT = 'DET'                    
060101                         IF IN1-EKH-SUBEL = ZERO                          
060201* SUMMABELOPP FÅR VARA NOLL NÄR DET ÄR EN 404-401 POST, SKROT             
060301                          IF IN1-EKH-KDEKHHT = '404' AND                  
060401                             IN1-EKH-KDEKSHT = '401'                      
060501                            CONTINUE                                      
060601                          ELSE                                            
060701                            IF IN1-EKH-KDEKHHT = '204' AND                
060801                               IN1-EKH-KDEKSHT = '204'                    
060901                              CONTINUE                                    
061001                            ELSE                                          
061101                              MOVE W-FEL-9 TO SPAR-BEFEL                  
061201                            END-IF                                        
061301                          END-IF                                          
061401                         ELSE                                             
061501                           IF IN1-EKH-KVANTAL NOT = ZERO                  
061601                             MOVE W-FEL-10 TO SPAR-BEFEL                  
061602                           ELSE                                           
061801                             IF IN1-EKH-KDEKHHT = '301'                   
061901                               IF IN1-EKH-KDEKSHT = '301' OR              
062001                                             '302' OR '303'               
062101                                 IF IN1-EKH-IDARTNR = ZERO                
062201                                   MOVE W-FEL-11 TO                       
062301                                        SPAR-BEFEL                        
062401                                 END-IF                                   
062501                               ELSE                                       
062601                                 IF IN1-EKH-IDARTNR NOT = ZERO            
062701                                   MOVE W-FEL-14 TO                       
062801                                        SPAR-BEFEL                        
062902                                 END-IF                                   
063001                               END-IF                                     
063101                             END-IF                                       
063201                           END-IF                                         
063301                         END-IF                                           
063401                       ELSE                                               
063501                         IF IN1-EKH-KDEKNIVA = 'DET'                      
063601                           IF IN1-EKH-SUBEL NOT = ZERO                    
063701                             MOVE W-FEL-12 TO SPAR-BEFEL                  
063801                           ELSE                                           
063901                             IF IN1-EKH-IDARTNR = ZERO                    
064001                               IF IN1-EKH-KDEKHHT = '204' AND             
064101                                  IN1-EKH-KDEKSHT = '204'                 
064201                                 CONTINUE                                 
064301                               ELSE                                       
064401                                 MOVE W-FEL-36 TO SPAR-BEFEL              
064501                               END-IF                                     
064601                             ELSE                                         
064602                              IF  IN1-EKH-KDEKHHT = '303'                 
064603                              AND IN1-EKH-KDEKSHT = '311'                 
064604                                 IF IN1-EKH-PRARTSTD = 0                  
064605                                   MOVE W-FEL-39   TO SPAR-BEFEL          
064606                                 END-IF                                   
064607                              END-IF                                      
064701                               IF IN1-EKH-KDEKHHT(1:1) = '2' OR           
064801                                  IN1-EKH-KDEKHHT = '303'                 
064901                                 CONTINUE                                 
065001                               ELSE                                       
065101                                 IF IN1-EKH-KVANTAL = ZERO                
065201* DET KAN KOMMA POSTER MED NOLL I ANTAL                                   
065301* UNDANTAGET GÄLLER BARA SKROTNING, 404-401 POSTER                        
065401* ALLA ANDRA POSTER BLIR DET EN FELPOST UTAV                              
065501                                   IF IN1-EKH-KDEKHHT = '404' AND         
065601                                      IN1-EKH-KDEKSHT = '401'             
065701                                     CONTINUE                             
065801                                   ELSE                                   
065901                                     MOVE W-FEL-13 TO SPAR-BEFEL          
066001                                   END-IF                                 
066101                                 END-IF                                   
066201                               END-IF                                     
066301                             END-IF                                       
066401                           END-IF                                         
066501                         END-IF                                           
066601                       END-IF                                             
066701                     END-IF                                               
066801                   END-IF                                                 
066901                 END-IF                                                   
067001             END-IF                                                       
067101           END-IF                                                         
067201         END-IF                                                           
067301       END-IF                                                             
067401     END-IF                                                               
067501     IF SPAR-BEFEL = SPACE                                                
067601       IF IN1-EKH-IDDISTR  > 0                                            
067701       OR IN1-EKH-IDKUNDNR > 0                                            
067801       OR IN1-EKH-IDVERGL  > 0                                            
067901         CONTINUE                                                         
068001       ELSE                                                               
068101         MOVE W-FEL-29 TO SPAR-BEFEL                                      
068201       END-IF                                                             
068301       IF IN1-EKH-PRKURS = 0 AND IN1-EKH-KDEKNIVA = 'DET'                 
068401         MOVE W-FEL-32 TO SPAR-BEFEL                                      
068501       END-IF                                                             
068601     END-IF                                                               
068701                                                                          
068801     IF SPAR-BEFEL = SPACE                                                
068901       IF IN1-FIL-IDPGM = 'W4183000' OR 'W4263400' OR 'W4263500'          
069001       OR 'W5402000' OR 'W4183C00'                                        
069101* SYSTEM W426-KRF, W54020, W41830 M. FL. SKALL GÅ DEN GAMLA VÄGEN         
069201         PERFORM BD-SKICKA-RATT-POST                                      
069301       ELSE                                                               
069401* HÄR GENERERAS KURSDIFF-POSTER FÖR DEALER-NET/DDI MARKNADER              
069501         IF IN1-EKH-KDEKNIVA = 'DET'                                      
069601           IF  IN1-EKH-PRARTNTO = ZERO                                    
069701           AND IN1-EKH-PRARTSTD = ZERO                                    
069802             MOVE W-FEL-38 TO SPAR-BEFEL                                  
069901             PERFORM BC-SKICKA-FELPOST                                    
070001           ELSE                                                           
070101             PERFORM BD-SKICKA-RATT-POST                                  
070201           END-IF                                                         
070301         ELSE                                                             
070401           IF IN1-EKH-KDEKNIVA = 'SUM'                                    
071001             PERFORM BD-SKICKA-RATT-POST                                  
072001           ELSE                                                           
073001             IF IN1-EKH-SUBEL = ZERO                                      
074001               CONTINUE                                                   
074101             ELSE                                                         
074201               PERFORM BD-SKICKA-RATT-POST                                
074301             END-IF                                                       
074401           END-IF                                                         
074501         END-IF                                                           
074601       END-IF                                                             
074701                                                                          
074801     ELSE                                                                 
074901       PERFORM BC-SKICKA-FELPOST                                          
075001     END-IF                                                               
075101     .                                                                    
075201     EJECT                                                                
075301                                                                          
075401 BC-SKICKA-FELPOST SECTION.                                               
075501     IF IN1-EKH-KDEKHHT = '2??' AND IN1-EKH-KDEKSHT = '2??'               
075601       CONTINUE                                                           
075701     ELSE                                                                 
075801       MOVE IN1-AREA             TO FEL-AREA                              
075901       MOVE SPAR-BEFEL           TO FEL-EKH-BEFELSAP                      
076001                                                                          
076101       MOVE IN1-FIL-CT-IDSYSTEM  TO FEL-FIL-IDCPYTXT(1:4)                 
076201       MOVE 'EKFA'               TO FEL-FIL-IDCPYTXT(5:4)                 
076301                                                                          
076401       PERFORM S12-SKRIV-FEL-POST                                         
076501     END-IF                                                               
076601                                                                          
076701     .                                                                    
076801     EJECT                                                                
076901                                                                          
077001 BD-SKICKA-RATT-POST SECTION.                                             
077101     MOVE IN1-AREA TO RATT-AREA                                           
077201     PERFORM S11-SKRIV-RATT-POST                                          
077301     .                                                                    
077401     EJECT                                                                
077501                                                                          
077600 Z-FINIT SECTION.                                                         
077700     CLOSE W57062                                                         
077800                                                                          
077900           W57064                                                         
078000           W57065                                                         
078100     SKIP2                                                                
078200     MOVE 'S' TO POSTSUM-OPKOD                                            
078300     CALL POSTSUM USING POSTSUM-PARM                                      
078400     .                                                                    
078500     EJECT                                                                
078601                                                                          
078700 S01-LAES-W57062  SECTION.                                                
078800     READ W57062 INTO IN1-AREA                                            
078900     AT END                                                               
079000        MOVE HIGH-VALUE TO IN1-AREA                                       
079100        SET END-OF-W57062 TO TRUE                                         
079200                                                                          
079300     NOT AT END                                                           
079400        MOVE 'W57062' TO POSTSUM-FDNAMN                                   
079500        MOVE 'W57062D1' TO POSTSUM-DDNAMN2                                
079600        MOVE 'INPOST'   TO POSTSUM-TRANSTYP                               
079700        CALL POSTSUM USING POSTSUM-PARM                                   
079800     END-READ                                                             
079900     .                                                                    
080000     EJECT                                                                
080101                                                                          
080200 S03-KONTROLLERA-KDVALISO SECTION.                                        
080301     MOVE IN1-EKH-DAVERDAT(3:4)    TO W-DATE-AAMM                         
080302     MOVE W-DATE-AAMM              TO CURR-TIAAMM                         
080601     MOVE SPAR-KDVALISO            TO CURR-KDVALISO-ROW                   
080602     MOVE WS-KDVALISO-HUV          TO CURR-KDVALISO-HUV                   
080603     MOVE 'M'                      TO CURR-KDVALTYP                       
080604     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
080605     IF CURR-KDSVAR = ' '                                                 
080606       MOVE CURR-PRKURS-NEW        TO W-PRKURS                            
080607       MOVE CURR-REVALUTA-TO       TO W-REVALUTA                          
080608       MOVE SPAR-KDVALISO          TO WS-KDVALISO                         
080609     ELSE                                                                 
080610       MOVE 1                      TO W-PRKURS                            
080611       MOVE 1                      TO W-REVALUTA                          
080612       MOVE SPACE                  TO WS-KDVALISO                         
080620     END-IF                                                               
081901     .                                                                    
082000     EJECT                                                                
082101                                                                          
082200 S11-SKRIV-RATT-POST SECTION.                                             
082300     WRITE RATT-POST FROM RATT-AREA                                       
082400                                                                          
082500     MOVE 'GODK-POST' TO POSTSUM-TRANSTYP                                 
082600     MOVE 'W57064' TO POSTSUM-FDNAMN                                      
082700     MOVE 'W57062D2' TO POSTSUM-DDNAMN2                                   
082800     CALL POSTSUM USING POSTSUM-PARM                                      
082900     .                                                                    
083000     EJECT                                                                
083101                                                                          
083200 S12-SKRIV-FEL-POST SECTION.                                              
083300     WRITE FEL-POST FROM FEL-AREA                                         
083400                                                                          
083500     MOVE 'FEL-POST' TO POSTSUM-TRANSTYP                                  
083600     MOVE 'W57065' TO POSTSUM-FDNAMN                                      
083700     MOVE 'W57065D5' TO POSTSUM-DDNAMN2                                   
083800     CALL POSTSUM USING POSTSUM-PARM                                      
083900     MOVE SPACE TO SPAR-BEFEL                                             
084000     .                                                                    
084100     EJECT                                                                
084201                                                                          
084300 IMS-GET-WDH5-HHT SECTION.                                                
084400     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
084500          DELIMITED BY SIZE INTO SSA1                                     
084600     MOVE '  GE' TO GODK-STATUSKODER                                      
084700     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH501 SSA1                    
084800     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
084900     PERFORM IMS-STATUSKONTROLL                                           
085000     .                                                                    
085100     EJECT                                                                
085201                                                                          
085300 IMS-GET-WDH5-SHT SECTION.                                                
085400     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
085500          DELIMITED BY SIZE INTO SSA1                                     
085600     MOVE '  GE' TO GODK-STATUSKODER                                      
085700     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH511 SSA1                   
085800     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
085900     PERFORM IMS-STATUSKONTROLL                                           
086000     .                                                                    
086100     EJECT                                                                
086201                                                                          
086300 IMS-GET-WDH5-NIVA SECTION.                                               
086400     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
086500          DELIMITED BY SIZE INTO SSA1                                     
086600     MOVE '  GE' TO GODK-STATUSKODER                                      
086700     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH521 SSA1                   
086800     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
086900     PERFORM IMS-STATUSKONTROLL                                           
087000     .                                                                    
087100     EJECT                                                                
087200 IMS-GET-WDH5-ALL SECTION.                                                
087300                                                                          
087400     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
087500          DELIMITED BY SIZE INTO SSA1                                     
087600     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
087700          DELIMITED BY SIZE INTO SSA2                                     
087800     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
087900          DELIMITED BY SIZE INTO SSA3                                     
088000     MOVE '  GE' TO GODK-STATUSKODER                                      
088100     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH521 SSA1 SSA2 SSA3          
088200     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
088300     PERFORM IMS-STATUSKONTROLL                                           
088400     .                                                                    
088500     EJECT                                                                
088601                                                                          
088700 IMS-GNP-WDH5      SECTION.                                               
088800     STRING 'WDH531   '                                                   
088900          DELIMITED BY SIZE INTO SSA1                                     
089000     MOVE '  GE' TO GODK-STATUSKODER                                      
089100     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
089200     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
089300     PERFORM IMS-STATUSKONTROLL                                           
089400     .                                                                    
089500     EJECT                                                                
091101                                                                          
091200 IMS-GU-WDB601    SECTION.                                                
091300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
091400          DELIMITED BY SIZE INTO SSA1                                     
091500     MOVE '  GE' TO GODK-STATUSKODER                                      
091600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
091700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
091800     PERFORM IMS-STATUSKONTROLL                                           
091900     IF SEGMENT-SAKNAS                                                    
092000         MOVE SPACE TO DCS-KDDC                                           
092100     END-IF                                                               
092200     .                                                                    
092301 IMS-GU-WDB601-TRADP SECTION.                                             
092401     STRING 'WDB601  (KDTRADP  =' W-KDTRADP ')'                           
092501            DELIMITED BY SIZE INTO SSA1                                   
092601     MOVE '  GE'                 TO GODK-STATUSKODER                      
092701     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
092801     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
092901     PERFORM IMS-STATUSKONTROLL                                           
093001     .                                                                    
093101     SKIP3                                                                
093201                                                                          
093300 IMS-STATUSKONTROLL SECTION.                                              
093400     SET STATUS-IX TO 1                                                   
093500     SEARCH GODK-STATUS                                                   
093600       AT END CALL FELLOG                                                 
093700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
093800     END-SEARCH                                                           
093900     .                                                                    
