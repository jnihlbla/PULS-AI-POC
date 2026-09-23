000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2121300.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   13/01/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPDATE AGREEMENT, ANNULATION AND NEW PURCHASER FOR CHINA         
001000*        AND USA WITH LOCAL SOURCING.                                     
001010*                                                                         
001100*        PROGRAMMET UPPDATERAR WDK7                                       
001200*        PROGRAMMET UPPDATERAR WDF102                                     
001300*        PROGRAMMET LÄSER      WDB6                                       
001310*        PROGRAMMET UPPDATERAR WDR3                                       
001400*                                                                         
001500                                                                          
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800 INPUT-OUTPUT SECTION.                                                    
001900 FILE-CONTROL.                                                            
002000                                                                          
002100*          --- GODKÄNDA POSTER FRÅN SI+                                   
002200     SELECT W21213                     ASSIGN TO W21213D1.                
002300                                                                          
002400                                                                          
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700                                                                          
002800 FD  W21213                                                               
002900     RECORDING       V                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY IS061BPA      -L.                                              
003300*01  -COPY IS061BUY      -L.                                              
003400                                                                          
003500                                                                          
003600                                                                          
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W2121300'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004110 77  WS-TIUPPDAT                 PIC S9(7) COMP-3 VALUE ZERO.             
004120 77  WS-TIUPPTID                 PIC S9(9) COMP-3 VALUE ZERO.             
004200                                                                          
004300 01  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004400 01  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004500                                                                          
004600 01  FELTEXT.                                                             
004700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004900                                                                          
004910 77  W-ANTAL-POSTER              PIC 9(7)    VALUE ZERO.                  
004920                                                                          
005000 77  W21213-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W21213                       VALUE 'J'.                   
005200                                                                          
005201 01  RKOD                        PIC S9(4)  VALUE +0  COMP SYNC.          
005210 01  WS-PLANT                    PIC X(5)    VALUE ZERO.                  
005220 01  WS-KDDC                     PIC X(2)    VALUE SPACE.                 
005300 01  W-IDAVTAL-CHAR              PIC X(12).                               
005400 01  FILLER REDEFINES W-IDAVTAL-CHAR.                                     
005500     03  W-IDAVTAL-NUM           PIC 9(12).                               
005600                                                                          
005700 01  W-TIAVTAL-CHAR              PIC X(8).                                
005800 01  FILLER REDEFINES W-TIAVTAL-CHAR.                                     
005900     03  W-TIAVTAL-NUM           PIC 9(8).                                
006000                                                                          
006010 01  WS-AA0101                   PIC 9(6).                                
006020 01  FILLER  REDEFINES WS-AA0101.                                         
006030     03  WS-AA                   PIC 9(2).                                
006040     03  WS-0101                 PIC 9(4).                                
006050                                                                          
006100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES DAGENS-DATUM.                                       
006300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006600                                                                          
006610 01  FILLER              PIC X(16)   VALUE 'IDDC-TABELL'.                 
006620*- - - - - - - - - - - - - TABELL MED ALLA IDDC PÅ WDB601                 
006650 01  IDDC-INDEX-WS.                                                       
006660     03 DC-MAX           PIC S9(3)   VALUE +100 COMP SYNC.                
006670                                                                          
006680     03 WDCIX            PIC S9(3)   VALUE +0  COMP SYNC.                 
006690     03 DCS-TRAEFF       PIC X       VALUE 'J'.                           
006691                                                                          
006692 01  IDDC-TABELL.                                                         
006693     03 DC-TAB  OCCURS 1 TO 100 DEPENDING ON DC-MAX                       
006694                INDEXED BY DCIX.                                          
006695        05 T-DCS.                                                         
006696          07 T-DCS-IDDC           PIC X(2).                               
006697          07 T-DCS-KDDC           PIC X(2).                               
006698          07 T-DCS-IDLEVNR-DC     PIC X(5).                               
006699          07 T-DCS-IDLEVNR-EMB    PIC X(5).                               
006701                                                                          
006702                                                                          
006703 01  FILLER              PIC X(16)   VALUE 'DC-TABELL-SORT'.              
006704*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
006705 01  TABENTRY-PARM.                                                       
006706     03  STEGLANGD               PIC S9(9) COMP.                          
006707     03  ANTAL                   PIC S9(9) COMP.                          
006708     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
006709                                                                          
006710 01  DYNAMISKA-SUBPROGRAM.                                                
006800*                                                                         
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
007210     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
007220     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
007300                                                                          
007400*    --- PARAMETRAR TILL POSTSUM                                          
007500*                                                                         
007600*01  -COPY W0005   -PRE  POSTSUM-                                         
007700                                                                          
007800*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
007900*                                                                         
008000 01  FILLER                      PIC X(16)   VALUE 'W005WDK7'.            
008100*01 -COPY W005WDK7                                                        
008200                                                                          
008300                                                                          
008400                                                                          
008500 01  IN-AREA-START               PIC X(24)   VALUE                        
008600                                             'IN-AREA-START'.             
008700 01  IN-AREA.                                                             
008800     03  IN-AREA-0.                                                       
008900         05  IN-IDPTYP           PIC X(3).                                
009000         05  FILLER              PIC X(400).                              
009100*   03  FILLER -COPY IS061BPA  -PRE BPA-  -RED  IN-AREA-0                 
009200*   03  FILLER -COPY IS061BUY  -PRE BUY-  -RED  IN-AREA-0                 
009300*                                                                         
009400                                                                          
009500                                                                          
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009610                                                                          
009620 01  CHKP-VAR.                                                            
009630 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
009640 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
009650 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
009660 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
009670 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
009680 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
009700                                                                          
009800 01  NYCKLAR-TILL-DLI.                                                    
009900     03  W-IDARTNR-X.                                                     
010000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010100     03  W-IDLEVNDC-X.                                                    
010200         05  W-IDLEVNDC          PIC X(5)    VALUE SPACE.                 
010300     03  W-IDDC-X.                                                        
010400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010500     03  W-IDAVTAL-X.                                                     
010600         05  W-IDAVTAL           PIC S9(13)   VALUE ZERO COMP-3.          
010700     03  W-IDBEST-X.                                                      
010800         05  W-IDBEST            PIC S9(13)   VALUE ZERO COMP-3.          
010900     03  W-WDK723KY-X.                                                    
011000         05  W-IDAVTAL-K723      PIC S9(13)   VALUE ZERO COMP-3.          
011100         05  W-IDLEVNR-K723      PIC X(5)     VALUE SPACE.                
011110     03  W-IDLEVNR-X.                                                     
011120         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
011130     03  W-IDLANDX2-X.                                                    
011140         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
011150     03  W-WDGXKEY-4579-X.                                                
011160          05 W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
011170          05 W-IDPGM             PIC X(8)    VALUE 'W2121300'.            
011180          05 FILLER              PIC X(18)   VALUE LOW-VALUE.             
011190     03  W-WDGXKEY-2253-X.                                                
011191          05 W-IDHTYP-2253       PIC X(4)    VALUE '2253'.                
011192          05 FILLER              PIC X(26)   VALUE LOW-VALUE.             
011194     03  W-WDGXKEY-2254-X.                                                
011195          05 W-IDDC-2254         PIC X(2)    VALUE SPACE.                 
011196          05 W-IDARTNR-2254      PIC S9(9)   VALUE ZERO COMP-3.           
011197                                                                          
011200                                                                          
011300                                                                          
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012000     88  IMS-EJ-OK                           VALUE 'XD'.                  
012100                                                                          
012200 01  GODK-STATUSKODER.                                                    
012300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012400                                                                          
012500 01  ALL-SSA.                                                             
012510     03 SSA1                     PIC X(64).                               
012600     03 SSA2                     PIC X(64).                               
012700     03 SSA3                     PIC X(64).                               
012800                                                                          
012900*    --- IMS FUNKTIONSKODER                                               
013000*01  -COPY W0003                                                          
013100                                                                          
013110*    --- DATA-AREA FÖR TRANSAKTION                                        
013120 01  FILLER                      PIC X(16) VALUE 'MSG-IO-AREA'.           
013130*01  -COPY WMSGAREA.                                                      
013140     EJECT                                                                
013150     05 FILLER REDEFINES MSG-MID-OUT.                                     
013160*       07  MID -COPY W2I19101   -PRE 2191-                               
013170     EJECT                                                                
013180                                                                          
013190*    --- KOMMUNIKATIONSAREA FÖR DISPATCHER                                
013191 01  FILLER                  PIC X(16) VALUE 'MSG-KOM-WMSGKOM'.           
013192*01  -COPY WMSGKOM                                                        
013193     EJECT                                                                
013194*    ---  DLI INPUT-OUTPUT AREA                                           
013195                                                                          
013200                                                                          
013300                                                                          
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500                                                                          
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
013700 01  DLI-IO-WDK701.                                                       
013800*    03  -COPY WDK701                                                     
013900                                                                          
014000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
014100 01  DLI-IO-WDK711.                                                       
014200*    03  -COPY WDK711                                                     
014300                                                                          
014400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
014500 01  DLI-IO-WDK722.                                                       
014600*    03  -COPY WDK722                                                     
014700                                                                          
014800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK723'.                      
014900 01  DLI-IO-WDK723.                                                       
015000*    03  -COPY WDK723                                                     
015100                                                                          
015200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK725'.                      
015300 01  DLI-IO-WDK725.                                                       
015400*    03  -COPY WDK725                                                     
015500                                                                          
015600                                                                          
015700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
015800 01  DLI-IO-WDB601.                                                       
015900*    03  -COPY WDB601                                                     
016000                                                                          
016100                                                                          
016200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
016300 01  DLI-IO-WDF101.                                                       
016400*    03  -COPY WDF101                                                     
016500                                                                          
016600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF102'.                      
016700 01  DLI-IO-WDF102.                                                       
016800*    03  -COPY WDF102                                                     
016810                                                                          
016820                                                                          
016830 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
016840 01  DLI-IO-WDGX4580.                                                     
016850*    03  -COPY WDGX4580                                                   
016900                                                                          
016910 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2254'.                    
016920 01  DLI-IO-WDGX2254.                                                     
016930*    03  -COPY WDGX2254                                                   
016940     EJECT                                                                
016950 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC901'.                      
016960 01  DLI-IO-WDC901.                                                       
016970*    03  -COPY WDC901                                                     
016980     EJECT                                                                
017000                                                                          
017100 LINKAGE SECTION.                                                         
017200                                                                          
017300*01  -COPY W0009  -PRE MSG-                                               
017400                                                                          
017420*01  -COPY W0009   -PRE ALT-                                              
017430     EJECT                                                                
017500*01  -COPY W0008  -PRE WDK6-                                              
017600     05  FILLER                  PIC X.                                   
017700                                                                          
017800*01  -COPY W0008  -PRE WDK7-                                              
017900     05  FILLER                  PIC X.                                   
018000                                                                          
018100*01  -COPY W0008  -PRE WDB6-                                              
018200     05  FILLER                  PIC X.                                   
018300                                                                          
018400*01  -COPY W0008  -PRE WDF1-                                              
018410     05  FILLER                  PIC X.                                   
018411                                                                          
018412*01  -COPY W0008  -PRE 4579-                                              
018413     05  FILLER                  PIC X.                                   
018420                                                                          
018430*01  -COPY W0008  -PRE 2253-                                              
018441     05  FILLER                  PIC X.                                   
018500                                                                          
018510*01  -COPY W0008  -PRE WDC9-                                              
018520     05  FILLER                  PIC X.                                   
018521*01  -COPY W0009  -PRE KOMA-                                              
018522     EJECT                                                                
018530                                                                          
018600 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDK6-PCB                       
018610                           WDK7-PCB WDB6-PCB                              
018620                           WDF1-PCB 4579-PCB                              
018630                           2253-PCB WDC9-PCB KOMA-PCB.                    
018700 MAIN SECTION.                                                            
018800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDK6-PCB                       
018900                           WDK7-PCB WDB6-PCB                              
018910                           WDF1-PCB 4579-PCB                              
018920                           2253-PCB WDC9-PCB KOMA-PCB.                    
019000                                                                          
019100     PERFORM A-INIT                                                       
019200     PERFORM S01-LAES-W21213                                              
019300     PERFORM UNTIL END-OF-W21213                                          
019400                                                                          
019500        EVALUATE IN-IDPTYP                                                
019600          WHEN 'BPA'                                                      
019700             IF BPA-ORDERDATE-END > ZERO                                  
019800                PERFORM B-UPDATE-ANNULLATION                              
019900             ELSE                                                         
020000                PERFORM C-UPDATE-AGREEMENT                                
020001                IF WS-KDDC = 'NC'                                         
020010                  PERFORM E-UPDATE-TULLFAKTOR-KINA                        
020020                ELSE                                                      
020021                  PERFORM F-UPDATE-TULLFAKTOR-USA                         
020030                END-IF                                                    
020100             END-IF                                                       
020200                                                                          
020300          WHEN 'BUY'                                                      
020400             PERFORM D-UPDATE-BUYER                                       
020500                                                                          
020600        END-EVALUATE                                                      
020700                                                                          
020710        IF CHKP-ANT > CHKP-MAX                                            
020720           PERFORM X-TAG-CHECKPOINT                                       
020730        END-IF                                                            
020800        PERFORM S01-LAES-W21213                                           
020900     END-PERFORM                                                          
021000                                                                          
021100                                                                          
021200     PERFORM Z-FINIT                                                      
021300     MOVE ZERO TO RETURN-CODE                                             
021400     GOBACK                                                               
021500     .                                                                    
021600                                                                          
021700                                                                          
021800 A-INIT SECTION.                                                          
021900     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
022000                                                                          
022100     OPEN INPUT W21213                                                    
022200                                                                          
022210     ACCEPT DAGENS-DATUM   FROM DATE                                      
022220     MOVE DAGENS-DATUM-AAR TO WS-AA                                       
022230     MOVE 0101             TO WS-0101                                     
022300                                                                          
022400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022410                                                                          
022411     ACCEPT WS-TIUPPDAT FROM DATE                                         
022412     ACCEPT WS-TIUPPTID FROM TIME                                         
022413     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
022414     MOVE +54                    TO MSG-KOM-KVLL                          
022415     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
022416     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
022417     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
022418     MOVE 'W2I19101'             TO MSG-KOM-IDCPYTXT                      
022419     MOVE 'ANSKLARM'             TO MSG-KOM-IDSNDNOD                      
022420     MOVE 'W2121300'             TO MSG-KOM-IDSNDJOB                      
022421     MOVE WS-TIUPPDAT            TO MSG-KOM-TIREGDAT                      
022422     MOVE WS-TIUPPTID            TO MSG-KOM-TIKLOCK                       
022423     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
022424                                                                          
022425     PERFORM IMS-RESTART                                                  
022430                                                                          
022440     PERFORM IMS-GHU-RESTART                                              
022450     IF 4580-KVPOST > ZERO                                                
022460        MOVE ZERO TO W-ANTAL-POSTER                                       
022470        PERFORM UNTIL W-ANTAL-POSTER = 4580-KVPOST                        
022480           PERFORM S01-LAES-W21213                                        
022490           ADD 1  TO W-ANTAL-POSTER                                       
022491        END-PERFORM                                                       
022492     END-IF                                                               
022493                                                                          
022494     PERFORM AA-SKAPA-DCTABELL                                            
022500     .                                                                    
022600                                                                          
022700                                                                          
022710 AA-SKAPA-DCTABELL SECTION.                                               
022720     MOVE 'AA-SKAPA-DCTABELL'  TO CURRENT-SECTION.                        
022730                                                                          
022740     SET DCIX TO +1                                                       
022750     PERFORM IMS-GN-WDB601                                                
022760                                                                          
022770     PERFORM UNTIL SEGMENT-SLUT                                           
022780       IF DCIX <= DC-MAX                                                  
022790         MOVE DCS-IDDC TO T-DCS-IDDC(DCIX)                                
022791         MOVE DCS-KDDC TO T-DCS-KDDC(DCIX)                                
022792         MOVE DCS-IDLEVNR-DC  TO T-DCS-IDLEVNR-DC(DCIX)                   
022793         MOVE DCS-IDLEVNR-EMB TO T-DCS-IDLEVNR-EMB(DCIX)                  
022795*                                                                         
022796         SET DCIX UP BY +1                                                
022797                                                                          
022798         PERFORM IMS-GN-WDB601                                            
022799       ELSE                                                               
022800                                                                          
022801           MOVE 'DC-TABELL SLUT. ÖKA DC-MAX' TO FELTEXT-STR               
022802           DISPLAY FELTEXT                                                
022803           CALL ABEND USING RKOD                                          
022804       END-IF                                                             
022805     END-PERFORM                                                          
022806                                                                          
022807*    --- SÄTTER TAKET PÅ TABELLEN                                         
022808     SET DCIX   DOWN BY +1                                                
022809     SET DC-MAX TO DCIX                                                   
022810                                                                          
022811*    --- SORTERA TABELLEN PÅ IDDC, FÖR ATT SEARCH SKA FUNKA               
022812     MOVE DC-MAX                  TO ANTAL                                
022813     MOVE LENGTH OF T-DCS(1)      TO STEGLANGD                            
022814     MOVE LENGTH OF T-DCS-IDDC(1) TO NYCKELLANGD                          
022815                                                                          
022816     CALL WINTSOR USING IDDC-TABELL  STEGLANGD  ANTAL                     
022817                  T-DCS-IDDC(1) NYCKELLANGD                               
022818     .                                                                    
022819     EJECT                                                                
022820 S10-FETCH-IDDC-FROM-DC-TAB SECTION.                                      
022821     MOVE 'S10-FETCH-IDDC-FROM-DC-TAB'    TO CURRENT-SECTION.             
022822                                                                          
022831     SET DCIX TO +1                                                       
022832     SEARCH DC-TAB                                                        
022833        AT END                                                            
022834           MOVE NEJ  TO DCS-TRAEFF                                        
022835        WHEN (T-DCS-IDLEVNR-DC(DCIX) = WS-PLANT) AND                      
022836             (T-DCS-KDDC(DCIX) = 'NC')                                    
022837           MOVE JA   TO DCS-TRAEFF                                        
022840                                                                          
022841        WHEN (T-DCS-IDLEVNR-EMB(DCIX) = WS-PLANT) AND                     
022842             (T-DCS-KDDC(DCIX) = 'NA')                                    
022843           MOVE JA   TO DCS-TRAEFF                                        
022846     END-SEARCH                                                           
022847                                                                          
022848     IF DCS-TRAEFF = JA                                                   
022849        MOVE T-DCS-IDDC(DCIX) TO W-IDDC                                   
022850        MOVE T-DCS-KDDC(DCIX) TO WS-KDDC                                  
022852     ELSE                                                                 
022853       DISPLAY 'IDLEVNR-DC' WS-PLANT ' EJ REG PÅ WDB6'                    
022854     END-IF                                                               
022855     .                                                                    
022856     EJECT                                                                
022860 B-UPDATE-ANNULLATION SECTION.                                            
022900     MOVE 'B-UPDATE-ANNULL ' TO CURRENT-SECTION                           
023000                                                                          
023100* DELETE AGREEMENT                                                        
023110     MOVE BPA-PLANT          TO WS-PLANT                                  
023120     PERFORM S10-FETCH-IDDC-FROM-DC-TAB                                   
023200     MOVE BPA-PARTNO         TO W-IDARTNR                                 
023700     MOVE BPA-ORDERNO        TO W-IDAVTAL-CHAR                            
023800     MOVE W-IDAVTAL-NUM      TO W-IDAVTAL                                 
023900                                                                          
024000     PERFORM IMS-GU-WDK711                                                
024100                                                                          
024200     PERFORM IMS-GHNP-WDK723                                              
024300     PERFORM UNTIL SEGMENT-SAKNAS                                         
024400                                                                          
024500        IF BPA-SUPPLIER-ID  = SAVT-IDLEVNR-AVT                            
024600           PERFORM IMS-DLET-WDK723                                        
024700        END-IF                                                            
024800                                                                          
024900        PERFORM IMS-GHNP-WDK723                                           
025000     END-PERFORM                                                          
025100                                                                          
025200     PERFORM BA-KOLLA-OM-ALLA-BORTA                                       
025300                                                                          
025400* CREATE ORDER                                                            
025500     MOVE BPA-ORDERNO        TO W-IDBEST                                  
025600     PERFORM IMS-GU-WDK725                                                
025700     IF SEGMENT-SAKNAS                                                    
025800        MOVE ALL '+'         TO WDK7-W005WDK7                             
025900        MOVE 'WDK725'        TO WDK7-IDSEGM                               
026000        MOVE W-IDARTNR       TO WDK7-IDARTNR-KFB                          
026100        MOVE W-IDDC          TO WDK7-IDDC-KFB                             
026200                                                                          
026300        MOVE BPA-ORDERNO     TO WDK7-IDBEST                               
026400        MOVE BPA-SUPPLIER-ID TO WDK7-IDLEVNR-BEST                         
026500        MOVE 5               TO WDK7-KDBEH-BEST                           
026600        MOVE DAGENS-DATUM    TO WDK7-TIBEST                               
026700                                                                          
026800        CALL W005WDK7  USING WDK7-W005WDK7 WDB6-PCB                       
026900                             WDK6-PCB WDK7-PCB                            
027000     END-IF                                                               
027100     .                                                                    
027200                                                                          
027300                                                                          
027400 BA-KOLLA-OM-ALLA-BORTA SECTION.                                          
027500     MOVE 'BA-KOLLA-ALLA-BO' TO CURRENT-SECTION                           
027600                                                                          
027700     PERFORM IMS-GU-WDK723-OKVAL                                          
027800     IF SEGMENT-SAKNAS                                                    
027900        PERFORM IMS-GHU-WDK722                                            
027910        IF SEGMENT-FINNS                                                  
028000           MOVE ZERO TO XLAG-KDAVT                                        
028100           PERFORM IMS-REPL-WDK722                                        
028110        END-IF                                                            
028200     END-IF                                                               
028300     .                                                                    
028400                                                                          
028500                                                                          
028600 C-UPDATE-AGREEMENT  SECTION.                                             
028700     MOVE 'C-UPDATE-AGREE  ' TO CURRENT-SECTION                           
028800                                                                          
028810     MOVE BPA-PLANT          TO WS-PLANT                                  
028820     PERFORM S10-FETCH-IDDC-FROM-DC-TAB                                   
028900     MOVE BPA-PARTNO            TO W-IDARTNR                              
029400     MOVE BPA-ORDERNO           TO W-IDAVTAL-CHAR                         
029500     MOVE W-IDAVTAL-NUM         TO W-IDAVTAL-K723                         
029600     MOVE BPA-SUPPLIER-ID       TO W-IDLEVNR-K723                         
029800     PERFORM IMS-GU-WDK723                                                
029900     IF SEGMENT-SAKNAS                                                    
030000        MOVE ALL '+'            TO WDK7-W005WDK7                          
030100        MOVE 'WDK723'           TO WDK7-IDSEGM                            
030200        MOVE W-IDARTNR          TO WDK7-IDARTNR-KFB                       
030300        MOVE W-IDDC             TO WDK7-IDDC-KFB                          
030400                                                                          
030500        MOVE W-IDAVTAL-K723     TO WDK7-IDAVTAL                           
030600        MOVE W-IDLEVNR-K723     TO WDK7-IDLEVNR-AVT                       
030700        MOVE BPA-SUPPLIER-SHIP  TO                                        
030800                                WDK7-IDLEVNR-SHIP IN WDK7-WDK723          
030900        MOVE BPA-ORDERDATE-FROM TO W-TIAVTAL-CHAR                         
031000        MOVE W-TIAVTAL-NUM      TO WDK7-TIAVTAL                           
031100                                                                          
031200        CALL W005WDK7  USING WDK7-W005WDK7 WDB6-PCB                       
031300                             WDK6-PCB WDK7-PCB                            
031310        PERFORM IMS-GHU-WDK722                                            
031311        IF SEGMENT-FINNS                                                  
031320           MOVE +1      TO XLAG-KDAVT                                     
031330           PERFORM IMS-REPL-WDK722                                        
031331        ELSE                                                              
031332           MOVE ALL '+'         TO WDK7-W005WDK7                          
031333           MOVE 'WDK722'        TO WDK7-IDSEGM                            
031334           MOVE W-IDARTNR       TO WDK7-IDARTNR-KFB                       
031335           MOVE W-IDDC          TO WDK7-IDDC-KFB                          
031336                                                                          
031337           MOVE +1              TO WDK7-KDAVT                             
031344           CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                     
031345                                WDK6-PCB WDK7-PCB                         
031346        END-IF                                                            
031347        PERFORM IMS-GHU-WDC901                                            
031348        IF SEGMENT-FINNS                                                  
031350          PERFORM IMS-DLET-WDC901                                         
031360        END-IF                                                            
031400     END-IF                                                               
031500                                                                          
031600*                                                                         
031610     PERFORM IMS-GU-WDK711                                                
031612     IF SEGMENT-FINNS                                                     
031620       IF SLAG-IDDC-REF = SPACE                                           
031630         PERFORM CA-UPDATE-SUPPL-CHANGE                                   
031631       ELSE                                                               
031632         MOVE '790' TO 2191-MID-KDLARM                                    
031633         PERFORM S02-SKAPA-LARM                                           
031634       END-IF                                                             
031650     END-IF                                                               
031700     .                                                                    
031701                                                                          
031710 CA-UPDATE-SUPPL-CHANGE SECTION.                                          
031720     MOVE 'CA-UPDATE-SUPPL-CHANGE ' TO CURRENT-SECTION                    
031800                                                                          
031830     MOVE W-IDDC                     TO W-IDDC-2254                       
031840     MOVE W-IDARTNR                  TO W-IDARTNR-2254                    
031850     PERFORM IMS-GHU-WDGX2254                                             
031860     IF SEGMENT-FINNS                                                     
031870        PERFORM HCA-UPD-WDG3-2254                                         
031880        PERFORM IMS-REPL-WDGX2254                                         
031890     ELSE                                                                 
031891        INITIALIZE 2254-WDGX2254                                          
031892        MOVE W-IDDC                  TO 2254-IDDC                         
031893        MOVE W-IDARTNR               TO 2254-IDARTNR                      
031894        PERFORM HCA-UPD-WDG3-2254                                         
031895        PERFORM IMS-ISRT-WDGX2254                                         
031896     END-IF                                                               
031897                                                                          
032098                                                                          
032099     .                                                                    
032100     EJECT                                                                
032110 HCA-UPD-WDG3-2254 SECTION.                                               
032120     MOVE 'HCA-UPD-WDG3-2254 '    TO CURRENT-SECTION                      
032130                                                                          
032150     MOVE BPA-SUPPLIER-ID         TO 2254-IDLEVNR-FRAM                    
032193     MOVE BPA-SUPPLIER-SHIP       TO 2254-IDLEVNR-SHIP-FRAM               
032198     MOVE BPA-ORDERDATE-FROM      TO 2254-TILEVDAT                        
032200     .                                                                    
032210     EJECT                                                                
032300                                                                          
032500                                                                          
032600 D-UPDATE-BUYER       SECTION.                                            
032700     MOVE 'D-UPDATE-BUYER  ' TO CURRENT-SECTION                           
032800                                                                          
032810     MOVE BUY-PLANT          TO WS-PLANT                                  
032820     PERFORM S10-FETCH-IDDC-FROM-DC-TAB                                   
032900     MOVE BUY-PARTNO            TO W-IDARTNR                              
033500     PERFORM IMS-GHU-WDK722                                               
033600     IF SEGMENT-SAKNAS                                                    
033700        MOVE ALL '+'            TO WDK7-W005WDK7                          
033800        MOVE 'WDK722'           TO WDK7-IDSEGM                            
033900        MOVE W-IDARTNR          TO WDK7-IDARTNR-KFB                       
034000        MOVE W-IDDC             TO WDK7-IDDC-KFB                          
034100                                                                          
034200        MOVE BUY-BUYER-CODE     TO WDK7-IDINK                             
034300                                                                          
034400        CALL W005WDK7  USING WDK7-W005WDK7 WDB6-PCB                       
034500                             WDK6-PCB WDK7-PCB                            
034510     ELSE                                                                 
034520        MOVE BUY-BUYER-CODE     TO XLAG-IDINK                             
034530        PERFORM IMS-REPL-WDK722                                           
034600     END-IF                                                               
034700     .                                                                    
034900                                                                          
035000 E-UPDATE-TULLFAKTOR-KINA SECTION.                                        
035001     MOVE 'E-UPDATE-TULLFAKTOR-KINA' TO CURRENT-SECTION                   
035002                                                                          
035003     MOVE W-IDLEVNR-K723 TO W-IDLEVNR                                     
035004     PERFORM IMS-GU-WDF101                                                
035005     IF SEGMENT-FINNS                                                     
035006       MOVE 'CN' TO W-IDLANDX2                                            
035007       PERFORM IMS-GNP-WDF102                                             
035008       IF SEGMENT-FINNS                                                   
035009         CONTINUE                                                         
035010       ELSE                                                               
035011         MOVE 171                  TO TULL-KDVALLEV                       
035012         MOVE WS-AA0101            TO TULL-TITULF                         
035013         MOVE 1.0300               TO TULL-RETULF-1                       
035014         MOVE 1.0300               TO TULL-RETULF-2                       
035015         MOVE 'CN'                 TO TULL-IDLANDX2                       
035016                                                                          
035017         PERFORM IMS-ISRT-WDF102                                          
035018       END-IF                                                             
035019     END-IF                                                               
035020     .                                                                    
035021                                                                          
035030                                                                          
035031 F-UPDATE-TULLFAKTOR-USA SECTION.                                         
035032     MOVE 'F-UPDATE-TULLFAKTOR-USA '  TO CURRENT-SECTION                  
035033*--                                                                       
035034*-- USA HAR IDLANDX2 = US OCH TULL-KDVALLEV = 121.                        
035035*-- USA HAR 1.0300 I PROCENT FRÅN START, KAN ÄNDRAS ENL. AH               
035036*-- VID NY MARKNAD MÅSTE DETTA KOLLAS UPP.                                
035037*--                                                                       
035038                                                                          
035039     MOVE W-IDLEVNR-K723 TO W-IDLEVNR                                     
035040     PERFORM IMS-GU-WDF101                                                
035041     IF SEGMENT-FINNS                                                     
035042       MOVE 'US' TO W-IDLANDX2                                            
035043       PERFORM IMS-GNP-WDF102                                             
035044       IF SEGMENT-FINNS                                                   
035045         CONTINUE                                                         
035046       ELSE                                                               
035047         MOVE 121                  TO TULL-KDVALLEV                       
035048         MOVE WS-AA0101            TO TULL-TITULF                         
035049         MOVE 1.0500               TO TULL-RETULF-1                       
035050         MOVE 1.0500               TO TULL-RETULF-2                       
035051         MOVE 'US'                 TO TULL-IDLANDX2                       
035052                                                                          
035053         PERFORM IMS-ISRT-WDF102                                          
035054       END-IF                                                             
035055     END-IF                                                               
035056     .                                                                    
035057                                                                          
035058                                                                          
035060 Z-FINIT SECTION.                                                         
035100     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
035200                                                                          
035300                                                                          
035400     CLOSE W21213                                                         
035500                                                                          
035600     MOVE 'S' TO POSTSUM-OPKOD                                            
035700     CALL POSTSUM USING POSTSUM-PARM                                      
035710                                                                          
035720     PERFORM IMS-GHU-RESTART                                              
035730     MOVE ZERO       TO 4580-KVPOST                                       
035740     ACCEPT 4580-TIUPPDAT FROM DATE                                       
035750     ACCEPT 4580-TIUPPTID FROM TIME                                       
035760     PERFORM IMS-REPL-RESTART                                             
035800     .                                                                    
035900                                                                          
036000                                                                          
036100 S01-LAES-W21213  SECTION.                                                
036200     MOVE 'S01-LAES-W21213 ' TO CURRENT-SECTION                           
036300                                                                          
036400     READ W21213 INTO IN-AREA                                             
036500     AT END                                                               
036600        MOVE HIGH-VALUE TO IN-IDPTYP                                      
036700        SET END-OF-W21213 TO TRUE                                         
036800                                                                          
036900     NOT AT END                                                           
037000        MOVE 'W21213' TO POSTSUM-FDNAMN                                   
037100        MOVE 'W21213D1' TO POSTSUM-DDNAMN2                                
037200        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
037300        CALL POSTSUM USING POSTSUM-PARM                                   
037310                                                                          
037320        ADD 1 TO W-ANTAL-POSTER                                           
037400     END-READ                                                             
037500     .                                                                    
037510                                                                          
037511 S02-SKAPA-LARM SECTION.                                                  
037512                                                                          
037513     MOVE LENGTH OF 2191-MID-W2I19101                                     
037514                                  TO   MSG-KVLL                           
037515     ADD  +17                     TO   MSG-KVLL                           
037516     MOVE   LOW-VALUE             TO   MSG-KDZ1                           
037517                                       MSG-KDZ2                           
037518     MOVE   'W2T191X'             TO   MSG-KDTRANS-1                      
037519     MOVE   '2191'                TO   MSG-IDTRANS-1                      
037520     MOVE   '1'                   TO   MSG-KDMFSFOR-1                     
037521     SKIP2                                                                
037522*    --- FLYTTA MIDDEN                                                    
037523     MOVE W-IDARTNR              TO 2191-MID-IDARTNR                      
037524     PERFORM IMS-GU-WDK722                                                
037525     IF SEGMENT-FINNS                                                     
037526       MOVE XLAG-IDANSK          TO 2191-MID-IDANSK                       
037527     ELSE                                                                 
037528       MOVE ZERO                 TO 2191-MID-IDANSK                       
037529     END-IF                                                               
037530     MOVE ZERO                   TO 2191-MID-KDCLAGER                     
037531                                    2191-MID-TISENBEK-DAG                 
037532                                    2191-MID-TISENBEK-KL                  
037533                                    2191-MID-IDDISTR                      
037534                                    2191-MID-IDKUNDNR                     
037535     MOVE SPACE                  TO 2191-MID-IDKR                         
037536                                    2191-MID-IDKUNDRF                     
037537     MOVE 'J'                    TO 2191-MID-FLNYLARM                     
037538     MOVE W-IDDC                 TO 2191-MID-IDDC                         
037539     MOVE SPACE                  TO 2191-MID-IDLEVNR                      
037540                                                                          
037541     CALL W006KOM USING MSG-PCB                                           
037542                        ALT-PCB                                           
037543                        KOMA-PCB                                          
037544                        MSG-KOM-WMSGKOM                                   
037545                        MSG-IO-AREA                                       
037546                                                                          
037547     .                                                                    
037548     EJECT                                                                
037549                                                                          
037550 X-TAG-CHECKPOINT   SECTION.                                              
037551                                                                          
037552     PERFORM IMS-GHU-RESTART                                              
037553     MOVE W-ANTAL-POSTER TO 4580-KVPOST                                   
037554     ACCEPT 4580-TIUPPDAT FROM DATE                                       
037555     ACCEPT 4580-TIUPPTID FROM TIME                                       
037556     PERFORM IMS-REPL-RESTART                                             
037557                                                                          
037558     PERFORM IMS-CHECKPOINT                                               
037560     MOVE ZERO TO CHKP-ANT                                                
037570     .                                                                    
037600                                                                          
037700                                                                          
037800* --- IMS SEKTIONER ---                                                   
037810                                                                          
037820                                                                          
037830 IMS-RESTART SECTION.                                                     
037840     MOVE 'IMS-RESTART     ' TO CURRENT-IMS-SECTION                       
037850                                                                          
037860     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
037870     MOVE '  ' TO GODK-STATUSKODER                                        
037880     CALL CBLTDLI USING XRST MSG-PCB                                      
037890                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
037891                        CHKP-AREA-LENGTH CHKP-AREA                        
037892     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037893     PERFORM IMS-STATUSKONTROLL                                           
037894     .                                                                    
037895                                                                          
037896                                                                          
037897 IMS-CHECKPOINT SECTION.                                                  
037898     MOVE 'IMS-CHECKPOINT  ' TO CURRENT-IMS-SECTION                       
037899                                                                          
037900     MOVE SPACE  TO CHKP-MSG-IO-AREA                                      
037901     MOVE '  XD' TO GODK-STATUSKODER                                      
037902     CALL CBLTDLI USING CHKP MSG-PCB                                      
037903                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
037904                        CHKP-AREA-LENGTH CHKP-AREA                        
037905     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037906     PERFORM IMS-STATUSKONTROLL                                           
037907                                                                          
037908     IF IMS-EJ-OK                                                         
037909       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
037910       DISPLAY FELTEXT                                                    
037911       CALL FELLOG                                                        
037912     END-IF                                                               
037913     .                                                                    
037920                                                                          
037930                                                                          
038000 IMS-GU-WDK701 SECTION.                                                   
038100     MOVE 'IMS-GU-WDK701   ' TO CURRENT-IMS-SECTION                       
038200                                                                          
038210     MOVE SPACE               TO ALL-SSA                                  
038300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
038400          DELIMITED BY SIZE INTO SSA1                                     
038500     MOVE '  GE'              TO GODK-STATUSKODER                         
038600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
038700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
038800     PERFORM IMS-STATUSKONTROLL                                           
038900     .                                                                    
039000                                                                          
039100                                                                          
039200 IMS-GU-WDK711 SECTION.                                                   
039300     MOVE 'IMS-GU-WDK711   ' TO CURRENT-IMS-SECTION                       
039400                                                                          
039410     MOVE SPACE               TO ALL-SSA                                  
039500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
039600          DELIMITED BY SIZE INTO SSA1                                     
039700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
039800          DELIMITED BY SIZE INTO SSA2                                     
039900     MOVE '  GE'              TO GODK-STATUSKODER                         
040000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
040100     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
040200     PERFORM IMS-STATUSKONTROLL                                           
040300     .                                                                    
040400                                                                          
040500                                                                          
042100 IMS-GHU-WDK722 SECTION.                                                  
042200     MOVE 'IMS-GHU-WDK722  ' TO CURRENT-IMS-SECTION                       
042300                                                                          
042310     MOVE SPACE               TO ALL-SSA                                  
042400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
042500          DELIMITED BY SIZE INTO SSA1                                     
042600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
042700          DELIMITED BY SIZE INTO SSA2                                     
042800     MOVE 'WDK722 '           TO SSA3                                     
042900     MOVE '  GE'              TO GODK-STATUSKODER                         
043000     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
043100     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
043200     PERFORM IMS-STATUSKONTROLL                                           
043300     .                                                                    
043400                                                                          
043410 IMS-GU-WDK722 SECTION.                                                   
043420     MOVE 'IMS-GU-WDK722    ' TO CURRENT-IMS-SECTION                      
043430                                                                          
043440     MOVE SPACE               TO ALL-SSA                                  
043450     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
043460          DELIMITED BY SIZE INTO SSA1                                     
043470     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
043480          DELIMITED BY SIZE INTO SSA2                                     
043490     MOVE 'WDK722 '           TO SSA3                                     
043491     MOVE '  GE'              TO GODK-STATUSKODER                         
043492     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
043493     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
043494     PERFORM IMS-STATUSKONTROLL                                           
043495     .                                                                    
043496                                                                          
043500                                                                          
043600 IMS-REPL-WDK722 SECTION.                                                 
043700     MOVE 'IMS-REPL-WDK722 ' TO CURRENT-IMS-SECTION                       
043800                                                                          
043810     MOVE SPACE               TO ALL-SSA                                  
043900     MOVE '  '             TO GODK-STATUSKODER                            
044000     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
044100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
044200     PERFORM IMS-STATUSKONTROLL                                           
044210     ADD +1 TO CHKP-ANT                                                   
044300     .                                                                    
044400                                                                          
044500                                                                          
044600 IMS-GU-WDK723 SECTION.                                                   
044700     MOVE 'IMS-GU-WDK723   ' TO CURRENT-IMS-SECTION                       
044800                                                                          
044810     MOVE SPACE               TO ALL-SSA                                  
044900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
045000          DELIMITED BY SIZE INTO SSA1                                     
045100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
045200          DELIMITED BY SIZE INTO SSA2                                     
045300     STRING 'WDK723  (WDK723KY =' W-WDK723KY-X ')'                        
045400          DELIMITED BY SIZE INTO SSA3                                     
045500     MOVE '  GE'              TO GODK-STATUSKODER                         
045600     CALL CBLTDLI USING GU   WDK7-PCB DLI-IO-WDK723 SSA1 SSA2 SSA3        
045700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
045800     PERFORM IMS-STATUSKONTROLL                                           
045900     .                                                                    
046000                                                                          
046100                                                                          
046200 IMS-GU-WDK723-OKVAL SECTION.                                             
046300     MOVE 'IMS-GU-WDK723-O ' TO CURRENT-IMS-SECTION                       
046400                                                                          
046410     MOVE SPACE               TO ALL-SSA                                  
046500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
046600          DELIMITED BY SIZE INTO SSA1                                     
046700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
046800          DELIMITED BY SIZE INTO SSA2                                     
046900     MOVE   'WDK723 '         TO SSA3                                     
047000     MOVE '  GE'              TO GODK-STATUSKODER                         
047100     CALL CBLTDLI USING GU   WDK7-PCB DLI-IO-WDK723 SSA1 SSA2 SSA3        
047200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
047300     PERFORM IMS-STATUSKONTROLL                                           
047400     .                                                                    
047500                                                                          
047600                                                                          
047700 IMS-GHNP-WDK723 SECTION.                                                 
047800     MOVE 'IMS-GHNP-WDK723 ' TO CURRENT-IMS-SECTION                       
047900                                                                          
047910     MOVE SPACE               TO ALL-SSA                                  
048000     MOVE 'WDK723 '           TO SSA1                                     
048100     MOVE '  GE'              TO GODK-STATUSKODER                         
048200     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK723 SSA1                  
048300     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
048400     PERFORM IMS-STATUSKONTROLL                                           
048500     .                                                                    
048600                                                                          
048700                                                                          
048800 IMS-DLET-WDK723 SECTION.                                                 
048900     MOVE 'IMS-DLET-WDK723 ' TO CURRENT-IMS-SECTION                       
049000                                                                          
049010     MOVE SPACE               TO ALL-SSA                                  
049100     MOVE '  '             TO GODK-STATUSKODER                            
049200     CALL CBLTDLI USING DLET WDK7-PCB DLI-IO-WDK723                       
049300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
049400     PERFORM IMS-STATUSKONTROLL                                           
049410     ADD +1 TO CHKP-ANT                                                   
049500     .                                                                    
049600                                                                          
049700                                                                          
049800 IMS-GU-WDK725 SECTION.                                                   
049900     MOVE 'IMS-GU-WDK725   ' TO CURRENT-IMS-SECTION                       
050000                                                                          
050010     MOVE SPACE               TO ALL-SSA                                  
050100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
050200          DELIMITED BY SIZE INTO SSA1                                     
050300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
050400          DELIMITED BY SIZE INTO SSA2                                     
050500     STRING 'WDK725  (IDBEST   =' W-IDBEST-X ')'                          
050600          DELIMITED BY SIZE INTO SSA3                                     
050700     MOVE '  GE'              TO GODK-STATUSKODER                         
050800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK725 SSA1 SSA2 SSA3          
050900     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
051000     PERFORM IMS-STATUSKONTROLL                                           
051100     .                                                                    
051200                                                                          
051300                                                                          
052400                                                                          
052401 IMS-GN-WDB601    SECTION.                                                
052402     STRING 'WDB601   '                                                   
052403          DELIMITED BY SIZE INTO SSA1                                     
052404     MOVE '  GB' TO GODK-STATUSKODER                                      
052405     CALL CBLTDLI USING GN WDB6-PCB  DLI-IO-WDB601 SSA1                   
052406     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
052407     PERFORM IMS-STATUSKONTROLL                                           
052408     .                                                                    
052409     EJECT                                                                
052410                                                                          
052411 IMS-GU-WDF101 SECTION.                                                   
052412     MOVE 'IMS-GU-WDF101   ' TO CURRENT-IMS-SECTION                       
052420                                                                          
052421     MOVE SPACE               TO ALL-SSA                                  
052430     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
052440          DELIMITED BY SIZE INTO SSA1                                     
052450     MOVE '  GE' TO GODK-STATUSKODER                                      
052460     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
052470     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
052480     PERFORM IMS-STATUSKONTROLL                                           
052490     .                                                                    
052491     SKIP3                                                                
052500                                                                          
052501 IMS-GNP-WDF102 SECTION.                                                  
052502     MOVE 'IMS-GNP-WDF102  ' TO CURRENT-IMS-SECTION                       
052503                                                                          
052504     MOVE SPACE               TO ALL-SSA                                  
052505     STRING 'WDF102  (IDLAND   =' W-IDLANDX2-X ')'                        
052506          DELIMITED BY SIZE INTO SSA1                                     
052507     MOVE '  GE' TO GODK-STATUSKODER                                      
052508     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF102 SSA1                   
052509     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
052510     PERFORM IMS-STATUSKONTROLL                                           
052511     .                                                                    
052512     SKIP3                                                                
052513                                                                          
052514 IMS-ISRT-WDF102 SECTION.                                                 
052515     MOVE 'IMS-ISRT-WDF102 ' TO CURRENT-IMS-SECTION                       
052520                                                                          
052521     MOVE SPACE               TO ALL-SSA                                  
052530     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
052540          DELIMITED BY SIZE INTO SSA1                                     
052550     MOVE 'WDF102 '           TO SSA2                                     
052560     MOVE '  II' TO GODK-STATUSKODER                                      
052570     CALL CBLTDLI USING ISRT WDF1-PCB DLI-IO-WDF102 SSA1 SSA2             
052580     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
052590     PERFORM IMS-STATUSKONTROLL                                           
052591     ADD +1 TO CHKP-ANT                                                   
052592     .                                                                    
052593                                                                          
052596 IMS-GHU-WDGX2254 SECTION.                                                
052597     MOVE 'IMS-GHU-WDGX2254 '   TO CURRENT-IMS-SECTION                    
052598                                                                          
052599     MOVE SPACES                TO SSA1 SSA2                              
052600     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2253-X ')'                    
052601          DELIMITED BY SIZE INTO SSA1                                     
052602     STRING 'WDGX2254(KY2254   =' W-WDGXKEY-2254-X ')'                    
052603          DELIMITED BY SIZE INTO SSA2                                     
052604     MOVE '  GE' TO GODK-STATUSKODER                                      
052605     CALL CBLTDLI USING GHU 2253-PCB DLI-IO-WDGX2254 SSA1 SSA2            
052606     MOVE 2253-STATUS-CODE TO STATUS-WS                                   
052607     PERFORM IMS-STATUSKONTROLL                                           
052608     .                                                                    
052609     SKIP2                                                                
052610 IMS-REPL-WDGX2254 SECTION.                                               
052611     MOVE 'IMS-REPL-WDGX2254 '   TO CURRENT-IMS-SECTION                   
052612                                                                          
052613     MOVE '  ' TO GODK-STATUSKODER                                        
052614     CALL CBLTDLI USING REPL 2253-PCB DLI-IO-WDGX2254                     
052615     MOVE 2253-STATUS-CODE TO STATUS-WS                                   
052616     PERFORM IMS-STATUSKONTROLL                                           
052617     ADD +1 TO CHKP-ANT                                                   
052618     .                                                                    
052619     EJECT                                                                
052620 IMS-ISRT-WDGX2254 SECTION.                                               
052621     MOVE 'IMS-ISRT-WDGX2254 '  TO CURRENT-IMS-SECTION                    
052622                                                                          
052623     MOVE SPACES              TO SSA1                                     
052624                                 SSA2                                     
052625     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2253-X ')'                    
052626          DELIMITED BY SIZE INTO SSA1                                     
052627     MOVE   'WDGX2254'        TO SSA2                                     
052628     MOVE '  II'              TO GODK-STATUSKODER                         
052629     CALL CBLTDLI USING ISRT 2253-PCB DLI-IO-WDGX2254 SSA1 SSA2           
052630     MOVE 2253-STATUS-CODE    TO STATUS-WS                                
052631     PERFORM IMS-STATUSKONTROLL                                           
052632     ADD +1 TO CHKP-ANT                                                   
052633     .                                                                    
052634     EJECT                                                                
052635 IMS-GHU-WDC901 SECTION.                                                  
052636                                                                          
052637     MOVE SPACES    TO SSA1                                               
052638     STRING 'WDC901  (IDDC     =' W-IDDC-X                                
052639                    '&IDARTNR  =' W-IDARTNR-X ')'                         
052640             DELIMITED BY SIZE INTO SSA1                                  
052641     MOVE '  GE'  TO GODK-STATUSKODER                                     
052642     CALL CBLTDLI USING GHU WDC9-PCB DLI-IO-WDC901 SSA1                   
052643     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
052644     PERFORM IMS-STATUSKONTROLL                                           
052645     .                                                                    
052646     EJECT                                                                
052654 IMS-DLET-WDC901 SECTION.                                                 
052655                                                                          
052656     MOVE 'WDC901  '   TO SSA1                                            
052657     MOVE '  '  TO GODK-STATUSKODER                                       
052658     CALL CBLTDLI USING DLET WDC9-PCB DLI-IO-WDC901 SSA1                  
052659     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
052660     PERFORM IMS-STATUSKONTROLL                                           
052661     ADD +1 TO CHKP-ANT                                                   
052662     .                                                                    
052663     EJECT                                                                
052664 IMS-GHU-RESTART  SECTION.                                                
052665     MOVE 'IMS-GHU-RESTART '  TO CURRENT-IMS-SECTION                      
052666                                                                          
052667     MOVE SPACE          TO ALL-SSA                                       
052668     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
052669          DELIMITED BY SIZE INTO SSA1                                     
052670     MOVE 'WDR470   '    TO SSA2                                          
052671     MOVE '    '         TO GODK-STATUSKODER                              
052672     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
052673     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
052674     PERFORM IMS-STATUSKONTROLL                                           
052675     .                                                                    
052676                                                                          
052677                                                                          
052678 IMS-REPL-RESTART SECTION.                                                
052679     MOVE 'IMS-REPL-RESTART'  TO CURRENT-IMS-SECTION                      
052680                                                                          
052681     MOVE '  '             TO GODK-STATUSKODER                            
052682     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
052683     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
052684     PERFORM IMS-STATUSKONTROLL                                           
052685     .                                                                    
052686                                                                          
052690 IMS-STATUSKONTROLL SECTION.                                              
052700                                                                          
052800     SET STATUS-IX TO 1                                                   
052900     SEARCH GODK-STATUS                                                   
053000       AT END                                                             
053100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
053200           DELIMITED BY SIZE INTO FELTEXT                                 
053300         DISPLAY FELTEXT                                                  
053400         CALL FELLOG                                                      
053500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
053600         CONTINUE                                                         
053700     END-SEARCH                                                           
053800     .                                                                    
