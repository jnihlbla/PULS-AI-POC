000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4403800.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   97/08/21.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*        PROGRAMMET GÄLLER NDC:ERNA I USA OCH KINA                        
001000*        LÄSER FIL MED ARTIKLAR SOM HAR KVROS-DAG > 0.                    
001100*        FROM SPRING 2021 EVEN KVROS-BULK > 0                             
001200*        FÖRSÖKER ATT TÄCKA KVROS FRÅN ETT ANNAT NDC-LAGER.               
001300*        OM MAN FÅR FULL TÄCKNING SÅ SKAPAS EN TÄCKNINGSTRANS.            
001400*                                                                         
001500*        PROGRAMMET ANROPAR W218ETA FÖR HÄMTNING                          
001600*        AV ETA-DATUM/NDC-LAGER. ENDAST FÖR RO-RADENS DC.                 
001700*        (ESTIMATED TIME AVAILABLE)                                       
001800*                                                                         
001900*        PROGRAMMET UPPDATERAR WLORDP (WDA5)                              
002000*                              WDK7                                       
002100*        HTR 4505 WL4505/WDR4 UPPDATERAS. (RO-TÄCKNINGS-HTR).             
002200*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800*    040830  SM  FLYTT AV DEAL-PR-LINE GRUPPEN                            
002900*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
003000*    CHANGE LOG:                                                          
003100*      YY/MM/DD - INITIALS        - DESCRIPTION.                          
003200*                                                                         
003300*      14/03/19 - REDDY RAHUL     - ETRACKER 10202052                     
003400*                                   INCLUDE AK IN THE CALCULATION         
003500*                                   FOR AVAILABILITY BEFORE WE            
003600*                                   SEND THE BACKORDER TO THE NEXT        
003700*                                   WAREHOUSE.                            
003800*                                                                         
003900*      SPRING 2021 - G KJELLSON   - KVROS-BULK ADDED TO INPUT FILE        
004000*                                                                         
004100*    ETRACKER 10254592  2015  DECOMISSION VOHF                            
004200*    STORY 2217565: RECOMPILING FOR WWDCLAND & WWDC99                     
004300                                                                          
004400     SKIP3                                                                
004500 ENVIRONMENT DIVISION.                                                    
004600     SKIP2                                                                
004700 INPUT-OUTPUT SECTION.                                                    
004800                                                                          
004900 FILE-CONTROL.                                                            
005000     SKIP2                                                                
005100*          --- FIL MED ARTIKLAR SOM HAR KVROS > 0                         
005200*          --- KVROS-DAG OR KVROS-BULK                                    
005300     SELECT W44035                     ASSIGN TO W44038D1.                
005400     EJECT                                                                
005500 DATA DIVISION.                                                           
005600     SKIP3                                                                
005700 FILE SECTION.                                                            
005800     SKIP3                                                                
005900 FD  W44035                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  -COPY W44035      -L.                                                
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600     SKIP2                                                                
006700                                                                          
006800*    -- CHECKED BY WY2000                                                 
006900 77  IDPGM                       PIC X(8)    VALUE 'W4403800'.            
007000 77  JA                          PIC X       VALUE 'J'.                   
007100 77  YES                         PIC X       VALUE 'Y'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
007300 77  WS-DISP                     PIC S9(7)   VALUE ZERO COMP-3.           
007400 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
007500 77  W-DISPONIBELT               PIC S9(7)   VALUE ZERO COMP-3.           
007600 77  WS-DISP-HOMEDC-CN           PIC S9(7)   VALUE ZERO COMP-3.           
007700 77  W-ANTAL-ARTIKLAR            PIC S9(7)   VALUE ZERO COMP-3.           
007800 77  W-KVART                     PIC S9(7)   VALUE ZERO COMP-3.           
007900 77  K-KDTAKORS                  PIC S9(3)   VALUE +21  COMP-3.           
008000 77  K-IDKUNDRF7-NOLL            PIC X(10)   VALUE '0000000   '.          
008100 77  WS-SVAR-ETA-DATUM           PIC 9(6).                                
008200*77  NAESTA-IDDC                 PIC X(2)    VALUE SPACE.                 
008300 77  CHKP-ID                     PIC X(08)  VALUE 'W4403800'.             
008400 77  CHKP-ANT                    PIC S9(3)  COMP-3 VALUE +0.              
008500 77  CHKP-MAX                    PIC S9(3)  COMP-3 VALUE 200.             
008600 77  MSG-IO-AREA-LENGTH          PIC S9(9)  VALUE +32 COMP SYNC.          
008700 77  MSG-IO-AREA                 PIC X(32)  VALUE SPACE.                  
008800 77  CHKP-AREA-1-LENGTH          PIC S9(9)  VALUE +32 COMP SYNC.          
008900 77  CHKP-AREA-1                 PIC X(32)  VALUE SPACE.                  
009000                                                                          
009100 77  W-IDLAND                    PIC X(2)   VALUE SPACE.                  
009200                                                                          
009300*01  -COPY WWDCKONS                                                       
009400*01  -COPY WWDCLAND                                                       
009500*01  -COPY WWDC99   -PRE IN-                                              
009700                                                                          
009800                                                                          
009900 01  WS-HELP-IX                  PIC 9(3)   VALUE ZERO.                   
010000 01  WS-HELP-IX-MAX              PIC 9(3)   VALUE 99.                     
010100 01  WS-IDDC-IX                  PIC 9(3)   VALUE ZERO.                   
010210 01  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
010300 01  WS-SPAR-IDDC-CLEAR.                                                  
010400     03 WS-IDDC-CLEAR            PIC X(3)   OCCURS 99.                    
010500 01  NAESTA-DC-IX                PIC 9(3)   VALUE ZERO.                   
010600 01  NAESTA-DC-IX-MAX            PIC 9(3)   VALUE 99.                     
010700 01  WS-NAESTA-IDDC-GRP.                                                  
010800     03 WS-NAESTA-IDDC           PIC X(3)   OCCURS 99.                    
010900                                                                          
011000 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
011100 01  FILLER REDEFINES WS-TIHHMMSS.                                        
011200     03 WS-TIHHMM                PIC 9(4).                                
011300     03 FILLER                   PIC 9(2).                                
011400                                                                          
011500 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
011600 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
011700     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
011800     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
011900     SKIP3                                                                
012000*                                                                         
012100*                                                                         
012200 01  W-SALDO-WDK7.                                                        
012300     03  W-KVLS              PIC S9(7) COMP-3.                            
012400     03  W-KVOKS-DAG         PIC S9(7) COMP-3.                            
012500     03  W-KVOKS-BULK        PIC S9(7) COMP-3.                            
012600     03  W-KVRESS            PIC S9(7) COMP-3.                            
012700     03  W-KVROS-DAG         PIC S9(7) COMP-3.                            
012800     03  W-KVROS-BULK        PIC S9(7) COMP-3.                            
012900     03  W-KVSPARR-KVAL      PIC S9(7) COMP-3.                            
013000     03  W-KVUTRS            PIC S9(7) COMP-3.                            
013100     03  W-KVSPANT           PIC S9(7) COMP-3.                            
013200     03  W-KVAKS-SDC         PIC S9(7) COMP-3.                            
013300*                                                                         
013400 01  W-SUM-SALDO-WDK7.                                                    
013500     03  W-SUM-KVROS-DAG         PIC S9(9) COMP-3 VALUE ZERO.             
013600     03  W-SUM-KVROS-BULK        PIC S9(9) COMP-3 VALUE ZERO.             
013700     03  W-SUM-HOME-KVROS-DAG    PIC S9(9) COMP-3 VALUE ZERO.             
013800     03  W-SUM-HOME-KVROS-BULK   PIC S9(9) COMP-3 VALUE ZERO.             
013900     03  W-SUM-KVRESS            PIC S9(9) COMP-3 VALUE ZERO.             
014000     EJECT                                                                
014100                                                                          
014200 01  FELTEXT.                                                             
014300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014500                                                                          
014600 77  DC-TAECK41-SW               PIC X       VALUE 'N'.                   
014700     88  DC-TAECK41                          VALUE 'J'.                   
014800     88  DC-TAECK41-EJ                       VALUE 'N'.                   
014900                                                                          
015000 77  DC-TAECK43-SW               PIC X       VALUE 'N'.                   
015100     88  DC-TAECK43                          VALUE 'J'.                   
015200     88  DC-TAECK43-EJ                       VALUE 'N'.                   
015300                                                                          
015400 77  DC-TAECK44-SW               PIC X       VALUE 'N'.                   
015500     88  DC-TAECK44                          VALUE 'J'.                   
015600     88  DC-TAECK44-EJ                       VALUE 'N'.                   
015700                                                                          
015800 77  DC-TAECK45-SW               PIC X       VALUE 'N'.                   
015900     88  DC-TAECK45                          VALUE 'J'.                   
016000     88  DC-TAECK45-EJ                       VALUE 'N'.                   
016100                                                                          
016200 77  DC-TAECK46-SW               PIC X       VALUE 'N'.                   
016300     88  DC-TAECK46                          VALUE 'J'.                   
016400     88  DC-TAECK46-EJ                       VALUE 'N'.                   
016500                                                                          
016501 77  DC-TAECK47-SW               PIC X       VALUE 'N'.                   
016502     88  DC-TAECK47                          VALUE 'J'.                   
016503     88  DC-TAECK47-EJ                       VALUE 'N'.                   
016504                                                                          
016505 77  DC-TAECK51-SW               PIC X       VALUE 'N'.                   
016506     88  DC-TAECK51                          VALUE 'J'.                   
016507     88  DC-TAECK51-EJ                       VALUE 'N'.                   
016508                                                                          
016509 77  DC-TAECK71-SW               PIC X       VALUE 'N'.                   
016510     88  DC-TAECK71                          VALUE 'J'.                   
016520     88  DC-TAECK71-EJ                       VALUE 'N'.                   
016530                                                                          
016540 77  DC-TAECK72-SW               PIC X       VALUE 'N'.                   
016550     88  DC-TAECK72                          VALUE 'J'.                   
016560     88  DC-TAECK72-EJ                       VALUE 'N'.                   
016570                                                                          
016580 77  DC-TAECK73-SW               PIC X       VALUE 'N'.                   
016590     88  DC-TAECK73                          VALUE 'J'.                   
016600     88  DC-TAECK73-EJ                       VALUE 'N'.                   
016700                                                                          
016800 77  DC-TAECK74-SW               PIC X       VALUE 'N'.                   
016900     88  DC-TAECK74                          VALUE 'J'.                   
017000     88  DC-TAECK74-EJ                       VALUE 'N'.                   
017100                                                                          
017200 77  DC-TAECK6A-SW               PIC X       VALUE 'N'.                   
017300     88  DC-TAECK6A                          VALUE 'J'.                   
017400     88  DC-TAECK6A-EJ                       VALUE 'N'.                   
017500                                                                          
017600 77  DC-TAECK61-SW               PIC X       VALUE 'N'.                   
017700     88  DC-TAECK61                          VALUE 'J'.                   
017800     88  DC-TAECK61-EJ                       VALUE 'N'.                   
017900                                                                          
018000 77  SALDO-SW                    PIC X       VALUE 'N'.                   
018100     88  SALDO-FINNS                         VALUE 'J'.                   
018200     88  SALDO-FINNS-EJ                      VALUE 'N'.                   
018300                                                                          
018400 77  DISPONIBELT-SW              PIC X       VALUE 'N'.                   
018500     88  DISPONIBELT-FINNS                   VALUE 'J'.                   
018600     88  DISPONIBELT-SAKNAS                  VALUE 'N'.                   
018700                                                                          
018800 77  ETA-SW                      PIC X       VALUE 'N'.                   
018900     88  ETA-PAA-VAEG                        VALUE 'J'.                   
019000     88  ETA-EJ-PAA-VAEG                     VALUE 'N'.                   
019100                                                                          
019200 77  TAECKNING-CLEAR-SW          PIC X       VALUE 'N'.                   
019300     88  FULL-TAECKNING                      VALUE 'J'.                   
019400     88  EJ-FULL-TAECKNING                   VALUE 'N'.                   
019500                                                                          
019600 77  BACKORDER-SW                PIC X       VALUE 'J'.                   
019700     88  BACKORDER-YES                       VALUE 'J'.                   
019800     88  BACKORDER-NO                        VALUE 'N'.                   
019900                                                                          
020000 77  DISP-HOMEDC-CN-SW           PIC X       VALUE 'N'.                   
020100     88  DISP-CN-FOUND                       VALUE 'J'.                   
020200     88  DISP-CN-NOTFOUND                    VALUE 'N'.                   
020300                                                                          
020400 77  FETCH-CN-BAL-SW             PIC X       VALUE 'J'.                   
020500     88  FETCH-CN-BAL-YES                    VALUE 'J'.                   
020600     88  FETCH-CN-BAL-NO                     VALUE 'N'.                   
020700                                                                          
020800 77  W44035-EOF-SW               PIC X       VALUE 'N'.                   
020900     88  END-OF-W44035                       VALUE 'J'.                   
021000     EJECT                                                                
021100                                                                          
021200*  --- FÖR ATT FÅ RÄTT SEKEL VID ANROP TILL W218ETA                       
021300 01  WS-ETA-DATUM                PIC 9(6).                                
021400 01  FILLER REDEFINES WS-ETA-DATUM.                                       
021500     03  WS-ETA-DATUM-AAR        PIC 9(2).                                
021600     03  WS-ETA-DATUM-MAANAD     PIC 9(2).                                
021700     03  WS-ETA-DATUM-DAG        PIC 9(2).                                
021800     EJECT                                                                
021900                                                                          
022000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
022100 01  FILLER REDEFINES DAGENS-DATUM.                                       
022200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
022300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
022400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
022500     EJECT                                                                
022600                                                                          
022700 01  DYNAMISKA-SUBPROGRAM.                                                
022800*                                                                         
022900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
023000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
023200     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
023300     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
023400     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
023500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
023600     EJECT                                                                
023700                                                                          
023800*    --- PARAMETRAR TILL ABEND                                            
023900 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
024000 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
024100     EJECT                                                                
024200*    --- PARAMETRAR TILL POSTSUM                                          
024300*                                                                         
024400*01  -COPY W0005   -PRE  POSTSUM-                                         
024500     EJECT                                                                
024600 01 FILLER               PIC X(8)  VALUE 'LETA'.                          
024700*   -COPY W218LETA -PRE ETA-.                                             
024800     EJECT                                                                
024900*    --- PARAMETRAR TILL WDAGAREA                                         
025000 01  FILLER                      PIC X(16) VALUE 'WDAGAREA'.              
025100*01  -COPY WDAGAREA                                                       
025200     EJECT                                                                
025300*    --- PARAMETRAR TILL W005INIT                                         
025400 01  FILLER                      PIC X(16) VALUE 'WMSGINIT'.              
025500*01  -COPY WMSGINIT                                                       
025600     EJECT                                                                
025700 01  IN-AREA-START               PIC X(24)   VALUE                        
025800                                             'IN-AREA-START'.             
025900                                                                          
026000                                                                          
026100*01  AREA -COPY W44035     -PRE IN-                                       
026200*                                                                         
026300     EJECT                                                                
026400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026500     SKIP3                                                                
026600 01  NYCKLAR-TILL-DLI.                                                    
026700                                                                          
026800     03  W-IDARTNR-K7-X.                                                  
026900         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
027000                                                                          
027100     03  W-IDARTNR-K6-X.                                                  
027200         05  W-IDARTNR-K6        PIC S9(9)   VALUE ZERO COMP-3.           
027300                                                                          
027400     03  W-IDDC-K7-X.                                                     
027500         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
027600                                                                          
027700     03  W-IDLAND-K7-X.                                                   
027800         05  W-IDLAND-K7         PIC X(2)    VALUE SPACE.                 
027900                                                                          
028000     03  W-KDSTARAD-X.                                                    
028100         05  W-KDSTARAD          PIC X        VALUE SPACE.                
028200                                                                          
028300     03  W-WDQ2CSEQ-X.                                                    
028400         05  W-IDDISTR           PIC S9(5)   VALUE +0 COMP-3.             
028500         05  W-IDKUNDNR          PIC S9(7)   VALUE +0 COMP-3.             
028600         05  W-IDKUNDRF.                                                  
028700           07  W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
028800           07  FILLER            PIC X(3)    VALUE SPACE.                 
028900                                                                          
029000     03 W1-WDA5ASEQ-X.                                                    
029100         05 W1-IDARTNR       PIC S9(09) COMP-3 VALUE ZERO.                
029200         05 W1-IDDC          PIC X(02).                                   
029300         05 W1-KDRAPRIO      PIC S9(03) COMP-3 VALUE ZERO.                
029400                                                                          
029500     03 W2-WDA5ASEQ-X.                                                    
029600         05 W2-IDARTNR       PIC S9(09) COMP-3 VALUE ZERO.                
029700         05 W2-IDDC          PIC X(02).                                   
029800         05 W2-KDRAPRIO      PIC S9(03) COMP-3 VALUE ZERO.                
029900                                                                          
030000     03 W-IDHTYP-4505-X.                                                  
030100         05 FILLER              PIC X(4)    VALUE '4505'.                 
030200         05 IDDC-4505           PIC X(2)    VALUE '51'.                   
030300         05 FILLER              PIC X(24)   VALUE LOW-VALUE.              
030400                                                                          
030500     03  W-WDQ101KY-MIN-X.                                                
030600         05  W-IDORDER-Q1-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
030700         05  W-IDARTNR-Q1-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
030800         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
030900         05  W-IDSEQVNR-Q1-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
031000         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
031100                                                                          
031200     03  W-WDQ101KY-MAX-X.                                                
031300         05  W-IDORDER-Q1-MAX    PIC S9(7)   VALUE ZERO COMP-3.           
031400         05  W-IDARTNR-Q1-MAX    PIC S9(9)   VALUE ZERO COMP-3.           
031500         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   VALUE ZERO COMP-3.           
031600         05  W-IDSEQVNR-Q1-MAX   PIC S9(3)   VALUE ZERO COMP-3.           
031700         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
031800                                                                          
031810     03  W-IDGMT-X.                                                       
031820         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
031830         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
031840                                                                          
031900     03  W-IDDC-X.                                                        
032000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
032100     SKIP2                                                                
032200*    --- STATUS-KOD FRÅN IMS                                              
032300 01  STATUS-WS                   PIC XX.                                  
032400     88  SEGMENT-FINNS                       VALUE '  '.                  
032500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
032800     88  IMS-EJ-OK                           VALUE 'XD'.                  
032900     SKIP2                                                                
033000 01  GODK-STATUSKODER.                                                    
033100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033200     SKIP3                                                                
033300 01  SSA1                        PIC X(96).                               
033400 01  SSA2                        PIC X(64).                               
033500 01  SSA3                        PIC X(64).                               
033600     EJECT                                                                
033700*    --- IMS FUNKTIONSKODER                                               
033800*01  -COPY W0003                                                          
033900     EJECT                                                                
034000*    ---  DLI INPUT-OUTPUT AREA                                           
034100                                                                          
034200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
034300 01  DLI-IO-WLARTC01.                                                     
034400*    03  -COPY WDK601  -PRE ARTC-                                         
034500     EJECT                                                                
034600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
034700 01  DLI-IO-WLARTC11.                                                     
034800*    03  -COPY WDK611  -PRE ARTC-                                         
034900     EJECT                                                                
035000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
035100 01  DLI-IO-WDK711.                                                       
035200*    03  -COPY WDK711  -PRE WDK7-                                         
035300     EJECT                                                                
035400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
035500 01  DLI-IO-WDK722.                                                       
035600*    03  -COPY WDK722  -PRE WDK7-                                         
035700     EJECT                                                                
035800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
035900 01  DLI-IO-WDK712.                                                       
036000*    03  -COPY WDK712  -PRE WDK7-                                         
036100     EJECT                                                                
036200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLORDP01'.                    
036300 01  DLI-IO-WLORDP01.                                                     
036400*    03  -COPY WDA501  -PRE ORDP-                                         
036500     EJECT                                                                
036600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLORQI01'.                    
036700 01  DLI-IO-WLORQI01.                                                     
036800*    03  -COPY WDQ201  -PRE ORQI-                                         
036900     EJECT                                                                
037000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLORQM01'.                    
037100 01  DLI-IO-WLORQM01.                                                     
037200*    03  -COPY WDQ101                                                     
037300     EJECT                                                                
037400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL450501'.                    
037500 01  DLI-IO-WL450501.                                                     
037600*03  WL450511 -COPY WDGX4506    -PRE 4505-                                
037700     EJECT                                                                
037800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
037900 01  DLI-IO-WDB601.                                                       
038000*    03  -COPY WDB601                                                     
038100     EJECT                                                                
038110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
038120 01  DLI-IO-WDB201.                                                       
038130*    03  -COPY WDB201                                                     
038140     EJECT                                                                
038200 01  FILLER                      PIC X(12) VALUE 'DUMMY-ARTC'.            
038300 01  ETA-ARTC-PCB                PIC X.                                   
038400 01  FILLER                      PIC X(12) VALUE 'DUMMY-LEVA'.            
038500 01  ETA-LEVA-PCB                PIC X.                                   
038600                                                                          
038700 LINKAGE SECTION.                                                         
038800                                                                          
038900*01  -COPY W0009   -PRE MSG-                                              
039000     EJECT                                                                
039100     -COPY W0008  -PRE USEA-                                              
039200     05  FILLER PIC X.                                                    
039300*01  -COPY W0008  -PRE WDK7-                                              
039400     05  FILLER                  PIC X.                                   
039500     EJECT                                                                
039600*01  -COPY W0008  -PRE ARTC-                                              
039700     05  FILLER                  PIC X.                                   
039800     EJECT                                                                
039900*01  -COPY W0008  -PRE ORDP-                                              
040000     05  FILLER                  PIC X.                                   
040100     EJECT                                                                
040200*01  -COPY W0008  -PRE ORQL-                                              
040300     05  FILLER                  PIC X.                                   
040400     EJECT                                                                
040500*01  -COPY W0008  -PRE ORQM-                                              
040600     05  FILLER                  PIC X.                                   
040700     EJECT                                                                
040800*01  -COPY W0008  -PRE 4505-                                              
040900     05  FILLER                  PIC X.                                   
041000     EJECT                                                                
041100*01  -COPY W0008  -PRE WDB6-                                              
041200     05  FILLER                  PIC X.                                   
041300     EJECT                                                                
041310*01  -COPY W0008  -PRE WDB2-                                              
041320     05  FILLER                  PIC X.                                   
041330     EJECT                                                                
041400 01  ETA-WDK7-PCB                PIC X.                                   
041500 01  ETA-INLC-PCB                PIC X.                                   
041600 01  ETA-WDB6-PCB                PIC X.                                   
041700 01  ETA-WDD9-PCB                PIC X.                                   
041800     EJECT                                                                
041900                                                                          
042000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK7-PCB ARTC-PCB             
042100     ORDP-PCB ORQL-PCB ORQM-PCB 4505-PCB WDB6-PCB WDB2-PCB                
042200     ETA-WDK7-PCB ETA-INLC-PCB ETA-WDB6-PCB ETA-WDD9-PCB.                 
042300 MAIN SECTION.                                                            
042400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK7-PCB ARTC-PCB             
042500     ORDP-PCB ORQL-PCB ORQM-PCB 4505-PCB WDB6-PCB WDB2-PCB                
042600     ETA-WDK7-PCB ETA-INLC-PCB ETA-WDB6-PCB ETA-WDD9-PCB.                 
042700                                                                          
042800     PERFORM A-INIT                                                       
042900     PERFORM S01-LAES-W44035                                              
043000     PERFORM UNTIL END-OF-W44035                                          
043100       IF CHKP-ANT > CHKP-MAX                                             
043200         PERFORM J-TAG-CHECKPOINT                                         
043300       END-IF                                                             
043400       PERFORM B-KOLLA-RESTORDER                                          
043500       IF FULL-TAECKNING                                                  
043600         PERFORM C-UPPDATERA-WDK7-HEMMA                                   
043700         MOVE +0   TO W-SUM-HOME-KVROS-DAG                                
043800                      W-SUM-HOME-KVROS-BULK                               
043900*        PERFORM D-SKAPA-TAECKNINGSTRANS                                  
044000       END-IF                                                             
044100       MOVE NEJ TO TAECKNING-CLEAR-SW                                     
044200       MOVE NEJ TO DC-TAECK41-SW                                          
044300                   DC-TAECK43-SW                                          
044400                   DC-TAECK44-SW                                          
044500                   DC-TAECK45-SW                                          
044600                   DC-TAECK46-SW                                          
044700                   DC-TAECK47-SW                                          
044800                   DC-TAECK51-SW                                          
044900                   DC-TAECK71-SW                                          
045000                   DC-TAECK72-SW                                          
045100                   DC-TAECK73-SW                                          
045200                   DC-TAECK74-SW                                          
045300                   DC-TAECK6A-SW                                          
045400                   DC-TAECK61-SW                                          
045500       PERFORM S01-LAES-W44035                                            
045600     END-PERFORM                                                          
045700                                                                          
045800                                                                          
045900     PERFORM Z-FINIT                                                      
046000                                                                          
046100     MOVE ZERO TO RETURN-CODE                                             
046200     GOBACK                                                               
046300     .                                                                    
046400     EJECT                                                                
046500                                                                          
046600 A-INIT SECTION.                                                          
046700     SKIP2                                                                
046800                                                                          
046900     OPEN INPUT W44035                                                    
047000                                                                          
047100     PERFORM IMS-RESTART                                                  
047200     MOVE ZERO TO CHKP-ANT                                                
047300     ACCEPT DAGENS-DATUM FROM DATE                                        
047400     MOVE NEJ TO   SALDO-SW                                               
047500                   DISPONIBELT-SW                                         
047600                   ETA-SW                                                 
047700                   TAECKNING-CLEAR-SW                                     
047800                   DISP-HOMEDC-CN-SW                                      
047900     MOVE +0  TO W-SUM-HOME-KVROS-DAG                                     
048000                 W-SUM-HOME-KVROS-BULK                                    
048100                                                                          
048200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
048300     PERFORM AB-FIXA-LOKAL-TID                                            
048400     .                                                                    
048500     EJECT                                                                
048600                                                                          
048700 AB-FIXA-LOKAL-TID SECTION.                                               
048800                                                                          
048900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
049000     MOVE '001'             TO MSGI-KDCALL                                
049100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
049200     MOVE 'W440'            TO MSGI-IDTRANS                               
049300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
049400                                                                          
049500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
049600     .                                                                    
049700     EJECT                                                                
049800                                                                          
049900 B-KOLLA-RESTORDER SECTION.                                               
050000                                                                          
050100*    FÖRSÖKER TÄCKA FRÅN ANNAT LAGER ÄN HEMMA                             
050200*    OM DET FINNS DISPONIBELT PÅ ANNAT LAGER FLYTTAR MAN                  
050300*    RESTORDER DIT OCH SKAPAR SEDAN EN TÄCKNINGSTRANS                     
050400                                                                          
050500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
050600*    LÄSER ALLA OTÄCKTA RO-RADER I PRIORITETSORDNING.           *         
050700*    BEHANDLA DE RADER SOM KAN TÄCKAS FRÅN HTR'S CLAGER.        *         
050800*                                                               *         
050900*    GER SÅ LÄNGE DISPONIBELT RÄCKER.                           *         
051000*                                                               *         
051100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
051200                                                                          
051300     MOVE LOW-VALUE    TO W1-WDA5ASEQ-X                                   
051400     MOVE HIGH-VALUE   TO W2-WDA5ASEQ-X                                   
051500     MOVE IN-IDARTNR   TO W1-IDARTNR                                      
051600                          W2-IDARTNR                                      
051700                          W-IDARTNR-K6                                    
051800                          W-IDARTNR-K7                                    
051900     MOVE IN-IDDC      TO W1-IDDC                                         
052000                          W2-IDDC                                         
052100                          IN-WS-IDDC                                      
052200     MOVE '2'          TO W-KDSTARAD                                      
052300     SET FETCH-CN-BAL-YES                                                 
052400         DISP-CN-NOTFOUND                                                 
052500                       TO TRUE                                            
052600     PERFORM IMS-GHN-ORDQ01-ORDP01                                        
052700                                                                          
052800     MOVE +1           TO W-DISPONIBELT                                   
052900     MOVE ZERO         TO WS-DISP-HOMEDC-CN                               
053000     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
053100                   W-DISPONIBELT = +0                                     
053200     MOVE ORDP-RAD-IDDISTR       TO W-IDDISTR                             
053300     MOVE ORDP-RAD-IDKUNDNR      TO W-IDKUNDNR                            
053400     MOVE K-IDKUNDRF7-NOLL       TO W-IDKUNDRF                            
053500                                                                          
053600       SET BACKORDER-YES TO TRUE                                          
053700       MOVE NEJ TO DISPONIBELT-SW                                         
053800                                                                          
053900       IF ORDP-RAD-KDORDKL > 1 AND                                        
054000          ORDP-RAD-IDDC NOT = DCS-IDDC                                    
054100          MOVE ORDP-RAD-IDDC TO W-IDDC                                    
054200          PERFORM IMS-GU-WDB601                                           
054300          IF SEGMENT-SAKNAS                                               
054400             MOVE NEJ TO DCS-FLCLEAR-BULK                                 
054500          ELSE                                                            
054600          DISPLAY 'DCS-FLCLEAR-BULK ' DCS-FLCLEAR-BULK                    
054700          END-IF                                                          
054800       END-IF                                                             
054900       IF ORDP-RAD-KDORDKL < 2 OR                                         
055000          DCS-FLCLEAR-BULK = YES                                          
055100         PERFORM S07-NOLLSTAELL-FAELT                                     
055200         PERFORM BA-HAEMTA-OHUV-INFO                                      
055300         PERFORM BB-HAEMTA-TIBERANK                                       
055400         IF ETA-EJ-PAA-VAEG                                               
055500*          FOR CN, CHECK IN HOME DC FIRST BY INCLUDING                    
055600*          AK IN AVAILABLE BALANCE CALCULATION                            
055700           IF IN-NDC-CN                                                   
055800             IF FETCH-CN-BAL-YES                                          
055900               PERFORM BG-BERAKNA-DISPONIBELT-CN                          
056000               SET FETCH-CN-BAL-NO       TO TRUE                          
056100             END-IF                                                       
056200             IF DISP-CN-FOUND                                             
056300               IF WS-DISP-HOMEDC-CN >= ORDP-RAD-KVART                     
056400                 SUBTRACT ORDP-RAD-KVART                                  
056500                                       FROM WS-DISP-HOMEDC-CN             
056600                 IF WS-DISP-HOMEDC-CN = 0                                 
056700                   MOVE +1               TO W-DISPONIBELT                 
056800                 ELSE                                                     
056900                   MOVE WS-DISP-HOMEDC-CN                                 
057000                                         TO W-DISPONIBELT                 
057100                 END-IF                                                   
057200                 SET BACKORDER-NO        TO TRUE                          
057300               END-IF                                                     
057400             END-IF                                                       
057500           END-IF                                                         
057600           IF BACKORDER-YES                                               
057700             PERFORM S07-NOLLSTAELL-FAELT                                 
057800             PERFORM BC-BERAKNA-DISPONIBELT                               
057900             IF DISPONIBELT-FINNS                                         
058000               PERFORM BD-TAECKNING-RAD                                   
058100               PERFORM BE-UPPDATERA-SALDO-WDK7                            
058200               PERFORM BF-SKAPA-ORDERBEKRAEFTELSE                         
058300               PERFORM D-SKAPA-TAECKNINGSTRANS                            
058400             END-IF                                                       
058500           END-IF                                                         
058600         END-IF                                                           
058700       END-IF                                                             
058800       MOVE '2' TO W-KDSTARAD                                             
058900       PERFORM IMS-GHN-ORDQ01-ORDP01                                      
059000     END-PERFORM                                                          
059100     .                                                                    
059200     EJECT                                                                
059300                                                                          
059400 BA-HAEMTA-OHUV-INFO SECTION.                                             
059500                                                                          
059600     MOVE ORDP-RAD-IDDISTR       TO W-IDDISTR                             
059700     MOVE ORDP-RAD-IDKUNDNR      TO W-IDKUNDNR                            
059800     MOVE K-IDKUNDRF7-NOLL       TO W-IDKUNDRF                            
059900     MOVE ORDP-RAD-IDKUNDRF(1:5) TO W-IDKUNDRF(3:5)                       
060000     PERFORM IMS-GU-ORQL-WDQ201                                           
060100                                                                          
060200     IF SEGMENT-FINNS                                                     
060300       MOVE ORQI-OHUV-IDDISTR    TO W-IDDISTR-WDB2                        
060400       MOVE ORQI-OHUV-IDKUNDNR   TO W-IDKUNDNR-WDB2                       
060500       PERFORM IMS-GU-WDB201                                              
060600                                                                          
060700       MOVE 1 TO WS-IDDC-IX                                               
060800                 WS-HELP-IX                                               
060900                                                                          
061000       PERFORM UNTIL WS-IDDC-IX > IX-DCCLEAR-MAX                          
061100          IF ORDP-RAD-KDORDKL < 2                                         
061200             MOVE GMT-IDDC-DAY(WS-IDDC-IX)     TO                         
061300                           WS-IDDC-CLEAR(WS-HELP-IX)                      
061400          ELSE                                                            
061500             IF DCS-FLCLEAR-BULK = YES                                    
061600                MOVE GMT-IDDC-BULK(WS-IDDC-IX) TO                         
061700                           WS-IDDC-CLEAR(WS-HELP-IX)                      
061800             END-IF                                                       
061900          END-IF                                                          
061910          ADD 1     TO WS-HELP-IX                                         
061920          ADD 1     TO WS-IDDC-IX                                         
061930       END-PERFORM                                                        
061940                                                                          
062000       PERFORM UNTIL WS-HELP-IX > WS-HELP-IX-MAX                          
062100          MOVE SPACE   TO WS-IDDC-CLEAR(WS-HELP-IX)                       
062200          ADD 1        TO WS-HELP-IX                                      
062300       END-PERFORM                                                        
062400     END-IF                                                               
062500     .                                                                    
062600     EJECT                                                                
062700                                                                          
062800 BB-HAEMTA-TIBERANK SECTION.                                              
062900                                                                          
063000     MOVE NEJ                  TO ETA-SW                                  
063100                                                                          
063200     IF ORQI-OHUV-KVDAGAR-DOW > 0                                         
063300       MOVE '612'              TO ETA-KDCALL                              
063400       MOVE ORDP-RAD-IDDC      TO ETA-IDDC-REC                            
063500       MOVE ORDP-RAD-IDARTNR   TO ETA-IDARTNR                             
063600       MOVE ORDP-RAD-IDLEVNR   TO ETA-IDLEVNR                             
063700       MOVE ZERO               TO ETA-KDFRAKT                             
063800       ACCEPT WS-ETA-DATUM     FROM DATE                                  
063900       MOVE WS-ETA-DATUM       TO ETA-TIAAMMDD-ANROP                      
064000       IF WS-ETA-DATUM-AAR > 50                                           
064100          MOVE 19              TO ETA-TISEKEL-ANROP                       
064200       ELSE                                                               
064300          MOVE 20              TO ETA-TISEKEL-ANROP                       
064400       END-IF                                                             
064500                                                                          
064600       CALL W218ETA USING ETA-W218LETA                                    
064700                           ETA-ARTC-PCB ETA-WDK7-PCB                      
064800                           ETA-INLC-PCB ETA-LEVA-PCB                      
064900                           ETA-WDB6-PCB ETA-WDD9-PCB                      
065000       IF ETA-SVAR-OK = JA                                                
065100         MOVE ETA-TIAAMMDD-SVAR TO WS-SVAR-ETA-DATUM                      
065200         MOVE 001                TO DAG-KDCALL                            
065300         MOVE DAGENS-DATUM       TO DAG-TIAAMMDD-FOM                      
065400         MOVE WS-SVAR-ETA-DATUM  TO DAG-TIAAMMDD-TOM                      
065500                                                                          
065600         CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA                    
065700                             DAG-KDSVAR                                   
065800                                                                          
065900         IF DAG-KDSVAR = 'F'                                              
066000           MOVE 'FEL FRÅN DAGKONV BABA-SECTION' TO FELTEXT                
066100           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
066200         ELSE                                                             
066300           IF DAG-KVKALDAG NOT > ORQI-OHUV-KVDAGAR-DOW                    
066400             MOVE JA TO ETA-SW                                            
066500           END-IF                                                         
066600         END-IF                                                           
066700       END-IF                                                             
066800     END-IF                                                               
066900     .                                                                    
067000     EJECT                                                                
067100                                                                          
067200 BC-BERAKNA-DISPONIBELT  SECTION.                                         
067300                                                                          
067400     MOVE NEJ                  TO SALDO-SW                                
067500                                                                          
067600     PERFORM IMS-GU-ARTC01                                                
067700     IF SEGMENT-FINNS                                                     
067800       PERFORM IMS-GNP-ARTC11                                             
067900       IF SEGMENT-FINNS                                                   
068000         IF ARTC-CLAG-PRARTSTD > +0                                       
068100           AND NOT (ARTC-CLAG-KDERS = 22 OR ARTC-CLAG-KDERS = 23 )        
068200                                                                          
068300           MOVE ORDP-RAD-IDDC TO W-IDDC-K7                                
068400*          PERFORM S03-HAEMTA-WDK7-INFO                                   
068500           PERFORM BCA-HAEMTA-ALLA-NAESTA-DC                              
068600           MOVE 1 TO NAESTA-DC-IX                                         
068700           PERFORM UNTIL NAESTA-DC-IX > NAESTA-DC-IX-MAX                  
068800                      OR DISPONIBELT-FINNS                                
068900                                                                          
069000              MOVE WS-NAESTA-IDDC(NAESTA-DC-IX) TO W-IDDC-K7              
069100              PERFORM S03-HAEMTA-WDK7-INFO                                
069200              IF SALDO-FINNS                                              
069300                 PERFORM S04-SALDO-BERAKNING                              
069400                 IF WS-DISP > ZERO                                        
069500                    PERFORM S05-KOLLA-KVQPACK                             
069600                    IF W-DISPONIBELT >= ORDP-RAD-KVART                    
069700                       MOVE JA  TO DISPONIBELT-SW                         
069800                    END-IF                                                
069900                 END-IF                                                   
070000              END-IF                                                      
070100                                                                          
070200              IF DISPONIBELT-SAKNAS                                       
070300                 PERFORM S07-NOLLSTAELL-FAELT                             
070400                 MOVE WS-NAESTA-IDDC(NAESTA-DC-IX) TO W-IDDC-K7           
070500                 PERFORM S03-HAEMTA-WDK7-INFO                             
070600              END-IF                                                      
070700              ADD 1 TO NAESTA-DC-IX                                       
070800           END-PERFORM                                                    
070900*          PERFORM BCA-WDK7-SALDO                                         
071000*          IF SALDO-FINNS                                                 
071100*            PERFORM S04-SALDO-BERAKNING                                  
071200*            IF WS-DISP > ZERO                                            
071300*              PERFORM S05-KOLLA-KVQPACK                                  
071400*              IF W-DISPONIBELT >= ORDP-RAD-KVART                         
071500*                MOVE JA TO DISPONIBELT-SW                                
071600*              ELSE                                                       
071700*                MOVE NAESTA-IDDC TO W-IDDC-K7                            
071800*                PERFORM S07-NOLLSTAELL-FAELT                             
071900*                PERFORM S06-KOLLA-NAESTA-DC                              
072000*              END-IF                                                     
072100*            ELSE                                                         
072200*              MOVE NAESTA-IDDC TO W-IDDC-K7                              
072300*              PERFORM S07-NOLLSTAELL-FAELT                               
072400*              PERFORM S06-KOLLA-NAESTA-DC                                
072500*            END-IF                                                       
072600*          ELSE                                                           
072700*            MOVE NAESTA-IDDC TO W-IDDC-K7                                
072800*            PERFORM S07-NOLLSTAELL-FAELT                                 
072900*            PERFORM S06-KOLLA-NAESTA-DC                                  
073000*          END-IF                                                         
073100         END-IF                                                           
073200       END-IF                                                             
073300     END-IF                                                               
073400     .                                                                    
073500     EJECT                                                                
073600                                                                          
073700 BCA-HAEMTA-ALLA-NAESTA-DC SECTION.                                       
073800                                                                          
073900     MOVE 1 TO WS-IDDC-IX                                                 
074000               NAESTA-DC-IX                                               
074100                                                                          
074200     PERFORM UNTIL WS-IDDC-IX > IX-DCCLEAR-MAX                            
074300        IF WS-IDDC-CLEAR(WS-IDDC-IX) = ORDP-RAD-IDDC                      
074400           CONTINUE                                                       
074500        ELSE                                                              
074600           IF NAESTA-DC-IX NOT > NAESTA-DC-IX-MAX                         
074700              MOVE WS-IDDC-CLEAR(WS-IDDC-IX) TO                           
074800                                 WS-NAESTA-IDDC(NAESTA-DC-IX)             
074900              ADD 1  TO NAESTA-DC-IX                                      
075000           END-IF                                                         
075100        END-IF                                                            
075200        ADD 1  TO WS-IDDC-IX                                              
075300     END-PERFORM                                                          
075400                                                                          
075500     PERFORM UNTIL NAESTA-DC-IX > NAESTA-DC-IX-MAX                        
075600        MOVE SPACE TO WS-NAESTA-IDDC(NAESTA-DC-IX)                        
075700        ADD 1 TO NAESTA-DC-IX                                             
075800     END-PERFORM                                                          
075900                                                                          
076000*    IF ORDP-RAD-IDDC = WS-IDDC-CLEAR(1)                                  
076100*      MOVE WS-IDDC-CLEAR(2)     TO W-IDDC-K7                             
076200*      MOVE WS-IDDC-CLEAR(3)     TO NAESTA-IDDC                           
076300*    ELSE                                                                 
076400*      IF ORDP-RAD-IDDC = WS-IDDC-CLEAR(2)                                
076500*        MOVE WS-IDDC-CLEAR(1)   TO W-IDDC-K7                             
076600*        MOVE WS-IDDC-CLEAR(3)   TO NAESTA-IDDC                           
076700*      ELSE                                                               
076800*        IF ORDP-RAD-IDDC = WS-IDDC-CLEAR(3)                              
076900*          MOVE WS-IDDC-CLEAR(1) TO W-IDDC-K7                             
077000*          MOVE WS-IDDC-CLEAR(2) TO NAESTA-IDDC                           
077100*        END-IF                                                           
077200*      END-IF                                                             
077300*    END-IF                                                               
077400*                                                                         
077500*    PERFORM S03-HAEMTA-WDK7-INFO                                         
077600     .                                                                    
077700     EJECT                                                                
077800                                                                          
077900 BD-TAECKNING-RAD SECTION.                                                
078000                                                                          
078100     MOVE ORDP-RAD-KVART        TO W-KVART                                
078200     PERFORM S02-SUM-SALDO-WDK7                                           
078300                                                                          
078400     MOVE W-IDDC-K7             TO ORDP-RAD-IDDC                          
078500                                   ORDP-RAD-IDDC-RO                       
078600                                                                          
078700     PERFORM IMS-REPLACE-ORDQ01-ORDP01                                    
078800                                                                          
078900     SUBTRACT ORDP-RAD-KVART FROM W-DISPONIBELT                           
079000     .                                                                    
079100     EJECT                                                                
079200                                                                          
079300 BE-UPPDATERA-SALDO-WDK7 SECTION.                                         
079400                                                                          
079500     IF W-SUM-KVROS-DAG   = +0 AND                                        
079600        W-SUM-KVROS-BULK  = +0 AND                                        
079700        W-SUM-KVRESS      = +0                                            
079800         CONTINUE                                                         
079900     ELSE                                                                 
080000        PERFORM IMS-GHU-WDK711                                            
080100                                                                          
080200        COMPUTE WDK7-SLAG-KVROS-DAG  = WDK7-SLAG-KVROS-DAG                
080300                                     + W-SUM-KVROS-DAG                    
080400        COMPUTE WDK7-SLAG-KVROS-BULK = WDK7-SLAG-KVROS-BULK               
080500                                     + W-SUM-KVROS-BULK                   
080600*       COMPUTE WDK7-SLAG-KVRESS     = WDK7-SLAG-KVRESS                   
080700*                                    + W-SUM-KVRESS                       
080800                                                                          
080900        PERFORM IMS-REPLACE-WDK711                                        
081000                                                                          
081100        MOVE JA                      TO TAECKNING-CLEAR-SW                
081200        MOVE W-IDDC-K7               TO WS-IDDC                           
081300        ADD W-SUM-KVROS-DAG          TO W-SUM-HOME-KVROS-DAG              
081400        ADD W-SUM-KVROS-BULK         TO W-SUM-HOME-KVROS-BULK             
081500     END-IF                                                               
081600     .                                                                    
081700     EJECT                                                                
081800                                                                          
081900 BF-SKAPA-ORDERBEKRAEFTELSE SECTION.                                      
082000                                                                          
082100     MOVE ORQI-OHUV-IDORDER      TO OBKR-IDORDER                          
082200     MOVE ORDP-RAD-IDARTNR       TO OBKR-IDARTNR                          
082300                                                                          
082400     MOVE OBKR-IDORDER           TO W-IDORDER-Q1-MIN                      
082500                                    W-IDORDER-Q1-MAX                      
082600     MOVE OBKR-IDARTNR           TO W-IDARTNR-Q1-MIN                      
082700                                    W-IDARTNR-Q1-MAX                      
082800     MOVE +1                     TO W-IDLOPNR-Q1-MIN                      
082900                                    W-IDLOPNR-Q1-MAX                      
083000                                    W-IDSEQVNR-Q1-MIN                     
083100                                    W-IDSEQVNR-Q1-MAX                     
083200     PERFORM IMS-GU-ORQM-WDQ101                                           
083300     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
083400        ADD +1                   TO W-IDLOPNR-Q1-MIN                      
083500                                    W-IDLOPNR-Q1-MAX                      
083600        PERFORM IMS-GN-ORQM-WDQ101                                        
083700     END-PERFORM                                                          
083800     MOVE W-IDLOPNR-Q1-MIN       TO OBKR-IDLOPNR                          
083900                                                                          
084000     MOVE +1                     TO OBKR-IDSEKVNR                         
084100     MOVE  15                    TO OBKR-KDORDBEK                         
084200     MOVE IDPGM                  TO OBKR-IDPGM                            
084300     MOVE SPACE                  TO OBKR-BEERS                            
084400     MOVE SPACE                  TO OBKR-IDBIL                            
084500     MOVE ORQI-OHUV-BEKUNDRF     TO OBKR-BEKUNDRF                         
084600     MOVE ORDP-RAD-BERADREF      TO OBKR-BERADREF                         
084700     MOVE ORDP-RAD-BEVOLREF      TO OBKR-BEVOLREF                         
084800     MOVE ORDP-RAD-IDKAMPRF      TO OBKR-IDKAMPRF                         
084900     MOVE +0                     TO OBKR-DIERS-KVOT                       
085000     MOVE NEJ                    TO OBKR-FLAKPLOC                         
085100     MOVE ORDP-RAD-FLINVEST      TO OBKR-FLINVEST                         
085200     MOVE NEJ                    TO OBKR-FLOBOK                           
085300     MOVE JA                     TO OBKR-FLOBTRAN                         
085400     MOVE NEJ                    TO OBKR-FLOBPRT                          
085500     MOVE ORDP-RAD-FLPRTILL      TO OBKR-FLPRTILL                         
085600     MOVE JA                     TO OBKR-FLRESTN                          
085700     MOVE JA                     TO OBKR-FLSLATT                          
085800     MOVE ORDP-RAD-FLERS         TO OBKR-FLTILLK                          
085900     MOVE +0                     TO OBKR-IDARTNR-TILLK                    
086000     MOVE ORDP-RAD-IDDISTR       TO OBKR-IDDISTR                          
086100     MOVE ORDP-RAD-IDKUNDNR      TO OBKR-IDKUNDNR                         
086200     MOVE K-IDKUNDRF7-NOLL       TO OBKR-IDKUNDRF                         
086300     MOVE ORDP-RAD-IDKUNDRF(1:5) TO OBKR-IDKUNDRF (3:5)                   
086400     MOVE K-IDKUNDRF7-NOLL       TO OBKR-IDKUNDRF-RO                      
086500     MOVE ORDP-RAD-IDKUNDRF(1:5)                                          
086600                                 TO OBKR-IDKUNDRF-RO (3:5)                
086700     MOVE ORDP-RAD-IDLEVNR       TO OBKR-IDLEVNR                          
086800     MOVE ORDP-RAD-IDLOPNR       TO OBKR-IDLOPNR-RO                       
086900     MOVE ORDP-RAD-IDSYSTEM      TO OBKR-IDSYSTEM                         
087000     MOVE ORDP-RAD-IDDC          TO OBKR-IDDC                             
087100     MOVE ORDP-RAD-IDDC-RO       TO OBKR-IDDC-RO                          
087200     MOVE ORDP-RAD-KDDSP         TO OBKR-KDDSP                            
087300     MOVE +0                     TO OBKR-KDERS                            
087400     MOVE ORDP-RAD-KDOI          TO OBKR-KDOI                             
087500     MOVE ORDP-RAD-CLEARGROUP    TO OBKR-CLEARGROUP                       
087600     MOVE ORDP-RAD-KDKVBRYT      TO OBKR-KDKVBRYT                         
087700     MOVE ORDP-RAD-KDPRTYP       TO OBKR-KDPRTYP                          
087800     MOVE ORDP-RAD-KDTPOTYP      TO OBKR-KDTPOTYP                         
087900     MOVE ORDP-RAD-KDVRINFO      TO OBKR-KDVRINFO                         
088000     MOVE +0                     TO OBKR-KVANNANT                         
088100                                    OBKR-KVAVBART                         
088200     MOVE ORDP-RAD-KVART         TO OBKR-KVBEART-Q                        
088300                                    OBKR-KVBEART                          
088400     MOVE +0                     TO OBKR-KVBEART-TILLK                    
088500                                    OBKR-KVPREAVB                         
088600                                    OBKR-KVPRERO                          
088700                                    OBKR-KVQPACK                          
088800                                    OBKR-KVRO                             
088900                                    OBKR-KVSLATT                          
089000     EJECT                                                                
089100     MOVE ORDP-RAD-PRARTNTO      TO OBKR-PRARTNTO                         
089200     MOVE ORDP-RAD-DEAL-PR-LINE  TO OBKR-DEAL-PR-LINE                     
089300     MOVE +0                     TO OBKR-PRBPRIS                          
089400     MOVE ORDP-RAD-REKSIFFR      TO OBKR-REKSIFFR                         
089500     MOVE +0                     TO OBKR-REKSIFFR-TILLK                   
089600     MOVE +0                     TO OBKR-RERF-RAD                         
089700     MOVE +0                     TO OBKR-TIDISPIN                         
089800     MOVE DAGENS-DATUM           TO OBKR-TIORDREG                         
089900     MOVE +0                     TO OBKR-TIPRIS                           
090000     MOVE MSGI-TILOKDAT          TO OBKR-TIREGDAT                         
090100     MOVE MSGI-TILOKTID          TO WS-TIHHMM                             
090200     MOVE WS-TIHHMMSS            TO OBKR-TIREGTID                         
090300     MOVE ORDP-RAD-DARODAT (3:6) TO OBKR-TIRODAT                          
090400     MOVE OBKR-TIREGDAT          TO WS-AAMMDD-9KOMPL                      
090500     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
090600       MOVE 20                   TO WS-SEKEL-9KOMPL                       
090700     ELSE                                                                 
090800       MOVE 19                   TO WS-SEKEL-9KOMPL                       
090900     END-IF                                                               
091000     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
091100     MOVE ORDP-RAD-TITPO         TO OBKR-TITPO                            
091200     MOVE DAGENS-DATUM           TO WS-AAMMDD-9KOMPL                      
091300     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
091400     MOVE ORDP-RAD-KDFRAKT       TO OBKR-KDFRAKT                          
091500     MOVE ORDP-RAD-KDORDKL       TO OBKR-KDORDKL                          
091600                                                                          
091700     MOVE ORDP-RAD-KDORDTYP-LDC  TO OBKR-KDORDTYP-LDC                     
091800     MOVE ORDP-RAD-TIREPDAT      TO OBKR-TIREPDAT                         
091900     MOVE ORDP-RAD-IDKUNDRF-WIP  TO OBKR-IDKUNDRF-WIP                     
092000     MOVE ZERO                   TO OBKR-TIDLEVDAT                        
092100     MOVE ORDP-RAD-PRAVCOST      TO OBKR-PRAVCOST                         
092200     MOVE ORDP-RAD-KDVALISO      TO OBKR-KDVALISO                         
092300                                                                          
092400     PERFORM IMS-ISRT-ORQM-WDQ101                                         
092500     .                                                                    
092600     EJECT                                                                
092700                                                                          
092800 BG-BERAKNA-DISPONIBELT-CN SECTION.                                       
092900                                                                          
093000     MOVE NEJ                  TO SALDO-SW                                
093100     PERFORM IMS-GU-ARTC01                                                
093200     IF SEGMENT-FINNS                                                     
093300       PERFORM IMS-GNP-ARTC11                                             
093400       IF SEGMENT-FINNS                                                   
093500         IF ARTC-CLAG-PRARTSTD > +0                                       
093600           AND NOT (ARTC-CLAG-KDERS = 22 OR ARTC-CLAG-KDERS = 23 )        
093700           MOVE ORDP-RAD-IDDC  TO W-IDDC-K7                               
093800           PERFORM S03-HAEMTA-WDK7-INFO                                   
093900           IF SALDO-FINNS                                                 
094000             PERFORM S08-SALDO-BERAKNING-CN                               
094100             IF WS-DISP > ZERO                                            
094200               PERFORM S05-KOLLA-KVQPACK                                  
094300               MOVE W-DISPONIBELT                                         
094400                               TO WS-DISP-HOMEDC-CN                       
094500               SET DISP-CN-FOUND                                          
094600                               TO TRUE                                    
094700             END-IF                                                       
094800           END-IF                                                         
094900         END-IF                                                           
095000       END-IF                                                             
095100     END-IF                                                               
095200     .                                                                    
095300     EJECT                                                                
095400                                                                          
095500 C-UPPDATERA-WDK7-HEMMA SECTION.                                          
095600                                                                          
095700     MOVE IN-IDARTNR    TO W-IDARTNR-K7                                   
095800     MOVE IN-IDDC       TO W-IDDC-K7                                      
095900     PERFORM IMS-GHU-WDK711                                               
096000     IF SEGMENT-FINNS                                                     
096100       COMPUTE WDK7-SLAG-KVROS-DAG  = WDK7-SLAG-KVROS-DAG                 
096200                                    - W-SUM-HOME-KVROS-DAG                
096300       COMPUTE WDK7-SLAG-KVROS-BULK = WDK7-SLAG-KVROS-BULK                
096400                                    - W-SUM-HOME-KVROS-BULK               
096500                                                                          
096600*      COMPUTE WDK7-SLAG-KVRESS    = WDK7-SLAG-KVRESS                     
096700*                                  - W-SUM-KVRESS                         
096800       PERFORM IMS-REPLACE-WDK711                                         
096900     END-IF                                                               
097000     .                                                                    
097100     EJECT                                                                
097200                                                                          
097300 D-SKAPA-TAECKNINGSTRANS SECTION.                                         
097400                                                                          
097500     IF WS-IDDC = WC-NDC-US-RU AND DC-TAECK41-EJ                          
097600       MOVE WC-NDC-US-RU     TO IDDC-4505                                 
097700       MOVE SPACE            TO 4505-4506-WDGX4506                        
097800       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
097900       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
098000       MOVE ZERO             TO 4505-4506-KVANTMOT                        
098100       PERFORM IMS-ISRT-4505-4506                                         
098200       MOVE JA TO DC-TAECK41-SW                                           
098300     END-IF                                                               
098400                                                                          
098500     IF WS-IDDC = WC-NDC-US-LA AND DC-TAECK43-EJ                          
098600       MOVE WC-NDC-US-LA     TO IDDC-4505                                 
098700       MOVE SPACE            TO 4505-4506-WDGX4506                        
098800       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
098900       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
099000       MOVE ZERO             TO 4505-4506-KVANTMOT                        
099100       PERFORM IMS-ISRT-4505-4506                                         
099200       MOVE JA TO DC-TAECK43-SW                                           
099300     END-IF                                                               
099400                                                                          
099500     IF WS-IDDC = WC-NDC-US-SE AND DC-TAECK44-EJ                          
099600       MOVE WC-NDC-US-SE     TO IDDC-4505                                 
099700       MOVE SPACE            TO 4505-4506-WDGX4506                        
099800       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
099900       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
100000       MOVE ZERO             TO 4505-4506-KVANTMOT                        
100100       PERFORM IMS-ISRT-4505-4506                                         
100200       MOVE JA TO DC-TAECK44-SW                                           
100300     END-IF                                                               
100400                                                                          
100500     IF WS-IDDC = WC-NDC-US-CH AND DC-TAECK45-EJ                          
100600       MOVE WC-NDC-US-CH     TO IDDC-4505                                 
100700       MOVE SPACE            TO 4505-4506-WDGX4506                        
100800       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
100900       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
101000       MOVE ZERO             TO 4505-4506-KVANTMOT                        
101100       PERFORM IMS-ISRT-4505-4506                                         
101200       MOVE JA TO DC-TAECK45-SW                                           
101300     END-IF                                                               
101400                                                                          
101500     IF WS-IDDC = WC-NDC-US-JA AND DC-TAECK46-EJ                          
101600       MOVE WC-NDC-US-JA     TO IDDC-4505                                 
101700       MOVE SPACE            TO 4505-4506-WDGX4506                        
101800       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
101900       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
102000       MOVE ZERO             TO 4505-4506-KVANTMOT                        
102100       PERFORM IMS-ISRT-4505-4506                                         
102200       MOVE JA TO DC-TAECK46-SW                                           
102300     END-IF                                                               
102301                                                                          
102302     IF WS-IDDC = WC-NDC-US-DA AND DC-TAECK47-EJ                          
102303       MOVE WC-NDC-US-DA     TO IDDC-4505                                 
102304       MOVE SPACE            TO 4505-4506-WDGX4506                        
102305       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
102306       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
102307       MOVE ZERO             TO 4505-4506-KVANTMOT                        
102308       PERFORM IMS-ISRT-4505-4506                                         
102309       MOVE JA TO DC-TAECK47-SW                                           
102310     END-IF                                                               
102320                                                                          
102330     IF WS-IDDC = WC-NDC-CA AND DC-TAECK51-EJ                             
102340       MOVE WC-NDC-CA        TO IDDC-4505                                 
102350       MOVE SPACE            TO 4505-4506-WDGX4506                        
102360       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
102370       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
102380       MOVE ZERO             TO 4505-4506-KVANTMOT                        
102390       PERFORM IMS-ISRT-4505-4506                                         
102400       MOVE JA TO DC-TAECK51-SW                                           
102500     END-IF                                                               
102600                                                                          
102700     IF WS-IDDC = WC-NDC-CN-71 AND DC-TAECK71-EJ                          
102800       MOVE WC-NDC-CN-71     TO IDDC-4505                                 
102900       MOVE SPACE            TO 4505-4506-WDGX4506                        
103000       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
103100       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
103200       MOVE ZERO             TO 4505-4506-KVANTMOT                        
103300       PERFORM IMS-ISRT-4505-4506                                         
103400       MOVE JA TO DC-TAECK71-SW                                           
103500     END-IF                                                               
103600                                                                          
103700     IF WS-IDDC = WC-NDC-CN-72 AND DC-TAECK72-EJ                          
103800       MOVE WC-NDC-CN-72     TO IDDC-4505                                 
103900       MOVE SPACE            TO 4505-4506-WDGX4506                        
104000       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
104100       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
104200       MOVE ZERO             TO 4505-4506-KVANTMOT                        
104300       PERFORM IMS-ISRT-4505-4506                                         
104400       MOVE JA TO DC-TAECK72-SW                                           
104500     END-IF                                                               
104600                                                                          
104700     IF WS-IDDC = WC-NDC-CN-73 AND DC-TAECK73-EJ                          
104800       MOVE WC-NDC-CN-73     TO IDDC-4505                                 
104900       MOVE SPACE            TO 4505-4506-WDGX4506                        
105000       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
105100       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
105200       MOVE ZERO             TO 4505-4506-KVANTMOT                        
105300       PERFORM IMS-ISRT-4505-4506                                         
105400       MOVE JA TO DC-TAECK73-SW                                           
105500     END-IF                                                               
105600                                                                          
105700     IF WS-IDDC = WC-NDC-CN-74 AND DC-TAECK74-EJ                          
105800       MOVE WC-NDC-CN-74     TO IDDC-4505                                 
105900       MOVE SPACE            TO 4505-4506-WDGX4506                        
106000       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
106100       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
106200       MOVE ZERO             TO 4505-4506-KVANTMOT                        
106300       PERFORM IMS-ISRT-4505-4506                                         
106400       MOVE JA TO DC-TAECK74-SW                                           
106500     END-IF                                                               
106600                                                                          
106700     IF WS-IDDC = WC-NDC-JP-6A AND DC-TAECK6A-EJ                          
106800       MOVE WC-NDC-JP-6A     TO IDDC-4505                                 
106900       MOVE SPACE            TO 4505-4506-WDGX4506                        
107000       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
107100       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
107200       MOVE ZERO             TO 4505-4506-KVANTMOT                        
107300       PERFORM IMS-ISRT-4505-4506                                         
107400       MOVE JA TO DC-TAECK6A-SW                                           
107500     END-IF                                                               
107600                                                                          
107601     IF WS-IDDC = WC-NDC-JP-61 AND DC-TAECK61-EJ                          
107602       MOVE WC-NDC-JP-61     TO IDDC-4505                                 
107603       MOVE SPACE            TO 4505-4506-WDGX4506                        
107604       MOVE IN-IDARTNR       TO 4505-4506-IDARTNR                         
107605       MOVE K-KDTAKORS       TO 4505-4506-KDTAKORS                        
107606       MOVE ZERO             TO 4505-4506-KVANTMOT                        
107607       PERFORM IMS-ISRT-4505-4506                                         
107608       MOVE JA TO DC-TAECK61-SW                                           
107609     END-IF                                                               
107610     .                                                                    
107620     EJECT                                                                
107630                                                                          
107640 Z-FINIT SECTION.                                                         
107650                                                                          
107660                                                                          
107670     CLOSE W44035                                                         
107680     SKIP2                                                                
107690     MOVE 'S' TO POSTSUM-OPKOD                                            
107700     CALL POSTSUM USING POSTSUM-PARM                                      
107800     .                                                                    
107900     EJECT                                                                
108000                                                                          
108100 S01-LAES-W44035  SECTION.                                                
108200     SKIP2                                                                
108300     READ W44035 INTO IN-AREA                                             
108400     AT END                                                               
108500        SET END-OF-W44035 TO TRUE                                         
108600                                                                          
108700     NOT AT END                                                           
108800        MOVE 'W44035' TO POSTSUM-FDNAMN                                   
108900        MOVE 'W44038D1' TO POSTSUM-DDNAMN2                                
109000        MOVE 'UT1'     TO POSTSUM-TRANSTYP                                
109100        CALL POSTSUM USING POSTSUM-PARM                                   
109200     END-READ                                                             
109300     ADD +1 TO CHKP-ANT                                                   
109400     .                                                                    
109500     EJECT                                                                
109600                                                                          
109700 S02-SUM-SALDO-WDK7 SECTION.                                              
109800*****************************************************************         
109900*    SALDO FÖR UPPDATERING WDK7 ACKUMULERAS.                              
110000*                                                                         
110100*      W-KVROS SÄNKS FÖR ATT FÅ KORREKTA                                  
110200*      VÄRDEN INFÖR EVENTUELLT YTTERLIGARE TÄCKNINGS-OMGÅNGAR.            
110300*****************************************************************         
110400                                                                          
110500     IF ORDP-RAD-KDORDKL < 2                                              
110600        ADD   W-KVART TO   W-SUM-KVROS-DAG                                
110700     ELSE                                                                 
110800        ADD   W-KVART TO   W-SUM-KVROS-BULK                               
110900     END-IF                                                               
111000     ADD      W-KVART TO   W-SUM-KVRESS                                   
111100     .                                                                    
111200     EJECT                                                                
111300                                                                          
111400                                                                          
111500 S03-HAEMTA-WDK7-INFO SECTION.                                            
111600                                                                          
111700     SEARCH ALL DC-LAND                                                   
111800        AT END                                                            
111900           MOVE SPACE          TO W-IDLAND                                
112000        WHEN DCLAND-IDDC (DCLAND-IX) = W-IDDC-K7                          
112100           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
112200                               TO W-IDLAND                                
112300     END-SEARCH                                                           
112400                                                                          
112500     MOVE NEJ                         TO SALDO-SW                         
112600     MOVE W-IDLAND                    TO W-IDLAND-K7                      
112700     PERFORM IMS-GU-WDK712                                                
112800     IF WDK7-LART-FLREFERAL = NEJ                                         
112900        PERFORM IMS-GHU-WDK711                                            
113000                                                                          
113100        IF SEGMENT-FINNS                                                  
113200           IF WDK7-SLAG-KDLEVSP = +0 AND                                  
113300              WDK7-SLAG-FLORDSP = NEJ                                     
113400              MOVE WDK7-SLAG-KVLS          TO W-KVLS                      
113500              MOVE WDK7-SLAG-KVRESS        TO W-KVRESS                    
113600              IF WDK7-SLAG-KVRESS < ZERO                                  
113700                 MOVE ZERO                 TO W-KVRESS                    
113800              END-IF                                                      
113900              MOVE WDK7-SLAG-KVROS-DAG     TO W-KVROS-DAG                 
114000              MOVE WDK7-SLAG-KVROS-BULK    TO W-KVROS-BULK                
114100              MOVE WDK7-SLAG-KVSPARR-KVAL  TO W-KVSPARR-KVAL              
114200              MOVE WDK7-SLAG-KVUTRS        TO W-KVUTRS                    
114300              MOVE WDK7-SLAG-KVOKS-DAG     TO W-KVOKS-DAG                 
114400              MOVE WDK7-SLAG-KVOKS-BULK    TO W-KVOKS-BULK                
114500              MOVE WDK7-SLAG-KVAKS-SDC     TO W-KVAKS-SDC                 
114600              IF DCLAND-CHINA (DCLAND-IX)                                 
114700                 PERFORM IMS-GU-WDK722                                    
114800                 IF SEGMENT-FINNS                                         
114900                    MOVE WDK7-XLAG-KVSPANT TO W-KVSPANT                   
115000                 ELSE                                                     
115100                    MOVE ZERO              TO W-KVSPANT                   
115200                 END-IF                                                   
115300              END-IF                                                      
115400              MOVE JA                      TO SALDO-SW                    
115500           END-IF                                                         
115600        END-IF                                                            
115700     END-IF                                                               
115800     .                                                                    
115900     EJECT                                                                
116000                                                                          
116100 S04-SALDO-BERAKNING SECTION.                                             
116200                                                                          
116300     COMPUTE WS-DISP = W-KVLS                                             
116400                     - W-KVROS-DAG                                        
116500                     - W-KVOKS-DAG                                        
116600                     - W-KVRESS                                           
116700                     - W-KVUTRS                                           
116800                     - W-KVSPARR-KVAL                                     
116900                                                                          
117000     IF ORDP-RAD-KDORDKL > 2 AND                                          
117100        DCS-FLCLEAR-BULK = YES                                            
117200        COMPUTE W-KVLS  = W-KVLS                                          
117300                        - W-KVROS-BULK                                    
117400                        - W-KVOKS-BULK                                    
117500     END-IF                                                               
117600     .                                                                    
117700     EJECT                                                                
117800                                                                          
117900 S05-KOLLA-KVQPACK SECTION.                                               
118000                                                                          
118100     MOVE WS-DISP          TO W-DISPONIBELT                               
118200                                                                          
118300     IF ARTC-CLAG-KVQPACK-1 > +1                                          
118400       IF ARTC-ART-KDSORT = 'KG' OR 'M ' OR 'L ' OR                       
118500          ORDP-RAD-KDKVBRYT = +0                                          
118600         IF W-DISPONIBELT < ORDP-RAD-KVART                                
118700           COMPUTE W-ANTAL-ARTIKLAR ROUNDED =                             
118800             (W-DISPONIBELT / ARTC-CLAG-KVQPACK-1) - 0.5                  
118900           COMPUTE W-DISPONIBELT ROUNDED =                                
119000              W-ANTAL-ARTIKLAR * ARTC-CLAG-KVQPACK-1                      
119100         END-IF                                                           
119200       END-IF                                                             
119300     END-IF                                                               
119400     .                                                                    
119500     EJECT                                                                
119600                                                                          
119700*S06-KOLLA-NAESTA-DC SECTION.                                             
119800*                                                                         
119900*    MOVE NEJ              TO SALDO-SW                                    
120000*                                                                         
120100*    PERFORM S03-HAEMTA-WDK7-INFO                                         
120200*    IF SALDO-FINNS                                                       
120300*      PERFORM S04-SALDO-BERAKNING                                        
120400*      IF WS-DISP > ZERO                                                  
120500*        PERFORM S05-KOLLA-KVQPACK                                        
120600*        IF W-DISPONIBELT >= ORDP-RAD-KVART                               
120700*          MOVE JA         TO DISPONIBELT-SW                              
120800*        END-IF                                                           
120900*      END-IF                                                             
121000*    END-IF                                                               
121100*    .                                                                    
121200*    EJECT                                                                
121300                                                                          
121400 S07-NOLLSTAELL-FAELT SECTION.                                            
121500                                                                          
121600     MOVE +0          TO W-DISPONIBELT                                    
121700                         W-KVLS                                           
121800                         W-KVRESS                                         
121900                         W-KVROS-DAG                                      
122000                         W-KVROS-BULK                                     
122100                         W-KVSPARR-KVAL                                   
122200                         W-KVUTRS                                         
122300                         W-KVOKS-DAG                                      
122400                         W-KVOKS-BULK                                     
122500                         W-SUM-KVROS-DAG                                  
122600                         W-SUM-KVROS-BULK                                 
122700                         W-SUM-KVRESS                                     
122800                                                                          
122900     MOVE +0          TO WS-DISP                                          
123000     .                                                                    
123100     EJECT                                                                
123200                                                                          
123300 S08-SALDO-BERAKNING-CN SECTION.                                          
123400                                                                          
123500     COMPUTE WS-DISP = W-KVLS                                             
123600                     + W-KVAKS-SDC                                        
123700                     - W-KVOKS-DAG                                        
123800                     - W-KVOKS-BULK                                       
123900                     - W-KVRESS                                           
124000                     - W-KVUTRS                                           
124100                     - W-KVSPARR-KVAL                                     
124200     .                                                                    
124300     EJECT                                                                
124400                                                                          
124500 J-TAG-CHECKPOINT SECTION.                                                
124600                                                                          
124700     PERFORM IMS-CHECKPOINT                                               
124800     MOVE    ZERO  TO CHKP-ANT                                            
124900     .                                                                    
125000     EJECT                                                                
125100*S08-ORDP-STPOS-WDA5ASEQ SECTION.                                         
125200*****************************************************************         
125300*                                                                         
125400*    POSITIONERAR FÖRE ARTIKELNS FÖRSTA WDA5ASEQ                          
125500*    GENOM ATT LÄSA GU MED IDARTNR + RESTEN LOW-VALUE.                    
125600*    NÖDVÄNDIGT EFTERSOM FLERA TÄCKNINGS-OMGÅNGAR KAN GÖRAS.              
125700*                                                                         
125800*****************************************************************         
125900                                                                          
126000*    MOVE LOW-VALUE              TO W1-WDA5ASEQ-X                         
126100*    MOVE IN-IDARTNR             TO W1-IDARTNR                            
126200*    MOVE IN-IDDC                TO W1-IDDC                               
126300                                                                          
126400*    PERFORM IMS-GU-ORDP-STPOS-ASEQ                                       
126500*    .                                                                    
126600*    EJECT                                                                
126700                                                                          
126800* --- IMS SEKTIONER ---                                                   
126900     SKIP3                                                                
127000 IMS-RESTART SECTION.                                                     
127100                                                                          
127200     MOVE SPACE TO MSG-IO-AREA                                            
127300     MOVE '  '  TO GODK-STATUSKODER                                       
127400     CALL CBLTDLI USING XRST MSG-PCB                                      
127500                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
127600                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
127700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
127800     PERFORM IMS-STATUSKONTROLL                                           
127900     IF IMS-EJ-OK                                                         
128000        CALL FELLOG                                                       
128100     END-IF                                                               
128200     .                                                                    
128300     SKIP1                                                                
128400 IMS-CHECKPOINT SECTION.                                                  
128500                                                                          
128600     MOVE CHKP-ID TO MSG-IO-AREA                                          
128700     MOVE '  XD' TO GODK-STATUSKODER                                      
128800     CALL CBLTDLI USING CHKP MSG-PCB                                      
128900                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
129000                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
129100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
129200     PERFORM IMS-STATUSKONTROLL                                           
129300     IF IMS-EJ-OK                                                         
129400        CALL FELLOG                                                       
129500     END-IF                                                               
129600     .                                                                    
129700     SKIP1                                                                
129800 IMS-GU-ARTC01 SECTION.                                                   
129900                                                                          
130000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-K6-X ')'                      
130100            DELIMITED BY SIZE INTO SSA1                                   
130200     MOVE '  GE'                TO GODK-STATUSKODER                       
130300     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-WLARTC01 SSA1                 
130400     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
130500     PERFORM IMS-STATUSKONTROLL                                           
130600     .                                                                    
130700     SKIP2                                                                
130800 IMS-GNP-ARTC11 SECTION.                                                  
130900     MOVE 'WLARTC11 '           TO SSA1                                   
131000     MOVE '  GE'                TO GODK-STATUSKODER                       
131100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
131200     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
131300     PERFORM IMS-STATUSKONTROLL                                           
131400     .                                                                    
131500     EJECT                                                                
131600 IMS-GHU-WDK711 SECTION.                                                  
131700                                                                          
131800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
131900          DELIMITED BY SIZE INTO SSA1                                     
132000     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
132100          DELIMITED BY SIZE INTO SSA2                                     
132200     MOVE '  GE' TO GODK-STATUSKODER                                      
132300     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
132400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
132500     PERFORM IMS-STATUSKONTROLL                                           
132600     .                                                                    
132700     SKIP2                                                                
132800 IMS-REPLACE-WDK711 SECTION.                                              
132900                                                                          
133000     MOVE '  ' TO GODK-STATUSKODER                                        
133100     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
133200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
133300     PERFORM IMS-STATUSKONTROLL                                           
133400     .                                                                    
133500     EJECT                                                                
133600 IMS-GU-WDK722 SECTION.                                                   
133700                                                                          
133800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
133900          DELIMITED BY SIZE INTO SSA1                                     
134000     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
134100          DELIMITED BY SIZE INTO SSA2                                     
134200     MOVE 'WDK722 '           TO SSA3                                     
134300     MOVE '  GE' TO GODK-STATUSKODER                                      
134400     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
134500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
134600     PERFORM IMS-STATUSKONTROLL                                           
134700     .                                                                    
134800     SKIP2                                                                
134900 IMS-GU-WDK712 SECTION.                                                   
135000                                                                          
135100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
135200          DELIMITED BY SIZE INTO SSA1                                     
135300     STRING 'WDK712  (IDLAND   =' W-IDLAND-K7-X ')'                       
135400          DELIMITED BY SIZE INTO SSA2                                     
135500     MOVE '  GE'              TO GODK-STATUSKODER                         
135600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
135700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
135800     PERFORM IMS-STATUSKONTROLL                                           
135900     IF SEGMENT-SAKNAS                                                    
136000        MOVE NEJ TO WDK7-LART-FLREFERAL                                   
136100     END-IF                                                               
136200     .                                                                    
136300*IMS-GU-ORDP-STPOS-ASEQ SECTION.                                          
136400                                                                          
136500*    STRING 'WLORDP01(WDA5ASEQ =' W1-WDA5ASEQ-X ')'                       
136600*           DELIMITED BY SIZE INTO SSA1                                   
136700*    MOVE '  GE'              TO GODK-STATUSKODER                         
136800*    CALL CBLTDLI USING GU ORDP-PCB DLI-IO-WLORDP01 SSA1                  
136900*    MOVE ORDP-STATUS-CODE      TO STATUS-WS                              
137000*    PERFORM IMS-STATUSKONTROLL                                           
137100*    .                                                                    
137200     SKIP2                                                                
137300 IMS-GHN-ORDQ01-ORDP01 SECTION.                                           
137400                                                                          
137500     STRING 'WLORDP01(WDA5ASEQ >' W1-WDA5ASEQ-X                           
137600                    '&WDA5ASEQ <' W2-WDA5ASEQ-X                           
137700                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
137800            DELIMITED BY SIZE INTO SSA1                                   
137900     MOVE '  GE'              TO GODK-STATUSKODER                         
138000     CALL CBLTDLI USING GHN ORDP-PCB DLI-IO-WLORDP01 SSA1                 
138100     MOVE ORDP-STATUS-CODE      TO STATUS-WS                              
138200     PERFORM IMS-STATUSKONTROLL                                           
138300     .                                                                    
138400     SKIP2                                                                
138500                                                                          
138600 IMS-REPLACE-ORDQ01-ORDP01 SECTION.                                       
138700                                                                          
138800     MOVE '  '               TO GODK-STATUSKODER                          
138900     CALL CBLTDLI USING REPL  ORDP-PCB DLI-IO-WLORDP01                    
139000     MOVE ORDP-STATUS-CODE   TO STATUS-WS                                 
139100     PERFORM IMS-STATUSKONTROLL                                           
139200     .                                                                    
139300     EJECT                                                                
139400 IMS-GU-ORQL-WDQ201 SECTION.                                              
139500                                                                          
139600     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
139700          DELIMITED BY SIZE INTO SSA1                                     
139800     MOVE '  GE'               TO GODK-STATUSKODER                        
139900     CALL CBLTDLI USING GU ORQL-PCB DLI-IO-WLORQI01 SSA1                  
140000     MOVE ORQL-STATUS-CODE    TO STATUS-WS                                
140100     PERFORM IMS-STATUSKONTROLL                                           
140200     .                                                                    
140300     SKIP2                                                                
140400 IMS-ISRT-4505-4506 SECTION.                                              
140500                                                                          
140600     STRING 'WL450501(WDGXKEY  =' W-IDHTYP-4505-X ')'                     
140700            DELIMITED BY SIZE INTO SSA1                                   
140800     MOVE 'WL450511 ' TO SSA2                                             
140900     MOVE '  ' TO GODK-STATUSKODER                                        
141000     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-WL450501 SSA1 SSA2           
141100     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
141200     PERFORM IMS-STATUSKONTROLL                                           
141300     .                                                                    
141400     EJECT                                                                
141500                                                                          
141600 IMS-GU-ORQM-WDQ101 SECTION.                                              
141700                                                                          
141800     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
141900                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
142000          DELIMITED BY SIZE INTO SSA1                                     
142100     MOVE '  GE'               TO GODK-STATUSKODER                        
142200     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-WLORQM01 SSA1                
142300     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
142400     PERFORM IMS-STATUSKONTROLL                                           
142500     .                                                                    
142600     SKIP2                                                                
142700 IMS-GN-ORQM-WDQ101 SECTION.                                              
142800                                                                          
142900     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
143000                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
143100          DELIMITED BY SIZE INTO SSA1                                     
143200     MOVE '  GEGB'             TO GODK-STATUSKODER                        
143300     CALL CBLTDLI USING GN   ORQM-PCB DLI-IO-WLORQM01 SSA1                
143400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
143500     PERFORM IMS-STATUSKONTROLL                                           
143600     .                                                                    
143700     SKIP2                                                                
143800 IMS-ISRT-ORQM-WDQ101 SECTION.                                            
143900                                                                          
144000     MOVE 'WLORQM01 '          TO SSA1                                    
144100     MOVE '    '               TO GODK-STATUSKODER                        
144200     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-WLORQM01 SSA1                
144300     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
144400     PERFORM IMS-STATUSKONTROLL                                           
144500     .                                                                    
144600     EJECT                                                                
144700 IMS-GU-WDB601 SECTION.                                                   
144800                                                                          
144900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
145000          DELIMITED BY SIZE INTO SSA1                                     
145100     MOVE '  GE'              TO GODK-STATUSKODER                         
145200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
145300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
145400     PERFORM IMS-STATUSKONTROLL                                           
145500     .                                                                    
145600     EJECT                                                                
145610 IMS-GU-WDB201 SECTION.                                                   
145620                                                                          
145630     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
145640          DELIMITED BY SIZE INTO SSA1                                     
145650     MOVE '    '              TO GODK-STATUSKODER                         
145660     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
145670     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
145680     PERFORM IMS-STATUSKONTROLL                                           
145690     .                                                                    
145691     EJECT                                                                
145700 IMS-STATUSKONTROLL SECTION.                                              
145800     SKIP2                                                                
145900     SET STATUS-IX TO 1                                                   
146000     SEARCH GODK-STATUS                                                   
146100       AT END                                                             
146200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
146300           DELIMITED BY SIZE INTO FELTEXT                                 
146400         DISPLAY FELTEXT                                                  
146500         CALL FELLOG                                                      
146600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
146700         CONTINUE                                                         
146800     END-SEARCH                                                           
146900     .                                                                    
