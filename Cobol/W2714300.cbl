000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2714300.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   96/03/04.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        LÄSER FIL MED REFILLARTIKLAR SOM ERSÄTTNINGS-PASSIVERATS.        
001100*        ÖVERFÖR DEN ERSATTA ARTIKELNS PB TILL DEN TILLKOMMANDE           
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDK7                                       
001500*        PROGRAMMET UPPDATERAR WLCKPA (WDR5) ÅTERSTART                    
001600*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000     SELECT W27143                     ASSIGN TO W27143D1.                
003100*          --- ERSÄTTNINGSPASSIVERADE REFILLARTIKLAR                      
003200     SELECT W27131                     ASSIGN TO W27143D2.                
003300*          --- MEMO DÅ TILLKOMMANDE ARTIKEL I SIN TUR ÄR                  
003400*          --- RENSAD ELLER ERSATT                                        
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W27143                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W27136      -L.                                                
004800     SKIP3                                                                
004900                                                                          
005000 FD  W27131                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY W27137 -PRE  W27131-  -L.                                 
005500     EJECT                                                                
005600                                                                          
006300 WORKING-STORAGE SECTION.                                                 
006400     SKIP2                                                                
006500                                                                          
006600*    -- CHECKED BY WY2000                                                 
006700 77  IDPGM                       PIC X(8)    VALUE 'W2714300'.            
006800 01  CHKP-VAR.                                                            
006900 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
007000 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
007100 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
007200 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
007300 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
007400 03  CHKP-MAX                    PIC S9(3)   VALUE +50.                   
007500 77  JA                          PIC X       VALUE 'J'.                   
007600 77  NEJ                         PIC X       VALUE 'N'.                   
007700 77  PASSIV                      PIC X       VALUE 'P'.                   
007800 77  AKTIV                       PIC X       VALUE 'A'.                   
007900                                                                          
008000 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
008100     88  OMSTART                             VALUE 'J'.                   
008200       EJECT                                                              
008300     SKIP2                                                                
008400 01  ARBETSAREOR.                                                         
008500     03 WS-FLREFBEO-OLD          PIC X(1)    VALUE SPACE.                 
008600     03 WS-FLWILSON-OLD          PIC X(1)    VALUE SPACE.                 
008700     03 WS-FLFLYG-OLD            PIC X(1)    VALUE SPACE.                 
008800     03 WS-FLREFILL-OLD          PIC X(1)    VALUE SPACE.                 
008900     03 WS-IDPERSON-BUY-OLD   PIC S9(3)      VALUE ZERO COMP-3.           
009000     03 WS-KVPB-REF-OLD       PIC S9(6)V9(1) VALUE ZERO COMP-3.           
009100     03 WS-KVPBREOI-OLD       PIC S9(6)V9(1) VALUE ZERO COMP-3.           
009200     03 WS-KVPB-REF-ADD       PIC S9(6)V9(1) VALUE ZERO COMP-3.           
009300     03 WS-KVPBREOI-ADD       PIC S9(6)V9(1) VALUE ZERO COMP-3.           
009400     03 WS-KVPB-TOT           PIC S9(6)V9(1) VALUE ZERO COMP-3.           
009500     03 WS-KVDAGAR-TACKT-LAST-RP PIC 9(3)    VALUE ZERO.                  
009600     03 WS-KVPB-IX-RP         PIC S9(7)V9(5) VALUE ZERO COMP-3.           
009700     03 IX                       PIC 9(2)    VALUE ZERO.                  
009800     03 IX2                      PIC 9(2)    VALUE ZERO.                  
009900     03 INDX                     PIC 9(3)    VALUE ZERO.                  
010000     03 WS-VECKA                 PIC 9(2)    VALUE ZERO.                  
010100     03 FIRST-VV-INNEV-RP        PIC 9(2)    VALUE ZERO.                  
010200     03 WS-KVPOST-IN             PIC S9(7)   VALUE ZERO.                  
010300     03 W-IDARTNR-SPAR           PIC S9(9)   VALUE ZERO COMP-3.           
010301     03 W-IDARTNR-OLD            PIC S9(9)   VALUE ZERO COMP-3.           
010310     03 WS-DAGENS-AAVV-NEXT      PIC 9(4)    VALUE ZERO.                  
010313     03 WS-TIPBJUST              PIC 9(4)    VALUE ZERO.                  
010314     03 WS-TIPBJUST-1            PIC 9(4)    VALUE ZERO.                  
010320     03 WS-TIPBJUST-2            PIC 9(4)    VALUE ZERO.                  
010330     03 WS-KVPB-JUST-1           PIC S9(6)V9(1) VALUE ZERO COMP-3.        
010340     03 WS-KVPB-JUST-2           PIC S9(6)V9(1) VALUE ZERO COMP-3.        
010350     03 WS-DIERS-TILLK           PIC 9(4)V9(3)  VALUE ZERO.               
010360     03 WS-DIERS-ERS             PIC 9(4)V9(3)  VALUE ZERO.               
010370     03 WS-RATIO                 PIC 9(4)V9(3)  VALUE ZERO.               
010400                                                                          
010500     03 WS-SEASON-OLD OCCURS 12.                                          
010600        05 WS-RESEASON-OLD       PIC S9(1)V9(2) VALUE ZERO.               
010700                                                                          
010800     03 WS-RP-FIRST-TIAARP   PIC 9(4)       VALUE ZERO.                   
010900     03 WS-RP-FIRST-GRP    REDEFINES WS-RP-FIRST-TIAARP.                  
011000        05 WS-RP-FIRST-TIAA  PIC 9(2).                                    
011100        05 WS-RP-FIRST-TIRP  PIC 9(2).                                    
011200                                                                          
011300     03 WS-RP-NEXT-TIAARP    PIC 9(4)       VALUE ZERO.                   
011400     03 WS-RP-NEXT-GRP     REDEFINES WS-RP-NEXT-TIAARP.                   
011500        05 WS-RP-NEXT-TIAA   PIC 9(2).                                    
011600        05 WS-RP-NEXT-TIRP   PIC 9(2).                                    
011700                                                                          
011800     03 WS-RP-FIRST-TIAAMMDD PIC 9(6)       VALUE ZERO.                   
011900     03 WS-RP-NEXT-TIAAMMDD  PIC 9(6)       VALUE ZERO.                   
012000                                                                          
012100     03 WS-KVDISP-AKT-NDC    PIC S9(7) VALUE ZERO COMP-3.                 
012200     03 WS-KVDISP-REST       PIC S9(7) VALUE ZERO COMP-3.                 
012300     03 WS-KVDISP-REST-SPAR  PIC S9(7) VALUE ZERO COMP-3.                 
012400     03 WS-KVPB-DAG-IX-RP    PIC S9(6)V9(5) VALUE ZERO COMP-3.            
012500     03 WS-KVPB-RP           PIC S9(6)V9(5) VALUE ZERO COMP-3.            
012600     03 WS-KVARBD-IX-RP      PIC S9(3)      VALUE ZERO COMP-3.            
012700     03 WS-TIREFMPB          PIC S9(7)      VALUE ZERO COMP-3.            
012800                                                                          
012900 01  FELTEXT.                                                             
013000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013200                                                                          
013300 77  W27143-EOF-SW               PIC X       VALUE 'N'.                   
013400     88  END-OF-W27143                       VALUE 'J'.                   
013500                                                                          
013600 77  SEASON-SW               PIC X           VALUE 'N'.                   
013700     88  SEASON-FINNS                        VALUE 'J'.                   
013800     88  SEASON-SAKNAS                       VALUE 'N'.                   
013900                                                                          
013910 77  HERITAGE-SW             PIC X           VALUE 'J'.                   
013920     88  HERITAGE                            VALUE 'J'.                   
013930     88  NO-HERITAGE                         VALUE 'N'.                   
013940                                                                          
014210 77  WDK727-SW                PIC X          VALUE 'N'.                   
014220     88  WDK727-UPD                          VALUE 'J'.                   
014230                                                                          
014240 77  PURCH-SW                 PIC X          VALUE 'N'.                   
014250     88  PURCH-JA                            VALUE 'J'.                   
014260                                                                          
014300 77  IDPERSON-UPD-SW          PIC X          VALUE 'N'.                   
014400     88  IDPERSON-UPD-JA                     VALUE 'J'.                   
014500                                                                          
014600 77  SW-ISRT-K711             PIC X          VALUE 'N'.                   
014610     88  SW-ISRT-K711-JA                     VALUE 'J'.                   
014620                                                                          
014700     EJECT                                                                
014701 01  WS-TISTOREF                 PIC 9(5)    VALUE ZERO.                  
014702 01  FILLER REDEFINES WS-TISTOREF.                                        
014710     03 WS-TISTOREF-AAVV         PIC 9(4).                                
014720     03 WS-TISTOREF-D            PIC 9(1).                                
014730                                                                          
014800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014900 01  FILLER REDEFINES DAGENS-DATUM.                                       
015000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
015100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015300                                                                          
015400 01  DAGENS-TIAAVVD              PIC 9(5)    VALUE ZERO.                  
015500 01  FILLER REDEFINES DAGENS-TIAAVVD.                                     
015600     03 DAGENS-TIAAVV            PIC 9(4).                                
015700     03 DAGENS-TID               PIC 9(1).                                
015800                                                                          
015900 01  WS-TIAAVVD                  PIC 9(5)    VALUE ZERO.                  
016000 01  FILLER REDEFINES WS-TIAAVVD.                                         
016100     03 WS-TIAAVV                PIC 9(4).                                
016200     03 WS-TID                   PIC 9(1).                                
016300                                                                          
016400 01  INNEV-TIAARP                PIC 9(4)    VALUE ZERO.                  
016500 01  FILLER REDEFINES INNEV-TIAARP.                                       
016600     03 INNEV-TIAA               PIC 9(2).                                
016700     03 INNEV-TIRP               PIC 9(2).                                
016800                                                                          
016900 01  WS-TIAAVVD                  PIC 9(5)    VALUE ZERO.                  
017000 01  FILLER REDEFINES WS-TIAAVVD.                                         
017100     03 WS-TIAAVV                PIC 9(4).                                
017200     03 WS-TID                   PIC 9(1).                                
017300                                                                          
017400 01  DD-PLUS-TVA-AR-TISSAAMMDD.                                           
017500     03  DD-PLUS-TVA-AR-TISS     PIC 9(2)    VALUE ZERO.                  
017600     03  DD-PLUS-TVA-AR-TIAAMMDD PIC 9(6)    VALUE ZERO.                  
017700                                                                          
017800 01  DYNAMISKA-SUBPROGRAM.                                                
017900*                                                                         
018000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
018300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
018400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018500     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
018600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
018700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
018800     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
018910     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
019000     EJECT                                                                
019100*01  -COPY WWDCKONS                                                       
019200     EJECT                                                                
019300*01  -COPY WWDC99                                                         
019400     EJECT                                                                
019500*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
019600 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
019700*   -COPY W005WDK7                                                        
019800     EJECT                                                                
020200*    --- PARAMETRAR TILL DATKORT                                          
020300*                                                                         
020400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27143'.              
020500     SKIP2                                                                
020600*    --- PARAMETRAR TILL ABEND                                            
020700                                                                          
020800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
020900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021000     EJECT                                                                
021100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
021200     SKIP2                                                                
021300*01  -COPY WDATKORT                                                       
021400     EJECT                                                                
021500*    --- PARAMETRAR TILL DATKONV                                          
021600*                                                                         
021700*01  -COPY WDATAREA                                                       
021800     EJECT                                                                
021900*    --- PARAMETRAR TILL DAGKONV                                          
022000*                                                                         
022100*01  -COPY WDAGAREA                                                       
022200     EJECT                                                                
022300*    --- PARAMETRAR TILL WORKDAY                                          
022400*                                                                         
022500*01  -COPY WORKAREA                                                       
022600     EJECT                                                                
022610*    --- PARAMETRAR TILL VECKOADD                                         
022620                                                                          
022630 01  W009VADD-AREA.                                                       
022640     03 VADD-DATUM-AAVV          PIC S9(5) VALUE ZERO COMP-3.             
022650     03 VADD-ANTAL               PIC S9(3) VALUE ZERO COMP-3.             
022660                                                                          
022700*    --- PARAMETRAR TILL POSTSUM                                          
022800*                                                                         
022900*01  -COPY W0005   -PRE  POSTSUM-                                         
023000     EJECT                                                                
023100 01  W27143-AREA-START           PIC X(24)   VALUE                        
023200                                             'W27143-AREA-START'.         
023300     SKIP2                                                                
023400                                                                          
023500*01  AREA -COPY W27136     -PRE W27143-                                   
023600*                                                                         
023700     EJECT                                                                
023800                                                                          
023900                                                                          
024000 01  W27137-AREA-START           PIC X(24)   VALUE                        
024100                                             'W27137-AREA-START'.         
024200     SKIP2                                                                
024300                                                                          
024400*01  AREA -COPY W27137     -PRE W27131-                                   
024500*                                                                         
024600     EJECT                                                                
024800                                                                          
025600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025700     SKIP3                                                                
025800 01  NYCKLAR-TILL-DLI.                                                    
025900     03  W-IDARTNR-X.                                                     
026000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
026100     03  W-KDSEGKEY-X.                                                    
026200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
026300     03  W-IDDC-X.                                                        
026400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
026500     03  W-IDDC-B6-X.                                                     
026600         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
026700     03  W-IDDC-MIN-X.                                                    
026800         05  W-IDDC-MIN          PIC X(2)   VALUE SPACE.                  
026900     03  W-IDDC-MAX-X.                                                    
027000         05  W-IDDC-MAX          PIC X(2)   VALUE SPACE.                  
027100     03  W-IDKORTNR-X.                                                    
027200         05  W-IDKORTNR          PIC S9(2)   VALUE ZERO COMP-3.           
027300     03  W-WDGX-2249-KEY-X.                                               
027400         05  W-IDHTYP-2249       PIC X(04)   VALUE '2249'.                
027500         05  W-IDPGM             PIC X(08)   VALUE 'W2714300'.            
027600         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
027700     03  W-WDGX-2250-KEY-X.                                               
027800         05  FILLER              PIC X       VALUE '1'.                   
027900     SKIP2                                                                
028000*    --- STATUS-KOD FRÅN IMS                                              
028100 01  STATUS-WS                   PIC XX.                                  
028200     88  SEGMENT-FINNS                       VALUE '  '.                  
028300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
028600     88  IMS-EJ-OK                           VALUE 'XD'.                  
028700     SKIP2                                                                
028800 01  GODK-STATUSKODER.                                                    
028900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029000     SKIP3                                                                
029100 01  SSA1                        PIC X(128).                              
029200 01  SSA2                        PIC X(128).                              
029210 01  SSA3                        PIC X(128).                              
029300     EJECT                                                                
029400*    --- IMS FUNKTIONSKODER                                               
029500*01  -COPY W0003                                                          
029600     EJECT                                                                
029700*    ---  DLI INPUT-OUTPUT AREA                                           
029800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
029900 01  DLI-IO-WDK701.                                                       
030000*    03  -COPY WDK701                                                     
030100     EJECT                                                                
030200                                                                          
030300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
030400 01  DLI-IO-WDK711.                                                       
030500*    03  -COPY WDK711                                                     
030600     EJECT                                                                
030700                                                                          
030710 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK727'.                      
030720 01  DLI-IO-WDK727.                                                       
030730*    03  -COPY WDK727                                                     
030740     EJECT                                                                
030750                                                                          
031300 01  FILLER         PIC X(24) VALUE 'DLI-IO-ERSA01'.                      
031400 01  DLI-IO-ERSA01.                                                       
031500*    03  -COPY WDD701    -PRE ERSA01-                                     
031600     EJECT                                                                
031700                                                                          
031800 01  FILLER         PIC X(24) VALUE 'DLI-IO-ERSA11'.                      
031900 01  DLI-IO-ERSA11.                                                       
032000*    03  -COPY WDD702    -PRE ERSA11-                                     
032100     EJECT                                                                
032200                                                                          
032300 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01'.                      
032400 01  DLI-IO-ARTC01.                                                       
032500*    03  -COPY WDK601                                                     
032600     EJECT                                                                
032700                                                                          
032800 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11'.                      
032900 01  DLI-IO-ARTC11.                                                       
033000*    03  -COPY WDK611                                                     
033100     EJECT                                                                
033200                                                                          
033300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLCKPA'.                      
033400 01  DLI-IO-WLCKPA.                                                       
033500*    03  -COPY WDGX2250 -PRE CKPA-                                        
033600     EJECT                                                                
033700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033800 01   DLI-IO-AREA-B601.                                                   
033900*     03  -COPY WDB601                                                    
034000                                                                          
034100 LINKAGE SECTION.                                                         
034200                                                                          
034300*01  -COPY W0009   -PRE MSG-                                              
034400     EJECT                                                                
034500*01  -COPY W0008  -PRE WDK7-                                              
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
035100*01  -COPY W0008  -PRE ERSA-                                              
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01  -COPY W0008  -PRE ARTC-                                              
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008  -PRE CKPA-                                              
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0008  -PRE WDB6-                                              
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036210*01  -COPY W0008  -PRE W005-WDB6-                                         
036220     05  FILLER                  PIC X.                                   
036230     EJECT                                                                
036240*01  -COPY W0008  -PRE W005-WDK6-                                         
036250     05  FILLER                  PIC X.                                   
036260     EJECT                                                                
036270*01  -COPY W0008  -PRE W005-WDK7-                                         
036280     05  FILLER                  PIC X.                                   
036290     EJECT                                                                
036300 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB                               
036400                           ERSA-PCB ARTC-PCB                              
036500                           CKPA-PCB WDB6-PCB                              
036510                           W005-WDB6-PCB W005-WDK6-PCB                    
036520                           W005-WDK7-PCB.                                 
036600 MAIN SECTION.                                                            
036700     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB                               
036800                           ERSA-PCB ARTC-PCB                              
036900                           CKPA-PCB WDB6-PCB                              
036910                           W005-WDB6-PCB W005-WDK6-PCB                    
036920                           W005-WDK7-PCB.                                 
037000                                                                          
037100     SKIP2                                                                
037200     PERFORM A-INIT                                                       
037300     IF NOT OMSTART                                                       
037400        PERFORM S01-LAES-W27143                                           
037500     END-IF                                                               
037600     PERFORM UNTIL END-OF-W27143                                          
037700        IF CHKP-ANT > CHKP-MAX                                            
037800          PERFORM X-TAG-CHECKPOINT                                        
037900        END-IF                                                            
038000        MOVE W27143-IDARTNR    TO W-IDARTNR                               
038010                                  W-IDARTNR-OLD                           
038100        MOVE W27143-IDDC       TO W-IDDC                                  
038200                                  W-IDDC-B6                               
038300        PERFORM IMS-GU-WDB601                                             
038400          IF W27143-KDREFSTA = 'A'                                        
038900            IF (W27143-KDERS = 02 OR 03 OR 05 OR 06 OR                    
039000                               22 OR 23 OR 25 OR 26)                      
039010              IF W27143-KDERS = 05 OR 06 OR 25 OR 26                      
039011                CONTINUE                                                  
039200              ELSE                                                        
039500                PERFORM B-BEHANDLA-BEORDRAD-ARTIKEL                       
039620                PERFORM C-BEHANDLA-TILLKOMMANDE                           
040910              END-IF                                                      
040920            END-IF                                                        
041000          END-IF                                                          
041300        PERFORM S01-LAES-W27143                                           
041400     END-PERFORM                                                          
041500                                                                          
041600                                                                          
041700     PERFORM Z-FINIT                                                      
041800                                                                          
041900     MOVE ZERO TO RETURN-CODE                                             
042000     GOBACK                                                               
042100     .                                                                    
042200     EJECT                                                                
042300                                                                          
042400                                                                          
042500                                                                          
042600                                                                          
042700                                                                          
042800                                                                          
042900 A-INIT SECTION.                                                          
043000     SKIP2                                                                
043100                                                                          
043200     OPEN INPUT  W27143                                                   
043300          OUTPUT W27131                                                   
043500                                                                          
043600     PERFORM IMS-RESTART                                                  
043700                                                                          
043800     PERFORM IMS-LAS-ATERSTART                                            
043900                                                                          
044000     IF SEGMENT-FINNS                                                     
044100        IF CKPA-2250-KVPOST     > ZERO                                    
044200           PERFORM AA-ATERSTART-EFTER-ABEND                               
044300           MOVE JA               TO OMSTART-SW                            
044400         ELSE                                                             
044500           MOVE NEJ              TO OMSTART-SW                            
044600        END-IF                                                            
044700     END-IF                                                               
044800                                                                          
044900     MOVE ZERO                 TO CHKP-ANT                                
045000                                                                          
045100                                                                          
045200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
045300                                                                          
045400     ACCEPT DAGENS-DATUM FROM DATE                                        
045500                                                                          
045600*    HÄMTA INNEVARANDE ÅR-VECKA                                           
045700*                      R-PERIOD                                           
045800*                      ANTAL VECKOR I PERIODEN                            
045900*                                                                         
046000*                                                                         
046100     MOVE DAGENS-DATUM   TO DAT-I-TIDATUM                                 
046200     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
046300     CALL WDATKONV USING DAT-KDDATFORM                                    
046400                         DAT-I-TIDATUM                                    
046500                         DAT-O-TIDATUM                                    
046600                         DAT-KDSVAR                                       
046700                                                                          
046800     IF DAT-KDSVAR-OK                                                     
046900        MOVE DAT-TIAAVVD     TO DAGENS-TIAAVVD                            
047000        MOVE DAT-TIAARP      TO INNEV-TIAARP                              
047100     ELSE                                                                 
047200        MOVE 'FEL FRÅN WDATKONV 1  I A-INIT SECTION I W27143' TO          
047300                                    FELTEXT-STR                           
047400        DISPLAY FELTEXT                                                   
047500        PERFORM S99-ABEND                                                 
047600     END-IF                                                               
047700                                                                          
047800*HÄMTA FÖRSTA VECKAN I INNEVARANDE R-PERIOD                               
047900                                                                          
048000     MOVE 'AARP'           TO DAT-KDDATFORM                               
048100     MOVE INNEV-TIAARP     TO DAT-I-TIDATUM                               
048200     CALL WDATKONV USING   DAT-KDDATFORM                                  
048300                           DAT-I-TIDATUM                                  
048400                           DAT-O-TIDATUM                                  
048500                           DAT-KDSVAR                                     
048600                                                                          
048700     IF DAT-KDSVAR-OK                                                     
048800        MOVE DAT-TIVV    TO FIRST-VV-INNEV-RP                             
048900     ELSE                                                                 
049000        MOVE 'FEL FRÅN WDATKONV 2  I A-INIT SECTION I W27143' TO          
049100                                    FELTEXT-STR                           
049200        DISPLAY FELTEXT                                                   
049300        PERFORM S99-ABEND                                                 
049400     END-IF                                                               
049500                                                                          
049510     MOVE DAGENS-TIAAVV        TO VADD-DATUM-AAVV                         
049520     MOVE +1                   TO VADD-ANTAL                              
049530     CALL W009VADD USING VADD-DATUM-AAVV VADD-ANTAL                       
049540                                                                          
049550     MOVE VADD-DATUM-AAVV      TO WS-DAGENS-AAVV-NEXT                     
049560                                                                          
049600     MOVE FUNCTION CURRENT-DATE (1:2) TO                                  
049700                               DAG-TISEKEL-FOM                            
049800     MOVE DAGENS-DATUM      TO DAG-TIAAMMDD-FOM                           
049900     MOVE 002               TO DAG-KDCALL                                 
050000     MOVE 730               TO DAG-KVKALDAG                               
050100                                                                          
050200     CALL WDAGKONV USING DAG-KDCALL                                       
050300                         DAG-DATUM-AREA                                   
050400                         DAG-KDSVAR                                       
050500     IF DAG-KDSVAR = SPACE                                                
050600        MOVE DAG-TISEKEL-TOM  TO DD-PLUS-TVA-AR-TISS                      
050700        MOVE DAG-TIAAMMDD-TOM TO DD-PLUS-TVA-AR-TIAAMMDD                  
050800     ELSE                                                                 
050900        MOVE 'FEL FRÅN WDAGKONV 1  I A-INIT SECTION I W27143' TO          
051000                                    FELTEXT-STR                           
051100        DISPLAY FELTEXT                                                   
051200        PERFORM S99-ABEND                                                 
051300     END-IF                                                               
051400     .                                                                    
051500     EJECT                                                                
051600                                                                          
051700                                                                          
051800 AA-ATERSTART-EFTER-ABEND SECTION.                                        
051900                                                                          
052000     PERFORM UNTIL WS-KVPOST-IN    = CKPA-2250-KVPOST  OR                 
052100                                            END-OF-W27143                 
052200       PERFORM S01-LAES-W27143                                            
052300     END-PERFORM                                                          
052400     .                                                                    
052500     EJECT                                                                
052600                                                                          
052700                                                                          
052800 B-BEHANDLA-BEORDRAD-ARTIKEL SECTION.                                     
052900                                                                          
053000     PERFORM BA-NOLLSTALL-ARTIKEL-LAGER                                   
053100                                                                          
053200     PERFORM IMS-GHU-ARTC11                                               
053300     IF SEGMENT-FINNS                                                     
053400        IF W27143-TIREFSTO-CLAG > +0                                      
053500           MOVE ZERO   TO CLAG-TIREFSTO                                   
053600        END-IF                                                            
053700        PERFORM IMS-REPL-ARTC                                             
053710        MOVE CLAG-TISTOREF TO WS-TISTOREF                                 
053800     END-IF                                                               
053900                                                                          
054000     PERFORM IMS-GHU-WDK711                                               
054010                                                                          
054020                                                                          
054100     IF SEGMENT-FINNS                                                     
054110        IF W27143-TIREFSTO-SLAG > +0                                      
054120           MOVE ZERO   TO SLAG-TIREFSTO                                   
054130        END-IF                                                            
054140                                                                          
054800        MOVE SLAG-IDPERSON-BUY                                            
054900                             TO WS-IDPERSON-BUY-OLD                       
055000        MOVE SLAG-FLREFILL   TO WS-FLREFILL-OLD                           
055100                                                                          
055300        IF W27143-KDERS = 02 OR 03 OR 22 OR 23                            
055500           MOVE SLAG-FLREFBEO      TO WS-FLREFBEO-OLD                     
055600           MOVE SLAG-KVPB-REF      TO WS-KVPB-REF-OLD                     
055700           MOVE SLAG-KVPBREOI      TO WS-KVPBREOI-OLD                     
055800           MOVE SLAG-FLWILSON      TO WS-FLWILSON-OLD                     
055810           MOVE SLAG-FLFLYG        TO WS-FLFLYG-OLD                       
055900           MOVE +1 TO IX                                                  
056000           PERFORM UNTIL IX > 12                                          
056100              MOVE SLAG-RESEASON(IX) TO WS-RESEASON-OLD(IX)               
056200              ADD +1 TO IX                                                
056300           END-PERFORM                                                    
056310           MOVE JA   TO SLAG-FLPB-FLYTT                                   
056400        END-IF                                                            
058400                                                                          
061700        PERFORM IMS-REPL-WDK711                                           
061800        ADD +1 TO CHKP-ANT                                                
061810        PERFORM IMS-GNP-WDK727                                            
061820        IF SEGMENT-FINNS                                                  
061830          MOVE PROG-KVPB-JUST(1)  TO WS-KVPB-JUST-1                       
061840        END-IF                                                            
061900     END-IF                                                               
062000     .                                                                    
062100     EJECT                                                                
062200                                                                          
062300                                                                          
062400 BA-NOLLSTALL-ARTIKEL-LAGER SECTION.                                      
062500                                                                          
062600     MOVE ZERO     TO WS-KVDISP-AKT-NDC                                   
062700                      WS-KVDISP-REST                                      
062800                      WS-KVDAGAR-TACKT-LAST-RP                            
062900                      WS-TIREFMPB                                         
063000                      WS-KVPB-REF-OLD                                     
063100                      WS-KVPBREOI-OLD                                     
063200                      WS-IDPERSON-BUY-OLD                                 
063210                      WS-KVPB-JUST-1                                      
063220                      WS-TIPBJUST-1                                       
063230                      WS-KVPB-JUST-2                                      
063240                      WS-TIPBJUST-2                                       
063250                      WS-TISTOREF-AAVV                                    
063300                                                                          
063400     MOVE SPACE    TO WS-FLREFBEO-OLD                                     
063500                      WS-FLWILSON-OLD                                     
063600                      WS-FLREFILL-OLD                                     
063700                      WS-FLFLYG-OLD                                       
063800     MOVE +1       TO IX                                                  
063900     PERFORM UNTIL IX > 12                                                
064000        MOVE 1.00  TO WS-RESEASON-OLD(IX)                                 
064100        ADD +1 TO IX                                                      
064200     END-PERFORM                                                          
064400     .                                                                    
064500     EJECT                                                                
064600                                                                          
081610 C-BEHANDLA-TILLKOMMANDE SECTION.                                         
081620                                                                          
081621     MOVE NEJ TO SW-ISRT-K711                                             
081630     PERFORM IMS-GU-ERSA01                                                
081640     IF SEGMENT-FINNS                                                     
081641        MOVE ERSA01-DIERS-ERS   TO WS-DIERS-ERS                           
081650        PERFORM IMS-GNP-ERSA11                                            
081660        PERFORM UNTIL SEGMENT-SAKNAS                                      
081670           MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR                         
081671           MOVE ERSA11-DIERS-TILLK TO WS-DIERS-TILLK                      
081680           PERFORM IMS-GU-ARTC01                                          
081690           IF SEGMENT-FINNS                                               
081691              IF ART-KDERS-UTG > 0                                        
081692                 PERFORM  S30-COMMON-VALUES                               
081693                 MOVE ERSA11-IDARTNR-TILLK TO                             
081694                      W27131-IDARTNR-TILLK                                
081695                 MOVE ART-KDERS-UTG TO W27131-KDERS-UTG-TILLK             
081696                 MOVE ZERO          TO W27131-KDERS-TILLK                 
081697                 PERFORM S12-SKRIV-W27131                                 
081698              ELSE                                                        
081699                 PERFORM IMS-GNP-ARTC11                                   
081700*                MOVE ZERO       TO WS-KDERS-TILLK                        
081701*                MOVE CLAG-KDERS TO WS-KDERS-TILLK                        
081702                 PERFORM CB-CHECK-INHERITANCE                             
081703                 IF NO-HERITAGE                                           
081711                    PERFORM S30-COMMON-VALUES                             
081712                    MOVE ERSA11-IDARTNR-TILLK TO                          
081713                      W27131-IDARTNR-TILLK                                
081714                    MOVE ART-KDERS-UTG  TO                                
081715                                   W27131-KDERS-UTG-TILLK                 
081716                    MOVE CLAG-KDERS     TO W27131-KDERS-TILLK             
081717                    PERFORM S12-SKRIV-W27131                              
081718                 ELSE                                                     
081719                    PERFORM CA-WDK7-TILLKOMMANDE                          
081720                 END-IF                                                   
081721              END-IF                                                      
081722           END-IF                                                         
081723           PERFORM IMS-GNP-ERSA11                                         
081724        END-PERFORM                                                       
081725     END-IF                                                               
085600     .                                                                    
085700     EJECT                                                                
085800                                                                          
085900                                                                          
086000 CA-WDK7-TILLKOMMANDE SECTION.                                            
086100                                                                          
086200     MOVE ERSA11-IDARTNR-TILLK   TO W-IDARTNR                             
086300     MOVE W27143-IDDC            TO W-IDDC                                
086310     MOVE NEJ                    TO PURCH-SW                              
086400                                                                          
086500     PERFORM IMS-GHU-WDK711                                               
086600     IF SEGMENT-FINNS                                                     
086700        IF SLAG-IDPERSON-BUY = ZERO                                       
086800           MOVE WS-IDPERSON-BUY-OLD TO SLAG-IDPERSON-BUY                  
086900           IF DCS-NDC-NA                                                  
087000              MOVE JA               TO IDPERSON-UPD-SW                    
087100           END-IF                                                         
087200        END-IF                                                            
087300        IF SLAG-FLREFILL = JA                                             
087400           MOVE WS-FLREFILL-OLD     TO SLAG-FLREFILL                      
087500        END-IF                                                            
087600        IF SLAG-KDREFSTA = 'P'                                            
087700          MOVE AKTIV        TO SLAG-KDREFSTA                              
087800          MOVE DAGENS-DATUM TO SLAG-TIREFSTA                              
087900        END-IF                                                            
088000        IF WS-FLFLYG-OLD = 'J' OR 'S'                                     
088010          IF SLAG-FLFLYG = 'N'                                            
088100            MOVE WS-FLFLYG-OLD TO SLAG-FLFLYG                             
088200          END-IF                                                          
088201        END-IF                                                            
088210        IF SLAG-IDDC-REF = SPACE                                          
088220          MOVE JA TO PURCH-SW                                             
088230        END-IF                                                            
088600                                                                          
088700        PERFORM CA2-UPDATE-RESEASON                                       
088800                                                                          
088900                                                                          
089000        PERFORM IMS-REPL-WDK711                                           
089100        ADD +1 TO CHKP-ANT                                                
089700     ELSE                                                                 
089800        MOVE ALL '+'      TO WDK7-W005WDK7                                
089900        MOVE 'WDK711'     TO WDK7-IDSEGM                                  
090000        MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                             
090100        MOVE W27143-IDDC  TO WDK7-IDDC-KFB                                
090200                             WDK7-IDDC                                    
090300        PERFORM CA1-INIT-WDK711                                           
090400                                                                          
090500        CALL W005WDK7 USING WDK7-W005WDK7 W005-WDB6-PCB                   
090600                                          W005-WDK6-PCB                   
090610                                          W005-WDK7-PCB                   
090700        ADD +1 TO CHKP-ANT                                                
090710        MOVE JA TO SW-ISRT-K711                                           
090800     END-IF                                                               
090900                                                                          
090902     IF PURCH-JA                                                          
090903       CONTINUE                                                           
090904     ELSE                                                                 
090910       IF WS-KVPB-REF-OLD > ZERO                                          
090911         COMPUTE WS-RATIO = WS-DIERS-TILLK /                              
090912                            WS-DIERS-ERS                                  
090920         PERFORM IMS-GHU-WDK727                                           
090930         IF SEGMENT-FINNS                                                 
090935*THE FUTURE FORECAST FOR THE NEW PART SHALL BE OLD PARTNO KVPB-SEP        
090936* + NEW PARTNO KVPB-SEP                                                   
090937           COMPUTE PROG-KVPB-JUST(1) = (WS-KVPB-REF-OLD *                 
090938                                        WS-RATIO) +                       
090939                                       SLAG-KVPB-REF                      
090940           IF PROG-KVPB-JUST (1) < 0.1                                    
090941             CONTINUE                                                     
090942           ELSE                                                           
090943             IF WS-TISTOREF-AAVV > DAGENS-TIAAVV                          
090950               MOVE WS-TISTOREF-AAVV TO PROG-TIPBJUST(1)                  
090951             ELSE                                                         
090952               MOVE WS-DAGENS-AAVV-NEXT TO PROG-TIPBJUST(1)               
090953             END-IF                                                       
090954****TO   GET SAME DATE ON OLD AND NEW PARTNO FOR FFC                      
090955             MOVE PROG-TIPBJUST(1) TO WS-TIPBJUST                         
090960             MOVE ZERO             TO PROG-KVPB-JUST(2)                   
090970             MOVE ZERO             TO PROG-TIPBJUST(2)                    
090980             PERFORM IMS-REPL-WDK727                                      
090981           END-IF                                                         
090990         ELSE                                                             
090991           MOVE ALL '+'   TO WDK7-W005WDK7                                
090992           MOVE 'WDK727' TO WDK7-IDSEGM                                   
090993           MOVE W-IDARTNR TO WDK7-IDARTNR-KFB                             
090994           MOVE W27143-IDDC TO WDK7-IDDC-KFB                              
090995           IF SW-ISRT-K711-JA                                             
090997             MOVE ZERO TO SLAG-KVPB-REF                                   
090998           END-IF                                                         
090999           COMPUTE WDK7-KVPB-JUST (1) = (WS-KVPB-REF-OLD *                
091000                                        WS-RATIO) +                       
091001                                       SLAG-KVPB-REF                      
091002           IF WDK7-KVPB-JUST (1) < 0.1                                    
091003             CONTINUE                                                     
091004           ELSE                                                           
091005             IF WS-TISTOREF-AAVV > DAGENS-TIAAVV                          
091006               MOVE WS-TISTOREF-AAVV TO WDK7-TIPBJUST(1)                  
091007             ELSE                                                         
091008               MOVE WS-DAGENS-AAVV-NEXT TO WDK7-TIPBJUST(1)               
091009             END-IF                                                       
091010****TO   GET SAME DATE ON OLD AND NEW PARTNO FOR FFC                      
091011             MOVE WDK7-TIPBJUST(1) TO WS-TIPBJUST                         
091012             MOVE ZERO             TO WDK7-KVPB-JUST(2)                   
091013             MOVE ZERO             TO WDK7-TIPBJUST(2)                    
091014                                                                          
091015             CALL W005WDK7 USING WDK7-W005WDK7 W005-WDB6-PCB              
091016                                               W005-WDK6-PCB              
091017                                               W005-WDK7-PCB              
091018             ADD +1 TO CHKP-ANT                                           
091019           END-IF                                                         
091020*          MOVE JA TO WDK727-SW                                           
091021         END-IF                                                           
091022       END-IF                                                             
091023     END-IF                                                               
091025****TO PHASE OUT OLD PARTNO WE SET THE FUTURE FORECAST TO                 
091026****0.1 AND THE DATE TO THE SAME DATE AS TH NEW IS RAMPED UP              
091027     MOVE W-IDARTNR-OLD      TO W-IDARTNR                                 
091028     IF WS-KVPB-REF-OLD > ZERO                                            
091029       PERFORM IMS-GHU-WDK727                                             
091030       IF SEGMENT-FINNS                                                   
091031         MOVE 0.1              TO PROG-KVPB-JUST(1)                       
091032         MOVE WS-TIPBJUST      TO PROG-TIPBJUST(1)                        
091033         PERFORM IMS-REPL-WDK727                                          
091034       ELSE                                                               
091035         MOVE ALL '+'     TO WDK7-W005WDK7                                
091036         MOVE 'WDK727' TO WDK7-IDSEGM                                     
091037         MOVE W-IDARTNR TO WDK7-IDARTNR-KFB                               
091038         MOVE W27143-IDDC TO WDK7-IDDC-KFB                                
091039         MOVE 0.1              TO WDK7-KVPB-JUST(1)                       
091040         MOVE WS-TIPBJUST      TO WDK7-TIPBJUST(1)                        
091050         MOVE ZERO                 TO WDK7-KVPB-JUST(2)                   
091051         MOVE ZERO                 TO WDK7-TIPBJUST(2)                    
091052                                                                          
091053         CALL W005WDK7 USING WDK7-W005WDK7 W005-WDB6-PCB                  
091054                                           W005-WDK6-PCB                  
091055                                           W005-WDK7-PCB                  
091056         ADD +1 TO CHKP-ANT                                               
091057       END-IF                                                             
091058     END-IF                                                               
091060*    UPDATE BUYER FOR ALL DCS IN NDC-NA                                   
091100     IF  IDPERSON-UPD-JA                                                  
091200     AND W-IDARTNR-SPAR NOT = W-IDARTNR                                   
091300        MOVE LOW-VALUES           TO W-IDDC-MIN                           
091400        MOVE HIGH-VALUES          TO W-IDDC-MAX                           
091500        MOVE W-IDARTNR            TO W-IDARTNR-SPAR                       
091600*                                                                         
091700        PERFORM IMS-GU-WDK701                                             
091800        IF SEGMENT-FINNS                                                  
091900           PERFORM IMS-GHNP-WDK711                                        
092000           PERFORM UNTIL SEGMENT-SAKNAS                                   
092100             MOVE SLAG-IDDC       TO WS-IDDC                              
092200             IF NDC-NA                                                    
092300                MOVE WS-IDPERSON-BUY-OLD                                  
092400                                  TO SLAG-IDPERSON-BUY                    
092500                PERFORM IMS-REPL-WDK711                                   
092600             END-IF                                                       
092700             PERFORM IMS-GHNP-WDK711                                      
092800           END-PERFORM                                                    
092900           MOVE NEJ               TO IDPERSON-UPD-SW                      
093000        END-IF                                                            
093100     END-IF                                                               
093200                                                                          
093300     .                                                                    
093400     EJECT                                                                
093500                                                                          
093600 CA1-INIT-WDK711 SECTION.                                                 
093700                                                                          
093800     MOVE AKTIV                 TO WDK7-KDREFSTA                          
093900     MOVE WS-IDPERSON-BUY-OLD   TO WDK7-IDPERSON-BUY                      
094000     IF DCS-NDC-NA                                                        
094100        MOVE JA                 TO IDPERSON-UPD-SW                        
094200     END-IF                                                               
094300     MOVE WS-FLREFILL-OLD       TO WDK7-FLREFILL                          
094400     MOVE WS-FLWILSON-OLD       TO WDK7-FLWILSON                          
094500     MOVE WS-FLFLYG-OLD         TO WDK7-FLFLYG                            
094600                                                                          
094700     MOVE +1 TO IX                                                        
094800     PERFORM UNTIL IX > 12                                                
094900        MOVE WS-RESEASON-OLD(IX) TO WDK7-RESEASON(IX)                     
095000        ADD +1 TO IX                                                      
095100     END-PERFORM                                                          
095200     MOVE DD-PLUS-TVA-AR-TISSAAMMDD TO WDK7-DASPSEA                       
095300***?????HUR GÖRA MED AUTOREFILL FLAGGA                                    
096600     IF W27143-FLPB-FLYTT = JA                                            
096700       CONTINUE                                                           
096800     ELSE                                                                 
096900       MOVE 'N'   TO WDK7-FLREFBEO                                        
097000     END-IF                                                               
098200     .                                                                    
098300     EJECT                                                                
098400                                                                          
098500 CA2-UPDATE-RESEASON SECTION.                                             
098600                                                                          
098700** CHECK IF THE NEW PART HAS ANY SEASON FACTOR ALREADY, INDICATED         
098800** BY RESEASON = 1.00.                                                    
098900     MOVE NEJ                         TO SEASON-SW                        
099000     PERFORM VARYING IX FROM 1 BY 1                                       
099100     UNTIL IX > 12 OR SEASON-FINNS                                        
099200        IF SLAG-RESEASON(IX) = 1.00                                       
099300           CONTINUE                                                       
099400        ELSE                                                              
099500           MOVE JA                    TO SEASON-SW                        
099600        END-IF                                                            
099700     END-PERFORM                                                          
099800                                                                          
099900** IF NEW PART HAS SEASON FACTOR ALREADY, THEN NOTHING SHOULD BE          
100000** UPDATED. ELSE COPY THE SEASON FACTOR FROM OLD PART TO NEW PART         
100100     IF SEASON-FINNS                                                      
100200        CONTINUE                                                          
100300     ELSE                                                                 
100400        MOVE +1                        TO IX                              
100500        PERFORM UNTIL IX > 12                                             
100600          MOVE WS-RESEASON-OLD(IX)     TO SLAG-RESEASON(IX)               
100700          ADD +1                       TO IX                              
100800        END-PERFORM                                                       
100900        MOVE DD-PLUS-TVA-AR-TISSAAMMDD TO SLAG-DASPSEA                    
101000     END-IF                                                               
101100     .                                                                    
101200     EJECT                                                                
105000                                                                          
105010 CB-CHECK-INHERITANCE SECTION.                                            
105020                                                                          
105021     MOVE 'J'      TO HERITAGE-SW                                         
105022**IF SUPERSEDED PARTNO HAVE A FUTURE FORECAST                             
105023     IF WS-KVPB-JUST-1 > ZERO                                             
105024       MOVE 'N'    TO HERITAGE-SW                                         
105025     END-IF                                                               
105030                                                                          
105031**IF SUPERSEDED PARTNO HAVE A KVPBREOI(REFILLS ANOTHER DC)                
105032     IF WS-KVPBREOI-OLD > ZERO                                            
105033       MOVE 'N'    TO HERITAGE-SW                                         
105034     END-IF                                                               
105035                                                                          
105036     MOVE ERSA11-IDARTNR-TILLK   TO W-IDARTNR                             
105037     MOVE W27143-IDDC            TO W-IDDC                                
105038**IF SUPERSEDING PARTNO HAVE ALREADY HAVE BEEN HANDLED(MOVED)             
105040     PERFORM IMS-GHU-WDK711                                               
105041     IF SEGMENT-FINNS                                                     
105042       IF SLAG-FLPB-FLYTT = JA                                            
105043         MOVE 'N'  TO HERITAGE-SW                                         
105044       END-IF                                                             
105045**IF SUPERSEDING PARTNO HAVE A FUTURE FORECAST                            
105046       PERFORM IMS-GNP-WDK727                                             
105047       IF SEGMENT-FINNS                                                   
105048         IF PROG-KVPB-JUST(1) > 0                                         
105049           MOVE 'N' TO HERITAGE-SW                                        
105050         END-IF                                                           
105051       END-IF                                                             
105052     END-IF                                                               
105054     .                                                                    
105055     EJECT                                                                
105060                                                                          
105100 Z-FINIT SECTION.                                                         
105200                                                                          
105300                                                                          
105400     PERFORM IMS-LAS-ATERSTART                                            
105500     MOVE ZERO                 TO CKPA-2250-KVPOST                        
105600                                  CKPA-2250-IDARTNR                       
105700                                  CKPA-2250-IDDC                          
105800     ACCEPT CKPA-2250-TIUPPDAT FROM DATE                                  
105900     ACCEPT CKPA-2250-TIUPPTID FROM TIME                                  
106000     IF SEGMENT-SAKNAS                                                    
106100       MOVE '1'                TO CKPA-2250-KDSEGKEY                      
106200       PERFORM IMS-ISRT-ATERSTART                                         
106300     ELSE                                                                 
106400       PERFORM IMS-REPL-ATERSTART                                         
106500     END-IF                                                               
106600                                                                          
106700     CLOSE W27143                                                         
106800           W27131                                                         
107000     SKIP2                                                                
107100     MOVE 'S' TO POSTSUM-OPKOD                                            
107200     CALL POSTSUM USING POSTSUM-PARM                                      
107300                                                                          
107400     .                                                                    
107500     EJECT                                                                
107600                                                                          
107700                                                                          
107800 S01-LAES-W27143  SECTION.                                                
107900     SKIP2                                                                
108000     READ W27143 INTO W27143-AREA                                         
108100     AT END                                                               
108200        SET END-OF-W27143 TO TRUE                                         
108300                                                                          
108400     NOT AT END                                                           
108500        MOVE 'W27143' TO POSTSUM-FDNAMN                                   
108600        MOVE 'W27143D1' TO POSTSUM-DDNAMN2                                
108700        CALL POSTSUM USING POSTSUM-PARM                                   
108800        ADD +1 TO WS-KVPOST-IN                                            
108900     END-READ                                                             
109000     .                                                                    
109100     EJECT                                                                
109200                                                                          
109300                                                                          
109400 S12-SKRIV-W27131 SECTION.                                                
109500     SKIP2                                                                
109600     WRITE W27131-POST FROM W27131-AREA                                   
109700                                                                          
109800     MOVE 'W27131 ' TO POSTSUM-FDNAMN                                     
109900     MOVE 'W27143D2' TO POSTSUM-DDNAMN2                                   
110000     CALL POSTSUM USING POSTSUM-PARM                                      
110100     .                                                                    
110200     EJECT                                                                
110300                                                                          
110400                                                                          
111600 S30-COMMON-VALUES SECTION.                                               
111700                                                                          
111800     MOVE W27143-IDARTNR   TO W27131-IDARTNR-ERS                          
111900     MOVE W27143-IDDC      TO W27131-IDDC-ERS                             
112200     MOVE WS-IDPERSON-BUY-OLD TO                                          
112300                      W27131-IDPERSON-BUY-ERS                             
112310     MOVE ZERO             TO W27131-IDARTNR-TILLK                        
112320                              W27131-KDERS-UTG-TILLK                      
112330                              W27131-KDERS-TILLK                          
112400     .                                                                    
112500                                                                          
112600                                                                          
112700 X-TAG-CHECKPOINT   SECTION.                                              
112800                                                                          
112900     PERFORM IMS-LAS-ATERSTART                                            
113000     MOVE WS-KVPOST-IN   TO CKPA-2250-KVPOST                              
113100     MOVE W27143-IDARTNR TO CKPA-2250-IDARTNR                             
113200     MOVE W27143-IDDC    TO CKPA-2250-IDDC                                
113300     ACCEPT CKPA-2250-TIUPPDAT FROM DATE                                  
113400     ACCEPT CKPA-2250-TIUPPTID FROM TIME                                  
113500     IF SEGMENT-SAKNAS                                                    
113600       MOVE '1'                TO CKPA-2250-KDSEGKEY                      
113700       PERFORM IMS-ISRT-ATERSTART                                         
113800     ELSE                                                                 
113900       PERFORM IMS-REPL-ATERSTART                                         
114000     END-IF                                                               
114100                                                                          
114200     PERFORM IMS-CHECKPOINT                                               
114300     MOVE ZERO                 TO CHKP-ANT                                
114400     .                                                                    
114500     EJECT                                                                
114600                                                                          
114700                                                                          
114800 S99-ABEND SECTION.                                                       
114900                                                                          
115000     MOVE 'S' TO POSTSUM-OPKOD                                            
115100     CALL POSTSUM USING POSTSUM-PARM                                      
115200     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
115300     .                                                                    
115400     EJECT                                                                
115500                                                                          
115600                                                                          
115700* --- IMS SEKTIONER ---                                                   
115800     EJECT                                                                
115900                                                                          
116000 IMS-GU-WDK701 SECTION.                                                   
116100                                                                          
116200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
116300          DELIMITED BY SIZE INTO SSA1                                     
116400     MOVE '  GE' TO GODK-STATUSKODER                                      
116500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
116600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
116700     PERFORM IMS-STATUSKONTROLL                                           
116800     .                                                                    
116900     EJECT                                                                
117000                                                                          
117100 IMS-GHU-WDK711 SECTION.                                                  
117200                                                                          
117300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
117400          DELIMITED BY SIZE INTO SSA1                                     
117500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
117600          DELIMITED BY SIZE INTO SSA2                                     
117700     MOVE '  GE' TO GODK-STATUSKODER                                      
117800     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
117900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
118000     PERFORM IMS-STATUSKONTROLL                                           
118100     .                                                                    
118200     EJECT                                                                
118300                                                                          
118400 IMS-GHNP-WDK711 SECTION.                                                 
118500                                                                          
118600     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
118700                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
118800          DELIMITED BY SIZE INTO SSA1                                     
118900     MOVE '  GE' TO GODK-STATUSKODER                                      
119000     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1                  
119100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
119200     PERFORM IMS-STATUSKONTROLL                                           
119300     .                                                                    
119400     SKIP3                                                                
119500                                                                          
119600 IMS-REPL-WDK711 SECTION.                                                 
119700                                                                          
119800     MOVE '  ' TO GODK-STATUSKODER                                        
119900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
120000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
120100     PERFORM IMS-STATUSKONTROLL                                           
120200     .                                                                    
120300     EJECT                                                                
120400                                                                          
120410 IMS-GNP-WDK727 SECTION.                                                  
120420                                                                          
120421     MOVE 'WDK727 ' TO SSA1                                               
120470     MOVE '  GE' TO GODK-STATUSKODER                                      
120480     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK727 SSA1                   
120490     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
120491     PERFORM IMS-STATUSKONTROLL                                           
120492     .                                                                    
120493     EJECT                                                                
120494                                                                          
120495 IMS-GHU-WDK727 SECTION.                                                  
120496                                                                          
120497     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
120498          DELIMITED BY SIZE INTO SSA1                                     
120499     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
120500          DELIMITED BY SIZE INTO SSA2                                     
120501     STRING 'WDK727  (KDSEGKEY =' W-KDSEGKEY ')'                          
120502          DELIMITED BY SIZE INTO SSA3                                     
120503     MOVE '  GE' TO GODK-STATUSKODER                                      
120504     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK727 SSA1 SSA2 SSA3         
120505     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
120506     PERFORM IMS-STATUSKONTROLL                                           
120507     .                                                                    
120508     EJECT                                                                
120509                                                                          
120510 IMS-REPL-WDK727 SECTION.                                                 
120511                                                                          
120512     MOVE '  ' TO GODK-STATUSKODER                                        
120513     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK727                       
120514     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
120515     PERFORM IMS-STATUSKONTROLL                                           
120516     .                                                                    
120517     EJECT                                                                
122500                                                                          
122600 IMS-GU-ERSA01 SECTION.                                                   
122700                                                                          
122800     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
122900          DELIMITED BY SIZE INTO SSA1                                     
123000     MOVE '  GE' TO GODK-STATUSKODER                                      
123100     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-ERSA01 SSA1                    
123200     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
123300     PERFORM IMS-STATUSKONTROLL                                           
123400     .                                                                    
123500     EJECT                                                                
123600                                                                          
123700                                                                          
123800 IMS-GNP-ERSA11 SECTION.                                                  
123900                                                                          
124000     MOVE 'WLERSA11 ' TO SSA1                                             
124100     MOVE '  GE' TO GODK-STATUSKODER                                      
124200     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-ERSA11 SSA1                   
124300     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
124400     PERFORM IMS-STATUSKONTROLL                                           
124500     .                                                                    
124600     EJECT                                                                
124700                                                                          
124800                                                                          
124900 IMS-GU-ARTC01 SECTION.                                                   
125000                                                                          
125100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
125200          DELIMITED BY SIZE INTO SSA1                                     
125300     MOVE '  GE' TO GODK-STATUSKODER                                      
125400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC01 SSA1                    
125500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
125600     PERFORM IMS-STATUSKONTROLL                                           
125700     .                                                                    
125800     EJECT                                                                
125900                                                                          
126000                                                                          
126100 IMS-GNP-ARTC11 SECTION.                                                  
126200                                                                          
126300     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY ')'                          
126400          DELIMITED BY SIZE INTO SSA1                                     
126500     MOVE '  GE' TO GODK-STATUSKODER                                      
126600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
126700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
126800     PERFORM IMS-STATUSKONTROLL                                           
126900     .                                                                    
127000     EJECT                                                                
127100                                                                          
127200                                                                          
127300 IMS-GHU-ARTC11 SECTION.                                                  
127400                                                                          
127500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
127600          DELIMITED BY SIZE INTO SSA1                                     
127700     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY ')'                          
127800          DELIMITED BY SIZE INTO SSA2                                     
127900     MOVE '  GE' TO GODK-STATUSKODER                                      
128000     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2              
128100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
128200     PERFORM IMS-STATUSKONTROLL                                           
128300     .                                                                    
128400     EJECT                                                                
128500                                                                          
128600                                                                          
128700 IMS-REPL-ARTC   SECTION.                                                 
128800                                                                          
128900     MOVE '  ' TO GODK-STATUSKODER                                        
129000     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC11                       
129100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
129200     PERFORM IMS-STATUSKONTROLL                                           
129300     .                                                                    
129400     EJECT                                                                
129500                                                                          
129600 IMS-GU-WDB601    SECTION.                                                
129700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
129800          DELIMITED BY SIZE INTO SSA1                                     
129900     MOVE '  ' TO GODK-STATUSKODER                                        
130000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
130100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
130200     PERFORM IMS-STATUSKONTROLL                                           
130300     .                                                                    
130400     EJECT                                                                
130500                                                                          
130600 IMS-LAS-ATERSTART SECTION.                                               
130700     SKIP2                                                                
130800     STRING 'WLCKPA01(WDGXKEY  =' W-WDGX-2249-KEY-X ')'                   
130900            DELIMITED BY SIZE INTO SSA1                                   
131000     STRING 'WLCKPA11(KDSEGKEY =' W-WDGX-2250-KEY-X ')'                   
131100            DELIMITED BY SIZE INTO SSA2                                   
131200     MOVE '  GE' TO GODK-STATUSKODER                                      
131300     CALL CBLTDLI USING GHU CKPA-PCB DLI-IO-WLCKPA SSA1 SSA2              
131400     MOVE CKPA-STATUS-CODE TO STATUS-WS                                   
131500     PERFORM IMS-STATUSKONTROLL                                           
131600     .                                                                    
131700     EJECT                                                                
131800                                                                          
131900 IMS-ISRT-ATERSTART SECTION.                                              
132000     SKIP2                                                                
132100     STRING 'WLCKPA01(WDGXKEY  =' W-WDGX-2249-KEY-X ')'                   
132200          DELIMITED BY SIZE INTO SSA1                                     
132300     MOVE 'WLCKPA11' TO SSA2                                              
132400     MOVE '  GE' TO GODK-STATUSKODER                                      
132500     CALL CBLTDLI USING ISRT CKPA-PCB DLI-IO-WLCKPA SSA1 SSA2             
132600     MOVE CKPA-STATUS-CODE TO STATUS-WS                                   
132700     PERFORM IMS-STATUSKONTROLL                                           
132800     .                                                                    
132900     EJECT                                                                
133000                                                                          
133100                                                                          
133200 IMS-REPL-ATERSTART SECTION.                                              
133300     SKIP2                                                                
133400     MOVE SPACE TO GODK-STATUSKODER                                       
133500     CALL CBLTDLI USING REPL CKPA-PCB DLI-IO-WLCKPA                       
133600     MOVE CKPA-STATUS-CODE TO STATUS-WS                                   
133700     PERFORM IMS-STATUSKONTROLL                                           
133800     .                                                                    
133900     EJECT                                                                
134000                                                                          
134100                                                                          
134200 IMS-RESTART SECTION.                                                     
134300     SKIP2                                                                
134400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
134500     MOVE '  ' TO GODK-STATUSKODER                                        
134600     CALL CBLTDLI USING XRST MSG-PCB                                      
134700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
134800                        CHKP-AREA-LENGTH CHKP-AREA                        
134900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
135000     PERFORM IMS-STATUSKONTROLL                                           
135100     .                                                                    
135200     EJECT                                                                
135300                                                                          
135400                                                                          
135500 IMS-CHECKPOINT SECTION.                                                  
135600     SKIP2                                                                
135700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
135800     MOVE '  XD' TO GODK-STATUSKODER                                      
135900     CALL CBLTDLI USING CHKP MSG-PCB                                      
136000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
136100                        CHKP-AREA-LENGTH CHKP-AREA                        
136200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136300     PERFORM IMS-STATUSKONTROLL                                           
136400                                                                          
136500     IF IMS-EJ-OK                                                         
136600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
136700       DISPLAY FELTEXT                                                    
136800       CALL FELLOG                                                        
136900     END-IF                                                               
137000     .                                                                    
137100     EJECT                                                                
137200                                                                          
137300                                                                          
138600 IMS-STATUSKONTROLL SECTION.                                              
138700     SKIP2                                                                
138800     SET STATUS-IX TO 1                                                   
138900     SEARCH GODK-STATUS                                                   
139000       AT END                                                             
139100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
139200           DELIMITED BY SIZE INTO FELTEXT                                 
139300         DISPLAY FELTEXT                                                  
139400         CALL FELLOG                                                      
139500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
139600         CONTINUE                                                         
139700     END-SEARCH                                                           
139800     .                                                                    
