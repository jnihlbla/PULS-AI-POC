000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2713600.                                                
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
001400*        PROGRAMMET UPPDATERAR WLOIGA (WDL7)                              
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
003000     SELECT W27136                     ASSIGN TO W27136D1.                
003100*          --- ERSÄTTNINGSPASSIVERADE REFILLARTIKLAR                      
003200     SELECT W27137                     ASSIGN TO W27136D2.                
003300*          --- MEMO DÅ TILLKOMMANDE ARTIKEL I SIN TUR ÄR                  
003400*          --- RENSAD ELLER ERSATT                                        
003500     SELECT W27135                     ASSIGN TO W27136D3.                
003600*          --- MEMO DÅ TILLKOMMANDE ARTIKEL INTE ÄRVER                    
003700*          --- SÄSONG                                                     
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W27136                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W27136      -L.                                                
004800     SKIP3                                                                
004900                                                                          
005000 FD  W27137                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY W27137 -PRE  W27137-  -L.                                 
005500     EJECT                                                                
005600                                                                          
005700 FD  W27135                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000                                                                          
006100*01  POST -COPY W27135 -PRE  W27135-  -L.                                 
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400     SKIP2                                                                
006500                                                                          
006600*    -- CHECKED BY WY2000                                                 
006700 77  IDPGM                       PIC X(8)    VALUE 'W2713600'.            
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
009910     03 WS-NO-MONTHS             PIC 9(5)    VALUE ZERO.                  
009920     03 WS-KDERS-TILLK           PIC 9(2)    VALUE ZERO.                  
010000     03 WS-VECKA                 PIC 9(2)    VALUE ZERO.                  
010100     03 FIRST-VV-INNEV-RP        PIC 9(2)    VALUE ZERO.                  
010200     03 WS-KVPOST-IN             PIC S9(7)   VALUE ZERO.                  
010300     03 W-IDARTNR-SPAR           PIC S9(9)   VALUE ZERO COMP-3.           
010301     03 W-IDARTNR-OLD            PIC S9(9)   VALUE ZERO COMP-3.           
010310     03 WS-TIPBJUST-1            PIC 9(4)    VALUE ZERO.                  
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
012200     03 WS-KVDISP-REST       PIC S9(6)V9(3) VALUE ZERO COMP-3.            
012300     03 WS-KVDISP-REST-SPAR  PIC S9(6)V9(3) VALUE ZERO COMP-3.            
012400     03 WS-KVPB-DAG-IX-RP    PIC S9(6)V9(5) VALUE ZERO COMP-3.            
012500     03 WS-KVPB-RP           PIC S9(6)V9(5) VALUE ZERO COMP-3.            
012600     03 WS-KVARBD-IX-RP      PIC S9(3)      VALUE ZERO COMP-3.            
012700     03 WS-TIREFMPB          PIC S9(7)      VALUE ZERO COMP-3.            
012800                                                                          
012900 01  FELTEXT.                                                             
013000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013200                                                                          
013300 77  W27136-EOF-SW               PIC X       VALUE 'N'.                   
013400     88  END-OF-W27136                       VALUE 'J'.                   
013500                                                                          
013600 77  SEASON-SW               PIC X           VALUE 'N'.                   
013700     88  SEASON-FINNS                        VALUE 'J'.                   
013800     88  SEASON-SAKNAS                       VALUE 'N'.                   
013900                                                                          
014000 77  FORSTA-SW                PIC X          VALUE 'J'.                   
014100     88  FORSTA                              VALUE 'J'.                   
014200                                                                          
014201 77  ONE-YEAR-SW              PIC X          VALUE 'N'.                   
014202     88  PLUS-ONE-YEAR                       VALUE 'J'.                   
014203                                                                          
014210 77  WDK727-SW                PIC X          VALUE 'N'.                   
014220     88  WDK727-UPD                          VALUE 'J'.                   
014230                                                                          
014240 77  PURCH-SW                 PIC X          VALUE 'N'.                   
014250     88  PURCH-JA                            VALUE 'J'.                   
014260                                                                          
014300 77  IDPERSON-UPD-SW          PIC X          VALUE 'N'.                   
014400     88  IDPERSON-UPD-JA                     VALUE 'J'.                   
014500                                                                          
014600 77  SW-FLPB-FLYTT            PIC X          VALUE 'J'.                   
014610     88 SS-ALREADY-DONE                      VALUE 'J'.                   
014700     EJECT                                                                
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
017710 01  DD-PLUS-ETT-AR                                                       
017720                                 PIC 9(6)    VALUE ZERO.                  
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
018900     03  W005WDL7                PIC X(8)    VALUE 'W005WDL7'.            
019000     EJECT                                                                
019100*01  -COPY WWDCKONS                                                       
019200     EJECT                                                                
019300*01  -COPY WWDC99                                                         
019400     EJECT                                                                
019500*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
019600 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
019700*   -COPY W005WDK7                                                        
019800     EJECT                                                                
019900 01 FILLER                       PIC X(8)    VALUE 'W005WDL7'.            
020000*   -COPY W005WDL7                                                        
020100     EJECT                                                                
020200*    --- PARAMETRAR TILL DATKORT                                          
020300*                                                                         
020400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27136'.              
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
022700*    --- PARAMETRAR TILL POSTSUM                                          
022800*                                                                         
022900*01  -COPY W0005   -PRE  POSTSUM-                                         
023000     EJECT                                                                
023100 01  W27136-AREA-START           PIC X(24)   VALUE                        
023200                                             'W27136-AREA-START'.         
023300     SKIP2                                                                
023400                                                                          
023500*01  AREA -COPY W27136     -PRE W27136-                                   
023600*                                                                         
023700     EJECT                                                                
023800                                                                          
023900                                                                          
024000 01  W27137-AREA-START           PIC X(24)   VALUE                        
024100                                             'W27137-AREA-START'.         
024200     SKIP2                                                                
024300                                                                          
024400*01  AREA -COPY W27137     -PRE W27137-                                   
024500*                                                                         
024600     EJECT                                                                
024700                                                                          
024800                                                                          
024900 01  W27135-AREA-START           PIC X(24)   VALUE                        
025000                                             'W27135-AREA-START'.         
025100     SKIP2                                                                
025200                                                                          
025300*01  AREA -COPY W27135     -PRE W27135-                                   
025400*                                                                         
025500     EJECT                                                                
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
027500         05  W-IDPGM             PIC X(08)   VALUE 'W2713600'.            
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
030800 01  FILLER         PIC X(24) VALUE 'DLI-IO-OIGA11'.                      
030900 01  DLI-IO-OIGA11.                                                       
031000*    03  -COPY WDL711                                                     
031100     EJECT                                                                
031200                                                                          
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
034800*01  -COPY W0008  -PRE OIGA-                                              
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
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
036300 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB OIGA-PCB                      
036400                           ERSA-PCB ARTC-PCB                              
036500                           CKPA-PCB WDB6-PCB.                             
036600 MAIN SECTION.                                                            
036700     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB OIGA-PCB                      
036800                           ERSA-PCB ARTC-PCB                              
036900                           CKPA-PCB WDB6-PCB.                             
037000                                                                          
037100     SKIP2                                                                
037200     PERFORM A-INIT                                                       
037300     IF NOT OMSTART                                                       
037400        PERFORM S01-LAES-W27136                                           
037500     END-IF                                                               
037600     PERFORM UNTIL END-OF-W27136                                          
037700        IF CHKP-ANT > CHKP-MAX                                            
037800          PERFORM X-TAG-CHECKPOINT                                        
037900        END-IF                                                            
038000        MOVE W27136-IDARTNR    TO W-IDARTNR                               
038010                                  W-IDARTNR-OLD                           
038100        MOVE W27136-IDDC       TO W-IDDC                                  
038200                                  W-IDDC-B6                               
038300        PERFORM IMS-GU-WDB601                                             
039500        PERFORM B-BEHANDLA-BEORDRAD-ARTIKEL                               
039600        IF W27136-KDREFSTA = 'P'                                          
040500* IN THIS PGM WE HANDLE KDERS 11 OR 17 OR 21 OR 27                        
040510          IF SS-ALREADY-DONE                                              
040520            CONTINUE                                                      
040530          ELSE                                                            
040600            PERFORM C-BEHANDLA-TILLKOMMANDE                               
040700          END-IF                                                          
040900        END-IF                                                            
041200                                                                          
041300        PERFORM S01-LAES-W27136                                           
041400     END-PERFORM                                                          
041500                                                                          
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
043200     OPEN INPUT  W27136                                                   
043300          OUTPUT W27137                                                   
043400                 W27135                                                   
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
047200        MOVE 'FEL FRÅN WDATKONV 1  I A-INIT SECTION I W27136' TO          
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
049000        MOVE 'FEL FRÅN WDATKONV 2  I A-INIT SECTION I W27136' TO          
049100                                    FELTEXT-STR                           
049200        DISPLAY FELTEXT                                                   
049300        PERFORM S99-ABEND                                                 
049400     END-IF                                                               
049500                                                                          
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
050900        MOVE 'FEL FRÅN WDAGKONV 1  I A-INIT SECTION I W27136' TO          
051000                                    FELTEXT-STR                           
051100        DISPLAY FELTEXT                                                   
051200        PERFORM S99-ABEND                                                 
051300     END-IF                                                               
051310     COMPUTE DD-PLUS-ETT-AR = DAGENS-DATUM + 10000                        
051400     .                                                                    
051500     EJECT                                                                
051600                                                                          
051700                                                                          
051800 AA-ATERSTART-EFTER-ABEND SECTION.                                        
051900                                                                          
052000     PERFORM UNTIL WS-KVPOST-IN    = CKPA-2250-KVPOST  OR                 
052100                                            END-OF-W27136                 
052200       PERFORM S01-LAES-W27136                                            
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
053400        IF W27136-TIREFSTO-CLAG > +0                                      
053500           MOVE ZERO   TO CLAG-TIREFSTO                                   
053600        END-IF                                                            
053700        PERFORM IMS-REPL-ARTC                                             
053800     END-IF                                                               
053900                                                                          
054000     PERFORM IMS-GHU-WDK711                                               
054100     IF SEGMENT-FINNS                                                     
054200        MOVE JA  TO SW-FLPB-FLYTT                                         
054300        IF SLAG-FLPB-FLYTT = JA                                           
054400          MOVE JA    TO SW-FLPB-FLYTT                                     
054500        ELSE                                                              
054600          MOVE NEJ   TO SW-FLPB-FLYTT                                     
054700        END-IF                                                            
054800        MOVE SLAG-IDPERSON-BUY                                            
054900                             TO WS-IDPERSON-BUY-OLD                       
055000        MOVE SLAG-FLREFILL   TO WS-FLREFILL-OLD                           
055100                                                                          
057000        IF W27136-KDERS = 11 OR 17 OR 21 OR 27                            
057100           PERFORM BB-BER-SLUTDATUM                                       
057200           MOVE SLAG-FLREFBEO      TO WS-FLREFBEO-OLD                     
057300           MOVE SLAG-KVPB-REF      TO WS-KVPB-REF-OLD                     
057400           MOVE SLAG-KVPBREOI      TO WS-KVPBREOI-OLD                     
057500           MOVE SLAG-FLWILSON      TO WS-FLWILSON-OLD                     
057600           MOVE SLAG-FLFLYG        TO WS-FLFLYG-OLD                       
057700           MOVE +1 TO IX                                                  
057800           PERFORM UNTIL IX > 12                                          
057900              MOVE SLAG-RESEASON(IX) TO WS-RESEASON-OLD(IX)               
058000              ADD +1 TO IX                                                
058100           END-PERFORM                                                    
058200        END-IF                                                            
058300*       END-IF                                                            
058400                                                                          
058500        IF W27136-TIREFSTO-SLAG > +0                                      
058600           MOVE ZERO   TO SLAG-TIREFSTO                                   
058700        END-IF                                                            
058800        IF W27136-KDREFSTA = 'P'                                          
058900                                                                          
059510*?????     MOVE 'S'    TO SLAG-FLREFBEO                                   
059600           MOVE ZERO TO SLAG-KVPB-REF                                     
059700                        SLAG-KVPBREOI                                     
059800                        SLAG-KVREFPKT                                     
059900                        SLAG-KVREFBER                                     
060000                        SLAG-KVREFOVL                                     
060100                        SLAG-TIREFPKT                                     
060300           IF SLAG-KDREFSTA = 'A'                                         
060400              MOVE DAGENS-DATUM TO SLAG-TIREFSTA                          
060500           END-IF                                                         
060600           MOVE 'P'    TO SLAG-KDREFSTA                                   
060800           MOVE ZERO   TO SLAG-KVPB-HIST                                  
060900                          SLAG-KVPBREOI-HIST                              
061000           MOVE ZERO   TO SLAG-TIREFMPB                                   
061100                          SLAG-TIREFPAF                                   
061300           MOVE JA     TO SLAG-FLPB-FLYTT                                 
061500        END-IF                                                            
061700        PERFORM IMS-REPL-WDK711                                           
061800        ADD +1 TO CHKP-ANT                                                
061810        PERFORM IMS-GHU-WDK727                                            
061820        IF SEGMENT-FINNS                                                  
061821          MOVE PROG-KVPB-JUST(1)     TO WS-KVPB-JUST-1                    
061822          MOVE PROG-TIPBJUST(1)      TO WS-TIPBJUST-1                     
061823          MOVE PROG-KVPB-JUST(2)     TO WS-KVPB-JUST-2                    
061824          MOVE PROG-TIPBJUST(2)      TO WS-TIPBJUST-2                     
061830        END-IF                                                            
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
064300     MOVE JA TO FORSTA-SW                                                 
064400     .                                                                    
064500     EJECT                                                                
064600                                                                          
064700                                                                          
064800 BB-BER-SLUTDATUM SECTION.                                                
064900                                                                          
065000     MOVE ZERO TO     WS-KVPB-DAG-IX-RP                                   
065100                      WS-KVPB-IX-RP                                       
065300                                                                          
065400*    DISPONIBELT                                                          
065500     COMPUTE WS-KVPB-TOT = SLAG-KVPB-REF + SLAG-KVPBREOI                  
065600     IF WS-KVPB-TOT > ZERO                                                
065700        COMPUTE WS-KVDISP-AKT-NDC =                                       
065800                   ((SLAG-KVLS + SLAG-KVRESS +                            
065900                     SLAG-KVBEART) -                                      
066000                    (SLAG-KVOKS-DAG + SLAG-KVOKS-BULK +                   
066100                     SLAG-KVROS-DAG + SLAG-KVROS-BULK))                   
066200                                                                          
066210        PERFORM BBB-DEMAND-GT-1YEAR                                       
066220                                                                          
066230        IF PLUS-ONE-YEAR                                                  
066240          CONTINUE                                                        
066250        ELSE                                                              
066300          IF WS-KVDISP-AKT-NDC > ZERO                                     
066400            MOVE WS-KVDISP-AKT-NDC TO WS-KVDISP-REST                      
066500                                                                          
066600            PERFORM UNTIL WS-KVDISP-REST <= 0                             
066700                                                                          
066800               PERFORM BBA-BER-ARBD-AKT-RP                                
066900*              BERÄKNA DAGSBEHOV, MULTIPLICERA MED ARB-DAGAR              
067000*              I PERIODEN, HÅLL PÅ TILLS DISPONIBELT ÄR SLUT              
067100               IF DCS-NDC-CN                                              
067300               OR DCS-JAPAN                                               
067400               OR DCS-INDIA                                               
067410               OR DCS-EMIRATES                                            
067500***              HAR 6 ARBETSDAGAR                                        
067600                 COMPUTE WS-KVPB-DAG-IX-RP =                              
067700                         (((SLAG-KVPB-REF *                               
067800                            SLAG-RESEASON (WS-RP-FIRST-TIRP)) /           
067900                            4.33) / 6)                                    
068000                                                                          
068100                 COMPUTE WS-KVPB-DAG-IX-RP = WS-KVPB-DAG-IX-RP +          
068200                         (SLAG-KVPBREOI / 4.33 / 6)                       
068300               ELSE                                                       
068400                 COMPUTE WS-KVPB-DAG-IX-RP =                              
068500                         (((SLAG-KVPB-REF *                               
068600                            SLAG-RESEASON (WS-RP-FIRST-TIRP)) /           
068700                            4.33) / 5)                                    
068800                                                                          
068900                 COMPUTE WS-KVPB-DAG-IX-RP = WS-KVPB-DAG-IX-RP +          
069000                         (SLAG-KVPBREOI / 4.33 / 5)                       
069100               END-IF                                                     
069200                                                                          
069300               COMPUTE WS-KVPB-RP ROUNDED =                               
069400                       WS-KVPB-DAG-IX-RP * WS-KVARBD-IX-RP                
069500                                                                          
069600               MOVE   WS-KVDISP-REST TO WS-KVDISP-REST-SPAR               
069700               COMPUTE WS-KVDISP-REST =                                   
069800                       WS-KVDISP-REST - WS-KVPB-RP                        
069900            END-PERFORM                                                   
070000                                                                          
070100*           OM DET FINNS BRIST NÄR SISTA PERIODEN ÄR SLUT                 
070200*           HUR MÅNGA DAGAR AV SISTA PERIODE RÄCKER LAGRET?               
070300                                                                          
070400            IF WS-KVDISP-REST < 0                                         
070500               IF WS-KVPB-DAG-IX-RP = ZERO                                
070600                  MOVE 1 TO WS-KVPB-DAG-IX-RP                             
070700               END-IF                                                     
070800               COMPUTE WS-KVDAGAR-TACKT-LAST-RP ROUNDED =                 
070900                       WS-KVDISP-REST-SPAR / WS-KVPB-DAG-IX-RP            
071000               IF WS-KVDAGAR-TACKT-LAST-RP < 1                            
071100                  MOVE 1 TO WS-KVDAGAR-TACKT-LAST-RP                      
071200               END-IF                                                     
071300                                                                          
071400*              ANTAL ARBETSDAGAR DET FINNS TÄCKNING                       
071500*              I SISTA PERIOD                                             
071600               MOVE W27136-IDDC             TO WORK-IDDC                  
071700               MOVE WS-KVDAGAR-TACKT-LAST-RP TO WORK-KVWORKD              
071800*              MOVE WS-RP-NEXT-TIAAMMDD     TO WORK-TIAAMMDD-FOM          
071810               MOVE WS-RP-FIRST-TIAAMMDD     TO WORK-TIAAMMDD-FOM         
071900               MOVE 002                     TO WORK-KDCALL                
072000               CALL WORKDAY                 USING WORK-KDCALL             
072100                                                   WORK-DATE-AREA         
072200                                                   WORK-KDSVAR            
072300               IF WORK-KDSVAR-OK                                          
072400                  MOVE WORK-TIAAMMDD-TOM TO WS-TIREFMPB                   
072500               ELSE                                                       
072600*    EMERGENCY SCR: 4190963  UNDVIK ABEND                                 
072700*                 MOVE 'FEL FRÅN WORKDAY  I BB SECTION I W27136'          
072800*                                       TO   FELTEXT-STR                  
072900*                 DISPLAY FELTEXT                                         
073000*                 PERFORM S99-ABEND                                       
073100                  MOVE DAGENS-DATUM         TO WS-TIREFMPB                
073200*    SLUT PÅ SCR                                                          
073300               END-IF                                                     
073500            ELSE                                                          
073600               MOVE WS-RP-FIRST-TIAAMMDD  TO WS-TIREFMPB                  
073700            END-IF                                                        
073800          ELSE                                                            
073900            MOVE DAGENS-DATUM         TO WS-TIREFMPB                      
074000          END-IF                                                          
074010        END-IF                                                            
074100     ELSE                                                                 
074200       MOVE DAGENS-DATUM           TO WS-TIREFMPB                         
074300     END-IF                                                               
074560     .                                                                    
074570     EJECT                                                                
074600                                                                          
074700                                                                          
074800 BBA-BER-ARBD-AKT-RP SECTION.                                             
074900                                                                          
075000     MOVE ZERO TO WS-KVARBD-IX-RP                                         
075100                                                                          
075200     IF FORSTA                                                            
075300*       VILKEN PERIOD ÄR DET NU                                           
075400        MOVE INNEV-TIAARP TO WS-RP-FIRST-TIAARP                           
075500        MOVE DAGENS-DATUM TO WS-RP-FIRST-TIAAMMDD                         
075600        MOVE NEJ TO FORSTA-SW                                             
075700                                                                          
075800     ELSE                                                                 
075900*       VI ÄR EN PERIOD LÄNGRE FRAM ÄN FÖRRA GÅNGEN                       
076000        MOVE WS-RP-NEXT-TIAARP    TO WS-RP-FIRST-TIAARP                   
076100        MOVE WS-RP-NEXT-TIAAMMDD  TO WS-RP-FIRST-TIAAMMDD                 
076200                                                                          
076300     END-IF                                                               
076400                                                                          
076500*    VAD HETER NÄSTA PERIOD                                               
076900     MOVE WS-RP-FIRST-TIAARP TO WS-RP-NEXT-TIAARP                         
077000     IF WS-RP-FIRST-TIRP = 12                                             
077100        MOVE 01 TO WS-RP-NEXT-TIRP                                        
077200        ADD 1      TO WS-RP-NEXT-TIAA                                     
077300     ELSE                                                                 
077400        ADD 1      TO WS-RP-NEXT-TIRP                                     
077500     END-IF                                                               
077700                                                                          
077800*    NÄR STARTAR DEN                                                      
077900     MOVE 'AARP'              TO DAT-KDDATFORM                            
078000     MOVE WS-RP-NEXT-TIAARP   TO DAT-I-TIDATUM                            
078100     CALL WDATKONV USING   DAT-KDDATFORM                                  
078200                           DAT-I-TIDATUM                                  
078300                           DAT-O-TIDATUM                                  
078400                           DAT-KDSVAR                                     
078500                                                                          
078600     IF DAT-KDSVAR-OK                                                     
078700        MOVE DAT-TIAAMMDD TO WS-RP-NEXT-TIAAMMDD                          
078800     ELSE                                                                 
078900        MOVE 'FEL FRÅN WDATKONV 2 I BBA SECTION I W27136'                 
079000                                TO    FELTEXT-STR                         
079100        DISPLAY FELTEXT                                                   
079200        PERFORM S99-ABEND                                                 
079300     END-IF                                                               
079400                                                                          
079500*    HUR MÅNGA ARBETSDAGAR HAR DEN                                        
079600     MOVE W27136-IDDC           TO WORK-IDDC                              
079700     MOVE WS-RP-FIRST-TIAAMMDD  TO WORK-TIAAMMDD-FOM                      
079800     MOVE WS-RP-NEXT-TIAAMMDD   TO WORK-TIAAMMDD-TOM                      
079900     MOVE 001                   TO WORK-KDCALL                            
080000     CALL WORKDAY                USING WORK-KDCALL                        
080100                                       WORK-DATE-AREA                     
080200                                       WORK-KDSVAR                        
080300     IF WORK-KDSVAR-OK                                                    
080400        MOVE WORK-KVWORKD       TO WS-KVARBD-IX-RP                        
080500     ELSE                                                                 
080600        MOVE 'FEL FRÅN WORKDAY  I BBA SECTION I W27136' TO                
080700                                    FELTEXT-STR                           
080800        DISPLAY FELTEXT                                                   
080900        PERFORM S99-ABEND                                                 
081000     END-IF                                                               
081100     .                                                                    
081300     EJECT                                                                
081400                                                                          
081410 BBB-DEMAND-GT-1YEAR     SECTION.                                         
081411                                                                          
081412**IF STOCK LAST MORE THAN 1 YEAR SET TIREFMPB TO                          
081413**TODAYS DATE + 1 YEAR AND STOP CALCULATING                               
081414     MOVE NEJ               TO ONE-YEAR-SW                                
081415     MOVE ZERO              TO WS-NO-MONTHS                               
081416                                                                          
081417     COMPUTE WS-NO-MONTHS = WS-KVDISP-AKT-NDC                             
081418                          / WS-KVPB-TOT                                   
081419                                                                          
081420     IF WS-NO-MONTHS > 12                                                 
081421       MOVE DD-PLUS-ETT-AR  TO WS-TIREFMPB                                
081423       MOVE JA              TO ONE-YEAR-SW                                
081424     END-IF                                                               
081430     .                                                                    
081440     EJECT                                                                
081500 C-BEHANDLA-TILLKOMMANDE SECTION.                                         
081600                                                                          
081700     PERFORM IMS-GU-ERSA01                                                
081800     IF SEGMENT-FINNS                                                     
081810        MOVE ERSA01-DIERS-ERS   TO WS-DIERS-ERS                           
081900        PERFORM IMS-GNP-ERSA11                                            
082000        PERFORM UNTIL SEGMENT-SAKNAS                                      
082100           MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR                         
082110           MOVE ERSA11-DIERS-TILLK TO WS-DIERS-TILLK                      
082200           PERFORM IMS-GU-ARTC01                                          
082300           IF SEGMENT-FINNS                                               
082400              IF ART-KDERS-UTG > 0                                        
082500                 PERFORM  S30-SKAPA-MEMOFIL                               
082600                 MOVE ART-KDERS-UTG TO W27137-KDERS-UTG-TILLK             
082700                 MOVE ZERO          TO W27137-KDERS-TILLK                 
082800                 PERFORM S12-SKRIV-W27137                                 
082900              ELSE                                                        
083000                 PERFORM IMS-GNP-ARTC11                                   
083001                 MOVE ZERO       TO WS-KDERS-TILLK                        
083010                 MOVE CLAG-KDERS TO WS-KDERS-TILLK                        
083100                 IF WS-KDERS-TILLK > 09                                   
083200*                OR WS-KDERS-TILLK = 11 OR 17 OR 21 OR 27                 
083410*                IF ( DCS-NDC                                             
083500*                AND  CLAG-KDERS > 19)                                    
083610*                OR ( DCS-SDC                                             
083700*                     AND (CLAG-KDERS > 9 AND                             
083800*                                (CLAG-KDERS NOT = 11 OR 17 OR 21         
083900*                                           OR 22 OR 23 OR 27)))          
084000*                OR (DCS-SDC AND CLAG-KDERS > 9)                          
084100*                OR (DCS-SDC AND (CLAG-KDERS NOT = 11 OR 21 OR            
084200*                                              22 OR 23 OR 27))           
084300                    PERFORM S30-SKAPA-MEMOFIL                             
084400                    MOVE ART-KDERS-UTG  TO                                
084500                                   W27137-KDERS-UTG-TILLK                 
084600                    MOVE CLAG-KDERS     TO W27137-KDERS-TILLK             
084700                    PERFORM S12-SKRIV-W27137                              
084800                 ELSE                                                     
084900                    PERFORM CA-WDK7-TILLKOMMANDE                          
085000                 END-IF                                                   
085100              END-IF                                                      
085200           END-IF                                                         
085300           PERFORM IMS-GNP-ERSA11                                         
085400        END-PERFORM                                                       
085500     END-IF                                                               
085600     .                                                                    
085700     EJECT                                                                
085800                                                                          
085900                                                                          
086000 CA-WDK7-TILLKOMMANDE SECTION.                                            
086100                                                                          
086200     MOVE ERSA11-IDARTNR-TILLK   TO W-IDARTNR                             
086300     MOVE W27136-IDDC            TO W-IDDC                                
086310     MOVE NEJ                    TO PURCH-SW                              
086400                                                                          
086500     PERFORM IMS-GHU-WDK711                                               
086600     IF SEGMENT-FINNS                                                     
086610        MOVE +1      TO WS-RATIO                                          
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
088110          END-IF                                                          
088200        END-IF                                                            
088210        IF SLAG-IDDC-REF = SPACE                                          
088220          MOVE JA TO PURCH-SW                                             
088230        END-IF                                                            
088400        PERFORM CA3-FORANDRA-PB                                           
088600                                                                          
088700        PERFORM CA2-UPDATE-RESEASON                                       
088800                                                                          
088900                                                                          
089000        PERFORM IMS-REPL-WDK711                                           
089100        ADD +1 TO CHKP-ANT                                                
089200        MOVE W27136-IDARTNR  TO W27135-IDARTNR-OLD                        
089300        MOVE ERSA11-IDARTNR-TILLK                                         
089400                             TO W27135-IDARTNR-NEW                        
089500        MOVE W27136-IDDC     TO W27135-IDDC                               
089600        PERFORM S13-SKRIV-W27135                                          
089700     ELSE                                                                 
089800        MOVE ALL '+'      TO WDK7-W005WDK7                                
089900        MOVE 'WDK711'     TO WDK7-IDSEGM                                  
090000        MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                             
090100        MOVE W27136-IDDC  TO WDK7-IDDC-KFB                                
090200                             WDK7-IDDC                                    
090300        PERFORM CA1-INIT-WDK711                                           
090400                                                                          
090500        CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB               
090600                                          WDK7-PCB                        
090700        ADD +1 TO CHKP-ANT                                                
090800     END-IF                                                               
090900***MOVE FORECAST IN THE FUTURE TO NEW PART                                
090901*    MOVE NEJ    TO WDK727-SW                                             
090902     IF PURCH-JA                                                          
090903       CONTINUE                                                           
090904     ELSE                                                                 
090910       IF WS-KVPB-JUST-1 > 0                                              
090920         PERFORM IMS-GHU-WDK727                                           
090930         IF SEGMENT-FINNS                                                 
090931***IF   THE NEW PART ALREADY HAS A FUTURE FORECAST DO NOT UPDATE          
090932           IF PROG-KVPB-JUST(1) > 0                                       
090933             CONTINUE                                                     
090934           ELSE                                                           
090935             COMPUTE WS-KVPB-JUST-1 = WS-KVPB-JUST-1 *                    
090936                                      WS-RATIO                            
090937             COMPUTE WS-KVPB-JUST-2 = WS-KVPB-JUST-2 *                    
090938                                      WS-RATIO                            
090940             MOVE WS-KVPB-JUST-1  TO PROG-KVPB-JUST(1)                    
090950             MOVE WS-TIPBJUST-1   TO PROG-TIPBJUST(1)                     
090960             MOVE WS-KVPB-JUST-2  TO PROG-KVPB-JUST(2)                    
090970             MOVE WS-TIPBJUST-2   TO PROG-TIPBJUST(2)                     
090980             PERFORM IMS-REPL-WDK727                                      
090981*            MOVE JA TO WDK727-SW                                         
090982           END-IF                                                         
090990         ELSE                                                             
090991           MOVE ALL '+'   TO WDK7-W005WDK7                                
090992           MOVE 'WDK727' TO WDK7-IDSEGM                                   
090993           MOVE W-IDARTNR TO WDK7-IDARTNR-KFB                             
090994           MOVE W27136-IDDC TO WDK7-IDDC-KFB                              
090995           COMPUTE WS-KVPB-JUST-1 = WS-KVPB-JUST-1 *                      
090996                                    WS-RATIO                              
090997           COMPUTE WS-KVPB-JUST-2 = WS-KVPB-JUST-2 *                      
090998                                    WS-RATIO                              
090999           MOVE WS-KVPB-JUST-1    TO WDK7-KVPB-JUST(1)                    
091000           MOVE WS-TIPBJUST-1     TO WDK7-TIPBJUST(1)                     
091001           MOVE WS-KVPB-JUST-2    TO WDK7-KVPB-JUST(2)                    
091002           MOVE WS-TIPBJUST-2     TO WDK7-TIPBJUST(2)                     
091003                                                                          
091004           CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB            
091005                                             WDK7-PCB                     
091006           ADD +1 TO CHKP-ANT                                             
091007*          MOVE JA TO WDK727-SW                                           
091008         END-IF                                                           
091014       END-IF                                                             
091022     END-IF                                                               
091023****DELETES  OLD PARTNO'S WDK727 SEGMENT SINCE VALUES ARE MOVED TO        
091024****NEW   PARTNO.                                                         
091025     MOVE W-IDARTNR-OLD      TO W-IDARTNR                                 
091026     PERFORM IMS-GHU-WDK727                                               
091027     IF SEGMENT-FINNS                                                     
091028       PERFORM IMS-DLET-WDK727                                            
091029     END-IF                                                               
091030*    UPDATE BUYER FOR ALL DCS IN NDC-NA                                   
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
095300                                                                          
095310     COMPUTE WS-RATIO = WS-DIERS-TILLK /                                  
095320                        WS-DIERS-ERS                                      
095400     MOVE ZERO TO WS-KVPB-REF-ADD                                         
095500     COMPUTE WS-KVPB-REF-ADD =                                            
095600                WS-KVPB-REF-OLD * WS-RATIO                                
095700     MOVE WS-KVPB-REF-ADD TO WDK7-KVPB-REF                                
095800*****FLYTTA PB TILL TILLKOMMANDE ARTIKEL ISF ADDERA ENL OVAN              
095900                                                                          
096000     MOVE ZERO TO WS-KVPBREOI-ADD                                         
096100     COMPUTE WS-KVPBREOI-ADD =                                            
096200                WS-KVPBREOI-OLD * WS-RATIO                                
096300     MOVE WS-KVPBREOI-ADD TO WDK7-KVPBREOI                                
096400*****FLYTTA PB TILL TILLKOMMANDE ARTIKEL ISF ADDERA ENL OVAN              
096500                                                                          
096600     IF W27136-FLPB-FLYTT = JA                                            
096700       CONTINUE                                                           
096800     ELSE                                                                 
096900       MOVE 'N'   TO WDK7-FLREFBEO                                        
097000     END-IF                                                               
097010     MOVE WS-TIREFMPB      TO WDK7-TIREFMPB                               
097100                                                                          
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
101300 CA3-FORANDRA-PB SECTION.                                                 
101400                                                                          
101410     COMPUTE WS-RATIO = WS-DIERS-TILLK /                                  
101420                        WS-DIERS-ERS                                      
101500     MOVE ZERO TO WS-KVPB-REF-ADD                                         
101600     COMPUTE WS-KVPB-REF-ADD =                                            
101700                WS-KVPB-REF-OLD * WS-RATIO                                
101800     COMPUTE SLAG-KVPB-REF =                                              
101900                  SLAG-KVPB-REF + WS-KVPB-REF-ADD                         
102000*****FLYTTA PB TILL TILLKOMMANDE ARTIKEL ISF ADDERA ENL OVAN              
102100     MOVE SLAG-KVPB-REF       TO SLAG-KVPB-HIST                           
102200***************************************************************           
102300     MOVE ZERO TO WS-KVPBREOI-ADD                                         
102400     COMPUTE WS-KVPBREOI-ADD =                                            
102500                WS-KVPBREOI-OLD * WS-RATIO                                
102600     COMPUTE SLAG-KVPBREOI =                                              
102700                  SLAG-KVPBREOI + WS-KVPBREOI-ADD                         
102800*****FLYTTA PB TILL TILLKOMMANDE ARTIKEL ISF ADDERA ENL OVAN              
102900     MOVE SLAG-KVPBREOI       TO SLAG-KVPBREOI-HIST                       
103000                                                                          
103100     IF W27136-FLPB-FLYTT = JA                                            
103200       CONTINUE                                                           
103300     ELSE                                                                 
103400       MOVE 'N'   TO SLAG-FLREFBEO                                        
103500     END-IF                                                               
103510     MOVE WS-TIREFMPB      TO SLAG-TIREFMPB                               
103600                                                                          
104800     .                                                                    
104900     EJECT                                                                
105000                                                                          
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
106700     CLOSE W27136                                                         
106800           W27137                                                         
106900           W27135                                                         
107000     SKIP2                                                                
107100     MOVE 'S' TO POSTSUM-OPKOD                                            
107200     CALL POSTSUM USING POSTSUM-PARM                                      
107300                                                                          
107400     .                                                                    
107500     EJECT                                                                
107600                                                                          
107700                                                                          
107800 S01-LAES-W27136  SECTION.                                                
107900     SKIP2                                                                
108000     READ W27136 INTO W27136-AREA                                         
108100     AT END                                                               
108200        SET END-OF-W27136 TO TRUE                                         
108300                                                                          
108400     NOT AT END                                                           
108500        MOVE 'W27136' TO POSTSUM-FDNAMN                                   
108600        MOVE 'W27136D1' TO POSTSUM-DDNAMN2                                
108700        CALL POSTSUM USING POSTSUM-PARM                                   
108800        ADD +1 TO WS-KVPOST-IN                                            
108900     END-READ                                                             
109000     .                                                                    
109100     EJECT                                                                
109200                                                                          
109300                                                                          
109400 S12-SKRIV-W27137 SECTION.                                                
109500     SKIP2                                                                
109600     WRITE W27137-POST FROM W27137-AREA                                   
109700                                                                          
109800     MOVE 'W27137 ' TO POSTSUM-FDNAMN                                     
109900     MOVE 'W27136D2' TO POSTSUM-DDNAMN2                                   
110000     CALL POSTSUM USING POSTSUM-PARM                                      
110100     .                                                                    
110200     EJECT                                                                
110300                                                                          
110400                                                                          
110500 S13-SKRIV-W27135 SECTION.                                                
110600     SKIP2                                                                
110700     WRITE W27135-POST FROM W27135-AREA                                   
110800                                                                          
110900     MOVE 'W27135 ' TO POSTSUM-FDNAMN                                     
111000     MOVE 'W27136D3' TO POSTSUM-DDNAMN2                                   
111100     CALL POSTSUM USING POSTSUM-PARM                                      
111200     .                                                                    
111300     EJECT                                                                
111400                                                                          
111500                                                                          
111600 S30-SKAPA-MEMOFIL SECTION.                                               
111700                                                                          
111800     MOVE W27136-IDARTNR   TO W27137-IDARTNR-ERS                          
111900     MOVE W27136-IDDC      TO W27137-IDDC-ERS                             
112000     MOVE ERSA11-IDARTNR-TILLK TO                                         
112100                      W27137-IDARTNR-TILLK                                
112200     MOVE WS-IDPERSON-BUY-OLD TO                                          
112300                      W27137-IDPERSON-BUY-ERS                             
112400     .                                                                    
112500                                                                          
112600                                                                          
112700 X-TAG-CHECKPOINT   SECTION.                                              
112800                                                                          
112900     PERFORM IMS-LAS-ATERSTART                                            
113000     MOVE WS-KVPOST-IN   TO CKPA-2250-KVPOST                              
113100     MOVE W27136-IDARTNR TO CKPA-2250-IDARTNR                             
113200     MOVE W27136-IDDC    TO CKPA-2250-IDDC                                
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
120410 IMS-GHU-WDK727 SECTION.                                                  
120420                                                                          
120430     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
120440          DELIMITED BY SIZE INTO SSA1                                     
120450     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
120460          DELIMITED BY SIZE INTO SSA2                                     
120461     STRING 'WDK727  (KDSEGKEY =' W-KDSEGKEY ')'                          
120462          DELIMITED BY SIZE INTO SSA3                                     
120470     MOVE '  GE' TO GODK-STATUSKODER                                      
120480     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK727 SSA1 SSA2 SSA3         
120490     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
120491     PERFORM IMS-STATUSKONTROLL                                           
120492     .                                                                    
120493     EJECT                                                                
120494                                                                          
120495 IMS-REPL-WDK727 SECTION.                                                 
120496                                                                          
120497     MOVE '  ' TO GODK-STATUSKODER                                        
120498     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK727                       
120499     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
120500     PERFORM IMS-STATUSKONTROLL                                           
120501     .                                                                    
120502     EJECT                                                                
120503                                                                          
120504 IMS-DLET-WDK727 SECTION.                                                 
120505                                                                          
120506     MOVE '  ' TO GODK-STATUSKODER                                        
120507     CALL CBLTDLI USING DLET WDK7-PCB DLI-IO-WDK727                       
120508     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
120509     PERFORM IMS-STATUSKONTROLL                                           
120510     .                                                                    
120511     EJECT                                                                
120512                                                                          
120520 IMS-GHU-OIGA11 SECTION.                                                  
120600                                                                          
120700     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
120800          DELIMITED BY SIZE INTO SSA1                                     
120900     STRING 'WLOIGA11(IDDC     =' W-IDDC-X ')'                            
121000          DELIMITED BY SIZE INTO SSA2                                     
121100     MOVE '  GE' TO GODK-STATUSKODER                                      
121200     CALL CBLTDLI USING GHU OIGA-PCB DLI-IO-OIGA11 SSA1 SSA2              
121300     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
121400     PERFORM IMS-STATUSKONTROLL                                           
121500     .                                                                    
121600 IMS-REPL-OIGA11 SECTION.                                                 
121700                                                                          
121800     MOVE '  ' TO GODK-STATUSKODER                                        
121900     CALL CBLTDLI USING REPL OIGA-PCB DLI-IO-OIGA11                       
122000     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
122100     PERFORM IMS-STATUSKONTROLL                                           
122200     .                                                                    
122300     EJECT                                                                
122400                                                                          
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
