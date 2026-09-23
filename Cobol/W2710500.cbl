000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2710500.                                                
000400*AUTHOR.         JOHAN NIHLBLAD.                                          
000500*DATE-WRITTEN.   JUNE 2005.                                               
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER RESTORDER ,RÄKNAR SUPERWEEK ,LÄSER              
001100*        STD-PRIS M.M. FÖR ATT FÅ FRAM EN LISTA ATT KUNNA                 
001200*        HITTA EVENTUELLA RESTORDER SOM EJ LÄNGRE ÄR NÖDVÄNDIGA.          
001300*        FÅNGAR ALLA RESTORDER FÖR REFILL SOM ÄR ÄLDRE ÄN 6 VECKOR        
001400*        I PGM W27103 LÄSES ALLA RESTORDER UT FÖR ATT FÅ FRAM             
001500*        TOTALSUMMA FÖR RESTORDER REFILL.                                 
001600*                                                                         
001700*        PROGRAMMET LÄSER      WDK6                                       
001800*                              WDK7                                       
001900*                              WDA5                                       
002000*    ABENDKODER:                                                          
002100*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
002200*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400                                                                          
003500     SELECT W27105                     ASSIGN TO W27105D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100                                                                          
004200 FD  W27105                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500*01  POST -COPY W27105 -PRE  UT-  -L.                                     
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP2                                                                
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W2710500'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  SKRIV-SW                    PIC X       VALUE 'J'.                   
005500 77  IMS-SECTION                 PIC X(16)   VALUE SPACE.                 
005600                                                                          
005700*    --- ARBETSFÄLT                                                       
005800 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
005900 01  ARBETSFAELT.                                                         
006000     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
006100                                                                          
006200     03  WS-KVPB-REF-WEEK    PIC S9(6)V9(2) VALUE ZERO COMP-3.            
006300     03  WS-KVPB-REF-SEAS    PIC S9(6)V9(2) VALUE ZERO COMP-3.            
006400     03  WS-SUPERWEEK        PIC S9(6)V9(2) VALUE ZERO COMP-3.            
006500     03  WS-REF-FROM-DC      PIC X(2)       VALUE SPACE.                  
006600                                                                          
006700     03  WS-CURRENT-DATE.                                                 
006800         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
006900         05  FILLER              PIC 9(4)   VALUE ZERO.                   
007000         05  FILLER              PIC 9(6)   VALUE ZERO.                   
007100                                                                          
007200     03  FILLER REDEFINES WS-CURRENT-DATE.                                
007300*-----   INKLUSIVE SEKEL                                                  
007400         05  WS-DAGENS-DATUM.                                             
007500             07 WS-DAGENS-SEKEL  PIC 9(2).                                
007600             07 WS-DAGENS-AAMMDD PIC 9(6).                                
007700         05  WS-DAGENS-TID.                                               
007800             07 WS-DAGENS-TIMME  PIC 9(2).                                
007900             07 WS-DAGENS-MINUT  PIC 9(2).                                
008000             07 WS-DAGENS-SEKUND PIC 9(2).                                
008100                                                                          
008200     03  WS-TIAAVVD              PIC 9(5)    VALUE ZERO.                  
008300     03  FILLER REDEFINES WS-TIAAVVD.                                     
008400         05 WS-TIAAVV            PIC 9(4).                                
008500         05 WS-TIDAG             PIC 9(1).                                
008600                                                                          
008700     03  WS-TIAAVVD-6V           PIC 9(5)    VALUE ZERO.                  
008800     03  FILLER REDEFINES WS-TIAAVVD-6V.                                  
008900         05 WS-TIAAVV-6V         PIC 9(4).                                
009000         05 WS-TIDAG-6V          PIC 9(1).                                
009100                                                                          
009200     03  WS-TIAAAAMMDD-6V        PIC 9(8)    VALUE ZERO.                  
009300     03  FILLER REDEFINES WS-TIAAAAMMDD-6V.                               
009400         05 WS-TISEKEL           PIC 9(2).                                
009500         05 WS-TIAAMMDD          PIC 9(6).                                
009600                                                                          
009700     03 WS-KVOKS-TOT             PIC S9(7)   VALUE ZERO COMP-3.           
009800     03 WS-PRMATRL          PIC S9(7)V9(2) VALUE ZERO COMP-3.             
009900     03 WS-TOTALSUM              PIC 9(8)    VALUE ZERO.                  
010000     03 WS-AVAILABLE             PIC S9(7)           COMP-3.              
010100                                                                          
010200                                                                          
010300     EJECT                                                                
010400*   ---USED TO CHECK IF CUSTOMER FROM CHINA OR NORTH AMERICA              
010500 77  I-IX                        PIC 9(1)   VALUE ZERO.                   
010600                                                                          
010700*77  WS-KUND-SW                  PIC X(2)   VALUE SPACES.                 
010800*    88  WS-KUND-CN                         VALUE '71' '72' '73'.         
010900*    88  WS-KUND-NA                         VALUE '41' '42' '43'          
011000*                                                 '44' '45' '46'          
011100*                                                 '51'.                   
011200                                                                          
011300 01  WS-IDKUNDNR                 PIC 9(6)   VALUE ZERO.                   
011400 01  FILLER REDEFINES WS-IDKUNDNR.                                        
011500     03  WS-IDKUNDNR-TAB OCCURS 6.                                        
011600         05  WS-IDKUNDNR-N       PIC 9(1).                                
011700 01  WS-IDKUNDNR-X                          VALUE SPACES.                 
011800     03  WS-IDKUNDNR-CN-NA       PIC X(2).                                
011900     03  FILLER                  PIC X(4).                                
012000                                                                          
012100     EJECT                                                                
012200*                                                                         
012300 01  FILLER                      PIC X(16)   VALUE 'DIST-DC-TAB'.         
012400     -COPY WWDIST57                                                       
012500                                                                          
012600*01  -COPY WWDIST35                                                       
012700     EJECT                                                                
012800*      --- VALID IDDC CODES                                               
012900*                                                                         
013000*01    -COPY WWDC99                                                       
013100*01    -COPY WWDC99 -PRE FROM-                                            
013200*01    -COPY WWDC99 -PRE KUND-                                            
013300       EJECT                                                              
013400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013500 01  FILLER REDEFINES DAGENS-DATUM.                                       
013600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013900     EJECT                                                                
014000                                                                          
014100 01  DYNAMISKA-SUBPROGRAM.                                                
014200*                                                                         
014300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
014400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
014700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014900     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
015000     SKIP2                                                                
015100*    --- PARAMETRAR TILL ABEND                                            
015200                                                                          
015300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
015400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
015500     SKIP2                                                                
015600 01  FELTEXT.                                                             
015700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
015800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
015900     EJECT                                                                
016000*    --- PARAMETRAR TILL W009VADD                                         
016100*                                                                         
016200 01  W009VADD-AREA.                                                       
016300     03 VADD-DATUM-AAVV          PIC S9(5)  COMP-3.                       
016400     03 VADD-ANTAL               PIC S9(3)  COMP-3.                       
016500     EJECT                                                                
016600                                                                          
016700*    --- PARAMETRAR TILL DATKORT                                          
016800*                                                                         
016900 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27105'.              
017000     SKIP2                                                                
017100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
017200     SKIP2                                                                
017300*01  -COPY WDATKORT                                                       
017400     EJECT                                                                
017500*    --- PARAMETRAR TILL POSTSUM                                          
017600*                                                                         
017700*01  -COPY W0005   -PRE  POSTSUM-                                         
017800     EJECT                                                                
017900*    --- PARAMETRAR TILL WDATKONV                                         
018000*                                                                         
018100*01  -COPY WDATAREA                                                       
018200     EJECT                                                                
018300 01  UT-AREA-START              PIC X(24)   VALUE                         
018400                                 'UT-AREA-START  '.                       
018500     SKIP2                                                                
018600                                                                          
018700*01  AREA -COPY W27105     -PRE UT-                                       
018800     EJECT                                                                
018900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019000                                                                          
019100     SKIP3                                                                
019200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019300     SKIP3                                                                
019400 01  NYCKLAR-TILL-DLI.                                                    
019500     03  W-IDARTNR-X.                                                     
019600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019700                                                                          
019800     03  W-IDDC-X.                                                        
019900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
020000                                                                          
020100     03  W-KDSEGKEY-X.                                                    
020200         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
020300                                                                          
020400     03  W-IDLAND-X.                                                      
020500         05  W-IDLAND            PIC X(2)   VALUE SPACE.                  
020600*                                                                         
020700     SKIP2                                                                
020800*    --- STATUS-KOD FRÅN IMS                                              
020900 01  STATUS-WS                   PIC XX.                                  
021000     88  SEGMENT-FINNS                       VALUE '  '.                  
021100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
021300     SKIP2                                                                
021400 01  GODK-STATUSKODER.                                                    
021500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021600     SKIP3                                                                
021700 01  SSA1                        PIC X(64).                               
021800 01  SSA2                        PIC X(64).                               
021900     EJECT                                                                
022000*    --- IMS FUNKTIONSKODER                                               
022100*01  -COPY W0003                                                          
022200     EJECT                                                                
022300*    ---  DLI INPUT-OUTPUT AREA                                           
022400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDA501'.           
022500     SKIP3                                                                
022600 01  DLI-IO-AREA-WDA501.                                                  
022700*    03  -COPY WDA501                                                     
022800     SKIP3                                                                
022900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
023000     SKIP3                                                                
023100 01  DLI-IO-AREA-WDK601.                                                  
023200*    03  -COPY WDK601                                                     
023300     SKIP3                                                                
023400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
023500     SKIP3                                                                
023600 01  DLI-IO-AREA-WDK611.                                                  
023700*    03  -COPY WDK611                                                     
023800     EJECT                                                                
023900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK701'.           
024000     SKIP3                                                                
024100 01  DLI-IO-AREA-WDK701.                                                  
024200*    03  -COPY WDK701                                                     
024300     SKIP3                                                                
024400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK711'.           
024500 01  DLI-IO-AREA-WDK711.                                                  
024600*    03  -COPY WDK711                                                     
024700     EJECT                                                                
024800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK712'.           
024900 01  DLI-IO-AREA-WDK712.                                                  
025000*    03  -COPY WDK712                                                     
025100     EJECT                                                                
025200 LINKAGE SECTION.                                                         
025300                                                                          
025400     EJECT                                                                
025500*01  -COPY W0008  -PRE WDA5-                                              
025600     05  FILLER                  PIC X.                                   
025700     EJECT                                                                
025800*01  -COPY W0008  -PRE WDK6-                                              
025900     05  FILLER                  PIC X.                                   
026000     EJECT                                                                
026100*01  -COPY W0008  -PRE WDK7-                                              
026200     05  FILLER                  PIC X.                                   
026300     EJECT                                                                
026400 PROCEDURE DIVISION  USING WDA5-PCB WDK6-PCB WDK7-PCB.                    
026500                                                                          
026600     ENTRY 'DLITCBL' USING WDA5-PCB WDK6-PCB WDK7-PCB.                    
026700                                                                          
026800     PERFORM A-INIT                                                       
026900     PERFORM IMS-GET-WDA5                                                 
027000     MOVE RAD-IDARTNR        TO W-IDARTNR                                 
027100     MOVE RAD-IDDC           TO WS-IDDC                                   
027200                                W-IDDC                                    
027300     MOVE RAD-IDDISTR        TO DIST35-IDDISTR                            
027400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
027500       IF  RAD-KDSTARAD = '2'                                             
027600       AND RAD-DARODAT < WS-TIAAAAMMDD-6V                                 
027700       AND CDC AND DIST35-REFILL                                          
027800         PERFORM B-BEHANDLA-ARTIKEL                                       
027900       END-IF                                                             
028000       PERFORM IMS-GET-WDA5                                               
028100       MOVE RAD-IDARTNR      TO W-IDARTNR                                 
028200       MOVE RAD-IDDC         TO WS-IDDC                                   
028300                                W-IDDC                                    
028400       MOVE RAD-IDDISTR      TO DIST35-IDDISTR                            
028500       MOVE ZERO             TO WS-SUPERWEEK                              
028600                                WS-KVPB-REF-WEEK                          
028700                                WS-KVPB-REF-SEAS                          
028800     END-PERFORM                                                          
028900                                                                          
029000     PERFORM Z-FINIT                                                      
029100                                                                          
029200     MOVE ZERO TO RETURN-CODE                                             
029300     GOBACK                                                               
029400     .                                                                    
029500     EJECT                                                                
029600 A-INIT SECTION.                                                          
029700                                                                          
029800     OPEN OUTPUT W27105                                                   
029900                                                                          
030000     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
030100     PERFORM AA-OMVANDLA-DATUM                                            
030200     .                                                                    
030300     EJECT                                                                
030400                                                                          
030500 AA-OMVANDLA-DATUM SECTION.                                               
030600                                                                          
030700                                                                          
030800     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
030900     MOVE WS-DAGENS-AAMMDD TO DAT-I-TIDATUM                               
031000                                                                          
031100     CALL WDATKONV USING DAT-KDDATFORM                                    
031200                       DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR             
031300                                                                          
031400     IF DAT-KDSVAR-OK                                                     
031500        MOVE DAT-TIAAVVD  TO WS-TIAAVVD                                   
031600     ELSE                                                                 
031700        MOVE NEJ TO SKRIV-SW                                              
031800     END-IF                                                               
031900     MOVE WS-TIAAVV           TO VADD-DATUM-AAVV                          
032000     MOVE -6                  TO VADD-ANTAL                               
032100     CALL W009VADD            USING VADD-DATUM-AAVV                       
032200                                    VADD-ANTAL                            
032300     MOVE VADD-DATUM-AAVV     TO WS-TIAAVV-6V                             
032400     MOVE WS-TIDAG            TO WS-TIDAG-6V                              
032500     MOVE 'AAVVD' TO DAT-KDDATFORM                                        
032600     MOVE WS-TIAAVVD-6V    TO DAT-I-TIDATUM                               
032700                                                                          
032800     CALL WDATKONV USING DAT-KDDATFORM                                    
032900                       DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR             
033000                                                                          
033100     IF DAT-KDSVAR-OK                                                     
033200        MOVE DAT-TIAAMMDD TO WS-TIAAMMDD                                  
033300     ELSE                                                                 
033400        MOVE NEJ TO SKRIV-SW                                              
033500     END-IF                                                               
033600     MOVE WS-DAGENS-SEKEL TO WS-TISEKEL                                   
033700     .                                                                    
033800     EJECT                                                                
033900                                                                          
034000 B-BEHANDLA-ARTIKEL SECTION.                                              
034100                                                                          
034200     PERFORM BA-RAKNA-SUPERWEEK                                           
034300     PERFORM BB-RAKNA-VARDE                                               
034400     IF NOT (DIST35-REFILL-NA                                             
034500        OR  DIST35-REFILL-NP                                              
034510        OR  DIST35-REFILL-NX                                              
034520        OR  DIST35-REFILL-NS                                              
034600        OR  DIST35-REFILL-CN)                                             
034800        IF  (WS-SUPERWEEK > 12)                                           
034900          IF (CLAG-PRARTSTD > 10)                                         
035000          OR (WS-TOTALSUM > 200)                                          
035100            MOVE SLAG-IDPERSON-BUY TO UT-IDPERSON                         
035200            MOVE W-IDARTNR         TO UT-IDARTNR                          
035300            MOVE WS-TOTALSUM       TO UT-TOTALSUM                         
035400            MOVE CLAG-PRARTSTD     TO UT-PRARTSTD                         
035500            MOVE WS-AVAILABLE      TO UT-AVAILABLE                        
035600            MOVE WS-IDDC           TO UT-IDDC                             
035700            MOVE SLAG-KVPB-REF     TO UT-KVPB-REF                         
035800            MOVE RAD-IDDISTR       TO UT-IDDISTR                          
035900            MOVE WS-PRMATRL         TO UT-PRMATRL                         
036000            PERFORM S01-SKRIV-W27105                                      
036100          END-IF                                                          
036200        END-IF                                                            
036300     END-IF                                                               
036400                                                                          
036500     IF  DIST35-REFILL-NP                                                 
036510     OR  DIST35-REFILL-NX                                                 
036520     OR  DIST35-REFILL-NS                                                 
036600        IF  (WS-SUPERWEEK > 10)                                           
036700          IF (CLAG-PRARTSTD > 10)                                         
036800          OR (WS-TOTALSUM > 300)                                          
036900            MOVE SLAG-IDPERSON-BUY TO UT-IDPERSON                         
037000            MOVE W-IDARTNR         TO UT-IDARTNR                          
037100            MOVE WS-TOTALSUM       TO UT-TOTALSUM                         
037200            MOVE CLAG-PRARTSTD     TO UT-PRARTSTD                         
037300            MOVE WS-AVAILABLE      TO UT-AVAILABLE                        
037400            MOVE WS-IDDC           TO UT-IDDC                             
037500            MOVE SLAG-KVPB-REF     TO UT-KVPB-REF                         
037600            MOVE RAD-IDDISTR       TO UT-IDDISTR                          
037700            MOVE WS-PRMATRL         TO UT-PRMATRL                         
037800            PERFORM S01-SKRIV-W27105                                      
037900          END-IF                                                          
038000        END-IF                                                            
038100     END-IF                                                               
038200                                                                          
038300     IF DIST35-REFILL-NA                                                  
038400        IF  (WS-SUPERWEEK > 10)                                           
038500          IF (CLAG-PRARTSTD > 10)                                         
038600          OR (WS-TOTALSUM > 300)                                          
038700            MOVE SLAG-IDPERSON-BUY  TO UT-IDPERSON                        
038800            MOVE W-IDARTNR          TO UT-IDARTNR                         
038900            MOVE WS-TOTALSUM        TO UT-TOTALSUM                        
039000            MOVE CLAG-PRARTSTD      TO UT-PRARTSTD                        
039100            MOVE WS-AVAILABLE       TO UT-AVAILABLE                       
039200            MOVE WS-IDDC            TO UT-IDDC                            
039300            MOVE SLAG-KVPB-REF      TO UT-KVPB-REF                        
039400            MOVE RAD-IDDISTR       TO UT-IDDISTR                          
039500            MOVE WS-PRMATRL         TO UT-PRMATRL                         
039600            PERFORM S01-SKRIV-W27105                                      
039700          END-IF                                                          
039800        END-IF                                                            
039900     END-IF                                                               
040000                                                                          
040200     IF DIST35-REFILL-CN                                                  
040300        IF  (WS-SUPERWEEK > 10)                                           
040400          IF (LART-PRMATRL > 20)                                          
040500          OR (WS-TOTALSUM > 600)                                          
040600            MOVE SLAG-IDPERSON-BUY   TO UT-IDPERSON                       
040700            MOVE W-IDARTNR          TO UT-IDARTNR                         
040800            MOVE WS-TOTALSUM        TO UT-TOTALSUM                        
040900            MOVE CLAG-PRARTSTD      TO UT-PRARTSTD                        
041000            MOVE WS-AVAILABLE       TO UT-AVAILABLE                       
041100            MOVE WS-IDDC            TO UT-IDDC                            
041200            MOVE SLAG-KVPB-REF      TO UT-KVPB-REF                        
041300            MOVE RAD-IDDISTR        TO UT-IDDISTR                         
041400            MOVE WS-PRMATRL         TO UT-PRMATRL                         
041500            PERFORM S01-SKRIV-W27105                                      
041600          END-IF                                                          
041700        END-IF                                                            
041800     END-IF                                                               
041900                                                                          
042000     .                                                                    
042100     EJECT                                                                
042200 BA-RAKNA-SUPERWEEK SECTION.                                              
042300                                                                          
042400      PERFORM C-KOLLA-DC                                                  
042500***LÄSER MOTTAGANDE DC                                                    
042600      PERFORM IMS-GU-WDK711                                               
042700      IF SEGMENT-FINNS                                                    
042800        MOVE SLAG-IDDC-REF TO WS-REF-FROM-DC                              
042900                              FROM-WS-IDDC                                
043000        COMPUTE WS-KVPB-REF-WEEK =                                        
043100                (SLAG-KVPB-REF + SLAG-KVPBREOI) / 4.33                    
043200                                                                          
043300        IF WS-KVPB-REF-WEEK = ZERO                                        
043400           MOVE +1 TO WS-KVPB-REF-WEEK                                    
043500        END-IF                                                            
043600                                                                          
043700        COMPUTE WS-SUPERWEEK =                                            
043800         (((SLAG-KVLS + SLAG-KVBEART +                                    
043900          SLAG-KVAKS-PAV + SLAG-KVAKS-SDC)                                
044000           -                                                              
044100         (SLAG-KVOKS-DAG + SLAG-KVOKS-BULK +                              
044200          SLAG-KVROS-DAG + SLAG-KVROS-BULK))                              
044300             /                                                            
044400             WS-KVPB-REF-WEEK)                                            
044500      END-IF                                                              
044600                                                                          
044700      .                                                                   
044800    EJECT                                                                 
044900                                                                          
045000 BB-RAKNA-VARDE SECTION.                                                  
045100                                                                          
045200      PERFORM BBA-RAKNA-AVAILABLE                                         
045400      IF DIST35-REFILL-CN                                                 
045500      OR DIST35-REFILL-INOM-CN                                            
045600      OR DIST35-REFILL-INOM-NA                                            
045700        IF DIST35-REFILL-INOM-NA                                          
045800          MOVE 'US'   TO W-IDLAND                                         
045900        END-IF                                                            
046100        IF DIST35-REFILL-CN                                               
046200        OR DIST35-REFILL-INOM-CN                                          
046300          MOVE 'CN'   TO W-IDLAND                                         
046400        END-IF                                                            
046500        PERFORM IMS-GU-WDK712                                             
046600        MOVE LART-PRMATRL    TO WS-PRMATRL                                
046700        COMPUTE WS-TOTALSUM = RAD-KVBEART-Q * LART-PRMATRL                
046800      ELSE                                                                
046900        PERFORM IMS-GU-WDK611                                             
047000        COMPUTE WS-TOTALSUM = RAD-KVBEART-Q * CLAG-PRARTSTD               
047100        MOVE  ZERO TO WS-PRMATRL                                          
047200      END-IF                                                              
047300      .                                                                   
047400      EJECT                                                               
047500                                                                          
047600 BBA-RAKNA-AVAILABLE SECTION.                                             
047700                                                                          
047800     COMPUTE WS-AVAILABLE ROUNDED = SLAG-KVLS -                           
047900                 SLAG-KVROS-BULK - SLAG-KVROS-DAG -                       
048000                 SLAG-KVOKS-BULK - SLAG-KVOKS-DAG                         
048100     .                                                                    
048200     EJECT                                                                
048300                                                                          
048400 C-KOLLA-DC SECTION.                                                      
048500                                                                          
048600**TAR FRAM MOTTAGANDE DC                                                  
048700     MOVE RAD-IDDISTR TO DIST35-IDDISTR                                   
048800     IF (CDC     AND  DIST35-REFILL)                                      
048900     OR (NDC-CN  AND  DIST35-REFILL-INOM-CN)                              
049000     OR (NDC-NA  AND  DIST35-REFILL-INOM-NA)                              
049100       SEARCH ALL DIST57-REFILL-DC                                        
049200         AT END                                                           
049300           MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                           
049400                             TO FELTEXT                                   
049500           CALL FELLOG                                                    
049600         WHEN DIST57-SOK-IDDISTR(DIST57-IX) = RAD-IDDISTR                 
049700           MOVE DIST57-REFILL-TO-DC(DIST57-IX)                            
049800                             TO W-IDDC                                    
049900       END-SEARCH                                                         
050000     END-IF                                                               
050100     .                                                                    
050200     EJECT                                                                
050300                                                                          
050400 Z-FINIT SECTION.                                                         
050500                                                                          
050600     CLOSE W27105                                                         
050700                                                                          
050800     MOVE 'S' TO POSTSUM-OPKOD                                            
050900     CALL POSTSUM USING POSTSUM-PARM                                      
051000     .                                                                    
051100     EJECT                                                                
051200 S01-SKRIV-W27105 SECTION.                                                
051300                                                                          
051400     WRITE UT-POST FROM UT-AREA                                           
051500                                                                          
051600     MOVE 'W27105'   TO POSTSUM-FDNAMN                                    
051700     MOVE 'W27105D1' TO POSTSUM-DDNAMN2                                   
051800     CALL POSTSUM USING POSTSUM-PARM                                      
051900     .                                                                    
052000     EJECT                                                                
052100* --- IMS SEKTIONER ---                                                   
052200     SKIP3                                                                
052300 IMS-GET-WDA5   SECTION.                                                  
052400                                                                          
052500     MOVE 'IMS-GET-WDA5  '  TO IMS-SECTION                                
052600     CALL CBLTDLI USING GN WDA5-PCB DLI-IO-AREA-WDA501                    
052700     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
052800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
052900     PERFORM IMS-STATUSKONTROLL                                           
053000     .                                                                    
053100     SKIP3                                                                
053200     EJECT                                                                
053300 IMS-GU-WDK611 SECTION.                                                   
053400                                                                          
053500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
053600          DELIMITED BY SIZE INTO SSA1                                     
053700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
053800          DELIMITED BY SIZE INTO SSA2                                     
053900     MOVE '  GE' TO GODK-STATUSKODER                                      
054000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
054100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
054200     PERFORM IMS-STATUSKONTROLL                                           
054300     .                                                                    
054400     EJECT                                                                
054500 IMS-GU-WDK711 SECTION.                                                   
054600                                                                          
054700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
054800          DELIMITED BY SIZE INTO SSA1                                     
054900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
055000          DELIMITED BY SIZE INTO SSA2                                     
055100     MOVE '  GE' TO GODK-STATUSKODER                                      
055200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
055300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
055400     PERFORM IMS-STATUSKONTROLL                                           
055500     .                                                                    
055600     EJECT                                                                
055700 IMS-GU-WDK712      SECTION.                                              
055800                                                                          
055900     MOVE 'IMS-GU-WDK712'   TO IMS-SECTION                                
056000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
056100          DELIMITED BY SIZE  INTO SSA1                                    
056200     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
056300          DELIMITED BY SIZE  INTO SSA2                                    
056400     MOVE '    '               TO GODK-STATUSKODER                        
056500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
056600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
056700     PERFORM IMS-STATUSKONTROLL                                           
056800     .                                                                    
056900     EJECT                                                                
057000 IMS-STATUSKONTROLL SECTION.                                              
057100                                                                          
057200     SET STATUS-IX TO 1                                                   
057300     SEARCH GODK-STATUS                                                   
057400       AT END                                                             
057500     DISPLAY '*****-**** IMS-SECTION ' IMS-SECTION                        
057600         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
057700           DELIMITED BY SIZE INTO FELTEXT-STR                             
057800         DISPLAY FELTEXT                                                  
057900         CALL FELLOG                                                      
058000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
058100         CONTINUE                                                         
058200     END-SEARCH                                                           
058300     .                                                                    
