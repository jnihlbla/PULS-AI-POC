000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4403200.                                        
000400 AUTHOR.                 BO SVENSSON.                                     
000500 DATE-WRITTEN.           JAN  1997.                                       
000600                                                                          
000700     REMARKS.                                                             
000800******************************************************************        
000900*                                                                         
001000*    RESTORDERTÄCKNING FÖR NDC. BMP.                                      
001100*                                                                         
001200*    ÄNDRINGS-LOGG:                                                       
001300*        KOPIERAT  FRÅN W44032 I SAMBAND MED NDC.                         
001400*        RENSAT PÅ KOD FÖR ATT UTFÖRA RAK TÄCKNING PÅ RESP. NDC.          
001500*                                                                         
001600******************************************************************        
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        TÄCKNING AV RESTORDER NDC.                                       
002000*                                                                         
002100*        INDATA. WDA5 WDK6 WDK7                                           
002200*                WDR4 (TÄCKN.TRANS)                                       
002300*                                                                         
002400*        UTDATA. WDG6                                                     
002500*                                                                         
002600*        UPPDATERA WDA5 WDK7 WDG6 WDR4                                    
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                   PIC X(8)   VALUE 'W4403200'.                 
004400 77  FELTEXT                 PIC X(80)  VALUE SPACE.                      
004500     SKIP2                                                                
004600*    ---- ARBETSVARIABLER                                                 
004700*                                                                         
004800 77  CHKP-ID                 PIC X(08)  VALUE 'W4403200'.                 
004900 77  CHKP-ANT                PIC S9(3)  COMP-3 VALUE +0.                  
005000 77  CHKP-MAX                PIC S9(5)  COMP-3 VALUE 900.                 
005100 77  MSG-IO-AREA-LENGTH-1    PIC S9(9)  VALUE +32 COMP SYNC.              
005200 77  MSG-IO-AREA-1           PIC X(32)  VALUE SPACE.                      
005300 77  CHKP-AREA-1-LENGTH      PIC S9(9)  VALUE +32 COMP SYNC.              
005400 77  CHKP-AREA-1             PIC X(32)  VALUE SPACE.                      
005500                                                                          
005600 77  RFS-IX                  PIC S9(4)  VALUE ZERO COMP SYNC.             
005700 77  MAX-RFS-IX              PIC S9(4)  VALUE +4   COMP SYNC.             
005800                                                                          
005900 77  W-DISPONIBELT           PIC S9(07) COMP-3 VALUE ZERO.                
006000 77  W-ANTAL-ARTIKLAR        PIC S9(07) COMP-3 VALUE ZERO.                
006100 77  W-KVART                 PIC S9(07) COMP-3 VALUE ZERO.                
006200 77  W-TIAAMMDD              PIC 9(06).                                   
006300 77  W-TIDDD                 PIC 9(03).                                   
006400 77  W-TIAA                  PIC 9(02).                                   
006500 77  W-IDLAND                PIC X(03).                                   
006600 77  SW-TACKBART             PIC X              VALUE SPACE.              
006700 77  WS-RELEASE-WIP          PIC X              VALUE 'N'.                
006800 77  SW-TAECKTRANS-FINNS     PIC X              VALUE 'N'.                
006900     88 TAECKTRANS-FINNS                        VALUE 'J'.                
007000     88 TAECKTRANS-SAKNAS                       VALUE 'N'.                
007100                                                                          
007200                                                                          
007300 77  W-IDKUNDRF-WIP          PIC X(10)          VALUE SPACE.              
007400 77  WS-TIRFS                PIC 9(6).                                    
007500                                                                          
007600 01  WS-DAT                         PIC 9(6).                             
007700 01  FILLER REDEFINES WS-DAT.                                             
007800     03 WS-YEAR                     PIC 9(2).                             
007900     03 WS-MONTH                    PIC 9(2).                             
008000     03 WS-DAYS                     PIC 9(2).                             
008100                                                                          
008200 77  WS-IDKONTO-DISPLAY      PIC 9(10)  VALUE ZERO.                       
008300                                                                          
008400                                                                          
008500 01  WS-IDORDNR-NUM                          PIC 9(7).                    
008600 01  WS-IDORDNR REDEFINES WS-IDORDNR-NUM     PIC X(7).                    
008700 01  WS-IDDISTR-NUM                          PIC 9(4).                    
008800 01  WS-IDDISTR REDEFINES WS-IDDISTR-NUM     PIC X(4).                    
008900 01  WS-IDKUNDNR-NUM                         PIC 9(6).                    
009000 01  WS-IDKUNDNR REDEFINES WS-IDKUNDNR-NUM   PIC X(6).                    
009100                                                                          
009200*                                                                         
009300 01  W-SALDO-WDK7.                                                        
009400     03  W-FLSPBULK          PIC X(1)  VALUE SPACE.                       
009500     03  W-KVLS              PIC S9(7) COMP-3.                            
009600     03  W-KVOKS-DAG         PIC S9(7) COMP-3.                            
009700     03  W-KVRESS            PIC S9(7) COMP-3.                            
009800     03  W-KVROS-DAG         PIC S9(7) COMP-3.                            
009900     03  W-KVROS-BULK        PIC S9(7) COMP-3.                            
010000     03  W-KVSPARR-KVAL      PIC S9(7) COMP-3.                            
010100     03  W-KVUTRS            PIC S9(7) COMP-3.                            
010200     03  W-KVSPANT           PIC S9(7) COMP-3.                            
010300*                                                                         
010400 01  W-SUM-SALDO-WDK7.                                                    
010500     03  W-SUM-KVROS-DAG     PIC S9(9) COMP-3 VALUE ZERO.                 
010600     03  W-SUM-KVROS-BULK    PIC S9(9) COMP-3 VALUE ZERO.                 
010700     03  W-SUM-KVRESS        PIC S9(9) COMP-3 VALUE ZERO.                 
010800     EJECT                                                                
010900                                                                          
011000*IDORDNR TABLE CREATED FROM W411ODNR                                      
011100 01  W-IDORDNR-TABELL.                                                    
011200     02  FILLER OCCURS 200.                                               
011300       03  TAB-IDORDNR        PIC 9(7)  VALUE ZERO.                       
011400*------------------------------- ARB.FÄLT FÖR TÄCKNING                    
011500 01      TACKW.                                                           
011600*                                                                         
011700  03     TACK-KVDISP         PIC S9(7)               COMP-3.              
011800     EJECT                                                                
011900*    ---- KONSTANTER                                                      
012000                                                                          
012100 77  JA                      PIC X       VALUE 'J'.                       
012200 77  NEJ                     PIC X       VALUE 'N'.                       
012300 77  ORD-IX                  PIC 999     VALUE ZERO.                      
012400 77  MAX-ORD-IX              PIC 999     VALUE 200.                       
012500 77  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16 COMP SYNC.             
012600     EJECT                                                                
012700*      --- VALID IDDC CODES                                               
012800*                                                                         
012900*01    -COPY WWDC99                                                       
013000*01    -COPY WWDCLAND                                                     
013100       EJECT                                                              
013200*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
013300     SKIP3                                                                
013400 01  DYNAMISKA-SUBPROGRAM.                                                
013500   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
013600   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
013700   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
013800   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
013900   03  VIMSREGT              PIC X(8)    VALUE 'VIMSREGT'.                
014000   03  W411ORDN              PIC X(8)    VALUE 'W411ORDN'.                
014100   03  W006KOM               PIC X(8)    VALUE 'W006KOM '.                
014200   03  WORKDAY               PIC X(8)    VALUE 'WORKDAY '.                
014300   03  W440EMOH              PIC X(8)    VALUE 'W440EMOH'.                
014400     EJECT                                                                
014500*    ----  PARAMETRAR TILL DATUMKORT                                      
014600                                                                          
014700 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
014800     SKIP3                                                                
014900*    -COPY WDATAREA                                                       
015000     EJECT                                                                
015100*    ----  PARAMETRAR TILL VIMSREGTIME FÖR KOLL OM BMP EL BATCH           
015200                                                                          
015300 01  FILLER.                                                              
015400     03  IMS-VIMSREGT        PIC S9(9)    COMP SYNC.                      
015500         88  BMP                          VALUE +8.                       
015600         88  BATCH                        VALUE +16 THRU +64.             
015700     EJECT                                                                
015800*    --- PARAMETRAR TILL SUBPROGRAM W440EMOH                              
015900*01 -COPY W440EMOH                                                        
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16)   VALUE 'W411ORDN'.            
016200*    --- PARAMETRAR TILL SUBPROGRAM W411ORDN                              
016300*01 -COPY W411ORDN                                                        
016400                                                                          
016500*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
016600*01 -COPY WORKAREA                                                        
016700                                                                          
016800     EJECT                                                                
016900*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
017000                                                                          
017100 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
017200     SKIP3                                                                
017300*    ---- STATUSKOD FRÅN IMS                                              
017400                                                                          
017500 01  STATUS-WS               PIC XX.                                      
017600     88  SEGMENT-SLUT                     VALUE 'GB'.                     
017700     88  SEGMENT-FINNS                    VALUE '  '.                     
017800     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
017900     88  IMS-EJ-OK                        VALUE 'XD'.                     
018000     SKIP3                                                                
018100 01  GODK-STATUSKODER.                                                    
018200   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
018300     SKIP3                                                                
018400 01  SSA1                    PIC X(230).                                  
018500 01  SSA2                    PIC X(230).                                  
018600 01  SSA3                    PIC X(230).                                  
018700     EJECT                                                                
018800*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
018900                                                                          
019000 01  FILLER                  PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.          
019100 01  NYCKLAR-TILL-DLI.                                                    
019200                                                                          
019300   03  W-WDGX-4505-KEY-X.                                                 
019400     05  W-IDHTYP-4505       PIC X(04) VALUE '4505'.                      
019500     05  W-IDDC-4505         PIC X(02) VALUE SPACE.                       
019600     05  FILLER              PIC X(24) VALUE LOW-VALUE.                   
019700                                                                          
019800   03  W1-WDA5ASEQ-X.                                                     
019900     05  W1-IDARTNR          PIC S9(09) COMP-3 VALUE ZERO.                
020000     05  W1-IDDC             PIC X(02).                                   
020100     05  W1-KDRAPRIO         PIC S9(03) COMP-3 VALUE ZERO.                
020200                                                                          
020300   03  W2-WDA5ASEQ-X.                                                     
020400     05  W2-IDARTNR          PIC S9(09) COMP-3 VALUE ZERO.                
020500     05  W2-IDDC             PIC X(02).                                   
020600     05  W2-KDRAPRIO         PIC S9(03) COMP-3 VALUE ZERO.                
020700                                                                          
020800   03  W-KDSTARAD-X.                                                      
020900     05  W-KDSTARAD          PIC X        VALUE SPACE.                    
021000                                                                          
021100   03  W-IDARTNR-K6-X.                                                    
021200     05  W-IDARTNR-K6        PIC S9(9)    COMP-3 VALUE ZERO.              
021300                                                                          
021400   03  W-IDARTNR-K7-X.                                                    
021500     05  W-IDARTNR-K7        PIC S9(9)    COMP-3 VALUE ZERO.              
021600                                                                          
021700   03  W-IDDC-K7-X.                                                       
021800     05  W-IDDC-K7           PIC X(2)     VALUE SPACE.                    
021900                                                                          
022000   03  W-IDGMT-X.                                                         
022100     05 W-IDDISTR-WDB2       PIC S9(5) VALUE ZERO COMP-3.                 
022200     05 W-IDKUNDNR-WDB2      PIC S9(7) VALUE ZERO COMP-3.                 
022300                                                                          
022400     EJECT                                                                
022500*01  -COPY W0003                                                          
022600     EJECT                                                                
022700 01  FILLER                  PIC X(16) VALUE 'RY4-POSTER VR'.             
022800     SKIP2                                                                
022900*01  AREA -COPY W440300     -PRE  RY4-                                    
023000     EJECT                                                                
023100* ---         DLI INOUT OUTPUT AREA                                       
023200* ---         DLI-IO-AREA                                                 
023300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ZZAC01'.           
023400 01  DLI-IO-ZZAC01.                                                       
023500*  03  WLZZAC01 -COPY WDGZ01  -PRE  LOGG-                                 
023600     EJECT                                                                
023700                                                                          
023800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ARTC01'.           
023900 01  DLI-IO-ARTC01.                                                       
024000*  03  WLARTC01 -COPY WDK601                                              
024100     EJECT                                                                
024200                                                                          
024300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ARTC11'.           
024400 01  DLI-IO-ARTC11.                                                       
024500*  03  WLARTC11 -COPY WDK611                                              
024600     EJECT                                                                
024700                                                                          
024800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK711'.           
024900 01  DLI-IO-WDK711.                                                       
025000*  03  WDK711 -COPY WDK711                                                
025100                                                                          
025200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK722'.           
025300 01  DLI-IO-WDK722.                                                       
025400*  03  WDK722 -COPY WDK722                                                
025500     EJECT                                                                
025600                                                                          
025700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-GX4505'.           
025800 01  DLI-IO-GX4505.                                                       
025900   03  4505-IDHTYP             PIC X(4).                                  
026000   03  4505-IDDC               PIC X(2).                                  
026100   03  FILLER                  PIC X(24).                                 
026200     EJECT                                                                
026300                                                                          
026400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-GX4506'.           
026500 01  DLI-IO-GX4506.                                                       
026600*  03  WDGX4506 -COPY WDGX4506                                            
026700     EJECT                                                                
026800                                                                          
026900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ORDP01'.           
027000 01  DLI-IO-ORDP01.                                                       
027100*  03  WLORDP01 -COPY WDA501                                              
027200                                                                          
027300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB2  '.           
027400 01  DLI-IO-WDB2.                                                         
027500*  03  WDB2  -COPY WDB201                                                 
027600                                                                          
027700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB6  '.           
027800 01  DLI-IO-WDB6.                                                         
027900*  03  WDB2  -COPY WDB601                                                 
028000                                                                          
028100                                                                          
028200*    ---  MSG INPUT-OUTPUT AREA                                           
028300*01  -COPY WMSGAREA                                                       
028400                                                                          
028500                                                                          
028600 01  FILLER                 PIC X(16)   VALUE 'KOM-OHUV-AREA'.            
028700 01  OHUV-AREA.                                                           
028800*    03   -COPY W4I25101   -PRE OHUV-                                     
028900     EJECT                                                                
029000 01  FILLER                 PIC X(16)   VALUE 'KOM-RAD-AREA'.             
029100 01  ORAD-AREA.                                                           
029200*    05   -COPY W4I25201   -PRE ORAD-                                     
029300     EJECT                                                                
029400*    --- AREOR FÖR W006KOM SUBMODUL                                       
029500 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
029600*01  -COPY WMSGKOM                                                        
029700     EJECT                                                                
029800 LINKAGE SECTION.                                                         
029900     SKIP2                                                                
030000*01  -COPY W0009      -PRE  MSG-                                          
030100     EJECT                                                                
030200 01  0693-PCB                     PIC X.                                  
030300 01  WDP8-PCB                     PIC X.                                  
030400*01  -COPY W0008      -PRE  ARTC-                                         
030500       05  FILLER                PIC X.                                   
030600     EJECT                                                                
030700*01  -COPY W0008      -PRE  WDK7-                                         
030800       05  FILLER                PIC X.                                   
030900     EJECT                                                                
031000*01  -COPY W0008      -PRE  ORDP-                                         
031100       05  FILLER                PIC X.                                   
031200     EJECT                                                                
031300*01  -COPY W0008      -PRE  ORDP2-                                        
031400       05  FILLER                PIC X.                                   
031500     EJECT                                                                
031600*01  -COPY W0008      -PRE  ZZAC-                                         
031700       05  FILLER                PIC X.                                   
031800     EJECT                                                                
031900*01  -COPY W0008      -PRE  4505-                                         
032000       05  FILLER                PIC X.                                   
032100*01  -COPY W0008      -PRE  WDB2-                                         
032200       05  FILLER                PIC X.                                   
032300*01  -COPY W0008      -PRE  WDB6-                                         
032400       05  FILLER                PIC X.                                   
032500     EJECT                                                                
032600 01  ORDN-XXKP-PCB               PIC X.                                   
032700 01  ORDN-ORQL-PCB               PIC X.                                   
032800 01  ORDN-PROC-PCB               PIC X.                                   
032900 01  ORDN-ORQI-PCB               PIC X.                                   
033000 01  EMOH-WDQ2-PCB               PIC X.                                   
033100     SKIP3                                                                
033200                                                                          
033300                                                                          
033400 PROCEDURE DIVISION  USING MSG-PCB   0693-PCB  WDP8-PCB                   
033500                           ARTC-PCB  WDK7-PCB                             
033600                           ORDP-PCB  ORDP2-PCB ZZAC-PCB                   
033700                           4505-PCB  WDB2-PCB  WDB6-PCB                   
033800                           ORDN-XXKP-PCB                                  
033900                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
034000                           ORDN-ORQI-PCB EMOH-WDQ2-PCB.                   
034100                                                                          
034200 STYRDEL SECTION.                                                         
034300                                                                          
034400     ENTRY 'DLITCBL' USING MSG-PCB   0693-PCB  WDP8-PCB                   
034500                           ARTC-PCB  WDK7-PCB                             
034600                           ORDP-PCB  ORDP2-PCB ZZAC-PCB                   
034700                           4505-PCB  WDB2-PCB  WDB6-PCB                   
034800                           ORDN-XXKP-PCB                                  
034900                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
035000                           ORDN-ORQI-PCB EMOH-WDQ2-PCB.                   
035100     PERFORM A-INIT                                                       
035200                                                                          
035300     PERFORM B-GET-TAECKTRANS                                             
035400                                                                          
035500     PERFORM UNTIL TAECKTRANS-SAKNAS                                      
035600       MOVE 4505-IDDC   TO W1-IDDC                                        
035700                           W2-IDDC                                        
035800                                                                          
035900       PERFORM C-DISPONIBELT-SALDO                                        
036000                                                                          
036100                                                                          
036240       IF TACK-KVDISP > ZERO AND                                          
036300          (W-KVROS-DAG   > ZERO OR                                        
036400           W-KVROS-BULK  > ZERO)                                          
036500         MOVE  TACK-KVDISP      TO W-DISPONIBELT                          
036600                                                                          
036700         PERFORM S08-ORDP-STPOS-WDA5ASEQ                                  
036800         PERFORM G-FULL-TAECKNING                                         
036900                                                                          
037000       ELSE                                                               
037100         CONTINUE                                                         
037200       END-IF                                                             
037300                                                                          
037400       PERFORM H-UPPD-SALDO-WDK7                                          
037500                                                                          
037600       IF BMP AND CHKP-ANT     >  CHKP-MAX                                
037700          PERFORM IMS-DELETE-450511-TAECKTRANS                            
037800                                                                          
037900          PERFORM J-TAG-CHECKPOINT                                        
038000                                                                          
038100          PERFORM B-GET-TAECKTRANS                                        
038200       ELSE                                                               
038300          PERFORM IMS-DELETE-450511-TAECKTRANS                            
038400          PERFORM IMS-GHNP-450511-TAECKTRANS                              
038500                                                                          
038600          IF SEGMENT-SAKNAS                                               
038700             PERFORM B-GET-TAECKTRANS                                     
038800          END-IF                                                          
038900       END-IF                                                             
039000                                                                          
039100     END-PERFORM                                                          
039200                                                                          
039300     MOVE ZERO TO RETURN-CODE                                             
039400     GOBACK                                                               
039500     .                                                                    
039600     EJECT                                                                
039700 A-INIT SECTION.                                                          
039800     SKIP2                                                                
039900     CALL VIMSREGT                                                        
040000     MOVE RETURN-CODE  TO IMS-VIMSREGT                                    
040100                                                                          
040200     IF BMP                                                               
040300        PERFORM IMS-RESTART                                               
040400     END-IF                                                               
040500     MOVE ZERO         TO CHKP-ANT                                        
040600                                                                          
040700     MOVE 'IDAG  '     TO DAT-KDDATFORM                                   
040800     CALL WDATKONV USING  DAT-KDDATFORM                                   
040900                          DAT-I-TIDATUM                                   
041000                          DAT-O-TIDATUM                                   
041100                          DAT-KDSVAR                                      
041200     MOVE DAT-TIAAMMDD TO W-TIAAMMDD                                      
041300                          WS-DAT                                          
041400     MOVE DAT-TIDDD    TO W-TIDDD                                         
041500     MOVE DAT-TIAA     TO W-TIAA                                          
041600                                                                          
041700     .                                                                    
041800     EJECT                                                                
041900 B-GET-TAECKTRANS SECTION.                                                
042000                                                                          
042100     MOVE NEJ TO SW-TAECKTRANS-FINNS                                      
042200     PERFORM IMS-GN-WDB601                                                
042300     PERFORM UNTIL SEGMENT-SLUT                                           
042400                OR TAECKTRANS-FINNS                                       
042500                                                                          
042600        MOVE DCS-IDDC TO WS-IDDC                                          
042700        IF NDC-NA OR NDC-PACIFIC OR NDC-CN OR NDC-NX                      
042800                  OR NDC-NS                                               
042910           MOVE DCS-IDDC TO W-IDDC-4505                                   
043000           PERFORM IMS-GU-450501                                          
043100           PERFORM IMS-GHNP-450511-TAECKTRANS                             
043200           IF SEGMENT-FINNS                                               
043310              MOVE JA  TO SW-TAECKTRANS-FINNS                             
043400           ELSE                                                           
043500              MOVE NEJ TO SW-TAECKTRANS-FINNS                             
043600              PERFORM IMS-GN-WDB601                                       
043700           END-IF                                                         
043800        ELSE                                                              
043900           PERFORM IMS-GN-WDB601                                          
044000        END-IF                                                            
044100     END-PERFORM                                                          
044200                                                                          
044300     .                                                                    
044400     EJECT                                                                
044500 C-DISPONIBELT-SALDO SECTION.                                             
044600                                                                          
044700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
044800*                                                                         
044900*    DISPONIBELT RÄKNAS UT.                                               
045000*                                                                         
045100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
045200                                                                          
045300     MOVE NEJ  TO SW-TACKBART                                             
045400                  W-FLSPBULK                                              
045500     MOVE +0   TO W-DISPONIBELT                                           
045600                  W-KVLS                                                  
045700                  W-KVRESS                                                
045800                  W-KVROS-DAG                                             
045900                  W-KVROS-BULK                                            
046000                  W-KVSPARR-KVAL                                          
046100                  W-KVSPANT                                               
046200                  W-KVUTRS                                                
046300                  W-KVOKS-DAG                                             
046400                  W-SUM-KVROS-DAG                                         
046500                  W-SUM-KVROS-BULK                                        
046600                  W-SUM-KVRESS                                            
046700                                                                          
046800     MOVE +0           TO TACK-KVDISP                                     
046900     MOVE 4506-IDARTNR TO W-IDARTNR-K6                                    
047000                                                                          
047100     PERFORM IMS-GU-ARTC01                                                
047200     IF SEGMENT-FINNS                                                     
047300       PERFORM IMS-GNP-ARTC11                                             
047400                                                                          
047500       IF SEGMENT-FINNS                                                   
047600                                                                          
047700         IF CLAG-PRARTSTD NOT = +0                                        
047800         AND NOT (CLAG-KDERS = 22                                         
047900              OR  CLAG-KDERS = 23)                                        
048000            PERFORM CA-WDK7-SALDO                                         
048100            IF SW-TACKBART = JA                                           
048200              COMPUTE TACK-KVDISP = W-KVLS                                
048300                                  - W-KVOKS-DAG                           
048400                                  - W-KVSPARR-KVAL                        
048500                                  - W-KVUTRS                              
048600                                  - W-KVRESS                              
048700            END-IF                                                        
048800         END-IF                                                           
048900       END-IF                                                             
049000     END-IF                                                               
049100     .                                                                    
049200     EJECT                                                                
049300 CA-WDK7-SALDO SECTION.                                                   
049400                                                                          
049500     SEARCH ALL DC-LAND                                                   
049600        AT END                                                            
049700           MOVE SPACE          TO W-IDLAND                                
049800        WHEN DCLAND-IDDC (DCLAND-IX) = 4505-IDDC                          
049900           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
050000                               TO W-IDLAND                                
050100     END-SEARCH                                                           
050200                                                                          
050300     MOVE 4506-IDARTNR TO W-IDARTNR-K7                                    
050400     MOVE 4505-IDDC    TO W-IDDC-K7                                       
050500                                                                          
050600     PERFORM IMS-GU-WDK711                                                
050700                                                                          
050800     IF  SEGMENT-FINNS                                                    
050900     AND SLAG-KDLEVSP = +0                                                
051000     AND SLAG-FLORDSP = NEJ                                               
051100       MOVE SLAG-KVLS          TO W-KVLS                                  
051200       MOVE SLAG-KVRESS        TO W-KVRESS                                
051300       IF SLAG-KVRESS < ZERO                                              
051400         MOVE ZERO             TO W-KVRESS                                
051500       END-IF                                                             
051600       MOVE SLAG-KVROS-DAG     TO W-KVROS-DAG                             
051700       MOVE SLAG-KVROS-BULK    TO W-KVROS-BULK                            
051800       MOVE SLAG-KVSPARR-KVAL  TO W-KVSPARR-KVAL                          
051900       MOVE SLAG-KVUTRS        TO W-KVUTRS                                
052000       MOVE SLAG-KVOKS-DAG     TO W-KVOKS-DAG                             
052100       MOVE SLAG-FLSPBULK      TO W-FLSPBULK                              
052200       MOVE ZERO               TO W-KVSPANT                               
052300       IF DCLAND-CHINA (DCLAND-IX)                                        
052400          PERFORM IMS-GU-WDK722                                           
052500          IF SEGMENT-FINNS                                                
052600             MOVE XLAG-KVSPANT TO W-KVSPANT                               
052700          ELSE                                                            
052800             MOVE ZERO         TO W-KVSPANT                               
052900          END-IF                                                          
053000       END-IF                                                             
053100       MOVE JA                 TO SW-TACKBART                             
053200     END-IF                                                               
053300                                                                          
053400                                                                          
053500     .                                                                    
053600     EJECT                                                                
053700 G-FULL-TAECKNING SECTION.                                                
053800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
053900*    LÄSER ALLA OTÄCKTA RO-RADER I PRIORITETSORDNING.           *         
054000*    BEHANDLA DE RADER SOM KAN TÄCKAS FRÅN HTR'S CLAGER.        *         
054100*                                                               *         
054200*    GER SÅ LÄNGE DISPONIBELT RÄCKER.                           *         
054300*                                                               *         
054400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
054500                                                                          
054600     MOVE LOW-VALUE    TO W1-WDA5ASEQ-X                                   
054700     MOVE HIGH-VALUE   TO W2-WDA5ASEQ-X                                   
054810     MOVE 4506-IDARTNR TO W1-IDARTNR                                      
054900                          W2-IDARTNR                                      
055000     MOVE 4505-IDDC    TO W1-IDDC                                         
055100                          W2-IDDC                                         
055200     MOVE '2'          TO W-KDSTARAD                                      
055300     PERFORM IMS-GHN-ORDQ01-ORDP01                                        
055400                                                                          
055500     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
055600                   W-DISPONIBELT = +0                                     
055700                                                                          
055800*FOR WORKSHOP ORDER IF THE ORDERCLASS IS 2 OR 1 THEN WE RELEASEE          
055900*THEM AND IF IT IS CLASS 3 IF TODAY DATE IS GREATER THEN                  
056000*REPDAT THEN RELEASE IT                                                   
056100       IF RAD-TIREPDAT NOT = ZERO                                         
056200          IF WS-DAT     > RAD-TIREPDAT OR                                 
056300           ( RAD-KDORDKL =  1  OR  2 )                                    
056400              MOVE 'Y'   TO WS-RELEASE-WIP                                
056500          END-IF                                                          
056600       END-IF                                                             
056700       IF RAD-TIREPDAT  = ZERO OR WS-RELEASE-WIP = 'Y'                    
056800         IF RAD-KDORDKL > 1                                               
056900         AND W-FLSPBULK = JA                                              
057000             CONTINUE                                                     
057100         ELSE                                                             
057200           IF CLAG-KVQPACK-1 > +1                                         
057300             IF ART-KDSORT = 'KG' OR 'M ' OR 'L ' OR                      
057400                RAD-KDKVBRYT = +0                                         
057500               IF W-DISPONIBELT < RAD-KVART                               
057600                 COMPUTE W-ANTAL-ARTIKLAR ROUNDED =                       
057700                        (W-DISPONIBELT / CLAG-KVQPACK-1) - 0.5            
057800                 COMPUTE W-DISPONIBELT ROUNDED =                          
057900                         W-ANTAL-ARTIKLAR * CLAG-KVQPACK-1                
058000               END-IF                                                     
058100             END-IF                                                       
058200           END-IF                                                         
058300                                                                          
058400           IF W-DISPONIBELT > +0                                          
058500             PERFORM GA-TAECKNING-RAD                                     
058600           END-IF                                                         
058700                                                                          
058800         END-IF                                                           
058900       END-IF                                                             
059000       MOVE '2' TO W-KDSTARAD                                             
059100       PERFORM IMS-GHN-ORDQ01-ORDP01                                      
059200     END-PERFORM                                                          
059300     .                                                                    
059400     EJECT                                                                
059500 GA-TAECKNING-RAD SECTION.                                                
059600                                                                          
059700     IF W-DISPONIBELT NOT < RAD-KVART                                     
059800        MOVE RAD-KVART          TO W-KVART                                
059900        PERFORM S01-SUM-SALDO-WDK7                                        
060000        PERFORM S02-SKAPA-TRANS                                           
060100                                                                          
060200        MOVE '3'               TO RAD-KDSTARAD                            
060300        MOVE W-TIAAMMDD        TO RAD-TIRES                               
060400*CALL FOR W440EMOH                                                        
060500          IF RAD-KDROPACK = '3'                                           
060600            MOVE RAD-IDDISTR   TO EMOH-IDDISTR                            
060700            MOVE RAD-IDKUNDNR  TO EMOH-IDKUNDNR                           
060800            MOVE RAD-IDORDNR5  TO EMOH-IDORDNR7                           
060900            MOVE RAD-KDORDKL   TO EMOH-KDORDKL                            
061000            MOVE RAD-IDDC      TO EMOH-IDDC                               
061100            CALL W440EMOH USING   EMOH-W440EMOH                           
061200                                  EMOH-WDQ2-PCB                           
061300                                                                          
061400            IF EMOH-KDSVAR-CREATE                                         
061500               PERFORM S11-SKAPA-ORDERHUVUD                               
061600               MOVE TAB-IDORDNR(ORD-IX)                                   
061700                               TO RAD-IDARBREF                            
061800            END-IF                                                        
061900            IF EMOH-KDSVAR-EXISTS                                         
062000               MOVE EMOH-IXHALV                                           
062100                               TO ORD-IX                                  
062200               MOVE TAB-IDORDNR(ORD-IX)                                   
062300                               TO RAD-IDARBREF                            
062400            END-IF                                                        
062500          END-IF                                                          
062600          IF WS-RELEASE-WIP = 'Y'                                         
062700              MOVE 'N'             TO WS-RELEASE-WIP                      
062800              PERFORM S11-SKAPA-ORDERHUVUD                                
062900          END-IF                                                          
063000          PERFORM IMS-REPLACE-ORDQ01-ORDP01                               
063100          SUBTRACT RAD-KVART FROM  W-DISPONIBELT                          
063200     ELSE                                                                 
063300        SUBTRACT W-DISPONIBELT FROM RAD-KVART                             
063400        PERFORM IMS-REPLACE-ORDQ01-ORDP01                                 
063500                                                                          
063600        MOVE W-DISPONIBELT      TO W-KVART                                
063700        PERFORM S01-SUM-SALDO-WDK7                                        
063800        PERFORM S02-SKAPA-TRANS                                           
063900        MOVE '3'                TO RAD-KDSTARAD                           
064000        MOVE W-DISPONIBELT      TO RAD-KVART                              
064100        MOVE W-TIAAMMDD         TO RAD-TIRES                              
064200*CALL FOR W440EMOH                                                        
064300        IF RAD-KDROPACK = '3'                                             
064400          MOVE RAD-IDDISTR      TO EMOH-IDDISTR                           
064500          MOVE RAD-IDKUNDNR     TO EMOH-IDKUNDNR                          
064600          MOVE RAD-IDORDNR5     TO EMOH-IDORDNR7                          
064700          MOVE RAD-KDORDKL      TO EMOH-KDORDKL                           
064800          MOVE RAD-IDDC         TO EMOH-IDDC                              
064900          CALL W440EMOH   USING    EMOH-W440EMOH                          
065000                                   EMOH-WDQ2-PCB                          
065100                                                                          
065200                                                                          
065300          IF EMOH-KDSVAR-CREATE                                           
065400            PERFORM S11-SKAPA-ORDERHUVUD                                  
065500            MOVE TAB-IDORDNR(ORD-IX)                                      
065600                                TO RAD-IDARBREF                           
065700          END-IF                                                          
065800          IF EMOH-KDSVAR-EXISTS                                           
065900            MOVE EMOH-IXHALV                                              
066000                                TO ORD-IX                                 
066100            MOVE TAB-IDORDNR(ORD-IX)                                      
066200                                TO RAD-IDARBREF                           
066300          END-IF                                                          
066400        END-IF                                                            
066500                                                                          
066600        IF WS-RELEASE-WIP = 'Y'                                           
066700            MOVE 'N'             TO WS-RELEASE-WIP                        
066800            PERFORM S11-SKAPA-ORDERHUVUD                                  
066900        END-IF                                                            
067000                                                                          
067100        PERFORM S10-INSERT-ORDP01                                         
067200                                                                          
067300        MOVE +0     TO W-DISPONIBELT                                      
067400     END-IF                                                               
067500     .                                                                    
067600     EJECT                                                                
067700 H-UPPD-SALDO-WDK7 SECTION.                                               
067800                                                                          
067900     IF W-SUM-KVROS-DAG   = +0 AND                                        
068000        W-SUM-KVROS-BULK  = +0 AND                                        
068100        W-SUM-KVRESS      = +0                                            
068200         CONTINUE                                                         
068300     ELSE                                                                 
068400        PERFORM IMS-GHU-WDK711                                            
068500        IF SEGMENT-FINNS                                                  
068600                                                                          
068700          COMPUTE SLAG-KVROS-DAG = SLAG-KVROS-DAG                         
068800                                  - W-SUM-KVROS-DAG                       
068900          COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                       
069000                                  - W-SUM-KVROS-BULK                      
069100          COMPUTE SLAG-KVRESS = SLAG-KVRESS + W-SUM-KVRESS                
069200                                                                          
069300          PERFORM IMS-REPLACE-WDK711                                      
069400        END-IF                                                            
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 J-TAG-CHECKPOINT SECTION.                                                
069900                                                                          
070000     PERFORM IMS-CHECKPOINT                                               
070100     MOVE    ZERO  TO CHKP-ANT                                            
070200     .                                                                    
070300     EJECT                                                                
070400 S01-SUM-SALDO-WDK7 SECTION.                                              
070500*****************************************************************         
070600*    SALDO FÖR UPPDATERING WDK7 ACKUMULERAS.                              
070700*                                                                         
070800*      W-KVROS SÄNKS FÖR ATT FÅ KORREKTA                                  
070900*      VÄRDEN INFÖR EVENTUELLT YTTERLIGARE TÄCKNINGS-OMGÅNGAR.            
071000*****************************************************************         
071100                                                                          
071200     IF  RAD-KDORDKL > 1                                                  
071300       ADD    W-KVART TO   W-SUM-KVROS-BULK                               
071400     ELSE                                                                 
071500       ADD    W-KVART TO   W-SUM-KVROS-DAG                                
071600     END-IF                                                               
071700                                                                          
071800     ADD      W-KVART TO   W-SUM-KVRESS                                   
071900     .                                                                    
072000     EJECT                                                                
072100 S02-SKAPA-TRANS SECTION.                                                 
072200                                                                          
072300     ACCEPT LOGG-TIAAMMDD FROM DATE                                       
072400     ACCEPT LOGG-TIKLOCK  FROM TIME                                       
072500     ADD +1                 TO LOGG-IDLOGLOP                              
072600     MOVE 'RY4'             TO RY4-IDPTYP                                 
072700     MOVE RAD-IDDC          TO RY4-IDDC                                   
072800     MOVE RAD-IDDISTR       TO RY4-IDDISTR                                
072900     MOVE RAD-IDKUNDNR      TO RY4-IDKUNDNR                               
073000     MOVE RAD-IDKUNDRF(1:5) TO RY4-IDRONR                                 
073100     MOVE RAD-IDARTNR       TO RY4-IDARTNR                                
073200     MOVE W-KVART           TO RY4-KVRO                                   
073300     MOVE RAD-KDORDKL       TO RY4-KDORDKL                                
073400     MOVE RAD-KDFAKTYP      TO RY4-KDFAKTYP                               
073500     MOVE RAD-KDVRINFO      TO RY4-KDVRINFO                               
073600     MOVE RY4-W440300       TO LOGG-LOGGPOST                              
073700                                                                          
073800     PERFORM IMS-INSERT-ZZAC01                                            
073900                                                                          
074000     PERFORM UNTIL SEGMENT-FINNS                                          
074100                                                                          
074200       IF LOGG-IDLOGLOP = 9                                               
074300         MOVE ZERO              TO LOGG-IDLOGLOP                          
074400         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
074500       END-IF                                                             
074600                                                                          
074700       ADD +1                   TO LOGG-IDLOGLOP                          
074800       PERFORM IMS-INSERT-ZZAC01                                          
074900     END-PERFORM                                                          
075000     .                                                                    
075100     EJECT                                                                
075200 S08-ORDP-STPOS-WDA5ASEQ SECTION.                                         
075300*****************************************************************         
075400*                                                                         
075500*    POSITIONERAR FÖRE ARTIKELNS FÖRSTA WDA5ASEQ                          
075600*    GENOM ATT LÄSA GU MED IDARTNR + RESTEN LOW-VALUE.                    
075700*    NÖDVÄNDIGT EFTERSOM FLERA TÄCKNINGS-OMGÅNGAR KAN GÖRAS.              
075800*                                                                         
075900*****************************************************************         
076000                                                                          
076100     MOVE LOW-VALUE              TO W1-WDA5ASEQ-X                         
076200     MOVE 4506-IDARTNR           TO W1-IDARTNR                            
076300     MOVE 4505-IDDC              TO W1-IDDC                               
076400                                                                          
076500     PERFORM IMS-GU-ORDP-STPOS-ASEQ                                       
076600     .                                                                    
076700     EJECT                                                                
076800 S10-INSERT-ORDP01 SECTION.                                               
076900*****************************************************************         
077000*                                                                         
077100*    RO-RAD INSERTAS DÅ TIDIGARE RAD MÅST DELAS PGA DEL-TÄCKNING.         
077200*    LÖPNR STEGAS UPP TILLS INSERT LYCKAS I OCH MED ATT ETT               
077300*    LEDIGT LÖPNR PÅTRÄFFAS.                                              
077400*                                                                         
077500*****************************************************************         
077600                                                                          
077700     ADD +1                TO RAD-IDLOPNR                                 
077800     PERFORM IMS-INSERT-ORDP01                                            
077900     PERFORM UNTIL SEGMENT-FINNS                                          
078000       ADD +1             TO RAD-IDLOPNR                                  
078100       PERFORM IMS-INSERT-ORDP01                                          
078200     END-PERFORM                                                          
078300     .                                                                    
078400     EJECT                                                                
078500 S11-SKAPA-ORDERHUVUD  SECTION.                                           
078600*****************************************************************         
078700*                                                                         
078800*    RO-RAD FRÅN LDC VERKSTADSORDER SKALL BIPACKAS SÅ SNART SOM           
078900*    MÖJLIGT MED EN SEPARAT ORDER PER VERKSTADSORDER. HÄR SKAPAS          
079000*    ORDERHUVUDEN SOM KOMMER ATT BIPACKA RADEN I W411BIPA.                
079100*                                                                         
079200*****************************************************************         
079300                                                                          
079400     PERFORM S11A-SKAPA-ORDERNR                                           
079500     PERFORM S11B-SKAPA-TRANS-ORDERHUVUD                                  
079600     PERFORM S11C-SKAPA-HUVUD-ORDERRADER                                  
079700     PERFORM S12-SKICKA-TRANS                                             
079800                                                                          
079900                                                                          
080000     .                                                                    
080100                                                                          
080200 S11A-SKAPA-ORDERNR  SECTION.                                             
080300*****************************************************************         
080400*                                                                         
080500*    TA UT ETT ORDERNUMMER MHA W411ORDN.                                  
080600*                                                                         
080700*****************************************************************         
080800                                                                          
080900                                                                          
081000     MOVE 'W440'         TO ORDN-IDSYSTEM                                 
081100     MOVE RAD-IDDISTR    TO ORDN-IDDISTR                                  
081200     MOVE RAD-IDKUNDNR   TO ORDN-IDKUNDNR                                 
081300     MOVE ZERO           TO ORDN-IDORDNR-IN                               
081400                                                                          
081500     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
081600                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
081700                                                                          
081800     MOVE ORDN-IDORDNR-UT TO WS-IDORDNR-NUM                               
081900     IF EMOH-KDSVAR-CREATE                                                
082000        MOVE EMOH-IXHALV  TO ORD-IX                                       
082100        MOVE WS-IDORDNR-NUM                                               
082200                          TO TAB-IDORDNR(ORD-IX)                          
082300     END-IF                                                               
082400     .                                                                    
082500 S11B-SKAPA-TRANS-ORDERHUVUD SECTION.                                     
082600                                                                          
082700     MOVE SPACE         TO MSG-KOM-WMSGKOM                                
082800     MOVE +54           TO MSG-KOM-KVLL                                   
082900     MOVE LOW-VALUE     TO MSG-KOM-KDZ1                                   
083000                           MSG-KOM-KDZ2                                   
083100     MOVE SPACE         TO MSG-KOM-KDTRANS                                
083200     MOVE 'W4I25101'    TO MSG-KOM-IDCPYTXT                               
083300     MOVE 'NDC-RO  '    TO MSG-KOM-IDSNDNOD                               
083400     MOVE 'W4403200'    TO MSG-KOM-IDSNDJOB                               
083500     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
083600     ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                    
083700     MOVE SPACE         TO MSG-KOM-IDMFSMED                               
083800                           MSG-KOM-KDSVAR                                 
083900                                                                          
084000     MOVE LENGTH OF OHUV-MID-W4I25101 TO MSG-KVLL                         
084100     ADD +17                TO MSG-KVLL                                   
084200     MOVE LOW-VALUE         TO MSG-KDZ1                                   
084300                               MSG-KDZ2                                   
084400     MOVE 'W4T251X'         TO MSG-KDTRANS-1                              
084500     MOVE '4251'            TO MSG-IDTRANS-1                              
084600     MOVE '1'               TO MSG-KDMFSFOR-1                             
084700                                                                          
084800     MOVE SPACE             TO OHUV-MID-W4I25101                          
084992     IF RAD-IDSYSTEM (1:3) = 'LYN'                                        
084993        MOVE 'LYND'         TO OHUV-MID-IDSYSTEM                          
084994     ELSE                                                                 
084995        IF RAD-IDSYSTEM (1:3) = 'POL'                                     
084996           MOVE 'POLD'      TO OHUV-MID-IDSYSTEM                          
084997        ELSE                                                              
084998           IF RAD-IDSYSTEM (1:3) = 'ECO'                                  
084999             MOVE 'ECOD'      TO OHUV-MID-IDSYSTEM                        
085000           ELSE                                                           
085001             IF RAD-IDSYSTEM (1:3) = 'VOU'                                
085002                MOVE 'VOUD'      TO OHUV-MID-IDSYSTEM                     
085003             ELSE                                                         
085004               IF RAD-IDSYSTEM (1:3) = 'TAD'                              
085005                  MOVE 'TADD'    TO OHUV-MID-IDSYSTEM                     
085006               ELSE                                                       
085007                 IF RAD-IDSYSTEM (1:3) = 'ACC'                            
085008                    MOVE 'ACCD'    TO OHUV-MID-IDSYSTEM                   
085009                 ELSE                                                     
085010                   IF RAD-IDSYSTEM (1:3) = 'APA'                          
085011                      MOVE 'APAD'  TO OHUV-MID-IDSYSTEM                   
085012                   ELSE                                                   
085013                     IF RAD-IDSYSTEM (1:3) = 'APB'                        
085014                        MOVE 'APBD' TO OHUV-MID-IDSYSTEM                  
085015                     ELSE                                                 
085016                       IF RAD-IDSYSTEM (1:3) = 'APC'                      
085017                          MOVE 'APCD' TO OHUV-MID-IDSYSTEM                
085018                       ELSE                                               
085019                         IF RAD-IDSYSTEM (1:3) = 'APD'                    
085020                            MOVE 'APDD'      TO OHUV-MID-IDSYSTEM         
085021                         ELSE                                             
085022                           IF RAD-IDSYSTEM (1:3) = 'APE'                  
085023                              MOVE 'APED'    TO OHUV-MID-IDSYSTEM         
085024                           ELSE                                           
085025                             IF RAD-IDSYSTEM (1:3) = 'APF'                
085026                               MOVE 'APFD'    TO OHUV-MID-IDSYSTEM        
085027                             ELSE                                         
085028                               IF RAD-IDSYSTEM (1:3) = 'APG'              
085029                                  MOVE 'APGD' TO OHUV-MID-IDSYSTEM        
085030                               ELSE                                       
085031                                 IF RAD-IDSYSTEM (1:3) = 'APH'            
085032                                    MOVE 'APHD'                           
085033                                              TO OHUV-MID-IDSYSTEM        
085034                                 ELSE                                     
085035                                  IF RAD-IDSYSTEM (1:3) = 'API'           
085036                                     MOVE 'APID'                          
085037                                              TO OHUV-MID-IDSYSTEM        
085038                                  ELSE                                    
085039                                    IF RAD-IDSYSTEM (1:3) = 'APJ'         
085040                                       MOVE 'APJD'                        
085041                                              TO OHUV-MID-IDSYSTEM        
085042                                    ELSE                                  
085043                                       MOVE 'LDCD'                        
085044                                              TO OHUV-MID-IDSYSTEM        
085045                                    END-IF                                
085046                                  END-IF                                  
085047                                 END-IF                                   
085048                               END-IF                                     
085049                             END-IF                                       
085050                           END-IF                                         
085051                         END-IF                                           
085052                       END-IF                                             
085053                      END-IF                                              
085054                    END-IF                                                
085055                 END-IF                                                   
085056               END-IF                                                     
085057             END-IF                                                       
085058           END-IF                                                         
085059        END-IF                                                            
085060     END-IF                                                               
085070     MOVE RAD-IDDISTR       TO WS-IDDISTR-NUM                             
085100     MOVE WS-IDDISTR        TO OHUV-MID-IDDISTR                           
085200     MOVE RAD-IDKUNDNR      TO WS-IDKUNDNR-NUM                            
085300     MOVE WS-IDKUNDNR       TO OHUV-MID-IDKUNDNR                          
085400     MOVE WS-IDORDNR        TO OHUV-MID-IDORDNR                           
085500     MOVE RAD-KDORDKL       TO OHUV-MID-KDORDKL                           
085600     IF RAD-IDKONTO = ZERO                                                
085700        MOVE SPACE          TO OHUV-MID-IDKONTO                           
085800     ELSE                                                                 
085900        MOVE RAD-IDKONTO    TO WS-IDKONTO-DISPLAY                         
086000        MOVE WS-IDKONTO-DISPLAY TO OHUV-MID-IDKONTO                       
086100        MOVE '57'               TO OHUV-MID-IDFTG                         
086200     END-IF                                                               
086300     MOVE RAD-IDKST         TO OHUV-MID-IDKST                             
086400      IF  EMOH-KDSVAR-CREATE                                              
086500        MOVE '3'            TO OHUV-MID-KDROPACK                          
086600        MOVE EMOH-BEGMT     TO OHUV-MID-BEGMT                             
086700        MOVE EMOH-ADGMT-GATA                                              
086800                            TO OHUV-MID-ADGMT-GATA                        
086900        MOVE EMOH-ADGMT-PADR                                              
087000                            TO OHUV-MID-ADGMT-PADR                        
087100        MOVE EMOH-BELAGINS-GRP                                            
087200                            TO OHUV-MID-BELAGINS                          
087300        INITIALIZE             EMOH-KDSVAR                                
087400      ELSE                                                                
087500        MOVE 'P'            TO OHUV-MID-KDROPACK                          
087600      END-IF                                                              
087700     MOVE RAD-IDANALYS      TO OHUV-MID-IDANALYS                          
087800     MOVE RAD-IDDC          TO OHUV-MID-IDDC                              
087900     MOVE NEJ               TO OHUV-MID-FLAUTFAK                          
088000     MOVE NEJ               TO OHUV-MID-FLAUTPAC                          
088100                               OHUV-MID-FLEMBORD                          
088200                               OHUV-MID-FLOVRLEV                          
088300                               OHUV-MID-FLFORBI                           
088400     MOVE RAD-KDORDTYP-LDC  TO OHUV-MID-KDORDTYP-LDC                      
088500     PERFORM S11BA-SKAPA-RFSDATUM                                         
088600     MOVE WS-TIRFS          TO OHUV-MID-TIRFS                             
088700     MOVE RAD-TIREPDAT      TO OHUV-MID-TIREPDAT                          
088800     MOVE NEJ               TO OHUV-MID-FLORDTIL                          
088900                               OHUV-MID-IDGROSS                           
089000***FÖR ATT HÅLLA REDA PÅ ATT DET ÄR SKAPAT ETT ORDERHUVUD FÖR EN          
089100     MOVE RAD-TIREPDAT      TO W-IDKUNDRF-WIP                             
089200***                                                                       
089300     DISPLAY 'NEW ORDER DETAILS'                                          
089400     DISPLAY '******************'                                         
089500     DISPLAY'IDDISTR  -  'OHUV-MID-IDDISTR                                
089600     DISPLAY'IDKUNDNR -  'OHUV-MID-IDKUNDNR                               
089700     DISPLAY'IDORDNR  -  'OHUV-MID-IDORDNR                                
089800     DISPLAY'IDARTNR  -  'RAD-IDARTNR                                     
089900     DISPLAY'KDROPACK -  'RAD-KDROPACK                                    
090000     DISPLAY'TIREPDAT -  'RAD-TIREPDAT                                    
090100     DISPLAY'OLD-ORDERNO-'RAD-IDORDNR5                                    
090200***                                                                       
090300     MOVE OHUV-AREA TO MSG-MID-OUT                                        
090400     PERFORM S12-SKICKA-TRANS                                             
090500     .                                                                    
090600     EJECT                                                                
090700 S11BA-SKAPA-RFSDATUM SECTION.                                            
090800                                                                          
090900     MOVE RAD-IDDISTR              TO W-IDDISTR-WDB2                      
091000     MOVE RAD-IDKUNDNR             TO W-IDKUNDNR-WDB2                     
091100     PERFORM IMS-GU-WDB201                                                
091200                                                                          
091300     MOVE RAD-IDDC                 TO WORK-IDDC                           
091400     MOVE +002                     TO WORK-KDCALL                         
091500     MOVE +001                     TO WORK-KVWORKD                        
091600     IF  RAD-TIREPDAT     = ZERO                                          
091700        MOVE WS-DAT                TO WORK-TIAAMMDD-FOM                   
091800     ELSE                                                                 
091900        MOVE RAD-TIREPDAT          TO WORK-TIAAMMDD-FOM                   
092000     END-IF                                                               
092100     CALL WORKDAY                  USING WORK-KDCALL                      
092200                                         WORK-DATE-AREA                   
092300                                         WORK-KDSVAR                      
092400     IF WORK-KDSVAR-FEL                                                   
092500        MOVE 'SECT S01-1, DATUM SAKNAS I WORKDAY'                         
092600                                   TO FELTEXT                             
092700        CALL ABEND                 USING RKOD-ABEND-UTAN-DUMP             
092800     ELSE                                                                 
092900       MOVE +003                   TO WORK-KDCALL                         
093000       MOVE GMT-KVDAGAR-RFS-DEF    TO WORK-KVWORKD                        
093100       PERFORM                                                            
093200       VARYING RFS-IX FROM 1 BY 1                                         
093300         UNTIL RFS-IX > MAX-RFS-IX                                        
093400         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
093500           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
093600                                   TO WORK-KVWORKD                        
093700         END-IF                                                           
093800       END-PERFORM                                                        
093900       ADD +1  TO WORK-KVWORKD                                            
094000*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
094100*      ANTAL DAGAR FÖRE RFS.                                              
094200*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
094300*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
094400*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
094500*                                                                         
094600                                                                          
094700       CALL WORKDAY                USING WORK-KDCALL                      
094800                                         WORK-DATE-AREA                   
094900                                         WORK-KDSVAR                      
095000       IF WORK-KDSVAR-FEL                                                 
095100          MOVE 'SECT S01-2, DATUM SAKNAS I WORKDAY'                       
095200                                   TO FELTEXT                             
095300          CALL ABEND               USING RKOD-ABEND-UTAN-DUMP             
095400       ELSE                                                               
095500         IF WORK-TIAAMMDD-FOM < WS-DAT                                    
095600           MOVE RAD-IDDC           TO WORK-IDDC                           
095700           MOVE +002               TO WORK-KDCALL                         
095800           MOVE +001               TO WORK-KVWORKD                        
095900           MOVE WS-DAT             TO WORK-TIAAMMDD-FOM                   
096000           CALL WORKDAY            USING WORK-KDCALL                      
096100                                         WORK-DATE-AREA                   
096200                                         WORK-KDSVAR                      
096300           IF WORK-KDSVAR-FEL                                             
096400              MOVE 'SECT S01-3, DATUM SAKNAS I WORKDAY'                   
096500                                   TO FELTEXT                             
096600              CALL ABEND           USING RKOD-ABEND-UTAN-DUMP             
096700           ELSE                                                           
096800              MOVE WORK-TIAAMMDD-TOM TO WS-TIRFS                          
096900           END-IF                                                         
097000         ELSE                                                             
097100           MOVE WORK-TIAAMMDD-FOM  TO WS-TIRFS                            
097200         END-IF                                                           
097300         IF WS-TIRFS = WS-DAT                                             
097400            MOVE ZERO              TO WS-TIRFS                            
097500*           OM VI SKICKAR DAGENS DATUM TILL 4251                          
097600*           KOMMER SEDAN W411TRAN ATT ANROPAS MED ZERO I TID              
097700*           OCH VI KOMMER ATT FÅ FÖRSTA TRANSPORT DEN DAGEN               
097800*           SKICKAR VI ZERO TILL 4251 KOMMER VI ATT FÅ FÖRSTA             
097900*           MÖJLIGA TRANSPORT (EVENTUELLT NÄSTA DAG)                      
098000*           PROBLEMET VI VILL LÖSA ÄR OM VI FÅR RFS = DAGENS              
098100*           KAN VI FÅ EN AVGÅNGSTID TIDIGARE ÄN ORDERN ÄR LAGD            
098200                                                                          
098300         END-IF                                                           
098400       END-IF                                                             
098500     END-IF                                                               
098600     .                                                                    
098700     EJECT                                                                
098800 S11C-SKAPA-HUVUD-ORDERRADER SECTION.                                     
098900                                                                          
099000     MOVE 'W4I25201'    TO MSG-KOM-IDCPYTXT                               
099100     MOVE LENGTH OF ORAD-MID-W4I25201 TO MSG-KVLL                         
099200     ADD +17            TO MSG-KVLL                                       
099300     MOVE LOW-VALUE     TO MSG-KDZ1                                       
099400                           MSG-KDZ2                                       
099500     MOVE 'W4T252X'     TO MSG-KDTRANS-1                                  
099600     MOVE '4252'        TO MSG-IDTRANS-1                                  
099700     MOVE '1'           TO MSG-KDMFSFOR-1                                 
099800                                                                          
099900     MOVE SPACE         TO ORAD-MID-W4I25201                              
100106     IF RAD-IDSYSTEM (1:3) = 'LYN'                                        
100107        MOVE 'LYND'         TO OHUV-MID-IDSYSTEM                          
100108     ELSE                                                                 
100109        IF RAD-IDSYSTEM (1:3) = 'POL'                                     
100110           MOVE 'POLD'      TO OHUV-MID-IDSYSTEM                          
100111        ELSE                                                              
100112           IF RAD-IDSYSTEM (1:3) = 'ECO'                                  
100113             MOVE 'ECOD'      TO OHUV-MID-IDSYSTEM                        
100114           ELSE                                                           
100115             IF RAD-IDSYSTEM (1:3) = 'VOU'                                
100116                MOVE 'VOUD'      TO OHUV-MID-IDSYSTEM                     
100117             ELSE                                                         
100118               IF RAD-IDSYSTEM (1:3) = 'TAD'                              
100119                  MOVE 'TADD'    TO OHUV-MID-IDSYSTEM                     
100120               ELSE                                                       
100121                 IF RAD-IDSYSTEM (1:3) = 'ACC'                            
100122                    MOVE 'ACCD'    TO OHUV-MID-IDSYSTEM                   
100123                 ELSE                                                     
100124                   IF RAD-IDSYSTEM (1:3) = 'APA'                          
100125                      MOVE 'APAD'  TO OHUV-MID-IDSYSTEM                   
100126                   ELSE                                                   
100127                     IF RAD-IDSYSTEM (1:3) = 'APB'                        
100128                        MOVE 'APBD' TO OHUV-MID-IDSYSTEM                  
100129                     ELSE                                                 
100130                       IF RAD-IDSYSTEM (1:3) = 'APC'                      
100131                          MOVE 'APCD' TO OHUV-MID-IDSYSTEM                
100132                       ELSE                                               
100133                         IF RAD-IDSYSTEM (1:3) = 'APD'                    
100134                            MOVE 'APDD'      TO OHUV-MID-IDSYSTEM         
100135                         ELSE                                             
100136                           IF RAD-IDSYSTEM (1:3) = 'APE'                  
100137                              MOVE 'APED'    TO OHUV-MID-IDSYSTEM         
100138                           ELSE                                           
100139                             IF RAD-IDSYSTEM (1:3) = 'APF'                
100140                               MOVE 'APFD'    TO OHUV-MID-IDSYSTEM        
100141                             ELSE                                         
100142                               IF RAD-IDSYSTEM (1:3) = 'APG'              
100143                                  MOVE 'APGD' TO OHUV-MID-IDSYSTEM        
100144                               ELSE                                       
100145                                 IF RAD-IDSYSTEM (1:3) = 'APH'            
100146                                    MOVE 'APHD'                           
100147                                              TO OHUV-MID-IDSYSTEM        
100148                                 ELSE                                     
100149                                  IF RAD-IDSYSTEM (1:3) = 'API'           
100150                                     MOVE 'APID'                          
100151                                              TO OHUV-MID-IDSYSTEM        
100152                                  ELSE                                    
100153                                    IF RAD-IDSYSTEM (1:3) = 'APJ'         
100154                                       MOVE 'APJD'                        
100155                                              TO OHUV-MID-IDSYSTEM        
100156                                    ELSE                                  
100157                                       MOVE 'LDCD'                        
100158                                              TO OHUV-MID-IDSYSTEM        
100159                                    END-IF                                
100160                                  END-IF                                  
100161                                 END-IF                                   
100162                               END-IF                                     
100163                             END-IF                                       
100164                           END-IF                                         
100165                         END-IF                                           
100166                       END-IF                                             
100167                      END-IF                                              
100168                    END-IF                                                
100169                 END-IF                                                   
100170               END-IF                                                     
100171             END-IF                                                       
100172           END-IF                                                         
100173        END-IF                                                            
100174     END-IF                                                               
100180     MOVE WS-IDDISTR    TO ORAD-MID-IDDISTR                               
100200     MOVE WS-IDKUNDNR   TO ORAD-MID-IDKUNDNR                              
100300     MOVE WS-IDORDNR    TO ORAD-MID-IDORDNR                               
100400     MOVE SPACE         TO ORAD-MID-BEVOLREF                              
100500     MOVE 'J'           TO ORAD-MID-FLSLUT                                
100600     MOVE ORAD-AREA     TO MSG-MID-OUT                                    
100700     .                                                                    
100800     EJECT                                                                
100900 S12-SKICKA-TRANS SECTION.                                                
101000                                                                          
101100     CALL W006KOM USING MSG-PCB                                           
101200                        0693-PCB                                          
101300                        WDP8-PCB                                          
101400                        MSG-KOM-WMSGKOM                                   
101500                        MSG-IO-AREA                                       
101600     .                                                                    
101700     EJECT                                                                
101800*    ---- IMS SEKTIONER                                                   
101900                                                                          
102000 IMS-GU-450501 SECTION.                                                   
102100                                                                          
102200     STRING 'WL450501(WDGXKEY  =' W-WDGX-4505-KEY-X ')'                   
102300            DELIMITED BY SIZE INTO SSA1                                   
102400     MOVE '  GE'                TO GODK-STATUSKODER                       
102500     CALL CBLTDLI USING GU 4505-PCB DLI-IO-GX4505 SSA1                    
102600     MOVE 4505-STATUS-CODE      TO STATUS-WS                              
102700     PERFORM IMS-STATUSKONTROLL                                           
102800     .                                                                    
102900     EJECT                                                                
103000 IMS-GHNP-450511-TAECKTRANS SECTION.                                      
103100                                                                          
103200     MOVE 'WL450511 '           TO SSA1                                   
103300     MOVE '  GE'                TO GODK-STATUSKODER                       
103400     CALL CBLTDLI USING GHNP 4505-PCB DLI-IO-GX4506 SSA1                  
103500     MOVE 4505-STATUS-CODE      TO STATUS-WS                              
103600     PERFORM IMS-STATUSKONTROLL                                           
103700     .                                                                    
103800     EJECT                                                                
103900 IMS-DELETE-450511-TAECKTRANS SECTION.                                    
104000                                                                          
104100     ADD +1                  TO CHKP-ANT                                  
104200     MOVE '  '               TO GODK-STATUSKODER                          
104300     CALL CBLTDLI USING DLET  4505-PCB DLI-IO-GX4506                      
104400     MOVE 4505-STATUS-CODE   TO STATUS-WS                                 
104500     PERFORM IMS-STATUSKONTROLL                                           
104600     .                                                                    
104700     SKIP3                                                                
104800 IMS-INSERT-ZZAC01 SECTION.                                               
104900                                                                          
105000     ADD +1                     TO CHKP-ANT                               
105100     MOVE 'WLZZAC01 '           TO SSA1                                   
105200     MOVE '  II'                TO GODK-STATUSKODER                       
105300     CALL CBLTDLI USING ISRT  ZZAC-PCB DLI-IO-ZZAC01 SSA1                 
105400     MOVE ZZAC-STATUS-CODE      TO STATUS-WS                              
105500     PERFORM IMS-STATUSKONTROLL                                           
105600     .                                                                    
105700     EJECT                                                                
105800 IMS-GU-ARTC01 SECTION.                                                   
105900                                                                          
106000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-K6-X ')'                      
106100            DELIMITED BY SIZE INTO SSA1                                   
106200     MOVE '  GE'                TO GODK-STATUSKODER                       
106300     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-ARTC01 SSA1                   
106400     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
106500     PERFORM IMS-STATUSKONTROLL                                           
106600     .                                                                    
106700     EJECT                                                                
106800 IMS-GNP-ARTC11 SECTION.                                                  
106900     MOVE 'WLARTC11 '           TO SSA1                                   
107000     MOVE '  GE'                TO GODK-STATUSKODER                       
107100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
107200     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
107300     PERFORM IMS-STATUSKONTROLL                                           
107400     .                                                                    
107500     SKIP2                                                                
107600 IMS-GU-WDK711 SECTION.                                                   
107700                                                                          
107800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
107900            DELIMITED BY SIZE INTO SSA1                                   
108000     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
108100            DELIMITED BY SIZE INTO SSA2                                   
108200     MOVE '  GE'                TO GODK-STATUSKODER                       
108300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
108400     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
108500     PERFORM IMS-STATUSKONTROLL                                           
108600     .                                                                    
108700     SKIP2                                                                
108800 IMS-GHU-WDK711 SECTION.                                                  
108900                                                                          
109000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
109100            DELIMITED BY SIZE INTO SSA1                                   
109200     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
109300            DELIMITED BY SIZE INTO SSA2                                   
109400     MOVE '  GE'                TO GODK-STATUSKODER                       
109500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
109600     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
109700     PERFORM IMS-STATUSKONTROLL                                           
109800     .                                                                    
109900     EJECT                                                                
110000 IMS-REPLACE-WDK711 SECTION.                                              
110100                                                                          
110200     ADD +1                     TO CHKP-ANT                               
110300     MOVE '  '                  TO GODK-STATUSKODER                       
110400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
110500     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
110600     PERFORM IMS-STATUSKONTROLL                                           
110700     .                                                                    
110800     EJECT                                                                
110900 IMS-GU-WDK722 SECTION.                                                   
111000                                                                          
111100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
111200            DELIMITED BY SIZE INTO SSA1                                   
111300     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
111400            DELIMITED BY SIZE INTO SSA2                                   
111500     MOVE   'WDK722 '           TO SSA3                                   
111600     MOVE '  GE'                TO GODK-STATUSKODER                       
111700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
111800     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
111900     PERFORM IMS-STATUSKONTROLL                                           
112000     .                                                                    
112100     EJECT                                                                
112200 IMS-GU-ORDP-STPOS-ASEQ SECTION.                                          
112300                                                                          
112400     STRING 'WLORDP01(WDA5ASEQ =' W1-WDA5ASEQ-X ')'                       
112500            DELIMITED BY SIZE INTO SSA1                                   
112600     MOVE '  GE'              TO GODK-STATUSKODER                         
112700     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-ORDP01 SSA1                    
112800     MOVE ORDP-STATUS-CODE      TO STATUS-WS                              
112900     PERFORM IMS-STATUSKONTROLL                                           
113000     .                                                                    
113100     SKIP3                                                                
113200 IMS-GHN-ORDQ01-ORDP01 SECTION.                                           
113300                                                                          
113400     STRING 'WLORDP01(WDA5ASEQ >' W1-WDA5ASEQ-X                           
113500                    '&WDA5ASEQ <' W2-WDA5ASEQ-X                           
113600                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
113700            DELIMITED BY SIZE INTO SSA1                                   
113800     MOVE '  GE'              TO GODK-STATUSKODER                         
113900     CALL CBLTDLI USING GHN ORDP-PCB DLI-IO-ORDP01 SSA1                   
114000     MOVE ORDP-STATUS-CODE      TO STATUS-WS                              
114100     PERFORM IMS-STATUSKONTROLL                                           
114200     .                                                                    
114300     EJECT                                                                
114400 IMS-INSERT-ORDP01 SECTION.                                               
114500                                                                          
114600     ADD +1                     TO CHKP-ANT                               
114700     MOVE 'WLORDP01 '           TO SSA1                                   
114800     MOVE '  II'                TO GODK-STATUSKODER                       
114900     CALL CBLTDLI USING ISRT  ORDP2-PCB DLI-IO-ORDP01 SSA1                
115000     MOVE ORDP2-STATUS-CODE     TO STATUS-WS                              
115100     PERFORM IMS-STATUSKONTROLL                                           
115200     .                                                                    
115300     SKIP3                                                                
115400 IMS-REPLACE-ORDQ01-ORDP01 SECTION.                                       
115500                                                                          
115600     ADD +1                  TO CHKP-ANT                                  
115700     MOVE '  '               TO GODK-STATUSKODER                          
115800     CALL CBLTDLI USING REPL  ORDP-PCB DLI-IO-ORDP01                      
115900     MOVE ORDP-STATUS-CODE   TO STATUS-WS                                 
116000     PERFORM IMS-STATUSKONTROLL                                           
116100     .                                                                    
116200     EJECT                                                                
116300 IMS-GU-WDB201 SECTION.                                                   
116400                                                                          
116500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
116600            DELIMITED BY SIZE INTO SSA1                                   
116700                                                                          
116800     MOVE '    ' TO GODK-STATUSKODER                                      
116900     CALL CBLTDLI USING                                                   
117000           GU WDB2-PCB DLI-IO-WDB2 SSA1                                   
117100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
117200     PERFORM IMS-STATUSKONTROLL                                           
117300     .                                                                    
117400 IMS-GN-WDB601 SECTION.                                                   
117500                                                                          
117600     MOVE 'WDB601 ' TO SSA1                                               
117700                                                                          
117800     MOVE '  GB' TO GODK-STATUSKODER                                      
117900     CALL CBLTDLI USING                                                   
118000           GN WDB6-PCB DLI-IO-WDB6 SSA1                                   
118100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
118200     PERFORM IMS-STATUSKONTROLL                                           
118300     .                                                                    
118400 IMS-RESTART SECTION.                                                     
118500                                                                          
118600     MOVE SPACE TO MSG-IO-AREA-1                                          
118700     MOVE '  '  TO GODK-STATUSKODER                                       
118800     CALL CBLTDLI USING XRST MSG-PCB                                      
118900                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
119000                        CHKP-AREA-1-LENGTH   CHKP-AREA-1                  
119100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
119200     PERFORM IMS-STATUSKONTROLL                                           
119300     IF IMS-EJ-OK                                                         
119400        CALL FELLOG                                                       
119500     END-IF                                                               
119600     .                                                                    
119700     SKIP1                                                                
119800 IMS-CHECKPOINT SECTION.                                                  
119900                                                                          
120000     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
120100     MOVE '  XD' TO GODK-STATUSKODER                                      
120200     CALL CBLTDLI USING CHKP MSG-PCB                                      
120300                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
120400                        CHKP-AREA-1-LENGTH   CHKP-AREA-1                  
120500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
120600     PERFORM IMS-STATUSKONTROLL                                           
120700     IF IMS-EJ-OK                                                         
120800        CALL FELLOG                                                       
120900     END-IF                                                               
121000     .                                                                    
121100     SKIP1                                                                
121200 IMS-STATUSKONTROLL SECTION.                                              
121300                                                                          
121400     SET STATUS-IX TO 1                                                   
121500     SEARCH GODK-STATUS                                                   
121600       AT END                                                             
121700         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
121800         DELIMITED BY SIZE INTO FELTEXT                                   
121900         CALL FELLOG                                                      
122000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
122100         CONTINUE                                                         
122200     END-SEARCH                                                           
122300     .                                                                    
