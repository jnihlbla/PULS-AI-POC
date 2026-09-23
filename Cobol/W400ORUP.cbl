000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W400ORUP.                                                
000400 AUTHOR.         ARUP DATTA.                                              
000500 DATE-WRITTEN.   2024-04-15.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        THE IS A SUBPROGRAM WHICH HANDLES ORDER LINE ADDITION            
001200*        AND UPDATION                                                     
001300*        TRANSACTION W4T258 INVOKED FOR ORDERLINE UPDATE                  
001400*                                                                         
001500*        THE PROGRAM READS     WDK6                                       
001700*                              WDB2                                       
001800*                              WDF5                                       
001900*                              WDQ2                                       
001910*                              WDQ4                                       
002000*                                                                         
002100*    ABENDCODES:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000*    -COPY WY2000W1                                                       
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W400ORUP'.            
004300 77  WS-CURRENT-SECTION          PIC X(30)   VALUE SPACE.                 
004400 77  WS-CURRENT-IMS-SECTION      PIC X(30)   VALUE SPACE.                 
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700 77  FEL                         PIC X       VALUE 'F'.                   
004800     EJECT                                                                
005200*                                                                         
005300 77  OK-SW                       PIC X       VALUE 'J'.                   
005400     88  EVERYTHING-OK                       VALUE 'J'.                   
005500     88  SOMETHING-WRONG                     VALUE 'N'.                   
005900                                                                          
006900 77  DUP-FND-SW                  PIC X       VALUE 'N'.                   
007000     88  DUP-FND                             VALUE 'J'.                   
007100                                                                          
007200 77  SW-SOFTWARE-ORDER           PIC X       VALUE 'N'.                   
007300     88  SOFTWARE-ORDER                      VALUE 'J'.                   
007400                                                                          
007500 77  PREPLANED-SW                PIC X       VALUE 'N'.                   
007600     88  PREPLANED                           VALUE 'J'.                   
007700                                                                          
007710 77  DC11-LINE-SW                PIC X       VALUE 'N'.                   
007720     88  DC11-LINE-FND                       VALUE 'J'.                   
007730                                                                          
007800 77  WS-UPD-FUNC                 PIC X       VALUE SPACES.                
007900     88 UPD-ORD-LINE                         VALUE 'U'.                   
008000     88 ADD-ORD-LINE                         VALUE 'A'.                   
008200                                                                          
008300     EJECT                                                                
008400 01  WS-WORK-AREA.                                                        
008500     03  KVRADER-IX                PIC S9(5)  VALUE +0 COMP SYNC.         
008600     03  KVRADER-IX-MAX            PIC S9(5)  VALUE +0 COMP SYNC.         
008700     03  4258-MID-INDX             PIC  9(3)  VALUE ZERO.                 
008800     03  4258-MID-INDX-MAX         PIC  9(3)  VALUE 13.                   
008900     03  MSG-IX                    PIC S9(9)  VALUE +0 COMP SYNC.         
009000     03  W-BLANKS                  PIC  9(5)  VALUE ZERO.                 
009100     03  W-LENGTH                  PIC  9(5)  VALUE ZERO.                 
009200     03  WS-KDSTARAD               PIC  9(1)  VALUE ZERO.                 
009300     03  WS-TIREGDAT               PIC  9(6)  VALUE ZERO.                 
009400     03  WS-IDDISTR                PIC  9(4)  VALUE ZERO.                 
009500     03  WS-IDKUNDNR               PIC  9(6)  VALUE ZERO.                 
009600     03  WS-IDORDNR                PIC  9(7)  VALUE ZERO.                 
009700     03  WS-IDARTNR                PIC  9(9)  VALUE ZERO.                 
009800     03  WS-KVBEART                PIC S9(7)  VALUE ZERO COMP-3.          
009900     03  WS-IDDC                   PIC  X(2)  VALUE SPACES.               
010000     03  WS-REKSIFFR               PIC  X     VALUE SPACES.               
010100     03  WS-KVRADER-IX-NUM         PIC  9(5)  VALUE ZERO.                 
010200     03  WS-KVRADER-IX-DISP        PIC  X(5)  VALUE SPACES.               
010300     03  WS-IDORDNR7               PIC  9(7)  VALUE ZERO.                 
010400     03  WS-KDORDSTA-O             PIC  X(2)  VALUE SPACE.                
010500     03  SPEC-FORBI                PIC  X(1)  VALUE 'S'.                  
010800     03  W-GMT-IDDC-CLEAR-GRP.                                            
010900*                                      GRUPP AV IDDC-CLEAR                
011000         05 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                            
011100                                   PIC X(2)    VALUE SPACE.               
011200     03 W-IDARTNR-CHAR.                                                   
011300        05 W-IDARTNR-NUM           PIC  9(8)   VALUE ZERO.                
011400     03 WS-UPD-REQUEST.                                                   
011500        05 WS-UPD-ARTNR-DATA  OCCURS 999 TIMES.                           
012100           07 WS-UPD-IDARTNR       PIC S9(9)  COMP-3.                     
012300     03 WS-ADD-REQUEST.                                                   
012400        05 WS-ADD-IDARTNR          PIC S9(9) COMP-3                       
012500                                      OCCURS 999 TIMES.                   
012510     03 WS-PART-DETAILS.                                                  
012520        05 WS-INPUT-PART    OCCURS 999 TIMES.                             
012530           07 WS-INP-IDLEVART      PIC X(3).                              
012540           07 WS-INP-IDARTNR       PIC X(9).                              
012550           07 WS-INP-REKSIFFR      PIC X.                                 
012560                                                                          
012600     03 W-IDARTNR-CNT            PIC 9(5)    VALUE ZERO.                  
012700     03 WS-PRARTNTO-NUM          PIC 9(7)V9(2).                           
012800     03 WS-PRARTNTO-RED          PIC 9(7).9(2).                           
012900     03 PRARTNTO-FILLER REDEFINES WS-PRARTNTO-RED.                        
013000        05 WS-PRARTNTO-ALFA      PIC X(10).                               
013100                                                                          
013200 77  IX-DCCLEAR-MAX            PIC S9(3)  COMP SYNC VALUE +99.            
013300 77  WS-INDEX                  PIC S9(9)  COMP SYNC VALUE ZERO.           
013400 77  SW-KDORDSTA-O-ALL-SPACE-FLAG PIC X(1)   VALUE 'J'.                   
013500 77  UPD-INDX                    PIC S9(4)   VALUE +0 COMP SYNC.          
013600 77  ADD-INDX                    PIC S9(4)   VALUE +0 COMP SYNC.          
013700 77  SEARCH-INDX                 PIC S9(4)   VALUE +0 COMP SYNC.          
013800                                                                          
013900 01  WS-TIRFS                    PIC 9(10).                               
014000 01  FILLER REDEFINES WS-TIRFS.                                           
014100     03  WS-TIRFS-DAT            PIC 9(6).                                
014200     03  WS-TIRFS-TID            PIC 9(4).                                
014300                                                                          
014400*                                                                         
014500 01  DAGENS-DATUM              PIC  9(6)  VALUE ZERO.                     
014600 01  FILLER              REDEFINES DAGENS-DATUM.                          
014700     03  DAGENS-AA             PIC  9(2).                                 
014800     03  DAGENS-MM             PIC  9(2).                                 
014900     03  DAGENS-DD             PIC  9(2).                                 
015000*                                                                         
015100 01  WS-LOCAL-DATE-AAMMDD      PIC 9(6)   VALUE ZERO.                     
015200                                                                          
015300 01  WS-IDSYSTEM               PIC  X(4)  VALUE SPACES.                   
015400     88 IDSYSTEM-LYNK                     VALUE 'LYNK'.                   
015500     88 IDSYSTEM-POLE                     VALUE 'POLE'.                   
015600     88 IDSYSTEM-ECOM                     VALUE 'ECOM'.                   
015700     88 IDSYSTEM-VOUI                     VALUE 'VOUI'.                   
015800     88 IDSYSTEM-TAD                      VALUE 'TAD '.                   
015900     88 IDSYSTEM-ACC                      VALUE 'ACC '.                   
016000     88 IDSYSTEM-APA                      VALUE 'APA '.                   
016100     88 IDSYSTEM-APB                      VALUE 'APB '.                   
016200     88 IDSYSTEM-APC                      VALUE 'APC '.                   
016300     88 IDSYSTEM-APD                      VALUE 'APD '.                   
016400     88 IDSYSTEM-APE                      VALUE 'APE '.                   
016500     88 IDSYSTEM-APF                      VALUE 'APF '.                   
016600     88 IDSYSTEM-APG                      VALUE 'APG '.                   
016700     88 IDSYSTEM-APH                      VALUE 'APH '.                   
016800     88 IDSYSTEM-API                      VALUE 'API '.                   
016900     88 IDSYSTEM-APJ                      VALUE 'APJ '.                   
017800     88 IDSYSTEM-VALID                    VALUE 'LYNK' 'POLE'             
017900                                                'ECOM'                    
018000                                                'VOUI' 'TAD '             
018100                                                'ACC ' 'APA '             
018200                                                'APB ' 'APC '             
018300                                                'APD ' 'APE '             
018400                                                'APF ' 'APG '             
018500                                                'APH ' 'API '             
018600                                                'APJ '.                   
018700                                                                          
018800 01  GENERAL-SUBPROGRAMS.                                                 
018900*                                                                         
019000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
019100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
019400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
019600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
019610     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
019620     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
019630     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
019700     SKIP2                                                                
019800*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
019900                                                                          
020000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
020100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
020200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
020300     SKIP2                                                                
020400 01  ERROR-TEXT.                                                          
020500     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
020600     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
020700     EJECT                                                                
020800 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
020900*01  -COPY WMSGCONV                                                       
020910 01  FILLER                      PIC X(16)   VALUE 'W411AREG'.            
020920*01  -COPY W411AREG                                                       
020930 01  FILLER                      PIC X(16)   VALUE 'W411SDCA'.            
020940*01  -COPY W411SDCA                                                       
020950 01  FILLER                      PIC X(16)   VALUE 'W411KVAN'.            
020960*01  -COPY W411KVAN                                                       
020970                                                                          
021000                                                                          
021100*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
021200 01  FILLER                      PIC X(16)   VALUE 'WDATKONV'.            
021300*01 -COPY WDATAREA                                                        
021400                                                                          
021500*    --- AREAS FOR SUB MODULES W006KOM                                    
021600 01  FILLER                    PIC X(16) VALUE 'MSG-KOM-WMSGKOM '.        
021700*01  -COPY WMSGKOM                                                        
021800                                                                          
021900 01  FILLER                    PIC X(16) VALUE 'MSG-IO-AREA     '.        
022000*01  -COPY WMSGAREA                                                       
022100                                                                          
022200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
022300 01 FILLER                       PIC X(8) VALUE 'W005INIT'.               
022400*   -COPY WMSGINIT                                                        
022500                                                                          
022600 01  FILLER                    PIC X(16) VALUE '4258-MID-AREA   '.        
022700*01  -COPY W4I25801 -PRE 4258-                                            
022800     EJECT                                                                
022900 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
023000*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
023100     EJECT                                                                
023200*01  FILLER   -COPY WWDIST11    -RED TEST-IDDISTR.                        
023300     EJECT                                                                
023400*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
023500     EJECT                                                                
023600*01  FILLER   -COPY WWDIST23    -RED TEST-IDDISTR.                        
023700     EJECT                                                                
023800*01  FILLER   -COPY WWDIST34    -RED TEST-IDDISTR.                        
023900     EJECT                                                                
024000*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
024100     EJECT                                                                
024200*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
024300*    ----DISTR-DEALER-PRICE----                                           
024400     EJECT                                                                
024500 01  FILLER                  PIC X(16)   VALUE 'WORKAREA   '.             
024600*    -COPY WORKAREA                                                       
024700     EJECT                                                                
024800*01  -COPY WWDCKONS                                                       
024900                                                                          
025000*    --- AREAS FOR IMS-SECTIONS                                           
025100*                                                                         
025200     EJECT                                                                
025300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025400     SKIP3                                                                
025500 01  KEYS-FOR-DLI.                                                        
025600                                                                          
025700     03  W-WDF5BSEQ.                                                      
025800         05  W-SEQB-IDLEVART     PIC X(30)   VALUE LOW-VALUE.             
025900                                                                          
026000     03  W-IDARTNR-X.                                                     
026100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
026200                                                                          
026300     03  W-IDORDER-X.                                                     
026400         05  W-IDORDER           PIC S9(7)      COMP-3.                   
026500                                                                          
027000     03  W-WDQ2CSEQ-X.                                                    
027100         05  W-WDQ2C-IDDISTR      PIC S9(5)   COMP-3 VALUE +0.            
027200         05  W-WDQ2C-IDKUNDNR     PIC S9(7)   COMP-3 VALUE +0.            
027300         05  W-WDQ2C-IDKUNDRF     PIC X(10)   VALUE SPACE.                
027400                                                                          
030300     03  W-KVART-MIN-X.                                                   
030400         05  W-KVART-MIN          PIC S9(7)  COMP-3   VALUE ZERO.         
030500                                                                          
030600     03  W-KVART-MAX-X.                                                   
030700         05  W-KVART-MAX          PIC S9(7)  COMP-3   VALUE ZERO.         
030800                                                                          
030900     03  W-IDDC-MIN-X.                                                    
031000         05  W-IDDC-MIN           PIC X(02)  VALUE SPACES.                
031100                                                                          
031200     03  W-IDDC-MAX-X.                                                    
031300         05  W-IDDC-MAX           PIC X(02)  VALUE SPACES.                
031400                                                                          
031500     03  W-WDQ4A1KY-MIN-X.                                                
031600         05  W-Q4SEQA-IDORDER-MIN PIC S9(7)    VALUE ZERO COMP-3.         
031700         05  W-Q4SEQA-IDARTNR-MIN PIC S9(9)    VALUE ZERO COMP-3.         
031800         05  FILLER               PIC X(11)    VALUE LOW-VALUE.           
031900                                                                          
032000     03  W-WDQ4A1KY-MAX-X.                                                
032100         05  W-Q4SEQA-IDORDER-MAX PIC S9(7)    VALUE ZERO COMP-3.         
032200         05  W-Q4SEQA-IDARTNR-MAX PIC S9(9)    VALUE ZERO COMP-3.         
032300         05  FILLER               PIC X(11)    VALUE HIGH-VALUE.          
032400                                                                          
032500     03  W-WDQ401KY-MIN-X.                                                
032600         05  W-Q4-IDORDER-MIN     PIC S9(7)  VALUE ZERO COMP-3.           
032700         05  FILLER               PIC X(16)  VALUE SPACE.                 
032800                                                                          
032900     03  W-WDQ401KY-MAX-X.                                                
033000         05  W-Q4-IDORDER-MAX     PIC S9(7)  VALUE ZERO COMP-3.           
033100         05  FILLER               PIC X(16)  VALUE SPACE.                 
033200                                                                          
033300     03  W-KDODELST-P.                                                    
033400         05  W-KDODELSTP          PIC X(1)   VALUE 'P'.                   
033500                                                                          
033600     03  W-KDODELST-U.                                                    
033700         05  W-KDODELSTU          PIC X(1)   VALUE 'U'.                   
033800                                                                          
033900     03  W-IDGMT-X.                                                       
034000         05  W-IDDISTR-WDB2       PIC S9(5)  VALUE ZERO COMP-3.           
034100         05  W-IDKUNDNR-WDB2      PIC S9(7)  VALUE ZERO COMP-3.           
034200                                                                          
034300     SKIP2                                                                
034400*    --- STATUS-KOD FRÅN IMS                                              
034500 01  STATUS-WS                   PIC XX.                                  
034600     88  SEGMENT-FOUND                       VALUE '  '.                  
034700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
034800     88  SEGMENT-FINAL                       VALUE 'GB'.                  
034900     SKIP2                                                                
035000 01  GOOD-STATUSCODES.                                                    
035100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035200     SKIP3                                                                
035300 01  ALL-SSA.                                                             
035400     03 SSA1                     PIC X(128).                              
035500     03 SSA2                     PIC X(64).                               
035600     EJECT                                                                
035700*    --- IMS FUNCTION CODES                                               
035800*01  -COPY W0003                                                          
035900     EJECT                                                                
036000*                                                                         
036100*    --- VALID IDDC CODES                                                 
036200*                                                                         
036300*01    -COPY WWDC99                                                       
036400     EJECT                                                                
036500*    ---  DLI INPUT-OUTPUT AREA                                           
036600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
036700 01  DLI-IO-WDK601.                                                       
036800*    03  -COPY WDK601                                                     
036900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
037000 01  DLI-IO-WDK611.                                                       
037100*    03  -COPY WDK611                                                     
037200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF501'.                      
037300 01  DLI-IO-WDF501.                                                       
037400*    03  -COPY WDF501                                                     
037500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
037600 01  DLI-IO-WDQ201.                                                       
037700*    03  -COPY WDQ201                                                     
037800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ211'.                      
037900 01  DLI-IO-WDQ211.                                                       
038000*    03  -COPY WDQ211                                                     
038100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ212'.                      
038200 01  DLI-IO-WDQ212.                                                       
038300*    03  -COPY WDQ212                                                     
038400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ221'.                      
038500 01  DLI-IO-WDQ221.                                                       
038600*    03  -COPY WDQ221                                                     
039000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ4A1'.                      
039100 01  DLI-IO-WDQ4A.                                                        
039200*    03  -COPY WDQ4A1                                                     
039300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ401'.                      
039400 01  DLI-IO-WDQ401.                                                       
039500*    03  -COPY WDQ401                                                     
039900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
040000 01  DLI-IO-WDB201.                                                       
040100*    03  -COPY WDB201                                                     
040200     EJECT                                                                
040300 LINKAGE SECTION.                                                         
040400*    -COPY W400ORUP                                                       
040500*                                                                         
040600*01  -COPY W0009  -PRE MSG-                                               
040700                                                                          
040800*01  -COPY W0009  -PRE 0693X-                                             
040900                                                                          
041000*01  -COPY W0008  -PRE WDP8-                                              
041100     05  FILLER                  PIC X.                                   
041200*01  -COPY W0008  -PRE USEA-                                              
041300     05  FILLER                  PIC X.                                   
041400*01  -COPY W0008  -PRE WDK6-                                              
041500     05  FILLER                  PIC X.                                   
041600*01  -COPY W0008  -PRE WDB2-                                              
041700     05  FILLER                  PIC X.                                   
041800*01  -COPY W0008  -PRE WDF5-                                              
041900     05  FILLER                  PIC X.                                   
042000*01  -COPY W0008  -PRE WDQ2CSEQ-                                          
042100     05  FILLER                  PIC X.                                   
042400*01  -COPY W0008  -PRE WDQ4A-                                             
042500     05  FILLER                  PIC X.                                   
042510 01  AREG-WDK6-PCB               PIC X.                                   
042520 01  AREG-WDK7-PCB               PIC X.                                   
042530                                                                          
042540 01  SDCA-ARTS-PCB               PIC X.                                   
042550 01  SDCA-WDB6-PCB               PIC X.                                   
042560 01  SDCA-WDK9-PCB               PIC X.                                   
042570 01  SDCA-WDR6-PCB               PIC X.                                   
042580 01  SDCA-WDK6-PCB               PIC X.                                   
042590 01  SDCA-WDQ4B-PCB              PIC X.                                   
042591 01  SDCA-WDQ2-PCB               PIC X.                                   
042592 01  SDCA-WDQ4-PCB               PIC X.                                   
042593 01  SDCA-WDB6-2-PCB             PIC X.                                   
042594 01  SDCA-WDK6-2-PCB             PIC X.                                   
042595 01  SDCA-WDK7-2-PCB             PIC X.                                   
042596 01  SDCA-WDK7-3-PCB             PIC X.                                   
042597                                                                          
042598 01  KVAN-WDB2-PCB               PIC X.                                   
042599 01  KVAN-WDC1-PCB               PIC X.                                   
042600     EJECT                                                                
042700 PROCEDURE DIVISION  USING ORUP-W400ORUP                                  
042800                           MSG-PCB                                        
042900                           0693X-PCB                                      
043000                           WDP8-PCB                                       
043100                           USEA-PCB                                       
043200                           WDK6-PCB                                       
043300                           WDB2-PCB                                       
043400                           WDF5-PCB                                       
043500                           WDQ2CSEQ-PCB                                   
043700                           WDQ4A-PCB                                      
043710                           AREG-WDK6-PCB AREG-WDK7-PCB                    
043720                           SDCA-ARTS-PCB SDCA-WDB6-PCB                    
043730                           SDCA-WDK9-PCB                                  
043740                           SDCA-WDR6-PCB SDCA-WDK6-PCB                    
043750                           SDCA-WDQ4B-PCB                                 
043760                           SDCA-WDQ2-PCB SDCA-WDQ4-PCB                    
043770                           SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB                
043780                           SDCA-WDK7-2-PCB                                
043790                           SDCA-WDK7-3-PCB                                
043791                           KVAN-WDB2-PCB KVAN-WDC1-PCB.                   
043800 MAIN SECTION.                                                            
043900                                                                          
044000     PERFORM A-INIT                                                       
044100                                                                          
044200     PERFORM B-VALIDATE-ORDERHEAD                                         
044300                                                                          
044400     IF EVERYTHING-OK                                                     
044500       EVALUATE ORUP-KDCALL                                               
044600         WHEN 001                                                         
044700           PERFORM C-VALIDATE-ORDERLINES                                  
044800           IF EVERYTHING-OK                                               
044900              PERFORM D-TRIGGER-4258-TRANS                                
045000           END-IF                                                         
045100         WHEN OTHER                                                       
045200            MOVE NOO              TO OK-SW                                
045300            MOVE '023'            TO ORUP-IDMSG-ERROR                     
045400            MOVE 'KDCALL'         TO ORUP-IDELMT-ERROR                    
045500            MOVE 'CALL OPTION INVALID'                                    
045600                                  TO ORUP-FEL-TEXT                        
045700       END-EVALUATE                                                       
045800     END-IF                                                               
045900                                                                          
046000     PERFORM Z-FINIT                                                      
046100                                                                          
046200     MOVE ZERO TO RETURN-CODE                                             
046300     GOBACK                                                               
046400     .                                                                    
046500     EJECT                                                                
046600 A-INIT SECTION.                                                          
046700                                                                          
046800     MOVE 'A-INIT'               TO WS-CURRENT-SECTION                    
046900                                                                          
047000     MOVE ALL SPACES             TO ORUP-OUTPUT-DATA                      
047100     MOVE JA                     TO OK-SW                                 
047200*                                                                         
047300     ACCEPT DAGENS-DATUM         FROM DATE                                
047400                                                                          
047500     MOVE ORUP-IDSYSTEM          TO WS-IDSYSTEM                           
047600                                                                          
047700     PERFORM AA-FIXED-KOM-INITIALIZE                                      
049300     .                                                                    
049400     EJECT                                                                
049410                                                                          
049500 AA-FIXED-KOM-INITIALIZE SECTION.                                         
049600                                                                          
049700     MOVE 'AA-FIXED-K'               TO WS-CURRENT-SECTION                
049701                                                                          
049702*    -- INITIALIZE W006KOM AREAS WITH FIXED VALUES                        
049703*    -- FIELDS WITH VARYING CONTENT ARE SET LATER                         
049704     MOVE SPACE                      TO MSG-KOM-WMSGKOM                   
049705     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
049706     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
049707     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
049708     MOVE 'W9031600'                 TO MSG-KOM-IDSNDJOB                  
049709     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
049710*    -- TIKLOCK WILL BE INCREMENTENTED FOR EACH part                      
049711*    -- THIS IS THE START VALUE                                           
049712     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
049713                                                                          
049714*    -- INITIALIZE TARGET TRANSACTION AREA WITH FIXED VALUES              
049715     MOVE LOW-VALUE                  TO MSG-KDZ1                          
049716     MOVE LOW-VALUE                  TO MSG-KDZ2                          
049717     .                                                                    
049720     EJECT                                                                
049721                                                                          
049722***************************************************************           
049723***  VALIDATE ORDER ID - ORDERREF(DISTR, CUSTOMER, ORDER, DATE)           
049724***************************************************************           
049730 B-VALIDATE-ORDERHEAD SECTION.                                            
049740                                                                          
049750     MOVE 'B-VALIDATE'                TO WS-CURRENT-SECTION               
049760                                                                          
050300     INSPECT ORUP-IDDISTR REPLACING LEADING SPACE BY ZERO                 
050400     IF ORUP-IDDISTR NOT NUMERIC                                          
050500        MOVE '024'                    TO ORUP-IDMSG-ERROR                 
050600        MOVE 'IDDISTR'                TO ORUP-IDELMT-ERROR                
050700        MOVE 'DIST NOT NUMERIC'       TO ORUP-FEL-TEXT                    
050900        MOVE NOO                      TO OK-SW                            
051000     ELSE                                                                 
051100        MOVE ORUP-IDDISTR             TO WS-IDDISTR                       
051200     END-IF                                                               
051400                                                                          
051500     IF EVERYTHING-OK                                                     
051600        INSPECT ORUP-IDKUNDNR REPLACING LEADING SPACE BY ZERO             
051700        IF ORUP-IDKUNDNR NOT NUMERIC                                      
051800           MOVE '024'                 TO ORUP-IDMSG-ERROR                 
051900           MOVE 'IDKUNDNR'            TO ORUP-IDELMT-ERROR                
052000           MOVE 'CUST NO MUST BE NUMERIC'                                 
052100                                      TO ORUP-FEL-TEXT                    
052200           MOVE NOO                   TO OK-SW                            
052300        ELSE                                                              
052400           MOVE ORUP-IDKUNDNR         TO WS-IDKUNDNR                      
052500        END-IF                                                            
052600     END-IF                                                               
052700                                                                          
052800     IF EVERYTHING-OK                                                     
052900        MOVE ZERO                     TO WS-IDORDNR7                      
053000        INSPECT ORUP-IDORDNR7 REPLACING LEADING SPACE BY ZERO             
053100        IF ORUP-IDORDNR7 NUMERIC   AND                                    
053200           ORUP-IDORDNR7         > ZERO                                   
053300           MOVE ORUP-IDORDNR7         TO WS-IDORDNR7                      
053400        ELSE                                                              
053500           MOVE '024'                 TO ORUP-IDMSG-ERROR                 
053600           MOVE 'IDORDR7'             TO ORUP-IDELMT-ERROR                
053700           MOVE 'ORDER NO NOT NUMERIC'                                    
053800                                      TO ORUP-FEL-TEXT                    
053900           MOVE NOO                   TO OK-SW                            
054000        END-IF                                                            
054100     END-IF                                                               
054200                                                                          
054300     IF EVERYTHING-OK                                                     
054400        INSPECT ORUP-TIREGDAT REPLACING LEADING SPACE BY ZERO             
054500        IF ORUP-TIREGDAT NOT NUMERIC                                      
054600           MOVE '024'                 TO ORUP-IDMSG-ERROR                 
054700           MOVE 'TIREGDAT'            TO ORUP-IDELMT-ERROR                
054800           MOVE 'DATE NOT NUMERIC' TO ORUP-FEL-TEXT                       
055000           MOVE NOO                   TO OK-SW                            
055100        ELSE                                                              
055200           MOVE 'AAMMDD'              TO DAT-KDDATFORM                    
055300           MOVE ORUP-TIREGDAT         TO DAT-I-TIDATUM                    
055400                                                                          
055500           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
055600                               DAT-O-TIDATUM DAT-KDSVAR                   
055700                                                                          
055800           IF DAT-KDSVAR-OK                                               
055900              MOVE ORUP-TIREGDAT      TO WS-TIREGDAT                      
056000           ELSE                                                           
056100              MOVE '023'              TO ORUP-IDMSG-ERROR                 
056200              MOVE 'REGDAT'           TO ORUP-IDELMT-ERROR                
056300              MOVE 'DATE INVALID'     TO ORUP-FEL-TEXT                    
056500              MOVE NOO                TO OK-SW                            
056600           END-IF                                                         
056700        END-IF                                                            
056800     END-IF                                                               
056900                                                                          
057000     IF EVERYTHING-OK                                                     
057020        PERFORM BA-CHECK-DISTRICT-CUST                                    
057030     END-IF                                                               
057031                                                                          
057040     IF EVERYTHING-OK                                                     
057050        PERFORM BB-VALIDATE-ORDERTYPE                                     
057060     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
057910 BA-CHECK-DISTRICT-CUST SECTION.                                          
057920                                                                          
057930     MOVE 'BA-CHECK-DISTRICT-CUST'                                        
057940                                  TO WS-CURRENT-SECTION                   
057950                                                                          
057960     MOVE ORUP-IDDISTR            TO W-IDDISTR-WDB2                       
057970                                     TEST-IDDISTR                         
057980                                     W-WDQ2C-IDDISTR                      
057990     MOVE ORUP-IDKUNDNR           TO W-IDKUNDNR-WDB2                      
057991                                     W-WDQ2C-IDKUNDNR                     
057992                                                                          
057993     PERFORM IMS-GU-WDB201                                                
057994                                                                          
057995     IF SEGMENT-MISSING                                                   
057996        MOVE NOO                  TO OK-SW                                
057997        MOVE '022'                TO ORUP-IDMSG-ERROR                     
057999        MOVE 'IDDISTR'            TO ORUP-IDELMT-ERROR                    
058000        MOVE 'FORMAL ERROR DISTRICT NO'                                   
058001                                  TO ORUP-FEL-TEXT                        
058002     ELSE                                                                 
058003        PERFORM BAA-VALIDATE-CUST                                         
058004                                                                          
058005        IF EVERYTHING-OK                                                  
058006           PERFORM BAB-CHECK-REFILL                                       
058007        END-IF                                                            
058008                                                                          
058009        IF EVERYTHING-OK                                                  
058010           PERFORM BAC-GET-LOCAL-TIME                                     
058011        END-IF                                                            
058012                                                                          
058013        IF IDSYSTEM-LYNK                                                  
058014           IF GMT-KDKUNDKAT NOT = '03'                                    
058016              MOVE NOO            TO OK-SW                                
058017              MOVE '022'          TO ORUP-IDMSG-ERROR                     
058018              MOVE 'IDDISTR'      TO ORUP-IDELMT-ERROR                    
058019              MOVE 'FORMAL ERROR DISTRICT NO'                             
058020                                  TO ORUP-FEL-TEXT                        
058021           END-IF                                                         
058022        END-IF                                                            
058023        IF IDSYSTEM-POLE                                                  
058024           IF GMT-KDKUNDKAT NOT = '02'                                    
058025              MOVE NOO            TO OK-SW                                
058026              MOVE '022'          TO ORUP-IDMSG-ERROR                     
058027              MOVE 'IDDISTR'      TO ORUP-IDELMT-ERROR                    
058028              MOVE 'FORMAL ERROR DISTRICT NO'                             
058029                                  TO ORUP-FEL-TEXT                        
058030           END-IF                                                         
058031        END-IF                                                            
058032     END-IF                                                               
058033     .                                                                    
058034     EJECT                                                                
058035 BAA-VALIDATE-CUST  SECTION.                                              
058036                                                                          
058037     MOVE 'BAA-VALIDATE-CUST'   TO WS-CURRENT-SECTION                     
058038                                                                          
058039     MOVE DAGENS-DATUM           TO TMP1-YYMMDD                           
058040     MOVE GMT-TISTADAT           TO TMP2-YYMMDD                           
058041     MOVE GMT-TISTODAT           TO TMP3-YYMMDD                           
058042     PERFORM WY2000Q1                                                     
058043     IF (TMP1-YYMMDD < TMP3-YYMMDD OR TMP3-YYMMDD = +0)                   
058044     AND                                                                  
058045      ((TMP1-YYMMDD NOT < TMP2-YYMMDD) AND TMP2-YYMMDD > +0)              
058046        CONTINUE                                                          
058047     ELSE                                                                 
058048        MOVE NOO                  TO OK-SW                                
058049        MOVE '022'                TO ORUP-IDMSG-ERROR                     
058050        MOVE 'IDKUNDKR'           TO ORUP-IDELMT-ERROR                    
058051        MOVE 'CUSTOMER INFO MISSING'                                      
058052                                  TO ORUP-FEL-TEXT                        
058053     END-IF                                                               
058054     .                                                                    
058055     EJECT                                                                
058056 BAB-CHECK-REFILL  SECTION.                                               
058057                                                                          
058058     MOVE 'BAB-CHECK-REFILL'     TO WS-CURRENT-SECTION                    
058059                                                                          
058060     IF DIST35-REFILL                                                     
058061     OR DIST35-REFILL-INOM-NDC                                            
058062     OR DIST35-NONVCC-REFILL                                              
058063        MOVE NOO                  TO OK-SW                                
058064        MOVE '120'                TO ORUP-IDMSG-ERROR                     
058065        MOVE 'IDDISTR'            TO ORUP-IDELMT-ERROR                    
058066        MOVE 'REFILL BLOCKED'     TO ORUP-FEL-TEXT                        
058067     END-IF                                                               
058068     .                                                                    
058069     EJECT                                                                
058070 BAC-GET-LOCAL-TIME   SECTION.                                            
058071                                                                          
058072     MOVE 'BAC-GET-LOCAL-TIME'  TO WS-CURRENT-SECTION                     
058073                                                                          
058074     MOVE ALL '+'                TO MSGI-WMSGINIT                         
058075     MOVE '013'                  TO MSGI-KDCALL                           
058076     MOVE 'WIDDC   '             TO MSGI-IDUSER                           
058077     MOVE GMT-IDDC-BULK(1)       TO MSGI-IDUSER(6:2)                      
058078     MOVE '4258'                 TO MSGI-IDTRANS                          
058079     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
058080                                                                          
058081     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
058082                                                                          
058083     MOVE MSGI-TILOKDAT          TO WS-LOCAL-DATE-AAMMDD                  
058084     .                                                                    
058085     EJECT                                                                
058086                                                                          
058087 BB-VALIDATE-ORDERTYPE  SECTION.                                          
058088                                                                          
058089     MOVE 'BB-VALIDATE-ORDERTYPE'                                         
058090                                 TO WS-CURRENT-SECTION                    
058091                                                                          
058092     MOVE WS-IDORDNR7            TO W-WDQ2C-IDKUNDRF                      
058093                                                                          
058094     IF EVERYTHING-OK                                                     
058095        PERFORM IMS-GU-WDQ201-CSEQ                                        
058096        IF SEGMENT-FOUND                                                  
058097           IF EVERYTHING-OK                                               
058098              PERFORM BBA-POPULATE-DC-CLEAR                               
058099           END-IF                                                         
058100           IF EVERYTHING-OK                                               
058101              PERFORM BBB-CHECK-ORDER-VALIDITY                            
058102           END-IF                                                         
058103           IF EVERYTHING-OK                                               
058104              PERFORM BBC-CHECK-PREPLANNED                                
058105           END-IF                                                         
058106        ELSE                                                              
058107           MOVE NOO               TO OK-SW                                
058108           MOVE '041'             TO ORUP-IDMSG-ERROR                     
058109           MOVE 'ORDER'           TO ORUP-IDELMT-ERROR                    
058110           MOVE 'ORDER MISSING'   TO ORUP-FEL-TEXT                        
058111        END-IF                                                            
058112     END-IF                                                               
058113     .                                                                    
058114     EJECT                                                                
058115                                                                          
058116 BBA-POPULATE-DC-CLEAR SECTION.                                           
058117                                                                          
058118     MOVE 'BBA-POPULATE-DC-CLEAR'                                         
058119                                 TO WS-CURRENT-SECTION                    
058120     IF OHUV-KDORDKL > 1                                                  
058121        MOVE +1 TO WS-INDEX                                               
058122        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
058123           MOVE GMT-IDDC-BULK(WS-INDEX)                                   
058124                                 TO W-GMT-IDDC-CLEAR  (WS-INDEX)          
058125           ADD +1                TO WS-INDEX                              
058126        END-PERFORM                                                       
058127     ELSE                                                                 
058128       IF OHUV-KDORDKL = 1                                                
058130          MOVE +1 TO WS-INDEX                                             
058131          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
058132             MOVE GMT-IDDC-DAY(WS-INDEX)                                  
058133                                 TO W-GMT-IDDC-CLEAR  (WS-INDEX)          
058134             ADD +1              TO WS-INDEX                              
058135          END-PERFORM                                                     
058137       ELSE                                                               
058138         IF OHUV-KDORDKL = 0                                              
058140            MOVE +1 TO WS-INDEX                                           
058141            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
058142               MOVE GMT-IDDC-VOR(WS-INDEX)                                
058143                                 TO W-GMT-IDDC-CLEAR  (WS-INDEX)          
058144               ADD +1            TO WS-INDEX                              
058145            END-PERFORM                                                   
058146         END-IF                                                           
058147       END-IF                                                             
058148     END-IF                                                               
058149     .                                                                    
058150     EJECT                                                                
058151                                                                          
058152 BBB-CHECK-ORDER-VALIDITY   SECTION.                                      
058153                                                                          
058154     MOVE 'BBB-CHECK-ORDER-VALIDITY'                                      
058155                                   TO WS-CURRENT-SECTION                  
058156                                                                          
058157     IF (OHUV-KDORDKL = 1 AND                                             
058158         GMT-FLORDTIL-KL1 = JA)                                           
058159     OR                                                                   
058160        (OHUV-KDORDKL = 2 AND                                             
058161         GMT-FLORDTIL-KL2 = JA)                                           
058162     OR                                                                   
058163        (OHUV-KDORDKL = 3 AND                                             
058164         GMT-FLORDTIL-KL3 = JA)                                           
058165     OR                                                                   
058166        (OHUV-KDORDKL = 4 AND                                             
058167         GMT-FLORDTIL-KL4 = JA)                                           
058168         MOVE NOO                  TO OK-SW                               
058169         MOVE '022'                TO ORUP-IDMSG-ERROR                    
058170         MOVE 'DISTR/CUST'         TO ORUP-IDELMT-ERROR                   
058171         MOVE 'DIST/CUST BLOCKED FOR ADD/UPDATE'                          
058172                                   TO ORUP-FEL-TEXT                       
058173     END-IF                                                               
058174                                                                          
058175     IF EVERYTHING-OK                                                     
058176        IF OHUV-FLBORT = NOO                                              
058177          IF OHUV-FLFORBI = JA         OR                                 
058178             OHUV-FLFORBI = SPEC-FORBI OR                                 
058179             OHUV-FLORDSPE = JA        OR                                 
058180             OHUV-KDTPOTYP > 0         OR OHUV-IDKAMPRF > 0  OR           
058181             OHUV-FLOVRLEV = JA        OR OHUV-KDFAKTYP = 'G'             
058182             MOVE NOO              TO OK-SW                               
058183             MOVE '022'            TO ORUP-IDMSG-ERROR                    
058184             MOVE 'ORDERTYP'       TO ORUP-IDELMT-ERROR                   
058185             MOVE 'ORDER TYPE INVALID'                                    
058186                                   TO ORUP-FEL-TEXT                       
058187          END-IF                                                          
058188        ELSE                                                              
058189           MOVE NOO                TO OK-SW                               
058190           MOVE '022'              TO ORUP-IDMSG-ERROR                    
058191           MOVE 'ORDER'            TO ORUP-IDELMT-ERROR                   
058192           MOVE 'ORDER CANCELLED'  TO ORUP-FEL-TEXT                       
058193        END-IF                                                            
058194     END-IF                                                               
058195                                                                          
058196     IF EVERYTHING-OK                                                     
058197        MOVE NOO                   TO SW-SOFTWARE-ORDER                   
058198        PERFORM IMS-GNP-WDQ2-WDQ211-FIRST                                 
058199                                                                          
058200        PERFORM UNTIL SEGMENT-MISSING OR                                  
058201                      SOFTWARE-ORDER                                      
058202          IF DIRL-IDDC = '11' AND DIRL-IDLEVNR = '1441'                   
058203              MOVE JA              TO SW-SOFTWARE-ORDER                   
058204          END-IF                                                          
058205          PERFORM IMS-GNP-WDQ2-WDQ211                                     
058206        END-PERFORM                                                       
058207                                                                          
058208        IF SOFTWARE-ORDER                                                 
058209           MOVE NOO                TO OK-SW                               
058210           MOVE '022'              TO ORUP-IDMSG-ERROR                    
058211           MOVE 'ORDER'            TO ORUP-IDELMT-ERROR                   
058212           MOVE 'SOFTWARE ORDER INVALID'                                  
058213                                   TO ORUP-FEL-TEXT                       
058214        END-IF                                                            
058215     END-IF                                                               
058216                                                                          
058217     IF EVERYTHING-OK                                                     
058218        PERFORM IMS-GNP-WDQ212                                            
058219        IF SEGMENT-FOUND                                                  
058220           MOVE ARB-TIRFS        TO WS-TIRFS                              
058221           MOVE ARB-KDORDSTA-O   TO WS-KDORDSTA-O                         
058222           MOVE JA               TO SW-KDORDSTA-O-ALL-SPACE-FLAG          
058223           PERFORM UNTIL SEGMENT-MISSING                                  
058224              IF ARB-KDORDSTA-O NOT = SPACE                               
058225                 MOVE NOO        TO SW-KDORDSTA-O-ALL-SPACE-FLAG          
058226              END-IF                                                      
058227              IF ARB-KDORDSTA  NOT = SPACE                                
058228                IF (ARB-KDORDSTA     = 'E' OR 'B' OR 'C' OR 'R')          
058229                AND (ARB-KDORDSTA-O  = ' ' OR 'B' OR 'C' OR 'R')          
058230                  CONTINUE                                                
058231                ELSE                                                      
058232                  MOVE NOO                TO OK-SW                        
058233                  MOVE '022'              TO ORUP-IDMSG-ERROR             
058234                  MOVE 'ORDER STATUS'     TO ORUP-IDELMT-ERROR            
058235                  MOVE 'ORDER NOT IN R STATUS'                            
058236                                          TO ORUP-FEL-TEXT                
058237                END-IF                                                    
058238              END-IF                                                      
058239              PERFORM IMS-GNP-WDQ212                                      
058240           END-PERFORM                                                    
058241        ELSE                                                              
058242          MOVE NOO                  TO OK-SW                              
058243          MOVE '022'                TO ORUP-IDMSG-ERROR                   
058244          MOVE 'DB ERROR'           TO ORUP-IDELMT-ERROR                  
058245          MOVE 'MISSING IN DATABASE'                                      
058246                                    TO ORUP-FEL-TEXT                      
058247        END-IF                                                            
058248     END-IF                                                               
058249     .                                                                    
058250     EJECT                                                                
058277 BBC-CHECK-PREPLANNED SECTION.                                            
058278                                                                          
058279     MOVE 'BBC-CHECK-PREPLANNED' TO WS-CURRENT-SECTION                    
058280                                                                          
058281     MOVE NOO                     TO PREPLANED-SW                         
058282                                                                          
058283     IF OHUV-TIREPDAT > ZERO                                              
058284         MOVE JA                  TO PREPLANED-SW                         
058285     END-IF                                                               
058286                                                                          
058287     IF PREPLANED                                                         
058288        MOVE WC-CDC-SE            TO WORK-IDDC                            
058289        MOVE +002                 TO WORK-KDCALL                          
058290        MOVE GMT-KVDAGAR-CDC      TO WORK-KVWORKD                         
058291        MOVE WS-LOCAL-DATE-AAMMDD                                         
058292                                  TO WORK-TIAAMMDD-FOM                    
058293        CALL WORKDAY           USING WORK-KDCALL                          
058294                                     WORK-DATE-AREA                       
058295                                     WORK-KDSVAR                          
058297        IF WORK-KDSVAR-FEL                                                
058298           MOVE NOO               TO OK-SW                                
058299           MOVE '022'             TO ORUP-IDMSG-ERROR                     
058300           MOVE 'DATE'            TO ORUP-IDELMT-ERROR                    
058301           MOVE 'DATE INVALID FROM WORKDAY'                               
058302                                  TO ORUP-FEL-TEXT                        
058303        ELSE                                                              
058304           IF WS-TIRFS-DAT  <= WORK-TIAAMMDD-NEXT-WORKDAY                 
058305              MOVE NOO            TO OK-SW                                
058306              MOVE '022'          TO ORUP-IDMSG-ERROR                     
058307              MOVE 'DATE'         TO ORUP-IDELMT-ERROR                    
058308              MOVE 'RFS DATE NEAR'                                        
058309                                  TO ORUP-FEL-TEXT                        
058310           END-IF                                                         
058311        END-IF                                                            
058312     END-IF                                                               
058313     .                                                                    
058314     EJECT                                                                
058315                                                                          
058316 C-VALIDATE-ORDERLINES SECTION.                                           
058317                                                                          
058320     MOVE 'C-VALIDATE-ORDERLINES'                                         
058400                                      TO WS-CURRENT-SECTION               
058530     IF EVERYTHING-OK AND ORUP-KVRADER  > 999                             
058540        MOVE NOO              TO OK-SW                                    
058550        MOVE '028'            TO ORUP-IDMSG-ERROR                         
058560        MOVE 'KVRADER '       TO ORUP-IDELMT-ERROR                        
058570        MOVE 'TOO MANY LINES' TO ORUP-FEL-TEXT                            
058580     END-IF                                                               
059200                                                                          
059300     IF EVERYTHING-OK                                                     
059310        INITIALIZE WS-UPD-REQUEST                                         
059320                   WS-ADD-REQUEST                                         
059330        MOVE ZERO                        TO WS-KVRADER-IX-NUM             
059340        MOVE SPACES                      TO WS-KVRADER-IX-DISP            
059350        MOVE +1                          TO UPD-INDX                      
059360                                            ADD-INDX                      
059600                                                                          
059700        IF ORUP-KVRADER NOT      > ZERO                                   
059800           MOVE NOO                   TO OK-SW                            
059900           MOVE '023'                 TO ORUP-IDMSG-ERROR                 
060000           MOVE 'KVRADER'             TO ORUP-IDELMT-ERROR                
060100           MOVE 'INVALID NO OF LINES' TO ORUP-FEL-TEXT                    
060200        ELSE                                                              
060300***        CHECK ORDER LINE INPUTS                                        
060500           IF ORUP-KVRADER >= 1                                           
060510              INITIALIZE WS-PART-DETAILS                                  
060600              MOVE  1                  TO KVRADER-IX                      
060700              MOVE  ORUP-KVRADER       TO KVRADER-IX-MAX                  
060800                                                                          
060900              PERFORM UNTIL KVRADER-IX  > KVRADER-IX-MAX                  
061000                         OR SOMETHING-WRONG                               
061200                MOVE KVRADER-IX        TO WS-KVRADER-IX-NUM               
061300                MOVE WS-KVRADER-IX-NUM TO WS-KVRADER-IX-DISP              
061401                                                                          
061410                IF EVERYTHING-OK                                          
061500                   PERFORM CA-VALIDATE-ACTIONCODE-PARTNUM                 
061600                END-IF                                                    
061610                                                                          
061700                IF EVERYTHING-OK                                          
061800                   PERFORM CB-VALIDATE-QTY-PRICE                          
061900                END-IF                                                    
062000                                                                          
062100                IF EVERYTHING-OK                                          
062200                   PERFORM CC-ADDITIONAL-VALIDATION                       
062300                END-IF                                                    
062400                                                                          
062500                ADD 1                  TO KVRADER-IX                      
062600              END-PERFORM                                                 
062615              IF EVERYTHING-OK                                            
062620                 PERFORM CD-CHECK-SAME-PART-ADD-UPD                       
062630              END-IF                                                      
062700           END-IF                                                         
062800        END-IF                                                            
062900     END-IF                                                               
063000     .                                                                    
063100     EJECT                                                                
063200 CA-VALIDATE-ACTIONCODE-PARTNUM SECTION.                                  
063300                                                                          
063400     MOVE 'CA-VALIDATE-ACTIONC'       TO WS-CURRENT-SECTION               
063401                                                                          
063404     IF ORUP-KDBEHX      (KVRADER-IX)  > SPACES                           
063405        MOVE ORUP-KDBEHX (KVRADER-IX) TO WS-UPD-FUNC                      
063406        IF UPD-ORD-LINE OR                                                
063407           ADD-ORD-LINE                                                   
063408           CONTINUE                                                       
063409        ELSE                                                              
063410           MOVE NOO                   TO OK-SW                            
063420           MOVE '124'                 TO ORUP-IDMSG-ERROR                 
063421           MOVE 'ACTION CODE'         TO ORUP-IDELMT-ERROR                
063422           STRING 'INVALID ACTION CODE '                                  
063423                                      DELIMITED BY SIZE                   
063424           'ON LINE '                 DELIMITED BY SIZE                   
063425           WS-KVRADER-IX-DISP         DELIMITED BY SPACES                 
063426                                    INTO ORUP-FEL-TEXT                    
063427        END-IF                                                            
063428     ELSE                                                                 
063429        MOVE NOO                      TO OK-SW                            
063430        MOVE '124'                    TO ORUP-IDMSG-ERROR                 
063431        MOVE 'ACTION CODE'            TO ORUP-IDELMT-ERROR                
063432        STRING 'NO ACTION CODE ON LINE '                                  
063433                                      DELIMITED BY SIZE                   
063434        WS-KVRADER-IX-DISP            DELIMITED BY SPACES                 
063435                                    INTO ORUP-FEL-TEXT                    
063436     END-IF                                                               
063437     IF EVERYTHING-OK                                                     
063438        IF IDSYSTEM-LYNK                                                  
063439           MOVE ORUP-IDLEVART (KVRADER-IX) TO W-SEQB-IDLEVART             
063440           PERFORM IMS-GU-WDF501-BSEQ                                     
063441           IF SEGMENT-MISSING                                             
063442              MOVE NOO                  TO OK-SW                          
063443              MOVE '025'                TO ORUP-IDMSG-ERROR               
063444              MOVE 'IDLEVART'           TO ORUP-IDELMT-ERROR              
063445              STRING 'PART '          DELIMITED BY SIZE                   
063446              W-SEQB-IDLEVART              DELIMITED BY SPACES            
063447              ' INVALID '                  DELIMITED BY SIZE              
063448              'ON LINE '                   DELIMITED BY SIZE              
063449              WS-KVRADER-IX-DISP           DELIMITED BY SPACES            
063450                                      INTO ORUP-FEL-TEXT                  
063451           ELSE                                                           
063452              MOVE XART-IDARTNR         TO W-IDARTNR                      
063453              MOVE W-IDARTNR            TO W-IDARTNR-NUM                  
063454           END-IF                                                         
063455        ELSE                                                              
063456           MOVE ZERO                  TO W-BLANKS                         
063457           INSPECT FUNCTION REVERSE (ORUP-IDLEVART (KVRADER-IX))          
063458                   TALLYING W-BLANKS FOR LEADING SPACES                   
063459           COMPUTE W-LENGTH = 30 - W-BLANKS                               
063460           IF ORUP-IDLEVART(KVRADER-IX) (1:W-LENGTH) NUMERIC              
063461              IF (W-LENGTH < 9 AND W-LENGTH > 0 )                         
063462                 MOVE ORUP-IDLEVART  (KVRADER-IX) (1:W-LENGTH)            
063463                                      TO W-IDARTNR                        
063464                                         W-IDARTNR-CHAR                   
063465                                                                          
063466              END-IF                                                      
063467           ELSE                                                           
063468              MOVE NOO                  TO OK-SW                          
063469              MOVE '025'                TO ORUP-IDMSG-ERROR               
063470              MOVE 'IDLEVART'           TO ORUP-IDELMT-ERROR              
063471              STRING 'PART '               DELIMITED BY SIZE              
063472              ORUP-IDLEVART  (KVRADER-IX) (1:W-LENGTH)                    
063473                                           DELIMITED BY SPACES            
063474              ' INVALID '                  DELIMITED BY SIZE              
063475              'ON LINE '                   DELIMITED BY SIZE              
063476              WS-KVRADER-IX-DISP           DELIMITED BY SPACES            
063477                                      INTO ORUP-FEL-TEXT                  
063478           END-IF                                                         
063479        END-IF                                                            
063480     END-IF                                                               
063481                                                                          
063482     IF EVERYTHING-OK                                                     
063483        PERFORM CAA-CHECK-PARTNUM                                         
063484     END-IF                                                               
063485     .                                                                    
063486     EJECT                                                                
063487 CAA-CHECK-PARTNUM  SECTION.                                              
063488                                                                          
063489     MOVE 'CAA-CHECK-PARTNUM'         TO WS-CURRENT-SECTION               
063490                                                                          
063491     PERFORM IMS-GU-WDK601                                                
063492     IF SEGMENT-MISSING                                                   
063493        MOVE NOO                   TO OK-SW                               
063494        MOVE '022'                 TO ORUP-IDMSG-ERROR                    
063495        MOVE 'IDARTNR'             TO ORUP-IDELMT-ERROR                   
063496        STRING 'PART '             DELIMITED BY SIZE                      
063497         W-IDARTNR-CHAR            DELIMITED BY SPACES                    
063498           ' NOT REGISTERED'       DELIMITED BY SIZE                      
063499              ' ON LINE '          DELIMITED BY SIZE                      
063500              WS-KVRADER-IX-DISP   DELIMITED BY SPACES                    
063501                                   INTO ORUP-FEL-TEXT                     
063502     ELSE                                                                 
063503        MOVE ORUP-IDLEVART (KVRADER-IX)                                   
063504                              TO WS-INP-IDLEVART(KVRADER-IX)              
063505        MOVE ART-IDARTNR      TO WS-INP-IDARTNR (KVRADER-IX)              
063506        MOVE ART-REKSIFFR     TO WS-INP-REKSIFFR(KVRADER-IX)              
063507        IF ART-KDSORT = 'SW'                                              
063508           MOVE NOO                   TO OK-SW                            
063509           MOVE '022'                 TO ORUP-IDMSG-ERROR                 
063510           MOVE 'KDSORT'              TO ORUP-IDELMT-ERROR                
063512           STRING 'SW PART '          DELIMITED BY SIZE                   
063513            W-IDARTNR-CHAR            DELIMITED BY SPACES                 
063514              ' NOT ALLOWED ON LINE ' DELIMITED BY SIZE                   
063515            WS-KVRADER-IX-DISP        DELIMITED BY SPACES                 
063516                                      INTO ORUP-FEL-TEXT                  
063517        END-IF                                                            
063518        IF EVERYTHING-OK                                                  
063519          PERFORM IMS-GNP-WDK611                                          
063520          IF SEGMENT-MISSING                                              
063521             MOVE NOO                TO OK-SW                             
063522             MOVE '022'              TO ORUP-IDMSG-ERROR                  
063523             MOVE 'PRARTSTD'         TO ORUP-IDELMT-ERROR                 
063524             MOVE 'STD PRICE MISSING'                                     
063525                                     TO ORUP-FEL-TEXT                     
063526             STRING 'STD PRICE FOR '    DELIMITED BY SIZE                 
063527              W-IDARTNR-CHAR            DELIMITED BY SPACES               
063528                ' MISSING ON LINE '     DELIMITED BY SIZE                 
063529                       WS-KVRADER-IX-DISP     DELIMITED BY SPACES         
063530                                        INTO ORUP-FEL-TEXT                
063531          ELSE                                                            
063532             IF CLAG-REDIRLEV > 0                                         
063533                MOVE NOO             TO OK-SW                             
063534                MOVE '022'           TO ORUP-IDMSG-ERROR                  
063535                MOVE 'DDGS'          TO ORUP-IDELMT-ERROR                 
063536                STRING 'DDGS PART '        DELIMITED BY SIZE              
063537                 W-IDARTNR-CHAR            DELIMITED BY SPACES            
063538                   ' NOT ALLOWED ON LINE ' DELIMITED BY SIZE              
063539                       WS-KVRADER-IX-DISP     DELIMITED BY SPACES         
063540                                           INTO ORUP-FEL-TEXT             
063541             END-IF                                                       
063542             IF EVERYTHING-OK                                             
063543                IF CLAG-KDERS >= 01 AND UPD-ORD-LINE                      
063544                   MOVE NOO          TO OK-SW                             
063545                   MOVE '022'        TO ORUP-IDMSG-ERROR                  
063546                   MOVE 'KDERS'      TO ORUP-IDELMT-ERROR                 
063547                   STRING 'NO UPDATE REQ FOR ' DELIMITED BY SIZE          
063548                     'SUPERSEEDED P/N '        DELIMITED BY SIZE          
063549                       W-IDARTNR-CHAR         DELIMITED BY SPACES         
063550                      ' ON LINE '             DELIMITED BY SIZE           
063551                       WS-KVRADER-IX-DISP     DELIMITED BY SPACES         
063552                                      INTO ORUP-FEL-TEXT                  
063553                END-IF                                                    
063554             END-IF                                                       
063555          END-IF                                                          
063556        END-IF                                                            
063557     END-IF                                                               
063558     .                                                                    
063559     EJECT                                                                
063560                                                                          
063561 CB-VALIDATE-QTY-PRICE SECTION.                                           
063562                                                                          
063563     MOVE 'CB-VALIDATE-QTY-PRI'       TO WS-CURRENT-SECTION               
063564                                                                          
066600     IF IDSYSTEM-ECOM                                                     
066700        IF ORUP-PRARTNTO-LOC (KVRADER-IX) = ZERO                          
066800           MOVE NOO                   TO OK-SW                            
066900           MOVE '041'                 TO ORUP-IDMSG-ERROR                 
067000           MOVE 'PRICE'               TO ORUP-IDELMT-ERROR                
067100           STRING 'PRICE MISSING '    DELIMITED BY SIZE                   
067110            ' FOR PART '              DELIMITED BY SIZE                   
067120            W-IDARTNR-CHAR            DELIMITED BY SPACES                 
067200           ' ON LINE '                DELIMITED BY SIZE                   
067300           WS-KVRADER-IX-DISP         DELIMITED BY SPACES                 
067400                                    INTO ORUP-FEL-TEXT                    
067500        END-IF                                                            
067600     ELSE                                                                 
067900        IF ORUP-PRARTNTO-LOC (KVRADER-IX) NOT = ZERO                      
068000           MOVE NOO                   TO OK-SW                            
068100           MOVE '023'                 TO ORUP-IDMSG-ERROR                 
068200           MOVE 'PRICE'               TO ORUP-IDELMT-ERROR                
068300           STRING 'PRICE INVALID '    DELIMITED BY SIZE                   
068410            ' FOR PART '              DELIMITED BY SIZE                   
068420            W-IDARTNR-CHAR            DELIMITED BY SPACES                 
068430           ' ON LINE '                DELIMITED BY SIZE                   
068500           WS-KVRADER-IX-DISP         DELIMITED BY SPACES                 
068600                                    INTO ORUP-FEL-TEXT                    
068700        END-IF                                                            
068800     END-IF                                                               
068810                                                                          
068820     IF EVERYTHING-OK                                                     
068830        IF ORUP-KVBEART (KVRADER-IX) NOT NUMERIC OR                       
068831           ORUP-KVBEART (KVRADER-IX) = 0                                  
068832           MOVE NOO                   TO OK-SW                            
068833           MOVE '023'                 TO ORUP-IDMSG-ERROR                 
068834           MOVE 'QUANTITY'            TO ORUP-IDELMT-ERROR                
068835           STRING 'QUANTITY INVALID'  DELIMITED BY SIZE                   
068836            ' FOR PART '              DELIMITED BY SIZE                   
068837            W-IDARTNR-CHAR            DELIMITED BY SPACES                 
068838           ' ON LINE '                DELIMITED BY SIZE                   
068839           WS-KVRADER-IX-DISP         DELIMITED BY SPACES                 
068840                                    INTO ORUP-FEL-TEXT                    
068841        ELSE                                                              
068842           MOVE ORUP-KVBEART (KVRADER-IX) TO WS-KVBEART                   
068844        END-IF                                                            
068850     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100 CC-ADDITIONAL-VALIDATION SECTION.                                        
069200                                                                          
069300     MOVE 'CC-ADDITIONAL-VALID'  TO WS-CURRENT-SECTION                    
069400                                                                          
069500     IF EVERYTHING-OK AND                                                 
069600        UPD-INDX > 1  AND UPD-ORD-LINE                                    
069700        PERFORM CCA-CHECK-UPD-REQ-DUPLICATES                              
069800     END-IF                                                               
069900                                                                          
070000     IF EVERYTHING-OK                                                     
070100        PERFORM CCB-CHECK-WDQ4                                            
070110     END-IF                                                               
070111     IF EVERYTHING-OK                                                     
070112        PERFORM CCC-CHECK-SDC-STOCK                                       
070113     END-IF                                                               
070120     .                                                                    
070130     EJECT                                                                
115900*****************************************************************         
116000*UPD ORDERLINE VALIDATION:USER CANNOT SEND SAME P/N IN MORE THAN          
116100*1 UPDATE REQUEST                                                         
116200*****************************************************************         
116300 CCA-CHECK-UPD-REQ-DUPLICATES SECTION.                                    
116400                                                                          
116500     MOVE 'CCA-CHECK-UPD-REQ-DUPLICATES'                                  
116600                                  TO WS-CURRENT-SECTION                   
116800     COMPUTE SEARCH-INDX   = UPD-INDX - 1                                 
116801     MOVE NOO                     TO DUP-FND-SW                           
116810                                                                          
116900     PERFORM UNTIL DUP-FND OR SEARCH-INDX < 1                             
117000        IF W-IDARTNR       = WS-UPD-IDARTNR(SEARCH-INDX)                  
117100           MOVE JA                 TO DUP-FND-SW                          
117200        END-IF                                                            
117300        SUBTRACT 1               FROM SEARCH-INDX                         
117400     END-PERFORM                                                          
117500                                                                          
117600     MOVE ZERO                     TO SEARCH-INDX                         
117700                                                                          
117800     IF DUP-FND                                                           
117900        MOVE NOO                   TO OK-SW                               
118000        MOVE '022'                 TO ORUP-IDMSG-ERROR                    
118100        MOVE 'IDARTNR'             TO ORUP-IDELMT-ERROR                   
118231        STRING 'DUPLICATE UPDATE REQUEST FOR '                            
118232                                   DELIMITED BY SIZE                      
118233        W-IDARTNR-CHAR            DELIMITED BY SPACES                     
118234        ' ON LINE '               DELIMITED BY SIZE                       
118235        WS-KVRADER-IX-DISP        DELIMITED BY SPACES                     
118240                                 INTO ORUP-FEL-TEXT                       
118300     END-IF                                                               
118400     .                                                                    
118500     EJECT                                                                
118600                                                                          
118700*****************************************************************         
118800*WITH THE INPUT P/N IN ADD/UPD REQUEST, CHECK AND VALIDATE WITH           
118900*EXISTING ORDERLINES IN Q4                                                
119000*****************************************************************         
119100 CCB-CHECK-WDQ4  SECTION.                                                 
119200                                                                          
119300     MOVE 'CCB-CHECK-WDQ4'        TO WS-CURRENT-SECTION                   
119400                                                                          
119500     MOVE OHUV-IDORDER            TO W-Q4SEQA-IDORDER-MIN                 
119600                                     W-Q4SEQA-IDORDER-MAX                 
119700                                                                          
119800     MOVE W-IDARTNR               TO W-Q4SEQA-IDARTNR-MIN                 
119900                                     W-Q4SEQA-IDARTNR-MAX                 
123610     PERFORM IMS-GU-WDQ4A                                                 
123611                                                                          
123620     IF ADD-ORD-LINE                                                      
123630        ADD W-IDARTNR          TO WS-ADD-IDARTNR(ADD-INDX)                
123650        ADD +1                 TO ADD-INDX                                
123660     ELSE                                                                 
123670        IF SEGMENT-FOUND                                                  
123680          MOVE ZEROES                 TO W-IDARTNR-CNT                    
123681          PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-FINAL OR               
123682                        SOMETHING-WRONG                                   
123683            ADD +1                   TO W-IDARTNR-CNT                     
123684            MOVE SEQA-IDARTNR        TO WS-UPD-IDARTNR(UPD-INDX)          
123685                                                                          
123686            IF W-IDARTNR-CNT > 1                                          
123687               MOVE NOO              TO OK-SW                             
123688               MOVE '022'            TO ORUP-IDMSG-ERROR                  
123689               MOVE 'IDARTNR'        TO ORUP-IDELMT-ERROR                 
123690               STRING 'SEND ADD REQ FOR ' DELIMITED BY SIZE               
123692                W-IDARTNR-CHAR            DELIMITED BY SPACES             
123694                ',INSTEAD OF UPD ON LINE ' DELIMITED BY SIZE              
123696                WS-KVRADER-IX-DISP       DELIMITED BY SPACES              
123697                                            INTO ORUP-FEL-TEXT            
123698            END-IF                                                        
123699            ADD +1                   TO UPD-INDX                          
123700            PERFORM IMS-GN-WDQ4A1                                         
123701          END-PERFORM                                                     
123702        ELSE                                                              
123703          IF UPD-ORD-LINE                                                 
123704             MOVE NOO                    TO OK-SW                         
123705             MOVE '022'                  TO ORUP-IDMSG-ERROR              
123706             MOVE 'IDARTNR'              TO ORUP-IDELMT-ERROR             
123707                                                                          
123708             STRING 'ADD PART '         DELIMITED BY SIZE                 
123709             W-IDARTNR-CHAR             DELIMITED BY SPACES               
123711             ' FIRST,BEFORE UPDATE ON LINE ' DELIMITED BY SIZE            
123713             WS-KVRADER-IX-DISP         DELIMITED BY SPACES               
123714                                        INTO ORUP-FEL-TEXT                
123715          END-IF                                                          
123716        END-IF                                                            
123717     END-IF                                                               
123720     .                                                                    
123800     EJECT                                                                
123810*****************************************************************         
123820*CHECK IF PART IN REQUEST IS IN SDC FOR ORDERCLASS>1.IF NO,               
123830*CHECK IF CDC LINE IS PRESENT.IF YES, CONTINUE.IF NO, THEN                
123840*ADD/UPD NOT POSSIBLE                                                     
123850*****************************************************************         
123860 CCC-CHECK-SDC-STOCK SECTION.                                             
123870                                                                          
123880     MOVE 'CCC-CHECK-SDC-'        TO WS-CURRENT-SECTION                   
123890                                                                          
123891     IF OHUV-KDORDKL > 1 AND DC11-LINE-SW = NOO                           
123892                         AND OHUV-TIREPDAT = 0                            
123896        IF UPD-ORD-LINE                                                   
123897          MOVE OHUV-IDORDER          TO W-Q4SEQA-IDORDER-MIN              
123898                                        W-Q4SEQA-IDORDER-MAX              
123899          MOVE W-IDARTNR             TO W-Q4SEQA-IDARTNR-MIN              
123900                                        W-Q4SEQA-IDARTNR-MAX              
123901                                                                          
123902          PERFORM IMS-GU-WDQ4A                                            
123903          IF SEGMENT-FOUND                                                
123904             MOVE SEQA-IDDC            TO WS-IDDC                         
123906             IF LDC                                                       
123908                MOVE +1 TO WS-INDEX                                       
123911                PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX OR                
123912                            W-GMT-IDDC-CLEAR(WS-INDEX) = SPACES OR        
123913                             W-GMT-IDDC-CLEAR(WS-INDEX) = '11'            
123915                   PERFORM CCCA-CALL-SDCA                                 
123916                   ADD +1                TO WS-INDEX                      
123917                END-PERFORM                                               
123918                IF SDCA-KDORDBEK > 0                                      
123919                   PERFORM CCCB-CHECK-CDC-ORDERLINE                       
123920                END-IF                                                    
123921             ELSE                                                         
123922                IF CDC                                                    
123923                  SET DC11-LINE-FND TO TRUE                               
123924                END-IF                                                    
123925             END-IF                                                       
123926          END-IF                                                          
123927        END-IF                                                            
123940     END-IF                                                               
123941     .                                                                    
123942     EJECT                                                                
123943                                                                          
123944 CCCA-CALL-SDCA   SECTION.                                                
123945                                                                          
123946     MOVE 'CCCA-CALL-SDCA'        TO WS-CURRENT-SECTION                   
123947                                                                          
123948     MOVE W-IDARTNR           TO AREG-IDARTNR                             
123949     MOVE SEQA-IDDC           TO AREG-IDDC                                
123950*AREG CALL                                                                
123952     CALL W411AREG USING AREG-W411AREG                                    
123953                         AREG-WDK6-PCB                                    
123954                         AREG-WDK7-PCB                                    
123955*KVAN CALL                                                                
123956     MOVE 0                    TO KVAN-KDKVBRYT-IN                        
123957     MOVE OHUV-IDSYSTEM        TO KVAN-IDSYSTEM-IN                        
123958     MOVE WS-KVBEART           TO KVAN-KVBEART-IN                         
123959     MOVE AREG-KVQPACK-0       TO KVAN-KVQPACK-0-IN                       
123960     MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                       
123961     MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                        
123962     MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                          
123963     MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                        
123964     MOVE OHUV-KDORDKL         TO KVAN-KDORDKL-IN                         
123965     MOVE OHUV-FLEMBORD        TO KVAN-FLEMBORD-IN                        
123966     MOVE OHUV-FLFORBI         TO KVAN-FLFORBI-IN                         
123967     MOVE OHUV-FLORDSPE        TO KVAN-FLORDSPE-IN                        
123968     MOVE OHUV-FLOVRLEV        TO KVAN-FLOVRLEV-IN                        
123969     MOVE 0                    TO KVAN-IDKAMPRF-IN                        
123970     MOVE W-GMT-IDDC-CLEAR(WS-INDEX)                                      
123971                               TO KVAN-IDDC-IN                            
123972     MOVE OHUV-IDDISTR         TO KVAN-IDDISTR-IN                         
123973     MOVE OHUV-IDKUNDNR        TO KVAN-IDKUNDNR-IN                        
123974     MOVE SPACE                TO KVAN-BERADREF-IN                        
123975     MOVE W-IDARTNR            TO KVAN-IDARTNR-IN                         
123976                                                                          
123977     CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB KVAN-WDC1-PCB        
123978                                                                          
123979     IF KVAN-KDORDBEK-UT = 0                                              
123980     MOVE KVAN-KVBEART-Q-UT        TO WS-KVBEART                          
123981     END-IF                                                               
123982*SDCA CALL                                                                
123983     MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                            
123984     MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                            
123985     MOVE NOO                  TO SDCA-FLORDSPE                           
123986     MOVE AREG-FLREFILL        TO SDCA-FLREFILL                           
123987     MOVE W-IDARTNR           TO SDCA-IDARTNR                             
123988     MOVE W-GMT-IDDC-CLEAR(WS-INDEX)                                      
123989                               TO SDCA-IDDC                               
123990     MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                           
123991     MOVE SPACE                TO SDCA-IDLEVNR                            
123992     MOVE OHUV-IDSYSTEM        TO SDCA-IDSYSTEM                           
123993     MOVE OHUV-KDORDKL         TO SDCA-KDORDKL                            
123994     MOVE OHUV-KDORDING        TO SDCA-KDORDING                           
123995     MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                           
123996     MOVE AREG-KDSORT          TO SDCA-KDSORT                             
123997     MOVE WS-KVBEART                                                      
123998                               TO SDCA-KVBEART-Q                          
123999     MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                          
124000     MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                           
124001     MOVE 'N'                  TO SDCA-FLSDCLEV                           
124002     MOVE +0                   TO SDCA-TIREPDAT                           
124003     MOVE +0                   TO SDCA-KVOKS-PREL                         
124004     MOVE +2                   TO SDCA-KDCALL                             
124005     MOVE WS-INDEX             TO SDCA-IXDCCLEAR                          
124006     CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                      
124007                         SDCA-WDB6-PCB SDCA-WDK9-PCB                      
124008                         SDCA-WDR6-PCB SDCA-WDK6-PCB                      
124009                         SDCA-WDQ4B-PCB SDCA-WDQ2-PCB                     
124010                         SDCA-WDQ4-PCB                                    
124011                         SDCA-WDB6-2-PCB                                  
124012                         SDCA-WDK6-2-PCB                                  
124013                         SDCA-WDK7-2-PCB                                  
124014                         SDCA-WDK7-3-PCB                                  
124016     .                                                                    
124017     EJECT                                                                
124018 CCCB-CHECK-CDC-ORDERLINE SECTION.                                        
124019                                                                          
124020     MOVE 'CCCB-CHECK-CDC-ORDERLINE'                                      
124021                                 TO WS-CURRENT-SECTION                    
124023     MOVE NOO                     TO DC11-LINE-SW                         
124024                                                                          
124025     MOVE OHUV-IDORDER            TO W-Q4SEQA-IDORDER-MIN                 
124026                                     W-Q4SEQA-IDORDER-MAX                 
124027                                                                          
124028     MOVE 0                       TO W-Q4SEQA-IDARTNR-MIN                 
124029     MOVE 99999999                TO W-Q4SEQA-IDARTNR-MAX                 
124031     PERFORM IMS-GU-WDQ4A                                                 
124032     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-FINAL                       
124033                                   OR DC11-LINE-FND                       
124035       IF SEQA-IDDC = '11'                                                
124036          SET DC11-LINE-FND       TO TRUE                                 
124037       END-IF                                                             
124038       PERFORM IMS-GN-WDQ4A1                                              
124039     END-PERFORM                                                          
124040                                                                          
124041     IF DC11-LINE-SW = NOO                                                
124042        MOVE NOO              TO OK-SW                                    
124043        MOVE '022'            TO ORUP-IDMSG-ERROR                         
124044        MOVE 'IDARTNR'        TO ORUP-IDELMT-ERROR                        
124049        STRING 'SEND ADD REQ FOR ' DELIMITED BY SIZE                      
124050         W-IDARTNR-CHAR            DELIMITED BY SPACES                    
124051         ',INSTEAD OF UPD ON LINE ' DELIMITED BY SIZE                     
124052         WS-KVRADER-IX-DISP       DELIMITED BY SPACES                     
124053                                     INTO ORUP-FEL-TEXT                   
124054     END-IF                                                               
124055     .                                                                    
124056     EJECT                                                                
124057*****************************************************************         
124060*UPD/ADD VALIDATION:USER CANNOT SEND SAME P/N IN ADD AND UPD REQ.         
124100*POSSIBLE TO SEND SAME P/N MANY TIMES IN ADD REQ.                         
124200*****************************************************************         
124300 CD-CHECK-SAME-PART-ADD-UPD SECTION.                                      
124400                                                                          
124500     MOVE 'CD-CHECK-SAME-PART-ADD-UPD'                                    
124600                                 TO WS-CURRENT-SECTION                    
124800     MOVE +1                     TO ADD-INDX                              
124900                                                                          
125000     PERFORM UNTIL ADD-INDX > 53                     OR                   
125100                   WS-ADD-IDARTNR(ADD-INDX) = ZERO   OR                   
125200                   DUP-FND                                                
125300       MOVE WS-ADD-IDARTNR(ADD-INDX)                                      
125400                                 TO W-IDARTNR                             
125410                                                                          
125420       MOVE +1                   TO UPD-INDX                              
125500       PERFORM UNTIL UPD-INDX > 13                   OR                   
125600                     WS-UPD-IDARTNR(UPD-INDX) = ZERO OR                   
125700                     DUP-FND                                              
125800         IF W-IDARTNR = WS-UPD-IDARTNR(UPD-INDX)                          
125900            MOVE JA              TO DUP-FND-SW                            
125901            MOVE W-IDARTNR TO W-IDARTNR-NUM                               
126000         END-IF                                                           
126100         ADD +1                  TO UPD-INDX                              
126200       END-PERFORM                                                        
126300       ADD +1                    TO ADD-INDX                              
126400     END-PERFORM                                                          
126500                                                                          
126600     IF DUP-FND                                                           
126700        MOVE NOO                 TO OK-SW                                 
126800        MOVE '022'               TO ORUP-IDMSG-ERROR                      
126900        MOVE 'IDARTNR'           TO ORUP-IDELMT-ERROR                     
127110        STRING 'DUPLICATE ADD/UPD REQUEST FOR '                           
127120                                   DELIMITED BY SIZE                      
127131        W-IDARTNR-CHAR            DELIMITED BY SPACES                     
127132        ' NOT ALLOWED '           DELIMITED BY SIZE                       
127140                                       INTO ORUP-FEL-TEXT                 
127200     END-IF                                                               
127300     .                                                                    
127400     EJECT                                                                
127500 D-TRIGGER-4258-TRANS  SECTION.                                           
127600     MOVE 'D-TRIGGER-4258-TRAN'   TO WS-CURRENT-SECTION                   
127700                                                                          
127710     INITIALIZE 4258-MID-W4I25801                                         
127720                                                                          
127800     MOVE ORUP-IDDISTR            TO WS-IDDISTR                           
127900     MOVE ORUP-IDKUNDNR           TO WS-IDKUNDNR                          
128000     MOVE ORUP-IDORDNR7           TO WS-IDORDNR                           
128100     MOVE 1                       TO KVRADER-IX                           
128200                                     4258-MID-INDX                        
128300     MOVE ORUP-KVRADER            TO KVRADER-IX-MAX                       
128400                                                                          
128500     PERFORM UNTIL KVRADER-IX  > KVRADER-IX-MAX                           
128600                                                                          
129000       PERFORM DA-POPULATE-4258-INPUT                                     
129010                                                                          
129100       IF KVRADER-IX  < ORUP-KVRADER                                      
129200          MOVE NOO                TO 4258-MID-FLSLUT                      
129300          IF 4258-MID-INDX = 4258-MID-INDX-MAX                            
129400             PERFORM DB-START-4258-DISPATCHER                             
129500             MOVE ZERO            TO 4258-MID-INDX                        
129600          END-IF                                                          
129700       ELSE                                                               
129800          MOVE JA                 TO 4258-MID-FLSLUT                      
129900          PERFORM DB-START-4258-DISPATCHER                                
130000       END-IF                                                             
130100       ADD 1                      TO KVRADER-IX                           
130200                                     4258-MID-INDX                        
130300     END-PERFORM                                                          
130400     .                                                                    
130500     EJECT                                                                
130600 DA-POPULATE-4258-INPUT  SECTION.                                         
130700                                                                          
130800     MOVE 'DA-POPULATE-4258-INPUT'                                        
130900                            TO WS-CURRENT-SECTION                         
131000                                                                          
131100     MOVE WS-IDSYSTEM       TO 4258-MID-IDSYSTEM                          
131200     MOVE WS-IDDISTR        TO 4258-MID-IDDISTR                           
131300     MOVE WS-IDKUNDNR       TO 4258-MID-IDKUNDNR                          
131400     MOVE WS-IDORDNR7       TO 4258-MID-IDORDNR7                          
131500     MOVE WS-TIREGDAT       TO 4258-MID-TIREGDAT                          
131600                                                                          
131700     MOVE ORUP-KDBEHX(KVRADER-IX)                                         
131800                            TO 4258-MID-KDBEHX   (4258-MID-INDX)          
131810     MOVE WS-INP-IDARTNR(KVRADER-IX)                                      
131811                            TO 4258-MID-IDARTNR  (4258-MID-INDX)          
131820     MOVE WS-INP-REKSIFFR(KVRADER-IX)                                     
131830                            TO 4258-MID-REKSIFFR (4258-MID-INDX)          
132100     MOVE ORUP-KVBEART(KVRADER-IX)                                        
132200                            TO 4258-MID-KVBEART  (4258-MID-INDX)          
132300     MOVE ORUP-PRARTNTO-LOC(KVRADER-IX)                                   
132400                            TO WS-PRARTNTO-NUM                            
132500     MOVE WS-PRARTNTO-NUM   TO WS-PRARTNTO-RED                            
132600     MOVE WS-PRARTNTO-ALFA  TO                                            
132700                           4258-MID-PRARTNTO-LOC (4258-MID-INDX)          
132800     MOVE ORUP-KDVALISO(KVRADER-IX)                                       
132900                            TO 4258-MID-KDVALISO (4258-MID-INDX)          
133000     MOVE ORUP-BERADREF(KVRADER-IX)                                       
133100                            TO 4258-MID-BERADREF (4258-MID-INDX)          
133200     .                                                                    
133300     EJECT                                                                
133400 DB-START-4258-DISPATCHER     SECTION.                                    
133500                                                                          
133600     MOVE 'DB-START-4258-DISPATCHER'                                      
133700                              TO WS-CURRENT-SECTION                       
133800                                                                          
133900*    -- INITIALIZE MID DATA TO W40258                                     
134000     COMPUTE MSG-KVLL = LENGTH OF 4258-MID-W4I25801 + 17                  
134100     MOVE 4258-MID-W4I25801   TO MSG-INDATA-MINUS-1-TRANSKOD              
134200     MOVE 'W4T258X '          TO MSG-KDTRANS-1                            
134300     MOVE '4258'              TO MSG-IDTRANS-1                            
134400     MOVE '1'                 TO MSG-KDMFSFOR-1                           
134500     STRING 'API ' WS-IDDISTR                                             
134600              DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                     
134700*                                                                         
134800     ADD  1                   TO MSG-KOM-TIKLOCK                          
134900     MOVE 'W4I25801'          TO MSG-KOM-IDCPYTXT                         
135000*                                                                         
135100     CALL W006KOM USING MSG-PCB                                           
135200                        0693X-PCB                                         
135300                        WDP8-PCB                                          
135400                        MSG-KOM-WMSGKOM                                   
135500                        MSG-IO-AREA                                       
135600     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
135700        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
135800                         DELIMITED BY SIZE  INTO ERROR-TEXT               
135900        DISPLAY  ERROR-TEXT                                               
136000        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
136100     END-IF                                                               
136200*    -- CLEAR ORDER-LINE AREA BEFORE FIRST LINE IS ADDED                  
136300     MOVE SPACE               TO 4258-MID-W4I25801                        
136400     .                                                                    
136500     EJECT                                                                
136600                                                                          
147400 Z-FINIT SECTION.                                                         
147500                                                                          
147600     MOVE 'Z-FINIT'          TO WS-CURRENT-SECTION                        
147610                                                                          
147700     IF SOMETHING-WRONG                                                   
147800        SET ORUP-KDSVAR-FEL  TO TRUE                                      
147900     END-IF                                                               
148000     .                                                                    
148100     EJECT                                                                
148200*** ---------------------                                                 
148300*** --- IMS SECTIONS  ---                                                 
148400*** ---------------------                                                 
148500                                                                          
148600 IMS-GU-WDK601  SECTION.                                                  
148700     MOVE 'IMS-GU-WDK601   ' TO WS-CURRENT-IMS-SECTION                    
148800                                                                          
148900     MOVE SPACE                 TO ALL-SSA                                
149000     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
149100            DELIMITED BY SIZE INTO SSA1                                   
149200     MOVE '  GE'                TO GOOD-STATUSCODES                       
149300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
149400     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
149500     PERFORM IMS-STATUSCHECK                                              
149600     .                                                                    
149700     EJECT                                                                
149800 IMS-GNP-WDK611  SECTION.                                                 
149900     MOVE 'IMS-GNP-WDK611  ' TO WS-CURRENT-IMS-SECTION                    
150000                                                                          
150100     MOVE SPACE                 TO ALL-SSA                                
150200     MOVE 'WDK611 '             TO SSA1                                   
150300     MOVE '  GE'                TO GOOD-STATUSCODES                       
150400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
150500     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
150600     PERFORM IMS-STATUSCHECK                                              
150700     .                                                                    
150800     EJECT                                                                
150900 IMS-GU-WDB201 SECTION.                                                   
151000     MOVE 'IMS-GU-WDB201   ' TO WS-CURRENT-IMS-SECTION                    
151100                                                                          
151200     MOVE SPACE               TO ALL-SSA                                  
151300     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
151400          DELIMITED BY SIZE INTO SSA1                                     
151500     MOVE '  GE'              TO GOOD-STATUSCODES                         
151600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
151700     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
151800     PERFORM IMS-STATUSCHECK                                              
151900     .                                                                    
152000     EJECT                                                                
152100 IMS-GU-WDF501-BSEQ SECTION.                                              
152200     MOVE 'IMS-GU-WDF501-BS' TO WS-CURRENT-IMS-SECTION                    
152300                                                                          
152400     MOVE SPACE                 TO ALL-SSA                                
152500     STRING 'WDF501  (WDF5BSEQ =' W-WDF5BSEQ ')'                          
152600            DELIMITED BY SIZE INTO SSA1                                   
152700     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
152800     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF501 SSA1                    
152900     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
153000     PERFORM IMS-STATUSCHECK                                              
153100     .                                                                    
153200     EJECT                                                                
153300 IMS-GU-WDQ201-CSEQ SECTION.                                              
153400     MOVE 'IMS-GU-WDQ201-CSEQ' TO WS-CURRENT-IMS-SECTION                  
153500                                                                          
153600     MOVE SPACE                TO ALL-SSA                                 
153700     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
153800          DELIMITED BY SIZE  INTO SSA1                                    
153900     MOVE '  GE'               TO GOOD-STATUSCODES                        
154000     CALL CBLTDLI USING GU WDQ2CSEQ-PCB DLI-IO-WDQ201 SSA1                
154100     MOVE WDQ2CSEQ-STATUS-CODE TO STATUS-WS                               
154200     PERFORM IMS-STATUSCHECK                                              
154300     .                                                                    
154400     EJECT                                                                
154500 IMS-GNP-WDQ2-WDQ211-FIRST SECTION.                                       
154600     MOVE 'IMS-GNP-WDQ2-WDQ211-FIRST'                                     
154700                               TO WS-CURRENT-IMS-SECTION                  
154800                                                                          
154900     MOVE 'WDQ211  *F'  TO SSA1                                           
155000     MOVE '  GE' TO GOOD-STATUSCODES                                      
155100     CALL CBLTDLI USING GNP WDQ2CSEQ-PCB  DLI-IO-WDQ211 SSA1              
155200     MOVE WDQ2CSEQ-STATUS-CODE TO STATUS-WS                               
155300     PERFORM IMS-STATUSCHECK                                              
155400     .                                                                    
155500     EJECT                                                                
155600 IMS-GNP-WDQ2-WDQ211 SECTION.                                             
155700     MOVE 'IMS-GNP-WDQ2-WDQ211'                                           
155800                               TO WS-CURRENT-IMS-SECTION                  
155900                                                                          
156000     MOVE 'WDQ211   ' TO SSA1                                             
156100     MOVE '  GE' TO GOOD-STATUSCODES                                      
156200     CALL CBLTDLI USING GNP WDQ2CSEQ-PCB  DLI-IO-WDQ211 SSA1              
156300     MOVE WDQ2CSEQ-STATUS-CODE TO STATUS-WS                               
156400     PERFORM IMS-STATUSCHECK                                              
156500     .                                                                    
156600     EJECT                                                                
156700 IMS-GNP-WDQ212  SECTION.                                                 
156800                                                                          
156900     MOVE 'IMS-GNP-WDQ212  '   TO WS-CURRENT-IMS-SECTION                  
157000                                                                          
157100     MOVE 'WDQ212'   TO SSA1                                              
157200     MOVE '  GE' TO GOOD-STATUSCODES                                      
157300     CALL CBLTDLI USING GNP WDQ2CSEQ-PCB  DLI-IO-WDQ212 SSA1              
157400     MOVE WDQ2CSEQ-STATUS-CODE TO STATUS-WS                               
157500     PERFORM IMS-STATUSCHECK                                              
157600     .                                                                    
157700     EJECT                                                                
163600 IMS-GU-WDQ4A SECTION.                                                    
163700     MOVE 'IMS-GU-WDQ4A' TO WS-CURRENT-IMS-SECTION                        
163800                                                                          
163900     STRING 'WDQ4A1  (WDQ4A1KY>=' W-WDQ4A1KY-MIN-X                        
164000                    '&WDQ4A1KY<=' W-WDQ4A1KY-MAX-X ')'                    
164100          DELIMITED BY SIZE INTO SSA1                                     
164200     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
164300     CALL CBLTDLI USING GU WDQ4A-PCB DLI-IO-WDQ4A SSA1                    
164400     MOVE WDQ4A-STATUS-CODE TO STATUS-WS                                  
164500     PERFORM IMS-STATUSCHECK                                              
164600     .                                                                    
164700     EJECT                                                                
164800 IMS-GN-WDQ4A1  SECTION.                                                  
164900     MOVE 'IMS-GN-WDQ4A1 ' TO WS-CURRENT-IMS-SECTION                      
165000                                                                          
165100     STRING 'WDQ4A1  (WDQ4A1KY>=' W-WDQ4A1KY-MIN-X                        
165200                    '&WDQ4A1KY<=' W-WDQ4A1KY-MAX-X ')'                    
165300          DELIMITED BY SIZE INTO SSA1                                     
165400     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
165500     CALL CBLTDLI USING GN WDQ4A-PCB DLI-IO-WDQ4A SSA1                    
165600     MOVE WDQ4A-STATUS-CODE TO STATUS-WS                                  
165700     PERFORM IMS-STATUSCHECK                                              
165800     .                                                                    
165900     EJECT                                                                
166000 IMS-STATUSCHECK SECTION.                                                 
166100                                                                          
166200     SET STATUS-IX TO 1                                                   
166300     SEARCH GOOD-STATUS                                                   
166400       AT END                                                             
166500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
166600           DELIMITED BY SIZE INTO ERROR-TEXT                              
166700         DISPLAY ERROR-TEXT                                               
166800         CALL FELLOG                                                      
166900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
167000         CONTINUE                                                         
167100     END-SEARCH                                                           
167200     .                                                                    
167300*    -COPY WY2000Q1                                                       
