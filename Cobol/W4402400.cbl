000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.             W4402400.                                        
000400 AUTHOR.                 CARINA VIKTORSSON.                               
000500 DATE-WRITTEN.           JULI 1988.                                       
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    BMP                                                                  
001000*      HTR 4553 WLXXLN/WDR4 UPPDATERAS MED CHKP-RÄKNARE MM.               
001100*      RESTART-FIL IN, GSAM-BAS UT (WDS1).                                
001200*      VID ÅTERSTART LÄSES RESTART-FIL FRAM TILL SISTA CHKP-LÄGE          
001300*      OCH GSAM-BAS SKRIVS FÖR VARJE RESTART-POST.                        
001400*      OCKSÅ INFILEN LÄSES FRAM TILL SISTA CHKP-LÄGE.                     
001500*      CHKP TAS VID BRYTNING IDARTNR.                                     
001600*                                                                         
001700*    FUNKTION:                                                            
001800*                                                                         
001900*        LÄSER INFIL MED INFORMATION OM ERSATTA, TILLKOMMANDE-            
002000*        FÖRÄNDRADE ARTIKLAR KOMPLETTERADE MED KUNDINFORMATION            
002100*        OCH RESTORDERINFORMATION.                                        
002200*                                                                         
002300*        ERSÄTTNING PÅ RO-REGISTRET OCH LARMKÖ AV RO/TPO:ER.              
002400*        RADER MED ERSATTA ARTIKLAR DELETAS PÅ RO-REGISTRET /             
002500*        LARMKÖ OCH NYA TILLKOMMANDE LÄGGS UPP.                           
002600*                                                                         
002700*        HTR WL4505/WDR4 SKAPAS FÖR TILLKOMMANDE ARTIKLAR OM              
002800*        DISP-SALDO > KVART                                               
002900*                                                                         
003000*    ABENDKODER:                                                          
003100*        U0016 - RETURKOD VID FEL ÅTERSTART                               
003200*        U0032 - DIVERSE FELSITUATIONER                                   
003300*                                                                         
003400*    E'TRACKER: 5444132 DATED 2007-08-21                                  
003500*                                                                         
003600     EJECT                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800                                                                          
003900 INPUT-OUTPUT SECTION.                                                    
004000                                                                          
004100 FILE-CONTROL.                                                            
004200     SKIP2                                                                
004300*    ---- INFIL:                                                          
004400     SELECT  INFIL         ASSIGN  W44024D1.                              
004500     SKIP2                                                                
004600*    ---- XRST-FIL: KOPIA AV UTFIL(+0) TAS IN SOM INPUT-FIL               
004700     SELECT  XRST-FIL      ASSIGN  W44024D2.                              
004800     EJECT                                                                
004900 DATA DIVISION.                                                           
005000                                                                          
005100 FILE SECTION.                                                            
005200     SKIP3                                                                
005300 FD  INFIL                                                                
005400     LABEL RECORD STANDARD                                                
005500     RECORDING  F                                                         
005600     BLOCK CONTAINS 0.                                                    
005700                                                                          
005800 01  INPOST -COPY W440001       -L.                                       
005900     SKIP3                                                                
006000 FD  XRST-FIL                                                             
006100     LABEL RECORD STANDARD                                                
006200     RECORDING  V                                                         
006300     BLOCK CONTAINS 0.                                                    
006400                                                                          
006500 01  FILLER                  PIC X(998).                                  
006600 01  POST   -COPY W440001   -PRE XRST-                                    
006700     EJECT                                                                
006800 WORKING-STORAGE SECTION.                                                 
006900                                                                          
007000*    -- CHECKED BY WY2000                                                 
007100 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W4402400'.                
007200     SKIP1                                                                
007300 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
007400     SKIP1                                                                
007500 77  JA                      PIC X       VALUE 'J'.                       
007600 77  NEJ                     PIC X       VALUE 'N'.                       
007700     SKIP1                                                                
007800 77  CHKP-ID                 PIC X(8)    VALUE 'W44024  '.                
007900 77  MSG-IO-AREA-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
008000 77  MSG-IO-AREA             PIC X(32)   VALUE SPACE.                     
008100 77  CHKP-AREA-1-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
008200 77  CHKP-AREA-1             PIC X(32)   VALUE SPACE.                     
008300     SKIP1                                                                
008400     SKIP1                                                                
008500 77  IX                      PIC S9(9)   VALUE +0   COMP SYNC.            
008600 77  IX1                     PIC 9(2)    VALUE ZERO.                      
008700 77  IX2                     PIC 9(2)    VALUE ZERO.                      
008800 77  INDX                    PIC 9(3)    VALUE ZERO.                      
008900 77  CLIX                    PIC S9(9)   VALUE +0   COMP SYNC.            
009000 77  CLIX-MAX                PIC S9(9)   VALUE +2   COMP SYNC.            
009100     SKIP1                                                                
009200 77  INFIL-EOF               PIC X       VALUE 'N'.                       
009300 77  XRST-FIL-EOF            PIC X       VALUE 'N'.                       
009400                                                                          
009500 77  IDDC-BRYT               PIC X(2)            VALUE SPACE.             
009600 77  IDARTNR-BRYT            PIC S9(9)   COMP-3  VALUE ZERO.              
009700 77  MAX-GSAM                PIC S9(4)   COMP.                            
009800*                                                                         
009900 77  WS-IDPRQUES             PIC S9(7)   VALUE +0.                        
010000                                                                          
010100 77  KDRC-DISPLAY            PIC Z(5)    VALUE ZERO.                      
010200     EJECT                                                                
010300                                                                          
010400 01  WX-PRARTNTO-LOC         PIC S9(7)V99 VALUE ZERO COMP-3.              
010500 01  WX-PRARTNTO-LOCPREL     PIC S9(7)V99 VALUE ZERO COMP-3.              
010600                                                                          
010700 01  DATUMFALT.                                                           
010800     03 DAGENS-DATUM             PIC S9(7) VALUE ZERO COMP-3.             
010900                                                                          
011000 01  FILLER                  PIC X(16)   VALUE 'WS-FLD**********'.        
011100 01  WS-FLD.                                                              
011200   03  WS-ANT-POST-IN        PIC S9(5)   COMP-3  VALUE ZERO.              
011300   03  WS-ANT-POST-GSAM      PIC S9(5)   COMP-3  VALUE ZERO.              
011400   03  WS-ANT-POST-XRST      PIC S9(5)   COMP-3  VALUE ZERO.              
011500*                                                                         
011600*    NÄSTA KLOCKSLAG                                                      
011700   03  WS-NXT-KL             PIC 9(6).                                    
011800   03  FILLER                REDEFINES WS-NXT-KL.                         
011900     05  WS-NXT-KL-TT        PIC 9(2).                                    
012000     05  WS-NXT-KL-MM        PIC 9(2).                                    
012100     05  WS-NXT-KL-SS        PIC 9(2).                                    
012200     SKIP1                                                                
012300*    LARM-ANSKAFFARE LÄS OCH SPARAS HÄR EN GÅNG PER ARTIKEL.              
012400   03  WS-IDANSK-LARM        PIC S9(3)   COMP-3  VALUE ZERO.              
012500     SKIP1                                                                
012600   03  WS-AAMMDD             PIC 9(6).                                    
012700     SKIP1                                                                
012800   03  WS-AAVV               PIC 9(4).                                    
012900   03  FILLER                REDEFINES WS-AAVV.                           
013000     05  WS-AAVV-AA          PIC 9(2).                                    
013100     05  WS-AAVV-VV          PIC 9(2).                                    
013200     SKIP1                                                                
013300   03  WS-TITPO-AAVV         PIC 9(4).                                    
013400     SKIP1                                                                
013500   03  WS-SUTPO-TOT-SPAR     PIC S9(7)   COMP-3.                          
013600     EJECT                                                                
013700*                                                                         
013800*01  -COPY WWPRODSL                                                       
013900                                                                          
014000*    INFO FRÅN SIST KONSUMERAD IN-POST FÖRE CHKP                          
014100   03  WS-CHKPID.                                                         
014200     05  WS-CHKPID-KVPOST    PIC S9(7)               COMP-3.              
014300     05  WS-CHKPID-IDDISTR   PIC S9(5)               COMP-3.              
014400     05  WS-CHKPID-IDKUNDNR  PIC S9(7)               COMP-3.              
014500     05  WS-CHKPID-IDKUNDRF  PIC X(10).                                   
014600     05  WS-CHKPID-IDLOPNRE  PIC S9(3)               COMP-3.              
014700     05  WS-CHKPID-IDKORTNR-ERS                                           
014800                             PIC S9(3)               COMP-3.              
014900     SKIP1                                                                
015000*    TIDS-STÄMPEL FÖR CHKP-HTR                                            
015100   03  WS-TS.                                                             
015200     05  WS-TS-TIAAMMDD      PIC 9(6).                                    
015300     05  WS-TS-TIKLOCK       PIC 9(8).                                    
015400     SKIP1                                                                
015500*    GENERELLT TIDSFÄLT                                                   
015600   03  WS-TIKLOCK-X.                                                      
015700     05  WS-TIKLOCK-HHMMSS   PIC 9(6).                                    
015800     05  FILLER              PIC X(2).                                    
015900 01 DB2-LASNING.                                                          
016000     03 FILLER                   PIC X(16)   VALUE                        
016100                                             'WS-DB2-SEKTION'.            
016200     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
016300                                                                          
016400                                                                          
016500 01 NYCKLAR-TP4TRAN.                                                      
016600     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
016700                                                                          
016800     SKIP3                                                                
016900 01  TEST-IDDISTR            PIC S9(5) COMP-3.                            
017000*01  FILLER -COPY WWDIST35 -RED TEST-IDDISTR.                             
017100     EJECT                                                                
017200*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
017300     EJECT                                                                
017400 01  FILLER                      PIC X(16)   VALUE 'DIST-DC-TAB'.         
017500*    -COPY WWDIST57                                                       
017600     EJECT                                                                
017700*------------------------------- ARB.FÄLT FÖR SUB-PGM W440RROT            
017800 01      FILLER              PIC X(16)   VALUE 'RROTW***********'.        
017900 01      RROTW.                                                           
018000*                                                                         
018100  03     RROTW-TIDISPIN      PIC S9(7)               COMP-3.              
018200     EJECT                                                                
018300 01  FILLER                  PIC X(16)   VALUE '****************'.        
018400 01  SW.                                                                  
018500     SKIP1                                                                
018600  02 SW-2232-LAST                PIC X(1).                                
018700     SKIP1                                                                
018800  02 SW-BRYTNING-KVANTER         PIC X(1).                                
018900     88  BRYTNING-JA             VALUE 'J'.                               
019000     88  BRYTNING-NEJ            VALUE 'N'.                               
019100                                                                          
019200 01  W-TOT-BEHOV             PIC S9(7)V9(1) COMP-3 VALUE ZERO.            
019300*                                                                         
019400 01  SW-SATSORDER            PIC X.                                       
019500     88  SATS-ORDER          VALUE 'J'.                                   
019600     SKIP2                                                                
019700*    ---- ARBETSFÄLT FÖR IHOPSÄTTNING AV DATUM                            
019800     SKIP1                                                                
019900 01  W-DATUM                 PIC 9(6).                                    
020000 01  FILLER REDEFINES W-DATUM.                                            
020100     03  W-AR                PIC 9(2).                                    
020200     03  W-MANAD             PIC 9(2).                                    
020300     03  W-DAG               PIC 9(2).                                    
020400     SKIP1                                                                
020500 01  W-RODATUM               PIC 9(5).                                    
020600 01  FILLER REDEFINES W-RODATUM.                                          
020700     03  W-D-AAR             PIC 9(2).                                    
020800     03  W-D-VECKA           PIC 9(2).                                    
020900     03  W-D-DAGNR           PIC 9(1).                                    
021000     EJECT                                                                
021100 01  ARTIKEL-AREA.                                                        
021200    03  DISP-SALDO           PIC S9(7)       COMP-3 VALUE ZERO.           
021300    03  SUTPO-TOT            PIC S9(7)       COMP-3 VALUE ZERO.           
021400    03  KVROS                PIC S9(7)       COMP-3 VALUE ZERO.           
021500    03  KVROS-DAG            PIC S9(7)       COMP-3 VALUE ZERO.           
021600    03  KVROS-BULK           PIC S9(7)       COMP-3 VALUE ZERO.           
021700    03  KVRESS               PIC S9(7)       COMP-3 VALUE ZERO.           
021800    03  KVLS                 PIC S9(7)       COMP-3 VALUE ZERO.           
021900    03  KVSPANT              PIC S9(7)       COMP-3 VALUE ZERO.           
022000    03  KVUTRS               PIC S9(7)       COMP-3 VALUE ZERO.           
022100    03  KDERS                PIC S9(3)       COMP-3 VALUE ZERO.           
022200 01  TACKBART                PIC S9(7)       COMP-3 VALUE ZERO.           
022300 01  ANTAL                   PIC S9(7)       COMP-3 VALUE ZERO.           
022400     EJECT                                                                
022500 01  FILLER                      PIC X(8)  VALUE 'IN-AREA'.               
022600*01  IN-POST  -COPY W440001                                               
022700     EJECT                                                                
022800 01  DYNAMISKA-SUBPROGRAM.                                                
022900   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
023000   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
023100   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
023200   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
023300   03  DATKORT               PIC X(8)    VALUE 'DATKORT '.                
023400   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
023500   03  W440RROT              PIC X(8)    VALUE 'W440RROT'.                
023600   03  W335PRNO              PIC X(8)    VALUE 'W335PRNO'.                
023700   03  W335PRQU              PIC X(8)    VALUE 'W335PRQU'.                
023800   03  WZ01SEND              PIC X(8)    VALUE 'WZ01SEND'.                
023900   03  W005WDK7              PIC X(8)    VALUE 'W005WDK7'.                
024000   03  W005WDL7              PIC X(8)    VALUE 'W005WDL7'.                
024100     SKIP3                                                                
024200 01  -COPY WWDCKONS                                                       
024300     EJECT                                                                
024400*    --- PARAMETRAR TILL W005WDK7                                         
024500 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
024600*   -COPY W005WDK7                                                        
024700     EJECT                                                                
024800*    --- PARAMETRAR TILL W005WDL7                                         
024900 01 FILLER                       PIC X(8)    VALUE 'W005WDL7'.            
025000*   -COPY W005WDL7                                                        
025100     EJECT                                                                
025200*    ----  PARAMETRAR TILL ABEND                                          
025300     SKIP1                                                                
025400 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16 COMP SYNC.               
025500 01  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.             
025600     EJECT                                                                
025700*    ----  PARAMETRAR TILL POSTSUM                                        
025800                                                                          
025900 01  -COPY W0005       -PRE POSTSUM-.                                     
026000     EJECT                                                                
026100*    ----  PARAMETRAR TILL DATUMKORT                                      
026200                                                                          
026300 01  DATUMKORT-ID            PIC X(6)   VALUE 'WDATUM'.                   
026400     SKIP3                                                                
026500 01  -COPY WDATKORT                                                       
026600     EJECT                                                                
026700*    ----  PARAMETRAR TILL WDATKONV                                       
026800                                                                          
026900 01  FILLER                  PIC X(16)  VALUE 'WDATAREA********'.         
027000     SKIP2                                                                
027100 01  -COPY WDATAREA                                                       
027200     EJECT                                                                
027300*    ----  PARAMETRAR TILL W440RROT                                       
027400                                                                          
027500 01  FILLER                  PIC X(16)  VALUE 'W440RROT********'.         
027600     SKIP2                                                                
027700 01  -COPY W440RROT                                                       
027800     EJECT                                                                
027900 01  FILLER                  PIC X(16)   VALUE 'W335PRNO     '.           
028000                                                                          
028100*01  -COPY W335PRNO                                                       
028200      EJECT                                                               
028300 01  FILLER                  PIC X(16)   VALUE 'W335PRQU     '.           
028400                                                                          
028500*01  -COPY W335PRQU                                                       
028600     EJECT                                                                
028700     EJECT                                                                
028800*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
028900                                                                          
029000 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
029100     SKIP3                                                                
029200*    ---- STATUSKOD FRÅN IMS                                              
029300                                                                          
029400 01  STATUS-WS               PIC XX.                                      
029500     88  SEGMENT-FINNS                    VALUE '  '.                     
029600     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
029700     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
029800     88  IMS-EJ-OK                        VALUE 'XD'.                     
029900     SKIP3                                                                
030000*    ---- SEG-LEVEL FRÅN IMS                                              
030100                                                                          
030200 01  SEG-LEVEL-WS            PIC X(2).                                    
030300     88  ROT-SEGM-SAKNAS                  VALUE '00'.                     
030400     SKIP3                                                                
030500 01  GODK-STATUSKODER.                                                    
030600   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
030700     SKIP3                                                                
030800 01  SSA1                    PIC X(64).                                   
030900 01  SSA2                    PIC X(64).                                   
031000 01  SSA3                    PIC X(64).                                   
031100     EJECT                                                                
031200*                            DB2 FUNKTIONSKODER                           
031300 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
031400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
031500                                                                          
031600 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
031700 01  DB2-WS.                                                              
031800     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
031900         88  CURSOR-OK                       VALUE 000.                   
032000         88  RADER-FINNS                     VALUE 000.                   
032100         88  RADER-SAKNAS                    VALUE 100.                   
032200         88  ATKOMST-FEL                     VALUE 904.                   
032300     03  GODK-SQLCODEKODER.                                               
032400         05  GODK-SQLCODE OCCURS 5                                        
032500             INDEXED BY SQLCODE-IX PIC 9(3).                              
032600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
032700     EJECT                                                                
032800     EJECT                                                                
032900*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
033000                                                                          
033100 01  NYCKLAR-TILL-DLI.                                                    
033200                                                                          
033300   03  W-WDA501KY-X.                                                      
033400     05  W-IDDISTR-X.                                                     
033500         07  W-IDDISTR           PIC S9(5)    COMP-3.                     
033600     05  W-IDKUNDNR-X.                                                    
033700         07  W-IDKUNDNR          PIC S9(7)    COMP-3.                     
033800     05  W-IDKUNDRF-X.                                                    
033900         07  W-IDKUNDRF          PIC X(10).                               
034000     05  W-IDARTNR-X.                                                     
034100         07  W-IDARTNR           PIC S9(9)    COMP-3.                     
034200     05  W-IDLOPNR-X.                                                     
034300         07  W-IDLOPNR           PIC S9(3)    COMP-3.                     
034400   03    W-IDDC-X.                                                        
034500     05  W-IDDC                  PIC X(2).                                
034600                                                                          
034700   03  W-IDARTNR-ARTC-X.                                                  
034800     05  W-IDARTNR-ARTC      PIC S9(9)    COMP-3.                         
034900                                                                          
035000   03  W-IDARTNR-WDK7-X.                                                  
035100     05  W-IDARTNR-WDK7      PIC S9(9)    COMP-3.                         
035200   03  W-IDDC-WDK7-X.                                                     
035300     05  W-IDDC-WDK7         PIC X(2).                                    
035400                                                                          
035500   03  W-IDARTNR-ARTM-X.                                                  
035600     05  W-IDARTNR-ARTM      PIC S9(9)    COMP-3.                         
035700                                                                          
035800   03  W-DABEHOV-X.                                                       
035900     05  W-WDK911-DABEHOV    PIC  9(6).                                   
036000                                                                          
036100   03  W-WDGX-NYCKEL-X.                                                   
036200     05  FILLER              PIC X(30).                                   
036300                                                                          
036400   03  W-IDHTYP-4505-X.                                                   
036500     05  FILLER              PIC X(4)    VALUE '4505'.                    
036600     05  W-4505-IDDC         PIC X(2).                                    
036700     05  FILLER              PIC X(24)   VALUE LOW-VALUE.                 
036800                                                                          
036900   03  W-IDHTYP-4553-X.                                                   
037000     05  FILLER              PIC X(4)    VALUE '4553'.                    
037100     05  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
037200                                                                          
037300   03  W-IDHTYP-2225-X.                                                   
037400     05  FILLER              PIC X(4)    VALUE '2225'.                    
037500     05  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
037600                                                                          
037700   03  W-2232-WDGXKEY.                                                    
037800     05  W-2232-IDANSK       PIC S9(3)    COMP-3.                         
037900     05  W-2232-LOW-VALUE    PIC X(3)     VALUE LOW-VALUE.                
038000                                                                          
038100   03  W-2224-WDGXKEY.                                                    
038200     05  W-2224-TISENBEK-DAG PIC S9(7)    COMP-3.                         
038300     05  W-2224-TISENBEK-KL  PIC S9(7)    COMP-3.                         
038400     05  W-2224-KDLARM       PIC S9(3)    COMP-3.                         
038500                                                                          
038600   03  W-IDDC-B6-X.                                                       
038700       05 W-IDDC-B6                  PIC X(2).                            
038800     EJECT                                                                
038900 01  -COPY WDGX01     -PRE W-2231-                                        
039000     EJECT                                                                
039100 01  -COPY WDGX2223   -PRE W-                                             
039200     EJECT                                                                
039300 01    NYCKEL-AREA.                                                       
039400   03  -COPY WDGX01                                                       
039500     EJECT                                                                
039600 01  -COPY W0003                                                          
039700     EJECT                                                                
039800* ---       DLI INPUT OUTPUT AREA.                                        
039900* ---       DLI-IO-AREA.                                                  
040000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ORDP01'.           
040100 01  DLI-IO-ORDP01.                                                       
040200*  03  WLORDP01 -COPY WDA501.                                             
040300     EJECT                                                                
040400                                                                          
040500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ARTC01'.           
040600 01  DLI-IO-ARTC01.                                                       
040700*  03  WLARTC01 -COPY WDK601   -PRE ART-                                  
040800     EJECT                                                                
040900                                                                          
041000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ARTC11'.           
041100 01  DLI-IO-ARTC11.                                                       
041200*  03  WLARTC11 -COPY WDK611                                              
041300     EJECT                                                                
041400                                                                          
041500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK701'.           
041600 01  DLI-IO-WDK701.                                                       
041700*  03  -COPY WDK701                                                       
041800                                                                          
041900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK711'.           
042000 01  DLI-IO-WDK711.                                                       
042100*  03  -COPY WDK711                                                       
042200     EJECT                                                                
042300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK722'.           
042400 01  DLI-IO-WDK722.                                                       
042500*  03  -COPY WDK722                                                       
042600     EJECT                                                                
042700                                                                          
042800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-450501'.           
042900 01  DLI-IO-450501.                                                       
043000*  03  WL450501 -COPY WDGX4505    -PRE 4505-                              
043100     SKIP2                                                                
043200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-450511'.           
043300 01  DLI-IO-450511.                                                       
043400*  03  WL450511 -COPY WDGX4506    -PRE 4505-                              
043500     EJECT                                                                
043600                                                                          
043700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-XXLN11'.           
043800 01  DLI-IO-XXLN11.                                                       
043900*  03  WLXXLN11 -COPY WDGX4554    -PRE XXLN-                              
044000     EJECT                                                                
044100                                                                          
044200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-XXBX11'.           
044300 01  DLI-IO-XXBX11.                                                       
044400*  03  WLXXBX11 -COPY WDGX2232    -PRE XXBX-                              
044500     EJECT                                                                
044600                                                                          
044700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-XXBU01'.           
044800 01  DLI-IO-XXBU01.                                                       
044900*  03  WLXXBU01 -COPY WDGX2223    -PRE XXBU-                              
045000     EJECT                                                                
045100                                                                          
045200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-XXBU11'.           
045300 01  DLI-IO-XXBU11.                                                       
045400*  03  WLXXBU11 -COPY WDGX2224    -PRE XXBU-                              
045500     EJECT                                                                
045600                                                                          
045700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ARTM01'.           
045800 01  DLI-IO-ARTM01.                                                       
045900*  03  WLARTM01 -COPY WDK901                                              
046000     EJECT                                                                
046100                                                                          
046200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ARTM11'.           
046300 01  DLI-IO-ARTM11.                                                       
046400*  03  WLARTM11 -COPY WDK911                                              
046500                                                                          
046600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
046700 01   DLI-IO-AREA-B601.                                                   
046800*     03  -COPY WDB601                                                    
046900     EJECT                                                                
047000                                                                          
047100 01  FILLER                    PIC X(16) VALUE 'GSAMFIL-IO-AREA'.         
047200 01  GSAMFIL-IO-AREA.                                                     
047300     03  GSAM-LRECL            PIC S9(4) COMP.                            
047400*    03 -COPY W440001         -PRE  GSAM-                                 
047500     EJECT                                                                
047600 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
047700     SKIP3                                                                
047800 01  -COPY WZ01SEND                                                       
047900     EJECT                                                                
048000 01  FILLER                      PIC X(16)  VALUE 'SEND-AREA'.            
048100     SKIP3                                                                
048200 01  SEND-AREA.                                                           
048300*    03  -COPY WZ01REQU  -PRE 3039-                                       
048400*    03  -COPY W30391I1  -PRE 3039-                                       
048500     EJECT                                                                
048600 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
048700*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
048800     EJECT                                                                
048900     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
049000     EJECT                                                                
049100 LINKAGE SECTION.                                                         
049200     SKIP2                                                                
049300 01  -COPY W0009      -PRE  MSG-                                          
049400     EJECT                                                                
049500 01  -COPY W0009      -PRE PRQRY-                                         
049600      SKIP2                                                               
049700 01  -COPY W0008      -PRE  ARTC-                                         
049800       05  FILLER                PIC X.                                   
049900     EJECT                                                                
050000 01  -COPY W0008      -PRE  WDK7-                                         
050100       05  FILLER                PIC X.                                   
050200     EJECT                                                                
050300 01  -COPY W0008      -PRE  ORDP-                                         
050400       05  FILLER                PIC X.                                   
050500     EJECT                                                                
050600 01  -COPY W0008      -PRE  ARTM-                                         
050700       05  FILLER                PIC X.                                   
050800     EJECT                                                                
050900 01  -COPY W0008      -PRE  XXLN-                                         
051000       05  FILLER                PIC X.                                   
051100     EJECT                                                                
051200 01  -COPY W0008      -PRE  XXBX-                                         
051300       05  FILLER                PIC X.                                   
051400     EJECT                                                                
051500 01  -COPY W0008      -PRE  XXBU-                                         
051600       05  FILLER                PIC X.                                   
051700     EJECT                                                                
051800 01  -COPY W0008      -PRE  4505-                                         
051900       05  FILLER                PIC X.                                   
052000     EJECT                                                                
052100 01  -COPY W0008      -PRE  WDB6-                                         
052200       05  FILLER                PIC X.                                   
052300     EJECT                                                                
052400 01  RROT-ARTM-PCB               PIC X.                                   
052500     SKIP3                                                                
052600 01  RROT-ARTS-PCB               PIC X.                                   
052700     SKIP3                                                                
052800 01  -COPY W0008      -PRE  GSAMFIL-                                      
052900       05  FILLER                PIC X.                                   
053000     EJECT                                                                
053100 01  PRNO-3107-PCB               PIC X.                                   
053200 01  PRQU-WDG2-PCB               PIC X.                                   
053300 01  PRQU-WDC7-PCB               PIC X.                                   
053400 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
053500     EJECT                                                                
053600 PROCEDURE DIVISION  USING  MSG-PCB                                       
053700                            PRQRY-PCB                                     
053800                            ARTC-PCB                                      
053900                            WDK7-PCB                                      
054000                            ORDP-PCB                                      
054100                            ARTM-PCB                                      
054200                            XXLN-PCB                                      
054300                            XXBX-PCB                                      
054400                            XXBU-PCB                                      
054500                            4505-PCB                                      
054600                            WDB6-PCB                                      
054700                            RROT-ARTM-PCB                                 
054800                            RROT-ARTS-PCB                                 
054900                            PRNO-3107-PCB                                 
055000                            PRQU-WDG2-PCB                                 
055100                            PRQU-WDC7-PCB                                 
055200                            PRQU-SJKO-WDK6-PCB                            
055300                            GSAMFIL-PCB.                                  
055400 MAIN SECTION.                                                            
055500     ENTRY 'DLITCBL' USING  MSG-PCB                                       
055600                            PRQRY-PCB                                     
055700                            ARTC-PCB                                      
055800                            WDK7-PCB                                      
055900                            ORDP-PCB                                      
056000                            ARTM-PCB                                      
056100                            XXLN-PCB                                      
056200                            XXBU-PCB                                      
056300                            XXBX-PCB                                      
056400                            4505-PCB                                      
056500                            WDB6-PCB                                      
056600                            RROT-ARTM-PCB                                 
056700                            RROT-ARTS-PCB                                 
056800                            PRNO-3107-PCB                                 
056900                            PRQU-WDG2-PCB                                 
057000                            PRQU-WDC7-PCB                                 
057100                            PRQU-SJKO-WDK6-PCB                            
057200                            GSAMFIL-PCB.                                  
057300                                                                          
057400     PERFORM A-INIT                                                       
057500                                                                          
057600     PERFORM C-BEHANDLA-SORTERADE-POSTER                                  
057700                                                                          
057800     PERFORM Z-FINIT                                                      
057900     MOVE ZERO TO RETURN-CODE                                             
058000     GOBACK                                                               
058100     .                                                                    
058200     EJECT                                                                
058300 A-INIT SECTION.                                                          
058400                                                                          
058500     MOVE +416               TO MAX-GSAM                                  
058600***** OBS  GSAM-LÄNGDEN SKALL VARA POSTLÄNGDEN PLUS 2 BYTES (VB)**        
058700                                                                          
058800     OPEN INPUT  INFIL                                                    
058900                                                                          
059000     PERFORM IMS-RESTART                                                  
059100                                                                          
059200     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
059300                                                                          
059400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
059500     MOVE D-AAR    TO W-AR W-D-AAR                                        
059600     MOVE D-MAANAD TO W-MANAD                                             
059700     MOVE D-DAG    TO W-DAG                                               
059800     MOVE D-VECKA  TO W-D-VECKA                                           
059900     MOVE D-DAGNR  TO W-D-DAGNR                                           
060000                                                                          
060100     ACCEPT WS-TIKLOCK-X     FROM TIME                                    
060200     ACCEPT DAGENS-DATUM     FROM DATE                                    
060300     .                                                                    
060400     EJECT                                                                
060500 C-BEHANDLA-SORTERADE-POSTER SECTION.                                     
060600                                                                          
060700     MOVE NEJ                TO INFIL-EOF                                 
060800     MOVE ZERO               TO WS-ANT-POST-IN                            
060900                                                                          
061000     PERFORM CA-INIT-BMP                                                  
061100                                                                          
061200     PERFORM S06-LAS-INFIL                                                
061300                                                                          
061400     PERFORM UNTIL (INFIL-EOF = JA)                                       
061500                                                                          
061600         PERFORM UNTIL (INFIL-EOF = JA                                    
061700                    OR  FOR-IDARTNR > ZERO)                               
061800           PERFORM S05-SKRIV-UTPOST                                       
061900           PERFORM S09-SPARA-CHKPID                                       
062000           PERFORM S06-LAS-INFIL                                          
062100         END-PERFORM                                                      
062200                                                                          
062300         MOVE FOR-IDDC    TO IDDC-BRYT                                    
062400         MOVE FOR-IDARTNR TO IDARTNR-BRYT                                 
062500         MOVE NEJ         TO SW-2232-LAST                                 
062600                                                                          
062700         PERFORM CC-LAS-ARTIKEL                                           
062800                                                                          
062900         PERFORM UNTIL (INFIL-EOF = JA                                    
063000                    OR FOR-IDDC    NOT = IDDC-BRYT                        
063100                    OR FOR-IDARTNR NOT = IDARTNR-BRYT)                    
063200                                                                          
063300             EVALUATE TRUE                                                
063400                                                                          
063500               WHEN FOR-KDRADERS = +1                                     
063600                 PERFORM CE-ERSATT-RAD                                    
063700                                                                          
063800               WHEN FOR-KDTILLK = +1                                      
063900                 PERFORM CD-BERAKNA-DISP-SALDO                            
064000                 PERFORM CF-TILLKOMMANDE-RAD                              
064100                                                                          
064200               WHEN OTHER                                                 
064300                 DISPLAY 'LASSI**HÄR FANNS CG-LTK-SECTION'                
064400                 DISPLAY 'HIT BORDE VI INTE HAMNAT!!!'                    
064500                 MOVE 'LASSIS KOMM. SE DISPLAYER NEDAN'                   
064600                             TO FELTEXT                                   
064700                 CALL ABEND USING RKOD-ABEND-MED-DUMP                     
064800             END-EVALUATE                                                 
064900                                                                          
065000             PERFORM S09-SPARA-CHKPID                                     
065100             PERFORM S06-LAS-INFIL                                        
065200                                                                          
065300         END-PERFORM                                                      
065400                                                                          
065500         PERFORM CH-UPPDATERA-ARTIKEL-SALDO                               
065600                                                                          
065700*---------------------------- BRYTNING IDDC/ARTNR HAR SKETT.              
065800*                             I DET FALL IN-FILEN EJ ÄR SLUT;             
065900*                              TAS CHECKPOINT.                            
066000*                             VID EOF IN-FIL;                             
066100*                              TAS INGEN CHECKPOINT, EFTERSOM             
066200*                              NORMALT PGM-AVSLUT SNART SKALL SKE.        
066300           IF  INFIL-EOF = NEJ                                            
066400             PERFORM CI-TAG-CHECKPOINT                                    
066500           END-IF                                                         
066600                                                                          
066700     END-PERFORM                                                          
066800     .                                                                    
066900     EJECT                                                                
067000 CA-INIT-BMP SECTION.                                                     
067100                                                                          
067200     MOVE NEJ                TO XRST-FIL-EOF                              
067300     MOVE ZERO               TO WS-ANT-POST-XRST                          
067400                                WS-ANT-POST-GSAM                          
067500                                                                          
067600                                                                          
067700     PERFORM IMS-GSAMFIL-OPEN                                             
067800     PERFORM IMS-GHU-XXLN-4554                                            
067900                                                                          
068000     IF SEGMENT-FINNS                                                     
068100                                                                          
068200       IF  XXLN-4554-KVPOST-UT > ZERO                                     
068300                                                                          
068400*--------------------------- XRST-FIL KOPIERAS TILL GSAMFILEN             
068500*                            FRAM TILL CHECKPOINT-LÄGE                    
068600         OPEN INPUT  XRST-FIL                                             
068700                                                                          
068800         PERFORM CAA-LAS-XRST                                             
068900                                                                          
069000         PERFORM UNTIL (XRST-FIL-EOF = JA                                 
069100                    OR  WS-ANT-POST-XRST >= XXLN-4554-KVPOST-UT)          
069200           MOVE MAX-GSAM TO GSAM-LRECL                                    
069300           MOVE XRST-POST  TO GSAM-FOR-W440001                            
069400           PERFORM S07-SKRIV-GSAM                                         
069500           PERFORM CAA-LAS-XRST                                           
069600         END-PERFORM                                                      
069700                                                                          
069800         IF  XRST-FIL-EOF      = JA                                       
069900         OR  WS-ANT-POST-XRST  NOT = XXLN-4554-KVPOST-UT                  
070000         OR  XRST-FOR-IDDISTR  NOT = XXLN-4554-IDDISTR-UT                 
070100         OR  XRST-FOR-IDKUNDNR NOT = XXLN-4554-IDKUNDNR-UT                
070200         OR  XRST-FOR-IDKUNDRF NOT = XXLN-4554-IDKUNDRF-UT                
070300         OR  XRST-FOR-IDLOPNRE NOT = XXLN-4554-IDLOPNRE-UT                
070400         OR  XRST-FOR-IDKORTNR-ERS                                        
070500                               NOT = XXLN-4554-IDKORTNR-UT                
070600           DISPLAY 'W4402400: FEL I ÅTERSTARTEN, RESTART-FIL'             
070700           CALL ABEND USING    RKOD-ABEND-UTAN-DUMP                       
070800         END-IF                                                           
070900                                                                          
071000*------- HÄR KOPIERAS DEN XRST-POST TILL GSAM-FILEN,                      
071100*        SOM VID FÖREGÅENDE EXEKVERING, VAR DEN SISTA SOM                 
071200*        SKREVS PÅ GSAM-FILEN FÖRE SISTA CHKP.                            
071300                                                                          
071400         MOVE MAX-GSAM TO GSAM-LRECL                                      
071500         MOVE XRST-POST  TO GSAM-FOR-W440001                              
071600         PERFORM S07-SKRIV-GSAM                                           
071700                                                                          
071800         CLOSE XRST-FIL                                                   
071900                                                                          
072000       END-IF                                                             
072100                                                                          
072200       IF  XXLN-4554-KVPOST-IN > ZERO                                     
072300                                                                          
072400*--------------------------- INFIL LÄSES                                  
072500*                            FRAM TILL CHECKPOINT-LÄGE                    
072600                                                                          
072700         PERFORM S06-LAS-INFIL                                            
072800                                                                          
072900         PERFORM UNTIL (INFIL-EOF = JA                                    
073000                    OR  WS-ANT-POST-IN >= XXLN-4554-KVPOST-IN)            
073100           PERFORM S06-LAS-INFIL                                          
073200         END-PERFORM                                                      
073300                                                                          
073400         IF  INFIL-EOF      = JA                                          
073500         OR  WS-ANT-POST-IN   NOT = XXLN-4554-KVPOST-IN                   
073600         OR  FOR-IDDISTR      NOT = XXLN-4554-IDDISTR-IN                  
073700         OR  FOR-IDKUNDNR     NOT = XXLN-4554-IDKUNDNR-IN                 
073800         OR  FOR-IDKUNDRF     NOT = XXLN-4554-IDKUNDRF-IN                 
073900         OR  FOR-IDLOPNRE     NOT = XXLN-4554-IDLOPNRE-IN                 
074000         OR  FOR-IDKORTNR-ERS NOT = XXLN-4554-IDKORTNR-IN                 
074100           DISPLAY 'W4402400: FEL I ÅTERSTARTEN, IN-FIL'                  
074200           CALL ABEND USING  RKOD-ABEND-UTAN-DUMP                         
074300         END-IF                                                           
074400                                                                          
074500*------- NU ÄR DEN IN-POST INLÄST, SOM VID FÖREGÅENDE EXEKVERING          
074600*        "KONSUMERADES" NÄRMAST FÖRE SISTA CHECKPOINT.                    
074700                                                                          
074800       END-IF                                                             
074900     END-IF                                                               
075000     .                                                                    
075100     EJECT                                                                
075200 CAA-LAS-XRST SECTION.                                                    
075300                                                                          
075400     READ XRST-FIL                                                        
075500          AT END MOVE JA TO XRST-FIL-EOF                                  
075600     END-READ                                                             
075700     IF XRST-FIL-EOF = NEJ                                                
075800        MOVE 'W44024'    TO POSTSUM-FDNAMN                                
075900        MOVE 'W44024D2'  TO POSTSUM-DDNAMN2                               
076000        MOVE 'XRST'      TO POSTSUM-TRANSTYP                              
076100        CALL POSTSUM USING POSTSUM-PARM                                   
076200        ADD +1           TO WS-ANT-POST-XRST                              
076300     END-IF                                                               
076400     .                                                                    
076500     EJECT                                                                
076600 CC-LAS-ARTIKEL SECTION.                                                  
076700                                                                          
076800     MOVE    FOR-IDARTNR TO W-IDARTNR-ARTC                                
076900                            W-IDARTNR-WDK7                                
077000                                                                          
077100     PERFORM IMS-GHU-WLARTC11                                             
077200                                                                          
077300     MOVE FOR-IDDC        TO W-IDDC-WDK7                                  
077400                                                                          
077500     IF FOR-IDDC NOT = DCS-IDDC                                           
077600        MOVE FOR-IDDC TO W-IDDC-B6                                        
077700        PERFORM IMS-GU-WDB601                                             
077800     END-IF                                                               
077900                                                                          
078000     IF DCS-CDC                                                           
078100        PERFORM CCA-FLYTTA-ARTIKELINFO-K6                                 
078200*                                                                         
078300*    NOTERA - DENNA LÄSNING GÖRS FÖR ATT HÄMTA/UPPDAT. TPO-SALDO          
078400*             VILKET INTE FÖREKOMMER PÅ NDC !!!                           
078500        PERFORM CCD-LAS-ARTM                                              
078600     ELSE                                                                 
078700        IF DCS-NDC                                                        
078800           PERFORM CCB-FLYTTA-ARTIKELINFO-K6                              
078900           PERFORM IMS-GHU-WDK711                                         
079000           IF SEGMENT-FINNS                                               
079100             PERFORM CCC-FLYTTA-ARTIKELINFO-K7                            
079200           ELSE                                                           
079300             MOVE ALL '+'        TO WDK7-W005WDK7                         
079400             MOVE 'WDK711'       TO WDK7-IDSEGM                           
079500             MOVE W-IDARTNR-WDK7 TO WDK7-IDARTNR-KFB                      
079600             MOVE W-IDDC-WDK7    TO WDK7-IDDC-KFB                         
079700                                  WDK7-IDDC                               
079800                                                                          
079900             CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB          
080000                                               WDK7-PCB                   
080100                                                                          
080200             MOVE ZERO       TO KVRESS                                    
080300                                KVROS-DAG                                 
080400                                KVROS-BULK                                
080500                                KVUTRS                                    
080600           END-IF                                                         
080700        END-IF                                                            
080800     END-IF                                                               
080900     .                                                                    
081000     EJECT                                                                
081100 CCA-FLYTTA-ARTIKELINFO-K6 SECTION.                                       
081200                                                                          
081300     MOVE CLAG-KDERS      TO KDERS                                        
081400     MOVE CLAG-KVLS       TO KVLS                                         
081500     MOVE CLAG-KVRESS     TO KVRESS                                       
081600     MOVE CLAG-KVROS      TO KVROS                                        
081700     MOVE CLAG-KVSPANT    TO KVSPANT                                      
081800     MOVE CLAG-KVUTRS     TO KVUTRS                                       
081900     MOVE CLAG-TIDISPIN   TO RROTW-TIDISPIN                               
082000     .                                                                    
082100     SKIP2                                                                
082200 CCB-FLYTTA-ARTIKELINFO-K6 SECTION.                                       
082300                                                                          
082400     MOVE CLAG-KDERS      TO KDERS                                        
082500     MOVE CLAG-KVLS       TO KVLS                                         
082600     MOVE CLAG-KVSPANT    TO KVSPANT                                      
082700     MOVE CLAG-TIDISPIN   TO RROTW-TIDISPIN                               
082800     .                                                                    
082900     SKIP2                                                                
083000 CCC-FLYTTA-ARTIKELINFO-K7 SECTION.                                       
083100                                                                          
083200     MOVE SLAG-KVRESS        TO KVRESS                                    
083300     MOVE SLAG-KVROS-DAG     TO KVROS-DAG                                 
083400     MOVE SLAG-KVROS-BULK    TO KVROS-BULK                                
083500     MOVE SLAG-KVUTRS        TO KVUTRS                                    
083600                                                                          
083700     PERFORM IMS-GNP-WDK722                                               
083800     IF SEGMENT-FINNS                                                     
083900        MOVE XLAG-KVSPANT    TO KVSPANT                                   
084000     ELSE                                                                 
084100        IF DCS-CHINA                                                      
084200           MOVE ZERO         TO KVSPANT                                   
084300        END-IF                                                            
084400     END-IF                                                               
084500     .                                                                    
084600     EJECT                                                                
084700 CCD-LAS-ARTM SECTION.                                                    
084800                                                                          
084900     MOVE FOR-IDARTNR TO W-IDARTNR-ARTM                                   
085000     PERFORM IMS-GHU-WLARTM01                                             
085100                                                                          
085200     IF  SEGMENT-FINNS                                                    
085300       MOVE ART-SUTPO-TOT     TO SUTPO-TOT                                
085400     ELSE                                                                 
085500       MOVE ZERO              TO SUTPO-TOT                                
085600     END-IF                                                               
085700                                                                          
085800     MOVE SUTPO-TOT           TO WS-SUTPO-TOT-SPAR                        
085900     .                                                                    
086000     EJECT                                                                
086100 CD-BERAKNA-DISP-SALDO SECTION.                                           
086200                                                                          
086300     IF FOR-KDERS NOT = ZERO                                              
086400                                                                          
086500*      * DETTA FALL RANSONERAS EJ (DVS FÖRBRUKNING ÖNSKVÄRD)              
086600*      * ARTIKEL SKALL UTGÅ ELLER ERSÄTTNING PÅ GÅNG                      
086700                                                                          
086800       IF KVLS < ZERO                                                     
086900         MOVE ZERO         TO KVLS                                        
087000       END-IF                                                             
087100       IF KVRESS < ZERO                                                   
087200         MOVE ZERO         TO KVRESS                                      
087300       END-IF                                                             
087400       COMPUTE DISP-SALDO =   KVLS                                        
087500                            - KVUTRS                                      
087600                            - KVRESS                                      
087700                            - KVSPANT                                     
087800     ELSE                                                                 
087900*                                                                         
088000*    NOTERA - INGEN RANSONERING PÅ NDC !!!                                
088100       IF FOR-IDDC NOT = DCS-IDDC                                         
088200          MOVE FOR-IDDC TO W-IDDC-B6                                      
088300          PERFORM IMS-GU-WDB601                                           
088400       END-IF                                                             
088500       IF DCS-CDC                                                         
088600          PERFORM IMS-GU-WLARTC01                                         
088700          MOVE ART-ART-KDPRODSL  TO TEST-KDPRODSL                         
088800          IF KDPRODSL-BIMA                                                
088900            CONTINUE                                                      
089000          ELSE                                                            
089100             PERFORM CDA-KOLLA-OM-SATSORDER                               
089200             IF SATS-ORDER                                                
089300                CONTINUE                                                  
089400             ELSE                                                         
089500                PERFORM CDB-BER-RANS-DISP                                 
089600             END-IF                                                       
089700          END-IF                                                          
089800       ELSE                                                               
089900          MOVE ZERO    TO DISP-SALDO                                      
090000       END-IF                                                             
090100     END-IF                                                               
090200     .                                                                    
090300     EJECT                                                                
090400 CDA-KOLLA-OM-SATSORDER   SECTION.                                        
090500******************************************************************        
090600*    FÖR SATSARTIKLAR SKALL FULL TÄCKNINGE GÖRAS OM                       
090700*    SATSBEOV / TOTALA BEHOVET > 0,8                                      
090800******************************************************************        
090900                                                                          
091000     MOVE NEJ TO SW-SATSORDER                                             
091100                                                                          
091200     IF ART-ART-FLIART = JA                                               
091300        IF CLAG-FLLSRDEL = NEJ                                            
091400           MOVE JA TO SW-SATSORDER                                        
091500        ELSE                                                              
091600           COMPUTE W-TOT-BEHOV = CLAG-KVPB-SATS + CLAG-KVPB-SEP           
091700                                                                          
091800           PERFORM IMS-GU-WDK701                                          
091900           IF SEGMENT-FINNS                                               
092000              PERFORM IMS-GNP-WDK711                                      
092100              PERFORM UNTIL SEGMENT-SAKNAS                                
092200                 IF SLAG-IDLEVNR = '1441'                                 
092300                    COMPUTE W-TOT-BEHOV =                                 
092400                       W-TOT-BEHOV + SLAG-KVPB-REF + SLAG-KVPBREOI        
092500                 END-IF                                                   
092600                 PERFORM IMS-GNP-WDK711                                   
092700              END-PERFORM                                                 
092800           END-IF                                                         
092900                                                                          
093000           IF CLAG-KVPB-SATS > ZERO                                       
093100              IF (CLAG-KVPB-SATS / W-TOT-BEHOV) NOT < 0.8                 
093200                 MOVE JA  TO SW-SATSORDER                                 
093300              END-IF                                                      
093400           END-IF                                                         
093500        END-IF                                                            
093600                                                                          
093700        MOVE ZERO TO W-TOT-BEHOV                                          
093800     END-IF                                                               
093900     .                                                                    
094000 CDB-BER-RANS-DISP SECTION.                                               
094100*                                                                         
094200*    BERÄKNA RANSONERAD DISPONIBEL KVANT.                                 
094300*                                                                         
094400     MOVE SPACE             TO RROT-W440RROT                              
094500                                                                          
094600     MOVE FOR-IDARTNR       TO RROT-IDARTNR                               
094700     MOVE 1                 TO RROT-KDLTK                                 
094800     MOVE KDERS             TO RROT-KDERS     (1)                         
094900     MOVE KVLS              TO RROT-KVLS      (1)                         
095000     MOVE KVRESS            TO RROT-KVRESS    (1)                         
095100     MOVE KVSPANT           TO RROT-KVSPANT   (1)                         
095200     MOVE KVUTRS            TO RROT-KVUTRS    (1)                         
095300     MOVE KVROS             TO RROT-KVROS     (1)                         
095400     MOVE RROTW-TIDISPIN    TO RROT-TIDISPIN  (1)                         
095500     MOVE CLAG-KVPB-SATS    TO RROT-KVPB-SATS (1)                         
095600     MOVE CLAG-KVPB-SEP     TO RROT-KVPB-SEP  (1)                         
095700     MOVE CLAG-KVPB-TPO     TO RROT-KVPB-TPO  (1)                         
095800     MOVE CLAG-REDIRLEV     TO RROT-REDIRLEV  (1)                         
095900                                                                          
096000     MOVE +0                TO RROT-KDERS     (2)                         
096100                               RROT-KVLS      (2)                         
096200                               RROT-KVRESS    (2)                         
096300                               RROT-KVSPANT   (2)                         
096400                               RROT-KVUTRS    (2)                         
096500                               RROT-KVROS     (2)                         
096600                               RROT-TIDISPIN  (2)                         
096700                               RROT-KVPB-SATS (2)                         
096800                               RROT-KVPB-SEP  (2)                         
096900                               RROT-KVPB-TPO  (2)                         
097000                               RROT-REDIRLEV  (2)                         
097100                                                                          
097200     CALL W440RROT          USING RROT-W440RROT                           
097300                                  RROT-ARTM-PCB                           
097400                                  RROT-ARTS-PCB                           
097500                                                                          
097600     MOVE RROT-KVDISP (1)   TO DISP-SALDO                                 
097700     .                                                                    
097800     EJECT                                                                
097900 CE-ERSATT-RAD SECTION.                                                   
098000                                                                          
098100     IF FOR-KDSTARAD = '1'                                                
098200        PERFORM S13-TA-BORT-LARMQ                                         
098300        IF FOR-FLTPOBEK = JA                                              
098400           PERFORM S10-SUB-SUTPO                                          
098500        END-IF                                                            
098600     ELSE                                                                 
098700        IF DCS-CDC                                                        
098800           COMPUTE KVROS = KVROS - FOR-KVART                              
098900        ELSE                                                              
099000           IF DCS-NDC                                                     
099100              IF FOR-KDORDKL = 0 OR 1                                     
099200                 COMPUTE KVROS-DAG = KVROS-DAG - FOR-KVART                
099300              ELSE                                                        
099400                 COMPUTE KVROS-BULK = KVROS-BULK - FOR-KVART              
099500              END-IF                                                      
099600           END-IF                                                         
099700        END-IF                                                            
099800     END-IF                                                               
099900     MOVE FOR-IDDISTR        TO W-IDDISTR                                 
100000                                TEST-IDDISTR                              
100100     MOVE FOR-IDKUNDNR       TO W-IDKUNDNR                                
100200     MOVE FOR-IDKUNDRF       TO W-IDKUNDRF                                
100300     MOVE FOR-IDARTNR        TO W-IDARTNR                                 
100400     MOVE FOR-IDLOPNR        TO W-IDLOPNR                                 
100500     MOVE FOR-IDDC           TO W-IDDC                                    
100600     PERFORM IMS-GHU-WLORDP01                                             
100700*** FIX CO                                                                
100800     IF SEGMENT-SAKNAS                                                    
100900       DISPLAY 'A5-SAKNAS ' FOR-IDDISTR FOR-IDKUNDNR FOR-IDKUNDRF         
101000                            FOR-IDARTNR FOR-IDLOPNR FOR-IDDC              
101100     ELSE                                                                 
101200     PERFORM IMS-DLET-WLORDP01                                            
101300     END-IF                                                               
101400                                                                          
101500******************************************************************        
101600*                                                                         
101700*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
101800*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
101900*                                                                         
102000******************************************************************        
102100                                                                          
102200     MOVE FOR-IDDISTR         TO W-TP4TRAN-IDDISTR                        
102300                                                                          
102400     PERFORM DB2-SELECT-TP4TRAN                                           
102500                                                                          
102600     IF DIST35-REFILL                                                     
102700     OR DIST35-REFILL-INOM-NDC                                            
102800     OR DIST35-NONVCC-NONVCC-REFILL                                       
102900     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
103000     OR DIST35-NA-TRANSFER                                                
103100     OR DIST35-NA-NDC-RETURNS                                             
103200     OR DIST35-PACIFIC-TRANSFER                                           
103300     OR DIST35-REFILL-INOM-JP                                             
103400     OR DIST35-CN-TRANSFER                                                
103500     OR DIST35-NONVCC-VCC-REFILL                                          
103600     OR DIST35-NONVCC-VCC-TRANSFER                                        
103700     OR RADER-FINNS                                                       
103800        MOVE FOR-IDARTNR      TO   W-IDARTNR-WDK7                         
103900                                                                          
104000       IF RADER-FINNS                                                     
104100         MOVE TP4TRAN-IDDC-REC TO W-IDDC-WDK7                             
104200       ELSE                                                               
104300         SEARCH ALL DIST57-REFILL-DC                                      
104400           AT END                                                         
104500             MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                         
104600                               TO FELTEXT                                 
104700             CALL FELLOG                                                  
104800           WHEN DIST57-SOK-IDDISTR(DIST57-IX) = FOR-IDDISTR               
104900             MOVE DIST57-REFILL-TO-DC(DIST57-IX)                          
105000                               TO W-IDDC-WDK7                             
105100         END-SEARCH                                                       
105200       END-IF                                                             
105300                                                                          
105400       PERFORM IMS-GHU-WDK711                                             
105500       COMPUTE SLAG-KVBEART = SLAG-KVBEART - FOR-KVART                    
105600       PERFORM IMS-REPL-WDK711                                            
105700     ELSE                                                                 
105800       IF DIST35-NONVCC-CDC-REFILL                                        
105900          MOVE FOR-IDARTNR    TO   W-IDARTNR-ARTC                         
106000          PERFORM IMS-GHU-WLARTC11                                        
106100          COMPUTE CLAG-KVBEART = CLAG-KVBEART - FOR-KVART                 
106200          PERFORM IMS-REPL-WLARTC11                                       
106300       END-IF                                                             
106400     END-IF                                                               
106500                                                                          
106600     PERFORM S05-SKRIV-UTPOST                                             
106700     .                                                                    
106800     EJECT                                                                
106900 CF-TILLKOMMANDE-RAD SECTION.                                             
107000                                                                          
107100     IF FOR-KDRESTR = +0 OR +47 OR +91                                    
107200          IF FOR-KDSTARAD = '1'                                           
107300*    NOTERA - LARMKÖ/TPOSALDO EJ AKTUELLT FÖR NDC                         
107400             IF DCS-CDC                                                   
107500                PERFORM CFA-UPPD-LARMQ-TILLK                              
107600                IF FOR-FLTPOBEK = JA                                      
107700                   PERFORM S11-ADD-SUTPO                                  
107800                END-IF                                                    
107900             END-IF                                                       
108000          ELSE                                                            
108100             IF DCS-CDC                                                   
108200                COMPUTE KVROS = KVROS + FOR-KVART                         
108300             ELSE                                                         
108400                IF DCS-NDC                                                
108500                   IF FOR-KDORDKL = 0 OR 1                                
108600                      COMPUTE KVROS-DAG = KVROS-DAG + FOR-KVART           
108700                   ELSE                                                   
108800                      COMPUTE KVROS-BULK = KVROS-BULK + FOR-KVART         
108900                   END-IF                                                 
109000                END-IF                                                    
109100             END-IF                                                       
109200          END-IF                                                          
109300                                                                          
109400          IF FOR-KDSTARAD = '2'                                           
109500             IF DISP-SALDO > +0 AND (FOR-KDRESTR = +0 OR +47)             
109600                PERFORM CFB-SKAPA-4505                                    
109700                IF DISP-SALDO < FOR-KVART                                 
109800                   MOVE DISP-SALDO TO TACKBART                            
109900                ELSE                                                      
110000                   MOVE FOR-KVART  TO TACKBART                            
110100                END-IF                                                    
110200                COMPUTE DISP-SALDO =                                      
110300                        DISP-SALDO - TACKBART                             
110400             END-IF                                                       
110500          END-IF                                                          
110600                                                                          
110700          IF FOR-KDSTARAD = '2'                                           
110800             PERFORM S04-REDIGERA-NYTT-RO-SEGMENT                         
110900             MOVE FOR-KDSTARAD TO RAD-KDSTARAD                            
111000             MOVE +0 TO RAD-TIRES FOR-TIRES                               
111100             MOVE FOR-KVART TO RAD-KVART RAD-KVRO                         
111200             PERFORM S02-LAS-NYA-ARTIKELREG                               
111300             PERFORM S03-ISRT-NYTT-RO-SEGMENT                             
111400                                                                          
111500******************************************************************        
111600*                                                                         
111700*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
111800*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
111900*                                                                         
112000******************************************************************        
112100                                                                          
112200             MOVE FOR-IDDISTR  TO W-TP4TRAN-IDDISTR                       
112300                                                                          
112400             PERFORM DB2-SELECT-TP4TRAN                                   
112500                                                                          
112600             MOVE FOR-IDDISTR TO TEST-IDDISTR                             
112700             IF DIST35-REFILL                                             
112800             OR DIST35-REFILL-INOM-NDC                                    
112900             OR DIST35-NONVCC-NONVCC-REFILL                               
113000             OR DIST35-NONVCC-NONVCC-TRANSFER                             
113100             OR DIST35-NA-TRANSFER                                        
113200             OR DIST35-NA-NDC-RETURNS                                     
113300             OR DIST35-PACIFIC-TRANSFER                                   
113400             OR DIST35-REFILL-INOM-JP                                     
113500             OR DIST35-CN-TRANSFER                                        
113600             OR DIST35-NONVCC-VCC-REFILL                                  
113700             OR DIST35-NONVCC-VCC-TRANSFER                                
113800             OR RADER-FINNS                                               
113900                MOVE FOR-IDARTNR TO W-IDARTNR-WDK7                        
114000                                                                          
114100               IF RADER-FINNS                                             
114200                 MOVE TP4TRAN-IDDC-REC TO W-IDDC-WDK7                     
114300               ELSE                                                       
114400                 SEARCH ALL DIST57-REFILL-DC                              
114500                   AT END                                                 
114600                     MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                 
114700                                       TO FELTEXT                         
114800                     CALL FELLOG                                          
114900                 WHEN DIST57-SOK-IDDISTR(DIST57-IX) = FOR-IDDISTR         
115000                     MOVE DIST57-REFILL-TO-DC(DIST57-IX)                  
115100                                       TO W-IDDC-WDK7                     
115200                 END-SEARCH                                               
115300               END-IF                                                     
115400               PERFORM IMS-GHU-WDK711                                     
115500               IF SEGMENT-FINNS                                           
115600                 COMPUTE SLAG-KVBEART = SLAG-KVBEART + FOR-KVART          
115700                 PERFORM IMS-REPL-WDK711                                  
115800               ELSE                                                       
115900                 MOVE ALL '+'        TO WDK7-W005WDK7                     
116000                 MOVE 'WDK711'       TO WDK7-IDSEGM                       
116100                 MOVE W-IDARTNR-WDK7 TO WDK7-IDARTNR-KFB                  
116200                 MOVE W-IDDC-WDK7    TO WDK7-IDDC-KFB                     
116300                                        WDK7-IDDC                         
116400                 MOVE FOR-KVART      TO WDK7-KVBEART                      
116500                                                                          
116600                 CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB               
116700                                                   ARTC-PCB               
116800                                                   WDK7-PCB               
116900               END-IF                                                     
117000             ELSE                                                         
117100               IF DIST35-NONVCC-CDC-REFILL                                
117200                 MOVE FOR-IDARTNR TO W-IDARTNR-ARTC                       
117300                 PERFORM IMS-GHU-WLARTC11                                 
117400                 COMPUTE CLAG-KVBEART = CLAG-KVBEART + FOR-KVART          
117500                 PERFORM IMS-REPL-WLARTC11                                
117600               END-IF                                                     
117700             END-IF                                                       
117800                                                                          
117900             PERFORM S05-SKRIV-UTPOST                                     
118000          ELSE                                                            
118100             PERFORM S04-REDIGERA-NYTT-RO-SEGMENT                         
118200             MOVE FOR-KDSTARAD TO RAD-KDSTARAD                            
118300             MOVE +0 TO RAD-TIRES FOR-TIRES                               
118400             MOVE FOR-KVART TO RAD-KVART                                  
118500             MOVE ZERO TO RAD-KVRO FOR-KVRO                               
118600             PERFORM S02-LAS-NYA-ARTIKELREG                               
118700             PERFORM S03-ISRT-NYTT-RO-SEGMENT                             
118800             PERFORM S05-SKRIV-UTPOST                                     
118900          END-IF                                                          
119000     ELSE                                                                 
119100          PERFORM S05-SKRIV-UTPOST                                        
119200     END-IF                                                               
119300     .                                                                    
119400     EJECT                                                                
119500 CFA-UPPD-LARMQ-TILLK SECTION.                                            
119600                                                                          
119700                                                                          
119800     IF  FOR-KDLARM > ZERO                                                
119900         PERFORM CFAA-SKAPA-LARM                                          
120000     END-IF                                                               
120100     .                                                                    
120200     EJECT                                                                
120300 CFAA-SKAPA-LARM SECTION.                                                 
120400                                                                          
120500       IF  SW-2232-LAST = NEJ                                             
120600*        * IDANSK-LARM HÄMTAS EN GÅNG PER ARTNR                           
120700         MOVE '2231'             TO W-2231-IDHTYP                         
120800         MOVE LOW-VALUE          TO W-2231-NYCKEL-VALFRI                  
120900         MOVE FOR-IDANSK         TO W-2232-IDANSK                         
121000         PERFORM IMS-GU-XXBX-2232                                         
121100         IF SEGMENT-FINNS                                                 
121200           MOVE XXBX-2232-IDANSK-LARM                                     
121300                                 TO WS-IDANSK-LARM                        
121400         ELSE                                                             
121500           MOVE ZERO             TO WS-IDANSK-LARM                        
121600         END-IF                                                           
121700         MOVE JA                 TO SW-2232-LAST                          
121800       END-IF                                                             
121900                                                                          
122000       MOVE '2223'               TO W-2223-IDHTYP                         
122100       MOVE WS-IDANSK-LARM       TO W-2223-IDANSK                         
122200       MOVE LOW-VALUE            TO W-2223-LOW-VALUE                      
122300       PERFORM IMS-GU-XXBU-2223                                           
122400                                                                          
122500*      * LARMKÖ-ROT LÄGGS UPP OM DEN SAKNAS                               
122600       IF  SEGMENT-SAKNAS                                                 
122700         MOVE W-2223-WDGX2223    TO XXBU-2223-WDGX2223                    
122800         PERFORM IMS-ISRT-XXBU-2223                                       
122900       END-IF                                                             
123000                                                                          
123100       MOVE FOR-TISENBEK-DAG     TO XXBU-2224-TISENBEK-DAG                
123200       MOVE FOR-TISENBEK-KL      TO XXBU-2224-TISENBEK-KL                 
123300       MOVE FOR-KDLARM           TO XXBU-2224-KDLARM                      
123400       MOVE FOR-IDARTNR          TO XXBU-2224-IDARTNR                     
123500       MOVE WC-CDC-SE            TO XXBU-2224-IDDC                        
123600       MOVE JA                   TO XXBU-2224-FLNYLARM                    
123700       MOVE FOR-IDDISTR          TO XXBU-2224-IDDISTR                     
123800       MOVE FOR-IDKUNDNR         TO XXBU-2224-IDKUNDNR                    
123900       MOVE '0000000   '         TO XXBU-2224-IDKUNDRF                    
124000       MOVE FOR-IDKUNDRF(1:5)    TO XXBU-2224-IDKUNDRF(3:5)               
124100       MOVE FOR-IDLOPNR          TO XXBU-2224-IDLOPNR                     
124200       MOVE FOR-TIREGDAT-LARM    TO XXBU-2224-TIREGDAT                    
124300       MOVE SPACE                TO XXBU-2224-IDTRANS                     
124400                                    XXBU-2224-KDMFSFOR                    
124500       MOVE ZERO                 TO XXBU-2224-IDKR                        
124600       MOVE SPACE                TO XXBU-2224-IDLEVNR                     
124700                                                                          
124800       PERFORM IMS-ISRT-XXBU-2224                                         
124900                                                                          
125000       PERFORM UNTIL (NOT SEGMENT-FINNS-REDAN)                            
125100         MOVE XXBU-2224-TISENBEK-KL TO WS-NXT-KL                          
125200         PERFORM CFAAA-NXT-SEKUND                                         
125300         MOVE WS-NXT-KL          TO XXBU-2224-TISENBEK-KL                 
125400         PERFORM IMS-ISRT-XXBU-2224                                       
125500       END-PERFORM                                                        
125600                                                                          
125700*      * DET EV. UPPRÄKNADE KLOCKSLAGET LÄGGS I POST-AREAN                
125800*        --> WDA5 FÅR SAMMA KL SOM XXBU-2224.                             
125900       MOVE XXBU-2224-TISENBEK-KL TO FOR-TISENBEK-KL                      
126000     .                                                                    
126100     EJECT                                                                
126200 CFAAA-NXT-SEKUND SECTION.                                                
126300*                                                                         
126400*    RÄKNAR UPP TILL NÄSTA SEKUND.                                        
126500*    GÅR ALDRIG ÖVER DYGNS-GRÄNS.                                         
126600*    NÄSTA SEKUND EFTER 23.59.59 GER 00.00.00 INOM SAMMA DYGN.            
126700*                                                                         
126800     ADD 1                   TO WS-NXT-KL-SS                              
126900     IF  WS-NXT-KL-SS > 59                                                
127000       MOVE ZERO             TO WS-NXT-KL-SS                              
127100       ADD 1                 TO WS-NXT-KL-MM                              
127200       IF  WS-NXT-KL-MM > 59                                              
127300         MOVE ZERO           TO WS-NXT-KL-MM                              
127400         ADD 1               TO WS-NXT-KL-TT                              
127500         IF  WS-NXT-KL-TT > 23                                            
127600           MOVE ZERO         TO WS-NXT-KL-TT                              
127700         END-IF                                                           
127800       END-IF                                                             
127900     END-IF                                                               
128000     .                                                                    
128100     EJECT                                                                
128200 CFB-SKAPA-4505 SECTION.                                                  
128300                                                                          
128400     MOVE '4505'               TO 4505-4505-IDHTYP                        
128500     MOVE FOR-IDDC             TO 4505-4505-IDDC                          
128600                                  W-4505-IDDC                             
128700     PERFORM IMS-ISRT-4505-4505                                           
128800                                                                          
128900     MOVE FOR-IDARTNR          TO 4505-4506-IDARTNR                       
129000     MOVE +21                  TO 4505-4506-KDTAKORS                      
129100     MOVE ZERO                 TO 4505-4506-KVANTMOT                      
129200     PERFORM IMS-ISRT-4505-4506                                           
129300     .                                                                    
129400     EJECT                                                                
129500 CH-UPPDATERA-ARTIKEL-SALDO SECTION.                                      
129600                                                                          
129700     IF IDDC-BRYT NOT = DCS-IDDC                                          
129800        MOVE IDDC-BRYT TO W-IDDC-B6                                       
129900        PERFORM IMS-GU-WDB601                                             
130000     END-IF                                                               
130100     IF DCS-CDC                                                           
130200        PERFORM IMS-GHU-WLARTC11                                          
130300        PERFORM CHA-REDIGERA-ARTIKEL-SEGM-K6                              
130400        PERFORM IMS-REPL-WLARTC11                                         
130500        PERFORM CHC-UPPD-ARTM                                             
130600     ELSE                                                                 
130700        MOVE    IDARTNR-BRYT TO W-IDARTNR-WDK7                            
130800        MOVE    IDDC-BRYT    TO W-IDDC-WDK7                               
130900                                                                          
131000        PERFORM IMS-GHU-WDK711                                            
131100        IF SEGMENT-FINNS                                                  
131200          MOVE KVRESS       TO SLAG-KVRESS                                
131300          MOVE KVROS-DAG    TO SLAG-KVROS-DAG                             
131400          MOVE KVROS-BULK   TO SLAG-KVROS-BULK                            
131500          PERFORM IMS-REPL-WDK711                                         
131600        ELSE                                                              
131700          MOVE ALL '+'      TO WDK7-W005WDK7                              
131800          MOVE 'WDK711'     TO WDK7-IDSEGM                                
131900          MOVE IDARTNR-BRYT TO WDK7-IDARTNR-KFB                           
132000          MOVE IDDC-BRYT    TO WDK7-IDDC-KFB                              
132100                               WDK7-IDDC                                  
132200          MOVE KVRESS       TO WDK7-KVRESS                                
132300          MOVE KVROS-DAG    TO WDK7-KVROS-DAG                             
132400          MOVE KVROS-BULK   TO WDK7-KVROS-BULK                            
132500                                                                          
132600          CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB             
132700                                            WDK7-PCB                      
132800        END-IF                                                            
132900     END-IF                                                               
133000     .                                                                    
133100     EJECT                                                                
133200 CHA-REDIGERA-ARTIKEL-SEGM-K6 SECTION.                                    
133300                                                                          
133400     IF KVROS > CLAG-KVROS                                                
133500       MOVE W-RODATUM TO CLAG-TIRODAT                                     
133600     END-IF                                                               
133700     MOVE KVRESS      TO CLAG-KVRESS                                      
133800     MOVE KVROS       TO CLAG-KVROS                                       
133900     .                                                                    
134000     SKIP2                                                                
134100 CHC-UPPD-ARTM SECTION.                                                   
134200                                                                          
134300     IF  SUTPO-TOT NOT = WS-SUTPO-TOT-SPAR                                
134400                                                                          
134500*      * SUTPO-TOT HAR FÖRÄNDRATS.                                        
134600*      * WLARTM01 MÅSTE FINNAS.                                           
134700*      * OM DEN INTE FANNS VID KÖRNINGENS START SÅ HAR DEN BLIVIT         
134800*      * UPPLAGD I SAMBAND MED ATT FÖRSTA WLARTM11 LADES UPP.             
134900                                                                          
135000       MOVE IDARTNR-BRYT TO W-IDARTNR-ARTM                                
135100       PERFORM IMS-GHU-WLARTM01                                           
135200                                                                          
135300       MOVE SUTPO-TOT    TO ART-SUTPO-TOT                                 
135400       PERFORM IMS-REPL-WLARTM01                                          
135500                                                                          
135600     END-IF                                                               
135700     .                                                                    
135800     EJECT                                                                
135900 CI-TAG-CHECKPOINT SECTION.                                               
136000                                                                          
136100     PERFORM IMS-GHU-XXLN-4554                                            
136200                                                                          
136300     MOVE SPACE              TO XXLN-4554-WDGX4554                        
136400     MOVE '1'                TO XXLN-4554-KDSEGKEY                        
136500                                                                          
136600     MOVE WS-CHKPID-KVPOST   TO XXLN-4554-KVPOST-IN                       
136700     MOVE WS-CHKPID-IDDISTR  TO XXLN-4554-IDDISTR-IN                      
136800     MOVE WS-CHKPID-IDKUNDNR TO XXLN-4554-IDKUNDNR-IN                     
136900     MOVE WS-CHKPID-IDKUNDRF TO XXLN-4554-IDKUNDRF-IN                     
137000     MOVE WS-CHKPID-IDLOPNRE TO XXLN-4554-IDLOPNRE-IN                     
137100     MOVE WS-CHKPID-IDKORTNR-ERS                                          
137200                             TO XXLN-4554-IDKORTNR-IN                     
137300                                                                          
137400     MOVE WS-ANT-POST-GSAM   TO XXLN-4554-KVPOST-UT                       
137500     MOVE GSAM-FOR-IDDISTR   TO XXLN-4554-IDDISTR-UT                      
137600     MOVE GSAM-FOR-IDKUNDNR  TO XXLN-4554-IDKUNDNR-UT                     
137700     MOVE GSAM-FOR-IDKUNDRF  TO XXLN-4554-IDKUNDRF-UT                     
137800     MOVE GSAM-FOR-IDLOPNRE  TO XXLN-4554-IDLOPNRE-UT                     
137900     MOVE GSAM-FOR-IDKORTNR-ERS                                           
138000                             TO XXLN-4554-IDKORTNR-UT                     
138100                                                                          
138200     ACCEPT WS-TS-TIAAMMDD   FROM DATE                                    
138300     MOVE WS-TS-TIAAMMDD     TO XXLN-4554-TIUPPDAT                        
138400     ACCEPT WS-TS-TIKLOCK    FROM TIME                                    
138500     MOVE WS-TS-TIKLOCK      TO XXLN-4554-TIUPPTID                        
138600                                                                          
138700     IF  SEGMENT-FINNS                                                    
138800       PERFORM IMS-REPL-XXLN                                              
138900     ELSE                                                                 
139000       PERFORM IMS-ISRT-XXLN-4554                                         
139100     END-IF                                                               
139200                                                                          
139300     MOVE CHKP-ID            TO MSG-IO-AREA                               
139400     PERFORM IMS-CHECKPOINT                                               
139500     .                                                                    
139600     EJECT                                                                
139700                                                                          
139800 Z-FINIT SECTION.                                                         
139900                                                                          
140000     CLOSE  INFIL                                                         
140100                                                                          
140200     PERFORM ZA-FINIT-BMP                                                 
140300                                                                          
140400     MOVE 'S' TO POSTSUM-OPKOD                                            
140500     CALL POSTSUM USING POSTSUM-PARM                                      
140600     .                                                                    
140700     EJECT                                                                
140800 ZA-FINIT-BMP SECTION.                                                    
140900*                                                                         
141000*    HTR FÖR CHKP-INFO TAS BORT                                           
141100*                                                                         
141200                                                                          
141300     PERFORM IMS-GHU-XXLN-4554                                            
141400                                                                          
141500     IF  SEGMENT-FINNS                                                    
141600       PERFORM IMS-DLET-XXLN                                              
141700     END-IF                                                               
141800                                                                          
141900     PERFORM IMS-GSAMFIL-CLOSE                                            
142000     .                                                                    
142100     EJECT                                                                
142200 S02-LAS-NYA-ARTIKELREG SECTION.                                          
142300*                                                                         
142400*    OM ROTEN WLARTM01 SAKNAS LÄGGS DEN UPP NOLLAD.                       
142500*                                                                         
142600     MOVE FOR-IDARTNR TO W-IDARTNR-ARTM                                   
142700     PERFORM IMS-GU-WLARTM01                                              
142800                                                                          
142900     IF  SEGMENT-SAKNAS                                                   
143000         MOVE FOR-IDARTNR    TO ART-IDARTNR                               
143100         MOVE ZERO           TO ART-KVOFFERT                              
143200         MOVE ZERO           TO ART-KVOKS-BULK                            
143300         MOVE ZERO           TO ART-KVOKS-DAG                             
143400         MOVE ZERO           TO ART-KVOKS-VOR                             
143500         MOVE ZERO           TO ART-KVPREAVB-BULK                         
143600         MOVE ZERO           TO ART-KVPREAVB-DAG                          
143700         MOVE ZERO           TO ART-KVPREAVB-VOR                          
143800         MOVE ZERO           TO ART-KVPRERO-BULK                          
143900         MOVE ZERO           TO ART-KVPRERO-DAG                           
144000         MOVE 1              TO ART-RERF-ART                              
144100         MOVE ZERO           TO ART-SUTPO-TOT                             
144200         MOVE SPACE          TO ART-FILLER                                
144300         PERFORM IMS-ISRT-WLARTM01                                        
144400     END-IF                                                               
144500     .                                                                    
144600     EJECT                                                                
144700                                                                          
144800 S03-ISRT-NYTT-RO-SEGMENT SECTION.                                        
144900                                                                          
145000     PERFORM IMS-ISRT-WLORDP01                                            
145100     PERFORM UNTIL SEGMENT-FINNS                                          
145200        ADD +1 TO RAD-IDLOPNR                                             
145300        PERFORM IMS-ISRT-WLORDP01                                         
145400     END-PERFORM                                                          
145500     MOVE RAD-IDLOPNR TO FOR-IDLOPNR                                      
145600     .                                                                    
145700     EJECT                                                                
145800 S04-REDIGERA-NYTT-RO-SEGMENT SECTION.                                    
145900                                                                          
146000     MOVE FOR-BERADREF           TO RAD-BERADREF                          
146100     MOVE FOR-IDANSK             TO RAD-IDANSK                            
146200     MOVE FOR-IDARTNR            TO RAD-IDARTNR                           
146300     MOVE FOR-IDDISTR            TO RAD-IDDISTR                           
146400                                    TEST-IDDISTR                          
146500     MOVE FOR-IDKONTO            TO RAD-IDKONTO                           
146600     MOVE FOR-IDKST              TO RAD-IDKST                             
146700     MOVE FOR-IDANALYS           TO RAD-IDANALYS                          
146800     MOVE FOR-IDKUNDNR           TO RAD-IDKUNDNR                          
146900     MOVE FOR-IDKUNDRF           TO RAD-IDKUNDRF                          
147000     MOVE FOR-IDDC               TO RAD-IDDC                              
147100     MOVE FOR-IDDC-RO            TO RAD-IDDC-RO                           
147200     MOVE FOR-KDOI               TO RAD-KDOI                              
147300     MOVE FOR-CLEARGROUP         TO RAD-CLEARGROUP                        
147400     MOVE FOR-KDDSP              TO RAD-KDDSP                             
147500     MOVE FOR-KDFAKTYP           TO RAD-KDFAKTYP                          
147600     MOVE FOR-KDFRAKT            TO RAD-KDFRAKT                           
147700     MOVE FOR-KDKVBRYT           TO RAD-KDKVBRYT                          
147800     MOVE FOR-KDORDKL            TO RAD-KDORDKL                           
147900     MOVE FOR-KDPRODSL           TO RAD-KDPRODSL                          
148000     MOVE FOR-KDRAPRIO           TO RAD-KDRAPRIO                          
148100     MOVE FOR-KDROO              TO RAD-KDROO                             
148200     MOVE FOR-KDTPOTYP           TO RAD-KDTPOTYP                          
148300     MOVE FOR-KDVRINFO           TO RAD-KDVRINFO                          
148400     MOVE FOR-PRARTNTO           TO RAD-PRARTNTO                          
148500     MOVE FOR-PRAVCOST           TO RAD-PRAVCOST                          
148600     MOVE FOR-KDROPACK           TO RAD-KDROPACK                          
148700     MOVE FOR-IDARBREF           TO RAD-IDARBREF                          
148800     MOVE FOR-REKSIFFR           TO RAD-REKSIFFR                          
148900     MOVE FOR-TIREGDAT           TO RAD-TIREGDAT                          
149000     MOVE FOR-TIRODAT            TO RAD-DARODAT                           
149100     IF FOR-TIRODAT NOT = ZERO                                            
149200       IF FOR-TIRODAT < 500000                                            
149300         MOVE 20                 TO RAD-DARODAT (1:2)                     
149400       ELSE                                                               
149500         IF FOR-TIRODAT < 999999                                          
149600           MOVE 19               TO RAD-DARODAT (1:2)                     
149700         ELSE                                                             
149800           MOVE 99999999         TO RAD-DARODAT                           
149900         END-IF                                                           
150000       END-IF                                                             
150100     END-IF                                                               
150200     MOVE FOR-TITPO              TO RAD-TITPO                             
150300     MOVE FOR-KDPRTYP            TO RAD-KDPRTYP                           
150400     MOVE FOR-BEVOLREF           TO RAD-BEVOLREF                          
150500     MOVE FOR-FLINVEST           TO RAD-FLINVEST                          
150600     MOVE FOR-FLPRTILL           TO RAD-FLPRTILL                          
150700     MOVE FOR-FLTPOBEK           TO RAD-FLTPOBEK                          
150800     MOVE FOR-BEKUNDRF           TO RAD-BEKUNDRF                          
150900     MOVE FOR-IDKAMPRF           TO RAD-IDKAMPRF                          
151000     MOVE FOR-IDLEVNR            TO RAD-IDLEVNR                           
151100     MOVE FOR-IDSYSTEM           TO RAD-IDSYSTEM                          
151200     MOVE FOR-KVBEART-Q          TO RAD-KVBEART-Q                         
151300     MOVE FOR-TIREGTID           TO RAD-TIREGTID                          
151400     MOVE FOR-TISENBEK-DAG       TO RAD-DASENDAT                          
151500     IF FOR-TISENBEK-DAG NOT = ZERO                                       
151600       IF FOR-TISENBEK-DAG < 500000                                       
151700         MOVE 20                 TO RAD-DASENDAT (1:2)                    
151800       ELSE                                                               
151900         IF FOR-TISENBEK-DAG < 999999                                     
152000           MOVE 19               TO RAD-DASENDAT (1:2)                    
152100         ELSE                                                             
152200           MOVE 99999999         TO RAD-DASENDAT                          
152300         END-IF                                                           
152400       END-IF                                                             
152500     END-IF                                                               
152600     MOVE FOR-TISENBEK-KL        TO RAD-TISENBEK-KL                       
152700     MOVE '00000     '           TO RAD-IDKUNDRF-LEV                      
152800     MOVE +0                     TO RAD-TIAVBOKN                          
152900     IF FOR-KDTILLK = +1                                                  
153000        MOVE 'J' TO RAD-FLERS                                             
153100        MOVE +3  TO RAD-KDORDING                                          
153200     ELSE                                                                 
153300        MOVE 'N' TO RAD-FLERS                                             
153400        MOVE FOR-KDORDING        TO RAD-KDORDING                          
153500     END-IF                                                               
153600     MOVE +1                     TO RAD-IDLOPNR                           
153700*    INITIALIZE                 RAD-DEAL-PR-LINE                          
153800     MOVE FOR-DEAL-PR-LINE      TO RAD-DEAL-PR-LINE                       
153900     IF DIST79-DEALER-PRICE                                               
154000       IF (FOR-KDTILLK = 1 AND FOR-KDSTARAD NOT = '1')                    
154100         PERFORM S20-KOMPLETTERA-PRIS                                     
154200       END-IF                                                             
154300     END-IF                                                               
154400     MOVE FOR-KDORDTYP-LDC       TO RAD-KDORDTYP-LDC                      
154500     MOVE FOR-TIREPDAT           TO RAD-TIREPDAT                          
154600     MOVE FOR-IDKUNDRF-WIP       TO RAD-IDKUNDRF-WIP                      
154700                                                                          
154800     .                                                                    
154900     EJECT                                                                
155000 S05-SKRIV-UTPOST SECTION.                                                
155100                                                                          
155200     MOVE MAX-GSAM TO GSAM-LRECL                                          
155300     MOVE IN-POST  TO GSAM-FOR-W440001                                    
155400     PERFORM S07-SKRIV-GSAM                                               
155500     .                                                                    
155600     EJECT                                                                
155700 S06-LAS-INFIL SECTION.                                                   
155800                                                                          
155900     READ INFIL INTO IN-POST                                              
156000        AT END MOVE JA TO INFIL-EOF                                       
156100     END-READ                                                             
156200                                                                          
156300     IF INFIL-EOF = NEJ                                                   
156400        IF FOR-IDDC NOT = DCS-IDDC                                        
156500           MOVE FOR-IDDC TO W-IDDC-B6                                     
156600           PERFORM IMS-GU-WDB601                                          
156700        END-IF                                                            
156800        MOVE 'W44024'    TO POSTSUM-FDNAMN                                
156900        MOVE 'W44024D1'  TO POSTSUM-DDNAMN2                               
157000        MOVE 'IN'        TO POSTSUM-TRANSTYP                              
157100        CALL POSTSUM USING POSTSUM-PARM                                   
157200        ADD +1           TO WS-ANT-POST-IN                                
157300     END-IF                                                               
157400     .                                                                    
157500     EJECT                                                                
157600 S07-SKRIV-GSAM SECTION.                                                  
157700                                                                          
157800     PERFORM IMS-GSAMFIL-ISRT                                             
157900     ADD +1              TO WS-ANT-POST-GSAM                              
158000     .                                                                    
158100     EJECT                                                                
158200 S09-SPARA-CHKPID SECTION.                                                
158300*                                                                         
158400*    HÄR SPARAS INFO FRÅN DEN SENAST KONSUMERADE IN-POSTEN,               
158500*    FÖR ATT KUNNA SPARA RÄTT INFO VID EV. CHKP.                          
158600*    DETTA GÖRS FÖRE LÄSNING AV NÄSTA IN-POST.                            
158700*                                                                         
158800     MOVE WS-ANT-POST-IN         TO WS-CHKPID-KVPOST                      
158900     MOVE FOR-IDDISTR            TO WS-CHKPID-IDDISTR                     
159000     MOVE FOR-IDKUNDNR           TO WS-CHKPID-IDKUNDNR                    
159100     MOVE FOR-IDKUNDRF           TO WS-CHKPID-IDKUNDRF                    
159200     MOVE FOR-IDLOPNRE           TO WS-CHKPID-IDLOPNRE                    
159300     MOVE FOR-IDKORTNR-ERS       TO WS-CHKPID-IDKORTNR-ERS                
159400     .                                                                    
159500     EJECT                                                                
159600 S10-SUB-SUTPO SECTION.                                                   
159700*                                                                         
159800*    SUTPO SÄNKS                                                          
159900*    I ARBETSFÄLT FÖR SUTPO-TOT FÖR SENARE UPPD I WLARTM01                
160000*    (VID SLUT PÅ ARTIKELN I INFIL),                                      
160100*    SAMT DIREKT UPPDAT I WLARTM11 (REPL ELLER DLET)                      
160200*                                                                         
160300                                                                          
160400     IF  SUTPO-TOT < FOR-KVART                                            
160500       MOVE 'FEL I SECTION S10-, SUTPO-TOT BLIR NEGATIVT'                 
160600                             TO FELTEXT                                   
160700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
160800     END-IF                                                               
160900                                                                          
161000     SUBTRACT FOR-KVART      FROM SUTPO-TOT                               
161100                                                                          
161200     MOVE FOR-IDARTNR TO W-IDARTNR-ARTM                                   
161300     MOVE FOR-TITPO          TO WS-AAMMDD                                 
161400     PERFORM S12-KONV-TILL-AAVV                                           
161500     MOVE WS-AAVV            TO WS-TITPO-AAVV                             
161600     MOVE WS-TITPO-AAVV      TO W-WDK911-DABEHOV                          
161700     IF WS-TITPO-AAVV NOT = ZERO                                          
161800       IF WS-TITPO-AAVV < 5000                                            
161900         MOVE 20             TO W-WDK911-DABEHOV (1:2)                    
162000       ELSE                                                               
162100         IF WS-TITPO-AAVV < 9999                                          
162200           MOVE 19           TO W-WDK911-DABEHOV (1:2)                    
162300         ELSE                                                             
162400           MOVE 999999       TO W-WDK911-DABEHOV                          
162500         END-IF                                                           
162600       END-IF                                                             
162700     END-IF                                                               
162800     PERFORM IMS-GHU-WLARTM11                                             
162900                                                                          
163000***  CO FIX                                                               
163100     IF SEGMENT-SAKNAS AND (FOR-TITPO = 031229 OR                         
163200                                        031230 OR                         
163300                                        031231)                           
163400       MOVE 200401       TO W-WDK911-DABEHOV                              
163500       PERFORM IMS-GHU-WLARTM11                                           
163600     END-IF                                                               
163700***  CO FIX-END                                                           
163800                                                                          
163900     IF  SEGMENT-FINNS                                                    
164000                                                                          
164100       IF  FOR-KDTPOTYP = 1                                               
164200       OR  FOR-KDTPOTYP = 2                                               
164300*        * SUTPO-PB SKALL PÅVERKAS                                        
164400                                                                          
164500         IF  ANT-SUTPO-PB < FOR-KVART                                     
164600           MOVE 'FEL I SECTION S10-, SUTPO-PB BLIR NEGATIVT'              
164700                             TO FELTEXT                                   
164800           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
164900         END-IF                                                           
165000                                                                          
165100         SUBTRACT FOR-KVART  FROM ANT-SUTPO-PB                            
165200                                                                          
165300       ELSE                                                               
165400*        * SUTPO-EJPB SKALL PÅVERKAS                                      
165500                                                                          
165600         IF  ANT-SUTPO-EJPB < FOR-KVART                                   
165700           MOVE 'FEL I SECTION S10-, SUTPO-EJPB BLIR NEGATIVT'            
165800                             TO FELTEXT                                   
165900           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
166000         END-IF                                                           
166100                                                                          
166200         SUBTRACT FOR-KVART  FROM ANT-SUTPO-EJPB                          
166300       END-IF                                                             
166400                                                                          
166500       IF  ANT-SUTPO-PB   > ZERO                                          
166600       OR  ANT-SUTPO-EJPB > ZERO                                          
166700         PERFORM IMS-REPL-WLARTM11                                        
166800       ELSE                                                               
166900         PERFORM IMS-DLET-WLARTM                                          
167000       END-IF                                                             
167100                                                                          
167200     ELSE                                                                 
167300                                                                          
167400       MOVE 'FEL I SECTION S10-, WLARTM11 SAKNAS'                         
167500                             TO FELTEXT                                   
167600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
167700                                                                          
167800     END-IF                                                               
167900     .                                                                    
168000     EJECT                                                                
168100 S11-ADD-SUTPO SECTION.                                                   
168200*                                                                         
168300*    SUTPO HÖJS                                                           
168400*    I ARBETSFÄLT FÖR SUTPO-TOT FÖR SENARE UPPD I WLARTM01                
168500*    SAMT DIREKT UPPDAT I WLARTM11 (ISRT ELLER REPL)                      
168600*    OM ROTEN WLARTM01 SAKNAS LÄGGS DEN UPP NOLLAD.                       
168700*                                                                         
168800                                                                          
168900     ADD FOR-KVART           TO SUTPO-TOT                                 
169000                                                                          
169100     MOVE FOR-IDARTNR TO W-IDARTNR-ARTM                                   
169200     MOVE FOR-TITPO          TO WS-AAMMDD                                 
169300     PERFORM S12-KONV-TILL-AAVV                                           
169400     MOVE WS-AAVV            TO WS-TITPO-AAVV                             
169500     MOVE WS-TITPO-AAVV      TO W-WDK911-DABEHOV                          
169600     IF WS-TITPO-AAVV NOT = ZERO                                          
169700       IF WS-TITPO-AAVV < 5000                                            
169800         MOVE 20             TO W-WDK911-DABEHOV (1:2)                    
169900       ELSE                                                               
170000         IF WS-TITPO-AAVV < 9999                                          
170100           MOVE 19           TO W-WDK911-DABEHOV (1:2)                    
170200         ELSE                                                             
170300           MOVE 999999       TO W-WDK911-DABEHOV                          
170400         END-IF                                                           
170500       END-IF                                                             
170600     END-IF                                                               
170700     PERFORM IMS-GHU-WLARTM11                                             
170800                                                                          
170900     IF  SEGMENT-FINNS                                                    
171000                                                                          
171100       IF  FOR-KDTPOTYP = 1                                               
171200       OR  FOR-KDTPOTYP = 2                                               
171300         ADD FOR-KVART       TO ANT-SUTPO-PB                              
171400       ELSE                                                               
171500         ADD FOR-KVART       TO ANT-SUTPO-EJPB                            
171600       END-IF                                                             
171700                                                                          
171800       PERFORM IMS-REPL-WLARTM11                                          
171900     ELSE                                                                 
172000                                                                          
172100       MOVE ARTM-SEG-LEVEL     TO SEG-LEVEL-WS                            
172200                                                                          
172300       IF  ROT-SEGM-SAKNAS                                                
172400         MOVE FOR-IDARTNR    TO ART-IDARTNR                               
172500         MOVE ZERO           TO ART-KVOFFERT                              
172600         MOVE ZERO           TO ART-KVOKS-BULK                            
172700         MOVE ZERO           TO ART-KVOKS-DAG                             
172800         MOVE ZERO           TO ART-KVOKS-VOR                             
172900         MOVE ZERO           TO ART-KVPREAVB-BULK                         
173000         MOVE ZERO           TO ART-KVPREAVB-DAG                          
173100         MOVE ZERO           TO ART-KVPREAVB-VOR                          
173200         MOVE ZERO           TO ART-KVPRERO-BULK                          
173300         MOVE ZERO           TO ART-KVPRERO-DAG                           
173400         MOVE ZERO           TO ART-RERF-ART                              
173500         MOVE ZERO           TO ART-SUTPO-TOT                             
173600         MOVE SPACE          TO ART-FILLER                                
173700         PERFORM IMS-ISRT-WLARTM01                                        
173800       END-IF                                                             
173900                                                                          
174000       MOVE WS-TITPO-AAVV    TO ANT-DABEHOV                               
174100       IF WS-TITPO-AAVV NOT = ZERO                                        
174200         IF WS-TITPO-AAVV < 5000                                          
174300           MOVE 20           TO ANT-DABEHOV (1:2)                         
174400         ELSE                                                             
174500           IF WS-TITPO-AAVV < 9999                                        
174600             MOVE 19         TO ANT-DABEHOV (1:2)                         
174700           ELSE                                                           
174800             MOVE 999999     TO ANT-DABEHOV                               
174900           END-IF                                                         
175000         END-IF                                                           
175100       END-IF                                                             
175200       MOVE ZERO             TO ANT-SUTPO-PB                              
175300       MOVE ZERO             TO ANT-SUTPO-EJPB                            
175400                                                                          
175500       IF  FOR-KDTPOTYP = 1                                               
175600       OR  FOR-KDTPOTYP = 2                                               
175700         ADD FOR-KVART       TO ANT-SUTPO-PB                              
175800       ELSE                                                               
175900         ADD FOR-KVART       TO ANT-SUTPO-EJPB                            
176000       END-IF                                                             
176100                                                                          
176200       PERFORM IMS-ISRT-WLARTM11                                          
176300     END-IF                                                               
176400     .                                                                    
176500     EJECT                                                                
176600 S12-KONV-TILL-AAVV SECTION.                                              
176700*                                                                         
176800*    KONVERTERAR AAMMDD TILL AAVV.                                        
176900*                                                                         
177000                                                                          
177100     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
177200     MOVE WS-AAMMDD          TO DAT-I-TIDATUM                             
177300                                                                          
177400     CALL WDATKONV USING     DAT-KDDATFORM                                
177500                             DAT-I-TIDATUM                                
177600                             DAT-O-TIDATUM                                
177700                             DAT-KDSVAR                                   
177800                                                                          
177900     IF  DAT-KDSVAR-OK                                                    
178000       MOVE DAT-TIAA-VECKA   TO WS-AAVV-AA                                
178100       MOVE DAT-TIVV         TO WS-AAVV-VV                                
178200                                                                          
178300     ELSE                                                                 
178400                                                                          
178500       MOVE 'FEL I SECTION S12-, FRÅN SUBPGM WDATKONV'                    
178600                             TO FELTEXT                                   
178700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
178800     END-IF                                                               
178900     .                                                                    
179000     EJECT                                                                
179100 S13-TA-BORT-LARMQ SECTION.                                               
179200                                                                          
179300     IF  FOR-KDLARM > ZERO                                                
179400       MOVE '2223'           TO W-2223-IDHTYP                             
179500       MOVE FOR-IDANSK-LARM  TO W-2223-IDANSK                             
179600       MOVE LOW-VALUE        TO W-2223-LOW-VALUE                          
179700       MOVE FOR-TISENBEK-DAG TO W-2224-TISENBEK-DAG                       
179800       MOVE FOR-TISENBEK-KL  TO W-2224-TISENBEK-KL                        
179900       MOVE FOR-KDLARM       TO W-2224-KDLARM                             
180000       PERFORM IMS-GHU-XXBU-2224                                          
180100                                                                          
180200       IF  SEGMENT-FINNS                                                  
180300         PERFORM IMS-DLET-XXBU                                            
180400       END-IF                                                             
180500     END-IF                                                               
180600     .                                                                    
180700     EJECT                                                                
180800 S20-KOMPLETTERA-PRIS  SECTION.                                           
180900                                                                          
181000     IF DIST79-DEALER-PRICE                                               
181100      MOVE FOR-PRARTNTO-LOC     TO WX-PRARTNTO-LOC                        
181200      MOVE FOR-PRARTNTO-LOCPREL TO WX-PRARTNTO-LOCPREL                    
181300                                                                          
181400      MOVE ZERO                 TO FOR-PRARTNTO                           
181500      MOVE ZERO                 TO FOR-PRAVCOST                           
181600      MOVE ZERO                 TO RAD-PRARTNTO                           
181700      MOVE ZERO                 TO RAD-PRAVCOST                           
181800      IF FOR-IDARTNR NOT = FOR-IDARTNR-URS                                
181900         AND FOR-IDARTNR-URS NOT = ZERO                                   
182000*         ERSATT ARTIKEL                                                  
182100        MOVE ZERO               TO FOR-PRARTNTO-LOC                       
182200                                   RAD-PRARTNTO-LOC                       
182300                                   FOR-PRARTNTO-LOCPREL                   
182400                                   RAD-PRARTNTO-LOCPREL                   
182500**                                 FOR-IDPRQUES                           
182600                                   RAD-IDPRQUES                           
182700      END-IF                                                              
182800      IF FOR-PRARTNTO-LOC = ZERO AND                                      
182900         FOR-PRARTNTO-LOCPREL = ZERO                                      
183000       IF FOR-IDPRQUES > ZERO                                             
183100*         TA BORT GAMLA PRISFRÅGAN                                        
183200         INITIALIZE PRQU-W335PRQU                                         
183300         MOVE FOR-IDDISTR            TO PRQU-IDDISTR                      
183400         MOVE FOR-IDKUNDNR           TO PRQU-IDKUNDNR                     
183500         MOVE FOR-IDKUNDRF(1:5)      TO PRQU-IDKUNDRF(3:5)                
183600         MOVE '00'                   TO PRQU-IDKUNDRF(1:2)                
183700         MOVE FOR-IDPRQUES           TO PRQU-IDPRQUES                     
183800         MOVE 4                      TO PRQU-KDCALL                       
183900         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
184000                                            PRQU-WDC7-PCB                 
184100                                            PRQU-SJKO-WDK6-PCB            
184200                                                                          
184300       END-IF                                                             
184400       MOVE ZERO                     TO FOR-IDPRQUES                      
184500                                        WS-IDPRQUES                       
184600       IF WS-IDPRQUES                = +0                                 
184700          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
184800          MOVE +1                    TO PRNO-KDCALL                       
184900                                                                          
185000          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
185100                                                                          
185200          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
185300                                        WS-IDPRQUES                       
185400          MOVE +1                    TO PRQU-KDCALL                       
185500*      ELSE                                                               
185600*         MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
185700*         MOVE +2                    TO PRNO-KDCALL                       
185800*                                                                         
185900*         CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
186000*                                                                         
186100*         MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
186200*                                     WS-IDPRQUES                         
186300*         MOVE +2                    TO PRQU-KDCALL                       
186400       END-IF                                                             
186500*         LÄGG IN NY PRISFRÅGA                                            
186600       MOVE FOR-IDDISTR              TO PRQU-IDDISTR                      
186700                                        W-IDDISTR                         
186800       MOVE FOR-IDKUNDNR             TO PRQU-IDKUNDNR                     
186900       MOVE FOR-IDKUNDRF(1:5)        TO PRQU-IDKUNDRF(3:5)                
187000       MOVE '00'                     TO PRQU-IDKUNDRF(1:2)                
187100       MOVE ZERO                     TO PRQU-IDORDER                      
187200       MOVE FOR-KDORDKL              TO PRQU-KDORDKL                      
187300       IF FOR-IDDISTR = 0778 AND FOR-KDORDKL < 3                          
187400         AND FOR-TIRODAT > 0                                              
187500         MOVE 4                      TO PRQU-KDORDKL                      
187600       END-IF                                                             
187700       MOVE 'N'                      TO PRQU-KDPRSTA                      
187800       MOVE FOR-IDARTNR              TO PRQU-IDARTNR                      
187900       MOVE FOR-KVBEART-Q            TO PRQU-KVBEART-Q                    
188000       MOVE FOR-KDVALISO             TO PRQU-KDVALISO                     
188100       MOVE FOR-PRARTNTO-LOC         TO PRQU-PRARTNTO-LOC                 
188200       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
188300       MOVE FOR-IDSYSTEM             TO PRQU-IDSYSTEM                     
188400*      PERFORM IMS-GU-GMTA-WDB201                                         
188500                                                                          
188600       CALL  W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                  
188700                                           PRQU-WDC7-PCB                  
188800                                           PRQU-SJKO-WDK6-PCB             
188900                                                                          
189000       MOVE PRQU-IDPRQUES            TO  RAD-IDPRQUES                     
189100                                         FOR-IDPRQUES                     
189200                                         WS-IDPRQUES                      
189300       MOVE PRQU-FLPRTILL            TO  RAD-FLPRTILL                     
189400                                         FOR-FLPRTILL                     
189500                                                                          
189600       IF PRQU-KDORDKL = 4 AND PRQU-IDDISTR = 0778                        
189700         AND FOR-KDORDKL < 3                                              
189800         MOVE 'N'                    TO RAD-FLPRTILL                      
189900       END-IF                                                             
190000       IF FOR-PRARTNTO-LOC = +0                                           
190100          MOVE PRQU-PRARTNTO-LOCPREL TO                                   
190200                         FOR-PRARTNTO-LOCPREL                             
190300                         RAD-PRARTNTO-LOCPREL                             
190400       END-IF                                                             
190500                                                                          
190600       IF FOR-PRARTNTO-LOC NOT = +0                                       
190700         IF FOR-KDPRTYP = SPACE                                           
190800           MOVE 'P'                TO RAD-KDPRTYP                         
190900*          MOVE FOR-TIREGDAT       TO RAD-TIPRIS                          
191000         END-IF                                                           
191100       END-IF                                                             
191200       PERFORM S21-SKICKA-PRISFRAGA                                       
191300       IF RAD-PRARTNTO-LOC = ZERO AND                                     
191400          RAD-PRARTNTO-LOCPREL = ZERO                                     
191500          IF WX-PRARTNTO-LOC NOT = ZERO                                   
191600           MOVE WX-PRARTNTO-LOC   TO RAD-PRARTNTO-LOCPREL                 
191700                                     FOR-PRARTNTO-LOCPREL                 
191800          ELSE                                                            
191900           MOVE WX-PRARTNTO-LOCPREL TO RAD-PRARTNTO-LOCPREL               
192000                                       FOR-PRARTNTO-LOCPREL               
192100          END-IF                                                          
192200          DISPLAY 'PRIS=0 ' RAD-IDDISTR , ' ' , RAD-IDKUNDNR              
192300          DISPLAY '       ' RAD-IDKUNDRF , ' ' , RAD-IDARTNR              
192400       END-IF                                                             
192500       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
192600       MOVE +3                      TO PRNO-KDCALL                        
192700                                                                          
192800       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
192900      END-IF                                                              
193000     END-IF                                                               
193100     .                                                                    
193200     EJECT                                                                
193300 S21-SKICKA-PRISFRAGA  SECTION.                                           
193400                                                                          
193500     MOVE 1                          TO 3039-REQU-IDMSGVER                
193600     MOVE SPACE                      TO 3039-REQU-KDPGMACT                
193700     MOVE 'W4402400'                 TO 3039-REQU-IDUSER                  
193800                                                                          
193900     MOVE SPACE                      TO 3039-MID-IDBUNDLE                 
194000     MOVE FOR-IDDISTR                TO 3039-MID-IDDISTR                  
194100     MOVE FOR-IDKUNDNR               TO 3039-MID-IDKUNDNR                 
194200     MOVE FOR-IDKUNDRF               TO 3039-MID-IDBUNDLE(3:5)            
194300     MOVE '00'                       TO 3039-MID-IDBUNDLE(1:2)            
194400     MOVE WS-IDPRQUES                TO 3039-MID-IDPRQUES                 
194500                                                                          
194600     PERFORM S24-SKICKA-OPEN                                              
194700     PERFORM S24-SKICKA-MEDDELANDE                                        
194800     PERFORM S24-SKICKA-CLOSE                                             
194900                                                                          
195000     .                                                                    
195100     EJECT                                                                
195200 S24-SKICKA-OPEN SECTION.                                                 
195300                                                                          
195400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
195500     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
195600     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
195700                                                                          
195800     IF SEND-KDRC > 0                                                     
195900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
196000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
196100       DELIMITED BY SIZE INTO FELTEXT                                     
196200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
196300     END-IF                                                               
196400     .                                                                    
196500     SKIP3                                                                
196600 S24-SKICKA-MEDDELANDE  SECTION.                                          
196700                                                                          
196800     MOVE 'PUT'                      TO SEND-KDFUNC                       
196900     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
197000     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
197100                                                                          
197200     IF SEND-KDRC > 0                                                     
197300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
197400       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
197500       DELIMITED BY SIZE INTO FELTEXT                                     
197600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
197700     END-IF                                                               
197800     .                                                                    
197900     SKIP3                                                                
198000 S24-SKICKA-CLOSE SECTION.                                                
198100                                                                          
198200     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
198300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
198400                                                                          
198500     IF SEND-KDRC > 0                                                     
198600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
198700       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
198800       DELIMITED BY SIZE INTO FELTEXT                                     
198900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
199000     END-IF                                                               
199100     .                                                                    
199200     EJECT                                                                
199300*    ---- IMS SEKTIONER                                                   
199400                                                                          
199500 IMS-RESTART      SECTION.                                                
199600                                                                          
199700     MOVE SPACE TO MSG-IO-AREA                                            
199800     MOVE '  ' TO GODK-STATUSKODER                                        
199900     CALL CBLTDLI USING XRST                                              
200000                        MSG-PCB                                           
200100                        MSG-IO-AREA-LENGTH                                
200200                        MSG-IO-AREA                                       
200300                        CHKP-AREA-1-LENGTH                                
200400                        CHKP-AREA-1                                       
200500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
200600     PERFORM IMS-STATUSKONTROLL                                           
200700     .                                                                    
200800     SKIP2                                                                
200900 IMS-CHECKPOINT   SECTION.                                                
201000                                                                          
201100     MOVE SPACE TO MSG-IO-AREA                                            
201200     MOVE '  XD' TO GODK-STATUSKODER                                      
201300     CALL CBLTDLI USING CHKP                                              
201400                        MSG-PCB                                           
201500                        MSG-IO-AREA-LENGTH                                
201600                        MSG-IO-AREA                                       
201700                        CHKP-AREA-1-LENGTH                                
201800                        CHKP-AREA-1                                       
201900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
202000     PERFORM IMS-STATUSKONTROLL                                           
202100                                                                          
202200     IF  IMS-EJ-OK                                                        
202300       DISPLAY 'IMS-KONTROLLREGIONEN EJ TILLGÄNGLIG'                      
202400       CALL FELLOG                                                        
202500     END-IF                                                               
202600     .                                                                    
202700     EJECT                                                                
202800*    ---- IMS SEKTIONER                                                   
202900                                                                          
203000 IMS-GHU-WLORDP01 SECTION.                                                
203100                                                                          
203200     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X                            
203300                    '&IDDC     =' W-IDDC-X ')'                            
203400            DELIMITED BY SIZE INTO SSA1                                   
203500     MOVE '  GE' TO GODK-STATUSKODER                                      
203600     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-ORDP01 SSA1                   
203700     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
203800     PERFORM IMS-STATUSKONTROLL                                           
203900     .                                                                    
204000     SKIP2                                                                
204100 IMS-DLET-WLORDP01 SECTION.                                               
204200                                                                          
204300     MOVE '  ' TO GODK-STATUSKODER                                        
204400     CALL CBLTDLI USING DLET ORDP-PCB DLI-IO-ORDP01                       
204500     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
204600     PERFORM IMS-STATUSKONTROLL                                           
204700     .                                                                    
204800     SKIP2                                                                
204900 IMS-ISRT-WLORDP01 SECTION.                                               
205000                                                                          
205100     MOVE 'WLORDP01 ' TO SSA1                                             
205200     MOVE '  II' TO GODK-STATUSKODER                                      
205300     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-ORDP01 SSA1                  
205400     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
205500     PERFORM IMS-STATUSKONTROLL                                           
205600     .                                                                    
205700     EJECT                                                                
205800 IMS-GU-WLARTC01 SECTION.                                                 
205900                                                                          
206000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ARTC-X ')'                    
206100            DELIMITED BY SIZE INTO SSA1                                   
206200     MOVE '  ' TO GODK-STATUSKODER                                        
206300     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-ARTC01 SSA1                   
206400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
206500     PERFORM IMS-STATUSKONTROLL                                           
206600     .                                                                    
206700     SKIP2                                                                
206800 IMS-GHU-WLARTC11 SECTION.                                                
206900                                                                          
207000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ARTC-X ')'                    
207100            DELIMITED BY SIZE INTO SSA1                                   
207200     MOVE   'WLARTC11'          TO SSA2                                   
207300     MOVE '  ' TO GODK-STATUSKODER                                        
207400     CALL CBLTDLI USING GHU  ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2             
207500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
207600     PERFORM IMS-STATUSKONTROLL                                           
207700     .                                                                    
207800     SKIP2                                                                
207900 IMS-REPL-WLARTC11 SECTION.                                               
208000                                                                          
208100     MOVE '  ' TO GODK-STATUSKODER                                        
208200     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC11                       
208300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
208400     PERFORM IMS-STATUSKONTROLL                                           
208500     .                                                                    
208600     EJECT                                                                
208700 IMS-GHU-WDK711 SECTION.                                                  
208800                                                                          
208900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-WDK7-X ')'                    
209000            DELIMITED BY SIZE INTO SSA1                                   
209100     STRING 'WDK711  (IDDC     =' W-IDDC-WDK7-X    ')'                    
209200            DELIMITED BY SIZE INTO SSA2                                   
209300     MOVE '  GE' TO GODK-STATUSKODER                                      
209400     CALL CBLTDLI USING GHU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2             
209500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
209600     PERFORM IMS-STATUSKONTROLL                                           
209700     .                                                                    
209800     SKIP2                                                                
209900 IMS-REPL-WDK711 SECTION.                                                 
210000                                                                          
210100     MOVE '  ' TO GODK-STATUSKODER                                        
210200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
210300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
210400     PERFORM IMS-STATUSKONTROLL                                           
210500     .                                                                    
210600     EJECT                                                                
210700 IMS-GHU-WLARTM01 SECTION.                                                
210800                                                                          
210900     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-ARTM-X ')'                    
211000            DELIMITED BY SIZE INTO SSA1                                   
211100     MOVE '  GE' TO GODK-STATUSKODER                                      
211200     CALL CBLTDLI USING GHU  ARTM-PCB DLI-IO-ARTM01 SSA1                  
211300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
211400     PERFORM IMS-STATUSKONTROLL                                           
211500     .                                                                    
211600     SKIP2                                                                
211700 IMS-GU-WLARTM01 SECTION.                                                 
211800                                                                          
211900     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-ARTM-X ')'                    
212000            DELIMITED BY SIZE INTO SSA1                                   
212100     MOVE '  GE' TO GODK-STATUSKODER                                      
212200     CALL CBLTDLI USING GU  ARTM-PCB DLI-IO-ARTM01 SSA1                   
212300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
212400     PERFORM IMS-STATUSKONTROLL                                           
212500     .                                                                    
212600     SKIP2                                                                
212700 IMS-ISRT-WLARTM01 SECTION.                                               
212800                                                                          
212900     MOVE 'WLARTM01 ' TO SSA1                                             
213000     MOVE '  ' TO GODK-STATUSKODER                                        
213100     CALL CBLTDLI USING ISRT ARTM-PCB DLI-IO-ARTM01 SSA1                  
213200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
213300     PERFORM IMS-STATUSKONTROLL                                           
213400     .                                                                    
213500     EJECT                                                                
213600 IMS-GHU-WLARTM11 SECTION.                                                
213700                                                                          
213800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-ARTM-X ')'                    
213900            DELIMITED BY SIZE INTO SSA1                                   
214000     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
214100            DELIMITED BY SIZE INTO SSA2                                   
214200     MOVE '  GE' TO GODK-STATUSKODER                                      
214300     CALL CBLTDLI USING GHU  ARTM-PCB DLI-IO-ARTM11 SSA1 SSA2             
214400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
214500     PERFORM IMS-STATUSKONTROLL                                           
214600     .                                                                    
214700     SKIP2                                                                
214800 IMS-ISRT-WLARTM11 SECTION.                                               
214900                                                                          
215000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-ARTM-X ')'                    
215100            DELIMITED BY SIZE INTO SSA1                                   
215200     MOVE 'WLARTM11 ' TO SSA2                                             
215300     MOVE '  ' TO GODK-STATUSKODER                                        
215400     CALL CBLTDLI USING ISRT ARTM-PCB DLI-IO-ARTM11 SSA1 SSA2             
215500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
215600     PERFORM IMS-STATUSKONTROLL                                           
215700     .                                                                    
215800     EJECT                                                                
215900 IMS-REPL-WLARTM01 SECTION.                                               
216000                                                                          
216100     MOVE '  ' TO GODK-STATUSKODER                                        
216200     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-ARTM01                       
216300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
216400     PERFORM IMS-STATUSKONTROLL                                           
216500     .                                                                    
216600     SKIP2                                                                
216700 IMS-REPL-WLARTM11 SECTION.                                               
216800                                                                          
216900     MOVE '  ' TO GODK-STATUSKODER                                        
217000     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-ARTM11                       
217100     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
217200     PERFORM IMS-STATUSKONTROLL                                           
217300     .                                                                    
217400     SKIP2                                                                
217500 IMS-DLET-WLARTM   SECTION.                                               
217600                                                                          
217700     MOVE '  ' TO GODK-STATUSKODER                                        
217800     CALL CBLTDLI USING DLET ARTM-PCB DLI-IO-ARTM11                       
217900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
218000     PERFORM IMS-STATUSKONTROLL                                           
218100     .                                                                    
218200     EJECT                                                                
218300 IMS-GHU-XXLN-4554 SECTION.                                               
218400                                                                          
218500     STRING 'WLXXLN01(WDGXKEY  =' W-IDHTYP-4553-X ')'                     
218600            DELIMITED BY SIZE INTO SSA1                                   
218700     STRING 'WLXXLN11(KDSEGKEY =' '1' ')'                                 
218800            DELIMITED BY SIZE INTO SSA2                                   
218900     MOVE '  GE' TO GODK-STATUSKODER                                      
219000     CALL CBLTDLI USING GHU  XXLN-PCB DLI-IO-XXLN11 SSA1 SSA2             
219100     MOVE XXLN-STATUS-CODE TO STATUS-WS                                   
219200     PERFORM IMS-STATUSKONTROLL                                           
219300     .                                                                    
219400     SKIP2                                                                
219500 IMS-REPL-XXLN SECTION.                                                   
219600                                                                          
219700     MOVE '  ' TO GODK-STATUSKODER                                        
219800     CALL CBLTDLI USING REPL XXLN-PCB DLI-IO-XXLN11                       
219900     MOVE XXLN-STATUS-CODE TO STATUS-WS                                   
220000     PERFORM IMS-STATUSKONTROLL                                           
220100     .                                                                    
220200     EJECT                                                                
220300 IMS-DLET-XXLN SECTION.                                                   
220400                                                                          
220500     MOVE '  ' TO GODK-STATUSKODER                                        
220600     CALL CBLTDLI USING DLET XXLN-PCB DLI-IO-XXLN11                       
220700     MOVE XXLN-STATUS-CODE TO STATUS-WS                                   
220800     PERFORM IMS-STATUSKONTROLL                                           
220900     .                                                                    
221000     SKIP2                                                                
221100 IMS-ISRT-XXLN-4554 SECTION.                                              
221200                                                                          
221300     STRING 'WLXXLN01(WDGXKEY  =' W-IDHTYP-4553-X ')'                     
221400            DELIMITED BY SIZE INTO SSA1                                   
221500     MOVE 'WLXXLN11 ' TO SSA2                                             
221600     MOVE '  ' TO GODK-STATUSKODER                                        
221700     CALL CBLTDLI USING ISRT XXLN-PCB DLI-IO-XXLN11 SSA1 SSA2             
221800     MOVE XXLN-STATUS-CODE TO STATUS-WS                                   
221900     PERFORM IMS-STATUSKONTROLL                                           
222000     .                                                                    
222100     EJECT                                                                
222200 IMS-GU-XXBX-2232 SECTION.                                                
222300                                                                          
222400     STRING 'WLXXBX01(WDGXKEY  =' W-2231-WDGX01 ')'                       
222500            DELIMITED BY SIZE INTO SSA1                                   
222600     STRING 'WLXXBX11(WDGXKEY  =' W-2232-WDGXKEY ')'                      
222700            DELIMITED BY SIZE INTO SSA2                                   
222800     MOVE '  GE' TO GODK-STATUSKODER                                      
222900     CALL CBLTDLI USING GU  XXBX-PCB DLI-IO-XXBX11 SSA1 SSA2              
223000     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
223100     PERFORM IMS-STATUSKONTROLL                                           
223200     .                                                                    
223300     SKIP2                                                                
223400 IMS-GU-XXBU-2223 SECTION.                                                
223500                                                                          
223600     STRING 'WLXXBU01(WDGXKEY  =' W-2223-WDGX2223 ')'                     
223700            DELIMITED BY SIZE INTO SSA1                                   
223800     MOVE '  GE' TO GODK-STATUSKODER                                      
223900     CALL CBLTDLI USING GU  XXBU-PCB DLI-IO-XXBU01 SSA1                   
224000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
224100     PERFORM IMS-STATUSKONTROLL                                           
224200     .                                                                    
224300     SKIP2                                                                
224400 IMS-ISRT-XXBU-2223 SECTION.                                              
224500                                                                          
224600     MOVE 'WLXXBU01 ' TO SSA1                                             
224700     MOVE '  ' TO GODK-STATUSKODER                                        
224800     CALL CBLTDLI USING ISRT XXBU-PCB DLI-IO-XXBU01 SSA1                  
224900     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
225000     PERFORM IMS-STATUSKONTROLL                                           
225100     .                                                                    
225200     EJECT                                                                
225300 IMS-GHU-XXBU-2224 SECTION.                                               
225400                                                                          
225500     STRING 'WLXXBU01(WDGXKEY  =' W-2223-WDGX2223 ')'                     
225600            DELIMITED BY SIZE INTO SSA1                                   
225700     STRING 'WLXXBU11(WDGXKEY  =' W-2224-WDGXKEY  ')'                     
225800            DELIMITED BY SIZE INTO SSA2                                   
225900     MOVE '  GE' TO GODK-STATUSKODER                                      
226000     CALL CBLTDLI USING GHU XXBU-PCB DLI-IO-XXBU11 SSA1 SSA2              
226100     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
226200     PERFORM IMS-STATUSKONTROLL                                           
226300     .                                                                    
226400     SKIP2                                                                
226500 IMS-ISRT-XXBU-2224 SECTION.                                              
226600                                                                          
226700     STRING 'WLXXBU01(WDGXKEY  =' W-2223-WDGX2223 ')'                     
226800            DELIMITED BY SIZE INTO SSA1                                   
226900     MOVE 'WLXXBU11 ' TO SSA2                                             
227000     MOVE '  II' TO GODK-STATUSKODER                                      
227100     CALL CBLTDLI USING ISRT  XXBU-PCB DLI-IO-XXBU11 SSA1 SSA2            
227200     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
227300     PERFORM IMS-STATUSKONTROLL                                           
227400     .                                                                    
227500     SKIP2                                                                
227600 IMS-DLET-XXBU SECTION.                                                   
227700                                                                          
227800     MOVE '  ' TO GODK-STATUSKODER                                        
227900     CALL CBLTDLI USING DLET XXBU-PCB DLI-IO-XXBU11                       
228000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
228100     PERFORM IMS-STATUSKONTROLL                                           
228200     .                                                                    
228300     EJECT                                                                
228400 IMS-ISRT-4505-4505 SECTION.                                              
228500                                                                          
228600     MOVE 'WL450501 ' TO SSA1                                             
228700     MOVE '  II' TO GODK-STATUSKODER                                      
228800     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-450501 SSA1                  
228900     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
229000     PERFORM IMS-STATUSKONTROLL                                           
229100     .                                                                    
229200     EJECT                                                                
229300 IMS-ISRT-4505-4506 SECTION.                                              
229400                                                                          
229500     STRING 'WL450501(WDGXKEY  =' W-IDHTYP-4505-X ')'                     
229600            DELIMITED BY SIZE INTO SSA1                                   
229700     MOVE 'WL450511 ' TO SSA2                                             
229800     MOVE '  II' TO GODK-STATUSKODER                                      
229900     CALL CBLTDLI USING ISRT  4505-PCB DLI-IO-450511 SSA1 SSA2            
230000     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
230100     PERFORM IMS-STATUSKONTROLL                                           
230200     .                                                                    
230300     SKIP2                                                                
230400 IMS-GSAMFIL-OPEN SECTION.                                                
230500                                                                          
230600     MOVE 'OUT' TO GSAMFIL-IO-AREA                                        
230700     MOVE '  ' TO GODK-STATUSKODER                                        
230800     CALL CBLTDLI USING OPEN-GSAM GSAMFIL-PCB GSAMFIL-IO-AREA             
230900     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
231000     PERFORM IMS-STATUSKONTROLL                                           
231100     .                                                                    
231200     SKIP2                                                                
231300 IMS-GSAMFIL-ISRT SECTION.                                                
231400                                                                          
231500     MOVE '  ' TO GODK-STATUSKODER                                        
231600     CALL CBLTDLI USING ISRT GSAMFIL-PCB GSAMFIL-IO-AREA                  
231700     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
231800     PERFORM IMS-STATUSKONTROLL                                           
231900     .                                                                    
232000     SKIP2                                                                
232100 IMS-GSAMFIL-CLOSE SECTION.                                               
232200                                                                          
232300     MOVE '  ' TO GODK-STATUSKODER                                        
232400     CALL CBLTDLI USING CLSE-GSAM GSAMFIL-PCB                             
232500     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
232600     PERFORM IMS-STATUSKONTROLL                                           
232700     .                                                                    
232800     EJECT                                                                
232900 IMS-GNP-WDK722 SECTION.                                                  
233000                                                                          
233100     MOVE 'WDK722   ' TO SSA1                                             
233200     MOVE '  GE' TO GODK-STATUSKODER                                      
233300     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1                   
233400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
233500     PERFORM IMS-STATUSKONTROLL                                           
233600     .                                                                    
233700     EJECT                                                                
233800 DB2-SELECT-TP4TRAN     SECTION.                                          
233900     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
234000                                                                          
234100     MOVE 000100 TO GODK-SQLCODEKODER                                     
234200                                                                          
234300     EXEC SQL                                                             
234400           SELECT  DISTINCT                                               
234500                   IDDC_REC                                               
234600                                                                          
234700           INTO   :TP4TRAN-IDDC-REC                                       
234800                                                                          
234900           FROM    TP4TRAN                                                
235000                                                                          
235100           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
235200     END-EXEC                                                             
235300                                                                          
235400     MOVE SQLCODE TO SQLCODE-WS                                           
235500     PERFORM DB2-STATUSKONTROLL                                           
235600     .                                                                    
235700     EJECT                                                                
235800 IMS-GU-WDB601    SECTION.                                                
235900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
236000          DELIMITED BY SIZE INTO SSA1                                     
236100     MOVE '  GE' TO GODK-STATUSKODER                                      
236200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
236300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
236400     PERFORM IMS-STATUSKONTROLL                                           
236500     IF SEGMENT-SAKNAS                                                    
236600         MOVE SPACE TO DCS-KDDC                                           
236700     END-IF                                                               
236800     .                                                                    
236900     SKIP3                                                                
237000 IMS-GU-WDK701 SECTION.                                                   
237100     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
237200          DELIMITED BY SIZE INTO SSA1                                     
237300     MOVE '  GE' TO GODK-STATUSKODER                                      
237400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701  SSA1                   
237500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
237600     PERFORM IMS-STATUSKONTROLL                                           
237700     .                                                                    
237800     SKIP3                                                                
237900 IMS-GNP-WDK711 SECTION.                                                  
238000     MOVE 'WDK711   ' TO SSA1                                             
238100     MOVE '  GE' TO GODK-STATUSKODER                                      
238200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711  SSA1                  
238300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
238400     PERFORM IMS-STATUSKONTROLL                                           
238500     .                                                                    
238600     SKIP3                                                                
238700 IMS-STATUSKONTROLL SECTION.                                              
238800                                                                          
238900     SET STATUS-IX TO 1                                                   
239000     SEARCH GODK-STATUS AT END                                            
239100       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
239200         DELIMITED BY SIZE INTO FELTEXT                                   
239300       CALL FELLOG                                                        
239400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
239500     END-SEARCH.                                                          
239600     EJECT                                                                
239700 DB2-STATUSKONTROLL  SECTION.                                             
239800                                                                          
239900     SET SQLCODE-IX TO 1                                                  
240000     SEARCH GODK-SQLCODE                                                  
240100       AT END                                                             
240200          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
240300          DELIMITED BY SIZE INTO FELTEXT                                  
240400          CALL ABEND USING RKOD-ABEND-DB2                                 
240500       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
240600     END-SEARCH                                                           
240700     .                                                                    
