000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4403600.                                        
000400 AUTHOR.                 INGER NILSSON.                                   
000500 DATE-WRITTEN.           SEPT 1988.                                       
000600                                                                          
000700*    FUNKTION:                                                            
000800*        AKTIVERING AV TPO-ORDER, MED UPPNÅTT DATUM, FRÅN                 
000900*        SEKVENSIELL KOPIA AV WDA5. TRANSAR TYP RZA skapas.               
001000*        PROGRAMMET LäGGER UT EN TRANS TILL 2109.                         
001100*                                                                         
001200*        INFIL:  W44061 - SEKVENSIELL KOPIA AV WDA5                       
001300*                                                                         
001400*        PROGRAMMET LÄSER OCH                                             
001500*                   UPPDATERAR  WLORDP  (WDA501) RESTORDER-/TPOREG        
001600*        PROGRAMMET LÄSER       WLARTC  (WDK601) ARTIKELREG               
001700*        PROGRAMMET LÄSER OCH                                             
001800*                   UPPDATERAR  WLARTM  (WDK901) ARTIKELREG               
001900*                                       (WDK911)                          
002000*        PROGRAMMET UPPDATERAR  WLZZAC  (WDG601) TRANSAKTIONSBAS          
002100*        PROGRAMMET LÄSER       WLORQI  (WDQ201) ORDERHUVUDSREG           
002200*                                                                         
002300*  ABENDKODER:                                                            
002400*                                                                         
002500*        U0016     - OM RETURKOD FRÅN SORT                                
002600*                                                                         
002700*    040830  SM    INLÄGGNING AV PRISFRÅGOR FÖR DNI                       
002800*    070827  SO    e'TRACKER 5444132                                      
002810*    STORY 2375089 ADD IDSYSTEM VOUI, ECOM                                
002900     EJECT                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500     SKIP2                                                                
003600     SELECT W44061  ASSIGN TO W44036D1.                                   
003700     SELECT SORTFIL ASSIGN TO W44036DS.                                   
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000                                                                          
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W44061                                                               
004400     LABEL RECORD STANDARD                                                
004500     RECORDING F                                                          
004600     BLOCK CONTAINS 0.                                                    
004700*01  W44061-POST  -COPY W44060     -L                                     
004800     EJECT                                                                
004900 SD  SORTFIL                                                              
005000     LABEL RECORD STANDARD                                                
005100     RECORDING F                                                          
005200     BLOCK CONTAINS 0.                                                    
005300*01  SORTPOST  -COPY W44060     -PRE S-                                   
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600     SKIP2                                                                
005700*    -COPY WY2000W1                                                       
005800     SKIP3                                                                
005900*    ---- ARBETSVARIABLER                                                 
006000*                                                                         
006100 77  PGM-NAMN                PIC X(08)  VALUE 'W4403600'.                 
006200 77  FELTEXT                 PIC X(80)  VALUE SPACE.                      
006300 77  INFIL-SLUT              PIC X(03)  VALUE SPACE.                      
006400 77  SORTFIL-SLUT            PIC X(03)  VALUE SPACE.                      
006500                                                                          
006600 77  MSG-IO-AREA-LENGTH-1    PIC S9(9) VALUE +32 COMP SYNC.               
006700 77  MSG-IO-AREA-1           PIC X(32) VALUE SPACE.                       
006800 77  CHKP-AREA-1-LENGTH      PIC S9(9) VALUE +32 COMP SYNC.               
006900 77  CHKP-AREA-1             PIC X(32) VALUE SPACE.                       
007000                                                                          
007100 77  W-SAMMANSLAGNING-RAD    PIC X.                                       
007200 77  W-DAT-TIAAMMDD          PIC 9(6).                                    
007300 77  W-DAT-TIAA              PIC 9(2).                                    
007400 77  W-DAT-TIVV              PIC 9(2).                                    
007500 77  W-DAT-TIAAP             PIC 9(3).                                    
007600 77  WS-NEXT-WORKDAY         PIC 9(6).                                    
007700 77  W-TPO-DATUM-UPPNADD     PIC X.                                       
007800 77  TIAAMMDD                PIC 9(6).                                    
007900 77  TIKLOCK                 PIC 9(8).                                    
008000 77  W-KVOKS-VOR             PIC 9(7)   VALUE ZERO.                       
008100 77  W-KVOKS-DAG             PIC 9(7)   VALUE ZERO.                       
008200 77  W-KVOKS-BULK            PIC 9(7)   VALUE ZERO.                       
008300 77  W-SUTPO-TOT             PIC 9(7)   VALUE ZERO.                       
008400 77  W-SUTPO-PB              PIC 9(7)   VALUE ZERO.                       
008500 77  W-SUTPO-EJPB            PIC 9(7)   VALUE ZERO.                       
008600 77  W-KVUPDAT               PIC S9(7) COMP-3 VALUE +0.                   
008700 77  W-KDORDING              PIC 9.                                       
008800 77  2109-IX                 PIC S9(9)  VALUE ZERO COMP SYNC.             
008900 77  2109-IX-MAX             PIC S9(9)  VALUE +18  COMP SYNC.             
009000                                                                          
009100 77  RFS-IX                  PIC S9(4)  VALUE ZERO COMP SYNC.             
009200 77  MAX-RFS-IX              PIC S9(4)  VALUE +4   COMP SYNC.             
009300                                                                          
009400 77  WS-IDPRQUES             PIC S9(7)  VALUE +0.                         
009500                                                                          
009600 77  W-IDKUNDRF-WIP          PIC X(10)  VALUE SPACE.                      
009700                                                                          
009800 77  WS-TIRFS                PIC 9(6).                                    
009900                                                                          
010000 01  WS-DAT                         PIC 9(6).                             
010100 01  FILLER REDEFINES WS-DAT.                                             
010200     03 WS-YEAR                     PIC 9(2).                             
010300     03 WS-MONTH                    PIC 9(2).                             
010400     03 WS-DAYS                     PIC 9(2).                             
010500                                                                          
010600 77  WS-IDKONTO-DISPLAY      PIC 9(10)  VALUE ZERO.                       
010700                                                                          
010800 01  WS-IDORDNR-NUM                          PIC 9(7).                    
010900 01  WS-IDORDNR REDEFINES WS-IDORDNR-NUM     PIC X(7).                    
011000 01  WS-IDDISTR-NUM                          PIC 9(4).                    
011100 01  WS-IDDISTR REDEFINES WS-IDDISTR-NUM     PIC X(4).                    
011200 01  WS-IDKUNDNR-NUM                         PIC 9(6).                    
011300 01  WS-IDKUNDNR REDEFINES WS-IDKUNDNR-NUM   PIC X(6).                    
011400                                                                          
011500 77  KDRC-DISPLAY            PIC Z(5)   VALUE ZERO.                       
011600     EJECT                                                                
011700                                                                          
011800 01  W-TPO-TIAAMMDD          PIC 9(6).                                    
011900 01  FILLER  REDEFINES W-TPO-TIAAMMDD.                                    
012000     03  W-TPO-TIAA              PIC 9(2).                                
012100     03  W-TPO-TIMM              PIC 9(2).                                
012200     03  W-TPO-TIDD              PIC 9(2).                                
012300                                                                          
012400 01  W-DATUM                 PIC 9(6).                                    
012500 01  FILLER  REDEFINES W-DATUM.                                           
012600     03  W-AAR               PIC 9(02).                                   
012700     03  W-MAANAD            PIC 9(02).                                   
012800     03  W-DAG               PIC 9(02).                                   
012900                                                                          
013000 01  W-TPO-TIAAAAVV          PIC 9(6).                                    
013100 01  FILLER REDEFINES W-TPO-TIAAAAVV.                                     
013200     03  W-TPO-SEKEL         PIC 9(2).                                    
013300     03  W-TPO-TIAAVV        PIC 9(4).                                    
013400*                                                                         
013500*    ---- KONSTANTER                                                      
013600 77  JA                      PIC X       VALUE 'J'.                       
013700 77  NEJ                     PIC X       VALUE 'N'.                       
013800     EJECT                                                                
013900* 01 -COPY W0005            -PRE POSTSUM-                                 
014000     EJECT                                                                
014100 01  FILLER                  PIC X(16)   VALUE 'PRISTILLAREA '.           
014200     SKIP2                                                                
014300*01  -COPY W335PRIS                                                       
014400     EJECT                                                                
014500 01  FILLER                  PIC X(16)   VALUE 'W335PRNO     '.           
014600     SKIP2                                                                
014700*01  -COPY W335PRNO                                                       
014800     EJECT                                                                
014900 01  FILLER                  PIC X(16)   VALUE 'W335PRQU     '.           
015000     SKIP2                                                                
015100*01  -COPY W335PRQU                                                       
015200     EJECT                                                                
015300 01  FILLER                  PIC X(16)   VALUE 'RZA-TRANSAREA'.           
015400     SKIP2                                                                
015500*01  -COPY WDGZRZA                                                        
015600     EJECT                                                                
015700 01  FILLER                  PIC X(16)   VALUE 'SORT-AREA'.               
015800     SKIP2                                                                
015900*01  -COPY W092P001     -PRE SORT-                                        
016000     EJECT                                                                
016100 01  FILLER                  PIC X(16)   VALUE 'RY4-TRANSAREA'.           
016200     SKIP2                                                                
016300*01  -COPY W440300          -PRE RY4-                                     
016400     EJECT                                                                
016500*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
016600     SKIP3                                                                
016700 01  DYNAMISKA-SUBPROGRAM.                                                
016800   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
016900   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
017000   03  WORKDAY               PIC X(8)    VALUE 'WORKDAY '.                
017100   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
017200   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
017300   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
017400   03  W335PRIS              PIC X(8)    VALUE 'W335PRIS'.                
017500   03  W335PRNO              PIC X(8)    VALUE 'W335PRNO'.                
017600   03  W335PRQU              PIC X(8)    VALUE 'W335PRQU'.                
017700   03  WZ01SEND              PIC X(8)    VALUE 'WZ01SEND'.                
017800   03  W411ORDN              PIC X(8)    VALUE 'W411ORDN'.                
017900   03  W006KOM               PIC X(8)    VALUE 'W006KOM '.                
018000     SKIP3                                                                
018100*    ----  PARAMETRAR TILL ABEND                                          
018200 01  RETURKODER.                                                          
018300   03  RKOD-ABEND-UTAN-DUMP  PIC S9(04) COMP SYNC VALUE +16.              
018400   03  RKOD-ABEND-MED-DUMP   PIC S9(04) COMP SYNC VALUE +33.              
018500     EJECT                                                                
018600*    ----  PARAMETRAR TILL WDATKONV                                       
018700                                                                          
018800 01  FILLER                  PIC X(8) VALUE 'WDATKONV'.                   
018900                                                                          
019000*01  FILLER -COPY WDATAREA                                                
019100     EJECT                                                                
019200*    ----  PARAMETRAR TILL WORKDAY                                        
019300                                                                          
019400 01  FILLER                  PIC X(8) VALUE 'WORKDAY '.                   
019500                                                                          
019600*01  FILLER -COPY WORKAREA                                                
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'W411ORDN'.            
019900*    --- PARAMETRAR TILL SUBPROGRAM W411ORDN                              
020000*01 -COPY W411ORDN                                                        
020100     EJECT                                                                
020200                                                                          
020300*    ----  PARAMETRAR TILL IDDISTR                                        
020400                                                                          
020500 01  TEST-IDDISTR            PIC 9(5) COMP-3.                             
020600                                                                          
020700*01  FILLER -COPY WWDIST19 -RED TEST-IDDISTR.                             
020800     EJECT                                                                
020900*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
021000     EJECT                                                                
021100*      --- VALID IDDC CODES                                               
021200*                                                                         
021300*01    -COPY WWDCKONS                                                     
021400                                                                          
021410*      --- BYTESARTIKLAR                                                  
021420*                                                                         
021430*01    -COPY WWBYT03                                                      
021440       EJECT                                                              
021500*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
021600                                                                          
021700 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
021800     SKIP3                                                                
021900*    ---- STATUSKOD FRÅN IMS                                              
022000                                                                          
022100 01  STATUS-WS               PIC XX.                                      
022200     88  SEGMENT-SLUT                     VALUE 'GB'.                     
022300     88  SEGMENT-FINNS                    VALUE '  '.                     
022400     88  SEGMENT-HAR-LAGTS-TILL           VALUE '  '.                     
022500     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
022600     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
022700     88  IMS-EJ-OK                        VALUE 'XD'.                     
022800     SKIP3                                                                
022900 01  GODK-STATUSKODER.                                                    
023000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
023100     SKIP3                                                                
023200 01  SSA1                    PIC X(224).                                  
023300 01  SSA2                    PIC X(224).                                  
023400 01  SSA3                    PIC X(224).                                  
023500     EJECT                                                                
023600*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
023700 01  FILLER                  PIC X(16)   VALUE 'NYCKLAR-TILL-DLI'.        
023800     SKIP2                                                                
023900 01  NYCKLAR-TILL-DLI.                                                    
024000                                                                          
024100   03  W-IDARTNR-X.                                                       
024200     05  W-IDARTNR           PIC S9(9)  COMP-3.                           
024300                                                                          
024400   03  W-IDORDER-X.                                                       
024500     05  W-IDORDER           PIC S9(7)  COMP-3.                           
024600                                                                          
024700   03  W-DABEHOV-X.                                                       
024800     05  W-DABEHOV          PIC  9(6).                                    
024900                                                                          
025000   03  W-WDQ2CSEQ-X.                                                      
025100     05  W-IDDISTR          PIC S9(5)   COMP-3.                           
025200     05  W-IDKUNDNR         PIC S9(7)   COMP-3.                           
025300     05  W-IDKUNDRF         PIC X(10)   VALUE SPACE.                      
025400                                                                          
025500   03  W1-WDA501KY-X.                                                     
025600     05  W1-IDDISTR         PIC S9(5)   COMP-3.                           
025700     05  W1-IDKUNDNR        PIC S9(7)   COMP-3.                           
025800     05  W1-IDKUNDRF        PIC X(10)   VALUE SPACE.                      
025900     05  W1-IDARTNR         PIC S9(9)   COMP-3.                           
026000     05  W1-IDLOPNR         PIC S9(3)   COMP-3.                           
026100                                                                          
026200   03  W2-WDA501KY-X.                                                     
026300     05  W2-IDDISTR         PIC S9(5)   COMP-3.                           
026400     05  W2-IDKUNDNR        PIC S9(7)   COMP-3.                           
026500     05  W2-IDKUNDRF        PIC X(10).                                    
026600     05  W2-IDARTNR         PIC S9(9)   COMP-3.                           
026700     05  W2-IDLOPNR         PIC S9(3)   COMP-3.                           
026800                                                                          
026900   03  W-KDSTARAD-X.                                                      
027000     05  W-KDSTARAD          PIC X.                                       
027100                                                                          
027200   03  W-IDDC-X.                                                          
027300     05  W-IDDC              PIC X(2).                                    
027400                                                                          
027500   03  W-IDGMT-X.                                                         
027600     05 W-IDDISTR-WDB2       PIC S9(5) VALUE ZERO COMP-3.                 
027700     05 W-IDKUNDNR-WDB2      PIC S9(7) VALUE ZERO COMP-3.                 
027800                                                                          
027900     EJECT                                                                
028000*    -COPY W0003                                                          
028100     EJECT                                                                
028200* ---         DLI INOUT OUTPUT AREA                                       
028300* ---         DLI-IO-AREA                                                 
028400                                                                          
028500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK601'.           
028600 01  DLI-IO-WDK601.                                                       
028700*  03  WDK601   -COPY WDK601                                              
028800     EJECT                                                                
028900                                                                          
029000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK611'.           
029100 01  DLI-IO-WDK611.                                                       
029200*  03  WDK611   -COPY WDK611                                              
029300     EJECT                                                                
029400                                                                          
029500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK901'.           
029600 01  DLI-IO-WDK901.                                                       
029700*  03  WDK901 -COPY WDK901                                                
029800     EJECT                                                                
029900                                                                          
030000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK911'.           
030100 01  DLI-IO-WDK911.                                                       
030200*  03  WDK911 -COPY WDK911                                                
030300     EJECT                                                                
030400                                                                          
030500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ZZAC01'.           
030600 01  DLI-IO-ZZAC01.                                                       
030700*  03  WLZZAC01 -COPY WDGZ01  -PRE ZZAC-                                  
030800     EJECT                                                                
030900                                                                          
031000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDQ201'.           
031100 01  DLI-IO-WDQ201.                                                       
031200*  03  -COPY WDQ201  -PRE ORQI-                                           
031300     EJECT                                                                
031400                                                                          
031500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDA501'.           
031600 01  DLI-IO-WDA501.                                                       
031700*  03  -COPY WDA501  -PRE ORDP-                                           
031800     EJECT                                                                
031900                                                                          
032000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDA5-RES'.         
032100 01  DLI-IO-WDA501-RES.                                                   
032200*  03  -COPY WDA501  -PRE RES                                             
032300     EJECT                                                                
032400                                                                          
032500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB2  '.           
032600 01  DLI-IO-WDB2.                                                         
032700*  03  WDB2  -COPY WDB201                                                 
032800                                                                          
032900                                                                          
033000*    MSG-AREA FÖR HOPP TILL W20109                                        
033100 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
033200 01  W-PROG-TO-PROG-SW-1.                                                 
033300     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
033400     03  2109-Z1                   PIC X.                                 
033500     03  2109-Z2                   PIC X.                                 
033600     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
033700     03  2109-IDTRANS              PIC X(4)  VALUE '440 '.                
033800     03  2109-KDMFSFOR             PIC X     VALUE '1'.                   
033900*    03  -COPY W2I10902    -PRE 2109-                                     
034000     EJECT                                                                
034100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
034200     SKIP3                                                                
034300 01  -COPY WZ01SEND                                                       
034400     EJECT                                                                
034500 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
034600     SKIP3                                                                
034700 01  SEND-AREA.                                                           
034800*    03  -COPY WZ01REQU  -PRE 3039-                                       
034900*    03  -COPY W30391I1  -PRE 3039-                                       
035000     EJECT                                                                
035100*    ---  MSG INPUT-OUTPUT AREA                                           
035200*01  -COPY WMSGAREA                                                       
035300     EJECT                                                                
035400 01  FILLER                 PIC X(16)   VALUE 'KOM-OHUV-AREA'.            
035500 01  OHUV-AREA.                                                           
035600*    03   -COPY W4I25101   -PRE OHUV-                                     
035700     EJECT                                                                
035800 01  FILLER                 PIC X(16)   VALUE 'KOM-RAD-AREA'.             
035900 01  ORAD-AREA.                                                           
036000*    05   -COPY W4I25201   -PRE ORAD-                                     
036100     EJECT                                                                
036200*    --- AREOR FÖR W006KOM SUBMODUL                                       
036300 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
036400*01  -COPY WMSGKOM                                                        
036500     EJECT                                                                
036600 LINKAGE SECTION.                                                         
036700     SKIP2                                                                
036800*01  -COPY W0009      -PRE  MSG-                                          
036900     EJECT                                                                
037000 01  0693-PCB                     PIC X.                                  
037100 01  WDP8-PCB                     PIC X.                                  
037200*01  -COPY W0009      -PRE  2109-                                         
037300     EJECT                                                                
037400*01  -COPY W0009      -PRE PRQRY-                                         
037500     SKIP2                                                                
037600*01  -COPY W0008      -PRE  WDK6-                                         
037700       05  FILLER                PIC X.                                   
037800     EJECT                                                                
037900*01  -COPY W0008      -PRE  WDK9-                                         
038000       05  FILLER                PIC X.                                   
038100     EJECT                                                                
038200*01  -COPY W0008      -PRE  WDA51-                                        
038300       05  FILLER                PIC X.                                   
038400     EJECT                                                                
038500*01  -COPY W0008      -PRE  WDA52-                                        
038600       05  FILLER                PIC X.                                   
038700     EJECT                                                                
038800*01  -COPY W0008      -PRE  WDQ2-                                         
038900       05  FILLER                PIC X.                                   
039000     EJECT                                                                
039100*01  -COPY W0008      -PRE  ZZAC-                                         
039200       05  FILLER                PIC X.                                   
039300     EJECT                                                                
039400*01  -COPY W0008      -PRE  WDB2-                                         
039500       05  FILLER                PIC X.                                   
039600     SKIP3                                                                
039700 01  PRIS-ARTC-PCB               PIC X.                                   
039800 01  PRIS-WDK7-PCB               PIC X.                                   
039810 01  PRIS-GMTA-PCB               PIC X.                                   
039900 01  PRIS-BETA-PCB               PIC X.                                   
040000 01  PRIS-PRIA-PCB               PIC X.                                   
040100 01  PRIS-PRIB-PCB               PIC X.                                   
040200 01  PRIS-COST-WDK6-PCB          PIC X.                                   
040300 01  PRIS-COST-WDK7-PCB          PIC X.                                   
040400 01  PRIS-COST-WDF1-PCB          PIC X.                                   
040500 01  PRIS-COST-9305-PCB          PIC X.                                   
040600 01  PRIS-COST-WDK72-PCB         PIC X.                                   
040700 01  PRIS-COST-WDB6-PCB          PIC X.                                   
040900 01  PRNO-3107-PCB               PIC X.                                   
041000 01  PRQU-WDG2-PCB               PIC X.                                   
041100 01  PRQU-WDC7-PCB               PIC X.                                   
041200 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
041300 01  ORDN-XXKP-PCB               PIC X.                                   
041400 01  ORDN-ORQL-PCB               PIC X.                                   
041500 01  ORDN-PROC-PCB               PIC X.                                   
041600 01  ORDN-ORQI-PCB               PIC X.                                   
041700     EJECT                                                                
041800 PROCEDURE DIVISION  USING MSG-PCB                                        
041900                           2109-PCB PRQRY-PCB                             
042000                           0693-PCB  WDP8-PCB                             
042100                           WDK6-PCB                                       
042200                           WDK9-PCB WDA51-PCB                             
042300                           WDA52-PCB WDQ2-PCB ZZAC-PCB WDB2-PCB           
042400                           PRIS-ARTC-PCB PRIS-WDK7-PCB                    
042410                           PRIS-GMTA-PCB                                  
042500                           PRIS-BETA-PCB PRIS-PRIA-PCB                    
042600                           PRIS-PRIB-PCB                                  
042700                           PRIS-COST-WDK6-PCB                             
042800                           PRIS-COST-WDK7-PCB                             
042900                           PRIS-COST-WDF1-PCB                             
043000                           PRIS-COST-9305-PCB                             
043100                           PRIS-COST-WDK72-PCB                            
043200                           PRIS-COST-WDB6-PCB                             
043400                           PRNO-3107-PCB                                  
043500                           PRQU-WDG2-PCB                                  
043600                           PRQU-WDC7-PCB                                  
043700                           PRQU-SJKO-WDK6-PCB                             
043800                           ORDN-XXKP-PCB                                  
043900                           ORDN-ORQL-PCB                                  
044000                           ORDN-PROC-PCB                                  
044100                           ORDN-ORQI-PCB.                                 
044200                                                                          
044300     ENTRY 'DLITCBL' USING MSG-PCB                                        
044400                           2109-PCB PRQRY-PCB                             
044500                           0693-PCB  WDP8-PCB                             
044600                           WDK6-PCB                                       
044700                           WDK9-PCB WDA51-PCB                             
044800                           WDA52-PCB WDQ2-PCB ZZAC-PCB WDB2-PCB           
044900                           PRIS-ARTC-PCB PRIS-WDK7-PCB                    
044910                           PRIS-GMTA-PCB                                  
045000                           PRIS-BETA-PCB PRIS-PRIA-PCB                    
045100                           PRIS-PRIB-PCB                                  
045200                           PRIS-COST-WDK6-PCB                             
045300                           PRIS-COST-WDK7-PCB                             
045400                           PRIS-COST-WDF1-PCB                             
045500                           PRIS-COST-9305-PCB                             
045600                           PRIS-COST-WDK72-PCB                            
045700                           PRIS-COST-WDB6-PCB                             
045900                           PRNO-3107-PCB                                  
046000                           PRQU-WDG2-PCB                                  
046100                           PRQU-WDC7-PCB                                  
046200                           PRQU-SJKO-WDK6-PCB                             
046300                           ORDN-XXKP-PCB                                  
046400                           ORDN-ORQL-PCB                                  
046500                           ORDN-PROC-PCB                                  
046600                           ORDN-ORQI-PCB.                                 
046700                                                                          
046800     PERFORM A-INIT                                                       
046900                                                                          
047000     SORT SORTFIL ASCENDING KEY S-RAD-IDARTNR                             
047100                                S-RAD-IDDISTR                             
047200                                S-RAD-IDKUNDNR                            
047300                                S-RAD-IDKUNDRF                            
047400                                                                          
047500          INPUT  PROCEDURE B-IN-BEHANDLING                                
047600          OUTPUT PROCEDURE C-UT-BEHANDLING                                
047700                                                                          
047800     IF SORT-RETURN > 0                                                   
047900        DISPLAY '***  W4403600  - FEL VID SORTERING ***'                  
048000        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
048100     ELSE                                                                 
048200        IF 2109-MID2-KVANTART > ZERO                                      
048300          PERFORM S03A-STARTA-2109                                        
048400        END-IF                                                            
048500        PERFORM Z-FINIT                                                   
048600        MOVE ZERO TO RETURN-CODE                                          
048700        GOBACK                                                            
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 A-INIT SECTION.                                                          
049200                                                                          
049300     OPEN INPUT W44061                                                    
049400     PERFORM IMS-RESTART                                                  
049500                                                                          
049600     ACCEPT W-DATUM FROM DATE                                             
049700                                                                          
049800     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
049900     MOVE W-DATUM      TO DAT-I-TIDATUM                                   
050000     CALL WDATKONV USING DAT-KDDATFORM                                    
050100                         DAT-I-TIDATUM                                    
050200                         DAT-O-TIDATUM                                    
050300                         DAT-KDSVAR                                       
050400                                                                          
050500     IF DAT-KDSVAR-OK                                                     
050600       MOVE DAT-TIAAMMDD TO W-DAT-TIAAMMDD                                
050700                            WS-DAT                                        
050800       MOVE DAT-TIAAP    TO W-DAT-TIAAP                                   
050900       MOVE DAT-TIAA     TO W-DAT-TIAA                                    
051000       MOVE DAT-TIVV     TO W-DAT-TIVV                                    
051100     ELSE                                                                 
051200     MOVE 'FEL FRÅN PROGRAM W44036 I SECTION A' TO FELTEXT                
051300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
051400     END-IF                                                               
051500                                                                          
051600*    BERÄKNA NÄSTA ARBETSDAG                                              
051700                                                                          
051800     MOVE W-DAT-TIAAMMDD      TO WORK-TIAAMMDD-FOM                        
051900                                 WORK-TIAAMMDD-TOM                        
052000     MOVE 003                 TO WORK-KDCALL                              
052100     MOVE 1                   TO WORK-KVWORKD                             
052200     MOVE WC-CDC-SE           TO WORK-IDDC                                
052300     CALL WORKDAY             USING WORK-KDCALL                           
052400                                    WORK-DATE-AREA                        
052500                                    WORK-KDSVAR                           
052600     IF WORK-KDSVAR-OK                                                    
052700        MOVE WORK-TIAAMMDD-NEXT-WORKDAY TO WS-NEXT-WORKDAY                
052800     ELSE                                                                 
052900        MOVE W-DAT-TIAAMMDD             TO WS-NEXT-WORKDAY                
053000     END-IF                                                               
053100                                                                          
053200     ACCEPT TIAAMMDD FROM DATE                                            
053300     ACCEPT TIKLOCK  FROM TIME                                            
053400                                                                          
053500     MOVE SPACE                 TO 2109-MID2-W2I10902                     
053600     MOVE +1                    TO 2109-IX                                
053700     .                                                                    
053800     EJECT                                                                
053900 B-IN-BEHANDLING SECTION.                                                 
054000                                                                          
054100     READ W44061 INTO S-RAD-W44060 AT END                                 
054200                 MOVE 'EOF' TO INFIL-SLUT                                 
054300     END-READ                                                             
054400                                                                          
054500     PERFORM UNTIL INFIL-SLUT = 'EOF'                                     
054600       IF S-RAD-KDTPOTYP > +0  AND                                        
054700          S-RAD-KDSTARAD = '1' AND                                        
054800          S-RAD-FLTPOBEK = JA                                             
054900              RELEASE S-SORTPOST                                          
055000       END-IF                                                             
055100                                                                          
055200       READ W44061 INTO S-RAD-W44060 AT END                               
055300                   MOVE 'EOF' TO INFIL-SLUT                               
055400       END-READ                                                           
055500                                                                          
055600     END-PERFORM                                                          
055700                                                                          
055800     .                                                                    
055900     EJECT                                                                
056000 C-UT-BEHANDLING SECTION.                                                 
056100                                                                          
056200     PERFORM S06-LAES-SORTFIL                                             
056310                                                                          
056400     PERFORM UNTIL SORTFIL-SLUT = 'EOS'                                   
056600       MOVE S-RAD-IDDISTR       TO W1-IDDISTR W-IDDISTR                   
056700       MOVE S-RAD-IDKUNDNR      TO W1-IDKUNDNR W-IDKUNDNR                 
056800       MOVE S-RAD-IDKUNDRF      TO W1-IDKUNDRF                            
056900       MOVE '00'                TO W-IDKUNDRF(1:2)                        
057000       MOVE S-RAD-IDORDNR5      TO W-IDKUNDRF(3:5)                        
057100       MOVE S-RAD-IDARTNR       TO W1-IDARTNR W-IDARTNR                   
057200       MOVE S-RAD-IDLOPNR       TO W1-IDLOPNR                             
057300       MOVE '1'                 TO W-KDSTARAD                             
057400       MOVE S-RAD-IDDC          TO W-IDDC                                 
057410                                                                          
057600         PERFORM IMS-GHU-ORDP-WDA5-OTAECKT                                
057700         IF SEGMENT-FINNS                                                 
057800           IF ORDP-RAD-KDTPOTYP = 7                                       
057900             PERFORM CA-KOLLA-FLRELSP                                     
058000           ELSE                                                           
058100             PERFORM CA-KOLLA-FLRELSP                                     
058200             PERFORM CB-KOLLA-TPO-DATUM-UPPNADD                           
058300           END-IF                                                         
058400           IF (W-TPO-DATUM-UPPNADD = JA AND CLAG-FLRELSP = NEJ) OR        
058500              (ORDP-RAD-KDTPOTYP = 7 AND CLAG-FLRELSP = NEJ)              
058600             PERFORM CC-AKTIVERA-TPO                                      
058700           END-IF                                                         
058800           IF W-KVUPDAT > 100                                             
058900             PERFORM S07-TA-CHECKPOINT                                    
059000           END-IF                                                         
059200         END-IF                                                           
059300       PERFORM S06-LAES-SORTFIL                                           
059400     END-PERFORM                                                          
059500     .                                                                    
059600     EJECT                                                                
059700 CA-KOLLA-FLRELSP SECTION.                                                
059800                                                                          
059900     PERFORM IMS-GU-ARTC-WDK601                                           
060000                                                                          
060100     PERFORM IMS-GNP-ARTC-WDK611                                          
060200     .                                                                    
060300                                                                          
060400 CB-KOLLA-TPO-DATUM-UPPNADD SECTION.                                      
060500                                                                          
060600     MOVE ORDP-RAD-TITPO   TO DAT-I-TIDATUM                               
060700     MOVE 'AAMMDD'    TO DAT-KDDATFORM                                    
060800     CALL WDATKONV USING DAT-KDDATFORM                                    
060900                         DAT-I-TIDATUM                                    
061000                         DAT-O-TIDATUM                                    
061100                         DAT-KDSVAR                                       
061200                                                                          
061300     IF DAT-KDSVAR-OK                                                     
061400       MOVE DAT-TIAAMMDD   TO W-TPO-TIAAMMDD                              
061500       MOVE DAT-TIAAVV-GRP TO W-TPO-TIAAVV                                
061600       MOVE DAT-TISEKEL    TO W-TPO-SEKEL                                 
061700     ELSE                                                                 
061800       MOVE 'FEL FRÅN PROGRAM W44036 I SECTION CB' TO FELTEXT             
061900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
062000     END-IF                                                               
062100                                                                          
062200     MOVE NEJ TO W-TPO-DATUM-UPPNADD                                      
062300     IF W-TPO-TIAAMMDD <= WS-NEXT-WORKDAY                                 
062400       MOVE JA    TO W-TPO-DATUM-UPPNADD                                  
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800 CC-AKTIVERA-TPO SECTION.                                                 
062900                                                                          
063000     MOVE ORDP-RAD-IDDISTR  TO TEST-IDDISTR                               
063100     IF DIST79-DEALER-PRICE                                               
063200       PERFORM S20-KOMPLETTERA-PRIS                                       
063300     ELSE                                                                 
063400        IF ORDP-RAD-PRARTNTO = +0                                         
063600         PERFORM S02-PRISTILLEMPA                                         
063700        END-IF                                                            
063800     END-IF                                                               
063900                                                                          
064000     MOVE ORDP-RAD-KVART TO W-SUTPO-TOT                                   
064100     MOVE ORDP-RAD-KDORDING TO W-KDORDING                                 
064200     IF ORDP-RAD-KDTPOTYP = 1 OR 2 OR 7                                   
064300       MOVE ORDP-RAD-KVART TO W-SUTPO-PB                                  
064400     ELSE                                                                 
064500       MOVE ORDP-RAD-KVART TO W-SUTPO-EJPB                                
064600     END-IF                                                               
064700     EVALUATE ORDP-RAD-KDORDKL                                            
064800     WHEN 0                                                               
064900       MOVE ORDP-RAD-KVART TO W-KVOKS-VOR                                 
065000     WHEN 1                                                               
065100       MOVE ORDP-RAD-KVART TO W-KVOKS-DAG                                 
065200     WHEN 2                                                               
065300       MOVE ORDP-RAD-KVART TO W-KVOKS-BULK                                
065400     WHEN 3                                                               
065500       MOVE ORDP-RAD-KVART TO W-KVOKS-BULK                                
065600     WHEN 4                                                               
065700       MOVE ORDP-RAD-KVART TO W-KVOKS-BULK                                
065800     END-EVALUATE                                                         
065900                                                                          
066000     MOVE ORDP-RAD-IDDISTR TO TEST-IDDISTR                                
066100     IF NOT DIST19-SATS                                                   
066200       PERFORM CCA-BEHANDLA-ORDERRAD                                      
066300       IF W-KDORDING = 1 OR 2                                             
066400         IF ORDP-RAD-KDTPOTYP NOT = 6                                     
066500           PERFORM S03-SKAPA-W2I10902                                     
066600         END-IF                                                           
066700       END-IF                                                             
066800       IF ORDP-RAD-KDTPOTYP = 6                                           
066900         PERFORM S05-SKAPA-RY4-TRANS                                      
067000       ELSE                                                               
067100         PERFORM S04-SKAPA-RZA-TRANS                                      
067200       END-IF                                                             
067300     END-IF                                                               
067400     IF ORDP-RAD-KDTPOTYP NOT = 4                                         
067500       PERFORM S01-UPPD-SALDO-WDK9                                        
067600     END-IF                                                               
067700     .                                                                    
067800     EJECT                                                                
067900 CCA-BEHANDLA-ORDERRAD SECTION.                                           
068000                                                                          
068100     MOVE NEJ TO W-SAMMANSLAGNING-RAD                                     
068200     PERFORM CCAA-SAMMANSLAGNING-RAD                                      
068300     PERFORM CCAB-UPPDATERA-WDA5                                          
068400     PERFORM IMS-GU-ORQI-WDQ2                                             
068500     .                                                                    
068600     EJECT                                                                
068700 CCAA-SAMMANSLAGNING-RAD SECTION.                                         
068900     MOVE LOW-VALUE         TO W1-WDA501KY-X                              
069000     MOVE HIGH-VALUE        TO W1-WDA501KY-X                              
069100     MOVE ORDP-RAD-IDDISTR  TO W1-IDDISTR    W2-IDDISTR                   
069200     MOVE ORDP-RAD-IDKUNDNR TO W1-IDKUNDNR   W2-IDKUNDNR                  
069300     MOVE ORDP-RAD-IDKUNDRF TO W1-IDKUNDRF   W2-IDKUNDRF                  
069400     MOVE ORDP-RAD-IDARTNR  TO W1-IDARTNR    W2-IDARTNR                   
069500     MOVE +0                TO W1-IDLOPNR                                 
069600     MOVE +999              TO W2-IDLOPNR                                 
069700     MOVE '3'               TO W-KDSTARAD                                 
069800     MOVE ORDP-RAD-IDDC     TO W-IDDC                                     
069900                                                                          
070000     PERFORM IMS-GHU-ORDP-WDA5-RES                                        
070100                                                                          
070200     PERFORM UNTIL SEGMENT-SAKNAS OR W-SAMMANSLAGNING-RAD = JA            
070300       IF RESRAD-KDFRAKT  = ORDP-RAD-KDFRAKT  AND                         
070400          RESRAD-KDORDKL  = ORDP-RAD-KDORDKL  AND                         
070500          RESRAD-PRARTNTO = ORDP-RAD-PRARTNTO AND                         
070600          RESRAD-PRARTNTO-loc = ORDP-RAD-PRARTNTO-loc and                 
070700          RESRAD-KDTPOTYP = ORDP-RAD-KDTPOTYP AND                         
070800          RESRAD-DARODAT  = ZERO                                          
070900         MOVE JA TO W-SAMMANSLAGNING-RAD                                  
071000       ELSE                                                               
071100         PERFORM IMS-GHN-ORDP-WDA5-RES                                    
071200       END-IF                                                             
071300     END-PERFORM                                                          
071400     .                                                                    
071500     EJECT                                                                
071600 CCAB-UPPDATERA-WDA5 SECTION.                                             
071700                                                                          
071800     IF W-SAMMANSLAGNING-RAD = JA                                         
071900        ADD ORDP-RAD-KVART  TO RESRAD-KVART                               
072000        MOVE W-DAT-TIAAMMDD TO RESRAD-TIRES                               
072100        PERFORM IMS-REPL-ORDP-WDA5-RES                                    
072200        ADD +1              TO W-KVUPDAT                                  
072300                                                                          
072400        MOVE ORDP-RAD-IDDISTR     TO W1-IDDISTR                           
072500        MOVE ORDP-RAD-IDKUNDNR    TO W1-IDKUNDNR                          
072600        MOVE ORDP-RAD-IDKUNDRF    TO W1-IDKUNDRF                          
072700        MOVE ORDP-RAD-IDARTNR     TO W1-IDARTNR                           
072800        MOVE ORDP-RAD-IDLOPNR     TO W1-IDLOPNR                           
072900        MOVE ORDP-RAD-IDDC        TO W-IDDC                               
073000                                                                          
073100        PERFORM IMS-GHU-ORDP-WDA5                                         
073200        IF SEGMENT-FINNS                                                  
073300           PERFORM IMS-DLET-ORDP-WDA5                                     
073400           ADD +1              TO W-KVUPDAT                               
073500        END-IF                                                            
073600     ELSE                                                                 
073700       MOVE W-DAT-TIAAMMDD  TO ORDP-RAD-TIRES                             
073800       MOVE '3'             TO ORDP-RAD-KDSTARAD                          
073900                                                                          
074000**SKAPA 'DÖSKALLAR' OM DET ÄR EN VERKSTADSORDER                           
074100       IF ORDP-RAD-IDKUNDRF-WIP NOT = SPACE AND                           
074200          ORDP-RAD-IDKUNDRF-WIP NOT = W-IDKUNDRF-WIP                      
074300         PERFORM S11-SKAPA-ORDERHUVUD                                     
074400       END-IF                                                             
074500                                                                          
074600       PERFORM IMS-REPL-ORDP-WDA5                                         
074700       ADD +1              TO W-KVUPDAT                                   
074800     END-IF                                                               
074900                                                                          
075000     .                                                                    
075100     EJECT                                                                
075200 Z-FINIT SECTION.                                                         
075300     SKIP2                                                                
075400     CLOSE W44061                                                         
075500     .                                                                    
075600     EJECT                                                                
075700 S01-UPPD-SALDO-WDK9 SECTION.                                             
075800                                                                          
075900     IF W-SUTPO-TOT > +0                                                  
076000       PERFORM IMS-GHU-ARTM-WDK901                                        
076100                                                                          
076200       ADD W-KVOKS-BULK TO ART-KVOKS-BULK                                 
076300       ADD W-KVOKS-DAG  TO ART-KVOKS-DAG                                  
076400       ADD W-KVOKS-VOR  TO ART-KVOKS-VOR                                  
076500*LASSI DET VAR FEL ATT UPPD SUTPO O. BEHOV FÖR BASLAGRET DÅ               
076600****** DETTA GÖRS REDAN I W115 !!!                                        
076700       IF ORDP-RAD-KDTPOTYP NOT = 3                                       
076800         SUBTRACT W-SUTPO-TOT FROM ART-SUTPO-TOT                          
076900       END-IF                                                             
077000                                                                          
077100       PERFORM IMS-REPL-ARTM-WDK901                                       
077200       ADD +1              TO W-KVUPDAT                                   
077300                                                                          
077400       IF ORDP-RAD-KDTPOTYP NOT = 3                                       
077500         MOVE W-TPO-TIAAAAVV TO W-DABEHOV                                 
077600                                                                          
077700         PERFORM IMS-GHNP-ARTM-WDK911                                     
077800                                                                          
077900         IF SEGMENT-FINNS                                                 
078000           SUBTRACT W-SUTPO-PB FROM ANT-SUTPO-PB                          
078100           SUBTRACT W-SUTPO-EJPB FROM ANT-SUTPO-EJPB                      
078200           IF ANT-SUTPO-PB = ZERO AND                                     
078300              ANT-SUTPO-EJPB = ZERO                                       
078400              PERFORM IMS-DLET-ARTM-WDK9                                  
078500              ADD +1            TO W-KVUPDAT                              
078600           ELSE                                                           
078700              PERFORM IMS-REPL-ARTM-WDK911                                
078800              ADD +1            TO W-KVUPDAT                              
078900           END-IF                                                         
079000         ELSE                                                             
079100           DISPLAY ' DISTRIKT  ' ORDP-RAD-IDDISTR                         
079200           DISPLAY ' KUNDNR    ' ORDP-RAD-IDKUNDNR                        
079300           DISPLAY ' ORDERNR   ' ORDP-RAD-IDORDNR5                        
079400           DISPLAY ' ARTIKELNR ' ORDP-RAD-IDARTNR                         
079500           DISPLAY ' ANTAL     ' ORDP-RAD-KVART                           
079600           DISPLAY ' DASENBEK  ' ORDP-RAD-DASENDAT                        
079700         END-IF                                                           
079800       END-IF                                                             
079900     END-IF                                                               
080000                                                                          
080100     MOVE ZERO TO W-KVOKS-BULK W-KVOKS-DAG W-KVOKS-VOR                    
080200                  W-SUTPO-TOT  W-SUTPO-PB  W-SUTPO-EJPB                   
080300     .                                                                    
080400     EJECT                                                                
080500 S02-PRISTILLEMPA  SECTION.                                               
080600                                                                          
080700     MOVE 1                   TO PRIS-KDCALL                              
080710     MOVE PGM-NAMN            TO PRIS-IDPGM                               
080800     MOVE ORDP-RAD-IDARTNR    TO PRIS-IDARTNR                             
080900     MOVE ORDP-RAD-IDDISTR    TO PRIS-IDDISTR                             
081000                                 TEST-IDDISTR                             
081100     MOVE ORDP-RAD-IDKUNDNR   TO PRIS-IDKUNDNR                            
081200     MOVE ORDP-RAD-IDDC       TO PRIS-IDDC                                
081300     MOVE ORDP-RAD-KDORDKL    TO PRIS-KDORDKL                             
081400     MOVE ORDP-RAD-KVART      TO PRIS-KVBEART                             
081500     MOVE ORDP-RAD-FLINVEST   TO PRIS-FLINVEST                            
081600                                                                          
081700     CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                      
081800                         PRIS-WDK7-PCB                                    
081810                         PRIS-GMTA-PCB PRIS-BETA-PCB PRIS-PRIA-PCB        
081900                         PRIS-PRIB-PCB                                    
082000                         PRIS-COST-WDK6-PCB                               
082100                         PRIS-COST-WDK7-PCB                               
082200                         PRIS-COST-WDF1-PCB                               
082300                         PRIS-COST-9305-PCB                               
082400                         PRIS-COST-WDK72-PCB                              
082500                         PRIS-COST-WDB6-PCB                               
082700                                                                          
082800     IF PRIS-KDSVAR = ' '                                                 
082900       MOVE PRIS-PRARTNTO TO ORDP-RAD-PRARTNTO                            
083000       MOVE PRIS-FLPRTILL TO ORDP-RAD-FLPRTILL                            
083100       MOVE PRIS-KDPRTYP  TO ORDP-RAD-KDPRTYP                             
083700       MOVE PRIS-KDVALISO TO ORDP-RAD-KDVALISO                            
083900     ELSE                                                                 
084000       MOVE 'FEL FRÅN PROGRAM W44036 I SECTION S02' TO FELTEXT            
084100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
084200     END-IF                                                               
084300                                                                          
084400     .                                                                    
084500     EJECT                                                                
084600 S03-SKAPA-W2I10902 SECTION.                                              
084700                                                                          
084710*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
084720     MOVE ORDP-RAD-IDARTNR   TO BYT03-IDARTNR                             
084730     IF NOT BYT03-OBJEKT                                                  
084740                                                                          
084800        MOVE 2109-IX          TO 2109-MID2-KVANTART                       
084900        MOVE ORDP-RAD-IDARTNR TO 2109-MID2-IDARTNR (2109-IX)              
085000        MOVE ORDP-RAD-IDDC    TO 2109-MID2-IDDC (2109-IX)                 
085100        MOVE '+'              TO 2109-MID2-KDTECKEN (2109-IX)             
085200        MOVE 'DT'             TO 2109-MID2-KDOI (2109-IX)                 
085300        MOVE SPACE            TO 2109-MID2-CLEARGROUP (2109-IX)           
085400        MOVE ORDP-RAD-KVART   TO 2109-MID2-KVOI (2109-IX)                 
085500        MOVE W-DATUM          TO 2109-MID2-TIUPPDAT (2109-IX)             
085600                                                                          
085700        ADD +1                TO 2109-IX                                  
085800        IF 2109-IX > 2109-IX-MAX                                          
085900          PERFORM S03A-STARTA-2109                                        
086000        END-IF                                                            
086010     END-IF                                                               
086100     .                                                                    
086200     EJECT                                                                
086300 S03A-STARTA-2109 SECTION.                                                
086400                                                                          
086500     COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                
086600                                                                          
086700     PERFORM IMS-PURG-ALT-MSG-2109                                        
086800                                                                          
086900     MOVE SPACE              TO 2109-MID2-W2I10902                        
087000     MOVE +1                 TO 2109-IX                                   
087100     .                                                                    
087200     EJECT                                                                
087300 S04-SKAPA-RZA-TRANS SECTION.                                             
087400                                                                          
087500     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
087600     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
087700     ADD +1 TO ZZAC-IDLOGLOP                                              
087800     IF ZZAC-IDLOGLOP = 0                                                 
087900       ADD +1   TO ZZAC-TIKLOCK                                           
088000       MOVE +1  TO ZZAC-IDLOGLOP                                          
088100     END-IF                                                               
088200     PERFORM IMS-GU-ARTC-WDK601                                           
088300                                                                          
088400     MOVE 'RZA'               TO RZA-IDPTYP                               
088500     MOVE ORDP-RAD-BERADREF   TO RZA-BERADREF                             
088600     MOVE ORQI-OHUV-BEVARREF  TO RZA-BEVARREF                             
088700     MOVE ORDP-RAD-BEVOLREF   TO RZA-BEVOLREF                             
088800     MOVE ORDP-RAD-IDARTNR    TO RZA-IDARTNR                              
088900     MOVE ORDP-RAD-IDKUNDRF   TO RZA-IDKUNDRF                             
089000     MOVE ORDP-RAD-IDSYSTEM   TO RZA-IDSYSTEM                             
089100     MOVE NEJ                 TO RZA-FLABON                               
089200     MOVE ORDP-RAD-FLINVEST   TO RZA-FLINVEST                             
089300     MOVE ORDP-RAD-IDDC       TO RZA-IDDC                                 
089400     MOVE ORDP-RAD-KDDSP      TO RZA-KDDSP                                
089500     MOVE ORDP-RAD-KDFAKTYP   TO RZA-KDFAKTYP                             
089600     IF ORDP-RAD-KDPRTYP = 'P'                                            
089700         MOVE 'M'             TO RZA-KDMANPR                              
089800     ELSE                                                                 
089900       MOVE ORDP-RAD-FLPRTILL TO RZA-KDMANPR                              
090000     END-IF                                                               
090100     MOVE ORDP-RAD-KDORDKL    TO RZA-KDORDKL                              
090200     MOVE ORDP-RAD-KDTPOTYP   TO RZA-KDTPOTYP                             
090300     MOVE ORDP-RAD-KDVRINFO   TO RZA-KDVRINFO                             
090400     MOVE ORDP-RAD-KVART      TO RZA-KVBEART                              
090500     MOVE ORDP-RAD-PRARTNTO   TO RZA-PRARTNTO                             
090610     IF DIST79-DEALER-PRICE                                               
090700       MOVE ORDP-RAD-PRARTNTO-LOC TO RZA-PRARTNTO                         
090800     END-IF                                                               
090900     MOVE ORDP-RAD-REKSIFFR   TO RZA-REKSIFFR                             
091000     MOVE W-DATUM             TO RZA-TIORDREG                             
091100     MOVE ART-KDPRODSL        TO RZA-KDPRODSL                             
091200     MOVE ART-IDFKNGRP        TO RZA-IDFKNGRP                             
091300     IF ORDP-RAD-KDTPOTYP = 1                                             
091400       IF ORDP-RAD-IDSYSTEM = 'VR'                                        
091500         MOVE 1 TO RZA-KDVRTPO                                            
091600       ELSE                                                               
091700         MOVE 2 TO RZA-KDVRTPO                                            
091800       END-IF                                                             
091900     ELSE                                                                 
092000       MOVE 0 TO RZA-KDVRTPO                                              
092100     END-IF                                                               
092200                                                                          
092300     PERFORM IMS-GNP-ARTC-WDK611                                          
092400                                                                          
092500     MOVE CLAG-PRARTBTO-EXP  TO RZA-PRARTBTO-EXP                          
092600                                                                          
092700     MOVE RZA-WDGZRZA        TO ZZAC-LOGGPOST                             
092800     MOVE SPACE              TO SORT-W092P001-ctx                         
092900     MOVE ORDP-RAD-IDDISTR   TO SORT-IDDISTR-S                            
093000     MOVE ORDP-RAD-IDKUNDNR  TO SORT-IDKUNDNR-S                           
093100     MOVE SORT-W092P001-ctx  TO ZZAC-SORTPOST                             
093200     PERFORM IMS-ISRT-ZZAC-WDG6                                           
093300     ADD +1              TO W-KVUPDAT                                     
093400     IF SEGMENT-FINNS-REDAN                                               
093500       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
093600         ADD +1 TO ZZAC-IDLOGLOP                                          
093700         IF ZZAC-IDLOGLOP = 0                                             
093800           ADD +1 TO ZZAC-TIKLOCK                                         
093900           MOVE +1 TO ZZAC-IDLOGLOP                                       
094000         END-IF                                                           
094100         PERFORM IMS-ISRT-ZZAC-WDG6                                       
094200       END-PERFORM                                                        
094300     END-IF                                                               
094400                                                                          
094500     .                                                                    
094600     EJECT                                                                
094700 S05-SKAPA-RY4-TRANS SECTION.                                             
094800                                                                          
094900     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
095000     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
095100     ADD +1 TO ZZAC-IDLOGLOP                                              
095200     IF ZZAC-IDLOGLOP = 0                                                 
095300       ADD +1   TO ZZAC-TIKLOCK                                           
095400       MOVE +1  TO ZZAC-IDLOGLOP                                          
095500     END-IF                                                               
095600                                                                          
095700     MOVE 'RY4'                   TO RY4-IDPTYP                           
095800     MOVE ORDP-RAD-IDDC           TO RY4-IDDC                             
095900     MOVE ORDP-RAD-IDDISTR        TO RY4-IDDISTR                          
096000     MOVE ORDP-RAD-IDKUNDNR       TO RY4-IDKUNDNR                         
096100     MOVE ORDP-RAD-IDKUNDRF (1:5) TO RY4-IDRONR                           
096200     MOVE ORDP-RAD-IDARTNR        TO RY4-IDARTNR                          
096300     MOVE ORDP-RAD-KVART          TO RY4-KVRO                             
096400     MOVE ORDP-RAD-KDORDKL        TO RY4-KDORDKL                          
096500     MOVE ORDP-RAD-KDFAKTYP       TO RY4-KDFAKTYP                         
096600     MOVE ORDP-RAD-KDVRINFO       TO RY4-KDVRINFO                         
096700                                                                          
096800     MOVE RY4-W440300   TO ZZAC-LOGGPOST                                  
096900     MOVE SPACE         TO ZZAC-SORTPOST                                  
097000     PERFORM IMS-ISRT-ZZAC-WDG6                                           
097100     ADD +1              TO W-KVUPDAT                                     
097200     IF SEGMENT-FINNS-REDAN                                               
097300       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
097400         ADD +1 TO ZZAC-IDLOGLOP                                          
097500         IF ZZAC-IDLOGLOP = 0                                             
097600           ADD +1 TO ZZAC-TIKLOCK                                         
097700           MOVE +1 TO ZZAC-IDLOGLOP                                       
097800         END-IF                                                           
097900         PERFORM IMS-ISRT-ZZAC-WDG6                                       
098000       END-PERFORM                                                        
098100     END-IF                                                               
098200                                                                          
098300     .                                                                    
098400     EJECT                                                                
098500 S06-LAES-SORTFIL SECTION.                                                
098600                                                                          
098700     RETURN SORTFIL AT END                                                
098800       MOVE 'EOS' TO SORTFIL-SLUT                                         
098900     END-RETURN                                                           
099000     .                                                                    
099100     EJECT                                                                
099200 S07-TA-CHECKPOINT SECTION.                                               
099300                                                                          
099400     PERFORM IMS-CHECKPOINT                                               
099500                                                                          
099600     MOVE +0   TO W-KVUPDAT                                               
099700     .                                                                    
099800     EJECT                                                                
099900 S11-SKAPA-ORDERHUVUD  SECTION.                                           
100000*****************************************************************         
100100*                                                                         
100200*    TPO-RAD FRÅN LDC VERKSTADSORDER SKALL BIPACKAS SÅ SNART SOM          
100300*    MÖJLIGT MED EN SEPARAT ORDER PER VERKSTADSORDER. HÄR SKAPAS          
100400*    ORDERHUVUDEN SOM KOMMER ATT BIPACKA RADEN I W411BIPA.                
100500*                                                                         
100600*****************************************************************         
100700                                                                          
100800     PERFORM S11A-SKAPA-ORDERNR                                           
100900     PERFORM S11B-SKAPA-TRANS-ORDERHUVUD                                  
101000     PERFORM S11C-SKAPA-HUVUD-ORDERRADER                                  
101100     PERFORM S12-SKICKA-TRANS                                             
101200                                                                          
101300                                                                          
101400     .                                                                    
101500                                                                          
101600 S11A-SKAPA-ORDERNR  SECTION.                                             
101700*****************************************************************         
101800*                                                                         
101900*    TA UT ETT ORDERNUMMER MHA W411ORDN.                                  
102000*                                                                         
102100*****************************************************************         
102200                                                                          
102300                                                                          
102400     MOVE 'W440'              TO ORDN-IDSYSTEM                            
102500     MOVE ORDP-RAD-IDDISTR    TO ORDN-IDDISTR                             
102600     MOVE ORDP-RAD-IDKUNDNR   TO ORDN-IDKUNDNR                            
102700     MOVE ZERO                TO ORDN-IDORDNR-IN                          
102800                                                                          
102900     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
103000                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
103100                                                                          
103200     MOVE ORDN-IDORDNR-UT TO WS-IDORDNR-NUM                               
103300     .                                                                    
103400 S11B-SKAPA-TRANS-ORDERHUVUD SECTION.                                     
103500                                                                          
103600     MOVE SPACE         TO MSG-KOM-WMSGKOM                                
103700     MOVE +54           TO MSG-KOM-KVLL                                   
103800     MOVE LOW-VALUE     TO MSG-KOM-KDZ1                                   
103900                           MSG-KOM-KDZ2                                   
104000     MOVE SPACE         TO MSG-KOM-KDTRANS                                
104100     MOVE 'W4I25101'    TO MSG-KOM-IDCPYTXT                               
104200     MOVE 'LDC-RO  '    TO MSG-KOM-IDSNDNOD                               
104300     MOVE 'W4403600'    TO MSG-KOM-IDSNDJOB                               
104400     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
104500     ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                    
104600     MOVE SPACE         TO MSG-KOM-IDMFSMED                               
104700                           MSG-KOM-KDSVAR                                 
104800                                                                          
104900     MOVE LENGTH OF OHUV-MID-W4I25101 TO MSG-KVLL                         
105000     ADD +17                TO MSG-KVLL                                   
105100     MOVE LOW-VALUE         TO MSG-KDZ1                                   
105200                               MSG-KDZ2                                   
105300     MOVE 'W4T251X'         TO MSG-KDTRANS-1                              
105400     MOVE '4251'            TO MSG-IDTRANS-1                              
105500     MOVE '1'               TO MSG-KDMFSFOR-1                             
105600                                                                          
105700     MOVE SPACE             TO OHUV-MID-W4I25101                          
105830     IF ORDP-RAD-IDSYSTEM (1:3) = 'LYN'                                   
105840        MOVE 'LYND'         TO OHUV-MID-IDSYSTEM                          
105850     ELSE                                                                 
105860        IF ORDP-RAD-IDSYSTEM (1:3) = 'POL'                                
105870           MOVE 'POLD'      TO OHUV-MID-IDSYSTEM                          
105880        ELSE                                                              
105890           IF ORDP-RAD-IDSYSTEM (1:3) = 'ECO'                             
105891             MOVE 'ECOD'      TO OHUV-MID-IDSYSTEM                        
105892           ELSE                                                           
105893             IF ORDP-RAD-IDSYSTEM (1:3) = 'VOU'                           
105894                MOVE 'VOUD'      TO OHUV-MID-IDSYSTEM                     
105895             ELSE                                                         
105896               IF ORDP-RAD-IDSYSTEM (1:3) = 'TAD'                         
105897                  MOVE 'TADD'    TO OHUV-MID-IDSYSTEM                     
105898               ELSE                                                       
105899                 IF ORDP-RAD-IDSYSTEM (1:3) = 'ACC'                       
105900                    MOVE 'ACCD'    TO OHUV-MID-IDSYSTEM                   
105901                 ELSE                                                     
105902                   IF ORDP-RAD-IDSYSTEM (1:3) = 'APA'                     
105903                      MOVE 'APAD'  TO OHUV-MID-IDSYSTEM                   
105904                   ELSE                                                   
105905                     IF ORDP-RAD-IDSYSTEM (1:3) = 'APB'                   
105906                        MOVE 'APBD' TO OHUV-MID-IDSYSTEM                  
105907                     ELSE                                                 
105908                       IF ORDP-RAD-IDSYSTEM (1:3) = 'APC'                 
105909                          MOVE 'APCD' TO OHUV-MID-IDSYSTEM                
105910                       ELSE                                               
105911                         IF ORDP-RAD-IDSYSTEM (1:3) = 'APD'               
105912                            MOVE 'APDD'      TO OHUV-MID-IDSYSTEM         
105913                         ELSE                                             
105914                           IF ORDP-RAD-IDSYSTEM (1:3) = 'APE'             
105915                              MOVE 'APED'    TO OHUV-MID-IDSYSTEM         
105916                           ELSE                                           
105917                             IF ORDP-RAD-IDSYSTEM (1:3) = 'APF'           
105918                               MOVE 'APFD'    TO OHUV-MID-IDSYSTEM        
105919                             ELSE                                         
105920                               IF ORDP-RAD-IDSYSTEM (1:3) = 'APG'         
105921                                  MOVE 'APGD' TO OHUV-MID-IDSYSTEM        
105922                               ELSE                                       
105923                                 IF ORDP-RAD-IDSYSTEM (1:3) =             
105924                                                            'APH'         
105925                                    MOVE 'APHD'                           
105926                                              TO OHUV-MID-IDSYSTEM        
105927                                 ELSE                                     
105928                                  IF ORDP-RAD-IDSYSTEM (1:3) =            
105929                                                            'API'         
105930                                     MOVE 'APID'                          
105931                                              TO OHUV-MID-IDSYSTEM        
105932                                  ELSE                                    
105933                                    IF ORDP-RAD-IDSYSTEM (1:3) =          
105934                                                            'APJ'         
105935                                       MOVE 'APJD'                        
105936                                              TO OHUV-MID-IDSYSTEM        
105937                                    ELSE                                  
105938                                       MOVE 'LDCD'                        
105939                                              TO OHUV-MID-IDSYSTEM        
105940                                    END-IF                                
105941                                  END-IF                                  
105942                                 END-IF                                   
105943                               END-IF                                     
105944                             END-IF                                       
105945                           END-IF                                         
105946                         END-IF                                           
105947                       END-IF                                             
105948                      END-IF                                              
105949                    END-IF                                                
105950                 END-IF                                                   
105951               END-IF                                                     
105952             END-IF                                                       
105953           END-IF                                                         
105954        END-IF                                                            
105955     END-IF                                                               
105960     MOVE ORDP-RAD-IDDISTR  TO WS-IDDISTR-NUM                             
106000     MOVE WS-IDDISTR        TO OHUV-MID-IDDISTR                           
106100     MOVE ORDP-RAD-IDKUNDNR TO WS-IDKUNDNR-NUM                            
106200     MOVE WS-IDKUNDNR       TO OHUV-MID-IDKUNDNR                          
106300     MOVE WS-IDORDNR        TO OHUV-MID-IDORDNR                           
106400     MOVE ORDP-RAD-KDORDKL  TO OHUV-MID-KDORDKL                           
106500     IF ORDP-RAD-IDKONTO = ZERO                                           
106600        MOVE SPACE          TO OHUV-MID-IDKONTO                           
106700     ELSE                                                                 
106800        MOVE ORDP-RAD-IDKONTO    TO WS-IDKONTO-DISPLAY                    
106900        MOVE WS-IDKONTO-DISPLAY  TO OHUV-MID-IDKONTO                      
107000        MOVE '57'                TO OHUV-MID-IDFTG                        
107100     END-IF                                                               
107200     MOVE SPACE             TO OHUV-MID-IDKST                             
107300     MOVE 'P'               TO OHUV-MID-KDROPACK                          
107400     MOVE ORDP-RAD-IDANALYS TO OHUV-MID-IDANALYS                          
107500     MOVE ORDP-RAD-IDDC     TO OHUV-MID-IDDC                              
107600     MOVE NEJ               TO OHUV-MID-FLAUTFAK                          
107700     MOVE NEJ               TO OHUV-MID-FLAUTPAC                          
107800                               OHUV-MID-FLEMBORD                          
107900                               OHUV-MID-FLOVRLEV                          
108000                               OHUV-MID-FLFORBI                           
108100     MOVE ORDP-RAD-KDORDTYP-LDC TO OHUV-MID-KDORDTYP-LDC                  
108200     PERFORM S11BA-SKAPA-RFSDATUM                                         
108300     MOVE WS-TIRFS          TO OHUV-MID-TIRFS                             
108400     MOVE ORDP-RAD-TIREPDAT      TO OHUV-MID-TIREPDAT                     
108500     MOVE NEJ                    TO OHUV-MID-FLORDTIL                     
108700                                    OHUV-MID-IDGROSS                      
108800***FÖR ATT HÅLLA REDA PÅ ATT DET ÄR SKAPAT ETT ORDERHUVUD FÖR EN          
108900***SPECIFIK VERKSTADSORDER                                                
109000     MOVE ORDP-RAD-IDKUNDRF-WIP  TO W-IDKUNDRF-WIP                        
109100***                                                                       
109200     MOVE OHUV-AREA TO MSG-MID-OUT                                        
109300***  DISPLAY OHUV-AREA                                                    
109400     PERFORM S12-SKICKA-TRANS                                             
109500     .                                                                    
109600     EJECT                                                                
109700 S11C-SKAPA-HUVUD-ORDERRADER SECTION.                                     
109800                                                                          
109900     MOVE 'W4I25201'    TO MSG-KOM-IDCPYTXT                               
110000     MOVE LENGTH OF ORAD-MID-W4I25201 TO MSG-KVLL                         
110100     ADD +17            TO MSG-KVLL                                       
110200     MOVE LOW-VALUE     TO MSG-KDZ1                                       
110300                           MSG-KDZ2                                       
110400     MOVE 'W4T252X'     TO MSG-KDTRANS-1                                  
110500     MOVE '4252'        TO MSG-IDTRANS-1                                  
110600     MOVE '1'           TO MSG-KDMFSFOR-1                                 
110700                                                                          
110800     MOVE SPACE         TO ORAD-MID-W4I25201                              
110940     IF ORDP-RAD-IDSYSTEM (1:3) = 'LYN'                                   
110950        MOVE 'LYND'         TO OHUV-MID-IDSYSTEM                          
110960     ELSE                                                                 
110970        IF ORDP-RAD-IDSYSTEM (1:3) = 'POL'                                
110980           MOVE 'POLD'      TO OHUV-MID-IDSYSTEM                          
110990        ELSE                                                              
110991           IF ORDP-RAD-IDSYSTEM (1:3) = 'ECO'                             
110992             MOVE 'ECOD'      TO OHUV-MID-IDSYSTEM                        
110993           ELSE                                                           
110994             IF ORDP-RAD-IDSYSTEM (1:3) = 'VOU'                           
110995                MOVE 'VOUD'      TO OHUV-MID-IDSYSTEM                     
110996             ELSE                                                         
110997               IF ORDP-RAD-IDSYSTEM (1:3) = 'TAD'                         
110998                  MOVE 'TADD'    TO OHUV-MID-IDSYSTEM                     
110999               ELSE                                                       
111000                 IF ORDP-RAD-IDSYSTEM (1:3) = 'ACC'                       
111001                    MOVE 'ACCD'    TO OHUV-MID-IDSYSTEM                   
111002                 ELSE                                                     
111003                   IF ORDP-RAD-IDSYSTEM (1:3) = 'APA'                     
111004                      MOVE 'APAD'  TO OHUV-MID-IDSYSTEM                   
111005                   ELSE                                                   
111006                     IF ORDP-RAD-IDSYSTEM (1:3) = 'APB'                   
111007                        MOVE 'APBD' TO OHUV-MID-IDSYSTEM                  
111008                     ELSE                                                 
111009                       IF ORDP-RAD-IDSYSTEM (1:3) = 'APC'                 
111010                          MOVE 'APCD' TO OHUV-MID-IDSYSTEM                
111011                       ELSE                                               
111012                         IF ORDP-RAD-IDSYSTEM (1:3) = 'APD'               
111013                            MOVE 'APDD'      TO OHUV-MID-IDSYSTEM         
111014                         ELSE                                             
111015                           IF ORDP-RAD-IDSYSTEM (1:3) = 'APE'             
111016                              MOVE 'APED'    TO OHUV-MID-IDSYSTEM         
111017                           ELSE                                           
111018                             IF ORDP-RAD-IDSYSTEM (1:3) = 'APF'           
111019                               MOVE 'APFD'    TO OHUV-MID-IDSYSTEM        
111020                             ELSE                                         
111021                               IF ORDP-RAD-IDSYSTEM (1:3) = 'APG'         
111022                                  MOVE 'APGD' TO OHUV-MID-IDSYSTEM        
111023                               ELSE                                       
111024                                 IF ORDP-RAD-IDSYSTEM (1:3) =             
111025                                                            'APH'         
111026                                    MOVE 'APHD'                           
111027                                              TO OHUV-MID-IDSYSTEM        
111028                                 ELSE                                     
111029                                  IF ORDP-RAD-IDSYSTEM (1:3) =            
111030                                                            'API'         
111031                                     MOVE 'APID'                          
111032                                              TO OHUV-MID-IDSYSTEM        
111033                                  ELSE                                    
111034                                    IF ORDP-RAD-IDSYSTEM (1:3) =          
111035                                                            'APJ'         
111036                                       MOVE 'APJD'                        
111037                                              TO OHUV-MID-IDSYSTEM        
111038                                    ELSE                                  
111039                                       MOVE 'LDCD'                        
111040                                              TO OHUV-MID-IDSYSTEM        
111041                                    END-IF                                
111042                                  END-IF                                  
111043                                 END-IF                                   
111044                               END-IF                                     
111045                             END-IF                                       
111046                           END-IF                                         
111047                         END-IF                                           
111048                       END-IF                                             
111049                      END-IF                                              
111050                    END-IF                                                
111051                 END-IF                                                   
111052               END-IF                                                     
111053             END-IF                                                       
111054           END-IF                                                         
111055        END-IF                                                            
111056     END-IF                                                               
111060     MOVE WS-IDDISTR    TO ORAD-MID-IDDISTR                               
111100     MOVE WS-IDKUNDNR   TO ORAD-MID-IDKUNDNR                              
111200     MOVE WS-IDORDNR    TO ORAD-MID-IDORDNR                               
111300     MOVE SPACE         TO ORAD-MID-BEVOLREF                              
111400     MOVE 'J'           TO ORAD-MID-FLSLUT                                
111500     MOVE ORAD-AREA     TO MSG-MID-OUT                                    
111600     .                                                                    
111700     EJECT                                                                
111800 S12-SKICKA-TRANS SECTION.                                                
111900                                                                          
112000     CALL W006KOM USING MSG-PCB                                           
112100                        0693-PCB                                          
112200                        WDP8-PCB                                          
112300                        MSG-KOM-WMSGKOM                                   
112400                        MSG-IO-AREA                                       
112500     .                                                                    
112600     EJECT                                                                
112700 S11BA-SKAPA-RFSDATUM SECTION.                                            
112800                                                                          
112900     MOVE ORDP-RAD-IDDISTR              TO W-IDDISTR-WDB2                 
113000     MOVE ORDP-RAD-IDKUNDNR             TO W-IDKUNDNR-WDB2                
113100     PERFORM IMS-GU-WDB201                                                
113200                                                                          
113300     MOVE ORDP-RAD-IDDC            TO WORK-IDDC                           
113400     MOVE +002                     TO WORK-KDCALL                         
113500     MOVE +001                     TO WORK-KVWORKD                        
113600     IF ORDP-RAD-TIREPDAT     = ZERO                                      
113700        MOVE WS-DAT                TO WORK-TIAAMMDD-FOM                   
113800     ELSE                                                                 
113900        MOVE ORDP-RAD-TIREPDAT     TO WORK-TIAAMMDD-FOM                   
114000     END-IF                                                               
114100     CALL WORKDAY                  USING WORK-KDCALL                      
114200                                         WORK-DATE-AREA                   
114300                                         WORK-KDSVAR                      
114400     IF WORK-KDSVAR-FEL                                                   
114500        MOVE 'SECT S01-1, DATUM SAKNAS I WORKDAY'                         
114600                                   TO FELTEXT                             
114700        CALL ABEND                 USING RKOD-ABEND-UTAN-DUMP             
114800     ELSE                                                                 
114900       MOVE +003                   TO WORK-KDCALL                         
115000       MOVE GMT-KVDAGAR-RFS-DEF    TO WORK-KVWORKD                        
115100       PERFORM                                                            
115200       VARYING RFS-IX FROM 1 BY 1                                         
115300         UNTIL RFS-IX > MAX-RFS-IX                                        
115400         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
115500           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
115600                                   TO WORK-KVWORKD                        
115700         END-IF                                                           
115800       END-PERFORM                                                        
115900       ADD +1  TO WORK-KVWORKD                                            
116000*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
116100*      ANTAL DAGAR FÖRE RFS.                                              
116200*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
116300*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
116400*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
116500*                                                                         
116600                                                                          
116700       CALL WORKDAY                USING WORK-KDCALL                      
116800                                         WORK-DATE-AREA                   
116900                                         WORK-KDSVAR                      
117000       IF WORK-KDSVAR-FEL                                                 
117100          MOVE 'SECT S01-2, DATUM SAKNAS I WORKDAY'                       
117200                                   TO FELTEXT                             
117300          CALL ABEND               USING RKOD-ABEND-UTAN-DUMP             
117400       ELSE                                                               
117500         IF WORK-TIAAMMDD-FOM < WS-DAT                                    
117600           MOVE ORDP-RAD-IDDC      TO WORK-IDDC                           
117700           MOVE +002               TO WORK-KDCALL                         
117800           MOVE +001               TO WORK-KVWORKD                        
117900           MOVE WS-DAT             TO WORK-TIAAMMDD-FOM                   
118000           CALL WORKDAY            USING WORK-KDCALL                      
118100                                         WORK-DATE-AREA                   
118200                                         WORK-KDSVAR                      
118300           IF WORK-KDSVAR-FEL                                             
118400              MOVE 'SECT S01-3, DATUM SAKNAS I WORKDAY'                   
118500                                   TO FELTEXT                             
118600              CALL ABEND           USING RKOD-ABEND-UTAN-DUMP             
118700           ELSE                                                           
118800              MOVE WORK-TIAAMMDD-TOM TO WS-TIRFS                          
118900           END-IF                                                         
119000         ELSE                                                             
119100           MOVE WORK-TIAAMMDD-FOM  TO WS-TIRFS                            
119200         END-IF                                                           
119300       END-IF                                                             
119400     END-IF                                                               
119500     .                                                                    
119600     EJECT                                                                
119700 S20-KOMPLETTERA-PRIS   SECTION.                                          
119800                                                                          
119900     IF DIST79-DEALER-PRICE                                               
120000      IF ORDP-RAD-PRARTNTO-LOC = ZERO AND                                 
120100         ORDP-RAD-PRARTNTO-LOCPREL = ZERO                                 
120200       IF ORDP-RAD-IDPRQUES > ZERO                                        
120300*         TA BORT GAMLA PRISFRÅGAN                                        
120400         INITIALIZE PRQU-W335PRQU                                         
120500         MOVE ORDP-RAD-IDDISTR       TO PRQU-IDDISTR                      
120600         MOVE ORDP-RAD-IDKUNDNR      TO PRQU-IDKUNDNR                     
120700         MOVE ORDP-RAD-IDKUNDRF(1:5) TO PRQU-IDKUNDRF(3:5)                
120800         MOVE '00'                   TO PRQU-IDKUNDRF(1:2)                
120900         MOVE ORDP-RAD-IDPRQUES      TO PRQU-IDPRQUES                     
121000         MOVE 4                      TO PRQU-KDCALL                       
121100         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
121200                                            PRQU-WDC7-PCB                 
121300                                            PRQU-SJKO-WDK6-PCB            
121400                                                                          
121500       END-IF                                                             
121600       MOVE ZERO                     TO ORDP-RAD-IDPRQUES                 
121700                                        WS-IDPRQUES                       
121800       IF WS-IDPRQUES                = +0                                 
121900          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
122000          MOVE +1                    TO PRNO-KDCALL                       
122100                                                                          
122200          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
122300                                                                          
122400          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
122500                                        WS-IDPRQUES                       
122600          MOVE +1                    TO PRQU-KDCALL                       
122700*      ELSE                                                               
122800*         MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
122900*         MOVE +2                    TO PRNO-KDCALL                       
123000*                                                                         
123100*         CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
123200*                                                                         
123300*         MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
123400*                                     WS-IDPRQUES                         
123500*         MOVE +2                    TO PRQU-KDCALL                       
123600       END-IF                                                             
123700                                                                          
123800*         LÄGG IN NY PRISFRÅGA                                            
123900       MOVE ORDP-RAD-IDDISTR         TO PRQU-IDDISTR                      
124000       MOVE ORDP-RAD-IDKUNDNR        TO PRQU-IDKUNDNR                     
124100       MOVE ORDP-RAD-IDKUNDRF(1:5)   TO PRQU-IDKUNDRF(3:5)                
124200       MOVE '00'                     TO PRQU-IDKUNDRF(1:2)                
124300       MOVE ZERO                     TO PRQU-IDORDER                      
124400       MOVE ORDP-RAD-KDORDKL         TO PRQU-KDORDKL                      
124500       IF PRQU-IDDISTR = 0778 AND PRQU-KDORDKL < 3                        
124600         AND ORDP-RAD-DARODAT > 0                                         
124700         MOVE 4                      TO PRQU-KDORDKL                      
124800       END-IF                                                             
124900       MOVE 'N'                      TO PRQU-KDPRSTA                      
125000       MOVE ORDP-RAD-IDARTNR         TO PRQU-IDARTNR                      
125100       MOVE ORDP-RAD-KVBEART-Q       TO PRQU-KVBEART-Q                    
125200       MOVE ORDP-RAD-KDVALISO        TO PRQU-KDVALISO                     
125300       MOVE ORDP-RAD-PRARTNTO-LOC    TO PRQU-PRARTNTO-LOC                 
125400       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
125500       MOVE ORDP-RAD-IDSYSTEM        TO PRQU-IDSYSTEM                     
125600                                                                          
125700       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
125800                                          PRQU-WDC7-PCB                   
125900                                          PRQU-SJKO-WDK6-PCB              
126000                                                                          
126100       MOVE PRQU-IDPRQUES            TO  ORDP-RAD-IDPRQUES                
126200                                         WS-IDPRQUES                      
126300       MOVE PRQU-FLPRTILL            TO  ORDP-RAD-FLPRTILL                
126400                                                                          
126500       IF PRQU-KDORDKL = 4 AND PRQU-IDDISTR = 0778                        
126600         AND ORDP-RAD-KDORDKL < 3                                         
126700         MOVE 'N'                    TO ORDP-RAD-FLPRTILL                 
126800       END-IF                                                             
126900       IF ORDP-RAD-PRARTNTO-LOC = +0                                      
127000          MOVE PRQU-PRARTNTO-LOCPREL TO                                   
127100                     ORDP-RAD-PRARTNTO-LOCPREL                            
127200       END-IF                                                             
127300                                                                          
127400       IF ORDP-RAD-PRARTNTO-LOC NOT = +0                                  
127500         IF ORDP-RAD-KDPRTYP = SPACE                                      
127600           MOVE 'P'                TO ORDP-RAD-KDPRTYP                    
127700*          MOVE ORDP-RAD-TIREGDAT  TO ORDP-RAD-TIPRIS                     
127800         END-IF                                                           
127900       END-IF                                                             
128000       PERFORM S21-SKICKA-PRISFRAGA                                       
128100       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
128200       MOVE +3                      TO PRNO-KDCALL                        
128300                                                                          
128400       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
128500      END-IF                                                              
128600     END-IF                                                               
128700     .                                                                    
128800     EJECT                                                                
128900 S21-SKICKA-PRISFRAGA  SECTION.                                           
129000                                                                          
129100     MOVE 1                          TO 3039-REQU-IDMSGVER                
129200     MOVE SPACE                      TO 3039-REQU-KDPGMACT                
129300     MOVE 'W4403600'                 TO 3039-REQU-IDUSER                  
129400                                                                          
129500     MOVE SPACE                      TO 3039-MID-IDBUNDLE                 
129600     MOVE ORDP-RAD-IDDISTR           TO 3039-MID-IDDISTR                  
129700     MOVE ORDP-RAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                 
129800     MOVE ORDP-RAD-IDORDNR7          TO 3039-MID-IDBUNDLE                 
129900     MOVE WS-IDPRQUES                TO 3039-MID-IDPRQUES                 
130000                                                                          
130100     PERFORM S24-SKICKA-OPEN                                              
130200     PERFORM S24-SKICKA-MEDDELANDE                                        
130300     PERFORM S24-SKICKA-CLOSE                                             
130400                                                                          
130500     .                                                                    
130600     EJECT                                                                
130700 S24-SKICKA-OPEN SECTION.                                                 
130800                                                                          
130900     MOVE 'OPEN'                     TO SEND-KDFUNC                       
131000     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
131100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
131200                                                                          
131300     IF SEND-KDRC > 0                                                     
131400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
131500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
131600       DELIMITED BY SIZE INTO FELTEXT                                     
131700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
131800     END-IF                                                               
131900     .                                                                    
132000     SKIP3                                                                
132100 S24-SKICKA-MEDDELANDE  SECTION.                                          
132200                                                                          
132300     MOVE 'PUT'                      TO SEND-KDFUNC                       
132400     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
132500     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
132600                                                                          
132700     IF SEND-KDRC > 0                                                     
132800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
132900       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
133000       DELIMITED BY SIZE INTO FELTEXT                                     
133100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
133200     END-IF                                                               
133300     .                                                                    
133400     SKIP3                                                                
133500 S24-SKICKA-CLOSE SECTION.                                                
133600                                                                          
133700     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
133800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
133900                                                                          
134000     IF SEND-KDRC > 0                                                     
134100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
134200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
134300       DELIMITED BY SIZE INTO FELTEXT                                     
134400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
134500     END-IF                                                               
134600     .                                                                    
134700     EJECT                                                                
134800*    ---- IMS SEKTIONER ----                                              
134900                                                                          
135000 IMS-PURG-ALT-MSG-2109 SECTION.                                           
135100     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
135200     MOVE SPACE TO GODK-STATUSKODER                                       
135300     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
135400     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
135500     PERFORM IMS-STATUSKONTROLL                                           
135600     .                                                                    
135700     SKIP2                                                                
135800 IMS-GU-ARTC-WDK601 SECTION.                                              
135900                                                                          
136000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
136100            DELIMITED BY SIZE INTO SSA1                                   
136200     MOVE '  '                 TO GODK-STATUSKODER                        
136300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
136400     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
136500     PERFORM IMS-STATUSKONTROLL                                           
136600     .                                                                    
136700     SKIP2                                                                
136800 IMS-GNP-ARTC-WDK611 SECTION.                                             
136900                                                                          
137000     MOVE 'WDK611  ' TO SSA1                                              
137100     MOVE '  '                 TO GODK-STATUSKODER                        
137200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
137300     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
137400     PERFORM IMS-STATUSKONTROLL                                           
137500     .                                                                    
137600     EJECT                                                                
137700 IMS-GU-ORQI-WDQ2 SECTION.                                                
137800                                                                          
137900     STRING  'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
138000             DELIMITED BY SIZE INTO SSA1                                  
138100     MOVE    '  '              TO GODK-STATUSKODER                        
138200     CALL    CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                 
138300     MOVE    WDQ2-STATUS-CODE TO STATUS-WS                                
138400     PERFORM IMS-STATUSKONTROLL                                           
138500     .                                                                    
138600     EJECT                                                                
138700 IMS-GHU-ARTM-WDK901 SECTION.                                             
138800                                                                          
138900     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
139000          DELIMITED BY SIZE INTO SSA1                                     
139100     MOVE '  ' TO GODK-STATUSKODER                                        
139200     CALL CBLTDLI USING GHU WDK9-PCB DLI-IO-WDK901 SSA1                   
139300     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
139400     PERFORM IMS-STATUSKONTROLL                                           
139500     .                                                                    
139600     SKIP3                                                                
139700 IMS-GHNP-ARTM-WDK911 SECTION.                                            
139800                                                                          
139900     STRING 'WDK911  (DABEHOV  =' W-DABEHOV-X ')'                         
140000          DELIMITED BY SIZE INTO SSA1                                     
140100     MOVE '  GE' TO GODK-STATUSKODER                                      
140200     CALL CBLTDLI USING GHNP WDK9-PCB DLI-IO-WDK911 SSA1                  
140300     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
140400     PERFORM IMS-STATUSKONTROLL                                           
140500     .                                                                    
140600     EJECT                                                                
140700 IMS-GHU-ORDP-WDA5-OTAECKT SECTION.                                       
140800                                                                          
140900     STRING 'WDA501  (WDA501KY =' W1-WDA501KY-X                           
141000                    '&KDSTARAD =' W-KDSTARAD-X                            
141100                    '&IDDC     =' W-IDDC-X ')'                            
141200            DELIMITED BY SIZE INTO SSA1                                   
141300     MOVE '  GE'               TO GODK-STATUSKODER                        
141400     CALL CBLTDLI USING GHU WDA51-PCB DLI-IO-WDA501 SSA1                  
141500     MOVE WDA51-STATUS-CODE    TO STATUS-WS                               
141600     PERFORM IMS-STATUSKONTROLL                                           
141700     .                                                                    
141800     EJECT                                                                
141900 IMS-GHU-ORDP-WDA5 SECTION.                                               
142000                                                                          
142100     STRING 'WDA501  (WDA501KY =' W1-WDA501KY-X                           
142200                    '&IDDC     =' W-IDDC-X ')'                            
142300            DELIMITED BY SIZE INTO SSA1                                   
142400     MOVE '  GE'               TO GODK-STATUSKODER                        
142500     CALL CBLTDLI USING GHU WDA51-PCB DLI-IO-WDA501 SSA1                  
142600     MOVE WDA51-STATUS-CODE    TO STATUS-WS                               
142700     PERFORM IMS-STATUSKONTROLL                                           
142800     .                                                                    
142900     EJECT                                                                
143000 IMS-GHU-ORDP-WDA5-RES SECTION.                                           
143100                                                                          
143200     STRING 'WDA501  (WDA501KY=>' W1-WDA501KY-X                           
143300                    '&WDA501KY=<' W2-WDA501KY-X                           
143400                    '&KDSTARAD =' W-KDSTARAD-X                            
143500                    '&IDDC     =' W-IDDC-X ')'                            
143600            DELIMITED BY SIZE INTO SSA1                                   
143700     MOVE '  GE'                TO GODK-STATUSKODER                       
143800     CALL CBLTDLI USING GHU WDA52-PCB DLI-IO-WDA501-RES SSA1              
143900     MOVE WDA52-STATUS-CODE     TO STATUS-WS                              
144000     PERFORM IMS-STATUSKONTROLL                                           
144100     .                                                                    
144200     SKIP3                                                                
144300 IMS-GHN-ORDP-WDA5-RES SECTION.                                           
144400                                                                          
144500     STRING 'WDA501  (WDA501KY=>' W1-WDA501KY-X                           
144600                    '&WDA501KY=<' W2-WDA501KY-X                           
144700                    '&KDSTARAD =' W-KDSTARAD-X                            
144800                    '&IDDC     =' W-IDDC-X ')'                            
144900            DELIMITED BY SIZE INTO SSA1                                   
145000     MOVE '  GE'                TO GODK-STATUSKODER                       
145100     CALL CBLTDLI USING GHN WDA52-PCB DLI-IO-WDA501-RES SSA1              
145200     MOVE WDA52-STATUS-CODE     TO STATUS-WS                              
145300     PERFORM IMS-STATUSKONTROLL                                           
145400     .                                                                    
145500     EJECT                                                                
145600 IMS-REPL-ORDP-WDA5-RES SECTION.                                          
145700                                                                          
145800     MOVE '  '               TO GODK-STATUSKODER                          
145900     CALL CBLTDLI USING REPL  WDA52-PCB DLI-IO-WDA501-RES                 
146000     MOVE WDA52-STATUS-CODE TO STATUS-WS                                  
146100     PERFORM IMS-STATUSKONTROLL                                           
146200     .                                                                    
146300     EJECT                                                                
146400 IMS-REPL-ORDP-WDA5 SECTION.                                              
146500                                                                          
146600     MOVE '  '               TO GODK-STATUSKODER                          
146700     CALL CBLTDLI USING REPL  WDA51-PCB DLI-IO-WDA501                     
146800     MOVE WDA51-STATUS-CODE TO STATUS-WS                                  
146900     PERFORM IMS-STATUSKONTROLL                                           
147000     .                                                                    
147100     SKIP3                                                                
147200 IMS-DLET-ORDP-WDA5 SECTION.                                              
147300                                                                          
147400     MOVE '  '               TO GODK-STATUSKODER                          
147500     CALL CBLTDLI USING DLET WDA51-PCB DLI-IO-WDA501                      
147600     MOVE WDA51-STATUS-CODE  TO STATUS-WS                                 
147700     PERFORM IMS-STATUSKONTROLL                                           
147800     .                                                                    
147900     EJECT                                                                
148000 IMS-REPL-ARTM-WDK901 SECTION.                                            
148100                                                                          
148200     MOVE '  ' TO GODK-STATUSKODER                                        
148300     CALL CBLTDLI USING REPL WDK9-PCB DLI-IO-WDK901                       
148400     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
148500     PERFORM IMS-STATUSKONTROLL                                           
148600     .                                                                    
148700     EJECT                                                                
148800 IMS-REPL-ARTM-WDK911 SECTION.                                            
148900                                                                          
149000     MOVE '  ' TO GODK-STATUSKODER                                        
149100     CALL CBLTDLI USING REPL WDK9-PCB DLI-IO-WDK911                       
149200     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
149300     PERFORM IMS-STATUSKONTROLL                                           
149400     .                                                                    
149500     EJECT                                                                
149600 IMS-DLET-ARTM-WDK9 SECTION.                                              
149700                                                                          
149800     MOVE '  ' TO GODK-STATUSKODER                                        
149900     CALL CBLTDLI USING DLET WDK9-PCB DLI-IO-WDK911                       
150000     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
150100     PERFORM IMS-STATUSKONTROLL                                           
150200     .                                                                    
150300     EJECT                                                                
150400 IMS-ISRT-ZZAC-WDG6 SECTION.                                              
150500                                                                          
150600     STRING 'WLZZAC01 '                                                   
150700          DELIMITED BY SIZE INTO SSA1                                     
150800     MOVE '  II' TO GODK-STATUSKODER                                      
150900     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-ZZAC01 SSA1                  
151000     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
151100     PERFORM IMS-STATUSKONTROLL                                           
151200     .                                                                    
151300     EJECT                                                                
151400 IMS-GU-WDB201 SECTION.                                                   
151500                                                                          
151600     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
151700            DELIMITED BY SIZE INTO SSA1                                   
151800                                                                          
151900     MOVE '    ' TO GODK-STATUSKODER                                      
152000     CALL CBLTDLI USING                                                   
152100           GU WDB2-PCB DLI-IO-WDB2 SSA1                                   
152200     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
152300     PERFORM IMS-STATUSKONTROLL                                           
152400     .                                                                    
152500*    ---- IMS CHECKPOINTHANTERING ----                                    
152600                                                                          
152700 IMS-RESTART  SECTION.                                                    
152800                                                                          
152900     MOVE SPACE TO MSG-IO-AREA-1                                          
153000     MOVE '  '  TO GODK-STATUSKODER                                       
153100     CALL CBLTDLI USING XRST MSG-PCB                                      
153200                          MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1              
153300                          CHKP-AREA-1-LENGTH CHKP-AREA-1                  
153400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
153500     PERFORM IMS-STATUSKONTROLL                                           
153600                                                                          
153700     IF IMS-EJ-OK                                                         
153800       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
153900       CALL FELLOG                                                        
154000     END-IF                                                               
154100     .                                                                    
154200     SKIP2                                                                
154300 IMS-CHECKPOINT SECTION.                                                  
154400                                                                          
154500     MOVE PGM-NAMN TO MSG-IO-AREA-1                                       
154600     MOVE '  XD'       TO GODK-STATUSKODER                                
154700     CALL CBLTDLI USING CHKP MSG-PCB                                      
154800                          MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1              
154900                          CHKP-AREA-1-LENGTH CHKP-AREA-1                  
155000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
155100     PERFORM IMS-STATUSKONTROLL                                           
155200     IF IMS-EJ-OK                                                         
155300       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
155400       CALL FELLOG                                                        
155500     END-IF                                                               
155600     .                                                                    
155700     SKIP2                                                                
155800 IMS-STATUSKONTROLL SECTION.                                              
155900                                                                          
156000     SET STATUS-IX TO 1                                                   
156100     SEARCH GODK-STATUS                                                   
156200       AT END                                                             
156300       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
156400       DELIMITED BY SIZE INTO FELTEXT                                     
156500       CALL FELLOG                                                        
156600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
156700         CONTINUE                                                         
156800     END-SEARCH                                                           
156900     .                                                                    
157000     EJECT                                                                
157100*    -COPY WY2000P1                                                       
