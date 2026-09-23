000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9033200.                                                
000300 AUTHOR.         GERRY CARMICHAEL.                                        
000400 DATE-WRITTEN.   JANUARI 98.                                              
000500*                                                                         
000600*REMARKS.                                                                 
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        PROGRAMMET HANTERAR BILDEN;                                      
001000*        FRÅGA PÅ ORDER VIA VDI-SYSTEMET                                  
001100*        PROGRAMMET SKALL VISA ALLA ORDERRADER                            
001200*        FÖR ETT VISST IDDISTR, IDKUNDNR OCH IDORDNR7.                    
001300*        PROGRAMMET HANTERAR OLIKA VERSIONER I VDI                        
001400*        ENLIGT IDVTYP.                                                   
001500*                                                                         
001600*        PROGRAMMET ANROPAR W218ETA FÖR HÄMTNING                          
001700*        AV ETA-DATUM/NDC-LAGER.                                          
001800*       (ESTIMATED TIME AVAILABLE)                                        
001900*                                                                         
002000*        LÄSER   WDQ2                                                     
002100*                WDQ3                                                     
002200*                WDQ4                                                     
002300*                WDE4                                                     
002400*                WDA5                                                     
002500*                WDE421                                                   
002600*                WDF106                                                   
002700*                                                                         
002800*        OM SIDAN INTE FULL OCH DET INTE FINNS FLER SEGMENT               
002900*        PÅ WDQ4 LÄSER MAN WDE4. OM SIDAN FORTFARANDE INTE                
003000*        ÄR FULL OCH DET INTE FINNS FLER SEGMENT PÅ WDE4                  
003100*        LÄSER MAN WDA5.                                                  
003200*                                                                         
003300*        BLIR SIDAN FULL OCH DET FINNS FLER SEGMENT PÅ                    
003400*        NÅGON AV BASERNA SPARAS DET EN FLAGGA OCH ETT RAD                
003500*        NUMMER SOM TALAR OM PÅ VILKEN BAS OCH VAR I BASEN                
003600*        MAN SKALL FORTSÄTTA ATT LÄSA VID EN EVENTUELL                    
003700*        BLÄDDRING.                                                       
003800*                                                                         
003900*    INDATA.                                                              
004000*        TRANSAKTION: W9T332                                              
004100*        MID:         W9I33201                                            
004200*                                                                         
004300*    UTDATA.                                                              
004400*        MOD:         W9O33201                                            
004500*                     W9O33202                                            
004600     SKIP3                                                                
004700 ENVIRONMENT DIVISION.                                                    
004800     SKIP3                                                                
004900 DATA DIVISION.                                                           
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400     SKIP3                                                                
005500 77  IDPGM                       PIC X(8)    VALUE 'W9033200'.            
005600                                                                          
005700 77    JA                        PIC X       VALUE 'J'.                   
005800 77    NEJ                       PIC X       VALUE 'N'.                   
005900                                                                          
006000 77   WS-IDPRODNR                PIC 9(7).                                
006100 77   WS-IDPLKLST                PIC 9(3).                                
006200 77   WS-IDARTNR-NUM             PIC 9(8).                                
006300 77   WS-IDRADNR-NUM             PIC 9(4).                                
006400 77   WS-IDORDNR-NUM             PIC 9(5).                                
006500 77   WS-IDPURAD-NUM             PIC 9(4).                                
006600 77   WS-KVDAGAR-DIFF            PIC 9(3).                                
006700 77   WS-IDKUNDRF-CONT           PIC X(10)   VALUE SPACE.                 
006800 77   W-KDORDSTA                 PIC X(2)    VALUE SPACE.                 
006900 77   W-KVPREAVB                 PIC S9(7)               COMP-3.          
007000 77   MAX-RAD                    PIC S9(7)   VALUE +11   COMP-3.          
007100 77   MAX-MOD-LANGD              PIC S9(7)   VALUE +0    COMP-3.          
007200 77   W-IDORDNR5-NUM             PIC S9(5)   VALUE ZERO  COMP-3.          
007300 77   W-FORRA-IDORDER            PIC S9(7)   VALUE ZERO  COMP-3.          
007400                                                                          
007500 77   W-TIPACKN                  PIC S9(6).                               
007600 77   W-KDTPOTYP                 PIC  9(1).                               
007700 77   W-SPAR-IDPURAD             PIC  9(5).                               
007800     EJECT                                                                
007900                                                                          
008000 01  MESSAGE-CODES.                                                       
008100     03  ERR-ORDER-NOT-FOUND     PIC X(3)    VALUE 'B10'.                 
008200     03  ERR-PART-MISSING        PIC X(3)    VALUE 'B13'.                 
008300     03  ERR-WRONG-KEY           PIC X(3)    VALUE 'B01'.                 
008400                                                                          
008500*      --- VALID IDDC CODES                                               
008600*                                                                         
008700*01    -COPY WWDC99                                                       
008800*01    -COPY WWDCKONS                                                     
008900       EJECT                                                              
009000*  --- FÖR ATT FÅ RÄTT SEKEL VID ANROP TILL W218ETA                       
009100 01  WS-ETA-DATUM                PIC 9(6).                                
009200 01  FILLER REDEFINES WS-ETA-DATUM.                                       
009300     03  WS-ETA-DATUM-AAR        PIC 9(2).                                
009400     03  WS-ETA-DATUM-MAANAD     PIC 9(2).                                
009500     03  WS-ETA-DATUM-DAG        PIC 9(2).                                
009600     EJECT                                                                
009700 01   W-IDRADNR.                                                          
009800   05 W-IDRADNR-1                PIC  9(1).                               
009900   05 W-IDRADNR-2-4              PIC  9(3).                               
010000                                                                          
010100 01  IDKUNDRF-WS                 PIC X(10).                               
010200 01  IDKUNDRF5-WS  REDEFINES  IDKUNDRF-WS.                                
010300     03 IDORDNR5-WS              PIC 9(5).                                
010400     03 FILLER                   PIC X(5).                                
010500 01  IDKUNDRF7-WS  REDEFINES  IDKUNDRF-WS.                                
010600     03 IDORDNR7-WS              PIC 9(7).                                
010700     03 FILLER                   PIC X(3).                                
010800 01  FILLER        REDEFINES  IDKUNDRF-WS.                                
010900     03 FILLER                   PIC X(5).                                
011000     03 IDKUNDRF-WS-POS6-7       PIC X(2).                                
011100     03 FILLER                   PIC X(3).                                
011200                                                                          
011300 01  TEST-IDDISTR             PIC S9(5) COMP-3.                           
011400     EJECT                                                                
011500* ----- GENERELLA SUBPROGRAM                                              
011600 01  DYNAMISKA-SUBPGM.                                                    
011700    03  CBLTDLI                  PIC  X(8)   VALUE 'CBLTDLI '.            
011800    03  FELLOG                   PIC  X(8)   VALUE 'FELLOG  '.            
011900                                                                          
012000 01  DYNAMISKA-SUBPROGRAM.                                                
012100   03  W218ETA                   PIC  X(8)   VALUE 'W218ETA '.            
012200                                                                          
012300 01 FILLER                       PIC  X(8)   VALUE 'LETA'.                
012400*   -COPY W218LETA -PRE ETA-.                                             
012500     EJECT                                                                
012600* ----- INDEXFÄLT                                                         
012700 77  IX-RAD                      PIC S9(9)   VALUE +0  COMP SYNC.         
012800                                                                          
012900* ----- SWITCHAR                                                          
013000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013100     88  NYCKLAR-OK                          VALUE 'J'.                   
013200                                                                          
013300 77  FORSTA-WDE4-SW              PIC X       VALUE 'J'.                   
013400     88  FORSTA-WDE4                         VALUE 'J'.                   
013500                                                                          
013600 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
013700     88  FIRST-TIME                          VALUE 'J'.                   
013800                                                                          
013900 77  SPAR-NYCKLAR-SW             PIC X       VALUE 'N'.                   
014000     88  SPAR-NYCKLAR-OK                     VALUE 'J'.                   
014100                                                                          
014200 01  WS-IDARTNR.                                                          
014300     05  WS-IDARTNR-8            PIC X(8).                                
014400                                                                          
014500     EJECT                                                                
014600 01    NYCKLAR-TILL-DLI.                                                  
014700     SKIP2                                                                
014800   03    W-WDQ2CSEQ-X.                                                    
014900     05    W-Q2CSEQ-IDGMTREF.                                             
015000       07  W-Q2CSEQ-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.              
015100       07  W-Q2CSEQ-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.              
015200       07  W-Q2CSEQ-IDKUNDRF.                                             
015300        09 W-Q2CSEQ-IDORDNR7 PIC X(7)    VALUE ZERO.                      
015400        09 FILLER            PIC X(3)    VALUE SPACE.                     
015500     SKIP2                                                                
015600   03    W-WDQ211KY-X.                                                    
015700     05    W-Q211KY-IDDC     PIC X(2)    VALUE SPACE.                     
015800     05    W-Q211KY-IDLEVNR  PIC X(5)    VALUE SPACE.                     
015900     SKIP2                                                                
016000   03    W-WDQ301KY-MIN-X.                                                
016100     05    W-Q301KY-MIN-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
016200     05    W-Q301KY-MIN-IDDC     PIC  X(2)   VALUE ZERO.                  
016300     05    W-Q301KY-MIN-IDPRODNR PIC S9(7)   VALUE ZERO  COMP-3.          
016400     05    W-Q301KY-MIN-IDPLKLST PIC S9(3)   VALUE ZERO  COMP-3.          
016500     SKIP2                                                                
016600   03    W-WDQ301KY-MAX-X.                                                
016700     05    W-Q301KY-MAX-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
016800     05    W-Q301KY-MAX-IDDC     PIC  X(2)   VALUE ZERO.                  
016900     05    W-Q301KY-MAX-IDPRODNR PIC S9(7)   VALUE ZERO  COMP-3.          
017000     05    W-Q301KY-MAX-IDPLKLST PIC S9(3)   VALUE ZERO  COMP-3.          
017100     SKIP2                                                                
017200   03    W-WDQ4ASEQ-X.                                                    
017300     05    W-Q4ASEQ-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
017400     05    W-Q4ASEQ-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
017500     05    W-Q4ASEQ-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
017600     SKIP2                                                                
017700   03    W-WDQ4ASEQ-MIN-X.                                                
017800     05    W-Q4ASEQ-MIN-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
017900     05    W-Q4ASEQ-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
018000     05    W-Q4ASEQ-MIN-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
018100     SKIP2                                                                
018200   03    W-WDQ4ASEQ-MAX-X.                                                
018300     05    W-Q4ASEQ-MAX-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
018400     05    W-Q4ASEQ-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
018500     05    W-Q4ASEQ-MAX-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
018600     SKIP2                                                                
018700   03    W-WDE4ASEQ-MIN-X.                                                
018800     05    W-IDDISTR-MIN         PIC S9(5)   VALUE ZERO COMP-3.           
018900     05    W-IDKUNDNR-MIN        PIC S9(7)   VALUE ZERO COMP-3.           
019000     05    W-IDKUNDRF-MIN        PIC X(10).                               
019100                                                                          
019200   03    W-WDE4ASEQ-MAX-X.                                                
019300     05    W-IDDISTR-MAX         PIC S9(5)   VALUE ZERO COMP-3.           
019400     05    W-IDKUNDNR-MAX        PIC S9(7)   VALUE ZERO COMP-3.           
019500     05    W-IDKUNDRF-MAX        PIC X(10).                               
019600                                                                          
019700   03    W-WDE421KY-X.                                                    
019800     05    W-IDPRODNR-E421       PIC S9(7)   VALUE ZERO COMP-3.           
019900     05    W-IDKOLLI-E421        PIC S9(5)   VALUE ZERO COMP-3.           
020000                                                                          
020100                                                                          
020200   03    W-WDA501KY-X.                                                    
020300     05    W-A501KY-IDGMTREF.                                             
020400       07  W-A501KY-IDDISTR  PIC S9(5)       VALUE ZERO  COMP-3.          
020500       07  W-A501KY-IDKUNDNR PIC S9(7)       VALUE ZERO  COMP-3.          
020600       07  W-A501KY-IDKUNDRF.                                             
020700        09 W-A501KY-IDORDNR5 PIC X(05)       VALUE SPACE.                 
020800        09 FILLER            PIC X(05)       VALUE SPACE.                 
020900     05    W-A501KY-IDARTNR  PIC S9(9)       VALUE ZERO  COMP-3.          
021000     05    W-A501KY-IDLOPNR  PIC S9(3)       VALUE ZERO  COMP-3.          
021100     SKIP2                                                                
021200   03    W-WDA501KY-MIN-X.                                                
021300     05    W-A501KY-MIN-IDGMTREF.                                         
021400       07  W-A501KY-MIN-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
021500       07  W-A501KY-MIN-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
021600       07  W-A501KY-MIN-IDKUNDRF.                                         
021700        09 W-A501KY-MIN-IDORDNR5 PIC X(05)   VALUE SPACE.                 
021800        09 FILLER                PIC X(05)   VALUE SPACE.                 
021900     05    W-A501KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
022000     05    W-A501KY-MIN-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
022100     SKIP2                                                                
022200   03    W-WDA501KY-MAX-X.                                                
022300     05    W-A501KY-MAX-IDGMTREF.                                         
022400       07  W-A501KY-MAX-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
022500       07  W-A501KY-MAX-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
022600       07  W-A501KY-MAX-IDKUNDRF.                                         
022700        09 W-A501KY-MAX-IDORDNR5 PIC X(05)   VALUE SPACE.                 
022800        09 FILLER                PIC X(05)   VALUE SPACE.                 
022900     05    W-A501KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
023000     05    W-A501KY-MAX-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
023100     SKIP2                                                                
023200   03    W-IDDC-X.                                                        
023300     05    W-IDDC                PIC  X(2)   VALUE ZERO.                  
023400     SKIP2                                                                
023500   03    W-IDARTNR-X.                                                     
023600     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
023700     SKIP2                                                                
023800   03    W-IDARTNR-WDA5-X.                                                
023900     05    W-IDARTNR-WDA5        PIC S9(9)   VALUE ZERO  COMP-3.          
024000     SKIP2                                                                
024100   03    W-IDLOPNR-X.                                                     
024200     05    W-IDLOPNR             PIC S9(3)   VALUE ZERO  COMP-3.          
024300     SKIP2                                                                
024400   03    W-IDKUNDRF-X.                                                    
024500     05    W-IDKUNDRF            PIC X(10).                               
024600                                                                          
024700     05    W-IDORDNR5-FILLER     REDEFINES W-IDKUNDRF.                    
024800       10  W-IDORDNR5            PIC X(5).                                
024900       10  FILLER                PIC X(5).                                
025000                                                                          
025100     05    W-IDORDNR7-FILLER     REDEFINES W-IDKUNDRF.                    
025200       10  W-IDORDNR7            PIC X(7).                                
025300       10  FILLER                PIC X(3).                                
025400     SKIP2                                                                
025500   03    W-IDPRODNR-X.                                                    
025600     05    W-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.          
025700     SKIP2                                                                
025800   03    W-IDKOLLI-X.                                                     
025900     05    W-IDKOLLI             PIC S9(5)   VALUE ZERO COMP-3.           
026000     SKIP2                                                                
026100   03    W-IDRADNRO-MIN-X.                                                
026200     05    W-IDRADNRO-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
026300     SKIP2                                                                
026400   03    W-IDPLKLST-X.                                                    
026500     05    W-IDPLKLST            PIC S9(3)   VALUE ZERO  COMP-3.          
026600     SKIP2                                                                
026700   03    W-IDPURAD-X.                                                     
026800     05    W-IDPURAD             PIC S9(5)   VALUE ZERO  COMP-3.          
026900     SKIP2                                                                
027000   03  W-IDLEVNR-X.                                                       
027100     05  W-IDLEVNR               PIC X(5)   VALUE SPACE.                  
027200     EJECT                                                                
027300******************************************************************        
027400*                                                                         
027500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
027600*                                                                         
027700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
027800     SKIP3                                                                
027900*01    MID -COPY W9I33201.                                                
028000     EJECT                                                                
028100*01    -COPY WMSGAREA                                                     
028200     EJECT                                                                
028300*  03    MOD -COPY W9O33201  -RED MSG-AREA.                               
028400     EJECT                                                                
028500*  03    MOD -COPY W9O33202  -RED MSG-AREA.                               
028600     EJECT                                                                
028700*01    -COPY WMFSAREA                                                     
028800     EJECT                                                                
028900******************************************************************        
029000*                                                                         
029100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
029200*                                                                         
029300 01    IMS-WS.                                                            
029400   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
029500     SKIP3                                                                
029600*                        **** STATUS-KOD FRÅN IMS                         
029700   03    STATUS-WS               PIC XX.                                  
029800     88    SEGMENT-FINNS                     VALUE '  '.                  
029900     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
030000     88    SEGMENT-SLUT                      VALUE 'GB'.                  
030100     SKIP3                                                                
030200   03    GODK-STATUSKODER.                                                
030300     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
030400     SKIP3                                                                
030500 01    SSA1                      PIC X(200).                              
030600 01    SSA2                      PIC X(64).                               
030700     EJECT                                                                
030800*                            IMS FUNKTIONSKODER                           
030900*01    -COPY W0003                                                        
031000     EJECT                                                                
031100******************************************************************        
031200*                                                                         
031300*        ARBETS-AREOR TILL IO-AREORNA                                     
031400*                                                                         
031500*    ---  DLI INPUT-OUTPUT AREA 1                                         
031600*    ---  DLI-IO-AREA                                                     
031700*                                                                         
031800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI01'.            
031900 01  IO-AREA-ORQI01.                                                      
032000*  03    WLORQI01 -COPY WDQ201                                            
032100     EJECT                                                                
032200                                                                          
032300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI11'.            
032400 01  IO-AREA-ORQI11.                                                      
032500*  03    WLORQI11 -COPY WDQ211                                            
032600     EJECT                                                                
032700                                                                          
032800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI12'.            
032900 01  IO-AREA-ORQI12.                                                      
033000*  03    WLORQI12 -COPY WDQ212                                            
033100     EJECT                                                                
033200                                                                          
033300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQA01'.            
033400 01  IO-AREA-ORQA01.                                                      
033500*  03    WLORQA01 -COPY WDQ301                                            
033600     EJECT                                                                
033700                                                                          
033800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQF01'.            
033900 01  IO-AREA-ORQF01.                                                      
034000*  03    WLORQF01 -COPY WDQ401 -PRE Q4-                                   
034100     EJECT                                                                
034200                                                                          
034300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC11'.            
034400 01  IO-AREA-ARTC11.                                                      
034500*  03    WLARTC11  -COPY WDK611                                           
034600     EJECT                                                                
034700                                                                          
034800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE601'.            
034900 01  IO-AREA-WDE601.                                                      
035000*  03  -COPY WDE601                                                       
035100     EJECT                                                                
035200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE611'.            
035300 01  IO-AREA-WDE611.                                                      
035400*  03  -COPY WDE611                                                       
035500     EJECT                                                                
035600                                                                          
035700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-LEVA14'.            
035800 01  IO-AREA-LEVA14.                                                      
035900*  03    WLLEVA14  -COPY WDF106                                           
036000     EJECT                                                                
036100                                                                          
036200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORDP11'.            
036300 01  IO-AREA-ORDP01.                                                      
036400*  03    WLORDP01  -COPY WDA501                                           
036500     EJECT                                                                
036600                                                                          
036700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE401'.            
036800 01  IO-AREA-WDE401.                                                      
036900*  03    -COPY WDE401                                                     
037000     EJECT                                                                
037100 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE411'.            
037200 01  IO-AREA-WDE411.                                                      
037300*  03    -COPY WDE411                                                     
037400     EJECT                                                                
037500 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE421'.            
037600 01 IO-AREA-WDE421.                                                       
037700*  03  -COPY WDE421                                                       
037800     EJECT                                                                
037900 01  FILLER                 PIC X(12) VALUE 'DUMMY-ARTC'.                 
038000 01  ETA-ARTC-PCB           PIC X.                                        
038100 01  FILLER                 PIC X(12) VALUE 'DUMMY-LEVA'.                 
038200 01  ETA-LEVA-PCB           PIC X.                                        
038300 LINKAGE SECTION.                                                         
038400                                                                          
038500*01    -COPY W0009     -PRE MSG-                                          
038600     EJECT                                                                
038700*01    -COPY W0008     -PRE ORQI-                                         
038800     05  FILLER                  PIC X.                                   
038900     EJECT                                                                
039000*01    -COPY W0008     -PRE ORQA-                                         
039100     05  FILLER                  PIC X.                                   
039200     EJECT                                                                
039300*01    -COPY W0008     -PRE ORQF-                                         
039400     05  FILLER                  PIC X.                                   
039500     EJECT                                                                
039600*01    -COPY W0008     -PRE ORDP-                                         
039700     05  FILLER                  PIC X.                                   
039800     EJECT                                                                
039900*01    -COPY W0008     -PRE WDE6-                                         
040000     05  FILLER                  PIC X.                                   
040100     EJECT                                                                
040200*01    -COPY W0008     -PRE ARTC-                                         
040300     05  FILLER                  PIC X.                                   
040400     EJECT                                                                
040500*01    -COPY W0008     -PRE WDE4-                                         
040600     05  FILLER                  PIC X.                                   
040700     EJECT                                                                
040800*01    -COPY W0008     -PRE LEVA-                                         
040900     05  FILLER                  PIC X.                                   
041000     EJECT                                                                
041100 01  ETA-WDK7-PCB           PIC X.                                        
041200 01  ETA-INLC-PCB           PIC X.                                        
041300 01  ETA-WDB6-PCB           PIC X.                                        
041400 01  ETA-WDD9-PCB           PIC X.                                        
041500     EJECT                                                                
041600 PROCEDURE DIVISION  USING MSG-PCB                                        
041700                           ORQI-PCB     ORQA-PCB                          
041800                           ORQF-PCB     ORDP-PCB                          
041900                           WDE6-PCB     ARTC-PCB      WDE4-PCB            
042000                           LEVA-PCB                                       
042100                           ETA-WDK7-PCB ETA-INLC-PCB                      
042200                           ETA-WDB6-PCB ETA-WDD9-PCB.                     
042300                                                                          
042400     ENTRY 'DLITCBL' USING MSG-PCB                                        
042500                           ORQI-PCB     ORQA-PCB                          
042600                           ORQF-PCB     ORDP-PCB                          
042700                           WDE6-PCB     ARTC-PCB      WDE4-PCB            
042800                           LEVA-PCB                                       
042900                           ETA-WDK7-PCB ETA-INLC-PCB                      
043000                           ETA-WDB6-PCB ETA-WDD9-PCB.                     
043100                                                                          
043200     PERFORM IMS-GET-MSG                                                  
043300     IF SEGMENT-FINNS                                                     
043400       PERFORM A-INIT                                                     
043500       PERFORM B-KONTROLLERA-NYCKLAR                                      
043600       IF NYCKLAR-OK                                                      
043700         PERFORM C-BEHANDLA-RADER                                         
043800       END-IF                                                             
043900     END-IF                                                               
044000     PERFORM E-KONTROLLERA-OM-SIDA-TOM                                    
044100     PERFORM F-BERAKNA-MAX-MOD-LANGD                                      
044200     MOVE    MAX-MOD-LANGD TO MSG-KVLL                                    
044300     PERFORM IMS-INSERT-MSG                                               
044400     MOVE    ZERO          TO RETURN-CODE                                 
044500     GOBACK                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 A-INIT SECTION.                                                          
044900                                                                          
045000     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I33201                    
045100     MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                     
045200     MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                    
045300     MOVE ' '                          TO MFS-KDTRTYP                     
045400     MOVE LOW-VALUE                    TO MSG-AREA                        
045500                                                                          
045600     MOVE MID-IDRADNR-NEXT             TO W-IDRADNR                       
045700     MOVE +1                           TO IX-RAD                          
045800                                                                          
045900     IF MID-IDVTYP NOT = '2'                                              
046000       MOVE '9332'                     TO MOD-IDTRANS                     
046100       MOVE 'W9O33201'                 TO MFS-IDMOD                       
046200       MOVE ZERO                       TO MOD-IDMFSFEL                    
046300       MOVE MID-IDORDNR7               TO MOD-IDORDNR7                    
046400                                          WS-IDKUNDRF-CONT                
046500       MOVE ZERO                       TO MOD-IDARTNR                     
046600                                          MOD-IDRADNR-NEXT                
046700                                          MOD-IDPRODNR-NEXT               
046800                                          MOD-IDPLKLST-NEXT               
046900                                          MOD-IDKOLLI-NEXT                
047000       MOVE SPACE                      TO MOD-FLSVAR-NEXT                 
047100                                          MOD-IDDC-NEXT                   
047200       PERFORM UNTIL IX-RAD    >   MAX-RAD                                
047300         MOVE ZERO             TO  MOD-IDARTNR-RAD(IX-RAD)                
047400                                   MOD-KVBEART(IX-RAD)                    
047500                                   MOD-KVAVBART(IX-RAD)                   
047600                                   MOD-KVLEVART(IX-RAD)                   
047700                                   MOD-TIRODAT(IX-RAD)                    
047800                                   MOD-IDKOLLI (IX-RAD)                   
047900                                   MOD-IDORDNR7-RO (IX-RAD)               
048000                                                                          
048100         MOVE SPACE            TO  MOD-KDORDSTA(IX-RAD)                   
048200                                   MOD-KDTPOTYP(IX-RAD)                   
048300                                   MOD-IDBIL(IX-RAD)                      
048400                                   MOD-IDDC(IX-RAD)                       
048500         ADD +1                TO  IX-RAD                                 
048600       END-PERFORM                                                        
048700     ELSE                                                                 
048800       MOVE '9332'                     TO MOD2-IDTRANS                    
048900       MOVE 'W9O33202'                 TO MFS-IDMOD                       
049000       MOVE ZERO                       TO MOD2-IDMFSFEL                   
049100       MOVE MID-IDORDNR7               TO MOD2-IDORDNR7                   
049200                                          WS-IDKUNDRF-CONT                
049300       MOVE ZERO                       TO MOD2-IDARTNR                    
049400                                          MOD2-IDRADNR-NEXT               
049500                                          MOD2-IDPRODNR-NEXT              
049600                                          MOD2-IDPLKLST-NEXT              
049700                                          MOD2-IDKOLLI-NEXT               
049800       MOVE SPACE                      TO MOD2-FLSVAR-NEXT                
049900                                          MOD2-IDDC-NEXT                  
050000       PERFORM UNTIL IX-RAD    >   MAX-RAD                                
050100         MOVE ZERO             TO  MOD2-IDARTNR-RAD(IX-RAD)               
050200                                   MOD2-KVBEART(IX-RAD)                   
050300                                   MOD2-KVAVBART(IX-RAD)                  
050400                                   MOD2-KVLEVART(IX-RAD)                  
050500                                   MOD2-TIRODAT(IX-RAD)                   
050600                                   MOD2-IDKOLLI (IX-RAD)                  
050700                                   MOD2-IDORDNR7-RO (IX-RAD)              
050800                                                                          
050900         MOVE SPACE            TO  MOD2-KDORDSTA(IX-RAD)                  
051000                                   MOD2-KDTPOTYP(IX-RAD)                  
051100                                   MOD2-IDRADINF(IX-RAD)                  
051200                                   MOD2-IDDC(IX-RAD)                      
051300         ADD +1                TO  IX-RAD                                 
051400       END-PERFORM                                                        
051500     END-IF                                                               
051600     .                                                                    
051700     EJECT                                                                
051800 B-KONTROLLERA-NYCKLAR SECTION.                                           
051900                                                                          
052000     IF MID-IDDISTR  NUMERIC AND                                          
052100        MID-IDKUNDNR NUMERIC                                              
052200        MOVE MID-IDDISTR     TO TEST-IDDISTR                              
052300     ELSE                                                                 
052400       MOVE NEJ              TO NYCKLAR-SW                                
052500       MOVE ERR-WRONG-KEY    TO MOD-IDMFSFEL                              
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 C-BEHANDLA-RADER SECTION.                                                
053000                                                                          
053100     MOVE +1                   TO IX-RAD                                  
053200                                                                          
053300     EVALUATE TRUE                                                        
053400     WHEN MID-FLSVAR-NEXT      =  'O' OR 'P' OR SPACE                     
053500         PERFORM CA-BEHANDLA-ORDERKOE                                     
053600         IF IX-RAD             >  MAX-RAD AND SEGMENT-FINNS               
053700             CONTINUE                                                     
053800          ELSE                                                            
053900             PERFORM CB-BEHANDLA-RESTORDER                                
054000         END-IF                                                           
054100                                                                          
054200                                                                          
054300     WHEN MID-FLSVAR-NEXT      =  'R'                                     
054400         PERFORM CB-BEHANDLA-RESTORDER                                    
054500                                                                          
054600     END-EVALUATE                                                         
054700     .                                                                    
054800     EJECT                                                                
054900 CA-BEHANDLA-ORDERKOE SECTION.                                            
055000                                                                          
055100     MOVE MID-IDDISTR          TO  W-Q2CSEQ-IDDISTR                       
055200     MOVE MID-IDKUNDNR         TO  W-Q2CSEQ-IDKUNDNR                      
055300     MOVE MID-IDORDNR7         TO  W-Q2CSEQ-IDKUNDRF                      
055400                                                                          
055500     PERFORM IMS-GET-WLORQI01-KVAL                                        
055600                                                                          
055700     IF SEGMENT-FINNS AND OHUV-FLBORT = NEJ                               
055800                                                                          
055900        IF MID-FLSVAR-NEXT  =  'P' OR SPACE                               
056000            PERFORM CAA-BEHANDLA-ORDERDEL                                 
056100                                                                          
056200            IF IX-RAD  >  MAX-RAD AND SEGMENT-FINNS                       
056300                CONTINUE                                                  
056400            ELSE                                                          
056500                PERFORM CAB-BEHANDLA-ORDERRADKOE                          
056600            END-IF                                                        
056700        ELSE                                                              
056800            PERFORM CAB-BEHANDLA-ORDERRADKOE                              
056900        END-IF                                                            
057000     END-IF                                                               
057100     .                                                                    
057200     EJECT                                                                
057300 CAA-BEHANDLA-ORDERDEL SECTION.                                           
057400                                                                          
057500     MOVE LOW-VALUE            TO  W-WDQ301KY-MIN-X                       
057600     MOVE OHUV-IDORDER         TO  W-Q301KY-MIN-IDORDER                   
057700     IF MID-IDPRODNR-NEXT      >   ZERO                                   
057800       MOVE MID-IDPRODNR-NEXT  TO  W-Q301KY-MIN-IDPRODNR                  
057900       MOVE MID-IDPLKLST-NEXT  TO  W-Q301KY-MIN-IDPLKLST                  
058000       MOVE MID-IDDC-NEXT      TO  W-Q301KY-MIN-IDDC                      
058100     END-IF                                                               
058200                                                                          
058300     MOVE HIGH-VALUE           TO  W-WDQ301KY-MAX-X                       
058400     MOVE OHUV-IDORDER         TO  W-Q301KY-MAX-IDORDER                   
058500                                                                          
058600     PERFORM IMS-GET-WLORQA01-OKVAL-GU                                    
058700     PERFORM UNTIL SEGMENT-SAKNAS             OR                          
058800                   SEGMENT-SLUT               OR                          
058900                   IX-RAD      > MAX-RAD                                  
059000         IF ODEL-KDODELSTA NOT = 'R'                                      
059100           PERFORM CAAA-BEHANDLA-WDE411                                   
059200         END-IF                                                           
059300         IF IX-RAD             > MAX-RAD                                  
059400             CONTINUE                                                     
059500          ELSE                                                            
059600             PERFORM IMS-GET-WLORQA01-OKVAL-GN                            
059700         END-IF                                                           
059800     END-PERFORM                                                          
059900     .                                                                    
060000     EJECT                                                                
060100                                                                          
060200 CAAA-BEHANDLA-WDE411 SECTION.                                            
060300                                                                          
060400     MOVE LOW-VALUE                      TO W-WDE4ASEQ-MIN-X              
060500     MOVE MID-IDDISTR                    TO W-IDDISTR-MIN                 
060600     MOVE MID-IDKUNDNR                   TO W-IDKUNDNR-MIN                
060700     MOVE SPACE                          TO W-IDKUNDRF-MIN                
060800     MOVE MID-IDORDNR7                   TO WS-IDORDNR-NUM                
060900     MOVE WS-IDORDNR-NUM                 TO W-IDKUNDRF-MIN                
061000     MOVE ODEL-IDLEVNR                   TO W-IDLEVNR                     
061100                                            W-Q211KY-IDLEVNR              
061200     MOVE ODEL-IDDC                      TO W-Q211KY-IDDC                 
061300                                            WS-IDDC                       
061400     MOVE ODEL-IDPRODNR                  TO W-IDPRODNR                    
061500                                         IN W-IDPRODNR-X                  
061600     MOVE ODEL-IDPLKLST                  TO W-IDPLKLST                    
061700     MOVE HIGH-VALUE                     TO W-WDE4ASEQ-MAX-X              
061800     MOVE MID-IDDISTR                    TO W-IDDISTR-MAX                 
061900     MOVE MID-IDKUNDNR                   TO W-IDKUNDNR-MAX                
062000     MOVE SPACE                          TO W-IDKUNDRF-MAX                
062100     MOVE MID-IDORDNR7                   TO WS-IDORDNR-NUM                
062200     MOVE WS-IDORDNR-NUM                 TO W-IDKUNDRF-MAX                
062300                                                                          
062400     PERFORM IMS-GU-WDE401                                                
062500     IF SEGMENT-FINNS                                                     
062600       MOVE KORD-IDPLKLST        TO WS-IDPLKLST                           
062700       MOVE KORD-IDPRODNR        TO WS-IDPRODNR                           
062800       IF MID-IDRADNR-NEXT > ZERO AND IX-RAD = +1                         
062900         MOVE MID-IDRADNR-NEXT        TO W-IDPURAD                        
063000         PERFORM IMS-GNP-WDE411-KVAL-RADNR                                
063100       ELSE                                                               
063200         EVALUATE TRUE                                                    
063300         WHEN MID-IDARTNR > ZERO                                          
063400           MOVE MID-IDARTNR           TO W-IDARTNR                        
063500           PERFORM IMS-GNP-WDE411-KVAL-ARTNR                              
063600          WHEN OTHER                                                      
063700           PERFORM IMS-GNP-WDE411                                         
063800         END-EVALUATE                                                     
063900       END-IF                                                             
064000       PERFORM UNTIL SEGMENT-SAKNAS OR IX-RAD > MAX-RAD                   
064100         PERFORM S01-FLYTTA-RAD-TILL-MOD                                  
064200         PERFORM S02-LAS-WDE421                                           
064300         PERFORM D-LAS-KOLLIN                                             
064400       END-PERFORM                                                        
064500     END-IF                                                               
064600     .                                                                    
064700     EJECT                                                                
064800                                                                          
064900 CAB-BEHANDLA-ORDERRADKOE SECTION.                                        
065000                                                                          
065100     MOVE LOW-VALUE            TO  W-WDQ4ASEQ-MIN-X                       
065200     MOVE HIGH-VALUE           TO  W-WDQ4ASEQ-MAX-X                       
065300     MOVE OHUV-IDORDER         TO  W-Q4ASEQ-MIN-IDORDER                   
065400                                   W-Q4ASEQ-MAX-IDORDER                   
065500                                   W-Q4ASEQ-IDORDER                       
065600                                                                          
065700     IF MID-IDRADNR-NEXT       >   ZERO AND MID-FLSVAR-NEXT = 'O'         
065800         MOVE MID-IDARTNR      TO  W-Q4ASEQ-IDARTNR                       
065900         MOVE W-IDRADNR-2-4    TO  W-Q4ASEQ-IDLOPNR                       
066000         PERFORM IMS-GET-WLORQF01-KVAL                                    
066100     ELSE                                                                 
066200       IF MID-IDARTNR >   ZERO                                            
066300         MOVE MID-IDARTNR      TO  W-Q4ASEQ-MIN-IDARTNR                   
066400                                   W-Q4ASEQ-MAX-IDARTNR                   
066500       END-IF                                                             
066600       PERFORM IMS-GET-WLORQF01-OKVAL                                     
066700     END-IF                                                               
066800                                                                          
066900     PERFORM UNTIL SEGMENT-SAKNAS    OR                                   
067000                   SEGMENT-SLUT      OR                                   
067100                   IX-RAD      > MAX-RAD                                  
067200                                                                          
067300         PERFORM CABA-REDIGERA-ORDERRAD                                   
067400         ADD +1            TO IX-RAD                                      
067500                                                                          
067600         PERFORM IMS-GET-WLORQF01-OKVAL                                   
067700                                                                          
067800     END-PERFORM                                                          
067900                                                                          
068000     IF IX-RAD                 > MAX-RAD     AND                          
068100        SEGMENT-FINNS                                                     
068200         PERFORM CABB-SPARA-ORDERRAD-NYCKLAR                              
068300     END-IF                                                               
068400     .                                                                    
068500     EJECT                                                                
068600 CABA-REDIGERA-ORDERRAD SECTION.                                          
068700                                                                          
068800     IF MID-IDVTYP NOT = '2'                                              
068900       IF OHUV-FLKLAR          = NEJ                                      
069000           MOVE ZERO           TO MOD-KVBEART(IX-RAD)                     
069100           MOVE ZERO           TO MOD-KVAVBART(IX-RAD)                    
069200           MOVE ZERO           TO MOD-TIRODAT(IX-RAD)                     
069300           MOVE Q4-ORAD-IDDC   TO MOD-IDDC(IX-RAD)                        
069400           MOVE 'E '           TO MOD-KDORDSTA(IX-RAD)                    
069500       ELSE                                                               
069600           MOVE Q4-ORAD-KVBEART-Q TO MOD-KVBEART(IX-RAD)                  
069700           MOVE Q4-ORAD-KVPREAVB TO MOD-KVAVBART(IX-RAD)                  
069800           MOVE Q4-ORAD-IDDC   TO MOD-IDDC(IX-RAD)                        
069900                                    WS-IDDC                               
070000           IF GOOD-DDC                                                    
070100             MOVE WC-CDC-SE    TO W-IDDC                                  
070200           ELSE                                                           
070300             MOVE Q4-ORAD-IDDC TO W-IDDC                                  
070400           END-IF                                                         
070500                                                                          
070600           PERFORM IMS-GET-WLORQI12-KVAL                                  
070700           MOVE ARB-DATRPAVD(3:6) TO MOD-TIRODAT(IX-RAD)                  
070800                                                                          
070900           MOVE 'R '           TO MOD-KDORDSTA(IX-RAD)                    
071000       END-IF                                                             
071100       MOVE Q4-ORAD-IDARTNR    TO MOD-IDARTNR-RAD(IX-RAD)                 
071200       MOVE Q4-ORAD-IDBIL      TO MOD-IDBIL(IX-RAD)                       
071300       IF Q4-ORAD-KDTPOTYP     >  ZERO                                    
071400           MOVE Q4-ORAD-KDTPOTYP TO W-KDTPOTYP                            
071500           MOVE W-KDTPOTYP     TO MOD-KDTPOTYP(IX-RAD)                    
071600        ELSE                                                              
071700           MOVE SPACE          TO MOD-KDTPOTYP(IX-RAD)                    
071800       END-IF                                                             
071900       MOVE Q4-ORAD-IDKUNDRF-RO(1:7)                                      
072000                               TO MOD-IDORDNR7-RO(IX-RAD)                 
072100     ELSE                                                                 
072200       IF OHUV-FLKLAR          = NEJ                                      
072300           MOVE ZERO           TO MOD2-KVBEART(IX-RAD)                    
072400           MOVE ZERO           TO MOD2-KVAVBART(IX-RAD)                   
072500           MOVE ZERO           TO MOD2-TIRODAT(IX-RAD)                    
072600           MOVE Q4-ORAD-IDDC   TO MOD2-IDDC(IX-RAD)                       
072700           MOVE 'E '           TO MOD2-KDORDSTA(IX-RAD)                   
072800       ELSE                                                               
072900           MOVE Q4-ORAD-KVBEART-Q TO MOD2-KVBEART(IX-RAD)                 
073000           MOVE Q4-ORAD-KVPREAVB TO MOD2-KVAVBART(IX-RAD)                 
073100           MOVE Q4-ORAD-IDDC   TO MOD2-IDDC(IX-RAD)                       
073200                                    WS-IDDC                               
073300           IF GOOD-DDC                                                    
073400             MOVE WC-CDC-SE    TO W-IDDC                                  
073500           ELSE                                                           
073600             MOVE Q4-ORAD-IDDC TO W-IDDC                                  
073700           END-IF                                                         
073800                                                                          
073900           PERFORM IMS-GET-WLORQI12-KVAL                                  
074000           MOVE ARB-DATRPAVD(3:6) TO MOD2-TIRODAT(IX-RAD)                 
074100                                                                          
074200           MOVE 'R '           TO MOD2-KDORDSTA(IX-RAD)                   
074300       END-IF                                                             
074400       MOVE Q4-ORAD-IDARTNR    TO MOD2-IDARTNR-RAD(IX-RAD)                
074500       MOVE Q4-ORAD-IDBIL         TO MOD2-IDBIL(IX-RAD)                   
074600       IF Q4-ORAD-KDTPOTYP     >  ZERO                                    
074700           MOVE Q4-ORAD-KDTPOTYP TO W-KDTPOTYP                            
074800           MOVE W-KDTPOTYP     TO MOD2-KDTPOTYP(IX-RAD)                   
074900        ELSE                                                              
075000           MOVE SPACE          TO MOD2-KDTPOTYP(IX-RAD)                   
075100       END-IF                                                             
075200       MOVE Q4-ORAD-IDKUNDRF-RO(1:7)                                      
075300                               TO MOD2-IDORDNR7-RO(IX-RAD)                
075400     END-IF                                                               
075500     .                                                                    
075600     EJECT                                                                
075700 CABB-SPARA-ORDERRAD-NYCKLAR SECTION.                                     
075800                                                                          
075900     MOVE ZERO                 TO  W-IDRADNR-1                            
076000     MOVE Q4-ORAD-IDLOPNR      TO  W-IDRADNR-2-4                          
076100     MOVE Q4-ORAD-IDARTNR      TO  WS-IDARTNR-NUM                         
076200     IF MID-IDVTYP NOT = '2'                                              
076300       MOVE 'O'                TO  MOD-FLSVAR-NEXT                        
076400       MOVE WS-IDARTNR-NUM     TO  MOD-IDARTNR                            
076500       MOVE W-IDRADNR          TO  MOD-IDRADNR-NEXT                       
076600     ELSE                                                                 
076700       MOVE 'O'                TO  MOD2-FLSVAR-NEXT                       
076800       MOVE WS-IDARTNR-NUM     TO  MOD2-IDARTNR                           
076900       MOVE W-IDRADNR          TO  MOD2-IDRADNR-NEXT                      
077000     END-IF                                                               
077100     .                                                                    
077200     EJECT                                                                
077300 CB-BEHANDLA-RESTORDER SECTION.                                           
077400                                                                          
077500     MOVE LOW-VALUE            TO  W-WDA501KY-MIN-X                       
077600     MOVE HIGH-VALUE           TO  W-WDA501KY-MAX-X                       
077700     MOVE MID-IDDISTR          TO  W-A501KY-MIN-IDDISTR                   
077800                                   W-A501KY-MAX-IDDISTR                   
077900                                   W-A501KY-IDDISTR                       
078000     MOVE MID-IDKUNDNR         TO  W-A501KY-MIN-IDKUNDNR                  
078100                                   W-A501KY-MAX-IDKUNDNR                  
078200                                   W-A501KY-IDKUNDNR                      
078300     MOVE SPACE                TO  W-A501KY-MIN-IDKUNDRF                  
078400                                   W-A501KY-MAX-IDKUNDRF                  
078500                                   W-A501KY-IDKUNDRF                      
078600     MOVE MID-IDORDNR7(3:5)    TO  W-A501KY-MIN-IDORDNR5                  
078700                                   W-A501KY-MAX-IDORDNR5                  
078800                                   W-A501KY-IDORDNR5                      
078900                                                                          
079000     IF MID-IDRADNR-NEXT       >   ZERO AND MID-FLSVAR-NEXT = 'R'         
079100       MOVE MID-IDARTNR        TO  W-A501KY-IDARTNR                       
079200       MOVE W-IDRADNR-2-4      TO  W-A501KY-IDLOPNR                       
079300       PERFORM IMS-GET-WLORDP01-KVAL                                      
079400     ELSE                                                                 
079500       IF MID-IDARTNR > ZERO                                              
079600         MOVE MID-IDARTNR      TO  W-A501KY-MIN-IDARTNR                   
079700                                   W-A501KY-MAX-IDARTNR                   
079800       END-IF                                                             
079900       PERFORM IMS-GET-WLORDP01-OKVAL                                     
080000     END-IF                                                               
080100                                                                          
080200     PERFORM UNTIL SEGMENT-SAKNAS    OR                                   
080300                   SEGMENT-SLUT      OR                                   
080400                   IX-RAD      > MAX-RAD                                  
080500                                                                          
080600         MOVE RAD-IDARTNR      TO W-IDARTNR                               
080700                               IN W-IDARTNR-X                             
080800                                                                          
080900         PERFORM IMS-GU-WLARTC11-KVAL                                     
081000         PERFORM CBA-REDIGERA-RO-RAD                                      
081100         ADD +1                TO IX-RAD                                  
081200                                                                          
081300         PERFORM IMS-GET-WLORDP01-OKVAL                                   
081400     END-PERFORM                                                          
081500                                                                          
081600     IF IX-RAD                 > MAX-RAD   AND                            
081700        SEGMENT-FINNS                                                     
081800           PERFORM CBB-SPARA-RESTORDER-NYCKLAR                            
081900     END-IF                                                               
082000     .                                                                    
082100     EJECT                                                                
082200 CBA-REDIGERA-RO-RAD SECTION.                                             
082300                                                                          
082400     IF MID-IDVTYP NOT = '2'                                              
082500       MOVE RAD-IDARTNR        TO MOD-IDARTNR-RAD(IX-RAD)                 
082600       MOVE SPACE              TO MOD-IDBIL(IX-RAD)                       
082700       MOVE RAD-KVBEART-Q      TO MOD-KVBEART(IX-RAD)                     
082800       MOVE RAD-IDDC           TO MOD-IDDC(IX-RAD)                        
082900       MOVE RAD-KDTPOTYP       TO MOD-KDTPOTYP(IX-RAD)                    
083000       IF RAD-DARODAT = ZERO                                              
083100         MOVE RAD-TITPO           TO MOD-TIRODAT(IX-RAD)                  
083200         MOVE RAD-KVART           TO MOD-KVAVBART(IX-RAD)                 
083300         IF RAD-KDSTARAD = '1' OR '3'                                     
083400           MOVE 'T '              TO MOD-KDORDSTA(IX-RAD)                 
083500         ELSE                                                             
083600           IF RAD-KDSTARAD = '4'                                          
083700             MOVE 'RT'            TO MOD-KDORDSTA(IX-RAD)                 
083800           END-IF                                                         
083900         END-IF                                                           
084000       ELSE                                                               
084100         MOVE RAD-IDDC            TO WS-IDDC                              
084300         IF NDC                                                           
084400           PERFORM CBAA-HAEMTA-TIBERANK                                   
084500                                                                          
084600           IF ETA-SVAR-OK = JA                                            
084700             MOVE ETA-TIAAMMDD-SVAR TO MOD-TIRODAT(IX-RAD)                
084800           ELSE                                                           
084900             MOVE ZERO            TO MOD-TIRODAT(IX-RAD)                  
085000           END-IF                                                         
085100         ELSE                                                             
085200           MOVE CLAG-TIDISPIN     TO MOD-TIRODAT(IX-RAD)                  
085300         END-IF                                                           
085400         MOVE RAD-KVRO            TO MOD-KVAVBART(IX-RAD)                 
085500         IF RAD-KDSTARAD = '2' OR '3'                                     
085600           MOVE 'B '              TO MOD-KDORDSTA(IX-RAD)                 
085700         ELSE                                                             
085800           IF RAD-KDSTARAD = '4'                                          
085900             MOVE 'RB'            TO MOD-KDORDSTA(IX-RAD)                 
086000           END-IF                                                         
086100         END-IF                                                           
086200       END-IF                                                             
086300       IF RAD-KDSTARAD = '4'                                              
086400         IF RAD-IDKUNDRF-LEV NOT = '00000     '                           
086500           IF RAD-IDKUNDRF-LEV NOT = WS-IDKUNDRF-CONT                     
086600             MOVE RAD-IDKUNDRF-LEV(1:5) TO MOD-IDORDNR7-RO(IX-RAD)        
086700           END-IF                                                         
086800         END-IF                                                           
086900       END-IF                                                             
087000     ELSE                                                                 
087100       MOVE RAD-IDARTNR        TO MOD2-IDARTNR-RAD(IX-RAD)                
087200       MOVE SPACE              TO MOD2-IDRADINF(IX-RAD)                   
087300       MOVE RAD-KVBEART-Q      TO MOD2-KVBEART(IX-RAD)                    
087400       MOVE RAD-IDDC           TO MOD2-IDDC(IX-RAD)                       
087500       MOVE RAD-KDTPOTYP       TO MOD2-KDTPOTYP(IX-RAD)                   
087600       IF RAD-DARODAT = ZERO                                              
087700         MOVE RAD-TITPO           TO MOD2-TIRODAT(IX-RAD)                 
087800         MOVE RAD-KVART           TO MOD2-KVAVBART(IX-RAD)                
087900         IF RAD-KDSTARAD = '1' OR '3'                                     
088000           MOVE 'T '              TO MOD2-KDORDSTA(IX-RAD)                
088100         ELSE                                                             
088200           IF RAD-KDSTARAD = '4'                                          
088300             MOVE 'RT'            TO MOD2-KDORDSTA(IX-RAD)                
088400           END-IF                                                         
088500         END-IF                                                           
088600       ELSE                                                               
088700         MOVE RAD-IDDC            TO WS-IDDC                              
088900         IF NDC                                                           
089000           PERFORM CBAA-HAEMTA-TIBERANK                                   
089100                                                                          
089200           IF ETA-SVAR-OK = JA                                            
089400             IF NDC-NA OR NDC-CN                                          
089500               MOVE ETA-TIAAMMDD-SVAR TO MOD2-TIRODAT(IX-RAD)             
089600             ELSE                                                         
089700               IF ETA-KVAVIS-ETA > +0                                     
089800                 MOVE ETA-TIAAMMDD-SVAR TO MOD2-TIRODAT(IX-RAD)           
089900               ELSE                                                       
090000                 MOVE ZERO        TO MOD2-TIRODAT(IX-RAD)                 
090100               END-IF                                                     
090200             END-IF                                                       
090300           ELSE                                                           
090400             MOVE ZERO            TO MOD2-TIRODAT(IX-RAD)                 
090500           END-IF                                                         
090600         ELSE                                                             
090700           MOVE CLAG-TIDISPIN     TO MOD2-TIRODAT(IX-RAD)                 
090800         END-IF                                                           
090900         MOVE RAD-KVRO            TO MOD2-KVAVBART(IX-RAD)                
091000         IF RAD-KDSTARAD = '2' OR '3'                                     
091100           MOVE 'B '              TO MOD2-KDORDSTA(IX-RAD)                
091200         ELSE                                                             
091300           IF RAD-KDSTARAD = '4'                                          
091400             MOVE 'RB'            TO MOD2-KDORDSTA(IX-RAD)                
091500           END-IF                                                         
091600         END-IF                                                           
091700       END-IF                                                             
091800       IF RAD-KDSTARAD = '4'                                              
091900         IF RAD-IDKUNDRF-LEV NOT = '00000     '                           
092000          IF RAD-IDKUNDRF-LEV NOT = WS-IDKUNDRF-CONT                      
092100            MOVE RAD-IDKUNDRF-LEV(1:5) TO MOD2-IDORDNR7-RO(IX-RAD)        
092200          END-IF                                                          
092300         END-IF                                                           
092400       END-IF                                                             
092500     END-IF                                                               
092600     .                                                                    
092700     EJECT                                                                
092800                                                                          
092900 CBAA-HAEMTA-TIBERANK SECTION.                                            
093000                                                                          
093100     MOVE '612'                TO ETA-KDCALL                              
093200     MOVE RAD-IDDC             TO ETA-IDDC-REC                            
093300     MOVE RAD-IDARTNR          TO ETA-IDARTNR                             
093400     MOVE SPACE                TO ETA-IDLEVNR                             
093500     MOVE ZERO                 TO ETA-KDFRAKT                             
093600     MOVE RAD-DARODAT(3:6)     TO ETA-TIAAMMDD-ANROP                      
093700                                  WS-ETA-DATUM                            
093800     IF WS-ETA-DATUM-AAR > 50                                             
093900        MOVE 19                TO ETA-TISEKEL-ANROP                       
094000     ELSE                                                                 
094100        MOVE 20                TO ETA-TISEKEL-ANROP                       
094200     END-IF                                                               
094300                                                                          
094400     CALL W218ETA  USING ETA-W218LETA                                     
094500                         ETA-ARTC-PCB ETA-WDK7-PCB                        
094600                         ETA-INLC-PCB ETA-LEVA-PCB                        
094700                         ETA-WDB6-PCB ETA-WDD9-PCB                        
094800     .                                                                    
094900                                                                          
095000     EJECT                                                                
095100 CBB-SPARA-RESTORDER-NYCKLAR SECTION.                                     
095200                                                                          
095300     IF MID-IDVTYP NOT = '2'                                              
095400       MOVE 'R'                TO  MOD-FLSVAR-NEXT                        
095500       MOVE ZERO               TO  W-IDRADNR-1                            
095600       MOVE RAD-IDARTNR        TO  WS-IDARTNR-NUM                         
095700       MOVE WS-IDARTNR-NUM     TO  MOD-IDARTNR                            
095800       MOVE RAD-IDLOPNR        TO  W-IDRADNR-2-4                          
095900       MOVE W-IDRADNR          TO  MOD-IDRADNR-NEXT                       
096000     ELSE                                                                 
096100       MOVE 'R'                TO  MOD2-FLSVAR-NEXT                       
096200       MOVE ZERO               TO  W-IDRADNR-1                            
096300       MOVE RAD-IDARTNR        TO  WS-IDARTNR-NUM                         
096400       MOVE WS-IDARTNR-NUM     TO  MOD2-IDARTNR                           
096500       MOVE RAD-IDLOPNR        TO  W-IDRADNR-2-4                          
096600       MOVE W-IDRADNR          TO  MOD2-IDRADNR-NEXT                      
096700     END-IF                                                               
096800     .                                                                    
096900     EJECT                                                                
097000                                                                          
097100 D-LAS-KOLLIN            SECTION.                                         
097200                                                                          
097300                                                                          
097400     PERFORM UNTIL                                                        
097500      SEGMENT-SAKNAS OR IX-RAD > MAX-RAD                                  
097600                                                                          
097700       IF MID-IDVTYP NOT = '2'                                            
097800         MOVE KKOLLI-KVLEVART       TO MOD-KVLEVART    (IX-RAD)           
097900         MOVE KKOLLI-IDKOLLI        TO MOD-IDKOLLI     (IX-RAD)           
098000         MOVE SPACE                 TO MOD-KDTPOTYP    (IX-RAD)           
098100                                                                          
098200         IF KOLLI-KDKOLSTA = +0                                           
098300           MOVE 'U'                 TO MOD-KDORDSTA    (IX-RAD)           
098400         ELSE                                                             
098500           IF KOLLI-KDKOLSTA = +1                                         
098600             MOVE 'P'               TO MOD-KDORDSTA    (IX-RAD)           
098700           ELSE                                                           
098800             IF KOLLI-KDKOLSTA = +2 OR +3                                 
098900               MOVE 'L'             TO MOD-KDORDSTA    (IX-RAD)           
099000             ELSE                                                         
099100               IF KOLLI-KDKOLSTA = +4                                     
099200                 MOVE 'LF'          TO MOD-KDORDSTA    (IX-RAD)           
099300               ELSE                                                       
099400                 IF KOLLI-KDKOLSTA = +7                                   
099500                   MOVE 'S'            TO MOD-KDORDSTA(IX-RAD)            
099600                 ELSE                                                     
099700                   IF KOLLI-KDKOLSTA = +9                                 
099800                     MOVE 'SF'           TO MOD-KDORDSTA(IX-RAD)          
099900                     MOVE KOLLI-TIFAKT   TO MOD-TIRODAT(IX-RAD)           
100000                   END-IF                                                 
100100                 END-IF                                                   
100200               END-IF                                                     
100300             END-IF                                                       
100400           END-IF                                                         
100500         END-IF                                                           
100600       ELSE                                                               
100700         MOVE KKOLLI-KVLEVART       TO MOD2-KVLEVART   (IX-RAD)           
100800         MOVE KKOLLI-IDKOLLI        TO MOD2-IDKOLLI    (IX-RAD)           
100900         MOVE SPACE                 TO MOD2-KDTPOTYP   (IX-RAD)           
101000                                                                          
101100         IF KOLLI-KDKOLSTA = +0                                           
101200           MOVE 'U'                 TO MOD2-KDORDSTA   (IX-RAD)           
101300         ELSE                                                             
101400           IF KOLLI-KDKOLSTA = +1                                         
101500             MOVE 'P'               TO MOD2-KDORDSTA   (IX-RAD)           
101600           ELSE                                                           
101700             IF KOLLI-KDKOLSTA = +2 OR +3                                 
101800               MOVE 'L'             TO MOD2-KDORDSTA   (IX-RAD)           
101900             ELSE                                                         
102000               IF KOLLI-KDKOLSTA = +4                                     
102100                 MOVE 'LF'          TO MOD2-KDORDSTA   (IX-RAD)           
102200               ELSE                                                       
102300                 IF KOLLI-KDKOLSTA = +7                                   
102400                   MOVE 'S'            TO MOD2-KDORDSTA(IX-RAD)           
102500                 ELSE                                                     
102600                   IF KOLLI-KDKOLSTA = +9                                 
102700                     MOVE 'SF'          TO MOD2-KDORDSTA(IX-RAD)          
102800                     MOVE KOLLI-TIFAKT  TO MOD2-TIRODAT(IX-RAD)           
102900                   END-IF                                                 
103000                 END-IF                                                   
103100               END-IF                                                     
103200             END-IF                                                       
103300           END-IF                                                         
103400         END-IF                                                           
103500       END-IF                                                             
103600                                                                          
103700       PERFORM IMS-GNP-WDE421                                             
103800       ADD +1                        TO IX-RAD                            
103900       IF SEGMENT-FINNS                                                   
104000         MOVE ORAD-IDARTNR           TO MOD-IDARTNR-RAD(IX-RAD)           
104100         MOVE KKOLLI-IDKOLLI         TO W-IDKOLLI                         
104200         PERFORM IMS-GNP-WDE611                                           
104300       END-IF                                                             
104400                                                                          
104500     END-PERFORM                                                          
104600     IF SEGMENT-FINNS                                                     
104700       MOVE 'P'                      TO MOD-FLSVAR-NEXT                   
104800       MOVE W-IDPURAD                TO WS-IDPURAD-NUM                    
104900       IF MID-IDVTYP NOT = '2'                                            
105000         MOVE WS-IDPURAD-NUM         TO MOD-IDRADNR-NEXT                  
105100         MOVE KKOLLI-IDPRODNR        TO MOD-IDPRODNR-NEXT                 
105200         MOVE WS-IDPLKLST            TO MOD-IDPLKLST-NEXT                 
105300         MOVE KKOLLI-IDKOLLI         TO MOD-IDKOLLI-NEXT                  
105400         MOVE ODEL-IDDC              TO MOD-IDDC-NEXT                     
105500       ELSE                                                               
105600         MOVE WS-IDPURAD-NUM         TO MOD2-IDRADNR-NEXT                 
105700         MOVE KKOLLI-IDPRODNR        TO MOD2-IDPRODNR-NEXT                
105800         MOVE WS-IDPLKLST            TO MOD2-IDPLKLST-NEXT                
105900         MOVE KKOLLI-IDKOLLI         TO MOD2-IDKOLLI-NEXT                 
106000         MOVE ODEL-IDDC              TO MOD2-IDDC-NEXT                    
106100       END-IF                                                             
106200     ELSE                                                                 
106300       PERFORM DA-LAS-NASTA-RAD                                           
106400     END-IF                                                               
106500     .                                                                    
106600     EJECT                                                                
106700 DA-LAS-NASTA-RAD      SECTION.                                           
106800                                                                          
106900     IF MID-IDARTNR > ZERO                                                
107000       MOVE MID-IDARTNR              TO W-IDARTNR                         
107100       PERFORM IMS-GNP-WDE411-KVAL-ARTNR                                  
107200     ELSE                                                                 
107300       PERFORM IMS-GNP-WDE411                                             
107400     END-IF                                                               
107500     IF SEGMENT-FINNS                                                     
107600       IF IX-RAD > MAX-RAD                                                
107700         MOVE 'P'                    TO MOD-FLSVAR-NEXT                   
107800         MOVE ORAD-IDPURAD           TO WS-IDPURAD-NUM                    
107900         IF MID-IDVTYP NOT = '2'                                          
108000           MOVE WS-IDPURAD-NUM       TO MOD-IDRADNR-NEXT                  
108100           MOVE WS-IDPRODNR          TO MOD-IDPRODNR-NEXT                 
108200           MOVE WS-IDPLKLST          TO MOD-IDPLKLST-NEXT                 
108300           MOVE ODEL-IDDC            TO MOD-IDDC-NEXT                     
108400         ELSE                                                             
108500           MOVE WS-IDPURAD-NUM       TO MOD2-IDRADNR-NEXT                 
108600           MOVE WS-IDPRODNR          TO MOD2-IDPRODNR-NEXT                
108700           MOVE WS-IDPLKLST          TO MOD2-IDPLKLST-NEXT                
108800           MOVE ODEL-IDDC            TO MOD2-IDDC-NEXT                    
108900         END-IF                                                           
109000       END-IF                                                             
109100       MOVE ORAD-IDPURAD             TO W-IDPURAD                         
109200     ELSE                                                                 
109300       IF IX-RAD > MAX-RAD                                                
109400         PERFORM DAA-LAS-NASTA-ORDERDEL                                   
109500       END-IF                                                             
109600     END-IF                                                               
109700     .                                                                    
109800     EJECT                                                                
109900 DAA-LAS-NASTA-ORDERDEL SECTION.                                          
110000                                                                          
110100     PERFORM IMS-GET-WLORQA01-OKVAL-GN                                    
110200     IF SEGMENT-FINNS                                                     
110300       IF ODEL-KDODELSTA NOT = 'R'                                        
110400         MOVE LOW-VALUE           TO W-WDE4ASEQ-MIN-X                     
110500         MOVE MID-IDDISTR         TO W-IDDISTR-MIN                        
110600         MOVE MID-IDKUNDNR        TO W-IDKUNDNR-MIN                       
110700         MOVE SPACE               TO W-IDKUNDRF-MIN                       
110800         MOVE MID-IDORDNR7        TO WS-IDORDNR-NUM                       
110900         MOVE WS-IDORDNR-NUM      TO W-IDKUNDRF-MIN                       
111000         MOVE ODEL-IDLEVNR        TO W-IDLEVNR                            
111100                                     W-Q211KY-IDLEVNR                     
111200         MOVE ODEL-IDDC           TO W-Q211KY-IDDC                        
111300                                     WS-IDDC                              
111400         MOVE ODEL-IDPRODNR       TO W-IDPRODNR                           
111500                                  IN W-IDPRODNR-X                         
111600         MOVE ODEL-IDPLKLST       TO W-IDPLKLST                           
111700         MOVE HIGH-VALUE          TO W-WDE4ASEQ-MAX-X                     
111800         MOVE MID-IDDISTR         TO W-IDDISTR-MAX                        
111900         MOVE MID-IDKUNDNR        TO W-IDKUNDNR-MAX                       
112000         MOVE SPACE               TO W-IDKUNDRF-MAX                       
112100         MOVE MID-IDORDNR7        TO WS-IDORDNR-NUM                       
112200         MOVE WS-IDORDNR-NUM      TO W-IDKUNDRF-MAX                       
112300                                                                          
112400         PERFORM IMS-GU-WDE401                                            
112500         IF SEGMENT-FINNS                                                 
112600           MOVE KORD-IDPRODNR    TO WS-IDPRODNR                           
112700           MOVE KORD-IDPLKLST    TO WS-IDPLKLST                           
112800           EVALUATE TRUE                                                  
112900             WHEN MID-IDARTNR > ZERO                                      
113000               MOVE MID-IDARTNR       TO W-IDARTNR                        
113100               PERFORM IMS-GNP-WDE411-KVAL-ARTNR                          
113200             WHEN OTHER                                                   
113300               PERFORM IMS-GNP-WDE411                                     
113400           END-EVALUATE                                                   
113500           IF SEGMENT-FINNS                                               
113600             MOVE ORAD-IDPURAD        TO WS-IDPURAD-NUM                   
113700             IF MID-IDVTYP NOT = '2'                                      
113800               MOVE 'P'               TO MOD-FLSVAR-NEXT                  
113900               MOVE WS-IDPURAD-NUM    TO MOD-IDRADNR-NEXT                 
114000               MOVE WS-IDPRODNR       TO MOD-IDPRODNR-NEXT                
114100               MOVE WS-IDPLKLST       TO MOD-IDPLKLST-NEXT                
114200               MOVE ODEL-IDDC         TO MOD-IDDC-NEXT                    
114300             ELSE                                                         
114400               MOVE 'P'               TO MOD2-FLSVAR-NEXT                 
114500               MOVE WS-IDPURAD-NUM    TO MOD2-IDRADNR-NEXT                
114600               MOVE WS-IDPRODNR       TO MOD2-IDPRODNR-NEXT               
114700               MOVE WS-IDPLKLST       TO MOD2-IDPLKLST-NEXT               
114800               MOVE ODEL-IDDC         TO MOD2-IDDC-NEXT                   
114900             END-IF                                                       
115000           END-IF                                                         
115100         END-IF                                                           
115200       END-IF                                                             
115300     END-IF                                                               
115400     .                                                                    
115500     EJECT                                                                
115600                                                                          
115700 E-KONTROLLERA-OM-SIDA-TOM SECTION.                                       
115800                                                                          
115900     IF IX-RAD                 = 1                                        
116000       IF MID-IDVTYP NOT = '2'                                            
116100         IF MID-IDARTNR > ZERO                                            
116200           MOVE ERR-PART-MISSING  TO MOD-IDMFSFEL                         
116300         ELSE                                                             
116400           MOVE ERR-ORDER-NOT-FOUND TO MOD-IDMFSFEL                       
116500         END-IF                                                           
116600       ELSE                                                               
116700         IF MID-IDARTNR > ZERO                                            
116800           MOVE ERR-PART-MISSING  TO MOD2-IDMFSFEL                        
116900         ELSE                                                             
117000           MOVE ERR-ORDER-NOT-FOUND TO MOD2-IDMFSFEL                      
117100         END-IF                                                           
117200       END-IF                                                             
117300     END-IF                                                               
117400     .                                                                    
117500     EJECT                                                                
117600 F-BERAKNA-MAX-MOD-LANGD SECTION.                                         
117700                                                                          
117800     IF MID-IDVTYP NOT = '2'                                              
117900       COMPUTE MAX-MOD-LANGD = LENGTH OF MOD-W9O33201 + 4                 
118000       MOVE 11                 TO  IX-RAD                                 
118100       PERFORM UNTIL IX-RAD    = 0                                        
118200           IF MOD-IDARTNR-RAD(IX-RAD) = ZERO                              
118300               SUBTRACT +62    FROM MAX-MOD-LANGD                         
118400               SUBTRACT +1     FROM IX-RAD                                
118500           ELSE                                                           
118600               MOVE ZERO       TO IX-RAD                                  
118700           END-IF                                                         
118800       END-PERFORM                                                        
118900     ELSE                                                                 
119000       COMPUTE MAX-MOD-LANGD = LENGTH OF MOD2-W9O33202 + 4                
119100       MOVE 11                 TO  IX-RAD                                 
119200       PERFORM UNTIL IX-RAD    = 0                                        
119300           IF MOD2-IDARTNR-RAD(IX-RAD) = ZERO                             
119400               SUBTRACT +62    FROM MAX-MOD-LANGD                         
119500               SUBTRACT +1     FROM IX-RAD                                
119600           ELSE                                                           
119700               MOVE ZERO       TO IX-RAD                                  
119800           END-IF                                                         
119900       END-PERFORM                                                        
120000     END-IF                                                               
120100     .                                                                    
120200     EJECT                                                                
120300                                                                          
120400 S01-FLYTTA-RAD-TILL-MOD   SECTION.                                       
120500                                                                          
120600     IF MID-IDVTYP NOT = '2'                                              
120700       MOVE ORAD-IDARTNR          TO MOD-IDARTNR-RAD(IX-RAD)              
120800                                                                          
120900       MOVE ORAD-IDBIL            TO MOD-IDBIL      (IX-RAD)              
121000       MOVE ORAD-KVBEART          TO MOD-KVBEART    (IX-RAD)              
121100       MOVE ORAD-KVAVBART         TO MOD-KVAVBART   (IX-RAD)              
121200       MOVE ORAD-KVLEVART         TO MOD-KVLEVART   (IX-RAD)              
121300       MOVE KORD-IDDC             TO MOD-IDDC       (IX-RAD)              
121400       MOVE ZERO                  TO MOD-IDKOLLI    (IX-RAD)              
121500       MOVE SPACE                 TO MOD-KDTPOTYP   (IX-RAD)              
121600                                                                          
121700       IF ORAD-KDRADSTA < +4                                              
121800         MOVE 'U'                 TO MOD-KDORDSTA   (IX-RAD)              
121900       ELSE                                                               
122000         IF ORAD-KVAVBART = ZERO                                          
122100           MOVE 'N'               TO MOD-KDORDSTA   (IX-RAD)              
122200         ELSE                                                             
122300           MOVE 'P'               TO MOD-KDORDSTA   (IX-RAD)              
122400         END-IF                                                           
122500       END-IF                                                             
122600     ELSE                                                                 
122700       MOVE ORAD-IDARTNR          TO MOD2-IDARTNR-RAD(IX-RAD)             
122800                                                                          
122900       IF GOOD-DDC                                                        
123000         PERFORM IMS-GET-WLORQI11-KVAL                                    
123100         MOVE DIRL-KVDAGAR-DIFF    TO WS-KVDAGAR-DIFF                     
123200         MOVE WS-KVDAGAR-DIFF(2:2) TO MOD2-KVDAGAR-DIFF (IX-RAD)          
123300         IF DIRL-KVDAGAR-DIFF < ZERO                                      
123400           MOVE '-'               TO MOD2-IDTECKEN  (IX-RAD)              
123500         ELSE                                                             
123600           MOVE '+'               TO MOD2-IDTECKEN  (IX-RAD)              
123700         END-IF                                                           
123800         PERFORM IMS-GET-WDF106                                           
123900         IF SEGMENT-FINNS                                                 
124000           MOVE ADR-BELEV         TO MOD2-IDLEVNMN  (IX-RAD)              
124100         ELSE                                                             
124200           MOVE SPACE             TO MOD2-IDLEVNMN  (IX-RAD)              
124300         END-IF                                                           
124400       ELSE                                                               
124500         MOVE ORAD-IDBIL          TO MOD2-IDBIL     (IX-RAD)              
124600       END-IF                                                             
124700       MOVE ORAD-KVBEART          TO MOD2-KVBEART   (IX-RAD)              
124800       MOVE ORAD-KVAVBART         TO MOD2-KVAVBART  (IX-RAD)              
124900       MOVE ORAD-KVLEVART         TO MOD2-KVLEVART  (IX-RAD)              
125000       MOVE KORD-IDDC             TO MOD2-IDDC      (IX-RAD)              
125100       MOVE ZERO                  TO MOD2-IDKOLLI   (IX-RAD)              
125200       MOVE SPACE                 TO MOD2-KDTPOTYP  (IX-RAD)              
125300                                                                          
125400       IF ORAD-KDRADSTA < +4                                              
125500         MOVE 'U'                 TO MOD2-KDORDSTA  (IX-RAD)              
125600       ELSE                                                               
125700         IF ORAD-KVAVBART = ZERO                                          
125800           MOVE 'N'               TO MOD2-KDORDSTA  (IX-RAD)              
125900         ELSE                                                             
126000           MOVE 'P'               TO MOD2-KDORDSTA  (IX-RAD)              
126100         END-IF                                                           
126200       END-IF                                                             
126300     END-IF                                                               
126400     PERFORM IMS-GET-WDE601                                               
126500     IF MID-IDVTYP NOT = '2'                                              
126600       IF SEGMENT-FINNS                                                   
126700         MOVE VORD-DABEGPAC(3:6)  TO MOD-TIRODAT    (IX-RAD)              
126800       ELSE                                                               
126900         MOVE ZERO                TO MOD-TIRODAT    (IX-RAD)              
127000       END-IF                                                             
127100       MOVE ORAD-IDKUNDRF-RO(1:5)                                         
127200                                    TO MOD-IDORDNR7-RO(IX-RAD)            
127300     ELSE                                                                 
127400       IF SEGMENT-FINNS                                                   
127500         MOVE VORD-DABEGPAC(3:6)  TO MOD2-TIRODAT   (IX-RAD)              
127600       ELSE                                                               
127700         MOVE ZERO                TO MOD2-TIRODAT   (IX-RAD)              
127800       END-IF                                                             
127900       MOVE ORAD-IDKUNDRF-RO(1:5)                                         
128000                                    TO MOD2-IDORDNR7-RO(IX-RAD)           
128100     END-IF                                                               
128200     .                                                                    
128300     EJECT                                                                
128400                                                                          
128500 S02-LAS-WDE421 SECTION.                                                  
128600                                                                          
128700     IF MID-IDKOLLI-NEXT > ZERO                                           
128800       MOVE ORAD-IDPURAD              TO W-IDPURAD                        
128900       MOVE MID-IDPRODNR-NEXT         TO W-IDPRODNR-E421                  
129000       MOVE MID-IDKOLLI-NEXT          TO W-IDKOLLI-E421                   
129100                                         W-IDKOLLI                        
129200       PERFORM IMS-GNP-WDE421-KVAL                                        
129300       IF SEGMENT-FINNS                                                   
129400         PERFORM IMS-GNP-WDE611                                           
129500       ELSE                                                               
129600         ADD +1                       TO IX-RAD                           
129700       END-IF                                                             
129800     ELSE                                                                 
129900       MOVE ORAD-IDPURAD              TO W-IDPURAD                        
130000       PERFORM IMS-GNP-WDE421                                             
130100       IF SEGMENT-FINNS                                                   
130200         MOVE KKOLLI-IDKOLLI          TO W-IDKOLLI                        
130300         PERFORM IMS-GNP-WDE611                                           
130400       ELSE                                                               
130500         ADD +1                       TO IX-RAD                           
130600       END-IF                                                             
130700     END-IF                                                               
130800     .                                                                    
130900     EJECT                                                                
131000                                                                          
131100* IMS SEKTIONER                                                           
131200     SKIP3                                                                
131300 IMS-GET-MSG SECTION.                                                     
131400                                                                          
131500     MOVE '  QC' TO GODK-STATUSKODER                                      
131600     CALL CBLTDLI  USING GU MSG-PCB MSG-IO-AREA                           
131700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
131800     PERFORM IMS-STATUSKONTROLL                                           
131900     .                                                                    
132000     SKIP3                                                                
132100 IMS-INSERT-MSG SECTION.                                                  
132200                                                                          
132300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
132400     MOVE SPACE TO GODK-STATUSKODER                                       
132500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
132600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
132700     PERFORM IMS-STATUSKONTROLL                                           
132800     .                                                                    
132900     EJECT                                                                
133000 IMS-GET-WLORQA01-OKVAL-GU SECTION.                                       
133100                                                                          
133200     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
133300                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
133400            DELIMITED BY SIZE INTO SSA1                                   
133500     MOVE '  GE' TO GODK-STATUSKODER                                      
133600     CALL CBLTDLI USING GU ORQA-PCB IO-AREA-ORQA01 SSA1                   
133700     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
133800     PERFORM IMS-STATUSKONTROLL                                           
133900     .                                                                    
134000     SKIP2                                                                
134100 IMS-GET-WLORQA01-OKVAL-GN SECTION.                                       
134200                                                                          
134300     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
134400                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
134500            DELIMITED BY SIZE INTO SSA1                                   
134600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
134700     CALL CBLTDLI USING GN ORQA-PCB IO-AREA-ORQA01 SSA1                   
134800     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
134900     PERFORM IMS-STATUSKONTROLL                                           
135000     .                                                                    
135100     EJECT                                                                
135200 IMS-GET-WLORQF01-KVAL SECTION.                                           
135300                                                                          
135400     STRING 'WLORQF01(WDQ4ASEQ =' W-WDQ4ASEQ-X ')'                        
135500            DELIMITED BY SIZE INTO SSA1                                   
135600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
135700     CALL CBLTDLI USING GU ORQF-PCB IO-AREA-ORQF01 SSA1                   
135800     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
135900     PERFORM IMS-STATUSKONTROLL                                           
136000     .                                                                    
136100     SKIP2                                                                
136200 IMS-GET-WLORQF01-OKVAL SECTION.                                          
136300                                                                          
136400     STRING 'WLORQF01(WDQ4ASEQ>=' W-WDQ4ASEQ-MIN-X                        
136500                    '&WDQ4ASEQ<=' W-WDQ4ASEQ-MAX-X ')'                    
136600            DELIMITED BY SIZE INTO SSA1                                   
136700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
136800     CALL CBLTDLI   USING GN ORQF-PCB IO-AREA-ORQF01 SSA1                 
136900     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
137000     PERFORM IMS-STATUSKONTROLL                                           
137100     .                                                                    
137200     EJECT                                                                
137300 IMS-GET-WLORQI01-KVAL SECTION.                                           
137400                                                                          
137500     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
137600            DELIMITED BY SIZE INTO SSA1                                   
137700     MOVE '  GE' TO GODK-STATUSKODER                                      
137800     CALL CBLTDLI USING GU ORQI-PCB IO-AREA-ORQI01 SSA1                   
137900     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
138000     PERFORM IMS-STATUSKONTROLL                                           
138100     .                                                                    
138200     SKIP2                                                                
138300 IMS-GET-WLORQI11-KVAL SECTION.                                           
138400                                                                          
138500     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
138600            DELIMITED BY SIZE INTO SSA1                                   
138700     STRING 'WLORQI11(WDQ211KY =' W-WDQ211KY-X ')'                        
138800            DELIMITED BY SIZE INTO SSA2                                   
138900     MOVE '  ' TO GODK-STATUSKODER                                        
139000     CALL CBLTDLI USING GU ORQI-PCB IO-AREA-ORQI11 SSA1 SSA2              
139100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
139200     PERFORM IMS-STATUSKONTROLL                                           
139300     .                                                                    
139400     EJECT                                                                
139500 IMS-GET-WLORQI12-KVAL SECTION.                                           
139600                                                                          
139700     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
139800            DELIMITED BY SIZE INTO SSA1                                   
139900     MOVE '  ' TO GODK-STATUSKODER                                        
140000     CALL CBLTDLI USING GU ORQI-PCB IO-AREA-ORQI12 SSA1                   
140100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
140200     PERFORM IMS-STATUSKONTROLL                                           
140300     .                                                                    
140400     EJECT                                                                
140500 IMS-GET-WLORDP01-KVAL SECTION.                                           
140600                                                                          
140700     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
140800            DELIMITED BY SIZE INTO SSA1                                   
140900     MOVE '  GE' TO GODK-STATUSKODER                                      
141000     CALL CBLTDLI USING GU ORDP-PCB IO-AREA-ORDP01 SSA1                   
141100     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
141200     PERFORM IMS-STATUSKONTROLL                                           
141300     .                                                                    
141400     SKIP2                                                                
141500 IMS-GET-WLORDP01-OKVAL SECTION.                                          
141600                                                                          
141700     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN-X                        
141800                    '&WDA501KY<=' W-WDA501KY-MAX-X ')'                    
141900            DELIMITED BY SIZE INTO SSA1                                   
142000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
142100     CALL CBLTDLI USING GN ORDP-PCB IO-AREA-ORDP01 SSA1                   
142200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
142300     PERFORM IMS-STATUSKONTROLL                                           
142400     .                                                                    
142500     EJECT                                                                
142600 IMS-GET-WDE601 SECTION.                                                  
142700                                                                          
142800     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
142900            DELIMITED BY SIZE INTO SSA1                                   
143000     MOVE '  GE'  TO GODK-STATUSKODER                                     
143100     CALL CBLTDLI USING GU WDE6-PCB IO-AREA-WDE601 SSA1                   
143200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
143300     PERFORM IMS-STATUSKONTROLL                                           
143400     .                                                                    
143500     EJECT                                                                
143600 IMS-GNP-WDE611        SECTION.                                           
143700                                                                          
143800     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
143900            DELIMITED BY SIZE INTO SSA1                                   
144000     MOVE '  '  TO GODK-STATUSKODER                                       
144100     CALL CBLTDLI USING GNP WDE6-PCB IO-AREA-WDE611 SSA1                  
144200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
144300     PERFORM IMS-STATUSKONTROLL                                           
144400     .                                                                    
144500     EJECT                                                                
144600 IMS-GU-WLARTC11-KVAL SECTION.                                            
144700                                                                          
144800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
144900            DELIMITED BY SIZE INTO SSA1                                   
145000     MOVE   'WLARTC11'         TO SSA2                                    
145100     MOVE '  GE' TO GODK-STATUSKODER                                      
145200     CALL CBLTDLI USING GU ARTC-PCB IO-AREA-ARTC11 SSA1 SSA2              
145300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
145400     PERFORM IMS-STATUSKONTROLL                                           
145500     .                                                                    
145600     EJECT                                                                
145700 IMS-GU-WDE401                SECTION.                                    
145800     STRING 'WDE401  (WDE4ASEQ>=' W-WDE4ASEQ-MIN-X                        
145900                    '&WDE4ASEQ<=' W-WDE4ASEQ-MAX-X                        
146000                    '&IDPRODNR =' W-IDPRODNR-X                            
146100                    '&IDPLKLST =' W-IDPLKLST-X ')'                        
146200            DELIMITED BY SIZE INTO SSA1                                   
146300     MOVE '  GE' TO GODK-STATUSKODER                                      
146400     CALL CBLTDLI USING GU WDE4-PCB IO-AREA-WDE401 SSA1                   
146500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
146600     PERFORM IMS-STATUSKONTROLL                                           
146700     .                                                                    
146800     SKIP2                                                                
146900 IMS-GNP-WDE411               SECTION.                                    
147000     MOVE 'WDE411'                     TO SSA1                            
147100     MOVE '  GE' TO GODK-STATUSKODER                                      
147200     CALL CBLTDLI USING GNP WDE4-PCB IO-AREA-WDE411 SSA1                  
147300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
147400     PERFORM IMS-STATUSKONTROLL                                           
147500     .                                                                    
147600     SKIP2                                                                
147700 IMS-GNP-WDE411-KVAL-RADNR    SECTION.                                    
147800     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
147900            DELIMITED BY SIZE INTO SSA1                                   
148000     MOVE '  GE' TO GODK-STATUSKODER                                      
148100     CALL CBLTDLI USING GNP WDE4-PCB IO-AREA-WDE411 SSA1                  
148200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
148300     PERFORM IMS-STATUSKONTROLL                                           
148400     .                                                                    
148500     SKIP3                                                                
148600 IMS-GNP-WDE411-KVAL-ARTNR    SECTION.                                    
148700     STRING 'WDE411  (IDARTNR  =' W-IDARTNR-X ')'                         
148800            DELIMITED BY SIZE INTO SSA1                                   
148900     MOVE '  GE' TO GODK-STATUSKODER                                      
149000     CALL CBLTDLI USING GNP WDE4-PCB IO-AREA-WDE411 SSA1                  
149100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
149200     PERFORM IMS-STATUSKONTROLL                                           
149300     .                                                                    
149400     EJECT                                                                
149500 IMS-GNP-WDE421               SECTION.                                    
149600     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
149700            DELIMITED BY SIZE INTO SSA1                                   
149800     MOVE   'WDE421'                   TO SSA2                            
149900     MOVE '  GE' TO GODK-STATUSKODER                                      
150000     CALL CBLTDLI USING GNP WDE4-PCB IO-AREA-WDE421 SSA1 SSA2             
150100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
150200     PERFORM IMS-STATUSKONTROLL                                           
150300     .                                                                    
150400     SKIP2                                                                
150500 IMS-GNP-WDE421-KVAL          SECTION.                                    
150600     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
150700            DELIMITED BY SIZE INTO SSA1                                   
150800     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
150900            DELIMITED BY SIZE INTO SSA2                                   
151000     MOVE '  GE' TO GODK-STATUSKODER                                      
151100     CALL CBLTDLI USING GNP WDE4-PCB IO-AREA-WDE421 SSA1 SSA2             
151200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
151300     PERFORM IMS-STATUSKONTROLL                                           
151400     .                                                                    
151500     EJECT                                                                
151600 IMS-GET-WDF106 SECTION.                                                  
151700                                                                          
151800     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
151900          DELIMITED BY SIZE INTO SSA1                                     
152000     MOVE 'WLLEVA14 ' TO SSA2                                             
152100     MOVE '  GE' TO GODK-STATUSKODER                                      
152200     CALL CBLTDLI USING GU LEVA-PCB IO-AREA-LEVA14 SSA1 SSA2              
152300     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
152400     PERFORM IMS-STATUSKONTROLL                                           
152500     .                                                                    
152600                                                                          
152700 IMS-STATUSKONTROLL SECTION.                                              
152800                                                                          
152900     SET STATUS-IX TO 1                                                   
153000     SEARCH GODK-STATUS                                                   
153100       AT END                                                             
153200         CALL FELLOG                                                      
153300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
153400         CONTINUE                                                         
153500     END-SEARCH                                                           
153600     .                                                                    
