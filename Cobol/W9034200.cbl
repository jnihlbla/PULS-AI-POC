000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9034200.                                                
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
002700*                WDF601                                                   
002800*                                                                         
002900*        OM SIDAN INTE FULL OCH DET INTE FINNS FLER SEGMENT               
003000*        PÅ WDQ4 LÄSER MAN WDE4. OM SIDAN FORTFARANDE INTE                
003100*        ÄR FULL OCH DET INTE FINNS FLER SEGMENT PÅ WDE4                  
003200*        LÄSER MAN WDA5.                                                  
003300*                                                                         
003400*        BLIR SIDAN FULL OCH DET FINNS FLER SEGMENT PÅ                    
003500*        NÅGON AV BASERNA SPARAS DET EN FLAGGA OCH ETT RAD                
003600*        NUMMER SOM TALAR OM PÅ VILKEN BAS OCH VAR I BASEN                
003700*        MAN SKALL FORTSÄTTA ATT LÄSA VID EN EVENTUELL                    
003800*        BLÄDDRING.                                                       
003900*                                                                         
004000*    INDATA.                                                              
004100*        TRANSAKTION: W90342T                                             
004200*        MID:         W9I34201                                            
004300*                                                                         
004400*    UTDATA.                                                              
004500*        MOD:         W9O34202                                            
004600     SKIP3                                                                
004700 ENVIRONMENT DIVISION.                                                    
004800     SKIP3                                                                
004900 DATA DIVISION.                                                           
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400     SKIP3                                                                
005500 77  IDPGM                       PIC X(8)    VALUE 'W9034200'.            
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
025800   03    W-IDPRODNR-F6-X.                                                 
025900     05    W-IDPRODNR-F6         PIC S9(7)   VALUE ZERO  COMP-3.          
026000     SKIP2                                                                
026100   03    W-IDKOLLI-X.                                                     
026200     05    W-IDKOLLI             PIC S9(5)   VALUE ZERO COMP-3.           
026300     SKIP2                                                                
026400   03    W-IDRADNRO-MIN-X.                                                
026500     05    W-IDRADNRO-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
026600     SKIP2                                                                
026700   03    W-IDPLKLST-X.                                                    
026800     05    W-IDPLKLST            PIC S9(3)   VALUE ZERO  COMP-3.          
026900     SKIP2                                                                
027000   03    W-IDPURAD-X.                                                     
027100     05    W-IDPURAD             PIC S9(5)   VALUE ZERO  COMP-3.          
027200     SKIP2                                                                
027300   03  W-IDLEVNR-X.                                                       
027400     05  W-IDLEVNR               PIC X(5)    VALUE SPACE.                 
027500     EJECT                                                                
027600******************************************************************        
027700*                                                                         
027800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
027900*                                                                         
028000 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
028100     SKIP3                                                                
028200*01    MID -COPY W9I34201.                                                
028300     EJECT                                                                
028400*01    -COPY WMSGAREA                                                     
028500     EJECT                                                                
028600*  03    MOD -COPY W9O34202  -RED MSG-AREA.                               
028700     EJECT                                                                
028800*01    -COPY WMFSAREA                                                     
028900     EJECT                                                                
029000******************************************************************        
029100*                                                                         
029200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
029300*                                                                         
029400 01    IMS-WS.                                                            
029500   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
029600     SKIP3                                                                
029700*                        **** STATUS-KOD FRÅN IMS                         
029800   03    STATUS-WS               PIC XX.                                  
029900     88    SEGMENT-FINNS                     VALUE '  '.                  
030000     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
030100     88    SEGMENT-SLUT                      VALUE 'GB'.                  
030200     SKIP3                                                                
030300   03    GODK-STATUSKODER.                                                
030400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
030500     SKIP3                                                                
030600 01    SSA1                      PIC X(200).                              
030700 01    SSA2                      PIC X(64).                               
030800     EJECT                                                                
030900*                            IMS FUNKTIONSKODER                           
031000*01    -COPY W0003                                                        
031100     EJECT                                                                
031200******************************************************************        
031300*                                                                         
031400*        ARBETS-AREOR TILL IO-AREORNA                                     
031500*                                                                         
031600*    ---  DLI INPUT-OUTPUT AREA 1                                         
031700*    ---  DLI-IO-AREA                                                     
031800*                                                                         
031900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI01'.            
032000 01  IO-AREA-ORQI01.                                                      
032100*  03    WLORQI01 -COPY WDQ201                                            
032200     EJECT                                                                
032300                                                                          
032400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI11'.            
032500 01  IO-AREA-ORQI11.                                                      
032600*  03    WLORQI11 -COPY WDQ211                                            
032700     EJECT                                                                
032800                                                                          
032900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI12'.            
033000 01  IO-AREA-ORQI12.                                                      
033100*  03    WLORQI12 -COPY WDQ212                                            
033200     EJECT                                                                
033300                                                                          
033400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQA01'.            
033500 01  IO-AREA-ORQA01.                                                      
033600*  03    WLORQA01 -COPY WDQ301                                            
033700     EJECT                                                                
033800                                                                          
033900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQF01'.            
034000 01  IO-AREA-ORQF01.                                                      
034100*  03    WLORQF01 -COPY WDQ401 -PRE Q4-                                   
034200     EJECT                                                                
034300                                                                          
034400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC11'.            
034500 01  IO-AREA-ARTC11.                                                      
034600*  03    WLARTC11  -COPY WDK611                                           
034700     EJECT                                                                
034800                                                                          
034900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE601'.            
035000 01  IO-AREA-WDE601.                                                      
035100*  03  -COPY WDE601                                                       
035200     EJECT                                                                
035300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE611'.            
035400 01  IO-AREA-WDE611.                                                      
035500*  03  -COPY WDE611                                                       
035600     EJECT                                                                
035700                                                                          
035800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-LEVA14'.            
035900 01  IO-AREA-LEVA14.                                                      
036000*  03    WLLEVA14  -COPY WDF106                                           
036100     EJECT                                                                
036200                                                                          
036300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORDP11'.            
036400 01  IO-AREA-ORDP01.                                                      
036500*  03    WLORDP01  -COPY WDA501                                           
036600     EJECT                                                                
036700                                                                          
036800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE401'.            
036900 01  IO-AREA-WDE401.                                                      
037000*  03    -COPY WDE401                                                     
037100     EJECT                                                                
037200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE411'.            
037300 01  IO-AREA-WDE411.                                                      
037400*  03    -COPY WDE411                                                     
037500     EJECT                                                                
037600 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE421'.            
037700 01 IO-AREA-WDE421.                                                       
037800*  03  -COPY WDE421                                                       
037900     EJECT                                                                
038000 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDF601'.            
038100 01 IO-AREA-WDF601.                                                       
038200*  03  -COPY WDF601                                                       
038300     EJECT                                                                
038400 01  FILLER                 PIC X(12) VALUE 'DUMMY-ARTC'.                 
038500 01  ETA-ARTC-PCB           PIC X.                                        
038600 01  FILLER                 PIC X(12) VALUE 'DUMMY-LEVA'.                 
038700 01  ETA-LEVA-PCB           PIC X.                                        
038800 LINKAGE SECTION.                                                         
038900                                                                          
039000*01    -COPY W0009     -PRE MSG-                                          
039100     EJECT                                                                
039200*01    -COPY W0008     -PRE ORQI-                                         
039300     05  FILLER                  PIC X.                                   
039400     EJECT                                                                
039500*01    -COPY W0008     -PRE ORQA-                                         
039600     05  FILLER                  PIC X.                                   
039700     EJECT                                                                
039800*01    -COPY W0008     -PRE ORQF-                                         
039900     05  FILLER                  PIC X.                                   
040000     EJECT                                                                
040100*01    -COPY W0008     -PRE ORDP-                                         
040200     05  FILLER                  PIC X.                                   
040300     EJECT                                                                
040400*01    -COPY W0008     -PRE WDE6-                                         
040500     05  FILLER                  PIC X.                                   
040600     EJECT                                                                
040700*01    -COPY W0008     -PRE ARTC-                                         
040800     05  FILLER                  PIC X.                                   
040900     EJECT                                                                
041000*01    -COPY W0008     -PRE WDE4-                                         
041100     05  FILLER                  PIC X.                                   
041200     EJECT                                                                
041300*01    -COPY W0008     -PRE LEVA-                                         
041400     05  FILLER                  PIC X.                                   
041500     EJECT                                                                
041600*01    -COPY W0008     -PRE WDF6-                                         
041700     05  FILLER                  PIC X.                                   
041800     EJECT                                                                
041900 01  ETA-WDK7-PCB           PIC X.                                        
042000 01  ETA-INLC-PCB           PIC X.                                        
042100 01  ETA-WDB6-PCB           PIC X.                                        
042200 01  ETA-WDD9-PCB           PIC X.                                        
042300     EJECT                                                                
042400 PROCEDURE DIVISION  USING MSG-PCB                                        
042500                           ORQI-PCB     ORQA-PCB                          
042600                           ORQF-PCB     ORDP-PCB                          
042700                           WDE6-PCB     ARTC-PCB      WDE4-PCB            
042800                           LEVA-PCB     WDF6-PCB                          
042900                           ETA-WDK7-PCB ETA-INLC-PCB ETA-WDB6-PCB         
043000                           ETA-WDD9-PCB.                                  
043100     ENTRY 'DLITCBL' USING MSG-PCB                                        
043200                           ORQI-PCB     ORQA-PCB                          
043300                           ORQF-PCB     ORDP-PCB                          
043400                           WDE6-PCB     ARTC-PCB      WDE4-PCB            
043500                           LEVA-PCB WDF6-PCB                              
043600                           ETA-WDK7-PCB ETA-INLC-PCB ETA-WDB6-PCB         
043700                           ETA-WDD9-PCB.                                  
043800     PERFORM IMS-GET-MSG                                                  
043900     IF SEGMENT-FINNS                                                     
044000       PERFORM A-INIT                                                     
044100       PERFORM B-KONTROLLERA-NYCKLAR                                      
044200       IF NYCKLAR-OK                                                      
044300         PERFORM C-BEHANDLA-RADER                                         
044400       END-IF                                                             
044500     END-IF                                                               
044600     PERFORM E-KONTROLLERA-OM-SIDA-TOM                                    
044700     PERFORM F-BERAKNA-MAX-MOD-LANGD                                      
044800     MOVE    MAX-MOD-LANGD TO MSG-KVLL                                    
044900     PERFORM IMS-INSERT-MSG                                               
045000     MOVE    ZERO          TO RETURN-CODE                                 
045100     GOBACK                                                               
045200     .                                                                    
045300     EJECT                                                                
045400 A-INIT SECTION.                                                          
045500                                                                          
045600     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I34201                    
045700     MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                     
045800     MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                    
045900     MOVE ' '                          TO MFS-KDTRTYP                     
046000     MOVE LOW-VALUE                    TO MSG-AREA                        
046100                                                                          
046200     MOVE MID-IDRADNR-NEXT             TO W-IDRADNR                       
046300     MOVE +1                           TO IX-RAD                          
046400                                                                          
046500     MOVE '9342'                       TO MOD2-IDTRANS                    
046600     MOVE 'W9O34202'                   TO MFS-IDMOD                       
046700     MOVE ZERO                         TO MOD2-IDMFSFEL                   
046800     MOVE MID-IDORDNR7                 TO MOD2-IDORDNR7                   
046900                                        WS-IDKUNDRF-CONT                  
047000     MOVE ZERO                         TO MOD2-IDARTNR                    
047100                                        MOD2-IDRADNR-NEXT                 
047200                                        MOD2-IDPRODNR-NEXT                
047300                                        MOD2-IDPLKLST-NEXT                
047400                                        MOD2-IDKOLLI-NEXT                 
047500     MOVE SPACE                        TO MOD2-FLSVAR-NEXT                
047600                                        MOD2-IDDC-NEXT                    
047700     PERFORM UNTIL IX-RAD      >   MAX-RAD                                
047800       MOVE ZERO               TO  MOD2-IDARTNR-RAD(IX-RAD)               
047900                                 MOD2-KVBEART(IX-RAD)                     
048000                                 MOD2-KVAVBART(IX-RAD)                    
048100                                 MOD2-KVLEVART(IX-RAD)                    
048200                                 MOD2-TIRODAT(IX-RAD)                     
048300                                 MOD2-IDKOLLI (IX-RAD)                    
048400                                 MOD2-IDORDNR7-RO (IX-RAD)                
048500                                                                          
048600       MOVE SPACE              TO  MOD2-KDORDSTA(IX-RAD)                  
048700                                 MOD2-KDTPOTYP(IX-RAD)                    
048800                                 MOD2-IDRADINF(IX-RAD)                    
048900                                 MOD2-IDDC(IX-RAD)                        
049000                                 MOD2-BERADREF(IX-RAD)                    
049100       ADD +1                  TO  IX-RAD                                 
049200     END-PERFORM                                                          
049300     .                                                                    
049400     EJECT                                                                
049500 B-KONTROLLERA-NYCKLAR SECTION.                                           
049600                                                                          
049700     IF MID-IDDISTR  NUMERIC AND                                          
049800        MID-IDKUNDNR NUMERIC                                              
049900       MOVE MID-IDDISTR     TO TEST-IDDISTR                               
050000     ELSE                                                                 
050100       MOVE NEJ              TO NYCKLAR-SW                                
050200       MOVE ERR-WRONG-KEY    TO MOD2-IDMFSFEL                             
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600 C-BEHANDLA-RADER SECTION.                                                
050700                                                                          
050800     MOVE +1                   TO IX-RAD                                  
050900                                                                          
051000     EVALUATE TRUE                                                        
051100     WHEN MID-FLSVAR-NEXT      =  'O' OR 'P' OR SPACE                     
051200         PERFORM CA-BEHANDLA-ORDERKOE                                     
051300         IF IX-RAD             >  MAX-RAD AND SEGMENT-FINNS               
051400             CONTINUE                                                     
051500          ELSE                                                            
051600             PERFORM CB-BEHANDLA-RESTORDER                                
051700         END-IF                                                           
051800                                                                          
051900                                                                          
052000     WHEN MID-FLSVAR-NEXT      =  'R'                                     
052100         PERFORM CB-BEHANDLA-RESTORDER                                    
052200                                                                          
052300     END-EVALUATE                                                         
052400     .                                                                    
052500     EJECT                                                                
052600 CA-BEHANDLA-ORDERKOE SECTION.                                            
052700                                                                          
052800     MOVE MID-IDDISTR          TO  W-Q2CSEQ-IDDISTR                       
052900     MOVE MID-IDKUNDNR         TO  W-Q2CSEQ-IDKUNDNR                      
053000     MOVE MID-IDORDNR7         TO  W-Q2CSEQ-IDKUNDRF                      
053100                                                                          
053200     PERFORM IMS-GET-WLORQI01-KVAL                                        
053300                                                                          
053400     IF SEGMENT-FINNS AND OHUV-FLBORT = NEJ                               
053500                                                                          
053600        IF MID-FLSVAR-NEXT  =  'P' OR SPACE                               
053700            PERFORM CAA-BEHANDLA-ORDERDEL                                 
053800                                                                          
053900            IF IX-RAD  >  MAX-RAD AND SEGMENT-FINNS                       
054000                CONTINUE                                                  
054100            ELSE                                                          
054200                PERFORM CAB-BEHANDLA-ORDERRADKOE                          
054300            END-IF                                                        
054400        ELSE                                                              
054500            PERFORM CAB-BEHANDLA-ORDERRADKOE                              
054600        END-IF                                                            
054700     END-IF                                                               
054800     .                                                                    
054900     EJECT                                                                
055000 CAA-BEHANDLA-ORDERDEL SECTION.                                           
055100                                                                          
055200     MOVE LOW-VALUE            TO  W-WDQ301KY-MIN-X                       
055300     MOVE OHUV-IDORDER         TO  W-Q301KY-MIN-IDORDER                   
055400     IF MID-IDPRODNR-NEXT      >   ZERO                                   
055500       MOVE MID-IDPRODNR-NEXT  TO  W-Q301KY-MIN-IDPRODNR                  
055600       MOVE MID-IDPLKLST-NEXT  TO  W-Q301KY-MIN-IDPLKLST                  
055700       MOVE MID-IDDC-NEXT      TO  W-Q301KY-MIN-IDDC                      
055800     END-IF                                                               
055900                                                                          
056000     MOVE HIGH-VALUE           TO  W-WDQ301KY-MAX-X                       
056100     MOVE OHUV-IDORDER         TO  W-Q301KY-MAX-IDORDER                   
056200                                                                          
056300     PERFORM IMS-GET-WLORQA01-OKVAL-GU                                    
056400     PERFORM UNTIL SEGMENT-SAKNAS             OR                          
056500                   SEGMENT-SLUT               OR                          
056600                   IX-RAD      > MAX-RAD                                  
056700         IF ODEL-KDODELSTA NOT = 'R'                                      
056800           PERFORM CAAA-BEHANDLA-WDE411                                   
056900         END-IF                                                           
057000         IF IX-RAD             > MAX-RAD                                  
057100             CONTINUE                                                     
057200          ELSE                                                            
057300             PERFORM IMS-GET-WLORQA01-OKVAL-GN                            
057400         END-IF                                                           
057500     END-PERFORM                                                          
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 CAAA-BEHANDLA-WDE411 SECTION.                                            
058000                                                                          
058100     MOVE LOW-VALUE                      TO W-WDE4ASEQ-MIN-X              
058200     MOVE MID-IDDISTR                    TO W-IDDISTR-MIN                 
058300     MOVE MID-IDKUNDNR                   TO W-IDKUNDNR-MIN                
058400     MOVE SPACE                          TO W-IDKUNDRF-MIN                
058500     MOVE MID-IDORDNR7                   TO WS-IDORDNR-NUM                
058600     MOVE WS-IDORDNR-NUM                 TO W-IDKUNDRF-MIN                
058700     MOVE ODEL-IDLEVNR                   TO W-IDLEVNR                     
058800                                            W-Q211KY-IDLEVNR              
058900     MOVE ODEL-IDDC                      TO W-Q211KY-IDDC                 
059000                                            WS-IDDC                       
059100     MOVE ODEL-IDPRODNR                  TO W-IDPRODNR                    
059200                                         IN W-IDPRODNR-X                  
059300     MOVE ODEL-IDPLKLST                  TO W-IDPLKLST                    
059400     MOVE HIGH-VALUE                     TO W-WDE4ASEQ-MAX-X              
059500     MOVE MID-IDDISTR                    TO W-IDDISTR-MAX                 
059600     MOVE MID-IDKUNDNR                   TO W-IDKUNDNR-MAX                
059700     MOVE SPACE                          TO W-IDKUNDRF-MAX                
059800     MOVE MID-IDORDNR7                   TO WS-IDORDNR-NUM                
059900     MOVE WS-IDORDNR-NUM                 TO W-IDKUNDRF-MAX                
060000                                                                          
060100     PERFORM IMS-GU-WDE401                                                
060200     IF SEGMENT-FINNS                                                     
060300       MOVE KORD-IDPLKLST        TO WS-IDPLKLST                           
060400       MOVE KORD-IDPRODNR        TO WS-IDPRODNR                           
060500       IF MID-IDRADNR-NEXT > ZERO AND IX-RAD = +1                         
060600         MOVE MID-IDRADNR-NEXT        TO W-IDPURAD                        
060700         PERFORM IMS-GNP-WDE411-KVAL-RADNR                                
060800       ELSE                                                               
060900         EVALUATE TRUE                                                    
061000         WHEN MID-IDARTNR > ZERO                                          
061100           MOVE MID-IDARTNR           TO W-IDARTNR                        
061200           PERFORM IMS-GNP-WDE411-KVAL-ARTNR                              
061300          WHEN OTHER                                                      
061400           PERFORM IMS-GNP-WDE411                                         
061500         END-EVALUATE                                                     
061600       END-IF                                                             
061700       PERFORM UNTIL SEGMENT-SAKNAS OR IX-RAD > MAX-RAD                   
061800         PERFORM S01-FLYTTA-RAD-TILL-MOD                                  
061900         PERFORM S02-LAS-WDE421                                           
062000         PERFORM D-LAS-KOLLIN                                             
062100       END-PERFORM                                                        
062200     END-IF                                                               
062300     .                                                                    
062400     EJECT                                                                
062500                                                                          
062600 CAB-BEHANDLA-ORDERRADKOE SECTION.                                        
062700                                                                          
062800     MOVE LOW-VALUE            TO  W-WDQ4ASEQ-MIN-X                       
062900     MOVE HIGH-VALUE           TO  W-WDQ4ASEQ-MAX-X                       
063000     MOVE OHUV-IDORDER         TO  W-Q4ASEQ-MIN-IDORDER                   
063100                                   W-Q4ASEQ-MAX-IDORDER                   
063200                                   W-Q4ASEQ-IDORDER                       
063300                                                                          
063400     IF MID-IDRADNR-NEXT       >   ZERO AND MID-FLSVAR-NEXT = 'O'         
063500         MOVE MID-IDARTNR      TO  W-Q4ASEQ-IDARTNR                       
063600         MOVE W-IDRADNR-2-4    TO  W-Q4ASEQ-IDLOPNR                       
063700         PERFORM IMS-GET-WLORQF01-KVAL                                    
063800     ELSE                                                                 
063900       IF MID-IDARTNR >   ZERO                                            
064000         MOVE MID-IDARTNR      TO  W-Q4ASEQ-MIN-IDARTNR                   
064100                                   W-Q4ASEQ-MAX-IDARTNR                   
064200       END-IF                                                             
064300       PERFORM IMS-GET-WLORQF01-OKVAL                                     
064400     END-IF                                                               
064500                                                                          
064600     PERFORM UNTIL SEGMENT-SAKNAS    OR                                   
064700                   SEGMENT-SLUT      OR                                   
064800                   IX-RAD      > MAX-RAD                                  
064900                                                                          
065000         PERFORM CABA-REDIGERA-ORDERRAD                                   
065100         ADD +1            TO IX-RAD                                      
065200                                                                          
065300         PERFORM IMS-GET-WLORQF01-OKVAL                                   
065400                                                                          
065500     END-PERFORM                                                          
065600                                                                          
065700     IF IX-RAD                 > MAX-RAD     AND                          
065800        SEGMENT-FINNS                                                     
065900         PERFORM CABB-SPARA-ORDERRAD-NYCKLAR                              
066000     END-IF                                                               
066100     .                                                                    
066200     EJECT                                                                
066300 CABA-REDIGERA-ORDERRAD SECTION.                                          
066400                                                                          
066500     IF OHUV-FLKLAR            = NEJ                                      
066600         MOVE ZERO             TO MOD2-KVBEART(IX-RAD)                    
066700         MOVE ZERO             TO MOD2-KVAVBART(IX-RAD)                   
066800         MOVE ZERO             TO MOD2-TIRODAT(IX-RAD)                    
066900         MOVE Q4-ORAD-IDDC     TO MOD2-IDDC(IX-RAD)                       
067000         MOVE 'E '             TO MOD2-KDORDSTA(IX-RAD)                   
067100     ELSE                                                                 
067200         MOVE Q4-ORAD-KVBEART-Q TO MOD2-KVBEART(IX-RAD)                   
067300         MOVE Q4-ORAD-KVPREAVB TO MOD2-KVAVBART(IX-RAD)                   
067400         MOVE Q4-ORAD-IDDC     TO MOD2-IDDC(IX-RAD)                       
067500                                  WS-IDDC                                 
067600         IF GOOD-DDC                                                      
067700           MOVE WC-CDC-SE      TO W-IDDC                                  
067800         ELSE                                                             
067900           MOVE Q4-ORAD-IDDC   TO W-IDDC                                  
068000         END-IF                                                           
068100                                                                          
068200         PERFORM IMS-GET-WLORQI12-KVAL                                    
068300         MOVE ARB-DATRPAVD(3:6) TO MOD2-TIRODAT(IX-RAD)                   
068400                                                                          
068500         MOVE 'R '             TO MOD2-KDORDSTA(IX-RAD)                   
068600     END-IF                                                               
068700     MOVE Q4-ORAD-IDARTNR      TO MOD2-IDARTNR-RAD(IX-RAD)                
068800     MOVE Q4-ORAD-IDBIL        TO MOD2-IDBIL(IX-RAD)                      
068900     IF Q4-ORAD-KDTPOTYP       >  ZERO                                    
069000         MOVE Q4-ORAD-KDTPOTYP TO W-KDTPOTYP                              
069100         MOVE W-KDTPOTYP       TO MOD2-KDTPOTYP(IX-RAD)                   
069200      ELSE                                                                
069300         MOVE SPACE            TO MOD2-KDTPOTYP(IX-RAD)                   
069400     END-IF                                                               
069500     MOVE Q4-ORAD-IDKUNDRF-RO(1:7)                                        
069600                               TO MOD2-IDORDNR7-RO(IX-RAD)                
069700     MOVE Q4-ORAD-BERADREF     TO MOD2-BERADREF(IX-RAD)                   
069800     .                                                                    
069900     EJECT                                                                
070000 CABB-SPARA-ORDERRAD-NYCKLAR SECTION.                                     
070100                                                                          
070200     MOVE ZERO                 TO  W-IDRADNR-1                            
070300     MOVE Q4-ORAD-IDLOPNR      TO  W-IDRADNR-2-4                          
070400     MOVE Q4-ORAD-IDARTNR      TO  WS-IDARTNR-NUM                         
070500     MOVE 'O'                  TO  MOD2-FLSVAR-NEXT                       
070600     MOVE WS-IDARTNR-NUM       TO  MOD2-IDARTNR                           
070700     MOVE W-IDRADNR            TO  MOD2-IDRADNR-NEXT                      
070800     .                                                                    
070900     EJECT                                                                
071000 CB-BEHANDLA-RESTORDER SECTION.                                           
071100                                                                          
071200     MOVE LOW-VALUE            TO  W-WDA501KY-MIN-X                       
071300     MOVE HIGH-VALUE           TO  W-WDA501KY-MAX-X                       
071400     MOVE MID-IDDISTR          TO  W-A501KY-MIN-IDDISTR                   
071500                                   W-A501KY-MAX-IDDISTR                   
071600                                   W-A501KY-IDDISTR                       
071700     MOVE MID-IDKUNDNR         TO  W-A501KY-MIN-IDKUNDNR                  
071800                                   W-A501KY-MAX-IDKUNDNR                  
071900                                   W-A501KY-IDKUNDNR                      
072000     MOVE SPACE                TO  W-A501KY-MIN-IDKUNDRF                  
072100                                   W-A501KY-MAX-IDKUNDRF                  
072200                                   W-A501KY-IDKUNDRF                      
072300     MOVE MID-IDORDNR7(3:5)    TO  W-A501KY-MIN-IDORDNR5                  
072400                                   W-A501KY-MAX-IDORDNR5                  
072500                                   W-A501KY-IDORDNR5                      
072600                                                                          
072700     IF MID-IDRADNR-NEXT       >   ZERO AND MID-FLSVAR-NEXT = 'R'         
072800       MOVE MID-IDARTNR        TO  W-A501KY-IDARTNR                       
072900       MOVE W-IDRADNR-2-4      TO  W-A501KY-IDLOPNR                       
073000       PERFORM IMS-GET-WLORDP01-KVAL                                      
073100     ELSE                                                                 
073200       IF MID-IDARTNR > ZERO                                              
073300         MOVE MID-IDARTNR      TO  W-A501KY-MIN-IDARTNR                   
073400                                   W-A501KY-MAX-IDARTNR                   
073500       END-IF                                                             
073600       PERFORM IMS-GET-WLORDP01-OKVAL                                     
073700     END-IF                                                               
073800                                                                          
073900     PERFORM UNTIL SEGMENT-SAKNAS    OR                                   
074000                   SEGMENT-SLUT      OR                                   
074100                   IX-RAD      > MAX-RAD                                  
074200                                                                          
074300         MOVE RAD-IDARTNR      TO W-IDARTNR                               
074400                               IN W-IDARTNR-X                             
074500                                                                          
074600         PERFORM IMS-GU-WLARTC11-KVAL                                     
074700         PERFORM CBA-REDIGERA-RO-RAD                                      
074800         ADD +1                TO IX-RAD                                  
074900                                                                          
075000         PERFORM IMS-GET-WLORDP01-OKVAL                                   
075100     END-PERFORM                                                          
075200                                                                          
075300     IF IX-RAD                 > MAX-RAD   AND                            
075400        SEGMENT-FINNS                                                     
075500           PERFORM CBB-SPARA-RESTORDER-NYCKLAR                            
075600     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900 CBA-REDIGERA-RO-RAD SECTION.                                             
076000                                                                          
076100     MOVE RAD-IDARTNR             TO MOD2-IDARTNR-RAD(IX-RAD)             
076200     MOVE RAD-BERADREF            TO MOD2-BERADREF(IX-RAD)                
076300     MOVE SPACE                   TO MOD2-IDRADINF(IX-RAD)                
076400     MOVE RAD-KVBEART-Q           TO MOD2-KVBEART(IX-RAD)                 
076500     MOVE RAD-IDDC                TO MOD2-IDDC(IX-RAD)                    
076600     MOVE RAD-KDTPOTYP            TO MOD2-KDTPOTYP(IX-RAD)                
076700     IF RAD-DARODAT = ZERO                                                
076800       MOVE RAD-TITPO             TO MOD2-TIRODAT(IX-RAD)                 
076900       MOVE RAD-KVART             TO MOD2-KVAVBART(IX-RAD)                
077000       IF RAD-KDSTARAD = '1' OR '3'                                       
077100         MOVE 'T '                TO MOD2-KDORDSTA(IX-RAD)                
077200       ELSE                                                               
077300         IF RAD-KDSTARAD = '4'                                            
077400           MOVE 'RT'              TO MOD2-KDORDSTA(IX-RAD)                
077500         END-IF                                                           
077600       END-IF                                                             
077700     ELSE                                                                 
077800       MOVE RAD-IDDC              TO WS-IDDC                              
078000       IF NDC                                                             
078100         PERFORM CBAA-HAEMTA-TIBERANK                                     
078200                                                                          
078300         IF ETA-SVAR-OK = JA                                              
078500           IF NDC-NA OR NDC-CN                                            
078600             MOVE ETA-TIAAMMDD-SVAR TO MOD2-TIRODAT(IX-RAD)               
078700           ELSE                                                           
078800             IF ETA-KVAVIS-ETA > +0                                       
078900               MOVE ETA-TIAAMMDD-SVAR TO MOD2-TIRODAT(IX-RAD)             
079000             ELSE                                                         
079100               MOVE ZERO          TO MOD2-TIRODAT(IX-RAD)                 
079200             END-IF                                                       
079300           END-IF                                                         
079400         ELSE                                                             
079500           MOVE ZERO              TO MOD2-TIRODAT(IX-RAD)                 
079600         END-IF                                                           
079700       ELSE                                                               
079800         MOVE CLAG-TIDISPIN       TO MOD2-TIRODAT(IX-RAD)                 
079900       END-IF                                                             
080000       MOVE RAD-KVRO              TO MOD2-KVAVBART(IX-RAD)                
080100       IF RAD-KDSTARAD = '2' OR '3'                                       
080200         MOVE 'B '                TO MOD2-KDORDSTA(IX-RAD)                
080300       ELSE                                                               
080400         IF RAD-KDSTARAD = '4'                                            
080500           MOVE 'RB'              TO MOD2-KDORDSTA(IX-RAD)                
080600         END-IF                                                           
080700       END-IF                                                             
080800     END-IF                                                               
080900     IF RAD-KDSTARAD = '4'                                                
081000       IF RAD-IDKUNDRF-LEV NOT = '00000     '                             
081100        IF RAD-IDKUNDRF-LEV NOT = WS-IDKUNDRF-CONT                        
081200          MOVE RAD-IDKUNDRF-LEV(1:5) TO MOD2-IDORDNR7-RO(IX-RAD)          
081300        END-IF                                                            
081400       END-IF                                                             
081500     END-IF                                                               
081600     .                                                                    
081700     EJECT                                                                
081800                                                                          
081900 CBAA-HAEMTA-TIBERANK SECTION.                                            
082000                                                                          
082100     MOVE '612'                TO ETA-KDCALL                              
082200     MOVE RAD-IDDC             TO ETA-IDDC-REC                            
082300     MOVE RAD-IDARTNR          TO ETA-IDARTNR                             
082400     MOVE SPACE                TO ETA-IDLEVNR                             
082500     MOVE ZERO                 TO ETA-KDFRAKT                             
082600     MOVE RAD-DARODAT(3:6)     TO ETA-TIAAMMDD-ANROP                      
082700                                  WS-ETA-DATUM                            
082800     IF WS-ETA-DATUM-AAR > 50                                             
082900        MOVE 19                TO ETA-TISEKEL-ANROP                       
083000     ELSE                                                                 
083100        MOVE 20                TO ETA-TISEKEL-ANROP                       
083200     END-IF                                                               
083300                                                                          
083400     CALL W218ETA  USING ETA-W218LETA                                     
083500                         ETA-ARTC-PCB ETA-WDK7-PCB                        
083600                         ETA-INLC-PCB ETA-LEVA-PCB                        
083700                         ETA-WDB6-PCB ETA-WDD9-PCB                        
083800     .                                                                    
083900                                                                          
084000     EJECT                                                                
084100 CBB-SPARA-RESTORDER-NYCKLAR SECTION.                                     
084200                                                                          
084300     MOVE 'R'                  TO  MOD2-FLSVAR-NEXT                       
084400     MOVE ZERO                 TO  W-IDRADNR-1                            
084500     MOVE RAD-IDARTNR          TO  WS-IDARTNR-NUM                         
084600     MOVE WS-IDARTNR-NUM       TO  MOD2-IDARTNR                           
084700     MOVE RAD-IDLOPNR          TO  W-IDRADNR-2-4                          
084800     MOVE W-IDRADNR            TO  MOD2-IDRADNR-NEXT                      
084900     .                                                                    
085000     EJECT                                                                
085100                                                                          
085200 D-LAS-KOLLIN            SECTION.                                         
085300                                                                          
085400                                                                          
085500     PERFORM UNTIL                                                        
085600      SEGMENT-SAKNAS OR IX-RAD > MAX-RAD                                  
085700                                                                          
085800       MOVE KKOLLI-KVLEVART         TO MOD2-KVLEVART   (IX-RAD)           
085900       MOVE KKOLLI-IDKOLLI          TO MOD2-IDKOLLI    (IX-RAD)           
086000       MOVE SPACE                   TO MOD2-KDTPOTYP   (IX-RAD)           
086100                                                                          
086200       IF KOLLI-KDKOLSTA = +0                                             
086300         MOVE 'U'                   TO MOD2-KDORDSTA   (IX-RAD)           
086400       ELSE                                                               
086500         IF KOLLI-KDKOLSTA = +1                                           
086600           MOVE 'P'                 TO MOD2-KDORDSTA   (IX-RAD)           
086700         ELSE                                                             
086800           IF KOLLI-KDKOLSTA = +2 OR +3                                   
086900             MOVE 'L'               TO MOD2-KDORDSTA   (IX-RAD)           
087000           ELSE                                                           
087100             IF KOLLI-KDKOLSTA = +4                                       
087200               MOVE 'LF'            TO MOD2-KDORDSTA   (IX-RAD)           
087300             ELSE                                                         
087400               IF KOLLI-KDKOLSTA = +7                                     
087500                 MOVE 'S'            TO MOD2-KDORDSTA(IX-RAD)             
087600               ELSE                                                       
087700                 IF KOLLI-KDKOLSTA = +9                                   
087800                   MOVE 'SF'           TO MOD2-KDORDSTA(IX-RAD)           
087900                   MOVE KOLLI-TIFAKT   TO MOD2-TIRODAT(IX-RAD)            
088000                 END-IF                                                   
088100               END-IF                                                     
088200             END-IF                                                       
088300           END-IF                                                         
088400         END-IF                                                           
088500       END-IF                                                             
088600                                                                          
088700       PERFORM IMS-GNP-WDE421                                             
088800       ADD +1                        TO IX-RAD                            
088900       IF IX-RAD > MAX-RAD                                                
089000         CONTINUE                                                         
089100       ELSE                                                               
089200         IF SEGMENT-FINNS                                                 
089300           MOVE ORAD-IDARTNR         TO MOD2-IDARTNR-RAD(IX-RAD)          
089400           MOVE KKOLLI-IDKOLLI       TO W-IDKOLLI                         
089500           PERFORM IMS-GNP-WDE611                                         
089600         END-IF                                                           
089700       END-IF                                                             
089800                                                                          
089900     END-PERFORM                                                          
090000     IF SEGMENT-FINNS                                                     
090100       MOVE 'P'                      TO MOD2-FLSVAR-NEXT                  
090200       MOVE W-IDPURAD                TO WS-IDPURAD-NUM                    
090300       MOVE WS-IDPURAD-NUM           TO MOD2-IDRADNR-NEXT                 
090400       MOVE KKOLLI-IDPRODNR          TO MOD2-IDPRODNR-NEXT                
090500       MOVE WS-IDPLKLST              TO MOD2-IDPLKLST-NEXT                
090600       MOVE KKOLLI-IDKOLLI           TO MOD2-IDKOLLI-NEXT                 
090700       MOVE ODEL-IDDC                TO MOD2-IDDC-NEXT                    
090800     ELSE                                                                 
090900       PERFORM DA-LAS-NASTA-RAD                                           
091000     END-IF                                                               
091100     .                                                                    
091200     EJECT                                                                
091300 DA-LAS-NASTA-RAD      SECTION.                                           
091400                                                                          
091500     IF MID-IDARTNR > ZERO                                                
091600       MOVE MID-IDARTNR              TO W-IDARTNR                         
091700       PERFORM IMS-GNP-WDE411-KVAL-ARTNR                                  
091800     ELSE                                                                 
091900       PERFORM IMS-GNP-WDE411                                             
092000     END-IF                                                               
092100     IF SEGMENT-FINNS                                                     
092200       IF IX-RAD > MAX-RAD                                                
092300         MOVE 'P'                    TO MOD2-FLSVAR-NEXT                  
092400         MOVE ORAD-IDPURAD           TO WS-IDPURAD-NUM                    
092500         MOVE WS-IDPURAD-NUM         TO MOD2-IDRADNR-NEXT                 
092600         MOVE WS-IDPRODNR            TO MOD2-IDPRODNR-NEXT                
092700         MOVE WS-IDPLKLST            TO MOD2-IDPLKLST-NEXT                
092800         MOVE ODEL-IDDC              TO MOD2-IDDC-NEXT                    
092900       END-IF                                                             
093000       MOVE ORAD-IDPURAD             TO W-IDPURAD                         
093100     ELSE                                                                 
093200       IF IX-RAD > MAX-RAD                                                
093300         PERFORM DAA-LAS-NASTA-ORDERDEL                                   
093400       END-IF                                                             
093500     END-IF                                                               
093600     .                                                                    
093700     EJECT                                                                
093800 DAA-LAS-NASTA-ORDERDEL SECTION.                                          
093900                                                                          
094000     PERFORM IMS-GET-WLORQA01-OKVAL-GN                                    
094100     IF SEGMENT-FINNS                                                     
094200       IF ODEL-KDODELSTA NOT = 'R'                                        
094300         MOVE LOW-VALUE           TO W-WDE4ASEQ-MIN-X                     
094400         MOVE MID-IDDISTR         TO W-IDDISTR-MIN                        
094500         MOVE MID-IDKUNDNR        TO W-IDKUNDNR-MIN                       
094600         MOVE SPACE               TO W-IDKUNDRF-MIN                       
094700         MOVE MID-IDORDNR7        TO WS-IDORDNR-NUM                       
094800         MOVE WS-IDORDNR-NUM      TO W-IDKUNDRF-MIN                       
094900         MOVE ODEL-IDLEVNR        TO W-IDLEVNR                            
095000                                     W-Q211KY-IDLEVNR                     
095100         MOVE ODEL-IDDC           TO W-Q211KY-IDDC                        
095200                                     WS-IDDC                              
095300         MOVE ODEL-IDPRODNR       TO W-IDPRODNR                           
095400                                  IN W-IDPRODNR-X                         
095500         MOVE ODEL-IDPLKLST       TO W-IDPLKLST                           
095600         MOVE HIGH-VALUE          TO W-WDE4ASEQ-MAX-X                     
095700         MOVE MID-IDDISTR         TO W-IDDISTR-MAX                        
095800         MOVE MID-IDKUNDNR        TO W-IDKUNDNR-MAX                       
095900         MOVE SPACE               TO W-IDKUNDRF-MAX                       
096000         MOVE MID-IDORDNR7        TO WS-IDORDNR-NUM                       
096100         MOVE WS-IDORDNR-NUM      TO W-IDKUNDRF-MAX                       
096200                                                                          
096300         PERFORM IMS-GU-WDE401                                            
096400         IF SEGMENT-FINNS                                                 
096500           MOVE KORD-IDPRODNR    TO WS-IDPRODNR                           
096600           MOVE KORD-IDPLKLST    TO WS-IDPLKLST                           
096700           EVALUATE TRUE                                                  
096800             WHEN MID-IDARTNR > ZERO                                      
096900               MOVE MID-IDARTNR       TO W-IDARTNR                        
097000               PERFORM IMS-GNP-WDE411-KVAL-ARTNR                          
097100             WHEN OTHER                                                   
097200               PERFORM IMS-GNP-WDE411                                     
097300           END-EVALUATE                                                   
097400           IF SEGMENT-FINNS                                               
097500             MOVE ORAD-IDPURAD        TO WS-IDPURAD-NUM                   
097600             MOVE 'P'                 TO MOD2-FLSVAR-NEXT                 
097700             MOVE WS-IDPURAD-NUM      TO MOD2-IDRADNR-NEXT                
097800             MOVE WS-IDPRODNR         TO MOD2-IDPRODNR-NEXT               
097900             MOVE WS-IDPLKLST         TO MOD2-IDPLKLST-NEXT               
098000             MOVE ODEL-IDDC           TO MOD2-IDDC-NEXT                   
098100           END-IF                                                         
098200         END-IF                                                           
098300       END-IF                                                             
098400     END-IF                                                               
098500     .                                                                    
098600     EJECT                                                                
098700                                                                          
098800 E-KONTROLLERA-OM-SIDA-TOM SECTION.                                       
098900                                                                          
099000     IF IX-RAD                 = 1                                        
099100       IF MID-IDARTNR > ZERO                                              
099200         MOVE ERR-PART-MISSING    TO MOD2-IDMFSFEL                        
099300       ELSE                                                               
099400         MOVE ERR-ORDER-NOT-FOUND TO MOD2-IDMFSFEL                        
099500       END-IF                                                             
099600     END-IF                                                               
099700     .                                                                    
099800     EJECT                                                                
099900 F-BERAKNA-MAX-MOD-LANGD SECTION.                                         
100000                                                                          
100100     COMPUTE MAX-MOD-LANGD = LENGTH OF MOD2-W9O34202 + 4                  
100200     MOVE 11                   TO  IX-RAD                                 
100300     PERFORM UNTIL IX-RAD      = 0                                        
100400         IF MOD2-IDARTNR-RAD(IX-RAD) = ZERO                               
100500             SUBTRACT +72      FROM MAX-MOD-LANGD                         
100600             SUBTRACT +1       FROM IX-RAD                                
100700         ELSE                                                             
100800             MOVE ZERO         TO IX-RAD                                  
100900         END-IF                                                           
101000     END-PERFORM                                                          
101100     .                                                                    
101200     EJECT                                                                
101300                                                                          
101400 S01-FLYTTA-RAD-TILL-MOD   SECTION.                                       
101500                                                                          
101600     MOVE ORAD-IDARTNR            TO MOD2-IDARTNR-RAD(IX-RAD)             
101700     MOVE ORAD-BERADREF           TO MOD2-BERADREF(IX-RAD)                
101800                                                                          
101900     IF GOOD-DDC                                                          
102000       PERFORM IMS-GET-WLORQI11-KVAL                                      
102100       MOVE DIRL-KVDAGAR-DIFF      TO WS-KVDAGAR-DIFF                     
102200       MOVE WS-KVDAGAR-DIFF(2:2)   TO MOD2-KVDAGAR-DIFF (IX-RAD)          
102300       IF DIRL-KVDAGAR-DIFF < ZERO                                        
102400         MOVE '-'                  TO MOD2-IDTECKEN  (IX-RAD)             
102500       ELSE                                                               
102600         MOVE '+'                  TO MOD2-IDTECKEN  (IX-RAD)             
102700       END-IF                                                             
102800       PERFORM IMS-GET-WDF106                                             
102900       IF SEGMENT-FINNS                                                   
103000         MOVE ADR-BELEV            TO MOD2-IDLEVNMN  (IX-RAD)             
103100       ELSE                                                               
103200         MOVE SPACE                TO MOD2-IDLEVNMN  (IX-RAD)             
103300       END-IF                                                             
103400     ELSE                                                                 
103500       MOVE ORAD-IDBIL             TO MOD2-IDBIL     (IX-RAD)             
103600     END-IF                                                               
103700     MOVE ORAD-KVBEART             TO MOD2-KVBEART   (IX-RAD)             
103800     MOVE ORAD-KVAVBART            TO MOD2-KVAVBART  (IX-RAD)             
103900     MOVE ORAD-KVLEVART            TO MOD2-KVLEVART  (IX-RAD)             
104000     MOVE KORD-IDDC                TO MOD2-IDDC      (IX-RAD)             
104100     MOVE ZERO                     TO MOD2-IDKOLLI   (IX-RAD)             
104200     MOVE SPACE                    TO MOD2-KDTPOTYP  (IX-RAD)             
104300                                                                          
104400     IF ORAD-KDRADSTA < +4                                                
104500       MOVE 'U'                    TO MOD2-KDORDSTA  (IX-RAD)             
104600       PERFORM S01A-KOLLA-DDGS                                            
104700     ELSE                                                                 
104800       IF ORAD-KVAVBART = ZERO                                            
104900         MOVE 'N'                  TO MOD2-KDORDSTA  (IX-RAD)             
105000       ELSE                                                               
105100         MOVE 'P'                  TO MOD2-KDORDSTA  (IX-RAD)             
105200       END-IF                                                             
105300     END-IF                                                               
105400     PERFORM IMS-GET-WDE601                                               
105500     IF SEGMENT-FINNS                                                     
105600       MOVE VORD-DABEGPAC(3:6)     TO MOD2-TIRODAT   (IX-RAD)             
105700     ELSE                                                                 
105800       MOVE ZERO                   TO MOD2-TIRODAT   (IX-RAD)             
105900     END-IF                                                               
106000     IF GOOD-DDC                                                          
106100        MOVE DIRL-TISKEPPN-DDC     TO MOD2-TIRODAT   (IX-RAD)             
106200     END-IF                                                               
106300     MOVE ORAD-IDKUNDRF-RO(1:5)                                           
106400                                   TO MOD2-IDORDNR7-RO(IX-RAD)            
106500     .                                                                    
106600     EJECT                                                                
106700                                                                          
106800 S01A-KOLLA-DDGS SECTION.                                                 
106900                                                                          
107000     MOVE ODEL-IDDC  TO WS-IDDC                                           
107100                                                                          
107200     IF GOOD-DDC                                                          
107300        MOVE ODEL-IDPRODNR TO W-IDPRODNR-F6                               
107400        PERFORM IMS-GU-WDF601                                             
107500        IF SEGMENT-FINNS                                                  
107600           MOVE 'RD'       TO MOD2-KDORDSTA(IX-RAD)                       
107700        END-IF                                                            
107800     END-IF                                                               
107900     .                                                                    
108000 S02-LAS-WDE421 SECTION.                                                  
108100                                                                          
108200     IF MID-IDKOLLI-NEXT > ZERO                                           
108300       MOVE ORAD-IDPURAD              TO W-IDPURAD                        
108400       MOVE MID-IDPRODNR-NEXT         TO W-IDPRODNR-E421                  
108500       MOVE MID-IDKOLLI-NEXT          TO W-IDKOLLI-E421                   
108600                                         W-IDKOLLI                        
108700       PERFORM IMS-GNP-WDE421-KVAL                                        
108800       IF SEGMENT-FINNS                                                   
108900         PERFORM IMS-GNP-WDE611                                           
109000       ELSE                                                               
109100         ADD +1                       TO IX-RAD                           
109200       END-IF                                                             
109300     ELSE                                                                 
109400       MOVE ORAD-IDPURAD              TO W-IDPURAD                        
109500       PERFORM IMS-GNP-WDE421                                             
109600       IF SEGMENT-FINNS                                                   
109700         MOVE KKOLLI-IDKOLLI          TO W-IDKOLLI                        
109800         PERFORM IMS-GNP-WDE611                                           
109900       ELSE                                                               
110000         ADD +1                       TO IX-RAD                           
110100       END-IF                                                             
110200     END-IF                                                               
110300     .                                                                    
110400     EJECT                                                                
110500                                                                          
110600* IMS SEKTIONER                                                           
110700     SKIP3                                                                
110800 IMS-GET-MSG SECTION.                                                     
110900                                                                          
111000     MOVE '  QC' TO GODK-STATUSKODER                                      
111100     CALL CBLTDLI  USING GU MSG-PCB MSG-IO-AREA                           
111200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
111300     PERFORM IMS-STATUSKONTROLL                                           
111400     .                                                                    
111500     SKIP3                                                                
111600 IMS-INSERT-MSG SECTION.                                                  
111700                                                                          
111800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
111900     MOVE SPACE TO GODK-STATUSKODER                                       
112000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
112100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
112200     PERFORM IMS-STATUSKONTROLL                                           
112300     .                                                                    
112400     EJECT                                                                
112500 IMS-GET-WLORQA01-OKVAL-GU SECTION.                                       
112600                                                                          
112700     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
112800                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
112900            DELIMITED BY SIZE INTO SSA1                                   
113000     MOVE '  GE' TO GODK-STATUSKODER                                      
113100     CALL CBLTDLI USING GU ORQA-PCB IO-AREA-ORQA01 SSA1                   
113200     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
113300     PERFORM IMS-STATUSKONTROLL                                           
113400     .                                                                    
113500     SKIP2                                                                
113600 IMS-GET-WLORQA01-OKVAL-GN SECTION.                                       
113700                                                                          
113800     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
113900                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
114000            DELIMITED BY SIZE INTO SSA1                                   
114100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
114200     CALL CBLTDLI USING GN ORQA-PCB IO-AREA-ORQA01 SSA1                   
114300     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
114400     PERFORM IMS-STATUSKONTROLL                                           
114500     .                                                                    
114600     EJECT                                                                
114700 IMS-GET-WLORQF01-KVAL SECTION.                                           
114800                                                                          
114900     STRING 'WLORQF01(WDQ4ASEQ =' W-WDQ4ASEQ-X ')'                        
115000            DELIMITED BY SIZE INTO SSA1                                   
115100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
115200     CALL CBLTDLI USING GU ORQF-PCB IO-AREA-ORQF01 SSA1                   
115300     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
115400     PERFORM IMS-STATUSKONTROLL                                           
115500     .                                                                    
115600     SKIP2                                                                
115700 IMS-GET-WLORQF01-OKVAL SECTION.                                          
115800                                                                          
115900     STRING 'WLORQF01(WDQ4ASEQ>=' W-WDQ4ASEQ-MIN-X                        
116000                    '&WDQ4ASEQ<=' W-WDQ4ASEQ-MAX-X ')'                    
116100            DELIMITED BY SIZE INTO SSA1                                   
116200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
116300     CALL CBLTDLI   USING GN ORQF-PCB IO-AREA-ORQF01 SSA1                 
116400     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
116500     PERFORM IMS-STATUSKONTROLL                                           
116600     .                                                                    
116700     EJECT                                                                
116800 IMS-GET-WLORQI01-KVAL SECTION.                                           
116900                                                                          
117000     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
117100            DELIMITED BY SIZE INTO SSA1                                   
117200     MOVE '  GE' TO GODK-STATUSKODER                                      
117300     CALL CBLTDLI USING GU ORQI-PCB IO-AREA-ORQI01 SSA1                   
117400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
117500     PERFORM IMS-STATUSKONTROLL                                           
117600     .                                                                    
117700     SKIP2                                                                
117800 IMS-GET-WLORQI11-KVAL SECTION.                                           
117900                                                                          
118000     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
118100            DELIMITED BY SIZE INTO SSA1                                   
118200     STRING 'WLORQI11(WDQ211KY =' W-WDQ211KY-X ')'                        
118300            DELIMITED BY SIZE INTO SSA2                                   
118400     MOVE '  ' TO GODK-STATUSKODER                                        
118500     CALL CBLTDLI USING GU ORQI-PCB IO-AREA-ORQI11 SSA1 SSA2              
118600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
118700     PERFORM IMS-STATUSKONTROLL                                           
118800     .                                                                    
118900     EJECT                                                                
119000 IMS-GET-WLORQI12-KVAL SECTION.                                           
119100                                                                          
119200     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
119300            DELIMITED BY SIZE INTO SSA1                                   
119400     MOVE '  ' TO GODK-STATUSKODER                                        
119500     CALL CBLTDLI USING GU ORQI-PCB IO-AREA-ORQI12 SSA1                   
119600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
119700     PERFORM IMS-STATUSKONTROLL                                           
119800     .                                                                    
119900     EJECT                                                                
120000 IMS-GET-WLORDP01-KVAL SECTION.                                           
120100                                                                          
120200     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
120300            DELIMITED BY SIZE INTO SSA1                                   
120400     MOVE '  GE' TO GODK-STATUSKODER                                      
120500     CALL CBLTDLI USING GU ORDP-PCB IO-AREA-ORDP01 SSA1                   
120600     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
120700     PERFORM IMS-STATUSKONTROLL                                           
120800     .                                                                    
120900     SKIP2                                                                
121000 IMS-GET-WLORDP01-OKVAL SECTION.                                          
121100                                                                          
121200     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN-X                        
121300                    '&WDA501KY<=' W-WDA501KY-MAX-X ')'                    
121400            DELIMITED BY SIZE INTO SSA1                                   
121500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
121600     CALL CBLTDLI USING GN ORDP-PCB IO-AREA-ORDP01 SSA1                   
121700     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
121800     PERFORM IMS-STATUSKONTROLL                                           
121900     .                                                                    
122000     EJECT                                                                
122100 IMS-GET-WDE601 SECTION.                                                  
122200                                                                          
122300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
122400            DELIMITED BY SIZE INTO SSA1                                   
122500     MOVE '  GE'  TO GODK-STATUSKODER                                     
122600     CALL CBLTDLI USING GU WDE6-PCB IO-AREA-WDE601 SSA1                   
122700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
122800     PERFORM IMS-STATUSKONTROLL                                           
122900     .                                                                    
123000     EJECT                                                                
123100 IMS-GNP-WDE611        SECTION.                                           
123200                                                                          
123300     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
123400            DELIMITED BY SIZE INTO SSA1                                   
123500     MOVE '  '  TO GODK-STATUSKODER                                       
123600     CALL CBLTDLI USING GNP WDE6-PCB IO-AREA-WDE611 SSA1                  
123700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
123800     PERFORM IMS-STATUSKONTROLL                                           
123900     .                                                                    
124000     EJECT                                                                
124100 IMS-GU-WLARTC11-KVAL SECTION.                                            
124200                                                                          
124300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
124400            DELIMITED BY SIZE INTO SSA1                                   
124500     MOVE   'WLARTC11'         TO SSA2                                    
124600     MOVE '  GE' TO GODK-STATUSKODER                                      
124700     CALL CBLTDLI USING GU ARTC-PCB IO-AREA-ARTC11 SSA1 SSA2              
124800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
124900     PERFORM IMS-STATUSKONTROLL                                           
125000     .                                                                    
125100     EJECT                                                                
125200 IMS-GU-WDE401                SECTION.                                    
125300     STRING 'WDE401  (WDE4ASEQ>=' W-WDE4ASEQ-MIN-X                        
125400                    '&WDE4ASEQ<=' W-WDE4ASEQ-MAX-X                        
125500                    '&IDPRODNR =' W-IDPRODNR-X                            
125600                    '&IDPLKLST =' W-IDPLKLST-X ')'                        
125700            DELIMITED BY SIZE INTO SSA1                                   
125800     MOVE '  GE' TO GODK-STATUSKODER                                      
125900     CALL CBLTDLI USING GU WDE4-PCB IO-AREA-WDE401 SSA1                   
126000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
126100     PERFORM IMS-STATUSKONTROLL                                           
126200     .                                                                    
126300     SKIP2                                                                
126400 IMS-GNP-WDE411               SECTION.                                    
126500     MOVE 'WDE411'                     TO SSA1                            
126600     MOVE '  GE' TO GODK-STATUSKODER                                      
126700     CALL CBLTDLI USING GNP WDE4-PCB IO-AREA-WDE411 SSA1                  
126800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
126900     PERFORM IMS-STATUSKONTROLL                                           
127000     .                                                                    
127100     SKIP2                                                                
127200 IMS-GNP-WDE411-KVAL-RADNR    SECTION.                                    
127300     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
127400            DELIMITED BY SIZE INTO SSA1                                   
127500     MOVE '  GE' TO GODK-STATUSKODER                                      
127600     CALL CBLTDLI USING GNP WDE4-PCB IO-AREA-WDE411 SSA1                  
127700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
127800     PERFORM IMS-STATUSKONTROLL                                           
127900     .                                                                    
128000     SKIP3                                                                
128100 IMS-GNP-WDE411-KVAL-ARTNR    SECTION.                                    
128200     STRING 'WDE411  (IDARTNR  =' W-IDARTNR-X ')'                         
128300            DELIMITED BY SIZE INTO SSA1                                   
128400     MOVE '  GE' TO GODK-STATUSKODER                                      
128500     CALL CBLTDLI USING GNP WDE4-PCB IO-AREA-WDE411 SSA1                  
128600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
128700     PERFORM IMS-STATUSKONTROLL                                           
128800     .                                                                    
128900     EJECT                                                                
129000 IMS-GNP-WDE421               SECTION.                                    
129100     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
129200            DELIMITED BY SIZE INTO SSA1                                   
129300     MOVE   'WDE421'                   TO SSA2                            
129400     MOVE '  GE' TO GODK-STATUSKODER                                      
129500     CALL CBLTDLI USING GNP WDE4-PCB IO-AREA-WDE421 SSA1 SSA2             
129600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
129700     PERFORM IMS-STATUSKONTROLL                                           
129800     .                                                                    
129900     SKIP2                                                                
130000 IMS-GNP-WDE421-KVAL          SECTION.                                    
130100     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
130200            DELIMITED BY SIZE INTO SSA1                                   
130300     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
130400            DELIMITED BY SIZE INTO SSA2                                   
130500     MOVE '  GE' TO GODK-STATUSKODER                                      
130600     CALL CBLTDLI USING GNP WDE4-PCB IO-AREA-WDE421 SSA1 SSA2             
130700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
130800     PERFORM IMS-STATUSKONTROLL                                           
130900     .                                                                    
131000     EJECT                                                                
131100 IMS-GET-WDF106 SECTION.                                                  
131200                                                                          
131300     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
131400          DELIMITED BY SIZE INTO SSA1                                     
131500     MOVE 'WLLEVA14 ' TO SSA2                                             
131600     MOVE '  GE' TO GODK-STATUSKODER                                      
131700     CALL CBLTDLI USING GU LEVA-PCB IO-AREA-LEVA14 SSA1 SSA2              
131800     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
131900     PERFORM IMS-STATUSKONTROLL                                           
132000     .                                                                    
132100                                                                          
132200 IMS-GU-WDF601    SECTION.                                                
132300     STRING 'WDF601  (IDPRODNR =' W-IDPRODNR-F6-X ')'                     
132400          DELIMITED BY SIZE INTO SSA1                                     
132500     MOVE '  GE' TO GODK-STATUSKODER                                      
132600     CALL CBLTDLI USING GU WDF6-PCB IO-AREA-WDF601 SSA1                   
132700     MOVE WDF6-STATUS-CODE    TO STATUS-WS                                
132800     PERFORM IMS-STATUSKONTROLL                                           
132900     .                                                                    
133000                                                                          
133100 IMS-STATUSKONTROLL SECTION.                                              
133200                                                                          
133300     SET STATUS-IX TO 1                                                   
133400     SEARCH GODK-STATUS                                                   
133500       AT END                                                             
133600         CALL FELLOG                                                      
133700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
133800         CONTINUE                                                         
133900     END-SEARCH                                                           
134000     .                                                                    
