000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9033500.                                                
000400 AUTHOR.         GERRY CARMICHAEL.                                        
000500 DATE-WRITTEN.   JANUARI 98.                                              
000600*                                                                         
000700*REMARKS.                                                                 
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR BILDEN;                                      
001100*        FRÅGA PÅ ORDER, ARTIKLAR VIA VDI-SYSTEMET                        
001200*        PROGRAMMET SKALL VISA ALLA ORDERRADER                            
001300*        FÖR EN VISS IDDISTR, IDKUNDNR OCH IDARTNR.                       
001400*        PROGRAMMET HANTERAR OLIKA VERSIONER I VDI                        
001500*        ENLIGT IDVTYP.                                                   
001600*                                                                         
001700*        PROGRAMMET ANROPAR W218ETA FÖR HÄMTNING                          
001800*        AV ETA-DATUM/NDC-LAGER.                                          
001900*       (ESTIMATED TIME AVAILABLE)                                        
002000*                                                                         
002100*        LÄSER   WDQ2                                                     
002200*                WDQ3                                                     
002300*                WDQ4                                                     
002400*                WDE4                                                     
002500*                WDA5                                                     
002600*                WDF1                                                     
002700*                                                                         
002800*        OM SIDAN INTE FULL OCH DET INTE FINNS FLER SEGMENT               
002900*        PÅ WDQ4 LÄSER MAN WDE4. OM SIDAN FORTFARANDE INTE                
003000*        ÄR FULL OCH DET INTE FINNS FLER SEGMENT PÅ WDE4                  
003100*        LÄSER MAN WDA5.                                                  
003200*                                                                         
003300*        BLIR SIDAN FULL OCH DET FINNS FLER SEGMENT PÅ                    
003400*        NÅGON AV BASERNA SPARAS DET EN FLAGGA OCH ETT RAD-               
003500*        NUMMER SOM TALAR OM PÅ VILKEN BAS OCH VAR I BASEN                
003600*        MAN SKALL FORTSÄTTA ATT LÄSA VID EN EVENTUELL                    
003700*        BLÄDDRING.                                                       
003800*                                                                         
003900*    INDATA.                                                              
004000*        TRANSAKTION: W90335T                                             
004100*        MID:         W9I33501                                            
004200*                                                                         
004300*    UTDATA.                                                              
004400*        MOD:         W9O33501                                            
004500*                     W9O33502                                            
004600     SKIP3                                                                
004700 ENVIRONMENT DIVISION.                                                    
004800     SKIP3                                                                
004900 DATA DIVISION.                                                           
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400     SKIP3                                                                
005500 77  IDPGM                       PIC X(8)    VALUE 'W9033500'.            
005600                                                                          
005700 77    JA                        PIC X       VALUE 'J'.                   
005800 77    NEJ                       PIC X       VALUE 'N'.                   
005900 77    FELTEXT                   PIC X(64)   VALUE SPACE.                 
006000                                                                          
006100 77   WS-KVDAGAR-DIFF            PIC 9(3).                                
006200 77   W-KDORDSTA                 PIC X(2)    VALUE SPACE.                 
006300 77   W-KVPREAVB                 PIC S9(7)             COMP-3.            
006400 77   MAX-RAD                    PIC S9(7)   VALUE +13 COMP-3.            
006500 77   MAX-MOD-LANGD              PIC S9(7)   VALUE +0  COMP-3.            
006600                                                                          
006700 77   W-TIAAMMDD                 PIC S9(6).                               
006800 77   W-TIPACKN                  PIC S9(6).                               
006900 77   W-TIDISPIN                 PIC S9(6).                               
007000 77   W-WDE4-IDORDNR5            PIC S9(5)   VALUE ZERO  COMP-3.          
007100 77   W-WDE4-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.          
007200 77   W-WDE4-IDPLKLST            PIC S9(3)   VALUE ZERO  COMP-3.          
007300 77   SW-ARTC11-LAEST            PIC X       VALUE 'N'.                   
007400                                                                          
007500 01   W-TIAAVVD                  PIC 9(5).                                
007600 01   FILLER REDEFINES W-TIAAVVD.                                         
007700      03  W-TIAA                 PIC 9(2).                                
007800      03  W-TIVV                 PIC 9(2).                                
007900      03  W-TID                  PIC 9(1).                                
008000     EJECT                                                                
008100* ----- PARAMETRAR TILL SUBPROGRAM WDATKONV                               
008200                                                                          
008300*01  FILLER -COPY WDATAREA                                                
008400     EJECT                                                                
008500* ----- INDEXFÄLT                                                         
008600 77  IX-RAD                      PIC S9(9)   VALUE +0  COMP SYNC.         
008700                                                                          
008800* ----- SWITCHAR                                                          
008900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009000     88  NYCKLAR-OK                          VALUE 'J'.                   
009100     EJECT                                                                
009200*      --- VALID IDDC CODES                                               
009300*                                                                         
009400*01    -COPY WWDC99                                                       
009500*01    -COPY WWDCKONS                                                     
009600       EJECT                                                              
009700*  --- FÖR ATT FÅ RÄTT SEKEL VID ANROP TILL W218ETA                       
009800 01  WS-ETA-DATUM                PIC 9(6).                                
009900 01  FILLER REDEFINES WS-ETA-DATUM.                                       
010000     03  WS-ETA-DATUM-AAR        PIC 9(2).                                
010100     03  WS-ETA-DATUM-MAANAD     PIC 9(2).                                
010200     03  WS-ETA-DATUM-DAG        PIC 9(2).                                
010300     EJECT                                                                
010400 01  TEST-IDDISTR             PIC S9(5) COMP-3.                           
010500     EJECT                                                                
010600* ----- GENERELLA SUB PROGRAM                                             
010700 01  GENERELLA-SUBPROGRAM.                                                
010800   03 CBLTDLI                    PIC X(8)    VALUE 'CBLTDLI '.            
010900   03 FELLOG                     PIC X(8)    VALUE 'FELLOG  '.            
011000   03 WDATKONV                   PIC X(8)    VALUE 'WDATKONV'.            
011100   03 W218ETA                    PIC X(8)    VALUE 'W218ETA '.            
011200                                                                          
011300 01 FILLER                       PIC  X(8)   VALUE 'LETA'.                
011400*   -COPY W218LETA -PRE ETA-.                                             
011500     EJECT                                                                
011600 01    NYCKLAR-TILL-DLI.                                                  
011700     SKIP2                                                                
011800   03    W-WDQ211KY-X.                                                    
011900     05    W-Q211KY-IDDC         PIC X(2)    VALUE SPACE.                 
012000     05    W-Q211KY-IDLEVNR      PIC X(5)    VALUE SPACE.                 
012100     SKIP2                                                                
012200   03    W-WDQ401KY-X.                                                    
012300     05    W-Q401KY-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
012400     05    W-Q401KY-IDDC         PIC  X(2)   VALUE ZERO.                  
012500     05    W-Q401KY-ADLAGOMR     PIC S9(3)   VALUE ZERO  COMP-3.          
012600     05    W-Q401KY-ADGANG       PIC S9(3)   VALUE ZERO  COMP-3.          
012700     05    W-Q401KY-ADPLATS      PIC S9(5)   VALUE ZERO  COMP-3.          
012800     05    W-Q401KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
012900     05    W-Q401KY-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
013000     SKIP2                                                                
013100   03    W-WDQ4B1KY-X.                                                    
013200     05    W-Q4B1KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
013300     05    W-Q4B1KY-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
013400     05    W-Q4B1KY-IDGMTREF.                                             
013500       07  W-Q4B1KY-IDDISTR      PIC S9(5)   VALUE ZERO  COMP-3.          
013600       07  W-Q4B1KY-IDKUNDNR     PIC S9(7)   VALUE ZERO  COMP-3.          
013700       07  W-Q4B1KY-IDKUNDRF.                                             
013800        09 W-Q4B1KY-IDORDNR7     PIC 9(07)   VALUE ZERO.                  
013900        09 FILLER                PIC X(03)   VALUE SPACE.                 
014000     05    W-Q4B1KY-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
014100     05    W-Q4B1KY-IDDC         PIC  X(2)   VALUE ZERO.                  
014200     05    W-Q4B1KY-ADLAGOMR     PIC S9(3)   VALUE ZERO  COMP-3.          
014300     05    W-Q4B1KY-ADGANG       PIC S9(3)   VALUE ZERO  COMP-3.          
014400     05    W-Q4B1KY-ADPLATS      PIC S9(5)   VALUE ZERO  COMP-3.          
014500     SKIP2                                                                
014600   03    W-WDQ4B1KY-MIN-X.                                                
014700     05    W-Q4B1KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
014800     05    W-Q4B1KY-MIN-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
014900     05    W-Q4B1KY-MIN-IDGMTREF.                                         
015000       07  W-Q4B1KY-MIN-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
015100       07  W-Q4B1KY-MIN-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
015200       07  W-Q4B1KY-MIN-IDKUNDRF.                                         
015300        09 W-Q4B1KY-MIN-IDORDNR7 PIC 9(07)   VALUE ZERO.                  
015400        09 FILLER                PIC X(03)   VALUE SPACE.                 
015500     05    W-Q4B1KY-MIN-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
015600     05    W-Q4B1KY-MIN-IDDC     PIC  X(2)   VALUE ZERO.                  
015700     05    W-Q4B1KY-MIN-ADLAGOMR PIC S9(3)   VALUE ZERO  COMP-3.          
015800     05    W-Q4B1KY-MIN-ADGANG   PIC S9(3)   VALUE ZERO  COMP-3.          
015900     05    W-Q4B1KY-MIN-ADPLATS  PIC S9(5)   VALUE ZERO  COMP-3.          
016000     SKIP2                                                                
016100   03    W-WDQ4B1KY-MAX-X.                                                
016200     05    W-Q4B1KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
016300     05    W-Q4B1KY-MAX-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
016400     05    W-Q4B1KY-MAX-IDGMTREF.                                         
016500       07  W-Q4B1KY-MAX-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
016600       07  W-Q4B1KY-MAX-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
016700       07  W-Q4B1KY-MAX-IDKUNDRF.                                         
016800        09 W-Q4B1KY-MAX-IDORDNR7 PIC 9(07)   VALUE ZERO.                  
016900        09 FILLER                PIC X(03)   VALUE SPACE.                 
017000     05    W-Q4B1KY-MIN-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
017100     05    W-Q4B1KY-MIN-IDDC     PIC  X(2)   VALUE ZERO.                  
017200     05    W-Q4B1KY-MAX-ADLAGOMR PIC S9(3)   VALUE ZERO  COMP-3.          
017300     05    W-Q4B1KY-MAX-ADGANG   PIC S9(3)   VALUE ZERO  COMP-3.          
017400     05    W-Q4B1KY-MAX-ADPLATS  PIC S9(5)   VALUE ZERO  COMP-3.          
017500     SKIP2                                                                
017600   03    W-WDE401KY-X.                                                    
017700     05    W-E401KY-IDGMTREF.                                             
017800       07  W-E401KY-IDDISTR      PIC S9(5)   VALUE ZERO  COMP-3.          
017900       07  W-E401KY-IDKUNDNR     PIC S9(7)   VALUE ZERO  COMP-3.          
018000       07  W-E401KY-IDKUNDRF.                                             
018100        09 W-E401KY-IDORDNR5     PIC 9(05)   VALUE ZERO.                  
018200        09 FILLER                PIC X(05)   VALUE SPACE.                 
018300     05    W-E401KY-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
018400     05    W-E401KY-IDPLKLST     PIC S9(3)   VALUE ZERO  COMP-3.          
018500     SKIP2                                                                
018600   03    W-WDE4C1KY-X.                                                    
018700     05    W-E4C1KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
018800     05    W-E4C1KY-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
018900     05    W-E4C1KY-IDPURAD      PIC S9(5)   VALUE ZERO  COMP-3.          
019000     SKIP2                                                                
019100   03    W-WDE4C1KY-MIN-X.                                                
019200     05    W-E4C1KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
019300     05    W-E4C1KY-MIN-IDPRODNR PIC S9(7)   VALUE ZERO  COMP-3.          
019400     05    W-E4C1KY-MIN-IDPURAD  PIC S9(5)   VALUE ZERO  COMP-3.          
019500     SKIP2                                                                
019600   03    W-WDE4C1KY-MAX-X.                                                
019700     05    W-E4C1KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
019800     05    W-E4C1KY-MAX-IDPRODNR PIC S9(7)   VALUE ZERO  COMP-3.          
019900     05    W-E4C1KY-MAX-IDPURAD  PIC S9(5)   VALUE ZERO  COMP-3.          
020000     SKIP2                                                                
020100   03    W-WDA501KY-X.                                                    
020200     05    W-A501KY-IDGMTREF.                                             
020300       07  W-A501KY-IDDISTR  PIC S9(5)       VALUE ZERO  COMP-3.          
020400       07  W-A501KY-IDKUNDNR PIC S9(7)       VALUE ZERO  COMP-3.          
020500       07  W-A501KY-IDORDNR5 PIC X(05)       VALUE SPACE.                 
020600       07  FILLER            PIC X(05)       VALUE SPACE.                 
020700     05    W-A501KY-IDARTNR  PIC S9(9)       VALUE ZERO  COMP-3.          
020800     05    W-A501KY-IDLOPNR  PIC S9(3)       VALUE ZERO  COMP-3.          
020900     SKIP2                                                                
021000   03    W-WDA501KY-MIN-X.                                                
021100     05    W-A501KY-MIN-IDGMTREF.                                         
021200       07  W-A501KY-MIN-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
021300       07  W-A501KY-MIN-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
021400       07  W-A501KY-MIN-IDORDNR5 PIC X(05)   VALUE SPACE.                 
021500       07  FILLER                PIC X(05)   VALUE SPACE.                 
021600     05    W-A501KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
021700     05    W-A501KY-MIN-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
021800     SKIP2                                                                
021900   03    W-WDA501KY-MAX-X.                                                
022000     05    W-A501KY-MAX-IDGMTREF.                                         
022100       07  W-A501KY-MAX-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
022200       07  W-A501KY-MAX-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
022300       07  W-A501KY-MAX-IDORDNR5 PIC X(05)   VALUE SPACE.                 
022400       07  FILLER                PIC X(05)   VALUE SPACE.                 
022500     05    W-A501KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
022600     05    W-A501KY-MAX-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
022700     SKIP2                                                                
022800   03    W-IDDISTR-X.                                                     
022900     05    W-IDDISTR             PIC S9(5)   VALUE ZERO  COMP-3.          
023000     SKIP2                                                                
023100   03    W-IDKUNDNR-X.                                                    
023200     05    W-IDKUNDNR            PIC S9(7)   VALUE ZERO  COMP-3.          
023300     SKIP2                                                                
023400   03    W-IDDC-X.                                                        
023500     05    W-IDDC                PIC  X(2)   VALUE ZERO.                  
023600     SKIP2                                                                
023700   03    W-IDORDER-X.                                                     
023800     05    W-IDORDER             PIC S9(7)   VALUE ZERO  COMP-3.          
023900     SKIP2                                                                
024000   03    W-IDARTNR-X.                                                     
024100     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
024200     SKIP2                                                                
024300   03    W-IDLOPNR-X.                                                     
024400     05    W-IDLOPNR             PIC S9(3)   VALUE ZERO  COMP-3.          
024500     SKIP2                                                                
024600   03    W-IDKUNDRF-MIN-X.                                                
024700     05    W-IDKUNDRF-MIN        PIC X(10).                               
024800                                                                          
024900     05    W-IDORDNR5-FILLER-MIN REDEFINES W-IDKUNDRF-MIN.                
025000       10  W-IDORDNR5-MIN        PIC X(5).                                
025100       10  FILLER                PIC X(5).                                
025200                                                                          
025300     05    W-IDORDNR7-FILLER-MIN REDEFINES W-IDKUNDRF-MIN.                
025400       10  W-IDORDNR7-MIN        PIC X(7).                                
025500       10  FILLER                PIC X(3).                                
025600     SKIP2                                                                
025700   03    W-IDKUNDRF-X.                                                    
025800     05    W-IDORDNR5            PIC 9(5).                                
025900     05    FILLER                PIC X(5).                                
026000                                                                          
026100     SKIP2                                                                
026200   03    W-IDPRODNR-X.                                                    
026300     05    W-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.          
026400                                                                          
026500   03    W-IDPURAD-X.                                                     
026600     05    W-IDPURAD             PIC S9(5)   VALUE ZERO  COMP-3.          
026700     SKIP2                                                                
026800   03  W-IDLEVNR-X.                                                       
026900     05  W-IDLEVNR               PIC X(5)   VALUE SPACE.                  
027000     EJECT                                                                
027100     EJECT                                                                
027200******************************************************************        
027300*                                                                         
027400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
027500*                                                                         
027600 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
027700     SKIP3                                                                
027800*01    MID -COPY W9I33501.                                                
027900     EJECT                                                                
028000*01    -COPY WMSGAREA                                                     
028100     EJECT                                                                
028200*  03    MOD -COPY W9O33501  -RED MSG-AREA.                               
028300     EJECT                                                                
028400*  03    MOD -COPY W9O33502  -RED MSG-AREA.                               
028500     EJECT                                                                
028600*01    -COPY WMFSAREA                                                     
028700     EJECT                                                                
028800******************************************************************        
028900*                                                                         
029000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
029100*                                                                         
029200 01    IMS-WS.                                                            
029300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
029400     SKIP3                                                                
029500*                        **** STATUS-KOD FRÅN IMS                         
029600   03    STATUS-WS               PIC XX.                                  
029700     88    SEGMENT-FINNS                     VALUE '  '.                  
029800     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
029900     88    SEGMENT-SLUT                      VALUE 'GB'.                  
030000     SKIP3                                                                
030100   03    GODK-STATUSKODER.                                                
030200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
030300     SKIP3                                                                
030400 01    SSA1                      PIC X(200).                              
030500 01    SSA2                      PIC X(64).                               
030600     EJECT                                                                
030700*                            IMS FUNKTIONSKODER                           
030800*01    -COPY W0003                                                        
030900     EJECT                                                                
031000******************************************************************        
031100*                                                                         
031200*        ARBETS-AREOR TILL IO-AREORNA                                     
031300*                                                                         
031400*    ---  DLI INPUT-OUTPUT AREA 1                                         
031500*    ---  DLI-IO-AREA                                                     
031600                                                                          
031700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI01'.            
031800 01  DLI-IO-ORQI01.                                                       
031900*  03    WLORQI01 -COPY WDQ201                                            
032000     EJECT                                                                
032100                                                                          
032200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI11'.            
032300 01  DLI-IO-ORQI11.                                                       
032400*  03    WLORQI11 -COPY WDQ211                                            
032500     EJECT                                                                
032600                                                                          
032700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI12'.            
032800 01  DLI-IO-ORQI12.                                                       
032900*  03    WLORQI12 -COPY WDQ212                                            
033000     EJECT                                                                
033100                                                                          
033200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQH01'.            
033300 01  DLI-IO-ORQH01.                                                       
033400*  03    WLORQH01 -COPY WDQ4B1      -PRE Q4-                              
033500     EJECT                                                                
033600                                                                          
033700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC11'.            
033800 01  DLI-IO-ARTC11.                                                       
033900*  03    WLARTC11  -COPY WDK611                                           
034000     EJECT                                                                
034100                                                                          
034200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQF01'.            
034300 01  DLI-IO-ORQF01.                                                       
034400*  03    WLORQF01 -COPY WDQ401                                            
034500     EJECT                                                                
034600                                                                          
034700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORDP01'.            
034800 01  DLI-IO-ORDP01.                                                       
034900*  03    WLORDP01  -COPY WDA501                                           
035000     EJECT                                                                
035100                                                                          
035200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE4C'.             
035300 01  DLI-IO-WDE4C.                                                        
035400*  03    -COPY WDE4C1                                                     
035500     EJECT                                                                
035600                                                                          
035700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-E40120'.            
035800 01  DLI-IO-E40120.                                                       
035900*    03      -COPY WDE401                                                 
036000     EJECT                                                                
036100*    03      -COPY WDE411                                                 
036200     EJECT                                                                
036300                                                                          
036400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE601'.            
036500 01  DLI-IO-WDE601.                                                       
036600*    03  -COPY WDE601                                                     
036700     EJECT                                                                
036800                                                                          
036900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-LEVA14'.            
037000 01  DLI-IO-LEVA14.                                                       
037100*    03  WLLEVA14  -COPY WDF106                                           
037200     EJECT                                                                
037300 01  FILLER                 PIC X(12) VALUE 'DUMMY-ARTC'.                 
037400 01  ETA-ARTC-PCB           PIC X.                                        
037500 01  FILLER                 PIC X(12) VALUE 'DUMMY-LEVA'.                 
037600 01  ETA-LEVA-PCB           PIC X.                                        
037700 LINKAGE SECTION.                                                         
037800                                                                          
037900*01    -COPY W0009     -PRE MSG-                                          
038000     EJECT                                                                
038100*01    -COPY W0008     -PRE ORQI-                                         
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400*01    -COPY W0008     -PRE ORQF-                                         
038500     05  FILLER                  PIC X.                                   
038600     EJECT                                                                
038700*01    -COPY W0008     -PRE ORQH-                                         
038800     05  FILLER                  PIC X.                                   
038900     EJECT                                                                
039000*01    -COPY W0008     -PRE WDE4-                                         
039100     05  FILLER                  PIC X.                                   
039200     EJECT                                                                
039300*01    -COPY W0008     -PRE WDE4C-                                        
039400     05  FILLER                  PIC X.                                   
039500     EJECT                                                                
039600*01    -COPY W0008     -PRE ORDP-                                         
039700     05  FILLER                  PIC X.                                   
039800     EJECT                                                                
039900*01    -COPY W0008     -PRE ARTC-                                         
040000     05  FILLER                  PIC X.                                   
040100     EJECT                                                                
040200*01    -COPY W0008     -PRE WDE6-                                         
040300     05  FILLER                  PIC X.                                   
040400     EJECT                                                                
040500*01    -COPY W0008     -PRE LEVA-                                         
040600     05  FILLER                  PIC X.                                   
040700     EJECT                                                                
040800 01  ETA-WDK7-PCB           PIC X.                                        
040900 01  ETA-INLC-PCB           PIC X.                                        
041000 01  ETA-WDB6-PCB           PIC X.                                        
041100 01  ETA-WDD9-PCB           PIC X.                                        
041200     EJECT                                                                
041300 PROCEDURE DIVISION  USING MSG-PCB  ORQI-PCB ORQF-PCB ORQH-PCB            
041400                           WDE4-PCB WDE4C-PCB ORDP-PCB ARTC-PCB           
041500                           WDE6-PCB LEVA-PCB                              
041600                           ETA-WDK7-PCB ETA-INLC-PCB                      
041700                           ETA-WDB6-PCB ETA-WDD9-PCB.                     
041800     ENTRY 'DLITCBL' USING MSG-PCB  ORQI-PCB ORQF-PCB ORQH-PCB            
041900                           WDE4-PCB WDE4C-PCB ORDP-PCB ARTC-PCB           
042000                           WDE6-PCB LEVA-PCB                              
042100                           ETA-WDK7-PCB ETA-INLC-PCB                      
042200                           ETA-WDB6-PCB ETA-WDD9-PCB.                     
042300                                                                          
042400 STYR SECTION.                                                            
042500                                                                          
042600     PERFORM IMS-GET-MSG                                                  
042700     IF SEGMENT-FINNS                                                     
042800         PERFORM A-INIT                                                   
042900         PERFORM B-KONTROLLERA-FLYTTA-NYCKLAR                             
043000         IF NYCKLAR-OK                                                    
043100             PERFORM C-BEHANDLA-RADER                                     
043200         END-IF                                                           
043300     END-IF                                                               
043400     PERFORM D-KONTROLLERA-OM-SIDA-TOM                                    
043500     PERFORM E-BERAKNA-MAX-MOD-LANGD                                      
043600     MOVE MAX-MOD-LANGD        TO MSG-KVLL                                
043700     PERFORM IMS-INSERT-MSG                                               
043800     MOVE ZERO                 TO RETURN-CODE                             
043900     GOBACK                                                               
044000     .                                                                    
044100     EJECT                                                                
044200                                                                          
044300 A-INIT SECTION.                                                          
044400                                                                          
044500     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I33501                    
044600     MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                     
044700     MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                    
044800     MOVE ' '                          TO MFS-KDTRTYP                     
044900     MOVE LOW-VALUE                    TO MSG-AREA                        
045000     MOVE +1                           TO  IX-RAD                         
045100                                                                          
045200     IF MID-IDVTYP NOT = '2'                                              
045300       MOVE 'W9O33501'                 TO MFS-IDMOD                       
045400       MOVE '9335'                     TO MOD-IDTRANS                     
045500       MOVE ZERO                       TO MOD-IDMFSFEL                    
045600       MOVE MID-IDORDNR7-NEXT          TO MOD-IDORDNR7-NEXT               
045700       MOVE MID-IDARTNR                TO MOD-IDARTNR                     
045800       MOVE ZERO                       TO MOD-IDKOLLI-NEXT                
045900                                                                          
046000       PERFORM UNTIL IX-RAD    >   MAX-RAD                                
046100           MOVE ZERO           TO  MOD-IDORDNR7-RAD (IX-RAD)              
046200                                     MOD-TIDISPIN   (IX-RAD)              
046300                                     MOD-KVBEART-Q  (IX-RAD)              
046400                                     MOD-KVPREAVB   (IX-RAD)              
046500                                     MOD-IDORDNR7-LEV (IX-RAD)            
046600                                     MOD-TIANNULL   (IX-RAD)              
046700                                     MOD-KDORDKL    (IX-RAD)              
046800           MOVE SPACE          TO  MOD-KDORDSTA     (IX-RAD)              
046900                                     MOD-IDDC-RAD   (IX-RAD)              
047000                                     MOD-IDBIL      (IX-RAD)              
047100           ADD +1              TO  IX-RAD                                 
047200       END-PERFORM                                                        
047300     ELSE                                                                 
047400       MOVE 'W9O33502'                 TO MFS-IDMOD                       
047500       MOVE '9335'                     TO MOD2-IDTRANS                    
047600       MOVE ZERO                       TO MOD2-IDMFSFEL                   
047700       MOVE MID-IDORDNR7-NEXT          TO MOD2-IDORDNR7-NEXT              
047800       MOVE MID-IDARTNR                TO MOD2-IDARTNR                    
047900       MOVE ZERO                       TO MOD2-IDKOLLI-NEXT               
048000                                                                          
048100       PERFORM UNTIL IX-RAD    >   MAX-RAD                                
048200           MOVE ZERO           TO  MOD2-IDORDNR7-RAD (IX-RAD)             
048300                                   MOD2-TIDISPIN  (IX-RAD)                
048400                                   MOD2-KVBEART-Q (IX-RAD)                
048500                                   MOD2-KVPREAVB  (IX-RAD)                
048600                                   MOD2-IDORDNR7-LEV (IX-RAD)             
048700                                   MOD2-TIANNULL  (IX-RAD)                
048800                                   MOD2-KDORDKL   (IX-RAD)                
048900                                   MOD2-KDANNULL  (IX-RAD)                
049000           MOVE SPACE          TO  MOD2-KDORDSTA    (IX-RAD)              
049100                                   MOD2-IDDC-RAD  (IX-RAD)                
049200                                   MOD2-IDRADINF  (IX-RAD)                
049300           ADD +1              TO  IX-RAD                                 
049400       END-PERFORM                                                        
049500     END-IF                                                               
049600     .                                                                    
049700     EJECT                                                                
049800 B-KONTROLLERA-FLYTTA-NYCKLAR SECTION.                                    
049900                                                                          
050000     IF MID-IDDISTR            NUMERIC AND                                
050100        MID-IDKUNDNR           NUMERIC AND                                
050200        MID-IDARTNR            NUMERIC                                    
050300         PERFORM  BA-FLYTTA-NYCKLAR                                       
050400      ELSE                                                                
050500         MOVE NEJ              TO NYCKLAR-SW                              
050600         MOVE 'B01'            TO MOD-IDMFSFEL                            
050700     END-IF                                                               
050800     .                                                                    
050900     EJECT                                                                
051000 BA-FLYTTA-NYCKLAR SECTION.                                               
051100                                                                          
051200     MOVE LOW-VALUE            TO  W-WDE4C1KY-MIN-X                       
051300                                   W-WDQ4B1KY-MIN-X                       
051400                                                                          
051500     MOVE HIGH-VALUE           TO  W-WDE4C1KY-MAX-X                       
051600                                   W-WDQ4B1KY-MAX-X                       
051700                                                                          
051800     MOVE MID-IDARTNR          TO  W-Q4B1KY-MIN-IDARTNR                   
051900                                   W-Q4B1KY-MAX-IDARTNR                   
052000                                   W-Q4B1KY-IDARTNR                       
052100                                                                          
052200                                   W-E4C1KY-MIN-IDARTNR                   
052300                                   W-E4C1KY-MAX-IDARTNR                   
052400                                   W-E4C1KY-IDARTNR                       
052500                                   W-IDARTNR                              
052600                                                                          
052700     MOVE MID-IDDISTR          TO  W-Q4B1KY-MIN-IDDISTR                   
052800                                   W-Q4B1KY-MAX-IDDISTR                   
052900                                   W-Q4B1KY-IDDISTR                       
053000                                                                          
053100                                   W-A501KY-MIN-IDDISTR                   
053200                                   W-A501KY-MAX-IDDISTR                   
053300                                   W-IDDISTR                              
053400                                   TEST-IDDISTR                           
053500                                                                          
053600     MOVE MID-IDKUNDNR         TO  W-Q4B1KY-MIN-IDKUNDNR                  
053700                                   W-Q4B1KY-MAX-IDKUNDNR                  
053800                                   W-Q4B1KY-IDKUNDNR                      
053900                                                                          
054000                                   W-A501KY-MIN-IDKUNDNR                  
054100                                   W-A501KY-MAX-IDKUNDNR                  
054200                                   W-IDKUNDNR                             
054300     .                                                                    
054400     EJECT                                                                
054500 C-BEHANDLA-RADER SECTION.                                                
054600                                                                          
054700     MOVE +1                   TO IX-RAD                                  
054800                                                                          
054900     EVALUATE TRUE                                                        
055000     WHEN MID-FLSVAR-NEXT      =  'P' OR SPACE                            
055100         PERFORM CA-BEHANDLA-WDE4-ORDER                                   
055200         IF IX-RAD             >  MAX-RAD AND SEGMENT-FINNS               
055300             CONTINUE                                                     
055400          ELSE                                                            
055500             PERFORM CB-BEHANDLA-ORDERRADKOE                              
055600             IF IX-RAD         >  MAX-RAD AND SEGMENT-FINNS               
055700                 CONTINUE                                                 
055800              ELSE                                                        
055900                 PERFORM CC-BEHANDLA-RESTORDER                            
056000             END-IF                                                       
056100         END-IF                                                           
056200                                                                          
056300     WHEN MID-FLSVAR-NEXT      =  'O'                                     
056400         PERFORM CB-BEHANDLA-ORDERRADKOE                                  
056500         IF IX-RAD             >  MAX-RAD  AND SEGMENT-FINNS              
056600             CONTINUE                                                     
056700          ELSE                                                            
056800             PERFORM CC-BEHANDLA-RESTORDER                                
056900         END-IF                                                           
057000                                                                          
057100     WHEN MID-FLSVAR-NEXT      =  'R'                                     
057200         PERFORM CC-BEHANDLA-RESTORDER                                    
057300                                                                          
057400     END-EVALUATE                                                         
057500     .                                                                    
057600     EJECT                                                                
057700 CA-BEHANDLA-WDE4-ORDER SECTION.                                          
057800                                                                          
057900     IF MID-IDPRODNR-NEXT           > ZERO                                
058000         MOVE MID-IDPRODNR-NEXT TO  W-E4C1KY-IDPRODNR                     
058100         MOVE MID-IDRADNR-NEXT  TO  W-E4C1KY-IDPURAD                      
058200         MOVE SPACE             TO  W-IDKUNDRF-X                          
058300         MOVE MID-IDORDNR7-NEXT TO  W-IDORDNR5                            
058400         PERFORM IMS-GET-WDE4C-KVAL                                       
058500      ELSE                                                                
058600         PERFORM IMS-GET-WDE4C-OKVAL                                      
058700     END-IF                                                               
058800                                                                          
058900     PERFORM UNTIL             SEGMENT-SAKNAS  OR                         
059000                               SEGMENT-SLUT    OR                         
059100                               IX-RAD > MAX-RAD                           
059200                                                                          
059300         MOVE SEQC-IDORDNR5    TO  W-WDE4-IDORDNR5                        
059400                                                                          
059500         PERFORM CAA-SKAPA-WDE401-WDE411-NYCKEL                           
059600         PERFORM IMS-GET-WDE401-11                                        
059700         PERFORM CAB-REDIGERA-PO-RAD                                      
059800                                                                          
059900         PERFORM IMS-GET-WDE4C-OKVAL                                      
060000     END-PERFORM                                                          
060100                                                                          
060200     IF IX-RAD                 > MAX-RAD  AND                             
060300        SEGMENT-FINNS                                                     
060400         PERFORM CAC-SPARA-PACK-ORDER-NYCKLAR                             
060500     END-IF                                                               
060600     .                                                                    
060700     EJECT                                                                
060800 CAA-SKAPA-WDE401-WDE411-NYCKEL SECTION.                                  
060900                                                                          
061000     MOVE MID-IDDISTR          TO  W-E401KY-IDDISTR                       
061100     MOVE MID-IDKUNDNR         TO  W-E401KY-IDKUNDNR                      
061200     MOVE SPACE                TO  W-E401KY-IDKUNDRF                      
061300     MOVE SEQC-IDORDNR5        TO  W-E401KY-IDORDNR5                      
061400     MOVE SEQC-IDPRODNR        TO  W-E401KY-IDPRODNR                      
061500     MOVE SEQC-IDPLKLST        TO  W-E401KY-IDPLKLST                      
061600     MOVE SEQC-IDPURAD         TO  W-IDPURAD                              
061700     .                                                                    
061800     EJECT                                                                
061900                                                                          
062000 CAB-REDIGERA-PO-RAD SECTION.                                             
062100                                                                          
062200     IF MID-IDVTYP NOT = '2'                                              
062300       IF ORAD-IDKUNDRF-RO     IN ORAD-WDE411                             
062400                               NOT = '00000     '                         
062500           MOVE ORAD-IDKUNDRF-RO IN ORAD-WDE411 (1:5)                     
062600                                 TO MOD-IDORDNR7-RAD(IX-RAD)              
062700        ELSE                                                              
062800           MOVE W-WDE4-IDORDNR5 TO MOD-IDORDNR7-RAD(IX-RAD)               
062900       END-IF                                                             
063000       MOVE ORAD-KVBEART       IN ORAD-WDE411                             
063100                               TO MOD-KVBEART-Q(IX-RAD)                   
063200       MOVE ORAD-KVAVBART      IN ORAD-WDE411                             
063300                               TO MOD-KVPREAVB(IX-RAD)                    
063400       MOVE ORAD-KDORDKL       IN ORAD-WDE411                             
063500                               TO MOD-KDORDKL (IX-RAD)                    
063600       IF ORAD-IDBIL IN ORAD-WDE411 = LOW-VALUE                           
063700         MOVE SPACE            TO MOD-IDBIL   (IX-RAD)                    
063800       ELSE                                                               
063900         MOVE ORAD-IDBIL       IN ORAD-WDE411                             
064000                               TO MOD-IDBIL (IX-RAD)                      
064100       END-IF                                                             
064200       MOVE KORD-IDDC          TO MOD-IDDC-RAD(IX-RAD)                    
064300                                                                          
064400       PERFORM CABA-BESTAM-STAT-FRAN-WDE601                               
064500       MOVE W-KDORDSTA         TO MOD-KDORDSTA(IX-RAD)                    
064600       MOVE W-TIDISPIN         TO MOD-TIDISPIN(IX-RAD)                    
064700       MOVE SEQC-IDORDNR5      TO MOD-IDORDNR7-LEV(IX-RAD)                
064800       ADD +1                  TO IX-RAD                                  
064900     ELSE                                                                 
065000       IF ORAD-IDKUNDRF-RO     IN ORAD-WDE411                             
065100                               NOT = '00000     '                         
065200           MOVE ORAD-IDKUNDRF-RO IN ORAD-WDE411 (1:5)                     
065300                                 TO MOD2-IDORDNR7-RAD(IX-RAD)             
065400        ELSE                                                              
065500           MOVE W-WDE4-IDORDNR5 TO MOD2-IDORDNR7-RAD(IX-RAD)              
065600       END-IF                                                             
065700       MOVE ORAD-KVBEART       IN ORAD-WDE411                             
065800                               TO MOD2-KVBEART-Q(IX-RAD)                  
065900       MOVE ORAD-KVAVBART      IN ORAD-WDE411                             
066000                               TO MOD2-KVPREAVB(IX-RAD)                   
066100       MOVE ORAD-KDORDKL       IN ORAD-WDE411                             
066200                               TO MOD2-KDORDKL (IX-RAD)                   
066300       MOVE ORAD-KDANNULL      IN ORAD-WDE411                             
066400                               TO MOD2-KDANNULL(IX-RAD)                   
066500       MOVE KORD-IDDC          TO MOD2-IDDC-RAD(IX-RAD)                   
066600                                  WS-IDDC                                 
066700                                  W-Q211KY-IDDC                           
066800       IF GOOD-DDC                                                        
066900         MOVE KORD-IDORDER     TO W-IDORDER                               
067000         MOVE ORAD-IDLEVNR     IN ORAD-WDE411                             
067100                               TO W-IDLEVNR                               
067200                                  W-Q211KY-IDLEVNR                        
067300         PERFORM IMS-GET-ORQI11-KVAL                                      
067400         IF SEGMENT-FINNS                                                 
067500           MOVE DIRL-KVDAGAR-DIFF    TO WS-KVDAGAR-DIFF                   
067600           MOVE WS-KVDAGAR-DIFF(2:2) TO MOD2-KVDAGAR-DIFF(IX-RAD)         
067700           IF DIRL-KVDAGAR-DIFF < ZERO                                    
067800             MOVE '-'             TO MOD2-IDTECKEN  (IX-RAD)              
067900           ELSE                                                           
068000             MOVE '+'             TO MOD2-IDTECKEN  (IX-RAD)              
068100           END-IF                                                         
068200         END-IF                                                           
068300         PERFORM IMS-GET-WDF106                                           
068400         IF SEGMENT-FINNS                                                 
068500           MOVE ADR-BELEV         TO MOD2-IDLEVNMN  (IX-RAD)              
068600         ELSE                                                             
068700           MOVE SPACE             TO MOD2-IDLEVNMN  (IX-RAD)              
068800         END-IF                                                           
068900       ELSE                                                               
069000         IF ORAD-IDBIL IN ORAD-WDE411 = LOW-VALUE                         
069100           MOVE SPACE          TO MOD2-IDBIL   (IX-RAD)                   
069200         ELSE                                                             
069300           MOVE ORAD-IDBIL     IN ORAD-WDE411                             
069400                               TO MOD2-IDBIL (IX-RAD)                     
069500         END-IF                                                           
069600       END-IF                                                             
069700                                                                          
069800       PERFORM CABA-BESTAM-STAT-FRAN-WDE601                               
069900       MOVE W-KDORDSTA         TO MOD2-KDORDSTA(IX-RAD)                   
070000       MOVE W-TIDISPIN         TO MOD2-TIDISPIN(IX-RAD)                   
070100       MOVE SEQC-IDORDNR5      TO MOD2-IDORDNR7-LEV(IX-RAD)               
070200       ADD +1                  TO IX-RAD                                  
070300     END-IF                                                               
070400     .                                                                    
070500     EJECT                                                                
070600 CABA-BESTAM-STAT-FRAN-WDE601 SECTION.                                    
070700                                                                          
070800     MOVE KORD-IDPRODNR        TO W-IDPRODNR                              
070900     PERFORM IMS-GU-WDE601                                                
071000                                                                          
071100     IF SEGMENT-FINNS                                                     
071200       IF VORD-KVORDRAD        =  VORD-KVORDRAD-PACK                      
071300         MOVE 'P '             TO W-KDORDSTA                              
071400         MOVE VORD-DABEGPAC(3:6) TO W-TIDISPIN                            
071500                                                                          
071600         IF VORD-KVKOLLI       =  VORD-KVKOLLI-FAKT AND                   
071700            VORD-KVKOLLI       =  VORD-KVKOLLI-LAST                       
071800           IF VORD-KVKOLLI > +0                                           
071900             MOVE 'SF'       TO W-KDORDSTA                                
072000             MOVE VORD-TIFAKT-SK TO W-TIDISPIN                            
072100           ELSE                                                           
072200             MOVE 'N '        TO W-KDORDSTA                               
072300             MOVE VORD-DABEGPAC(3:6) TO W-TIDISPIN                        
072400           END-IF                                                         
072500         ELSE                                                             
072600           IF VORD-KVKOLLI       =  VORD-KVKOLLI-FL AND                   
072700             VORD-KVKOLLI-FAKT  >= ZERO             AND                   
072800             VORD-KVKOLLI-LAST  >= ZERO                                   
072900             MOVE 'S'       TO W-KDORDSTA                                 
073000             MOVE VORD-TILASTN-SK TO W-TIDISPIN                           
073100           END-IF                                                         
073200         END-IF                                                           
073300       ELSE                                                               
073400         MOVE 'U '             TO W-KDORDSTA                              
073500         MOVE VORD-DABEGPAC(3:6) TO W-TIDISPIN                            
073600       END-IF                                                             
073700     ELSE                                                                 
073800       MOVE '  '               TO W-KDORDSTA                              
073900     END-IF                                                               
074000                                                                          
074100     IF ORAD-KDRADSTA          IN  ORAD-WDE411                            
074200                               >  3             AND                       
074300        W-KDORDSTA             =  'U '                                    
074400        IF ORAD-KVAVBART = ZERO                                           
074500          MOVE 'N '            TO W-KDORDSTA                              
074600        ELSE                                                              
074700          MOVE 'P '            TO W-KDORDSTA                              
074800        END-IF                                                            
074900        MOVE VORD-DABEGPAC(3:6) TO W-TIDISPIN                             
075000     END-IF                                                               
075100     .                                                                    
075200     EJECT                                                                
075300 CAC-SPARA-PACK-ORDER-NYCKLAR SECTION.                                    
075400                                                                          
075500     IF MID-IDVTYP NOT = '2'                                              
075600       MOVE SEQC-IDORDNR5      TO  MOD-IDORDNR7-NEXT                      
075700       MOVE 'P'                TO  MOD-FLSVAR-NEXT                        
075800       MOVE SEQC-IDPRODNR      TO  MOD-IDPRODNR-NEXT                      
075900       MOVE SEQC-IDPURAD       TO  MOD-IDRADNR-NEXT                       
076000     ELSE                                                                 
076100       MOVE SEQC-IDORDNR5      TO  MOD2-IDORDNR7-NEXT                     
076200       MOVE 'P'                TO  MOD2-FLSVAR-NEXT                       
076300       MOVE SEQC-IDPRODNR      TO  MOD2-IDPRODNR-NEXT                     
076400       MOVE SEQC-IDPURAD       TO  MOD2-IDRADNR-NEXT                      
076500     END-IF                                                               
076600     .                                                                    
076700     EJECT                                                                
076800 CB-BEHANDLA-ORDERRADKOE SECTION.                                         
076900                                                                          
077000     IF MID-IDORDNR7-NEXT       >   ZERO AND MID-FLSVAR-NEXT = 'O'        
077100         MOVE MID-IDRADNR-NEXT  TO  W-Q4B1KY-IDLOPNR                      
077200         MOVE SPACE             TO  W-Q4B1KY-IDKUNDRF                     
077300         MOVE MID-IDORDNR7-NEXT TO  W-Q4B1KY-IDORDNR7                     
077400         MOVE MID-IDORDER-NEXT  TO  W-Q4B1KY-IDORDER                      
077500         MOVE MID-IDDC-NEXT     TO  W-Q4B1KY-IDDC                         
077600         MOVE MID-ADLAGOMR-NEXT TO  W-Q4B1KY-ADLAGOMR                     
077700         MOVE MID-ADGANG-NEXT   TO  W-Q4B1KY-ADGANG                       
077800         MOVE MID-ADPLATS-NEXT  TO  W-Q4B1KY-ADPLATS                      
077900         PERFORM IMS-GET-ORQH01-KVAL                                      
078000      ELSE                                                                
078100         PERFORM IMS-GET-ORQH01-OKVAL                                     
078200     END-IF                                                               
078300                                                                          
078400     PERFORM UNTIL SEGMENT-SAKNAS    OR                                   
078500                   SEGMENT-SLUT      OR                                   
078600                   IX-RAD      > MAX-RAD                                  
078700                                                                          
078800         PERFORM CBA-SKAPA-WDQ401-NYCKEL                                  
078900         PERFORM IMS-GET-ORQF01-KVAL                                      
079000                                                                          
079100         PERFORM CBB-LAS-OHUV-ARBTAB                                      
079200                                                                          
079300         PERFORM CBC-REDIGERA-ORDERRAD                                    
079400         ADD +1                TO IX-RAD                                  
079500                                                                          
079600         PERFORM IMS-GET-ORQH01-OKVAL                                     
079700                                                                          
079800     END-PERFORM                                                          
079900                                                                          
080000     IF IX-RAD                 > MAX-RAD     AND                          
080100        SEGMENT-FINNS                                                     
080200         PERFORM CBD-SPARA-ORDERRAD-NYCKLAR                               
080300     END-IF                                                               
080400     .                                                                    
080500     EJECT                                                                
080600 CBA-SKAPA-WDQ401-NYCKEL SECTION.                                         
080700                                                                          
080800     MOVE Q4-SEQB-IDORDER      TO W-Q401KY-IDORDER                        
080900     MOVE Q4-SEQB-IDDC         TO W-Q401KY-IDDC                           
081000     MOVE Q4-SEQB-ADLAGOMR     TO W-Q401KY-ADLAGOMR                       
081100     MOVE Q4-SEQB-ADGANG       TO W-Q401KY-ADGANG                         
081200     MOVE Q4-SEQB-ADPLATS      TO W-Q401KY-ADPLATS                        
081300     MOVE Q4-SEQB-IDARTNR      TO W-Q401KY-IDARTNR                        
081400     MOVE Q4-SEQB-IDLOPNR      TO W-Q401KY-IDLOPNR                        
081500     .                                                                    
081600     EJECT                                                                
081700 CBB-LAS-OHUV-ARBTAB SECTION.                                             
081800                                                                          
081900     MOVE ORAD-IDORDER         IN ORAD-WDQ401                             
082000                               TO W-IDORDER                               
082100                                                                          
082200     MOVE ORAD-IDDC            IN ORAD-WDQ401                             
082300                               TO WS-IDDC                                 
082400     IF GOOD-DDC                                                          
082500       MOVE WC-CDC-SE          TO W-IDDC                                  
082600     ELSE                                                                 
082700       MOVE ORAD-IDDC          TO W-IDDC                                  
082800     END-IF                                                               
082900     PERFORM IMS-GET-ORQI12-KVAL                                          
083000     .                                                                    
083100     EJECT                                                                
083200 CBC-REDIGERA-ORDERRAD SECTION.                                           
083300                                                                          
083400     IF MID-IDVTYP NOT = '2'                                              
083500       IF OHUV-FLKLAR             = NEJ                                   
083600           MOVE ZERO              TO MOD-TIDISPIN(IX-RAD)                 
083700           MOVE ZERO              TO MOD-KVBEART-Q(IX-RAD)                
083800           MOVE ZERO              TO MOD-KVPREAVB(IX-RAD)                 
083900           MOVE ORAD-KDORDKL      IN ORAD-WDQ401                          
084000                                  TO MOD-KDORDKL(IX-RAD)                  
084100           MOVE ORAD-IDDC         IN ORAD-WDQ401                          
084200                                  TO MOD-IDDC-RAD(IX-RAD)                 
084300           IF ORAD-IDBIL          IN ORAD-WDQ401 = LOW-VALUE              
084400             MOVE SPACE           TO MOD-IDBIL   (IX-RAD)                 
084500           ELSE                                                           
084600             MOVE ORAD-IDBIL      IN ORAD-WDQ401                          
084700                                  TO MOD-IDBIL (IX-RAD)                   
084800           END-IF                                                         
084900           MOVE 'E '              TO MOD-KDORDSTA(IX-RAD)                 
085000        ELSE                                                              
085100           MOVE ARB-DATRPAVD(3:6) TO W-TIAAMMDD                           
085200           MOVE W-TIAAMMDD        TO MOD-TIDISPIN(IX-RAD)                 
085300           MOVE ORAD-KVBEART-Q    IN ORAD-WDQ401                          
085400                                  TO MOD-KVBEART-Q(IX-RAD)                
085500           MOVE ORAD-KVPREAVB     IN ORAD-WDQ401                          
085600                                  TO MOD-KVPREAVB(IX-RAD)                 
085700           MOVE ORAD-KDORDKL      IN ORAD-WDQ401                          
085800                                  TO MOD-KDORDKL(IX-RAD)                  
085900           MOVE ORAD-IDDC         IN ORAD-WDQ401                          
086000                                  TO MOD-IDDC-RAD(IX-RAD)                 
086100           IF ORAD-IDBIL          IN ORAD-WDQ401 = LOW-VALUE              
086200             MOVE SPACE           TO MOD-IDBIL   (IX-RAD)                 
086300           ELSE                                                           
086400             MOVE ORAD-IDBIL      IN ORAD-WDQ401                          
086500                                  TO MOD-IDBIL (IX-RAD)                   
086600           END-IF                                                         
086700           MOVE 'R '              TO MOD-KDORDSTA(IX-RAD)                 
086800       END-IF                                                             
086900       IF ORAD-IDKUNDRF-RO        IN ORAD-WDQ401                          
087000                                  NOT = '0000000   '                      
087100           MOVE ORAD-IDKUNDRF-RO  IN ORAD-WDQ401 (1:7)                    
087200                                  TO MOD-IDORDNR7-RAD(IX-RAD)             
087300        ELSE                                                              
087400           MOVE ORAD-IDORDNR7     IN ORAD-WDQ401                          
087500                                  TO MOD-IDORDNR7-RAD(IX-RAD)             
087600       END-IF                                                             
087700       MOVE ORAD-IDORDNR7         IN ORAD-WDQ401                          
087800                                  TO MOD-IDORDNR7-LEV(IX-RAD)             
087900     ELSE                                                                 
088000       IF OHUV-FLKLAR             = NEJ                                   
088100           MOVE ZERO              TO MOD2-TIDISPIN(IX-RAD)                
088200           MOVE ZERO              TO MOD2-KVBEART-Q(IX-RAD)               
088300           MOVE ZERO              TO MOD2-KVPREAVB(IX-RAD)                
088400           MOVE ORAD-KDORDKL      IN ORAD-WDQ401                          
088500                                  TO MOD2-KDORDKL(IX-RAD)                 
088600           MOVE ORAD-IDDC         IN ORAD-WDQ401                          
088700                                  TO MOD2-IDDC-RAD(IX-RAD)                
088800           IF ORAD-IDBIL          IN ORAD-WDQ401 = LOW-VALUE              
088900             MOVE SPACE           TO MOD2-IDBIL  (IX-RAD)                 
089000           ELSE                                                           
089100             MOVE ORAD-IDBIL      IN ORAD-WDQ401                          
089200                                  TO MOD2-IDBIL (IX-RAD)                  
089300           END-IF                                                         
089400           MOVE 'E '              TO MOD2-KDORDSTA(IX-RAD)                
089500           MOVE ZERO              TO MOD2-KDANNULL(IX-RAD)                
089600        ELSE                                                              
089700           MOVE ARB-DATRPAVD(3:6) TO W-TIAAMMDD                           
089800           MOVE W-TIAAMMDD        TO MOD2-TIDISPIN(IX-RAD)                
089900           MOVE ORAD-KVBEART-Q    IN ORAD-WDQ401                          
090000                                  TO MOD2-KVBEART-Q(IX-RAD)               
090100           MOVE ORAD-KVPREAVB     IN ORAD-WDQ401                          
090200                                  TO MOD2-KVPREAVB(IX-RAD)                
090300           MOVE ORAD-KDORDKL      IN ORAD-WDQ401                          
090400                                  TO MOD2-KDORDKL(IX-RAD)                 
090500           MOVE ORAD-IDDC         IN ORAD-WDQ401                          
090600                                  TO MOD2-IDDC-RAD(IX-RAD)                
090700           IF ORAD-IDBIL          IN ORAD-WDQ401 = LOW-VALUE              
090800             MOVE SPACE           TO MOD2-IDBIL  (IX-RAD)                 
090900           ELSE                                                           
091000             MOVE ORAD-IDBIL      IN ORAD-WDQ401                          
091100                                  TO MOD2-IDBIL (IX-RAD)                  
091200           END-IF                                                         
091300           MOVE 'R '              TO MOD2-KDORDSTA(IX-RAD)                
091400           MOVE ZERO              TO MOD2-KDANNULL(IX-RAD)                
091500       END-IF                                                             
091600       IF ORAD-IDKUNDRF-RO        IN ORAD-WDQ401                          
091700                                  NOT = '0000000   '                      
091800           MOVE ORAD-IDKUNDRF-RO  IN ORAD-WDQ401 (1:7)                    
091900                                  TO MOD2-IDORDNR7-RAD(IX-RAD)            
092000        ELSE                                                              
092100           MOVE ORAD-IDORDNR7     IN ORAD-WDQ401                          
092200                                  TO MOD2-IDORDNR7-RAD(IX-RAD)            
092300       END-IF                                                             
092400       MOVE ORAD-IDORDNR7         IN ORAD-WDQ401                          
092500                                  TO MOD2-IDORDNR7-LEV(IX-RAD)            
092600     END-IF                                                               
092700     .                                                                    
092800     EJECT                                                                
092900 CBD-SPARA-ORDERRAD-NYCKLAR SECTION.                                      
093000                                                                          
093100     IF MID-IDVTYP NOT = '2'                                              
093200       MOVE Q4-SEQB-IDORDNR7   TO  MOD-IDORDNR7-NEXT                      
093300       MOVE 'O'                TO  MOD-FLSVAR-NEXT                        
093400       MOVE Q4-SEQB-IDLOPNR    TO  MOD-IDRADNR-NEXT                       
093500       MOVE Q4-SEQB-IDDC       TO  MOD-IDDC-NEXT                          
093600       MOVE Q4-SEQB-ADLAGOMR   TO  MOD-ADLAGOMR-NEXT                      
093700       MOVE Q4-SEQB-ADGANG     TO  MOD-ADGANG-NEXT                        
093800       MOVE Q4-SEQB-ADPLATS    TO  MOD-ADPLATS-NEXT                       
093900       MOVE Q4-SEQB-IDORDER    TO  MOD-IDORDER-NEXT                       
094000     ELSE                                                                 
094100       MOVE Q4-SEQB-IDORDNR7   TO  MOD2-IDORDNR7-NEXT                     
094200       MOVE 'O'                TO  MOD2-FLSVAR-NEXT                       
094300       MOVE Q4-SEQB-IDLOPNR    TO  MOD2-IDRADNR-NEXT                      
094400       MOVE Q4-SEQB-IDDC       TO  MOD2-IDDC-NEXT                         
094500       MOVE Q4-SEQB-ADLAGOMR   TO  MOD2-ADLAGOMR-NEXT                     
094600       MOVE Q4-SEQB-ADGANG     TO  MOD2-ADGANG-NEXT                       
094700       MOVE Q4-SEQB-ADPLATS    TO  MOD2-ADPLATS-NEXT                      
094800       MOVE Q4-SEQB-IDORDER    TO  MOD2-IDORDER-NEXT                      
094900     END-IF                                                               
095000     .                                                                    
095100     EJECT                                                                
095200 CC-BEHANDLA-RESTORDER SECTION.                                           
095300                                                                          
095400     MOVE LOW-VALUE            TO  W-WDA501KY-MIN-X                       
095500                                                                          
095600     MOVE HIGH-VALUE           TO  W-WDA501KY-MAX-X                       
095700                                                                          
095800     MOVE MID-IDDISTR          TO  W-A501KY-MIN-IDDISTR                   
095900                                   W-A501KY-MAX-IDDISTR                   
096000                                   W-A501KY-IDDISTR                       
096100                                                                          
096200     MOVE MID-IDKUNDNR         TO  W-A501KY-MIN-IDKUNDNR                  
096300                                   W-A501KY-MAX-IDKUNDNR                  
096400                                   W-A501KY-IDKUNDNR                      
096500                                                                          
096600     MOVE MID-IDARTNR          TO  W-A501KY-MIN-IDARTNR                   
096700                                   W-A501KY-MAX-IDARTNR                   
096800                                   W-A501KY-IDARTNR                       
096900                                   W-IDARTNR                              
097000                                                                          
097100     IF MID-IDORDNR7-NEXT (3:5)  > ZERO AND MID-FLSVAR-NEXT = 'R'         
097200         MOVE MID-IDORDNR7-NEXT (3:5) TO  W-A501KY-IDORDNR5               
097300         MOVE MID-IDRADNR-NEXT        TO  W-A501KY-IDLOPNR                
097400         PERFORM IMS-GET-ORDP01-KVAL                                      
097500      ELSE                                                                
097600         PERFORM IMS-GET-ORDP01-OKVAL                                     
097700     END-IF                                                               
097800                                                                          
097900     PERFORM UNTIL SEGMENT-SAKNAS    OR                                   
098000                   SEGMENT-SLUT      OR                                   
098100                   IX-RAD      > MAX-RAD                                  
098200         IF RAD-KDSTARAD       < +4                                       
098300             PERFORM CCA-REDIGERA-RO-RAD                                  
098400             ADD +1            TO IX-RAD                                  
098500         END-IF                                                           
098600         PERFORM IMS-GET-ORDP01-OKVAL                                     
098700     END-PERFORM                                                          
098800                                                                          
098900     IF IX-RAD                 > MAX-RAD   AND                            
099000        SEGMENT-FINNS                                                     
099100         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                  
099200                       RAD-KDSTARAD < 4                                   
099300             PERFORM IMS-GET-ORDP01-OKVAL                                 
099400         END-PERFORM                                                      
099500         IF SEGMENT-FINNS                                                 
099600             PERFORM CCB-SPARA-RESTORDER-NYCKLAR                          
099700         END-IF                                                           
099800     END-IF                                                               
099900     .                                                                    
100000     EJECT                                                                
100100 CCA-REDIGERA-RO-RAD SECTION.                                             
100200                                                                          
100300     IF MID-IDVTYP NOT = '2'                                              
100400       MOVE RAD-IDORDNR5           TO MOD-IDORDNR7-RAD(IX-RAD)            
100500                                      MOD-IDORDNR7-LEV(IX-RAD)            
100600                                                                          
100700       IF RAD-KDTPOTYP > ZERO                                             
100800         MOVE RAD-TITPO            TO W-TIDISPIN                          
100900         MOVE 'T '                 TO MOD-KDORDSTA (IX-RAD)               
101000         IF RAD-DARODAT NOT = ZERO                                        
101100           MOVE 'B '             TO MOD-KDORDSTA (IX-RAD)                 
101200         END-IF                                                           
101300       ELSE                                                               
101400         PERFORM IMS-GU-ARTC11                                            
101500         MOVE JA                   TO SW-ARTC11-LAEST                     
101600         MOVE 'B '                 TO MOD-KDORDSTA(IX-RAD)                
101700         MOVE RAD-IDDC             TO WS-IDDC                             
101900         IF NDC                                                           
102000           PERFORM CCAA-HAEMTA-TIBERANK                                   
102100                                                                          
102200           IF ETA-SVAR-OK = JA                                            
102300             MOVE ETA-TIAAMMDD-SVAR TO W-TIDISPIN                         
102400           ELSE                                                           
102500             MOVE ZERO            TO  W-TIDISPIN                          
102600           END-IF                                                         
102700         ELSE                                                             
102800           MOVE CLAG-TIDISPIN     TO W-TIDISPIN                           
102900         END-IF                                                           
103000       END-IF                                                             
103100                                                                          
103200       MOVE W-TIDISPIN         TO MOD-TIDISPIN(IX-RAD)                    
103300       MOVE RAD-KVBEART-Q      TO MOD-KVBEART-Q(IX-RAD)                   
103400       MOVE RAD-KVART          TO MOD-KVPREAVB(IX-RAD)                    
103500       MOVE RAD-KDORDKL        TO MOD-KDORDKL(IX-RAD)                     
103600       MOVE RAD-IDDC           TO MOD-IDDC-RAD(IX-RAD)                    
103700       MOVE SPACE              TO MOD-IDBIL(IX-RAD)                       
103800     ELSE                                                                 
103900       MOVE RAD-IDORDNR5       TO MOD2-IDORDNR7-RAD(IX-RAD)               
104000                                  MOD2-IDORDNR7-LEV(IX-RAD)               
104100                                                                          
104200       IF RAD-KDTPOTYP > ZERO                                             
104300         MOVE RAD-TITPO          TO W-TIDISPIN                            
104400         MOVE 'T '               TO MOD2-KDORDSTA (IX-RAD)                
104500         IF RAD-DARODAT NOT = ZERO                                        
104600           MOVE 'B '             TO MOD2-KDORDSTA (IX-RAD)                
104700         END-IF                                                           
104800                                                                          
104900       ELSE                                                               
105000         PERFORM IMS-GU-ARTC11                                            
105100         MOVE JA                 TO SW-ARTC11-LAEST                       
105200         MOVE 'B '               TO MOD2-KDORDSTA(IX-RAD)                 
105300         MOVE RAD-IDDC           TO WS-IDDC                               
105500         IF NDC                                                           
105600           PERFORM CCAA-HAEMTA-TIBERANK                                   
105700                                                                          
105800           IF ETA-SVAR-OK = JA                                            
106000             IF NDC-NA OR NDC-CN                                          
106100               MOVE ETA-TIAAMMDD-SVAR TO W-TIDISPIN                       
106200             ELSE                                                         
106300               IF ETA-KVAVIS-ETA > +0                                     
106400                 MOVE ETA-TIAAMMDD-SVAR TO W-TIDISPIN                     
106500               ELSE                                                       
106600                 MOVE ZERO        TO  W-TIDISPIN                          
106700               END-IF                                                     
106800             END-IF                                                       
106900           ELSE                                                           
107000             MOVE ZERO            TO  W-TIDISPIN                          
107100           END-IF                                                         
107200         ELSE                                                             
107300           MOVE CLAG-TIDISPIN     TO W-TIDISPIN                           
107400         END-IF                                                           
107500       END-IF                                                             
107600                                                                          
107700       MOVE W-TIDISPIN         TO MOD2-TIDISPIN(IX-RAD)                   
107800       MOVE RAD-KVBEART-Q      TO MOD2-KVBEART-Q(IX-RAD)                  
107900       MOVE RAD-KVART          TO MOD2-KVPREAVB(IX-RAD)                   
108000       MOVE RAD-KDORDKL        TO MOD2-KDORDKL(IX-RAD)                    
108100       MOVE RAD-IDDC           TO MOD2-IDDC-RAD(IX-RAD)                   
108200       MOVE SPACE              TO MOD2-IDBIL(IX-RAD)                      
108300       MOVE ZERO               TO MOD2-KDANNULL(IX-RAD)                   
108400     END-IF                                                               
108500                                                                          
108600     IF RAD-KDSTARAD = '1' AND                                            
108700        RAD-FLTPOBEK = JA                                                 
108800       MOVE RAD-IDARTNR        TO  W-IDARTNR                              
108900       IF SW-ARTC11-LAEST = NEJ                                           
109000         PERFORM IMS-GU-ARTC11                                            
109100       END-IF                                                             
109200       IF SEGMENT-FINNS                                                   
109300         PERFORM CCAB-TIDIGAST-ANNULL-DATUM                               
109400       END-IF                                                             
109500     END-IF                                                               
109600     MOVE NEJ                  TO SW-ARTC11-LAEST                         
109700     .                                                                    
109800     EJECT                                                                
109900 CCAA-HAEMTA-TIBERANK SECTION.                                            
110000                                                                          
110100     MOVE '612'                TO ETA-KDCALL                              
110200     MOVE RAD-IDDC             TO ETA-IDDC-REC                            
110300     MOVE RAD-IDARTNR          TO ETA-IDARTNR                             
110400     MOVE SPACE                TO ETA-IDLEVNR                             
110500     MOVE ZERO                 TO ETA-KDFRAKT                             
110600     MOVE RAD-DARODAT(3:6)     TO ETA-TIAAMMDD-ANROP                      
110700                                  WS-ETA-DATUM                            
110800     IF WS-ETA-DATUM-AAR > 50                                             
110900        MOVE 19                TO ETA-TISEKEL-ANROP                       
111000     ELSE                                                                 
111100        MOVE 20                TO ETA-TISEKEL-ANROP                       
111200     END-IF                                                               
111300                                                                          
111400     CALL W218ETA  USING ETA-W218LETA                                     
111500                         ETA-ARTC-PCB ETA-WDK7-PCB                        
111600                         ETA-INLC-PCB ETA-LEVA-PCB                        
111700                         ETA-WDB6-PCB ETA-WDD9-PCB                        
111800     .                                                                    
111900                                                                          
112000     EJECT                                                                
112100 CCAB-TIDIGAST-ANNULL-DATUM SECTION.                                      
112200                                                                          
112300     MOVE RAD-TITPO    TO DAT-I-TIDATUM                                   
112400     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
112500     CALL WDATKONV USING DAT-KDDATFORM                                    
112600                         DAT-I-TIDATUM                                    
112700                         DAT-O-TIDATUM                                    
112800                         DAT-KDSVAR                                       
112900                                                                          
113000     IF DAT-KDSVAR-OK                                                     
113100        MOVE DAT-TIAAVVD TO W-TIAAVVD                                     
113200        IF W-TIVV < CLAG-KVFRYSTI                                         
113300           IF W-TIAA = ZERO                                               
113400             MOVE 99 TO W-TIAA                                            
113500           ELSE                                                           
113600             COMPUTE W-TIAA = W-TIAA - 1                                  
113700           END-IF                                                         
113800           COMPUTE CLAG-KVFRYSTI = CLAG-KVFRYSTI - W-TIVV                 
113900           COMPUTE W-TIVV = 52 - CLAG-KVFRYSTI                            
114000        ELSE                                                              
114100           IF W-TIVV = CLAG-KVFRYSTI                                      
114200              IF W-TIAA = ZERO                                            
114300                MOVE 99 TO W-TIAA                                         
114400              ELSE                                                        
114500                COMPUTE W-TIAA = W-TIAA - 1                               
114600              END-IF                                                      
114700              MOVE 52 TO W-TIVV                                           
114800           ELSE                                                           
114900              COMPUTE W-TIVV = W-TIVV - CLAG-KVFRYSTI                     
115000           END-IF                                                         
115100        END-IF                                                            
115200        MOVE W-TIAAVVD    TO DAT-I-TIDATUM                                
115300        MOVE 'AAVVD'      TO DAT-KDDATFORM                                
115400        CALL WDATKONV USING DAT-KDDATFORM                                 
115500                            DAT-I-TIDATUM                                 
115600                            DAT-O-TIDATUM                                 
115700                            DAT-KDSVAR                                    
115800        IF DAT-KDSVAR-OK                                                  
115900          IF MID-IDVTYP NOT = '2'                                         
116000           MOVE DAT-TIAAMMDD    TO MOD-TIANNULL(IX-RAD)                   
116100          ELSE                                                            
116200           MOVE DAT-TIAAMMDD    TO MOD2-TIANNULL(IX-RAD)                  
116300          END-IF                                                          
116400        ELSE                                                              
116500           CALL FELLOG                                                    
116600           MOVE 'FEL AAVVD PÅ WDA5 I CCAA-SECTION' TO FELTEXT             
116700        END-IF                                                            
116800     ELSE                                                                 
116900        CALL FELLOG                                                       
117000        MOVE 'FEL TITPO PÅ WDA5 I CCAA-SECTION' TO FELTEXT                
117100     END-IF                                                               
117200     .                                                                    
117300     EJECT                                                                
117400 CCB-SPARA-RESTORDER-NYCKLAR SECTION.                                     
117500                                                                          
117600     IF MID-IDVTYP NOT = '2'                                              
117700       MOVE RAD-IDORDNR5       TO  MOD-IDORDNR7-NEXT                      
117800       MOVE 'R'                TO  MOD-FLSVAR-NEXT                        
117900       MOVE RAD-IDLOPNR        TO  MOD-IDRADNR-NEXT                       
118000     ELSE                                                                 
118100       MOVE RAD-IDORDNR5       TO  MOD2-IDORDNR7-NEXT                     
118200       MOVE 'R'                TO  MOD2-FLSVAR-NEXT                       
118300       MOVE RAD-IDLOPNR        TO  MOD2-IDRADNR-NEXT                      
118400     END-IF                                                               
118500     .                                                                    
118600     EJECT                                                                
118700 D-KONTROLLERA-OM-SIDA-TOM SECTION.                                       
118800                                                                          
118900     IF IX-RAD                 = 1                                        
119000       IF MID-IDVTYP NOT = '2'                                            
119100         MOVE 'B10'            TO MOD-IDMFSFEL                            
119200       ELSE                                                               
119300         MOVE 'B10'            TO MOD2-IDMFSFEL                           
119400       END-IF                                                             
119500     END-IF                                                               
119600     .                                                                    
119700     EJECT                                                                
119800 E-BERAKNA-MAX-MOD-LANGD SECTION.                                         
119900                                                                          
120000     IF MID-IDVTYP NOT = '2'                                              
120100       COMPUTE MAX-MOD-LANGD = LENGTH OF MOD-W9O33501 + 4                 
120200       MOVE 13                 TO  IX-RAD                                 
120300       PERFORM UNTIL IX-RAD    = 0                                        
120400           IF MOD-IDORDNR7-RAD(IX-RAD) = ZERO                             
120500               SUBTRACT +58    FROM MAX-MOD-LANGD                         
120600               SUBTRACT +1     FROM IX-RAD                                
120700            ELSE                                                          
120800               MOVE ZERO       TO IX-RAD                                  
120900           END-IF                                                         
121000       END-PERFORM                                                        
121100     ELSE                                                                 
121200       COMPUTE MAX-MOD-LANGD = LENGTH OF MOD2-W9O33502 + 4                
121300       MOVE 13                 TO  IX-RAD                                 
121400       PERFORM UNTIL IX-RAD    = 0                                        
121500           IF MOD2-IDORDNR7-RAD(IX-RAD) = ZERO                            
121600               SUBTRACT +59    FROM MAX-MOD-LANGD                         
121700               SUBTRACT +1     FROM IX-RAD                                
121800            ELSE                                                          
121900               MOVE ZERO       TO IX-RAD                                  
122000           END-IF                                                         
122100       END-PERFORM                                                        
122200     END-IF                                                               
122300     .                                                                    
122400     EJECT                                                                
122500                                                                          
122600* IMS SEKTIONER                                                           
122700     SKIP3                                                                
122800 IMS-GET-MSG SECTION.                                                     
122900                                                                          
123000     MOVE '  QC' TO GODK-STATUSKODER                                      
123100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
123200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
123300     PERFORM IMS-STATUSKONTROLL                                           
123400     .                                                                    
123500     SKIP3                                                                
123600 IMS-INSERT-MSG SECTION.                                                  
123700                                                                          
123800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
123900     MOVE SPACE TO GODK-STATUSKODER                                       
124000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
124100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
124200     PERFORM IMS-STATUSKONTROLL                                           
124300     .                                                                    
124400     EJECT                                                                
124500 IMS-GET-ORQF01-KVAL SECTION.                                             
124600                                                                          
124700     STRING 'WLORQF01(WDQ401KY =' W-WDQ401KY-X ')'                        
124800            DELIMITED BY SIZE INTO SSA1                                   
124900     MOVE '  GE' TO GODK-STATUSKODER                                      
125000     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-ORQF01 SSA1                    
125100     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
125200     PERFORM IMS-STATUSKONTROLL                                           
125300     .                                                                    
125400     EJECT                                                                
125500 IMS-GET-ORQH01-KVAL SECTION.                                             
125600                                                                          
125700     STRING 'WLORQH01(WDQ4B1KY =' W-WDQ4B1KY-X ')'                        
125800            DELIMITED BY SIZE INTO SSA1                                   
125900     MOVE '  GE' TO GODK-STATUSKODER                                      
126000     CALL CBLTDLI USING GU ORQH-PCB DLI-IO-ORQH01 SSA1                    
126100     MOVE ORQH-STATUS-CODE TO STATUS-WS                                   
126200     PERFORM IMS-STATUSKONTROLL                                           
126300     .                                                                    
126400     EJECT                                                                
126500 IMS-GET-ORQH01-OKVAL SECTION.                                            
126600                                                                          
126700     STRING 'WLORQH01(WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
126800                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
126900                    '&IDGMTREF>=' W-Q4B1KY-MIN-IDGMTREF                   
127000                    '&IDGMTREF<=' W-Q4B1KY-MAX-IDGMTREF ')'               
127100            DELIMITED BY SIZE INTO SSA1                                   
127200     MOVE '  GE' TO GODK-STATUSKODER                                      
127300     CALL CBLTDLI USING GN ORQH-PCB DLI-IO-ORQH01 SSA1                    
127400     MOVE ORQH-STATUS-CODE TO STATUS-WS                                   
127500     PERFORM IMS-STATUSKONTROLL                                           
127600     .                                                                    
127700     EJECT                                                                
127800 IMS-GET-ORQI11-KVAL SECTION.                                             
127900                                                                          
128000     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
128100            DELIMITED BY SIZE INTO SSA1                                   
128200     STRING 'WLORQI11(WDQ211KY =' W-WDQ211KY-X ')'                        
128300            DELIMITED BY SIZE INTO SSA2                                   
128400     MOVE '  GE' TO GODK-STATUSKODER                                      
128500     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-ORQI11 SSA1 SSA2               
128600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
128700     PERFORM IMS-STATUSKONTROLL                                           
128800     .                                                                    
128900     EJECT                                                                
129000 IMS-GET-ORQI12-KVAL SECTION.                                             
129100                                                                          
129200     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
129300            DELIMITED BY SIZE INTO SSA1                                   
129400     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
129500            DELIMITED BY SIZE INTO SSA2                                   
129600     MOVE '  ' TO GODK-STATUSKODER                                        
129700     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-ORQI12 SSA1 SSA2               
129800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
129900     PERFORM IMS-STATUSKONTROLL                                           
130000     .                                                                    
130100     EJECT                                                                
130200 IMS-GET-ORDP01-KVAL SECTION.                                             
130300                                                                          
130400     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
130500            DELIMITED BY SIZE INTO SSA1                                   
130600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
130700     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-ORDP01 SSA1                    
130800     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
130900     PERFORM IMS-STATUSKONTROLL                                           
131000     .                                                                    
131100     EJECT                                                                
131200 IMS-GET-ORDP01-OKVAL SECTION.                                            
131300                                                                          
131400     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN-X                        
131500                    '&WDA501KY<=' W-WDA501KY-MAX-X                        
131600                    '&IDARTNR  =' W-IDARTNR-X ')'                         
131700            DELIMITED BY SIZE INTO SSA1                                   
131800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
131900     CALL CBLTDLI USING GN ORDP-PCB DLI-IO-ORDP01 SSA1                    
132000     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
132100     PERFORM IMS-STATUSKONTROLL                                           
132200     .                                                                    
132300     EJECT                                                                
132400 IMS-GET-WDE401-11 SECTION.                                               
132500                                                                          
132600     STRING 'WDE401  *D(WDE401KY =' W-WDE401KY-X ')'                      
132700            DELIMITED BY SIZE INTO SSA1                                   
132800     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
132900            DELIMITED BY SIZE INTO SSA2                                   
133000     MOVE '  ' TO GODK-STATUSKODER                                        
133100     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E40120 SSA1 SSA2               
133200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
133300     PERFORM IMS-STATUSKONTROLL                                           
133400     .                                                                    
133500     EJECT                                                                
133600 IMS-GET-WDE4C-KVAL SECTION.                                              
133700                                                                          
133800     STRING 'WDE4C1  (WDE4C1KY =' W-WDE4C1KY-X                            
133900                    '&IDDISTR  =' W-IDDISTR-X                             
134000                    '&IDKUNDNR =' W-IDKUNDNR-X                            
134100                    '&IDKUNDRF =' W-IDKUNDRF-X ')'                        
134200            DELIMITED BY SIZE INTO SSA1                                   
134300     MOVE '  GE'  TO GODK-STATUSKODER                                     
134400     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-WDE4C SSA1                    
134500     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
134600     PERFORM IMS-STATUSKONTROLL                                           
134700     .                                                                    
134800     EJECT                                                                
134900 IMS-GET-WDE4C-OKVAL SECTION.                                             
135000                                                                          
135100     STRING 'WDE4C1  (WDE4C1KY>=' W-WDE4C1KY-MIN-X                        
135200                    '&WDE4C1KY<=' W-WDE4C1KY-MAX-X                        
135300                    '&IDDISTR  =' W-IDDISTR-X                             
135400                    '&IDKUNDNR =' W-IDKUNDNR-X ')'                        
135500            DELIMITED BY SIZE INTO SSA1                                   
135600     MOVE '  GE'  TO GODK-STATUSKODER                                     
135700     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-WDE4C SSA1                    
135800     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
135900     PERFORM IMS-STATUSKONTROLL                                           
136000     .                                                                    
136100     EJECT                                                                
136200 IMS-GU-ARTC11 SECTION.                                                   
136300                                                                          
136400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
136500            DELIMITED BY SIZE INTO SSA1                                   
136600     MOVE   'WLARTC11'         TO SSA2                                    
136700     MOVE '  GE' TO GODK-STATUSKODER                                      
136800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2               
136900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
137000     PERFORM IMS-STATUSKONTROLL                                           
137100     .                                                                    
137200     EJECT                                                                
137300 IMS-GU-WDE601 SECTION.                                                   
137400                                                                          
137500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
137600            DELIMITED BY SIZE INTO SSA1                                   
137700     MOVE '  GE' TO GODK-STATUSKODER                                      
137800     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
137900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
138000     PERFORM IMS-STATUSKONTROLL                                           
138100     .                                                                    
138200     EJECT                                                                
138300 IMS-GET-WDF106 SECTION.                                                  
138400                                                                          
138500     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
138600          DELIMITED BY SIZE INTO SSA1                                     
138700     MOVE 'WLLEVA14 ' TO SSA2                                             
138800     MOVE '  GE' TO GODK-STATUSKODER                                      
138900     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-LEVA14 SSA1 SSA2               
139000     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
139100     PERFORM IMS-STATUSKONTROLL                                           
139200     .                                                                    
139300     EJECT                                                                
139400 IMS-STATUSKONTROLL SECTION.                                              
139500                                                                          
139600     SET STATUS-IX TO 1                                                   
139700     SEARCH GODK-STATUS                                                   
139800       AT END                                                             
139900         CALL  FELLOG                                                     
140000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
140100         CONTINUE                                                         
140200     END-SEARCH                                                           
140300     .                                                                    
