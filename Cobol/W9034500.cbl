000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9034500.                                                
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
001400*                                                                         
001500*        PROGRAMMET ANROPAR W218ETA FÖR HÄMTNING                          
001600*        AV ETA-DATUM/NDC-LAGER.                                          
001700*       (ESTIMATED TIME AVAILABLE)                                        
001800*                                                                         
001900*        LÄSER   WDQ2                                                     
002000*                WDQ3                                                     
002100*                WDQ4                                                     
002200*                WDE4                                                     
002300*                WDA5                                                     
002400*                WDF1                                                     
002500*                                                                         
002600*        OM SIDAN INTE FULL OCH DET INTE FINNS FLER SEGMENT               
002700*        PÅ WDQ4 LÄSER MAN WDE4. OM SIDAN FORTFARANDE INTE                
002800*        ÄR FULL OCH DET INTE FINNS FLER SEGMENT PÅ WDE4                  
002900*        LÄSER MAN WDA5.                                                  
003000*                                                                         
003100*        BLIR SIDAN FULL OCH DET FINNS FLER SEGMENT PÅ                    
003200*        NÅGON AV BASERNA SPARAS DET EN FLAGGA OCH ETT RAD-               
003300*        NUMMER SOM TALAR OM PÅ VILKEN BAS OCH VAR I BASEN                
003400*        MAN SKALL FORTSÄTTA ATT LÄSA VID EN EVENTUELL                    
003500*        BLÄDDRING.                                                       
003600*                                                                         
003700*    INDATA.                                                              
003800*        TRANSAKTION: W90345T                                             
003900*        MID:         W9I34501                                            
004000*                                                                         
004100*    UTDATA.                                                              
004200*        MOD:         W9O34502                                            
004300     SKIP3                                                                
004400 ENVIRONMENT DIVISION.                                                    
004500     SKIP3                                                                
004600 DATA DIVISION.                                                           
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100     SKIP3                                                                
005200 77  IDPGM                       PIC X(8)    VALUE 'W9034500'.            
005300                                                                          
005400 77    JA                        PIC X       VALUE 'J'.                   
005500 77    NEJ                       PIC X       VALUE 'N'.                   
005600 77    FELTEXT                   PIC X(64)   VALUE SPACE.                 
005700                                                                          
005800 77   WS-KVDAGAR-DIFF            PIC 9(3).                                
005900 77   W-KDORDSTA                 PIC X(2)    VALUE SPACE.                 
006000 77   W-KVPREAVB                 PIC S9(7)             COMP-3.            
006100 77   MAX-RAD                    PIC S9(7)   VALUE +13 COMP-3.            
006200 77   MAX-MOD-LANGD              PIC S9(7)   VALUE +0  COMP-3.            
006300                                                                          
006400 77   W-TIAAMMDD                 PIC S9(6).                               
006500 77   W-TIPACKN                  PIC S9(6).                               
006600 77   W-TIDISPIN                 PIC S9(6).                               
006700 77   W-WDE4-IDORDNR5            PIC S9(5)   VALUE ZERO  COMP-3.          
006800 77   W-WDE4-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.          
006900 77   W-WDE4-IDPLKLST            PIC S9(3)   VALUE ZERO  COMP-3.          
007000 77   SW-ARTC11-LAEST            PIC X       VALUE 'N'.                   
007100                                                                          
007200 01   W-TIAAVVD                  PIC 9(5).                                
007300 01   FILLER REDEFINES W-TIAAVVD.                                         
007400      03  W-TIAA                 PIC 9(2).                                
007500      03  W-TIVV                 PIC 9(2).                                
007600      03  W-TID                  PIC 9(1).                                
007700     EJECT                                                                
007800*                                                                         
007900 01  TEST-IDDISTR              PIC S9(5)    COMP-3.                       
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
010400* ----- GENERELLA SUB PROGRAM                                             
010500 01  GENERELLA-SUBPROGRAM.                                                
010600   03 CBLTDLI                    PIC X(8)    VALUE 'CBLTDLI '.            
010700   03 FELLOG                     PIC X(8)    VALUE 'FELLOG  '.            
010800   03 WDATKONV                   PIC X(8)    VALUE 'WDATKONV'.            
010900   03 W218ETA                    PIC X(8)    VALUE 'W218ETA '.            
011000                                                                          
011100 01 FILLER                       PIC  X(8)   VALUE 'LETA'.                
011200*   -COPY W218LETA -PRE ETA-.                                             
011300     EJECT                                                                
011400 01    NYCKLAR-TILL-DLI.                                                  
011500     SKIP2                                                                
011600   03    W-WDQ211KY-X.                                                    
011700     05    W-Q211KY-IDDC         PIC X(2)    VALUE SPACE.                 
011800     05    W-Q211KY-IDLEVNR      PIC X(5)   VALUE SPACE.                  
011900     SKIP2                                                                
012000   03    W-WDQ401KY-X.                                                    
012100     05    W-Q401KY-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
012200     05    W-Q401KY-IDDC         PIC  X(2)   VALUE ZERO.                  
012300     05    W-Q401KY-ADLAGOMR     PIC S9(3)   VALUE ZERO  COMP-3.          
012400     05    W-Q401KY-ADGANG       PIC S9(3)   VALUE ZERO  COMP-3.          
012500     05    W-Q401KY-ADPLATS      PIC S9(5)   VALUE ZERO  COMP-3.          
012600     05    W-Q401KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
012700     05    W-Q401KY-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
012800     SKIP2                                                                
012900   03    W-WDQ4B1KY-X.                                                    
013000     05    W-Q4B1KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
013100     05    W-Q4B1KY-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
013200     05    W-Q4B1KY-IDGMTREF.                                             
013300       07  W-Q4B1KY-IDDISTR      PIC S9(5)   VALUE ZERO  COMP-3.          
013400       07  W-Q4B1KY-IDKUNDNR     PIC S9(7)   VALUE ZERO  COMP-3.          
013500       07  W-Q4B1KY-IDKUNDRF.                                             
013600        09 W-Q4B1KY-IDORDNR7     PIC 9(07)   VALUE ZERO.                  
013700        09 FILLER                PIC X(03)   VALUE SPACE.                 
013800     05    W-Q4B1KY-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
013900     05    W-Q4B1KY-IDDC         PIC  X(2)   VALUE ZERO.                  
014000     05    W-Q4B1KY-ADLAGOMR     PIC S9(3)   VALUE ZERO  COMP-3.          
014100     05    W-Q4B1KY-ADGANG       PIC S9(3)   VALUE ZERO  COMP-3.          
014200     05    W-Q4B1KY-ADPLATS      PIC S9(5)   VALUE ZERO  COMP-3.          
014300     SKIP2                                                                
014400   03    W-WDQ4B1KY-MIN-X.                                                
014500     05    W-Q4B1KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
014600     05    W-Q4B1KY-MIN-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
014700     05    W-Q4B1KY-MIN-IDGMTREF.                                         
014800       07  W-Q4B1KY-MIN-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
014900       07  W-Q4B1KY-MIN-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
015000       07  W-Q4B1KY-MIN-IDKUNDRF.                                         
015100        09 W-Q4B1KY-MIN-IDORDNR7 PIC 9(07)   VALUE ZERO.                  
015200        09 FILLER                PIC X(03)   VALUE SPACE.                 
015300     05    W-Q4B1KY-MIN-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
015400     05    W-Q4B1KY-MIN-IDDC     PIC  X(2)   VALUE ZERO.                  
015500     05    W-Q4B1KY-MIN-ADLAGOMR PIC S9(3)   VALUE ZERO  COMP-3.          
015600     05    W-Q4B1KY-MIN-ADGANG   PIC S9(3)   VALUE ZERO  COMP-3.          
015700     05    W-Q4B1KY-MIN-ADPLATS  PIC S9(5)   VALUE ZERO  COMP-3.          
015800     SKIP2                                                                
015900   03    W-WDQ4B1KY-MAX-X.                                                
016000     05    W-Q4B1KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
016100     05    W-Q4B1KY-MAX-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
016200     05    W-Q4B1KY-MAX-IDGMTREF.                                         
016300       07  W-Q4B1KY-MAX-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
016400       07  W-Q4B1KY-MAX-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
016500       07  W-Q4B1KY-MAX-IDKUNDRF.                                         
016600        09 W-Q4B1KY-MAX-IDORDNR7 PIC 9(07)   VALUE ZERO.                  
016700        09 FILLER                PIC X(03)   VALUE SPACE.                 
016800     05    W-Q4B1KY-MIN-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
016900     05    W-Q4B1KY-MIN-IDDC     PIC  X(2)   VALUE ZERO.                  
017000     05    W-Q4B1KY-MAX-ADLAGOMR PIC S9(3)   VALUE ZERO  COMP-3.          
017100     05    W-Q4B1KY-MAX-ADGANG   PIC S9(3)   VALUE ZERO  COMP-3.          
017200     05    W-Q4B1KY-MAX-ADPLATS  PIC S9(5)   VALUE ZERO  COMP-3.          
017300     SKIP2                                                                
017400   03    W-WDE401KY-X.                                                    
017500     05    W-E401KY-IDGMTREF.                                             
017600       07  W-E401KY-IDDISTR      PIC S9(5)   VALUE ZERO  COMP-3.          
017700       07  W-E401KY-IDKUNDNR     PIC S9(7)   VALUE ZERO  COMP-3.          
017800       07  W-E401KY-IDKUNDRF.                                             
017900        09 W-E401KY-IDORDNR5     PIC 9(05)   VALUE ZERO.                  
018000        09 FILLER                PIC X(05)   VALUE SPACE.                 
018100     05    W-E401KY-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
018200     05    W-E401KY-IDPLKLST     PIC S9(3)   VALUE ZERO  COMP-3.          
018300     SKIP2                                                                
018400   03    W-WDE4C1KY-X.                                                    
018500     05    W-E4C1KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
018600     05    W-E4C1KY-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
018700     05    W-E4C1KY-IDPURAD      PIC S9(5)   VALUE ZERO  COMP-3.          
018800     SKIP2                                                                
018900   03    W-WDE4C1KY-MIN-X.                                                
019000     05    W-E4C1KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
019100     05    W-E4C1KY-MIN-IDPRODNR PIC S9(7)   VALUE ZERO  COMP-3.          
019200     05    W-E4C1KY-MIN-IDPURAD  PIC S9(5)   VALUE ZERO  COMP-3.          
019300     SKIP2                                                                
019400   03    W-WDE4C1KY-MAX-X.                                                
019500     05    W-E4C1KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
019600     05    W-E4C1KY-MAX-IDPRODNR PIC S9(7)   VALUE ZERO  COMP-3.          
019700     05    W-E4C1KY-MAX-IDPURAD  PIC S9(5)   VALUE ZERO  COMP-3.          
019800     SKIP2                                                                
019900   03    W-WDA501KY-X.                                                    
020000     05    W-A501KY-IDGMTREF.                                             
020100       07  W-A501KY-IDDISTR  PIC S9(5)       VALUE ZERO  COMP-3.          
020200       07  W-A501KY-IDKUNDNR PIC S9(7)       VALUE ZERO  COMP-3.          
020300       07  W-A501KY-IDORDNR5 PIC X(05)       VALUE SPACE.                 
020400       07  FILLER            PIC X(05)       VALUE SPACE.                 
020500     05    W-A501KY-IDARTNR  PIC S9(9)       VALUE ZERO  COMP-3.          
020600     05    W-A501KY-IDLOPNR  PIC S9(3)       VALUE ZERO  COMP-3.          
020700     SKIP2                                                                
020800   03    W-WDA501KY-MIN-X.                                                
020900     05    W-A501KY-MIN-IDGMTREF.                                         
021000       07  W-A501KY-MIN-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
021100       07  W-A501KY-MIN-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
021200       07  W-A501KY-MIN-IDORDNR5 PIC X(05)   VALUE SPACE.                 
021300       07  FILLER                PIC X(05)   VALUE SPACE.                 
021400     05    W-A501KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
021500     05    W-A501KY-MIN-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
021600     SKIP2                                                                
021700   03    W-WDA501KY-MAX-X.                                                
021800     05    W-A501KY-MAX-IDGMTREF.                                         
021900       07  W-A501KY-MAX-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
022000       07  W-A501KY-MAX-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
022100       07  W-A501KY-MAX-IDORDNR5 PIC X(05)   VALUE SPACE.                 
022200       07  FILLER                PIC X(05)   VALUE SPACE.                 
022300     05    W-A501KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
022400     05    W-A501KY-MAX-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
022500     SKIP2                                                                
022600   03    W-IDDISTR-X.                                                     
022700     05    W-IDDISTR             PIC S9(5)   VALUE ZERO  COMP-3.          
022800     SKIP2                                                                
022900   03    W-IDKUNDNR-X.                                                    
023000     05    W-IDKUNDNR            PIC S9(7)   VALUE ZERO  COMP-3.          
023100     SKIP2                                                                
023200   03    W-IDDC-X.                                                        
023300     05    W-IDDC                PIC  X(2)   VALUE ZERO.                  
023400     SKIP2                                                                
023500   03    W-IDORDER-X.                                                     
023600     05    W-IDORDER             PIC S9(7)   VALUE ZERO  COMP-3.          
023700     SKIP2                                                                
023800   03    W-IDARTNR-X.                                                     
023900     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
024000     SKIP2                                                                
024100   03    W-IDLOPNR-X.                                                     
024200     05    W-IDLOPNR             PIC S9(3)   VALUE ZERO  COMP-3.          
024300     SKIP2                                                                
024400   03    W-IDKUNDRF-MIN-X.                                                
024500     05    W-IDKUNDRF-MIN        PIC X(10).                               
024600                                                                          
024700     05    W-IDORDNR5-FILLER-MIN REDEFINES W-IDKUNDRF-MIN.                
024800       10  W-IDORDNR5-MIN        PIC X(5).                                
024900       10  FILLER                PIC X(5).                                
025000                                                                          
025100     05    W-IDORDNR7-FILLER-MIN REDEFINES W-IDKUNDRF-MIN.                
025200       10  W-IDORDNR7-MIN        PIC X(7).                                
025300       10  FILLER                PIC X(3).                                
025400     SKIP2                                                                
025500   03    W-IDKUNDRF-X.                                                    
025600     05    W-IDORDNR5            PIC 9(5).                                
025700     05    FILLER                PIC X(5).                                
025800                                                                          
025900     SKIP2                                                                
026000   03    W-IDPRODNR-X.                                                    
026100     05    W-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.          
026200                                                                          
026300   03    W-IDPURAD-X.                                                     
026400     05    W-IDPURAD             PIC S9(5)   VALUE ZERO  COMP-3.          
026500     SKIP2                                                                
026600   03  W-IDLEVNR-X.                                                       
026700     05  W-IDLEVNR               PIC X(5)    VALUE SPACE.                 
026800     EJECT                                                                
026900     EJECT                                                                
027000******************************************************************        
027100*                                                                         
027200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
027300*                                                                         
027400 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
027500     SKIP3                                                                
027600*01    MID -COPY W9I34501.                                                
027700     EJECT                                                                
027800*01    -COPY WMSGAREA                                                     
027900     EJECT                                                                
028000*  03    MOD -COPY W9O34502  -RED MSG-AREA.                               
028100     EJECT                                                                
028200*01    -COPY WMFSAREA                                                     
028300     EJECT                                                                
028400******************************************************************        
028500*                                                                         
028600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028700*                                                                         
028800 01    IMS-WS.                                                            
028900   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
029000     SKIP3                                                                
029100*                        **** STATUS-KOD FRÅN IMS                         
029200   03    STATUS-WS               PIC XX.                                  
029300     88    SEGMENT-FINNS                     VALUE '  '.                  
029400     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
029500     88    SEGMENT-SLUT                      VALUE 'GB'.                  
029600     SKIP3                                                                
029700   03    GODK-STATUSKODER.                                                
029800     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
029900     SKIP3                                                                
030000 01    SSA1                      PIC X(200).                              
030100 01    SSA2                      PIC X(64).                               
030200     EJECT                                                                
030300*                            IMS FUNKTIONSKODER                           
030400*01    -COPY W0003                                                        
030500     EJECT                                                                
030600******************************************************************        
030700*                                                                         
030800*        ARBETS-AREOR TILL IO-AREORNA                                     
030900*                                                                         
031000*    ---  DLI INPUT-OUTPUT AREA 1                                         
031100*    ---  DLI-IO-AREA                                                     
031200                                                                          
031300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI01'.            
031400 01  DLI-IO-ORQI01.                                                       
031500*  03    WLORQI01 -COPY WDQ201                                            
031600     EJECT                                                                
031700                                                                          
031800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI11'.            
031900 01  DLI-IO-ORQI11.                                                       
032000*  03    WLORQI11 -COPY WDQ211                                            
032100     EJECT                                                                
032200                                                                          
032300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQI12'.            
032400 01  DLI-IO-ORQI12.                                                       
032500*  03    WLORQI12 -COPY WDQ212                                            
032600     EJECT                                                                
032700                                                                          
032800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQH01'.            
032900 01  DLI-IO-ORQH01.                                                       
033000*  03    WLORQH01 -COPY WDQ4B1      -PRE Q4-                              
033100     EJECT                                                                
033200                                                                          
033300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC11'.            
033400 01  DLI-IO-ARTC11.                                                       
033500*  03    WLARTC11  -COPY WDK611                                           
033600     EJECT                                                                
033700                                                                          
033800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORQF01'.            
033900 01  DLI-IO-ORQF01.                                                       
034000*  03    WLORQF01 -COPY WDQ401                                            
034100     EJECT                                                                
034200                                                                          
034300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORDP01'.            
034400 01  DLI-IO-ORDP01.                                                       
034500*  03    WLORDP01  -COPY WDA501                                           
034600     EJECT                                                                
034700                                                                          
034800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE4C'.             
034900 01  DLI-IO-WDE4C.                                                        
035000*  03    -COPY WDE4C1                                                     
035100     EJECT                                                                
035200                                                                          
035300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-E40120'.            
035400 01  DLI-IO-E40120.                                                       
035500*    03      -COPY WDE401                                                 
035600     EJECT                                                                
035700*    03      -COPY WDE411                                                 
035800     EJECT                                                                
035900                                                                          
036000 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDE601'.            
036100 01  DLI-IO-WDE601.                                                       
036200*    03  -COPY WDE601                                                     
036300     EJECT                                                                
036400                                                                          
036500 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-LEVA14'.            
036600 01  DLI-IO-LEVA14.                                                       
036700*    03  WLLEVA14  -COPY WDF106                                           
036800     EJECT                                                                
036900 01  FILLER                 PIC X(12) VALUE 'DUMMY-ARTC'.                 
037000 01  ETA-ARTC-PCB           PIC X.                                        
037100 01  FILLER                 PIC X(12) VALUE 'DUMMY-LEVA'.                 
037200 01  ETA-LEVA-PCB           PIC X.                                        
037300 LINKAGE SECTION.                                                         
037400                                                                          
037500*01    -COPY W0009     -PRE MSG-                                          
037600     EJECT                                                                
037700*01    -COPY W0008     -PRE ORQI-                                         
037800     05  FILLER                  PIC X.                                   
037900     EJECT                                                                
038000*01    -COPY W0008     -PRE ORQF-                                         
038100     05  FILLER                  PIC X.                                   
038200     EJECT                                                                
038300*01    -COPY W0008     -PRE ORQH-                                         
038400     05  FILLER                  PIC X.                                   
038500     EJECT                                                                
038600*01    -COPY W0008     -PRE WDE4-                                         
038700     05  FILLER                  PIC X.                                   
038800     EJECT                                                                
038900*01    -COPY W0008     -PRE WDE4C-                                        
039000     05  FILLER                  PIC X.                                   
039100     EJECT                                                                
039200*01    -COPY W0008     -PRE ORDP-                                         
039300     05  FILLER                  PIC X.                                   
039400     EJECT                                                                
039500*01    -COPY W0008     -PRE ARTC-                                         
039600     05  FILLER                  PIC X.                                   
039700     EJECT                                                                
039800*01    -COPY W0008     -PRE WDE6-                                         
039900     05  FILLER                  PIC X.                                   
040000     EJECT                                                                
040100*01    -COPY W0008     -PRE LEVA-                                         
040200     05  FILLER                  PIC X.                                   
040300     EJECT                                                                
040400 01  ETA-WDK7-PCB           PIC X.                                        
040500 01  ETA-INLC-PCB           PIC X.                                        
040600 01  ETA-WDB6-PCB           PIC X.                                        
040700 01  ETA-WDD9-PCB           PIC X.                                        
040800     EJECT                                                                
040900 PROCEDURE DIVISION  USING MSG-PCB  ORQI-PCB ORQF-PCB ORQH-PCB            
041000                           WDE4-PCB WDE4C-PCB ORDP-PCB ARTC-PCB           
041100                           WDE6-PCB LEVA-PCB                              
041200                           ETA-WDK7-PCB ETA-INLC-PCB                      
041300                           ETA-WDB6-PCB ETA-WDD9-PCB.                     
041400     ENTRY 'DLITCBL' USING MSG-PCB  ORQI-PCB ORQF-PCB ORQH-PCB            
041500                           WDE4-PCB WDE4C-PCB ORDP-PCB ARTC-PCB           
041600                           WDE6-PCB LEVA-PCB                              
041700                           ETA-WDK7-PCB ETA-INLC-PCB                      
041800                           ETA-WDB6-PCB ETA-WDD9-PCB.                     
041900                                                                          
042000 STYR SECTION.                                                            
042100                                                                          
042200     PERFORM IMS-GET-MSG                                                  
042300     IF SEGMENT-FINNS                                                     
042400         PERFORM A-INIT                                                   
042500         PERFORM B-KONTROLLERA-FLYTTA-NYCKLAR                             
042600         IF NYCKLAR-OK                                                    
042700             PERFORM C-BEHANDLA-RADER                                     
042800         END-IF                                                           
042900     END-IF                                                               
043000     PERFORM D-KONTROLLERA-OM-SIDA-TOM                                    
043100     PERFORM E-BERAKNA-MAX-MOD-LANGD                                      
043200     MOVE MAX-MOD-LANGD        TO MSG-KVLL                                
043300     PERFORM IMS-INSERT-MSG                                               
043400     MOVE ZERO                 TO RETURN-CODE                             
043500     GOBACK                                                               
043600     .                                                                    
043700     EJECT                                                                
043800                                                                          
043900 A-INIT SECTION.                                                          
044000                                                                          
044100     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I34501                    
044200     MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                     
044300     MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                    
044400     MOVE ' '                          TO MFS-KDTRTYP                     
044500     MOVE LOW-VALUE                    TO MSG-AREA                        
044600     MOVE +1                           TO  IX-RAD                         
044700                                                                          
044800     MOVE 'W9O34502'                   TO MFS-IDMOD                       
044900     MOVE '9345'                       TO MOD2-IDTRANS                    
045000     MOVE ZERO                         TO MOD2-IDMFSFEL                   
045100     MOVE MID-IDORDNR7-NEXT            TO MOD2-IDORDNR7-NEXT              
045200     MOVE MID-IDARTNR                  TO MOD2-IDARTNR                    
045300     MOVE ZERO                         TO MOD2-IDKOLLI-NEXT               
045400                                                                          
045500     PERFORM UNTIL IX-RAD      >   MAX-RAD                                
045600         MOVE ZERO             TO  MOD2-IDORDNR7-RAD (IX-RAD)             
045700                                 MOD2-TIDISPIN    (IX-RAD)                
045800                                 MOD2-KVBEART-Q (IX-RAD)                  
045900                                 MOD2-KVPREAVB    (IX-RAD)                
046000                                 MOD2-IDORDNR7-LEV (IX-RAD)               
046100                                 MOD2-TIANNULL    (IX-RAD)                
046200                                 MOD2-KDORDKL     (IX-RAD)                
046300                                 MOD2-KDANNULL    (IX-RAD)                
046400         MOVE SPACE            TO  MOD2-KDORDSTA    (IX-RAD)              
046500                                 MOD2-IDDC-RAD    (IX-RAD)                
046600                                 MOD2-IDRADINF    (IX-RAD)                
046700                                 MOD2-BERADREF    (IX-RAD)                
046800         ADD +1                TO  IX-RAD                                 
046900     END-PERFORM                                                          
047000     .                                                                    
047100     EJECT                                                                
047200 B-KONTROLLERA-FLYTTA-NYCKLAR SECTION.                                    
047300                                                                          
047400     IF MID-IDDISTR            NUMERIC AND                                
047500        MID-IDKUNDNR           NUMERIC AND                                
047600        MID-IDARTNR            NUMERIC                                    
047700         PERFORM  BA-FLYTTA-NYCKLAR                                       
047800      ELSE                                                                
047900         MOVE NEJ              TO NYCKLAR-SW                              
048000         MOVE 'B01'            TO MOD2-IDMFSFEL                           
048100     END-IF                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 BA-FLYTTA-NYCKLAR SECTION.                                               
048500                                                                          
048600     MOVE LOW-VALUE            TO  W-WDE4C1KY-MIN-X                       
048700                                   W-WDQ4B1KY-MIN-X                       
048800                                                                          
048900     MOVE HIGH-VALUE           TO  W-WDE4C1KY-MAX-X                       
049000                                   W-WDQ4B1KY-MAX-X                       
049100                                                                          
049200     MOVE MID-IDARTNR          TO  W-Q4B1KY-MIN-IDARTNR                   
049300                                   W-Q4B1KY-MAX-IDARTNR                   
049400                                   W-Q4B1KY-IDARTNR                       
049500                                                                          
049600                                   W-E4C1KY-MIN-IDARTNR                   
049700                                   W-E4C1KY-MAX-IDARTNR                   
049800                                   W-E4C1KY-IDARTNR                       
049900                                   W-IDARTNR                              
050000                                                                          
050100     MOVE MID-IDDISTR          TO  W-Q4B1KY-MIN-IDDISTR                   
050200                                   W-Q4B1KY-MAX-IDDISTR                   
050300                                   W-Q4B1KY-IDDISTR                       
050400                                                                          
050500                                   W-A501KY-MIN-IDDISTR                   
050600                                   W-A501KY-MAX-IDDISTR                   
050700                                   W-IDDISTR                              
050800                                   TEST-IDDISTR                           
050900                                                                          
051000     MOVE MID-IDKUNDNR         TO  W-Q4B1KY-MIN-IDKUNDNR                  
051100                                   W-Q4B1KY-MAX-IDKUNDNR                  
051200                                   W-Q4B1KY-IDKUNDNR                      
051300                                                                          
051400                                   W-A501KY-MIN-IDKUNDNR                  
051500                                   W-A501KY-MAX-IDKUNDNR                  
051600                                   W-IDKUNDNR                             
051700     .                                                                    
051800     EJECT                                                                
051900 C-BEHANDLA-RADER SECTION.                                                
052000                                                                          
052100     MOVE +1                   TO IX-RAD                                  
052200                                                                          
052300     EVALUATE TRUE                                                        
052400     WHEN MID-FLSVAR-NEXT      =  'P' OR SPACE                            
052500         PERFORM CA-BEHANDLA-WDE4-ORDER                                   
052600         IF IX-RAD             >  MAX-RAD AND SEGMENT-FINNS               
052700             CONTINUE                                                     
052800          ELSE                                                            
052900             PERFORM CB-BEHANDLA-ORDERRADKOE                              
053000             IF IX-RAD         >  MAX-RAD AND SEGMENT-FINNS               
053100                 CONTINUE                                                 
053200              ELSE                                                        
053300                 PERFORM CC-BEHANDLA-RESTORDER                            
053400             END-IF                                                       
053500         END-IF                                                           
053600                                                                          
053700     WHEN MID-FLSVAR-NEXT      =  'O'                                     
053800         PERFORM CB-BEHANDLA-ORDERRADKOE                                  
053900         IF IX-RAD             >  MAX-RAD  AND SEGMENT-FINNS              
054000             CONTINUE                                                     
054100          ELSE                                                            
054200             PERFORM CC-BEHANDLA-RESTORDER                                
054300         END-IF                                                           
054400                                                                          
054500     WHEN MID-FLSVAR-NEXT      =  'R'                                     
054600         PERFORM CC-BEHANDLA-RESTORDER                                    
054700                                                                          
054800     END-EVALUATE                                                         
054900     .                                                                    
055000     EJECT                                                                
055100 CA-BEHANDLA-WDE4-ORDER SECTION.                                          
055200                                                                          
055300     IF MID-IDPRODNR-NEXT           > ZERO                                
055400         MOVE MID-IDPRODNR-NEXT TO  W-E4C1KY-IDPRODNR                     
055500         MOVE MID-IDRADNR-NEXT  TO  W-E4C1KY-IDPURAD                      
055600         MOVE SPACE             TO  W-IDKUNDRF-X                          
055700         MOVE MID-IDORDNR7-NEXT TO  W-IDORDNR5                            
055800         PERFORM IMS-GET-WDE4C-KVAL                                       
055900      ELSE                                                                
056000         PERFORM IMS-GET-WDE4C-OKVAL                                      
056100     END-IF                                                               
056200                                                                          
056300     PERFORM UNTIL             SEGMENT-SAKNAS  OR                         
056400                               SEGMENT-SLUT    OR                         
056500                               IX-RAD > MAX-RAD                           
056600                                                                          
056700         MOVE SEQC-IDORDNR5    TO  W-WDE4-IDORDNR5                        
056800                                                                          
056900         PERFORM CAA-SKAPA-WDE401-WDE411-NYCKEL                           
057000         PERFORM IMS-GET-WDE401-11                                        
057100         IF ORAD-FLFYSAVV = JA AND ORAD-KVLEVART = 0                      
057200           CONTINUE                                                       
057300         ELSE                                                             
057400           PERFORM CAB-REDIGERA-PO-RAD                                    
057500         END-IF                                                           
057600                                                                          
057700         PERFORM IMS-GET-WDE4C-OKVAL                                      
057800     END-PERFORM                                                          
057900                                                                          
058000     IF IX-RAD                 > MAX-RAD  AND                             
058100        SEGMENT-FINNS                                                     
058200         PERFORM CAC-SPARA-PACK-ORDER-NYCKLAR                             
058300     END-IF                                                               
058400     .                                                                    
058500     EJECT                                                                
058600 CAA-SKAPA-WDE401-WDE411-NYCKEL SECTION.                                  
058700                                                                          
058800     MOVE MID-IDDISTR          TO  W-E401KY-IDDISTR                       
058900     MOVE MID-IDKUNDNR         TO  W-E401KY-IDKUNDNR                      
059000     MOVE SPACE                TO  W-E401KY-IDKUNDRF                      
059100     MOVE SEQC-IDORDNR5        TO  W-E401KY-IDORDNR5                      
059200     MOVE SEQC-IDPRODNR        TO  W-E401KY-IDPRODNR                      
059300     MOVE SEQC-IDPLKLST        TO  W-E401KY-IDPLKLST                      
059400     MOVE SEQC-IDPURAD         TO  W-IDPURAD                              
059500     .                                                                    
059600     EJECT                                                                
059700                                                                          
059800 CAB-REDIGERA-PO-RAD SECTION.                                             
059900                                                                          
060000     IF ORAD-IDKUNDRF-RO       IN ORAD-WDE411                             
060100                             NOT = '00000     '                           
060200         MOVE ORAD-IDKUNDRF-RO IN ORAD-WDE411 (1:5)                       
060300                               TO MOD2-IDORDNR7-RAD(IX-RAD)               
060400     ELSE                                                                 
060500         MOVE W-WDE4-IDORDNR5 TO MOD2-IDORDNR7-RAD(IX-RAD)                
060600     END-IF                                                               
060700     MOVE ORAD-KVBEART         IN ORAD-WDE411                             
060800                             TO MOD2-KVBEART-Q(IX-RAD)                    
060900     MOVE ORAD-KVAVBART        IN ORAD-WDE411                             
061000                             TO MOD2-KVPREAVB(IX-RAD)                     
061100     MOVE ORAD-KDORDKL         IN ORAD-WDE411                             
061200                             TO MOD2-KDORDKL (IX-RAD)                     
061300     MOVE ORAD-KDANNULL        IN ORAD-WDE411                             
061400                             TO MOD2-KDANNULL(IX-RAD)                     
061500     MOVE ORAD-BERADREF        IN ORAD-WDE411                             
061600                             TO MOD2-BERADREF(IX-RAD)                     
061700     MOVE KORD-IDDC            TO MOD2-IDDC-RAD(IX-RAD)                   
061800                                WS-IDDC                                   
061900                                W-Q211KY-IDDC                             
062000     IF GOOD-DDC                                                          
062100       MOVE KORD-IDORDER       TO W-IDORDER                               
062200       MOVE ORAD-IDLEVNR       IN ORAD-WDE411                             
062300                             TO W-IDLEVNR                                 
062400                                W-Q211KY-IDLEVNR                          
062500       PERFORM IMS-GET-ORQI11-KVAL                                        
062600       IF SEGMENT-FINNS                                                   
062700         MOVE DIRL-KVDAGAR-DIFF      TO WS-KVDAGAR-DIFF                   
062800         MOVE WS-KVDAGAR-DIFF(2:2) TO MOD2-KVDAGAR-DIFF(IX-RAD)           
062900         IF DIRL-KVDAGAR-DIFF < ZERO                                      
063000           MOVE '-'               TO MOD2-IDTECKEN  (IX-RAD)              
063100         ELSE                                                             
063200           MOVE '+'               TO MOD2-IDTECKEN  (IX-RAD)              
063300         END-IF                                                           
063400       END-IF                                                             
063500       PERFORM IMS-GET-WDF106                                             
063600       IF SEGMENT-FINNS                                                   
063700         MOVE ADR-BELEV           TO MOD2-IDLEVNMN  (IX-RAD)              
063800       ELSE                                                               
063900         MOVE SPACE               TO MOD2-IDLEVNMN  (IX-RAD)              
064000       END-IF                                                             
064100     ELSE                                                                 
064200       IF ORAD-IDBIL IN ORAD-WDE411 = LOW-VALUE                           
064300         MOVE SPACE            TO MOD2-IDBIL   (IX-RAD)                   
064400       ELSE                                                               
064500         MOVE ORAD-IDBIL       IN ORAD-WDE411                             
064600                             TO MOD2-IDBIL (IX-RAD)                       
064700       END-IF                                                             
064800     END-IF                                                               
064900                                                                          
065000     PERFORM CABA-BESTAM-STAT-FRAN-WDE601                                 
065100     MOVE W-KDORDSTA           TO MOD2-KDORDSTA(IX-RAD)                   
065200     MOVE W-TIDISPIN           TO MOD2-TIDISPIN(IX-RAD)                   
065300     MOVE SEQC-IDORDNR5        TO MOD2-IDORDNR7-LEV(IX-RAD)               
065400     ADD +1                    TO IX-RAD                                  
065500     .                                                                    
065600     EJECT                                                                
065700 CABA-BESTAM-STAT-FRAN-WDE601 SECTION.                                    
065800                                                                          
065900     MOVE KORD-IDPRODNR        TO W-IDPRODNR                              
066000     PERFORM IMS-GU-WDE601                                                
066100                                                                          
066200     IF SEGMENT-FINNS                                                     
066300       IF VORD-KVORDRAD        =  VORD-KVORDRAD-PACK                      
066400         MOVE 'P '             TO W-KDORDSTA                              
066500         MOVE VORD-DABEGPAC(3:6) TO W-TIDISPIN                            
066600                                                                          
066700         IF VORD-KVKOLLI  =  VORD-KVKOLLI-FL AND                          
066800            VORD-KVKOLLI-FAKT = ZERO         AND                          
066900            VORD-KVKOLLI-LAST = ZERO                                      
067000           IF VORD-KVKOLLI > +0                                           
067100             MOVE 'S ' TO W-KDORDSTA                                      
067200             MOVE VORD-TILASTN-SK    TO W-TIDISPIN                        
067300           ELSE                                                           
067400             MOVE 'N '               TO W-KDORDSTA                        
067500             IF GOOD-DDC                                                  
067600               IF VORD-KDVIA = '01'                                       
067700                 MOVE 'U '           TO W-KDORDSTA                        
067800               END-IF                                                     
067900             END-IF                                                       
068000             MOVE VORD-DABEGPAC(3:6) TO W-TIDISPIN                        
068100           END-IF                                                         
068200         ELSE                                                             
068300           IF VORD-KVKOLLI-FAKT = VORD-KVKOLLI  AND                       
068400              VORD-KVKOLLI-LAST = VORD-KVKOLLI                            
068500              MOVE 'SF' TO W-KDORDSTA                                     
068600              MOVE VORD-TILASTN-SK    TO W-TIDISPIN                       
068700           END-IF                                                         
068800******FIX TL 030513 *******                                               
068900           IF VORD-KVKOLLI-FAKT > VORD-KVKOLLI  AND                       
069000              VORD-KVKOLLI-LAST > VORD-KVKOLLI                            
069100              MOVE 'SF' TO W-KDORDSTA                                     
069200              MOVE VORD-TILASTN-SK    TO W-TIDISPIN                       
069300           END-IF                                                         
069400***************************                                               
069500         END-IF                                                           
069600       ELSE                                                               
069700         MOVE 'U '             TO W-KDORDSTA                              
069800         MOVE VORD-DABEGPAC(3:6) TO W-TIDISPIN                            
069900       END-IF                                                             
070000     ELSE                                                                 
070100       MOVE '  '               TO W-KDORDSTA                              
070200     END-IF                                                               
070300                                                                          
070400     IF ORAD-KDRADSTA          IN  ORAD-WDE411                            
070500                               >  3             AND                       
070600        W-KDORDSTA             =  'U '                                    
070700        IF ORAD-KVAVBART = ZERO                                           
070800          MOVE 'N '            TO W-KDORDSTA                              
070900        END-IF                                                            
071000        MOVE VORD-DABEGPAC(3:6) TO W-TIDISPIN                             
071100     END-IF                                                               
071200     .                                                                    
071300     EJECT                                                                
071400 CAC-SPARA-PACK-ORDER-NYCKLAR SECTION.                                    
071500                                                                          
071600     MOVE SEQC-IDORDNR5        TO  MOD2-IDORDNR7-NEXT                     
071700     MOVE 'P'                  TO  MOD2-FLSVAR-NEXT                       
071800     MOVE SEQC-IDPRODNR        TO  MOD2-IDPRODNR-NEXT                     
071900     MOVE SEQC-IDPURAD         TO  MOD2-IDRADNR-NEXT                      
072000     .                                                                    
072100     EJECT                                                                
072200 CB-BEHANDLA-ORDERRADKOE SECTION.                                         
072300                                                                          
072400     IF MID-IDORDNR7-NEXT       >   ZERO AND MID-FLSVAR-NEXT = 'O'        
072500         MOVE MID-IDRADNR-NEXT  TO  W-Q4B1KY-IDLOPNR                      
072600         MOVE SPACE             TO  W-Q4B1KY-IDKUNDRF                     
072700         MOVE MID-IDORDNR7-NEXT TO  W-Q4B1KY-IDORDNR7                     
072800         MOVE MID-IDORDER-NEXT  TO  W-Q4B1KY-IDORDER                      
072900         MOVE MID-IDDC-NEXT     TO  W-Q4B1KY-IDDC                         
073000         MOVE MID-ADLAGOMR-NEXT TO  W-Q4B1KY-ADLAGOMR                     
073100         MOVE MID-ADGANG-NEXT   TO  W-Q4B1KY-ADGANG                       
073200         MOVE MID-ADPLATS-NEXT  TO  W-Q4B1KY-ADPLATS                      
073300         PERFORM IMS-GET-ORQH01-KVAL                                      
073400      ELSE                                                                
073500         PERFORM IMS-GET-ORQH01-OKVAL                                     
073600     END-IF                                                               
073700                                                                          
073800     PERFORM UNTIL SEGMENT-SAKNAS    OR                                   
073900                   SEGMENT-SLUT      OR                                   
074000                   IX-RAD      > MAX-RAD                                  
074100                                                                          
074200         PERFORM CBA-SKAPA-WDQ401-NYCKEL                                  
074300         PERFORM IMS-GET-ORQF01-KVAL                                      
074400                                                                          
074500         PERFORM CBB-LAS-OHUV-ARBTAB                                      
074600                                                                          
074700         PERFORM CBC-REDIGERA-ORDERRAD                                    
074800         ADD +1                TO IX-RAD                                  
074900                                                                          
075000         PERFORM IMS-GET-ORQH01-OKVAL                                     
075100                                                                          
075200     END-PERFORM                                                          
075300                                                                          
075400     IF IX-RAD                 > MAX-RAD     AND                          
075500        SEGMENT-FINNS                                                     
075600         PERFORM CBD-SPARA-ORDERRAD-NYCKLAR                               
075700     END-IF                                                               
075800     .                                                                    
075900     EJECT                                                                
076000 CBA-SKAPA-WDQ401-NYCKEL SECTION.                                         
076100                                                                          
076200     MOVE Q4-SEQB-IDORDER      TO W-Q401KY-IDORDER                        
076300     MOVE Q4-SEQB-IDDC         TO W-Q401KY-IDDC                           
076400     MOVE Q4-SEQB-ADLAGOMR     TO W-Q401KY-ADLAGOMR                       
076500     MOVE Q4-SEQB-ADGANG       TO W-Q401KY-ADGANG                         
076600     MOVE Q4-SEQB-ADPLATS      TO W-Q401KY-ADPLATS                        
076700     MOVE Q4-SEQB-IDARTNR      TO W-Q401KY-IDARTNR                        
076800     MOVE Q4-SEQB-IDLOPNR      TO W-Q401KY-IDLOPNR                        
076900     .                                                                    
077000     EJECT                                                                
077100 CBB-LAS-OHUV-ARBTAB SECTION.                                             
077200                                                                          
077300     MOVE ORAD-IDORDER         IN ORAD-WDQ401                             
077400                               TO W-IDORDER                               
077500                                                                          
077600     MOVE ORAD-IDDC            IN ORAD-WDQ401                             
077700                               TO WS-IDDC                                 
077800     IF GOOD-DDC                                                          
077900       MOVE WC-CDC-SE          TO W-IDDC                                  
078000     ELSE                                                                 
078100       MOVE ORAD-IDDC          TO W-IDDC                                  
078200     END-IF                                                               
078300     PERFORM IMS-GET-ORQI12-KVAL                                          
078400     .                                                                    
078500     EJECT                                                                
078600 CBC-REDIGERA-ORDERRAD SECTION.                                           
078700                                                                          
078800     IF OHUV-FLKLAR               = NEJ                                   
078900         MOVE ZERO                TO MOD2-TIDISPIN(IX-RAD)                
079000         MOVE ZERO                TO MOD2-KVBEART-Q(IX-RAD)               
079100         MOVE ZERO                TO MOD2-KVPREAVB(IX-RAD)                
079200         MOVE ORAD-KDORDKL        IN ORAD-WDQ401                          
079300                                TO MOD2-KDORDKL(IX-RAD)                   
079400         MOVE ORAD-IDDC           IN ORAD-WDQ401                          
079500                                TO MOD2-IDDC-RAD(IX-RAD)                  
079600         IF ORAD-IDBIL            IN ORAD-WDQ401 = LOW-VALUE              
079700           MOVE SPACE             TO MOD2-IDBIL  (IX-RAD)                 
079800         ELSE                                                             
079900           MOVE ORAD-IDBIL        IN ORAD-WDQ401                          
080000                                TO MOD2-IDBIL (IX-RAD)                    
080100         END-IF                                                           
080200         MOVE 'E '                TO MOD2-KDORDSTA(IX-RAD)                
080300         MOVE ZERO                TO MOD2-KDANNULL(IX-RAD)                
080400     ELSE                                                                 
080500         MOVE ARB-DATRPAVD(3:6) TO W-TIAAMMDD                             
080600         MOVE W-TIAAMMDD          TO MOD2-TIDISPIN(IX-RAD)                
080700         MOVE ORAD-KVBEART-Q      IN ORAD-WDQ401                          
080800                                TO MOD2-KVBEART-Q(IX-RAD)                 
080900         MOVE ORAD-KVPREAVB       IN ORAD-WDQ401                          
081000                                TO MOD2-KVPREAVB(IX-RAD)                  
081100         MOVE ORAD-KDORDKL        IN ORAD-WDQ401                          
081200                                TO MOD2-KDORDKL(IX-RAD)                   
081300         MOVE ORAD-IDDC           IN ORAD-WDQ401                          
081400                                TO MOD2-IDDC-RAD(IX-RAD)                  
081500         IF ORAD-IDBIL            IN ORAD-WDQ401 = LOW-VALUE              
081600           MOVE SPACE             TO MOD2-IDBIL  (IX-RAD)                 
081700         ELSE                                                             
081800           MOVE ORAD-IDBIL        IN ORAD-WDQ401                          
081900                                TO MOD2-IDBIL (IX-RAD)                    
082000         END-IF                                                           
082100         MOVE 'R '                TO MOD2-KDORDSTA(IX-RAD)                
082200         MOVE ZERO                TO MOD2-KDANNULL(IX-RAD)                
082300     END-IF                                                               
082400     IF ORAD-IDKUNDRF-RO          IN ORAD-WDQ401                          
082500                                NOT = '0000000   '                        
082600         MOVE ORAD-IDKUNDRF-RO    IN ORAD-WDQ401 (1:7)                    
082700                                TO MOD2-IDORDNR7-RAD(IX-RAD)              
082800      ELSE                                                                
082900         MOVE ORAD-IDORDNR7       IN ORAD-WDQ401                          
083000                                TO MOD2-IDORDNR7-RAD(IX-RAD)              
083100     END-IF                                                               
083200     MOVE ORAD-IDORDNR7           IN ORAD-WDQ401                          
083300                                  TO MOD2-IDORDNR7-LEV(IX-RAD)            
083400     MOVE ORAD-BERADREF           IN ORAD-WDQ401                          
083500                                  TO MOD2-BERADREF(IX-RAD)                
083600     .                                                                    
083700     EJECT                                                                
083800 CBD-SPARA-ORDERRAD-NYCKLAR SECTION.                                      
083900                                                                          
084000     MOVE Q4-SEQB-IDORDNR7     TO  MOD2-IDORDNR7-NEXT                     
084100     MOVE 'O'                  TO  MOD2-FLSVAR-NEXT                       
084200     MOVE Q4-SEQB-IDLOPNR      TO  MOD2-IDRADNR-NEXT                      
084300     MOVE Q4-SEQB-IDDC         TO  MOD2-IDDC-NEXT                         
084400     MOVE Q4-SEQB-ADLAGOMR     TO  MOD2-ADLAGOMR-NEXT                     
084500     MOVE Q4-SEQB-ADGANG       TO  MOD2-ADGANG-NEXT                       
084600     MOVE Q4-SEQB-ADPLATS      TO  MOD2-ADPLATS-NEXT                      
084700     MOVE Q4-SEQB-IDORDER      TO  MOD2-IDORDER-NEXT                      
084800     .                                                                    
084900     EJECT                                                                
085000 CC-BEHANDLA-RESTORDER SECTION.                                           
085100                                                                          
085200     MOVE LOW-VALUE            TO  W-WDA501KY-MIN-X                       
085300                                                                          
085400     MOVE HIGH-VALUE           TO  W-WDA501KY-MAX-X                       
085500                                                                          
085600     MOVE MID-IDDISTR          TO  W-A501KY-MIN-IDDISTR                   
085700                                   W-A501KY-MAX-IDDISTR                   
085800                                   W-A501KY-IDDISTR                       
085900                                                                          
086000     MOVE MID-IDKUNDNR         TO  W-A501KY-MIN-IDKUNDNR                  
086100                                   W-A501KY-MAX-IDKUNDNR                  
086200                                   W-A501KY-IDKUNDNR                      
086300                                                                          
086400     MOVE MID-IDARTNR          TO  W-A501KY-MIN-IDARTNR                   
086500                                   W-A501KY-MAX-IDARTNR                   
086600                                   W-A501KY-IDARTNR                       
086700                                   W-IDARTNR                              
086800                                                                          
086900     IF MID-IDORDNR7-NEXT (3:5)  > ZERO AND MID-FLSVAR-NEXT = 'R'         
087000         MOVE MID-IDORDNR7-NEXT (3:5) TO  W-A501KY-IDORDNR5               
087100         MOVE MID-IDRADNR-NEXT        TO  W-A501KY-IDLOPNR                
087200         PERFORM IMS-GET-ORDP01-KVAL                                      
087300      ELSE                                                                
087400         PERFORM IMS-GET-ORDP01-OKVAL                                     
087500     END-IF                                                               
087600                                                                          
087700     PERFORM UNTIL SEGMENT-SAKNAS    OR                                   
087800                   SEGMENT-SLUT      OR                                   
087900                   IX-RAD      > MAX-RAD                                  
088000         IF RAD-KDSTARAD       < +4                                       
088100             PERFORM CCA-REDIGERA-RO-RAD                                  
088200             ADD +1            TO IX-RAD                                  
088300         END-IF                                                           
088400         PERFORM IMS-GET-ORDP01-OKVAL                                     
088500     END-PERFORM                                                          
088600                                                                          
088700     IF IX-RAD                 > MAX-RAD   AND                            
088800        SEGMENT-FINNS                                                     
088900         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                  
089000                       RAD-KDSTARAD < 4                                   
089100             PERFORM IMS-GET-ORDP01-OKVAL                                 
089200         END-PERFORM                                                      
089300         IF SEGMENT-FINNS                                                 
089400             PERFORM CCB-SPARA-RESTORDER-NYCKLAR                          
089500         END-IF                                                           
089600     END-IF                                                               
089700     .                                                                    
089800     EJECT                                                                
089900 CCA-REDIGERA-RO-RAD SECTION.                                             
090000                                                                          
090100     MOVE RAD-IDORDNR5         TO MOD2-IDORDNR7-RAD(IX-RAD)               
090200                                MOD2-IDORDNR7-LEV(IX-RAD)                 
090300                                                                          
090400     IF RAD-KDTPOTYP > ZERO                                               
090500       MOVE RAD-TITPO            TO W-TIDISPIN                            
090600       MOVE 'T '                 TO MOD2-KDORDSTA (IX-RAD)                
090700       IF RAD-DARODAT NOT = ZERO                                          
090800         MOVE 'B '               TO MOD2-KDORDSTA (IX-RAD)                
090900       END-IF                                                             
091000     ELSE                                                                 
091100       PERFORM IMS-GU-ARTC11                                              
091200       MOVE JA                   TO SW-ARTC11-LAEST                       
091300       MOVE 'B '                 TO MOD2-KDORDSTA(IX-RAD)                 
091400       MOVE RAD-IDDC             TO WS-IDDC                               
091600       IF NDC                                                             
091700         PERFORM CCAA-HAEMTA-TIBERANK                                     
091800                                                                          
091900         IF ETA-SVAR-OK = JA                                              
092100           IF NDC-NA OR NDC-CN                                            
092200             MOVE ETA-TIAAMMDD-SVAR TO W-TIDISPIN                         
092300           ELSE                                                           
092400             IF ETA-KVAVIS-ETA > +0                                       
092500               MOVE ETA-TIAAMMDD-SVAR TO W-TIDISPIN                       
092600             ELSE                                                         
092700               MOVE ZERO          TO  W-TIDISPIN                          
092800             END-IF                                                       
092900           END-IF                                                         
093000         ELSE                                                             
093100           MOVE ZERO              TO  W-TIDISPIN                          
093200         END-IF                                                           
093300       ELSE                                                               
093400         MOVE CLAG-TIDISPIN       TO W-TIDISPIN                           
093500       END-IF                                                             
093600     END-IF                                                               
093700                                                                          
093800     MOVE W-TIDISPIN           TO MOD2-TIDISPIN(IX-RAD)                   
093900     MOVE RAD-KVBEART-Q        TO MOD2-KVBEART-Q(IX-RAD)                  
094000     MOVE RAD-KVART            TO MOD2-KVPREAVB(IX-RAD)                   
094100     MOVE RAD-KDORDKL          TO MOD2-KDORDKL(IX-RAD)                    
094200     MOVE RAD-IDDC             TO MOD2-IDDC-RAD(IX-RAD)                   
094300     MOVE RAD-BERADREF         TO MOD2-BERADREF(IX-RAD)                   
094400     MOVE SPACE                TO MOD2-IDBIL(IX-RAD)                      
094500     MOVE ZERO                 TO MOD2-KDANNULL(IX-RAD)                   
094600                                                                          
094700     IF RAD-KDSTARAD = '1' AND                                            
094800        RAD-FLTPOBEK = JA                                                 
094900       MOVE RAD-IDARTNR        TO  W-IDARTNR                              
095000       IF SW-ARTC11-LAEST = NEJ                                           
095100         PERFORM IMS-GU-ARTC11                                            
095200       END-IF                                                             
095300       IF SEGMENT-FINNS                                                   
095400         PERFORM CCAB-TIDIGAST-ANNULL-DATUM                               
095500       END-IF                                                             
095600     END-IF                                                               
095700     MOVE NEJ                  TO SW-ARTC11-LAEST                         
095800     .                                                                    
095900     EJECT                                                                
096000 CCAA-HAEMTA-TIBERANK SECTION.                                            
096100                                                                          
096200     MOVE '612'                TO ETA-KDCALL                              
096300     MOVE RAD-IDDC             TO ETA-IDDC-REC                            
096400     MOVE RAD-IDARTNR          TO ETA-IDARTNR                             
096500     MOVE SPACE                TO ETA-IDLEVNR                             
096600     MOVE ZERO                 TO ETA-KDFRAKT                             
096700     MOVE RAD-DARODAT(3:6)     TO ETA-TIAAMMDD-ANROP                      
096800                                  WS-ETA-DATUM                            
096900     IF WS-ETA-DATUM-AAR > 50                                             
097000        MOVE 19                TO ETA-TISEKEL-ANROP                       
097100     ELSE                                                                 
097200        MOVE 20                TO ETA-TISEKEL-ANROP                       
097300     END-IF                                                               
097400                                                                          
097500     CALL W218ETA  USING ETA-W218LETA                                     
097600                         ETA-ARTC-PCB ETA-WDK7-PCB                        
097700                         ETA-INLC-PCB ETA-LEVA-PCB                        
097800                         ETA-WDB6-PCB ETA-WDD9-PCB                        
097900     .                                                                    
098000                                                                          
098100     EJECT                                                                
098200 CCAB-TIDIGAST-ANNULL-DATUM SECTION.                                      
098300                                                                          
098400     MOVE RAD-TITPO    TO DAT-I-TIDATUM                                   
098500     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
098600     CALL WDATKONV USING DAT-KDDATFORM                                    
098700                         DAT-I-TIDATUM                                    
098800                         DAT-O-TIDATUM                                    
098900                         DAT-KDSVAR                                       
099000                                                                          
099100     IF DAT-KDSVAR-OK                                                     
099200        MOVE DAT-TIAAVVD TO W-TIAAVVD                                     
099300        IF W-TIVV < CLAG-KVFRYSTI                                         
099400           IF W-TIAA = ZERO                                               
099500             MOVE 99 TO W-TIAA                                            
099600           ELSE                                                           
099700             COMPUTE W-TIAA = W-TIAA - 1                                  
099800           END-IF                                                         
099900           COMPUTE CLAG-KVFRYSTI = CLAG-KVFRYSTI - W-TIVV                 
100000           COMPUTE W-TIVV = 52 - CLAG-KVFRYSTI                            
100100        ELSE                                                              
100200           IF W-TIVV = CLAG-KVFRYSTI                                      
100300              IF W-TIAA = ZERO                                            
100400                MOVE 99 TO W-TIAA                                         
100500              ELSE                                                        
100600                COMPUTE W-TIAA = W-TIAA - 1                               
100700              END-IF                                                      
100800              MOVE 52 TO W-TIVV                                           
100900           ELSE                                                           
101000              COMPUTE W-TIVV = W-TIVV - CLAG-KVFRYSTI                     
101100           END-IF                                                         
101200        END-IF                                                            
101300        MOVE W-TIAAVVD    TO DAT-I-TIDATUM                                
101400        MOVE 'AAVVD'      TO DAT-KDDATFORM                                
101500        CALL WDATKONV USING DAT-KDDATFORM                                 
101600                            DAT-I-TIDATUM                                 
101700                            DAT-O-TIDATUM                                 
101800                            DAT-KDSVAR                                    
101900        IF DAT-KDSVAR-OK                                                  
102000           MOVE DAT-TIAAMMDD    TO MOD2-TIANNULL(IX-RAD)                  
102100        ELSE                                                              
102200           CALL FELLOG                                                    
102300           MOVE 'FEL AAVVD PÅ WDA5 I CCAA-SECTION' TO FELTEXT             
102400        END-IF                                                            
102500     ELSE                                                                 
102600        CALL FELLOG                                                       
102700        MOVE 'FEL TITPO PÅ WDA5 I CCAA-SECTION' TO FELTEXT                
102800     END-IF                                                               
102900     .                                                                    
103000     EJECT                                                                
103100 CCB-SPARA-RESTORDER-NYCKLAR SECTION.                                     
103200                                                                          
103300     MOVE RAD-IDORDNR5         TO  MOD2-IDORDNR7-NEXT                     
103400     MOVE 'R'                  TO  MOD2-FLSVAR-NEXT                       
103500     MOVE RAD-IDLOPNR          TO  MOD2-IDRADNR-NEXT                      
103600     .                                                                    
103700     EJECT                                                                
103800 D-KONTROLLERA-OM-SIDA-TOM SECTION.                                       
103900                                                                          
104000     IF IX-RAD                 = 1                                        
104100         MOVE 'B10'            TO MOD2-IDMFSFEL                           
104200     END-IF                                                               
104300     .                                                                    
104400     EJECT                                                                
104500 E-BERAKNA-MAX-MOD-LANGD SECTION.                                         
104600                                                                          
104700     COMPUTE MAX-MOD-LANGD = LENGTH OF MOD2-W9O34502 + 4                  
104800     MOVE 13                   TO  IX-RAD                                 
104900     PERFORM UNTIL IX-RAD      = 0                                        
105000         IF MOD2-IDORDNR7-RAD(IX-RAD) = ZERO                              
105100             SUBTRACT +69      FROM MAX-MOD-LANGD                         
105200             SUBTRACT +1       FROM IX-RAD                                
105300          ELSE                                                            
105400             MOVE ZERO         TO IX-RAD                                  
105500         END-IF                                                           
105600     END-PERFORM                                                          
105700     .                                                                    
105800     EJECT                                                                
105900                                                                          
106000* IMS SEKTIONER                                                           
106100     SKIP3                                                                
106200 IMS-GET-MSG SECTION.                                                     
106300                                                                          
106400     MOVE '  QC' TO GODK-STATUSKODER                                      
106500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
106600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106700     PERFORM IMS-STATUSKONTROLL                                           
106800     .                                                                    
106900     SKIP3                                                                
107000 IMS-INSERT-MSG SECTION.                                                  
107100                                                                          
107200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
107300     MOVE SPACE TO GODK-STATUSKODER                                       
107400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
107500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
107600     PERFORM IMS-STATUSKONTROLL                                           
107700     .                                                                    
107800     EJECT                                                                
107900 IMS-GET-ORQF01-KVAL SECTION.                                             
108000                                                                          
108100     STRING 'WLORQF01(WDQ401KY =' W-WDQ401KY-X ')'                        
108200            DELIMITED BY SIZE INTO SSA1                                   
108300     MOVE '  GE' TO GODK-STATUSKODER                                      
108400     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-ORQF01 SSA1                    
108500     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
108600     PERFORM IMS-STATUSKONTROLL                                           
108700     .                                                                    
108800     EJECT                                                                
108900 IMS-GET-ORQH01-KVAL SECTION.                                             
109000                                                                          
109100     STRING 'WLORQH01(WDQ4B1KY =' W-WDQ4B1KY-X ')'                        
109200            DELIMITED BY SIZE INTO SSA1                                   
109300     MOVE '  GE' TO GODK-STATUSKODER                                      
109400     CALL CBLTDLI USING GU ORQH-PCB DLI-IO-ORQH01 SSA1                    
109500     MOVE ORQH-STATUS-CODE TO STATUS-WS                                   
109600     PERFORM IMS-STATUSKONTROLL                                           
109700     .                                                                    
109800     EJECT                                                                
109900 IMS-GET-ORQH01-OKVAL SECTION.                                            
110000                                                                          
110100     STRING 'WLORQH01(WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
110200                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
110300                    '&IDGMTREF>=' W-Q4B1KY-MIN-IDGMTREF                   
110400                    '&IDGMTREF<=' W-Q4B1KY-MAX-IDGMTREF ')'               
110500            DELIMITED BY SIZE INTO SSA1                                   
110600     MOVE '  GE' TO GODK-STATUSKODER                                      
110700     CALL CBLTDLI USING GN ORQH-PCB DLI-IO-ORQH01 SSA1                    
110800     MOVE ORQH-STATUS-CODE TO STATUS-WS                                   
110900     PERFORM IMS-STATUSKONTROLL                                           
111000     .                                                                    
111100     EJECT                                                                
111200 IMS-GET-ORQI11-KVAL SECTION.                                             
111300                                                                          
111400     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
111500            DELIMITED BY SIZE INTO SSA1                                   
111600     STRING 'WLORQI11(WDQ211KY =' W-WDQ211KY-X ')'                        
111700            DELIMITED BY SIZE INTO SSA2                                   
111800     MOVE '  GE' TO GODK-STATUSKODER                                      
111900     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-ORQI11 SSA1 SSA2               
112000     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
112100     PERFORM IMS-STATUSKONTROLL                                           
112200     .                                                                    
112300     EJECT                                                                
112400 IMS-GET-ORQI12-KVAL SECTION.                                             
112500                                                                          
112600     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
112700            DELIMITED BY SIZE INTO SSA1                                   
112800     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
112900            DELIMITED BY SIZE INTO SSA2                                   
113000     MOVE '  ' TO GODK-STATUSKODER                                        
113100     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-ORQI12 SSA1 SSA2               
113200     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
113300     PERFORM IMS-STATUSKONTROLL                                           
113400     .                                                                    
113500     EJECT                                                                
113600 IMS-GET-ORDP01-KVAL SECTION.                                             
113700                                                                          
113800     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
113900            DELIMITED BY SIZE INTO SSA1                                   
114000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
114100     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-ORDP01 SSA1                    
114200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
114300     PERFORM IMS-STATUSKONTROLL                                           
114400     .                                                                    
114500     EJECT                                                                
114600 IMS-GET-ORDP01-OKVAL SECTION.                                            
114700                                                                          
114800     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN-X                        
114900                    '&WDA501KY<=' W-WDA501KY-MAX-X                        
115000                    '&IDARTNR  =' W-IDARTNR-X ')'                         
115100            DELIMITED BY SIZE INTO SSA1                                   
115200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
115300     CALL CBLTDLI USING GN ORDP-PCB DLI-IO-ORDP01 SSA1                    
115400     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
115500     PERFORM IMS-STATUSKONTROLL                                           
115600     .                                                                    
115700     EJECT                                                                
115800 IMS-GET-WDE401-11 SECTION.                                               
115900                                                                          
116000     STRING 'WDE401  *D(WDE401KY =' W-WDE401KY-X ')'                      
116100            DELIMITED BY SIZE INTO SSA1                                   
116200     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
116300            DELIMITED BY SIZE INTO SSA2                                   
116400     MOVE '  ' TO GODK-STATUSKODER                                        
116500     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E40120 SSA1 SSA2               
116600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
116700     PERFORM IMS-STATUSKONTROLL                                           
116800     .                                                                    
116900     EJECT                                                                
117000 IMS-GET-WDE4C-KVAL SECTION.                                              
117100                                                                          
117200     STRING 'WDE4C1  (WDE4C1KY =' W-WDE4C1KY-X                            
117300                    '&IDDISTR  =' W-IDDISTR-X                             
117400                    '&IDKUNDNR =' W-IDKUNDNR-X                            
117500                    '&IDKUNDRF =' W-IDKUNDRF-X ')'                        
117600            DELIMITED BY SIZE INTO SSA1                                   
117700     MOVE '  GE'  TO GODK-STATUSKODER                                     
117800     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-WDE4C SSA1                    
117900     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
118000     PERFORM IMS-STATUSKONTROLL                                           
118100     .                                                                    
118200     EJECT                                                                
118300 IMS-GET-WDE4C-OKVAL SECTION.                                             
118400                                                                          
118500     STRING 'WDE4C1  (WDE4C1KY>=' W-WDE4C1KY-MIN-X                        
118600                    '&WDE4C1KY<=' W-WDE4C1KY-MAX-X                        
118700                    '&IDDISTR  =' W-IDDISTR-X                             
118800                    '&IDKUNDNR =' W-IDKUNDNR-X ')'                        
118900            DELIMITED BY SIZE INTO SSA1                                   
119000     MOVE '  GE'  TO GODK-STATUSKODER                                     
119100     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-WDE4C SSA1                    
119200     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
119300     PERFORM IMS-STATUSKONTROLL                                           
119400     .                                                                    
119500     EJECT                                                                
119600 IMS-GU-ARTC11 SECTION.                                                   
119700                                                                          
119800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
119900            DELIMITED BY SIZE INTO SSA1                                   
120000     MOVE   'WLARTC11'         TO SSA2                                    
120100     MOVE '  GE' TO GODK-STATUSKODER                                      
120200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2               
120300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
120400     PERFORM IMS-STATUSKONTROLL                                           
120500     .                                                                    
120600     EJECT                                                                
120700 IMS-GU-WDE601 SECTION.                                                   
120800                                                                          
120900     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
121000            DELIMITED BY SIZE INTO SSA1                                   
121100     MOVE '  GE' TO GODK-STATUSKODER                                      
121200     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
121300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
121400     PERFORM IMS-STATUSKONTROLL                                           
121500     .                                                                    
121600     EJECT                                                                
121700 IMS-GET-WDF106 SECTION.                                                  
121800                                                                          
121900     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
122000          DELIMITED BY SIZE INTO SSA1                                     
122100     MOVE 'WLLEVA14 ' TO SSA2                                             
122200     MOVE '  GE' TO GODK-STATUSKODER                                      
122300     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-LEVA14 SSA1 SSA2               
122400     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
122500     PERFORM IMS-STATUSKONTROLL                                           
122600     .                                                                    
122700     EJECT                                                                
122800 IMS-STATUSKONTROLL SECTION.                                              
122900                                                                          
123000     SET STATUS-IX TO 1                                                   
123100     SEARCH GODK-STATUS                                                   
123200       AT END                                                             
123300         CALL  FELLOG                                                     
123400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
123500         CONTINUE                                                         
123600     END-SEARCH                                                           
123700     .                                                                    
