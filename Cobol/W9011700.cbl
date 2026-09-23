000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9011700.                                                
000300 AUTHOR.         GÖRAN KJELLSON                                           
000400 DATE-WRITTEN.   VINTERN 2017/2019                                        
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        TILLGÄNGLIGHETSFRÅGA - GRIP.                                     
000900*                                                                         
001000*        PROGRAMMET ANROPAR W911SLDO                                      
001100*                                                                         
001200*                                                                         
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W90117T                                             
001600*        MID:         W90117I2                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W90117O2/W90117O3                                   
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200 DATA DIVISION.                                                           
002300                                                                          
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600 77    IDPGM                     PIC X(8)    VALUE 'W9011700'.            
002700 77    CURRENT-SECTION           PIC X(16)   VALUE SPACE.                 
002800 77    CURRENT-IMS-SECTION       PIC X(16)   VALUE SPACE.                 
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77    ERROR-TEXT                PIC X(80) VALUE SPACE.                   
003200 77    FELTEXT                   PIC X(80) VALUE SPACE.                   
003300 77    KDRC-DISPLAY              PIC Z(5).                                
003400 77    RKOD-ABEND-WITH-DUMP      PIC S9(4)   COMP VALUE +1000.            
003500                                                                          
003600 77    YES                       PIC X       VALUE 'Y'.                   
003700 77    NOO                       PIC X       VALUE 'N'.                   
003800 77    WS-IDLEVART               PIC X(30)   VALUE SPACE.                 
003900 77    WS-IDARTNR-Z              PIC Z(9).                                
004000 77  MSG-IX                      PIC S9(9)   VALUE +0  COMP SYNC.         
004100                                                                          
004200 77    OK-SW                     PIC X       VALUE 'Y'.                   
004300   88  EVERYTHING-OK                         VALUE 'Y'.                   
004400   88  SOMETHING-WRONG                       VALUE 'N'.                   
004500                                                                          
004600 01    WS-IDSYSTEM               PIC X(4)    VALUE SPACE.                 
004700                                                                          
004800 77    REQUEST-SW                PIC X(3)    VALUE SPACE.                 
004900   88  LYNK-REQUEST                          VALUE 'LYN'.                 
005000   88  VCC-REQUEST                           VALUE 'VCC'.                 
005100                                                                          
005200 01    CURRENT-INPUT.                                                     
005300   03  CURRI-IDDISTR             PIC 9(4).                                
005400   03  CURRI-IDKUNDNR            PIC 9(6).                                
005500   03  CURRI-ADPOSTNR            PIC X(10).                               
005600   03  CURRI-IDLANDX2            PIC X(2).                                
005700   03  CURRI-IDLEVART            PIC X(30).                               
005800   03  CURRI-IDARTNR             PIC 9(9).                                
005900   03  CURRI-KVBEART             PIC 9(6).                                
006000   03  CURRI-FLKVBRYT            PIC X(1).                                
006100                                                                          
006200 01  CURRENT-OUTPUT.                                                      
006300   03 CURRO-IDDISTR        PIC 9(4)      VALUE ZERO.                      
006400   03 CURRO-IDKUNDNR       PIC 9(6)      VALUE ZERO.                      
006500   03 CURRO-IDARTNR        PIC 9(9)      VALUE ZERO.                      
006600   03 CURRO-IDLEVART       PIC X(30)     VALUE SPACE.                     
006700   03 CURRO-BEART          PIC X(25)     VALUE SPACE.                     
006800   03 CURRO-BEART-ENG      PIC X(25)     VALUE SPACE.                     
006900   03 CURRO-FLSVAR         PIC X         VALUE SPACE.                     
007000   03 CURRO-KVLS-DLEV      PIC 9(7)      VALUE ZERO.                      
007100   03 CURRO-KDSORT         PIC X(2)      VALUE SPACE.                     
007200   03 CURRO-KDORDBEK       PIC X(2)      VALUE SPACE.                     
007300   03 CURRO-TEORDBEK       PIC X(70)     VALUE SPACE.                     
007400   03 CURRO-TEORDBEK-ENG   PIC X(70)     VALUE SPACE.                     
007500   03 CURRO-TIBERANK-NUM   PIC 9(6)      VALUE ZERO.                      
007600   03 CURRO-TIDISPIN-NUM   PIC 9(6)      VALUE ZERO.                      
007700   03 CURRO-IDDC-LEV       PIC X(2)      VALUE SPACE.                     
007800   03 CURRO-PRINK          PIC 9(7)V9(2) VALUE ZERO.                      
007900   03 CURRO-KDFEL          PIC X(3)      VALUE SPACE.                     
008000   03 CURRO-IDARTNR-TILLK  PIC 9(9)      VALUE ZERO.                      
008100   03 CURRO-IDLEVART-TILLK PIC X(30)     VALUE SPACE.                     
008110   03 CURRO-FLLDCKND       PIC X         VALUE SPACE.                     
008200                                                                          
008300 01    W-BLANKS                  PIC  9(5)   VALUE ZERO.                  
008400 01    W-LENGTH                  PIC  9(5)   VALUE ZERO.                  
008500                                                                          
008600 01    W-IDARTNR-CHAR.                                                    
008700   03  W-IDARTNR-NUM             PIC  9(9)   VALUE ZERO.                  
008800                                                                          
008900 01    WS-DATUM-NUM              PIC 9(6)    VALUE ZERO.                  
009000 01    FILLER REDEFINES WS-DATUM-NUM.                                     
009100   03  WS-TIAA                   PIC 9(2).                                
009200   03  WS-TIMM                   PIC 9(2).                                
009300   03  WS-TIDD                   PIC 9(2).                                
009400 01    WS-DATUM-X.                                                        
009500   03  WS-TITT                   PIC 9(2) VALUE 20.                       
009600   03  WS-TIAA-X                 PIC 9(2).                                
009700   03  FILLER                    PIC X(1) VALUE '-'.                      
009800   03  WS-TIMM-X                 PIC 9(2).                                
009900   03  FILLER                    PIC X(1) VALUE '-'.                      
010000   03  WS-TIDD-X                 PIC 9(2).                                
010100*      --- VALID IDDC CODES                                               
010200*                                                                         
010300*01   --COPY WWDC99                                                       
010400*01   --COPY WWDCKONS                                                     
010500                                                                          
010600 01  DYNAMISKA-SUBPROGRAM.                                                
010700   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
010800   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
010900   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
011000   03  WZ01SUB                   PIC X(8)    VALUE 'WZ01SUB '.            
011100   03  WZ01AUTH                  PIC X(8)    VALUE 'WZ01AUTH'.            
011200   03  W009REDU                  PIC X(8)    VALUE 'W009REDU'.            
011300   03  W911SLDO                  PIC X(8)    VALUE 'W911SLDO'.            
011400   03  WMSGCONV                  PIC X(8)    VALUE 'WMSGCONV'.            
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011700     SKIP3                                                                
011800*01  -COPY WZ01SUB                                                        
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH   '.         
012100     SKIP3                                                                
012200*01  -COPY WZ01AUTH                                                       
012300*                                                                         
012400 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
012500*01  -COPY WMSGCONV                                                       
012600                                                                          
012700 01   FILLER             PIC X(5)  VALUE 'SLDO '.                         
012800*   -COPY W911SLDO                                                        
012900                                                                          
013000*    --- PARAMETERS FOR W009REDU                                          
013100 01  WS-REDUIN                   PIC X(30)  VALUE SPACE.                  
013200 01  WS-REDUUT                   PIC X(30)  VALUE SPACE.                  
013300                                                                          
013400******************************************************************        
013500*                                                                         
013600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
013900     SKIP3                                                                
014000 01  REQU-AREA.                                                           
014100*    03  -COPY WZ01REQ2                                                   
014200*    03  -COPY W90117I2                                                   
014300                                                                          
014400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
014500     SKIP3                                                                
014600 01  RESP-AREA.                                                           
014700*    03  -COPY WZ01RESP                                                   
014800*    03  -COPY W90117O2                                                   
014900                                                                          
015000 01  RESP-AREA-V3.                                                        
015100*    03  -COPY WZ01RES2                                                   
015200*    03  -COPY W90117O3                                                   
015300                                                                          
015400 01  MESSAGE-CODES.                                                       
015500     03  SYS-ERROR               PIC X(3)    VALUE '099'.                 
015600     03  BAD-REQUEST             PIC X(3)    VALUE '400'.                 
015700     03  NOT-FOUND               PIC X(3)    VALUE '404'.                 
015800     03  POSTAL-CODE-MISSING     PIC X(3)    VALUE 'B11'.                 
015900     03  ERR-IS-INVALID          PIC X(3)    VALUE '023'.                 
016000     03  ERR-NOT-FOUND           PIC X(3)    VALUE '025'.                 
016100     03  PART-NOT-FOUND          PIC X(3)    VALUE 'A01'.                 
016200     03  CUST-NOT-FOUND          PIC X(3)    VALUE 'A02'.                 
016300     03  POST-CODE-NOT-FOUND     PIC X(3)    VALUE 'A03'.                 
016400                                                                          
016500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016600 01  DLI-KEYS.                                                            
016700                                                                          
016800   03  W-WDF5BSEQ.                                                        
016900     05  W-SEQB-IDLEVART         PIC X(30)   VALUE LOW-VALUE.             
017000                                                                          
017100     03  W-IDARTNR-X.                                                     
017200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017300                                                                          
017400     03  W-IDLEVNR-X.                                                     
017500         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
017600                                                                          
017700     03  W-WDGXKEY-X.                                                     
017800         05  W-IDHTYP            PIC X(4)    VALUE '4133'.                
017900         05  W-IDSYSMOT          PIC X(10)   VALUE SPACE.                 
018000         05  FILLER              PIC X(16)   VALUE LOW-VALUE.             
018100                                                                          
018200     03  W-IDLANDX2              PIC X(2)    VALUE SPACE .                
018300     03  W-ADPOSTNR              PIC X(10)   VALUE SPACE.                 
018400     03  W-ADPOSTNR-DEF          PIC X(10)   VALUE '9999999999'.          
018500                                                                          
018600                                                                          
018700 01  STATUS-WS                   PIC XX.                                  
018800     88  SEGMENT-FOUND                       VALUE '  '.                  
018900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
019000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
019100     88  END-OF-DATA                         VALUE 'GB'.                  
019200                                                                          
019300 01  GOOD-STATUSCODES.                                                    
019400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019500                                                                          
019600 01  ALL-SSA.                                                             
019700     03 SSA1                     PIC X(80).                               
019800     03 SSA2                     PIC X(120).                              
019900                                                                          
020000                                                                          
020100*    --- IMS FUNCTION CODES                                               
020200*01  -COPY W0003                                                          
020300                                                                          
020400 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDF501'.          
020500 01  DLI-IO-WDF501.                                                       
020600*    03  -COPY WDF501                                                     
020700                                                                          
020800 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDF502'.          
020900 01  DLI-IO-WDF502.                                                       
021000*    03  -COPY WDF502                                                     
021100                                                                          
021200 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDGX4134'.        
021300 01  DLI-IO-WDGX4134.                                                     
021400*    03  -COPY WDGX4134                                                   
021500                                                                          
021600                                                                          
021700                                                                          
021800                                                                          
021900 LINKAGE SECTION.                                                         
022000                                                                          
022100*01  -COPY W0009     -PRE MSG-                                            
022200                                                                          
022300 01  ATAB-PCB                    PIC X.                                   
022400                                                                          
022500*01  -COPY W0008     -PRE WDF5B-                                          
022600     05  FILLER                  PIC X.                                   
022700                                                                          
022800*01  -COPY W0008     -PRE WDF5-                                           
022900     05  FILLER                  PIC X.                                   
023000                                                                          
023100*01  -COPY W0008     -PRE WDR5-                                           
023200     05  FILLER                  PIC X.                                   
023300                                                                          
023400 01  SLDO-WDF1-PCB               PIC X.                                   
023500 01  SLDO-WDF2-PCB               PIC X.                                   
023600 01  SLDO-WDF2A-PCB              PIC X.                                   
023700 01  SLDO-WDK7-PCB               PIC X.                                   
023800 01  SLDO-WDK6-PCB               PIC X.                                   
023900 01  SLDO-XXKJ-PCB               PIC X.                                   
024000 01  SLDO-WDD7-PCB               PIC X.                                   
024100 01  SLDO-BENA-PCB               PIC X.                                   
024200 01  SLDO-WDB3-PCB               PIC X.                                   
024300 01  SLDO-WDR6-PCB               PIC X.                                   
024310 01  SLDO-WDB2-PCB               PIC X.                                   
024320 01  SLDO-WDB1-PCB               PIC X.                                   
024400                                                                          
025000 01  SLDO-KVAN-WDB2-PCB          PIC X.                                   
025100 01  SLDO-KVAN-WDC1-PCB          PIC X.                                   
025200                                                                          
025300 01  SLDO-AREG-WDK6-PCB          PIC X.                                   
025400 01  SLDO-AREG-WDK7-PCB          PIC X.                                   
025500                                                                          
025600 01  SLDO-DLEV-LEVF-PCB          PIC X.                                   
025700 01  SLDO-DLEV-LEVG-PCB          PIC X.                                   
025800 01  SLDO-DLEV-LEVA-PCB          PIC X.                                   
025900 01  SLDO-DLEV-ARTS-PCB          PIC X.                                   
026000 01  SLDO-DLEV-WDB6-PCB          PIC X.                                   
026100 01  SLDO-DLEV-FILA-PCB          PIC X.                                   
026200                                                                          
026300 01  SLDO-SPAR-WDF8-PCB          PIC X.                                   
026400 01  SLDO-SPAR-WDF8A-PCB         PIC X.                                   
026500 01  SLDO-SPAR-WDK6-PCB          PIC X.                                   
026600                                                                          
026700 01  SLDO-SDCA-ARTS-PCB          PIC X.                                   
026800 01  SLDO-SDCA-WDB6-PCB          PIC X.                                   
026900 01  SLDO-SDCA-WDK9-PCB          PIC X.                                   
027000 01  SLDO-SDCA-WDR6-PCB          PIC X.                                   
027100 01  SLDO-SDCA-WDK6-PCB          PIC X.                                   
027200 01  SLDO-SDCA-WDQ4B-PCB         PIC X.                                   
027300 01  SLDO-SDCA-WDQ2-PCB          PIC X.                                   
027400 01  SLDO-SDCA-WDQ4-PCB          PIC X.                                   
027500 01  SLDO-SDCA-WDB6-2-PCB        PIC X.                                   
027600 01  SLDO-SDCA-WDK6-2-PCB        PIC X.                                   
027700 01  SLDO-SDCA-WDK7-2-PCB        PIC X.                                   
027800 01  SLDO-SDCA-WDK7-3-PCB        PIC X.                                   
027900                                                                          
028000 01  SLDO-NDCA-USEA-PCB          PIC X.                                   
028100 01  SLDO-NDCA-WDK7-PCB          PIC X.                                   
028200 01  SLDO-NDCA-WDL6-PCB          PIC X.                                   
028300 01  SLDO-NDCA-WDB6-PCB          PIC X.                                   
028400                                                                          
028500 01  SLDO-RANS-XXKM-PCB          PIC X.                                   
028600 01  SLDO-RANS-ARTM-PCB          PIC X.                                   
028700 01  SLDO-RANS-ARTS-PCB          PIC X.                                   
028800                                                                          
028900 01  SLDO-CDCA-ARTM-PCB          PIC X.                                   
029000 01  SLDO-CDCA-INLB-PCB          PIC X.                                   
029100 01  SLDO-CDCA-WDB2-PCB          PIC X.                                   
029200 01  SLDO-CDCA-WDC1-PCB          PIC X.                                   
029300                                                                          
029400 01  SLDO-CLDC-WDB6-PCB          PIC X.                                   
029500                                                                          
029600 01  SLDO-ETA-ARTC-PCB           PIC X.                                   
029700 01  SLDO-ETA-WDK7-PCB           PIC X.                                   
029800 01  SLDO-ETA-INLC-PCB           PIC X.                                   
029900 01  SLDO-ETA-LEVA-PCB           PIC X.                                   
030000 01  SLDO-ETA-WDB6-PCB           PIC X.                                   
030100 01  SLDO-ETA-WDD9-PCB           PIC X.                                   
030200                                                                          
030300 01  SLDO-XDCA-USEA-PCB          PIC X.                                   
030400 01  SLDO-XDCA-WDB6-PCB          PIC X.                                   
030500 01  SLDO-XDCA-WDK6-PCB          PIC X.                                   
030600 01  SLDO-XDCA-WDK7-PCB          PIC X.                                   
030700 01  SLDO-XDCA-WDK9-PCB          PIC X.                                   
030800 01  SLDO-XDCA-WDL6-PCB          PIC X.                                   
030900 01  SLDO-XDCA-WDQ4B-PCB         PIC X.                                   
031000 01  SLDO-XDCA-WDQ2-PCB          PIC X.                                   
031100 01  SLDO-XDCA-WDQ4-PCB          PIC X.                                   
031200 01  SLDO-XDCA-WDR6-PCB          PIC X.                                   
031300 01  SLDO-XDCA-WDB6-2-PCB        PIC X.                                   
031400 01  SLDO-XDCA-WDK6-2-PCB        PIC X.                                   
031500 01  SLDO-XDCA-WDK7-2-PCB        PIC X.                                   
031600 01  SLDO-XDCA-WDK7-3-PCB        PIC X.                                   
031700                                                                          
031800 PROCEDURE DIVISION  USING MSG-PCB ATAB-PCB WDF5B-PCB WDF5-PCB            
031900                                                      WDR5-PCB            
032000                           SLDO-WDF1-PCB SLDO-WDF2-PCB                    
032100                           SLDO-WDF2A-PCB SLDO-WDK7-PCB                   
032200                           SLDO-WDK6-PCB                                  
032300                           SLDO-XXKJ-PCB SLDO-WDD7-PCB                    
032400                           SLDO-BENA-PCB SLDO-WDB3-PCB                    
032500                           SLDO-WDR6-PCB                                  
032510                           SLDO-WDB2-PCB SLDO-WDB1-PCB                    
032600                                                                          
033000                           SLDO-KVAN-WDB2-PCB SLDO-KVAN-WDC1-PCB          
033100                                                                          
033200                           SLDO-AREG-WDK6-PCB SLDO-AREG-WDK7-PCB          
033300                                                                          
033400                           SLDO-DLEV-LEVF-PCB SLDO-DLEV-LEVG-PCB          
033500                           SLDO-DLEV-LEVA-PCB SLDO-DLEV-ARTS-PCB          
033600                           SLDO-DLEV-WDB6-PCB SLDO-DLEV-FILA-PCB          
033700                                                                          
033800                           SLDO-SPAR-WDF8-PCB SLDO-SPAR-WDF8A-PCB         
033900                           SLDO-SPAR-WDK6-PCB                             
034000                                                                          
034100                           SLDO-SDCA-ARTS-PCB SLDO-SDCA-WDB6-PCB          
034200                           SLDO-SDCA-WDK9-PCB SLDO-SDCA-WDR6-PCB          
034300                           SLDO-SDCA-WDK6-PCB SLDO-SDCA-WDQ4B-PCB         
034400                           SLDO-SDCA-WDQ2-PCB SLDO-SDCA-WDQ4-PCB          
034500                           SLDO-SDCA-WDB6-2-PCB                           
034600                           SLDO-SDCA-WDK6-2-PCB                           
034700                           SLDO-SDCA-WDK7-2-PCB                           
034800                           SLDO-SDCA-WDK7-3-PCB                           
034900                                                                          
035000                           SLDO-NDCA-USEA-PCB SLDO-NDCA-WDK7-PCB          
035100                           SLDO-NDCA-WDL6-PCB SLDO-NDCA-WDB6-PCB          
035200                                                                          
035300                           SLDO-RANS-XXKM-PCB SLDO-RANS-ARTM-PCB          
035400                           SLDO-RANS-ARTS-PCB                             
035500                                                                          
035600                           SLDO-CDCA-ARTM-PCB SLDO-CDCA-INLB-PCB          
035700                           SLDO-CDCA-WDB2-PCB SLDO-CDCA-WDC1-PCB          
035800                                                                          
035900                           SLDO-CLDC-WDB6-PCB                             
036000                                                                          
036100                           SLDO-ETA-ARTC-PCB  SLDO-ETA-WDK7-PCB           
036200                           SLDO-ETA-INLC-PCB  SLDO-ETA-LEVA-PCB           
036300                           SLDO-ETA-WDB6-PCB  SLDO-ETA-WDD9-PCB           
036400                                                                          
036500                           SLDO-XDCA-USEA-PCB                             
036600                           SLDO-XDCA-WDB6-PCB SLDO-XDCA-WDK6-PCB          
036700                           SLDO-XDCA-WDK7-PCB SLDO-XDCA-WDK9-PCB          
036800                           SLDO-XDCA-WDL6-PCB SLDO-XDCA-WDQ4B-PCB         
036900                           SLDO-XDCA-WDQ2-PCB SLDO-XDCA-WDQ4-PCB          
037000                           SLDO-XDCA-WDR6-PCB                             
037100                           SLDO-XDCA-WDB6-2-PCB                           
037200                           SLDO-XDCA-WDK6-2-PCB                           
037300                           SLDO-XDCA-WDK7-2-PCB                           
037400                           SLDO-XDCA-WDK7-3-PCB.                          
037500 MAIN SECTION.                                                            
037600                                                                          
037700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
037800     IF SUB-KDRC = 0                                                      
037900       MOVE 001           TO AUTH-KDCALL                                  
038000       CALL WZ01AUTH USING AUTH-WZ01AUTH REQU-WZ01REQ2                    
038100       IF AUTH-KDRC = 0 OR 4                                              
038200          IF REQU-KDPGMACT = 'S'                                          
038300             PERFORM A-INIT-SPARA-INPUT                                   
038400             IF EVERYTHING-OK                                             
038500                PERFORM B-BEHANDLA-FRAGA                                  
038600             END-IF                                                       
038700          ELSE                                                            
038800            MOVE SYS-ERROR  TO RESP-IDMSG-ERROR                           
038900                               OF RESP-AREA                               
039000            MOVE 'KDPGMACT' TO RESP-IDELMT-ERROR                          
039100                               OF RESP-AREA                               
039200          END-IF                                                          
039300       ELSE                                                               
039400          IF AUTH-KDRC = 4                                                
039500            MOVE BAD-REQUEST TO RESP-IDMSG-ERROR                          
039600                                OF RESP-AREA                              
039700          ELSE                                                            
039800            MOVE AUTH-KDRC TO KDRC-DISPLAY                                
039900            STRING 'WZ01AUTH GETARG ERROR RC=' KDRC-DISPLAY               
040000            DELIMITED BY SIZE INTO ERROR-TEXT                             
040100            CALL ABEND USING RKOD-ABEND-WITH-DUMP                         
040200          END-IF                                                          
040300       END-IF                                                             
040400       PERFORM C-EDIT-RESPONSE                                            
040500       PERFORM S02-RETURN-RESPONSE                                        
040600     END-IF                                                               
040700                                                                          
040800     MOVE ZERO                         TO RETURN-CODE                     
040900                                                                          
041000     GOBACK                                                               
041100     .                                                                    
041200                                                                          
041300                                                                          
041400 A-INIT-SPARA-INPUT SECTION.                                              
041500      MOVE 'A-INIT-SPARA-INPUT' TO CURRENT-SECTION                        
041600                                                                          
041700      MOVE REQU-IDOUTVER       TO RESP-IDMSGVER                           
041800      MOVE SPACE               TO RESP-IDMSG-ERROR                        
041900                                  OF RESP-AREA                            
042000                                  RESP-IDMSG-INFO                         
042100                                  OF RESP-AREA                            
042200                                  RESP-IDELMT-ERROR                       
042300                                  OF RESP-AREA                            
042400                                                                          
042500     IF REQU-IDOUTVER = 002                                               
042600        INITIALIZE RESP-W90117O2-CTX                                      
042700     ELSE                                                                 
042800        INITIALIZE RESP-W90117O3-CTX                                      
042900     END-IF                                                               
043000     MOVE ZERO         TO CURRO-IDDISTR                                   
043100     MOVE ZERO         TO CURRO-IDKUNDNR                                  
043200     MOVE SPACE        TO CURRO-IDLEVART                                  
043300     MOVE SPACE        TO CURRO-BEART                                     
043400     MOVE SPACE        TO CURRO-BEART-ENG                                 
043500     MOVE NOO          TO CURRO-FLSVAR                                    
043600     MOVE ZERO         TO CURRO-KVLS-DLEV                                 
043700     MOVE SPACE        TO CURRO-KDSORT                                    
043800     MOVE SPACE        TO CURRO-KDORDBEK                                  
043900     MOVE SPACE        TO CURRO-TEORDBEK                                  
044000     MOVE SPACE        TO CURRO-TEORDBEK-ENG                              
044100     MOVE ZERO         TO CURRO-TIBERANK-NUM                              
044200     MOVE ZERO         TO CURRO-TIDISPIN-NUM                              
044300     MOVE SPACE        TO CURRO-IDDC-LEV                                  
044400     MOVE ZERO         TO CURRO-PRINK                                     
044500     MOVE SPACE        TO CURRO-KDFEL                                     
044600     MOVE ZERO         TO CURRO-IDARTNR-TILLK                             
044700     MOVE SPACE        TO CURRO-IDLEVART-TILLK                            
044800     MOVE AUTH-IDSYSTEM        TO WS-IDSYSTEM                             
044900                                                                          
045000     IF REQU-IDDISTR OF REQU-W90117I2-CTX IS NOT NUMERIC                  
045100        MOVE NOO               TO OK-SW                                   
045200        MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                        
045300                                  OF RESP-AREA                            
045400        MOVE 'INVALID DISTRICT'                                           
045500                               TO RESP-IDELMT-ERROR                       
045600                                  OF RESP-AREA                            
045700     END-IF                                                               
045800                                                                          
045900     IF REQU-IDKUNDNR OF REQU-W90117I2-CTX IS NOT NUMERIC                 
046000        MOVE NOO               TO OK-SW                                   
046100        MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                        
046200                                  OF RESP-AREA                            
046300        MOVE 'INVALID CUSTOMER'                                           
046400                               TO RESP-IDELMT-ERROR                       
046500                                  OF RESP-AREA                            
046600     END-IF                                                               
046700                                                                          
046800     IF AUTH-IDSYSTEM = 'LYNK'                                            
046900        MOVE 'LYNK' TO REQUEST-SW                                         
047000     ELSE                                                                 
047100        MOVE 'VCC ' TO REQUEST-SW                                         
047200     END-IF                                                               
047300     MOVE REQU-IDDISTR     OF REQU-W90117I2-CTX                           
047400                            TO CURRI-IDDISTR                              
047500     MOVE REQU-IDKUNDNR OF REQU-W90117I2-CTX                              
047600                            TO CURRI-IDKUNDNR                             
047700     MOVE REQU-ADPOSTNR OF REQU-W90117I2-CTX                              
047800                            TO CURRI-ADPOSTNR                             
047900     MOVE REQU-IDLANDX2 OF REQU-W90117I2-CTX                              
048000                            TO CURRI-IDLANDX2                             
048100     PERFORM AA-CHECK-OUT-PART-NO                                         
048200     MOVE REQU-KVBEART     OF REQU-W90117I2-CTX                           
048300                            TO CURRI-KVBEART                              
048400     IF CURRI-KVBEART NOT NUMERIC OR                                      
048500        CURRI-KVBEART            = ZERO                                   
048600        MOVE NOO                TO OK-SW                                  
048700        MOVE NOT-FOUND          TO RESP-IDMSG-ERROR                       
048800                                   OF RESP-AREA                           
048900        MOVE 'QUANTITY MISSING'                                           
049000                                TO RESP-IDELMT-ERROR                      
049100                                   OF RESP-AREA                           
049200     END-IF                                                               
049300                                                                          
049400     MOVE REQU-FLKVBRYT OF REQU-W90117I2-CTX                              
049500                            TO CURRI-FLKVBRYT                             
049600                                                                          
049700     .                                                                    
049800                                                                          
049900                                                                          
050000 AA-CHECK-OUT-PART-NO SECTION.                                            
050100     MOVE 'AA-CHECK-PART   ' TO CURRENT-SECTION                           
050200                                                                          
050300     INSPECT FUNCTION REVERSE(REQU-IDLEVART) TALLYING W-BLANKS            
050400                      FOR LEADING SPACES                                  
050500     IF LYNK-REQUEST                                                      
050600        MOVE REQU-IDLEVART     TO CURRI-IDLEVART                          
050700                                  CURRO-IDLEVART                          
050800        MOVE ZERO              TO CURRI-IDARTNR                           
050900     ELSE                                                                 
051000        COMPUTE W-LENGTH = 30 - W-BLANKS                                  
051100        IF W-LENGTH <= 9 AND                                              
051200           REQU-IDLEVART(1:W-LENGTH) NUMERIC                              
051300           MOVE REQU-IDLEVART(1:W-LENGTH)                                 
051400                                 TO CURRI-IDARTNR                         
051500                                    CURRO-IDARTNR                         
051600                                    CURRI-IDLEVART                        
051700                                    CURRO-IDLEVART                        
051800        ELSE                                                              
051900           MOVE NOO              TO OK-SW                                 
052000           MOVE 58               TO RESP-IDMSG-ERROR                      
052100                                    OF RESP-AREA                          
052200           MOVE REQU-IDLEVART    TO CURRI-IDLEVART                        
052300                                    CURRO-IDLEVART                        
052400        END-IF                                                            
052500     END-IF                                                               
052600     .                                                                    
052700                                                                          
052800                                                                          
052900 B-BEHANDLA-FRAGA SECTION.                                                
053000     MOVE 'B-BEHANDLA-FRAGA' TO CURRENT-SECTION                           
053100                                                                          
053200     IF CURRI-IDDISTR = ZERO AND                                          
053300        CURRI-IDKUNDNR = ZERO                                             
053400        PERFORM BA-GET-DISTRICT-CUSTOMER                                  
053500        IF EVERYTHING-OK                                                  
053600           MOVE 4134-IDDISTR     TO CURRI-IDDISTR                         
053700                                    CURRO-IDDISTR                         
053800                                    SLDO-IDDISTR-IN                       
053900           MOVE 4134-IDKUNDNR    TO CURRI-IDKUNDNR                        
054000                                    CURRO-IDKUNDNR                        
054100                                    SLDO-IDKUNDNR-IN                      
054200        END-IF                                                            
054300     ELSE                                                                 
054400        MOVE CURRI-IDDISTR       TO CURRO-IDDISTR                         
054500                                    SLDO-IDDISTR-IN                       
054600        MOVE CURRI-IDKUNDNR      TO CURRO-IDKUNDNR                        
054700                                    SLDO-IDKUNDNR-IN                      
054800     END-IF                                                               
054900                                                                          
055000     IF EVERYTHING-OK                                                     
055100        IF LYNK-REQUEST                                                   
055200           PERFORM BB-CONVERT-PARTNO                                      
055300           MOVE XART-IDARTNR     TO SLDO-IDARTNR-IN                       
055400        ELSE                                                              
055500           MOVE CURRI-IDARTNR    TO SLDO-IDARTNR-IN                       
055600        END-IF                                                            
055700                                                                          
055800        MOVE CURRI-KVBEART       TO SLDO-KVBEART-IN                       
055900        IF CURRI-FLKVBRYT = 'Y'                                           
056000           MOVE 1                TO SLDO-KDORDKL-IN                       
056100        ELSE                                                              
056200           MOVE 4                TO SLDO-KDORDKL-IN                       
056300        END-IF                                                            
056400                                                                          
056500        MOVE ZERO            TO SLDO-KVAVBART                             
056600                                SLDO-TIDISPIN                             
056700                                SLDO-KDORDBEK                             
056800                                SLDO-FLTPO1                               
056900                                SLDO-KVFRYSTI                             
057000        MOVE SPACE           TO SLDO-IDDC                                 
057100                                SLDO-IDMFSMED                             
057200                                                                          
057300        CALL W911SLDO USING SLDO-W911SLDO                                 
057400                            SLDO-WDF1-PCB SLDO-WDF2-PCB                   
057500                            SLDO-WDF2A-PCB SLDO-WDK7-PCB                  
057600                            SLDO-WDK6-PCB                                 
057700                            SLDO-XXKJ-PCB SLDO-WDD7-PCB                   
057800                            SLDO-BENA-PCB SLDO-WDB3-PCB                   
057900                            SLDO-WDR6-PCB                                 
057910                            SLDO-WDB2-PCB SLDO-WDB1-PCB                   
058000                                                                          
058400                            SLDO-KVAN-WDB2-PCB SLDO-KVAN-WDC1-PCB         
058500                                                                          
058600                            SLDO-AREG-WDK6-PCB SLDO-AREG-WDK7-PCB         
058700                                                                          
058800                            SLDO-DLEV-LEVF-PCB SLDO-DLEV-LEVG-PCB         
058900                            SLDO-DLEV-LEVA-PCB SLDO-DLEV-ARTS-PCB         
059000                            SLDO-DLEV-WDB6-PCB SLDO-DLEV-FILA-PCB         
059100                                                                          
059200                            SLDO-SPAR-WDF8-PCB SLDO-SPAR-WDF8A-PCB        
059300                            SLDO-SPAR-WDK6-PCB                            
059400                                                                          
059500                            SLDO-SDCA-ARTS-PCB SLDO-SDCA-WDB6-PCB         
059600                            SLDO-SDCA-WDK9-PCB SLDO-SDCA-WDR6-PCB         
059700                            SLDO-SDCA-WDK6-PCB SLDO-SDCA-WDQ4B-PCB        
059800                            SLDO-SDCA-WDQ2-PCB SLDO-SDCA-WDQ4-PCB         
059900                            SLDO-SDCA-WDB6-2-PCB                          
060000                            SLDO-SDCA-WDK6-2-PCB                          
060100                            SLDO-SDCA-WDK7-2-PCB                          
060200                            SLDO-SDCA-WDK7-3-PCB                          
060300                                                                          
060400                            SLDO-NDCA-USEA-PCB SLDO-NDCA-WDK7-PCB         
060500                            SLDO-NDCA-WDL6-PCB SLDO-NDCA-WDB6-PCB         
060600                                                                          
060700                            SLDO-RANS-XXKM-PCB SLDO-RANS-ARTM-PCB         
060800                            SLDO-RANS-ARTS-PCB                            
060900                                                                          
061000                            SLDO-CDCA-ARTM-PCB SLDO-CDCA-INLB-PCB         
061100                            SLDO-CDCA-WDB2-PCB SLDO-CDCA-WDC1-PCB         
061200                                                                          
061300                            SLDO-CLDC-WDB6-PCB                            
061400                            SLDO-ETA-ARTC-PCB  SLDO-ETA-WDK7-PCB          
061500                            SLDO-ETA-INLC-PCB  SLDO-ETA-LEVA-PCB          
061600                            SLDO-ETA-WDB6-PCB  SLDO-ETA-WDD9-PCB          
061700                                                                          
061800                            SLDO-XDCA-USEA-PCB                            
061900                            SLDO-XDCA-WDB6-PCB SLDO-XDCA-WDK6-PCB         
062000                            SLDO-XDCA-WDK7-PCB SLDO-XDCA-WDK9-PCB         
062100                            SLDO-XDCA-WDL6-PCB SLDO-XDCA-WDQ4B-PCB        
062200                            SLDO-XDCA-WDQ2-PCB SLDO-XDCA-WDQ4-PCB         
062300                            SLDO-XDCA-WDR6-PCB                            
062400                            SLDO-XDCA-WDB6-2-PCB                          
062500                            SLDO-XDCA-WDK6-2-PCB                          
062600                            SLDO-XDCA-WDK7-2-PCB                          
062700                            SLDO-XDCA-WDK7-3-PCB                          
062800******************************************************                    
062900*                                                    *                    
063000*    SLDO-IDMFSMED  B10-FELAKTIG KUND                *                    
063100*                   010-ARTIKEL KAN LEVERERAS        *                    
063200*                   080-ARTIKELN KAN INTE LEVERERAS  *                    
063300*                                                    *                    
063400******************************************************                    
063500                                                                          
063600        MOVE ZERO               TO RESP-IDMSG-ERROR                       
063700                                   OF RESP-AREA                           
063800        MOVE SLDO-KVAVBART      TO CURRO-KVLS-DLEV                        
063900        MOVE SLDO-BEART         TO CURRO-BEART                            
064000        MOVE SLDO-BEART-ENG     TO CURRO-BEART-ENG                        
064100        MOVE SLDO-KDSORT        TO CURRO-KDSORT                           
064200        MOVE SLDO-KDORDBEK      TO CURRO-KDORDBEK                         
064300        MOVE SLDO-TEORDBEK      TO CURRO-TEORDBEK                         
064400        MOVE SLDO-TEORDBEK-ENG  TO CURRO-TEORDBEK-ENG                     
064500        MOVE SLDO-TIKLAR        TO CURRO-TIBERANK-NUM                     
064600        MOVE SLDO-TIDISPIN      TO CURRO-TIDISPIN-NUM                     
064700                                                                          
064800        MOVE SLDO-IDDC          TO CURRO-IDDC-LEV                         
064900        MOVE SLDO-PRINK         TO CURRO-PRINK                            
065000        MOVE SPACE              TO CURRO-KDFEL                            
065100                                   CURRO-FLSVAR                           
065200        MOVE SLDO-IDARTNR-TILLK TO CURRO-IDARTNR-TILLK                    
065210        MOVE SLDO-FLLDCKND      TO CURRO-FLLDCKND                         
065300        IF LYNK-REQUEST                                                   
065400           PERFORM BD-CONVERT-IDLEVART                                    
065500           IF WS-IDLEVART NOT = SPACE                                     
065600              MOVE WS-IDLEVART  TO CURRO-IDLEVART-TILLK                   
065700           ELSE                                                           
065800              IF SLDO-IDARTNR-TILLK = ZERO                                
065900                 MOVE SPACE     TO CURRO-IDLEVART-TILLK                   
066000              ELSE                                                        
066100                 MOVE SLDO-IDARTNR-TILLK                                  
066200                                TO WS-IDARTNR-Z                           
066300                 MOVE FUNCTION TRIM (WS-IDARTNR-Z)                        
066400                                TO CURRO-IDLEVART-TILLK                   
066500              END-IF                                                      
066600           END-IF                                                         
066700        ELSE                                                              
066800           IF SLDO-IDARTNR-TILLK = ZERO                                   
066900              MOVE SPACE        TO CURRO-IDLEVART-TILLK                   
067000           ELSE                                                           
067100              MOVE SLDO-IDARTNR-TILLK                                     
067200                                TO WS-IDARTNR-Z                           
067300              MOVE FUNCTION TRIM (WS-IDARTNR-Z)                           
067400                                TO CURRO-IDLEVART-TILLK                   
067500           END-IF                                                         
067600        END-IF                                                            
067700        IF SLDO-IDMFSMED = 'B10'                                          
067800           MOVE SLDO-IDMFSMED   TO CURRO-KDFEL                            
067900                                   RESP-IDMSG-ERROR                       
068000                                   OF RESP-AREA                           
068100        END-IF                                                            
068200        IF SLDO-KDORDBEK = 58                                             
068300           MOVE SLDO-KDORDBEK   TO CURRO-KDFEL                            
068400                                   RESP-IDMSG-ERROR                       
068500                                   OF RESP-AREA                           
068600        END-IF                                                            
068700        IF SLDO-IDMFSMED = '010'                                          
068800           MOVE YES TO CURRO-FLSVAR                                       
068900        ELSE                                                              
069000           MOVE NOO TO CURRO-FLSVAR                                       
069100        END-IF                                                            
069200     END-IF                                                               
069300     .                                                                    
069400                                                                          
069500                                                                          
069600 BA-GET-DISTRICT-CUSTOMER SECTION.                                        
069700     MOVE 'BA-GET-DIST-CUST' TO CURRENT-SECTION                           
069800                                                                          
069900     MOVE REQU-ADPOSTNR    TO WS-REDUIN                                   
070000     CALL W009REDU USING WS-REDUIN WS-REDUUT                              
070100     MOVE WS-REDUUT        TO W-ADPOSTNR                                  
070200     MOVE WS-IDSYSTEM      TO W-IDSYSMOT                                  
070300     MOVE REQU-IDLANDX2    TO W-IDLANDX2                                  
070400     PERFORM IMS-GU-WDGX4134                                              
070500     IF SEGMENT-MISSING                                                   
070600        MOVE NOO           TO OK-SW                                       
070700        MOVE POSTAL-CODE-MISSING                                          
070800                           TO RESP-IDMSG-ERROR                            
070900                              OF RESP-AREA                                
071000     END-IF                                                               
071100     .                                                                    
071200                                                                          
071300 BB-CONVERT-PARTNO SECTION.                                               
071400     MOVE 'BB-CONVERT-PARTNO' TO CURRENT-SECTION                          
071500                                                                          
071600                                                                          
071700     IF LYNK-REQUEST                                                      
071800        MOVE CURRI-IDLEVART  TO  W-SEQB-IDLEVART                          
071900        PERFORM IMS-GU-WDF501-BSEQ                                        
072000        IF SEGMENT-MISSING                                                
072100           MOVE ZERO         TO XART-IDARTNR                              
072200        END-IF                                                            
072300     END-IF                                                               
072400     .                                                                    
072500                                                                          
072600                                                                          
072700 BC-CONVERT-DATE SECTION.                                                 
072800     MOVE 'BC-CONVERT-DATE ' TO CURRENT-SECTION                           
072900                                                                          
073000     MOVE WS-TIAA              TO WS-TIAA-X                               
073100     MOVE WS-TIMM              TO WS-TIMM-X                               
073200     MOVE WS-TIDD              TO WS-TIDD-X                               
073300     .                                                                    
073400                                                                          
073500                                                                          
073600 BD-CONVERT-IDLEVART SECTION.                                             
073700     MOVE 'BD-CONVERT-ART  ' TO CURRENT-SECTION                           
073800                                                                          
073900     IF SLDO-IDARTNR-TILLK > ZERO                                         
074000       MOVE SLDO-IDARTNR-TILLK TO W-IDARTNR                               
074100       PERFORM IMS-GU-WDF501                                              
074200       PERFORM IMS-GNP-WDF502                                             
074300       IF SEGMENT-FOUND                                                   
074400          MOVE XLEV-IDLEVART   TO WS-IDLEVART                             
074500       ELSE                                                               
074600          MOVE SPACE           TO WS-IDLEVART                             
074700       END-IF                                                             
074800     ELSE                                                                 
074900       MOVE SPACE              TO WS-IDLEVART                             
075000     END-IF                                                               
075100     .                                                                    
075200                                                                          
075300                                                                          
075400 C-EDIT-RESPONSE SECTION.                                                 
075500     MOVE 'C-EDIT-RESPONSE ' TO CURRENT-SECTION                           
075600                                                                          
075700     IF REQU-IDOUTVER = 002                                               
075800        MOVE CURRO-IDDISTR           TO RESP-IDDISTR                      
075900                                     IN RESP-W90117O2-CTX                 
076000        MOVE CURRO-IDKUNDNR          TO RESP-IDKUNDNR                     
076100                                     IN RESP-W90117O2-CTX                 
076200        MOVE CURRO-FLSVAR            TO RESP-FLSVAR                       
076300                                     IN RESP-W90117O2-CTX                 
076400        MOVE CURRO-KDORDBEK          TO RESP-KDORDBEK                     
076500                                     IN RESP-W90117O2-CTX                 
076600        MOVE CURRO-TEORDBEK-ENG      TO RESP-TEORDBEK                     
076700                                     IN RESP-W90117O2-CTX                 
076800        IF CURRO-TIBERANK-NUM NOT = ZERO                                  
076900           MOVE CURRO-TIBERANK-NUM   TO WS-DATUM-NUM                      
077000           PERFORM BC-CONVERT-DATE                                        
077100           MOVE WS-DATUM-X           TO RESP-TIBERANK                     
077200                                     IN RESP-W90117O2-CTX                 
077300        ELSE                                                              
077400           MOVE SPACE                TO RESP-TIBERANK                     
077500                                     IN RESP-W90117O2-CTX                 
077600        END-IF                                                            
077700        MOVE CURRO-KDFEL             TO RESP-KDFEL                        
077800                                     IN RESP-W90117O2-CTX                 
077900        MOVE CURRO-IDLEVART          TO RESP-IDLEVART                     
078000                                     IN RESP-W90117O2-CTX                 
078100        MOVE CURRO-BEART-ENG         TO RESP-BEART                        
078200                                     IN RESP-W90117O2-CTX                 
078300        MOVE CURRO-KDSORT            TO RESP-KDSORT                       
078400                                     IN RESP-W90117O2-CTX                 
078500        MOVE CURRO-PRINK             TO RESP-PRINK                        
078600                                     IN RESP-W90117O2-CTX                 
078700        MOVE CURRO-IDLEVART-TILLK    TO RESP-IDLEVART-TILLK               
078800                                     IN RESP-W90117O2-CTX                 
078900        MOVE CURRO-KVLS-DLEV         TO RESP-KVLS-DLEV                    
079000                                     IN RESP-W90117O2-CTX                 
079100        MOVE CURRO-IDDC-LEV          TO RESP-IDDC-LEV                     
079200                                     IN RESP-W90117O2-CTX                 
079300     ELSE                                                                 
079400        MOVE CURRO-IDDISTR           TO RESP-IDDISTR                      
079500                                     IN RESP-W90117O3-CTX                 
079600        MOVE CURRO-IDKUNDNR          TO RESP-IDKUNDNR                     
079700                                     IN RESP-W90117O3-CTX                 
079800        MOVE CURRO-FLSVAR            TO RESP-FLSVAR                       
079900                                     IN RESP-W90117O3-CTX                 
080000        MOVE CURRO-KDORDBEK          TO RESP-KDORDBEK                     
080100                                     IN RESP-W90117O3-CTX                 
080200        MOVE CURRO-TEORDBEK-ENG      TO RESP-TEORDBEK                     
080300                                     IN RESP-W90117O3-CTX                 
080400        IF CURRO-TIBERANK-NUM NOT = ZERO                                  
080500           MOVE CURRO-TIBERANK-NUM   TO WS-DATUM-NUM                      
080600           PERFORM BC-CONVERT-DATE                                        
080700           MOVE WS-DATUM-X           TO RESP-TIBERANK                     
080800                                     IN RESP-W90117O3-CTX                 
080900        ELSE                                                              
081000           MOVE SPACE                TO RESP-TIBERANK                     
081100                                     IN RESP-W90117O3-CTX                 
081200        END-IF                                                            
081300        IF CURRO-TIDISPIN-NUM NOT = ZERO                                  
081400           MOVE CURRO-TIDISPIN-NUM   TO WS-DATUM-NUM                      
081500           PERFORM BC-CONVERT-DATE                                        
081600           MOVE WS-DATUM-X           TO RESP-TIDISPIN                     
081700                                     IN RESP-W90117O3-CTX                 
081800        ELSE                                                              
081900           MOVE SPACE                TO RESP-TIDISPIN                     
082000                                     IN RESP-W90117O3-CTX                 
082100        END-IF                                                            
082200        MOVE CURRO-IDLEVART          TO RESP-IDLEVART                     
082300                                     IN RESP-W90117O3-CTX                 
082400        MOVE CURRO-BEART-ENG         TO RESP-BEARTEXT                     
082500                                     IN RESP-W90117O3-CTX                 
082600        MOVE CURRO-KDSORT            TO RESP-KDSORT                       
082700                                     IN RESP-W90117O3-CTX                 
082800        MOVE CURRO-PRINK             TO RESP-PRINK                        
082900                                     IN RESP-W90117O3-CTX                 
083000        MOVE CURRO-IDLEVART-TILLK    TO RESP-IDLEVART-TILLK               
083100                                     IN RESP-W90117O3-CTX                 
083200        MOVE CURRO-KVLS-DLEV         TO RESP-KVLS-DLEV                    
083300                                     IN RESP-W90117O3-CTX                 
083400        MOVE CURRO-IDDC-LEV          TO RESP-IDDC-LEV                     
083500                                     IN RESP-W90117O3-CTX                 
083510        MOVE CURRO-FLLDCKND          TO RESP-FLLDCKND                     
083520                                     IN RESP-W90117O3-CTX                 
083600     END-IF                                                               
083700     IF REQU-IDRESVER = '002'                                             
083800       PERFORM S11-MSG-CONV                                               
083900     END-IF                                                               
084000     .                                                                    
084100                                                                          
084200                                                                          
084300                                                                          
084400*    --- DISPATCHER SECTIONS                                              
084500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
084600                                                                          
084700     MOVE 'GETARG'               TO SUB-KDFUNC                            
084800     MOVE 'CARPARTS.PULS.STOCKVALUEQUERY'   TO SUB-ADDISPABS              
084900     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
085000                                                                          
085100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
085200                                                                          
085300     IF SUB-KDRC > 0                                                      
085400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
085500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
085600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
085700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
085800     END-IF                                                               
085900     .                                                                    
086000     SKIP3                                                                
086100 S02-RETURN-RESPONSE SECTION.                                             
086200                                                                          
086300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
086400                                                                          
086500     IF REQU-IDOUTVER = 002                                               
086600       COMPUTE SUB-KVDLEN = LENGTH OF RESP-WZ01RESP +                     
086700                            LENGTH OF RESP-W90117O2-CTX                   
086800       CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA           
086900     END-IF                                                               
087000     IF REQU-IDOUTVER = 003                                               
087100       COMPUTE SUB-KVDLEN = LENGTH OF RESP-WZ01RES2 +                     
087200                            LENGTH OF RESP-W90117O3-CTX                   
087300       CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA-V3        
087400     END-IF                                                               
087500                                                                          
087600     IF SUB-KDRC > 0                                                      
087700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
087800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
087900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
088000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
088100     END-IF                                                               
088200     .                                                                    
088300                                                                          
088400 S11-MSG-CONV SECTION.                                                    
088500     MOVE LOW-VALUES              TO RESP-MESSAGES (1)                    
088600                                     RESP-MESSAGES (2)                    
088700     MOVE 1                       TO MSG-IX                               
088800*    REQUEST OK                                                           
088900     MOVE 200                     TO RESP-KDSTATUS-API                    
089000     IF RESP-IDMSG-INFO OF RESP-AREA > SPACE                              
089100       MOVE SPACES                TO MSG-CONV-AREA                        
089200       MOVE RESP-IDMSG-INFO OF RESP-AREA                                  
089300                                  TO MSG-CONV-IDMSG-IN                    
089400       CALL WMSGCONV           USING MSG-CONV-AREA                        
089500       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
089600       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
089700       ADD 1                      TO MSG-IX                               
089800     END-IF                                                               
089900     IF RESP-IDMSG-ERROR OF RESP-AREA > SPACE                             
090000        AND RESP-IDMSG-ERROR OF RESP-AREA NOT = '000'                     
090100*      BAD REQUEST                                                        
090200       MOVE 400                   TO RESP-KDSTATUS-API                    
090300       MOVE SPACES                TO MSG-CONV-AREA                        
090400                                                                          
090500       EVALUATE RESP-IDMSG-ERROR OF RESP-AREA                             
090600         WHEN 'B10'                                                       
090700*      NOT FOUND                                                          
090800           MOVE 404               TO RESP-KDSTATUS-API                    
090900           MOVE CUST-NOT-FOUND    TO MSG-CONV-IDMSG-IN                    
091000         WHEN 'B11'                                                       
091100*      NOT FOUND                                                          
091200           MOVE 404               TO RESP-KDSTATUS-API                    
091300           MOVE POST-CODE-NOT-FOUND                                       
091400                                  TO MSG-CONV-IDMSG-IN                    
091500         WHEN '58'                                                        
091600         WHEN '058'                                                       
091700*      NOT FOUND                                                          
091800           MOVE 404               TO RESP-KDSTATUS-API                    
091900           MOVE PART-NOT-FOUND    TO MSG-CONV-IDMSG-IN                    
092000         WHEN '400'                                                       
092100           MOVE SYS-ERROR         TO MSG-CONV-IDMSG-IN                    
092200           MOVE 'Key mapping'     TO MSG-CONV-IDELMT                      
092300         WHEN '404'                                                       
092400           EVALUATE RESP-IDELMT-ERROR OF RESP-AREA                        
092500             WHEN 'INVALID DISTRICT'                                      
092600               MOVE ERR-IS-INVALID                                        
092700                                  TO MSG-CONV-IDMSG-IN                    
092800               MOVE 'IDDISTR'     TO MSG-CONV-IDELMT                      
092900             WHEN 'INVALID CUSTOMER'                                      
093000               MOVE ERR-IS-INVALID                                        
093100                                  TO MSG-CONV-IDMSG-IN                    
093200               MOVE 'IDKUNDNR'    TO MSG-CONV-IDELMT                      
093300             WHEN 'QUANTITY MISSING'                                      
093400               MOVE ERR-IS-INVALID                                        
093500                                  TO MSG-CONV-IDMSG-IN                    
093600               MOVE 'KVANTAL'     TO MSG-CONV-IDELMT                      
093700           END-EVALUATE                                                   
093800         WHEN OTHER                                                       
093900           MOVE RESP-IDMSG-ERROR OF RESP-AREA                             
094000                                  TO MSG-CONV-IDMSG-IN                    
094100           MOVE RESP-IDELMT-ERROR OF RESP-AREA                            
094200                                  TO MSG-CONV-IDELMT                      
094300       END-EVALUATE                                                       
094400                                                                          
094500       CALL WMSGCONV           USING MSG-CONV-AREA                        
094600       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
094700       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
094800     END-IF                                                               
094900     .                                                                    
095000                                                                          
095100 IMS-GU-WDF501-BSEQ SECTION.                                              
095200     MOVE 'IMS-GU-WDF501-BS' TO CURRENT-IMS-SECTION                       
095300                                                                          
095400     MOVE SPACE                 TO ALL-SSA                                
095500     STRING 'WDF501  (WDF5BSEQ =' W-WDF5BSEQ ')'                          
095600            DELIMITED BY SIZE INTO SSA1                                   
095700     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
095800     CALL CBLTDLI USING GU WDF5B-PCB DLI-IO-WDF501 SSA1                   
095900     MOVE WDF5B-STATUS-CODE     TO STATUS-WS                              
096000     PERFORM IMS-STATUSCHECK                                              
096100     .                                                                    
096200                                                                          
096300                                                                          
096400 IMS-GU-WDF501 SECTION.                                                   
096500     MOVE 'GU-WDF501       '  TO CURRENT-IMS-SECTION                      
096600                                                                          
096700     MOVE SPACE               TO ALL-SSA                                  
096800     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
096900          DELIMITED BY SIZE INTO SSA1                                     
097000     MOVE '    '              TO GOOD-STATUSCODES                         
097100     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF501 SSA1                    
097200     MOVE WDF5-STATUS-CODE    TO STATUS-WS                                
097300     PERFORM IMS-STATUSCHECK                                              
097400     .                                                                    
097500                                                                          
097600 IMS-GNP-WDF502 SECTION.                                                  
097700     MOVE 'GNP-WDF502       ' TO CURRENT-IMS-SECTION                      
097800                                                                          
097900     MOVE SPACE               TO ALL-SSA                                  
098000     MOVE   'WDF502  '        TO SSA1                                     
098100     MOVE '  GE'              TO GOOD-STATUSCODES                         
098200     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-WDF502 SSA1                   
098300     MOVE WDF5-STATUS-CODE    TO STATUS-WS                                
098400     PERFORM IMS-STATUSCHECK                                              
098500     .                                                                    
098600                                                                          
098700                                                                          
098800 IMS-GU-WDGX4134   SECTION.                                               
098900     MOVE 'IMS-GU-WDGX4134 ' TO CURRENT-IMS-SECTION                       
099000                                                                          
099100     MOVE SPACE                 TO ALL-SSA                                
099200     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
099300          DELIMITED BY SIZE INTO SSA1                                     
099400     STRING 'WDGX4134(IDLANDX2 =' W-IDLANDX2                              
099500                    '&ADPOSTNF<=' W-ADPOSTNR                              
099600                    '&ADPOSTNT>=' W-ADPOSTNR                              
099700                    '!IDLANDX2 =' W-IDLANDX2                              
099800                    '&ADPOSTNF =' W-ADPOSTNR-DEF                          
099900                    '&ADPOSTNT =' W-ADPOSTNR-DEF ')'                      
100000            DELIMITED BY SIZE INTO SSA2                                   
100100     MOVE '  GE'                  TO GOOD-STATUSCODES                     
100200     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX4134 SSA1 SSA2             
100300     MOVE WDR5-STATUS-CODE        TO STATUS-WS                            
100400     PERFORM IMS-STATUSCHECK                                              
100500     .                                                                    
100600 IMS-STATUSCHECK SECTION.                                                 
100700                                                                          
100800     SET STATUS-IX                 TO 1                                   
100900     SEARCH GOOD-STATUS                                                   
101000       AT END                                                             
101100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
101200         DELIMITED BY SIZE INTO ERROR-TEXT                                
101300         CALL FELLOG                                                      
101400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
101500         CONTINUE                                                         
101600     END-SEARCH                                                           
101700     .                                                                    
