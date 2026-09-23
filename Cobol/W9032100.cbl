000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9032100.                                                
000300 AUTHOR.         GÖRAN KJELLSON                                           
000400 DATE-WRITTEN.   NOVEMBER 2020                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000720*    FUNCTION.                                                            
000800*        ORDER QUESTION/STATUS API                                        
000900*                                                                         
001000*                                                                         
001100*                                                                         
001200*    INPUT.                                                               
001300*        TRANSAKTION: W90321T                                             
001400*        MID:         W90321I1                                            
001500*                                                                         
001600*    OUTPUT.                                                              
001700*        MOD:         W90321O1                                            
001800* CHANGE HISTORY:                                                         
001900* 2404470/CHANGES OF 9321 : ADD NEW FIELDS                                
002000* ADD AN IF TO CHECK OBKR-IDDISTR AND OBKR-IDKUNDRF IS THE SAME           
002100* AS CURRENT OHUV-VALUES AFTER CALLING IMS-GU-WDQ101 AND                  
002200* IMS-GU-WDQ101-BO                                                        
002300* 3497490/ORDER STATUS API DOES NOT PICK SUPERSEEDING PART.               
002400* WHEN A SUPERSEEDED PART A (KDERS 0X) IS BO IN AN ORDER,LATER            
002500* THE SUERSESSION CHANGES FROM 0X TO 1X/2X.                               
002600* W440D1 ROUTINE PICKS THE REC AND SWITCHES THE PART TO B.                
002700* TO PICK THE SWITCHED PART,ADD CONDITION FOR SINGLE-SUPERSESS &          
002800* OBKR-IDARTNR-TILLK > 0 & OBKR-IDPGM = 'W4402800' IN F- SECTION.         
002900*                                                                         
003000 ENVIRONMENT DIVISION.                                                    
003100 DATA DIVISION.                                                           
003200                                                                          
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77    IDPGM                     PIC X(8)    VALUE 'W9032100'.            
003600 77    CURRENT-SECTION           PIC X(16)   VALUE SPACE.                 
003700 77    CURRENT-IMS-SECTION       PIC X(16)   VALUE SPACE.                 
003800 77    CURRENT-YYMMDD            PIC 9(6)    VALUE ZERO.                  
003900 77    CURRENT-IDKUNDRF          PIC X(7)    VALUE SPACE.                 
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77    ERROR-TEXT                PIC X(80) VALUE SPACE.                   
004200 77    FELTEXT                   PIC X(80) VALUE SPACE.                   
004300 77    KDRC-DISPLAY              PIC Z(5).                                
004400 77    RKOD-ABEND-NO-DUMP        PIC S9(4)   COMP VALUE +16.              
004500 77    RKOD-ABEND-WITH-DUMP      PIC S9(4)   COMP VALUE +1000.            
004600                                                                          
004700 77    JAA                       PIC X       VALUE 'J'.                   
004800 77    YES                       PIC X       VALUE 'Y'.                   
004900 77    NOO                       PIC X       VALUE 'N'.                   
005000 77    WS-TILOKDAT               PIC 9(6)    VALUE ZERO.                  
005100 77    WS-IDLEVART               PIC X(30)   VALUE SPACE.                 
005200 77    WS-IDARTNR                PIC Z(9)    VALUE zero.                  
005300 77    WS-IDKUNDRF-RO            PIC 9(7)    VALUE ZERO.                  
005310 77    W-KDKUNDKAT               PIC 9(2)    VALUE ZERO.                  
005320 77    W-IDARTNR-TILLK           PIC S9(9)   VALUE ZERO COMP-3.           
005400                                                                          
005500 01    WS-IDSYSTEM               PIC  X(4)   VALUE SPACE.                 
005600 01    WS-SAVE-IDLEVART          PIC  X(30)  VALUE SPACE.                 
005700                                                                          
005800                                                                          
005900 01    WS-CHAR-DATE.                                                      
006000   03  FILLER                    PIC X(2)    VALUE '20'.                  
006100   03  WS-YY-CHAR                PIC 9(2)    VALUE ZERO.                  
006200   03  FILLER                    PIC X(1)    VALUE '-'.                   
006300   03  WS-MM-CHAR                PIC 9(2)    VALUE ZERO.                  
006400   03  FILLER                    PIC X(1)    VALUE '-'.                   
006500   03  WS-DD-CHAR                PIC 9(2)    VALUE ZERO.                  
006600                                                                          
006700 01    WS-DATE-NUM6              PIC 9(6)    VALUE ZERO.                  
006800 01    WS-DATE-NUM6-EDIT REDEFINES WS-DATE-NUM6.                          
006900   03  WS-DATE-NUM6-YY           PIC 9(2).                                
007000   03  WS-DATE-NUM6-MM           PIC 9(2).                                
007100   03  WS-DATE-NUM6-DD           PIC 9(2).                                
007200                                                                          
007300 01    WS-DATE-NUM10             PIC 9(6)    VALUE ZERO.                  
007400 01    WS-DATE-NUM10-EDIT REDEFINES WS-DATE-NUM10.                        
007500   03  WS-DATE-NUM10-YY          PIC 9(2).                                
007600   03  WS-DATE-NUM10-MM          PIC 9(2).                                
007700   03  WS-DATE-NUM10-DD          PIC 9(2).                                
007800   03  FILLER                    PIC 9(4).                                
007900                                                                          
008000 01    WS-KDORDBEK-DDGS          PIC 9(2)    VALUE ZERO.                  
008100 01    WS-KDORDBEK               PIC 9(2)    VALUE ZERO.                  
008200   88  BLOCKED                               VALUE 21 22 40 52 54         
008300                                                   55 57 58 66 67.        
008400   88  Q-ADAPTED                             VALUE 43 44.                 
008500   88  SINGLE-SUPERSESS                      VALUE 41.                    
008600   88  MULTIPLE-SUPERSESS                    VALUE 61 65.                 
008700   88  CANCELLED                             VALUE 80 83 85 87 84.        
008800   88  TPO-ORDER                             VALUE 70.                    
008900   88  BACK-ORDER                            VALUE 90 91.                 
009000   88  VOR-ORDER                             VALUE 92 93.                 
009100   88  BACK-ORDER-90                         VALUE 90.                    
009200   88  VOR-ORDER-92                          VALUE 92.                    
009300   88  VOR-ORDER-93                          VALUE 93.                    
009400                                                                          
009500 01    WS-KDMEDD.                                                         
009600   03  WS-REGISTRED              PIC 9(3)    VALUE 001.                   
009700   03  WS-REPLACED               PIC 9(3)    VALUE 002.                   
009800   03  WS-QUANTIFIED             PIC 9(3)    VALUE 003.                   
009900   03  WS-BLOCKED                PIC 9(3)    VALUE 004.                   
010000   03  WS-IN-PROGRESS            PIC 9(3)    VALUE 005.                   
010100   03  WS-BACKORDERED            PIC 9(3)    VALUE 006.                   
010200   03  WS-READY-FOR-PICK-UP      PIC 9(3)    VALUE 007.                   
010300   03  WS-COMPLETED-IN-WH        PIC 9(3)    VALUE 008.                   
010400   03  WS-CANCELLED              PIC 9(3)    VALUE 009.                   
010500                                                                          
010600 77    OK-SW                     PIC X       VALUE 'Y'.                   
010700   88  EVERYTHING-OK                         VALUE 'Y'.                   
010800   88  SOMETHING-WRONG                       VALUE 'N'.                   
010900                                                                          
011000 77    KDVORATG-SW               PIC X       VALUE 'N'.                   
011100   88  KDVORATG-FOUND                        VALUE 'Y'.                   
011200   88  KDVORATG-MISSING                      VALUE 'N'.                   
011300                                                                          
011400 77    DDGS-SW                   PIC X       VALUE 'Y'.                   
011500   88  DDGS-OK                               VALUE 'Y'.                   
011600   88  DDGS-NOT-OK                           VALUE 'N'.                   
011700                                                                          
011800 77    DDGS-LINE-SW              PIC X       VALUE 'N'.                   
011900 77    BO-RELEASE-SW             PIC X       VALUE 'N'.                   
012000   88  BO-RELEASED                           VALUE 'Y'.                   
012100   88  BO-NOT-RELEASED                       VALUE 'N'.                   
012200                                                                          
012300 77    VOR-RELEASE-SW            PIC X       VALUE 'N'.                   
012400   88  VOR-RELEASED                          VALUE 'Y'.                   
012500   88  VOR-NOT-RELEASED                      VALUE 'N'.                   
012600                                                                          
012700 77    QUANT-ADAPT-SW            PIC X       VALUE 'N'.                   
012800   88  QUANT-ADAPTED                         VALUE 'Y'.                   
012900   88  NOT-QUANT-ADAPTED                     VALUE 'N'.                   
013000                                                                          
013100 77    SHIPPED-INVOICED-SW       PIC X       VALUE 'N'.                   
013200   88  SHIPPED-INVOICED                      VALUE 'Y'.                   
013300   88  NOT-SHIPPED-INVOICED                  VALUE 'N'.                   
013310                                                                          
013320 77    LYNK-NON-API-SW           PIC X       VALUE 'N'.                   
013330   88  LYNK-NON-API                          VALUE 'J'.                   
013340                                                                          
013400                                                                          
013500 01  WS-Q-ADAPT-TAB.                                                      
013600     03  WS-Q-ADADAPT OCCURS 100.                                         
013700       05  WS-IDARTNR-ADAPT      PIC S9(9)   COMP-3.                      
013800       05  WS-KVBEART-ADAPT      PIC S9(7)   COMP-3.                      
013900       05  WS-KVBEART-Q-ADAPT    PIC S9(7)   COMP-3.                      
014000                                                                          
014100 01  WS-IDDC-CHECK               PIC X(2)    VALUE SPACE.                 
014200 01  WS-RFS-TAB.                                                          
014300     03  WS-RFS OCCURS 25.                                                
014400       05  WS-IDDC-RFS           PIC X(2).                                
014500       05  WS-TIRFS-RFS          PIC 9(10).                               
014600       05  WS-BERADREF           PIC X(10).                               
014700                                                                          
014800 77    PART-IX                   PIC 9(3)    VALUE ZERO.                  
014900 77    ADAPT-IX                  PIC 9(3)    VALUE ZERO.                  
015000 77    ADAPT-IX-MAX              PIC 9(3)    VALUE 100.                   
015100 77    RFS-IX                    PIC 9(3)    VALUE ZERO.                  
015200 77    RFS-IX-MAX                PIC 9(3)    VALUE 25.                    
015300 77    LINE-IX                   PIC 9(3)    VALUE ZERO.                  
015400 77    LINE-IX-MAX               PIC 9(3)    VALUE 5.                     
015500                                                                          
015600                                                                          
015700 01  SAVE-WDQ1-VALUES.                                                    
015800     03 SAVE-OBKR-IDORDER        PIC S9(7)  COMP-3 VALUE ZERO.            
015900     03 SAVE-OBKR-IDARTNR        PIC S9(9)  COMP-3 VALUE ZERO.            
016000     03 SAVE-OBKR-IDLOPNR        PIC S9(3)  COMP-3 VALUE ZERO.            
016100     03 SAVE-OBKR-IDSEKVNR       PIC S9(3)  COMP-3 VALUE ZERO.            
016200     03 SAVE-OBKR-IDDC           PIC X(2)   VALUE SPACES.                 
016300     03 SAVE-OBKR-KDORDBEK       PIC 9(2)   VALUE ZERO.                   
016400                                                                          
016500 01  DYNAMISKA-SUBPROGRAM.                                                
016600   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
016700   03  ABEND                     PIC X(8)   VALUE 'ABEND   '.             
016800   03  WZ01SUB                   PIC X(8)   VALUE 'WZ01SUB '.             
016900   03  WZ01AUTH                  PIC X(8)    VALUE 'WZ01AUTH'.            
017000   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
017200   03  WZ01SEND                  PIC X(8)   VALUE 'WZ01SEND'.             
017300   03  W006KOM                   PIC X(8)   VALUE 'W006KOM '.             
017400*                                                                         
017500 01  FILLER                      PIC X(16)  VALUE 'SUB-CONTROL'.          
017600                                                                          
017700*01  -COPY WZ01SUB                                                        
017800*                                                                         
017900 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH   '.         
018000     SKIP3                                                                
018100*01  -COPY WZ01AUTH                                                       
018200*                                                                         
018700*    --- AREAS FOR SUB MODULES W006KOM                                    
018800                                                                          
018900 01  FILLER                    PIC X(16) VALUE 'MSG-KOM-WMSGKOM '.        
019000*01  -COPY WMSGKOM                                                        
019100                                                                          
019200 01  FILLER                    PIC X(16) VALUE 'MSG-IO-AREA     '.        
019300*01  -COPY WMSGAREA                                                       
019400                                                                          
019500                                                                          
019600                                                                          
019700******************************************************************        
019800*                                                                         
019900*    AREOR FÖR REQUEST AND RESPONS                                        
020000*                                                                         
020100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
020200                                                                          
020300 01  REQU-AREA.                                                           
020400*    03  -COPY WZ01REQ2                                                   
020500*    03  -COPY W90321I1                                                   
020600                                                                          
020700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
020800                                                                          
020900 01  RESP-AREA.                                                           
021000*    03  -COPY WZ01RESP                                                   
021100*    03  -COPY W90321O1                                                   
021200                                                                          
021300                                                                          
021400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021500 01  DLI-KEYS.                                                            
021600                                                                          
021700     03  W-WDQ2CSEQ.                                                      
021800         05  W-IDDISTR-CSEQ      PIC S9(5)   VALUE ZERO COMP-3.           
021900         05  W-IDKUNDNR-CSEQ     PIC S9(7)   VALUE ZERO COMP-3.           
022000         05  FILLER              PIC 9(2)    VALUE ZERO.                  
022100         05  W-IDORDNR5-CSEQ     PIC 9(5).                                
022200         05  FILLER              PIC X(3)    VALUE SPACE.                 
022300                                                                          
022400     03  W-WDQ2CSEQ-BO.                                                   
022500         05  W-IDDISTR-CSEQ-BO   PIC S9(5)   VALUE ZERO COMP-3.           
022600         05  W-IDKUNDNR-CSEQ-BO  PIC S9(7)   VALUE ZERO COMP-3.           
022700         05  FILLER              PIC 9(2)    VALUE ZERO.                  
022800         05  W-IDORDNR5-CSEQ-BO  PIC 9(5).                                
022900         05  FILLER              PIC X(3)    VALUE SPACE.                 
023000                                                                          
023100     03  W-IDGMT-X.                                                       
023200         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
023300         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
023400                                                                          
023500     03  W-WDQ401KY-MIN-X.                                                
023600         05  W-IDORDER-Q4-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
023700         05  FILLER              PIC  X(16)   VALUE LOW-VALUE.            
023800                                                                          
023900     03  W-WDQ401KY-MAX-X.                                                
024000         05  W-IDORDER-Q4-MAX    PIC S9(7)   VALUE ZERO COMP-3.           
024100         05  FILLER              PIC  X(16)   VALUE HIGH-VALUE.           
024200                                                                          
024300     03  W-IDARTNR-X.                                                     
024400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
024500                                                                          
024600     03  W-IDLEVNR-X.                                                     
024700         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
024800                                                                          
024900     03  W-WDQ101KY-MIN-X.                                                
025000         05  W-IDORDER-MIN       PIC S9(7)    VALUE ZERO COMP-3.          
025100         05  FILLER              PIC  X(13)   VALUE LOW-VALUE.            
025200                                                                          
025300     03  W-WDQ101KY-MAX-X.                                                
025400         05  W-IDORDER-MAX       PIC S9(7)    VALUE ZERO COMP-3.          
025500         05  FILLER              PIC  X(13)   VALUE HIGH-VALUE.           
025600                                                                          
025700     03  W-WDQ101KY-BO-MIN-X.                                             
025800         05  W-IDORDER-BO-MIN    PIC S9(7)    VALUE ZERO COMP-3.          
025900         05  FILLER              PIC  X(13)   VALUE LOW-VALUE.            
026000                                                                          
026100     03  W-WDQ101KY-BO-MAX-X.                                             
026200         05  W-IDORDER-BO-MAX    PIC S9(7)    VALUE ZERO COMP-3.          
026300         05  FILLER              PIC  X(13)   VALUE HIGH-VALUE.           
026400                                                                          
026500     03  W-WDE4ASEQ-X.                                                    
026600         05  W-SEQA-IDDISTR          PIC S9(5)  VALUE ZERO COMP-3.        
026700         05  W-SEQA-IDKUNDNR         PIC S9(7)  VALUE ZERO COMP-3.        
026800         05  W-SEQA-IDKUNDRF         PIC X(10)  VALUE SPACE.              
026900                                                                          
027000   03  W-IDPURAD-X.                                                       
027100       07 W-IDPURAD               PIC S9(5)   VALUE ZERO  COMP-3.         
027200                                                                          
027300     03  W-IDSKYLT-X.                                                     
027400         05  W-IDSKYLT           PIC X(3)      VALUE 'GB'.                
027500                                                                          
027600     03  W-IDPRODNR-X.                                                    
027700         05  W-IDPRODNR          PIC S9(7)     VALUE ZERO COMP-3.         
027800                                                                          
027900     03  W-IDKOLLI-X.                                                     
028000         05  W-IDKOLLI           PIC S9(5)     VALUE ZERO COMP-3.         
028100                                                                          
028200     03  W-WDA501KY-A5-MIN-X.                                             
028300         05  W-IDDISTR-A5-MIN      PIC S9(5) VALUE ZERO COMP-3.           
028400         05  W-IDKUNDNR-A5-MIN     PIC S9(7) VALUE ZERO COMP-3.           
028500         05  W-IDKUNDRF-A5-MIN     PIC X(10) VALUE SPACE.                 
028600         05  W-IDARTNR-A5-MIN      PIC S9(9) VALUE ZERO COMP-3.           
028700         05  FILLER                PIC X(2)  VALUE LOW-VALUE.             
028800                                                                          
028900     03  W-WDA501KY-A5-MAX-X.                                             
029000         05  W-IDDISTR-A5-MAX      PIC S9(5) VALUE ZERO COMP-3.           
029100         05  W-IDKUNDNR-A5-MAX     PIC S9(7) VALUE ZERO COMP-3.           
029200         05  W-IDKUNDRF-A5-MAX     PIC X(10) VALUE SPACE.                 
029300         05  W-IDARTNR-A5-MAX      PIC S9(9) VALUE ZERO COMP-3.           
029400         05  FILLER                PIC X(2)  VALUE HIGH-VALUE.            
029500                                                                          
029600     03  W-KDORDKL-X.                                                     
029700         05  W-KDORDKL             PIC S9    VALUE ZERO COMP-3.           
029800                                                                          
029900     03  W-KDSTARAD-X.                                                    
030000         05  W-KDSTARAD            PIC X(1)  VALUE '4'.                   
030100                                                                          
030200     03  W-WDA6A1KY-MIN-X.                                                
030300         05  W-A6-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.           
030400         05  W-A6-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.           
030500         05  W-A6-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.                 
030600         05  FILLER                PIC X(23) VALUE LOW-VALUE.             
030700                                                                          
030800     03  W-WDA6A1KY-MAX-X.                                                
030900         05  W-A6-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.           
031000         05  W-A6-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.           
031100         05  W-A6-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.                 
031200         05  FILLER                PIC X(23) VALUE HIGH-VALUE.            
031300                                                                          
031400                                                                          
031500     03  W-WDA601KY-X.                                                    
031600       05    W-A601KY-IDDISTR       PIC S9(5) VALUE ZERO COMP-3.          
031700       05    W-A601KY-IDKUNDNR      PIC S9(7) VALUE ZERO COMP-3.          
031800       05    W-A601KY-IDKUNDRF      PIC X(10) VALUE SPACE.                
031900       05    W-A601KY-TIREGDAT-URSP PIC S9(7) VALUE ZERO COMP-3.          
032000       05    W-A601KY-IDARTNR       PIC S9(9) VALUE ZERO COMP-3.          
032100       05    W-A601KY-TIREGTID-URSP PIC S9(9) VALUE ZERO COMP-3.          
032200       05    W-A601KY-TIREGDAT-AVV  PIC S9(7) VALUE ZERO COMP-3.          
032300       05    W-A601KY-TIREGTID-AVV  PIC S9(9) VALUE ZERO COMP-3.          
032400                                                                          
032500                                                                          
032600 01  MESSAGE-CODES.                                                       
032700     03  SYS-ERROR               PIC X(3)    VALUE '099'.                 
032800     03  OK-REQUEST              PIC X(3)    VALUE '200'.                 
032900     03  ORDER-CREATED           PIC X(3)    VALUE '201'.                 
033000     03  BAD-REQUEST             PIC X(3)    VALUE '400'.                 
033100     03  NOT-FOUND               PIC X(3)    VALUE '404'.                 
033200                                                                          
033300                                                                          
033400 01  STATUS-WS                   PIC XX.                                  
033500     88  SEGMENT-FOUND                       VALUE '  '.                  
033600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
033700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
033800     88  END-OF-DATA                         VALUE 'GB'.                  
033900                                                                          
034000 01  GOOD-STATUSCODES.                                                    
034100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034200                                                                          
034300 01  ALL-SSA.                                                             
034400     03 SSA1                     PIC X(240).                              
034500     03 SSA2                     PIC X(80).                               
034600                                                                          
034700*    --- IMS FUNCTION CODES                                               
034800*01  -COPY W0003                                                          
034900                                                                          
035000                                                                          
035100 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDQ201'.          
035200 01  DLI-IO-WDQ201.                                                       
035300*    03  -COPY WDQ201                                                     
035400                                                                          
035500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDQ201-BO'.          
035600 01  DLI-IO-WDQ201-BO.                                                    
035700*    03  -COPY WDQ201  -PRE BO-                                           
035800                                                                          
035900 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDQ212'.          
036000 01  DLI-IO-WDQ212.                                                       
036100*    03  -COPY WDQ212                                                     
036200                                                                          
036300 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDQ211'.          
036400 01  DLI-IO-WDQ211.                                                       
036500*    03  -COPY WDQ211                                                     
036600                                                                          
036700 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDB201'.          
036800 01  DLI-IO-WDB201.                                                       
036900*    03  -COPY WDB201                                                     
037000                                                                          
037100 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDQ401'.          
037200 01  DLI-IO-WDQ401.                                                       
037300*    03  -COPY WDQ401 -PRE Q4-                                            
037400                                                                          
037500 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDQ401-P'.        
037600 01  DLI-IO-WDQ401-P.                                                     
037700*    03  -COPY WDQ401 -PRE P-                                             
037800                                                                          
037900 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK601'.          
038000 01  DLI-IO-WDK601.                                                       
038100*    03  -COPY WDK601                                                     
038200                                                                          
038300 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK611'.          
038400 01  DLI-IO-WDK611.                                                       
038500*    03  -COPY WDK611                                                     
038600                                                                          
038700 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDF502'.          
038800 01  DLI-IO-WDF502.                                                       
038900*    03  -COPY WDF502                                                     
039000                                                                          
039100 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDQ101'.          
039200 01  DLI-IO-WDQ101.                                                       
039300*    03  -COPY WDQ101                                                     
039400                                                                          
039500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDQ101-BO'.        
039600 01  DLI-IO-WDQ101-BO.                                                    
039700*    03  -COPY WDQ101  -PRE BO-                                           
039800                                                                          
039900 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDE401'.          
040000 01  DLI-IO-WDE401.                                                       
040100*    03  -COPY WDE401                                                     
040200                                                                          
040300 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDE411'.          
040400 01  DLI-IO-WDE411.                                                       
040500*    03  -COPY WDE411 -PRE E4-                                            
040600                                                                          
040700 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDE421'.          
040800 01  DLI-IO-WDE421.                                                       
040900*    03  -COPY WDE421                                                     
041000                                                                          
041100 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-BENA11'.          
041200 01  DLI-IO-BENA11.                                                       
041300*    03  WLBENA11    -COPY WDD311                                         
041400                                                                          
041500 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDE611'.          
041600 01  DLI-IO-WDE611.                                                       
041700*    03  -COPY WDE611                                                     
041800                                                                          
041900 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDF601'.          
042000 01  DLI-IO-WDF601.                                                       
042100*    03  -COPY WDF601                                                     
042200                                                                          
042300 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDA501'.          
042400 01  DLI-IO-WDA501.                                                       
042500*    03  -COPY WDA501                                                     
042600                                                                          
042700 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDA6A1'.          
042800 01  DLI-IO-WDA6A1.                                                       
042900*    03  -COPY WDA6A1                                                     
043000                                                                          
043100 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDA601'.          
043200 01  DLI-IO-WDA601.                                                       
043300*    03  -COPY WDA601                                                     
043400                                                                          
043500                                                                          
043600                                                                          
043700 LINKAGE SECTION.                                                         
043800                                                                          
043900*01  -COPY W0009   -PRE MSG-                                              
044000                                                                          
044100 01  ATAB-PCB                    PIC X.                                   
044200                                                                          
044300*01  -COPY W0008     -PRE WDQ2-                                           
044400     05  FILLER                  PIC X.                                   
044500                                                                          
044600*01  -COPY W0008     -PRE WDB2-                                           
044700     05  FILLER                  PIC X.                                   
044800                                                                          
044900*01  -COPY W0008     -PRE WDQ4-                                           
045000     05  FILLER                  PIC X.                                   
045100                                                                          
045200*01  -COPY W0008     -PRE WDK6-                                           
045300     05  FILLER                  PIC X.                                   
045400                                                                          
045500*01  -COPY W0008     -PRE WDF5-                                           
045600     05  FILLER                  PIC X.                                   
045700                                                                          
045800*01  -COPY W0008     -PRE WDQ1-                                           
045900     05  FILLER                  PIC X.                                   
046000                                                                          
046100*01  -COPY W0008     -PRE WDQ1-BO-                                        
046200     05  FILLER                  PIC X.                                   
046300                                                                          
046400*01  -COPY W0008     -PRE WDE4-                                           
046500     05  FILLER                  PIC X.                                   
046600                                                                          
046700*01  -COPY W0008     -PRE BENA-                                           
046800     05  FILLER                  PIC X.                                   
046900                                                                          
047000*01  -COPY W0008     -PRE WDE6-                                           
047100     05  FILLER                  PIC X.                                   
047200                                                                          
047300*01  -COPY W0008     -PRE WDF6-                                           
047400     05  FILLER                  PIC X.                                   
047500                                                                          
047600*01  -COPY W0008     -PRE WDA5-                                           
047700     05  FILLER                  PIC X.                                   
047800                                                                          
047900*01  -COPY W0008     -PRE WDA6A-                                          
048000     05  FILLER                  PIC X.                                   
048100                                                                          
048200*01  -COPY W0008     -PRE WDA6-                                           
048300     05  FILLER                  PIC X.                                   
048400                                                                          
048500                                                                          
048600 PROCEDURE DIVISION  USING MSG-PCB  ATAB-PCB                              
048700                           WDQ2-PCB WDB2-PCB WDQ4-PCB                     
048800                           WDK6-PCB WDF5-PCB WDQ1-PCB WDQ1-BO-PCB         
048900                           WDE4-PCB BENA-PCB WDE6-PCB                     
049000                           WDF6-PCB WDA5-PCB WDA6A-PCB                    
049100                           WDA6-PCB.                                      
049200 MAIN SECTION.                                                            
049300     ENTRY 'DLITCBL' USING MSG-PCB  ATAB-PCB                              
049400                           WDQ2-PCB WDB2-PCB WDQ4-PCB                     
049500                           WDK6-PCB WDF5-PCB WDQ1-PCB                     
049600                           WDE4-PCB BENA-PCB WDE6-PCB                     
049700                           WDF6-PCB WDA5-PCB WDA6A-PCB                    
049800                           WDA6-PCB.                                      
049900                                                                          
050000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
050100     IF SUB-KDRC = 0                                                      
050200        MOVE 001                 TO AUTH-KDCALL                           
050300        CALL WZ01AUTH         USING AUTH-WZ01AUTH                         
050400                                    REQU-WZ01REQ2                         
050500        IF AUTH-KDRC = 0                                                  
050600           IF REQU-KDPGMACT = 'S'                                         
050700                                                                          
050800              PERFORM A-INIT-SAVE-INPUT                                   
050900              PERFORM B-CHECK-INPUT                                       
051000                                                                          
051010              IF EVERYTHING-OK                                            
051020                 PERFORM D-GET-CUSTOMER-INFO                              
051030              END-IF                                                      
051100              IF EVERYTHING-OK                                            
051200                 PERFORM C-GET-ORDER-HEAD                                 
051300              END-IF                                                      
051700              IF EVERYTHING-OK                                            
051800                 PERFORM E-GET-ORDER-LINES                                
051900                 PERFORM F-GET-ORDER-CONFIRMATIONS                        
052000              END-IF                                                      
052100*   fix if you want to adapt to test format                               
052200*    MOVE 4 TO RESP-KVRADER                                               
052300*   end-fix                                                               
052400           ELSE                                                           
052500              MOVE SYS-ERROR    TO RESP-IDMSG-ERROR                       
052600           END-IF                                                         
052700        ELSE                                                              
052800           IF AUTH-KDRC = 4                                               
052900              MOVE BAD-REQUEST TO RESP-IDMSG-ERROR                        
053000           ELSE                                                           
053100              MOVE AUTH-KDRC TO KDRC-DISPLAY                              
053200              STRING 'WZ01AUTH GETARG ERROR RC=' KDRC-DISPLAY             
053300              DELIMITED BY SIZE INTO ERROR-TEXT                           
053400              CALL ABEND USING RKOD-ABEND-WITH-DUMP                       
053500           END-IF                                                         
053600        END-IF                                                            
053700                                                                          
053800        PERFORM S02-RETURN-RESPONSE                                       
053900     END-IF                                                               
054000                                                                          
054100     MOVE ZERO                         TO RETURN-CODE                     
054200                                                                          
054300     GOBACK                                                               
054400     .                                                                    
054500                                                                          
054600                                                                          
054700 A-INIT-SAVE-INPUT SECTION.                                               
054800     MOVE 'A-INIT-SAVE-INPUT' TO CURRENT-SECTION                          
054900     MOVE AUTH-IDSYSTEM          TO WS-IDSYSTEM                           
055000                                                                          
055100     MOVE 001                    TO RESP-IDMSGVER                         
055200     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
055300                                    RESP-IDMSG-INFO                       
055400                                    RESP-IDELMT-ERROR                     
055500     MOVE SPACE                  TO RESP-W90321O1-CTX                     
055600     MOVE REQU-IDDISTR           TO RESP-IDDISTR-OREF                     
055700     MOVE REQU-IDKUNDNR          TO RESP-IDKUNDNR-OREF                    
055800     MOVE REQU-IDORDNR7          TO RESP-IDORDNR7-OREF                    
055900     MOVE REQU-TIREGDAT          TO RESP-TIREGDAT-OREF                    
056000                                                                          
056100     MOVE SPACE                  TO RESP-IDMFSINF                         
056200                                    RESP-TEMFSINF                         
056300                                                                          
056400*    -- INITIALIZE W006KOM AREAS WITH FIXED VALUES                        
056500*    -- FIELDS WITH VARYING CONTENT ARE SET LATER                         
056600     MOVE SPACE                      TO MSG-KOM-WMSGKOM                   
056700     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
056800     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
056900     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
057000     MOVE 'W9032100'                 TO MSG-KOM-IDSNDJOB                  
057100     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
057200*    -- TIKLOCK WILL BE INCREMENTENTED FOR EACH ORDER                     
057300*    -- THIS IS THE START VALUE                                           
057400     MOVE FUNCTION CURRENT-DATE(3:6) TO CURRENT-YYMMDD                    
057500     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
057600                                                                          
057700*    -- INITIALIZE TARGET TRANSACTION AREA WITH FIXED VALUES              
057800     MOVE LOW-VALUE                  TO MSG-KDZ1                          
057900     MOVE LOW-VALUE                  TO MSG-KDZ2                          
058000*   fix to adapt to test format                                           
058100*    MOVE 4 TO RESP-KVRADER                                               
058200                                                                          
058300     MOVE 1 TO ADAPT-IX                                                   
058400     PERFORM UNTIL ADAPT-IX > ADAPT-IX-MAX                                
058500        MOVE ZERO TO WS-IDARTNR-ADAPT   (ADAPT-IX)                        
058600                     WS-KVBEART-ADAPT   (ADAPT-IX)                        
058700                     WS-KVBEART-Q-ADAPT (ADAPT-IX)                        
058800        ADD 1 TO ADAPT-IX                                                 
058900     END-PERFORM                                                          
059000                                                                          
059100     MOVE 1 TO RFS-IX                                                     
059200     PERFORM UNTIL RFS-IX > RFS-IX-MAX                                    
059300        MOVE SPACE TO WS-IDDC-RFS  (RFS-IX)                               
059400        MOVE ZERO  TO WS-TIRFS-RFS (RFS-IX)                               
059500        MOVE SPACE TO WS-BERADREF  (RFS-IX)                               
059600        ADD 1 TO RFS-IX                                                   
059700     END-PERFORM                                                          
059710                                                                          
059720     MOVE NOO               TO LYNK-NON-API-SW                            
059800     .                                                                    
059900                                                                          
060000                                                                          
060100 B-CHECK-INPUT SECTION.                                                   
060200     MOVE 'B-CHECK-INPUT  ' TO CURRENT-SECTION                            
060300                                                                          
060400     IF REQU-IDDISTR  = ZERO  OR                                          
060500        REQU-IDORDNR7 = ZERO OR                                           
060600        REQU-TIREGDAT = ZERO                                              
060700        MOVE NOO             TO OK-SW                                     
060800        MOVE NOT-FOUND       TO RESP-IDMFSINF                             
060900        MOVE 'ORDER ID IS INVALID'                                        
061000                             TO RESP-TEMFSINF                             
061100     END-IF                                                               
061200                                                                          
061300     IF EVERYTHING-OK                                                     
061400        PERFORM BA-CHECK-CALLER-ID                                        
061500* TO BE CODED ???                                                         
061600     END-IF                                                               
061700     .                                                                    
061800                                                                          
061900                                                                          
062000 BA-CHECK-CALLER-ID SECTION.                                              
062100     MOVE 'BA-CHECK-CALLER' TO CURRENT-SECTION                            
062200                                                                          
062300     .                                                                    
062400                                                                          
062500                                                                          
062600 C-GET-ORDER-HEAD SECTION.                                                
062700     MOVE 'C-GET-ORDERHEAD' TO CURRENT-SECTION                            
062800                                                                          
062900     MOVE REQU-IDDISTR   TO W-IDDISTR-CSEQ                                
063000     MOVE REQU-IDKUNDNR  TO W-IDKUNDNR-CSEQ                               
063100     MOVE REQU-IDORDNR7  TO W-IDORDNR5-CSEQ                               
063200                            CURRENT-IDKUNDRF                              
063300                                                                          
063400     PERFORM IMS-GU-WDQ201-CSEQ                                           
063410                                                                          
063420     IF SEGMENT-FOUND                                                     
063421        IF OHUV-IDSYSTEM(1:3) NOT = 'LYN'                                 
063422           AND W-KDKUNDKAT = 03                                           
063423             MOVE JAA    TO LYNK-NON-API-SW                               
063430        END-IF                                                            
063440     END-IF                                                               
063500     IF SEGMENT-FOUND AND                                                 
063600      ((OHUV-TIREGDAT = REQU-TIREGDAT AND                                 
064000        OHUV-IDSYSTEM = WS-IDSYSTEM) OR LYNK-NON-API)                     
064010        AND OHUV-IDSYSTEM NOT = 'LDCB'                                    
064020        AND OHUV-IDSYSTEM(1:3) NOT = 'OVR'                                
064100           MOVE OHUV-KDORDKL     TO RESP-KDORDKL                          
064200           MOVE OHUV-IDDISTR     TO RESP-IDDISTR                          
064300           MOVE OHUV-IDKUNDNR    TO RESP-IDKUNDNR                         
064400           IF OHUV-TIREPDAT NOT = ZERO                                    
064500              MOVE OHUV-TIREPDAT TO WS-DATE-NUM6                          
064600              MOVE ZERO          TO WS-DATE-NUM10                         
064700              PERFORM S60-CONVERT-DATE                                    
064800              MOVE WS-CHAR-DATE  TO RESP-TIREPDAT                         
064900           ELSE                                                           
065000              MOVE SPACE         TO RESP-TIREPDAT                         
065100           END-IF                                                         
065200           MOVE OHUV-BEGMT-RAD1  TO RESP-BEGMT-RAD1                       
065300           MOVE OHUV-BEGMT-RAD2  TO RESP-BEGMT-RAD2                       
065400           MOVE OHUV-ADGMT-GATA  TO RESP-ADGMT-GATA                       
065500           MOVE OHUV-ADPOSTNR IN OHUV-ADPOST-PNRORT                       
065600                                 TO RESP-ADPOSTNR                         
065700           MOVE OHUV-ADCITY IN OHUV-ADPOST-PNRORT                         
065800                                 TO RESP-ADCITY                           
065900           MOVE OHUV-ADGMT-LAND  TO RESP-ADGMT-LAND                       
066000           MOVE OHUV-BEBETRAD-1  TO RESP-IDNAMN                           
066100           MOVE OHUV-IDMAIL      TO RESP-IDMAIL                           
066200           MOVE OHUV-BETELNR     TO RESP-BETELNR                          
066300           MOVE OHUV-BELAGINS-DEL1                                        
066400                                 TO RESP-BELAGINS-DEL                     
066500           MOVE OHUV-BEKUNDRF    TO RESP-BEKUNDRF-001                     
066600           MOVE YES              TO RESP-FLCANCEL                         
066700                                                                          
066800           PERFORM CA-SEARCH-FOR-TIRFS                                    
066900     ELSE                                                                 
067000        MOVE NOO             TO OK-SW                                     
067100        MOVE NOT-FOUND       TO RESP-IDMFSINF                             
067200        STRING 'ORDER ' REQU-IDAPIORDREF ' NOT FOUND'                     
067300        DELIMITED BY SIZE INTO RESP-TEMFSINF                              
067400     END-IF                                                               
067500     .                                                                    
067600                                                                          
067700 CA-SEARCH-FOR-TIRFS SECTION.                                             
067800     MOVE 'CA-SEARCH-TIRFS' TO CURRENT-SECTION                            
067900                                                                          
068000     MOVE 1 TO RFS-IX                                                     
068100     PERFORM IMS-GNP-WDQ211                                               
068200                                                                          
068300     PERFORM UNTIL SEGMENT-MISSING                                        
068400        MOVE DIRL-IDDC      TO WS-IDDC-RFS  (RFS-IX)                      
068500        MOVE DIRL-TISKEPPN-DDC TO WS-TIRFS-RFS (RFS-IX)                   
068600        MOVE 'Y'               TO DDGS-LINE-SW                            
068700        ADD 1 TO RFS-IX                                                   
068800        PERFORM IMS-GNP-WDQ211                                            
068900     END-PERFORM                                                          
069000                                                                          
069100     PERFORM IMS-GNP-WDQ212                                               
069200     PERFORM UNTIL SEGMENT-MISSING                                        
069300        MOVE ARB-IDDC       TO WS-IDDC-RFS  (RFS-IX)                      
069400        MOVE ARB-TIRFS      TO WS-TIRFS-RFS (RFS-IX)                      
069500        ADD 1 TO RFS-IX                                                   
069600        PERFORM IMS-GNP-WDQ212                                            
069700     END-PERFORM                                                          
069800                                                                          
069900     .                                                                    
070000                                                                          
070100 D-GET-CUSTOMER-INFO SECTION.                                             
070200     MOVE 'D-GET-CUSTOMER-' TO CURRENT-SECTION                            
070300                                                                          
070400     MOVE REQU-IDDISTR      TO W-IDDISTR-WDB2                             
070500     MOVE REQU-IDKUNDNR     TO W-IDKUNDNR-WDB2                            
070600                                                                          
070700     PERFORM IMS-GU-WDB201                                                
070800                                                                          
070900     IF SEGMENT-FOUND                                                     
070910       MOVE GMT-IDLANDX2      TO RESP-IDLANDX2                            
070920       MOVE GMT-KDKUNDKAT     TO W-KDKUNDKAT                              
070930     ELSE                                                                 
070940       IF SEGMENT-MISSING                                                 
070950          MOVE NOO             TO OK-SW                                   
070960          MOVE NOT-FOUND       TO RESP-IDMFSINF                           
070970          MOVE 'ORDER ID IS INVALID'                                      
070980                               TO RESP-TEMFSINF                           
070990       END-IF                                                             
070991     END-IF                                                               
071000     .                                                                    
071100                                                                          
071200                                                                          
071300 E-GET-ORDER-LINES   SECTION.                                             
071400     MOVE 'E-GET-ORDERLINE' TO CURRENT-SECTION                            
071500                                                                          
071600     MOVE YES               TO DDGS-SW                                    
071700     MOVE ZERO              TO RESP-KVRADER                               
071800     MOVE OHUV-IDORDER      TO W-IDORDER-Q4-MIN                           
071900                               W-IDORDER-Q4-MAX                           
072000     PERFORM IMS-GN-WDQ401                                                
072100     IF SEGMENT-MISSING OR END-OF-DATA                                    
072200        MOVE NOO            TO RESP-FLCANCEL                              
072300     ELSE                                                                 
072400        PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATA                      
072500*          ADD 1            TO RESP-KVRADER                               
072600           PERFORM EA-Q4-RESPONS                                          
072700           PERFORM IMS-GN-WDQ401                                          
072800        END-PERFORM                                                       
072900     END-IF                                                               
073000                                                                          
073100     PERFORM EB-SEARCH-FOR-QUANT-ADAPTION                                 
073200                                                                          
073300     MOVE OHUV-IDDISTR       TO W-SEQA-IDDISTR                            
073400     MOVE OHUV-IDKUNDNR      TO W-SEQA-IDKUNDNR                           
073500     MOVE OHUV-IDKUNDRF(3:5) TO W-SEQA-IDKUNDRF                           
073600     PERFORM IMS-GU-WDE401                                                
073700     PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATA                         
073800        MOVE NOO            TO RESP-FLCANCEL                              
073900        PERFORM IMS-GNP-WDE411                                            
074000        PERFORM UNTIL SEGMENT-MISSING                                     
074010          IF  LYNK-NON-API  AND                                           
074020            (E4-ORAD-IDKUNDRF-RO  NOT = '00000     ' AND                  
074030             E4-ORAD-IDKUNDRF-RO  NOT = '0000000   ')                     
074060            CONTINUE                                                      
074070          ELSE                                                            
074100           IF E4-ORAD-FLFYSAVV = 'N' AND E4-ORAD-KDANNULL = '0'           
074200* ORDER IS not nilpick then increase index otherwise not                  
074300*            if a order nil-pick and the order line still on              
074400*            e4, but the row should not be shown                          
074500*            The information will be taken from Q1 in                     
074600*            FA-Q1-RESPONS                                                
074700             ADD 1        TO RESP-KVRADER                                 
074800             PERFORM EC-E4-RESPONS                                        
074900             IF E4-ORAD-FLDIRLEV = JAA                                    
074910                MOVE YES            TO RESP-FLCANCEL                      
075000                PERFORM ED-CHECK-DDGS-LINE                                
075100             END-IF                                                       
075200           END-IF                                                         
075210          END-IF                                                          
075300          PERFORM IMS-GNP-WDE411                                          
075400        END-PERFORM                                                       
075500        PERFORM IMS-GN-WDE401                                             
075600     END-PERFORM                                                          
075700                                                                          
075800     IF DDGS-NOT-OK                                                       
075900        MOVE NOO TO RESP-FLCANCEL                                         
076000     END-IF                                                               
076100     .                                                                    
076200                                                                          
076300                                                                          
076400 EA-Q4-RESPONS   SECTION.                                                 
076500     MOVE 'EA-Q4-RESPONS  ' TO CURRENT-SECTION                            
076600                                                                          
076610     IF  LYNK-NON-API  AND                                                
076620     (Q4-ORAD-IDKUNDRF-RO NOT = '00000     ' AND                          
076630      Q4-ORAD-IDKUNDRF-RO NOT = '0000000   ')                             
076640       CONTINUE                                                           
076650     ELSE                                                                 
076660       ADD 1            TO RESP-KVRADER                                   
076700       MOVE Q4-ORAD-IDARTNR   TO W-IDARTNR                                
076800       PERFORM IMS-GU-WDK601                                              
076900       PERFORM S30-GET-PART-DESCRIPTION                                   
077000       MOVE ART-KDSORT        TO RESP-KDSORT(RESP-KVRADER)                
077100       MOVE Q4-ORAD-BERADREF  TO RESP-BERADREF(RESP-KVRADER)              
077200       PERFORM S10-CONVERT-IDLEVART                                       
077300       IF WS-IDLEVART NOT = SPACE                                         
077400          MOVE WS-IDLEVART    TO RESP-IDLEVART(RESP-KVRADER)              
077500                                 RESP-IDLEVART-DEL(RESP-KVRADER)          
077600       ELSE                                                               
077700          MOVE W-IDARTNR      TO WS-IDARTNR                               
077800          MOVE FUNCTION TRIM(WS-IDARTNR)                                  
077900                              TO RESP-IDLEVART(RESP-KVRADER)              
078000                                 RESP-IDLEVART-DEL(RESP-KVRADER)          
078100       END-IF                                                             
078200*pd    MOVE Q4-ORAD-KVBEART   TO RESP-KVBEART(RESP-KVRADER)               
078300       MOVE Q4-ORAD-KVBEART-Q TO RESP-KVBEART-Q(RESP-KVRADER)             
078400                                 RESP-KVBEART(RESP-KVRADER)               
078500       MOVE ZERO              TO RESP-KVLEVART(RESP-KVRADER)              
078600*      this has to be chanaged when the new respons area                  
078700*      is updated                                                         
078800*      MOVE Q4-ORAD-IDKUNDRF-RO(1:7)                                      
078900*                             TO WS-IDKUNDRF-RO                           
079000*      MOVE WS-IDKUNDRF-RO    TO RESP-IDORDNR7(RESP-KVRADER)              
079100                                                                          
079200       MOVE WS-REGISTRED      TO RESP-KDMEDD-STATUS(RESP-KVRADER)         
079300       IF Q4-ORAD-KVBEART = Q4-ORAD-KVBEART-Q                             
079400          MOVE ZERO           TO RESP-KDMEDD-QUANT(RESP-KVRADER)          
079500       ELSE                                                               
079600          MOVE WS-QUANTIFIED  TO RESP-KDMEDD-QUANT(RESP-KVRADER)          
079700       END-IF                                                             
079800       IF Q4-ORAD-FLTILLK = 'N'                                           
079900          MOVE ZERO           TO RESP-KDMEDD-PART(RESP-KVRADER)           
080000       ELSE                                                               
080100          PERFORM S20-GET-ORDERED-PART                                    
080200          IF WS-IDLEVART NOT = SPACE                                      
080300             MOVE WS-IDLEVART TO RESP-IDLEVART(RESP-KVRADER)              
080400          ELSE                                                            
080500             MOVE W-IDARTNR   TO WS-IDARTNR                               
080600             MOVE FUNCTION TRIM(WS-IDARTNR)                               
080700                              TO RESP-IDLEVART(RESP-KVRADER)              
080800          END-IF                                                          
080900          MOVE WS-REPLACED    TO RESP-KDMEDD-PART(RESP-KVRADER)           
081000       END-IF                                                             
081100       MOVE SPACE             TO RESP-TIRFS(RESP-KVRADER)                 
081200       MOVE Q4-ORAD-IDDC      TO WS-IDDC-CHECK                            
081210                                 RESP-IDDC(RESP-KVRADER)                  
081300       PERFORM S50-CHECK-RFS                                              
081400*      Section S50-CHECK-RFS will update the value with valid RFS         
081500                                                                          
081600*      BELOW FIEDS WILL NEVER BE SET FOR Q4-LINES                         
081700       MOVE ZERO              TO RESP-IDFAKT(RESP-KVRADER)                
081800                                 RESP-IDORDNR7(RESP-KVRADER)              
081900       MOVE SPACE             TO RESP-TIDISPIN(RESP-KVRADER)              
081910     END-IF                                                               
082000     .                                                                    
082100                                                                          
082200                                                                          
082300 EB-SEARCH-FOR-QUANT-ADAPTION SECTION.                                    
082400     MOVE 'EB-SEARCH-Q-A  ' TO CURRENT-SECTION                            
082410                                                                          
082500*                                                                         
082600*    When printing the order line, we move the line from                  
082700*    WDQ4 to WDE4 and ordered quantity is treated in a                    
082800*    mysterious way.                                                      
082900*    Q4-KVBEART-Q is moved to E4-KVBEART and we lose the                  
083000*    information in Q4-KVBEART.                                           
083100*    But if there has been a quantity adaptaion we can find               
083200*    what we need in WDQ1.                                                
083300*    Thats why we save all order confirmations 43 and 44                  
083400*    to be used later in section EC-E4-RESPONS                            
083500*                                                                         
083600                                                                          
083700     MOVE OHUV-IDORDER      TO W-IDORDER-MIN                              
083800                               W-IDORDER-MAX                              
083900     MOVE 1 TO ADAPT-IX                                                   
084000                                                                          
084100     PERFORM IMS-GU-WDQ101                                                
084200     PERFORM UNTIL SEGMENT-MISSING                                        
084300                OR ADAPT-IX > ADAPT-IX-MAX                                
084400      IF OBKR-IDDISTR = OHUV-IDDISTR AND                                  
084500         OBKR-IDKUNDRF = OHUV-IDKUNDRF                                    
084600        MOVE OBKR-KDORDBEK  TO WS-KDORDBEK                                
084700        IF Q-ADAPTED                                                      
084800           MOVE OBKR-IDARTNR   TO WS-IDARTNR-ADAPT   (ADAPT-IX)           
084900           MOVE OBKR-KVBEART   TO WS-KVBEART-ADAPT   (ADAPT-IX)           
085000           MOVE OBKR-KVBEART-Q TO WS-KVBEART-Q-ADAPT (ADAPT-IX)           
085100           ADD 1 TO ADAPT-IX                                              
085200        END-IF                                                            
085300      END-IF                                                              
085400        PERFORM IMS-GN-WDQ101                                             
085500     END-PERFORM                                                          
085600     .                                                                    
085700                                                                          
085800                                                                          
085900 EC-E4-RESPONS   SECTION.                                                 
086000     MOVE 'EC-E4-RESPONS  ' TO CURRENT-SECTION                            
086100                                                                          
086200     MOVE E4-ORAD-IDARTNR   TO W-IDARTNR                                  
086300     MOVE E4-ORAD-BERADREF  TO RESP-BERADREF(RESP-KVRADER)                
086400     PERFORM IMS-GU-WDK601                                                
086500     MOVE ART-KDSORT        TO RESP-KDSORT(RESP-KVRADER)                  
086600     PERFORM S10-CONVERT-IDLEVART                                         
086700     IF WS-IDLEVART NOT = SPACE                                           
086800        MOVE WS-IDLEVART    TO RESP-IDLEVART(RESP-KVRADER)                
086900                               RESP-IDLEVART-DEL(RESP-KVRADER)            
087000     ELSE                                                                 
087100        MOVE W-IDARTNR      TO WS-IDARTNR                                 
087200        MOVE FUNCTION TRIM(WS-IDARTNR)                                    
087300                            TO RESP-IDLEVART(RESP-KVRADER)                
087400                               RESP-IDLEVART-DEL(RESP-KVRADER)            
087500     END-IF                                                               
087600                                                                          
087700     PERFORM S30-GET-PART-DESCRIPTION                                     
087800*                                                                         
087900*    We need a small fix here to get correct values                       
088000*    in RESP-KVBEART.                                                     
088100*    This is explained in the comment in EB-SEARCH-FOR-QUANT-ADAPT        
088200*                                                                         
088300     PERFORM ECA-CHECK-QUANT-ADAPTION                                     
088400                                                                          
088500     IF QUANT-ADAPTED                                                     
088600        MOVE WS-KVBEART-ADAPT(ADAPT-IX)                                   
088700                                 TO RESP-KVBEART(RESP-KVRADER)            
088800        MOVE WS-KVBEART-Q-ADAPT(ADAPT-IX)                                 
088900                                 TO RESP-KVBEART-Q(RESP-KVRADER)          
089000     ELSE                                                                 
089100* if we are splitting lines then we need to move orderd qty               
089200* from kvlevart                                                           
089300        IF   E4-ORAD-KVLEVART > ZERO                                      
089400          MOVE E4-ORAD-KVLEVART  TO RESP-KVBEART(RESP-KVRADER)            
089500                                    RESP-KVBEART-Q(RESP-KVRADER)          
089600        ELSE                                                              
089700          MOVE E4-ORAD-KVBEART   TO RESP-KVBEART(RESP-KVRADER)            
089800                                    RESP-KVBEART-Q(RESP-KVRADER)          
089900        END-IF                                                            
090000     END-IF                                                               
090100     MOVE E4-ORAD-KVLEVART     TO RESP-KVLEVART(RESP-KVRADER)             
090200*    MOVE E4-ORAD-IDKUNDRF-RO(1:5)                                        
090300*                           TO RESP-IDORDNR7(RESP-KVRADER)                
090400                                                                          
090500     IF E4-ORAD-KDRADSTA < 5                                              
090600        MOVE WS-IN-PROGRESS TO RESP-KDMEDD-STATUS(RESP-KVRADER)           
090700     END-IF                                                               
090800     PERFORM S40-CHECK-QUANTIFIED                                         
090900     IF QUANT-ADAPTED                                                     
091000        MOVE WS-QUANTIFIED  TO RESP-KDMEDD-QUANT(RESP-KVRADER)            
091100     ELSE                                                                 
091200        MOVE ZERO           TO RESP-KDMEDD-QUANT(RESP-KVRADER)            
091300     END-IF                                                               
091400     IF E4-ORAD-FLTILLK = 'N'                                             
091500        MOVE ZERO           TO RESP-KDMEDD-PART(RESP-KVRADER)             
091600     ELSE                                                                 
091700        PERFORM S20-GET-ORDERED-PART                                      
091800        IF WS-IDLEVART NOT = SPACE                                        
091900           MOVE WS-IDLEVART TO RESP-IDLEVART(RESP-KVRADER)                
092000        ELSE                                                              
092100           MOVE W-IDARTNR   TO WS-IDARTNR                                 
092200           MOVE FUNCTION TRIM(WS-IDARTNR)                                 
092300                            TO RESP-IDLEVART(RESP-KVRADER)                
092400        END-IF                                                            
092500        MOVE WS-REPLACED    TO RESP-KDMEDD-PART(RESP-KVRADER)             
092600     END-IF                                                               
092700                                                                          
092800     MOVE SPACE             TO RESP-TIRFS(RESP-KVRADER)                   
092900     MOVE KORD-IDDC         TO WS-IDDC-CHECK                              
092910                               RESP-IDDC(RESP-KVRADER)                    
093000     PERFORM S50-CHECK-RFS                                                
093100*    Section S50-CHECK-RFS will update the value with valid RFS           
093200                                                                          
093300*    BELOW FIEDS WILL NEVER BE SET FOR E4-LINES                           
093400*    MOVE SPACE             TO RESP-TIDISPIN(RESP-KVRADER)                
093500     MOVE ZERO              TO RESP-IDORDNR7(RESP-KVRADER)                
093600                                                                          
093700     MOVE ZERO              TO RESP-IDFAKT(RESP-KVRADER)                  
093800     PERFORM S52-CHECK-SHIPPED-INVOICED                                   
093900     .                                                                    
094000                                                                          
094100                                                                          
094200 ECA-CHECK-QUANT-ADAPTION SECTION.                                        
094300     MOVE 'ECA-CHECK-Q-A  ' TO CURRENT-SECTION                            
094400                                                                          
094500     MOVE NOO TO QUANT-ADAPT-SW                                           
094600     MOVE 1   TO ADAPT-IX                                                 
094700                                                                          
094800     PERFORM UNTIL ADAPT-IX > ADAPT-IX-MAX                                
094900          OR (E4-ORAD-IDARTNR = WS-IDARTNR-ADAPT(ADAPT-IX) AND            
095000              E4-ORAD-KVBEART = WS-KVBEART-Q-ADAPT(ADAPT-IX))             
095100                                                                          
095200        ADD 1 TO ADAPT-IX                                                 
095300     END-PERFORM                                                          
095400                                                                          
095500     IF ADAPT-IX NOT > ADAPT-IX-MAX                                       
095600        MOVE YES  TO QUANT-ADAPT-SW                                       
095700     END-IF                                                               
095800     .                                                                    
095900                                                                          
096000                                                                          
096100 ED-CHECK-DDGS-LINE SECTION.                                              
096200     MOVE 'ED-CHECK-DDGS  ' TO CURRENT-SECTION                            
096300                                                                          
096400     MOVE E4-ORAD-IDPRODNR  TO W-IDPRODNR                                 
096500     PERFORM IMS-GU-WDF601                                                
096600*    IF SEGMENT-MISSING                                                   
096700*       MOVE NOO TO DDGS-SW                                               
096800*    END-IF                                                               
096900     .                                                                    
097000                                                                          
097100                                                                          
097200 F-GET-ORDER-CONFIRMATIONS SECTION.                                       
097300     MOVE 'F-GET-ORDER-CONFIRMATIONS ' TO CURRENT-SECTION                 
097400                                                                          
097500     MOVE OHUV-IDORDER      TO W-IDORDER-MIN                              
097600                               W-IDORDER-MAX                              
097700                                                                          
097800     PERFORM IMS-GU-WDQ101                                                
097900     PERFORM UNTIL SEGMENT-MISSING                                        
098000        IF OBKR-IDDISTR = OHUV-IDDISTR AND                                
098100           OBKR-IDKUNDRF = OHUV-IDKUNDRF                                  
098200          MOVE OBKR-KDORDBEK TO WS-KDORDBEK                               
098300          IF CANCELLED                                                    
098400             ADD 1 TO RESP-KVRADER                                        
098500             PERFORM FA-Q1-RESPONS                                        
098600          ELSE                                                            
098700             IF BLOCKED                                                   
098800             OR (MULTIPLE-SUPERSESS AND OBKR-IDSEKVNR = 1)                
098900             OR (SINGLE-SUPERSESS   AND OBKR-IDARTNR-TILLK > 0            
099000                                    AND OBKR-IDPGM = 'W4402800')          
099100                ADD 1 TO RESP-KVRADER                                     
099200                PERFORM FA-Q1-RESPONS                                     
099300             ELSE                                                         
099400                IF BACK-ORDER OR TPO-ORDER                                
099500                   MOVE NOO TO RESP-FLCANCEL                              
099600                   ADD 1 TO RESP-KVRADER                                  
099700                   PERFORM FA-Q1-RESPONS                                  
099800                ELSE                                                      
099900                   IF VOR-ORDER                                           
100000                      MOVE NOO TO RESP-FLCANCEL                           
100100                      ADD 1 TO RESP-KVRADER                               
100200                      PERFORM FA-Q1-RESPONS                               
100300                   END-IF                                                 
100400                END-IF                                                    
100500             END-IF                                                       
100600          END-IF                                                          
100700        END-IF                                                            
100800      PERFORM IMS-GN-WDQ101                                               
100900     END-PERFORM                                                          
101000     .                                                                    
101100                                                                          
101200                                                                          
101300 FA-Q1-RESPONS   SECTION.                                                 
101400     MOVE 'FA-Q1-RESPONS  ' TO CURRENT-SECTION                            
101500                                                                          
101600     MOVE NOO               TO SHIPPED-INVOICED-SW                        
101700                                                                          
101800     MOVE OBKR-IDARTNR      TO W-IDARTNR                                  
101900     MOVE OBKR-BERADREF     TO RESP-BERADREF(RESP-KVRADER)                
101910     MOVE OBKR-IDDC         TO RESP-IDDC(RESP-KVRADER)                    
102000     PERFORM IMS-GU-WDK601                                                
102100     MOVE ART-KDSORT        TO RESP-KDSORT(RESP-KVRADER)                  
102200     PERFORM S10-CONVERT-IDLEVART                                         
102310     IF WS-IDLEVART NOT = SPACE                                           
102400        MOVE WS-IDLEVART    TO RESP-IDLEVART(RESP-KVRADER)                
102500                               RESP-IDLEVART-DEL(RESP-KVRADER)            
102600                               WS-SAVE-IDLEVART                           
102700     ELSE                                                                 
102800        MOVE W-IDARTNR      TO WS-IDARTNR                                 
102900        MOVE FUNCTION TRIM(WS-IDARTNR)                                    
103000                            TO RESP-IDLEVART(RESP-KVRADER)                
103100                               RESP-IDLEVART-DEL(RESP-KVRADER)            
103200                               WS-SAVE-IDLEVART                           
103300     END-IF                                                               
103400     PERFORM S30-GET-PART-DESCRIPTION                                     
103500     MOVE OBKR-KVBEART      TO RESP-KVBEART(RESP-KVRADER)                 
103600     MOVE OBKR-KVBEART-Q    TO RESP-KVBEART-Q(RESP-KVRADER)               
103700     MOVE ZERO              TO RESP-KVLEVART(RESP-KVRADER)                
103800     MOVE ZERO              TO RESP-IDORDNR7(RESP-KVRADER)                
103900*    MOVE OBKR-TIDISPIN     TO RESP-TIDISPIN(RESP-KVRADER)                
104000     IF VOR-ORDER                                                         
104100* for code 92,93 we should show correct ordered qty                       
104200           IF VOR-ORDER-92                                                
104300             MOVE OBKR-KVPRERO TO RESP-KVBEART(RESP-KVRADER)              
104400                                  RESP-KVBEART-Q(RESP-KVRADER)            
104500           END-IF                                                         
104600           IF VOR-ORDER-93                                                
104700             MOVE OBKR-KVANNANT   TO RESP-KVBEART(RESP-KVRADER)           
104800                                     RESP-KVBEART-Q(RESP-KVRADER)         
104900           END-IF                                                         
105000        PERFORM FAA-CHECK-RELEASED-VOR                                    
105100        IF VOR-NOT-RELEASED                                               
105200           PERFORM IMS-GNP-WDK611                                         
105300           IF CLAG-TIDISPIN < CURRENT-YYMMDD                              
105400              MOVE SPACE       TO RESP-TIDISPIN(RESP-KVRADER)             
105500           ELSE                                                           
105600              MOVE CLAG-TIDISPIN TO WS-DATE-NUM6                          
105700              MOVE ZERO          TO WS-DATE-NUM10                         
105800              PERFORM S60-CONVERT-DATE                                    
105900              MOVE WS-CHAR-DATE  TO RESP-TIDISPIN(RESP-KVRADER)           
106000           END-IF                                                         
106100           MOVE WS-BACKORDERED TO RESP-KDMEDD-STATUS(RESP-KVRADER)        
106200        ELSE                                                              
106300           PERFORM FAC-VOR-BO-CONFIRMATIONS                               
106400        END-IF                                                            
106500     ELSE                                                                 
106600        IF BACK-ORDER OR TPO-ORDER                                        
106700           IF BACK-ORDER-90                                               
106800             MOVE OBKR-KVRO      TO RESP-KVBEART(RESP-KVRADER)            
106900             MOVE OBKR-KVRO      TO RESP-KVBEART-Q(RESP-KVRADER)          
107000           ELSE                                                           
107100             MOVE OBKR-KVBEART   TO RESP-KVBEART(RESP-KVRADER)            
107200             MOVE OBKR-KVBEART-Q TO RESP-KVBEART-Q(RESP-KVRADER)          
107300           END-IF                                                         
107400           MOVE ZERO           TO RESP-KVLEVART(RESP-KVRADER)             
107500*          COMPUTE RESP-KVLEVART(RESP-KVRADER) =                          
107600*                  OBKR-KVBEART-Q - OBKR-KVRO                             
107700           PERFORM FAB-CHECK-RELEASED-BO                                  
107800           IF BO-NOT-RELEASED                                             
107810              MOVE YES              TO RESP-FLCANCEL                      
107900              PERFORM IMS-GNP-WDK611                                      
108000              IF CLAG-TIDISPIN < CURRENT-YYMMDD                           
108100                 MOVE SPACE TO RESP-TIDISPIN(RESP-KVRADER)                
108200              ELSE                                                        
108300                 MOVE CLAG-TIDISPIN TO WS-DATE-NUM6                       
108400                 MOVE ZERO          TO WS-DATE-NUM10                      
108500                 PERFORM S60-CONVERT-DATE                                 
108600                 MOVE WS-CHAR-DATE TO RESP-TIDISPIN(RESP-KVRADER)         
108700              END-IF                                                      
108800              MOVE WS-BACKORDERED                                         
108900                               TO RESP-KDMEDD-STATUS(RESP-KVRADER)        
109000           ELSE                                                           
109100              PERFORM FAC-VOR-BO-CONFIRMATIONS                            
109200           END-IF                                                         
109300        ELSE                                                              
109400           IF (SINGLE-SUPERSESS AND OBKR-IDARTNR-TILLK > 0                
109500                                AND OBKR-IDPGM = 'W4402800')              
109600              MOVE OBKR-KVBEART   TO RESP-KVBEART(RESP-KVRADER)           
109700              MOVE OBKR-KVBEART-Q TO RESP-KVBEART-Q(RESP-KVRADER)         
109800              MOVE WS-REPLACED TO RESP-KDMEDD-STATUS(RESP-KVRADER)        
109900              PERFORM FAB-CHECK-RELEASED-BO                               
110000              IF BO-NOT-RELEASED                                          
110100                 PERFORM IMS-GNP-WDK611                                   
110200                 IF CLAG-TIDISPIN < CURRENT-YYMMDD                        
110300                    MOVE SPACE TO RESP-TIDISPIN(RESP-KVRADER)             
110400                 ELSE                                                     
110500                    MOVE CLAG-TIDISPIN TO WS-DATE-NUM6                    
110600                    MOVE ZERO          TO WS-DATE-NUM10                   
110700                    PERFORM S60-CONVERT-DATE                              
110800                    MOVE WS-CHAR-DATE  TO                                 
110900                                       RESP-TIDISPIN(RESP-KVRADER)        
111000                 END-IF                                                   
111100              ELSE                                                        
111200                 PERFORM FAC-VOR-BO-CONFIRMATIONS                         
111300              END-IF                                                      
111400           ELSE                                                           
111500              MOVE SPACE          TO RESP-TIDISPIN(RESP-KVRADER)          
111600              IF BLOCKED                                                  
111700              OR (MULTIPLE-SUPERSESS AND OBKR-IDSEKVNR = 1)               
111800                 MOVE WS-BLOCKED  TO                                      
111900                                  RESP-KDMEDD-STATUS(RESP-KVRADER)        
112000              ELSE                                                        
112100                 IF CANCELLED                                             
112110                    MOVE NOO              TO RESP-FLCANCEL                
112200                    MOVE WS-CANCELLED                                     
112300                               TO RESP-KDMEDD-STATUS(RESP-KVRADER)        
112400                 END-IF                                                   
112500              END-IF                                                      
112600           END-IF                                                         
112700        END-IF                                                            
112800     END-IF                                                               
112900                                                                          
113000     MOVE ZERO              TO RESP-KDMEDD-QUANT(RESP-KVRADER)            
113100     IF OBKR-FLTILLK = 'N'                                                
113200        MOVE ZERO           TO RESP-KDMEDD-PART(RESP-KVRADER)             
113300     ELSE                                                                 
113310*DUE TO BUG #3523694, SAVING THE CURRENT WDQ1 AS S20 ALSO ISSUES          
113320*GN CALL TO WDQ1.IF CURRENT WDQ1 IS NOT SAVED, THE NEXT RECS              
113330*ARE LOST.                                                                
113400        PERFORM S15-SAVE-WDQ1-VALUES                                      
113500        PERFORM S20-GET-ORDERED-PART                                      
113600        PERFORM S16-GET-RIGHT-WDQ1-REC                                    
113700                                                                          
113710**      W-IDARTNR IS THE ORDERED PART NUMBER                              
113800        IF WS-IDLEVART NOT = SPACE                                        
113900           MOVE WS-IDLEVART TO RESP-IDLEVART(RESP-KVRADER)                
114000        ELSE                                                              
114100           MOVE W-IDARTNR   TO WS-IDARTNR                                 
114200           MOVE FUNCTION TRIM(WS-IDARTNR)                                 
114300                            TO RESP-IDLEVART(RESP-KVRADER)                
114400        END-IF                                                            
114410*       BELOW LINE CAUSED ISSUE FOR #4432719 AS OBKR VALUE IS             
114420*       LOST IN S16- RESULTING IN W-IDARTNR AS 0                          
114430*       MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                              
114431                                                                          
114440**      W-IDARTNR-TILLK IS THE REPLACED PARTNUMBER                        
114450        MOVE W-IDARTNR-TILLK   TO W-IDARTNR                               
114600        PERFORM S10-CONVERT-IDLEVART                                      
114700        IF WS-IDLEVART NOT = SPACE                                        
114800           MOVE WS-IDLEVART    TO RESP-IDLEVART-DEL(RESP-KVRADER)         
114900        ELSE                                                              
115000           MOVE W-IDARTNR      TO WS-IDARTNR                              
115100           MOVE FUNCTION TRIM(WS-IDARTNR)                                 
115200                               TO RESP-IDLEVART-DEL(RESP-KVRADER)         
115300        END-IF                                                            
115310*       #4432719                                                          
115320*       WS-SAVE-IDLEVART HAS THE REPLACED P/N.IT SHOULD NOT BE            
115330*       MOVED TO 'ORDERED PARTNUMBER.SO BELOW "IF" IS COMMENTED'          
115400*       IF WS-IDSYSTEM(1:3) = 'LYN'                                       
115500*          MOVE WS-SAVE-IDLEVART                                          
115600*                              TO RESP-IDLEVART(RESP-KVRADER)             
115700*       END-IF                                                            
115800        MOVE WS-REPLACED       TO RESP-KDMEDD-PART(RESP-KVRADER)          
115900     END-IF                                                               
116000                                                                          
116100*    BELOW FIEDS WILL NEVER BE SET FOR Q4-LINES                           
116200     IF NOT SHIPPED-INVOICED                                              
116300        MOVE ZERO           TO RESP-IDFAKT(RESP-KVRADER)                  
116400     END-IF                                                               
116500     .                                                                    
116600                                                                          
116700                                                                          
116800 FAA-CHECK-RELEASED-VOR SECTION.                                          
116900     MOVE 'FAA-CHECK-VOR  ' TO CURRENT-SECTION                            
117000                                                                          
117100*    IF VOR-KDVORATG = 2 ON WDA6 THE VOR ORDER IS RELEASED AND            
117200*    WE CAN FIND ATTACHED ORDER IN VOR-IDKUNDRF-LEV                       
117300*    SO WE CAN READ WDQ2/WDQ4 AND WDE4 TO FIND THE STATUS FOR             
117400*    THE ORDER LINE                                                       
117500*    IF VOR-KDVORATG < 2 IT IS STILL ON THE VOR QUEUE                     
117600                                                                          
117700                                                                          
117800     MOVE  'N'                 TO KDVORATG-SW                             
117900     MOVE OBKR-IDDISTR         TO W-A6-MIN-IDDISTR                        
118000                                  W-A6-MAX-IDDISTR                        
118100     MOVE OBKR-IDKUNDNR        TO W-A6-MIN-IDKUNDNR                       
118200                                  W-A6-MAX-IDKUNDNR                       
118300     MOVE OBKR-IDORDNR7(1:7)   TO W-A6-MIN-IDKUNDRF                       
118400                                  W-A6-MAX-IDKUNDRF                       
118500     MOVE OBKR-IDARTNR         TO W-IDARTNR                               
118600     MOVE NOO                  TO VOR-RELEASE-SW                          
118700     PERFORM IMS-GN-WDA6A                                                 
118800     PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATA                         
118900                OR KDVORATG-FOUND                                         
119000       MOVE SEQA-IDDISTR         TO W-A601KY-IDDISTR                      
119100       MOVE SEQA-IDKUNDNR        TO W-A601KY-IDKUNDNR                     
119200       MOVE SEQA-IDKUNDRF        TO W-A601KY-IDKUNDRF                     
119300       MOVE SEQA-IDARTNR         TO W-A601KY-IDARTNR                      
119400       MOVE SEQA-TIREGTID-URSP   TO W-A601KY-TIREGTID-URSP                
119500       MOVE SEQA-TIREGDAT-URSP   TO W-A601KY-TIREGDAT-URSP                
119600       MOVE SEQA-TIREGTID-AVV    TO W-A601KY-TIREGTID-AVV                 
119700       MOVE SEQA-TIREGDAT-AVV    TO W-A601KY-TIREGDAT-AVV                 
119800       PERFORM IMS-GU-WDA601                                              
119900       IF VOR-KDVORATG = '2'                                              
120000         MOVE VOR-IDDISTR           TO W-IDDISTR-CSEQ-BO                  
120100         MOVE VOR-IDKUNDNR          TO W-IDKUNDNR-CSEQ-BO                 
120200         MOVE VOR-IDKUNDRF-LEV(3:5) TO W-IDORDNR5-CSEQ-BO                 
120300         MOVE 'Y'                   TO KDVORATG-SW                        
120400         PERFORM IMS-GU-WDQ201-CSEQ-BO                                    
120500         IF SEGMENT-FOUND                                                 
120600            PERFORM FAAA-CHECK-UNPRINTED-VOR                              
120700            IF VOR-NOT-RELEASED                                           
120800               PERFORM FAAB-CHECK-PRINTED-VOR                             
120900            END-IF                                                        
121000            IF VOR-NOT-RELEASED                                           
121100               MOVE YES             TO VOR-RELEASE-SW                     
121200*    The vor order is released but nothing on q4 or e4                    
121300*    We have to put the SW to released so we can look                     
121400*    for order confirmations                                              
121500            END-IF                                                        
121600         END-IF                                                           
121700       ELSE                                                               
121800         PERFORM IMS-GN-WDA6A                                             
121900       END-IF                                                             
122000     END-PERFORM                                                          
122100     .                                                                    
122200                                                                          
122300                                                                          
122400 FAAA-CHECK-UNPRINTED-VOR SECTION.                                        
122500     MOVE 'FAAA-UNPRINTED-VOR' TO CURRENT-SECTION                         
122600                                                                          
122700     MOVE BO-OHUV-IDORDER     TO W-IDORDER-Q4-MIN                         
122800                                 W-IDORDER-Q4-MAX                         
122900     PERFORM IMS-GU-WDQ401-PART                                           
123000     PERFORM UNTIL SEGMENT-MISSING                                        
123100                OR END-OF-DATA                                            
123200                OR VOR-RELEASED                                           
123300                                                                          
123400              MOVE YES              TO VOR-RELEASE-SW                     
123500              MOVE WS-REGISTRED     TO                                    
123600                   RESP-KDMEDD-STATUS(RESP-KVRADER)                       
123700              MOVE BO-OHUV-IDORDNR7                                       
123800                                 TO RESP-IDORDNR7(RESP-KVRADER)           
123900        PERFORM IMS-GN-WDQ401-PART                                        
124000     END-PERFORM                                                          
124100     .                                                                    
124200                                                                          
124300                                                                          
124400 FAAB-CHECK-PRINTED-VOR SECTION.                                          
124500     MOVE 'FAAB-PRINTED-VOR' TO CURRENT-SECTION                           
124600                                                                          
124700     MOVE BO-OHUV-IDDISTR       TO W-SEQA-IDDISTR                         
124800     MOVE BO-OHUV-IDKUNDNR      TO W-SEQA-IDKUNDNR                        
124900     MOVE BO-OHUV-IDKUNDRF(3:5) TO W-SEQA-IDKUNDRF                        
125000     PERFORM IMS-GU-WDE401                                                
125100     PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATA                         
125200                OR VOR-RELEASED                                           
125300        PERFORM IMS-GNP-WDE411-PARTNO                                     
125400        PERFORM UNTIL SEGMENT-MISSING                                     
125500                   OR VOR-RELEASED                                        
125600           MOVE YES              TO VOR-RELEASE-SW                        
125700           MOVE BO-OHUV-IDORDNR7 TO RESP-IDORDNR7(RESP-KVRADER)           
125800           MOVE WS-IN-PROGRESS   TO                                       
125900                RESP-KDMEDD-STATUS(RESP-KVRADER)                          
126000           PERFORM S52-CHECK-SHIPPED-INVOICED                             
126100           PERFORM IMS-GNP-WDE411-PARTNO                                  
126200        END-PERFORM                                                       
126300        PERFORM IMS-GN-WDE401                                             
126400     END-PERFORM                                                          
126500     .                                                                    
126600                                                                          
126700                                                                          
126800 FAB-CHECK-RELEASED-BO SECTION.                                           
126900     MOVE 'FAB-CHECK-BO   ' TO CURRENT-SECTION                            
127000                                                                          
127100*    IF RAD-KDSTARAD = 4 ON WDA5 THE BACK ORDER IS RELEASED AND           
127200*    WE CAN FIND ATTACHED ORDER IN RAD-IDKUNDRF-LEV                       
127300*    SO WE CAN READ WDQ2/WDQ4 AND WDE4 TO FIND THE STATUS FOR             
127400*    THE ORDER LINE                                                       
127500*    IF RAD-KDSTARAD < 4 IT IS STILL BACK ORDER                           
127600                                                                          
127700     MOVE OBKR-IDDISTR         TO W-IDDISTR-A5-MIN                        
127800                                  W-IDDISTR-A5-MAX                        
127900     MOVE OBKR-IDKUNDNR        TO W-IDKUNDNR-A5-MIN                       
128000                                  W-IDKUNDNR-A5-MAX                       
128100     MOVE OBKR-IDORDNR7(3:5)   TO W-IDKUNDRF-A5-MIN                       
128200                                  W-IDKUNDRF-A5-MAX                       
128300     IF (SINGLE-SUPERSESS AND OBKR-IDARTNR-TILLK > 0                      
128400                          AND OBKR-IDPGM = 'W4402800')                    
128500       MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR-A5-MIN                        
128600                                  W-IDARTNR-A5-MAX                        
128700     ELSE                                                                 
128800       MOVE OBKR-IDARTNR       TO W-IDARTNR-A5-MIN                        
128900                                  W-IDARTNR-A5-MAX                        
129000     END-IF                                                               
129100     MOVE OBKR-KDORDKL         TO W-KDORDKL                               
129200     MOVE NOO                  TO BO-RELEASE-SW                           
129300     PERFORM IMS-GU-WDA501                                                
129400     IF SEGMENT-FOUND                                                     
129500        MOVE RAD-IDDISTR           TO W-IDDISTR-CSEQ-BO                   
129600        MOVE RAD-IDKUNDNR          TO W-IDKUNDNR-CSEQ-BO                  
129700        MOVE RAD-IDKUNDRF-LEV(1:5) TO W-IDORDNR5-CSEQ-BO                  
129800        MOVE RAD-IDARTNR           TO W-IDARTNR                           
129900        PERFORM IMS-GU-WDQ201-CSEQ-BO                                     
130000        IF SEGMENT-FOUND                                                  
130100           PERFORM FABA-CHECK-UNPRINTED-BO                                
130200           IF BO-NOT-RELEASED                                             
130300              PERFORM FABB-CHECK-PRINTED-BO                               
130400           END-IF                                                         
130500        END-IF                                                            
130600           IF BO-NOT-RELEASED                                             
130700              MOVE YES             TO BO-RELEASE-SW                       
130800*    The vor order is released but nothing on q4 or e4                    
130900*    We have to put the SW to released so we can look                     
131000*    for order confirmations                                              
131100           END-IF                                                         
131200     END-IF                                                               
131300     .                                                                    
131400                                                                          
131500 FABA-CHECK-UNPRINTED-BO SECTION.                                         
131600     MOVE 'FABA-UNPRINTED-BO' TO CURRENT-SECTION                          
131700                                                                          
131800     MOVE BO-OHUV-IDORDER     TO W-IDORDER-Q4-MIN                         
131900                                 W-IDORDER-Q4-MAX                         
132000     PERFORM IMS-GU-WDQ401-PART                                           
132100     PERFORM UNTIL SEGMENT-MISSING                                        
132200                OR END-OF-DATA                                            
132300                OR BO-RELEASED                                            
132400                                                                          
132500*       IF Q4-ORAD-IDKUNDRF-RO = SPACE OR '0000000   '                    
132600*          CONTINUE                                                       
132700*       ELSE                                                              
132800*          IF Q4-ORAD-IDKUNDRF-RO(3:5) = OBKR-IDORDNR7(3:5)               
132900              MOVE YES              TO BO-RELEASE-SW                      
133000              MOVE WS-REGISTRED     TO                                    
133100                   RESP-KDMEDD-STATUS(RESP-KVRADER)                       
133200              MOVE BO-OHUV-IDORDNR7 TO RESP-IDORDNR7(RESP-KVRADER)        
133300*          END-IF                                                         
133400*       END-IF                                                            
133500        PERFORM IMS-GN-WDQ401-PART                                        
133600     END-PERFORM                                                          
133700     .                                                                    
133800                                                                          
133900                                                                          
134000 FABB-CHECK-PRINTED-BO SECTION.                                           
134100     MOVE 'FABB-PRINTED-BO' TO CURRENT-SECTION                            
134200                                                                          
134300     MOVE BO-OHUV-IDDISTR       TO W-SEQA-IDDISTR                         
134400     MOVE BO-OHUV-IDKUNDNR      TO W-SEQA-IDKUNDNR                        
134500     MOVE BO-OHUV-IDKUNDRF(3:5) TO W-SEQA-IDKUNDRF                        
134600     PERFORM IMS-GU-WDE401                                                
134700     PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATA                         
134800                OR BO-RELEASED                                            
134900        PERFORM IMS-GNP-WDE411-PARTNO                                     
135000        PERFORM UNTIL SEGMENT-MISSING                                     
135100                   OR BO-RELEASED                                         
135200           MOVE YES                 TO BO-RELEASE-SW                      
135300           MOVE BO-OHUV-IDORDNR7    TO RESP-IDORDNR7(RESP-KVRADER)        
135400           MOVE WS-IN-PROGRESS      TO                                    
135500                RESP-KDMEDD-STATUS(RESP-KVRADER)                          
135600           PERFORM S52-CHECK-SHIPPED-INVOICED                             
135700           PERFORM IMS-GNP-WDE411-PARTNO                                  
135800        END-PERFORM                                                       
135900        PERFORM IMS-GN-WDE401                                             
136000     END-PERFORM                                                          
136100     .                                                                    
136200                                                                          
136300                                                                          
136400 FAC-VOR-BO-CONFIRMATIONS SECTION.                                        
136500     MOVE 'FAC-VOR-BO-CONF' TO CURRENT-SECTION                            
136600                                                                          
136700     MOVE OHUV-IDORDER      TO W-IDORDER-BO-MIN                           
136800                               W-IDORDER-BO-MAX                           
136900     PERFORM IMS-GU-WDQ101-BO                                             
137000     PERFORM UNTIL SEGMENT-MISSING                                        
137100      IF OBKR-IDDISTR = OHUV-IDDISTR AND                                  
137200         OBKR-IDKUNDRF = OHUV-IDKUNDRF                                    
137300        MOVE BO-OBKR-KDORDBEK  TO WS-KDORDBEK                             
137400        IF BO-OBKR-IDKUNDRF-RO = CURRENT-IDKUNDRF                         
137500           IF BLOCKED                                                     
137600              MOVE BO-OBKR-KVBEART-Q TO RESP-KVBEART(RESP-KVRADER)        
137700              MOVE WS-BLOCKED  TO RESP-KDMEDD-STATUS(RESP-KVRADER)        
137800              MOVE OHUV-IDORDNR7 TO RESP-IDORDNR7(RESP-KVRADER)           
137900           END-IF                                                         
138000        END-IF                                                            
138100      END-IF                                                              
138200      PERFORM IMS-GN-WDQ101-BO                                            
138300     END-PERFORM                                                          
138400     .                                                                    
138500                                                                          
138600                                                                          
138700*    --- DISPATCHER SECTIONS                                              
138800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
138900                                                                          
139000     MOVE 'GETARG'               TO SUB-KDFUNC                            
139100     MOVE 'CARPARTS.PULS.APIORDERQUERY'     TO SUB-ADDISPABS              
139200     MOVE SPACE TO REQU-AREA                                              
139300     MOVE LENGTH OF REQU-AREA         TO SUB-KVDLEN                       
139400                                                                          
139500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
139600                                                                          
139700     IF SUB-KDRC > 0                                                      
139800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
139900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
140000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
140100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
140200     END-IF                                                               
140300     .                                                                    
140400     SKIP3                                                                
140500 S02-RETURN-RESPONSE SECTION.                                             
140600                                                                          
140700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
140800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
140900                                                                          
141000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
141100                                                                          
141200     IF SUB-KDRC > 0                                                      
141300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
141400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
141500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
141600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
141700     END-IF                                                               
141800     .                                                                    
141900                                                                          
142000 S10-CONVERT-IDLEVART SECTION.                                            
142100     MOVE 'S10-CONVERT-ART' TO CURRENT-SECTION                            
142200                                                                          
142300     MOVE SPACE  TO WS-IDLEVART                                           
142400     IF WS-IDSYSTEM(1:3) = 'LYN'                                          
142500        PERFORM IMS-GU-WDF502                                             
142600        IF SEGMENT-FOUND                                                  
142700           MOVE XLEV-IDLEVART TO WS-IDLEVART                              
142800        END-IF                                                            
142900     END-IF                                                               
143000     .                                                                    
143100                                                                          
143200 S15-SAVE-WDQ1-VALUES SECTION.                                            
143300     MOVE 'S15-SAVE-WDQ1-VALUES      ' TO CURRENT-SECTION                 
143400                                                                          
143500     MOVE OBKR-IDORDER      TO SAVE-OBKR-IDORDER                          
143600     MOVE OBKR-IDARTNR      TO SAVE-OBKR-IDARTNR                          
143700     MOVE OBKR-IDLOPNR      TO SAVE-OBKR-IDLOPNR                          
143800     MOVE OBKR-IDSEKVNR     TO SAVE-OBKR-IDSEKVNR                         
143900     MOVE OBKR-IDDC         TO SAVE-OBKR-IDDC                             
144000     MOVE OBKR-KDORDBEK     TO SAVE-OBKR-KDORDBEK                         
144100     .                                                                    
144200                                                                          
144300 S16-GET-RIGHT-WDQ1-REC SECTION.                                          
144400     MOVE 'S16-GET-RIGHT-WDQ1-REC    ' TO CURRENT-SECTION                 
144500                                                                          
144600     MOVE SAVE-OBKR-IDORDER  TO W-IDORDER-MIN                             
144700                                W-IDORDER-MAX                             
144800                                                                          
144900     PERFORM IMS-GU-WDQ101                                                
145000     PERFORM UNTIL SEGMENT-MISSING OR                                     
145100         (OBKR-IDARTNR  = SAVE-OBKR-IDARTNR  AND                          
145200          OBKR-IDLOPNR  = SAVE-OBKR-IDLOPNR  AND                          
145300          OBKR-IDSEKVNR = SAVE-OBKR-IDSEKVNR AND                          
145400          OBKR-IDDC     = SAVE-OBKR-IDDC     AND                          
145500          OBKR-KDORDBEK = SAVE-OBKR-KDORDBEK)                             
145600        PERFORM IMS-GN-WDQ101                                             
145700     END-PERFORM                                                          
145800     .                                                                    
145900                                                                          
146000 S20-GET-ORDERED-PART SECTION.                                            
146100     MOVE 'S20-GET-ORDERED-PART      ' TO CURRENT-SECTION                 
146200                                                                          
146300     MOVE OHUV-IDORDER      TO W-IDORDER-MIN                              
146400                               W-IDORDER-MAX                              
146500                                                                          
146600     PERFORM IMS-GU-WDQ101                                                
146700     PERFORM UNTIL SEGMENT-MISSING OR                                     
146800        OBKR-IDARTNR-TILLK = W-IDARTNR                                    
146900        PERFORM IMS-GN-WDQ101                                             
147000     END-PERFORM                                                          
147100                                                                          
147200     IF SEGMENT-FOUND                                                     
147210        MOVE W-IDARTNR      TO W-IDARTNR-TILLK                            
147300        MOVE OBKR-IDARTNR   TO W-IDARTNR                                  
147400        PERFORM S10-CONVERT-IDLEVART                                      
147500     ELSE                                                                 
147600        MOVE SPACE          TO WS-IDLEVART                                
147700     END-IF                                                               
147800     .                                                                    
147900                                                                          
148000                                                                          
148100 S30-GET-PART-DESCRIPTION SECTION.                                        
148200     MOVE 'S30-GET-PART-DESCRIPTION  ' TO CURRENT-SECTION                 
148300                                                                          
148400     PERFORM IMS-GU-BENA-BENA11                                           
148500     IF SEGMENT-FOUND                                                     
148600        MOVE TEXT-BEART      TO RESP-BEART(RESP-KVRADER)                  
148700     END-IF                                                               
148800     .                                                                    
148900                                                                          
149000                                                                          
149100 S40-CHECK-QUANTIFIED SECTION.                                            
149200     MOVE 'S40-CHECK-QUANTIFIED      ' TO CURRENT-SECTION                 
149300                                                                          
149400     MOVE OHUV-IDORDER      TO W-IDORDER-MIN                              
149500                               W-IDORDER-MAX                              
149600                                                                          
149700     PERFORM IMS-GN-WDQ101                                                
149800     PERFORM UNTIL SEGMENT-MISSING OR                                     
149900        OBKR-KDORDBEK = 43 OR 44                                          
150000        PERFORM IMS-GN-WDQ101                                             
150100     END-PERFORM                                                          
150200     .                                                                    
150300                                                                          
150400                                                                          
150500 S50-CHECK-RFS SECTION.                                                   
150600     MOVE 'S50-CHECK-RFS  ' TO CURRENT-SECTION                            
150700                                                                          
150800     MOVE 1   TO RFS-IX                                                   
150900                                                                          
151000     PERFORM UNTIL RFS-IX > RFS-IX-MAX                                    
151100          OR WS-IDDC-CHECK = WS-IDDC-RFS(RFS-IX)                          
151200                                                                          
151300        ADD 1 TO RFS-IX                                                   
151400     END-PERFORM                                                          
151500     IF RFS-IX NOT > RFS-IX-MAX  AND                                      
151600        WS-IDDC-CHECK = WS-IDDC-RFS(RFS-IX)                               
151700            IF DDGS-LINE-SW =  'Y' AND E4-ORAD-FLDIRLEV = JAA             
151800              MOVE WS-TIRFS-RFS(RFS-IX)                                   
151900                                 TO WS-DATE-NUM6                          
152000              MOVE ZERO          TO WS-DATE-NUM10                         
152100              PERFORM S60-CONVERT-DATE                                    
152200              MOVE WS-CHAR-DATE  TO RESP-TIRFS(RESP-KVRADER)              
152300*read the last kdordbek 96                                                
152400              PERFORM IMS-GU-WDQ101                                       
152500              PERFORM UNTIL SEGMENT-MISSING                               
152600               IF OBKR-IDDISTR = OHUV-IDDISTR AND                         
152700                  OBKR-IDKUNDRF = OHUV-IDKUNDRF                           
152800                IF OBKR-IDARTNR   = W-IDARTNR AND                         
152900                   OBKR-KDORDBEK  = 96                                    
153000                 MOVE OBKR-TIDISPIN                                       
153100                                    TO WS-DATE-NUM6                       
153200                 MOVE ZERO          TO WS-DATE-NUM10                      
153300                 PERFORM S60-CONVERT-DATE                                 
153400                 MOVE WS-CHAR-DATE  TO RESP-TIDISPIN(RESP-KVRADER)        
153500                 MOVE SPACE         TO RESP-TIRFS(RESP-KVRADER)           
153600              MOVE 'N'           TO DDGS-LINE-SW                          
153700                END-IF                                                    
153800               END-IF                                                     
153900               PERFORM IMS-GN-WDQ101                                      
154000              END-PERFORM                                                 
154100            ELSE                                                          
154200               MOVE ZERO                 TO WS-DATE-NUM6                  
154300               MOVE WS-TIRFS-RFS(RFS-IX) (1:6)                            
154400                                  TO WS-DATE-NUM10                        
154500                PERFORM S60-CONVERT-DATE                                  
154600               MOVE WS-CHAR-DATE  TO RESP-TIRFS(RESP-KVRADER)             
154700            END-IF                                                        
154800     END-IF                                                               
154900     .                                                                    
155000                                                                          
155100                                                                          
155200 S52-CHECK-SHIPPED-INVOICED SECTION.                                      
155300     MOVE 'S52-CHECK-SHIPP-INV      ' TO CURRENT-SECTION                  
155400                                                                          
155500     MOVE E4-ORAD-IDPURAD      TO W-IDPURAD                               
155600     PERFORM IMS-GNP-WDE421                                               
155700     PERFORM UNTIl SEGMENT-MISSING                                        
155800        MOVE KKOLLI-IDPRODNR   TO W-IDPRODNR                              
155900        MOVE KKOLLI-IDKOLLI    TO W-IDKOLLI                               
156000                                  RESP-IDKOLLI(RESP-KVRADER)              
156100        MOVE KKOLLI-KVLEVART   TO RESP-KVLEVART(RESP-KVRADER)             
156120                                                                          
156200        PERFORM IMS-GU-WDE611                                             
156300        IF SEGMENT-FOUND                                                  
156400           MOVE KOLLI-IDFAKT   TO RESP-IDFAKT(RESP-KVRADER)               
156500           IF KOLLI-IDFAKT > 0                                            
156600              MOVE YES         TO SHIPPED-INVOICED-SW                     
156700           END-IF                                                         
156800           IF KOLLI-KDKOLSTA = +6                                         
156900             MOVE WS-IN-PROGRESS                                          
157000                               TO RESP-KDMEDD-STATUS(RESP-KVRADER)        
157100           ELSE                                                           
157200             IF KOLLI-KDKOLSTA = +7                                       
157300               MOVE WS-READY-FOR-PICK-UP                                  
157400                               TO RESP-KDMEDD-STATUS(RESP-KVRADER)        
157500             ELSE                                                         
157600               IF KOLLI-KDKOLSTA = +9                                     
157700                 MOVE WS-COMPLETED-IN-WH                                  
157800                               TO RESP-KDMEDD-STATUS(RESP-KVRADER)        
157900               END-IF                                                     
158000             END-IF                                                       
158100           END-IF                                                         
158200        END-IF                                                            
158300        PERFORM IMS-GNP-WDE421                                            
158400        IF SEGMENT-FOUND                                                  
158500           ADD 1 TO RESP-KVRADER                                          
158600           MOVE RESP-IDLEVART (RESP-KVRADER - 1) TO                       
158700                RESP-IDLEVART (RESP-KVRADER)                              
158800           MOVE RESP-KVBEART  (RESP-KVRADER - 1) TO                       
158900                RESP-KVBEART  (RESP-KVRADER)                              
159000           MOVE RESP-IDLEVART-DEL (RESP-KVRADER - 1) TO                   
159100                RESP-IDLEVART-DEL (RESP-KVRADER)                          
159200           MOVE RESP-KVBEART-Q    (RESP-KVRADER - 1) TO                   
159300                RESP-KVBEART-Q    (RESP-KVRADER)                          
159400           MOVE ZERO       TO  RESP-KVLEVART(RESP-KVRADER)                
159500           MOVE RESP-BEART    (RESP-KVRADER - 1) TO                       
159600                RESP-BEART    (RESP-KVRADER)                              
159610           MOVE RESP-BERADREF (RESP-KVRADER - 1) TO                       
159620                RESP-BERADREF (RESP-KVRADER)                              
159700           MOVE RESP-KDSORT   (RESP-KVRADER - 1) TO                       
159800                RESP-KDSORT   (RESP-KVRADER)                              
159900           MOVE ZERO       TO  RESP-IDFAKT(RESP-KVRADER)                  
160000           MOVE RESP-IDORDNR7 (RESP-KVRADER - 1) TO                       
160100                RESP-IDORDNR7 (RESP-KVRADER)                              
160200           MOVE ZERO       TO  RESP-IDKOLLI(RESP-KVRADER)                 
160300           MOVE RESP-TIDISPIN (RESP-KVRADER - 1) TO                       
160400                RESP-TIDISPIN (RESP-KVRADER)                              
160500           MOVE RESP-TIRFS    (RESP-KVRADER - 1) TO                       
160600                RESP-TIRFS    (RESP-KVRADER)                              
160700           MOVE ZERO       TO  RESP-KDMEDD-PART(RESP-KVRADER)             
160800           MOVE ZERO       TO  RESP-KDMEDD-QUANT(RESP-KVRADER)            
160900           MOVE ZERO       TO  RESP-KDMEDD-STATUS(RESP-KVRADER)           
161000        END-IF                                                            
161100     END-PERFORM                                                          
161200     .                                                                    
161300                                                                          
161400                                                                          
161500 S60-CONVERT-DATE SECTION.                                                
161600     MOVE 'S60-CONVERT-DATE          ' TO CURRENT-SECTION                 
161700                                                                          
161800     IF WS-DATE-NUM6 NOT = ZERO                                           
161900        MOVE WS-DATE-NUM6-YY   TO WS-YY-CHAR                              
162000        MOVE WS-DATE-NUM6-MM   TO WS-MM-CHAR                              
162100        MOVE WS-DATE-NUM6-DD   TO WS-DD-CHAR                              
162200     ELSE                                                                 
162300        MOVE WS-DATE-NUM10-YY  TO WS-YY-CHAR                              
162400        MOVE WS-DATE-NUM10-MM  TO WS-MM-CHAR                              
162500        MOVE WS-DATE-NUM10-DD  TO WS-DD-CHAR                              
162600     END-IF                                                               
162700     .                                                                    
162800                                                                          
162900                                                                          
163000 IMS-GU-WDQ201-CSEQ SECTION.                                              
163100     MOVE 'GU-WDQ2-CSEQ    '  TO CURRENT-IMS-SECTION                      
163200                                                                          
163300     MOVE SPACE               TO ALL-SSA                                  
163400     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ ')'                          
163500          DELIMITED BY SIZE INTO SSA1                                     
163600     MOVE '  GE'              TO GOOD-STATUSCODES                         
163700     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
163800     MOVE WDQ2-STATUS-CODE    TO STATUS-WS                                
163900     PERFORM IMS-STATUSCHECK                                              
164000     .                                                                    
164100                                                                          
164200 IMS-GU-WDQ201-CSEQ-BO SECTION.                                           
164300     MOVE 'GU-WDQ2-CSEQ-BO '  TO CURRENT-IMS-SECTION                      
164400                                                                          
164500     MOVE SPACE               TO ALL-SSA                                  
164600     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-BO ')'                       
164700          DELIMITED BY SIZE INTO SSA1                                     
164800     MOVE '  GE'              TO GOOD-STATUSCODES                         
164900     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201-BO SSA1                 
165000     MOVE WDQ2-STATUS-CODE    TO STATUS-WS                                
165100     PERFORM IMS-STATUSCHECK                                              
165200     .                                                                    
165300                                                                          
165400                                                                          
165500 IMS-GNP-WDQ212 SECTION.                                                  
165600     MOVE 'GNP-WDQ212      '  TO CURRENT-IMS-SECTION                      
165700                                                                          
165800     MOVE  SPACE              TO ALL-SSA                                  
165900     MOVE 'WDQ212  '          TO SSA1                                     
166000     MOVE '  GE'              TO GOOD-STATUSCODES                         
166100     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-WDQ212 SSA1                   
166200     MOVE WDQ2-STATUS-CODE    TO STATUS-WS                                
166300     PERFORM IMS-STATUSCHECK                                              
166400     .                                                                    
166500                                                                          
166600 IMS-GNP-WDQ211 SECTION.                                                  
166700     MOVE 'GNP-WDQ211      '  TO CURRENT-IMS-SECTION                      
166800                                                                          
166900     MOVE  SPACE              TO ALL-SSA                                  
167000     MOVE 'WDQ211  '          TO SSA1                                     
167100     MOVE '  GE'              TO GOOD-STATUSCODES                         
167200     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-WDQ211 SSA1                   
167300     MOVE WDQ2-STATUS-CODE    TO STATUS-WS                                
167400     PERFORM IMS-STATUSCHECK                                              
167500     .                                                                    
167600                                                                          
167700 IMS-GU-WDB201 SECTION.                                                   
167800     MOVE 'GU-WDB201       '  TO CURRENT-IMS-SECTION                      
167900                                                                          
168000     MOVE SPACE               TO ALL-SSA                                  
168100     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
168200          DELIMITED BY SIZE INTO SSA1                                     
168300     MOVE '  GE'               TO GOOD-STATUSCODES                        
168400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
168500     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
168600     PERFORM IMS-STATUSCHECK                                              
168700     .                                                                    
168800                                                                          
168900 IMS-GN-WDQ401 SECTION.                                                   
169000     MOVE 'GN-WDQ401       '  TO CURRENT-IMS-SECTION                      
169100                                                                          
169200     MOVE SPACE               TO ALL-SSA                                  
169300     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
169400                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
169500          DELIMITED BY SIZE INTO SSA1                                     
169600     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
169700     CALL CBLTDLI USING GN WDQ4-PCB DLI-IO-WDQ401 SSA1                    
169800     MOVE WDQ4-STATUS-CODE    TO STATUS-WS                                
169900     PERFORM IMS-STATUSCHECK                                              
170000     .                                                                    
170100                                                                          
170200 IMS-GN-WDQ401-PART SECTION.                                              
170300     MOVE 'GN-WDQ401       '  TO CURRENT-IMS-SECTION                      
170400                                                                          
170500     MOVE SPACE               TO ALL-SSA                                  
170600     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
170700                    '&WDQ401KY<=' W-WDQ401KY-MAX-X                        
170800                    '&IDARTNR  =' W-IDARTNR-X ')'                         
170900          DELIMITED BY SIZE INTO SSA1                                     
171000     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
171100     CALL CBLTDLI USING GN WDQ4-PCB DLI-IO-WDQ401-P SSA1                  
171200     MOVE WDQ4-STATUS-CODE    TO STATUS-WS                                
171300     PERFORM IMS-STATUSCHECK                                              
171400     .                                                                    
171500                                                                          
171600 IMS-GU-WDQ401-PART SECTION.                                              
171700     MOVE 'GU-WDQ401       '  TO CURRENT-IMS-SECTION                      
171800                                                                          
171900     MOVE SPACE               TO ALL-SSA                                  
172000     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
172100                    '&WDQ401KY<=' W-WDQ401KY-MAX-X                        
172200                    '&IDARTNR  =' W-IDARTNR-X ')'                         
172300          DELIMITED BY SIZE INTO SSA1                                     
172400     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
172500     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-WDQ401-P SSA1                  
172600     MOVE WDQ4-STATUS-CODE    TO STATUS-WS                                
172700     PERFORM IMS-STATUSCHECK                                              
172800     .                                                                    
172900                                                                          
173000 IMS-GU-WDK601      SECTION.                                              
173100     MOVE 'GU-WDK601       '  TO CURRENT-IMS-SECTION                      
173200                                                                          
173300     MOVE SPACE               TO ALL-SSA                                  
173400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
173500          DELIMITED BY SIZE INTO SSA1                                     
173600     MOVE '  GE'              TO GOOD-STATUSCODES                         
173700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
173800     MOVE    WDK6-STATUS-CODE TO STATUS-WS                                
173900     PERFORM IMS-STATUSCHECK                                              
174000     .                                                                    
174100                                                                          
174200 IMS-GNP-WDK611      SECTION.                                             
174300     MOVE 'GNP-WDK611      '  TO CURRENT-IMS-SECTION                      
174400                                                                          
174500     MOVE SPACE               TO ALL-SSA                                  
174600     MOVE 'WDK611  '          TO SSA1                                     
174700     MOVE '  '                TO GOOD-STATUSCODES                         
174800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
174900     MOVE    WDK6-STATUS-CODE TO STATUS-WS                                
175000     PERFORM IMS-STATUSCHECK                                              
175100     .                                                                    
175200                                                                          
175300 IMS-GU-WDF502 SECTION.                                                   
175400     MOVE 'GU-WDF502       '  TO CURRENT-IMS-SECTION                      
175500                                                                          
175600     MOVE SPACE               TO ALL-SSA                                  
175700     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
175800          DELIMITED BY SIZE INTO SSA1                                     
175900     MOVE   'WDF502  '        TO SSA2                                     
176000     MOVE '  GE'              TO GOOD-STATUSCODES                         
176100     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
176200     MOVE WDF5-STATUS-CODE    TO STATUS-WS                                
176300     PERFORM IMS-STATUSCHECK                                              
176400     .                                                                    
176500                                                                          
176600 IMS-GU-WDQ101 SECTION.                                                   
176700     MOVE 'GU-WDQ101       '  TO CURRENT-IMS-SECTION                      
176800                                                                          
176900     MOVE SPACE               TO ALL-SSA                                  
177000        STRING 'WDQ101  (WDQ101KY>=' W-WDQ101KY-MIN-X                     
177100                       '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                 
177200          DELIMITED BY SIZE INTO SSA1                                     
177300     MOVE '  GE'              TO GOOD-STATUSCODES                         
177400     CALL CBLTDLI USING GU WDQ1-PCB DLI-IO-WDQ101 SSA1                    
177500     MOVE WDQ1-STATUS-CODE    TO STATUS-WS                                
177600     PERFORM IMS-STATUSCHECK                                              
177700     .                                                                    
177800                                                                          
177900                                                                          
178000 IMS-GN-WDQ101 SECTION.                                                   
178100     MOVE 'GN-WDQ101       '  TO CURRENT-IMS-SECTION                      
178200                                                                          
178300     MOVE SPACE               TO ALL-SSA                                  
178400        STRING 'WDQ101  (WDQ101KY>=' W-WDQ101KY-MIN-X                     
178500                       '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                 
178600          DELIMITED BY SIZE INTO SSA1                                     
178700     MOVE '  GE'              TO GOOD-STATUSCODES                         
178800     CALL CBLTDLI USING GN WDQ1-PCB DLI-IO-WDQ101 SSA1                    
178900     MOVE WDQ1-STATUS-CODE    TO STATUS-WS                                
179000     PERFORM IMS-STATUSCHECK                                              
179100     .                                                                    
179200                                                                          
179300                                                                          
179400 IMS-GU-WDQ101-BO SECTION.                                                
179500     MOVE 'GU-WDQ101-BO    '  TO CURRENT-IMS-SECTION                      
179600                                                                          
179700     MOVE SPACE               TO ALL-SSA                                  
179800        STRING 'WDQ101  (WDQ101KY>=' W-WDQ101KY-BO-MIN-X                  
179900                       '&WDQ101KY<=' W-WDQ101KY-BO-MAX-X ')'              
180000          DELIMITED BY SIZE INTO SSA1                                     
180100     MOVE '  GE'              TO GOOD-STATUSCODES                         
180200     CALL CBLTDLI USING GU WDQ1-BO-PCB DLI-IO-WDQ101-BO SSA1              
180300     MOVE WDQ1-BO-STATUS-CODE TO STATUS-WS                                
180400     PERFORM IMS-STATUSCHECK                                              
180500     .                                                                    
180600                                                                          
180700                                                                          
180800 IMS-GN-WDQ101-BO SECTION.                                                
180900     MOVE 'GN-WDQ101-BO    '  TO CURRENT-IMS-SECTION                      
181000                                                                          
181100     MOVE SPACE               TO ALL-SSA                                  
181200        STRING 'WDQ101  (WDQ101KY>=' W-WDQ101KY-BO-MIN-X                  
181300                       '&WDQ101KY<=' W-WDQ101KY-BO-MAX-X ')'              
181400          DELIMITED BY SIZE INTO SSA1                                     
181500     MOVE '  GE'              TO GOOD-STATUSCODES                         
181600     CALL CBLTDLI USING GN WDQ1-BO-PCB DLI-IO-WDQ101-BO SSA1              
181700     MOVE WDQ1-BO-STATUS-CODE TO STATUS-WS                                
181800     PERFORM IMS-STATUSCHECK                                              
181900     .                                                                    
182000                                                                          
182100                                                                          
182200 IMS-GU-WDE401    SECTION.                                                
182300     MOVE 'GU-WDE401       '  TO CURRENT-IMS-SECTION                      
182400                                                                          
182500     MOVE SPACE               TO ALL-SSA                                  
182600        STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                     
182700          DELIMITED BY SIZE INTO SSA1                                     
182800     MOVE '  GE'              TO GOOD-STATUSCODES                         
182900     CALL CBLTDLI USING GU  WDE4-PCB DLI-IO-WDE401 SSA1                   
183000     MOVE WDE4-STATUS-CODE    TO STATUS-WS                                
183100     PERFORM IMS-STATUSCHECK                                              
183200     .                                                                    
183300                                                                          
183400                                                                          
183500 IMS-GN-WDE401    SECTION.                                                
183600     MOVE 'GN-WDE401       '  TO CURRENT-IMS-SECTION                      
183700                                                                          
183800     MOVE SPACE               TO ALL-SSA                                  
183900        STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                     
184000          DELIMITED BY SIZE INTO SSA1                                     
184100     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
184200     CALL CBLTDLI USING GN  WDE4-PCB DLI-IO-WDE401 SSA1                   
184300     MOVE WDE4-STATUS-CODE    TO STATUS-WS                                
184400     PERFORM IMS-STATUSCHECK                                              
184500     .                                                                    
184600                                                                          
184700                                                                          
184800 IMS-GNP-WDE411    SECTION.                                               
184900     MOVE 'GNP-WDE411      '  TO CURRENT-IMS-SECTION                      
185000                                                                          
185100     MOVE SPACE               TO ALL-SSA                                  
185200     MOVE 'WDE411  '          TO SSA1                                     
185300     MOVE '  GE'              TO GOOD-STATUSCODES                         
185400     CALL CBLTDLI USING GNP  WDE4-PCB DLI-IO-WDE411 SSA1                  
185500     MOVE WDE4-STATUS-CODE    TO STATUS-WS                                
185600     PERFORM IMS-STATUSCHECK                                              
185700     .                                                                    
185800                                                                          
185900                                                                          
186000 IMS-GNP-WDE411-PARTNO  SECTION.                                          
186100     MOVE 'GNP-WDE411-PART '  TO CURRENT-IMS-SECTION                      
186200                                                                          
186300     MOVE SPACE               TO ALL-SSA                                  
186400     STRING 'WDE411  (IDARTNR  =' W-IDARTNR-X ')'                         
186500          DELIMITED BY SIZE INTO SSA1                                     
186600     MOVE '  GE'              TO GOOD-STATUSCODES                         
186700     CALL CBLTDLI USING GNP  WDE4-PCB DLI-IO-WDE411 SSA1                  
186800     MOVE WDE4-STATUS-CODE    TO STATUS-WS                                
186900     PERFORM IMS-STATUSCHECK                                              
187000     .                                                                    
187100                                                                          
187200                                                                          
187300 IMS-GNP-WDE421    SECTION.                                               
187400     MOVE 'GNP-WDE421      '  TO CURRENT-IMS-SECTION                      
187500                                                                          
187600     MOVE SPACE               TO ALL-SSA                                  
187700     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
187800            DELIMITED BY SIZE INTO SSA1                                   
187900     MOVE 'WDE421  '          TO SSA2                                     
188000     MOVE '  GE'              TO GOOD-STATUSCODES                         
188100     CALL CBLTDLI USING GNP  WDE4-PCB DLI-IO-WDE421 SSA1 SSA2             
188200     MOVE WDE4-STATUS-CODE    TO STATUS-WS                                
188300     PERFORM IMS-STATUSCHECK                                              
188400     .                                                                    
188500                                                                          
188600                                                                          
188700 IMS-GU-BENA-BENA11 SECTION.                                              
188800     MOVE 'GNP-WDE411      '  TO CURRENT-IMS-SECTION                      
188900                                                                          
189000     MOVE SPACE               TO ALL-SSA                                  
189100                                                                          
189200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
189300          DELIMITED BY SIZE INTO SSA1                                     
189400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
189500          DELIMITED BY SIZE INTO SSA2                                     
189600     MOVE '  GE'              TO GOOD-STATUSCODES                         
189700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1 SSA2               
189800     MOVE BENA-STATUS-CODE    TO STATUS-WS                                
189900     PERFORM IMS-STATUSCHECK                                              
190000     .                                                                    
190100                                                                          
190200                                                                          
190300 IMS-GU-WDE611 SECTION.                                                   
190400     MOVE 'GU-WDE611       '  TO CURRENT-IMS-SECTION                      
190500                                                                          
190600     MOVE SPACE               TO ALL-SSA                                  
190700                                                                          
190800     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
190900          DELIMITED BY SIZE INTO SSA1                                     
191000     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
191100          DELIMITED BY SIZE INTO SSA2                                     
191200     MOVE '  GE'              TO GOOD-STATUSCODES                         
191300     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
191400     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
191500     PERFORM IMS-STATUSCHECK                                              
191600     .                                                                    
191700                                                                          
191800                                                                          
191900 IMS-GU-WDF601 SECTION.                                                   
192000     MOVE 'GU-WDF601       '  TO CURRENT-IMS-SECTION                      
192100                                                                          
192200     MOVE SPACE               TO ALL-SSA                                  
192300                                                                          
192400     STRING 'WDF601  (IDPRODNR =' W-IDPRODNR-X ')'                        
192500          DELIMITED BY SIZE INTO SSA1                                     
192600     MOVE '  GE'              TO GOOD-STATUSCODES                         
192700     CALL CBLTDLI USING GU WDF6-PCB DLI-IO-WDF601 SSA1                    
192800     MOVE WDF6-STATUS-CODE    TO STATUS-WS                                
192900     PERFORM IMS-STATUSCHECK                                              
193000     .                                                                    
193100                                                                          
193200                                                                          
193300 IMS-GU-WDA501 SECTION.                                                   
193400     MOVE 'IMS-GU-WDA501'     TO CURRENT-IMS-SECTION                      
193500                                                                          
193600     MOVE SPACE               TO ALL-SSA                                  
193700                                                                          
193800     STRING 'WDA501  (WDA501KY>=' W-WDA501KY-A5-MIN-X                     
193900                    '&WDA501KY<=' W-WDA501KY-A5-MAX-X                     
194000                    '&KDORDKL  =' W-KDORDKL-X                             
194100                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
194200          DELIMITED BY SIZE INTO SSA1                                     
194300     MOVE '  GE'              TO GOOD-STATUSCODES                         
194400     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-WDA501  SSA1                   
194500     MOVE WDA5-STATUS-CODE    TO STATUS-WS                                
194600     PERFORM IMS-STATUSCHECK                                              
194700     .                                                                    
194800                                                                          
194900 IMS-GN-WDA6A SECTION.                                                    
195000     MOVE 'IMS-GN-WDA6A'      TO CURRENT-IMS-SECTION                      
195100                                                                          
195200     MOVE SPACE               TO ALL-SSA                                  
195300                                                                          
195400     STRING 'WDA6A1  (WDA6A1KY>=' W-WDA6A1KY-MIN-X                        
195500                    '&WDA6A1KY<=' W-WDA6A1KY-MAX-X                        
195600                    '&IDARTNR  =' W-IDARTNR-X ')'                         
195700          DELIMITED BY SIZE INTO SSA1                                     
195800     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
195900     CALL  CBLTDLI  USING GN   WDA6A-PCB DLI-IO-WDA6A1 SSA1               
196000     MOVE WDA6A-STATUS-CODE   TO STATUS-WS                                
196100     PERFORM IMS-STATUSCHECK                                              
196200     .                                                                    
196300                                                                          
196400 IMS-GU-WDA601 SECTION.                                                   
196500     MOVE 'IMS-GU-WDA601'     TO CURRENT-IMS-SECTION                      
196600                                                                          
196700     MOVE SPACE               TO ALL-SSA                                  
196800                                                                          
196900     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
197000          DELIMITED BY SIZE INTO SSA1                                     
197100     MOVE '  '                TO GOOD-STATUSCODES                         
197200     CALL  CBLTDLI  USING GU   WDA6-PCB DLI-IO-WDA601 SSA1                
197300     MOVE WDA6-STATUS-CODE   TO STATUS-WS                                 
197400     PERFORM IMS-STATUSCHECK                                              
197500     .                                                                    
197600                                                                          
197700 IMS-STATUSCHECK SECTION.                                                 
197800                                                                          
197900     SET STATUS-IX                 TO 1                                   
198000     SEARCH GOOD-STATUS                                                   
198100       AT END                                                             
198200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
198300         DELIMITED BY SIZE INTO ERROR-TEXT                                
198400         CALL FELLOG                                                      
198500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
198600         CONTINUE                                                         
198700     END-SEARCH                                                           
198800     .                                                                    
