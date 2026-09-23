000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL013400.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
000400 DATE-WRITTEN.   MAJ 2006                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    NAME:       'CARPARTS.LDC.ORDERQUEUEPRC2'                            
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        LDC-ORDERQUEUE IN PRC                                            
001200*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001300*        PROGRAMMET UPPDATERAR WLORQA (WDQ3)                              
001400*                              WLXXKQ (WDR4)                              
001500*        PROGRAMMET LÄSER      WLXXKH (WDR1)                              
001600*                              WDB2                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSACTION: WL0134U                                             
002000*        REQUEST:     WL0134I1                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        RESPONSE:    WL0134O1                                            
002400*                                                                         
002500*    CHANGE LOG:                                                          
002600*    ETRACKER: 10143273 2012-09  LOCAL SOURCING CHINA                     
002700*                                                                         
002800*    2523176 - WMS-PRINT PICKING ROUND                                    
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300 INPUT-OUTPUT SECTION.                                                    
003400 FILE-CONTROL.                                                            
003500                                                                          
003600 DATA DIVISION.                                                           
003700 FILE SECTION.                                                            
003800                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'WL013400'.            
004100 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
004200 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004300 77  KDRC-DISPLAY                PIC Z(5).                                
004400 77  WS-CURRENT-SECTION          PIC X(16)   VALUE 'MAIN'.                
004500 77  WS-CURRENT-IMS-SECTION      PIC X(16)   VALUE SPACE.                 
004600                                                                          
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  YES                         PIC X       VALUE 'Y'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000                                                                          
005100 77  TAB-IX                      PIC S9(9)   VALUE +0  COMP SYNC.         
005200 77  IX1                         PIC S9(9)   VALUE +0  COMP SYNC.         
005300 77  ORDER-IX                    PIC S9(9)   VALUE +0  COMP SYNC.         
005400 77  COMPARE-IX                  PIC S9(9)   VALUE +0  COMP SYNC.         
005500 77  MSG-IX                      PIC S9(9)   VALUE +0  COMP SYNC.         
005600 77  LABL-IX                     PIC S9(9)   VALUE +0  COMP SYNC.         
005700                                                                          
005800 77  MAX-ANTAL-BILD-RADER        PIC S9(9)   VALUE 1100  COMP-3.          
005900 77  MAX-PLKSATS-RADER           PIC S9(5)   VALUE 1100  COMP-3.          
006000 77  MAX-ANTAL-ORDERDELAR        PIC S9(3)   VALUE 99    COMP-3.          
006100 77  WS-ANTPLK                   PIC S9(3)   COMP-3 VALUE 0.              
006200 77  FILLER                      PIC X(08)   VALUE 'COUNT   '.            
006300 77  WS-PLKSATS-KVRADER          PIC S9(5)   COMP-3 VALUE 0.              
006400 77  WS-KVORDER                  PIC S9(7)   COMP-3 VALUE 0.              
006500 77  WS-PRC-KVORDER              PIC S9(7)   COMP-3 VALUE 0.              
006600 77  WS-KVRADER                  PIC S9(5)   COMP-3 VALUE 0.              
006700 77  WS-PRC-KVRADER              PIC S9(5)   COMP-3 VALUE 0.              
006800 77  WS-TEST-IDDISTR             PIC 9(5).                                
006900 77  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
007000 77  TEST-IDDISTR-PREV           PIC 9(5)    COMP-3.                      
007100 77  WS-TEST-IDKUNDNR            PIC 9(7).                                
007200 77  TEST-IDKUNDNR               PIC 9(7)    COMP-3.                      
007300 77  TEST-IDKUNDNR-PREV          PIC 9(7)    COMP-3.                      
007400 77  WS-IDLOPNR-PL               PIC S9(3)   COMP-3 VALUE 0.              
007500 77  WS-IDBORD                   PIC X(03)   VALUE SPACE.                 
007600 77  WS-ODEL-IDPRC-FIRST         PIC X(04)   VALUE SPACE.                 
007700 77  FILLER                      PIC X(08)   VALUE 'PRC-LAST'.            
007800 77  WS-KDCROSS                  PIC X(02)   VALUE SPACE.                 
007900                                                                          
008000 01  WS-IDLIST.                                                           
008100     03  WS-IDPRODNR             PIC 9(7)    VALUE ZERO.                  
008200     03  WS-IDPLKLST             PIC 9(3)    VALUE ZERO.                  
008300                                                                          
008400 01  WS-IDPRC-LEN                PIC 9.                                   
008500 01  WS-IDPRC.                                                            
008600     03  WS-IDPRCBAS             PIC X(03).                               
008700     03  WS-IDPRCVAR             PIC X(01).                               
008800                                                                          
008900 01  WS-IDUSER.                                                           
009000     03  WS-IDUSER-1             PIC X(03) VALUE '000'.                   
009100     03  WS-IDUSER-2             PIC X(05).                               
009200                                                                          
009300 01  WS-ORQA-ODEL-DARFS.                                                  
009400     03  WS-ODEL-SEKEL           PIC 9(02).                               
009500     03  WS-ODEL-YYMMDD          PIC 9(06).                               
009600     03  WS-ODEL-HHMM            PIC 9(04).                               
009700                                                                          
009800 01  WS-DARFS.                                                            
009900     03  WS-DARFS-YYYY           PIC 9(04).                               
010000     03  WS-DARFS-MM             PIC 9(02).                               
010100     03  WS-DARFS-DD             PIC 9(02).                               
010200     03  WS-DARFS-HHMM           PIC 9(04).                               
010300                                                                          
010400 77  FILLER                      PIC X(08)   VALUE 'NYCKL-SW'.            
010500 77  INIT-SW                     PIC X       VALUE 'J'.                   
010600     88  INIT-OK                             VALUE 'J'.                   
010700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010800     88  NYCKLAR-OK                          VALUE 'J'.                   
010900     88  NYCKLAR-FEL                         VALUE 'N'.                   
011000 77  PRC-FEL-SW                  PIC X       VALUE 'J'.                   
011100     88  PRC-FEL                             VALUE 'N'.                   
011200 77  PLOCKSATS-SW                PIC X.                                   
011300     88  PLOCKSATS-OK                        VALUE 'J'.                   
011400     88  PLOCKSATS-FEL                       VALUE 'N'.                   
011500 77  PRE-PRINT-SW                PIC X       VALUE 'N'.                   
011600     88  PRE-PRINT-YES                       VALUE 'J'.                   
011700 77  READ-SW                     PIC X.                                   
011800     88  READ-OK                             VALUE 'J'.                   
011900                                                                          
012000 77  KDMETOD-SW                  PIC X.                                   
012100     88  KDMETOD-FEL                         VALUE 'N'.                   
012200                                                                          
012300 77  WS-KDMATT                   PIC X(1).                                
012400     88 US-MATT                              VALUE 'U'.                   
012500                                                                          
012600 01  SMALL-LETTERS              PIC X(31)  VALUE                          
012700     'abcdefghijklmnopqrstuvwxyzåäöüé'.                                   
012800 01  CAPS-LETTERS               PIC X(31)  VALUE                          
012900     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
013000                                                                          
013100 77  FILLER                      PIC X(08)   VALUE 'DAGENS  '.            
013200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013300 01  FILLER REDEFINES DAGENS-DATUM.                                       
013400     03  DAGENS-AA               PIC 9(2).                                
013500     03  DAGENS-MM               PIC 9(2).                                
013600     03  DAGENS-DD               PIC 9(2).                                
013700                                                                          
013800 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
013900                                                                          
014000 77  WS-DATUM                    PIC 9(6).                                
014100 77  WS-DATUM-LOK                PIC 9(6).                                
014200 77  WS-DATUM-AADDD              PIC 9(5).                                
014300                                                                          
014400 77  FILLER                      PIC X(08)   VALUE 'WSORDDEL'.            
014500 01  WS-ORDDEL.                                                           
014600     03  WS-ODEL-IDORDER         PIC S9(7) COMP-3.                        
014700     03  WS-ODEL-IDDC            PIC X(2).                                
014800     03  WS-ODEL-IDPRODNR        PIC S9(7) COMP-3.                        
014900     03  WS-ODEL-IDPLKLST        PIC S9(3) COMP-3.                        
015000                                                                          
015100 01  TAB-PREL-IX                 PIC S9(9) COMP SYNC.                     
015200 01  TAB-PREL-MAX                PIC S9(9) COMP SYNC.                     
015300 01  TAB-PREL-ORDDELAR.                                                   
015400     03  TAB-PREL-ORDDEL         OCCURS 99.                               
015500         05 TAB-PREL-IDORDER     PIC S9(7) COMP-3.                        
015600         05 TAB-PREL-IDDC        PIC X(2).                                
015700         05 TAB-PREL-IDPRODNR    PIC S9(7) COMP-3.                        
015800         05 TAB-PREL-IDPLKLST    PIC S9(3) COMP-3.                        
015900                                                                          
016000 77  FILLER                      PIC X(08)   VALUE 'TAB-IX  '.            
016100 01  TAB-IX                      PIC S9(9) VALUE +0 COMP SYNC.            
016200 01  TAB-IX-MAX                  PIC S9(9) VALUE +0 COMP SYNC.            
016300 01  TAB-ORDDELAR.                                                        
016400     03  TAB-ORDDEL              OCCURS 99.                               
016500         05 TAB-IDORDER          PIC S9(7) COMP-3.                        
016600         05 TAB-IDDC             PIC X(2).                                
016700         05 TAB-IDPRODNR         PIC S9(7) COMP-3.                        
016800         05 TAB-IDPLKLST         PIC S9(3) COMP-3.                        
016900                                                                          
017000*    LÄNKAREOR TILL SUBPROGRAM                                            
017100                                                                          
017200*    WL013410  LAGRING I HÄNDELSEBASER                                    
017300*01  -COPY WL013410                                                       
017400                                                                          
017500*    WL013420  REDIGERING UTSKRIFT AV PLOCKETIKETTER                      
017600*01  -COPY WL013420                                                       
017700                                                                          
017800*    WL013430  REDIGERING UTSKRIFT AV PACKUNDERLAG                        
017900*01  -COPY WL013430                                                       
018000                                                                          
018100*    WL013440  UPPDATERING AV ORDERBASER                                  
018200*01  -COPY WL013440                                                       
018300                                                                          
018400                                                                          
018500                                                                          
018600 77  FILLER                      PIC X(08)   VALUE 'KLOCKAN '.            
018700                                                                          
018800 01  WS-KLOCKAN-LOK.                                                      
018900     03  WS-TIHHMMSS-LOK         PIC 9(6).                                
019000     03  FILLER                  PIC X(2).                                
019100                                                                          
019200 01  ALL-PLUS.                                                            
019300     03  FILLER                  PIC X(80)  VALUE ALL '+'.                
019400                                                                          
019500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
019600 01  GENERAL-SUBPROGRAMS.                                                 
019700     03  WL013410                PIC X(8)    VALUE 'WL013410'.            
019800     03  WL013420                PIC X(8)    VALUE 'WL013420'.            
019900     03  WL013430                PIC X(8)    VALUE 'WL013430'.            
020000     03  WL013440                PIC X(8)    VALUE 'WL013440'.            
020100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
020400     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
020500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
020600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
020700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
020800     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
020900     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
021000     03  W612MRP                 PIC X(8)    VALUE 'W612MRP '.            
021100                                                                          
021200 01  FILLER                      PIC X(16)   VALUE 'ABEND    '.           
021300 01  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
021400                                                                          
021500 01  FILLER                      PIC X(16)   VALUE 'WL01TIDZ '.           
021600*01  -COPY WL01TIDZ                                                       
021700                                                                          
021800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
021900*01  -COPY WZ01SUB                                                        
022000                                                                          
022100 01  FILLER                      PIC X(16)   VALUE 'WORKAREA'.            
022200*01 -COPY WORKAREA                                                        
022300                                                                          
022400 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND'.             
022500*01 -COPY WWOMVAND                                                        
022600                                                                          
022700 01  FILLER                      PIC X(16)  VALUE 'WDATKONV '.            
022800*   -COPY WDATAREA                                                        
022900*                                                                         
023000 01  FILLER                      PIC X(16)  VALUE 'WZ01AUTH'.             
023100*01  -COPY WZ01AUTH                                                       
023200*                                                                         
023300 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
023400*01  -COPY WMSGCONV                                                       
023500                                                                          
023600 01  FILLER                      PIC X(16)   VALUE 'W612MRP '.            
023700*01  -COPY W612LABL                                                       
023800*01  -COPY WPLKSUMM                                                       
023900                                                                          
024000 01  FILLER                      PIC X(16)   VALUE 'W612LABL'.            
024100*01  -COPY WZ01REQU -PRE LABL-                                            
024200                                                                          
024300 01  FILLER                      PIC X(16) VALUE 'GENERIC SEARCH'.        
024400*   -COPY WWDC05                                                          
024500                                                                          
024600                                                                          
024700 01  MEDDELANDE.                                                          
024800   03  ERR-IS-INVALID            PIC X(03) VALUE '023'.                   
024900   03  ERR-ORDER-NOT-COMPLETE    PIC X(03) VALUE '194'.                   
025000   03  ERR-ORDER-MISSING         PIC X(03) VALUE '304'.                   
025100   03  TOO-MANY-LINES            PIC X(03) VALUE '028'.                   
025200   03  DATA-ITEM-MISSING         PIC X(03) VALUE '041'.                   
025300   03  SYSTEM-ERROR              PIC X(03) VALUE '099'.                   
025400   03  ORDER-PRINTED             PIC X(03) VALUE '128'.                   
025500   03  INF-NOTHING-PRINTED       PIC X(03) VALUE '250'.                   
025600   03  PICKING-UNIT-ON-PRINTER-QUEUE       PIC X(03) VALUE '131'.         
025700   03  PU-ON-PRE-PRINT-QUEUE     PIC X(03) VALUE '135'.                   
025800   03  NON-COMPLETED-PICKING-UNIT-ON       PIC X(03) VALUE '132'.         
025900   03  NO-LINES-SELECTED         PIC X(03) VALUE '293'.                   
026000   03  WRONG-ORDER-SCREEN        PIC X(03) VALUE '300'.                   
026100   03  HAS-WRONG-STATUS          PIC X(03) VALUE '390'.                   
026200   03  INVALID-COMBINATION-OF-DATA PIC X(03) VALUE '401'.                 
026300   03  WRONG-IDPRC               PIC X(08) VALUE 'IDPRC'.                 
026400   03  WRONG-IDTRP               PIC X(08) VALUE 'IDTRP'.                 
026500   03  WRONG-IDDC                PIC X(08) VALUE 'IDDC'.                  
026600   03  WRONG-IDANSTNR            PIC X(08) VALUE 'IDANSTNR'.              
026700   03  WRONG-ORDPART             PIC X(08) VALUE 'ORDPART'.               
026800   03  WRONG-IDPRODNR            PIC X(09) VALUE 'IDPRODNR'.              
026900   03  ERR-UNAUTHORIZED          PIC X(3)  VALUE '00A'.                   
027000                                                                          
027100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
027200 01  REQU-AREA.                                                           
027300*    03  -COPY WZ01REQ2                                                   
027400     03  REQU-DATA-AREA.                                                  
027500*        05  -COPY WL0134I1 -L                                            
027600*        05  -COPY WL0134I2 -L                                            
027700                                                                          
027800*    -COPY WL0134I1 -PRE V1-                                              
027900*    -COPY WL0134I2                                                       
028000                                                                          
028100 01  RESP-DATA-KVDLEN            PIC S9(9) BINARY.                        
028200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
028300 01  RESP-AREA.                                                           
028400*    03  -COPY WZ01RES2                                                   
028500     03  RESP-DATA-AREA.                                                  
028600*        05  -COPY WL0134O1 -L                                            
028700*        05  -COPY WL0134O2 -L                                            
028800                                                                          
028900*    -COPY WL0134O1                                                       
029000*    -COPY WL0134O2                                                       
029100                                                                          
029200 01  NYCKLAR-TILL-DLI.                                                    
029300                                                                          
029400     03  W-IDORDER-X.                                                     
029500         05  W-IDORDER           PIC S9(7)  VALUE ZERO COMP-3.            
029600                                                                          
029700     03  W-4003-IDHTYP-X.                                                 
029800         05  W-4003-IDHTYP       PIC  X(04) VALUE '4003'.                 
029900         05  W-4003-IDPRODNR     PIC  9(07).                              
030000         05  W-4003-IDPLKLST     PIC  9(03).                              
030100         05  W-LOW-VALUE         PIC  X(16) VALUE LOW-VALUE.              
030200                                                                          
030300     03  W-4007-IDHTYP-X.                                                 
030400         05  W-4007-IDHTYP       PIC  X(04) VALUE '4007'.                 
030500         05  W-4007-IDPRODNR     PIC  9(07).                              
030600         05  W-4007-IDPLKLST     PIC  9(03).                              
030700         05  W-LOW-VALUE         PIC  X(16) VALUE LOW-VALUE.              
030800                                                                          
030900     03  W-4447-IDHTYP-X.                                                 
031000         05  W-4447-IDHTYP       PIC  X(04) VALUE '4447'.                 
031100         05  W-4447-IDDC         PIC  X(02).                              
031200         05  W-4447-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
031300                                                                          
031400     03  W-4448-IDPRC-X.                                                  
031500         05  W-4448-IDPRC        PIC  X(04).                              
031600         05  W-4448-LOW-VALUE    PIC  X(01) VALUE LOW-VALUE.              
031700                                                                          
031800     03  W-4403-WDGX01KY-X.                                               
031900         05  W-4403-IDHTYP       PIC  X(04) VALUE '4403'.                 
032000         05  W-4403-IDDC         PIC  X(02).                              
032100         05  W-4403-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
032200                                                                          
032300     03  W-4404-IDPRC-X.                                                  
032400         05  W-4404-IDPRC        PIC X(04)  VALUE SPACE.                  
032500                                                                          
032600     03  W-WDQ301KY-X.                                                    
032700         05  W-Q301KY-IDORDER        PIC S9(7)  COMP-3.                   
032800         05  W-Q301KY-IDDC           PIC X(2).                            
032900         05  W-Q301KY-IDPRODNR       PIC S9(7)  COMP-3.                   
033000         05  W-Q301KY-IDPLKLST       PIC S9(3)  COMP-3.                   
033100                                                                          
033200     03  W-WDQ301KY-MIN-X.                                                
033300         05  W-Q301-MIN-IDORDER  PIC S9(7) COMP-3.                        
033400         05  W-Q301-MIN-IDDC     PIC X(2).                                
033500         05  W-Q301-MIN-IDPRODNR PIC S9(7) COMP-3.                        
033600         05  W-Q301-MIN-IDPLKLST PIC S9(3) COMP-3.                        
033700                                                                          
033800     03  W-WDQ301KY-MAX-X.                                                
033900         05  W-Q301-MAX-IDORDER  PIC S9(7) COMP-3.                        
034000         05  W-Q301-MAX-IDDC     PIC X(2).                                
034100         05  W-Q301-MAX-IDPRODNR PIC S9(7) COMP-3.                        
034200         05  W-Q301-MAX-IDPLKLST PIC S9(3) COMP-3.                        
034300                                                                          
034400     03  W-WDQ3DSEQ-X.                                                    
034500         05  W-Q3DSEQ-IDPRODNR       PIC S9(7)  COMP-3.                   
034600         05  W-Q3DSEQ-IDPLKLST       PIC S9(3)  COMP-3.                   
034700*WDQ3K1                                                                   
034800                                                                          
034900     03  W-WDQ3K1KY-MIN-X.                                                
035000         05  W-Q3K1KY-MIN-IDDC       PIC X(2).                            
035100         05  W-Q3K1KY-MIN-DARFS      PIC 9(12).                           
035200         05  FILLER-MIN              PIC X(14) VALUE LOW-VALUE.           
035300                                                                          
035400     03  W-WDQ3K1KY-MAX-X.                                                
035500         05  W-Q3K1KY-MAX-IDDC       PIC X(2).                            
035600         05  W-Q3K1KY-MAX-DARFS      PIC 9(12).                           
035700         05  FILLER-MAX              PIC X(14) VALUE HIGH-VALUE.          
035800                                                                          
035900     03  W-MIN-IDPRC-X.                                                   
036000         05  W-MIN-IDPRC             PIC X(4).                            
036100                                                                          
036200     03  W-MAX-IDPRC-X.                                                   
036300         05  W-MAX-IDPRC             PIC X(4).                            
036400                                                                          
036500*END WDQ3K1                                                               
036600                                                                          
036700*WDQ3H1                                                                   
036800                                                                          
036900     03  W-WDQ3H1KY-MIN-X.                                                
037000         05  W-Q3H1KY-MIN-IDDC        PIC X(2).                           
037100         05  W-Q3H1KY-MIN-IDPRCPLK    PIC X(4).                           
037200         05  W-Q3H1KY-MIN-IDLOTNR-PLK PIC S9(3) COMP-3.                   
037300         05  FILLER-MIN               PIC X(6) VALUE LOW-VALUE.           
037400                                                                          
037500     03  W-WDQ3H1KY-MAX-X.                                                
037600         05  W-Q3H1KY-MAX-IDDC        PIC X(2).                           
037700         05  W-Q3H1KY-MAX-IDPRCPLK    PIC X(4).                           
037800         05  W-Q3H1KY-MAX-IDLOTNR-PLK PIC S9(3) COMP-3.                   
037900         05  FILLER-MAX               PIC X(6) VALUE HIGH-VALUE.          
038000                                                                          
038100*END WDQ3H1                                                               
038200                                                                          
038300     03  W-IDGMT-X.                                                       
038400         05  W-WDB2-IDDISTR      PIC S9(5)   VALUE ZERO COMP-3.           
038500         05  W-WDB2-IDKUNDNR     PIC S9(7)   VALUE ZERO COMP-3.           
038600                                                                          
038700     03  W-IDDC-B6-X.                                                     
038800         05 W-IDDC-B6            PIC X(2).                                
038900                                                                          
039000     03  W-WDE601-IDPRODNR-X.                                             
039100         05  W-IDPRODNR-WDE6     PIC S9(7)   VALUE ZERO COMP-3.           
039200                                                                          
039300 01  STATUS-WS                   PIC XX.                                  
039400     88  SEGMENT-FINNS           VALUE '  '.                              
039500     88  SEGMENT-SAKNAS          VALUE 'GE'.                              
039600     88  END-OF-DATA             VALUE 'GB'.                              
039700                                                                          
039800 01  GODK-STATUSKODER.                                                    
039900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040000                                                                          
040100 01  SSA1                        PIC X(192).                              
040200 01  SSA2                        PIC X(64).                               
040300                                                                          
040400*    --- IMS FUNKTIONSKODER                                               
040500*01  -COPY W0003                                                          
040600                                                                          
040700 01  FILLER               PIC X(16)   VALUE 'WDGX4003'.                   
040800 01  DLI-IO-WDGX4003.                                                     
040900*    03  -COPY WDGX4003                                                   
041000                                                                          
041100 01  FILLER               PIC X(16)   VALUE 'WDGX4007'.                   
041200 01  DLI-IO-WDGX4007.                                                     
041300*    03  -COPY WDGX4007                                                   
041400                                                                          
041500 01  FILLER               PIC X(16)   VALUE 'WDGX4012'.                   
041600 01  DLI-IO-WDGX4012.                                                     
041700*    03  -COPY WDGX4012                                                   
041800                                                                          
041900 01  FILLER               PIC X(16)   VALUE 'WDGX4448'.                   
042000 01  DLI-IO-WDGX4448.                                                     
042100*    03  -COPY WDGX4448  -PRE XXKH-                                       
042200                                                                          
042300 01  FILLER               PIC X(16)   VALUE 'WDGX4404'.                   
042400 01  DLI-IO-WDGX4404.                                                     
042500*    03  -COPY WDGX4404                                                   
042600                                                                          
042700 01  FILLER               PIC X(16)   VALUE 'WDB201-AREA'.                
042800 01  DLI-IO-WDB201.                                                       
042900*    03  -COPY WDB201                                                     
043000                                                                          
043100 01  FILLER               PIC X(16)   VALUE 'WDB601-AREA'.                
043200 01  DLI-IO-WDB601.                                                       
043300*    03  -COPY WDB601                                                     
043400                                                                          
043500 01  FILLER               PIC X(16)   VALUE 'ORQA  -AREA'.                
043600 01  DLI-IO-ORQA.                                                         
043700*    03  -COPY WDQ301     -PRE ORQA-                                      
043800                                                                          
043900 01  FILLER               PIC X(16)   VALUE 'WDQ201-AREA'.                
044000 01  DLI-IO-WDQ201.                                                       
044100*    03  -COPY WDQ201                                                     
044200                                                                          
044300 01  FILLER               PIC X(16)   VALUE 'WDQ301-AREA'.                
044400 01  DLI-IO-WDQ301.                                                       
044500*    03  -COPY WDQ301                                                     
044600                                                                          
044700 01  FILLER               PIC X(16)   VALUE 'WDQ3K1-AREA'.                
044800 01  DLI-IO-WDQ3K1.                                                       
044900*    03  -COPY WDQ3K1                                                     
045000                                                                          
045100 01  FILLER               PIC X(16)   VALUE 'WDQ3H1-AREA'.                
045200 01  DLI-IO-WDQ3H1.                                                       
045300*    03  -COPY WDQ3H1                                                     
045400                                                                          
045500 01  FILLER               PIC X(16)   VALUE 'WDE601-AREA'.                
045600 01  DLI-IO-WDE601.                                                       
045700*    03  -COPY WDE601                                                     
045800                                                                          
045900 LINKAGE SECTION.                                                         
046000 01  MSG-PCB                     PIC X.                                   
046100 01  0693-PCB                    PIC X.                                   
046200 01  3410-ALT-PCB                PIC X.                                   
046300 01  3410-4397-PCB               PIC X.                                   
046400 01  PRNT-PCB                    PIC X.                                   
046500 01  SYNQ-PCB                    PIC X.                                   
046600 01  PRNT-PCB2                   PIC X.                                   
046700                                                                          
046800*01  -COPY W0008  -PRE ORQD-                                              
046900   05 FILLER                     PIC X.                                   
047000*01  -COPY W0008  -PRE XXKH-                                              
047100   05 FILLER                     PIC X.                                   
047200*01  -COPY W0008  -PRE 4403-                                              
047300   05 FILLER                     PIC X.                                   
047400*01  -COPY W0008  -PRE WDB2-                                              
047500   05 FILLER                     PIC X.                                   
047600*01  -COPY W0008  -PRE WDB6-                                              
047700   05 FILLER                     PIC X.                                   
047800*01  -COPY W0008  -PRE WDQ2-                                              
047900   05 FILLER                     PIC X.                                   
048000*01  -COPY W0008  -PRE WDQ3-R-                                            
048100   05 FILLER                     PIC X.                                   
048200*01  -COPY W0008  -PRE WDQ3-                                              
048300   05 FILLER                     PIC X.                                   
048400*01  -COPY W0008  -PRE WDQ3K-                                             
048500   05 FILLER                     PIC X.                                   
048600*01  -COPY W0008  -PRE WDQ3H-                                             
048700   05 FILLER                     PIC X.                                   
048800*01  -COPY W0008  -PRE 4003-                                              
048900   05 FILLER                     PIC X.                                   
049000*01  -COPY W0008  -PRE 4007-                                              
049100   05 FILLER                     PIC X.                                   
049200*01  -COPY W0008  -PRE WDE6-                                              
049300   05 FILLER                     PIC X.                                   
049400                                                                          
049500*  WL013410 PCB                                                           
049600 01  3410-4003-PCB               PIC X.                                   
049700 01  3410-4007-PCB               PIC X.                                   
049800 01  3410-4017-PCB               PIC X.                                   
049900 01  3410-4448-PCB               PIC X.                                   
050000 01  3410-4453-PCB               PIC X.                                   
050100 01  3410-4512-PCB               PIC X.                                   
050200 01  3410-WDM2-PCB               PIC X.                                   
050300 01  3410-4536-PCB               PIC X.                                   
050400 01  3410-WDA5-PCB               PIC X.                                   
050500 01  3410-WDA6-PCB               PIC X.                                   
050600 01  3410-WDA6B-PCB              PIC X.                                   
050700 01  3410-WDB2-PCB               PIC X.                                   
050800 01  3410-WDB6-PCB               PIC X.                                   
050900 01  3410-WDD3-PCB               PIC X.                                   
051000 01  3410-WDD5-PCB               PIC X.                                   
051100 01  3410-WDG6-PCB               PIC X.                                   
051200 01  3410-WDK6-PCB               PIC X.                                   
051300 01  3410-WDK7-PCB               PIC X.                                   
051400 01  3410-WDK9-PCB               PIC X.                                   
051500 01  3410-WDQ1-PCB               PIC X.                                   
051600 01  3410-WDQ2-PCB               PIC X.                                   
051700 01  3410-WDQ2C-PCB              PIC X.                                   
051800 01  3410-WDQ3-PCB               PIC X.                                   
051900 01  3410-WDQ4-PCB               PIC X.                                   
052000 01  3410-4541-PCB               PIC X.                                   
052100 01  3410-2203-PCB               PIC X.                                   
052200 01  3410-ROLL-WDP4A-PCB         PIC X.                                   
052300 01  3410-ORQICSQ-PCB            PIC X.                                   
052400 01  3410-WLLOGA-PCB             PIC X.                                   
052500 01  3410-DEAV-ARTM-PCB          PIC X.                                   
052600 01  3410-DEAV-ARTS-PCB          PIC X.                                   
052700 01  3410-DEAV-WDB6-PCB          PIC X.                                   
052800 01  3410-DEAV-WDB2-PCB          PIC X.                                   
052900 01  3410-DEAV-WDL7-PCB          PIC X.                                   
053000 01  3410-DEAV-WDK7-PCB          PIC X.                                   
053100 01  3410-DEAV-WDR2-PCB          PIC X.                                   
053200 01  3410-DEAV-WDR5-PCB          PIC X.                                   
053300 01  3410-DEAV-WDC1-PCB          PIC X.                                   
053400 01  3410-RANS-XXKM-PCB          PIC X.                                   
053500 01  3410-RANS-ARTM-PCB          PIC X.                                   
053600 01  3410-RANS-ARTS-PCB          PIC X.                                   
053700 01  3410-AREG-WDK6-PCB          PIC X.                                   
053800 01  3410-AREG-WDK7-PCB          PIC X.                                   
053900 01  3410-SPAR-WDF8-PCB          PIC X.                                   
054000 01  3410-SPAR-WDF8A-PCB         PIC X.                                   
054100 01  3410-SPAR-WDK6-PCB          PIC X.                                   
054200 01  3410-PRQU-WDG2-PCB          PIC X.                                   
054300 01  3410-PRQU-WDC7-PCB          PIC X.                                   
054400 01  3410-PRQU-WDK6-PCB          PIC X.                                   
054500 01  3410-PRNO-3107-PCB          PIC X.                                   
054600 01  3410-PLATS-DM-PCB           PIC X.                                   
054700 01  3410-PLATS-DN-PCB           PIC X.                                   
054800 01  3410-PLATS-DP-PCB           PIC X.                                   
054900 01  3410-PLATS-DO-PCB           PIC X.                                   
055000 01  3410-PLATS-WDE6C-PCB        PIC X.                                   
055100 01  3410-PLATS-GMTC-PCB         PIC X.                                   
055200 01  3410-PLATS-WDB6-PCB         PIC X.                                   
055300 01  3410-KOM-WDP8-PCB           PIC X.                                   
055400                                                                          
055500*  WL013420 PCB                                                           
055600 01  3420-4003-PCB               PIC X.                                   
055700 01  3420-WDQ3D-PCB              PIC X.                                   
055800 01  3420-WDF5-PCB               PIC X.                                   
055900 01  3420-WDB6-PCB               PIC X.                                   
056000 01  3420-WDE6-PCB               PIC X.                                   
056100 01  3420-WDQ2-PCB               PIC X.                                   
056200 01  3420-4535-PCB               PIC X.                                   
056300 01  SYNQ-ATAB-PCB               PIC X.                                   
056400 01  3420-WDQ3-PCB               PIC X.                                   
056500                                                                          
056600*  WL013430 PCB                                                           
056700 01  3430-4007-PCB               PIC X.                                   
056800 01  3430-4447-PCB               PIC X.                                   
056900 01  3430-4535-PCB               PIC X.                                   
057000 01  3430-4732-PCB               PIC X.                                   
057100 01  3430-WDB2-PCB               PIC X.                                   
057200 01  3430-WDQ2-PCB               PIC X.                                   
057300 01  3430-WDQ3-PCB               PIC X.                                   
057400 01  3430-WDB6-PCB               PIC X.                                   
057500 01  3430-WDE6-PCB               PIC X.                                   
057600 01  3430-WDI2-PCB               PIC X.                                   
057700 01  3430-WDF5-PCB               PIC X.                                   
057800 01  3430-WDK5-PCB               PIC X.                                   
057900                                                                          
058000*  WL013440 PCB                                                           
058100 01  3440-4007-PCB               PIC X.                                   
058200 01  3440-4017-PCB               PIC X.                                   
058300 01  3440-4447-PCB               PIC X.                                   
058400 01  3440-4487-PCB               PIC X.                                   
058500 01  3440-4726-PCB               PIC X.                                   
058600 01  3440-WDE4-PCB               PIC X.                                   
058700 01  3440-WDE6-PCB               PIC X.                                   
058800 01  3440-WDG6-PCB               PIC X.                                   
058900 01  3440-WDQ2-PCB               PIC X.                                   
059000 01  3440-WDQ2C-PCB              PIC X.                                   
059100 01  3440-WDQ3-PCB               PIC X.                                   
059200 01  3440-WDB6-PCB               PIC X.                                   
059300 01  3440-WDK5-PCB               PIC X.                                   
059400 01  DNOT-ORQP-PCB               PIC X.                                   
059500 01  DNOT-ORQP2-PCB              PIC X.                                   
059600 01  DNOT-ORQP3-PCB              PIC X.                                   
059700 01  DNOT-4013-PCB               PIC X.                                   
059800 01  DNOT-BENA-PCB               PIC X.                                   
059900                                                                          
060000*  W612MRP PCB                                                            
060100 01  MRP-WDD3-PCB                PIC X(4).                                
060200 01  MRP-WDK6-PCB                PIC X(4).                                
060300 01  MRP-WDG2-PCB                PIC X(4).                                
060400 01  MRP-WDC3-PCB                PIC X(4).                                
060500 01  MRP-WDJ1-PCB                PIC X(4).                                
060600 01  MRP-WDJ4-PCB                PIC X(4).                                
060700 01  MRP-WDD3A-PCB               PIC X(4).                                
060800                                                                          
060900 PROCEDURE DIVISION  USING MSG-PCB  0693-PCB 3410-ALT-PCB                 
061000                           3410-4397-PCB PRNT-PCB SYNQ-PCB                
061100                           PRNT-PCB2                                      
061200                           ORQD-PCB XXKH-PCB 4403-PCB                     
061300                           WDB2-PCB WDB6-PCB WDQ2-PCB                     
061400                           WDQ3-R-PCB WDQ3-PCB                            
061500                           WDQ3K-PCB WDQ3H-PCB                            
061600                           4003-PCB 4007-PCB WDE6-PCB                     
061700*  WL013410 PCB                                                           
061800                           3410-4003-PCB       3410-4007-PCB              
061900                           3410-4017-PCB       3410-4448-PCB              
062000                           3410-4453-PCB                                  
062100                           3410-4512-PCB                                  
062200                           3410-WDM2-PCB       3410-4536-PCB              
062300                           3410-WDA5-PCB       3410-WDA6-PCB              
062400                           3410-WDA6B-PCB      3410-WDB2-PCB              
062500                           3410-WDB6-PCB       3410-WDD3-PCB              
062600                           3410-WDD5-PCB       3410-WDG6-PCB              
062700                           3410-WDK6-PCB       3410-WDK7-PCB              
062800                           3410-WDK9-PCB                                  
062900                           3410-WDQ1-PCB       3410-WDQ2-PCB              
063000                           3410-WDQ2C-PCB      3410-WDQ3-PCB              
063100                           3410-WDQ4-PCB       3410-4541-PCB              
063200                           3410-2203-PCB  3410-ROLL-WDP4A-PCB             
063300                           3410-ORQICSQ-PCB    3410-WLLOGA-PCB            
063400                           3410-DEAV-ARTM-PCB                             
063500                           3410-DEAV-ARTS-PCB  3410-DEAV-WDB6-PCB         
063600                           3410-DEAV-WDB2-PCB                             
063700                           3410-DEAV-WDL7-PCB  3410-DEAV-WDK7-PCB         
063800                           3410-DEAV-WDR2-PCB  3410-DEAV-WDR5-PCB         
063900                           3410-DEAV-WDC1-PCB                             
064000                           3410-RANS-XXKM-PCB  3410-RANS-ARTM-PCB         
064100                           3410-RANS-ARTS-PCB                             
064200                           3410-AREG-WDK6-PCB  3410-AREG-WDK7-PCB         
064300                           3410-SPAR-WDF8-PCB  3410-SPAR-WDF8A-PCB        
064400                           3410-SPAR-WDK6-PCB                             
064500                           3410-PRQU-WDG2-PCB  3410-PRQU-WDC7-PCB         
064600                           3410-PRQU-WDK6-PCB                             
064700                           3410-PRNO-3107-PCB                             
064800                           3410-PLATS-DM-PCB   3410-PLATS-DN-PCB          
064900                           3410-PLATS-DP-PCB                              
065000                           3410-PLATS-DO-PCB  3410-PLATS-WDE6C-PCB        
065100                           3410-PLATS-GMTC-PCB 3410-PLATS-WDB6-PCB        
065200                           3410-KOM-WDP8-PCB                              
065300*  WL013420 PCB                                                           
065400                           3420-4003-PCB       3420-WDQ3D-PCB             
065500                           3420-WDF5-PCB       3420-WDB6-PCB              
065600                           3420-WDE6-PCB       3420-WDQ2-PCB              
065700                           3420-4535-PCB SYNQ-ATAB-PCB                    
065800                           3420-WDQ3-PCB                                  
065900*  WL013430 PCB                                                           
066000                           3430-4007-PCB       3430-4447-PCB              
066100                           3430-4535-PCB       3430-4732-PCB              
066200                           3430-WDB2-PCB       3430-WDQ2-PCB              
066300                           3430-WDQ3-PCB       3430-WDB6-PCB              
066400                           3430-WDE6-PCB       3430-WDI2-PCB              
066500                           3430-WDF5-PCB       3430-WDK5-PCB              
066600*  WL013440 PCB                                                           
066700                           3440-4007-PCB       3440-4017-PCB              
066800                           3440-4447-PCB       3440-4487-PCB              
066900                           3440-4726-PCB       3440-WDE4-PCB              
067000                           3440-WDE6-PCB       3440-WDG6-PCB              
067100                           3440-WDQ2-PCB       3440-WDQ2C-PCB             
067200                           3440-WDQ3-PCB       3440-WDB6-PCB              
067300                           3440-WDK5-PCB                                  
067400                           DNOT-ORQP-PCB                                  
067500                           DNOT-ORQP2-PCB                                 
067600                           DNOT-ORQP3-PCB                                 
067700                           DNOT-4013-PCB                                  
067800                           DNOT-BENA-PCB                                  
067900                                                                          
068000* W612MRP PCB                                                             
068100                           MRP-WDD3-PCB                                   
068200                           MRP-WDK6-PCB                                   
068300                           MRP-WDG2-PCB                                   
068400                           MRP-WDC3-PCB                                   
068500                           MRP-WDJ1-PCB                                   
068600                           MRP-WDJ4-PCB                                   
068700                           MRP-WDD3A-PCB                                  
068800                           .                                              
068900                                                                          
069000 MAIN SECTION.                                                            
069100                                                                          
069200     PERFORM S10-FETCH-REQUEST-ARGUMENT                                   
069300     PERFORM A-INIT                                                       
069400                                                                          
069500     IF INIT-OK                                                           
069600       PERFORM B-CONTROL-KEYS                                             
069700       IF NYCKLAR-OK                                                      
069800         IF REQU-UPDATE                                                   
069900           PERFORM G-CHECK-INPUT                                          
070000           IF PLOCKSATS-OK                                                
070100             PERFORM H-UPPDATERA                                          
070200           END-IF                                                         
070300         END-IF                                                           
070400         IF REQU-QUERY OR                                                 
070500            (REQU-UPDATE AND PRE-PRINT-YES)                               
070600           PERFORM C-VISA-ORDER                                           
070700         END-IF                                                           
070800         IF SUB-KDTRANS(1:6) NOT = 'WLA134'                               
070900           MOVE REQU-IDPRC-KEY   TO RESP-IDPRC-KEY                        
071000         END-IF                                                           
071100       END-IF                                                             
071200     END-IF                                                               
071300                                                                          
071400     IF SUB-KDTRANS(1:6) = 'WLA134'                                       
071500       PERFORM S30-MSG-CONV                                               
071600       IF REQU-KDPGMACT = 'E'                                             
071700         PERFORM S12-RETURN-RESPONSE                                      
071800       ELSE                                                               
071900         PERFORM S11-RETURN-RESPONSE                                      
072000       END-IF                                                             
072100     ELSE                                                                 
072200       PERFORM S11-RETURN-RESPONSE                                        
072300     END-IF                                                               
072400                                                                          
072500     MOVE ZERO                   TO RETURN-CODE                           
072600     GOBACK                                                               
072700     .                                                                    
072800                                                                          
072900 A-INIT SECTION.                                                          
073000     MOVE 'A-INIT         '      TO WS-CURRENT-SECTION                    
073100                                                                          
073200     PERFORM S20-INITIATE-RESPONSE                                        
073300                                                                          
073400     PERFORM AA-HANDLE-INPUT-VERSIONS                                     
073500                                                                          
073600     ACCEPT DAGENS-DATUM       FROM DATE                                  
073700     ACCEPT DAGENS-TID         FROM TIME                                  
073800                                                                          
073900     IF SUB-KDTRANS(1:7) = 'WL0134T' OR 'WL0134U' OR 'WL0134Y'            
074000        OR 'WL0138U'                                                      
074100       CONTINUE                                                           
074200     ELSE                                                                 
074300       MOVE FUNCTION UPPER-CASE (REQU-KDPGMACT)                           
074400                                 TO REQU-KDPGMACT                         
074500       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)                           
074600                                 TO REQU-IDDC-KEY                         
074700       MOVE FUNCTION UPPER-CASE (REQU-IDPRC-KEY)                          
074800                                 TO REQU-IDPRC-KEY                        
074900       INSPECT REQU-IDPRC-KEY CONVERTING                                  
075000                   SMALL-LETTERS TO CAPS-LETTERS                          
075100                                                                          
075200       MOVE 001                  TO AUTH-KDCALL                           
075300       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
075400                                    REQU-WZ01REQ2                         
075500       IF AUTH-KDRC > 0                                                   
075600*        For query, its not necessary to have user / access token         
075700         IF REQU-QUERY AND AUTH-KDRC <= 4                                 
075800           CONTINUE                                                       
075900         ELSE                                                             
076000           MOVE ERR-UNAUTHORIZED TO RESP-IDMSG-ERROR                      
076100           MOVE NEJ              TO INIT-SW                               
076200         END-IF                                                           
076300       END-IF                                                             
076400     END-IF                                                               
076500                                                                          
076600     IF REQU-KDPGMACT = 'E'                                               
076700       PERFORM AB-INIT-SUBPGM-AREAS                                       
076800     END-IF                                                               
076900                                                                          
077000     IF REQU-KVRADER-MAX1 NOT NUMERIC                                     
077100       MOVE ZERO                 TO REQU-KVRADER-MAX1                     
077200     END-IF                                                               
077300                                                                          
077400     IF REQU-KDPGMACT = 'S' OR 'E'                                        
077500       MOVE REQU-IDDC-KEY        TO W-IDDC-B6                             
077600       PERFORM IMS-08-GU-WDB601                                           
077700       IF SEGMENT-SAKNAS                                                  
077800         MOVE SYSTEM-ERROR       TO RESP-IDMSG-ERROR                      
077900         MOVE WRONG-IDDC         TO RESP-IDELMT-ERROR                     
078000         MOVE NEJ                TO INIT-SW                               
078100       END-IF                                                             
078200     ELSE                                                                 
078300       MOVE SYSTEM-ERROR         TO RESP-IDMSG-ERROR                      
078400       MOVE WRONG-IDDC           TO RESP-IDELMT-ERROR                     
078500       MOVE NEJ                  TO INIT-SW                               
078600     END-IF                                                               
078700                                                                          
078800     IF INIT-OK                                                           
078900       MOVE '011'                TO MSGI-KDCALL                           
079000       MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                         
079100       MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                         
079200       MOVE DAGENS-TID           TO MSGI-TILOKTID                         
             MOVE DCS-IDDC             TO MSGI-IDDC                             
079300       CALL WL01TIDZ          USING MSGI-WL01TIDZ                         
079400       MOVE MSGI-TILOKDAT(1:6)   TO DAGENS-DATUM                          
079500       MOVE MSGI-TILOKTID(1:4)   TO DAGENS-TID(1:4)                       
079600                                                                          
079700       MOVE DAGENS-DATUM         TO WS-DATUM                              
079800       MOVE WS-DATUM             TO WS-DATUM-LOK                          
079900       MOVE DAGENS-TID           TO WS-KLOCKAN-LOK                        
080000                                                                          
080100       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
080200       MOVE WS-DATUM             TO DAT-I-TIDATUM                         
080300       CALL WDATKONV          USING DAT-KDDATFORM                         
080400                                    DAT-I-TIDATUM                         
080500                                    DAT-O-TIDATUM                         
080600                                    DAT-KDSVAR                            
080700                                                                          
080800       IF DAT-KDSVAR-FEL                                                  
080900         MOVE 'DATUMKONVERTERINGEN HAR GÅTT SNETT'                        
081000                                 TO ERROR-TEXT                            
081100         CALL FELLOG                                                      
081200       END-IF                                                             
081300                                                                          
081400       MOVE DAT-TIAADDD          TO WS-DATUM-AADDD                        
081500                                                                          
081600       MOVE 1                    TO WS-ANTPLK                             
081700       MOVE REQU-IDPRC-KEY       TO W-4448-IDPRC                          
081800                                                                          
081900       MOVE ZERO                 TO TEST-IDKUNDNR-PREV                    
082000                                    TEST-IDDISTR-PREV                     
082100                                    TEST-IDKUNDNR                         
082200                                    TEST-IDDISTR                          
082300     END-IF                                                               
082400                                                                          
082500     .                                                                    
082600                                                                          
082700 AA-HANDLE-INPUT-VERSIONS SECTION.                                        
082800                                                                          
082900     IF REQU-IDINVER = '001'                                              
083000       MOVE REQU-DATA-AREA       TO V1-REQU-WL0134I1                      
083100       MOVE V1-REQU-IDDC-KEY     TO REQU-IDDC-KEY                         
083200       MOVE V1-REQU-IDPRC-KEY    TO REQU-IDPRC-KEY                        
083300       MOVE V1-REQU-IDTRP-KEY    TO REQU-IDTRP-KEY                        
083400       IF V1-REQU-IDANSTNR = ALL '+'                                      
083500         MOVE ALL-PLUS           TO REQU-IDANSTNR                         
083600       ELSE                                                               
083700         MOVE V1-REQU-IDANSTNR   TO REQU-IDANSTNR                         
083800       END-IF                                                             
083900       MOVE V1-REQU-IDBORD       TO REQU-IDBORD                           
084000       MOVE V1-REQU-KDMATT       TO REQU-KDMATT                           
084100       IF V1-REQU-KVRADER-MAX1 = ALL '+'                                  
084200         MOVE ALL-PLUS           TO REQU-KVRADER-MAX1                     
084300       ELSE                                                               
084400         MOVE V1-REQU-KVRADER-MAX1                                        
084500                                 TO REQU-KVRADER-MAX1                     
084600       END-IF                                                             
084700       MOVE V1-REQU-IDTRANS      TO REQU-IDTRANS                          
084800       PERFORM                                                            
084900       VARYING TAB-IX FROM 1 BY 1                                         
085000         UNTIL TAB-IX > MAX-ANTAL-BILD-RADER                              
085100         MOVE V1-REQU-TABELLRAD (TAB-IX)                                  
085200                                 TO REQU-TABELLRAD (TAB-IX)               
085300       END-PERFORM                                                        
085400       MOVE SPACES               TO REQU-DARFSDAT-KEY                     
085500                                    REQU-FLPREPRINT                       
085600       MOVE ZERO                 TO REQU-IDQUEUENR                        
085700     ELSE                                                                 
085800       IF REQU-IDINVER = '002'                                            
085900         MOVE REQU-DATA-AREA     TO REQU-WL0134I2                         
086000       ELSE                                                               
086100         MOVE SYSTEM-ERROR       TO RESP-IDMSG-ERROR                      
086200         MOVE 'IDINVER'          TO RESP-IDELMT-ERROR                     
086300       END-IF                                                             
086400     END-IF                                                               
086500                                                                          
086600     .                                                                    
086700                                                                          
086800 AB-INIT-SUBPGM-AREAS SECTION.                                            
086900                                                                          
087000     INITIALIZE 3410-WL013410                                             
087100*                                                                         
087200     IF REQU-IDTRANS = 'L138' OR 'A138'                                   
087300       INITIALIZE 3420-WL013420                                           
087400       INITIALIZE 3430-WL013430                                           
087500     END-IF                                                               
087600     INITIALIZE 3440-WL013440                                             
087700*                                                                         
087800     .                                                                    
087900                                                                          
088000 B-CONTROL-KEYS SECTION.                                                  
088100     MOVE 'B-CONTROL-KEYS   '    TO WS-CURRENT-SECTION                    
088200                                                                          
088300     MOVE JA                     TO NYCKLAR-SW                            
088400                                                                          
088500     MOVE LOW-VALUES             TO W-WDQ3K1KY-MIN-X                      
088600     MOVE HIGH-VALUES            TO W-WDQ3K1KY-MAX-X                      
088700     MOVE REQU-IDDC-KEY          TO W-Q3K1KY-MIN-IDDC                     
088800                                    W-Q3K1KY-MAX-IDDC                     
088900                                                                          
089000     PERFORM BA-CONTROL-PRC                                               
089100                                                                          
089200     PERFORM BB-CONTROL-RFS                                               
089300                                                                          
089400     IF REQU-KDPGMACT = 'E'                                               
089500       PERFORM BC-CONTROL-USER                                            
089600     END-IF                                                               
089700                                                                          
089800     IF REQU-IDBORD = ALL '+'                                             
089900       MOVE SPACE                TO WS-IDBORD                             
090000     ELSE                                                                 
090100       MOVE REQU-IDBORD          TO WS-IDBORD                             
090200     END-IF                                                               
090300     MOVE REQU-IDDC-KEY          TO RESP-IDDC-KEY                         
090400                                                                          
090500     IF NYCKLAR-OK                                                        
090600       MOVE REQU-IDPRC-KEY       TO RESP-IDPRC-KEY                        
090700       IF PRE-PRINT-SW = NEJ AND                                          
090800          WS-IDUSER-2 NUMERIC                                             
090900         MOVE WS-IDUSER-2        TO RESP-IDANSTNR                         
091000       END-IF                                                             
091100       MOVE WS-IDBORD            TO RESP-IDBORD                           
091200       INSPECT RESP-IDDC-KEY  REPLACING LEADING ZERO BY SPACE             
091300       INSPECT RESP-IDPRC-KEY REPLACING LEADING ZERO BY SPACE             
091400       INSPECT RESP-IDTRP-KEY REPLACING LEADING ZERO BY SPACE             
091500       IF REQU-KDPGMACT = 'E'                                             
091600         MOVE WS-IDBORD          TO RESP-IDBORD                           
091700       END-IF                                                             
091800     END-IF                                                               
091900     .                                                                    
092000                                                                          
092100 BA-CONTROL-PRC SECTION.                                                  
092200     MOVE 'BA-CONTROL-PRC  '     TO WS-CURRENT-SECTION                    
092300                                                                          
092400     MOVE LOW-VALUE              TO W-MIN-IDPRC                           
092500     MOVE HIGH-VALUE             TO W-MAX-IDPRC                           
092600                                                                          
092700     MOVE REQU-IDPRC-KEY         TO WS-IDPRC                              
092800     INSPECT WS-IDPRC REPLACING ALL '+' BY SPACE                          
092900                                                                          
093000     IF DCS-CDC                                                           
093100       IF WS-IDPRCBAS NUMERIC AND WS-IDPRCBAS > ZERO                      
093200         CONTINUE                                                         
093300       ELSE                                                               
093400         MOVE NEJ                TO NYCKLAR-SW                            
093500         MOVE WRONG-IDPRC        TO RESP-IDELMT-ERROR                     
093600         MOVE '023'              TO RESP-IDMSG-ERROR                      
093700       END-IF                                                             
093800     END-IF                                                               
093900                                                                          
094000     IF NYCKLAR-OK                                                        
094100       COMPUTE WS-IDPRC-LEN = FUNCTION LENGTH                             
094200                                (FUNCTION TRIM (WS-IDPRC))                
094300       IF WS-IDPRC-LEN > ZEROES                                           
094400         MOVE FUNCTION TRIM (WS-IDPRC)                                    
094500                                 TO W-MIN-IDPRC (1:WS-IDPRC-LEN)          
094600                                    W-MAX-IDPRC (1:WS-IDPRC-LEN)          
094700       END-IF                                                             
094800     END-IF                                                               
094900     .                                                                    
095000                                                                          
095100 BB-CONTROL-RFS  SECTION.                                                 
095200                                                                          
095300     MOVE 'BB-CONTROL-RFS    '   TO WS-CURRENT-SECTION                    
095400                                                                          
095500     IF REQU-DARFSDAT-KEY = SPACES OR ALL '+'                             
095600       CONTINUE                                                           
095700     ELSE                                                                 
095800       IF FUNCTION TEST-FORMATTED-DATETIME                                
095900                        ('YYYY-MM-DD', REQU-DARFSDAT-KEY) = 0             
096000         MOVE REQU-DARFSDAT-KEY (1:4)                                     
096100                                 TO WS-DARFS-YYYY                         
096200         MOVE REQU-DARFSDAT-KEY (6:2)                                     
096300                                 TO WS-DARFS-MM                           
096400         MOVE REQU-DARFSDAT-KEY (9:2)                                     
096500                                 TO WS-DARFS-DD                           
096600         MOVE 9999               TO WS-DARFS-HHMM                         
096700                                                                          
096800         MOVE WS-DARFS           TO W-Q3K1KY-MAX-DARFS                    
096900       ELSE                                                               
097000         MOVE NEJ                TO NYCKLAR-SW                            
097100         MOVE '023'              TO RESP-IDMSG-ERROR                      
097200         MOVE 'DARFSDAT'         TO RESP-IDELMT-ERROR                     
097300       END-IF                                                             
097400     END-IF                                                               
097500     .                                                                    
097600                                                                          
097700 BC-CONTROL-USER SECTION.                                                 
097800                                                                          
097900     MOVE 'BC-CONTROL-USER   '   TO WS-CURRENT-SECTION                    
098000                                                                          
098100     IF REQU-IDINVER = '002'                                              
098200       PERFORM BCA-NEW-CONTROL-USER                                       
098300     ELSE                                                                 
098400       PERFORM BCB-OLD-CONTROL-USER                                       
098500     END-IF                                                               
098600     .                                                                    
098700                                                                          
098800 BCA-NEW-CONTROL-USER SECTION.                                            
098900                                                                          
099000     MOVE 'BCA-NEW-CONTROL-USER' TO WS-CURRENT-SECTION                    
099100                                                                          
099200     MOVE NEJ                    TO PRE-PRINT-SW                          
099300                                                                          
099400     IF REQU-FLPREPRINT = JA OR YES                                       
099500       MOVE JA                   TO PRE-PRINT-SW                          
099600*      Set the IDTRANS to A138 even though it was trigered                
099700*      from web with L138. This is to mimic "printing" as                 
099800*      if it was done from handheld and follow same rules                 
099900*      like no default case creation etc.                                 
100000       MOVE 'A138'               TO REQU-IDTRANS                          
100100*      Move * to identify preprint lists in doc retrieval                 
100200       MOVE '*'                  TO REQU-IDUSER (8:1)                     
100300                                                                          
100400       IF REQU-IDANSTNR = ALL '+' OR ZERO                                 
100500         CONTINUE                                                         
100600       ELSE                                                               
100700         MOVE NEJ                TO NYCKLAR-SW                            
100800         MOVE '023'              TO RESP-IDMSG-ERROR                      
100900         MOVE WRONG-IDANSTNR     TO RESP-IDELMT-ERROR                     
101000       END-IF                                                             
101100                                                                          
101200       IF NYCKLAR-OK                                                      
101300         IF REQU-IDQUEUENR = ALL '+' OR ZERO                              
101400           MOVE NEJ              TO NYCKLAR-SW                            
101500           MOVE '026'            TO RESP-IDMSG-ERROR                      
101600           MOVE 'IDQUEUENR'      TO RESP-IDELMT-ERROR                     
101700         ELSE                                                             
101800           IF REQU-IDQUEUENR NUMERIC AND                                  
101900              REQU-IDQUEUENR > ZERO AND                                   
102000              REQU-IDQUEUENR <= 999                                       
102100             STRING '99' REQU-IDQUEUENR                                   
102200                       DELIMITED BY SIZE                                  
102300                               INTO WS-IDUSER-2                           
102400           ELSE                                                           
102500             MOVE NEJ            TO NYCKLAR-SW                            
102600             MOVE '023'          TO RESP-IDMSG-ERROR                      
102700             MOVE 'IDQUEUENR'    TO RESP-IDELMT-ERROR                     
102800           END-IF                                                         
102900         END-IF                                                           
103000       END-IF                                                             
103100     ELSE                                                                 
103200       MOVE NEJ                  TO PRE-PRINT-SW                          
103300       IF REQU-IDANSTNR = ALL '+' OR ZERO                                 
103400         MOVE NEJ                TO NYCKLAR-SW                            
103500         MOVE '026'              TO RESP-IDMSG-ERROR                      
103600         MOVE WRONG-IDANSTNR     TO RESP-IDELMT-ERROR                     
103700       ELSE                                                               
103800         IF REQU-IDANSTNR NUMERIC AND                                     
103900            REQU-IDANSTNR > ZERO  AND                                     
104000*           REQU-IDANSTNR < 99000                                         
104100            (REQU-IDANSTNR < 99000 OR                                     
104200             REQU-IDANSTNR = 99999)                                       
104300           MOVE REQU-IDANSTNR    TO WS-IDUSER-2                           
104400*          Following IF block is to support gradual rollout of            
104500*          pre print queue number functionality. Until then, 99999        
104600*          user is considered as pre-print.                               
104700           IF REQU-IDANSTNR = 99999                                       
104800             IF SUB-KDTRANS = 'WLA134  '                                  
104900               MOVE NEJ          TO NYCKLAR-SW                            
105000               MOVE '023'        TO RESP-IDMSG-ERROR                      
105100               MOVE WRONG-IDANSTNR                                        
105200                                 TO RESP-IDELMT-ERROR                     
105300             ELSE                                                         
105400               MOVE JA           TO PRE-PRINT-SW                          
105500               MOVE 'A138'       TO REQU-IDTRANS                          
105600*      Move * to identify preprint lists in doc retrieval                 
105700               MOVE '*'          TO REQU-IDUSER (8:1)                     
105800             END-IF                                                       
105900           END-IF                                                         
106000         ELSE                                                             
106100           MOVE NEJ              TO NYCKLAR-SW                            
106200           MOVE '023'            TO RESP-IDMSG-ERROR                      
106300           MOVE WRONG-IDANSTNR   TO RESP-IDELMT-ERROR                     
106400         END-IF                                                           
106500       END-IF                                                             
106600     END-IF                                                               
106700     .                                                                    
106800                                                                          
106900 BCB-OLD-CONTROL-USER SECTION.                                            
107000                                                                          
107100     MOVE 'BCB-OLD-CONTROL-USER' TO WS-CURRENT-SECTION                    
107200                                                                          
107300     MOVE NEJ                    TO PRE-PRINT-SW                          
107400     IF REQU-IDANSTNR = ALL '+'                                           
107500       MOVE NEJ                  TO NYCKLAR-SW                            
107600       MOVE '026'                TO RESP-IDMSG-ERROR                      
107700       MOVE WRONG-IDANSTNR       TO RESP-IDELMT-ERROR                     
107800     ELSE                                                                 
107900       IF REQU-IDANSTNR NUMERIC AND                                       
108000          REQU-IDANSTNR > ZEROES                                          
108100         MOVE REQU-IDANSTNR      TO WS-IDUSER-2                           
108200         IF REQU-IDANSTNR = 99999                                         
108300           IF SUB-KDTRANS = 'WLA134  '                                    
108400             MOVE NEJ            TO NYCKLAR-SW                            
108500             MOVE '023'          TO RESP-IDMSG-ERROR                      
108600             MOVE WRONG-IDANSTNR TO RESP-IDELMT-ERROR                     
108700           ELSE                                                           
108800             MOVE JA             TO PRE-PRINT-SW                          
108900*            Set the IDTRANS to A138 even though it was triggered         
109000*            from web with L138. This is to mimic "printing" as           
109100*            if it was done from handheld and follow same rules           
109200*            like no default case creation etc.                           
109300             MOVE 'A138'         TO REQU-IDTRANS                          
109400*            Move * to identify preprint lists in doc retrieval           
109500             MOVE '*'            TO REQU-IDUSER (8:1)                     
109600           END-IF                                                         
109700         END-IF                                                           
109800       ELSE                                                               
109900         MOVE NEJ                TO NYCKLAR-SW                            
110000         MOVE '023'              TO RESP-IDMSG-ERROR                      
110100         MOVE WRONG-IDANSTNR     TO RESP-IDELMT-ERROR                     
110200       END-IF                                                             
110300     END-IF                                                               
110400     .                                                                    
110500                                                                          
110600 C-VISA-ORDER SECTION.                                                    
110700     MOVE 'C-VISA-ORDER    '     TO WS-CURRENT-SECTION                    
110800                                                                          
110900     MOVE 1                      TO WS-ANTPLK                             
111000     MOVE 0                      TO WS-KVORDER                            
111100                                    WS-KVRADER                            
111200                                                                          
111300     PERFORM CA-READ-NEXT-ORDERDEL                                        
111400     IF SEGMENT-FINNS                                                     
111500       MOVE SEQK-IDPRC           TO W-4448-IDPRC                          
111600       PERFORM CB-READ-PRC-KANAL                                          
111700       IF SEGMENT-FINNS                                                   
111800         MOVE ZERO               TO IX1                                   
111900         PERFORM UNTIL IX1 >= MAX-ANTAL-BILD-RADER                        
112000                    OR SEGMENT-SAKNAS                                     
112100           ADD 1                 TO IX1                                   
112200           PERFORM CC-MOVE-TO-SCREEN                                      
112300           PERFORM CA-READ-NEXT-ORDERDEL                                  
112400         END-PERFORM                                                      
112500         MOVE IX1                TO RESP-KVRADER-MAX1                     
112600         IF REQU-QUERY                                                    
112700           IF SEGMENT-FINNS                                               
112800             MOVE TOO-MANY-LINES TO RESP-IDMSG-INFO                       
112900           END-IF                                                         
113000         END-IF                                                           
113100       ELSE                                                               
113200         IF RESP-IDMSG-INFO = SPACES                                      
113300           MOVE DATA-ITEM-MISSING                                         
113400                                 TO RESP-IDMSG-INFO                       
113500           MOVE WRONG-IDPRC      TO RESP-IDELMT-ERROR                     
113600         END-IF                                                           
113700       END-IF                                                             
113800     ELSE                                                                 
113900       IF RESP-IDMSG-INFO = SPACES                                        
114000         MOVE DATA-ITEM-MISSING  TO RESP-IDMSG-INFO                       
114100         MOVE WRONG-ORDPART      TO RESP-IDELMT-ERROR                     
114200       END-IF                                                             
114300     END-IF                                                               
114400     .                                                                    
114500                                                                          
114600 CA-READ-NEXT-ORDERDEL SECTION.                                           
114700     MOVE 'CA-READ-NEXT-ORD'     TO WS-CURRENT-SECTION                    
114800                                                                          
114900     MOVE NEJ                    TO READ-SW                               
115000                                                                          
115100     PERFORM UNTIL READ-OK                                                
115200       PERFORM IMS-19-GN-WDQ3K1KY                                         
115300       IF SEGMENT-SAKNAS OR END-OF-DATA                                   
115400         MOVE JA                 TO READ-SW                               
115500       END-IF                                                             
115600       IF SEGMENT-FINNS                                                   
115700         MOVE SEQK-IDORDER       TO W-Q301-MIN-IDORDER                    
115800                                    W-Q301-MAX-IDORDER                    
115900         IF SEQK-IDORDER NOT = W-IDORDER                                  
116000           MOVE SEQK-IDORDER     TO W-IDORDER                             
116100           PERFORM IMS-03-GU-WDQ201                                       
116200         END-IF                                                           
116300         IF SEGMENT-FINNS                                                 
116400           IF OHUV-FLKLAR = JA                                            
116500             MOVE JA             TO READ-SW                               
116600           END-IF                                                         
116700         END-IF                                                           
116800         IF NOT DCS-CDC                                                   
116900           PERFORM CAA-READ-CROSS-DOCK                                    
117000         END-IF                                                           
117100       END-IF                                                             
117200     END-PERFORM                                                          
117300     .                                                                    
117400                                                                          
117500 CAA-READ-CROSS-DOCK SECTION.                                             
117600     MOVE 'CAA-READ-CROSS-DOCK'  TO WS-CURRENT-SECTION                    
117700                                                                          
117800     MOVE '11'                   TO W-Q301-MIN-IDDC                       
117900                                    W-Q301-MAX-IDDC                       
118000     MOVE ZERO                   TO W-Q301-MIN-IDPRODNR                   
118100                                    W-Q301-MIN-IDPLKLST                   
118200     MOVE 9999999                TO W-Q301-MAX-IDPRODNR                   
118300     MOVE 999                    TO W-Q301-MAX-IDPLKLST                   
118400                                                                          
118500     PERFORM IMS-18-GU-WDQ301                                             
118600                                                                          
118700     IF WDQ3-R-STATUS-CODE = SPACE                                        
118800       MOVE 'CD'                 TO WS-KDCROSS                            
118900     ELSE                                                                 
119000       MOVE SPACE                TO WS-KDCROSS                            
119100     END-IF                                                               
119200     .                                                                    
119300                                                                          
119400 CB-READ-PRC-KANAL SECTION.                                               
119500     MOVE 'CB-READ-PRC-KANAL'    TO WS-CURRENT-SECTION                    
119600                                                                          
119700     MOVE REQU-IDDC-KEY          TO W-4447-IDDC                           
119800                                                                          
119900     PERFORM IMS-07-GU-WDGX4448                                           
120000                                                                          
120100     IF SEGMENT-FINNS                                                     
120200       MOVE XXKH-4448-KVORDER    TO WS-PRC-KVORDER                        
120300       MOVE XXKH-4448-KVRADER    TO WS-PRC-KVRADER                        
120400     END-IF                                                               
120500     .                                                                    
120600                                                                          
120700 CC-MOVE-TO-SCREEN SECTION.                                               
120800     MOVE 'CC-MOVE-TO-SCREEN'    TO WS-CURRENT-SECTION                    
120900                                                                          
121000     MOVE SEQK-DARFS             TO WS-ORQA-ODEL-DARFS                    
121100     MOVE OHUV-IDKUNDNR          TO TEST-IDKUNDNR                         
121200                                    WS-TEST-IDKUNDNR                      
121300     MOVE OHUV-IDDISTR           TO TEST-IDDISTR                          
121400                                    WS-TEST-IDDISTR                       
121500     IF TEST-IDKUNDNR-PREV NOT =  TEST-IDKUNDNR  OR                       
121600        TEST-IDDISTR-PREV  NOT =  TEST-IDDISTR                            
121700       MOVE TEST-IDKUNDNR        TO TEST-IDKUNDNR-PREV                    
121800       MOVE TEST-IDDISTR         TO TEST-IDDISTR-PREV                     
121900       PERFORM CCA-DIST-KUND-LDC                                          
122000     END-IF                                                               
122100                                                                          
122200     IF WS-ODEL-YYMMDD > WS-DATUM                                         
122300     AND (OHUV-IDSYSTEM      = 'LDC ' OR 'ECOM')                          
122400       MOVE 001                  TO WORK-KDCALL                           
122500       IF DCS-CHINA                                                       
122600         MOVE REQU-IDDC-KEY      TO WORK-IDDC                             
122700       ELSE                                                               
122800         MOVE '11'               TO WORK-IDDC                             
122900       END-IF                                                             
123000       MOVE WS-DATUM             TO WORK-TIAAMMDD-FOM                     
123100       MOVE WS-ODEL-YYMMDD       TO WORK-TIAAMMDD-TOM                     
123200       CALL WORKDAY           USING WORK-KDCALL                           
123300                                    WORK-DATE-AREA                        
123400                                    WORK-KDSVAR                           
123500                                                                          
123600       IF WORK-KDSVAR-FEL                                                 
123700         MOVE 'FEL FRÅN WORKDAY I S02-SECTION.'                           
123800                                 TO ERROR-TEXT                            
123900         CALL FELLOG                                                      
124000       END-IF                                                             
124100     ELSE                                                                 
124200       MOVE 000                  TO WORK-KVWORKD                          
124300     END-IF                                                               
124400                                                                          
124500     IF (DCS-NDC                             AND                          
124600         WORK-KVWORKD > GMT-KVDAGAR-CDC + 1  AND                          
124700         (OHUV-IDSYSTEM      = 'LDC ' OR 'ECOM'))                         
124800     OR (DCS-SDC                             AND                          
124900         WORK-KVWORKD > GMT-KVDAGAR-SDC + 1  AND                          
125000         (OHUV-IDSYSTEM      = 'LDC ' OR 'ECOM'))                         
125100     OR (DCS-CDC                             AND                          
125200         WORK-KVWORKD > GMT-KVDAGAR-CDC + 1  AND                          
125300         (OHUV-IDSYSTEM      = 'LDC ' OR 'ECOM'))                         
125400       COMPUTE IX1 = IX1 - 1                                              
125500     ELSE                                                                 
125600       MOVE NEJ                  TO RESP-FLORDDEL (IX1)                   
125700       MOVE SEQK-IDPRC           TO RESP-IDPRC-RAD(IX1)                   
125800                                                                          
125900       MOVE WS-KDCROSS           TO RESP-KDCROSS  (IX1)                   
126000       MOVE OHUV-IDDEPT          TO RESP-IDDEPT   (IX1)                   
126100                                                                          
126200       MOVE OHUV-IDDISTR         TO RESP-IDDISTR  (IX1)                   
126300       MOVE OHUV-IDKUNDNR        TO RESP-IDKUNDNR (IX1)                   
126400       MOVE OHUV-IDKUNDRF (3:5)  TO RESP-IDORDNR5 (IX1)                   
126500       INSPECT RESP-IDORDNR5 (IX1) REPLACING LEADING ZERO BY SPACE        
126600                                                                          
126700       MOVE OHUV-KDORDKL         TO RESP-KDORDKL  (IX1)                   
126800       MOVE SEQK-DARFS (3:6)     TO RESP-TIRFSDAT (IX1)                   
126900       MOVE SEQK-DARFS (9:4)     TO RESP-TIRFSTID (IX1)                   
127000                                                                          
127100       MOVE SEQK-KVRADER         TO RESP-KVORDRAD (IX1)                   
127200       MOVE REQU-KDMATT          TO WS-KDMATT                             
127300                                                                          
127400       IF US-MATT                                                         
127500         COMPUTE RESP-VKORDNTO (IX1) = SEQK-VKORDNTO                      
127600                           * CONV-KG-TO-LB                                
127700         COMPUTE RESP-VLORDNTO (IX1) = SEQK-VLORDNTO                      
127800                           * CONV-M3-TO-FT3                               
127900       ELSE                                                               
128000         MOVE SEQK-VKORDNTO      TO RESP-VKORDNTO (IX1)                   
128100         MOVE SEQK-VLORDNTO      TO RESP-VLORDNTO (IX1)                   
128200       END-IF                                                             
128300       MOVE SEQK-IDTRP           TO RESP-IDTRP (IX1)                      
128400                                                                          
128500       MOVE WS-ANTPLK            TO RESP-KVPLOCK (IX1)                    
128600       ADD 1                     TO WS-KVORDER                            
128700       ADD SEQK-KVRADER          TO WS-KVRADER                            
128800       IF WS-KVORDER >= WS-PRC-KVORDER                                    
128900          OR                                                              
129000          WS-KVRADER > WS-PRC-KVRADER                                     
129100         MOVE 0                  TO WS-KVORDER                            
129200         MOVE 0                  TO WS-KVRADER                            
129300         ADD 1                   TO WS-ANTPLK                             
129400       END-IF                                                             
129500                                                                          
129600       IF SEQK-IDPRODNR > ZERO                                            
129700         MOVE SEQK-IDPRODNR      TO RESP-IDPRODNR (IX1)                   
129800                                    REQU-IDPRODNR (IX1)                   
129900         MOVE SEQK-IDPLKLST      TO RESP-IDPLKLST (IX1)                   
130000                                    REQU-IDPLKLST (IX1)                   
130100       ELSE                                                               
130200*THESE ARE HIDDEN FIELDS IN PULS WEB MUST BE GREATER THAN ZERO.           
130300         MOVE 654321             TO RESP-IDPRODNR (IX1)                   
130400         MOVE 654321             TO REQU-IDPRODNR (IX1)                   
130500         MOVE IX1                TO RESP-IDPLKLST (IX1)                   
130600         MOVE IX1                TO REQU-IDPLKLST (IX1)                   
130700       END-IF                                                             
130800     END-IF                                                               
130900     .                                                                    
131000                                                                          
131100 CCA-DIST-KUND-LDC SECTION.                                               
131200     MOVE 'CCA-DIST-KUND-LDC'    TO WS-CURRENT-SECTION                    
131300                                                                          
131400     MOVE  TEST-IDDISTR          TO W-WDB2-IDDISTR                        
131500     MOVE  TEST-IDKUNDNR         TO W-WDB2-IDKUNDNR                       
131600                                                                          
131700     PERFORM IMS-04-GU-WDB201                                             
131800                                                                          
131900*    SÄTT DEFAULTVÄRDEN OM WDB2 SAKNAS                                    
132000                                                                          
132100     IF SEGMENT-SAKNAS                                                    
132200       MOVE NEJ                  TO GMT-FLLDCKND                          
132300     END-IF                                                               
132400     .                                                                    
132500                                                                          
132600 G-CHECK-INPUT SECTION.                                                   
132700     MOVE 'G-CHECK-INPUT   '     TO WS-CURRENT-SECTION                    
132800                                                                          
132900     MOVE JA                     TO PLOCKSATS-SW                          
133000     MOVE ZERO                   TO TAB-PREL-IX                           
133100                                                                          
133200     PERFORM GA-LAGRA-ORDERDELAR                                          
133300     IF TAB-PREL-IX = ZERO                                                
133400       IF RESP-IDMSG-ERROR = SPACES                                       
133500         MOVE NO-LINES-SELECTED  TO RESP-IDMSG-ERROR                      
133600         MOVE SPACES             TO RESP-IDELMT-ERROR                     
133700       END-IF                                                             
133800       MOVE NEJ                  TO PLOCKSATS-SW                          
133900     ELSE                                                                 
134000       MOVE TAB-PREL-IX          TO TAB-PREL-MAX                          
134100     END-IF                                                               
134200     .                                                                    
134300                                                                          
134400 GA-LAGRA-ORDERDELAR SECTION.                                             
134500     MOVE 'GA-LAGRA-ORDERDELAR'  TO WS-CURRENT-SECTION                    
134600                                                                          
134700*LK  MOVE JA                     TO KDMETOD-SW                            
134800     MOVE 1                      TO IX1                                   
134900     MOVE ZERO                   TO WS-PLKSATS-KVRADER                    
135000                                                                          
135100     PERFORM UNTIL IX1 > REQU-KVRADER-MAX1 OR                             
135200                   PLOCKSATS-FEL                                          
135300       IF REQU-FLORDDEL (IX1) = JA                                        
135400         IF REQU-IDPRODNR (IX1) NUMERIC AND                               
135500            REQU-IDPLKLST (IX1) NUMERIC                                   
135600           MOVE LOW-VALUES       TO WS-ORDDEL                             
135700*LK        PERFORM GAA-CONTROL-KDMETOD                                    
135800           IF PLOCKSATS-OK                                                
135900             PERFORM GAB-CONTROL-KDODELSTA                                
136000           END-IF                                                         
136100           IF PLOCKSATS-OK                                                
136200             PERFORM GAC-CHECK-PRCBAS-PRCVAR                              
136300           END-IF                                                         
136400           IF PLOCKSATS-OK                                                
136500             IF WS-ORDDEL NOT = LOW-VALUES                                
136600               IF WS-ODEL-IDORDER NOT = W-IDORDER                         
136700                 MOVE WS-ODEL-IDORDER                                     
136800                                 TO W-IDORDER                             
136900                 PERFORM IMS-03-GU-WDQ201                                 
137000               END-IF                                                     
137100               IF SEGMENT-FINNS                                           
137200                 IF OHUV-FLKLAR = JA                                      
137300                   ADD 1         TO TAB-PREL-IX                           
137400                   IF TAB-PREL-IX <= MAX-ANTAL-ORDERDELAR                 
137500                     MOVE WS-ORDDEL                                       
137600                                 TO TAB-PREL-ORDDEL (TAB-PREL-IX)         
137700                   END-IF                                                 
137800                 ELSE                                                     
137900                   MOVE NEJ      TO PLOCKSATS-SW                          
138000                   MOVE ERR-ORDER-NOT-COMPLETE                            
138100                                 TO RESP-IDMSG-ERROR                      
138200                 END-IF                                                   
138300               ELSE                                                       
138400                 MOVE NEJ        TO PLOCKSATS-SW                          
138500                 MOVE ERR-ORDER-MISSING                                   
138600                                 TO RESP-IDMSG-ERROR                      
138700               END-IF                                                     
138800             END-IF                                                       
138900           END-IF                                                         
139000         ELSE                                                             
139100           MOVE NEJ              TO PLOCKSATS-SW                          
139200           MOVE ERR-IS-INVALID   TO RESP-IDMSG-ERROR                      
139300           MOVE 'IDPRODNR'       TO RESP-IDELMT-ERROR                     
139400         END-IF                                                           
139500       END-IF                                                             
139600       ADD 1                     TO IX1                                   
139700       IF TAB-PREL-IX > MAX-ANTAL-ORDERDELAR                              
139800         MOVE NEJ                TO PLOCKSATS-SW                          
139900         MOVE TOO-MANY-LINES     TO RESP-IDMSG-ERROR                      
140000         MOVE SPACES             TO RESP-IDELMT-ERROR                     
140100       END-IF                                                             
140200     END-PERFORM                                                          
140300     .                                                                    
140400                                                                          
140500 GAA-CONTROL-KDMETOD SECTION.                                             
140600                                                                          
140700     MOVE 'GAA-CONTROL-KDMETOD'  TO WS-CURRENT-SECTION                    
140800                                                                          
140900     MOVE REQU-IDPRODNR (IX1)    TO W-IDPRODNR-WDE6                       
141000     PERFORM IMS-17-GU-WDE601                                             
141100     IF SEGMENT-FINNS                                                     
141200       IF REQU-IDTRANS = 'L138' OR 'A138'                                 
141300*        NEW PACKING METOD                                                
141400         IF VORD-KDMETOD NOT = +3                                         
141500           MOVE NEJ              TO KDMETOD-SW                            
141600           MOVE NEJ              TO PLOCKSATS-SW                          
141700           MOVE WRONG-ORDER-SCREEN                                        
141800                                 TO RESP-IDMSG-ERROR                      
141900           MOVE WRONG-IDPRODNR   TO RESP-IDELMT-ERROR                     
142000         END-IF                                                           
142100       ELSE                                                               
142200*        OLD PACKING METOD                                                
142300         IF VORD-KDMETOD = +3                                             
142400           MOVE NEJ              TO KDMETOD-SW                            
142500           MOVE NEJ              TO PLOCKSATS-SW                          
142600           MOVE WRONG-ORDER-SCREEN                                        
142700                                 TO RESP-IDMSG-ERROR                      
142800           MOVE WRONG-IDPRODNR   TO RESP-IDELMT-ERROR                     
142900         END-IF                                                           
143000       END-IF                                                             
143100     END-IF                                                               
143200     .                                                                    
143300                                                                          
143400 GAB-CONTROL-KDODELSTA SECTION.                                           
143500                                                                          
143600     MOVE 'GAB-CONTROL-KDODELSTA'                                         
143700                                 TO WS-CURRENT-SECTION                    
143800                                                                          
143900     MOVE REQU-IDPRODNR (IX1)    TO W-Q3DSEQ-IDPRODNR                     
144000     MOVE REQU-IDPLKLST (IX1)    TO W-Q3DSEQ-IDPLKLST                     
144100                                                                          
144200     PERFORM IMS-05-GU-WDQ3DSEQ-WLORQA01                                  
144300     IF SEGMENT-FINNS                                                     
144400       IF ORQA-ODEL-IDDC NOT = REQU-IDDC-KEY                              
144500         PERFORM IMS-06-GN-WDQ3DSEQ-WLORQA01                              
144600*TILLÄGG PGA AV ATT DET FINNS "LÖSA" WDQ301 DVS ORENSADE WDQ301           
144700*SOM HAR SAMMA PRODNR SOM EN NY ORDER.                                    
144800*RESNINGSFEL? ELLER PGA FELUPPDATERING AV WDQ301 TIDIGARE?                
144900       END-IF                                                             
145000     END-IF                                                               
145100                                                                          
145200     IF SEGMENT-FINNS                                                     
145300       IF ORQA-ODEL-KDODELSTA = 'R'                                       
145400         IF ORQA-ODEL-IDDC = REQU-IDDC-KEY                                
145500           MOVE ORQA-ODEL-IDORDER                                         
145600                                 TO WS-ODEL-IDORDER                       
145700           MOVE ORQA-ODEL-IDDC   TO WS-ODEL-IDDC                          
145800           MOVE ORQA-ODEL-IDPRODNR                                        
145900                                 TO WS-ODEL-IDPRODNR                      
146000           MOVE ORQA-ODEL-IDPLKLST                                        
146100                                 TO WS-ODEL-IDPLKLST                      
146200           ADD ORQA-ODEL-KVRADER TO WS-PLKSATS-KVRADER                    
146300           IF WS-PLKSATS-KVRADER > MAX-PLKSATS-RADER                      
146400             MOVE NEJ            TO PLOCKSATS-SW                          
146500             MOVE TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
146600             MOVE SPACES         TO RESP-IDELMT-ERROR                     
146700           END-IF                                                         
146800         END-IF                                                           
146900       ELSE                                                               
147000         MOVE ORDER-PRINTED      TO RESP-IDMSG-ERROR                      
147100         MOVE NEJ                TO PLOCKSATS-SW                          
147200       END-IF                                                             
147300     ELSE                                                                 
147400       MOVE NEJ                  TO PLOCKSATS-SW                          
147500       MOVE DATA-ITEM-MISSING    TO RESP-IDMSG-ERROR                      
147600       MOVE WRONG-ORDPART        TO RESP-IDELMT-ERROR                     
147700     END-IF                                                               
147800     .                                                                    
147900                                                                          
148000 GAC-CHECK-PRCBAS-PRCVAR SECTION.                                         
148100     MOVE 'GAC-CHECK-PRCBAS-PRCVAR '                                      
148200                                 TO WS-CURRENT-SECTION                    
148300**** BLOCK SO YOU CAN NOT CHOOSE SAME ORDERS                              
148400                                                                          
148500     MOVE IX1                    TO COMPARE-IX                            
148600     ADD +1                      TO COMPARE-IX                            
148700                                                                          
148800     PERFORM UNTIL COMPARE-IX > REQU-KVRADER-MAX1                         
148900       IF REQU-FLORDDEL (COMPARE-IX) = JA                                 
149000         IF REQU-IDPRODNR (IX1) = REQU-IDPRODNR (COMPARE-IX)              
149100           MOVE NEJ              TO PLOCKSATS-SW                          
149200           MOVE WRONG-ORDPART    TO RESP-IDELMT-ERROR                     
149300           MOVE INVALID-COMBINATION-OF-DATA                               
149400                                 TO RESP-IDMSG-ERROR                      
149500         END-IF                                                           
149600       END-IF                                                             
149700       ADD +1                    TO COMPARE-IX                            
149800     END-PERFORM                                                          
149900     .                                                                    
150000                                                                          
150100                                                                          
150200 H-UPPDATERA SECTION.                                                     
150300     MOVE 'H-UPPDATERA     '     TO WS-CURRENT-SECTION                    
150400                                                                          
150500     PERFORM HA-UPPDATERA-ORDERDELAR                                      
150600     IF PLOCKSATS-OK                                                      
150700       PERFORM HB-LAGRA-ETIKETTER-O-UNDERLAG                              
150800       PERFORM HC-REDIGERA-SKRIV-ETIKETTER                                
150900       PERFORM HD-REDIGERA-SKRIV-UNDERLAG                                 
151000       PERFORM HE-UPPDATERA-ORDERBASER                                    
151100       IF DCS-INDIA                                                       
151200         PERFORM HG-PRINT-MRP-LABELS                                      
151300       END-IF                                                             
151400                                                                          
151500       IF SUB-KDTRANS(1:6) NOT = 'WLA134'                                 
151600         IF PRE-PRINT-YES                                                 
151700           MOVE SPACES           TO RESP-IDLIST                           
151800         ELSE                                                             
151900           MOVE WS-IDLIST        TO RESP-IDLIST                           
152000         END-IF                                                           
152100       END-IF                                                             
152200       PERFORM HF-RENSA-ORDERDELAR                                        
152300     END-IF                                                               
152400     .                                                                    
152500                                                                          
152600 HG-PRINT-MRP-LABELS SECTION.                                             
152700     MOVE 'HG-PRINT-MRP-LABELS'  TO WS-CURRENT-SECTION                    
152800                                                                          
152900     INITIALIZE PLK-SUMM-WPLKSUMM                                         
153000     MOVE RESP-PL-IDAFPRCD-TOT   TO PLK-SUMM-IDAFPRCD-TOT                 
153100     MOVE RESP-PL-IDLOPNR-ORD-TOT                                         
153200                                 TO PLK-SUMM-IDLOPNR-ORD-TOT              
153300     MOVE RESP-PL-IDLOPNR-PL-TOT TO PLK-SUMM-IDLOPNR-PL-TOT               
153400     MOVE RESP-PL-IDPRC-TOT      TO PLK-SUMM-IDPRC-TOT                    
153500     MOVE RESP-PL-KVRADER-MAX1   TO PLK-SUMM-KVRADER                      
153600     MOVE RESP-PL-IDTRPTNR       TO PLK-SUMM-IDTRPTNR                     
153700     MOVE RESP-PL-TIRFSDAT       TO PLK-SUMM-TIRFSDAT                     
153800     MOVE RESP-PL-TIRFSTID       TO PLK-SUMM-TIRFSTID                     
153900                                                                          
154000     INITIALIZE LABL-W612LABL                                             
154100     MOVE REQU-IDUSER            TO LABL-REQU-IDUSER                      
154200     MOVE REQU-KDPGMACT          TO LABL-REQU-KDPGMACT                    
154300                                                                          
154400     MOVE REQU-IDDC-KEY          TO LABL-IDDC                             
154500     MOVE WS-IDLIST              TO LABL-IDLIST                           
154600     MOVE RESP-PL-KVRADER-MAX1   TO LABL-KVRADER                          
154700                                                                          
154800     MOVE 1                      TO LABL-IX                               
154900     PERFORM                                                              
155000     VARYING IX1 FROM 1 BY 1                                              
155100       UNTIL (IX1 > RESP-PL-KVRADER-MAX1) OR                              
155200             (IX1 > 500)                                                  
155300       MOVE RESP-PL-IDARTNR (IX1)                                         
155400                                 TO LABL-IDARTNR (LABL-IX)                
155500       MOVE RESP-PL-KVAVBART(IX1)                                         
155600                                 TO LABL-KVANTAL (LABL-IX)                
155700       MOVE RESP-PL-KDARTURS(IX1)                                         
155800                                 TO LABL-KDARTURS(LABL-IX)                
155900       MOVE 1                    TO LABL-KVQPACK (LABL-IX)                
156000       COMPUTE LABL-IX = LABL-IX + 1                                      
156100     END-PERFORM                                                          
156200                                                                          
156300     CALL W612MRP             USING LABL-W612LABL                         
156400                                    PLK-SUMM-WPLKSUMM                     
156500                                    LABL-REQU-WZ01REQU                    
156600                                    PRNT-PCB                              
156700                                    PRNT-PCB2                             
156800                                    MRP-WDD3-PCB                          
156900                                    MRP-WDK6-PCB                          
157000                                    MRP-WDG2-PCB                          
157100                                    MRP-WDC3-PCB                          
157200                                    MRP-WDJ1-PCB                          
157300                                    MRP-WDJ4-PCB                          
157400                                    MRP-WDD3A-PCB                         
157500                                                                          
157600     .                                                                    
157700                                                                          
157800 HA-UPPDATERA-ORDERDELAR    SECTION.                                      
157900     MOVE 'HA-UPPDATERA-ORDERDELAR'                                       
158000                                 TO WS-CURRENT-SECTION                    
158100                                                                          
158200     MOVE ZERO                   TO TAB-IX                                
158300                                                                          
158400     PERFORM                                                              
158500     VARYING TAB-PREL-IX FROM 1 BY 1                                      
158600       UNTIL TAB-PREL-IX > TAB-PREL-MAX                                   
158700       MOVE TAB-PREL-IDORDER (TAB-PREL-IX)                                
158800                                 TO W-Q301KY-IDORDER                      
158900       MOVE TAB-PREL-IDDC (TAB-PREL-IX)                                   
159000                                 TO W-Q301KY-IDDC                         
159100       MOVE TAB-PREL-IDPRODNR (TAB-PREL-IX)                               
159200                                 TO W-Q301KY-IDPRODNR                     
159300       MOVE TAB-PREL-IDPLKLST (TAB-PREL-IX)                               
159400                                 TO W-Q301KY-IDPLKLST                     
159500                                                                          
159600       PERFORM IMS-09-GHU-WDQ301                                          
159700       IF SEGMENT-FINNS AND ODEL-KDODELSTA = 'R'                          
159800         ADD 1                   TO TAB-IX                                
159900         MOVE W-WDQ301KY-X       TO TAB-ORDDEL (TAB-IX)                   
160000         IF TAB-IX = +1                                                   
160100           MOVE ODEL-IDPRC       TO WS-ODEL-IDPRC-FIRST                   
160200           MOVE ODEL-IDPRC       TO 3420-IDPRC                            
160300           PERFORM HAA-HAMTA-PLOCKSATSNR                                  
160400         END-IF                                                           
160500         MOVE WS-IDUSER          TO ODEL-IDUSER                           
160600         MOVE WS-IDBORD          TO ODEL-IDBORD                           
160700         MOVE WS-DATUM-LOK       TO ODEL-DAUTSKR                          
160800         MOVE 20                 TO ODEL-DAUTSKR (1:2)                    
160900         MOVE WS-TIHHMMSS-LOK    TO ODEL-TIUTSTID                         
161000         MOVE 'U'                TO ODEL-KDODELSTA                        
161100         MOVE WS-ODEL-IDPRC-FIRST                                         
161200                                 TO ODEL-IDPRCPLK                         
161300         MOVE TAB-PREL-IDPRODNR (TAB-PREL-IX)                             
161400                                 TO W-IDPRODNR-WDE6                       
161500         MOVE WS-IDLOPNR-PL      TO ODEL-IDLOTNR-PLK                      
161600                                                                          
161700         PERFORM IMS-10-REPL-WDQ301                                       
161800       END-IF                                                             
161900     END-PERFORM                                                          
162000                                                                          
162100     IF TAB-IX > 0                                                        
162200       MOVE TAB-IX               TO TAB-IX-MAX                            
162300       IF PRE-PRINT-YES                                                   
162400*     using existing message for testing purpose                          
162500*        MOVE PU-ON-PRE-PRINT-QUEUE                                       
162600         MOVE PICKING-UNIT-ON-PRINTER-QUEUE                               
162700                                 TO RESP-IDMSG-INFO                       
162800       ELSE                                                               
162900         MOVE PICKING-UNIT-ON-PRINTER-QUEUE                               
163000                                 TO RESP-IDMSG-INFO                       
163100       END-IF                                                             
163200     ELSE                                                                 
163300       MOVE INF-NOTHING-PRINTED  TO RESP-IDMSG-INFO                       
163400       MOVE NEJ                  TO PLOCKSATS-SW                          
163500     END-IF                                                               
163600     .                                                                    
163700                                                                          
163800 HAA-HAMTA-PLOCKSATSNR SECTION.                                           
163900     MOVE 'HAA-HAMTA-PLOCKSATSNR'                                         
164000                                 TO WS-CURRENT-SECTION                    
164100                                                                          
164200     MOVE REQU-IDDC-KEY          TO W-4403-IDDC                           
164300     MOVE WS-ODEL-IDPRC-FIRST    TO W-4404-IDPRC                          
164400     PERFORM IMS-11-GHU-WDGX4404                                          
164500     IF SEGMENT-FINNS                                                     
164600       IF 4404-IDLOPNR-PL = 999                                           
164700         MOVE 1                  TO 4404-IDLOPNR-PL                       
164800       ELSE                                                               
164900         ADD  1                  TO 4404-IDLOPNR-PL                       
165000       END-IF                                                             
165100       PERFORM HAAA-CHECK-CURR-IDLOTNR-PLK                                
165200       PERFORM IMS-12-REPL-WDGX4404                                       
165300     ELSE                                                                 
165400       MOVE WS-ODEL-IDPRC-FIRST  TO 4404-IDPRC                            
165500       MOVE 1                    TO 4404-IDLOPNR-PL                       
165600       PERFORM HAAA-CHECK-CURR-IDLOTNR-PLK                                
165700       PERFORM IMS-17-ISRT-WDGX4404                                       
165800     END-IF                                                               
165900     MOVE 4404-IDLOPNR-PL        TO WS-IDLOPNR-PL                         
166000     .                                                                    
166100                                                                          
166200 HAAA-CHECK-CURR-IDLOTNR-PLK SECTION.                                     
166300     MOVE 'HAAA-CHECK-CURR-IDLOTNR-PLK'                                   
166400                                 TO WS-CURRENT-SECTION                    
166500* Temp check needed for some time so we don't create duplicate            
166600* picking lists with same PRC/LOTNR-PLK combination after                 
166700* switching to new sequence based on prc and not prcgrp like befor        
166800                                                                          
166900     MOVE W-4403-IDDC            TO W-Q3H1KY-MIN-IDDC                     
167000                                    W-Q3H1KY-MAX-IDDC                     
167100     MOVE W-4404-IDPRC           TO W-Q3H1KY-MIN-IDPRCPLK                 
167200                                    W-Q3H1KY-MAX-IDPRCPLK                 
167300     MOVE 4404-IDLOPNR-PL        TO W-Q3H1KY-MIN-IDLOTNR-PLK              
167400                                    W-Q3H1KY-MAX-IDLOTNR-PLK              
167500     PERFORM                                                              
167600       UNTIL SEGMENT-SAKNAS                                               
167700       PERFORM IMS-20-GU-WDQ3H1KY                                         
167800       IF SEGMENT-FINNS                                                   
167900         ADD 1                   TO W-Q3H1KY-MIN-IDLOTNR-PLK              
168000                                    W-Q3H1KY-MAX-IDLOTNR-PLK              
168100                                    4404-IDLOPNR-PL                       
168200       END-IF                                                             
168300     END-PERFORM                                                          
168400     .                                                                    
168500                                                                          
168600 HB-LAGRA-ETIKETTER-O-UNDERLAG SECTION.                                   
168700     MOVE 'HB-LAGRA-ETIKETTER-O-UNDERLAG'                                 
168800                                 TO WS-CURRENT-SECTION                    
168900                                                                          
169000     MOVE WS-IDUSER              TO 3410-IDUSER                           
169100     MOVE WS-IDBORD              TO 3410-IDBORD                           
169200     MOVE WS-DATUM-LOK           TO 3410-TIAAMMDD                         
169300     MOVE WS-TIHHMMSS-LOK        TO 3410-TIHHMMSS                         
169400     MOVE WS-DATUM-AADDD         TO 3410-TIAADDD                          
169500     MOVE WS-IDLOPNR-PL          TO 3410-IDLOPNR                          
169600     MOVE TAB-IDPRODNR(1)        TO 3410-IDPRODNR-KEY                     
169700     MOVE TAB-IDPLKLST(1)        TO 3410-IDPLKLST-KEY                     
169800     MOVE REQU-IDTRANS           TO 3410-IDTRANS                          
169900                                                                          
170000     PERFORM                                                              
170100     VARYING TAB-IX FROM 1 BY 1                                           
170200       UNTIL TAB-IX > TAB-IX-MAX                                          
170300       MOVE TAB-IDORDER (TAB-IX) TO 3410-IDORDER (TAB-IX)                 
170400       MOVE TAB-IDDC    (TAB-IX) TO 3410-IDDC (TAB-IX)                    
170500       MOVE TAB-IDPRODNR(TAB-IX) TO 3410-IDPRODNR(TAB-IX)                 
170600       MOVE TAB-IDPLKLST(TAB-IX) TO 3410-IDPLKLST(TAB-IX)                 
170700     END-PERFORM                                                          
170800     MOVE TAB-IX-MAX             TO 3410-IX                               
170900                                                                          
171000     CALL WL013410 USING 3410-WL013410  MSG-PCB  0693-PCB                 
171100          3410-ALT-PCB  3410-4397-PCB 3410-4003-PCB                       
171200          3410-4007-PCB         3410-4017-PCB                             
171300          3410-4448-PCB         3410-4453-PCB                             
171400          3410-4512-PCB                                                   
171500          3410-WDM2-PCB                                                   
171600          3410-4536-PCB         3410-WDA5-PCB                             
171700          3410-WDA6-PCB         3410-WDA6B-PCB                            
171800          3410-WDB2-PCB         3410-WDB6-PCB                             
171900          3410-WDD3-PCB         3410-WDD5-PCB                             
172000          3410-WDG6-PCB         3410-WDK6-PCB                             
172100          3410-WDK7-PCB         3410-WDK9-PCB                             
172200          3410-WDQ1-PCB                                                   
172300          3410-WDQ2-PCB         3410-WDQ2C-PCB                            
172400          3410-WDQ3-PCB         3410-WDQ4-PCB                             
172500          3410-4541-PCB         3410-2203-PCB                             
172600          3410-ROLL-WDP4A-PCB                                             
172700          3410-ORQICSQ-PCB      3410-WLLOGA-PCB                           
172800          3410-DEAV-ARTM-PCB    3410-DEAV-ARTS-PCB                        
172900          3410-DEAV-WDB6-PCB                                              
173000          3410-DEAV-WDB2-PCB    3410-DEAV-WDL7-PCB                        
173100          3410-DEAV-WDK7-PCB    3410-DEAV-WDR2-PCB                        
173200          3410-DEAV-WDR5-PCB    3410-DEAV-WDC1-PCB                        
173300          3410-RANS-XXKM-PCB    3410-RANS-ARTM-PCB                        
173400          3410-RANS-ARTS-PCB                                              
173500          3410-AREG-WDK6-PCB                                              
173600          3410-AREG-WDK7-PCB                                              
173700          3410-SPAR-WDF8-PCB    3410-SPAR-WDF8A-PCB                       
173800          3410-SPAR-WDK6-PCB    3410-PRQU-WDG2-PCB                        
173900          3410-PRQU-WDC7-PCB    3410-PRQU-WDK6-PCB                        
174000          3410-PRNO-3107-PCB                                              
174100          3410-PLATS-DM-PCB     3410-PLATS-DN-PCB                         
174200          3410-PLATS-DP-PCB                                               
174300          3410-PLATS-DO-PCB     3410-PLATS-WDE6C-PCB                      
174400          3410-PLATS-GMTC-PCB   3410-PLATS-WDB6-PCB                       
174500          3410-KOM-WDP8-PCB                                               
174600          .                                                               
174700 HC-REDIGERA-SKRIV-ETIKETTER   SECTION.                                   
174800     MOVE 'HC-REDIGERA-SKRIV-ETIKETTER'                                   
174900                                 TO WS-CURRENT-SECTION                    
175000                                                                          
175100*                                                                         
175200*    ÄNDRINGEN ATT ANVÄNDA 3410-...-KEY VÄRDEN ÄR GJORT                   
175300*    FÖR ATT FÅ RÄTT NYCKLAR TILL RÖTTERNA I HÄNDELSEBASERNA              
175400*    ÄNDRINGEN ÄR KOPPLAD TILL ÄNDRING I PROGRAM                          
175500*    WL013410 SECTION S05- SKAPA-PLOCKSATS-PU                             
175600*                                                                         
175700                                                                          
175800     MOVE 3410-IDPRODNR-KEY      TO WS-IDPRODNR                           
175900     MOVE 3410-IDPLKLST-KEY      TO WS-IDPLKLST                           
176000     MOVE WS-IDLIST              TO 3420-IDLIST                           
176100     MOVE REQU-IDDC-KEY          TO 3420-IDDC                             
176200     MOVE 3410-IDPRODNR-KEY      TO 3420-IDPRODNR-KEY                     
176300     MOVE 3410-IDPLKLST-KEY      TO 3420-IDPLKLST-KEY                     
176400     MOVE REQU-IDUSER            TO 3420-IDUSER                           
176500     MOVE REQU-IDTRANS           TO 3420-IDTRANS                          
176600                                                                          
176700     IF REQU-IDTRANS = 'L138' OR 'A138'                                   
176800       MOVE +1                   TO ORDER-IX                              
176900       PERFORM UNTIL ORDER-IX > TAB-IX-MAX                                
177000         MOVE 3410-IDPRODNR(ORDER-IX) TO 3420-IDPRODNR(ORDER-IX)          
177100         MOVE 3410-IDPLKLST(ORDER-IX) TO 3420-IDPLKLST(ORDER-IX)          
177200         MOVE 3410-IDORDER (ORDER-IX) TO 3420-IDORDER (ORDER-IX)          
177300         MOVE 3410-IDTRPTNR(ORDER-IX) TO 3420-IDTRPTNR(ORDER-IX)          
177400         MOVE 3410-ADFLGEO (ORDER-IX) TO 3420-ADFLGEO (ORDER-IX)          
177500         MOVE 3410-ADFLOMR (ORDER-IX) TO 3420-ADFLOMR (ORDER-IX)          
177600         MOVE 3410-ADRUTNIV(ORDER-IX) TO 3420-ADRUTNIV(ORDER-IX)          
177700         MOVE 3410-TIRFSDAT(ORDER-IX) TO 3420-TIRFSDAT(ORDER-IX)          
177800         MOVE 3410-TIRFSTID(ORDER-IX) TO 3420-TIRFSTID(ORDER-IX)          
177900         MOVE 3410-IDDC-CROSS(ORDER-IX) TO                                
178000                                    3420-IDDC-CROSS(ORDER-IX)             
178100         IF 3410-VKORDNTO(ORDER-IX) NUMERIC                               
178200           MOVE 3410-VKORDNTO(ORDER-IX) TO 3420-VKORDNTO(ORDER-IX)        
178300         ELSE                                                             
178400           MOVE +000000.0        TO 3420-VKORDNTO(ORDER-IX)               
178500         END-IF                                                           
178600         ADD +1                  TO ORDER-IX                              
178700       END-PERFORM                                                        
178800     END-IF                                                               
178900                                                                          
179000     CALL WL013420 USING 3420-WL013420  RESP-PL-WL0134O2                  
179100                         PRNT-PCB SYNQ-PCB                                
179200                         3420-4003-PCB  3420-WDQ3D-PCB                    
179300                         3420-WDF5-PCB  3420-WDB6-PCB                     
179400                         3420-WDE6-PCB  3420-WDQ2-PCB                     
179500                         3420-4535-PCB  SYNQ-ATAB-PCB                     
179600                         3420-WDQ3-PCB                                    
179700     .                                                                    
179800                                                                          
179900 HD-REDIGERA-SKRIV-UNDERLAG    SECTION.                                   
180000     MOVE 'HD-REDIGERA-SKRIV-UNDERLAG'                                    
180100                                 TO WS-CURRENT-SECTION                    
180200*                                                                         
180300*    ÄNDRINGEN ATT ANVÄNDA 3410-...-KEY VÄRDEN ÄR GJORT                   
180400*    FÖR ATT FÅ RÄTT NYCKLAR TILL RÖTTERNA I HÄNDELSEBASERNA              
180500*    ÄNDRINGEN ÄR KOPPLAD TILL ÄNDRING I PROGRAM                          
180600*    WL013410 SECTION S05- SKAPA-PLOCKSATS-PU                             
180700*                                                                         
180800     MOVE 3410-IDPRODNR-KEY      TO WS-IDPRODNR                           
180900     MOVE 3410-IDPLKLST-KEY      TO WS-IDPLKLST                           
181000     MOVE WS-IDLIST              TO 3430-IDLIST                           
181100     MOVE REQU-IDDC-KEY          TO 3430-IDDC                             
181200     MOVE 3410-IDPRODNR-KEY      TO 3430-IDPRODNR-KEY                     
181300     MOVE 3410-IDPLKLST-KEY      TO 3430-IDPLKLST-KEY                     
181400     MOVE REQU-IDUSER            TO 3430-IDUSER                           
181500     MOVE REQU-IDTRANS           TO 3430-IDTRANS                          
181600*L138                                                                     
181700     IF REQU-IDTRANS = 'L138' OR 'A138'                                   
181800       MOVE +1                   TO ORDER-IX                              
181900       PERFORM UNTIL ORDER-IX > TAB-IX-MAX                                
182000         MOVE 3420-IDPRODNR(ORDER-IX) TO 3430-IDPRODNR(ORDER-IX)          
182100         MOVE 3420-IDPLKLST(ORDER-IX) TO 3430-IDPLKLST(ORDER-IX)          
182200         MOVE 3420-ADFLGEO(ORDER-IX)  TO 3430-ADFLGEO(ORDER-IX)           
182300         MOVE 3420-ADFLOMR(ORDER-IX)  TO 3430-ADFLOMR(ORDER-IX)           
182400         MOVE 3420-ADRUTNIV(ORDER-IX) TO 3430-ADRUTNIV(ORDER-IX)          
182500         MOVE 3420-VKORDNTO(ORDER-IX) TO 3430-VKORDNTO(ORDER-IX)          
182600         MOVE 3420-IDORDER (ORDER-IX) TO 3430-IDORDER (ORDER-IX)          
182700         MOVE 3420-IDTRPTNR(ORDER-IX) TO 3430-IDTRPTNR(ORDER-IX)          
182800         MOVE 3420-IDDC-CROSS(ORDER-IX)                                   
182900                                 TO 3430-IDDC-CROSS(ORDER-IX)             
183000         ADD +1                  TO ORDER-IX                              
183100       END-PERFORM                                                        
183200     END-IF                                                               
183300                                                                          
183400     CALL WL013430 USING 3430-WL013430       PRNT-PCB                     
183500                         3430-4007-PCB       3430-4447-PCB                
183600                         3430-4535-PCB       3430-4732-PCB                
183700                         3430-WDB2-PCB       3430-WDQ2-PCB                
183800                         3430-WDQ3-PCB       3430-WDB6-PCB                
183900                         3430-WDE6-PCB       3430-WDI2-PCB                
184000                         3430-WDF5-PCB       3430-WDK5-PCB                
184100     .                                                                    
184200                                                                          
184300 HE-UPPDATERA-ORDERBASER       SECTION.                                   
184400     MOVE 'HE-UPPDATERA-ORDERBASER'                                       
184500                                 TO WS-CURRENT-SECTION                    
184600*                                                                         
184700*    ÄNDRINGEN ATT ANVÄNDA 3410-...-KEY VÄRDEN ÄR GJORT                   
184800*    FÖR ATT FÅ RÄTT NYCKLAR TILL RÖTTERNA I HÄNDELSEBASERNA              
184900*    ÄNDRINGEN ÄR KOPPLAD TILL ÄNDRING I PROGRAM                          
185000*    WL013410 SECTION S05- SKAPA-PLOCKSATS-PU                             
185100*                                                                         
185200     MOVE REQU-IDDC-KEY          TO 3440-IDDC                             
185300     MOVE 3410-IDPRODNR-KEY      TO 3440-IDPRODNR-KEY                     
185400     MOVE 3410-IDPLKLST-KEY      TO 3440-IDPLKLST-KEY                     
185500     MOVE WS-DATUM-LOK           TO 3440-TIAAMMDD                         
185600     MOVE WS-TIHHMMSS-LOK        TO 3440-TIHHMMSS                         
185700     MOVE REQU-IDTRANS           TO 3440-IDTRANS                          
185800                                                                          
185900*L138                                                                     
186000     IF REQU-IDTRANS = 'L138' OR 'A138'                                   
186100       MOVE +1                   TO ORDER-IX                              
186200       PERFORM UNTIL ORDER-IX > TAB-IX-MAX                                
186300         MOVE 3430-ADFLGEO(ORDER-IX)  TO 3440-ADFLGEO(ORDER-IX)           
186400         MOVE 3430-ADFLOMR(ORDER-IX)  TO 3440-ADFLOMR(ORDER-IX)           
186500         MOVE 3430-ADRUTNIV(ORDER-IX) TO 3440-ADRUTNIV(ORDER-IX)          
186600         MOVE 3430-IDTRPTNR(ORDER-IX) TO 3440-IDTRPTNR(ORDER-IX)          
186700         MOVE 3430-VKORDNTO(ORDER-IX) TO 3440-VKORDNTO(ORDER-IX)          
186800         MOVE 3430-IDPRODNR(ORDER-IX) TO 3440-IDPRODNR(ORDER-IX)          
186900         MOVE 3430-IDDC-CROSS(ORDER-IX)                                   
187000                                 TO 3440-IDDC-CROSS(ORDER-IX)             
187100         ADD +1                  TO ORDER-IX                              
187200       END-PERFORM                                                        
187300     END-IF                                                               
187400                                                                          
187500     CALL WL013440 USING 3440-WL013440                                    
187600                         3440-4007-PCB       3440-4017-PCB                
187700                         3440-4447-PCB       3440-4487-PCB                
187800                         3440-4726-PCB       3440-WDE4-PCB                
187900                         3440-WDE6-PCB       3440-WDG6-PCB                
188000                         3440-WDQ2-PCB       3440-WDQ2C-PCB               
188100                         3440-WDQ3-PCB       3440-WDB6-PCB                
188200                         3440-WDK5-PCB                                    
188300                         DNOT-ORQP-PCB                                    
188400                         DNOT-ORQP2-PCB                                   
188500                         DNOT-ORQP3-PCB                                   
188600                         DNOT-4013-PCB                                    
188700                         DNOT-BENA-PCB                                    
188800     .                                                                    
188900                                                                          
189000 HF-RENSA-ORDERDELAR           SECTION.                                   
189100     MOVE 'HF-RENSA-ORDERDELAR'  TO WS-CURRENT-SECTION                    
189200                                                                          
189300     MOVE 3430-IDPRODNR-KEY      TO W-4003-IDPRODNR                       
189400                                    W-4007-IDPRODNR                       
189500     MOVE 3430-IDPLKLST-KEY      TO W-4003-IDPLKLST                       
189600                                    W-4007-IDPLKLST                       
189700     PERFORM IMS-13-GHU-WDGX4003                                          
189800     IF SEGMENT-FINNS                                                     
189900       PERFORM IMS-14-DLET-WDGX4003                                       
190000     END-IF                                                               
190100     PERFORM IMS-15-GHU-WDGX4007                                          
190200     IF SEGMENT-FINNS                                                     
190300       PERFORM IMS-16-DLET-WDGX4007                                       
190400     END-IF                                                               
190500     .                                                                    
190600                                                                          
190700*    --- DISPATCHER SECTIONS                                              
190800 S10-FETCH-REQUEST-ARGUMENT SECTION.                                      
190900     MOVE 'S10-FETCH-REQUEST-ARG '  TO WS-CURRENT-SECTION                 
191000                                                                          
191100     MOVE SPACES                            TO REQU-AREA                  
191200                                                                          
191300     MOVE 'GETARG'                          TO SUB-KDFUNC                 
191400     MOVE 'CARPARTS.LDC.ORDERQUEUEPRC2'     TO SUB-ADDISPABS              
191500                                                                          
191600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
191700                                                                          
191800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
191900                                                                          
192000     IF SUB-KDRC > 0                                                      
192100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
192200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
192300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
192400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
192500     END-IF                                                               
192600     .                                                                    
192700                                                                          
192800 S11-RETURN-RESPONSE SECTION.                                             
192900     MOVE 'S11-RETURN-RESPONS'   TO WS-CURRENT-SECTION                    
193000                                                                          
193100     MOVE 'RETURN'               TO SUB-KDFUNC                            
193200     COMPUTE RESP-DATA-KVDLEN     = LENGTH OF RESP-WL0134O1               
193300*                                 - (LENGTH OF RESP-TABELLRAD             
193400*                                 * (MAX-PLKSATS-RADER                    
193500*                                 - RESP-KVRADER-MAX1))                   
193600     MOVE RESP-WL0134O1 (1 : RESP-DATA-KVDLEN)                            
193700                                 TO RESP-DATA-AREA                        
193800                                      (1 : RESP-DATA-KVDLEN)              
193900     COMPUTE SUB-KVDLEN           = LENGTH OF RESP-WZ01RES2               
194000                                  + RESP-DATA-KVDLEN                      
194100     CALL WZ01SUB             USING SUB-CONTROL-AREA                      
194200                                    SUB-KVDLEN                            
194300                                    RESP-AREA                             
194400                                                                          
194500     IF SUB-KDRC > 0                                                      
194600       MOVE SUB-KDRC             TO KDRC-DISPLAY                          
194700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
194800             DELIMITED BY SIZE INTO ERROR-TEXT                            
194900       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
195000     END-IF                                                               
195100     .                                                                    
195200                                                                          
195300 S12-RETURN-RESPONSE SECTION.                                             
195400     MOVE 'S12-RETURN-RESPONS'   TO WS-CURRENT-SECTION                    
195500                                                                          
195600     MOVE 'RETURN'               TO SUB-KDFUNC                            
195700     COMPUTE RESP-DATA-KVDLEN     = LENGTH OF RESP-PL-WL0134O2            
195800*                                 - (LENGTH OF RESP-PL-TABELLRAD          
195900*                                 * (MAX-PLKSATS-RADER                    
196000*                                 - RESP-PL-KVRADER-MAX1))                
196100     MOVE RESP-PL-WL0134O2 (1 : RESP-DATA-KVDLEN)                         
196200                                 TO RESP-DATA-AREA                        
196300                                      (1 : RESP-DATA-KVDLEN)              
196400     COMPUTE SUB-KVDLEN           = LENGTH OF RESP-WZ01RES2               
196500                                  + RESP-DATA-KVDLEN                      
196600                                                                          
196700     CALL WZ01SUB             USING SUB-CONTROL-AREA                      
196800                                    SUB-KVDLEN                            
196900                                    RESP-AREA                             
197000                                                                          
197100     IF SUB-KDRC > 0                                                      
197200       MOVE SUB-KDRC             TO KDRC-DISPLAY                          
197300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
197400             DELIMITED BY SIZE INTO ERROR-TEXT                            
197500       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
197600     END-IF                                                               
197700     .                                                                    
197800                                                                          
197900 S20-INITIATE-RESPONSE SECTION.                                           
198000     MOVE ALL '+'                TO RESP-AREA                             
198100                                    RESP-WL0134O1                         
198200                                    RESP-PL-WL0134O2                      
198300     IF SUB-KDTRANS = 'WLA134'                                            
198400       MOVE LOW-VALUES           TO RESP-WL0134O1                         
198500     END-IF                                                               
198600     MOVE 001                    TO RESP-IDRESVER                         
198700     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
198800                                    RESP-IDMSG-INFO                       
198900                                    RESP-IDELMT-ERROR                     
199000     MOVE ZERO                   TO RESP-KVRADER-MAX1                     
199010     MOVE ZERO                   TO RESP-PL-KVRADER-MAX1                  
199100     .                                                                    
199200                                                                          
199300 S30-MSG-CONV SECTION.                                                    
199400     MOVE SPACES                 TO RESP-MESSAGES (1)                     
199500                                    RESP-MESSAGES (2)                     
199600     MOVE 1                      TO MSG-IX                                
199700*    REQUEST OK                                                           
199800     MOVE 200                    TO RESP-KDSTATUS-API                     
199900     IF RESP-IDMSG-INFO > SPACE                                           
200000       MOVE SPACES               TO MSG-CONV-AREA                         
200100       MOVE RESP-IDMSG-INFO      TO MSG-CONV-IDMSG-IN                     
200200       CALL WMSGCONV          USING MSG-CONV-AREA                         
200300       MOVE MSG-CONV-IDMSG-OUT   TO RESP-IDMSG   (MSG-IX)                 
200400       MOVE MSG-CONV-MESSAGE     TO RESP-MESSAGE (MSG-IX)                 
200500       ADD 1                     TO MSG-IX                                
200600     END-IF                                                               
200700     IF RESP-IDMSG-ERROR > SPACE                                          
200800*      BAD REQUEST                                                        
200900       MOVE 400                  TO RESP-KDSTATUS-API                     
201000       MOVE SPACES               TO MSG-CONV-AREA                         
201100       MOVE RESP-IDMSG-ERROR     TO MSG-CONV-IDMSG-IN                     
201200       MOVE RESP-IDELMT-ERROR    TO MSG-CONV-IDELMT                       
201300       CALL WMSGCONV          USING MSG-CONV-AREA                         
201400       MOVE MSG-CONV-IDMSG-OUT   TO RESP-IDMSG   (MSG-IX)                 
201500       MOVE MSG-CONV-MESSAGE     TO RESP-MESSAGE (MSG-IX)                 
201600     END-IF                                                               
201700     .                                                                    
201800                                                                          
201900 IMS-03-GU-WDQ201 SECTION.                                                
202000     MOVE 'IMS-03'    TO WS-CURRENT-IMS-SECTION                           
202100                                                                          
202200     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
202300          DELIMITED BY SIZE INTO SSA1                                     
202400     MOVE '  GE'           TO GODK-STATUSKODER                            
202500     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
202600     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
202700     PERFORM IMS-STATUSKONTROLL                                           
202800     .                                                                    
202900                                                                          
203000 IMS-04-GU-WDB201      SECTION.                                           
203100     MOVE 'IMS-04'     TO WS-CURRENT-IMS-SECTION                          
203200                                                                          
203300     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
203400          DELIMITED BY SIZE INTO SSA1                                     
203500     MOVE '  GE'              TO GODK-STATUSKODER                         
203600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
203700     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
203800     PERFORM IMS-STATUSKONTROLL                                           
203900     .                                                                    
204000                                                                          
204100 IMS-05-GU-WDQ3DSEQ-WLORQA01 SECTION.                                     
204200     MOVE 'IMS-05' TO WS-CURRENT-IMS-SECTION                              
204300                                                                          
204400     STRING 'WLORQA01(WDQ3DSEQ =' W-WDQ3DSEQ-X ')'                        
204500          DELIMITED BY SIZE INTO SSA1                                     
204600     MOVE '  GE'              TO GODK-STATUSKODER                         
204700     CALL CBLTDLI USING GU ORQD-PCB DLI-IO-ORQA SSA1                      
204800     MOVE ORQD-STATUS-CODE    TO STATUS-WS                                
204900     PERFORM IMS-STATUSKONTROLL                                           
205000     .                                                                    
205100                                                                          
205200 IMS-06-GN-WDQ3DSEQ-WLORQA01 SECTION.                                     
205300     MOVE 'IMS-06' TO WS-CURRENT-IMS-SECTION                              
205400                                                                          
205500     STRING 'WLORQA01(WDQ3DSEQ =' W-WDQ3DSEQ-X ')'                        
205600          DELIMITED BY SIZE INTO SSA1                                     
205700     MOVE '  GE'              TO GODK-STATUSKODER                         
205800     CALL CBLTDLI USING GN ORQD-PCB DLI-IO-ORQA SSA1                      
205900     MOVE ORQD-STATUS-CODE    TO STATUS-WS                                
206000     PERFORM IMS-STATUSKONTROLL                                           
206100     .                                                                    
206200                                                                          
206300 IMS-07-GU-WDGX4448  SECTION.                                             
206400     MOVE 'IMS-07' TO WS-CURRENT-IMS-SECTION                              
206500                                                                          
206600     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
206700          DELIMITED BY SIZE INTO SSA1                                     
206800     STRING 'WLXXKH11(WDGXKEY  =' W-4448-IDPRC-X ')'                      
206900          DELIMITED BY SIZE INTO SSA2                                     
207000     MOVE '  GE'              TO GODK-STATUSKODER                         
207100     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-WDGX4448 SSA1 SSA2             
207200     MOVE XXKH-STATUS-CODE    TO STATUS-WS                                
207300     PERFORM IMS-STATUSKONTROLL                                           
207400     .                                                                    
207500                                                                          
207600 IMS-08-GU-WDB601    SECTION.                                             
207700     MOVE 'IMS-08'     TO WS-CURRENT-IMS-SECTION                          
207800                                                                          
207900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
208000          DELIMITED BY SIZE INTO SSA1                                     
208100     MOVE '  GE' TO GODK-STATUSKODER                                      
208200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
208300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
208400     PERFORM IMS-STATUSKONTROLL                                           
208500     .                                                                    
208600                                                                          
208700                                                                          
208800 IMS-09-GHU-WDQ301  SECTION.                                              
208900     MOVE 'IMS-09' TO WS-CURRENT-IMS-SECTION                              
209000                                                                          
209100     STRING 'WDQ301  (WDQ301KY =' W-WDQ301KY-X ')'                        
209200          DELIMITED BY SIZE INTO SSA1                                     
209300     MOVE '  GE'              TO GODK-STATUSKODER                         
209400     CALL CBLTDLI USING GHU WDQ3-PCB DLI-IO-WDQ301 SSA1                   
209500     MOVE WDQ3-STATUS-CODE     TO STATUS-WS                               
209600     PERFORM IMS-STATUSKONTROLL                                           
209700     .                                                                    
209800                                                                          
209900 IMS-10-REPL-WDQ301       SECTION.                                        
210000     MOVE 'IMS-10' TO WS-CURRENT-IMS-SECTION                              
210100                                                                          
210200     MOVE '  '            TO GODK-STATUSKODER                             
210300     CALL CBLTDLI USING REPL WDQ3-PCB DLI-IO-WDQ301                       
210400     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
210500     PERFORM IMS-STATUSKONTROLL                                           
210600     .                                                                    
210700                                                                          
210800 IMS-11-GHU-WDGX4404 SECTION.                                             
210900     MOVE 'IMS-11' TO WS-CURRENT-SECTION                                  
211000                                                                          
211100     STRING 'WDR401  (WDGXKEY  =' W-4403-WDGX01KY-X ')'                   
211200          DELIMITED BY SIZE INTO SSA1                                     
211300     STRING 'WDGX4404(IDPRC    =' W-4404-IDPRC-X ')'                      
211400          DELIMITED BY SIZE INTO SSA2                                     
211500     MOVE '  GE'              TO GODK-STATUSKODER                         
211600     CALL CBLTDLI USING GHU 4403-PCB DLI-IO-WDGX4404 SSA1 SSA2            
211700     MOVE 4403-STATUS-CODE    TO STATUS-WS                                
211800     PERFORM IMS-STATUSKONTROLL                                           
211900     .                                                                    
212000                                                                          
212100 IMS-12-REPL-WDGX4404 SECTION.                                            
212200     MOVE 'IMS-12  ' TO WS-CURRENT-IMS-SECTION                            
212300                                                                          
212400     MOVE '    '           TO GODK-STATUSKODER                            
212500     CALL CBLTDLI USING REPL 4403-PCB DLI-IO-WDGX4404                     
212600     MOVE 4403-STATUS-CODE TO STATUS-WS                                   
212700     PERFORM IMS-STATUSKONTROLL                                           
212800     .                                                                    
212900                                                                          
213000 IMS-17-ISRT-WDGX4404 SECTION.                                            
213100     MOVE 'IMS-12  ' TO WS-CURRENT-IMS-SECTION                            
213200                                                                          
213300     STRING 'WDR401  (WDGXKEY  =' W-4403-WDGX01KY-X ')'                   
213400          DELIMITED BY SIZE INTO SSA1                                     
213500     MOVE   'WDGX4404 ' TO SSA2                                           
213600                                                                          
213700     MOVE '    '           TO GODK-STATUSKODER                            
213800     CALL CBLTDLI USING ISRT 4403-PCB DLI-IO-WDGX4404 SSA1 SSA2           
213900     MOVE 4403-STATUS-CODE TO STATUS-WS                                   
214000     PERFORM IMS-STATUSKONTROLL                                           
214100     .                                                                    
214200                                                                          
214300 IMS-13-GHU-WDGX4003   SECTION.                                           
214400     MOVE 'IMS-13'  TO WS-CURRENT-IMS-SECTION                             
214500                                                                          
214600     STRING 'WL400301(WDGXKEY  =' W-4003-IDHTYP-X ')'                     
214700          DELIMITED BY SIZE INTO SSA1                                     
214800     MOVE '  GE'              TO GODK-STATUSKODER                         
214900     CALL CBLTDLI USING GHU 4003-PCB DLI-IO-WDGX4003 SSA1                 
215000     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
215100     PERFORM IMS-STATUSKONTROLL                                           
215200     .                                                                    
215300                                                                          
215400 IMS-14-DLET-WDGX4003   SECTION.                                          
215500     MOVE 'IMS-14'  TO WS-CURRENT-IMS-SECTION                             
215600                                                                          
215700     MOVE '    '           TO GODK-STATUSKODER                            
215800     CALL CBLTDLI USING DLET 4003-PCB DLI-IO-WDGX4003                     
215900     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
216000     PERFORM IMS-STATUSKONTROLL                                           
216100     .                                                                    
216200 IMS-15-GHU-WDGX4007  SECTION.                                            
216300     MOVE 'IMS-15' TO WS-CURRENT-IMS-SECTION                              
216400                                                                          
216500     STRING 'WL400701(WDGXKEY  =' W-4007-IDHTYP-X ')'                     
216600          DELIMITED BY SIZE INTO SSA1                                     
216700     MOVE '  GE'              TO GODK-STATUSKODER                         
216800     CALL CBLTDLI USING GHU 4007-PCB DLI-IO-WDGX4007 SSA1                 
216900     MOVE 4007-STATUS-CODE    TO STATUS-WS                                
217000     PERFORM IMS-STATUSKONTROLL                                           
217100     .                                                                    
217200                                                                          
217300 IMS-16-DLET-WDGX4007  SECTION.                                           
217400     MOVE 'IMS-16' TO WS-CURRENT-IMS-SECTION                              
217500                                                                          
217600     MOVE '    '           TO GODK-STATUSKODER                            
217700     CALL CBLTDLI USING DLET 4007-PCB DLI-IO-WDGX4007                     
217800     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
217900     PERFORM IMS-STATUSKONTROLL                                           
218000     .                                                                    
218100                                                                          
218200 IMS-17-GU-WDE601 SECTION.                                                
218300     MOVE 'IMS-17'   TO WS-CURRENT-IMS-SECTION                            
218400                                                                          
218500     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
218600          DELIMITED BY SIZE INTO SSA1                                     
218700     MOVE '  GE'              TO GODK-STATUSKODER                         
218800     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
218900     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
219000     PERFORM IMS-STATUSKONTROLL                                           
219100     .                                                                    
219200                                                                          
219300 IMS-18-GU-WDQ301   SECTION.                                              
219400     MOVE 'IMS-18' TO WS-CURRENT-IMS-SECTION                              
219500                                                                          
219600     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN-X                        
219700                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
219800          DELIMITED BY SIZE INTO SSA1                                     
219900     MOVE '  GE'              TO GODK-STATUSKODER                         
220000     CALL CBLTDLI USING GU WDQ3-R-PCB DLI-IO-WDQ301 SSA1                  
220100*****MOVE WDQ3-STATUS-CODE    TO STATUS-WS                                
220200     PERFORM IMS-STATUSKONTROLL                                           
220300     .                                                                    
220400                                                                          
220500 IMS-19-GN-WDQ3K1KY   SECTION.                                            
220600     MOVE ' IMS-19-GN-WDQ3K1KY'  TO WS-CURRENT-IMS-SECTION                
220700                                                                          
220800     STRING 'WDQ3K1  (WDQ3K1KY>=' W-WDQ3K1KY-MIN-X                        
220900                    '&WDQ3K1KY<=' W-WDQ3K1KY-MAX-X                        
221000                    '&IDPRC   >=' W-MIN-IDPRC-X                           
221100                    '&IDPRC   <=' W-MAX-IDPRC-X ')'                       
221200             DELIMITED BY SIZE INTO SSA1                                  
221300     MOVE '  GBGE'               TO GODK-STATUSKODER                      
221400     CALL CBLTDLI USING GN WDQ3K-PCB DLI-IO-WDQ3K1 SSA1                   
221500     MOVE WDQ3K-STATUS-CODE      TO STATUS-WS                             
221600     PERFORM IMS-STATUSKONTROLL                                           
221700     .                                                                    
221800                                                                          
221900 IMS-20-GU-WDQ3H1KY   SECTION.                                            
222000     MOVE ' IMS-20-GU-WDQ3H1KY'  TO WS-CURRENT-IMS-SECTION                
222100                                                                          
222200     STRING 'WDQ3H1  (WDQ3H1KY>=' W-WDQ3H1KY-MIN-X                        
222300                    '&WDQ3H1KY<=' W-WDQ3H1KY-MAX-X ')'                    
222400             DELIMITED BY SIZE INTO SSA1                                  
222500     MOVE '  GE'                 TO GODK-STATUSKODER                      
222600     CALL CBLTDLI USING GU WDQ3H-PCB DLI-IO-WDQ3H1 SSA1                   
222700     MOVE WDQ3H-STATUS-CODE      TO STATUS-WS                             
222800     PERFORM IMS-STATUSKONTROLL                                           
222900     .                                                                    
223000                                                                          
223100 IMS-STATUSKONTROLL SECTION.                                              
223200                                                                          
223300     SET STATUS-IX TO 1                                                   
223400     SEARCH GODK-STATUS                                                   
223500       AT END CALL FELLOG                                                 
223600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
223700     END-SEARCH                                                           
223800     .                                                                    
223900     EJECT                                                                
