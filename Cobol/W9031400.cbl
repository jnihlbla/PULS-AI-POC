000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9031400.                                                
000300 AUTHOR.         GÖRAN KJELLSON                                           
000400 DATE-WRITTEN.   OCTOBER 2020                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        ORDER API LYNC&CO          .                                     
000900*                                                                         
001000*                                                                         
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W90314U                                             
001400*        MID:         W90314I1                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W90314O1                                            
001800*                                                                         
001900 ENVIRONMENT DIVISION.                                                    
002000 DATA DIVISION.                                                           
002100                                                                          
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400 77    IDPGM                     PIC X(8)    VALUE 'W9031400'.            
002500 77    CURRENT-SECTION           PIC X(16)   VALUE SPACE.                 
002600 77    CURRENT-IMS-SECTION       PIC X(16)   VALUE SPACE.                 
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77    ERROR-TEXT                PIC X(80) VALUE SPACE.                   
002900 77    FELTEXT                   PIC X(80) VALUE SPACE.                   
003000 77    KDRC-DISPLAY              PIC Z(5).                                
003100 77    RKOD-ABEND-NO-DUMP        PIC S9(4)   COMP VALUE +16.              
003200 77    RKOD-ABEND-WITH-DUMP      PIC S9(4)   COMP VALUE +1000.            
003300                                                                          
003400 77    JAA                       PIC X       VALUE 'J'.                   
003500 77    YES                       PIC X       VALUE 'Y'.                   
003600 77    NOO                       PIC X       VALUE 'N'.                   
003700 77    WS-TILOKDAT               PIC 9(6).                                
003800 77    DAGENS-DATUM              PIC 9(6)    VALUE ZERO.                  
003900                                                                          
004000 77    OK-SW                     PIC X       VALUE 'Y'.                   
004100   88  EVERYTHING-OK                         VALUE 'Y'.                   
004200   88  SOMETHING-WRONG                       VALUE 'N'.                   
004300                                                                          
004400 77    SW-REFILL                 PIC X       VALUE 'N'.                   
004500   88  REFILLORDER                           VALUE 'Y'.                   
004600   88  NOT-REFILL                            VALUE 'N'.                   
004700                                                                          
004800 77    CALL-ABEND-SW             PIC X       VALUE 'N'.                   
004900   88  CALL-ABEND                            VALUE 'Y'.                   
005000                                                                          
005100 77    PART-IX                   PIC 9(3)    VALUE ZERO.                  
005200 77    LINE-IX                   PIC 9(3)    VALUE ZERO.                  
005300 77    LINE-IX-MAX               PIC 9(3)    VALUE 5.                     
005400 77    RFS-IX                    PIC S9(4)   COMP VALUE +0.               
005500 77    RFS-IX-MAX                PIC S9(4)   COMP VALUE +4.               
005600 77    WS-KVDAGAR-RFS-DEF        PIC S9(3)   COMP-3.                      
005700 77    WS-CDC-SE                 PIC X(2)    VALUE '11'.                  
005800                                                                          
005900 77    WS-IDORDNR                PIC 9(7)    VALUE ZERO.                  
006000 77    WS-IDORDNR-SEQ            PIC 9(7)    VALUE ZERO.                  
006100 77    WS-KDORDKL                PIC 9(1)    VALUE ZERO.                  
006200 77    WS-IDDISTR-NUM            PIC 9(4)    VALUE ZERO.                  
006300 77    WS-IDKUNDNR-NUM           PIC 9(6)    VALUE ZERO.                  
006400 77    WS-IDDC-RECV              PIC x(2)    VALUE SPACES.                
006500 77    WS-IDDC-SEND              PIC x(2)    VALUE SPACES.                
006600 01    WS-PRARTNTO-NUM           PIC 9(7)V9(2).                           
006700 01    WS-PRARTNTO-RED           PIC 9(7).9(2).                           
006800 01    PRARTNTO-FILLER REDEFINES WS-PRARTNTO-RED.                         
006900       03  WS-PRARTNTO-ALFA      PIC X(10).                               
007000                                                                          
007100                                                                          
007200 01    WS-IDSYSTEM               PIC  X(4)   VALUE SPACE.                 
007300 01    WS-IDSYSTEM-LYNK          PIC  X(4)   VALUE 'LYNK'.                
007400 01    WS-IDSYSTEM-POLE          PIC  X(4)   VALUE 'POLE'.                
007500 01    WS-IDSYSTEM-ECOM          PIC  X(4)   VALUE 'ECOM'.                
007600 01    WS-IDSYSTEM-VOUI          PIC  X(4)   VALUE 'VOUI'.                
007700 01    WS-IDSYSTEM-TAD           PIC  X(4)   VALUE 'TAD '.                
007800 01    WS-IDSYSTEM-ACC           PIC  X(4)   VALUE 'ACC '.                
007900 01    WS-IDSYSTEM-APA           PIC  X(4)   VALUE 'APA '.                
008000 01    WS-IDSYSTEM-APB           PIC  X(4)   VALUE 'APB '.                
008100 01    WS-IDSYSTEM-APC           PIC  X(4)   VALUE 'APC '.                
008200 01    WS-IDSYSTEM-APD           PIC  X(4)   VALUE 'APD '.                
008300 01    WS-IDSYSTEM-APE           PIC  X(4)   VALUE 'APE '.                
008400 01    WS-IDSYSTEM-APF           PIC  X(4)   VALUE 'APF '.                
008500 01    WS-IDSYSTEM-APG           PIC  X(4)   VALUE 'APG '.                
008600 01    WS-IDSYSTEM-APH           PIC  X(4)   VALUE 'APH '.                
008700 01    WS-IDSYSTEM-API           PIC  X(4)   VALUE 'API '.                
008800 01    WS-IDSYSTEM-APJ           PIC  X(4)   VALUE 'APJ '.                
008900                                                                          
009000 01    W-BLANKS                  PIC  9(5)   VALUE ZERO.                  
009100 01    W-LENGTH                  PIC  9(5)   VALUE ZERO.                  
009200                                                                          
009300 01    W-IDARTNR-CHAR.                                                    
009400   03  W-IDARTNR-NUM             PIC  9(8)   VALUE ZERO.                  
009500                                                                          
009600                                                                          
009700 01    WS-TIREPDAT               PIC 9(6)    VALUE ZERO.                  
009800 01    FILLER REDEFINES WS-TIREPDAT.                                      
009900   03  WS-TIAA                   PIC 9(2).                                
010000   03  WS-TIMM                   PIC 9(2).                                
010100   03  WS-TIDD                   PIC 9(2).                                
010200                                                                          
010300 01    WS-TIREPDAT-X.                                                     
010400   03  WS-TISS-X                 PIC 9(2)    VALUE ZERO.                  
010500   03  WS-TIAA-X                 PIC 9(2)    VALUE ZERO.                  
010600   03  FILLER                    PIC X(1)    VALUE '-'.                   
010700   03  WS-TIMM-X                 PIC 9(2)    VALUE ZERO.                  
010800   03  FILLER                    PIC X(1)    VALUE '-'.                   
010900   03  WS-TIDD-X                 PIC 9(2)    VALUE ZERO.                  
011000                                                                          
011100 01    WS-TIREPDAT-8             PIC 9(8)    VALUE ZERO.                  
011200 01    FILLER REDEFINES WS-TIREPDAT-8.                                    
011300   03  WS-TISS-8                 PIC 9(2).                                
011400   03  WS-TIAA-8                 PIC 9(2).                                
011500   03  WS-TIMM-8                 PIC 9(2).                                
011600   03  WS-TIDD-8                 PIC 9(2).                                
011700                                                                          
011800 01    WS-TODAY-DATE-8           PIC 9(8)    VALUE ZERO.                  
011900                                                                          
012000 01  FILLER                  PIC X(16)   VALUE 'WORKAREA   '.             
012100*   -COPY WORKAREA                                                        
012200                                                                          
012300*      --- VALID IDDC CODES                                               
012400*                                                                         
012500*01   -COPY WWDC99                                                        
012600*01   --COPY WWDCKONS                                                     
012700                                                                          
012800 01  DYNAMISKA-SUBPROGRAM.                                                
012900   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
013000   03  ABEND                     PIC X(8)   VALUE 'ABEND   '.             
013100   03  WZ01SUB                   PIC X(8)   VALUE 'WZ01SUB '.             
013200   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
013300   03  WDATKONV                  PIC X(8)   VALUE 'WDATKONV'.             
013400   03  W009REDU                  PIC X(8)   VALUE 'W009REDU'.             
013500   03  WZ20DAYS                  PIC X(8)   VALUE 'WZ20DAYS'.             
013600   03  W411ORDN                  PIC X(8)   VALUE 'W411ORDN'.             
013700   03  W005INIT                  PIC X(8)   VALUE 'W005INIT'.             
013800   03  WZ01SEND                  PIC X(8)   VALUE 'WZ01SEND'.             
013900   03  WZ01AUTH                  PIC X(8)   VALUE 'WZ01AUTH'.             
014000   03  W006KOM                   PIC X(8)   VALUE 'W006KOM '.             
014100   03  WISOLAND                  PIC X(8)   VALUE 'WISOLAND'.             
014200   03  WORKDAY                   PIC X(8)   VALUE 'WORKDAY '.             
014300*                                                                         
014400 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
014500                                                                          
014600 01  FILLER REDEFINES TEST-IDDISTR.                                       
014700*    03 -COPY WWDIST79                                                    
014800     EJECT                                                                
014900*    COPYBOOK TO CHECK REFILL DISTRICT                                    
015000*01  -COPY WWDIST35                                                       
015100     EJECT                                                                
015200*    COPYBOOK TO SEARCH RECEVING DC FOR REFILL DISTRICT                   
015300*01  -COPY WWDIST57                                                       
015400     EJECT                                                                
015500 01  FILLER                      PIC X(16)  VALUE 'SUB-CONTROL'.          
015600                                                                          
015700*01  -COPY WZ01SUB                                                        
015800*                                                                         
015900 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH   '.         
016000     SKIP3                                                                
016100*01  -COPY WZ01AUTH                                                       
016200*                                                                         
016300*    --- PARAMETERS FOR WDATKONV                                          
016400*01  -COPY WDATAREA                                                       
016500                                                                          
016600*    --- PARAMETERS FOR W009REDU                                          
016700 01  WS-REDUIN                   PIC X(30)  VALUE SPACE.                  
016800 01  WS-REDUUT                   PIC X(30)  VALUE SPACE.                  
016900                                                                          
017000*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
017100 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
017200*01 -COPY WISOLAND                                                        
017300                                                                          
017400 01  WZ20DAYS                    PIC X(16)  VALUE 'WZ20DAYS'.             
017500     SKIP3                                                                
017600*    -COPY WZ20DAYS                                                       
017700                                                                          
017800 01 FILLER                       PIC X(16)  VALUE 'W411ORDN***'.          
017900*   -COPY W411ORDN                                                        
018000                                                                          
018100                                                                          
018200 01 FILLER                       PIC X(8) VALUE 'W005INIT'.               
018300*   -COPY WMSGINIT                                                        
018400                                                                          
018500                                                                          
018600*    --- AREAS FOR SUB MODULES W006KOM                                    
018700                                                                          
018800 01  FILLER                    PIC X(16) VALUE 'MSG-KOM-WMSGKOM '.        
018900*01  -COPY WMSGKOM                                                        
019000                                                                          
019100 01  FILLER                    PIC X(16) VALUE 'MSG-IO-AREA     '.        
019200*01  -COPY WMSGAREA                                                       
019300                                                                          
019400 01  FILLER                    PIC X(16) VALUE '4251-MID-AREA   '.        
019500*01  -COPY W4I25101 -PRE 4251-                                            
019600                                                                          
019700 01  FILLER                    PIC X(16) VALUE '4252-MID-AREA   '.        
019800 01  LIX                       PIC S9(4)  BINARY.                         
019900*01  -COPY W4I25201 -PRE 4252-                                            
020000                                                                          
020100                                                                          
020200******************************************************************        
020300*                                                                         
020400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
020500*                                                                         
020600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
020700                                                                          
020800 01  REQU-AREA.                                                           
020900*    03  -COPY WZ01REQ2                                                   
021000*    03  -COPY W90314I1                                                   
021100                                                                          
021200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
021300                                                                          
021400 01  RESP-AREA.                                                           
021500*    03  -COPY WZ01RESP                                                   
021600*    03  -COPY W90314O1                                                   
021700                                                                          
021800                                                                          
021900 01  HDR-AREA.                                                            
022000*    03  -COPY WZ01REQU -PRE MAIL-                                        
022100*    03  -COPY WZ04HDR                                                    
022200     EJECT                                                                
022300 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
022400 01  SEND-AREA.                                                           
022500*    03  -COPY WZ01SEND                                                   
022600     EJECT                                                                
022700 01  SEND-RAD.                                                            
022800   03  MAIL-RAD                PIC X(80)  VALUE SPACE.                    
022900                                                                          
023000                                                                          
023100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023200 01  DLI-KEYS.                                                            
023300                                                                          
023400     03  W-IDGMT-X.                                                       
023500         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
023600         05  W-IDKUNDNR          PIC S9(7) VALUE ZERO COMP-3.             
023700                                                                          
023800     03  W-WDF5BSEQ.                                                      
023900         05  W-SEQB-IDLEVART     PIC X(30)   VALUE LOW-VALUE.             
024000                                                                          
024100     03  W-IDARTNR-X.                                                     
024200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
024300                                                                          
024400     03  W-KDSEGKEY-X.                                                    
024500         05  W-KDSEGKEY          PIC X(01)   VALUE '1'.                   
024600                                                                          
024700     03  W-IDDC-X.                                                        
024800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
024900                                                                          
025000     03  W-IDDC-B6-X.                                                     
025100         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
025200                                                                          
025300     03  W-IDDC-REF-B6-X.                                                 
025400         05  W-IDDC-REF-B6       PIC X(2)    VALUE SPACE.                 
025500                                                                          
025600     03  W-WDGXKEY-X.                                                     
025700         05  W-IDHTYP            PIC X(4)    VALUE '4133'.                
025800         05  W-IDSYSMOT          PIC X(10)   VALUE 'LYNK   '.             
025900         05  FILLER              PIC X(16)   VALUE LOW-VALUE.             
026000                                                                          
026100     03 W-WDQ2C1KY-X.                                                     
026200        05  W-SEQC-IDDISTR       PIC S9(5)   VALUE +0 COMP-3.             
026300        05  W-SEQC-IDKUNDNR      PIC S9(7)   VALUE +0 COMP-3.             
026400        05  W-SEQC-IDKUNDRF.                                              
026500          07  W-SEQC-IDORDNR7    PIC 9(7)    VALUE ZERO.                  
026600          07  FILLER             PIC X(3)    VALUE SPACE.                 
026700                                                                          
026800     03  W-WDB501KY-X.                                                    
026900         05  W-IDDC-WDB5         PIC X(2)    VALUE SPACE.                 
027000         05  W-KDFRAKT-WDB5      PIC S9(3)   VALUE ZERO COMP-3.           
027100         05  W-IDDISTR-WDB5      PIC S9(5)   VALUE ZERO COMP-3.           
027200         05  W-IDKUNDNR-WDB5     PIC S9(7)   VALUE ZERO COMP-3.           
027300*                                                                         
027400     03  W-WDB501KY-DEF-X.                                                
027500         05  W-IDDC-WDB5-DEF     PIC X(2)    VALUE SPACE.                 
027600         05  W-KDFRAKT-WDB5-DEF  PIC S9(3)   VALUE ZERO COMP-3.           
027700         05  W-IDDISTR-WDB5-DEF  PIC S9(5)   VALUE ZERO COMP-3.           
027800         05  W-IDKUNDNR-WDB5-DEF PIC S9(7) VALUE +9999999 COMP-3.         
027900*                                                                         
028000     03  W-IDLANDX2              PIC X(2)    VALUE SPACE .                
028100     03  W-ADPOSTNR              PIC X(10)   VALUE SPACE.                 
028200     03  W-ADPOSTNR-DEF          PIC X(10)   VALUE '9999999999'.          
028300                                                                          
028400 01  MESSAGE-CODES.                                                       
028500     03  SYS-ERROR               PIC X(3)    VALUE '099'.                 
028600     03  OK-REQUEST              PIC X(3)    VALUE '200'.                 
028700     03  ORDER-CREATED           PIC X(3)    VALUE '201'.                 
028800     03  BAD-REQUEST             PIC X(3)    VALUE '400'.                 
028900     03  NOT-FOUND               PIC X(3)    VALUE '404'.                 
029000                                                                          
029100                                                                          
029200 01  STATUS-WS                   PIC XX.                                  
029300     88  SEGMENT-FOUND                       VALUE '  '.                  
029400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
029500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
029600     88  END-OF-DATA                         VALUE 'GB'.                  
029700                                                                          
029800 01  GOOD-STATUSCODES.                                                    
029900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030000                                                                          
030100 01  ALL-SSA.                                                             
030200     03 SSA1                     PIC X(80).                               
030300     03 SSA2                     PIC X(120).                              
030400     03 SSA3                     PIC X(120).                              
030500                                                                          
030600     EJECT                                                                
030700*    --- IMS FUNCTION CODES                                               
030800*01  -COPY W0003                                                          
030900                                                                          
031000     EJECT                                                                
031100 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDB201'.          
031200 01  DLI-IO-WDB201.                                                       
031300*    03  -COPY WDB201                                                     
031400                                                                          
031500                                                                          
031600 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDF501'.          
031700 01  DLI-IO-WDF501.                                                       
031800*    03  -COPY WDF501                                                     
031900                                                                          
032000                                                                          
032100 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK601'.          
032200 01  DLI-IO-WDK601.                                                       
032300*    03  -COPY WDK601                                                     
032400                                                                          
032500 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK611'.          
032600 01  DLI-IO-WDK611.                                                       
032700*    03  -COPY WDK611                                                     
032800                                                                          
032900 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK621'.          
033000 01  DLI-IO-WDK621.                                                       
033100*    03  -COPY WDK621                                                     
033200                                                                          
033300 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK629'.          
033400 01  DLI-IO-WDK629.                                                       
033500*    03  -COPY WDK629                                                     
033600                                                                          
033700 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK711'.          
033800 01  DLI-IO-WDK711.                                                       
033900*    03  -COPY WDK711                                                     
034000                                                                          
034100 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDGX4134'.        
034200 01  DLI-IO-WDGX4134.                                                     
034300*    03  -COPY WDGX4134                                                   
034400                                                                          
034500 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDQ2C1'.          
034600 01  DLI-IO-WDQ2C1.                                                       
034700*    03  WDQ2C1 -COPY WDQ2C1                                              
034800                                                                          
034900 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDB601'.          
035000 01  DLI-IO-WDB601.                                                       
035100*    03  -COPY WDB601                                                     
035200                                                                          
035300 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDB616'.          
035400 01  DLI-IO-WDB616.                                                       
035500*    03  -COPY WDB616                                                     
035600                                                                          
035700 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDB501'.          
035800 01  DLI-IO-WDB501.                                                       
035900*    03  -COPY WDB501                                                     
036000                                                                          
036100                                                                          
036200 LINKAGE SECTION.                                                         
036300                                                                          
036400*01  -COPY W0009   -PRE MSG-                                              
036500                                                                          
036600*01  -COPY W0009   -PRE DISTRDOC-                                         
036700                                                                          
036800*01  -COPY W0009   -PRE 0693X-                                            
036900                                                                          
037000 01  ATAB-PCB                    PIC X.                                   
037100                                                                          
037200*01  -COPY W0008   -PRE WDP8-                                             
037300     05  FILLER                  PIC X.                                   
037400                                                                          
037500*01  -COPY W0008     -PRE WDB2-                                           
037600     05  FILLER                  PIC X.                                   
037700                                                                          
037800*01  -COPY W0008     -PRE WDF5-                                           
037900     05  FILLER                  PIC X.                                   
038000                                                                          
038100*01  -COPY W0008     -PRE WDK6-                                           
038200     05  FILLER                  PIC X.                                   
038300                                                                          
038400*01  -COPY W0008     -PRE WDP7-                                           
038500     05  FILLER                  PIC X.                                   
038600                                                                          
038700*01  -COPY W0008     -PRE WDR5-                                           
038800     05  FILLER                  PIC X.                                   
038900*01  -COPY W0008     -PRE WDQ2C-                                          
039000     05  FILLER                  PIC X.                                   
039100*01  -COPY W0008     -PRE WDK7-                                           
039200     05  FILLER                  PIC X.                                   
039300*01  -COPY W0008     -PRE WDB6-                                           
039400     05  FILLER                  PIC X.                                   
039500*01  -COPY W0008     -PRE GMTC-                                           
039600     05  FILLER                  PIC X.                                   
039700                                                                          
039800 01  ORDN-XXKP-PCB               PIC X.                                   
039900 01  ORDN-ORQL-PCB               PIC X.                                   
040000 01  ORDN-PROC-PCB               PIC X.                                   
040100 01  ORDN-ORQI-PCB               PIC X.                                   
040200                                                                          
040300                                                                          
040400 PROCEDURE DIVISION  USING MSG-PCB  DISTRDOC-PCB 0693X-PCB                
040500                           ATAB-PCB WDP8-PCB                              
040600                           WDB2-PCB WDF5-PCB WDK6-PCB WDP7-PCB            
040700                           WDR5-PCB WDQ2C-PCB WDK7-PCB                    
040800                           WDB6-PCB GMTC-PCB                              
040900                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
041000                           ORDN-PROC-PCB ORDN-ORQI-PCB.                   
041100                                                                          
041200 MAIN SECTION.                                                            
041300     ENTRY 'DLITCBL' USING MSG-PCB  DISTRDOC-PCB 0693X-PCB                
041400                           ATAB-PCB WDP8-PCB                              
041500                           WDB2-PCB WDF5-PCB WDK6-PCB WDP7-PCB            
041600                           WDR5-PCB WDQ2C-PCB WDK7-PCB                    
041700                           WDB6-PCB GMTC-PCB                              
041800                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
041900                           ORDN-PROC-PCB ORDN-ORQI-PCB.                   
042000                                                                          
042100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
042200     IF SUB-KDRC = 0                                                      
042300        MOVE 001                 TO AUTH-KDCALL                           
042400        CALL WZ01AUTH         USING AUTH-WZ01AUTH                         
042500                                    REQU-WZ01REQ2                         
042600                                                                          
042700        IF AUTH-KDRC = 0                                                  
042800           IF REQU-KDPGMACT = 'E'                                         
042900                                                                          
043000              PERFORM A-INIT-SAVE-INPUT                                   
043100              PERFORM B-CHECK-INPUT                                       
043200                                                                          
043300              IF EVERYTHING-OK                                            
043400                 PERFORM D-CREATE-DISPATCH-ORDER-HEAD                     
043500                 PERFORM E-CREATE-DISPATCH-ORDER-LINES                    
043600                 PERFORM F-CREATE-ORDER-RESPONSE                          
043700              END-IF                                                      
043800           ELSE                                                           
043900             MOVE SYS-ERROR TO RESP-IDMSG-ERROR                           
044000           END-IF                                                         
044100       ELSE                                                               
044200          IF AUTH-KDRC = 4                                                
044300            MOVE BAD-REQUEST TO RESP-IDMSG-ERROR                          
044400          ELSE                                                            
044500            MOVE AUTH-KDRC TO KDRC-DISPLAY                                
044600            STRING 'WZ01AUTH GETARG ERROR RC=' KDRC-DISPLAY               
044700            DELIMITED BY SIZE INTO ERROR-TEXT                             
044800            CALL ABEND USING RKOD-ABEND-WITH-DUMP                         
044900          END-IF                                                          
045000       END-IF                                                             
045100                                                                          
045200       PERFORM S02-RETURN-RESPONSE                                        
045300     END-IF                                                               
045400                                                                          
045500     MOVE ZERO                         TO RETURN-CODE                     
045600                                                                          
045700     GOBACK                                                               
045800     .                                                                    
045900                                                                          
046000                                                                          
046100 A-INIT-SAVE-INPUT SECTION.                                               
046200     MOVE 'A-INIT-SAVE-INPUT' TO CURRENT-SECTION                          
046300                                                                          
046400     MOVE 001                    TO RESP-IDMSGVER                         
046500     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
046600                                    RESP-IDMSG-INFO                       
046700                                    RESP-IDELMT-ERROR                     
046800     MOVE REQU-IDDISTR           TO RESP-IDDISTR                          
046900     MOVE REQU-IDKUNDNR          TO RESP-IDKUNDNR                         
047000     MOVE SPACE                  TO RESP-IDAPIORDREF                      
047100                                                                          
047200     MOVE AUTH-IDSYSTEM          TO WS-IDSYSTEM                           
047300                                                                          
047400*    ACCEPT RESP-TIREGDAT      FROM DATE                                  
047500                                                                          
047600     ACCEPT WS-TODAY-DATE-8    FROM DATE                                  
047700     ADD 20000000                TO WS-TODAY-DATE-8                       
047800                                                                          
047900     MOVE SPACE                  TO RESP-IDMFSINF                         
048000                                    RESP-TEMFSINF                         
048100                                                                          
048200*    -- INITIALIZE W006KOM AREAS WITH FIXED VALUES                        
048300*    -- FIELDS WITH VARYING CONTENT ARE SET LATER                         
048400     MOVE SPACE                      TO MSG-KOM-WMSGKOM                   
048500     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
048600     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
048700     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
048800     MOVE 'W9031400'                 TO MSG-KOM-IDSNDJOB                  
048900     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
049000*    -- TIKLOCK WILL BE INCREMENTENTED FOR EACH ORDER                     
049100*    -- THIS IS THE START VALUE                                           
049200     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
049300                                                                          
049400*    -- INITIALIZE TARGET TRANSACTION AREA WITH FIXED VALUES              
049500     MOVE LOW-VALUE                  TO MSG-KDZ1                          
049600     MOVE LOW-VALUE                  TO MSG-KDZ2                          
049700                                                                          
049800*    -- GET CURRENT DATE AND VALIDATE                                     
049900     ACCEPT DAGENS-DATUM           FROM DATE                              
050000                                                                          
050100     MOVE 'AAMMDD'                   TO DAT-KDDATFORM                     
050200     MOVE DAGENS-DATUM               TO DAT-I-TIDATUM                     
050300                                                                          
050400     CALL WDATKONV  USING DAT-KDDATFORM DAT-I-TIDATUM                     
050500                          DAT-O-TIDATUM DAT-KDSVAR                        
050600                                                                          
050700     IF DAT-KDSVAR-OK                                                     
050800        CONTINUE                                                          
050900     ELSE                                                                 
051000        MOVE NOO                     TO OK-SW                             
051100        MOVE NOT-FOUND               TO RESP-IDMFSINF                     
051200        MOVE 'INVALID CURRENT DATE - CONTACT IT SUPPORT'                  
051300                                     TO RESP-TEMFSINF                     
051400     END-IF                                                               
051500                                                                          
051600     .                                                                    
051700                                                                          
051800                                                                          
051900 B-CHECK-INPUT SECTION.                                                   
052000     MOVE 'B-CHECK-INPUT  ' TO CURRENT-SECTION                            
052100                                                                          
052200     MOVE NOO             TO SW-REFILL                                    
052300     MOVE REQU-IDDISTR    TO DIST35-IDDISTR                               
052400                                                                          
052500     IF DIST35-REFILL                                                     
052600     OR DIST35-REFILL-INOM-NDC                                            
052700     OR DIST35-NONVCC-REFILL                                              
052800        MOVE YES          TO SW-REFILL                                    
052900        SEARCH ALL DIST57-REFILL-DC                                       
053000          AT END                                                          
053100              MOVE NOO          TO OK-SW                                  
053200              MOVE NOT-FOUND    TO RESP-IDMFSINF                          
053300              MOVE 'DISTRICT MISSING IN WWDIST57'                         
053400                                TO RESP-TEMFSINF                          
053500          WHEN DIST57-SOK-IDDISTR(DIST57-IX) = REQU-IDDISTR               
053600             MOVE DIST57-REFILL-TO-DC(DIST57-IX)                          
053700                                TO WS-IDDC-RECV                           
053800        END-SEARCH                                                        
053900     END-IF                                                               
054000                                                                          
054100     IF REQU-KVRADER = ZERO                                               
054200        MOVE NOO          TO OK-SW                                        
054300        MOVE NOT-FOUND    TO RESP-IDMFSINF                                
054400        MOVE 'ORDER LINES MISSING'                                        
054500                          TO RESP-TEMFSINF                                
054600     ELSE                                                                 
054700        MOVE 1            TO PART-IX                                      
054800        PERFORM UNTIL PART-IX > REQU-KVRADER                              
054900           IF WS-IDSYSTEM = WS-IDSYSTEM-ECOM                              
055000              IF REQU-PRARTNTO-LOC(PART-IX) = ZERO                        
055100                 MOVE NOO       TO OK-SW                                  
055200                 MOVE NOT-FOUND TO RESP-IDMFSINF                          
055300                 MOVE 'PRICE IS MISSING '                                 
055400                                TO RESP-TEMFSINF                          
055500              END-IF                                                      
055600           END-IF                                                         
055700           IF WS-IDSYSTEM = WS-IDSYSTEM-POLE OR                           
055800                            WS-IDSYSTEM-LYNK OR                           
055900                            WS-IDSYSTEM-VOUI OR                           
056000                            WS-IDSYSTEM-TAD  OR                           
056100                            WS-IDSYSTEM-ACC  OR                           
056200                            WS-IDSYSTEM-APA  OR                           
056300                            WS-IDSYSTEM-APB  OR                           
056400                            WS-IDSYSTEM-APC  OR                           
056500                            WS-IDSYSTEM-APD  OR                           
056600                            WS-IDSYSTEM-APE  OR                           
056700                            WS-IDSYSTEM-APF  OR                           
056800                            WS-IDSYSTEM-APG  OR                           
056900                            WS-IDSYSTEM-APH  OR                           
057000                            WS-IDSYSTEM-API  OR                           
057100                            WS-IDSYSTEM-APJ                               
057200             IF REQU-PRARTNTO-LOC(PART-IX) NOT = ZERO                     
057300                 MOVE NOO          TO OK-SW                               
057400                 MOVE NOT-FOUND    TO RESP-IDMFSINF                       
057500                 MOVE 'MANUAL PRICE NOT ALLOWED'                          
057600                                   TO RESP-TEMFSINF                       
057700             END-IF                                                       
057800           END-IF                                                         
057900           IF REQU-KVBEART(PART-IX) NOT NUMERIC OR                        
058000              REQU-KVBEART(PART-IX) = ZERO                                
058100              MOVE NOO             TO OK-SW                               
058200              MOVE NOT-FOUND       TO RESP-IDMFSINF                       
058300              MOVE 'ORDERED QUANTITY MISSING'                             
058400                                   TO RESP-TEMFSINF                       
058500           END-IF                                                         
058600           ADD 1 TO PART-IX                                               
058700        END-PERFORM                                                       
058800     END-IF                                                               
058900                                                                          
059000                                                                          
059100                                                                          
059200     IF EVERYTHING-OK AND                                                 
059300        REQU-IDAPI (1:20) = 'PULSPURCHASEORDERB2C'                        
059400        IF NOT-REFILL                                                     
059500           PERFORM BA-CHECK-B2C-FIELDS                                    
059600        END-IF                                                            
059700     ELSE                                                                 
059800        IF EVERYTHING-OK                                                  
059900           PERFORM BB-CHECK-DISTRICT-CUSTOMER                             
060000           IF EVERYTHING-OK                                               
060100              PERFORM BC-CHECK-B2B-FIELDS                                 
060200           END-IF                                                         
060300        END-IF                                                            
060400     END-IF                                                               
060500                                                                          
060600     IF EVERYTHING-OK AND                                                 
060700        NOT-REFILL                                                        
060800        PERFORM BD-CHECK-CALLER-ID                                        
060900     END-IF                                                               
061000                                                                          
061100     IF EVERYTHING-OK AND                                                 
061200        NOT-REFILL                                                        
061300        PERFORM BE-CHECK-REPAIR-DATE                                      
061400     END-IF                                                               
061500                                                                          
061600     IF EVERYTHING-OK                                                     
061700        PERFORM BF-CHECK-ORDER-LINES                                      
061800     END-IF                                                               
061900                                                                          
062000     IF EVERYTHING-OK                                                     
062100        PERFORM BG-CHECK-ORDER-NUMBER                                     
062200     END-IF                                                               
062300                                                                          
062400     IF EVERYTHING-OK                                                     
062500        PERFORM BH-CHECK-FREIGHT-CODE                                     
062600     END-IF                                                               
062700                                                                          
062800     .                                                                    
062900                                                                          
063000                                                                          
063100 BA-CHECK-B2C-FIELDS SECTION.                                             
063200     MOVE 'BA-CHECK-B2C   ' TO CURRENT-SECTION                            
063300                                                                          
063400                                                                          
063500     IF REQU-BEGMT-RAD1 = SPACE                                           
063600       MOVE NOO                 TO OK-SW                                  
063700       MOVE NOT-FOUND           TO RESP-IDMFSINF                          
063800       MOVE 'DELIVERY NAME  MISSING'                                      
063900                                TO RESP-TEMFSINF                          
064000     ELSE                                                                 
064100       IF REQU-ADGMT-GATA = SPACE                                         
064200         MOVE NOO               TO OK-SW                                  
064300         MOVE NOT-FOUND         TO RESP-IDMFSINF                          
064400         MOVE 'DELIVERY STREET MISSING'                                   
064500                                TO RESP-TEMFSINF                          
064600       ELSE                                                               
064700         IF REQU-ADCITY = SPACE                                           
064800           MOVE NOO             TO OK-SW                                  
064900           MOVE NOT-FOUND       TO RESP-IDMFSINF                          
065000           MOVE 'DELIVERY CITY MISSING'                                   
065100                                TO RESP-TEMFSINF                          
065200         ELSE                                                             
065300           IF REQU-ADPOSTNR = SPACE                                       
065400             MOVE NOO           TO OK-SW                                  
065500             MOVE NOT-FOUND     TO RESP-IDMFSINF                          
065600             MOVE 'DELIVERY POSTAL CODE MISSING'                          
065700                                TO RESP-TEMFSINF                          
065800           ELSE                                                           
065900             MOVE REQU-IDLANDX2    TO LAND-IDLANDX2                       
066000             MOVE SPACE            TO LAND-IDLANDX3                       
066100             CALL WISOLAND USING LAND-WISOLAND                            
066200             IF LAND-KDSVAR NOT = SPACE                                   
066300                MOVE NOO           TO OK-SW                               
066400                MOVE NOT-FOUND     TO RESP-IDMFSINF                       
066500                MOVE 'LAND CODE IS MISSING OR INVALID'                    
066600                                   TO RESP-TEMFSINF                       
066700             ELSE                                                         
066800                IF REQU-ADGMT-LAND = SPACE                                
066900                  MOVE NOO         TO OK-SW                               
067000                  MOVE NOT-FOUND   TO RESP-IDMFSINF                       
067100                  MOVE 'DELIVERY COUNTRY CODE MISSING'                    
067200                                   TO RESP-TEMFSINF                       
067300                ELSE                                                      
067400                  IF REQU-KDORDKL-URS = '1' OR '2' OR '4'                 
067500                    MOVE REQU-KDORDKL-URS TO WS-KDORDKL                   
067600                    IF REQU-TIREPDAT NOT = SPACE                          
067700                       MOVE NOO       TO OK-SW                            
067800                       MOVE NOT-FOUND TO RESP-IDMFSINF                    
067900                       MOVE 'REPAIRDATE NOT ALLOWED'                      
068000                                      TO RESP-TEMFSINF                    
068100                    END-IF                                                
068200                  ELSE                                                    
068300                    MOVE NOO          TO OK-SW                            
068400                    MOVE NOT-FOUND    TO RESP-IDMFSINF                    
068500                    MOVE 'PRIO MISSING OR INVALID'                        
068600                                      TO RESP-TEMFSINF                    
068700                  END-IF                                                  
068800                END-IF                                                    
068900             END-IF                                                       
069000           END-IF                                                         
069100         END-IF                                                           
069200       END-IF                                                             
069300     END-IF                                                               
069400                                                                          
069500     IF EVERYTHING-OK                                                     
069600        MOVE WS-IDSYSTEM         TO W-IDSYSMOT                            
069700        MOVE FUNCTION UPPER-CASE(REQU-ADPOSTNR)                           
069800                                 TO WS-REDUIN                             
069900        CALL W009REDU USING WS-REDUIN WS-REDUUT                           
070000        MOVE WS-REDUUT           TO W-ADPOSTNR                            
070100        MOVE REQU-IDLANDX2       TO W-IDLANDX2                            
070200        PERFORM IMS-GU-WDGX4134                                           
070300        IF SEGMENT-FOUND                                                  
070400           MOVE 4134-IDDISTR     TO W-IDDISTR                             
070500           MOVE 4134-IDKUNDNR    TO W-IDKUNDNR                            
070600           IF 4134-ADPOSTNR-FOM = W-ADPOSTNR-DEF                          
070700              PERFORM BAA-SEND-UPD-REQ-MAIL                               
070800           END-IF                                                         
070900        ELSE                                                              
071000           MOVE NOO              TO OK-SW                                 
071100           MOVE NOT-FOUND        TO RESP-IDMFSINF                         
071200           MOVE 'POSTAL CODE NOT FOUND'                                   
071300                                 TO RESP-TEMFSINF                         
071400        END-IF                                                            
071500     END-IF                                                               
071600                                                                          
071700     .                                                                    
071800                                                                          
071900                                                                          
072000 BAA-SEND-UPD-REQ-MAIL   SECTION.                                         
072100     MOVE 'BAA-SEND-MAIL  ' TO CURRENT-SECTION                            
072200                                                                          
072300     PERFORM S10-OPEN-DP                                                  
072400     PERFORM S10-PUT-HDR                                                  
072500     MOVE SPACE TO SEND-RAD                                               
072600     STRING 'Hi,'                                                         
072700     DELIMITED BY SIZE INTO SEND-RAD                                      
072800     PERFORM S10-PUT-LINE                                                 
072900                                                                          
073000     MOVE SPACE TO SEND-RAD                                               
073100     PERFORM S10-PUT-LINE                                                 
073200                                                                          
073300     MOVE SPACE TO SEND-RAD                                               
073400     STRING 'Default values have been used for Country: '                 
073500             REQU-IDLANDX2 ' and Postal Code: ' REQU-ADPOSTNR             
073600     DELIMITED BY SIZE INTO SEND-RAD                                      
073700     PERFORM S10-PUT-LINE                                                 
073800                                                                          
073900     MOVE SPACE TO SEND-RAD                                               
074000     STRING 'IDORIGSYS: ' WS-IDSYSTEM                                     
074100     DELIMITED BY SIZE INTO SEND-RAD                                      
074200     PERFORM S10-PUT-LINE                                                 
074300                                                                          
074400     MOVE SPACE TO SEND-RAD                                               
074500     STRING 'Customer ref: ' REQU-BEKUNDRF-001                            
074600     DELIMITED BY SIZE INTO SEND-RAD                                      
074700     PERFORM S10-PUT-LINE                                                 
074800                                                                          
074900     MOVE SPACE TO SEND-RAD                                               
075000     STRING 'Postal code: ' REQU-ADPOSTNR                                 
075100     DELIMITED BY SIZE INTO SEND-RAD                                      
075200     PERFORM S10-PUT-LINE                                                 
075300                                                                          
075400     MOVE SPACE TO SEND-RAD                                               
075500     STRING 'Default values are used for Country ' REQU-IDLANDX2          
075600     DELIMITED BY SIZE INTO SEND-RAD                                      
075700     PERFORM S10-PUT-LINE                                                 
075800                                                                          
075900     MOVE SPACE TO SEND-RAD                                               
076000     MOVE 4134-IDDISTR   TO WS-IDDISTR-NUM                                
076100     STRING 'Default District ' WS-IDDISTR-NUM                            
076200     DELIMITED BY SIZE INTO SEND-RAD                                      
076300     PERFORM S10-PUT-LINE                                                 
076400                                                                          
076500     MOVE SPACE TO SEND-RAD                                               
076600     MOVE 4134-IDKUNDNR  TO WS-IDKUNDNR-NUM                               
076700     STRING 'Default Customer ' WS-IDKUNDNR-NUM                           
076800     DELIMITED BY SIZE INTO SEND-RAD                                      
076900     PERFORM S10-PUT-LINE                                                 
077000                                                                          
077100     MOVE SPACE TO SEND-RAD                                               
077200     STRING 'Please update the Postal Code on Screen 0815'                
077300     DELIMITED BY SIZE INTO SEND-RAD                                      
077400     PERFORM S10-PUT-LINE                                                 
077500                                                                          
077600     MOVE SPACE TO SEND-RAD                                               
077700     PERFORM S10-PUT-LINE                                                 
077800                                                                          
077900     MOVE SPACE TO SEND-RAD                                               
078000     STRING 'If you have any questions please contact '                   
078100            'DCIDHELP@VOLVOCARS.COM.'                                     
078200     DELIMITED BY SIZE INTO SEND-RAD                                      
078300     PERFORM S10-PUT-LINE                                                 
078400                                                                          
078500     PERFORM S10-CLOSE-DP                                                 
078600     .                                                                    
078700                                                                          
078800                                                                          
078900 BB-CHECK-DISTRICT-CUSTOMER SECTION.                                      
079000     MOVE 'BB-CHECK-D-C   ' TO CURRENT-SECTION                            
079100                                                                          
079200     MOVE REQU-IDDISTR   TO W-IDDISTR                                     
079300     MOVE REQU-IDKUNDNR  TO W-IDKUNDNR                                    
079400     MOVE REQU-IDDISTR   TO TEST-IDDISTR                                  
079500     PERFORM IMS-GU-WDB201                                                
079600     IF SEGMENT-MISSING                                                   
079700        MOVE NOO         TO OK-SW                                         
079800        MOVE NOT-FOUND   TO RESP-IDMFSINF                                 
079900        MOVE 'DISTRICT CUSTOMER NOT FOUND'                                
080000                         TO RESP-TEMFSINF                                 
080100     ELSE                                                                 
080200        IF WS-IDSYSTEM = WS-IDSYSTEM-LYNK                                 
080300           IF GMT-KDKUNDKAT NOT = '03'                                    
080400              MOVE NOO         TO OK-SW                                   
080500              MOVE NOT-FOUND   TO RESP-IDMFSINF                           
080600              MOVE 'DISTRICT CUSTOMER INVALID  '                          
080700                               TO RESP-TEMFSINF                           
080800           END-IF                                                         
080900        END-IF                                                            
081000        IF WS-IDSYSTEM = WS-IDSYSTEM-POLE                                 
081100           IF GMT-KDKUNDKAT NOT = '02'                                    
081200              MOVE NOO         TO OK-SW                                   
081300              MOVE NOT-FOUND   TO RESP-IDMFSINF                           
081400              MOVE 'DISTRICT CUSTOMER INVALID  '                          
081500                               TO RESP-TEMFSINF                           
081600           END-IF                                                         
081700        END-IF                                                            
081800     END-IF                                                               
081900     .                                                                    
082000                                                                          
082100                                                                          
082200 BC-CHECK-B2B-FIELDS SECTION.                                             
082300     MOVE 'BC-CHECK-B2B   ' TO CURRENT-SECTION                            
082400                                                                          
082500     IF REQU-KDORDKL-URS = '0' OR '1' OR '2' OR '3' or '4'                
082600        MOVE REQU-KDORDKL-URS    TO WS-KDORDKL                            
082700        IF REFILLORDER                                                    
082800           IF REQU-KDORDKL-URS = '0' or '1' OR '4'                        
082900              CONTINUE                                                    
083000           ELSE                                                           
083100              MOVE NOO           TO OK-SW                                 
083200              MOVE NOT-FOUND     TO RESP-IDMFSINF                         
083300              MOVE 'ORDER CLASS NOT ALLOWED'                              
083400                                 TO RESP-TEMFSINF                         
083500           END-IF                                                         
083600        END-IF                                                            
083700     ELSE                                                                 
083800        IF REQU-TIREPDAT = SPACE AND                                      
083900           NOT-REFILL                                                     
084000           MOVE NOO           TO OK-SW                                    
084100           MOVE NOT-FOUND     TO RESP-IDMFSINF                            
084200           MOVE 'TRANSPORT PRIO MISSING OR WRONG'                         
084300                              TO RESP-TEMFSINF                            
084400        END-IF                                                            
084500     END-IF                                                               
084600     IF NOT-REFILL                                                        
084700        IF REQU-TIREPDAT NOT = SPACE                                      
084800           MOVE REQU-TIREPDAT       TO WS-TIREPDAT-X                      
084900           MOVE WS-TIAA-X           TO WS-TIAA                            
085000           MOVE WS-TIMM-X           TO WS-TIMM                            
085100           MOVE WS-TIDD-X           TO WS-TIDD                            
085200           MOVE WS-TISS-X           TO WS-TISS-8                          
085300           MOVE WS-TIAA-X           TO WS-TIAA-8                          
085400           MOVE WS-TIMM-X           TO WS-TIMM-8                          
085500           MOVE WS-TIDD-X           TO WS-TIDD-8                          
085600        ELSE                                                              
085700           MOVE ZERO                TO WS-TIREPDAT                        
085800        END-IF                                                            
085900     END-IF                                                               
086000     .                                                                    
086100                                                                          
086200                                                                          
086300 BD-CHECK-CALLER-ID SECTION.                                              
086400     MOVE 'BD-CHECK-CALLER' TO CURRENT-SECTION                            
086500                                                                          
086600* this seems a bit strange butu for b to c I guess we need to test        
086700*    IF REQU-IDNAMN = SPACE                                               
086800*       MOVE NOO             TO OK-SW                                     
086900*       MOVE NOT-FOUND       TO RESP-IDMFSINF                             
087000*       MOVE 'CONTACT NAME MISSING'                                       
087100*                            TO RESP-TEMFSINF                             
087200*    ELSE                                                                 
087300*       IF REQU-BETELNR = SPACE                                           
087400*          MOVE NOO          TO OK-SW                                     
087500*          MOVE NOT-FOUND    TO RESP-IDMFSINF                             
087600*          MOVE 'TELEPHONE NUMBER MISSING'                                
087700*                            TO RESP-TEMFSINF                             
087800*       ELSE                                                              
087900*          IF REQU-IDMAIL = SPACE                                         
088000**            MOVE NOO       TO OK-SW                                     
088100*             MOVE NOT-FOUND TO RESP-IDMFSINF                             
088200*             MOVE 'TELEPHONE NUMBER MISSING'                             
088300*                            TO RESP-TEMFSINF                             
088400*          END-IF                                                         
088500*       END-IF                                                            
088600*    END-IF                                                               
088700     .                                                                    
088800                                                                          
088900                                                                          
089000 BE-CHECK-REPAIR-DATE SECTION.                                            
089100     MOVE 'BE-CHECK-REPDAT ' TO CURRENT-SECTION                           
089200                                                                          
089300     PERFORM IMS-GU-WDB201                                                
089400     IF SEGMENT-MISSING                                                   
089500        MOVE '11'          TO GMT-IDDC-BULK(1)                            
089600        MOVE NOO           TO GMT-FLLDCKND                                
089700     END-IF                                                               
089800                                                                          
089900     IF WS-TIREPDAT NOT = ZERO                                            
090000        MOVE 'AAMMDD'      TO DAT-KDDATFORM                               
090100        MOVE WS-TIREPDAT   TO DAT-I-TIDATUM                               
090200                                                                          
090300        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
090400                            DAT-O-TIDATUM DAT-KDSVAR                      
090500                                                                          
090600        IF NOT DAT-KDSVAR-OK                                              
090700           MOVE NOO         TO OK-SW                                      
090800           MOVE NOT-FOUND   TO RESP-IDMFSINF                              
090900           MOVE 'REPAIR DATE NOT VALID'                                   
091000                            TO RESP-TEMFSINF                              
091100        ELSE                                                              
091200           IF WS-TIREPDAT-8 < WS-TODAY-DATE-8                             
091300             MOVE NOO         TO OK-SW                                    
091400             MOVE NOT-FOUND   TO RESP-IDMFSINF                            
091500             MOVE 'REPAIR DATE NOT VALID'                                 
091600                              TO RESP-TEMFSINF                            
091700           ELSE                                                           
091800             IF GMT-FLLDCKND = NOO                                        
091900              MOVE NOO        TO OK-SW                                    
092000              MOVE NOT-FOUND  TO RESP-IDMFSINF                            
092100              MOVE                                                        
092200               'REPAIR ORDER NOT POSSIBLE FOR NONE LDC CUSTOMER'          
092300                              TO RESP-TEMFSINF                            
092400             END-IF                                                       
092500           END-IF                                                         
092600        END-IF                                                            
092700     END-IF                                                               
092800                                                                          
092900     IF EVERYTHING-OK AND                                                 
093000        WS-TIREPDAT NOT = ZERO                                            
093100        MOVE ALL '+'             TO MSGI-WMSGINIT                         
093200        MOVE '013'               TO MSGI-KDCALL                           
093300        MOVE 'WIDDC   '          TO MSGI-IDUSER                           
093400        MOVE GMT-IDDC-BULK(1) TO MSGI-IDUSER(6:2)                         
093500        MOVE '9314'              TO MSGI-IDTRANS                          
093600                                                                          
093700        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
093800                                                                          
093900        MOVE MSGI-TILOKDAT       TO WS-TILOKDAT                           
094000        MOVE 'YYMMDD'            TO DAYS-KDDATFMT1                        
094100        MOVE WS-TILOKDAT         TO DAYS-TIDATE1                          
094200        MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                        
094300        MOVE WS-TIREPDAT         TO DAYS-TIDATE2                          
094400        MOVE ZERO                TO DAYS-KVDAYS                           
094500        MOVE SPACE               TO DAYS-IDCALEND                         
094600                                                                          
094700        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
094800                                                                          
094900        IF DAYS-KDRC = ZERO                                               
095000           IF DAYS-KVDAYS <= ZERO                                         
095100             MOVE 1              TO WS-KDORDKL                            
095200           ELSE                                                           
095300             MOVE GMT-KVDAGAR-RFS-DEF                                     
095400                                 TO WS-KVDAGAR-RFS-DEF                    
095500             PERFORM                                                      
095600              VARYING RFS-IX FROM 1 BY 1                                  
095700              UNTIL RFS-IX > RFS-IX-MAX                                   
095800                IF GMT-IDDC-RFS (RFS-IX) = GMT-IDDC-BULK(1)               
095900                   MOVE GMT-KVDAGAR-RFS (RFS-IX)                          
096000                                 TO WS-KVDAGAR-RFS-DEF                    
096100                END-IF                                                    
096200             END-PERFORM                                                  
096300                                                                          
096400             IF DAYS-KVDAYS < WS-KVDAGAR-RFS-DEF                          
096500                MOVE 2           TO WS-KDORDKL                            
096600             ELSE                                                         
096700                MOVE 3           TO WS-KDORDKL                            
096800             END-IF                                                       
096900           END-IF                                                         
097000        ELSE                                                              
097100              STRING 'REPAIR DATE ' REQU-TIREPDAT ' IS NOT VALID'         
097200              DELIMITED BY SIZE INTO RESP-TEMFSINF                        
097300        END-IF                                                            
097400     END-IF                                                               
097500     .                                                                    
097600                                                                          
097700                                                                          
097800 BF-CHECK-ORDER-LINES SECTION.                                            
097900     MOVE 'BF-CHECK-LINES  ' TO CURRENT-SECTION                           
098000                                                                          
098100     MOVE 1            TO PART-IX                                         
098200     PERFORM UNTIL PART-IX > REQU-KVRADER OR                              
098300                   SOMETHING-WRONG                                        
098400        MOVE REQU-IDLEVART (PART-IX) TO W-SEQB-IDLEVART                   
098500        IF WS-IDSYSTEM = WS-IDSYSTEM-LYNK                                 
098600           PERFORM IMS-GU-WDF501-BSEQ                                     
098700           IF SEGMENT-MISSING                                             
098800              MOVE NOO          TO OK-SW                                  
098900              MOVE NOT-FOUND    TO RESP-IDMFSINF                          
099000              STRING 'PARTNUMER ' DELIMITED BY SIZE                       
099100              W-SEQB-IDLEVART     DELIMITED BY SPACES                     
099200              ' NOT FOUND'        DELIMITED BY SIZE                       
099300              INTO RESP-TEMFSINF                                          
099400           ELSE                                                           
099500              MOVE XART-IDARTNR TO W-IDARTNR                              
099600           END-IF                                                         
099700        ELSE                                                              
099800           MOVE ZERO TO W-BLANKS                                          
099900           INSPECT FUNCTION REVERSE(REQU-IDLEVART (PART-IX))              
100000                   TALLYING W-BLANKS FOR LEADING SPACES                   
100100           COMPUTE W-LENGTH = 30 - W-BLANKS                               
100200           IF (W-LENGTH < 9 AND W-LENGTH > 0 ) AND                        
100300              REQU-IDLEVART (PART-IX) (1:W-LENGTH) NUMERIC                
100400              MOVE REQU-IDLEVART  (PART-IX) (1:W-LENGTH)                  
100500                                TO W-IDARTNR                              
100600                                   W-IDARTNR-CHAR                         
100700           ELSE                                                           
100800              MOVE NOO          TO OK-SW                                  
100900              MOVE NOT-FOUND    TO RESP-IDMFSINF                          
101000              STRING 'PARTNUMER ' DELIMITED BY SIZE                       
101100              W-SEQB-IDLEVART     DELIMITED BY SPACES                     
101200              ' INVALID  '        DELIMITED BY SIZE                       
101300              INTO RESP-TEMFSINF                                          
101400           END-IF                                                         
101500        END-IF                                                            
101600                                                                          
101700        IF EVERYTHING-OK                                                  
101800           PERFORM IMS-GU-WDK601                                          
101900           IF SEGMENT-MISSING                                             
102000              MOVE NOO          TO OK-SW                                  
102100              MOVE NOT-FOUND    TO RESP-IDMFSINF                          
102200              MOVE 'ORDERED QUANTITY MISSING'                             
102300                                TO RESP-TEMFSINF                          
102400              STRING 'PARTNUMER ' DELIMITED BY SIZE                       
102500              W-SEQB-IDLEVART     DELIMITED BY SPACES                     
102600              ' NOT FOUND'        DELIMITED BY SIZE                       
102700              INTO RESP-TEMFSINF                                          
102800           ELSE                                                           
102900              PERFORM IMS-GNP-WDK611                                      
103000              IF SEGMENT-MISSING                                          
103100                 MOVE NOO       TO OK-SW                                  
103200                 MOVE NOT-FOUND TO RESP-IDMFSINF                          
103300                 STRING 'PARTNUMER '  DELIMITED BY SIZE                   
103400                 W-SEQB-IDLEVART      DELIMITED BY SPACES                 
103500                 ' STD PRICE MISSING' DELIMITED BY SIZE                   
103600                 INTO RESP-TEMFSINF                                       
103700              END-IF                                                      
103800           END-IF                                                         
103900        END-IF                                                            
104000        IF EVERYTHING-OK AND                                              
104100           REFILLORDER                                                    
104200           MOVE WS-IDDC-RECV          TO W-IDDC                           
104300                                         WS-IDDC                          
104400                                         W-IDDC-B6                        
104500           IF CDC-SE                                                      
104600              PERFORM IMS-GU-WDK611                                       
104700              PERFORM IMS-GNP-WDK629                                      
104800              IF SEGMENT-FOUND                                            
104900                 MOVE CREF-IDDC-REF   TO W-IDDC-REF-B6                    
105000                 PERFORM IMS-GU-WDB616                                    
105100                 IF SEGMENT-FOUND AND                                     
105200                    REF-IDDISTR-REFILL = REQU-IDDISTR                     
105300                    MOVE CREF-IDDC-REF                                    
105400                                      TO WS-IDDC-SEND                     
105500                 ELSE                                                     
105600                    MOVE NOO          TO OK-SW                            
105700                    MOVE NOT-FOUND    TO RESP-IDMFSINF                    
105800                    STRING 'INVALID DISTRICT FOR PART REF CDC'            
105900                                              W-IDARTNR-CHAR              
106000                    DELIMITED BY SIZE INTO RESP-TEMFSINF                  
106100                 END-IF                                                   
106200              ELSE                                                        
106300                 MOVE NOO             TO OK-SW                            
106400                 MOVE NOT-FOUND       TO RESP-IDMFSINF                    
106500                 STRING 'INVALID REF TO CDC FOR PART '                    
106600                                              W-IDARTNR-CHAR              
106700                    DELIMITED BY SIZE INTO RESP-TEMFSINF                  
106800              END-IF                                                      
106900           ELSE                                                           
107000             PERFORM IMS-GU-WDK711                                        
107100             IF SEGMENT-FOUND                                             
107200              IF SLAG-IDDC-REF NOT > SPACES                               
107300                 MOVE NOO             TO OK-SW                            
107400                 MOVE NOT-FOUND       TO RESP-IDMFSINF                    
107500                 STRING 'PARTNUMER ' W-IDARTNR-CHAR                       
107600                                            ' LOCALLY PURCHASED'          
107700                    DELIMITED BY SIZE INTO RESP-TEMFSINF                  
107800              ELSE                                                        
107900                 MOVE SLAG-IDDC-REF   TO W-IDDC-REF-B6                    
108000                 PERFORM IMS-GU-WDB601                                    
108100                 IF SEGMENT-FOUND                                         
108200                    PERFORM IMS-GU-WDB616                                 
108300                    IF SEGMENT-FOUND                                      
108400                       IF (SLAG-IDDC-REF = WS-CDC-SE                      
108500                       AND DCS-IDDISTR-REFILL = REQU-IDDISTR )            
108600                       OR  REF-IDDISTR-REFILL = REQU-IDDISTR              
108700                           MOVE SLAG-IDDC-REF                             
108800                                         TO WS-IDDC-SEND                  
108900                       ELSE                                               
109000                           MOVE NOO          TO OK-SW                     
109100                           MOVE NOT-FOUND    TO RESP-IDMFSINF             
109200                           STRING 'INVALID DISTRICT FOR PART '            
109300                                              W-IDARTNR-CHAR              
109400                           DELIMITED BY SIZE INTO RESP-TEMFSINF           
109500                       END-IF                                             
109600                    ELSE                                                  
109700                       MOVE NOO          TO OK-SW                         
109800                       MOVE NOT-FOUND    TO RESP-IDMFSINF                 
109900                       STRING 'INVALID REFILL DISTRICT '                  
110000                                             W-IDARTNR-CHAR               
110100                       DELIMITED BY SIZE INTO RESP-TEMFSINF               
110200                    END-IF                                                
110300                 ELSE                                                     
110400                    MOVE NOO          TO OK-SW                            
110500                    MOVE NOT-FOUND    TO RESP-IDMFSINF                    
110600                    STRING 'INVALID DISTRICT :' W-IDARTNR-CHAR            
110700                    DELIMITED BY SIZE INTO RESP-TEMFSINF                  
110800                 END-IF                                                   
110900              END-IF                                                      
111000             ELSE                                                         
111100                MOVE NOO                TO OK-SW                          
111200                MOVE NOT-FOUND          TO RESP-IDMFSINF                  
111300                STRING 'PART NOT IN DC ' W-IDARTNR-CHAR                   
111400                    DELIMITED BY SIZE INTO RESP-TEMFSINF                  
111500             END-IF                                                       
111600           END-IF                                                         
111700        END-IF                                                            
111800        ADD 1 TO PART-IX                                                  
111900     END-PERFORM                                                          
112000     .                                                                    
112100                                                                          
112200                                                                          
112300 BG-CHECK-ORDER-NUMBER SECTION.                                           
112400     MOVE 'BG-CHECK-ORDER-NUM ' TO CURRENT-SECTION                        
112500                                                                          
112600     MOVE 'API'                 TO ORDN-IDSYSTEM                          
112700     MOVE W-IDDISTR             TO ORDN-IDDISTR                           
112800     MOVE W-IDKUNDNR            TO ORDN-IDKUNDNR                          
112900                                                                          
113000     IF REQU-IDORDNR7 NOT > ZERO                                          
113100        MOVE ZERO               TO ORDN-IDORDNR-IN                        
113200                                                                          
113300        CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB                   
113400                         ORDN-ORQL-PCB ORDN-PROC-PCB ORDN-ORQI-PCB        
113500                                                                          
113600        MOVE ORDN-IDORDNR-UT    TO WS-IDORDNR                             
113700     ELSE                                                                 
113800           MOVE REQU-IDDISTR    TO W-SEQC-IDDISTR                         
113900           MOVE REQU-IDKUNDNR   TO W-SEQC-IDKUNDNR                        
114000           MOVE REQU-IDORDNR7   TO W-SEQC-IDORDNR7                        
114100                                    WS-IDORDNR-SEQ                        
114200           PERFORM IMS-GU-WDQ2C                                           
114300           IF SEGMENT-FOUND                                               
114400              PERFORM UNTIL SEGMENT-MISSING                               
114500               ADD +1           TO W-SEQC-IDORDNR7                        
114600                                    WS-IDORDNR-SEQ                        
114700                PERFORM IMS-GU-WDQ2C                                      
114800              END-PERFORM                                                 
114900              MOVE NOO          TO OK-SW                                  
115000              MOVE NOT-FOUND    TO RESP-IDMFSINF                          
115100              STRING 'ORDER EXIST, '                                      
115200                       WS-IDORDNR-SEQ ' NEXT AVAILABLE'                   
115300                    DELIMITED BY SIZE INTO RESP-TEMFSINF                  
115400           ELSE                                                           
115500              MOVE REQU-IDORDNR7   TO WS-IDORDNR                          
115600           END-IF                                                         
115700*       END-IF                                                            
115800     END-IF                                                               
115900     .                                                                    
116000                                                                          
116100                                                                          
116200 BH-CHECK-FREIGHT-CODE SECTION.                                           
116300     MOVE 'BH-CHECK-FREIGHT-CODE'  TO CURRENT-SECTION                     
116400                                                                          
116500     IF REFILLORDER                                                       
116600       IF REQU-KDFRAKT > ZERO                                             
116700          MOVE WS-IDDC-SEND       TO W-IDDC-WDB5                          
116800                                       W-IDDC-WDB5-DEF                    
116900          MOVE REQU-KDFRAKT       TO W-KDFRAKT-WDB5                       
117000                                       W-KDFRAKT-WDB5-DEF                 
117100          MOVE REQU-IDDISTR       TO W-IDDISTR-WDB5                       
117200                                       W-IDDISTR-WDB5-DEF                 
117300          MOVE REQU-IDKUNDNR      TO W-IDKUNDNR-WDB5                      
117400                                                                          
117500          PERFORM IMS-GU-GMTC-WDB501                                      
117600          IF SEGMENT-MISSING                                              
117700             MOVE NOO             TO OK-SW                                
117800             MOVE NOT-FOUND       TO RESP-IDMFSINF                        
117900             MOVE 'FREIGHT CODE INVALID'                                  
118000                                  TO RESP-TEMFSINF                        
118100          END-IF                                                          
118200       END-IF                                                             
118300     ELSE                                                                 
118400* FRIEGHT CODE IS BLOCKED FOR POLE,LYNK                                   
118500       IF WS-IDSYSTEM = WS-IDSYSTEM-POLE OR                               
118600                         WS-IDSYSTEM-LYNK OR                              
118700                         WS-IDSYSTEM-TAD                                  
118800         IF REQU-KDFRAKT > ZERO                                           
118900             MOVE NOO        TO OK-SW                                     
119000             MOVE NOT-FOUND  TO RESP-IDMFSINF                             
119100             MOVE 'MANUAL FREIGHT CODE NOT ALLOWED'                       
119200                             TO RESP-TEMFSINF                             
119300         END-IF                                                           
119400       END-IF                                                             
119500     END-IF                                                               
119600     .                                                                    
119700                                                                          
119800                                                                          
119900 D-CREATE-DISPATCH-ORDER-HEAD SECTION.                                    
120000     MOVE 'D-CR-ORDER-HEAD ' TO CURRENT-SECTION                           
120100                                                                          
120200     MOVE W-IDDISTR              TO WS-IDDISTR-NUM                        
120300     MOVE W-IDKUNDNR             TO WS-IDKUNDNR-NUM                       
120400*    -- INITIALIZE MID DATA TO W40251                                     
120500     MOVE SPACE                  TO 4251-MID-W4I25101                     
120600     MOVE WS-IDSYSTEM            TO 4251-MID-IDSYSTEM                     
120700     MOVE WS-IDDISTR-NUM         TO 4251-MID-IDDISTR                      
120800     MOVE WS-IDKUNDNR-NUM        TO 4251-MID-IDKUNDNR                     
120900     MOVE WS-IDORDNR             TO 4251-MID-IDORDNR                      
121000     MOVE WS-KDORDKL             TO 4251-MID-KDORDKL                      
121100     IF REQU-KDFRAKT > ZERO                                               
121200        MOVE REQU-KDFRAKT        TO 4251-MID-KDFRAKT                      
121300     END-IF                                                               
121400     IF REQU-BELAGINS > SPACES                                            
121500        MOVE REQU-BELAGINS       TO 4251-MID-BELAGINS                     
121600     END-IF                                                               
121700     IF NOT-REFILL                                                        
121800        MOVE WS-TIREPDAT         TO 4251-MID-TIREPDAT                     
121900        IF WS-TIREPDAT = ZERO                                             
122000           CONTINUE                                                       
122100        ELSE                                                              
122200           MOVE 'PW'             TO 4251-MID-KDORDTYP-LDC                 
122300           PERFORM DA-CREATE-RFS-DATE                                     
122400        END-IF                                                            
122500        MOVE REQU-BEGMT-RAD1     TO 4251-MID-BEGMT-RAD1                   
122600        MOVE REQU-BEGMT-RAD2     TO 4251-MID-BEGMT-RAD2                   
122700        MOVE REQU-ADGMT-GATA     TO 4251-MID-ADGMT-GATA                   
122800        MOVE REQU-IDNAMN         TO 4251-MID-BEBETRAD-1                   
122900        MOVE REQU-BETELNR        TO 4251-MID-BETELNR                      
123000        MOVE REQU-IDMAIL         TO 4251-MID-IDMAIL                       
123100        MOVE SPACE               TO 4251-MID-ADGMT-PADR                   
123200        IF GMT-KDPOSTNR = 'L'                                             
123300          MOVE REQU-ADPOSTNR     TO 4251-MID-ADGMT-PADR                   
123400          MOVE REQU-ADCITY       TO 4251-MID-ADGMT-PADR(11:25)            
123500        ELSE                                                              
123600          MOVE REQU-ADCITY       TO 4251-MID-ADGMT-PADR                   
123700          MOVE REQU-ADPOSTNR     TO 4251-MID-ADGMT-PADR(26:10)            
123800        END-IF                                                            
123900     END-IF                                                               
124000                                                                          
124100     MOVE SPACE                  TO 4251-MID-IDDC                         
124200                                                                          
124300     MOVE NOO                    TO 4251-MID-FLFORBI                      
124400     MOVE ZERO                   TO 4251-MID-KDTPOTYP                     
124500     MOVE ZERO                   TO 4251-MID-TITPO                        
124600                                                                          
124700     MOVE REQU-BEKUNDRF-001      TO 4251-MID-BEKUNDRF                     
124800                                                                          
124900*    -- INITALIZE SOME FIELDS THAT MUST NOT BE BLANK                      
125000     MOVE NOO                    TO 4251-MID-FLEMBORD                     
125100                                    4251-MID-FLAUTPAC                     
125200                                    4251-MID-FLOVRLEV                     
125300                                                                          
125400     MOVE ZERO                   TO 4251-MID-IDDEPT                       
125500                                    4251-MID-IDGROSS                      
125600                                                                          
125700*    -- MOVE TO MSG-IO-AREA AND ADD TRANSACTION PREFIX                    
125800*    -- OUTSIDE THE MID COPYTEXT                                          
125900     COMPUTE MSG-KVLL = LENGTH OF 4251-MID-W4I25101 + 17                  
126000     MOVE 4251-MID-W4I25101   TO MSG-INDATA-MINUS-1-TRANSKOD              
126100     MOVE 'W4T251X '          TO MSG-KDTRANS-1                            
126200     MOVE '4251'              TO MSG-IDTRANS-1                            
126300     MOVE '1'                 TO MSG-KDMFSFOR-1                           
126400                                                                          
126500*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
126600*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
126700     MOVE 'W4I25101'          TO MSG-KOM-IDCPYTXT                         
126800     MOVE W-IDDISTR           TO WS-IDDISTR-NUM                           
126900              STRING 'API ' WS-IDDISTR-NUM                                
127000              DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                     
127100     ADD  1                   TO MSG-KOM-TIKLOCK                          
127200                                                                          
127300     CALL W006KOM USING MSG-PCB                                           
127400                        0693X-PCB                                         
127500                        WDP8-PCB                                          
127600                        MSG-KOM-WMSGKOM                                   
127700                        MSG-IO-AREA                                       
127800                                                                          
127900     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
128000        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
128100          DELIMITED BY SIZE  INTO ERROR-TEXT                              
128200        DISPLAY  ERROR-TEXT                                               
128300        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
128400     END-IF                                                               
128500                                                                          
128600*    -- CLEAR ORDER-LINE AREA BEFORE FIRST LINE IS ADDED                  
128700     MOVE SPACE                     TO 4252-MID-W4I25201                  
128800     .                                                                    
128900                                                                          
129000                                                                          
129100 DA-CREATE-RFS-DATE SECTION.                                              
129200     MOVE 'DA-CREATE-RFS   ' TO CURRENT-SECTION                           
129300                                                                          
129400     MOVE GMT-IDDC-BULK(1)         TO WORK-IDDC                           
129500     MOVE +002                     TO WORK-KDCALL                         
129600     MOVE +001                     TO WORK-KVWORKD                        
129700     MOVE WS-TIREPDAT              TO WORK-TIAAMMDD-FOM                   
129800     CALL WORKDAY                  USING WORK-KDCALL                      
129900                                         WORK-DATE-AREA                   
130000                                         WORK-KDSVAR                      
130100     IF WORK-KDSVAR-FEL                                                   
130200        MOVE 'SECT DA-1, DATE MISSING IN WORKDAY'                         
130300                                   TO FELTEXT                             
130400        CALL ABEND                 USING RKOD-ABEND-NO-DUMP               
130500     ELSE                                                                 
130600       MOVE +003                   TO WORK-KDCALL                         
130700                                                                          
130800       MOVE GMT-KVDAGAR-RFS-DEF    TO WORK-KVWORKD                        
130900       PERFORM                                                            
131000       VARYING RFS-IX FROM 1 BY 1                                         
131100         UNTIL RFS-IX > RFS-IX-MAX                                        
131200         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
131300           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
131400                                   TO WORK-KVWORKD                        
131500         END-IF                                                           
131600       END-PERFORM                                                        
131700       ADD +1  TO WORK-KVWORKD                                            
131800*      +1 TO MAKE THE VARIABLE TO CONTAIN NO OF DAYS BEFORE RFS.          
131900*      0 GIVES THE SAME DAY, 1 GIVES FIRST WORK DAY BEFORE AND SO         
132000*      IF NOT ADDING +1 THE VARIEABLE WOULD BE SET SO                     
132100*      1 WILL GIVE THE SAME DAY, 2 FIRST WORK DAY AND SO ON...            
132200*                                                                         
132300       CALL WORKDAY                USING WORK-KDCALL                      
132400                                         WORK-DATE-AREA                   
132500                                         WORK-KDSVAR                      
132600       IF WORK-KDSVAR-FEL                                                 
132700          MOVE 'SECT CBA-2, DATUM SAKNAS I WORKDAY'                       
132800                                   TO FELTEXT                             
132900          CALL ABEND               USING RKOD-ABEND-NO-DUMP               
133000       ELSE                                                               
133100         IF WORK-TIAAMMDD-FOM < WS-TILOKDAT                               
133200           MOVE GMT-IDDC-BULK(1)   TO WORK-IDDC                           
133300           MOVE +002               TO WORK-KDCALL                         
133400           MOVE +001               TO WORK-KVWORKD                        
133500           MOVE WS-TILOKDAT        TO WORK-TIAAMMDD-FOM                   
133600           CALL WORKDAY            USING WORK-KDCALL                      
133700                                         WORK-DATE-AREA                   
133800                                         WORK-KDSVAR                      
133900           IF WORK-KDSVAR-FEL                                             
134000              MOVE 'SECT CBA-3, DATUM SAKNAS I WORKDAY'                   
134100                                   TO FELTEXT                             
134200              CALL ABEND           USING RKOD-ABEND-NO-DUMP               
134300           ELSE                                                           
134400              MOVE WORK-TIAAMMDD-TOM TO 4251-MID-TIRFS                    
134500           END-IF                                                         
134600         ELSE                                                             
134700           MOVE WORK-TIAAMMDD-FOM  TO 4251-MID-TIRFS                      
134800         END-IF                                                           
134900       END-IF                                                             
135000     END-IF                                                               
135100     .                                                                    
135200                                                                          
135300                                                                          
135400 E-CREATE-DISPATCH-ORDER-LINES SECTION.                                   
135500     MOVE 'E-CR-ORDER-LINES' TO CURRENT-SECTION                           
135600                                                                          
135700                                                                          
135800     MOVE 'W4I25201' TO MSG-KOM-IDCPYTXT                                  
135900                                                                          
136000     MOVE 1          TO PART-IX                                           
136100                        LINE-IX                                           
136200     PERFORM UNTIL PART-IX > REQU-KVRADER                                 
136300        MOVE WS-IDSYSTEM         TO 4252-MID-IDSYSTEM                     
136400        MOVE WS-IDDISTR-NUM      TO 4252-MID-IDDISTR                      
136500        MOVE WS-IDKUNDNR-NUM     TO 4252-MID-IDKUNDNR                     
136600        MOVE WS-IDORDNR          TO 4252-MID-IDORDNR                      
136700                                                                          
136800        PERFORM EA-GET-PART-INFO                                          
136900        MOVE W-IDARTNR           TO 4252-MID-IDARTNR(LINE-IX)             
137000        MOVE ART-REKSIFFR        TO 4252-MID-REKSIFFR(LINE-IX)            
137100        MOVE REQU-KVBEART(PART-IX)                                        
137200                                 TO 4252-MID-KVBEART(LINE-IX)             
137300                                                                          
137400        IF REFILLORDER                                                    
137500           MOVE WS-IDDC-RECV     TO WS-IDDC                               
137600                                    W-IDDC                                
137700           IF CDC-SE                                                      
137800              PERFORM IMS-GHU-WDK611                                      
137900              IF SEGMENT-FOUND                                            
138000                 ADD REQU-KVBEART(PART-IX)                                
138100                                 TO CLAG-KVBEART                          
138200                 PERFORM IMS-REPL-WDK611                                  
138300              END-IF                                                      
138400           ELSE                                                           
138500              PERFORM IMS-GHU-WDK711                                      
138600              IF SEGMENT-FOUND                                            
138700                 ADD REQU-KVBEART(PART-IX)                                
138800                                 TO SLAG-KVBEART                          
138900                 MOVE DAGENS-DATUM                                        
139000                                 TO SLAG-TIORDREG                         
139100                 PERFORM IMS-REPL-WDK711                                  
139200              END-IF                                                      
139300           END-IF                                                         
139400        END-IF                                                            
139500                                                                          
139600        IF NOT-REFILL                                                     
139700           MOVE REQU-PRARTNTO-LOC(PART-IX)                                
139800                                 TO WS-PRARTNTO-NUM                       
139900           MOVE WS-PRARTNTO-NUM  TO WS-PRARTNTO-RED                       
140000           MOVE WS-PRARTNTO-ALFA                                          
140100                                 TO 4252-MID-PRARTNTO-LOC(LINE-IX)        
140200           MOVE REQU-KDVALISO(PART-IX)                                    
140300                                 TO 4252-MID-KDVALISO(LINE-IX)            
140400        END-IF                                                            
140500                                                                          
140600*   move orderLINE TO ORDERLINE                                           
140700        MOVE REQU-BERADREF(PART-IX)                                       
140800                                 TO 4252-MID-BERADREF(LINE-IX)            
140900                                                                          
141000                                                                          
141100        IF GMT-FLLDCKND = JAA                                             
141200                                                                          
141300           MOVE REQU-BEKUNDRF-001                                         
141400                                 TO 4252-MID-IDKUNDRF-WIP(LINE-IX)        
141500        ELSE                                                              
141600           MOVE SPACE            TO 4252-MID-IDKUNDRF-WIP(LINE-IX)        
141700        END-IF                                                            
141800                                                                          
141900        IF PART-IX < REQU-KVRADER                                         
142000           MOVE NOO TO 4252-MID-FLSLUT                                    
142100           IF LINE-IX = LINE-IX-MAX                                       
142200              PERFORM EB-DISPATCH-ORDER-LINE                              
142300              MOVE ZERO TO LINE-IX                                        
142400           END-IF                                                         
142500        ELSE                                                              
142600           MOVE JAA TO 4252-MID-FLSLUT                                    
142700           PERFORM EB-DISPATCH-ORDER-LINE                                 
142800        END-IF                                                            
142900        ADD 1 TO PART-IX                                                  
143000                 LINE-IX                                                  
143100     END-PERFORM                                                          
143200     .                                                                    
143300                                                                          
143400                                                                          
143500 EA-GET-PART-INFO SECTION.                                                
143600     MOVE 'EA-GET-PART-INFO' TO CURRENT-SECTION                           
143700                                                                          
143800     IF WS-IDSYSTEM = WS-IDSYSTEM-LYNK                                    
143900        MOVE REQU-IDLEVART (PART-IX) TO W-SEQB-IDLEVART                   
144000        PERFORM IMS-GU-WDF501-BSEQ                                        
144100                                                                          
144200        MOVE XART-IDARTNR      TO W-IDARTNR                               
144300     ELSE                                                                 
144400        MOVE ZERO TO W-BLANKS                                             
144500        INSPECT FUNCTION REVERSE(REQU-IDLEVART (PART-IX))                 
144600                TALLYING W-BLANKS FOR LEADING SPACES                      
144700        COMPUTE W-LENGTH = 30 - W-BLANKS                                  
144800        MOVE REQU-IDLEVART (PART-IX) (1:W-LENGTH)                         
144900                               TO W-IDARTNR                               
145000     END-IF                                                               
145100     PERFORM IMS-GU-WDK601                                                
145200     .                                                                    
145300 EB-DISPATCH-ORDER-LINE SECTION.                                          
145400                                                                          
145500     COMPUTE MSG-KVLL = LENGTH OF 4252-MID-W4I25201 + 17                  
145600     MOVE 4252-MID-W4I25201   TO MSG-INDATA-MINUS-1-TRANSKOD              
145700     MOVE 'W4T252X '          TO MSG-KDTRANS-1                            
145800     MOVE '4252'              TO MSG-IDTRANS-1                            
145900     MOVE '1'                 TO MSG-KDMFSFOR-1                           
146000                                                                          
146100     CALL W006KOM USING MSG-PCB                                           
146200                        0693X-PCB                                         
146300                        WDP8-PCB                                          
146400                        MSG-KOM-WMSGKOM                                   
146500                        MSG-IO-AREA                                       
146600                                                                          
146700     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
146800        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
146900          DELIMITED BY SIZE  INTO ERROR-TEXT                              
147000        DISPLAY  ERROR-TEXT                                               
147100        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
147200     END-IF                                                               
147300                                                                          
147400*    -- CLEAR ORDER-LINE AREA BEFORE NEXT LINE IS ADDED                   
147500     MOVE SPACE               TO 4252-MID-W4I25201                        
147600     .                                                                    
147700                                                                          
147800                                                                          
147900 F-CREATE-ORDER-RESPONSE SECTION.                                         
148000     MOVE 'F-CR-ORDER-RESP ' TO CURRENT-SECTION                           
148100                                                                          
148200     MOVE ORDER-CREATED      TO RESP-IDMFSINF                             
148300     MOVE SPACE              TO RESP-TEMFSINF                             
148400                                                                          
148500     MOVE WS-IDDISTR-NUM     TO RESP-IDDISTR                              
148600     MOVE WS-IDKUNDNR-NUM    TO RESP-IDKUNDNR                             
148700     MOVE WS-IDORDNR         TO RESP-IDORDNR7                             
148800                                                                          
148900     PERFORM FA-GET-LOCAL-DATE                                            
149000     .                                                                    
149100                                                                          
149200                                                                          
149300 FA-GET-LOCAL-DATE SECTION.                                               
149400     MOVE 'FA-GET-LOCAL-DT ' TO CURRENT-SECTION                           
149500                                                                          
149600     PERFORM IMS-GU-WDB201                                                
149700                                                                          
149800     MOVE ALL '+'             TO MSGI-WMSGINIT                            
149900     MOVE '013'               TO MSGI-KDCALL                              
150000     MOVE 'WIDDC   '          TO MSGI-IDUSER                              
150100     IF WS-KDORDKL = 0                                                    
150200       MOVE GMT-IDDC-VOR(1) TO MSGI-IDUSER(6:2)                           
150300     ELSE                                                                 
150400       IF WS-KDORDKL = 1                                                  
150500         MOVE GMT-IDDC-DAY(1) TO MSGI-IDUSER(6:2)                         
150600       ELSE                                                               
150700         IF WS-KDORDKL > 1                                                
150800           MOVE GMT-IDDC-BULK(1) TO MSGI-IDUSER(6:2)                      
150900         END-IF                                                           
151000       END-IF                                                             
151100     END-IF                                                               
151200     MOVE '9314'              TO MSGI-IDTRANS                             
151300                                                                          
151400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
151500                                                                          
151600     MOVE MSGI-TILOKDAT       TO WS-TILOKDAT                              
151700     MOVE WS-TILOKDAT         TO RESP-TIREGDAT                            
151800     .                                                                    
151900                                                                          
152000                                                                          
152100*    --- DISPATCHER SECTIONS                                              
152200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
152300                                                                          
152400     MOVE 'GETARG'               TO SUB-KDFUNC                            
152500     MOVE 'CARPARTS.PULS.APIORDERENTRY'     TO SUB-ADDISPABS              
152600     MOVE SPACE TO REQU-AREA                                              
152700     MOVE 999                         TO REQU-KVRADER                     
152800     MOVE LENGTH OF REQU-AREA         TO SUB-KVDLEN                       
152900                                                                          
153000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
153100                                                                          
153200     IF SUB-KDRC > 0                                                      
153300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
153400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
153500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
153600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
153700     END-IF                                                               
153800     .                                                                    
153900     SKIP3                                                                
154000 S02-RETURN-RESPONSE SECTION.                                             
154100                                                                          
154200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
154300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
154400                                                                          
154500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
154600                                                                          
154700     IF SUB-KDRC > 0                                                      
154800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
154900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
155000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
155100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
155200     END-IF                                                               
155300     .                                                                    
155400                                                                          
155500 S10-OPEN-DP SECTION.                                                     
155600                                                                          
155700     MOVE 'OPEN'                        TO SEND-KDFUNC                    
155800     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
155900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
156000                         SEND-OPEN-AREA                                   
156100     IF SEND-KDRC > 0                                                     
156200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
156300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
156400       DELIMITED BY SIZE INTO FELTEXT                                     
156500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
156600     END-IF                                                               
156700     .                                                                    
156800     EJECT                                                                
156900 S10-PUT-HDR SECTION.                                                     
157000                                                                          
157100     MOVE 1                       TO MAIL-REQU-IDMSGVER                   
157200     MOVE 'R'                     TO MAIL-REQU-KDPGMACT                   
157300     MOVE IDPGM                   TO MAIL-REQU-IDUSER                     
157400     MOVE 'POSTALCODE'            TO HDR-IDOUTTYPE                        
157500     MOVE 'MISSING'               TO HDR-IDOUTREC                         
157600     MOVE SPACE                   TO HDR-IDLIST                           
157700     MOVE 'PUT'                   TO SEND-KDFUNC                          
157800     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
157900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
158000                         SEND-KVDLEN                                      
158100                         HDR-AREA                                         
158200                                                                          
158300     IF SEND-KDRC > ZERO                                                  
158400       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
158500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
158600       DELIMITED BY SIZE INTO FELTEXT                                     
158700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
158800     END-IF                                                               
158900     .                                                                    
159000     EJECT                                                                
159100 S10-PUT-LINE SECTION.                                                    
159200                                                                          
159300     MOVE 'PUT'                           TO SEND-KDFUNC                  
159400     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
159500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
159600                         SEND-KVDLEN                                      
159700                         SEND-RAD                                         
159800     IF SEND-KDRC > ZERO                                                  
159900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
160000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
160100       DELIMITED BY SIZE INTO FELTEXT                                     
160200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
160300     END-IF                                                               
160400     .                                                                    
160500     EJECT                                                                
160600 S10-CLOSE-DP SECTION.                                                    
160700                                                                          
160800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
160900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
161000                                                                          
161100     IF SEND-KDRC > 0                                                     
161200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
161300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
161400       DELIMITED BY SIZE INTO FELTEXT                                     
161500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
161600     END-IF                                                               
161700     .                                                                    
161800                                                                          
161900                                                                          
162000                                                                          
162100 IMS-GU-WDB201 SECTION.                                                   
162200     MOVE 'IMS-GU-WDB201   ' TO CURRENT-IMS-SECTION                       
162300                                                                          
162400     MOVE SPACE               TO ALL-SSA                                  
162500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
162600          DELIMITED BY SIZE INTO SSA1                                     
162700     MOVE '  GE'              TO GOOD-STATUSCODES                         
162800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
162900     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
163000     PERFORM IMS-STATUSCHECK                                              
163100     .                                                                    
163200                                                                          
163300                                                                          
163400 IMS-GU-WDF501-BSEQ SECTION.                                              
163500     MOVE 'IMS-GU-WDF501-BS' TO CURRENT-IMS-SECTION                       
163600                                                                          
163700     MOVE SPACE                 TO ALL-SSA                                
163800     STRING 'WDF501  (WDF5BSEQ =' W-WDF5BSEQ ')'                          
163900            DELIMITED BY SIZE INTO SSA1                                   
164000     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
164100     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF501 SSA1                    
164200     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
164300     PERFORM IMS-STATUSCHECK                                              
164400     .                                                                    
164500                                                                          
164600                                                                          
164700 IMS-GU-WDK601  SECTION.                                                  
164800     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
164900                                                                          
165000     MOVE SPACE                 TO ALL-SSA                                
165100     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
165200            DELIMITED BY SIZE INTO SSA1                                   
165300     MOVE '  GE'                TO GOOD-STATUSCODES                       
165400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
165500     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
165600     PERFORM IMS-STATUSCHECK                                              
165700     .                                                                    
165800                                                                          
165900 IMS-GNP-WDK611  SECTION.                                                 
166000     MOVE 'IMS-GNP-WDK611  ' TO CURRENT-IMS-SECTION                       
166100                                                                          
166200     MOVE SPACE                 TO ALL-SSA                                
166300     MOVE 'WDK611 '             TO SSA1                                   
166400     MOVE '  GE'                TO GOOD-STATUSCODES                       
166500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
166600     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
166700     PERFORM IMS-STATUSCHECK                                              
166800     .                                                                    
166900                                                                          
167000 IMS-GNP-WDK621  SECTION.                                                 
167100     MOVE 'IMS-GNP-WDK621  ' TO CURRENT-IMS-SECTION                       
167200                                                                          
167300     MOVE SPACE                 TO ALL-SSA                                
167400     MOVE 'WDK621 '             TO SSA1                                   
167500     MOVE '  GE'                TO GOOD-STATUSCODES                       
167600     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
167700     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
167800     PERFORM IMS-STATUSCHECK                                              
167900     .                                                                    
168000                                                                          
168100 IMS-GU-WDK611  SECTION.                                                  
168200     MOVE 'IMS-GU-WDK611  '  TO CURRENT-IMS-SECTION                       
168300                                                                          
168400     MOVE SPACE                 TO ALL-SSA                                
168500     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
168600            DELIMITED BY SIZE INTO SSA1                                   
168700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
168800            DELIMITED BY SIZE INTO SSA2                                   
168900     MOVE '  GE'                TO GOOD-STATUSCODES                       
169000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
169100     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
169200     PERFORM IMS-STATUSCHECK                                              
169300     .                                                                    
169400                                                                          
169500                                                                          
169600 IMS-GNP-WDK629  SECTION.                                                 
169700     MOVE 'IMS-GNP-WDK629  ' TO CURRENT-IMS-SECTION                       
169800                                                                          
169900     MOVE 'WDK629  '            TO SSA1                                   
170000     MOVE '  GE'                TO GOOD-STATUSCODES                       
170100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK629 SSA1                   
170200     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
170300     PERFORM IMS-STATUSCHECK                                              
170400     .                                                                    
170500                                                                          
170600 IMS-GHU-WDK611  SECTION.                                                 
170700     MOVE 'IMS-GHU-WDK611  ' TO CURRENT-IMS-SECTION                       
170800                                                                          
170900     MOVE SPACE                 TO ALL-SSA                                
171000     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
171100            DELIMITED BY SIZE INTO SSA1                                   
171200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
171300            DELIMITED BY SIZE INTO SSA2                                   
171400     MOVE '  GE'                TO GOOD-STATUSCODES                       
171500     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
171600     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
171700     PERFORM IMS-STATUSCHECK                                              
171800     .                                                                    
171900                                                                          
172000 IMS-REPL-WDK611  SECTION.                                                
172100     MOVE 'IMS-REPL-WDK611 ' TO CURRENT-IMS-SECTION                       
172200                                                                          
172300     MOVE '  '                  TO GOOD-STATUSCODES                       
172400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
172500     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
172600     PERFORM IMS-STATUSCHECK                                              
172700     .                                                                    
172800                                                                          
172900 IMS-GU-WDGX4134  SECTION.                                                
173000     MOVE 'IMS-GU-WDGX4134 ' TO CURRENT-IMS-SECTION                       
173100                                                                          
173200     MOVE SPACE                 TO ALL-SSA                                
173300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
173400          DELIMITED BY SIZE INTO SSA1                                     
173500     STRING 'WDGX4134(IDLANDX2 =' W-IDLANDX2                              
173600                    '&ADPOSTNF<=' W-ADPOSTNR                              
173700                    '&ADPOSTNT>=' W-ADPOSTNR                              
173800                    '!IDLANDX2 =' W-IDLANDX2                              
173900                    '&ADPOSTNF =' W-ADPOSTNR-DEF                          
174000                    '&ADPOSTNT =' W-ADPOSTNR-DEF ')'                      
174100            DELIMITED BY SIZE INTO SSA2                                   
174200     MOVE '  GE'                  TO GOOD-STATUSCODES                     
174300     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX4134 SSA1 SSA2             
174400     MOVE WDR5-STATUS-CODE        TO STATUS-WS                            
174500     PERFORM IMS-STATUSCHECK                                              
174600     .                                                                    
174700                                                                          
174800 IMS-GU-WDQ2C  SECTION.                                                   
174900     MOVE 'IMS-GU-WDQ2C    ' TO CURRENT-IMS-SECTION                       
175000                                                                          
175100     STRING 'WDQ2C1  (WDQ2C1KY =' W-WDQ2C1KY-X ')'                        
175200          DELIMITED BY SIZE INTO SSA1                                     
175300     MOVE '  GE'             TO GOOD-STATUSCODES                          
175400     CALL CBLTDLI USING GU WDQ2C-PCB DLI-IO-WDQ2C1 SSA1                   
175500     MOVE WDQ2C-STATUS-CODE  TO STATUS-WS                                 
175600     PERFORM IMS-STATUSCHECK                                              
175700     .                                                                    
175800                                                                          
175900 IMS-GU-WDK711  SECTION.                                                  
176000     MOVE 'IMS-GU-WDK711   ' TO CURRENT-IMS-SECTION                       
176100                                                                          
176200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
176300          DELIMITED BY SIZE INTO SSA1                                     
176400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
176500          DELIMITED BY SIZE INTO SSA2                                     
176600     MOVE '  GE'             TO GOOD-STATUSCODES                          
176700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
176800     MOVE WDK7-STATUS-CODE   TO STATUS-WS                                 
176900     PERFORM IMS-STATUSCHECK                                              
177000     .                                                                    
177100                                                                          
177200 IMS-GHU-WDK711  SECTION.                                                 
177300     MOVE 'IMS-GHU-WDK711  ' TO CURRENT-IMS-SECTION                       
177400                                                                          
177500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
177600          DELIMITED BY SIZE INTO SSA1                                     
177700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
177800          DELIMITED BY SIZE INTO SSA2                                     
177900     MOVE '  GE'             TO GOOD-STATUSCODES                          
178000     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
178100     MOVE WDK7-STATUS-CODE   TO STATUS-WS                                 
178200     PERFORM IMS-STATUSCHECK                                              
178300     .                                                                    
178400                                                                          
178500 IMS-REPL-WDK711 SECTION.                                                 
178600     MOVE 'IMS-REPL-WDK711 ' TO CURRENT-IMS-SECTION                       
178700                                                                          
178800     MOVE '  '               TO GOOD-STATUSCODES                          
178900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
179000     MOVE WDK7-STATUS-CODE   TO STATUS-WS                                 
179100     PERFORM IMS-STATUSCHECK                                              
179200     .                                                                    
179300                                                                          
179400 IMS-GU-WDB601    SECTION.                                                
179500     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
179600                                                                          
179700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
179800          DELIMITED BY SIZE INTO SSA1                                     
179900     MOVE '  GE'             TO GOOD-STATUSCODES                          
180000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
180100     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
180200     PERFORM IMS-STATUSCHECK                                              
180300     .                                                                    
180400                                                                          
180500 IMS-GU-WDB616    SECTION.                                                
180600     MOVE 'IMS-GU-WDB616  '  TO CURRENT-IMS-SECTION                       
180700                                                                          
180800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
180900          DELIMITED BY SIZE INTO SSA1                                     
181000     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-B6-X ')'                     
181100          DELIMITED BY SIZE INTO SSA2                                     
181200     MOVE '  GE'             TO GOOD-STATUSCODES                          
181300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
181400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
181500     PERFORM IMS-STATUSCHECK                                              
181600     .                                                                    
181700                                                                          
181800 IMS-GU-GMTC-WDB501 SECTION.                                              
181900     MOVE 'IMS-GU-GMTC-WDB501 ' TO CURRENT-IMS-SECTION                    
182000                                                                          
182100     STRING 'WDB501  (WDB501KY =' W-WDB501KY-X                            
182200                    '!WDB501KY =' W-WDB501KY-DEF-X  ')'                   
182300          DELIMITED BY SIZE INTO SSA1                                     
182400     MOVE '  GE'             TO GOOD-STATUSCODES                          
182500     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-WDB501 SSA1                    
182600     MOVE GMTC-STATUS-CODE   TO STATUS-WS                                 
182700     PERFORM IMS-STATUSCHECK                                              
182800     .                                                                    
182900                                                                          
183000 IMS-STATUSCHECK SECTION.                                                 
183100                                                                          
183200     SET STATUS-IX                 TO 1                                   
183300     SEARCH GOOD-STATUS                                                   
183400       AT END                                                             
183500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
183600         DELIMITED BY SIZE INTO ERROR-TEXT                                
183700         CALL FELLOG                                                      
183800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
183900         CONTINUE                                                         
184000     END-SEARCH                                                           
184100     .                                                                    
