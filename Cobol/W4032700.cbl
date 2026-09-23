000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4032700.                                                
000400 AUTHOR.         LARSSON THOMAS.                                          
000500 DATE-WRITTEN.   23/10/22.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*    FUNCTION:                                                            
000900*                                                                         
001000*        THE PROGRAM UPDATES   WDE6                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W4T327                                              
001400*        REQU:        W40327I1                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001700*        RESP:        W40327O1                                            
001800*                                                                         
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W4032700'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
002900 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003000 77  FILLER                      PIC X(08)   VALUE 'CURRENT'.             
003100 77  WS-CURRENT-SECTION          PIC X(64)   VALUE SPACE.                 
003200 77  FILLER                      PIC X(08)   VALUE 'IMS-SEC'.             
003300 77  WS-CURRENT-IMS-SECTION      PIC X(64)   VALUE SPACE.                 
003400                                                                          
003500*    --- WORK FIELDS                                                      
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  YES                         PIC X       VALUE 'Y'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004000 77  FIRST-SW                    PIC X       VALUE 'Y'.                   
004100 77  KDKOLLI-SW                  PIC X       VALUE 'N'.                   
004200 77  VKORDBTO-SW                 PIC X       VALUE 'N'.                   
004300 77  VLORDBTO-SW                 PIC X       VALUE 'N'.                   
004400 77  RAETT                       PIC X       VALUE 'R'.                   
004500 77  FEL                         PIC X       VALUE 'F'.                   
004600 77  WS-MAX-500-RADER            PIC 9(5)    VALUE 500.                   
004700 77  LINE-IX                     PIC 9(3)    VALUE ZERO.                  
004800 77  MAX-TAB                     PIC 9(2)    VALUE ZERO.                  
004900 77  MAX-RAD                     PIC 9(3)    VALUE ZERO.                  
005000 77  MAX-LINES                   PIC S9(9)   VALUE +50  COMP-3.           
005100 77  FILLER                      PIC X(08)   VALUE 'AAAAAAAA'.            
005200 77  WS-ABSTRACT-ADRESS          PIC X(50)                                
005300            VALUE 'CARPARTS.LDC.CASEREPORTING5'.                          
005400                                                                          
005500 77    TMS-IX                    PIC S9(9)   VALUE +0   COMP-3.           
005600 77    RADIND                    PIC S9(9)   VALUE +0   COMP-3.           
005700 77    RADL                      PIC S9(9)   VALUE +0   COMP-3.           
005800 77    MAX-RADINDX               PIC S9(9)   VALUE +50  COMP-3.           
005900 77  FILLER                      PIC X(08)  VALUE 'A2A2A2A2'.             
006000 77    CNT                       PIC S9(9)  VALUE +0   COMP-3.            
006100 77    FILLER                    PIC  X(8)  VALUE 'SUBKVDLE'.             
006200 77    ACC-ORAD-VKORDNTO         PIC 9(6)V9(3) VALUE ZERO.                
006300 77    WS-ACC-ORAD-VKORDNTO      PIC 9(6)V9(3) VALUE ZERO.                
006400 01    WS-ORAD-VKORDN-ED         PIC Z(5)9.99.                            
006500 01    WS-ORAD-VKORDNTO-CHAR     PIC X(10).                               
006500 01    WS-IDUSER                 PIC 9(08).                               
006600*                                                                         
006700 77  FILLER                      PIC X(08)   VALUE 'BBBBBBBB'.            
006800 01  WS-KDMATT                   PIC X.                                   
006900     88 US-MEASUREMENT           VALUE 'U'.                               
007000     88 SIS-MEASUREMENT          VALUE 'S'.                               
007100*                                                                         
007200 01 W-RESP-AREA.                                                          
007300   03 W-RESP-RAD           OCCURS 500 TIMES.                              
007400     07 W-RESP-IDPRODNR    PIC 9(7).                                      
007500     07 W-RESP-IDKOLLI     PIC 9(5).                                      
007600*                                                                         
007700 01 WS-COPY-WDE611.                                                       
007800*    03  -COPY WDE611 -PRE WS-COPY-.                                      
007900*                                                                         
008000*                                                                         
008100 01 WS-COPY-WDE421.                                                       
008200*    03  -COPY WDE421 -PRE WS-COPY-.                                      
008300*                                                                         
008400 77    WS-WEIGHT                 PIC X(01).                               
008500       88  WEIGHT-MISMATCH       VALUE 'J'.                               
008600 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
008700 77    FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.            
008800                                                                          
008900 77    WS-TIDPUNKT               PIC 9(8)   VALUE ZERO.                   
009000 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
009100 77    WS-TMS-IDPRODNR           PIC X(7)   VALUE SPACE.                  
009200 77    FILLER                    PIC X(8)   VALUE 'FFFFFFFF'.             
009300 77    WS-DIKOLLIL-NEW           PIC 9(4) VALUE ZERO.                     
009400 77    WS-DIKOLLIB-NEW           PIC 9(4) VALUE ZERO.                     
009500 77    WS-DIKOLLIH-NEW           PIC 9(4) VALUE ZERO.                     
009600 77    WS-DIKOLLIL               PIC 9(4) VALUE ZERO.                     
009700 77    WS-DIKOLLIB               PIC 9(4) VALUE ZERO.                     
009800 77    WS-DIKOLLIH               PIC 9(4) VALUE ZERO.                     
009900 77    WS-VKTARA-OLD             PIC 9(6)V9(1)  VALUE ZERO.               
010000 77    WS-VKTARA-NEW             PIC 9(6)V9(1)  VALUE ZERO.               
010100 77    WS-VOL-NEW                PIC S9(4)V9(3) VALUE ZERO.               
010200 77    KDRC-DISPLAY              PIC 9(4)    VALUE ZERO.                  
010300 77    WS-IDDISTR-NUM            PIC 9(4)    VALUE ZERO.                  
010400 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
010500 77    WS-KDPRTVAL-FS            PIC X(2)    VALUE SPACE.                 
010600 77    WS-KDPRTVAL-ADR           PIC X(2)    VALUE SPACE.                 
010700 77    WS-VKORDBTO-CONV          PIC S9(6)V9 VALUE ZERO.                  
010800 77    WS-VOL-CONV               PIC S9(4)V9(3) VALUE ZERO.               
010900                                                                          
011000 01  FILLER                      PIC X(16)   VALUE 'WS-ARB-TAB'.          
011100 01  WS-ARB-TAB.                                                          
011200     03  WS-TABSTEG OCCURS 500.                                           
011300         05  WS-VKORDBTO         PIC S9(6)V9 VALUE ZERO.                  
011400                                                                          
011500 01  WS-IDPRTLST.                                                         
011600     03 WS-SYSTDEL               PIC X(1).                                
011700     03 WS-LISTTYP               PIC X(2).                                
011800     03 WS-DC                    PIC X(2).                                
011900     03 WS-KDPRT                 PIC X(3).                                
012000                                                                          
012100 01  TABENTRY-PARM.                                                       
012200     03  STEGLAANGD              PIC S9(9) COMP VALUE 22.                 
012300     03  ANTAL                   PIC S9(9) COMP.                          
012400     03  NYCKELLAANGD            PIC S9(9) COMP VALUE  2.                 
012500                                                                          
012600 01  TAB-IX                    PIC S9(9)  COMP-3 VALUE ZERO.              
012700                                                                          
012800 01  SORT-TABELL.                                                         
012900     03  TAB-RAD OCCURS 1100.                                             
013000       05 TAB-IDPRODNR         PIC S9(7)  COMP-3.                         
013100       05 TAB-IDPLKLST         PIC S9(3)  COMP-3.                         
013200       05 TAB-IDDISTR          PIC S9(5)  COMP-3.                         
013300       05 TAB-IDKUNDNR         PIC S9(7)  COMP-3.                         
013400       05 TAB-IDORDNR7         PIC 9(7).                                  
013500       05 TAB-IDLOPNR-ORD      PIC S9(3)  COMP-3.                         
013600*                                                                         
013700     EJECT                                                                
013800 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
013900 01     FILLER REDEFINES TEST-IDDISTR.                                    
014000*  03   -COPY WWDIST03.                                                   
014100     SKIP2                                                                
014200 01     FILLER REDEFINES TEST-IDDISTR.                                    
014300*  03   -COPY WWDIST21.                                                   
014400     SKIP2                                                                
014500 01     FILLER REDEFINES TEST-IDDISTR.                                    
014600*  03   -COPY WWDIST47.                                                   
014700     EJECT                                                                
014800 01     FILLER REDEFINES TEST-IDDISTR.                                    
014900*  03   -COPY WWDIST85.                                                   
015000     EJECT                                                                
015100*01  WDATAREA      -COPY WDATAREA.                                        
015200     EJECT                                                                
015300*                                                                         
015400*01    -COPY WWDC99                                                       
015500*                                                                         
015600*    --- PARAMETRAR TILL ABEND                                            
015700                                                                          
015800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
016000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
016100 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
016200     SKIP3                                                                
016300*    --- PARAMETERS TO WZ01SEND                                           
016400 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
016500     SKIP3                                                                
016600*01  -COPY WZ01SEND                                                       
016700     EJECT                                                                
016800 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
016900 01  HDR-AREA.                                                            
017000*    03  -COPY WZ01REQU  -PRE HDR-                                        
017100*    03  -COPY WZ04HDR                                                    
017200*                                                                         
017300 01  FILLER                      PIC X(9)    VALUE 'SEND-AREA'.           
017400 01  SEND-AREA                   PIC X(100)  VALUE SPACE.                 
017500*                                                                         
017600 77  DUPLICATE-CHECK             PIC X       VALUE 'N'.                   
017700     88  DUP-KOLLI                           VALUE 'J'.                   
017800     88  NO-DUP-KOLLI                        VALUE 'N'.                   
017900                                                                          
018000 77  KEYS-SW                     PIC X       VALUE 'J'.                   
018100     88  KEYS-OK                             VALUE 'J'.                   
018200     88  KEYS-WRONG                          VALUE 'N'.                   
018300*                                                                         
018400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
018500     88  INDATA-OK                           VALUE 'J'.                   
018600     88  INDATA-FEL                          VALUE 'N'.                   
018700*                                                                         
018800***************************************************************           
018900*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
019000*                                                                         
019100                                                                          
019200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
019300 01  GENERAL-SUBPROGRAMS.                                                 
019400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
019700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
019800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB'.             
020000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
020100     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
020200     03  W403TMS1                PIC X(8)    VALUE 'W403TMS1'.            
020300     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
020400     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
020500*        PRISFRÅGA                                                        
020600     SKIP2                                                                
020700*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
020800 01  FILLER                      PIC X(16) VALUE 'WWOMVAND '.             
020900*   -COPY WWOMVAND                                                        
021000     SKIP3                                                                
021100 01  FILLER                      PIC X(16) VALUE 'W006PRT'.               
021200*01  -COPY W006PRT                                                        
021300     SKIP3                                                                
021400 01  FILLER                      PIC X(16) VALUE 'SUB-CONTROL'.           
021500*01  -COPY WZ01SUB                                                        
021600     SKIP3                                                                
021700 01  FILLER                      PIC X(08) VALUE 'WL01TIDZ'.              
021800*01  -COPY WL01TIDZ                                                       
021900     EJECT                                                                
022000 01  FILLER                      PIC X(16)   VALUE 'WDECAREA '.           
022100*01  -COPY WDECAREA                                                       
022200     EJECT                                                                
022300*TMS PACKNING INFO                                                        
022400 01  FILLER                      PIC X(8)   VALUE  'W403TMS1'.            
022500*    -COPY W403TMS1                                                       
022600 01    FILLER                 PIC X(16) VALUE 'MID W4I33301 MID'.         
022700 01  4333-MID-IO-AREA.                                                    
022800                                                                          
022900       03  4333-MID-KVLL         PIC S9(4)   COMP SYNC.                   
023000       03  FILLER                PIC X(2)    VALUE LOW-VALUE.             
023100       03  FILLER                PIC X(8)    VALUE 'W4T333  '.            
023200       03  4333-IDTRANS          PIC X(4)    VALUE '4327'.                
023300       03  4333-MID-KDMFSFOR     PIC X.                                   
023400*      03  MID -COPY W4I33301  -PRE 4333-.                                
023500     SKIP2                                                                
023600   03  FILLER                 PIC X(16) VALUE 'MID W4I34101 MID'.         
023700   01  4341-MID-IO-AREA.                                                  
023800                                                                          
023900       03  4341-MID-LL           PIC S9(4)   COMP SYNC.                   
024000       03  FILLER                PIC X(2)    VALUE LOW-VALUE.             
024100       03  FILLER                PIC X(8)    VALUE 'W4T341  '.            
024200       03  4341-IDTRANS          PIC X(4)    VALUE '4327'.                
024300       03  4341-MID-KDMFSFOR     PIC X.                                   
024400       03  4341-MID-DATA-AREA    PIC X(61).                               
024500*      03  MID -COPY W4I34101 -RED 4341-MID-DATA-AREA -PRE 4341-.         
024600     SKIP2                                                                
024700*                                                                         
024800*01  COPY WMSGINIT                                                        
024900     SKIP3                                                                
025000*                                                                         
025100*    --- PARAMETERS TO ABEND                                              
025200                                                                          
025300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
025400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
025500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
025600     EJECT                                                                
025700*                                                                         
025800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
025900     SKIP3                                                                
026000 01  REQU-AREA.                                                           
026100*    03  -COPY WZ01REQU                                                   
026200*    03  -COPY W40327I1                                                   
026300     EJECT                                                                
026400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
026500     SKIP3                                                                
026600 01  RESP-AREA.                                                           
026700*    03  -COPY WZ01RESP                                                   
026800*    03  -COPY W40327O1                                                   
026900                                                                          
027000*                                                                         
027100 01  HEADER.                                                              
027200     03  FILLER.                                                          
027300        05  FILLER               PIC X(50)  VALUE                         
027400            'Gross weight input from screen does not match with'.         
027500        05  FILLER               PIC X(50)  VALUE                         
027600            ' weight in PULS system for below ORDER details.'.            
027700     03  FILLER.                                                          
027800        05  FILLER               PIC X(50)  VALUE                         
027900            'Please check the parts and emballage weight includ'.         
028000        05  FILLER               PIC X(50)  VALUE                         
028100            'ed in this case stated below.All weight are in KG.'.         
028200     03  FILLER.                                                          
028300        05  FILLER               PIC X(100) VALUE SPACE .                 
028400     03  FILLER.                                                          
028500        05  FILLER               PIC X(5)   VALUE SPACE.                  
028600        05  FILLER               PIC X(8)   VALUE 'District'.             
028700        05  FILLER               PIC X(4)   VALUE X'05050505'.            
028800        05  HEAD-DIST            PIC Z(5)   VALUE ZERO.                   
028900        05  FILLER               PIC X(78)  VALUE SPACE.                  
029000     03  FILLER.                                                          
029100        05  FILLER               PIC X(5)   VALUE SPACE.                  
029200        05  FILLER               PIC X(8)   VALUE 'Customer'.             
029300        05  FILLER               PIC X(4)   VALUE X'05050505'.            
029400        05  HEAD-IDKUNDNR        PIC Z(6)9  VALUE ZERO.                   
029500        05  FILLER               PIC X(76)  VALUE SPACE.                  
029600     03  FILLER.                                                          
029700        05  FILLER               PIC X(5)   VALUE SPACE.                  
029800        05  FILLER               PIC X(12)  VALUE 'Order number'.         
029900        05  FILLER               PIC X(3)   VALUE X'050505'.              
030000        05  HEAD-IDORDER         PIC Z(5)   VALUE ZERO.                   
030100        05  FILLER               PIC X(75)  VALUE SPACE.                  
030200     03  FILLER.                                                          
030300        05  FILLER               PIC X(5)   VALUE SPACE.                  
030400        05  FILLER               PIC X(11)  VALUE 'Case number'.          
030500        05  FILLER               PIC X(4)   VALUE X'05050505'.            
030600        05  HEAD-IDKOLLI         PIC Z(5)   VALUE SPACE.                  
030700        05  FILLER               PIC X(75)  VALUE SPACE.                  
030800     03  FILLER.                                                          
030900        05  FILLER               PIC X(5)   VALUE SPACE.                  
031000        05  FILLER               PIC X(9)   VALUE 'Case code'.            
031100        05  FILLER               PIC X(4)   VALUE X'05050505'.            
031200        05  HEAD-KDKOLLI         PIC X(82)  VALUE SPACE.                  
031300     03  FILLER.                                                          
031400        05  FILLER               PIC X(5)   VALUE SPACE.                  
031500        05  FILLER               PIC X(10)  VALUE 'Prodnr    '.           
031600        05  FILLER               PIC X(4)   VALUE X'05050505'.            
031700        05  HEAD-IDPRODNR        PIC Z(8)   VALUE ZERO.                   
031800        05  FILLER               PIC X(73)  VALUE SPACE.                  
031810     03  FILLER.                                                          
031820        05  FILLER               PIC X(5)   VALUE SPACE.                  
031830        05  FILLER               PIC X(10)  VALUE 'Picking ID'.           
031840        05  FILLER               PIC X(4)   VALUE X'05050505'.            
031850        05  HEAD-IDPLKLST        PIC Z(8)   VALUE ZERO.                   
031860        05  FILLER               PIC X(73)  VALUE SPACE.                  
031900     03  FILLER.                                                          
032000        05  FILLER               PIC X(5)   VALUE SPACE.                  
032100        05  FILLER               PIC X(17)  VALUE                         
032200                                            'Input case weight'.          
032300        05  FILLER               PIC X(3)   VALUE X'050505'.              
032400        05  HEAD-VKORDBTO        PIC Z(6).999 VALUE ZERO.                 
032500        05  FILLER               PIC X(67)  VALUE SPACE.                  
032600     03  FILLER.                                                          
032700        05  FILLER               PIC X(5)   VALUE SPACE.                  
032800        05  FILLER               PIC X(25)  VALUE                         
032900                                 'Calculated weight for the'.             
033000        05  FILLER               PIC X(2)   VALUE X'0505'.                
033100        05  HEAD-VKARTNTO        PIC Z(6).999 VALUE ZERO.                 
033200        05  FILLER               PIC X(60)  VALUE SPACE.                  
033300     03  FILLER.                                                          
033400        05  FILLER               PIC X(5)   VALUE SPACE.                  
033500        05  FILLER               PIC X(95)  VALUE                         
033600                                     'parts in the case'.                 
034300     03  FILLER.                                                          
034400        05  FILLER               PIC X(100) VALUE SPACE.                  
034500     03  FILLER.                                                          
034600        05  FILLER               PIC X(100) VALUE 'Order line'.           
034700     03  FILLER.                                                          
034800        05  FILLER               PIC X(2)   VALUE 'No'.                   
034900        05  FILLER               PIC X(2)   VALUE X'0505'.                
035000        05  FILLER               PIC X(7)   VALUE 'Part no'.              
035100        05  FILLER               PIC X(2)   VALUE X'0505'.                
035200        05  FILLER               PIC X(6)   VALUE SPACE.                  
035300        05  FILLER               PIC X(3)   VALUE 'Qty'.                  
035400        05  FILLER               PIC X(2)   VALUE X'0505'.                
035500        05  FILLER               PIC X(22)  VALUE                         
035600                                  'Part Weight + Part emb'.               
035700        05  FILLER               PIC X(2)   VALUE X'0505'.                
035800        05  FILLER               PIC X(11)  VALUE                         
035900                                  'Part Weight'.                          
036000        05  FILLER               PIC X(3)   VALUE X'050505'.              
036100        05  FILLER               PIC X(41)  VALUE 'Location'.             
036200 01  HEADER-TAB  REDEFINES HEADER.                                        
036300     03 TAB-LINE   OCCURS 16 TIMES.                                       
036400        05 FILLER  PIC X(100).                                            
036500                                                                          
036600 01  LINEDATA.                                                            
036700     03 LINE-TAB OCCURS 50 TIMES.                                         
036800        05  LINE1-NUMBER         PIC X(5)    VALUE SPACE.                 
036900        05  FILLER               PIC X(2)    VALUE X'0505'.               
037000        05  LINE1-IDARTNR        PIC Z(9)    VALUE ZERO.                  
037100        05  FILLER               PIC X(2)    VALUE X'0505'.               
037200        05  LINE1-KVAVBART       PIC Z(9)    VALUE ZERO.                  
037300        05  FILLER               PIC X(2)    VALUE X'0505'.               
037400        05  LINE1-VKOLDNET       PIC Z(9)9.999 VALUE ZERO.                
037500        05  FILLER               PIC X(3)    VALUE X'050505'.             
037600        05  LINE1-VKNEWNET       PIC Z(9)9.999 VALUE ZERO.                
037700        05  FILLER               PIC X(3)    VALUE X'050505'.             
037800        05  LINE1-ADLAGOMR       PIC Z(3)    VALUE ZERO.                  
037900        05  FILLER               PIC X(1)    VALUE '.'.                   
038000        05  LINE1-ADGANG         PIC 9(2)    VALUE ZERO.                  
038100        05  FILLER               PIC X(1)    VALUE '.'.                   
038200        05  LINE1-ADPLATS        PIC X(5)    VALUE SPACE.                 
038300        05  FILLER               PIC X(1)    VALUE '.'.                   
038400                                                                          
038500 01  LINE-END                    PIC X(100) VALUE                         
038600        'More parts exist.Please check the case.'.                        
038700                                                                          
038800*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
038900*                                                                         
039000 01  MESAGE-CODES.                                                        
039100     03  INF-UPDATED             PIC X(3)    VALUE '001'.                 
039200     03  IS-INVALID              PIC X(3)    VALUE '023'.                 
039300     03  MUST-BE-NUMERIC         PIC X(3)    VALUE '024'.                 
039400     03  TOO-MANY-LINES          PIC X(3)    VALUE '028'.                 
039500     03  ERR-ORDER-PARTS-MISSING PIC X(3)    VALUE '041'.                 
039600     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '041'.                 
039700     03  SYSTEM-ERROR            PIC X(3)    VALUE '099'.                 
039800     03  ERR-SHOULD-NOT-BE-ZERO  PIC X(3)    VALUE '126'.                 
039900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
040000     03  WRONG-PRINTER           PIC X(3)    VALUE '347'.                 
040100     03  INF-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.                 
040200     03 ERR-ORDER-HAS-WRONG-STATUS  PIC  X(03)  VALUE '273'.              
040300     03 WEIGHT-CANNOT-BE-LESS-THAN  PIC X(3) VALUE '430'.                 
040400                                                                          
040500     SKIP3                                                                
040600*                                                                         
040700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
040800*                                                                         
040900******************************************************************        
041000*                                                                         
041100*                                                                         
041200 01  FILLER                      PIC X(16)   VALUE 'DLI-WS'.              
041300     SKIP3                                                                
041400 01  KEYS-TO-DLI.                                                         
041500     03    W-IDARTNR-X.                                                   
041600       05    W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
041700                                                                          
041800     03    W-KDSEGKEY-X.                                                  
041900       05    W-KDSEGKEY                PIC X(1)  VALUE '1'.               
042000                                                                          
042100     03    W-IDDC-X.                                                      
042200       05    W-IDDC                    PIC X(2)  VALUE SPACE.             
042300*                                                                         
042400     03  W-IDPRODNR-X.                                                    
042500         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
042600*                                                                         
042700     03  W-KDKOLLI-X.                                                     
042800         05  W-KDKOLLI           PIC X(8)    VALUE SPACE.                 
042900*                                                                         
043000     03  W-IDKOLLI-X.                                                     
043100         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
043200                                                                          
043300     03  W-WDE4FSEQ-MIN-X.                                                
043400       05  W-IDPRODNR-MIN        PIC S9(7)    VALUE ZERO COMP-3.          
043500       05  W-IDKOLLI-MIN         PIC S9(5)    VALUE ZERO COMP-3.          
043600                                                                          
043700     03  W-WDE4FSEQ-MAX-X.                                                
043800       05  W-IDPRODNR-MAX        PIC S9(7)    VALUE ZERO COMP-3.          
043900       05  W-IDKOLLI-MAX         PIC S9(5)    VALUE ZERO COMP-3.          
044000*                                                                         
044100*Q3I1                                                                     
044200     03  W-WDQ3ISEQ-X.                                                    
044300         05  W-IDDC-Q3I1         PIC  X(2)   VALUE SPACE.                 
044400         05  W-IDPRCPLK-Q3I1     PIC  X(4)   VALUE SPACE.                 
044500         05  W-IDLOTNRP-Q3I1     PIC S9(03)  VALUE ZERO COMP-3.           
044600*E611                                                                     
044700     03 W-IDPRODNR-WDE601-X.                                              
044800        05  W-IDPRODNR-WDE601    PIC S9(7)   VALUE ZERO  COMP-3.          
044900     03 W-IDKOLLI-WDE611-X.                                               
045000        05  W-IDKOLLI-WDE611     PIC S9(5)   VALUE ZERO  COMP-3.          
045100                                                                          
045200     03 W-WDE401-KUNDORDER-X.                                             
045300       05  W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
045400       05  W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
045500       05  W-401-IDKUNDRF.                                                
045600         07 W-401-IDORDNR        PIC  9(5)   VALUE ZERO.                  
045700         07 FILLER               PIC X(05)   VALUE SPACE.                 
045800       05  W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
045900       05  W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
046000                                                                          
046100     03 W-WDE421KY-X.                                                     
046200       05  W-421-IDPRODNR        PIC S9(7)   COMP-3 VALUE ZERO.           
046300       05  W-421-IDKOLLI         PIC S9(5)   COMP-3 VALUE ZERO.           
046400     03    W-WDE4F1KY.                                                    
046500       05    W-E4F-IDPRODNR   PIC S9(7)   VALUE ZERO  COMP-3.             
046600       05    W-E4F-IDKOLLI    PIC S9(5)   VALUE ZERO  COMP-3.             
046700                                                                          
046800*                                                                         
046900   03    W-4321-IDHTYP-X.                                                 
047000     05    W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                  
047100     05    W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.               
047200*                                                                         
047300   03    W-IDDC-B6-X.                                                     
047400     05    W-IDDC-B6             PIC  X(2).                               
047500*                                                                         
047600 01    IMS-WS.                                                            
047700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
047800     SKIP3                                                                
047900*                        **** STATUS-KOD FRÅN IMS                         
048000*                        **** STATUS-KOD FRÅN IMS                         
048100   03    STATUS-WS               PIC XX.                                  
048200     88    SEGMENT-FOUND                     VALUE '  '.                  
048300     88    ISRT-OK                           VALUE '  '.                  
048400     88    SEGMENT-END                       VALUE 'GB'.                  
048500     88    SEGMENT-MISSING                   VALUE 'GE'.                  
048600     88    SEGMENT-FOUND-EXISTS              VALUE 'II'.                  
048700     SKIP3                                                                
048800 01  FILLER                      PIC X(08)   VALUE 'SSA-AREA'.            
048900 01  GOOD-STATUSCODES.                                                    
049000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
049100     SKIP3                                                                
049200 01    SSA1                      PIC X(416).                              
049300 01    SSA2                      PIC X(384).                              
049400 01    SSA3                      PIC X(384).                              
049500     EJECT                                                                
049600*    --- IMS FUNCTION CODES                                               
049700*01  -COPY W0003                                                          
049800     EJECT                                                                
049900*    ---  DLI INPUT-OUTPUT AREA                                           
050000                                                                          
050100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK501'.                      
050200 01  DLI-IO-WDK501.                                                       
050300*    03  -COPY WDK501                                                     
050400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
050500 01  DLI-IO-WDE601.                                                       
050600*    03  -COPY WDE601                                                     
050700     EJECT                                                                
050800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
050900 01  DLI-IO-WDE611.                                                       
051000*    03  -COPY WDE611                                                     
051100     EJECT                                                                
051200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
051300 01  DLI-IO-WDE401.                                                       
051400*    03  -COPY WDE401                                                     
051500     EJECT                                                                
051600 01  FILLER        PIC X(16)  VALUE 'DLI-IO-WDE411'.                      
051700 01  DLI-IO-WDE411.                                                       
051800   03    WDE411   -COPY WDE411                                            
051900     EJECT                                                                
052000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE421'.                      
052100 01  DLI-IO-WDE421.                                                       
052200*    03  -COPY WDE421                                                     
052300     EJECT                                                                
052400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-E4F401'.             
052500 01  DLI-IO-E4F401.                                                       
052600*    03  -COPY WDE401 -PRE E4F-                                           
052700     EJECT                                                                
052000 01  FILLER         PIC X(16) VALUE 'DLI-IO-E4F421'.                      
052100 01  DLI-IO-E4F421.                                                       
052200*    03  -COPY WDE421 -PRE E4F-                                           
052300     EJECT                                                                
052800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ301'.                      
052900 01  DLI-IO-WDQ301.                                                       
053000*    03  -COPY WDQ301                                                     
053100*                                                                         
053200 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA4'.               
053300 01  DLI-IO-AREA4.                                                        
053400     03  IO-AREA4                PIC X(100)  VALUE SPACE.                 
053500*                                                                         
053600*    03  WLXXJK01  -COPY WDGX01      -PRE 4321-  -RED IO-AREA4.           
053700*    03  WLXXJK11  -COPY WDGX4322    -RED IO-AREA4.                       
053800*01    FILLER    PIC X(16)  VALUE 'DLI-IO-Q301'.                          
053900*                                                                         
054000 01  FILLER                      PIC X(16) VALUE 'WDB601 AREA'.           
054100 01  DLI-IO-AREA-B601.                                                    
054200*    03  -COPY WDB601                                                     
054300 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK601'.              
054400 01    DLI-IO-WDK601.                                                     
054500*      03  -COPY WDK601                                                   
054600     EJECT                                                                
054700 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK611'.              
054800 01    DLI-IO-WDK611.                                                     
054900*      03  -COPY WDK611                                                   
055000     EJECT                                                                
055100 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK711'.              
055200 01    DLI-IO-WDK711.                                                     
055300*      03  -COPY WDK711                                                   
055400     EJECT                                                                
055500 01  FILLER         PIC X(16) VALUE 'LINKAGE-SECTION:'.                   
055600 LINKAGE SECTION.                                                         
055700*01  -COPY W0009   -PRE MSG-                                              
055800*01    -COPY W0009     -PRE DISTRDOC-                                     
055900     EJECT                                                                
056000                                                                          
056100 01  TMS-CRE-PCB                 PIC X.                                   
056200 01  TMS-DEL-PCB                 PIC X.                                   
056300 01  ATAB-PCB                    PIC X.                                   
056400                                                                          
056500*01  -COPY W0008   -PRE 4333-                                             
056600     05  FILLER                  PIC X.                                   
056700                                                                          
056800*01  -COPY W0008   -PRE 4341-                                             
056900     05  FILLER                  PIC X.                                   
057000                                                                          
057100*01  -COPY W0008   -PRE WDK5-                                             
057200     05  FILLER                  PIC X.                                   
057300                                                                          
057400*01  -COPY W0008   -PRE WDE6-                                             
057500     05  FILLER                  PIC X.                                   
057600                                                                          
057700*01  -COPY W0008   -PRE WDE4-                                             
057800     05  FILLER                  PIC X.                                   
057900                                                                          
058000*01  -COPY W0008   -PRE WDE41-                                            
058100     05  FILLER                  PIC X.                                   
058200                                                                          
058300*01  -COPY W0008   -PRE XXJK-                                             
058400     05  FILLER                  PIC X.                                   
058500                                                                          
058600*01  -COPY W0008   -PRE WDB6-                                             
058700     05  FILLER                  PIC X.                                   
058800                                                                          
058900*01  -COPY W0008   -PRE WDQ3I-                                            
059000     05  FILLER                  PIC X.                                   
059100                                                                          
059200*01  -COPY W0008   -PRE WDK6-                                             
059300     05  FILLER                  PIC X.                                   
059400                                                                          
059500*01  -COPY W0008   -PRE WDK7-                                             
059600     05  FILLER                  PIC X.                                   
059700                                                                          
059800 01  TMS-1165-PCB               PIC X.                                    
059900 01  TMS-4141-PCB               PIC X.                                    
060000 01  TMS-WDB2-PCB               PIC X.                                    
060100 01  TMS-WDB6-PCB               PIC X.                                    
060200 01  TMS-WDD3-PCB               PIC X.                                    
060300 01  TMS-WDB1-PCB               PIC X.                                    
060400 01  TMS-WDE4A-PCB              PIC X.                                    
060500 01  TMS-WDE4F-PCB              PIC X.                                    
060600 01  TMS-WDQ2-PCB               PIC X.                                    
060700 01  TMS-WDQ3-PCB               PIC X.                                    
060800 01  TMS-WDK6-PCB               PIC X.                                    
060900 01  TMS-WDE6-PCB               PIC X.                                    
061000 01  TMS-WDK5-PCB               PIC X.                                    
061100 01  TMS-WDQ2C-PCB              PIC X.                                    
061200                                                                          
061300     EJECT                                                                
061400                                                                          
061500 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB                           
061600     TMS-CRE-PCB TMS-DEL-PCB 4333-PCB 4341-PCB ATAB-PCB                   
061700     WDK5-PCB WDE6-PCB WDE4-PCB WDE41-PCB                                 
061800     XXJK-PCB WDB6-PCB WDQ3I-PCB WDK6-PCB WDK7-PCB                        
061900     TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB TMS-WDB6-PCB                  
062000     TMS-WDD3-PCB TMS-WDB1-PCB TMS-WDE4A-PCB TMS-WDE4F-PCB                
062100     TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                  
062200     TMS-WDK5-PCB TMS-WDQ2C-PCB.                                          
062300                                                                          
062400 MAIN SECTION.                                                            
062500                                                                          
062600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
062700     IF SUB-KDRC = 0                                                      
062800       PERFORM A-INIT                                                     
062900       PERFORM B-KONTROLL-KEYS                                            
063000       IF KEYS-OK                                                         
063100         IF REQU-KDPGMACT = 'E'                                           
063200           PERFORM C-CHECK-INPUT                                          
063300           IF INDATA-OK                                                   
063400             PERFORM E-UPDATE-IDKOLLI                                     
063500           END-IF                                                         
063600         END-IF                                                           
063700                                                                          
063800         IF INDATA-OK                                                     
063900         OR RESP-IDMSG-INFO = 001                                         
064000           PERFORM F-READ-SHOW-INFO                                       
064100         END-IF                                                           
064200       END-IF                                                             
064300       PERFORM S02-RETURN-RESPONSE                                        
064400     END-IF                                                               
064500                                                                          
064600     MOVE ZERO TO RETURN-CODE                                             
064700     GOBACK                                                               
064800     .                                                                    
064900     EJECT                                                                
065000                                                                          
065100 A-INIT SECTION.                                                          
065200                                                                          
065300     MOVE 'STA A-INIT'  TO WS-CURRENT-SECTION                             
065400                                                                          
065500     MOVE ALL '+' TO RESP-AREA                                            
065600     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
065700                     RESP-IDMSG-INFO                                      
065800                     RESP-IDELMT-ERROR                                    
065900     MOVE '001'   TO RESP-IDMSGVER                                        
066000                                                                          
066100     MOVE ZERO    TO RESP-KVRADER-MAX1                                    
066200                                                                          
066300     MOVE REQU-FLSKRIV-CLABEL    TO RESP-FLSKRIV-CLABEL                   
066400     MOVE REQU-FLSKRIV-DELNOTE   TO RESP-FLSKRIV-DELNOTE                  
066500     MOVE FUNCTION UPPER-CASE (REQU-KDMATT)                               
066600                                 TO REQU-KDMATT                           
066700     MOVE REQU-KDMATT  TO WS-KDMATT                                       
066800                                                                          
066900     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
067000     ACCEPT WS-TIDPUNKT                   FROM TIME                       
067100                                                                          
067200     MOVE JA      TO INDATA-SW                                            
067300     MOVE JA      TO KEYS-SW                                              
067400                                                                          
067500     INITIALIZE TMS-W403TMS1                                              
067600     INITIALIZE SORT-TABELL                                               
067700                                                                          
067800     IF REQU-IDDC-KEY = 'NU' OR '++'                                      
067900* ---- FIX FOR REQUEST ERRORS                                             
068000       MOVE ERR-WRONG-KEY               TO RESP-IDELMT-ERROR              
068100       MOVE NEJ                         TO KEYS-SW                        
068200     ELSE                                                                 
068300       IF REQU-KDPGMACT = 'S' OR 'E'                                      
068400         MOVE REQU-IDDC-KEY             TO RESP-IDDC-KEY                  
068500         MOVE REQU-IDDC-KEY             TO W-IDDC-B6                      
068600                                           WS-IDDC                        
068700                                           W-IDDC                         
068800         PERFORM IMS-GU-WDB601                                            
068900       ELSE                                                               
069000         MOVE IS-INVALID                TO RESP-IDMSG-ERROR               
069100         MOVE 'KDPGMACT'                TO RESP-IDELMT-ERROR              
069200         MOVE NEJ                       TO KEYS-SW                        
069300       END-IF                                                             
069400     END-IF                                                               
069500                                                                          
069600     IF KEYS-OK                                                           
069700       MOVE '011'                       TO MSGI-KDCALL                    
069800       MOVE DCS-IDTIDZON                TO MSGI-IDTIDZON                  
069900       MOVE WS-DAGENS-DATUM             TO MSGI-TILOKDAT                  
070000       MOVE WS-TIDPUNKT                 TO MSGI-TILOKTID                  
             MOVE DCS-IDDC                    TO MSGI-IDDC                      
070100       CALL WL01TIDZ USING MSGI-WL01TIDZ                                  
070200       MOVE MSGI-TILOKDAT(1:6)          TO WS-DAGENS-DATUM                
070300       MOVE MSGI-TILOKTID(1:4)          TO WS-TIDPUNKT(1:4)               
070400       MOVE FUNCTION CURRENT-DATE(13:2) TO WS-TIDPUNKT(5:2)               
070500     END-IF                                                               
070600     .                                                                    
070700     SKIP2                                                                
070800 B-KONTROLL-KEYS          SECTION.                                        
070900     MOVE 'STA B-KONTROLL-KEYS      '  TO WS-CURRENT-SECTION              
071000                                                                          
071100     IF KEYS-OK                                                           
071200       IF REQU-IDPRC-KEY = ALL '+'                                        
071300         IF REQU-IDLOTNR-KEY NOT = ALL '+'                                
071400           MOVE IS-INVALID      TO RESP-IDMSG-ERROR                       
071500           MOVE 'IDPRC'         TO RESP-IDELMT-ERROR                      
071600           MOVE NEJ TO KEYS-SW                                            
071700         END-IF                                                           
071800       ELSE                                                               
071900         IF REQU-IDPRCBAS NUMERIC                                         
072000            IF REQU-IDPRCBAS > ZERO                                       
072100              MOVE REQU-IDPRC-KEY TO RESP-IDPRC-KEY                       
072200            ELSE                                                          
072300              MOVE ERR-SHOULD-NOT-BE-ZERO  TO RESP-IDMSG-ERROR            
072400              MOVE 'IDPRC'      TO RESP-IDELMT-ERROR                      
072500              MOVE NEJ TO KEYS-SW                                         
072600            END-IF                                                        
072700         ELSE                                                             
072800            MOVE MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                      
072900            MOVE 'IDPRC'         TO RESP-IDELMT-ERROR                     
073000            MOVE NEJ TO KEYS-SW                                           
073100         END-IF                                                           
073200       END-IF                                                             
073300     END-IF                                                               
073400                                                                          
073500     IF KEYS-OK                                                           
073600       IF REQU-IDLOTNR-KEY = ALL '+'                                      
073700         IF REQU-IDPRC-KEY NOT = ALL '+'                                  
073800           IF REQU-IDPRCBAS NUMERIC                                       
073900             MOVE IS-INVALID    TO RESP-IDMSG-ERROR                       
074000             MOVE 'IDLOTNR'     TO RESP-IDELMT-ERROR                      
074100             MOVE NEJ TO KEYS-SW                                          
074200           END-IF                                                         
074300         END-IF                                                           
074400       ELSE                                                               
074500         IF REQU-IDLOTNR-KEY NUMERIC                                      
074600            IF REQU-IDLOTNR-KEY  > ZERO                                   
074700              MOVE REQU-IDLOTNR-KEY TO RESP-IDLOTNR-KEY                   
074800            ELSE                                                          
074900              MOVE ERR-SHOULD-NOT-BE-ZERO  TO RESP-IDMSG-ERROR            
075000              MOVE 'IDLOTNR'    TO RESP-IDELMT-ERROR                      
075100              MOVE NEJ TO KEYS-SW                                         
075200            END-IF                                                        
075300         ELSE                                                             
075400            MOVE MUST-BE-NUMERIC  TO RESP-IDMSG-ERROR                     
075500            MOVE 'IDLOTNR'        TO RESP-IDELMT-ERROR                    
075600            MOVE NEJ TO KEYS-SW                                           
075700         END-IF                                                           
075800       END-IF                                                             
075900     END-IF                                                               
076000                                                                          
076100     .                                                                    
076200     SKIP2                                                                
076300 C-CHECK-INPUT SECTION.                                                   
076400     MOVE 'C-CHECK-INPUT'   TO WS-CURRENT-SECTION                         
                                                                                
082600     IF REQU-KVRADER-MAX1 NOT = ALL '+'                                   
082700       IF REQU-KVRADER-MAX1 NUMERIC                                       
082800       AND REQU-KVRADER-MAX1 > ZERO                                       
082900         MOVE REQU-KVRADER-MAX1   TO RESP-KVRADER-MAX1                    
083000       ELSE                                                               
083100         MOVE NEJ                 TO INDATA-SW                            
083200       END-IF                                                             
083300     ELSE                                                                 
083400       MOVE NEJ                   TO INDATA-SW                            
083500     END-IF                                                               
076500                                                                          
           IF INDATA-OK                                                         
076600      PERFORM CA-NOLLA-ARB-TAB                                            
076700     END-IF                                                               
076800     IF DCS-CDC                                                           
076900       IF REQU-FLSKRIV-CLABEL = 'Y'                                       
077000         IF REQU-PRTVAL-ADRESSFL = ALL '+'                                
077100             MOVE WRONG-PRINTER TO RESP-IDMSG-ERROR                       
077200             MOVE 'LBL'         TO RESP-IDELMT-ERROR                      
077300             MOVE NEJ           TO INDATA-SW                              
077400         ELSE                                                             
077500           MOVE REQU-PRTVAL-ADRESSFL TO WS-KDPRTVAL-ADR                   
077600           MOVE '4'              TO WS-SYSTDEL                            
077700           MOVE 'KF'             TO WS-LISTTYP                            
077800           MOVE WS-IDDC          TO WS-DC                                 
077900           MOVE WS-KDPRTVAL-ADR  TO WS-KDPRT                              
078000                                                                          
078100           MOVE 001              TO PRT-KDCALL                            
078200           MOVE WS-IDPRTLST      TO PRT-IDPRTLST                          
078300                                                                          
078400           CALL W006PRT USING PRT-W006PRT                                 
078500                                                                          
078600           IF PRT-KDSVAR = RAETT                                          
078700             CONTINUE                                                     
078800           ELSE                                                           
078900              MOVE WRONG-PRINTER TO RESP-IDMSG-ERROR                      
079000              MOVE 'LBL'         TO RESP-IDELMT-ERROR                     
079100              MOVE NEJ           TO INDATA-SW                             
079200           END-IF                                                         
079300         END-IF                                                           
079400       END-IF                                                             
079500                                                                          
079600       IF REQU-FLSKRIV-DELNOTE = 'Y'                                      
079700         IF REQU-PRTVAL-FOLJEFL = ALL '+'                                 
079800            MOVE WRONG-PRINTER TO RESP-IDMSG-ERROR                        
079900            MOVE 'DN'      TO RESP-IDELMT-ERROR                           
080000            MOVE NEJ      TO INDATA-SW                                    
080100         ELSE                                                             
080200           MOVE REQU-PRTVAL-FOLJEFL TO WS-KDPRTVAL-FS                     
080300           MOVE '4'              TO WS-SYSTDEL                            
080400           MOVE 'FS'             TO WS-LISTTYP                            
080500           MOVE WS-IDDC          TO WS-DC                                 
080600           MOVE WS-KDPRTVAL-FS   TO WS-KDPRT                              
080700                                                                          
080800           MOVE 001              TO PRT-KDCALL                            
080900           MOVE WS-IDPRTLST      TO PRT-IDPRTLST                          
081000                                                                          
081100           CALL W006PRT USING PRT-W006PRT                                 
081200                                                                          
081300           IF PRT-KDSVAR = RAETT                                          
081400             CONTINUE                                                     
081500           ELSE                                                           
081600              MOVE WRONG-PRINTER TO RESP-IDMSG-ERROR                      
081700              MOVE 'DN'      TO RESP-IDELMT-ERROR                         
081800              MOVE NEJ    TO INDATA-SW                                    
081900           END-IF                                                         
082000         END-IF                                                           
082100       END-IF                                                             
082200     END-IF                                                               
082300                                                                          
082400     MOVE +1                 TO RADIND                                    
082500                                                                          
083600     IF REQU-KVRADER-MAX1 = ALL '+'                                       
            CONTINUE                                                            
           ELSE                                                                 
083700      PERFORM UNTIL RADIND > REQU-KVRADER-MAX1                            
083800                                                                          
083900       IF REQU-FLSKRIV(RADIND) = 'Y' OR 'J'                               
084000                                                                          
084100         IF REQU-IDPRODNR(RADIND) NOT = ALL '+'                           
084200           IF REQU-IDPRODNR(RADIND) NUMERIC                               
084300           AND REQU-IDPRODNR(RADIND) > ZERO                               
084400               CONTINUE                                                   
084500           ELSE                                                           
084600             MOVE 'IDP'  TO RESP-IDMSG-ERROR-RAD (RADIND)                 
084700             MOVE 'IDPRC'TO RESP-IDELMT-ERROR                             
084800             PERFORM S11-SKAPA-ERR                                        
084900           END-IF                                                         
085000         ELSE                                                             
085100           MOVE 'IDP'  TO RESP-IDMSG-ERROR-RAD (RADIND)                   
085200           MOVE 'IDPRC'TO RESP-IDELMT-ERROR                               
085300           PERFORM S11-SKAPA-ERR                                          
085400         END-IF                                                           
085500                                                                          
085600         IF REQU-IDKOLLI (RADIND) NOT = ALL '+'                           
085700           IF REQU-IDKOLLI (RADIND) NUMERIC                               
085800          AND REQU-IDKOLLI (RADIND) > ZERO                                
085900*TO CHECK IF THE NEW VALUE ALREADY EXISTS IN THE DB                       
086000             IF REQU-IDKOLLI (RADIND) NOT                                 
086100                                    = REQU-IDKOLLI-OLD (RADIND)           
086200               MOVE REQU-IDPRODNR(RADIND) TO W-IDPRODNR-WDE601            
086300               MOVE REQU-IDKOLLI(RADIND)  TO W-IDKOLLI-WDE611             
086400               PERFORM IMS-GU-WDE611                                      
086500               IF SEGMENT-FOUND                                           
086600                 MOVE 'IDK'  TO RESP-IDMSG-ERROR-RAD (RADIND)             
086700                 MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                      
086800                 PERFORM S11-SKAPA-ERR                                    
086900               END-IF                                                     
087000             END-IF                                                       
087100*                                                                         
087200           ELSE                                                           
087300             MOVE 'IDK'  TO RESP-IDMSG-ERROR-RAD (RADIND)                 
087400             MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                          
087500             PERFORM S11-SKAPA-ERR                                        
087600           END-IF                                                         
087700         ELSE                                                             
087800             MOVE 'IDK'  TO RESP-IDMSG-ERROR-RAD (RADIND)                 
087900             MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                          
088000             PERFORM S11-SKAPA-ERR                                        
088100         END-IF                                                           
088200                                                                          
088300         IF REQU-IDKOLLI-OLD (RADIND) NOT = ALL '+'                       
088400           IF REQU-IDKOLLI-OLD (RADIND) NUMERIC                           
088500           AND REQU-IDKOLLI-OLD (RADIND) > ZERO                           
088600*TO CHECK IF THE EXISTING VALUE IN DB IS A VALID VALUE                    
088700             MOVE REQU-IDPRODNR(RADIND)     TO W-IDPRODNR-WDE601          
088800             MOVE REQU-IDKOLLI-OLD(RADIND)  TO W-IDKOLLI-WDE611           
088900             PERFORM IMS-GU-WDE611                                        
089000             IF SEGMENT-MISSING                                           
089100               MOVE 'IDK'  TO RESP-IDMSG-ERROR-RAD (RADIND)               
089200               MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                        
089300               PERFORM S11-SKAPA-ERR                                      
089400             END-IF                                                       
089500*                                                                         
089600           ELSE                                                           
089700             MOVE 'IDK'  TO RESP-IDMSG-ERROR-RAD (RADIND)                 
089800             MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                          
089900             PERFORM S11-SKAPA-ERR                                        
090000           END-IF                                                         
090100         ELSE                                                             
090200           MOVE 'IDK'  TO RESP-IDMSG-ERROR-RAD (RADIND)                   
090300           MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                            
090400           PERFORM S11-SKAPA-ERR                                          
090500         END-IF                                                           
090600                                                                          
090700         IF REQU-IDPLKLST(RADIND) NOT = ALL '+'                           
090800           IF REQU-IDPLKLST(RADIND) NUMERIC                               
090900           AND REQU-IDPLKLST(RADIND) > ZERO                               
091000             CONTINUE                                                     
091100           ELSE                                                           
091200             MOVE 'IDL'    TO RESP-IDMSG-ERROR-RAD (RADIND)               
091300             MOVE 'IDPLKLST' TO RESP-IDELMT-ERROR                         
091400             PERFORM S11-SKAPA-ERR                                        
091500           END-IF                                                         
091600         ELSE                                                             
091700           MOVE 'IDL'    TO RESP-IDMSG-ERROR-RAD (RADIND)                 
091800           MOVE 'IDPLKLST' TO RESP-IDELMT-ERROR                           
091900           PERFORM S11-SKAPA-ERR                                          
092000         END-IF                                                           
092100                                                                          
092200         IF REQU-IDDISTR (RADIND) NOT = ALL '+'                           
092300           IF REQU-IDDISTR (RADIND) NUMERIC                               
092400           AND REQU-IDDISTR (RADIND) > ZERO                               
092500             CONTINUE                                                     
092600           ELSE                                                           
092700             MOVE 'IDD'    TO RESP-IDMSG-ERROR-RAD (RADIND)               
092800             MOVE 'IDDISTR' TO RESP-IDELMT-ERROR                          
092900             PERFORM S11-SKAPA-ERR                                        
093000           END-IF                                                         
093100         ELSE                                                             
093200           MOVE 'IDD'    TO RESP-IDMSG-ERROR-RAD (RADIND)                 
093300           MOVE 'IDDISTR' TO RESP-IDELMT-ERROR                            
093400           PERFORM S11-SKAPA-ERR                                          
093500         END-IF                                                           
093600                                                                          
093700         IF REQU-IDKUNDNR(RADIND) NOT = ALL '+'                           
093800           IF REQU-IDKUNDNR(RADIND) NUMERIC                               
093900             CONTINUE                                                     
094000           ELSE                                                           
094100             MOVE 'IDC'    TO RESP-IDMSG-ERROR-RAD (RADIND)               
094200             MOVE 'IDKUNDNR' TO RESP-IDELMT-ERROR                         
094300             PERFORM S11-SKAPA-ERR                                        
094400           END-IF                                                         
094500         ELSE                                                             
094600           MOVE 'IDC'    TO RESP-IDMSG-ERROR-RAD (RADIND)                 
094700           MOVE 'IDKUNDNR' TO RESP-IDELMT-ERROR                           
094800           PERFORM S11-SKAPA-ERR                                          
094900         END-IF                                                           
095000                                                                          
095100         IF REQU-IDORDNR (RADIND) NOT = ALL '+'                           
095200           IF REQU-IDORDNR (RADIND) NUMERIC                               
095300           AND REQU-IDORDNR (RADIND) > ZERO                               
095400             CONTINUE                                                     
095500           ELSE                                                           
095600            MOVE 'IDO'   TO RESP-IDMSG-ERROR-RAD (RADIND)                 
095700            MOVE 'IDORDNR' TO RESP-IDELMT-ERROR                           
095800            PERFORM S11-SKAPA-ERR                                         
095900           END-IF                                                         
096000         ELSE                                                             
096100           MOVE 'IDO'    TO RESP-IDMSG-ERROR-RAD (RADIND)                 
096200           MOVE 'IDORDNR' TO RESP-IDELMT-ERROR                            
096300           PERFORM S11-SKAPA-ERR                                          
096400         END-IF                                                           
096500                                                                          
096600         IF REQU-KDKOLLI (RADIND) NOT = ALL '+'                           
096700            MOVE REQU-KDKOLLI (RADIND) TO W-KDKOLLI                       
096800            PERFORM IMS-GU-WDK501                                         
096900            IF SEGMENT-MISSING                                            
097000               MOVE 'KDK'    TO RESP-IDMSG-ERROR-RAD (RADIND)             
097100               MOVE 'KDKOLLI' TO RESP-IDELMT-ERROR                        
097200               PERFORM S11-SKAPA-ERR                                      
097300            ELSE                                                          
097400              IF EMB-DIKOLLIH = ZERO                                      
097500                IF REQU-DIKOLLIH(RADIND) = (ALL '+' OR ZERO)              
097600                   MOVE 'KDK'    TO RESP-IDMSG-ERROR-RAD (RADIND)         
097700                   MOVE 'DIKOLLIH'     TO RESP-IDELMT-ERROR               
097800                   PERFORM S11-SKAPA-ERR                                  
097900                END-IF                                                    
098000              END-IF                                                      
098100              IF EMB-DIKOLLIL = ZERO                                      
098200                IF REQU-DIKOLLIL(RADIND) = (ALL '+' OR ZERO)              
098300                   MOVE 'KDK'    TO RESP-IDMSG-ERROR-RAD (RADIND)         
098400                   MOVE 'DIKOLLIL'     TO RESP-IDELMT-ERROR               
098500                   PERFORM S11-SKAPA-ERR                                  
098600                END-IF                                                    
098700              END-IF                                                      
098800              IF EMB-DIKOLLIB = ZERO                                      
098900                IF REQU-DIKOLLIB(RADIND) = (ALL '+' OR ZERO)              
099000                   MOVE 'KDK'    TO RESP-IDMSG-ERROR-RAD (RADIND)         
099100                   MOVE 'DIKOLLIB'     TO RESP-IDELMT-ERROR               
099200                   PERFORM S11-SKAPA-ERR                                  
099300                END-IF                                                    
099400              END-IF                                                      
099500            END-IF                                                        
099600         ELSE                                                             
099700            MOVE 'KDK'    TO RESP-IDMSG-ERROR-RAD (RADIND)                
099800            MOVE 'KDKOLLI' TO RESP-IDELMT-ERROR                           
099900            PERFORM S11-SKAPA-ERR                                         
100000         END-IF                                                           
100100                                                                          
100200                                                                          
100300         IF REQU-DIKOLLIL(RADIND) NOT = ALL '+'                           
100400           IF REQU-DIKOLLIL(RADIND) NUMERIC AND                           
100500              REQU-DIKOLLIL(RADIND) NOT = ZERO                            
100600             CONTINUE                                                     
100700           ELSE                                                           
100800             MOVE 'DIL'    TO RESP-IDMSG-ERROR-RAD (RADIND)               
100900             MOVE 'DIKOLLIL' TO RESP-IDELMT-ERROR                         
101000             PERFORM S11-SKAPA-ERR                                        
101100           END-IF                                                         
101200         END-IF                                                           
101300         IF REQU-DIKOLLIH(RADIND) NOT = ALL '+'                           
101400           IF REQU-DIKOLLIH(RADIND) NUMERIC AND                           
101500              REQU-DIKOLLIH(RADIND) NOT = ZERO                            
101600             CONTINUE                                                     
101700           ELSE                                                           
101800             MOVE 'DIH'    TO RESP-IDMSG-ERROR-RAD (RADIND)               
101900             MOVE 'DIKOLLIH' TO RESP-IDELMT-ERROR                         
102000             PERFORM S11-SKAPA-ERR                                        
102100           END-IF                                                         
102200         END-IF                                                           
102300                                                                          
102400         IF REQU-DIKOLLIB(RADIND) NOT = ALL '+'                           
102500           IF REQU-DIKOLLIB(RADIND) NUMERIC AND                           
102600              REQU-DIKOLLIB(RADIND) NOT = ZERO                            
102700             CONTINUE                                                     
102800           ELSE                                                           
102900             MOVE 'DIB'    TO RESP-IDMSG-ERROR-RAD (RADIND)               
103000             MOVE 'DIKOLLIB' TO RESP-IDELMT-ERROR                         
103100             PERFORM S11-SKAPA-ERR                                        
103200           END-IF                                                         
103300         END-IF                                                           
103400         PERFORM CB-CHECK-REQU-VKORDBTO                                   
103500                                                                          
103600       END-IF                                                             
103700                                                                          
103800       ADD +1              TO RADIND                                      
103900      END-PERFORM                                                         
           END-IF                                                               
104000                                                                          
104100     .                                                                    
104200     SKIP2                                                                
104300 CA-NOLLA-ARB-TAB         SECTION.                                        
104400     MOVE 'STA CA-NOLLA'    TO WS-CURRENT-SECTION                         
104500                                                                          
104600     MOVE +1                TO RADIND                                     
104700     PERFORM UNTIL RADIND > REQU-KVRADER-MAX1                             
104800       MOVE ZERO            TO WS-VKORDBTO(RADIND)                        
104900       ADD +1               TO RADIND                                     
105000     END-PERFORM                                                          
105100     .                                                                    
105200     EJECT                                                                
105300 CB-CHECK-REQU-VKORDBTO SECTION.                                          
105400     MOVE 'STA CB-CHECK'      TO WS-CURRENT-SECTION                       
105500                                                                          
105600     IF REQU-VKORDBTO (RADIND) = ALL '+'                                  
105700       CONTINUE                                                           
105800     ELSE                                                                 
105900        INSPECT REQU-VKORDBTO (RADIND) REPLACING                          
106000               LEADING SPACE BY ZERO                                      
106100        MOVE REQU-VKORDBTO (RADIND) TO DEC-IDFRIDATA                      
106200        MOVE 5                      TO DEC-KVHELTAL                       
106300        MOVE 1                      TO DEC-KVDECIMAL                      
106400        CALL WDECEDIT USING DEC-WDECAREA                                  
106500                                                                          
106600        IF DEC-KDSVAR-OK AND DEC-IDEDITDATA > ZERO                        
106700           MOVE DEC-IDEDITDATA     TO  WS-VKORDBTO (RADIND)               
106800           INSPECT WS-VKORDBTO (RADIND) REPLACING                         
106900                            LEADING SPACE BY ZERO                         
107000           MOVE JA TO VKORDBTO-SW                                         
107100           PERFORM CBA-CHECK-WEIGHT                                       
107200        ELSE                                                              
107300           MOVE 'VKO'  TO RESP-IDMSG-ERROR-RAD (RADIND)                   
107400           MOVE 'VKORDBTO' TO RESP-IDELMT-ERROR                           
107500           PERFORM S11-SKAPA-ERR                                          
107600        END-IF                                                            
107700     END-IF                                                               
107800     .                                                                    
107900     SKIP2                                                                
108000 CBA-CHECK-WEIGHT      SECTION.                                           
108100     MOVE 'CBA-CHECK-WEI'     TO WS-CURRENT-SECTION                       
108200                                                                          
108300     MOVE REQU-IDPRODNR (RADIND) TO W-IDPRODNR-MIN                        
108400                                    W-IDPRODNR-MAX                        
                                          W-421-IDPRODNR                        
108500     MOVE REQU-IDKOLLI-OLD (RADIND) TO W-IDKOLLI-MIN                      
108600                                       W-IDKOLLI-MAX                      
                                             W-421-IDKOLLI                      
108700     MOVE 'N' TO WS-WEIGHT                                                
108800     MOVE ZERO TO ACC-ORAD-VKORDNTO                                       
108900     MOVE +1 TO LINE-IX                                                   
109000     PERFORM IMS-GU-WDE411-FSEQ                                           
           IF SEGMENT-FOUND                                                     
111700      PERFORM IMS-GNP-WDE401                                              
           END-IF                                                               
109100     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
111700      PERFORM IMS-GNP-WDE421                                              
109200      COMPUTE WS-ACC-ORAD-VKORDNTO ROUNDED =                              
109300           ORAD-VKART-NTO-KG *  E4F-KKOLLI-KVLEVART                       
109400      END-COMPUTE                                                         
109500      ADD WS-ACC-ORAD-VKORDNTO TO ACC-ORAD-VKORDNTO                       
109600      MOVE ORAD-IDARTNR       TO W-IDARTNR                                
109700      IF LINE-IX <= MAX-LINES                                             
109800       MOVE LINE-IX             TO MAX-TAB                                
109900                                 LINE1-NUMBER (LINE-IX)                   
110000       IF CDC                                                             
110100         PERFORM IMS-GU-WDK611                                            
110200         MOVE CLAG-ADGANG     TO LINE1-ADGANG  (LINE-IX)                  
110300         MOVE CLAG-ADPLATS    TO LINE1-ADPLATS (LINE-IX)                  
110400         MOVE CLAG-ADLAGOMR   TO LINE1-ADLAGOMR(LINE-IX)                  
110500       ELSE                                                               
110600         PERFORM IMS-GU-WDK711                                            
110700         MOVE SLAG-ADGANG     TO LINE1-ADGANG  (LINE-IX)                  
110800         MOVE SLAG-ADPLATS    TO LINE1-ADPLATS (LINE-IX)                  
110900         MOVE SLAG-ADLAGOMR   TO LINE1-ADLAGOMR(LINE-IX)                  
111000       END-IF                                                             
             MOVE E4F-KORD-IDUSER   TO WS-IDUSER                                
111100       MOVE E4F-KKOLLI-KVLEVART TO LINE1-KVAVBART(LINE-IX)                
111200       MOVE ORAD-IDARTNR      TO LINE1-IDARTNR (LINE-IX)                  
111300       MOVE ORAD-VKARTNTO     TO LINE1-VKOLDNET(LINE-IX)                  
111400       MOVE ORAD-VKART-NTO-KG TO LINE1-VKNEWNET(LINE-IX)                  
111500       ADD 1 TO  LINE-IX                                                  
111600      END-IF                                                              
111800      PERFORM IMS-GN-WDE411-FSEQ                                          
111900     END-PERFORM                                                          
112000                                                                          
112100     MOVE LINE-IX             TO MAX-RAD                                  
112200     IF US-MEASUREMENT                                                    
112300      MOVE ZERO TO WS-VKORDBTO-CONV                                       
112400      MOVE WS-VKORDBTO (RADIND) TO WS-VKORDBTO-CONV                       
112500      COMPUTE WS-VKORDBTO-CONV ROUNDED =                                  
112600              WS-VKORDBTO-CONV * CONV-LB-TO-KG                            
112700      END-COMPUTE                                                         
112800      IF WS-VKORDBTO-CONV < ACC-ORAD-VKORDNTO                             
112900       MOVE 'J' TO WS-WEIGHT                                              
113000       PERFORM S23-MOVE-LINEDATA                                          
113100      END-IF                                                              
113200     ELSE                                                                 
113300      IF WS-VKORDBTO (RADIND) < ACC-ORAD-VKORDNTO                         
113400       MOVE 'J' TO WS-WEIGHT                                              
113500       PERFORM S23-MOVE-LINEDATA                                          
113600      END-IF                                                              
113700     END-IF                                                               
113800     IF WEIGHT-MISMATCH                                                   
113900        MOVE NEJ TO VKORDBTO-SW                                           
114000        PERFORM S21-SEND-OPEN                                             
114100        PERFORM S22-PUT-HEADER                                            
114200        PERFORM S24-WRITE-LINE                                            
114300        PERFORM S25-SEND-CLOSE                                            
114400        MOVE NEJ                    TO INDATA-SW                          
114500        IF US-MEASUREMENT                                                 
114600         COMPUTE ACC-ORAD-VKORDNTO ROUNDED =                              
114700                 ACC-ORAD-VKORDNTO * CONV-KG-TO-LB                        
114800        END-IF                                                            
114900        MOVE ACC-ORAD-VKORDNTO      TO WS-ORAD-VKORDN-ED                  
115000        MOVE WS-ORAD-VKORDN-ED      TO WS-ORAD-VKORDNTO-CHAR              
115100        IF RESP-IDMSG-ERROR = SPACES                                      
115200         MOVE 'VKO'  TO RESP-IDMSG-ERROR-RAD (RADIND)                     
115300         STRING 'GROSS WT' WS-ORAD-VKORDNTO-CHAR                          
115400         DELIMITED BY SIZE INTO RESP-IDELMT-ERROR                         
115500         MOVE WEIGHT-CANNOT-BE-LESS-THAN  TO                              
115600                                     RESP-IDMSG-ERROR                     
115700        END-IF                                                            
115800     END-IF                                                               
115900     .                                                                    
116000 E-UPDATE-IDKOLLI          SECTION.                                       
116100     MOVE 'E-UPDATE-IDKOLLI'  TO WS-CURRENT-SECTION                       
116200                                                                          
116300     MOVE +1                 TO RADIND                                    
116400     MOVE +1                 TO TMS-IX                                    
116500     MOVE REQU-IDDC-KEY      TO TMS-IDDC                                  
116600                                                                          
116700     PERFORM UNTIL RADIND > REQU-KVRADER-MAX1                             
116800      IF REQU-FLSKRIV(RADIND) = 'J' OR 'Y'                                
116900         PERFORM EA-LAES-WDE6                                             
117000*LK RESTRICT UPDATE DB'S WHEN ONLY PRINT REQUIRED FOR ALREADY             
117100*LK PACKED CASES                                                          
117200         IF KOLLI-KDKOLSTA = 0                                            
117300            PERFORM EB-UPPDATERA-KOLLIREG                                 
117400            PERFORM EC-SEND-TMSINFO                                       
117500         END-IF                                                           
117600         PERFORM EJ-PRINT-LABELS                                          
117700      END-IF                                                              
117800                                                                          
117900      ADD +1      TO RADIND                                               
118000      MOVE NEJ    TO KDKOLLI-SW                                           
118100                     VKORDBTO-SW                                          
118200                     VLORDBTO-SW                                          
118300     END-PERFORM                                                          
118400                                                                          
118500*LK TMS CALL FOR LAST CASE                                                
118600     IF TMS-IDKOLLI (1) > 0                                               
118700       CALL W403TMS1 USING TMS-W403TMS1                                   
118800            TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                              
118900            TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                        
119000            TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                        
119100            TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                      
119200            TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                        
119300            TMS-WDK5-PCB TMS-WDQ2C-PCB                                    
119400     END-IF                                                               
119500                                                                          
119600     MOVE INF-UPDATED               TO RESP-IDMSG-INFO                    
119700                                                                          
119800     .                                                                    
119900     SKIP2                                                                
120000 EA-LAES-WDE6  SECTION.                                                   
120100     MOVE 'EA-FLYTTA-NYCKLAR' TO WS-CURRENT-SECTION                       
120200                                                                          
120300     MOVE REQU-IDPRODNR(RADIND)     TO WS-IDPRODNR                        
120400                                       W-IDPRODNR                         
120500     PERFORM IMS-GU-WDE601                                                
120600*TO USE THE IDKOLLI THAT WAS PRESENT BEFORE THE CHANGE.                   
120700     MOVE REQU-IDKOLLI-OLD(RADIND)  TO W-IDKOLLI                          
120800*                                                                         
120900     PERFORM IMS-GNP-WDE611                                               
121000                                                                          
121100     IF REQU-KDKOLLI(RADIND) NOT = KOLLI-KDKOLLI                          
121200*LK CASE CODE CHANGE                                                      
121300       MOVE JA TO KDKOLLI-SW                                              
121400       MOVE REQU-KDKOLLI (RADIND) TO W-KDKOLLI                            
121500       PERFORM IMS-GU-WDK501                                              
121600       IF SEGMENT-FOUND                                                   
121700          MOVE EMB-VKTARA   TO WS-VKTARA-NEW                              
121800          MOVE EMB-DIKOLLIL TO WS-DIKOLLIL-NEW                            
121900          MOVE EMB-DIKOLLIB TO WS-DIKOLLIB-NEW                            
122000          MOVE EMB-DIKOLLIH TO WS-DIKOLLIH-NEW                            
122100       END-IF                                                             
122200       MOVE KOLLI-KDKOLLI       TO W-KDKOLLI                              
122300       PERFORM IMS-GU-WDK501                                              
122400       IF SEGMENT-FOUND                                                   
122500          MOVE EMB-VKTARA       TO WS-VKTARA-OLD                          
122600       END-IF                                                             
122700     ELSE                                                                 
122800      IF REQU-DIKOLLIL(RADIND) NOT = ALL '+' OR                           
122900         REQU-DIKOLLIB(RADIND) NOT = ALL '+' OR                           
123000         REQU-DIKOLLIH(RADIND) NOT = ALL '+'                              
123100          MOVE EMB-DIKOLLIL TO WS-DIKOLLIL-NEW                            
123200          MOVE EMB-DIKOLLIB TO WS-DIKOLLIB-NEW                            
123300          MOVE EMB-DIKOLLIH TO WS-DIKOLLIH-NEW                            
123400      END-IF                                                              
123500     END-IF                                                               
123600                                                                          
123700     IF REQU-DIKOLLIL(RADIND) = ALL '+'                                   
123800        CONTINUE                                                          
123900     ELSE                                                                 
124000        MOVE JA TO VLORDBTO-SW                                            
124100        MOVE REQU-DIKOLLIL(RADIND) TO WS-DIKOLLIL-NEW                     
124200        IF US-MEASUREMENT                                                 
124300          COMPUTE WS-DIKOLLIL-NEW ROUNDED =                               
124400                  WS-DIKOLLIL-NEW * CONV-IN-TO-CM                         
124500          END-COMPUTE                                                     
124600        END-IF                                                            
124700     END-IF                                                               
124800                                                                          
124900     IF REQU-DIKOLLIB(RADIND) = ALL '+'                                   
125000         CONTINUE                                                         
125100     ELSE                                                                 
125200         MOVE JA TO VLORDBTO-SW                                           
125300         MOVE REQU-DIKOLLIB(RADIND) TO WS-DIKOLLIB-NEW                    
125400         IF US-MEASUREMENT                                                
125500          COMPUTE WS-DIKOLLIB-NEW ROUNDED =                               
125600                  WS-DIKOLLIB-NEW * CONV-IN-TO-CM                         
125700          END-COMPUTE                                                     
125800        END-IF                                                            
125900     END-IF                                                               
126000                                                                          
126100     IF REQU-DIKOLLIH(RADIND) = ALL '+'                                   
126200        CONTINUE                                                          
126300     ELSE                                                                 
126400        MOVE JA TO VLORDBTO-SW                                            
126500        MOVE REQU-DIKOLLIH(RADIND) TO WS-DIKOLLIH-NEW                     
126600        IF US-MEASUREMENT                                                 
126700          COMPUTE WS-DIKOLLIH-NEW ROUNDED =                               
126800                  WS-DIKOLLIH-NEW * CONV-IN-TO-CM                         
126900          END-COMPUTE                                                     
127000        END-IF                                                            
127100     END-IF                                                               
127200                                                                          
127300     COMPUTE WS-VOL-NEW  = WS-DIKOLLIL-NEW *                              
127400                           WS-DIKOLLIH-NEW *                              
127500                           WS-DIKOLLIB-NEW / 1000000                      
127600     END-COMPUTE                                                          
127700     .                                                                    
127800     SKIP2                                                                
127900 EB-UPPDATERA-KOLLIREG      SECTION.                                      
128000     MOVE 'EB-UPPDATERA-KOLLIREG'    TO WS-CURRENT-SECTION                
128100                                                                          
128200     PERFORM IMS-GHU-WDE601                                               
128300     MOVE VORD-IDDISTR       TO  TEST-IDDISTR                             
128400     IF REQU-VKORDBTO (RADIND) = ALL '+'                                  
128500       CONTINUE                                                           
128600     ELSE                                                                 
128700       MOVE JA TO VKORDBTO-SW                                             
128800     END-IF                                                               
128900                                                                          
129000*LK  IF KOLLI-KDKOLSTA  =   ZERO                                          
129100*LK    IF VORD-KVKOLLI     >  VORD-KVKOLPAC                               
129200*LK      COMPUTE VORD-KVKOLPAC = VORD-KVKOLPAC + 1                        
129300*LK      END-COMPUTE                                                      
129400*LK    END-IF                                                             
129500*LK  END-IF                                                               
129600                                                                          
129700     MOVE WS-DAGENS-DATUM     TO VORD-TIPACKN-SK                          
129800                                                                          
129900     IF KDKOLLI-SW = JA                                                   
130000       IF VKORDBTO-SW = NEJ                                               
130100*ADD NEW TARA WEIGHT                                                      
130200          COMPUTE VORD-VKORDBTO ROUNDED =                                 
130300                  VORD-VKORDBTO + WS-VKTARA-NEW                           
130400          END-COMPUTE                                                     
130500*DECREASE EXISTING TARA WEIGHT                                            
130600          COMPUTE VORD-VKORDBTO ROUNDED =                                 
130700                  VORD-VKORDBTO - WS-VKTARA-OLD                           
130800          END-COMPUTE                                                     
130900       END-IF                                                             
131000     END-IF                                                               
131100                                                                          
131200     IF VKORDBTO-SW = JA                                                  
131300*WHEN THE WEIGHT IS UPDATED MANUALLY                                      
131400       IF US-MEASUREMENT                                                  
131500        MOVE ZERO TO WS-VKORDBTO-CONV                                     
131600        MOVE WS-VKORDBTO (RADIND) TO WS-VKORDBTO-CONV                     
131700        COMPUTE WS-VKORDBTO-CONV ROUNDED =                                
131800                WS-VKORDBTO-CONV * CONV-LB-TO-KG                          
131900        END-COMPUTE                                                       
132000        COMPUTE VORD-VKORDBTO ROUNDED =                                   
132100                VORD-VKORDBTO + WS-VKORDBTO-CONV                          
132200        END-COMPUTE                                                       
132300       ELSE                                                               
132400        COMPUTE VORD-VKORDBTO ROUNDED =                                   
132500                VORD-VKORDBTO + WS-VKORDBTO (RADIND)                      
132600        END-COMPUTE                                                       
132700       END-IF                                                             
132800       COMPUTE VORD-VKORDBTO ROUNDED =                                    
132900               VORD-VKORDBTO - KOLLI-VKORDBTO-KOLLI                       
133000       END-COMPUTE                                                        
133100     END-IF                                                               
133200                                                                          
133300     IF VLORDBTO-SW = JA  OR KDKOLLI-SW = JA                              
133400*WHEN THE VOLUME IS UPDATED MANUALLY                                      
133500        COMPUTE VORD-VLORDBTO ROUNDED =                                   
133600                VORD-VLORDBTO + WS-VOL-NEW                                
133700        END-COMPUTE                                                       
133800        COMPUTE VORD-VLORDBTO ROUNDED =                                   
133900                VORD-VLORDBTO - KOLLI-VLORDBTO-KOLLI                      
134000        END-COMPUTE                                                       
134100     END-IF                                                               
134200                                                                          
134300     PERFORM IMS-REPL-WDE601                                              
134400     PERFORM EBA-BEHANDLA-KOLLI                                           
134500     .                                                                    
134600     EJECT                                                                
134700 EBA-BEHANDLA-KOLLI      SECTION.                                         
134800     MOVE 'EBA-BEHANDLA-KOLLI '         TO WS-CURRENT-SECTION             
134900     SKIP3                                                                
135000     MOVE REQU-IDPRODNR(RADIND)    TO W-IDPRODNR                          
135100     MOVE REQU-IDKOLLI-OLD(RADIND) TO W-IDKOLLI                           
135200                                                                          
135300     PERFORM IMS-GHU-WDE611                                               
135400                                                                          
135500     IF KDKOLLI-SW = JA                                                   
135600*ADD NEW TARA WEIGHT                                                      
135700       IF VKORDBTO-SW = NEJ                                               
135800          COMPUTE KOLLI-VKORDBTO-KOLLI ROUNDED =                          
135900                  KOLLI-VKORDBTO-KOLLI + WS-VKTARA-NEW                    
136000          END-COMPUTE                                                     
136100*DECREASE EXISTING TARA WEIGHT                                            
136200          COMPUTE KOLLI-VKORDBTO-KOLLI ROUNDED =                          
136300                  KOLLI-VKORDBTO-KOLLI - WS-VKTARA-OLD                    
136400          END-COMPUTE                                                     
136500       END-IF                                                             
136600**MOVE NEW VOLUME                                                         
136700       IF VLORDBTO-SW = NEJ                                               
136800          MOVE WS-VOL-NEW        TO KOLLI-VLORDBTO-KOLLI                  
136900          MOVE WS-DIKOLLIL-NEW   TO KOLLI-DIKOLLIL                        
137000          MOVE WS-DIKOLLIB-NEW   TO KOLLI-DIKOLLIB                        
137100          MOVE WS-DIKOLLIH-NEW   TO KOLLI-DIKOLLIH                        
137200       END-IF                                                             
137300**                                                                        
137400       MOVE REQU-KDKOLLI(RADIND) TO KOLLI-KDKOLLI                         
137500     END-IF                                                               
137600                                                                          
137700     IF VKORDBTO-SW = JA                                                  
137800*WHEN WEIGHT IS UPDATED MANUALLY                                          
137900       IF US-MEASUREMENT                                                  
138000        MOVE ZERO TO WS-VKORDBTO-CONV                                     
138100        MOVE WS-VKORDBTO (RADIND) TO WS-VKORDBTO-CONV                     
138200        COMPUTE WS-VKORDBTO-CONV ROUNDED =                                
138300                WS-VKORDBTO-CONV * CONV-LB-TO-KG                          
138400        END-COMPUTE                                                       
138500        MOVE WS-VKORDBTO-CONV      TO KOLLI-VKORDBTO-KOLLI                
138600       ELSE                                                               
138700        MOVE WS-VKORDBTO (RADIND)  TO KOLLI-VKORDBTO-KOLLI                
138800       END-IF                                                             
138900     END-IF                                                               
139000                                                                          
139100     IF VLORDBTO-SW = JA                                                  
139200*WHEN VOLUME IS UPDATED MANUALLY                                          
139300       MOVE WS-VOL-NEW  TO KOLLI-VLORDBTO-KOLLI                           
139400       MOVE WS-DIKOLLIL-NEW TO KOLLI-DIKOLLIL                             
139500       MOVE WS-DIKOLLIB-NEW TO KOLLI-DIKOLLIB                             
139600       MOVE WS-DIKOLLIH-NEW TO KOLLI-DIKOLLIH                             
139700     END-IF                                                               
139800                                                                          
139900     IF KOLLI-KDKOLSTA  =   ZERO                                          
140000       MOVE 1                     TO KOLLI-KDKOLSTA                       
140100       MOVE WS-DAGENS-DATUM       TO KOLLI-TIPACKN                        
140200       MOVE WS-TIDPUNKT (1:6)     TO KOLLI-TIPACTID                       
140300       PERFORM S12-SKAPA-4322                                             
140400*LK  TMS CASE INFO                                                        
140500     END-IF                                                               
140600*                                                                         
140700     IF REQU-IDKOLLI(RADIND) = KOLLI-IDKOLLI                              
140800        PERFORM IMS-REPL-WDE611                                           
140900     ELSE                                                                 
141000        PERFORM EBB-INSERT-IDKOLLI                                        
141100     END-IF                                                               
141200*                                                                         
141300     .                                                                    
141400     EJECT                                                                
141500 EBB-INSERT-IDKOLLI SECTION.                                              
141600     MOVE 'EBB-INSERT-IDKOLLI' TO WS-CURRENT-SECTION                      
141700                                                                          
141800     MOVE KOLLI-WDE611            TO WS-COPY-KOLLI-WDE611                 
141900     PERFORM IMS-DLET-WDE611                                              
142000     MOVE WS-COPY-KOLLI-WDE611    TO KOLLI-WDE611                         
142100     MOVE REQU-IDKOLLI(RADIND)    TO KOLLI-IDKOLLI                        
142200     PERFORM IMS-ISRT-WDE611                                              
142300*UPDATE CORRESPONDING WDE421                                              
142400     MOVE REQU-IDPRODNR(RADIND)   TO W-401-IDPRODNR                       
142500                                     W-421-IDPRODNR                       
142600     MOVE REQU-IDPLKLST(RADIND)   TO W-401-IDPLKLST                       
142700     MOVE REQU-IDDISTR (RADIND)   TO W-401-IDDISTR                        
142800     MOVE REQU-IDKUNDNR(RADIND)   TO W-401-IDKUNDNR                       
142900     MOVE SPACES                  TO W-401-IDKUNDRF                       
143000     MOVE REQU-IDORDNR (RADIND)   TO W-401-IDORDNR                        
143100                                                                          
143200     MOVE WS-COPY-KOLLI-IDKOLLI   TO W-421-IDKOLLI                        
143300                                                                          
143400     PERFORM IMS-GU-WDE401                                                
143500     PERFORM IMS-GHNP-WDE421-KVAL                                         
143600     PERFORM UNTIL SEGMENT-MISSING                                        
143700                                                                          
143800        MOVE KKOLLI-WDE421 TO WS-COPY-KKOLLI-WDE421                       
143900        PERFORM IMS-DLET-WDE421                                           
144000        MOVE WS-COPY-KKOLLI-WDE421 TO KKOLLI-WDE421                       
144100        MOVE REQU-IDKOLLI(RADIND)  TO KKOLLI-IDKOLLI                      
144200        PERFORM IMS-ISRT-WDE421                                           
144300                                                                          
144400        PERFORM IMS-GHNP-WDE421-KVAL                                      
144500     END-PERFORM                                                          
144600     .                                                                    
144700 EC-SEND-TMSINFO    SECTION.                                              
144800     MOVE 'EC-SEND-TMSINFO' TO WS-CURRENT-SECTION                         
144900                                                                          
145000     IF FIRST-SW = 'Y'                                                    
145100        MOVE REQU-IDDISTR (RADIND) TO TMS-IDDISTR                         
145200        MOVE REQU-IDKUNDNR(RADIND) TO TMS-IDKUNDNR                        
145300        MOVE REQU-IDORDNR (RADIND) TO TMS-IDORDNR7                        
145400        MOVE REQU-IDPRODNR(RADIND) TO WS-TMS-IDPRODNR                     
145500        MOVE KOLLI-IDKOLLI TO TMS-IDKOLLI(TMS-IX)                         
145600        MOVE 'N'  TO FIRST-SW                                             
145700     ELSE                                                                 
145800        IF REQU-IDPRODNR(RADIND) = WS-TMS-IDPRODNR                        
145900          MOVE KOLLI-IDKOLLI TO TMS-IDKOLLI(TMS-IX)                       
146000          CONTINUE                                                        
146100        ELSE                                                              
146200            CALL W403TMS1 USING TMS-W403TMS1                              
146300                 TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                         
146400                 TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                   
146500                 TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                   
146600                 TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                 
146700                 TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                   
146800                 TMS-WDK5-PCB TMS-WDQ2C-PCB                               
146900                                                                          
147000            INITIALIZE TMS-W403TMS1                                       
147100            MOVE REQU-IDDC-KEY TO TMS-IDDC                                
147200            MOVE REQU-IDPRODNR(RADIND) TO WS-TMS-IDPRODNR                 
147300            MOVE REQU-IDDISTR (RADIND) TO TMS-IDDISTR                     
147400            MOVE REQU-IDKUNDNR(RADIND) TO TMS-IDKUNDNR                    
147500            MOVE REQU-IDORDNR (RADIND) TO TMS-IDORDNR7                    
147600            MOVE +1   TO TMS-IX                                           
147700            MOVE KOLLI-IDKOLLI TO TMS-IDKOLLI(TMS-IX)                     
147800        END-IF                                                            
147900     END-IF                                                               
148000     ADD +1 TO TMS-IX                                                     
148100     .                                                                    
148200     SKIP2                                                                
148300 EJ-PRINT-LABELS SECTION.                                                 
148400                                                                          
148500     ADD +1         TO RADL                                               
148600     IF REQU-FLSKRIV-CLABEL = 'Y' OR 'J'                                  
148700*       MOVE 'Y'     TO RESP-L128-FLBG                                    
148800        PERFORM EJA-PRINT-CASE-LABEL                                      
148900     ELSE                                                                 
149000        IF REQU-FLSKRIV-CLABEL = 'N'                                      
149100           CONTINUE                                                       
149200*          MOVE ZERO  TO RESP-L128-KVRADER                                
149300*          MOVE 'N'   TO RESP-L128-FLBG                                   
149400        END-IF                                                            
149500     END-IF                                                               
149600                                                                          
149700     IF REQU-FLSKRIV-DELNOTE = 'Y' OR 'J'                                 
149800*      MOVE 'Y'       TO RESP-L129-FLBG                                   
149900       PERFORM EJB-PRINT-DEL-NOTE                                         
150000     ELSE                                                                 
150100       IF REQU-FLSKRIV-DELNOTE = 'N'                                      
150200         CONTINUE                                                         
150300*        MOVE 'N'     TO RESP-L129-FLBG                                   
150400*        MOVE ZERO    TO RESP-L129-KVRADER                                
150500       END-IF                                                             
150600     END-IF                                                               
150700     .                                                                    
150800     EJECT                                                                
150900 EJA-PRINT-CASE-LABEL         SECTION.                                    
151000     MOVE 'STA EFAA-PRINT-CASE-LABEL  '     TO WS-CURRENT-SECTION         
151100*L128-MID HAR OCCURS MAX 15                                               
151200                                                                          
151300     IF RADIND <= MAX-RADINDX                                             
151400                                                                          
151500      IF DCS-CDC                                                          
151600       MOVE WS-KDPRTVAL-ADR        TO 4333-MID-KDPRTVAL-UT                
151700       MOVE REQU-IDDISTR (RADIND)  TO WS-IDDISTR-NUM                      
151800       MOVE WS-IDDISTR-NUM         TO 4333-MID-IDDISTR-UT                 
151900       MOVE REQU-IDKUNDNR(RADIND)  TO WS-IDKUNDNR-NUM                     
152000       MOVE WS-IDKUNDNR-NUM        TO 4333-MID-IDKUNDNR-UT                
152100       MOVE REQU-IDORDNR (RADIND)  TO 4333-MID-IDORDNR-UT                 
152200       MOVE REQU-IDKOLLI(RADIND)   TO 4333-MID-IDKOLLI-UT                 
152300       MOVE REQU-IDDC-KEY          TO 4333-MID-IDDC-UT                    
152400                                                                          
152500       MOVE REQU-IDPRODNR(RADIND) TO 4333-MID-IDPRODNR-UT                 
152600       MOVE ZERO                   TO 4333-MID-IDKOLLI-TOM                
152700       MOVE '++++'                 TO 4333-MID-IDDISTR-IN                 
152800       MOVE '++++++'               TO 4333-MID-IDKUNDNR-IN                
152900       MOVE '+++++'                TO 4333-MID-IDORDNR-IN                 
153000                                        4333-MID-IDKOLLI-IN               
153100       MOVE '++'                   TO 4333-MID-IDDC-IN                    
153200       MOVE '+++++++'              TO 4333-MID-IDPRODNR-IN                
153300       MOVE '++'                   TO 4333-MID-KDPRTVAL-IN                
153400                                                                          
153500       MOVE '1'                    TO 4333-MID-KDMFSFOR                   
153600       COMPUTE 4333-MID-KVLL = LENGTH OF 4333-MID-W4I33301 + 17           
153700                                                                          
153800       PERFORM IMS-PURG-4333-MSG                                          
153900      END-IF                                                              
154000     END-IF                                                               
154100     .                                                                    
154200     SKIP2                                                                
154300 EJB-PRINT-DEL-NOTE           SECTION.                                    
154400     MOVE 'STA EFAB-PRINT-DEL-NOTE    '     TO WS-CURRENT-SECTION         
154500*L129-MID HAR OCCURS MAX 15                                               
154600                                                                          
154700     IF RADIND <= MAX-RADINDX                                             
154800                                                                          
154900       IF DCS-CDC                                                         
155000       MOVE '++++'           TO  4341-MID-IDDISTR-IN                      
155100       MOVE REQU-IDDISTR (RADIND)  TO WS-IDDISTR-NUM                      
155200       MOVE WS-IDDISTR-NUM   TO  4341-MID-IDDISTR-UT                      
155300       MOVE '++++++'         TO  4341-MID-IDKUNDNR-IN                     
155400       MOVE REQU-IDKUNDNR(RADIND)  TO WS-IDKUNDNR-NUM                     
155500       MOVE WS-IDKUNDNR-NUM  TO  4341-MID-IDKUNDNR-UT                     
155600       MOVE '+++++'          TO  4341-MID-IDORDNR-IN                      
155700       MOVE REQU-IDORDNR (RADIND) TO 4341-MID-IDORDNR-UT                  
155800       MOVE '+'              TO  4341-MID-IDPLKLST-IN                     
155900       MOVE ZERO             TO  4341-MID-IDPLKLST-UT                     
156000       MOVE '+++++'          TO 4341-MID-IDKOLLI-IN                       
156100       MOVE REQU-IDKOLLI(RADIND) TO 4341-MID-IDKOLLI-UT                   
156200       MOVE '+++++'          TO 4341-MID-IDKOLLI-TOM-IN                   
156300       MOVE ZERO             TO 4341-MID-IDKOLLI-TOM-UT                   
156400       MOVE '++'             TO 4341-MID-KDPRTVAL-IN                      
156500       MOVE WS-KDPRTVAL-FS   TO 4341-MID-KDPRTVAL-UT                      
156600       MOVE '++'             TO 4341-MID-IDDC-IN                          
156700       MOVE REQU-IDDC-KEY    TO 4341-MID-IDDC-UT                          
156800       MOVE 'N'              TO 4341-MID-FL-SVENSK-FSEDEL                 
156900                                                                          
157000       COMPUTE 4341-MID-LL = LENGTH OF 4341-MID-W4I34101 + 17             
157100       MOVE '1'              TO 4341-MID-KDMFSFOR                         
157200                                                                          
157300       PERFORM IMS-PURG-4341-MSG                                          
157400       END-IF                                                             
157500                                                                          
157600     END-IF                                                               
157700     .                                                                    
157800     SKIP2                                                                
157900 F-READ-SHOW-INFO SECTION.                                                
158000     MOVE 'F-READ-SHOW-INFO'   TO WS-CURRENT-SECTION                      
158100                                                                          
158200     IF REQU-KDPGMACT = 'E'                                               
158300       MOVE ZERO                    TO RADIND                             
158400     END-IF                                                               
158500                                                                          
158600     IF (REQU-IDPRC-KEY        = ALL '+'                                  
158700     AND REQU-IDLOTNR-KEY      = ALL '+')                                 
158800        MOVE KEYS-ARE-MISSING        TO RESP-IDMSG-INFO                   
158900        MOVE 'IDPRC'                 TO RESP-IDELMT-ERROR                 
159000     ELSE                                                                 
159100                                                                          
159200*ALT 2 - LÄS MED IDPRC                                                    
159300       PERFORM FA-READ-IDPRC-IDLOTNR                                      
159400     END-IF                                                               
159500                                                                          
159600     MOVE RADIND TO RESP-KVRADER-MAX1                                     
159700                                                                          
159800*    IF REQU-KDPGMACT = 'S'                                               
159900*      CONTINUE                                                           
160000*    ELSE                                                                 
160100*      MOVE 500 TO RESP-KVRADER-MAX1                                      
160200*    END-IF                                                               
160300                                                                          
160400*    IF REQU-KDPGMACT = 'S'                                               
160500       IF RESP-KVRADER-MAX1 >= WS-MAX-500-RADER                           
160600         MOVE TOO-MANY-LINES TO RESP-IDMSG-INFO                           
160700       END-IF                                                             
160800       IF RADIND = +00000                                                 
160900         MOVE INF-LINES-NOT-FOUND TO RESP-IDMSG-INFO                      
161000       END-IF                                                             
161100*    END-IF                                                               
161200     .                                                                    
161300     EJECT                                                                
161400 FA-READ-IDPRC-IDLOTNR      SECTION.                                      
161500     MOVE 'FA-READ-IDPRC-IDLOTNR '  TO WS-CURRENT-SECTION                 
161600*ALT2-IDPRC-IDLOTNR                                                       
161700                                                                          
161800     MOVE REQU-IDPRC-KEY            TO W-IDPRCPLK-Q3I1                    
161900     MOVE REQU-IDLOTNR-KEY          TO W-IDLOTNRP-Q3I1                    
162000     MOVE REQU-IDDC-KEY             TO W-IDDC-Q3I1                        
162100                                                                          
162200     MOVE +0        TO RADIND                                             
162300     MOVE 1         TO TAB-IX                                             
162400     PERFORM IMS-GU-WDQ301                                                
162500                                                                          
162600     PERFORM UNTIL SEGMENT-MISSING                                        
162700                OR SEGMENT-END                                            
162800       IF ODEL-KVRADER  > ZERO                                            
162900         MOVE ODEL-IDPRODNR    TO TAB-IDPRODNR (TAB-IX)                   
163000         MOVE ODEL-IDPLKLST    TO TAB-IDPLKLST (TAB-IX)                   
163100         MOVE ODEL-IDDISTR     TO TAB-IDDISTR (TAB-IX)                    
163200         MOVE ODEL-IDKUNDNR    TO TAB-IDKUNDNR (TAB-IX)                   
163300         MOVE ODEL-IDORDNR7    TO TAB-IDORDNR7 (TAB-IX)                   
163400         MOVE ODEL-IDLOPNR-ORD TO TAB-IDLOPNR-ORD (TAB-IX)                
163500         ADD +1                TO TAB-IX                                  
163600       END-IF                                                             
163700       PERFORM IMS-GN-WDQ301                                              
163800     END-PERFORM                                                          
163900                                                                          
164000     IF TAB-IX > 1                                                        
164100       COMPUTE ANTAL = TAB-IX - 1                                         
164200       CALL WINTSOR USING SORT-TABELL                                     
164300                            STEGLAANGD                                    
164400                            ANTAL                                         
164500                            TAB-IDLOPNR-ORD (1)                           
164600                            NYCKELLAANGD                                  
164700                                                                          
164800     END-IF                                                               
164900                                                                          
165000     MOVE +1 TO TAB-IX                                                    
165100     PERFORM UNTIL RADIND > WS-MAX-500-RADER                              
165200                OR TAB-IX > ANTAL                                         
165300       MOVE TAB-IDPRODNR(TAB-IX)    TO W-401-IDPRODNR                     
165400       MOVE TAB-IDPLKLST(TAB-IX)    TO W-401-IDPLKLST                     
165500       MOVE TAB-IDDISTR (TAB-IX)    TO W-401-IDDISTR                      
165600       MOVE TAB-IDKUNDNR(TAB-IX)    TO W-401-IDKUNDNR                     
165700       MOVE SPACES                  TO W-401-IDKUNDRF                     
165800       MOVE TAB-IDORDNR7(TAB-IX)    TO W-401-IDORDNR                      
165900                                                                          
166000       PERFORM IMS-GU-WDE401                                              
166100       PERFORM IMS-GNP-WDE421-OKVAL                                       
166200                                                                          
166300       PERFORM UNTIL SEGMENT-MISSING                                      
166400         MOVE KKOLLI-IDPRODNR      TO W-IDPRODNR-WDE601                   
166500         MOVE KKOLLI-IDKOLLI       TO W-IDKOLLI-WDE611                    
166600         IF RADIND > 0                                                    
166700           PERFORM CHECK-FOR-DUPLICATE-KOLLI                              
166800         END-IF                                                           
166900         IF NO-DUP-KOLLI                                                  
167000           PERFORM IMS-GU-WDE611                                          
167100           IF SEGMENT-FOUND                                               
167200*   SKIP LINES SO WE DONT SHOW ALREADY INVOICED LINES. THIS IS A          
167300*   POSSIBILITY IF THERE ARE OLD ORDERS WITH SAME PICKING ROUND           
167400*   IDENTITY (PRC+LOTNR)                                                  
167500             IF KOLLI-KDKOLSTA < 7                                        
167600*LK TO CONTROL WEB PRINTED UNPACKED ORDER                                 
167700             AND KOLLI-DIKOLLIL > 0                                       
167800               ADD +1                  TO RADIND                          
167900               IF REQU-KDPGMACT = 'E'                                     
168000               MOVE REQU-FLSKRIV (RADIND) TO RESP-FLSKRIV (RADIND)        
168100               ELSE                                                       
168200               MOVE 'Y' TO RESP-FLSKRIV (RADIND)                          
168300               END-IF                                                     
168400               MOVE TAB-IDPRODNR(TAB-IX) TO RESP-IDPRODNR(RADIND)         
168500                                      W-RESP-IDPRODNR(RADIND)             
168600                  RESP-IDPRODNR(RADIND)                                   
168700               MOVE TAB-IDPLKLST(TAB-IX) TO RESP-IDPLKLST(RADIND)         
168800               MOVE KOLLI-IDKOLLI      TO RESP-IDKOLLI (RADIND)           
168900                                          W-RESP-IDKOLLI (RADIND)         
169000               MOVE KOLLI-KDKOLLI      TO RESP-KDKOLLI(RADIND)            
169100*              MOVE SPACE              TO RESP-KDKOLLI-IN(RADIND)         
169200               MOVE TAB-IDDISTR (TAB-IX) TO RESP-IDDISTR (RADIND)         
169300               MOVE TAB-IDKUNDNR(TAB-IX) TO RESP-IDKUNDNR(RADIND)         
169400               MOVE TAB-IDORDNR7(TAB-IX) TO RESP-IDORDNR (RADIND)         
169500               MOVE KOLLI-KDKOLSTA TO RESP-KDKOLSTA(RADIND)               
169600               MOVE KOLLI-KVORDRAD TO RESP-KVORDRAD(RADIND)               
169700               MOVE KOLLI-KDFARLIG-KOLLI                                  
169800                                     TO RESP-KDFARLIG(RADIND)             
169900               IF US-MEASUREMENT                                          
170000                MOVE ZERO TO WS-VKORDBTO-CONV                             
170100                MOVE ZERO TO WS-VOL-CONV                                  
170200                MOVE KOLLI-VKORDBTO-KOLLI TO WS-VKORDBTO-CONV             
170300                MOVE KOLLI-VLORDBTO-KOLLI TO WS-VOL-CONV                  
170400                COMPUTE WS-VKORDBTO-CONV ROUNDED =                        
170500                        WS-VKORDBTO-CONV * CONV-KG-TO-LB                  
170600                END-COMPUTE                                               
170700                COMPUTE WS-VOL-CONV ROUNDED =                             
170800                        WS-VOL-CONV * CONV-M3-TO-FT3                      
170900                END-COMPUTE                                               
171000                MOVE WS-VKORDBTO-CONV TO RESP-VKORDBTO(RADIND)            
171100                MOVE WS-VOL-CONV TO RESP-VLORDBTO(RADIND)                 
171200               ELSE                                                       
171300                MOVE KOLLI-VKORDBTO-KOLLI TO                              
171400                             RESP-VKORDBTO(RADIND)                        
171500                MOVE KOLLI-VLORDBTO-KOLLI TO                              
171600                             RESP-VLORDBTO(RADIND)                        
171700               END-IF                                                     
171800               IF US-MEASUREMENT                                          
171900                MOVE KOLLI-DIKOLLIL        TO WS-DIKOLLIL                 
172000                MOVE KOLLI-DIKOLLIH        TO WS-DIKOLLIH                 
172100                MOVE KOLLI-DIKOLLIB        TO WS-DIKOLLIB                 
172200                COMPUTE WS-DIKOLLIL ROUNDED =                             
172300                        WS-DIKOLLIL * CONV-CM-TO-IN                       
172400                END-COMPUTE                                               
172500                COMPUTE WS-DIKOLLIH ROUNDED =                             
172600                        WS-DIKOLLIH * CONV-CM-TO-IN                       
172700                END-COMPUTE                                               
172800                COMPUTE WS-DIKOLLIB ROUNDED =                             
172900                        WS-DIKOLLIB * CONV-CM-TO-IN                       
173000                END-COMPUTE                                               
173100                MOVE WS-DIKOLLIL       TO                                 
173200                             RESP-DIKOLLIL(RADIND)                        
173300                MOVE WS-DIKOLLIH       TO                                 
173400                             RESP-DIKOLLIH(RADIND)                        
173500                MOVE WS-DIKOLLIB       TO                                 
173600                             RESP-DIKOLLIB(RADIND)                        
173700               ELSE                                                       
173800                MOVE KOLLI-DIKOLLIL       TO                              
173900                             RESP-DIKOLLIL(RADIND)                        
174000                MOVE KOLLI-DIKOLLIH       TO                              
174100                             RESP-DIKOLLIH(RADIND)                        
174200                MOVE KOLLI-DIKOLLIB       TO                              
174300                             RESP-DIKOLLIB(RADIND)                        
174400               END-IF                                                     
174500               MOVE KOLLI-IDTRPTNR TO RESP-IDTRPTNR(RADIND)               
174600               MOVE SPACE       TO RESP-IDMSG-ERROR-RAD (RADIND)          
174700             END-IF                                                       
174800           END-IF                                                         
174900         END-IF                                                           
175000         PERFORM IMS-GNP-WDE421-OKVAL                                     
175100       END-PERFORM                                                        
175200       ADD +1 TO TAB-IX                                                   
175300     END-PERFORM                                                          
175400     .                                                                    
175500     EJECT                                                                
175600 CHECK-FOR-DUPLICATE-KOLLI    SECTION.                                    
175700     MOVE 1 TO CNT                                                        
175800     SET NO-DUP-KOLLI TO TRUE                                             
175900     PERFORM UNTIL CNT > RADIND OR DUPLICATE-CHECK = 'J'                  
176000       IF (KKOLLI-IDKOLLI = W-RESP-IDKOLLI (CNT) AND                      
176100        TAB-IDPRODNR(TAB-IX)  = W-RESP-IDPRODNR(CNT) )                    
176200         SET DUP-KOLLI TO TRUE                                            
176300       END-IF                                                             
176400       ADD  1 TO CNT                                                      
176500     END-PERFORM                                                          
176600     .                                                                    
176700     EJECT                                                                
176800                                                                          
176900*TAG BORT OM KODEN OM DEN EJ BEHÖVS EFTER TESTER.                         
177000*    --- DISPATCHER SECTIONS                                              
177100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
177200     MOVE 'STA S01-FETCH-REQUEST'    TO WS-CURRENT-SECTION                
177300                                                                          
177400     MOVE 'GETARG'               TO SUB-KDFUNC                            
177500     MOVE WS-ABSTRACT-ADRESS     TO SUB-ADDISPABS                         
177600     MOVE WS-MAX-500-RADER       TO REQU-KVRADER-MAX1                     
177700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
177800                                                                          
177900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
178000                                                                          
178100     IF SUB-KDRC > 0                                                      
178200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
178300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
178400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
178500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
178600     END-IF                                                               
178700     .                                                                    
178800     SKIP3                                                                
178900 S02-RETURN-RESPONSE SECTION.                                             
179000     MOVE 'STA S02-RETURN-RESPONSE'    TO WS-CURRENT-SECTION              
179100                                                                          
179200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
179300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
179400                                                                          
179500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
179600                                                                          
179700     IF SUB-KDRC > 0                                                      
179800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
179900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
180000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
180100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
180200     END-IF                                                               
180300     .                                                                    
180400     EJECT                                                                
180500 S11-SKAPA-ERR  SECTION.                                                  
180600     MOVE 'S11-SKAPA-ERR '            TO WS-CURRENT-SECTION               
180700                                                                          
180800     MOVE NEJ                    TO INDATA-SW                             
180900     MOVE IS-INVALID             TO RESP-IDMSG-ERROR                      
181000     .                                                                    
181100     EJECT                                                                
181200 S12-SKAPA-4322 SECTION.                                                  
181300     MOVE 'S12-SKAPA-4322 '            TO WS-CURRENT-SECTION              
181400                                                                          
181500*SKAPA INFO TILL SVENSKA ÅF, SÄNDS VIA VR.                                
181600     IF DIST03-SVERIGE-100-799                                            
181700     OR DIST03-NORGE                                                      
181800     OR DIST03-DANMARK-900                                                
181900     OR DIST85-PU-VIA-VR                                                  
182000     OR DIST21-TYRE                                                       
182100     AND NOT DIST47-INTERNA                                               
182200        MOVE WS-IDPRODNR        TO 4322-IDPRODNR                          
182300        IF REQU-IDKOLLI(RADIND) = KOLLI-IDKOLLI                           
182400           MOVE KOLLI-IDKOLLI   TO 4322-IDKOLLI                           
182500        ELSE                                                              
182600           MOVE REQU-IDKOLLI(RADIND)                                      
182700                                TO 4322-IDKOLLI                           
182800        END-IF                                                            
182900        PERFORM IMS-ISRT-4322-SEGM                                        
183000     END-IF                                                               
183100     .                                                                    
183200     EJECT                                                                
183300 S21-SEND-OPEN SECTION.                                                   
183400                                                                          
183500     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
183600     MOVE 'OPEN'                  TO SEND-KDFUNC                          
183700     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
183800                                     SEND-OPEN-AREA                       
183900     IF SEND-KDRC > ZERO                                                  
184000       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
184100       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
184200       DELIMITED BY SIZE INTO FELTEXT                                     
184300       DISPLAY FELTEXT                                                    
184400       CALL FELLOG                                                        
184500     END-IF                                                               
184600     .                                                                    
184700     EJECT                                                                
184800 S22-PUT-HEADER        SECTION.                                           
184900                                                                          
185000     MOVE 001                    TO HDR-REQU-IDMSGVER                     
185100     MOVE 'R'                    TO HDR-REQU-KDPGMACT                     
185200     MOVE IDPGM                  TO HDR-REQU-IDUSER                       
185300     MOVE 'WRONGWEIGHT'          TO HDR-IDOUTTYPE                         
185400     MOVE '004'                  TO HDR-IDOUTREC                          
185500     MOVE '004'                  TO HDR-IDLIST                            
185600     MOVE 'PUT'                  TO SEND-KDFUNC                           
185700     MOVE LENGTH OF HDR-AREA     TO SEND-KVDLEN                           
185800     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
185900                                    SEND-KVDLEN                           
186000                                    HDR-AREA                              
186100     IF SEND-KDRC > ZERO                                                  
186200       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
186300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
186400       DELIMITED BY SIZE INTO FELTEXT                                     
186500       DISPLAY FELTEXT                                                    
186600       CALL FELLOG                                                        
186700     END-IF                                                               
186800     .                                                                    
186900 S23-MOVE-LINEDATA SECTION.                                               
187000     MOVE REQU-IDDISTR(RADIND)     TO HEAD-DIST                           
187100     MOVE REQU-IDKUNDNR(RADIND)    TO HEAD-IDKUNDNR                       
187200     MOVE REQU-IDORDNR (RADIND)    TO HEAD-IDORDER                        
187300     MOVE REQU-IDKOLLI(RADIND)     TO HEAD-IDKOLLI                        
187400     MOVE REQU-KDKOLLI(RADIND)     TO HEAD-KDKOLLI                        
187500     MOVE REQU-IDPRODNR (RADIND)   TO HEAD-IDPRODNR                       
187600     IF US-MEASUREMENT                                                    
187700      MOVE WS-VKORDBTO-CONV        TO HEAD-VKORDBTO                       
187800     ELSE                                                                 
187900      MOVE WS-VKORDBTO (RADIND)    TO HEAD-VKORDBTO                       
188000     END-IF                                                               
188100     MOVE ACC-ORAD-VKORDNTO        TO HEAD-VKARTNTO                       
188200     MOVE WS-IDUSER                TO HEAD-IDPLKLST                       
188300     .                                                                    
188400 S24-WRITE-LINE  SECTION.                                                 
188500     MOVE 1 TO LINE-IX                                                    
188600     PERFORM UNTIL LINE-IX > 16                                           
188700        MOVE TAB-LINE(LINE-IX)      TO SEND-AREA                          
188800        PERFORM  S25-PUT-LINE                                             
188900        ADD 1 TO LINE-IX                                                  
189000     END-PERFORM                                                          
189100                                                                          
189200     MOVE 1 TO LINE-IX                                                    
189300     PERFORM UNTIL LINE-IX > MAX-TAB                                      
189400        MOVE LINE-TAB(LINE-IX)      TO SEND-AREA                          
189500        PERFORM  S25-PUT-LINE                                             
189600        ADD 1 TO LINE-IX                                                  
189700     END-PERFORM                                                          
189800                                                                          
189900      IF MAX-RAD > MAX-LINES                                              
190000        MOVE LINE-END     TO SEND-AREA                                    
190100        PERFORM  S25-PUT-LINE                                             
190200      END-IF                                                              
190300     .                                                                    
190400 S25-PUT-LINE     SECTION.                                                
190500                                                                          
190600     MOVE 'PUT'                     TO SEND-KDFUNC                        
190700     MOVE LENGTH OF SEND-AREA       TO SEND-KVDLEN                        
190800     CALL WZ01SEND               USING SEND-CONTROL-AREA                  
190900                                       SEND-KVDLEN                        
191000                                       SEND-AREA                          
191100     IF SEND-KDRC > ZERO                                                  
191200       MOVE SEND-KDRC               TO KDRC-DISPLAY                       
191300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
191400       DELIMITED BY SIZE INTO FELTEXT                                     
191500       DISPLAY FELTEXT                                                    
191600       CALL FELLOG                                                        
191700     END-IF                                                               
191800     .                                                                    
191900 S25-SEND-CLOSE SECTION.                                                  
192000                                                                          
192100     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
192200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
192300                                                                          
192400     IF SEND-KDRC > 0                                                     
192500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
192600       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
192700       DELIMITED BY SIZE INTO FELTEXT                                     
192800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
192900     END-IF                                                               
193000     .                                                                    
193100     EJECT                                                                
193200                                                                          
193300* --- IMS SECTIONS ---                                                    
193400     SKIP3                                                                
193500                                                                          
193600 IMS-GU-WDK501 SECTION.                                                   
193700     MOVE 'IMS-GU-WDK501'     TO WS-CURRENT-IMS-SECTION                   
193800                                                                          
193900     STRING 'WDK501  (KDKOLLI  =' W-KDKOLLI-X ')'                         
194000          DELIMITED BY SIZE INTO SSA1                                     
194100     MOVE '  GE' TO GOOD-STATUSCODES                                      
194200     CALL CBLTDLI USING GU WDK5-PCB DLI-IO-WDK501 SSA1                    
194300     MOVE WDK5-STATUS-CODE TO STATUS-WS                                   
194400     PERFORM IMS-STATUSCHECK                                              
194500     .                                                                    
194600     EJECT                                                                
194700 IMS-GU-WDE601 SECTION.                                                   
194800     MOVE 'IMS-GU-WDE601'     TO WS-CURRENT-IMS-SECTION                   
194900                                                                          
195000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
195100          DELIMITED BY SIZE INTO SSA1                                     
195200     MOVE '  ' TO GOOD-STATUSCODES                                        
195300     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
195400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
195500     PERFORM IMS-STATUSCHECK                                              
195600     .                                                                    
195700     EJECT                                                                
195800 IMS-GNP-WDE611 SECTION.                                                  
195900     MOVE 'IMS-GNP-WDE611'    TO WS-CURRENT-IMS-SECTION                   
196000                                                                          
196100     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
196200          DELIMITED BY SIZE INTO SSA1                                     
196300     MOVE '  ' TO GOOD-STATUSCODES                                        
196400     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1                   
196500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
196600     PERFORM IMS-STATUSCHECK                                              
196700     .                                                                    
196800     EJECT                                                                
196900 IMS-GU-WDE611 SECTION.                                                   
197000                                                                          
197100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-WDE601-X ')'                 
197200          DELIMITED BY SIZE INTO SSA1                                     
197300     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-WDE611-X ')'                  
197400          DELIMITED BY SIZE INTO SSA2                                     
197500     MOVE '  GE' TO GOOD-STATUSCODES                                      
197600     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
197700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
197800     PERFORM IMS-STATUSCHECK                                              
197900     .                                                                    
198000     EJECT                                                                
198100 IMS-GU-WDQ301 SECTION.                                                   
198200     MOVE 'IMS-GU-WDQ301'     TO WS-CURRENT-IMS-SECTION                   
198300                                                                          
198400     STRING 'WDQ301  (WDQ3ISEQ =' W-WDQ3ISEQ-X ')'                        
198500          DELIMITED BY SIZE INTO SSA1                                     
198600     MOVE '  GE' TO GOOD-STATUSCODES                                      
198700     CALL CBLTDLI USING GU WDQ3I-PCB DLI-IO-WDQ301 SSA1                   
198800     MOVE WDQ3I-STATUS-CODE TO STATUS-WS                                  
198900     PERFORM IMS-STATUSCHECK                                              
199000     .                                                                    
199100     EJECT                                                                
199200 IMS-GN-WDQ301 SECTION.                                                   
199300     MOVE 'IMS-GN-WDQ301'     TO WS-CURRENT-IMS-SECTION                   
199400                                                                          
199500     STRING 'WDQ301  (WDQ3ISEQ =' W-WDQ3ISEQ-X ')'                        
199600          DELIMITED BY SIZE INTO SSA1                                     
199700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
199800     CALL CBLTDLI USING GN WDQ3I-PCB DLI-IO-WDQ301 SSA1                   
199900     MOVE WDQ3I-STATUS-CODE TO STATUS-WS                                  
200000     PERFORM IMS-STATUSCHECK                                              
200100     .                                                                    
200200     EJECT                                                                
200300 IMS-GU-WDE401              SECTION.                                      
200400     MOVE 'IMS-GU-WDE401  '   TO WS-CURRENT-IMS-SECTION                   
200500                                                                          
200600     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
200700          DELIMITED BY SIZE INTO SSA1                                     
200800     MOVE '    ' TO GOOD-STATUSCODES                                      
200900     CALL CBLTDLI USING GU    WDE4-PCB DLI-IO-WDE401 SSA1                 
201000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
201100     PERFORM IMS-STATUSCHECK                                              
201200     SKIP3                                                                
201300     .                                                                    
201400 IMS-GNP-WDE421-OKVAL SECTION.                                            
201500     MOVE 'IMS-GNP-WDE421-OKVAL'    TO WS-CURRENT-IMS-SECTION             
201600                                                                          
201700     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
201800            DELIMITED BY SIZE INTO SSA1                                   
201900     MOVE 'WDE421'            TO SSA2                                     
202000     MOVE '  GE' TO GOOD-STATUSCODES                                      
202100     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE421 SSA1 SSA2              
202200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
202300     PERFORM IMS-STATUSCHECK                                              
202400     .                                                                    
202500     EJECT                                                                
202600 IMS-GHNP-WDE421-KVAL SECTION.                                            
202700     MOVE 'IMS-GHNP-WDE421-KVAL'    TO WS-CURRENT-IMS-SECTION             
202800                                                                          
202900     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
203000            DELIMITED BY SIZE INTO SSA1                                   
203100     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
203200            DELIMITED BY SIZE INTO SSA2                                   
203300     MOVE '  GE' TO GOOD-STATUSCODES                                      
203400     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE421 SSA1 SSA2             
203500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
203600     PERFORM IMS-STATUSCHECK                                              
203700     .                                                                    
203800     EJECT                                                                
203900 IMS-DLET-WDE421  SECTION.                                                
204000     MOVE 'IMS-DLET-WDE421'  TO WS-CURRENT-IMS-SECTION                    
204100                                                                          
204200     MOVE '    '   TO GOOD-STATUSCODES                                    
204300     CALL CBLTDLI USING DLET WDE4-PCB DLI-IO-WDE421                       
204400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
204500     PERFORM IMS-STATUSCHECK                                              
204600     .                                                                    
204700 IMS-ISRT-WDE421 SECTION.                                                 
204800     MOVE 'IMS-ISRT-WDE421'  TO WS-CURRENT-IMS-SECTION                    
204900                                                                          
205000     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
205100          DELIMITED BY SIZE INTO SSA1                                     
205200     MOVE   'WDE421'          TO SSA2                                     
205300     MOVE '  ' TO GOOD-STATUSCODES                                        
205400     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE421 SSA1 SSA2             
205500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
205600     PERFORM IMS-STATUSCHECK                                              
205700     .                                                                    
205800 IMS-GU-WDE411-FSEQ SECTION.                                              
205900     STRING 'WDE411  (WDE4FSEQ>=' W-WDE4FSEQ-MIN-X                        
206000                    '&WDE4FSEQ<=' W-WDE4FSEQ-MAX-X ')'                    
206100          DELIMITED BY SIZE INTO SSA1                                     
206200     MOVE '  GE'                TO GOOD-STATUSCODES                       
206300     CALL CBLTDLI USING GU WDE41-PCB DLI-IO-WDE411 SSA1                   
206400     MOVE WDE41-STATUS-CODE        TO STATUS-WS                           
206500     PERFORM IMS-STATUSCHECK                                              
206600     .                                                                    
206700     SKIP3                                                                
206800 IMS-GN-WDE411-FSEQ SECTION.                                              
206900     STRING 'WDE411  (WDE4FSEQ>=' W-WDE4FSEQ-MIN-X                        
207000                    '&WDE4FSEQ<=' W-WDE4FSEQ-MAX-X ')'                    
207100          DELIMITED BY SIZE INTO SSA1                                     
207200                                                                          
207300     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
207400     CALL CBLTDLI USING GN WDE41-PCB DLI-IO-WDE411 SSA1                   
207500     MOVE WDE41-STATUS-CODE        TO STATUS-WS                           
207600     PERFORM IMS-STATUSCHECK                                              
207700     .                                                                    
207800     SKIP3                                                                
207900 IMS-GNP-WDE401 SECTION.                                                  
208000     MOVE 'IMS-GNP-WDE401      ' TO WS-CURRENT-IMS-SECTION                
208100                                                                          
208200     MOVE 'WDE401 '         TO SSA3                                       
208300     MOVE '  '              TO GOOD-STATUSCODES                           
208400     CALL CBLTDLI USING GNP WDE41-PCB DLI-IO-E4F401 SSA3                  
208500     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
208600     PERFORM IMS-STATUSCHECK                                              
208700     .                                                                    
208800     EJECT                                                                
207900 IMS-GNP-WDE421 SECTION.                                                  
208000     MOVE 'IMS-GNP-WDE401      ' TO WS-CURRENT-IMS-SECTION                
208100     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
                            DELIMITED BY SIZE INTO SSA1                         
208300     MOVE '  '              TO GOOD-STATUSCODES                           
208400     CALL CBLTDLI USING GNP WDE41-PCB DLI-IO-E4F421 SSA1                  
208500     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
208600     PERFORM IMS-STATUSCHECK                                              
208700     .                                                                    
208800     EJECT                                                                
208900 IMS-GHU-WDE601           SECTION.                                        
209000     MOVE 'IMS-GHU-WDE601 '   TO WS-CURRENT-IMS-SECTION                   
209100                                                                          
209200     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
209300            DELIMITED BY SIZE INTO SSA1                                   
209400     MOVE '    ' TO GOOD-STATUSCODES                                      
209500     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-WDE601 SSA1                
209600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
209700     PERFORM IMS-STATUSCHECK                                              
209800     .                                                                    
209900     EJECT                                                                
210000 IMS-REPL-WDE601   SECTION.                                               
210100     MOVE 'IMS-REPL-WDE601  ' TO WS-CURRENT-IMS-SECTION                   
210200                                                                          
210300     MOVE '    ' TO GOOD-STATUSCODES                                      
210400     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
210500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
210600     PERFORM IMS-STATUSCHECK                                              
210700     SKIP3                                                                
210800     .                                                                    
210900 IMS-ISRT-WDE611 SECTION.                                                 
211000     MOVE 'IMS-ISRT-WDE611'  TO WS-CURRENT-IMS-SECTION                    
211100                                                                          
211200     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
211300          DELIMITED BY SIZE INTO SSA1                                     
211400     MOVE 'WDE611 ' TO SSA2                                               
211500     MOVE '  ' TO GOOD-STATUSCODES                                        
211600     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE611 SSA1 SSA2             
211700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
211800     PERFORM IMS-STATUSCHECK                                              
211900     .                                                                    
212000     SKIP2                                                                
212100 IMS-GHU-WDE611   SECTION.                                                
212200     MOVE 'IMS-GHU-WDE611'   TO WS-CURRENT-IMS-SECTION                    
212300                                                                          
212400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
212500            DELIMITED BY SIZE INTO SSA1                                   
212600     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
212700            DELIMITED BY SIZE INTO SSA2                                   
212800     MOVE '  GE' TO GOOD-STATUSCODES                                      
212900     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-WDE611 SSA1 SSA2           
213000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
213100     PERFORM IMS-STATUSCHECK                                              
213200     SKIP3                                                                
213300     .                                                                    
213400 IMS-REPL-WDE611   SECTION.                                               
213500     MOVE 'IMS-REPL-WDE611'    TO WS-CURRENT-IMS-SECTION                  
213600                                                                          
213700     MOVE '    ' TO GOOD-STATUSCODES                                      
213800     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
213900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
214000     PERFORM IMS-STATUSCHECK                                              
214100     SKIP3                                                                
214200     .                                                                    
214300 IMS-DLET-WDE611    SECTION.                                              
214400     MOVE 'IMS-DLET-WDE611'    TO WS-CURRENT-IMS-SECTION                  
214500                                                                          
214600     MOVE '    ' TO GOOD-STATUSCODES                                      
214700     CALL CBLTDLI USING DLET WDE6-PCB DLI-IO-WDE611                       
214800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
214900     PERFORM IMS-STATUSCHECK                                              
215000     .                                                                    
215100     SKIP3                                                                
215200 IMS-ISRT-4322-SEGM SECTION.                                              
215300     MOVE 'IMS-ISRT-4322-SEGM'         TO  WS-CURRENT-IMS-SECTION         
215400                                                                          
215500     STRING 'WLXXJK01(WDGXKEY  =' W-4321-IDHTYP-X ')'                     
215600            DELIMITED BY SIZE INTO SSA1                                   
215700     MOVE 'WLXXJK11*L' TO SSA2                                            
215800     MOVE '  ' TO GOOD-STATUSCODES                                        
215900     CALL CBLTDLI USING ISRT XXJK-PCB DLI-IO-AREA4 SSA1 SSA2              
216000     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
216100     PERFORM IMS-STATUSCHECK                                              
216200     .                                                                    
216300     EJECT                                                                
216400                                                                          
216500 IMS-GU-WDB601    SECTION.                                                
216600     MOVE 'IMS-GU-WDB601       '    TO WS-CURRENT-IMS-SECTION             
216700                                                                          
216800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
216900     DELIMITED BY SIZE INTO SSA1                                          
217000     MOVE '  '    TO GOOD-STATUSCODES                                     
217100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
217200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
217300     PERFORM IMS-STATUSCHECK                                              
217400     IF SEGMENT-MISSING                                                   
217500        MOVE SPACE TO DCS-KDDC                                            
217600     END-IF                                                               
217700     .                                                                    
217800     EJECT                                                                
217900 IMS-GU-WDK611  SECTION.                                                  
218000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
218100          DELIMITED BY SIZE INTO SSA1                                     
218200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
218300          DELIMITED BY SIZE INTO SSA2                                     
218400     MOVE '  GE' TO GOOD-STATUSCODES                                      
218500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
218600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
218700     PERFORM IMS-STATUSCHECK                                              
218800     .                                                                    
218900     SKIP3                                                                
219000 IMS-GU-WDK711 SECTION.                                                   
219100                                                                          
219200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
219300          DELIMITED BY SIZE INTO SSA1                                     
219400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
219500          DELIMITED BY SIZE INTO SSA2                                     
219600     MOVE '  GE' TO GOOD-STATUSCODES                                      
219700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
219800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
219900     PERFORM IMS-STATUSCHECK                                              
220000     .                                                                    
220100     SKIP2                                                                
220200 IMS-PURG-4333-MSG SECTION.                                               
220300                                                                          
220400     MOVE SPACE TO GOOD-STATUSCODES                                       
220500     CALL CBLTDLI USING PURG                                              
220600                        4333-PCB                                          
220700                        4333-MID-IO-AREA                                  
220800     MOVE 4333-STATUS-CODE TO STATUS-WS                                   
220900     PERFORM IMS-STATUSCHECK                                              
221000     .                                                                    
221100     SKIP3                                                                
221200 IMS-PURG-4341-MSG SECTION.                                               
221300                                                                          
221400     MOVE SPACE TO GOOD-STATUSCODES                                       
221500     CALL CBLTDLI USING PURG                                              
221600                        4341-PCB                                          
221700                        4341-MID-IO-AREA                                  
221800     MOVE 4341-STATUS-CODE TO STATUS-WS                                   
221900     PERFORM IMS-STATUSCHECK                                              
222000     SKIP3                                                                
222100     .                                                                    
222200 IMS-STATUSCHECK SECTION.                                                 
222300                                                                          
222400     SET STATUS-IX TO 1                                                   
222500     SEARCH GOOD-STATUS                                                   
222600       AT END                                                             
222700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
222800         DELIMITED BY SIZE INTO ERROR-TEXT                                
222900         CALL FELLOG                                                      
223000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
223100         CONTINUE                                                         
223200     END-SEARCH                                                           
223300     .                                                                    
