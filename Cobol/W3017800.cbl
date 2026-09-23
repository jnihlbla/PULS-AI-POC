000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3017800.                                                
000300 AUTHOR.         WEB-ACADEMY / MARKUS A, STEFAN K, KENT J, CONNY E        
000400                            OCH LILLE KJELL A.                            
000500 DATE-WRITTEN.   00/03/31.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        DETTA ÄR ETT MPP-PROGRAM SOM KOMMUNICERAR MOT EN                 
001000*        JSP-SERVLET ISTÄLLET FÖR MOT MFS.                                
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*        PROGRAMMET LÄSER      WDD3                                       
001400*        PROGRAMMET UPPDATERAR WDR4  WDGX3162                             
001500*        PROGRAMMET UPPDATERAR WDA9                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W30178T                                             
001900*        MID:         W30178I1                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W30178O1                                            
002300*                                                                         
002400*    CHANGE LOG:                                                          
002500*                                                                         
002600*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
002700*      ----------------------------------------------------------         
002800*      14/11/17 - REDDY RAHUL     - REMOVE SCRAP QTY FIELDS               
002900*                                   ETRACKER 10193018                     
003000*                                                                         
003100*      15/10/21 - REDDY RAHUL     - SHOW CORE NUMBERS IN NES              
003200*                                   ETRACKER 10251636                     
003000*                                                                         
003100*      16/06/20 - HÅKAN BOHLIN    - CREATE CSV FILE FROM NES              
003200*                                   ETRACKER 10251631                     
003300*                                                                         
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700                                                                          
003800 DATA DIVISION.                                                           
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'W3017800'.            
004400                                                                          
004500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004600 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004700                                                                          
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000                                                                          
005100 01  ALL-PLUS.                                                            
005200     03 ALL-PLUS-LINE            PIC X(25)   VALUE ALL '+'.               
005300                                                                          
005400 77  IX                          PIC S9(4)   BINARY VALUE ZERO.           
005500 77  INDX                        PIC S9(4)   BINARY VALUE ZERO.           
005600 77  MAX-INDX                    PIC S9(4)   BINARY VALUE +100.           
005700 77  TRANS-TID                   PIC 9(9).                                
005800 77  ANTAL-RADER                 PIC 9(5).                                
005900 77  WS-FLUPD                    PIC X       VALUE 'N'.                   
006000 01  WS-KVRAD-TOT                PIC 9(6)    VALUE ZERO.                  
006100 01  WS-IDARTNR-NY-INMAT         PIC 9(8)    VALUE ZERO.                  
006200 01  WS-EGEN-BILD                PIC 9(4)    VALUE 3178.                  
006300 01  WS-VISA                     PIC X.                                   
006400 01  WS-IMS-SECTION              PIC X(32).                               
006500 01  WS-IMS-LAES                 PIC X(32).                               
006600 01  KDRC-DISPLAY                PIC Z(4)9.                               
006700 01  WS-ADRESS                   PIC X(50)                                
006800                     VALUE 'CARPARTS.NES.REMANCONFIRM'.                   
006900                                                                          
007000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007100                                                                          
007200 77  USECASE-SW                  PIC X       VALUE '0'.                   
007300     88  NO-CASE                             VALUE '0'.                   
007400     88  SEARCH-ORDERS                       VALUE '1'.                   
007500     88  SEARCH-ORDERS-BY-DATE               VALUE '2'.                   
007600     88  SEARCH-ORDER-LINES                  VALUE '3'.                   
007700     88  UPDATE-ORDER-LINES                  VALUE '4'.                   
007800     88  NEW-ORDER-LINE                      VALUE '5'.                   
007900                                                                          
007200 77  CSV-FILE-SW                 PIC X       VALUE 'N'.                   
007400     88  CREATE-CSV-FILE                     VALUE 'J'.                   
007900                                                                          
008000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008100     88  INDATA-OK                           VALUE 'J'.                   
008200     88  INDATA-NOT-OK                       VALUE 'N'.                   
008300                                                                          
008400 77  INDATA-FEL-SW               PIC X       VALUE 'N'.                   
008500     88  FEL-FINNS                           VALUE 'J'.                   
008600     88  INGA-FEL                            VALUE 'N'.                   
008700                                                                          
008800 77  NY-ARTIKEL-SW               PIC X       VALUE 'N'.                   
008900     88  NY-ARTIKEL                          VALUE 'J'.                   
009000     88  INGEN-NY-ARTIKEL                    VALUE 'N'.                   
009100                                                                          
009200                                                                          
009300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009400     88  NYCKLAR-OK                          VALUE 'J'.                   
009500     88  NYCKLAR-FEL                         VALUE 'N'.                   
009200                                                                          
009300 77  FORSTA-SW                   PIC X       VALUE 'J'.                   
009400     88  FORSTA-TRAFF                        VALUE 'J'.                   
009600                                                                          
009700 77  MID-KOLL-SW                 PIC X(6)    VALUE SPACE.                 
009800     88  GODK-MID                            VALUE 'W30178'.              
009900                                                                          
010000 01  ARBETSFALT.                                                          
010100     03 WS-KVACCEPT              PIC 9(7)    VALUE ZERO.                  
010200     03 WS-DIFF-MOT              PIC S9(9)    VALUE ZERO COMP-3.          
010300                                                                          
010400 01  FILLER                      PIC X(16)   VALUE 'ARBNYCKEL'.           
010500 01  WS-NYCKELGRUPP.                                                      
010600     03 WS-NYCKELFAELT           PIC X(10).                               
010700                                                                          
010800 01  FILLER                      PIC X(16)  VALUE 'RAD-TABELL'.           
010900 01  LINE-TABLE.                                                          
011000     03 LINE-TABLE-LINE       OCCURS 500.                                 
011100       05 WS-FLRADFEL-TAB          PIC X(1).                              
011200       05 WS-KVAVBART-KDFEL-TAB    PIC X(3).                              
011300       05 WS-KVAVBART-TAB          PIC 9(6).                              
011400       05 WS-KVAVBART-TAB-ALFA     PIC X(6).                              
011500 01  FILLER                        PIC X(16) VALUE 'NY RAD'.              
011600 01  NY-ARTIKEL-RAD.                                                      
011700       03 WS-FLNYRADFEL            PIC X(1).                              
011800       03 WS-IDARTNR-NY            PIC 9(8) VALUE ZERO.                   
011900       03 WS-KVAVBART-NY           PIC 9(7) VALUE ZERO.                   
012000                                                                          
012100     03 DAGENS-DATUM             PIC 9(8)       VALUE ZERO.               
012200     03 DAGENS-DATUM-GRP         REDEFINES DAGENS-DATUM.                  
012300        05 DAGENS-DATUM-DAAA           PIC 9(2).                          
012400        05 DAGENS-DATUM-DAAAMMDD       PIC 9(6).                          
012500                                                                          
012600     03 DAGENS-DAAAAAPP          PIC 9(6)   VALUE ZERO.                   
012700     03 DAGENS-DAAAPP-GRP        REDEFINES DAGENS-DAAAAAPP.               
012800        05 DAGENS-DAAA                 PIC 9(2).                          
012900        05 DAGENS-DAAAPP               PIC 9(4).                          
008700                                                                          
008800 01  WS-CURRENT-DATE-TIME.                                                
008900     03  WS-YEAR                 PIC 9(4).                                
009000     03  WS-MONTH                PIC 9(2).                                
009100     03  WS-DAY                  PIC 9(2).                                
009200     03  WS-HOUR                 PIC 9(2).                                
009300     03  WS-MINUTE               PIC 9(2).                                
009400 01  FILLER REDEFINES WS-CURRENT-DATE-TIME.                               
009500     03  FILLER                  PIC X(2).                                
009600     03  WS-TIYYMMDDHHMM         PIC X(10).                               
013000                                                                          
013100                                                                          
013200 01  ERROR-CODES.                                                         
013300     03  NO-ERROR                    PIC X(3) VALUE '000'.                
013400     03  NOT-NUMERIC                 PIC X(3) VALUE '001'.                
013500     03  SCRAPPED-MORE-THEN-RECIVED  PIC X(3) VALUE '002'.                
013600     03  LINE-NOT-IN-DATABASE        PIC X(3) VALUE '003'.                
013700     03  PART-NOT-IN-PARTS-FILE      PIC X(3) VALUE '004'.                
013800     03  SCRAPPING-AND-NO-RECIVING   PIC X(3) VALUE '005'.                
013900     03  NOT-BYTES                   PIC X(3) VALUE '006'.                
014000     03  UPDATE-NOT-ALLOWED          PIC X(3) VALUE '007'.                
014100     03  MISSING-IN-REGISTER         PIC X(3) VALUE '010'.                
014200     EJECT                                                                
014300                                                                          
014400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014500 01  GENERELLA-SUBPROGRAM.                                                
014600*    03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014700     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
014800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015300     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
015400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
015400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
015500     EJECT                                                                
015600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015700****01 -COPY WMEDAREA                                                     
015800*    SKIP3                                                                
015900*01  MESSAGE-CODES.                                                       
016000*    03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
016100*    03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
016200*    03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
016300*    03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016400*    03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
016500*    03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
016600*    03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
           EJECT                                                                
016800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016900*                                                                         
017000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
017100     SKIP3                                                                
017200*    --- PARAMETRAR TILL ABEND                                            
017300                                                                          
017400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017600     SKIP2                                                                
017700*01 -COPY WMSGINIT                                                        
017600     EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
017700*01 -COPY WZ01SEND                                                        
017800     EJECT                                                                
028800 01  SEND-AREA-TO-CSV-FIL.                                                
           03 CSV-IDDISTR              PIC Z(3)9.                               
           03 FILLER                   PIC X(1) VALUE ';'.                      
           03 CSV-DAORDREG             PIC 9(8).                                
           03 FILLER                   PIC X(1) VALUE ';'.                      
           03 CSV-IDORDER              PIC Z(6)9.                               
           03 FILLER                   PIC X(1) VALUE ';'.                      
           03 CSV-IDARTNR              PIC Z(7)9.                               
           03 FILLER                   PIC X(1) VALUE ';'.                      
           03 CSV-BEART                PIC X(25).                               
           03 FILLER                   PIC X(1) VALUE ';'.                      
           03 CSV-KVLEVART             PIC Z(6)9.                               
           03 FILLER                   PIC X(1) VALUE ';'.                      
           03 CSV-KVAVBART             PIC Z(6)9.                               
           03 FILLER                   PIC X(1) VALUE ';'.                      
           03 CSV-KVACCEPT             PIC Z(6)9.                               
           03 FILLER                   PIC X(1) VALUE ';'.                      
                                                                                
028800 01  SEND-AREA-TO-CSV-RUB.                                                
           03 FILLER                  PIC X(14) VALUE 'REMANUFACTURER'.         
           03 FILLER                  PIC X(1)  VALUE ';'.                      
           03 FILLER                  PIC X(13) VALUE 'ORDER REGDATE'.          
           03 FILLER                  PIC X(1)  VALUE ';'.                      
           03 FILLER                  PIC X(8)  VALUE 'ORDER NO'.               
           03 FILLER                  PIC X(1)  VALUE ';'.                      
           03 FILLER                  PIC X(7)  VALUE 'CORE NO'.                
           03 FILLER                  PIC X(1)  VALUE ';'.                      
           03 FILLER                  PIC X(11) VALUE 'DESCRIPTION'.            
           03 FILLER                  PIC X(1) VALUE ';'.                       
           03 FILLER                  PIC X(13) VALUE 'DELIVERED QTY'.          
           03 FILLER                  PIC X(1) VALUE ';'.                       
           03 FILLER                  PIC X(12) VALUE 'RECEIVED QTY'.           
           03 FILLER                  PIC X(1) VALUE ';'.                       
           03 FILLER                  PIC X(12) VALUE 'ACCEPTED QTY'.           
           03 FILLER                  PIC X(1) VALUE ';'.                       
                                                                                
028800 01  HDR-AREA.                                                            
028900*    03 -COPY WZ01REQU -PRE HDR-                                          
029000*    03 -COPY WZ04HDR                                                     
017800     EJECT                                                                
017900*    --- PARAMETRAR TILL DATKONV                                          
018000*                                                                         
018100*01  -COPY WDATAREA                                                       
018200     EJECT                                                                
018300*01  -COPY WDECAREA                                                       
018400     EJECT                                                                
018500*01   -COPY  WSECAREA                                                     
018600     EJECT                                                                
018700 01  DISTRIKT                    PIC X(24) VALUE                          
018800                                 'DISTRIKTCOPYTEXT'.                      
018900 01  TEST-IDDISTR       PIC 9(5)   COMP-3.                                
019000*01  FILLER -COPY WWDIS134    -RED TEST-IDDISTR.                          
019100     EJECT                                                                
019200 01  BYTESENHETER                PIC X(24) VALUE                          
019300                                 'BYTES-AREA-START'.                      
019400 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
019500                                                                          
019600*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
019700     EJECT                                                                
019800                                                                          
019900*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
020000*                                                                         
020100 01  FILLER                      PIC X(16)   VALUE 'SPARAREA'.            
020200*01  SPAR-AREA.                                                           
020300*    03  SPAR-IDTRANS           PIC X(4)    VALUE '3178'.                 
020400*    03  SPAR-IDDISTR-ENTER       PIC S9(1)        COMP-3.                
020500*    03  SPAR-IDDISTR-NEXT        PIC S9(1)        COMP-3.                
020600*    03  SPAR-IDORDNR-ENTER       PIC S9(1)        COMP-3.                
020700*    03  SPAR-IDORDNR-NEXT        PIC S9(1)        COMP-3.                
020800*    03  SPAR-DAORDREG-ENTER      PIC S9(1)        COMP-3.                
020900*    03  SPAR-DAORDREG-NEXT       PIC S9(1)        COMP-3.                
021000     EJECT                                                                
021100 77  WS-IDDISTR-NUM               PIC 9(4)   VALUE ZERO.                  
021200 77  WS-DAORDREG-NUM              PIC 9(8)   VALUE ZERO.                  
021300 77  WS-IDORDER-NUM               PIC 9(7)   VALUE ZERO.                  
021400 77  WS-IDRADNR-NUM               PIC 9(5)   VALUE ZERO.                  
021500                                                                          
021600 01  FILLER                      PIC X(16)   VALUE 'BLADDRING'.           
021700*    -- BLÄDDRINGSRADNUMMER PÅ FÖRSTA RAD SOM VISAS PÅ WÄBB-SIDAN         
021800*    -- OBS: HAR INGET MED ORDER-RADNUMMER ATT GÖRA!                      
021900 77  WS-IDDIARAD-START            PIC S9(5)  COMP-3 VALUE +1.             
022000                                                                          
022100*    -- AKTUELLT ANTAL IFYLLDA RADER SOM LÄSTS IN FRÅN WEB-SIDAN          
022200 77  WS-KVDIARAD-AKT-IN           PIC S9(5)  COMP-3 VALUE +0.             
022300                                                                          
022400*    -- MAX ANTAL RADER SOM FÅR SKRIVAS UT TILL WÄBB-SIDAN                
022500 77  WS-KVDIARAD-MAX              PIC S9(5)  COMP-3 VALUE +1.             
022600                                                                          
022700*    -- AKTUELLT ANTAL RADER SOM SKRIVS UT PÅ WEB-SIDAN                   
022800 77  WS-KVDIARAD-AKT-UT           PIC S9(5)  COMP-3 VALUE +0.             
022900                                                                          
023000*    -- SENASTE UNIKA ORDER                                               
023100 77  SPAR-IDORDER                 PIC S9(7)  COMP-3 VALUE ZERO.           
023300*                                                                         
023400*                                                                         
023500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
023600*                                                                         
023700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
023800     SKIP3                                                                
023900*01  -COPY WZ01SUB                                                        
024000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
024100     SKIP3                                                                
024200     SKIP3                                                                
024300 01  MID-AREA.                                                            
024400*    03 -COPY WMIDPRE2                                                    
024500*    03 -COPY W30178I1                                                    
024600     EJECT                                                                
024700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
024800     SKIP3                                                                
024900 01  MOD-AREA.                                                            
025000*    03  -COPY WMODPRE2                                                   
025100*    03  -COPY W30178O1                                                   
025200     EJECT                                                                
025300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025400     SKIP3                                                                
025500 01  MFS-IDMOD                   PIC X(8).                                
025600     EJECT                                                                
025700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025800*                                                                         
025900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026000     SKIP3                                                                
026100 01  NYCKLAR-TILL-DLI.                                                    
026200*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
026300     03  W-IDDISTR-MIN-X.                                                 
026400         05  W-IDDISTR-MIN     PIC S9(5)        COMP-3.                   
026500                                                                          
026600     03  W-IDORDNR-MIN-X.                                                 
026700         05  W-IDORDNR-MIN     PIC S9(7)        COMP-3.                   
026800                                                                          
026900     03  W-DAORDREG-MIN-X.                                                
027000         05  W-DAORDREG-MIN     PIC 9(8)        COMP-3.                   
027100                                                                          
027200     03  W-IDARTNR-X.                                                     
027300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
027400                                                                          
027500     03  W-IDSKYLT-X.                                                     
027600         05  W-IDSKYLT           PIC XXX     VALUE 'GB '.                 
027700                                                                          
027800     03  W-WDGXKEY-X.                                                     
027900         05 W-3161-IDHTYP        PIC X(4)    VALUE '3161'.                
028000         05 W-3161-IDDISTR       PIC S9(5)   COMP-3.                      
028100         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
028200                                                                          
028300     03  W-KY3162-X.                                                      
028400         05 W-3162-DAORDREG      PIC  9(8)  VALUE ZERO.                   
028500         05 W-3162-IDORDER       PIC S9(7)  VALUE ZERO   COMP-3.          
028600         05 W-3162-IDARTNR       PIC S9(9)  VALUE ZERO   COMP-3.          
028700         05 W-3162-IDRADNR       PIC S9(5)  VALUE ZERO COMP-3.            
028800                                                                          
028900     03  W-KY3162-MIN-X.                                                  
029000         05 W-3162-DAORDREG-MIN PIC 9(8)  VALUE ZERO.                     
029100         05 W-3162-IDORDER-MIN PIC S9(7)  VALUE ZERO   COMP-3.            
029200         05 W-3162-IDARTNR-MIN PIC S9(9)  VALUE ZERO   COMP-3.            
029300         05 W-3162-IDRADNR-MIN PIC S9(5)  VALUE +00001 COMP-3.            
029400                                                                          
029500     03  W-KY3162-MAX-X.                                                  
029600         05 W-3162-DAORDREG-MAX PIC 9(8)  VALUE 99999999.                 
029700         05 W-3162-IDORDER-MAX PIC S9(7)  VALUE +9999999  COMP-3.         
029800         05 W-3162-IDARTNR-MAX PIC S9(9)  VALUE +99999999 COMP-3.         
029900         05 W-3162-IDRADNR-MAX PIC S9(5)  VALUE +99999    COMP-3.         
030000                                                                          
030100     03  W-A9-IDARTNR-X.                                                  
030200         05 W-A9-IDARTNR       PIC S9(9)  VALUE ZERO  COMP-3.             
030300     03  W-A9-IDDISTR-X.                                                  
030400         05 W-A9-IDDISTR       PIC S9(5)  VALUE ZERO  COMP-3.             
030500     03  W-A9-DAAAPP-X.                                                   
030600         05  W-A9-DAAAPP       PIC  9(6)  VALUE ZERO.                     
030700     SKIP2                                                                
030800*    --- STATUS-KOD FRÅN IMS                                              
030900 01  STATUS-WS                   PIC XX.                                  
031000     88  SEGMENT-FINNS                       VALUE '  '.                  
031100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
031200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031300     SKIP2                                                                
031400 01  GODK-STATUSKODER.                                                    
031500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031600     SKIP3                                                                
031700 01  SSA1                        PIC X(128).                              
031800 01  SSA2                        PIC X(128).                              
031900 01  SSA3                        PIC X(128).                              
032000     EJECT                                                                
032100*    --- IMS FUNKTIONSKODER                                               
032200*01  -COPY W0003                                                          
032300     EJECT                                                                
032400*    ---  DLI INPUT-OUTPUT AREA                                           
032500                                                                          
032600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
032700 01  DLI-IO-WDK601.                                                       
032800*    03  -COPY WDK601                                                     
032900     EJECT                                                                
033000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
033100 01  DLI-IO-WDD311.                                                       
033200*    03  -COPY WDD311                                                     
033300     EJECT                                                                
033400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3161'.                    
033500 01  DLI-IO-WDGX3161.                                                     
033600*    03  -COPY WDGX3161                                                   
033700     EJECT                                                                
033800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3162'.                    
033900 01  DLI-IO-WDGX3162.                                                     
034000*    03  -COPY WDGX3162                                                   
034100     EJECT                                                                
034200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA901'.                      
034300 01  DLI-IO-WDA901.                                                       
034400*    03  -COPY WDA901                                                     
034500     EJECT                                                                
034600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA911'.                      
034700 01  DLI-IO-WDA911.                                                       
034800*    03  -COPY WDA911                                                     
034900     EJECT                                                                
035000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA921'.                      
035100 01  DLI-IO-WDA921.                                                       
035200*    03  -COPY WDA921                                                     
035300     EJECT                                                                
035400 LINKAGE SECTION.                                                         
035500*01  -COPY W0009   -PRE MSG-                                              
                                                                                
035500*01  -COPY W0009   -PRE DISTRDOC-                                         
                                                                                
035600*01  -COPY W0008   -PRE USEA-                                             
035700     05  FILLER                  PIC X.                                   
035800                                                                          
035900*01  -COPY W0008  -PRE WDK6-                                              
036000     05  FILLER                  PIC X.                                   
036100                                                                          
036200*01  -COPY W0008  -PRE WDD3-                                              
036300     05  FILLER                  PIC X.                                   
036400                                                                          
036500*01  -COPY W0008  -PRE 3161-                                              
036600     05  FILLER                  PIC X.                                   
036700                                                                          
036800*01  -COPY W0008  -PRE WDA9-                                              
036900     05  FILLER                  PIC X.                                   
037000     EJECT                                                                
037100 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB USEA-PCB WDK6-PCB         
037200     WDD3-PCB 3161-PCB WDA9-PCB.                                          
037300 MAIN SECTION.                                                            
037400     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB USEA-PCB WDK6-PCB         
037500     WDD3-PCB 3161-PCB WDA9-PCB.                                          
037600                                                                          
037700     PERFORM S20-HAEMTA-ANROPSDATA                                        
037800     IF SUB-KDRC = 0                                                      
037900       PERFORM A-INIT                                                     
038000       PERFORM B-KOLLA-NYCKLAR                                            
038100       IF NYCKLAR-OK                                                      
038200         IF MID-KDPGMACT = 'U'                                            
038300           PERFORM G-KOLLA-INPUT                                          
038400                                                                          
038500           IF INDATA-OK                                                   
038600                                                                          
038700             PERFORM H-UPPDATERA                                          
038800           END-IF                                                         
038900         ELSE                                                             
039000           EVALUATE TRUE                                                  
039100             WHEN MID-KDPGMACT = 'Q' OR 'T' OR 'E'                        
039200               PERFORM C-FOERSTA-SIDA                                     
039300             WHEN MID-KDPGMACT = 'P'                                      
039400               PERFORM D1-FOREGANDE-SIDA                                  
039500             WHEN MID-KDPGMACT = 'N'                                      
039600               PERFORM D2-NAESTA-SIDA                                     
039700             WHEN OTHER                                                   
039800               CONTINUE                                                   
039900           END-EVALUATE                                                   
040000         END-IF                                                           
040100         PERFORM F-LAES-VISA-INFO                                         
040200       END-IF                                                             
040300       PERFORM S21-RETURNERA-SVAR                                         
040400      END-IF                                                              
040500                                                                          
040600*    CALL ABEND                                                           
040700                                                                          
040800     MOVE ZERO TO RETURN-CODE                                             
040900     GOBACK                                                               
041000     .                                                                    
041100     EJECT                                                                
041200 A-INIT SECTION.                                                          
041300     MOVE 'A-INIT'   TO WS-IMS-SECTION                                    
041400     MOVE 'W30178O1' TO MFS-IDMOD                                         
041500*****MOVE SPACE TO  MOD-W30178O1                                          
041600     MOVE ALL '+' TO  MOD-W30178O1                                        
041700*****INITIALIZE MOD-W30178O1                                              
041800     INITIALIZE MOD-MODPREF                                               
041900     MOVE MID-KDPGMACT   TO MOD-KDPGMACT                                  
042000     MOVE MID-KDDIASTATE TO MOD-KDDIASTATE                                
042100     MOVE ZERO           TO MOD-KVDIARAD-AKTUELLT                         
042200                                                                          
042300     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
042400     MOVE DAGENS-DATUM-DAAAMMDD      TO DAT-I-TIDATUM                     
042500     MOVE 'AAMMDD'                   TO DAT-KDDATFORM                     
042600                                                                          
042700     CALL WDATKONV USING DAT-KDDATFORM                                    
042800                         DAT-I-TIDATUM                                    
042900                         DAT-O-TIDATUM                                    
043000                         DAT-KDSVAR                                       
043100                                                                          
043200     IF DAT-KDSVAR-OK                                                     
043300       MOVE DAT-TIAAPP   TO DAGENS-DAAAPP                                 
043400     ELSE                                                                 
043500       MOVE 'FEL FRÅN WDATKONV 1  I A-INIT SECTION I W30178'              
043600                               TO FELTEXT                                 
043700       PERFORM S99-ABEND                                                  
043800     END-IF                                                               
043900     MOVE DAGENS-DATUM-DAAA    TO DAGENS-DAAA                             
044000     .                                                                    
044100     EJECT                                                                
044200                                                                          
044300 B-KOLLA-NYCKLAR SECTION.                                                 
044400     MOVE 'B-KOLLA'          TO WS-IMS-SECTION                            
044500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
044600     MOVE '001'             TO MSGI-KDCALL                                
044700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
044800     MOVE MID-IDUSER        TO MSGI-IDUSER                                
044900     MOVE '3178'            TO MSGI-IDTRANS                               
045000                                                                          
045100*    -- LAGRA/HÄMTA NEDANSTÅENDE I/FRÅN USER-BASEN                        
045200     MOVE MID-IDDISTR       TO MSGI-IDDISTR                               
045300                                                                          
045400     CALL W005INIT USING  MSGI-WMSGINIT USEA-PCB                          
045500                                                                          
045600     MOVE JA TO NYCKLAR-SW                                                
045700*                                                                         
045800*    -- KONTROLL AV DISTRIKT                                              
045900*                                                                         
046000     MOVE MSGI-IDDISTR TO MOD-IDDISTR                                     
046100**** MOVE MSGI-IDDISTR TO WS-NYCKELFAELT.                                 
046200**** MOVE WS-NYCKELGRUPP TO MOD-IDDISTR                                   
046300     MOVE MSGI-IDSPRAK   TO MOD-IDSPRAK                                   
046400     IF MSGI-IDDISTR NOT = ALL '+'                                        
046500       IF MSGI-IDDISTR NUMERIC                                            
046600         MOVE MSGI-IDDISTR   TO TEST-IDDISTR                              
046700         MOVE MSGI-IDDISTR   TO WS-IDDISTR-NUM                            
046800         IF DIS134-BYTESRENOV                                             
046900           MOVE MSGI-IDDISTR TO WS-IDDISTR-NUM                            
047000                                MID-IDDISTR                               
047100         ELSE                                                             
047200           MOVE '109 REMANUFACTURER IS INVALID'                           
047300                             TO MOD-TEWEBERR                              
047400           MOVE NEJ          TO NYCKLAR-SW                                
047500         END-IF                                                           
047600       ELSE                                                               
047700         MOVE '110 REMANUFACTURER MUST BE NUMERIC'                        
047800                             TO MOD-TEWEBERR                              
047900         MOVE NEJ            TO NYCKLAR-SW                                
048000       END-IF                                                             
048100     ELSE                                                                 
048200       MOVE '111 REMANUFACTURER MUST BE ENTERED'                          
048300                             TO MOD-TEWEBERR                              
048400       MOVE NEJ              TO NYCKLAR-SW                                
048500     END-IF                                                               
048600                                                                          
048700*    -- KONTROLL AV ORDERDATUM                                            
048800*                                                                         
048900     IF NYCKLAR-OK                                                        
049000       IF MID-DAORDREG NOT = ALL '+'                                      
049100         IF MID-DAORDREG NUMERIC                                          
049200           MOVE MID-DAORDREG TO WS-DAORDREG-NUM                           
049300           MOVE WS-DAORDREG-NUM TO DAT-I-TIDATUM                          
049400           MOVE 'AAMMDD'        TO DAT-KDDATFORM                          
049500                                                                          
049600           CALL WDATKONV USING DAT-KDDATFORM                              
049700                               DAT-I-TIDATUM                              
049800                               DAT-O-TIDATUM                              
049900                               DAT-KDSVAR                                 
050000                                                                          
050100           IF DAT-KDSVAR-OK                                               
050200             MOVE DAT-TIAAPP  TO DAGENS-DAAAPP                            
050300             IF WS-DAORDREG-NUM < 999999                                  
050400               IF WS-DAORDREG-NUM < 500000                                
050500                 ADD 20000000 TO WS-DAORDREG-NUM                          
050600               ELSE                                                       
050700                 ADD 19000000 TO WS-DAORDREG-NUM                          
050800               END-IF                                                     
050900             END-IF                                                       
051000           ELSE                                                           
051100             MOVE '023 DATE IS NOT VALID'                                 
051200                              TO MOD-TEWEBERR                             
051300             MOVE NEJ         TO NYCKLAR-SW                               
051400           END-IF                                                         
051500         ELSE                                                             
051600           MOVE '024 DATE MUST BE NUMERIC'                                
051700                              TO MOD-TEWEBERR                             
051800           MOVE NEJ TO NYCKLAR-SW                                         
051900         END-IF                                                           
052000       END-IF                                                             
052100     END-IF                                                               
052200                                                                          
052300*    -- KONTROLL AV ORDERNUMMER                                           
052400     IF NYCKLAR-OK                                                        
052500       IF MID-IDORDER NOT = ALL '+'                                       
052600         IF MID-IDORDER NUMERIC                                           
052700           MOVE MID-IDORDER TO WS-IDORDER-NUM                             
052800         ELSE                                                             
052900           MOVE '024 ORDER NO MUST BE NUMERIC'                            
053000                            TO MOD-TEWEBERR                               
053100           MOVE NEJ         TO NYCKLAR-SW                                 
053200         END-IF                                                           
053300       END-IF                                                             
053400     END-IF                                                               
053500*                                                                         
053600*    -- LAGRING AV RAD-STARTVÄRDEN                                        
053700*                                                                         
053800     IF MID-IDDIARAD NOT = ALL '+'                                        
053900       IF MID-IDDIARAD NUMERIC                                            
054000         MOVE MID-IDDIARAD TO WS-IDDIARAD-START                           
054100       ELSE                                                               
054200*        -- STARTA FRÅN BÖRJAN OM INGET ANNAT SÄGS                        
054300         MOVE 1 TO WS-IDDIARAD-START                                      
054400       END-IF                                                             
054500     ELSE                                                                 
054600       MOVE 1 TO WS-IDDIARAD-START                                        
054700     END-IF                                                               
054800                                                                          
054900     IF MID-KVDIARAD-MAX NOT = ALL '+' AND                                
              MID-KDPGMACT NOT = 'E'                                            
055000       IF MID-KVDIARAD-MAX NUMERIC                                        
055100         MOVE MID-KVDIARAD-MAX TO WS-KVDIARAD-MAX                         
055200       ELSE                                                               
055300*        -- ÖNSKAT MAX-VÄRDE SKA VARA IFYLLT!                             
055400*        -- OM INTE, GE DOM BARA EN RAD                                   
055500         MOVE 500 TO WS-KVDIARAD-MAX                                      
055600       END-IF                                                             
055700     ELSE                                                                 
055800       MOVE 500 TO WS-KVDIARAD-MAX                                        
055900     END-IF                                                               
056000                                                                          
056100*    -- KONTROLLERA OM PERSONEN FÅR ANVÄNDA DISTRIKTET                    
056200     MOVE MSGI-IDUSER    TO SEC-IDUSER                                    
056300     MOVE WS-EGEN-BILD   TO SEC-IDTRANS                                   
056400     MOVE WS-IDDISTR-NUM TO SEC-IDKEY                                     
056500     CALL WSECURIT USING    SEC-IDUSER                                    
056600                            SEC-IDTRANS                                   
056700                            SEC-IDKEY                                     
056800                            SEC-KDSVAR                                    
056900     IF SEC-KDSVAR = 'F'                                                  
057000       MOVE                                                               
057100         '114 YOUR ID IS NOT AUTHORIZED FOR THIS REMANUFACTURER'          
057200                                  TO MOD-TEWEBERR                         
057300       MOVE NEJ                   TO NYCKLAR-SW                           
057400     END-IF                                                               
057500*                                                                         
057600     IF NYCKLAR-FEL                                                       
057700       CONTINUE                                                           
057800     ELSE                                                                 
057900       IF WS-IDDISTR-NUM    > ZERO                                        
058000       AND WS-DAORDREG-NUM  = ZERO                                        
058100       AND WS-IDORDER-NUM   = ZERO                                        
058200         SET SEARCH-ORDERS TO TRUE                                        
058300       END-IF                                                             
058400                                                                          
058500       IF WS-IDDISTR-NUM    > ZERO                                        
058600       AND WS-DAORDREG-NUM  > ZERO                                        
058700       AND WS-IDORDER-NUM   = ZERO                                        
058800         SET SEARCH-ORDERS-BY-DATE TO TRUE                                
058900       END-IF                                                             
059000                                                                          
059100       IF WS-IDDISTR-NUM    > ZERO                                        
059200       AND WS-DAORDREG-NUM  > ZERO                                        
059300       AND WS-IDORDER-NUM   > ZERO                                        
059400         IF MID-KDPGMACT = 'U'                                            
059500            SET UPDATE-ORDER-LINES TO TRUE                                
059600         ELSE                                                             
059400            IF MID-KDPGMACT = 'E'                                         
059500               SET CREATE-CSV-FILE TO TRUE                                
                  ELSE                                                          
059700               SET SEARCH-ORDER-LINES TO TRUE                             
059800            END-IF                                                        
059800         END-IF                                                           
059900       END-IF                                                             
060000     END-IF                                                               
060100     .                                                                    
060200     EJECT                                                                
060300 C-FOERSTA-SIDA SECTION.                                                  
060400     MOVE 'C-FOERSTA'   TO WS-IMS-SECTION                                 
060500     MOVE '010 FIRST PAGE SHOWN'                                          
060600                         TO MOD-TEWEBINF                                  
060700                                                                          
060800     MOVE 1 TO WS-IDDIARAD-START                                          
060900     .                                                                    
061000     EJECT                                                                
061100 D1-FOREGANDE-SIDA SECTION.                                               
061200     MOVE 'D-FOREGANDE'   TO WS-IMS-SECTION                               
061300     SUBTRACT WS-KVDIARAD-MAX FROM WS-IDDIARAD-START                      
061400     IF WS-IDDIARAD-START < ZERO                                          
061500       MOVE 1 TO WS-IDDIARAD-START                                        
061600       MOVE '010 FIRST PAGE SHOWN'                                        
061700                                   TO MOD-TEWEBINF                        
061800     END-IF                                                               
061900     IF WS-IDDIARAD-START = 1                                             
062000       MOVE '010 FIRST PAGE SHOWN'                                        
062100                                   TO MOD-TEWEBINF                        
062200     END-IF                                                               
062300     .                                                                    
062400     EJECT                                                                
062500 D2-NAESTA-SIDA SECTION.                                                  
062600     MOVE 'D2-NAESTA'   TO WS-IMS-SECTION                                 
062700     ADD WS-KVDIARAD-MAX TO WS-IDDIARAD-START                             
062800     .                                                                    
062900     EJECT                                                                
063000 F-LAES-VISA-INFO SECTION.                                                
063100     MOVE 'F-LAES-VISA'   TO WS-IMS-SECTION                               
063200     PERFORM FA-LAES-GRUNDDATA                                            
063300                                                                          
063400     IF SEGMENT-SAKNAS                                                    
063500       MOVE '112 REMANUFACTURER NOT FOUND'                                
063600                             TO MOD-TEWEBERR                              
063700     ELSE                                                                 
063800*      -- POSITIONERA FÖR LÄSNING AV DATA                                 
063900*       W-3162-IDARTNR-MIN, OCH MAX ÄR DEFAULT I NYCKELAREAN              
064000                                                                          
064100       IF WS-DAORDREG-NUM > ZERO                                          
064200         MOVE WS-DAORDREG-NUM TO W-3162-DAORDREG-MIN                      
064300                                 W-3162-DAORDREG-MAX                      
064400       END-IF                                                             
064500                                                                          
064600       IF WS-IDORDER-NUM > ZERO                                           
064700         MOVE WS-IDORDER-NUM TO W-3162-IDORDER-MIN                        
064800                                W-3162-IDORDER-MAX                        
064900       END-IF                                                             
065000                                                                          
065100       PERFORM FB-LAES-RADDATA                                            
065200                                                                          
065300     END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 FA-LAES-GRUNDDATA SECTION.                                               
065700     MOVE 'FA-LAES-GRUND'  TO WS-IMS-SECTION                              
065800     MOVE WS-IDDISTR-NUM TO W-3161-IDDISTR                                
065900     PERFORM IMS-GU-WDGX3161                                              
066000     .                                                                    
066100     EJECT                                                                
066200 FB-LAES-RADDATA SECTION.                                                 
066300     MOVE 'FB-LAES-RADDATA'   TO WS-IMS-SECTION                           
066400     IF SEARCH-ORDERS         PERFORM FBA-ORDERS          END-IF          
066500     IF SEARCH-ORDERS-BY-DATE PERFORM FBA-ORDERS          END-IF          
066600                                                                          
066700     IF SEARCH-ORDER-LINES    PERFORM FBC-ORDER-LINES     END-IF          
066700     IF CREATE-CSV-FILE       PERFORM FBC-ORDER-LINES     END-IF          
066800     IF UPDATE-ORDER-LINES    PERFORM FBD-UPDATE-LINES    END-IF          
066900                                                                          
067000     MOVE WS-IDDIARAD-START   TO MOD-IDDIARAD-START                       
067100     MOVE WS-KVDIARAD-AKT-UT  TO MOD-KVDIARAD-AKTUELLT                    
067200     .                                                                    
067300     EJECT                                                                
067400 FBA-ORDERS            SECTION.                                           
067500     MOVE 'FBA-ORDERS'   TO WS-IMS-SECTION                                
067600     IF SEARCH-ORDERS-BY-DATE                                             
067700       MOVE WS-DAORDREG-NUM TO W-3162-DAORDREG-MIN                        
067800                               W-3162-DAORDREG-MAX                        
067900     END-IF                                                               
068000     PERFORM IMS-GNP-WDGX3162-MIN-MAX                                     
068100                                                                          
068200*    -- SÖK FRAM TILL FÖRSTA AKTUELLA RAD (VID EV BLÄDDRING)              
068300     MOVE 1 TO INDX                                                       
068400     PERFORM UNTIL SEGMENT-SAKNAS                                         
068500             OR INDX >= WS-IDDIARAD-START                                 
068600       MOVE 3162-IDORDER   TO  SPAR-IDORDER                               
068700*      -- SKIPPA ÖVRIGA ORDERRADER MED SAMMA ORDERNR                      
068800       PERFORM UNTIL SEGMENT-SAKNAS                                       
068900               OR 3162-IDORDER NOT = SPAR-IDORDER                         
069000         PERFORM IMS-GNP-WDGX3162-MIN-MAX                                 
069100       END-PERFORM                                                        
069200       ADD 1 TO INDX                                                      
069300     END-PERFORM                                                          
069400                                                                          
069500*    -- DAX ATT BÖRJA FYLLA I MODEN                                       
069600     MOVE ZERO TO INDX                                                    
069700     PERFORM UNTIL SEGMENT-SAKNAS                                         
069800             OR INDX >= WS-KVDIARAD-MAX                                   
069900                                                                          
070000       ADD 1 TO INDX                                                      
070100       MOVE 'J'            TO MOD-FLORDER-RAD(INDX)                       
070200       MOVE 3162-IDRADNR   TO MOD-IDRADNR-RAD(INDX)                       
070300       MOVE 3162-DAORDREG  TO MOD-DAORDREG-RAD(INDX)                      
070400       MOVE 3162-IDORDER   TO MOD-IDORDER-RAD(INDX)                       
070500                              SPAR-IDORDER                                
070600*      MOVE 'J' TO MID-FLVISA                                             
070700       IF MID-FLVISA = 'J'                                                
070800*        -- SKIPPA ÖVRIGA ORDERRADER MED SAMMA ORDERNR                    
070900         PERFORM UNTIL SEGMENT-SAKNAS                                     
071000                 OR 3162-IDORDER NOT = SPAR-IDORDER                       
071100           IF 3162-KVAVBART = 0 AND                                       
071200              (3162-KVAVIS > 0 AND                                        
071300              3162-DAREGDAT = 0) OR                                       
071400              (3162-KVAVIS = 0 AND                                        
071500              3162-DAREGDAT = 0)                                          
071600             MOVE 'N'      TO MOD-FLORDER-RAD(INDX)                       
071700           END-IF                                                         
071800           IF MOD-FLORDER-RAD(INDX) NOT = 'N'                             
071900             IF 3162-KVAVIS = 3162-KVAVBART                               
072000               CONTINUE                                                   
072100             ELSE                                                         
072200               MOVE 'F'      TO MOD-FLORDER-RAD (INDX)                    
072300             END-IF                                                       
072400           END-IF                                                         
072500           PERFORM IMS-GNP-WDGX3162-MIN-MAX                               
072600         END-PERFORM                                                      
072700       ELSE                                                               
072800         MOVE 'N'          TO WS-VISA                                     
072900         MOVE 'F'          TO MOD-FLORDER-RAD(INDX)                       
073000         PERFORM UNTIL SEGMENT-SAKNAS                                     
073100                 OR 3162-IDORDER NOT = SPAR-IDORDER                       
073200           IF (3162-KVAVIS   > 0 AND                                      
073300              3162-KVAVBART = 0 AND                                       
073400              3162-DAREGDAT = ZERO) OR                                    
073500              (3162-KVAVIS = 0 AND                                        
073600              3162-KVAVBART = 0 AND                                       
073700              3162-DAREGDAT = ZERO)                                       
073800                                                                          
073900             MOVE 'J'      TO WS-VISA                                     
074000           END-IF                                                         
074100           PERFORM IMS-GNP-WDGX3162-MIN-MAX                               
074200         END-PERFORM                                                      
074300         IF WS-VISA = 'N'                                                 
074400           ADD -1 TO INDX                                                 
074500         END-IF                                                           
074600       END-IF                                                             
074700     END-PERFORM                                                          
074800     MOVE INDX TO WS-KVDIARAD-AKT-UT                                      
074900     .                                                                    
075000     EJECT                                                                
075100 FBC-ORDER-LINES       SECTION.                                           
075200     MOVE 'FBC-ORDERS-LINES'   TO WS-IMS-SECTION                          
075300     MOVE WS-DAORDREG-NUM TO W-3162-DAORDREG-MIN                          
075400                             W-3162-DAORDREG-MAX                          
075500     MOVE WS-IDORDER-NUM TO W-3162-IDORDER-MIN                            
075600                            W-3162-IDORDER-MAX                            
075700     PERFORM IMS-GNP-WDGX3162-MIN-MAX                                     
075800                                                                          
075900*    -- SÖK FRAM TILL FÖRSTA AKTUELLA RAD (VID EV BLÄDDRING)              
076000     MOVE 1 TO INDX                                                       
076100     PERFORM UNTIL SEGMENT-SAKNAS                                         
076200             OR INDX >= WS-IDDIARAD-START                                 
076300       PERFORM IMS-GNP-WDGX3162-MIN-MAX                                   
076400       ADD 1 TO INDX                                                      
076500     END-PERFORM                                                          
076600                                                                          
076700*    -- DAX ATT BÖRJA FYLLA I MODEN                                       
076800     MOVE ZERO TO INDX                                                    
076900     COMPUTE WS-KVRAD-TOT = WS-IDDIARAD-START - 1                         
077000     PERFORM UNTIL SEGMENT-SAKNAS                                         
077100             OR INDX >= WS-KVDIARAD-MAX                                   
077200                                                                          
077300       ADD 1 TO INDX                                                      
077400       ADD 1 TO WS-KVRAD-TOT                                              
077500       MOVE 3162-IDRADNR     TO MOD-IDRADNR-RAD(INDX)                     
077600       MOVE 3162-DAORDREG    TO MOD-DAORDREG-RAD(INDX)                    
077700       MOVE 3162-IDORDER     TO MOD-IDORDER-RAD(INDX)                     
077800       MOVE 3162-IDARTNR     TO MOD-IDARTNR-RAD(INDX)                     
077900                                W-IDARTNR                                 
078000                                                                          
078100       PERFORM IMS-GU-WDD3-BSEQ-BENA11                                    
078200       IF SEGMENT-FINNS                                                   
078300          MOVE TEXT-BEART    TO MOD-BEART-RAD(INDX)                       
078400       ELSE                                                               
078500          MOVE '********'    TO MOD-BEART-RAD(INDX)                       
078600       END-IF                                                             
078700       MOVE 3162-KVAVIS      TO MOD-KVLEVART-RAD(INDX)                    
078800                                                                          
078900       MOVE NO-ERROR         TO MOD-KDFEL-KVAVBART-RAD(INDX)              
079000       MOVE 3162-KVAVBART    TO MOD-KVAVBART-RAD(INDX)                    
079100                                                                          
079200       MOVE 3162-KVAVBART    TO WS-KVACCEPT                               
079300       MOVE WS-KVACCEPT      TO MOD-KVACCEPT-RAD(INDX)                    
             IF CREATE-CSV-FILE                                                 
                PERFORM FBCA-CREATE-CSV-FILE                                    
             END-IF                                                             
079400       PERFORM IMS-GNP-WDGX3162-MIN-MAX                                   
079500     END-PERFORM                                                          
079600     MOVE INDX TO WS-KVDIARAD-AKT-UT                                      
079700                                                                          
079800     IF SEGMENT-FINNS                                                     
079900       PERFORM UNTIL SEGMENT-SAKNAS                                       
080000         ADD +1            TO WS-KVRAD-TOT                                
080100         PERFORM IMS-GNP-WDGX3162-MIN-MAX                                 
080200       END-PERFORM                                                        
080300     END-IF                                                               
080400                                                                          
080500     MOVE WS-KVRAD-TOT TO MOD-KVRAD-TOT                                   
080600                                                                          
080700     MOVE NO-ERROR       TO MOD-KDFEL-AVBART-NY                           
080800                            MOD-KDFEL-IDARTNR-NY                          
080900     MOVE ZEROES         TO MOD-IDARTNR-NY                                
081000                            MOD-KVAVBART-NY                               
           IF CREATE-CSV-FILE AND (NOT FORSTA-TRAFF)                            
              PERFORM S25-SEND-CLOSE                                            
              MOVE 'COPY TO EXCEL FUNCTION STARTED' TO MOD-TEWEBINF             
           END-IF                                                               
081100     .                                                                    
081200     EJECT                                                                
081300 FBCA-CREATE-CSV-FILE SECTION.                                            
254000     IF FORSTA-TRAFF                                                      
254100        PERFORM S22-SEND-OPEN                                             
254200        PERFORM S23-PUT-HEADER                                            
              PERFORM S24-PUT-LINE                                              
254300        MOVE NEJ TO FORSTA-SW                                             
254400     END-IF                                                               
7600       MOVE WS-IDDISTR-NUM         TO CSV-IDDISTR                           
7600       MOVE MOD-DAORDREG-RAD(INDX) TO CSV-DAORDREG                          
7700       MOVE MOD-IDORDER-RAD(INDX)  TO CSV-IDORDER                           
7800       MOVE MOD-IDARTNR-RAD(INDX)  TO CSV-IDARTNR                           
8300       MOVE MOD-BEART-RAD(INDX)    TO CSV-BEART                             
8700       MOVE MOD-KVLEVART-RAD(INDX) TO CSV-KVLEVART                          
9000       MOVE MOD-KVAVBART-RAD(INDX) TO CSV-KVAVBART                          
9300       MOVE MOD-KVACCEPT-RAD(INDX) TO CSV-KVACCEPT                          
           PERFORM S24-PUT-LINE                                                 
081100     .                                                                    
081200     EJECT                                                                
081300 FBD-UPDATE-LINES SECTION.                                                
081400     MOVE 'FBD-UPDATE-LINES '   TO WS-IMS-SECTION                         
081500     MOVE WS-DAORDREG-NUM TO W-3162-DAORDREG-MIN                          
081600                             W-3162-DAORDREG-MAX                          
081700     MOVE WS-IDORDER-NUM TO W-3162-IDORDER-MIN                            
081800                            W-3162-IDORDER-MAX                            
081900     PERFORM IMS-GNP-WDGX3162-MIN-MAX                                     
082000                                                                          
082100*    -- SÖK FRAM TILL FÖRSTA AKTUELLA RAD (VID EV BLÄDDRING)              
082200     MOVE 1 TO INDX                                                       
082300     PERFORM UNTIL SEGMENT-SAKNAS                                         
082400             OR INDX >= WS-IDDIARAD-START                                 
082500       PERFORM IMS-GNP-WDGX3162-MIN-MAX                                   
082600       ADD 1 TO INDX                                                      
082700     END-PERFORM                                                          
082800                                                                          
082900*    -- DAX ATT BÖRJA FYLLA I MODEN                                       
083000     MOVE ZERO TO INDX                                                    
083100     MOVE ZERO TO WS-KVRAD-TOT                                            
083200     PERFORM UNTIL SEGMENT-SAKNAS                                         
083300             OR INDX >= WS-KVDIARAD-MAX                                   
083400                                                                          
083500       ADD 1 TO INDX                                                      
083600       MOVE 3162-IDRADNR   TO MOD-IDRADNR-RAD(INDX)                       
083700       MOVE 3162-DAORDREG  TO MOD-DAORDREG-RAD(INDX)                      
083800       MOVE 3162-IDORDER   TO MOD-IDORDER-RAD(INDX)                       
083900       MOVE 3162-IDARTNR   TO MOD-IDARTNR-RAD(INDX)                       
084000                              W-IDARTNR                                   
084100                                                                          
084200       MOVE MSGI-IDSPRAK   TO W-IDSKYLT                                   
084300       IF W-IDSKYLT = 'SV'                                                
084400          MOVE 'S'         TO W-IDSKYLT                                   
084500       END-IF                                                             
084600       IF W-IDSKYLT NOT = 'S'                                             
084700          MOVE 'GB'        TO W-IDSKYLT                                   
084800       END-IF                                                             
084900       PERFORM IMS-GU-WDD3-BSEQ-BENA11                                    
085000       IF SEGMENT-FINNS                                                   
085100          MOVE TEXT-BEART  TO MOD-BEART-RAD(INDX)                         
085200       ELSE                                                               
085300          MOVE '********'  TO MOD-BEART-RAD(INDX)                         
085400       END-IF                                                             
085500       MOVE 3162-KVAVIS    TO MOD-KVLEVART-RAD(INDX)                      
085600                                                                          
085700       MOVE WS-KVAVBART-KDFEL-TAB(INDX) TO                                
085800                                 MOD-KDFEL-KVAVBART-RAD(INDX)             
085900       IF WS-KVAVBART-KDFEL-TAB(INDX) = NO-ERROR                          
086000         MOVE 3162-KVAVBART  TO MOD-KVAVBART-RAD(INDX)                    
086100       ELSE                                                               
086200         MOVE ALL-PLUS       TO MOD-KVAVBART-RAD(INDX)                    
086300       END-IF                                                             
086400                                                                          
086500       MOVE 3162-KVAVBART  TO WS-KVACCEPT                                 
086600       MOVE WS-KVACCEPT    TO MOD-KVACCEPT-RAD(INDX)                      
086700                                                                          
086800       ADD 1 TO WS-KVRAD-TOT                                              
086900       PERFORM IMS-GNP-WDGX3162-MIN-MAX                                   
087000     END-PERFORM                                                          
087100     MOVE INDX TO WS-KVDIARAD-AKT-UT                                      
087200                                                                          
087300     IF SEGMENT-FINNS                                                     
087400       PERFORM UNTIL SEGMENT-SAKNAS                                       
087500         ADD 1 TO WS-KVRAD-TOT                                            
087600         PERFORM IMS-GNP-WDGX3162-MIN-MAX                                 
087700       END-PERFORM                                                        
087800     END-IF                                                               
087900     MOVE WS-KVRAD-TOT     TO MOD-KVRAD-TOT                               
088000                                                                          
088100*** NYA RADEN ***                                                         
088200     IF WS-FLNYRADFEL = JA                                                
088300       MOVE WS-IDARTNR-NY-INMAT TO MOD-IDARTNR-NY                         
088400       MOVE WS-KVAVBART-NY      TO MOD-KVAVBART-NY                        
088500     ELSE                                                                 
088600       MOVE NO-ERROR        TO MOD-KDFEL-AVBART-NY                        
088700                               MOD-KDFEL-IDARTNR-NY                       
088800       MOVE ZEROES          TO MOD-IDARTNR-NY                             
088900                               MOD-KVAVBART-NY                            
089000     END-IF                                                               
089100     .                                                                    
089200     EJECT                                                                
089300 G-KOLLA-INPUT SECTION.                                                   
089400     MOVE 'G-KOLLA '   TO WS-IMS-SECTION                                  
089500     SKIP2                                                                
089600     MOVE JA  TO INDATA-SW                                                
089700     MOVE NEJ TO INDATA-FEL-SW                                            
089800     MOVE NEJ TO WS-FLNYRADFEL                                            
089900     MOVE ZERO TO WS-IDARTNR-NY                                           
090000     MOVE ZERO TO WS-IDARTNR-NY-INMAT                                     
090100     MOVE ZERO TO WS-KVAVBART-NY                                          
090200                                                                          
090300     MOVE 1 TO INDX                                                       
090400     PERFORM UNTIL INDX > WS-KVDIARAD-MAX                                 
090500        MOVE NEJ       TO WS-FLRADFEL-TAB(INDX)                           
090600        MOVE NO-ERROR  TO WS-KVAVBART-KDFEL-TAB(INDX)                     
090700        MOVE ZERO      TO WS-KVAVBART-TAB(INDX)                           
090800        ADD 1 TO INDX                                                     
090900     END-PERFORM                                                          
091000                                                                          
091100**** CHECK NYRADEN ****                                                   
091200     MOVE ZERO                  TO MOD-KDFEL-IDARTNR-NY                   
091300                                   MOD-KDFEL-AVBART-NY                    
091400                                                                          
091500     IF MID-IDARTNR-NY NOT = ALL '+'                                      
091600       IF MID-IDARTNR-NY NOT NUMERIC                                      
091700         MOVE JA                TO INDATA-FEL-SW                          
091800         MOVE JA                TO WS-FLNYRADFEL                          
091900         MOVE ZERO              TO WS-IDARTNR-NY                          
092000                                   WS-IDARTNR-NY-INMAT                    
092100         MOVE NOT-NUMERIC       TO MOD-KDFEL-IDARTNR-NY                   
092200         MOVE '024 PART NO MUST BE NUMERIC'                               
092300                                  TO MOD-TEWEBERR                         
092400       ELSE                                                               
092500         IF MID-IDARTNR-NY > ZERO                                         
092600           MOVE JA TO NY-ARTIKEL-SW                                       
092700           MOVE MID-IDARTNR-NY  TO WS-IDARTNR-NY                          
092800                                   WS-IDARTNR-NY-INMAT                    
092900           PERFORM S13-CHECK-BYTES                                        
093000         END-IF                                                           
093100       END-IF                                                             
093200     END-IF                                                               
093300     IF NY-ARTIKEL                                                        
093400       IF MID-KVAVBART-NY NOT = ALL '+'                                   
093500         IF MID-KVAVBART-NY NUMERIC                                       
093600           MOVE MID-KVAVBART-NY TO WS-KVAVBART-NY                         
093700           MOVE MID-KVAVBART-NY TO DEC-IDFRIDATA                          
093800           MOVE 7               TO DEC-KVHELTAL                           
093900           MOVE 0               TO DEC-KVDECIMAL                          
094000           CALL WDECEDIT USING DEC-WDECAREA                               
094100                                                                          
094200           IF DEC-KDSVAR-OK                                               
094300             MOVE DEC-IDEDITDATA     TO WS-KVAVBART-NY                    
094400             IF MID-KVAVBART-NY > 0                                       
094500               CONTINUE                                                   
094600             ELSE                                                         
094700               MOVE JA                  TO INDATA-FEL-SW                  
094800               MOVE JA                  TO WS-FLNYRADFEL                  
094900               MOVE UPDATE-NOT-ALLOWED  TO MOD-KDFEL-AVBART-NY            
095000               MOVE '023 RECEIVED QTY IS INVALID'                         
095100                                        TO MOD-TEWEBERR                   
095200             END-IF                                                       
095300                                                                          
095400           ELSE                                                           
095500             MOVE JA                 TO INDATA-FEL-SW                     
095600             MOVE JA                 TO WS-FLNYRADFEL                     
095700             MOVE UPDATE-NOT-ALLOWED TO MOD-KDFEL-AVBART-NY               
095800             MOVE '023 RECEIVED QTY IS INVALID'                           
095900                                     TO MOD-TEWEBERR                      
096000           END-IF                                                         
096100         ELSE                                                             
096200           MOVE JA                  TO INDATA-FEL-SW                      
096300           MOVE JA                  TO WS-FLNYRADFEL                      
096400           MOVE NOT-NUMERIC         TO MOD-KDFEL-AVBART-NY                
096500           MOVE '024 RECEIVED QTY MUST BE NUMERIC'                        
096600                                    TO MOD-TEWEBERR                       
096700         END-IF                                                           
096800       END-IF                                                             
096900     END-IF                                                               
097000                                                                          
097100*KOLLA RADER                                                              
097200     IF MID-INPUT NOT = ALL '+'                                           
097300       MOVE WS-IDDISTR-NUM  TO W-3161-IDDISTR                             
097400       PERFORM IMS-GU-WDGX3161                                            
097500       MOVE 1 TO INDX                                                     
097600       PERFORM UNTIL INDX > WS-KVDIARAD-MAX OR INDATA-NOT-OK              
097700        OR MID-IDARTNR-RAD(INDX) NOT NUMERIC                              
097800         IF (MID-KVAVBART-RAD(INDX) NOT = ALL '+')                        
097900           IF NY-ARTIKEL AND                                              
098000              WS-KVDIARAD-AKT-IN > 0                                      
098100             MOVE 'UPDATE NOT ALLOWED'  TO MOD-TEWEBERR                   
098200             MOVE NEJ TO INDATA-SW                                        
098300             MOVE JA  TO WS-FLNYRADFEL                                    
098400           ELSE                                                           
098500             PERFORM GA-CHECK-LINE-INPUT                                  
098600           END-IF                                                         
098700         END-IF                                                           
098800         ADD +1 TO INDX                                                   
098900       END-PERFORM                                                        
099000       MOVE INDX TO WS-KVDIARAD-AKT-IN                                    
099100     ELSE                                                                 
099200       MOVE NEJ TO INDATA-SW                                              
099300       MOVE '004 UPDATE PRESSED, BUT NO DATA ENTERED'                     
099400                                   TO MOD-TEWEBERR                        
099500       SET SEARCH-ORDER-LINES TO TRUE                                     
099600       MOVE ZERO TO WS-KVDIARAD-AKT-IN                                    
099700     END-IF                                                               
099800     IF FEL-FINNS                                                         
099900       MOVE NEJ TO INDATA-SW                                              
100000     END-IF                                                               
100100     .                                                                    
100200     EJECT                                                                
100300 GA-CHECK-LINE-INPUT SECTION.                                             
100400     MOVE 'GA-CHECK-LINE '   TO WS-IMS-SECTION                            
100500*    -- KOLLA MOTTAGET                                                    
100600     IF MID-KVAVBART-RAD(INDX) NOT = ALL '+'                              
100700       IF MID-KVAVBART-RAD(INDX) NOT NUMERIC                              
100800         MOVE JA             TO INDATA-FEL-SW                             
100900         MOVE JA             TO WS-FLRADFEL-TAB(INDX)                     
101000         MOVE NOT-NUMERIC    TO WS-KVAVBART-KDFEL-TAB(INDX)               
101100         MOVE '024 RECEIVED QTY MUST BE NUMERIC'                          
101200                             TO MOD-TEWEBERR                              
101300         MOVE MID-KVAVBART-RAD(INDX) TO                                   
101400                                WS-KVAVBART-TAB-ALFA(INDX)                
101500       ELSE                                                               
101600         MOVE MID-KVAVBART-RAD(INDX) TO WS-KVAVBART-TAB(INDX)             
101700       END-IF                                                             
101800     ELSE                                                                 
101900       MOVE ZERO             TO MOD-KVAVBART-RAD(INDX)                    
102000     END-IF                                                               
102100*                                                                         
102200                                                                          
102300*    FINNS RADEN PÅ BASEN                                                 
102400     IF WS-FLRADFEL-TAB(INDX) = NEJ                                       
102500       MOVE WS-DAORDREG-NUM           TO W-3162-DAORDREG                  
102600       MOVE WS-IDORDER-NUM            TO W-3162-IDORDER                   
102700       MOVE MID-IDARTNR-RAD(INDX)     TO W-3162-IDARTNR                   
102800       MOVE MID-IDRADNR-RAD(INDX)     TO W-3162-IDRADNR                   
102900       PERFORM IMS-GNP-WDGX3162                                           
103000       IF SEGMENT-FINNS                                                   
103100         CONTINUE                                                         
103200       ELSE                                                               
103300         MOVE JA             TO INDATA-FEL-SW                             
103400         MOVE JA             TO WS-FLRADFEL-TAB(INDX)                     
103500         MOVE LINE-NOT-IN-DATABASE                                        
103600                             TO MOD-KDFEL-KVAVBART-RAD(INDX)              
103700         MOVE '027 LINE(S) NOT FOUND'                                     
103800                             TO MOD-TEWEBERR                              
103900       END-IF                                                             
104000     END-IF                                                               
104100     .                                                                    
104200     EJECT                                                                
104300 H-UPPDATERA SECTION.                                                     
104400     MOVE 'H-UPPDATERA '   TO WS-IMS-SECTION                              
104500     IF NY-ARTIKEL                                                        
104600       PERFORM HB-UPDATE-NEW-ART-LINE                                     
104700     ELSE                                                                 
104800       IF MID-TABELLINPUT NOT = ALL '+'                                   
104900         MOVE 1 TO INDX                                                   
105000         MOVE NEJ TO WS-FLUPD                                             
105100         PERFORM UNTIL INDX > WS-KVDIARAD-MAX OR                          
105200            MID-IDARTNR-RAD(INDX) NOT NUMERIC                             
105300           IF (MID-KVAVBART-RAD(INDX) NOT = ALL '+')                      
105400              PERFORM HA-UPDATE-LINE-DATA                                 
105500           END-IF                                                         
105600           ADD +1 TO INDX                                                 
105700         END-PERFORM                                                      
105800       END-IF                                                             
105900     END-IF                                                               
106000                                                                          
106100     IF WS-FLUPD = JA                                                     
106200       MOVE '001 OK, UPDATE DONE'                 TO MOD-TEWEBINF         
106300     ELSE                                                                 
106400       MOVE '002 NOTHING HAS BEEN UPDATED'        TO MOD-TEWEBINF         
106500     END-IF                                                               
106600     .                                                                    
106700     EJECT                                                                
106800 HA-UPDATE-LINE-DATA SECTION.                                             
106900     MOVE 'HA-UPDATE-LINE'   TO WS-IMS-SECTION                            
107000     MOVE WS-IDDISTR-NUM TO W-3161-IDDISTR                                
107100                                                                          
107200     MOVE WS-DAORDREG-NUM           TO W-3162-DAORDREG                    
107300     MOVE WS-IDORDER-NUM            TO W-3162-IDORDER                     
107400     MOVE MID-IDARTNR-RAD(INDX)     TO W-3162-IDARTNR                     
107500     MOVE MID-IDRADNR-RAD(INDX)     TO W-3162-IDRADNR                     
107600     PERFORM IMS-GHU-WDGX3162                                             
107700     IF SEGMENT-FINNS                                                     
107800       IF (WS-KVAVBART-TAB(INDX) = 3162-KVAVBART) AND                     
107900          (MID-KVAVBART-RAD(INDX) NOT = ZERO) AND                         
108000           3162-DAREGDAT NOT = ZERO                                       
108100         CONTINUE                                                         
108200       ELSE                                                               
108300         MOVE MID-IDARTNR-RAD(INDX) TO W-A9-IDARTNR                       
108400         MOVE MID-IDDISTR           TO W-A9-IDDISTR                       
108500         MOVE DAGENS-DAAAAAPP       TO W-A9-DAAAPP                        
108600         PERFORM IMS-GHU-WDA921                                           
108700         IF SEGMENT-SAKNAS                                                
108800            PERFORM S10-SKAPA-WDA9                                        
108900         END-IF                                                           
109000         MOVE FUNCTION CURRENT-DATE(9:6) TO 3162-TIREGTID                 
109100                                            UPP-TIREGTID                  
109200         IF 3162-DAREGDAT = 0                                             
109300           MOVE DAGENS-DATUM               TO 3162-DAREGDAT               
109400         END-IF                                                           
109500         MOVE MID-IDUSER                 TO 3162-IDUSER                   
109600                                            UPP-IDUSER                    
109700         MOVE ZERO TO WS-DIFF-MOT                                         
109800         IF WS-KVAVBART-TAB(INDX) NOT = 3162-KVAVBART AND                 
109900            MID-KVAVBART-RAD(INDX) NOT = ALL '+'                          
110000           COMPUTE WS-DIFF-MOT =                                          
110100                   WS-KVAVBART-TAB(INDX) - 3162-KVAVBART                  
110200           COMPUTE UPP-SUMOTT-REM =                                       
110300                   UPP-SUMOTT-REM + WS-DIFF-MOT                           
110400           MOVE WS-KVAVBART-TAB(INDX) TO 3162-KVAVBART                    
110500         END-IF                                                           
110600         PERFORM IMS-REPL-WDGX3162                                        
110700         PERFORM IMS-REPL-WDA921                                          
110800** UPPDATERA ÄVEN KVLS PÅ WDA911                                          
110900         PERFORM IMS-GHU-WDA911                                           
111000         COMPUTE UPD-KVLS-REM = UPD-KVLS-REM + WS-DIFF-MOT                
               IF UPD-KVLS-REM < ZERO                                           
                  MOVE ZERO TO UPD-KVLS-REM                                     
               END-IF                                                           
111100         PERFORM IMS-REPL-WDA911                                          
111200         MOVE JA TO WS-FLUPD                                              
111300       END-IF                                                             
111400     END-IF                                                               
111500     .                                                                    
111600     EJECT                                                                
111700 HB-UPDATE-NEW-ART-LINE SECTION.                                          
111800     MOVE 'HB-UPDATE-NEW'   TO WS-IMS-SECTION                             
111900*** COUNT NUMBER OF LINES FOR THE NEXT IDRADNR                            
112000     MOVE WS-IDDISTR-NUM            TO W-3161-IDDISTR                     
112100     PERFORM  IMS-GU-WDGX3161                                             
112200     IF MID-DAORDREG NOT = ALL '+' AND                                    
112300        MID-IDORDER NOT = ALL '+'                                         
112400       MOVE WS-DAORDREG-NUM           TO W-3162-DAORDREG-MIN              
112500                                         W-3162-DAORDREG-MAX              
112600       MOVE WS-IDORDER-NUM            TO W-3162-IDORDER-MIN               
112700                                         W-3162-IDORDER-MAX               
112800       PERFORM IMS-GNP-WDGX3162-MIN-MAX                                   
112900       MOVE 1 TO ANTAL-RADER                                              
113000       PERFORM UNTIL SEGMENT-SAKNAS                                       
113100         PERFORM IMS-GNP-WDGX3162-MIN-MAX                                 
113200         ADD 1 TO ANTAL-RADER                                             
113300       END-PERFORM                                                        
113400                                                                          
113500       MOVE WS-IDARTNR-NY         TO W-A9-IDARTNR                         
113600       MOVE MID-IDDISTR         TO W-A9-IDDISTR                           
113700       MOVE DAGENS-DAAAAAPP       TO W-A9-DAAAPP                          
113800       PERFORM IMS-GHU-WDA921                                             
113900       IF SEGMENT-SAKNAS                                                  
114000         PERFORM S10-SKAPA-WDA9                                           
114100       END-IF                                                             
114200       MOVE FUNCTION CURRENT-DATE(9:6) TO UPP-TIREGTID                    
114300       MOVE MID-IDUSER                 TO UPP-IDUSER                      
114400       COMPUTE UPP-SUMOTT-REM = UPP-SUMOTT-REM + WS-KVAVBART-NY           
114500                                                                          
114600       PERFORM IMS-REPL-WDA921                                            
114700                                                                          
114800** UPPDATERA ÄVEN KVLS PÅ WDA911                                          
114900       PERFORM IMS-GHU-WDA911                                             
115000       COMPUTE UPD-KVLS-REM = UPD-KVLS-REM + WS-KVAVBART-NY               
             IF UPD-KVLS-REM < ZERO                                             
                MOVE ZERO TO UPD-KVLS-REM                                       
             END-IF                                                             
115100       PERFORM IMS-REPL-WDA911                                            
115200                                                                          
115300       MOVE WS-DAORDREG-NUM           TO 3162-DAORDREG                    
115400       MOVE WS-IDORDER-NUM            TO 3162-IDORDER                     
115500       MOVE WS-IDARTNR-NY             TO 3162-IDARTNR                     
115600       MOVE ANTAL-RADER               TO 3162-IDRADNR                     
115700                                         MOD-KVRAD-TOT                    
115800       MOVE WS-KVAVBART-NY            TO 3162-KVAVBART                    
115900       MOVE ZERO                      TO 3162-KVAVIS                      
116000       MOVE MID-IDUSER                TO 3162-IDUSER                      
116100       MOVE DAGENS-DATUM              TO 3162-DAREGDAT                    
116200       ACCEPT TRANS-TID FROM TIME                                         
116300       MOVE TRANS-TID(1:7)            TO 3162-TIREGTID                    
116400                                                                          
116500       PERFORM IMS-ISRT-WDGX3162                                          
116600       MOVE JA TO WS-FLUPD                                                
116700     ELSE                                                                 
116800       MOVE '115 ORDERNR/ORDERDATE MUST BE ENTERED'                       
116900                             TO MOD-TEWEBERR                              
117000       MOVE NEJ TO WS-FLUPD                                               
117100     END-IF                                                               
117200     .                                                                    
117300     EJECT                                                                
117400 S10-SKAPA-WDA9 SECTION.                                                  
117500     MOVE 'S10-SKAPA-WDA9 '   TO WS-IMS-SECTION                           
117600     PERFORM IMS-GU-WDA901                                                
117700     IF SEGMENT-FINNS                                                     
117800        CONTINUE                                                          
117900     ELSE                                                                 
118000       IF NY-ARTIKEL                                                      
118100         MOVE WS-IDARTNR-NY         TO UPB-IDARTNR                        
118200                                       W-A9-IDARTNR                       
118300         PERFORM IMS-ISRT-WDA901                                          
118400       ELSE                                                               
118500         MOVE MID-IDARTNR-RAD(INDX) TO UPB-IDARTNR                        
118600                                       W-A9-IDARTNR                       
118700         PERFORM IMS-ISRT-WDA901                                          
118800       END-IF                                                             
118900     END-IF                                                               
119000                                                                          
119100     PERFORM IMS-GU-WDA911                                                
119200     IF SEGMENT-FINNS                                                     
119300        CONTINUE                                                          
119400     ELSE                                                                 
119500        PERFORM S11-SKAPA-WDA911                                          
119600        PERFORM IMS-ISRT-WDA911                                           
119700     END-IF                                                               
119800                                                                          
119900     PERFORM IMS-GU-WDA921                                                
120000     IF SEGMENT-FINNS                                                     
120100        CONTINUE                                                          
120200     ELSE                                                                 
120300        PERFORM S12-SKAPA-WDA921                                          
120400        PERFORM IMS-ISRT-WDA921                                           
120500        PERFORM IMS-GHU-WDA921                                            
120600     END-IF                                                               
120700     .                                                                    
120800     EJECT                                                                
120900 S11-SKAPA-WDA911 SECTION.                                                
121000     MOVE 'S11-SKAPA-WDA9 '   TO WS-IMS-LAES                              
121100     INITIALIZE UPD-WDA911                                                
121200     MOVE MID-IDDISTR      TO UPD-IDDISTR                                 
121300                              W-A9-IDDISTR                                
121400     MOVE DAGENS-DATUM     TO UPD-DAREGDAT                                
121500     MOVE MID-IDUSER       TO UPD-IDUSER                                  
121600     MOVE FUNCTION CURRENT-DATE(9:6) TO UPD-TIREGTID                      
121700     .                                                                    
121800     EJECT                                                                
121900 S12-SKAPA-WDA921 SECTION.                                                
122000     MOVE 'S12-SKAPA-WDA921'   TO WS-IMS-LAES                             
122100     INITIALIZE UPP-WDA921                                                
122200     MOVE DAGENS-DAAAAAPP  TO UPP-DAAAPP                                  
122300                              W-A9-DAAAPP                                 
122400     MOVE DAGENS-DATUM     TO UPP-DAREGDAT                                
122500     MOVE MID-IDUSER       TO UPP-IDUSER                                  
122600     MOVE FUNCTION CURRENT-DATE(9:6) TO UPP-TIREGTID                      
122700     .                                                                    
122800     EJECT                                                                
122900 S13-CHECK-BYTES  SECTION.                                                
123000     MOVE 'S13-CHECK-BYTES'  TO WS-IMS-LAES                               
123100     MOVE WS-IDARTNR-NY            TO TEST-IDARTNR                        
123200                                                                          
123300     IF BYT03-OBJEKT                                                      
123400       MOVE WS-IDARTNR-NY           TO W-IDARTNR                          
123500       PERFORM IMS-GET-WDK601                                             
123600       IF SEGMENT-FINNS                                                   
123700         MOVE ART-IDFKNGRP         TO UPB-IDFKNGRP                        
123800       ELSE                                                               
123900         MOVE JA                   TO INDATA-FEL-SW                       
124000         MOVE JA                   TO WS-FLNYRADFEL                       
124100         MOVE MISSING-IN-REGISTER     TO MOD-KDFEL-IDARTNR-NY             
124200         MOVE '025 PART NOT FOUND'                                        
124300                                      TO MOD-TEWEBERR                     
124400       END-IF                                                             
124500     ELSE                                                                 
124600       MOVE JA                        TO INDATA-FEL-SW                    
124700       MOVE JA                        TO WS-FLNYRADFEL                    
124800       MOVE NOT-BYTES                 TO MOD-KDFEL-IDARTNR-NY             
124900       MOVE '023 PART NO IS INVALID'                                      
125000                                TO MOD-TEWEBERR                           
125100     END-IF                                                               
125200     .                                                                    
125300     EJECT                                                                
125400 S20-HAEMTA-ANROPSDATA SECTION.                                           
125500                                                                          
125600     MOVE 'GETARG'            TO SUB-KDFUNC                               
125700     MOVE WS-ADRESS           TO SUB-ADDISPABS                            
125800     MOVE LENGTH OF MID-AREA  TO SUB-KVDLEN                               
125900                                                                          
126000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN MID-AREA              
126100                                                                          
126200     IF SUB-KDRC > 0                                                      
126300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
126400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
126500       DELIMITED BY SIZE INTO FELTEXT                                     
126600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
126700     END-IF                                                               
126800     .                                                                    
126900     EJECT                                                                
127000 S21-RETURNERA-SVAR SECTION.                                              
127100                                                                          
127200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
127300                                                                          
127400     COMPUTE SUB-KVDLEN   = LENGTH OF MOD-AREA                            
127500       - LENGTH OF MOD-TABELLRAD * (500 - MOD-KVDIARAD-AKTUELLT)          
127600                                                                          
127700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN MOD-AREA              
127800                                                                          
127900     IF SUB-KDRC > 0                                                      
128000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
128100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
128200       DELIMITED BY SIZE INTO FELTEXT                                     
128300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
128400     END-IF                                                               
128600     .                                                                    
328200     EJECT                                                                
328300                                                                          
328400 S22-SEND-OPEN SECTION.                                                   
328500     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
328600     MOVE 'OPEN'                  TO SEND-KDFUNC                          
328700     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
328800                                     SEND-OPEN-AREA                       
328900     IF SEND-KDRC > ZERO                                                  
329000       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
329100       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
329200       DELIMITED BY SIZE INTO FELTEXT                                     
329300       DISPLAY FELTEXT                                                    
329400       CALL FELLOG                                                        
329500     END-IF                                                               
329600     .                                                                    
329700     EJECT                                                                
329800 S23-PUT-HEADER SECTION.                                                  
249200     MOVE FUNCTION CURRENT-DATE(1:12)                                     
249300                TO WS-CURRENT-DATE-TIME                                   
329900     MOVE 1                       TO HDR-REQU-IDMSGVER                    
330000     MOVE SPACE                   TO HDR-REQU-KDPGMACT                    
330100     MOVE IDPGM                   TO HDR-REQU-IDUSER                      
330200     MOVE 'W30178'                TO HDR-IDOUTTYPE                        
330300     MOVE WS-IDDISTR-NUM          TO HDR-IDOUTREC                         
330400     MOVE WS-TIYYMMDDHHMM         TO HDR-IDLIST                           
330500     MOVE 'PUT'                   TO SEND-KDFUNC                          
330600     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
330700     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
330800                                     SEND-KVDLEN                          
330900                                     HDR-AREA                             
331000     IF SEND-KDRC > ZERO                                                  
331100       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
331200       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
331300       DELIMITED BY SIZE       INTO FELTEXT                               
331400       DISPLAY FELTEXT                                                    
331500       CALL FELLOG                                                        
331600     END-IF                                                               
331700     .                                                                    
331800     EJECT                                                                
331900 S24-PUT-LINE SECTION.                                                    
           IF FORSTA-TRAFF                                                      
332000        MOVE 'PUT'                   TO SEND-KDFUNC                       
332100        MOVE LENGTH OF SEND-AREA-TO-CSV-RUB TO SEND-KVDLEN                
332200        CALL WZ01SEND             USING SEND-CONTROL-AREA                 
332300                                        SEND-KVDLEN                       
332400                                        SEND-AREA-TO-CSV-RUB              
           ELSE                                                                 
332000        MOVE 'PUT'                   TO SEND-KDFUNC                       
332100        MOVE LENGTH OF SEND-AREA-TO-CSV-FIL TO SEND-KVDLEN                
332200        CALL WZ01SEND             USING SEND-CONTROL-AREA                 
332300                                        SEND-KVDLEN                       
332400                                        SEND-AREA-TO-CSV-FIL              
           END-IF                                                               
332500     IF SEND-KDRC > ZERO                                                  
332600       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
332700       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
332800       DELIMITED BY SIZE       INTO FELTEXT                               
332900       DISPLAY FELTEXT                                                    
333000       CALL FELLOG                                                        
333100     END-IF                                                               
333200     .                                                                    
333300     SKIP2                                                                
333400 S25-SEND-CLOSE SECTION.                                                  
333500     MOVE 'CLOSE'                TO SEND-KDFUNC                           
333600     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
333700     IF SEND-KDRC > 0                                                     
333800       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
333900       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
334000       DELIMITED BY SIZE       INTO FELTEXT                               
334100       DISPLAY FELTEXT                                                    
334200       CALL FELLOG                                                        
334300     END-IF                                                               
334400     .                                                                    
128500     EJECT                                                                
128700 S99-ABEND SECTION.                                                       
128800                                                                          
128900     SKIP2                                                                
129000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
129100     .                                                                    
129200     EJECT                                                                
129300                                                                          
129400                                                                          
129500* --- IMS SEKTIONER ---                                                   
129600*    SKIP3                                                                
129700*IMS-GET-MSG SECTION.                                                     
129800*                                                                         
129900*    MOVE '  QC' TO GODK-STATUSKODER                                      
130000*    CALL CBLTDLI USING GU MSG-PCB MID-AREA                               
130100*    MOVE MSG-STATUS-CODE TO STATUS-WS                                    
130200*    PERFORM IMS-STATUSKONTROLL                                           
130300*    .                                                                    
130400*    SKIP3                                                                
130500*IMS-INSERT-MSG SECTION.                                                  
130600*                                                                         
130700*    MOVE LOW-VALUE TO MOD-KDZ1 MOD-KDZ2                                  
130800*    MOVE SPACE TO GODK-STATUSKODER                                       
130900*    CALL CBLTDLI USING ISRT MSG-PCB MOD-AREA MFS-IDMOD                   
131000*    MOVE MSG-STATUS-CODE TO STATUS-WS                                    
131100*    PERFORM IMS-STATUSKONTROLL                                           
131200*    .                                                                    
131300     EJECT                                                                
131400 IMS-GET-WDK601 SECTION.                                                  
131500                                                                          
131600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
131700          DELIMITED BY SIZE INTO SSA1                                     
131800     MOVE '  GE' TO GODK-STATUSKODER                                      
131900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
132000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
132100     PERFORM IMS-STATUSKONTROLL                                           
132200     .                                                                    
132300     EJECT                                                                
132400 IMS-GU-WDD3-BSEQ-BENA11   SECTION.                                       
132500                                                                          
132600     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
132700            DELIMITED BY SIZE INTO SSA1                                   
132800     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
132900            DELIMITED BY SIZE INTO SSA2                                   
133000     MOVE '  GE' TO GODK-STATUSKODER                                      
133100     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
133200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
133300     PERFORM IMS-STATUSKONTROLL                                           
133400     .                                                                    
133500     EJECT                                                                
133600 IMS-GU-WDGX3161 SECTION.                                                 
133700                                                                          
133800     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
133900          DELIMITED BY SIZE INTO SSA1                                     
134000     MOVE '  GE' TO GODK-STATUSKODER                                      
134100     CALL CBLTDLI USING GU 3161-PCB DLI-IO-WDGX3161 SSA1                  
134200     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
134300     PERFORM IMS-STATUSKONTROLL                                           
134400     .                                                                    
134500     EJECT                                                                
134600 IMS-GNP-WDGX3162-MIN-MAX SECTION.                                        
134700                                                                          
134800     STRING 'WDGX3162(KY3162  >=' W-KY3162-MIN-X                          
134900                    '&KY3162  <=' W-KY3162-MAX-X ')'                      
135000          DELIMITED BY SIZE INTO SSA1                                     
135100     MOVE '  GE' TO GODK-STATUSKODER                                      
135200     CALL CBLTDLI USING GNP 3161-PCB DLI-IO-WDGX3162 SSA1                 
135300     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
135400     PERFORM IMS-STATUSKONTROLL                                           
135500     .                                                                    
135600     EJECT                                                                
135700 IMS-GNP-WDGX3162         SECTION.                                        
135800                                                                          
135900     STRING 'WDGX3162(KY3162  >=' W-KY3162-X     ')'                      
136000          DELIMITED BY SIZE INTO SSA1                                     
136100     MOVE '  GE' TO GODK-STATUSKODER                                      
136200     CALL CBLTDLI USING GNP 3161-PCB DLI-IO-WDGX3162 SSA1                 
136300     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
136400     PERFORM IMS-STATUSKONTROLL                                           
136500     .                                                                    
136600     EJECT                                                                
136700 IMS-GHU-WDGX3162 SECTION.                                                
136800                                                                          
136900     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
137000          DELIMITED BY SIZE INTO SSA1                                     
137100     STRING 'WDGX3162(KY3162  >=' W-KY3162-X     ')'                      
137200          DELIMITED BY SIZE INTO SSA2                                     
137300     MOVE '  GE' TO GODK-STATUSKODER                                      
137400     CALL CBLTDLI USING GHU 3161-PCB DLI-IO-WDGX3162 SSA1 SSA2            
137500     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
137600     PERFORM IMS-STATUSKONTROLL                                           
137700     .                                                                    
137800     EJECT                                                                
137900 IMS-ISRT-WDGX3162 SECTION.                                               
138000                                                                          
138100     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
138200          DELIMITED BY SIZE INTO SSA1                                     
138300     MOVE 'WDGX3162 ' TO SSA2                                             
138400     MOVE '  ' TO GODK-STATUSKODER                                        
138500     CALL CBLTDLI USING ISRT 3161-PCB DLI-IO-WDGX3162 SSA1 SSA2           
138600     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
138700     PERFORM IMS-STATUSKONTROLL                                           
138800     .                                                                    
138900     EJECT                                                                
139000 IMS-REPL-WDGX3162 SECTION.                                               
139100                                                                          
139200     MOVE '  ' TO GODK-STATUSKODER                                        
139300     CALL CBLTDLI USING REPL 3161-PCB DLI-IO-WDGX3162                     
139400     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
139500     PERFORM IMS-STATUSKONTROLL                                           
139600     .                                                                    
139700     EJECT                                                                
139800 IMS-GHU-WDA911 SECTION.                                                  
139900     MOVE 'GHU-WDA911 '   TO WS-IMS-LAES                                  
140000     STRING 'WDA901  (IDARTNR  =' W-A9-IDARTNR-X ')'                      
140100          DELIMITED BY SIZE INTO SSA1                                     
140200     STRING 'WDA911  (IDDISTR  =' W-A9-IDDISTR-X ')'                      
140300          DELIMITED BY SIZE INTO SSA2                                     
140400     MOVE '  GE' TO GODK-STATUSKODER                                      
140500     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA911 SSA1 SSA2              
140600     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
140700     PERFORM IMS-STATUSKONTROLL                                           
140800     .                                                                    
140900     EJECT                                                                
141000 IMS-GHU-WDA921 SECTION.                                                  
141100     MOVE 'GHU-WDA921'   TO WS-IMS-LAES                                   
141200     STRING 'WDA901  (IDARTNR  =' W-A9-IDARTNR-X ')'                      
141300          DELIMITED BY SIZE INTO SSA1                                     
141400     STRING 'WDA911  (IDDISTR  =' W-A9-IDDISTR-X ')'                      
141500          DELIMITED BY SIZE INTO SSA2                                     
141600     STRING 'WDA921  (DAAAPP   =' W-A9-DAAAPP-X ')'                       
141700          DELIMITED BY SIZE INTO SSA3                                     
141800     MOVE '  GE' TO GODK-STATUSKODER                                      
141900     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA921 SSA1 SSA2 SSA3         
142000     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
142100     PERFORM IMS-STATUSKONTROLL                                           
142200     .                                                                    
142300     EJECT                                                                
142400 IMS-GU-WDA901 SECTION.                                                   
142500     MOVE 'GU-WDA901'   TO WS-IMS-LAES                                    
142600     STRING 'WDA901  (IDARTNR  =' W-A9-IDARTNR-X ')'                      
142700          DELIMITED BY SIZE INTO SSA1                                     
142800     MOVE '  GE' TO GODK-STATUSKODER                                      
142900     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA901 SSA1                   
143000     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
143100     PERFORM IMS-STATUSKONTROLL                                           
143200     .                                                                    
143300     EJECT                                                                
143400 IMS-GU-WDA911 SECTION.                                                   
143500     MOVE 'GU-WDA911'   TO WS-IMS-LAES                                    
143600     STRING 'WDA901  (IDARTNR  =' W-A9-IDARTNR-X ')'                      
143700          DELIMITED BY SIZE INTO SSA1                                     
143800     STRING 'WDA911  (IDDISTR  =' W-A9-IDDISTR-X ')'                      
143900          DELIMITED BY SIZE INTO SSA2                                     
144000     MOVE '  GE' TO GODK-STATUSKODER                                      
144100     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA911 SSA1 SSA2               
144200     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
144300     PERFORM IMS-STATUSKONTROLL                                           
144400     .                                                                    
144500     EJECT                                                                
144600 IMS-GU-WDA921 SECTION.                                                   
144700     MOVE 'GU-WDA921'   TO WS-IMS-LAES                                    
144800     STRING 'WDA901  (IDARTNR  =' W-A9-IDARTNR-X ')'                      
144900          DELIMITED BY SIZE INTO SSA1                                     
145000     STRING 'WDA911  (IDDISTR  =' W-A9-IDDISTR-X ')'                      
145100          DELIMITED BY SIZE INTO SSA2                                     
145200     STRING 'WDA921  (DAAAPP   =' W-A9-DAAAPP-X ')'                       
145300          DELIMITED BY SIZE INTO SSA3                                     
145400     MOVE '  GE' TO GODK-STATUSKODER                                      
145500     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA921 SSA1 SSA2 SSA3          
145600     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
145700     PERFORM IMS-STATUSKONTROLL                                           
145800     .                                                                    
145900     EJECT                                                                
146000 IMS-ISRT-WDA901 SECTION.                                                 
146100     MOVE 'ISRT-WDA901 '   TO WS-IMS-LAES                                 
146200     MOVE 'WDA901   ' TO SSA1                                             
146300     MOVE '  II' TO GODK-STATUSKODER                                      
146400     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA901 SSA1                  
146500     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
146600     PERFORM IMS-STATUSKONTROLL                                           
146700     .                                                                    
146800     EJECT                                                                
146900 IMS-ISRT-WDA911 SECTION.                                                 
147000     MOVE 'ISRT-WDA911 '   TO WS-IMS-LAES                                 
147100     STRING 'WDA901  (IDARTNR  =' W-A9-IDARTNR-X ')'                      
147200          DELIMITED BY SIZE INTO SSA1                                     
147300     MOVE 'WDA911   ' TO SSA2                                             
147400     MOVE '  ' TO GODK-STATUSKODER                                        
147500     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA911 SSA1 SSA2             
147600     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
147700     PERFORM IMS-STATUSKONTROLL                                           
147800     .                                                                    
147900     EJECT                                                                
148000 IMS-ISRT-WDA921 SECTION.                                                 
148100     MOVE 'ISRT-WDA921 '   TO WS-IMS-LAES                                 
148200     STRING 'WDA901  (IDARTNR  =' W-A9-IDARTNR-X ')'                      
148300          DELIMITED BY SIZE INTO SSA1                                     
148400     STRING 'WDA911  (IDDISTR  =' W-A9-IDDISTR-X ')'                      
148500          DELIMITED BY SIZE INTO SSA2                                     
148600     MOVE 'WDA921   ' TO SSA3                                             
148700     MOVE '  ' TO GODK-STATUSKODER                                        
148800     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA921 SSA1 SSA2 SSA3        
148900     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
149000     PERFORM IMS-STATUSKONTROLL                                           
149100     .                                                                    
149200     EJECT                                                                
149300 IMS-REPL-WDA911 SECTION.                                                 
149400     MOVE 'REPL-WDA911 '   TO WS-IMS-LAES                                 
149500     MOVE '  ' TO GODK-STATUSKODER                                        
149600     CALL CBLTDLI USING REPL WDA9-PCB DLI-IO-WDA911                       
149700     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
149800     PERFORM IMS-STATUSKONTROLL                                           
149900     .                                                                    
150000     SKIP3                                                                
150100 IMS-REPL-WDA921 SECTION.                                                 
150200     MOVE 'REPL-WDA921 '   TO WS-IMS-LAES                                 
150300     MOVE '  ' TO GODK-STATUSKODER                                        
150400     CALL CBLTDLI USING REPL WDA9-PCB DLI-IO-WDA921                       
150500     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
150600     PERFORM IMS-STATUSKONTROLL                                           
150700     .                                                                    
150800     EJECT                                                                
150900 IMS-STATUSKONTROLL SECTION.                                              
151000                                                                          
151100     SET STATUS-IX TO 1                                                   
151200     SEARCH GODK-STATUS                                                   
151300       AT END                                                             
151400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
151500         DELIMITED BY SIZE INTO FELTEXT                                   
151600         CALL FELLOG                                                      
151700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
151800         CONTINUE                                                         
151900     END-SEARCH                                                           
152000     .                                                                    
