000100*                                                                         
000201******************************************************************        
000301*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0101      *        
000401******************************************************************        
000501*                                                                         
000601 ID DIVISION.                                                             
000701 PROGRAM-ID.     W6030100.                                                
000801 AUTHOR.         BO LINDAHL, FRONTEC.                                     
000901 DATE-WRITTEN.   94/12/16.                                                
001001 DATE-COMPILED.                                                           
001101                                                                          
001201*    FUNKTION:                                                            
001301*        NDC/SDC LOSSNING                                                 
001401*        BESTÄLLNING AV LISTA SAMTLIGA KOLLIN I FAKTURA                   
001501*        LIST-PROGRAMMET STARTAS VIA SOP.                                 
001601*                                                                         
001701*        PROGRAMMET LÄSER      WLINLD (WDL6)                              
001801*                                      WDB6                               
001901*        PROGRAMMET UPPDATERAR         WDK7                               
002001*                              WLINLC (WDL6)                              
002101*                              WL6301 (WDR5)                              
002201*        PROGRAMMET LOGGAR SALDO FÖRÄNDRINGAR PÅ WLLOGA (WDL9)            
002301*                                                                         
002401*    INDATA.                                                              
002501*        TRANSAKTION: W6T301                                              
002601*        MID:         W6I30101                                            
002701*                                                                         
002801*    UTDATA.                                                              
002901*        MOD:         W6O30101                                            
003001                                                                          
003101     SKIP3                                                                
003201 ENVIRONMENT DIVISION.                                                    
003301     EJECT                                                                
003401 DATA DIVISION.                                                           
003501 WORKING-STORAGE SECTION.                                                 
003601*   -COPY WY2000W1                                                        
003701*   -COPY WWDC99                                                          
003801*   -COPY WWDIST35                                                        
003900     SKIP3                                                                
004000 77  IDPGM                       PIC X(08)   VALUE 'W6030100'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400 77  SWITCH                      PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004610 77  YES                         PIC X       VALUE 'Y'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  SW-TRAEFF                   PIC X       VALUE 'N'.                   
004900 77  SW-TIBERANK                 PIC X       VALUE 'N'.                   
005000 77  SW-ADINLOMR                 PIC X       VALUE 'N'.                   
005100 77  SW-REC-UPPD                 PIC X       VALUE 'N'.                   
005200 77  SW-LOC-UPPD                 PIC X       VALUE 'N'.                   
005300 77  WS-PRIOART                  PIC X       VALUE SPACE.                 
005400 77  WS-KVBEHOV                  PIC S9(7)V9(1) VALUE ZERO.               
005500 77  WS-KVTILLGANG               PIC S9(7)V9(1) VALUE ZERO.               
005600 77  WS-DIFF                     PIC S9(7)V9(1) VALUE ZERO.               
005700 77  WS-SPAR-IDFAKT              PIC S9(7)   COMP-3 VALUE ZERO.           
005800 77  WS-IDLTERM                  PIC X(8)    VALUE SPACE.                 
005901 77  ETA-IDKUNDNR                PIC 9(7)  VALUE ZERO.                    
006000                                                                          
006100*    --- INDEX FÖR BLÄDDRINGSRADER MM                                     
006200 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006300 77  PER-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
006400 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
006500 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006600                                                                          
006700*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
006800 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
006900                                                                          
007000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007100 77  WS-IDDC-SEND                PIC X(2)     VALUE SPACE.                
007200 77  WS-TIFAKT                   PIC X(6).                                
007300 77  TIFAKT-WS REDEFINES WS-TIFAKT PIC 9(6).                              
007400                                                                          
007500 77  WS-IDDC-REC                 PIC X(2)     VALUE SPACE.                
007600 77  WS-SAP-IDDISTR              PIC S9(5)    VALUE ZERO COMP-3.          
007700 77  WS-SAP-IDKUNDNR             PIC S9(7)    VALUE ZERO COMP-3.          
007800 77  WS-SAP-PRARTNTO             PIC S9(7)V99 VALUE ZERO COMP-3.          
007900 77  WS-SAP-PRARTSTD             PIC S9(7)V99 VALUE ZERO COMP-3.          
008001 77  WS-SAP-PRAVCOST             PIC S9(7)V99 VALUE ZERO COMP-3.          
008100 77  WS-SAP-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
008200 77  W-IDSEKVNR-SAP              PIC S9(3) VALUE 0   COMP-3.              
008300 77  NOLL-RAKNARE                PIC S9(5)   VALUE ZERO COMP-3.           
008400 77  WS-SAP-IDFAKT               PIC 9(7)  VALUE ZERO.                    
008500 77  WS-SAP-X-IDFAKT             PIC X(7)  VALUE ZERO.                    
008600                                                                          
008700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008800     88  INDATA-OK                           VALUE 'J'.                   
008900     88  INDATA-FEL                          VALUE 'N'.                   
009000                                                                          
009100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009200     88  NYCKLAR-OK                          VALUE 'J'.                   
009300     88  NYCKLAR-FEL                         VALUE 'N'.                   
009400                                                                          
009500 77  ATERHOPP-SW                 PIC X       VALUE 'N'.                   
009600     88  ATERHOPP                            VALUE 'J'.                   
009700     88  EJ-ATERHOPP                         VALUE 'N'.                   
009800                                                                          
009900 77  HOPP-SW                     PIC X       VALUE 'N'.                   
010000     88  HOPP                                VALUE 'J'.                   
010100     88  EJ-HOPP                             VALUE 'N'.                   
010200                                                                          
010300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010400     88  EGEN-MID                            VALUE '6301'.                
010500     88  GODK-MID                            VALUE '6301'.                
010600     88  HELP-MID                            VALUE '0551'.                
010700                                                                          
010800 77  LAND-SW                     PIC X       VALUE 'J'.                   
010900     88  AKTUELLT-LAND-SVERIGE               VALUE 'J'.                   
011000     88  AKTUELLT-EJ-SVERIGE                 VALUE 'N'.                   
011100                                                                          
011201 77  LAND2-SW                    PIC X       VALUE 'J'.                   
011301     88  AKTUELLT-LAND-USA                   VALUE 'J'.                   
011401     88  AKTUELLT-EJ-USA                     VALUE 'N'.                   
011501                                                                          
011600 77  INLEV-SW                    PIC X       VALUE 'J'.                   
011700     88  INLEV-JA                            VALUE 'J'.                   
011800     88  INLEV-NEJ                           VALUE 'N'.                   
011900                                                                          
012000 01  IDDC-WS                     PIC X(2).                                
012100                                                                          
012200 01  WS-IDDC-KOLL.                                                        
012300     03  FILLER                  PIC X(5) VALUE 'WIDDC'.                  
012400     03  WS-IDDC-TID             PIC X(2) VALUE SPACE.                    
012500     03  FILLER                  PIC X    VALUE SPACE.                    
012600                                                                          
012700 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012800 01  WLOGG-TID                   PIC S9(9)   VALUE ZERO.                  
012900 01  LOGG-DATUM                  PIC S9(8)   VALUE ZERO.                  
013000                                                                          
013100                                                                          
013200 01  AKTUELL-TID.                                                         
013300     03  AKTUELL-TTMM            PIC 9(4).                                
013400     03  FILLER                  PIC 9(4).                                
013500                                                                          
013600 01  WS-LOKALTID.                                                         
013700     03  WS-LOKAL-SEKEL          PIC X(1).                                
013800     03  FILLER                  PIC X(5).                                
013900                                                                          
014000 01  WS-LOKALTID-NUM             PIC 9(6)  VALUE ZERO.                    
014100 01  WS-LOKALCLOCK-NUM           PIC 9(4)  VALUE ZERO.                    
       01  WS-DABERANK-PROP            PIC 9(8)  VALUE ZERO.                    
       01  WS-DABERANK-DISCH           PIC 9(8)  VALUE ZERO.                    
014200                                                                          
014300 01  W.                                                                   
014400     05  W-KVANT-UPD             PIC S9(3).                               
014500     05  W-DATUM-X.                                                       
014600         10  W-DATUM             PIC 9(6).                                
014700                                                                          
014800     05  W-KVAVIS                PIC S9(7)   VALUE ZERO COMP-3.           
014900     05  W-CMD                   PIC X(3).                                
015000     05  W-TEMFSINF              PIC X(40).                               
015100     EJECT                                                                
015200 01  GENERELLA-SUBPROGRAM.                                                
015300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
015400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015700     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
015800     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
015900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016000     EJECT                                                                
016100*01 -COPY W006PRT                                                         
016200     EJECT                                                                
016300*    --- PARAMETRAR TILL SUBPROGRAM W218ETA                               
016400*01 -COPY W218LETA              -PRE LETA-                                
016500     EJECT                                                                
016600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016700*01 -COPY WMSGINIT                                                        
016800     EJECT                                                                
016900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017000*01 -COPY WMEDAREA                                                        
017100     EJECT                                                                
017200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
017300*01 -COPY WDATAREA                                                        
017400     EJECT                                                                
017500 01  MESSAGE-CODES.                                                       
017600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
017700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
017800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
017900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
018000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
018100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
018200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
018300     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
018400     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
018500     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
018600     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
018700     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
018800     03  INGET-PRINTAT           PIC X(3)    VALUE '167'.                 
018900     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
019000     SKIP3                                                                
019100 01  MESSAGE-TEXTS.                                                       
019200     03  INF-TRAILER-RECEIVED-TEXT                                        
019300                                 PIC X(20)   VALUE                        
019400         'TRAILER RECEIVED    '.                                          
019500 01  MESSAGE-TEXT2.                                                       
019600     03  UPDATING-NOT-ALLOWED-WRONG-DC                                    
019700                                 PIC X(31)   VALUE                        
019800         'UPDATING NOT ALLOWED - WRONG DC'.                               
019900 01  MESSAGE-TEXT3.                                                       
020000     03  NOTHING-PRINTED-WRONG-DC                                         
020100                                 PIC X(31)   VALUE                        
020200         'NOTHING PRINTED - WRONG DC'.                                    
020300     03  BINNING-STARTED-USE-6302                                         
020400                                 PIC X(31)   VALUE                        
020500         'BINNING STARTED - USE 6302'.                                    
020600     EJECT                                                                
020700 01  PROG-TO-PROG-SW.                                                     
020800*    03  -COPY WMSGSOP                                                    
020900     EJECT                                                                
021000 01  WS-PARAMETRAR.                                                       
021100     03  WS-URVAL.                                                        
021200         05  URV-IDFAKT          PIC 9(7) VALUE ZERO.                     
021300     03  WS-PRINTER.                                                      
021400         05  URV-IDPRINTER       PIC X(8) VALUE SPACE.                    
021500     SKIP3                                                                
021600 01  WS-BC-PARAMETRAR.                                                    
021700     03  WS-BC.                                                           
021800         05  BC-URV-IDFAKT       PIC 9(7)  VALUE ZERO.                    
021900         05  BC-URV-IDKUNDRF     PIC X(10) VALUE SPACE.                   
022000         05  BC-URV-IDKUNDNR     PIC 9(7)  VALUE ZERO.                    
022100         05  BC-URV-IDKOLLI      PIC 9(5)  VALUE ZERO.                    
022200     EJECT                                                                
022300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
022400*                                                                         
022500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
022600     SKIP3                                                                
022700*01  MID -COPY W6I30101                                                   
022800     EJECT                                                                
022900 01  FILLER                      PIC X(16)   VALUE                        
023000                                 '6302-MID-AREA'.                         
023100     SKIP3                                                                
023200*01  MID -COPY W6I30201          -PRE 6302-                               
023300     EJECT                                                                
023400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023500     SKIP3                                                                
023600*01  -COPY WMSGAREA                                                       
023700     EJECT                                                                
023800     03  MOD REDEFINES MSG-AREA.                                          
023900*      05  -COPY W6O30101                                                 
024000     EJECT                                                                
024100 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
024200 01  P-TO-P-AREA.                                                         
024300     03  P-TO-P-LL               PIC S9(4)   COMP SYNC.                   
024400     03  P-TO-P-Z1               PIC  X(1)   VALUE LOW-VALUE.             
024500     03  P-TO-P-Z2               PIC  X(1)   VALUE LOW-VALUE.             
024600     03  P-TO-P-TRANSKOD         PIC  X(7).                               
024700     03  FILLER                  PIC  X(1).                               
024800     03  P-TO-P-FROM-MID         PIC  X(4).                               
024900     03  P-TO-P-KDMFSFOR         PIC  X(1).                               
025000     03  P-TO-P-DATA             PIC  X(500).                             
025100     EJECT                                                                
025200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025300     SKIP3                                                                
025400*01  -COPY WMFSAREA                                                       
025500     EJECT                                                                
025600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025800     SKIP3                                                                
025900 01  NYCKLAR-TILL-DLI.                                                    
026000     03  W-WDGXKEY-6301.                                                  
026100         05  W-6301-IDHTYP       PIC X(4).                                
026200         05  W-6301-IDDC         PIC X(2).                                
026300         05  W-6301-LOWVALUE     PIC X(24).                               
026400                                                                          
026500     03  W-WDGXKEY-6302.                                                  
026600         05  W-6302-DABERANK     PIC 9(8).                                
026700         05  W-6302-IDFAKT       PIC S9(7)              COMP-3.           
026800                                                                          
026900     03  W-IDARTNR-X.                                                     
027000         05  W-IDARTNR           PIC S9(9)              COMP-3.           
027100                                                                          
027200     03  W-KDSEGKEY-X.                                                    
027300         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
027400                                                                          
027500     03  W-DAINLEV-X.                                                     
027600         05  W-DAINLEV           PIC 9(16).                               
027700                                                                          
027800     03  W-IDDC                  PIC X(2).                                
027900                                                                          
028000     03  W-IDDC-B6-X.                                                     
028100         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
028200                                                                          
028300     03  W-IDDC-B6-TEST-X.                                                
028400         05 W-IDDC-B6-TEST       PIC X(2).                                
028500                                                                          
028600     03  W-IDPTYP                PIC X(3).                                
028700                                                                          
028800     03  W-WDL6A1KY-MIN.                                                  
028900         05  W-SEQA-IDFAKT-MIN    PIC S9(7)             COMP-3.           
029000         05  W-SEQA-IDKUNDRF-MIN  PIC X(10).                              
029100         05  W-SEQA-IDKUNDNR-MIN  PIC S9(7)             COMP-3.           
029200         05  W-SEQA-IDKOLLI-MIN   PIC S9(5)             COMP-3.           
029300         05  W-SEQA-IDARTNR-MIN   PIC S9(9)             COMP-3.           
029400         05  W-SEQA-DAINLEV-MIN   PIC 9(16).                              
029500                                                                          
029600     03  W-WDL6A1KY-MAX.                                                  
029700         05  W-SEQA-IDFAKT-MAX    PIC S9(7)             COMP-3.           
029800         05  W-SEQA-IDKUNDRF-MAX  PIC X(10).                              
029900         05  W-SEQA-IDKUNDNR-MAX  PIC S9(7)             COMP-3.           
030000         05  W-SEQA-IDKOLLI-MAX   PIC S9(5)             COMP-3.           
030100         05  W-SEQA-IDARTNR-MAX   PIC S9(9)             COMP-3.           
030200         05  W-SEQA-DAINLEV-MAX   PIC 9(16).                              
030300                                                                          
030400     03  W-IDFAKT-X.                                                      
030500         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
030600                                                                          
030700     03  W-DABERANK-X.                                                    
030800         05  W-DABERANK          PIC 9(8)   VALUE ZERO.                   
030900                                                                          
031000     03  W-IDLBBET               PIC X(12)   VALUE SPACE.                 
031100                                                                          
031200*--------W6G1                                                             
031300     03  W-W6GXKEY-6005-X.                                                
031400         05  W-IDHTYP-6005         PIC X(4)    VALUE '6005'.              
031500         05  W-IDDC-6005           PIC X(2)    VALUE '11'.                
031600         05  FILLER                PIC X(24)   VALUE LOW-VALUE.           
031700                                                                          
031800     03  W-W6GXKEY-6006-X.                                                
031900         05  W-ADINLOMR-6006       PIC X(4)    VALUE SPACE.               
032000         05  FILLER                PIC X(1)    VALUE LOW-VALUE.           
032100                                                                          
032200     SKIP2                                                                
032300*    --- STATUS-KOD FRÅN IMS                                              
032400 01  STATUS-WS                   PIC XX.                                  
032500     88  SEGMENT-FINNS                       VALUE '  '.                  
032600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032800     SKIP2                                                                
032900 01  GODK-STATUSKODER.                                                    
033000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033100     SKIP3                                                                
033200 01  SSA1                        PIC X(160).                              
033300 01  SSA2                        PIC X(128).                              
033400     EJECT                                                                
033500*    --- IMS FUNKTIONSKODER                                               
033600*01  -COPY W0003                                                          
033700     EJECT                                                                
033800*    ---  DLI INPUT-OUTPUT AREA                                           
033900     SKIP3                                                                
034000 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WL630101'.            
034100 01  DLI-IO-AREA-6301.                                                    
034200*    03  -COPY WDGX6301                                                   
034300     EJECT                                                                
034400 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WL630111'.            
034500 01  DLI-IO-AREA-6302.                                                    
034600*    03  -COPY WDGX6302                                                   
034700     EJECT                                                                
034800 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDL611'.              
034900 01  DLI-IO-AREA-WDL611.                                                  
035000*    03  -COPY WDL611                                                     
035100     EJECT                                                                
035200 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDL6A1'.              
035300 01  DLI-IO-AREA-WDL6A1.                                                  
035400*    03  -COPY WDL6A1                                                     
035500     EJECT                                                                
035600 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK711'.              
035700 01  DLI-IO-AREA-WDK711.                                                  
035800*    03  -COPY WDK711                                                     
035900     EJECT                                                                
036000 01  FILLER               PIC X(16)   VALUE 'WLLOGA01'.                   
036100*01  WLLOGA01  -COPY WDL901                                               
036200     EJECT                                                                
036300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
036400 01  DLI-IO-AREA-B601.                                                    
036500*    03  -COPY WDB601                                                     
036600     EJECT                                                                
036700 01  FILLER               PIC X(16)   VALUE 'WDB601 TEST'.                
036800 01  DLI-IO-AREA-B601-TEST.                                               
036900*    03  -COPY WDB601   -PRE TEST-                                        
037000     EJECT                                                                
037100 01  FILLER               PIC X(16)   VALUE 'WDB601 REC AREA'.            
037200 01  DLI-IO-AREA-B601-REC.                                                
037300*    03  -COPY WDB601 -PRE REC-                                           
037400     EJECT                                                                
037500*                                                                         
037600 01  FILLER               PIC X(16)   VALUE 'DLI-IO-W6G130'.              
037700 01  DLI-IO-AREA-W6G130.                                                  
037800*    03  -COPY W6GX6006                                                   
037900     EJECT                                                                
038000 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK601'.              
038100 01  DLI-IO-AREA-WDK601.                                                  
038200*    03  -COPY WDK601                                                     
038300     EJECT                                                                
038400                                                                          
038500 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK611'.              
038600 01  DLI-IO-AREA-WDK611.                                                  
038700*    03  -COPY WDK611                                                     
038800     EJECT                                                                
038900                                                                          
039000 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDR901'.              
039100 01  DLI-IO-WDR901.                                                       
039200*    03  WDR901    -COPY WDR901                                           
039300*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
039401     EJECT                                                                
039501 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDR801'.              
039601 01  DLI-IO-WDR801.                                                       
039701*    03  WDR801    -COPY WDR801 -PRE EKO-                                 
039801*    07  -COPY W510EKHA  -RED EKO-FIL-WDR801-DATA -PRE EKO-               
039900     EJECT                                                                
040000 LINKAGE SECTION.                                                         
040100                                                                          
040200*01  -COPY W0009   -PRE MSG-                                              
040300     EJECT                                                                
040400*01  -COPY W0009   -PRE ALT1-                                             
040500     EJECT                                                                
040600*01  -COPY W0009   -PRE ALT2-                                             
040700     EJECT                                                                
040800*01  -COPY W0009   -PRE ALT-                                              
040900     EJECT                                                                
041000*01  -COPY W0008  -PRE USEA-                                              
041100     05  FILLER                  PIC X.                                   
041200     EJECT                                                                
041300*01  -COPY W0008  -PRE GX63-                                              
041400     05  FILLER                  PIC X.                                   
041500     EJECT                                                                
041600*01  -COPY W0008  -PRE INLC-                                              
041700     05  FILLER                  PIC X.                                   
041800     EJECT                                                                
041900*01  -COPY W0008  -PRE INLD-                                              
042000     05  FILLER                  PIC X.                                   
042100     EJECT                                                                
042200*01  -COPY W0008  -PRE WDK7-                                              
042300     05  FILLER                  PIC X.                                   
042400     EJECT                                                                
042500*01  -COPY W0008  -PRE ARTC-                                              
042600     05  FILLER                  PIC X.                                   
042700     EJECT                                                                
042800*01  -COPY W0008  -PRE LEVA-                                              
042900     05  FILLER                  PIC X.                                   
043000     EJECT                                                                
043100*01  -COPY W0008  -PRE LOGA-                                              
043200     05  FILLER                  PIC X.                                   
043300*01  -COPY W0008      -PRE WDB6-                                          
043400     05  FILLER                  PIC X.                                   
043500*01  -COPY W0008      -PRE W6G1-                                          
043600     05  FILLER                  PIC X.                                   
043700*01  -COPY W0008  -PRE WDR9-                                              
043800     05  FILLER                  PIC X.                                   
043901*01  -COPY W0008  -PRE WDR8-                                              
044001     05  FILLER                  PIC X.                                   
044100     EJECT                                                                
044200 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB ALT-PCB              
044300                           USEA-PCB                                       
044400                           GX63-PCB INLC-PCB INLD-PCB WDK7-PCB            
044500                           ARTC-PCB LEVA-PCB LOGA-PCB WDB6-PCB            
044600                           W6G1-PCB WDR9-PCB WDR8-PCB.                    
044700 MAIN SECTION.                                                            
044800     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB ALT-PCB              
044900                           USEA-PCB                                       
045000                           GX63-PCB INLC-PCB INLD-PCB WDK7-PCB            
045100                           ARTC-PCB LEVA-PCB LOGA-PCB WDB6-PCB            
045200                           W6G1-PCB WDR9-PCB WDR8-PCB.                    
045300                                                                          
045400     PERFORM IMS-GET-MSG                                                  
045500                                                                          
045600     IF SEGMENT-FINNS                                                     
045700        PERFORM A-INIT                                                    
045800        PERFORM B-KOLLA-NYCKLAR                                           
045900        IF NYCKLAR-OK                                                     
046000                                                                          
046100           IF MFS-UPDATE                                                  
046200              PERFORM G-KOLLA-INPUT                                       
046300                                                                          
046400              IF INDATA-OK                                                
046500                 PERFORM H-UPPDATERA                                      
046600              END-IF                                                      
046700                                                                          
046800           ELSE                                                           
046900              IF MFS-FIRST                                                
047000                 PERFORM C-FOERSTA-SIDA                                   
047100                                                                          
047200              ELSE                                                        
047300                 IF MFS-NEXT                                              
047400                    PERFORM D-NAESTA-SIDA                                 
047500                                                                          
047600                 ELSE                                                     
047700                    PERFORM E-SAMMA-SIDA                                  
047800                                                                          
047900                 END-IF                                                   
048000              END-IF                                                      
048100           END-IF                                                         
048200                                                                          
048300           IF  EJ-ATERHOPP                                                
048400           AND EJ-HOPP                                                    
048500              PERFORM F-LAES-VISA-INFO                                    
048600           END-IF                                                         
048700        END-IF                                                            
048800                                                                          
048900                                                                          
049000        IF ATERHOPP                                                       
049100           COMPUTE P-TO-P-LL = LNG-P-TO-P-PREFIX +                        
049200                               LENGTH OF MID-W6I30101                     
049300           MOVE LOW-VALUE      TO P-TO-P-Z1                               
049400           MOVE LOW-VALUE      TO P-TO-P-Z2                               
049500           MOVE 'W6T301U'      TO P-TO-P-TRANSKOD                         
049600           MOVE '6301'         TO P-TO-P-FROM-MID                         
049700           MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR                         
049800                                                                          
049900           MOVE MID-W6I30101   TO P-TO-P-DATA                             
050000           PERFORM IMS-ISRT-ALT1-PCB-6301                                 
050100        ELSE                                                              
050200           IF HOPP                                                        
050300              COMPUTE P-TO-P-LL =   LNG-P-TO-P-PREFIX +                   
050400                                   LENGTH OF 6302-MID-W6I30201            
050500                                                                          
050600              MOVE LOW-VALUE      TO P-TO-P-Z1                            
050700              MOVE LOW-VALUE      TO P-TO-P-Z2                            
050800              MOVE 'W6T302 '      TO P-TO-P-TRANSKOD                      
050900              MOVE '6301'         TO P-TO-P-FROM-MID                      
051000              MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR                      
051100                                                                          
051200              PERFORM I-EDIT-MID-W6I30201                                 
051300              MOVE 6302-MID-W6I30201 TO P-TO-P-DATA                       
051400              PERFORM IMS-ISRT-ALT2-PCB-6302                              
051500                                                                          
051600           ELSE                                                           
051700              COMPUTE MSG-KVLL = LENGTH OF MOD-W6O30101 + 4               
051800              PERFORM IMS-INSERT-MSG                                      
051900           END-IF                                                         
052000        END-IF                                                            
052100     END-IF                                                               
052200                                                                          
052300     MOVE ZERO TO RETURN-CODE                                             
052400     GOBACK                                                               
052500     .                                                                    
052600     EJECT                                                                
052700 A-INIT SECTION.                                                          
052800                                                                          
052900     IF MSG-DUBBLA-TRANSKODER                                             
053000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I30101                 
053100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
053200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
053300     ELSE                                                                 
053400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I30101                  
053500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
053600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
053700     END-IF                                                               
053800                                                                          
053900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
054000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
054100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
054200                                                                          
054300     MOVE LOW-VALUE TO MSG-AREA                                           
054400     MOVE 'W6O301N1' TO MFS-IDMOD                                         
054500     MOVE '6301' TO MOD-IDTRANS                                           
054600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
054700                                                                          
054800     IF EGEN-MID OR HELP-MID                                              
054900        CONTINUE                                                          
055000     ELSE                                                                 
055100        MOVE SPACE TO MFS-KDTRTYP                                         
055200        MOVE '7'   TO MFS-IDPFK                                           
055300     END-IF                                                               
055400                                                                          
           IF MFS-SPLIT                                                         
              MOVE 'PROP   G OUT   MAN' TO MOD-RUBKOL01                         
              MOVE 'ETA'       TO MOD-RUBKOL04                                  
              MOVE 'ETA'       TO MOD-RUBKOL05                                  
              MOVE 'ETA'       TO MOD-RUBKOL06                                  
           ELSE                                                                 
              MOVE 'CARRIER    TRP UNL'     TO MOD-RUBKOL01                     
              MOVE SPACES   TO MOD-RUBKOL04                                     
              MOVE '    STA'      TO MOD-RUBKOL05                               
              MOVE 'AREA'     TO MOD-RUBKOL06                                   
           END-IF                                                               
055500     MOVE SPACE TO W-TEMFSINF                                             
055600     ACCEPT DAGENS-DATUM FROM DATE                                        
055700     MOVE DAGENS-DATUM(3:2) TO PER-IX                                     
055800     ACCEPT AKTUELL-TID  FROM TIME                                        
055900     .                                                                    
056000     EJECT                                                                
056100 B-KOLLA-NYCKLAR SECTION.                                                 
056200                                                                          
056300     MOVE ALL '+' TO MSGI-WMSGINIT                                        
056400     MOVE '001' TO MSGI-KDCALL                                            
056500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
056600     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
056700     MOVE '6301'                 TO MSGI-IDTRANS                          
056800                                                                          
056900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
057000     MOVE MSGI-IDLAND-SPR      TO MED-IDSKYLT                             
057100     MOVE JA                   TO NYCKLAR-SW                              
057200                                                                          
057300     IF EGEN-MID OR HELP-MID                                              
057400*    -- KONTROLL AV TIFAKT                                                
057500        MOVE MFS-RENSA-FAELT TO MOD-TIFAKT-IN                             
057600                                                                          
057700        IF MID-TIFAKT-IN = ALL '+'                                        
057800           MOVE MID-TIFAKT-UT TO WS-TIFAKT                                
057900           INSPECT WS-TIFAKT REPLACING LEADING SPACE BY ZERO              
058000        ELSE                                                              
058100           MOVE MID-TIFAKT-IN TO WS-TIFAKT                                
058200           MOVE '7' TO MFS-IDPFK                                          
058300           MOVE SPACE TO MFS-KDTRTYP                                      
058400        END-IF                                                            
058500                                                                          
058600        IF WS-TIFAKT NUMERIC                                              
058700           CONTINUE                                                       
058800        ELSE                                                              
058900          MOVE NEJ TO NYCKLAR-SW                                          
059000        END-IF                                                            
059100     ELSE                                                                 
059200        MOVE MFS-RENSA-FAELT TO MOD-TIFAKT-IN                             
059300        MOVE ZERO TO WS-TIFAKT                                            
059400                     MOD-TIFAKT-UT                                        
059500     END-IF                                                               
059600                                                                          
059700     IF EGEN-MID OR HELP-MID                                              
059800*    -- KONTROLL AV IDDC-SEND                                             
059900        MOVE MFS-RENSA-FAELT TO MOD-IDDC-SEND-IN                          
060000                                                                          
060100        IF MID-IDDC-SEND-IN = ALL '+'                                     
060200           MOVE MID-IDDC-SEND-UT TO WS-IDDC-SEND                          
060300        ELSE                                                              
060400          MOVE MID-IDDC-SEND-IN TO WS-IDDC-SEND                           
060500          MOVE '7'          TO MFS-IDPFK                                  
060600          MOVE SPACE        TO MFS-KDTRTYP                                
060700        END-IF                                                            
060800                                                                          
060900        IF WS-IDDC-SEND = SPACE                                           
061000           CONTINUE                                                       
061100        ELSE                                                              
061200           MOVE WS-IDDC-SEND       TO W-IDDC-B6                           
061300           PERFORM IMS-GU-WDB601                                          
061400           IF DCS-KDDC = SPACE                                            
061500              MOVE NEJ TO NYCKLAR-SW                                      
061600           END-IF                                                         
061700        END-IF                                                            
061800     ELSE                                                                 
061900        MOVE MFS-RENSA-FAELT TO MOD-IDDC-SEND-IN                          
062000        MOVE SPACE TO WS-IDDC-SEND                                        
062100                      MOD-IDDC-SEND-UT                                    
062200     END-IF                                                               
062300                                                                          
062400     IF EGEN-MID OR HELP-MID                                              
062500*    -- KONTROLL AV IDLBBET                                               
062600        MOVE MFS-RENSA-FAELT TO MOD-IDLBBET-IN                            
062700                                                                          
062800        IF MID-IDLBBET-IN          = ALL '+'                              
062900           MOVE MID-IDLBBET-UT     TO W-IDLBBET                           
063000        ELSE                                                              
063100           MOVE MID-IDLBBET-IN     TO W-IDLBBET                           
063200           MOVE '7'                TO MFS-IDPFK                           
063300           MOVE SPACE              TO MFS-KDTRTYP                         
063400        END-IF                                                            
063500     ELSE                                                                 
063600        MOVE MFS-RENSA-FAELT TO MOD-IDLBBET-IN                            
063700        MOVE SPACE TO W-IDLBBET                                           
063800     END-IF                                                               
063900                                                                          
064000     IF EGEN-MID OR HELP-MID                                              
064100*    -- KONTROLL AV IDDC                                                  
064200        MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                               
064300                                                                          
064400        IF MID-IDDC-IN = ALL '+'                                          
064500           MOVE MID-IDDC-UT TO W-IDDC                                     
064600        ELSE                                                              
064700          MOVE MID-IDDC-IN TO W-IDDC                                      
064800          MOVE '7'          TO MFS-IDPFK                                  
064900          MOVE SPACE        TO MFS-KDTRTYP                                
065000        END-IF                                                            
065100                                                                          
065200        IF W-IDDC = SPACE                                                 
065300           MOVE MSGI-IDDC   TO W-IDDC                                     
065400        ELSE                                                              
065500           MOVE W-IDDC        TO W-IDDC-B6                                
065600           PERFORM IMS-GU-WDB601                                          
065700           IF DCS-KDDC = SPACE OR DCS-DDC                                 
065800              MOVE NEJ TO NYCKLAR-SW                                      
065900           END-IF                                                         
066000        END-IF                                                            
066100     ELSE                                                                 
066200        MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                               
066300        MOVE MSGI-IDDC TO W-IDDC                                          
066400                          MOD-IDDC-UT                                     
066500     END-IF                                                               
066600*    -- KONTROLL AV RECEVING IDDC                                         
066700     MOVE W-IDDC   TO W-IDDC-B6                                           
066800                      WS-IDDC-REC                                         
066900     PERFORM IMS-GU-WDB601-REC                                            
067000     IF SEGMENT-FINNS                                                     
067100       IF REC-DCS-SWEDEN                                                  
067200          SET AKTUELLT-LAND-SVERIGE TO TRUE                               
067300       ELSE                                                               
067400          SET AKTUELLT-EJ-SVERIGE   TO TRUE                               
067500       END-IF                                                             
067601       IF REC-DCS-USA                                                     
067701          SET AKTUELLT-LAND-USA     TO TRUE                               
067801       ELSE                                                               
067901          SET AKTUELLT-EJ-USA       TO TRUE                               
068001       END-IF                                                             
068100       IF REC-DCS-DDC                                                     
068200         MOVE NEJ       TO NYCKLAR-SW                                     
068300       END-IF                                                             
068400     END-IF                                                               
068500                                                                          
068600     IF MID-IDDC-SEND-IN = ALL '+'                                        
068700     AND MID-IDDC-IN     = ALL '+'                                        
068800     AND MID-IDLBBET-IN  = ALL '+'                                        
068900     AND MID-TIFAKT-IN   = ALL '+'                                        
069000        CONTINUE                                                          
069100     ELSE                                                                 
069200        MOVE +1 TO INDX                                                   
069300        PERFORM UNTIL INDX > MAX-INDX                                     
069400                                                                          
069500           IF MID-TIBERANK(INDX) NOT = ALL '+'                            
069600              MOVE ALL '+'   TO MID-TIBERANK(INDX)                        
069700           END-IF                                                         
069800           IF MID-ADINLOMR(INDX) NOT = ALL '+'                            
069900              MOVE ALL '+'   TO MID-ADINLOMR(INDX)                        
070000           END-IF                                                         
070100                                                                          
070200           ADD +1 TO INDX                                                 
070300        END-PERFORM                                                       
070400                                                                          
070500        IF MID-ADINLOMR-PRT NOT = ALL '+'                                 
070600           MOVE ALL '+'      TO MID-ADINLOMR-PRT                          
070700        END-IF                                                            
070800     END-IF                                                               
070900                                                                          
071000     IF GODK-MID                                                          
071100     OR NYCKLAR-OK                                                        
071200        MOVE WS-TIFAKT       TO MOD-TIFAKT-UT                             
071300        MOVE WS-IDDC-SEND    TO MOD-IDDC-SEND-UT                          
071400        MOVE W-IDLBBET       TO MOD-IDLBBET-UT                            
071500        MOVE W-IDDC          TO MOD-IDDC-UT                               
071600        IF MOD-TIFAKT-UT = ZERO                                           
071700           INSPECT MOD-TIFAKT-UT REPLACING LEADING ZERO BY SPACE          
071800        END-IF                                                            
071900     ELSE                                                                 
072000        MOVE MFS-RENSA-FAELT TO MOD-TIFAKT-UT                             
072100        MOVE MFS-RENSA-FAELT TO MOD-IDLBBET-UT                            
072200        MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                               
072300        MOVE MFS-RENSA-FAELT TO MOD-IDDC-SEND-UT                          
072400     END-IF                                                               
072500                                                                          
072600     IF NYCKLAR-FEL                                                       
072700        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
072800        CALL WMEDKONV USING MED-WMEDAREA                                  
072900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
073000        PERFORM MFS-RENSA-FAELT-IN                                        
073100        PERFORM MFS-RENSA-FAELT-UT                                        
073200     END-IF                                                               
073300     .                                                                    
073400     EJECT                                                                
073500 C-FOERSTA-SIDA SECTION.                                                  
073600                                                                          
073700     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
073800     CALL WMEDKONV USING MED-WMEDAREA                                     
073900     MOVE MED-MFSINF      TO MOD-TEMFSFEL                                 
074000                                                                          
074100     MOVE ZERO            TO W-IDFAKT                                     
074200                             W-DABERANK                                   
074300     PERFORM MFS-RENSA-FAELT-IN                                           
074400     .                                                                    
074500     EJECT                                                                
074600 D-NAESTA-SIDA SECTION.                                                   
074700                                                                          
074800     IF MID-TIBERANK-NEXT NUMERIC                                         
074900        MOVE MID-TIBERANK-NEXT TO W-DABERANK                              
075000        IF MID-TIBERANK-NEXT NOT = ZERO                                   
075100          IF MID-TIBERANK-NEXT < 500000                                   
075200            MOVE 20            TO W-DABERANK (1:2)                        
075300          ELSE                                                            
075400            IF MID-TIBERANK-NEXT < 999999                                 
075500              MOVE 19          TO W-DABERANK (1:2)                        
075600            ELSE                                                          
075700              MOVE 99999999    TO W-DABERANK                              
075800            END-IF                                                        
075900          END-IF                                                          
076000        END-IF                                                            
076100     ELSE                                                                 
076200        MOVE ZERO TO W-DABERANK                                           
076300     END-IF                                                               
076400                                                                          
076500     INSPECT MID-IDFAKT-NEXT REPLACING LEADING SPACE BY ZERO              
076600     IF MID-IDFAKT-NEXT NUMERIC                                           
076700         MOVE MID-IDFAKT-NEXT TO W-IDFAKT                                 
076800     ELSE                                                                 
076900         MOVE ZERO TO W-IDFAKT                                            
077000     END-IF                                                               
077100                                                                          
077200     MOVE MID-IDDC-NEXT TO W-IDDC                                         
077300     PERFORM MFS-RENSA-FAELT-IN                                           
077400                                                                          
077500     IF  W-DABERANK       = ZERO                                          
077600     AND W-IDFAKT         = ZERO                                          
077700     AND W-IDDC           = SPACE                                         
077800         MOVE INF-SISTA-SIDAN TO MED-IDMFSINF                             
077900         CALL WMEDKONV USING MED-WMEDAREA                                 
078000         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
078100                                                                          
078200         MOVE MID-IDDC-ENTER TO W-IDDC                                    
078300         IF  MID-TIBERANK-ENTER NUMERIC                                   
078400             MOVE MID-TIBERANK-ENTER TO W-DABERANK                        
078500             IF MID-TIBERANK-ENTER NOT = ZERO                             
078600               IF MID-TIBERANK-ENTER < 500000                             
078700                 MOVE 20             TO W-DABERANK (1:2)                  
078800               ELSE                                                       
078900                 IF MID-TIBERANK-ENTER < 999999                           
079000                   MOVE 19           TO W-DABERANK (1:2)                  
079100                 ELSE                                                     
079200                   MOVE 99999999     TO W-DABERANK                        
079300                 END-IF                                                   
079400               END-IF                                                     
079500             END-IF                                                       
079600         ELSE                                                             
079700             MOVE ZERO TO W-DABERANK                                      
079800         END-IF                                                           
079900                                                                          
080000         INSPECT MID-IDFAKT-ENTER REPLACING LEADING SPACE BY ZERO         
080100         IF  MID-IDFAKT-ENTER NUMERIC                                     
080200             MOVE MID-IDFAKT-ENTER TO W-IDFAKT                            
080300         ELSE                                                             
080400             MOVE ZERO TO W-IDFAKT                                        
080500         END-IF                                                           
080600     END-IF                                                               
080700     .                                                                    
080800     EJECT                                                                
080900 E-SAMMA-SIDA SECTION.                                                    
081000                                                                          
081100     IF EGEN-MID                                                          
081200     OR HELP-MID                                                          
081300                                                                          
081400        IF MID-TIBERANK-ENTER NUMERIC                                     
081500           MOVE MID-TIBERANK-ENTER TO W-DABERANK                          
081600           IF MID-TIBERANK-ENTER NOT = ZERO                               
081700             IF MID-TIBERANK-ENTER < 500000                               
081800               MOVE 20             TO W-DABERANK (1:2)                    
081900             ELSE                                                         
082000               IF MID-TIBERANK-ENTER < 999999                             
082100                 MOVE 19           TO W-DABERANK (1:2)                    
082200               ELSE                                                       
082300                 MOVE 99999999     TO W-DABERANK                          
082400               END-IF                                                     
082500             END-IF                                                       
082600           END-IF                                                         
082700        ELSE                                                              
082800           MOVE ZERO TO W-DABERANK                                        
082900        END-IF                                                            
083000                                                                          
083100        IF MID-IDFAKT-ENTER NUMERIC                                       
083200           MOVE MID-IDFAKT-ENTER TO W-IDFAKT                              
083300        ELSE                                                              
083400           MOVE ZERO TO W-IDFAKT                                          
083500        END-IF                                                            
083600                                                                          
083700        IF  MID-CMD-IN (1) = ALL '+'                                      
083800        AND MID-CMD-IN (2) = ALL '+'                                      
083900        AND MID-CMD-IN (3) = ALL '+'                                      
084000        AND MID-CMD-IN (4) = ALL '+'                                      
084100        AND MID-CMD-IN (5) = ALL '+'                                      
084200        AND MID-CMD-IN (6) = ALL '+'                                      
084300        AND MID-CMD-IN (7) = ALL '+'                                      
084400        AND MID-CMD-IN (8) = ALL '+'                                      
084500        AND MID-CMD-IN (9) = ALL '+'                                      
084600        AND MID-CMD-IN (10) = ALL '+'                                     
084700        AND MID-CMD-IN (11) = ALL '+'                                     
084800        AND MID-CMD-IN (12) = ALL '+'                                     
084900        AND MID-CMD-IN (13) = ALL '+'                                     
085000        AND MID-TIBERANK(1) = ALL '+'                                     
085100        AND MID-TIBERANK(2) = ALL '+'                                     
085200        AND MID-TIBERANK(3) = ALL '+'                                     
085300        AND MID-TIBERANK(4) = ALL '+'                                     
085400        AND MID-TIBERANK(5) = ALL '+'                                     
085500        AND MID-TIBERANK(6) = ALL '+'                                     
085600        AND MID-TIBERANK(7) = ALL '+'                                     
085700        AND MID-TIBERANK(8) = ALL '+'                                     
085800        AND MID-TIBERANK(9) = ALL '+'                                     
085900        AND MID-TIBERANK(10) = ALL '+'                                    
086000        AND MID-TIBERANK(11) = ALL '+'                                    
086100        AND MID-TIBERANK(12) = ALL '+'                                    
086200        AND MID-TIBERANK(13) = ALL '+'                                    
086300        AND MID-ADINLOMR(1) = ALL '+'                                     
086400        AND MID-ADINLOMR(2) = ALL '+'                                     
086500        AND MID-ADINLOMR(3) = ALL '+'                                     
086600        AND MID-ADINLOMR(4) = ALL '+'                                     
086700        AND MID-ADINLOMR(5) = ALL '+'                                     
086800        AND MID-ADINLOMR(6) = ALL '+'                                     
086900        AND MID-ADINLOMR(7) = ALL '+'                                     
087000        AND MID-ADINLOMR(8) = ALL '+'                                     
087100        AND MID-ADINLOMR(9) = ALL '+'                                     
087200        AND MID-ADINLOMR(10) = ALL '+'                                    
087300        AND MID-ADINLOMR(11) = ALL '+'                                    
087400        AND MID-ADINLOMR(12) = ALL '+'                                    
087500        AND MID-ADINLOMR(13) = ALL '+'                                    
087600        AND MID-ADINLOMR-PRT = ALL '+'                                    
087700           PERFORM MFS-RENSA-FAELT-IN                                     
087800        ELSE                                                              
087900           PERFORM EA-KOLLA-CMD                                           
088000           IF  INDATA-OK                                                  
088100               IF  W-CMD  = 'REC'                                         
088200               OR  W-CMD  = 'R  '                                         
088300               OR  W-CMD  = 'A  '                                         
088400               OR  W-CMD  = 'I  '                                         
088500               OR  W-CMD  = 'C  '                                         
088600               OR  SW-TIBERANK = JA                                       
088700               OR  SW-ADINLOMR = JA                                       
088800                   MOVE INF-PRESS-PF11 TO MED-IDMFSINF                    
088900                   CALL WMEDKONV USING MED-WMEDAREA                       
089000                   MOVE MED-MFSINF TO MOD-TEMFSFEL                        
089100                   PERFORM EB-MID-INDATA-TILL-MOD                         
089200               ELSE                                                       
089300                  IF  W-CMD = 'S  ' OR 'X  '                              
089400                      MOVE JA TO HOPP-SW                                  
089500                  ELSE                                                    
089600                     IF W-CMD = 'PR '                                     
089700                        MOVE +1 TO INDX                                   
089800                        PERFORM UNTIL INDX > MAX-INDX                     
089900                           IF MID-CMD-IN(INDX) = 'PR '                    
090000                              PERFORM EC-SKAPA-PRINTER-TRANS              
090100                           END-IF                                         
090200                           ADD +1 TO INDX                                 
090300                        END-PERFORM                                       
090400                     ELSE                                                 
090500                        IF W-CMD = 'BC '                                  
090600                           MOVE +1 TO INDX                                
090700                           PERFORM UNTIL INDX > MAX-INDX                  
090800                              IF MID-CMD-IN(INDX) = 'BC '                 
090900                                 PERFORM ED-SKAPA-BC-TRANS                
091000                              END-IF                                      
091100                              ADD +1 TO INDX                              
091200                           END-PERFORM                                    
091300                        END-IF                                            
091400                     END-IF                                               
091500                  END-IF                                                  
091600               END-IF                                                     
091700           END-IF                                                         
091800        END-IF                                                            
091900     ELSE                                                                 
092000        PERFORM MFS-RENSA-FAELT-IN                                        
092100     END-IF                                                               
092200     .                                                                    
092300     EJECT                                                                
092400 EA-KOLLA-CMD SECTION.                                                    
092500******************************************************************        
092600* ENTER  CMD = S    HOPP TILL BILD 6302                                   
092700*        CMD = X                                                          
092800*        CMD = PR/P STARTAR W612S1                                        
092900*        CMD = BC   STARTAR W612S3                                        
093000* PF11   CMD = C     =  STOCKED IN CUSTOM                                 
093100*        CMD = A     =  AVAILABLE, READY AT CUSTOM                        
093200*        CMD = I     =  IN TRANSIT FROM CUSTOM                            
093300*        CMD = REC/R =  RECEIVED                                          
093400*        STATUS 'M' UPPDATERAS PÅ 6302 VID SAKNAT KOLLI                   
093500*                   I FAKTURA = MISSING                                   
093600******************************************************************        
093700     MOVE JA    TO INDATA-SW                                              
093800     MOVE NEJ   TO SW-TIBERANK                                            
093900                   SW-ADINLOMR                                            
094000     MOVE SPACE TO W-CMD                                                  
094100     MOVE +1    TO INDX                                                   
094200                                                                          
094300     PERFORM UNTIL INDX > MAX-INDX                                        
094400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR (INDX)                 
094500                                                                          
094600         IF  MID-CMD-IN (INDX) NOT = ALL '+'                              
094700         AND MID-CMD-IN (INDX) NOT = 'S  '                                
094800         AND MID-CMD-IN (INDX) NOT = 'X  '                                
094900         AND MID-CMD-IN (INDX) NOT = 'REC'                                
095000         AND MID-CMD-IN (INDX) NOT = 'R  '                                
095100         AND MID-CMD-IN (INDX) NOT = 'I  '                                
095200         AND MID-CMD-IN (INDX) NOT = 'C  '                                
095300         AND MID-CMD-IN (INDX) NOT = 'A  '                                
095400         AND MID-CMD-IN (INDX) NOT = 'PR '                                
095500         AND MID-CMD-IN (INDX) NOT = 'BC '                                
095600         AND MID-CMD-IN (INDX) NOT = SPACE                                
095700             MOVE NEJ TO INDATA-SW                                        
095800             MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)               
095900         END-IF                                                           
096000                                                                          
096100         IF  MID-CMD-IN (INDX) = 'S  '                                    
096200         OR  MID-CMD-IN (INDX) = 'X  '                                    
096300         OR  MID-CMD-IN (INDX) = 'REC'                                    
096400         OR  MID-CMD-IN (INDX) = 'R  '                                    
096500         OR  MID-CMD-IN (INDX) = 'A  '                                    
096600         OR  MID-CMD-IN (INDX) = 'I  '                                    
096700         OR  MID-CMD-IN (INDX) = 'C  '                                    
096800         OR  MID-CMD-IN (INDX) = 'PR '                                    
096900         OR  MID-CMD-IN (INDX) = 'BC '                                    
097000             IF  MID-IDFAKT (INDX) = ZERO                                 
097100                 MOVE NEJ TO INDATA-SW                                    
097200                 MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)           
097300             END-IF                                                       
097400         END-IF                                                           
097500                                                                          
097600         IF  MID-CMD-IN (INDX) = 'S  '                                    
097700         OR  MID-CMD-IN (INDX) = 'X  '                                    
097800             IF  W-CMD = SPACE                                            
097900                 MOVE MID-CMD-IN (INDX) TO W-CMD                          
098000             ELSE                                                         
098100                 MOVE NEJ TO INDATA-SW                                    
098200                 MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)           
098300             END-IF                                                       
098400         END-IF                                                           
098500                                                                          
098600         IF  MID-CMD-IN (INDX) = 'REC'                                    
098700         OR  MID-CMD-IN (INDX) = 'R  '                                    
098800         OR  MID-CMD-IN (INDX) = 'A  '                                    
098900         OR  MID-CMD-IN (INDX) = 'I  '                                    
099000         OR  MID-CMD-IN (INDX) = 'C  '                                    
099100             IF  W-CMD = SPACE                                            
099200                 MOVE MID-CMD-IN (INDX) TO W-CMD                          
099300             ELSE                                                         
099400                 IF  W-CMD         = 'S  '                                
099500                 OR  W-CMD         = 'X  '                                
099600                 OR  W-CMD         = 'PR '                                
099700                 OR  W-CMD         = 'BC '                                
099800                     MOVE NEJ TO INDATA-SW                                
099900                     MOVE MFS-ALFA-FAELT-FEL                              
100000                                  TO MOD-CMD-ATTR (INDX)                  
100100                 END-IF                                                   
100200             END-IF                                                       
100300         END-IF                                                           
100400                                                                          
100500         IF  MID-CMD-IN (INDX) = 'PR '                                    
100600             IF  W-CMD = SPACE                                            
100700                 MOVE MID-CMD-IN (INDX) TO W-CMD                          
100800             ELSE                                                         
100900                IF W-CMD = 'S  '                                          
101000                OR W-CMD = 'X  '                                          
101100                OR W-CMD = 'R  '                                          
101200                OR W-CMD = 'REC'                                          
101300                OR W-CMD = 'A  '                                          
101400                OR W-CMD = 'I  '                                          
101500                OR W-CMD = 'C  '                                          
101600                OR W-CMD = 'BC '                                          
101700                   MOVE NEJ TO INDATA-SW                                  
101800                   MOVE MFS-ALFA-FAELT-FEL                                
101900                               TO MOD-CMD-ATTR (INDX)                     
102000                END-IF                                                    
102100             END-IF                                                       
102200         END-IF                                                           
102300                                                                          
102400         IF  MID-CMD-IN (INDX) = 'BC '                                    
102500             IF  W-CMD = SPACE                                            
102600                 MOVE MID-CMD-IN (INDX) TO W-CMD                          
102700             ELSE                                                         
102800                IF W-CMD = 'S  '                                          
102900                OR W-CMD = 'X  '                                          
103000                OR W-CMD = 'R  '                                          
103100                OR W-CMD = 'REC'                                          
103200                OR W-CMD = 'A  '                                          
103300                OR W-CMD = 'I  '                                          
103400                OR W-CMD = 'C  '                                          
103500                OR W-CMD = 'PR '                                          
103600                   MOVE NEJ TO INDATA-SW                                  
103700                   MOVE MFS-ALFA-FAELT-FEL                                
103800                               TO MOD-CMD-ATTR (INDX)                     
103900                END-IF                                                    
104000             END-IF                                                       
104100         END-IF                                                           
104200                                                                          
104300         IF MID-TIBERANK(INDX) NOT = ALL '+'                              
104400            MOVE JA TO SW-TIBERANK                                        
104500            MOVE MFS-NUM-FAELT-RAETT TO MOD-TIBERANK-ATTR(INDX)           
104600         END-IF                                                           
104700                                                                          
104800         IF MID-ADINLOMR(INDX) NOT = ALL '+'                              
104900            MOVE JA TO SW-ADINLOMR                                        
105000            MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-ATTR(INDX)          
105100         END-IF                                                           
105200         ADD 1            TO INDX                                         
105300     END-PERFORM                                                          
105400                                                                          
105500     IF MID-ADINLOMR-PRT NOT = ALL '+'                                    
105600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PRT-ATTR                 
105700     END-IF                                                               
105800                                                                          
105900     IF W-CMD = 'PR '                                                     
106000        IF MID-ADINLOMR-PRT = ALL '+' OR SPACE                            
106100           CONTINUE                                                       
106200        ELSE                                                              
106300           PERFORM EAA-CHECK-ADINLOMR-PRT                                 
106400        END-IF                                                            
106500     END-IF                                                               
106600                                                                          
106700     IF INDATA-FEL                                                        
106800        IF MED-IDMFSFEL = SPACE                                           
106900          MOVE ERR-CORR-HILITE-FLDS                                       
107000                                 TO MED-IDMFSFEL                          
107100        END-IF                                                            
107200        CALL WMEDKONV USING MED-WMEDAREA                                  
107300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
107400        PERFORM MFS-ROER-EJ-FAELT-UT                                      
107500        PERFORM MFS-ROER-EJ-FAELT-IN                                      
107600     END-IF                                                               
107700                                                                          
107800     IF MSGI-IDDC = W-IDDC                                                
107900        CONTINUE                                                          
108000     ELSE                                                                 
108100        IF W-CMD = 'PR ' OR 'BC '                                         
108200           MOVE NOTHING-PRINTED-WRONG-DC TO MOD-TEMFSFEL                  
108300           MOVE NEJ TO INDATA-SW                                          
108400        END-IF                                                            
108500     END-IF                                                               
108600                                                                          
108700     .                                                                    
108800     EJECT                                                                
108900 EAA-CHECK-ADINLOMR-PRT SECTION.                                          
109000                                                                          
109100     IF DCS-IDDC NOT = W-IDDC                                             
109200        MOVE W-IDDC              TO W-IDDC-B6                             
109300        PERFORM IMS-GU-WDB601                                             
109400     END-IF                                                               
109500                                                                          
109600     IF DCS-CDC                                                           
109700       MOVE SPACE                TO PRT-IDPRTLST                          
109800       MOVE '6M'                 TO PRT-IDPRTLST (1:2)                    
109900       MOVE MID-ADINLOMR-PRT     TO PRT-IDPRTLST (3:4)                    
110000       MOVE 1                    TO PRT-KDCALL                            
110100       CALL W006PRT           USING PRT-W006PRT                           
110200       IF PRT-KDSVAR = 'F'                                                
110300         MOVE SPACE              TO PRT-IDPRTLST                          
110400         MOVE '6L'               TO PRT-IDPRTLST (1:2)                    
110500         MOVE MID-ADINLOMR-PRT   TO PRT-IDPRTLST (3:4)                    
110600         MOVE 1                  TO PRT-KDCALL                            
110700         CALL W006PRT         USING PRT-W006PRT                           
110800         IF PRT-KDSVAR = 'F'                                              
110900           MOVE NEJ              TO INDATA-SW                             
111000           MOVE ERR-WRONG-PRINTER                                         
111100                                 TO MED-IDMFSFEL                          
111200           MOVE MFS-ALFA-FAELT-FEL                                        
111300                                 TO MOD-ADINLOMR-PRT-ATTR                 
111400         ELSE                                                             
111500           MOVE PRT-IDLTERM      TO WS-IDLTERM                            
111600           MOVE MFS-ALFA-FAELT-RAETT                                      
111700                                 TO MOD-ADINLOMR-PRT-ATTR                 
111800         END-IF                                                           
111900       ELSE                                                               
112000         MOVE PRT-IDLTERM        TO WS-IDLTERM                            
112100         MOVE MFS-ALFA-FAELT-RAETT                                        
112200                                 TO MOD-ADINLOMR-PRT-ATTR                 
112300       END-IF                                                             
112400     ELSE                                                                 
112500       MOVE NEJ                  TO INDATA-SW                             
112600       MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADINLOMR-PRT-ATTR                 
112700       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
112800     END-IF                                                               
112900     .                                                                    
113000     EJECT                                                                
113100 EB-MID-INDATA-TILL-MOD SECTION.                                          
113200                                                                          
113300* * * * * FÖR VARJE MID-FÄLT                                              
113400* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
113500* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
113600* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
113700                                                                          
113800     MOVE +1 TO INDX                                                      
113900                                                                          
114000     PERFORM UNTIL INDX > MAX-INDX                                        
114100         IF  MID-CMD-IN (INDX) NOT = ALL '+'                              
114200             MOVE MID-CMD-IN (INDX) TO MOD-CMD-IN (INDX)                  
114300             MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR (INDX)            
114400         ELSE                                                             
114500             MOVE MFS-RENSA-FAELT TO MOD-CMD-IN(INDX)                     
114600         END-IF                                                           
114700         IF  MID-TIBERANK (INDX) NOT = ALL '+'                            
114800             MOVE MID-TIBERANK(INDX) TO MOD-TIBERANK (INDX)               
114900             MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIBERANK-ATTR(INDX)        
115000         ELSE                                                             
115100             MOVE MFS-RENSA-FAELT TO MOD-TIBERANK(INDX)                   
115200         END-IF                                                           
115300         IF  MID-ADINLOMR (INDX) NOT = ALL '+'                            
115400             MOVE MID-ADINLOMR(INDX) TO MOD-ADINLOMR (INDX)               
115500             MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-ATTR(INDX)        
115600         ELSE                                                             
115700             MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR(INDX)                   
115800         END-IF                                                           
115900                                                                          
116000         ADD 1 TO INDX                                                    
116100     END-PERFORM                                                          
116200                                                                          
116300     IF MID-ADINLOMR-PRT NOT = ALL '+'                                    
116400       MOVE MID-ADINLOMR-PRT     TO MOD-ADINLOMR-PRT                      
116500       MOVE MFS-ADD-LAES-IN-FAELT                                         
116600                                 TO MOD-ADINLOMR-PRT-ATTR                 
116700     ELSE                                                                 
116800       MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-PRT                      
116900     END-IF                                                               
117000     .                                                                    
117100     EJECT                                                                
117200 EC-SKAPA-PRINTER-TRANS SECTION.                                          
117300*                                                                         
117400     IF DCS-IDDC NOT = W-IDDC                                             
117500        MOVE W-IDDC      TO W-IDDC-B6                                     
117600        PERFORM IMS-GU-WDB601                                             
117700     END-IF                                                               
117800                                                                          
117900     IF WS-IDLTERM = SPACE                                                
118000       MOVE DCS-IDPRTLST-INL     TO PRT-IDPRTLST                          
118100       MOVE 1                    TO PRT-KDCALL                            
118200       CALL W006PRT           USING PRT-W006PRT                           
118300       MOVE PRT-IDLTERM          TO URV-IDPRINTER                         
118400     ELSE                                                                 
118500       MOVE WS-IDLTERM           TO URV-IDPRINTER                         
118600     END-IF                                                               
118700                                                                          
118800     MOVE MID-IDFAKT(INDX)  TO URV-IDFAKT                                 
118900                                                                          
119000     MOVE '6301'   TO MSGSOP-IDTRANS                                      
119100     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
119200     MOVE 'W612S1' TO MSGSOP-IDPROCESS                                    
119300     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
119400                                                                          
119500     STRING 'URVAL(' WS-URVAL ')PRT('                                     
119600            WS-PRINTER ')'                                                
119700            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
119800                                                                          
119900     PERFORM IMS-INSERT-ALTMSG                                            
120000     MOVE INF-PRINT-BEGAERD TO MED-IDMFSINF                               
120100     CALL WMEDKONV USING MED-WMEDAREA                                     
120200     MOVE MED-TEMFSINF      TO MOD-TEMFSFEL                               
120300     .                                                                    
120400     EJECT                                                                
120500 ED-SKAPA-BC-TRANS SECTION.                                               
120600                                                                          
120700     MOVE W-IDFAKT         TO WS-SPAR-IDFAKT                              
120800     INSPECT MID-IDFAKT(INDX) REPLACING LEADING SPACE BY ZERO             
120900     MOVE MID-IDFAKT(INDX) TO BC-URV-IDFAKT                               
121000                              W-IDFAKT                                    
121100     MOVE ZERO             TO BC-URV-IDKUNDRF                             
121200                              BC-URV-IDKUNDNR                             
121300                              BC-URV-IDKOLLI                              
121400     MOVE '6301'           TO W-6301-IDHTYP                               
121500     MOVE W-IDDC           TO W-6301-IDDC                                 
121600     MOVE LOW-VALUE        TO W-6301-LOWVALUE                             
121700                                                                          
121800                                                                          
121900     PERFORM IMS-GHU-WL630111-GE                                          
122000     IF SEGMENT-FINNS                                                     
122100        IF 6302-KVRADER-MOT > ZERO                                        
122200           MOVE BINNING-STARTED-USE-6302 TO MOD-TEMFSFEL                  
122300        ELSE                                                              
122400           MOVE '6301'   TO MSGSOP-IDTRANS                                
122500           MOVE '1'      TO MSGSOP-KDMFSFOR                               
122600           MOVE 'W612S3' TO MSGSOP-IDPROCESS                              
122700           MOVE 'O'      TO MSGSOP-KDSOPFUNK                              
122800                                                                          
122900           STRING 'URVAL(' WS-BC ')'                                      
123000                   DELIMITED BY SIZE INTO MSGSOP-TESYMBV                  
123100                                                                          
123200           PERFORM IMS-INSERT-ALTMSG                                      
123300           MOVE INF-PRINT-BEGAERD TO MED-IDMFSINF                         
123400           CALL WMEDKONV USING MED-WMEDAREA                               
123500           MOVE MED-TEMFSINF      TO MOD-TEMFSFEL                         
123600        END-IF                                                            
123700        MOVE WS-SPAR-IDFAKT TO W-IDFAKT                                   
123800     END-IF                                                               
123900     .                                                                    
124000     EJECT                                                                
124100 F-LAES-VISA-INFO SECTION.                                                
124200                                                                          
124300     PERFORM FA-LAES-GRUNDDATA                                            
124400                                                                          
124500     IF SEGMENT-SAKNAS                                                    
124600        MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                             
124700        CALL WMEDKONV USING MED-WMEDAREA                                  
124800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
124900                                                                          
125000     ELSE                                                                 
125100****************** FÖRSTA SÖKTA                                           
125200                                                                          
125300        MOVE 6301-IDDC         TO MOD-IDDC-ENTER                          
125400                                  MOD-IDDC-NEXT                           
125500        MOVE ZERO              TO INDX                                    
125600        MOVE NEJ               TO SW-TRAEFF                               
125700        PERFORM FB-LAES-RADDATA                                           
125800                                                                          
125900        IF SEGMENT-FINNS                                                  
126000           PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = JA                 
126100             IF WS-IDDC-SEND = SPACE                                      
126200               MOVE 6302-TIFAKT   TO TMP1-YYMMDD                          
126300               MOVE TIFAKT-WS     TO TMP2-YYMMDD                          
126400               PERFORM WY2000P1                                           
126500               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
126600                  MOVE 6302-DABERANK (3:6) TO MOD-TIBERANK-ENTER          
126700                  MOVE 6302-IDFAKT   TO MOD-IDFAKT-ENTER                  
126800                  MOVE JA TO SW-TRAEFF                                    
126900               END-IF                                                     
127000             ELSE                                                         
127100               MOVE 6302-TIFAKT   TO TMP1-YYMMDD                          
127200               MOVE TIFAKT-WS     TO TMP2-YYMMDD                          
127300               PERFORM WY2000P1                                           
127400               IF (WS-IDDC-SEND = 6302-IDDC-SEND) AND                     
127500                  (TMP1-YYMMDD >= TMP2-YYMMDD)                            
127600                  MOVE 6302-DABERANK (3:6) TO MOD-TIBERANK-ENTER          
127700                  MOVE 6302-IDFAKT   TO MOD-IDFAKT-ENTER                  
127800                  MOVE JA TO SW-TRAEFF                                    
127900               ELSE                                                       
128000                  IF DCS-IDDC NOT = WS-IDDC-SEND                          
128100                     MOVE WS-IDDC-SEND TO W-IDDC-B6                       
128200                     PERFORM IMS-GU-WDB601                                
128300                  END-IF                                                  
128400                  IF TEST-DCS-IDDC NOT = 6302-IDDC-SEND                   
128500                     MOVE 6302-IDDC-SEND TO W-IDDC-B6-TEST                
128600                     PERFORM IMS-GU-WDB601-TEST                           
128700                  END-IF                                                  
128800                  IF (DCS-CDC AND TEST-DCS-DDC) AND                       
128900                  (TMP1-YYMMDD >= TMP2-YYMMDD)                            
129000                     MOVE 6302-DABERANK (3:6)                             
129100                          TO MOD-TIBERANK-ENTER                           
129200                     MOVE 6302-IDFAKT   TO MOD-IDFAKT-ENTER               
129300                     MOVE JA TO SW-TRAEFF                                 
129400                  END-IF                                                  
129500               END-IF                                                     
129600             END-IF                                                       
129700             IF SW-TRAEFF = NEJ                                           
129800                PERFORM FB-LAES-RADDATA                                   
129900             END-IF                                                       
130000          END-PERFORM                                                     
130100       ELSE                                                               
130200          MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                           
130300          CALL WMEDKONV USING MED-WMEDAREA                                
130400          MOVE MED-MFSFEL    TO MOD-TEMFSFEL                              
130500          MOVE ZERO          TO MOD-TIBERANK-ENTER                        
130600          MOVE ZERO          TO MOD-IDFAKT-ENTER                          
130700          MOVE SPACE         TO MOD-IDDC-ENTER                            
130800       END-IF                                                             
130900                                                                          
131000****************** SÖKTA FAKTUROR                                         
131100                                                                          
131200       PERFORM UNTIL INDX = MAX-INDX                                      
131300                                                                          
131400         IF SEGMENT-FINNS                                                 
131500            IF DCS-IDDC NOT = WS-IDDC-SEND                                
131600               MOVE WS-IDDC-SEND TO W-IDDC-B6                             
131700               PERFORM IMS-GU-WDB601                                      
131800            END-IF                                                        
131900            IF TEST-DCS-IDDC NOT = 6302-IDDC-SEND                         
132000               MOVE 6302-IDDC-SEND TO W-IDDC-B6-TEST                      
132100               PERFORM IMS-GU-WDB601-TEST                                 
132200            END-IF                                                        
132300            IF (WS-IDDC-SEND = SPACE) OR                                  
132400               (WS-IDDC-SEND = 6302-IDDC-SEND) OR                         
132500               (DCS-CDC AND TEST-DCS-DDC)                                 
132600               MOVE 6302-TIFAKT   TO TMP1-YYMMDD                          
132700               MOVE TIFAKT-WS     TO TMP2-YYMMDD                          
132800               PERFORM WY2000P1                                           
132900               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
133000                  ADD +1 TO INDX                                          
133100                  MOVE 6302-IDDC-SEND    TO MOD-IDDC-SEND(INDX)           
133201                  MOVE 6302-IDDC-LEV     TO MOD-IDDC-LEV(INDX)            
133300                  MOVE 6302-IDFAKT       TO MOD-IDFAKT(INDX)              
133400                  MOVE 6302-TIFAKT       TO MOD-TIFAKT(INDX)              
133500                  IF DCS-IDDC NOT = W-IDDC                                
133600                     MOVE W-IDDC TO W-IDDC-B6                             
133700                     PERFORM IMS-GU-WDB601                                
133800                  END-IF                                                  
133900                  IF DCS-NDC-NA OR DCS-NDC-PF                             
134000                  OR DCS-CHINA OR DCS-CDC OR DCS-NDC-OTHERS               
134000                  OR DCS-NDC-SA                                           
134100                     IF MID-TIBERANK(INDX) = ALL '+'                      
134200                     OR (INDATA-SW = JA AND MFS-UPDATE)                   
134300                        MOVE 6302-DABERANK (3:6)                          
134400                                         TO MOD-TIBERANK(INDX)            
134500                        MOVE MFS-FORMATETS-ATTR TO                        
134600                                   MOD-TIBERANK-ATTR(INDX)                
134700                     END-IF                                               
134800                     IF DCS-NDC-PF OR DCS-CDC OR                          
134800                        DCS-NDC-OTHERS OR DCS-NDC-SA                      
134900                        IF MID-ADINLOMR(INDX) = ALL '+'                   
135000                        OR (INDATA-SW = JA AND MFS-UPDATE)                
135100                          MOVE 6302-ADINLOMR TO MOD-ADINLOMR(INDX)        
135200                          MOVE MFS-FORMATETS-ATTR   TO                    
135300                                   MOD-ADINLOMR-ATTR(INDX)                
135400                        END-IF                                            
135500                     ELSE                                                 
135600                        MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR(INDX)        
135700                     END-IF                                               
135800                  ELSE                                                    
135900                     MOVE MFS-RENSA-FAELT   TO MOD-ADINLOMR(INDX)         
136000                                               MOD-TIBERANK(INDX)         
136100                  END-IF                                                  
                        IF MFS-SPLIT                                            
                         MOVE 6302-FLMANETA TO MOD-ADINLOMR(INDX)               
                         MOVE MFS-CLOSE-FIELD TO                                
                              MOD-ADINLOMR-ATTR(INDX)                           
                         IF 6302-DABERANK-PROP = 0                              
                          MOVE ZEROS TO                                         
                                   MOD-IDLBBET-KDTRPSTA(INDX)(1:1)              
                         ELSE                                                   
                          MOVE 6302-DABERANK-PROP TO WS-DABERANK-PROP           
                          MOVE WS-DABERANK-PROP (3:6) TO                        
                                   MOD-IDLBBET-KDTRPSTA(INDX)(1:6)              
                         END-IF                                                 
                         IF  6302-DABERANK-DISCH = 0                            
                          MOVE 6302-DABERANK-DISCH TO                           
                                   MOD-IDLBBET-KDTRPSTA(INDX)(8:1)              
                         ELSE                                                   
                          MOVE 6302-DABERANK-DISCH TO WS-DABERANK-DISCH         
                          MOVE WS-DABERANK-DISCH (3:6) TO                       
                                   MOD-IDLBBET-KDTRPSTA(INDX)(8:6)              
                         END-IF                                                 
                        ELSE                                                    
                         MOVE 6302-ADINLOMR TO MOD-ADINLOMR(INDX)               
                         MOVE MFS-FORMATETS-ATTR TO                             
                                       MOD-ADINLOMR-ATTR(INDX)                  
136200                   MOVE 6302-IDLBBET TO                                   
                                       MOD-IDLBBET-KDTRPSTA(INDX)(1:12)         
136300                   MOVE 6302-KDTRPSTA TO                                  
                                       MOD-IDLBBET-KDTRPSTA(INDX)(13:1)         
                        END-IF                                                  
136400                  MOVE 6302-KVKOLLI-FAKT TO MOD-KVKOLLI-FAKT(INDX)        
136500                  MOVE 6302-KVKOLLI-MOT  TO MOD-KVKOLLI-MOT(INDX)         
136600                  MOVE 6302-KVRADER-MOT  TO MOD-KVRADER-MOT(INDX)         
136700                  MOVE 6302-KVRADER-FAKT TO MOD-KVRADER-FAKT(INDX)        
136800                  MOVE 6302-KVRADER-PRIO TO MOD-KVRADER-PRIO(INDX)        
136900               END-IF                                                     
137000            END-IF                                                        
137100            PERFORM FB-LAES-RADDATA                                       
137200         ELSE                                                             
137300            ADD +1 TO INDX                                                
137400            MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR     (INDX)              
137500                                     MOD-TIBERANK-ATTR(INDX)              
137600                                     MOD-ADINLOMR-ATTR(INDX)              
137700            MOVE MFS-RENSA-FAELT  TO MOD-CMD-IN       (INDX)              
137800                                     MOD-IDDC-SEND    (INDX)              
137901                                     MOD-IDDC-LEV     (INDX)              
138000                                     MOD-IDFAKT       (INDX)              
138100                                     MOD-TIFAKT       (INDX)              
138200                                     MOD-TIBERANK     (INDX)              
138300                                     MOD-ADINLOMR     (INDX)              
138400                                  MOD-IDLBBET-KDTRPSTA(INDX)              
138600                                     MOD-KVKOLLI-FAKT (INDX)              
138700                                     MOD-KVKOLLI-MOT  (INDX)              
138800                                     MOD-KVRADER-FAKT (INDX)              
138900                                     MOD-KVRADER-PRIO (INDX)              
139000         END-IF                                                           
139100                                                                          
139200       END-PERFORM                                                        
139300                                                                          
139400****************** NÄSTA SÖKTA                                            
139500                                                                          
139600       MOVE NEJ TO SW-TRAEFF                                              
139700       IF  SEGMENT-FINNS                                                  
139800          PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = JA                  
139900             IF WS-IDDC-SEND = SPACE                                      
140000                MOVE 6302-TIFAKT   TO TMP1-YYMMDD                         
140100                MOVE TIFAKT-WS     TO TMP2-YYMMDD                         
140200                PERFORM WY2000P1                                          
140300                IF TMP1-YYMMDD >= TMP2-YYMMDD                             
140400                   MOVE 6302-DABERANK (3:6) TO MOD-TIBERANK-NEXT          
140500                   MOVE 6302-IDFAKT   TO MOD-IDFAKT-NEXT                  
140600                   MOVE JA TO SW-TRAEFF                                   
140700                END-IF                                                    
140800             ELSE                                                         
140900                MOVE 6302-TIFAKT   TO TMP1-YYMMDD                         
141000                MOVE TIFAKT-WS     TO TMP2-YYMMDD                         
141100                PERFORM WY2000P1                                          
141200                IF (WS-IDDC-SEND = 6302-IDDC-SEND) AND                    
141300                   (TMP1-YYMMDD >= TMP2-YYMMDD)                           
141400                   MOVE 6302-DABERANK (3:6) TO MOD-TIBERANK-NEXT          
141500                   MOVE 6302-IDFAKT   TO MOD-IDFAKT-NEXT                  
141600                   MOVE JA TO SW-TRAEFF                                   
141700                ELSE                                                      
141800                   IF DCS-IDDC NOT = WS-IDDC-SEND                         
141900                      MOVE WS-IDDC-SEND TO W-IDDC-B6                      
142000                      PERFORM IMS-GU-WDB601                               
142100                   END-IF                                                 
142200                   IF TEST-DCS-IDDC NOT = 6302-IDDC-SEND                  
142300                      MOVE 6302-IDDC-SEND TO W-IDDC-B6-TEST               
142400                      PERFORM IMS-GU-WDB601-TEST                          
142500                   END-IF                                                 
142600                   IF (DCS-CDC AND TEST-DCS-DDC) AND                      
142700                   (TMP1-YYMMDD >= TMP2-YYMMDD)                           
142800                      MOVE 6302-DABERANK (3:6)                            
142900                           TO MOD-TIBERANK-ENTER                          
143000                      MOVE 6302-IDFAKT   TO MOD-IDFAKT-ENTER              
143100                      MOVE JA TO SW-TRAEFF                                
143200                   END-IF                                                 
143300                END-IF                                                    
143400             END-IF                                                       
143500             IF SW-TRAEFF = NEJ                                           
143600                PERFORM FB-LAES-RADDATA                                   
143700             END-IF                                                       
143800          END-PERFORM                                                     
143900                                                                          
144000          IF SW-TRAEFF = JA                                               
144100             MOVE 6302-TIFAKT   TO TMP1-YYMMDD                            
144200             MOVE TIFAKT-WS     TO TMP2-YYMMDD                            
144300             PERFORM WY2000P1                                             
144400             IF WS-IDDC-SEND = SPACE                                      
144500             AND TMP1-YYMMDD >= TMP2-YYMMDD                               
144600                MOVE 6302-DABERANK (3:6)  TO MOD-TIBERANK-NEXT            
144700                MOVE 6302-IDFAKT     TO MOD-IDFAKT-NEXT                   
144800                MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                 
144900                CALL WMEDKONV USING MED-WMEDAREA                          
145000                MOVE MED-TEMFSINF     TO MOD-TEMFSINF                     
145100             ELSE                                                         
145200                MOVE 6302-TIFAKT   TO TMP1-YYMMDD                         
145300                MOVE TIFAKT-WS     TO TMP2-YYMMDD                         
145400                PERFORM WY2000P1                                          
145500*               IF (WS-IDDC-SEND = 6302-IDDC-SEND)                        
145600***************** NY TEST                                                 
145700                IF DCS-IDDC NOT = WS-IDDC-SEND                            
145800                   MOVE WS-IDDC-SEND TO W-IDDC-B6                         
145900                   PERFORM IMS-GU-WDB601                                  
146000                END-IF                                                    
146100                IF TEST-DCS-IDDC NOT = 6302-IDDC-SEND                     
146200                   MOVE 6302-IDDC-SEND TO W-IDDC-B6-TEST                  
146300                   PERFORM IMS-GU-WDB601-TEST                             
146400                END-IF                                                    
146500                IF (DCS-CDC AND TEST-DCS-DDC)                             
146600                   AND (TMP1-YYMMDD >= TMP2-YYMMDD)                       
146700                OR (WS-IDDC-SEND = 6302-IDDC-SEND)                        
146800                   AND (TMP1-YYMMDD >= TMP2-YYMMDD)                       
146900***************** NY TEST                                                 
147000                  MOVE 6302-DABERANK (3:6)   TO MOD-TIBERANK-NEXT         
147100                  MOVE 6302-IDFAKT     TO MOD-IDFAKT-NEXT                 
147200                                                                          
147300                  MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF               
147400                  CALL WMEDKONV USING MED-WMEDAREA                        
147500                  MOVE MED-TEMFSINF     TO MOD-TEMFSINF                   
147600                END-IF                                                    
147700             END-IF                                                       
147800          ELSE                                                            
147900            MOVE ZERO             TO MOD-TIBERANK-NEXT                    
148000            MOVE ZERO             TO MOD-IDFAKT-NEXT                      
148100            MOVE SPACE            TO MOD-IDDC-NEXT                        
148200          END-IF                                                          
148300       ELSE                                                               
148400          MOVE ZERO             TO MOD-TIBERANK-NEXT                      
148500          MOVE ZERO             TO MOD-IDFAKT-NEXT                        
148600          MOVE SPACE            TO MOD-IDDC-NEXT                          
148700       END-IF                                                             
148800     END-IF                                                               
148900     .                                                                    
149000     EJECT                                                                
149100 FA-LAES-GRUNDDATA SECTION.                                               
149200                                                                          
149300     MOVE '6301'          TO W-6301-IDHTYP                                
149400     MOVE W-IDDC          TO W-6301-IDDC                                  
149500     MOVE LOW-VALUE       TO W-6301-LOWVALUE                              
149600                                                                          
149700     PERFORM IMS-GU-WL630101                                              
149800     .                                                                    
149900     EJECT                                                                
150000 FB-LAES-RADDATA SECTION.                                                 
150100                                                                          
150200     IF MFS-UPDATE                                                        
150300        IF DCS-IDDC NOT = IDDC-WS                                         
150400           MOVE IDDC-WS TO W-IDDC-B6                                      
150500           PERFORM IMS-GU-WDB601                                          
150600        END-IF                                                            
150700        IF DCS-NDC-NA OR DCS-NDC-PF OR DCS-CDC OR                         
150700           DCS-NDC-OTHERS OR DCS-NDC-SA                                   
150800           MOVE ZERO TO W-DABERANK                                        
150900        END-IF                                                            
151000     END-IF                                                               
151100     MOVE W-DABERANK TO W-6302-DABERANK                                   
151200     MOVE W-IDFAKT   TO W-6302-IDFAKT                                     
151300                                                                          
151400     IF  W-IDLBBET = SPACE                                                
151500         PERFORM IMS-GNP-WL630111-A                                       
151600     ELSE                                                                 
151700         PERFORM IMS-GNP-WL630111-B                                       
151800     END-IF                                                               
151900     .                                                                    
152000     EJECT                                                                
152100 G-KOLLA-INPUT SECTION.                                                   
152200                                                                          
152300     MOVE JA TO INDATA-SW                                                 
152400                                                                          
152500     MOVE MID-IDDC-ENTER TO W-IDDC                                        
152600                                                                          
152700     IF  MID-TIBERANK-ENTER NUMERIC                                       
152800         MOVE MID-TIBERANK-ENTER TO W-DABERANK                            
152900         IF MID-TIBERANK-ENTER NOT = ZERO                                 
153000           IF MID-TIBERANK-ENTER < 500000                                 
153100             MOVE 20             TO W-DABERANK (1:2)                      
153200           ELSE                                                           
153300             IF MID-TIBERANK-ENTER < 999999                               
153400               MOVE 19           TO W-DABERANK (1:2)                      
153500             ELSE                                                         
153600               MOVE 99999999     TO W-DABERANK                            
153700             END-IF                                                       
153800           END-IF                                                         
153900         END-IF                                                           
154000     ELSE                                                                 
154100         MOVE ZERO TO W-DABERANK                                          
154200     END-IF                                                               
154300                                                                          
154400     IF MID-IDFAKT-ENTER NUMERIC                                          
154500        MOVE MID-IDFAKT-ENTER TO W-IDFAKT                                 
154600     ELSE                                                                 
154700        MOVE ZERO TO W-IDFAKT                                             
154800     END-IF                                                               
154900                                                                          
155000     IF  (MID-CMD-IN (1) = ALL '+'  OR SPACE)                             
155100     AND (MID-CMD-IN (2) = ALL '+'  OR SPACE)                             
155200     AND (MID-CMD-IN (3) = ALL '+'  OR SPACE)                             
155300     AND (MID-CMD-IN (4) = ALL '+'  OR SPACE)                             
155400     AND (MID-CMD-IN (5) = ALL '+'  OR SPACE)                             
155500     AND (MID-CMD-IN (6) = ALL '+'  OR SPACE)                             
155600     AND (MID-CMD-IN (7) = ALL '+'  OR SPACE)                             
155700     AND (MID-CMD-IN (8) = ALL '+'  OR SPACE)                             
155800     AND (MID-CMD-IN (9) = ALL '+'  OR SPACE)                             
155900     AND (MID-CMD-IN (10) = ALL '+' OR SPACE)                             
156000     AND (MID-CMD-IN (11) = ALL '+' OR SPACE)                             
156100     AND (MID-CMD-IN (12) = ALL '+' OR SPACE)                             
156200     AND (MID-CMD-IN (13) = ALL '+' OR SPACE)                             
156300     AND (MID-TIBERANK(1) = ALL '+' OR SPACE)                             
156400     AND (MID-TIBERANK(2) = ALL '+' OR SPACE)                             
156500     AND (MID-TIBERANK(3) = ALL '+' OR SPACE)                             
156600     AND (MID-TIBERANK(4) = ALL '+' OR SPACE)                             
156700     AND (MID-TIBERANK(5) = ALL '+' OR SPACE)                             
156800     AND (MID-TIBERANK(6) = ALL '+' OR SPACE)                             
156900     AND (MID-TIBERANK(7) = ALL '+' OR SPACE)                             
157000     AND (MID-TIBERANK(8) = ALL '+' OR SPACE)                             
157100     AND (MID-TIBERANK(9) = ALL '+' OR SPACE)                             
157200     AND (MID-TIBERANK(10) = ALL '+' OR SPACE)                            
157300     AND (MID-TIBERANK(11) = ALL '+' OR SPACE)                            
157400     AND (MID-TIBERANK(12) = ALL '+' OR SPACE)                            
157500     AND (MID-TIBERANK(13) = ALL '+' OR SPACE)                            
157600     AND (MID-ADINLOMR(1)  = ALL '+' OR SPACE)                            
157700     AND (MID-ADINLOMR(2)  = ALL '+' OR SPACE)                            
157800     AND (MID-ADINLOMR(3)  = ALL '+' OR SPACE)                            
157900     AND (MID-ADINLOMR(4)  = ALL '+' OR SPACE)                            
158000     AND (MID-ADINLOMR(5)  = ALL '+' OR SPACE)                            
158100     AND (MID-ADINLOMR(6)  = ALL '+' OR SPACE)                            
158200     AND (MID-ADINLOMR(7)  = ALL '+' OR SPACE)                            
158300     AND (MID-ADINLOMR(8)  = ALL '+' OR SPACE)                            
158400     AND (MID-ADINLOMR(9)  = ALL '+' OR SPACE)                            
158500     AND (MID-ADINLOMR(10) = ALL '+' OR SPACE)                            
158600     AND (MID-ADINLOMR(11) = ALL '+' OR SPACE)                            
158700     AND (MID-ADINLOMR(12) = ALL '+' OR SPACE)                            
158800     AND (MID-ADINLOMR(13) = ALL '+' OR SPACE)                            
158900        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
159000        MOVE NEJ TO INDATA-SW                                             
159100        IF MID-ADINLOMR-PRT = ALL '+' OR SPACE                            
159200           CONTINUE                                                       
159300        ELSE                                                              
159400           MOVE MFS-ALFA-FAELT-FEL                                        
159500                                 TO MOD-ADINLOMR-PRT-ATTR                 
159600        END-IF                                                            
159700     ELSE                                                                 
159800        MOVE SPACE TO W-CMD                                               
159900        MOVE +1 TO INDX                                                   
160000        PERFORM UNTIL INDX > MAX-INDX                                     
160100                                                                          
160200           IF  MID-CMD-IN (INDX) NOT = ALL '+'                            
160300           AND MID-CMD-IN(INDX) NOT = SPACE                               
160400              IF  MID-CMD-IN (INDX) NOT = 'REC'                           
160500              AND MID-CMD-IN (INDX) NOT = 'R  '                           
160600              AND MID-CMD-IN (INDX) NOT = 'A  '                           
160700              AND MID-CMD-IN (INDX) NOT = 'I  '                           
160800              AND MID-CMD-IN (INDX) NOT = 'C  '                           
160900                  MOVE MFS-ALFA-FAELT-FEL                                 
161000                               TO MOD-CMD-ATTR (INDX)                     
161100                  MOVE ERR-CORR-HILITE-FLDS                               
161200                               TO MED-IDMFSFEL                            
161300                  MOVE NEJ TO INDATA-SW                                   
161400              ELSE                                                        
161500                 IF W-CMD = SPACE                                         
161600                    MOVE MID-CMD-IN(INDX) TO W-CMD                        
161700                    MOVE MFS-ALFA-FAELT-RAETT                             
161800                               TO MOD-CMD-ATTR (INDX)                     
161900                 ELSE                                                     
162000                    IF W-CMD = 'REC' OR 'R  '                             
162100                       IF MID-CMD-IN(INDX) = 'REC' OR 'R  '               
162200                          MOVE MFS-ALFA-FAELT-RAETT TO                    
162300                                     MOD-CMD-ATTR(INDX)                   
162400                       ELSE                                               
162500                          MOVE MFS-ALFA-FAELT-FEL                         
162600                                        TO MOD-CMD-ATTR (INDX)            
162700                          MOVE ERR-CORR-HILITE-FLDS                       
162800                                        TO MED-IDMFSFEL                   
162900                          MOVE NEJ TO INDATA-SW                           
163000                       END-IF                                             
163100                    END-IF                                                
163200                    IF W-CMD = 'I  '                                      
163300                       IF MID-CMD-IN(INDX) = 'I  '                        
163400                          MOVE MFS-ALFA-FAELT-RAETT TO                    
163500                                     MOD-CMD-ATTR(INDX)                   
163600                       ELSE                                               
163700                          MOVE MFS-ALFA-FAELT-FEL                         
163800                                         TO MOD-CMD-ATTR (INDX)           
163900                          MOVE ERR-CORR-HILITE-FLDS                       
164000                                         TO MED-IDMFSFEL                  
164100                          MOVE NEJ TO INDATA-SW                           
164200                       END-IF                                             
164300                    END-IF                                                
164400                    IF W-CMD = 'A  '                                      
164500                       IF MID-CMD-IN(INDX) = 'A  '                        
164600                          MOVE MFS-ALFA-FAELT-RAETT TO                    
164700                                     MOD-CMD-ATTR(INDX)                   
164800                       ELSE                                               
164900                          MOVE MFS-ALFA-FAELT-FEL                         
165000                                         TO MOD-CMD-ATTR (INDX)           
165100                          MOVE ERR-CORR-HILITE-FLDS                       
165200                                         TO MED-IDMFSFEL                  
165300                          MOVE NEJ TO INDATA-SW                           
165400                       END-IF                                             
165500                    END-IF                                                
165600                    IF W-CMD = 'C  '                                      
165700                       IF MID-CMD-IN(INDX) = 'C  '                        
165800                          MOVE MFS-ALFA-FAELT-RAETT TO                    
165900                                     MOD-CMD-ATTR(INDX)                   
166000                       ELSE                                               
166100                          MOVE MFS-ALFA-FAELT-FEL                         
166200                                        TO MOD-CMD-ATTR (INDX)            
166300                          MOVE ERR-CORR-HILITE-FLDS                       
166400                                        TO MED-IDMFSFEL                   
166500                          MOVE NEJ TO INDATA-SW                           
166600                       END-IF                                             
166700                    END-IF                                                
166800                 END-IF                                                   
166900                 IF MID-CMD-IN(INDX) = 'C  ' OR 'I  ' OR 'A  '            
167000                    IF DCS-IDDC NOT = W-IDDC                              
167100                       MOVE W-IDDC TO W-IDDC-B6                           
167200                       PERFORM IMS-GU-WDB601                              
167300                    END-IF                                                
167400                    IF DCS-SDC OR DCS-CDC                                 
167500                       MOVE MFS-ALFA-FAELT-FEL                            
167600                                        TO MOD-CMD-ATTR (INDX)            
167700                       MOVE ERR-CORR-HILITE-FLDS                          
167800                                        TO MED-IDMFSFEL                   
167900                       MOVE NEJ TO INDATA-SW                              
168000                    END-IF                                                
168100                 END-IF                                                   
168200              END-IF                                                      
168300           ELSE                                                           
168400              MOVE MFS-RENSA-FAELT TO MOD-CMD-IN(INDX)                    
168500           END-IF                                                         
168600                                                                          
168700           IF DCS-IDDC NOT = W-IDDC                                       
168800              MOVE W-IDDC        TO W-IDDC-B6                             
168900              PERFORM IMS-GU-WDB601                                       
169000           END-IF                                                         
169100                                                                          
169200           IF MID-ADINLOMR (INDX) = ALL '+' OR SPACE                      
169300              IF DCS-CDC AND (MID-CMD-IN(INDX) = 'REC' OR 'R  ')          
169400                 MOVE NEJ        TO INDATA-SW                             
169500                 MOVE ERR-CORR-HILITE-FLDS                                
169600                                 TO MED-IDMFSFEL                          
169700                 MOVE MFS-ALFA-FAELT-FEL                                  
169800                                 TO MOD-ADINLOMR-ATTR(INDX)               
169900              ELSE                                                        
170000                 MOVE MFS-ALFA-FAELT-RAETT                                
170100                                 TO MOD-ADINLOMR-ATTR(INDX)               
170200              END-IF                                                      
170300           ELSE                                                           
170400              IF DCS-SDC OR DCS-NDC-NA                                    
170500                 MOVE MFS-ALFA-FAELT-FEL                                  
170600                            TO MOD-ADINLOMR-ATTR(INDX)                    
170700                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
170800                 MOVE NEJ TO INDATA-SW                                    
170900              ELSE                                                        
171000                 IF DCS-CDC                                               
171100                    MOVE MID-ADINLOMR (INDX)                              
171200                                 TO W-ADINLOMR-6006                       
171300                    PERFORM IMS-GU-W6G130                                 
171400                    IF SEGMENT-SAKNAS                                     
                                                                                
171500                       MOVE NEJ  TO INDATA-SW                             
171600                       MOVE MFS-ALFA-FAELT-FEL                            
171700                                 TO MOD-ADINLOMR-ATTR(INDX)               
171800                       MOVE UPDATING-NOT-ALLOWED                          
171900                                 TO MED-IDMFSFEL                          
172000                    ELSE                                                  
172100                       MOVE MFS-ALFA-FAELT-RAETT                          
172200                                 TO MOD-ADINLOMR-ATTR(INDX)               
172300                    END-IF                                                
172400                 ELSE                                                     
172500                    IF W-CMD = 'S  ' OR 'PR ' OR 'BC '                    
172600                       MOVE MFS-ALFA-FAELT-FEL                            
172700                                 TO MOD-ADINLOMR-ATTR(INDX)               
172800                       MOVE ERR-CORR-HILITE-FLDS                          
172900                                 TO MED-IDMFSFEL                          
173000                       MOVE NEJ  TO INDATA-SW                             
173100                    ELSE                                                  
173200                       MOVE MFS-ALFA-FAELT-RAETT                          
173300                                 TO MOD-ADINLOMR-ATTR(INDX)               
173400                    END-IF                                                
173500                 END-IF                                                   
173600              END-IF                                                      
173700           END-IF                                                         
173800                                                                          
173900           IF MID-TIBERANK(INDX) = ALL '+' OR SPACE                       
174000              MOVE MFS-ALFA-FAELT-RAETT                                   
174100                            TO MOD-TIBERANK-ATTR(INDX)                    
174200           ELSE                                                           
174300              IF DCS-IDDC NOT = W-IDDC                                    
174400                 MOVE W-IDDC TO W-IDDC-B6                                 
174500                 PERFORM IMS-GU-WDB601                                    
174600              END-IF                                                      
174700              IF DCS-SDC                                                  
174800                 MOVE MFS-ALFA-FAELT-FEL                                  
174900                            TO MOD-TIBERANK-ATTR(INDX)                    
175000                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
175100                 MOVE NEJ TO INDATA-SW                                    
175200              ELSE                                                        
175300                 IF W-CMD = 'S  ' OR 'PR ' OR 'BC '                       
175400                    MOVE MFS-ALFA-FAELT-FEL                               
175500                               TO MOD-TIBERANK-ATTR(INDX)                 
175600                    MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL             
175700                    MOVE NEJ TO INDATA-SW                                 
175800                 ELSE                                                     
175900                    PERFORM GA-KOLLA-DATUM                                
176000                 END-IF                                                   
176100              END-IF                                                      
176200           END-IF                                                         
176300                                                                          
176400           ADD 1 TO INDX                                                  
176500        END-PERFORM                                                       
176600     END-IF                                                               
176700                                                                          
176800     IF  INDATA-FEL                                                       
176900        CALL WMEDKONV USING MED-WMEDAREA                                  
177000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
177100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
177200        PERFORM MFS-ROER-EJ-FAELT-IN                                      
177300     END-IF                                                               
177400                                                                          
177500     IF W-IDDC = MSGI-IDDC                                                
177600        CONTINUE                                                          
177700     ELSE                                                                 
177800        MOVE UPDATING-NOT-ALLOWED-WRONG-DC TO MOD-TEMFSFEL                
177900        MOVE NEJ TO INDATA-SW                                             
178000        PERFORM MFS-ROER-EJ-FAELT-UT                                      
178100        PERFORM MFS-ROER-EJ-FAELT-IN                                      
178200     END-IF                                                               
178300     .                                                                    
178400     EJECT                                                                
178500 GA-KOLLA-DATUM SECTION.                                                  
178600                                                                          
178700     IF MID-TIBERANK(INDX) NUMERIC                                        
178800***********                                                               
178900        MOVE 'AAMMDD'           TO DAT-KDDATFORM                          
179000        MOVE MID-TIBERANK(INDX) TO DAT-I-TIDATUM                          
179100        CALL WDATKONV    USING DAT-KDDATFORM                              
179200                               DAT-I-TIDATUM                              
179300                               DAT-O-TIDATUM                              
179400                               DAT-KDSVAR                                 
179500                                                                          
179600        IF DAT-KDSVAR NOT = SPACE                                         
179700           MOVE MFS-ALFA-FAELT-FEL   TO MOD-TIBERANK-ATTR(INDX)           
179800           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
179900           MOVE NEJ                  TO INDATA-SW                         
180000***********                                                               
180100        ELSE                                                              
180200           MOVE DAGENS-DATUM       TO TMP1-YYMMDD                         
180300           MOVE MID-TIBERANK(INDX) TO TMP2-YYMMDD                         
180400           PERFORM WY2000P1                                               
180500           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
180600              MOVE MFS-ALFA-FAELT-FEL   TO MOD-TIBERANK-ATTR(INDX)        
180700              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
180800              MOVE NEJ                  TO INDATA-SW                      
180900           ELSE                                                           
181000              MOVE MFS-ALFA-FAELT-RAETT TO MOD-TIBERANK-ATTR(INDX)        
181100           END-IF                                                         
181200*          MOVE MFS-ALFA-FAELT-RAETT TO MOD-TIBERANK-ATTR(INDX)           
181300        END-IF                                                            
181400     ELSE                                                                 
181500        MOVE MFS-ALFA-FAELT-FEL   TO MOD-TIBERANK-ATTR(INDX)              
181600        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
181700        MOVE NEJ                  TO INDATA-SW                            
181800     END-IF                                                               
181900     .                                                                    
182000     EJECT                                                                
182100 H-UPPDATERA SECTION.                                                     
182200                                                                          
182300     MOVE +1        TO INDX                                               
182400     MOVE ZERO      TO W-KVANT-UPD                                        
182500     MOVE W-IDDC    TO IDDC-WS                                            
182600                       W-6301-IDDC                                        
182700     MOVE '6301'    TO W-6301-IDHTYP                                      
182800     MOVE LOW-VALUE TO W-6301-LOWVALUE                                    
182900     PERFORM IMS-GU-WL630101                                              
183000                                                                          
183100     PERFORM UNTIL INDX  > MAX-INDX OR W-KVANT-UPD > 100                  
183200                                                                          
183300        MOVE MID-IDFAKT(INDX)   TO W-IDFAKT                               
183400                                                                          
183500        IF DCS-IDDC NOT = IDDC-WS                                         
183600           MOVE IDDC-WS TO W-IDDC-B6                                      
183700           PERFORM IMS-GU-WDB601                                          
183800        END-IF                                                            
183900        IF DCS-NDC-NA OR DCS-NDC-PF OR DCS-CDC OR                         
183900           DCS-NDC-OTHERS OR DCS-NDC-SA                                   
184000           PERFORM IMS-GNP-WL630111                                       
184100           IF SEGMENT-FINNS                                               
184201              MOVE 6302-IDDISTR TO DIST35-IDDISTR                         
184300              PERFORM S98-FIXA-LOKAL-TID                                  
184400           END-IF                                                         
184500        END-IF                                                            
184600                                                                          
184700        IF MID-CMD-IN(INDX) = 'REC' OR 'R  '                              
184800           IF MID-TIBERANK(INDX) = ALL '+' OR SPACE                       
184900              IF DCS-IDDC NOT = IDDC-WS                                   
185000                 MOVE IDDC-WS TO W-IDDC-B6                                
185100                 PERFORM IMS-GU-WDB601                                    
185200              END-IF                                                      
185300              IF DCS-NDC-NA OR DCS-NDC-PF OR DCS-CDC OR                   
185400                 DCS-NDC-OTHERS OR DCS-NDC-SA                             
185401                 IF DIST35-RETUR                                          
185501                    CONTINUE                                              
185601                 ELSE                                                     
185701                   MOVE 6302-IDKUNDNR TO ETA-IDKUNDNR                     
185801                   IF ETA-IDKUNDNR(6:2) = 1 OR 17 OR 2                    
185901                      MOVE '613' TO LETA-KDCALL                           
186001                   ELSE                                                   
186101                      MOVE '608' TO LETA-KDCALL                           
186201                   END-IF                                                 
186301                   PERFORM S97-CALL-W218ETA                               
186401                 END-IF                                                   
186500              END-IF                                                      
186600           END-IF                                                         
186700           PERFORM HA-UPPDATERA-REC                                       
186800                                                                          
186900        ELSE                                                              
187000           IF MID-CMD-IN(INDX) = 'A  ' OR 'I  ' OR 'C  '                  
187100              IF MID-TIBERANK(INDX) = ALL '+' OR SPACE                    
187301                 IF DIST35-RETUR                                          
187401                   CONTINUE                                               
187501                 ELSE                                                     
187601                   IF MID-CMD-IN(INDX) = 'C  '                            
187701                      MOVE '605'    TO LETA-KDCALL                        
187801                   ELSE                                                   
187901                      IF MID-CMD-IN(INDX) = 'A  '                         
188001                         MOVE '606' TO LETA-KDCALL                        
188101                      ELSE                                                
188201                         MOVE '607' TO LETA-KDCALL                        
188301                      END-IF                                              
188401                   END-IF                                                 
188501                   PERFORM S97-CALL-W218ETA                               
188601                 END-IF                                                   
188700              END-IF                                                      
188800              PERFORM HB-UPPDATERA-STATUS-ETA-LOC                         
188900           ELSE                                                           
189000              IF MID-TIBERANK(INDX) = ALL '+' OR SPACE                    
189100                 CONTINUE                                                 
189200              ELSE                                                        
189300                 PERFORM HB-UPPDATERA-STATUS-ETA-LOC                      
189400              END-IF                                                      
189500              IF MID-ADINLOMR(INDX) = ALL '+' OR SPACE                    
189600                 CONTINUE                                                 
189700              ELSE                                                        
189800                 IF SW-LOC-UPPD = JA                                      
189900                    CONTINUE                                              
190000                 ELSE                                                     
190100                    PERFORM HD-UPPDATERA-LOC                              
190200                 END-IF                                                   
190300              END-IF                                                      
190400           END-IF                                                         
190500        END-IF                                                            
190600                                                                          
190700        ADD 1 TO INDX                                                     
190800     END-PERFORM                                                          
190900     .                                                                    
191000     EJECT                                                                
191100 HA-UPPDATERA-REC SECTION.                                                
191200                                                                          
191300     MOVE NEJ TO SW-REC-UPPD                                              
191400     MOVE MID-IDFAKT (INDX) TO W-SEQA-IDFAKT-MIN                          
191500                               W-SEQA-IDFAKT-MAX                          
191600     MOVE SPACE             TO W-SEQA-IDKUNDRF-MIN                        
191700     MOVE ZERO              TO W-SEQA-IDKUNDNR-MIN                        
191800                               W-SEQA-IDKOLLI-MIN                         
191900                               W-SEQA-IDARTNR-MIN                         
192000                               W-SEQA-DAINLEV-MIN                         
192100     MOVE '9999999999'      TO W-SEQA-IDKUNDRF-MAX                        
192200     MOVE 9999999           TO W-SEQA-IDKUNDNR-MAX                        
192300     MOVE 99999             TO W-SEQA-IDKOLLI-MAX                         
192400     MOVE 999999999         TO W-SEQA-IDARTNR-MAX                         
192500     MOVE 9999999999999999  TO W-SEQA-DAINLEV-MAX                         
192600     MOVE 'R30'             TO W-IDPTYP                                   
192700                                                                          
192800     PERFORM IMS-GU-WDL6A1                                                
192900                                                                          
193000     PERFORM UNTIL SEGMENT-SAKNAS OR W-KVANT-UPD > 100                    
193100                                                                          
193200        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
193300        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
193400        MOVE NEJ          TO INLEV-SW                                     
193500        PERFORM IMS-GHU-INLC11                                            
193600        IF INL-TIINLMOT > ZERO                                            
193700          MOVE JA TO INLEV-SW                                             
193800        END-IF                                                            
193900                                                                          
194000        MOVE INL-IDDISTR              TO WS-SAP-IDDISTR                   
194100        MOVE INL-IDKUNDNR             TO WS-SAP-IDKUNDNR                  
194200        MOVE INL-PRARTNTO             TO WS-SAP-PRARTNTO                  
194300        MOVE INL-KVAVIS               TO W-KVAVIS                         
194400        MOVE '310'                    TO INL-IDPTYP                       
194500        IF DCS-IDDC NOT = IDDC-WS                                         
194600           MOVE IDDC-WS TO W-IDDC-B6                                      
194700           PERFORM IMS-GU-WDB601                                          
194800        END-IF                                                            
194900        IF DCS-NDC-NA OR DCS-NDC-PF OR DCS-CDC OR                         
194910           DCS-NDC-OTHERS OR DCS-NDC-SA                                   
195000           IF MID-TIBERANK(INDX) = ALL '+' OR SPACE                       
195101              IF DIST35-RETUR                                             
195201                MOVE ZERO               TO INL-TIBERANK                   
195301              ELSE                                                        
195401                MOVE LETA-TIAAMMDD-SVAR TO INL-TIBERANK                   
195501              END-IF                                                      
195600           ELSE                                                           
195700              MOVE MID-TIBERANK(INDX) TO INL-TIBERANK                     
195800           END-IF                                                         
195900           MOVE WS-LOKALTID-NUM       TO INL-TIINLMOT                     
196000           MOVE WS-LOKALCLOCK-NUM     TO INL-TIINLMTI                     
196100           IF MID-ADINLOMR(INDX) = ALL '+' OR SPACE                       
196200              CONTINUE                                                    
196300           ELSE                                                           
196400              MOVE MID-ADINLOMR(INDX) TO INL-ADINLOMR                     
196500           END-IF                                                         
196600        ELSE                                                              
196700           MOVE DAGENS-DATUM          TO INL-TIINLMOT                     
196800           MOVE AKTUELL-TTMM          TO INL-TIINLMTI                     
196900        END-IF                                                            
197000        PERFORM IMS-REPL-INLC11                                           
197100                                                                          
197200        IF DCS-CDC                                                        
197300           PERFORM IMS-GHU-WDK611                                         
197400           IF SEGMENT-FINNS                                               
197500              ADD W-KVAVIS       TO CLAG-KVAKS-CDC                        
197600              SUBTRACT W-KVAVIS                                           
197700                               FROM CLAG-KVAKS-PAV                        
197800              MOVE CLAG-PRARTSTD TO WS-SAP-PRARTSTD                       
197900              PERFORM IMS-REPL-WDK611                                     
198000              PERFORM HC-SKAPA-SALDOLOGG                                  
198101              MOVE 6302-IDDC-SEND TO WS-IDDC                              
198200              IF (NDC-CN OR NDC-US)                                       
198301              AND AKTUELLT-LAND-SVERIGE                                   
198400              AND INLEV-NEJ                                               
198500                PERFORM IMS-GU-WDK601                                     
198600                PERFORM S98-SAP-TRANS-VCCS                                
198700              END-IF                                                      
198800           END-IF                                                         
198900        ELSE                                                              
199000           PERFORM IMS-GHU-WDK711                                         
199100           IF SEGMENT-FINNS                                               
199200              ADD W-KVAVIS        TO SLAG-KVAKS-SDC                       
199300              SUBTRACT W-KVAVIS                                           
199400                                FROM SLAG-KVAKS-PAV                       
199501              MOVE SLAG-PRAVCOST  TO WS-SAP-PRAVCOST                      
199600              PERFORM IMS-REPL-WDK711                                     
199700              PERFORM HC-SKAPA-SALDOLOGG                                  
199801              MOVE 6302-IDDC-SEND TO WS-IDDC                              
199901              MOVE 6302-IDDISTR   TO DIST35-IDDISTR                       
200001              IF CDC-SE                                                   
200101              AND AKTUELLT-LAND-USA                                       
200201              AND DIST35-NDCCN-NDCUS-REFILL                               
200301              AND INLEV-NEJ                                               
200401                PERFORM S98-SAP-TRANS-VCUS                                
200501              END-IF                                                      
200600           END-IF                                                         
200700        END-IF                                                            
200800                                                                          
200900        MOVE JA TO SW-REC-UPPD                                            
201000        ADD 1 TO W-KVANT-UPD                                              
201100        PERFORM IMS-GN-WDL6A1                                             
201200     END-PERFORM                                                          
201300                                                                          
201400     IF SEGMENT-FINNS                                                     
201500        MOVE JA TO ATERHOPP-SW                                            
201600     ELSE                                                                 
201700        IF SW-REC-UPPD = JA                                               
201800           MOVE INF-TRAILER-RECEIVED-TEXT TO MOD-TEMFSFEL                 
201900           PERFORM MFS-RENSA-FAELT-IN                                     
202000        END-IF                                                            
202100     END-IF                                                               
202200                                                                          
202300     IF SW-REC-UPPD = JA                                                  
202400        IF DCS-IDDC NOT = IDDC-WS                                         
202500           MOVE IDDC-WS TO W-IDDC-B6                                      
202600           PERFORM IMS-GU-WDB601                                          
202700        END-IF                                                            
202800        IF DCS-NDC-NA OR DCS-NDC-PF OR DCS-CDC OR                         
202800           DCS-NDC-OTHERS OR DCS-NDC-SA                                   
202900           PERFORM IMS-GNP-WL630111                                       
203001           MOVE 6302-IDDISTR TO DIST35-IDDISTR                            
203100           PERFORM IMS-DLET-WL630111                                      
203200           MOVE 'R'                TO 6302-KDTRPSTA                       
203300           IF MID-TIBERANK(INDX) = ALL '+' OR SPACE                       
203401              IF DIST35-RETUR                                             
203501                MOVE ZERO TO 6302-DABERANK                                
203601              ELSE                                                        
203701                MOVE LETA-TIAAMMDD-SVAR TO 6302-DABERANK                  
203801                IF LETA-TIAAMMDD-SVAR NOT = ZERO                          
203901                  IF LETA-TIAAMMDD-SVAR < 500000                          
204001                    MOVE 20             TO 6302-DABERANK (1:2)            
204101                  ELSE                                                    
204201                    IF LETA-TIAAMMDD-SVAR < 999999                        
204301                      MOVE 19           TO 6302-DABERANK (1:2)            
204401                    ELSE                                                  
204501                      MOVE 99999999     TO 6302-DABERANK                  
204601                    END-IF                                                
204701                  END-IF                                                  
204801                END-IF                                                    
204901              END-IF                                                      
205001           ELSE                                                           
205101              MOVE MID-TIBERANK(INDX) TO 6302-DABERANK                    
205201              IF MID-TIBERANK(INDX) NOT = ZERO                            
205301                IF MID-TIBERANK(INDX) < 500000                            
205401                  MOVE 20             TO 6302-DABERANK (1:2)              
205501                ELSE                                                      
205601                  IF MID-TIBERANK(INDX) < 999999                          
205701                    MOVE 19           TO 6302-DABERANK (1:2)              
205801                  ELSE                                                    
205901                    MOVE 99999999     TO 6302-DABERANK                    
206001                  END-IF                                                  
206101                END-IF                                                    
206201              END-IF                                                      
206301           END-IF                                                         
206401           IF MID-ADINLOMR(INDX) = ALL '+' OR SPACE                       
206501              CONTINUE                                                    
206601           ELSE                                                           
206701              MOVE MID-ADINLOMR(INDX) TO 6302-ADINLOMR                    
206801           END-IF                                                         
205102           MOVE NEJ   TO 6302-FLMANETA                                    
205103           MOVE SPACE TO 6302-IDUSER-MANETA                               
206901           PERFORM IMS-ISRT-WL630111                                      
207001        ELSE                                                              
207101           PERFORM IMS-GNP-WL630111                                       
207201           MOVE MID-CMD-IN(INDX) TO 6302-KDTRPSTA                         
207301           PERFORM IMS-REPL-WL630111                                      
207401        END-IF                                                            
207501     END-IF                                                               
207601     .                                                                    
207701     EJECT                                                                
207801 HB-UPPDATERA-STATUS-ETA-LOC SECTION.                                     
207901                                                                          
208001     MOVE NEJ TO SW-REC-UPPD                                              
208101                 SW-LOC-UPPD                                              
208201                                                                          
208301     MOVE MID-IDFAKT (INDX) TO W-SEQA-IDFAKT-MIN                          
208401                               W-SEQA-IDFAKT-MAX                          
208501     MOVE SPACE             TO W-SEQA-IDKUNDRF-MIN                        
208601     MOVE ZERO              TO W-SEQA-IDKUNDNR-MIN                        
208701                               W-SEQA-IDKOLLI-MIN                         
208801                               W-SEQA-IDARTNR-MIN                         
208901                               W-SEQA-DAINLEV-MIN                         
209001     MOVE '9999999999'      TO W-SEQA-IDKUNDRF-MAX                        
209101     MOVE 9999999           TO W-SEQA-IDKUNDNR-MAX                        
209201     MOVE 99999             TO W-SEQA-IDKOLLI-MAX                         
209301     MOVE 999999999         TO W-SEQA-IDARTNR-MAX                         
209401     MOVE 9999999999999999  TO W-SEQA-DAINLEV-MAX                         
209501     MOVE 'R30'             TO W-IDPTYP                                   
209601                                                                          
209701     PERFORM IMS-GN-WDL6A1                                                
209801                                                                          
209901     PERFORM UNTIL SEGMENT-SAKNAS OR W-KVANT-UPD > 100                    
210001        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
210101        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
210201                                                                          
210301        PERFORM IMS-GHU-INLC11                                            
210401        IF MID-TIBERANK(INDX) = ALL '+' OR SPACE                          
210501           IF DIST35-RETUR                                                
210601             IF INL-TIBERANK = ZERO                                       
210701                CONTINUE                                                  
210801             ELSE                                                         
210901                MOVE ZERO TO INL-TIBERANK                                 
211001                ADD 1 TO W-KVANT-UPD                                      
211101             END-IF                                                       
211301           ELSE                                                           
211401             IF LETA-TIAAMMDD-SVAR = INL-TIBERANK                         
211501                CONTINUE                                                  
211601             ELSE                                                         
211701                MOVE LETA-TIAAMMDD-SVAR TO INL-TIBERANK                   
211801                ADD 1 TO W-KVANT-UPD                                      
211901             END-IF                                                       
212001           END-IF                                                         
212101        ELSE                                                              
212201           MOVE MID-TIBERANK(INDX) TO INL-TIBERANK                        
212301           IF MID-TIBERANK(INDX) = INL-TIBERANK                           
212401              CONTINUE                                                    
212501           ELSE                                                           
212601              MOVE MID-TIBERANK(INDX) TO INL-TIBERANK                     
212701              ADD 1 TO W-KVANT-UPD                                        
212801           END-IF                                                         
212901        END-IF                                                            
213001        IF MID-ADINLOMR(INDX) = ALL '+' OR SPACE                          
213101           CONTINUE                                                       
213201        ELSE                                                              
213301           IF MID-ADINLOMR(INDX) = INL-ADINLOMR                           
213401              CONTINUE                                                    
213501           ELSE                                                           
213601              MOVE MID-ADINLOMR(INDX) TO INL-ADINLOMR                     
213701              ADD 1 TO W-KVANT-UPD                                        
213801              MOVE JA TO SW-LOC-UPPD                                      
213901           END-IF                                                         
214001        END-IF                                                            
214101                                                                          
214201        PERFORM IMS-REPL-INLC11                                           
214301        MOVE JA TO SW-REC-UPPD                                            
214401                                                                          
214501        PERFORM IMS-GN-WDL6A1                                             
214601     END-PERFORM                                                          
214701                                                                          
214801     IF  SEGMENT-FINNS                                                    
214901        MOVE JA TO ATERHOPP-SW                                            
215001     ELSE                                                                 
215101        IF SW-REC-UPPD = JA                                               
215201           MOVE INF-UPDATE-DONE TO MED-IDMFSFEL                           
215301           CALL WMEDKONV USING MED-WMEDAREA                               
215401           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
215501           PERFORM MFS-RENSA-FAELT-IN                                     
215601        END-IF                                                            
215701     END-IF                                                               
215801                                                                          
215901     IF SW-REC-UPPD = JA                                                  
216001        PERFORM IMS-GNP-WL630111                                          
216101        MOVE 6302-IDDISTR TO DIST35-IDDISTR                               
216201        PERFORM IMS-DLET-WL630111                                         
216301        IF MID-CMD-IN(INDX) = 'A' OR 'I' OR 'C'                           
216401           MOVE MID-CMD-IN(INDX)    TO 6302-KDTRPSTA                      
216501        END-IF                                                            
216601        IF MID-TIBERANK(INDX) = ALL '+' OR SPACE                          
216701            IF DIST35-RETUR                                               
216901              MOVE ZERO TO 6302-DABERANK                                  
217001            ELSE                                                          
217101              MOVE LETA-TIAAMMDD-SVAR TO 6302-DABERANK                    
217201              IF LETA-TIAAMMDD-SVAR NOT = ZERO                            
217301                IF LETA-TIAAMMDD-SVAR < 500000                            
217401                  MOVE 20             TO 6302-DABERANK (1:2)              
217501                ELSE                                                      
217601                  IF LETA-TIAAMMDD-SVAR < 999999                          
217701                    MOVE 19           TO 6302-DABERANK (1:2)              
217801                  ELSE                                                    
217901                    MOVE 99999999     TO 6302-DABERANK                    
218001                  END-IF                                                  
218101                END-IF                                                    
218201              END-IF                                                      
218301            END-IF                                                        
218401        ELSE                                                              
218501            MOVE MID-TIBERANK(INDX) TO 6302-DABERANK                      
218601            IF MID-TIBERANK(INDX) NOT = ZERO                              
218701              IF MID-TIBERANK(INDX) < 500000                              
218801                MOVE 20             TO 6302-DABERANK (1:2)                
218901              ELSE                                                        
219001                IF MID-TIBERANK(INDX) < 999999                            
219101                  MOVE 19           TO 6302-DABERANK (1:2)                
219201                ELSE                                                      
219301                  MOVE 99999999     TO 6302-DABERANK                      
219401                END-IF                                                    
219501              END-IF                                                      
219601            END-IF                                                        
219701        END-IF                                                            
219801        IF MID-ADINLOMR(INDX) = ALL '+' OR SPACE                          
219901           CONTINUE                                                       
220001        ELSE                                                              
220101           MOVE MID-ADINLOMR(INDX) TO 6302-ADINLOMR                       
220201        END-IF                                                            
216301        IF MID-CMD-IN(INDX) = 'A' OR 'I' OR 'C'                           
218502          MOVE NEJ   TO 6302-FLMANETA                                     
218503          MOVE SPACE TO 6302-IDUSER-MANETA                                
              ELSE                                                              
218502          MOVE YES   TO 6302-FLMANETA                                     
218503          MOVE MSG-SIGNON-USERID TO 6302-IDUSER-MANETA                    
              END-IF                                                            
220301        PERFORM IMS-ISRT-WL630111                                         
220401     END-IF                                                               
220501     .                                                                    
220601     EJECT                                                                
220701 HC-SKAPA-SALDOLOGG SECTION.                                              
220801     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
220901     MOVE 9                         TO LOGG-IDSEKVNR                      
221001     MOVE W-IDDC                    TO LOGG-IDDC                          
221101     MOVE 'INBO'                    TO LOGG-IDHUVTYP                      
221201     MOVE '310'                     TO LOGG-IDSUBTYP                      
221301     MOVE 'W6030100'                TO LOGG-IDPGM                         
221401     MOVE '6301'                    TO LOGG-IDTRANS                       
221501     MOVE MSG-SIGNON-USERID         TO LOGG-IDUSER                        
221601     MOVE SPACE                     TO LOGG-REF                           
221701     MOVE W-IDFAKT                  TO LOGG-IDFAKT                        
221801     MOVE MID-IDLBBET-IN            TO LOGG-IDLBBET                       
221901     MOVE WS-TIFAKT                 TO LOGG-TIFAKT                        
222001     MOVE '+'                       TO LOGG-IDTECKEN-KVAKS                
222101     MOVE '-'                       TO LOGG-IDTECKEN-KVAKS-PAV            
222201     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
222301     MOVE SPACE                     TO LOGG-IDTECKEN-KVLS                 
222401     MOVE W-KVAVIS                  TO LOGG-KVART-SALDO                   
222801     IF DCS-CDC                                                           
222901        COMPUTE LOGG-KVAKS          =  CLAG-KVAKS-CDC                     
223001                                    +  CLAG-KVAKS-T                       
223101        MOVE CLAG-KVAKS-PAV         TO LOGG-KVAKS-PAV                     
223201        MOVE CLAG-KVEFRS            TO LOGG-KVEFRS                        
223301        MOVE CLAG-KVLS              TO LOGG-KVLS                          
223401     ELSE                                                                 
223501        MOVE SLAG-KVAKS-SDC         TO LOGG-KVAKS                         
223601        MOVE SLAG-KVAKS-PAV         TO LOGG-KVAKS-PAV                     
223701        MOVE SLAG-KVEFRS            TO LOGG-KVEFRS                        
223801        MOVE SLAG-KVLS              TO LOGG-KVLS                          
223901     END-IF                                                               
224001     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
224101     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
224201     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
224301     ACCEPT WLOGG-TID FROM TIME                                           
224401     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
224501                                                                          
224601     PERFORM IMS-ISRT-WDL901                                              
224701     IF SEGMENT-FINNS-REDAN                                               
224801       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
224901          ADD -1 TO LOGG-IDSEKVNR                                         
225001          PERFORM IMS-ISRT-WDL901                                         
225101       END-PERFORM                                                        
225201     END-IF                                                               
225301     .                                                                    
225401     EJECT                                                                
225501 HD-UPPDATERA-LOC SECTION.                                                
225601                                                                          
225701     MOVE NEJ TO SW-REC-UPPD                                              
225801                                                                          
225901     MOVE MID-IDFAKT (INDX) TO W-SEQA-IDFAKT-MIN                          
226001                               W-SEQA-IDFAKT-MAX                          
226101     MOVE SPACE             TO W-SEQA-IDKUNDRF-MIN                        
226201     MOVE ZERO              TO W-SEQA-IDKUNDNR-MIN                        
226301                               W-SEQA-IDKOLLI-MIN                         
226401                               W-SEQA-IDARTNR-MIN                         
226501                               W-SEQA-DAINLEV-MIN                         
226601     MOVE '9999999999'      TO W-SEQA-IDKUNDRF-MAX                        
226701     MOVE 9999999           TO W-SEQA-IDKUNDNR-MAX                        
226801     MOVE 99999             TO W-SEQA-IDKOLLI-MAX                         
226901     MOVE 999999999         TO W-SEQA-IDARTNR-MAX                         
227001     MOVE 9999999999999999  TO W-SEQA-DAINLEV-MAX                         
227101                                                                          
227201     PERFORM IMS-GN-WDL6A1-R30-310                                        
227301                                                                          
227401     PERFORM UNTIL SEGMENT-SAKNAS OR W-KVANT-UPD > 100                    
227501        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
227601        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
227701                                                                          
227801        PERFORM IMS-GHU-INLC11                                            
227901        IF MID-ADINLOMR(INDX) = ALL '+' OR SPACE                          
228001           CONTINUE                                                       
228101        ELSE                                                              
228201           IF MID-ADINLOMR(INDX) = INL-ADINLOMR                           
228301              CONTINUE                                                    
228401           ELSE                                                           
228501              MOVE MID-ADINLOMR(INDX) TO INL-ADINLOMR                     
228601              ADD 1 TO W-KVANT-UPD                                        
228701           END-IF                                                         
228801        END-IF                                                            
228901                                                                          
229001        PERFORM IMS-REPL-INLC11                                           
229101        MOVE JA TO SW-REC-UPPD                                            
229201                                                                          
229301        PERFORM IMS-GN-WDL6A1-R30-310                                     
229401     END-PERFORM                                                          
229501                                                                          
229601     IF  SEGMENT-FINNS                                                    
229701        MOVE JA TO ATERHOPP-SW                                            
229801     ELSE                                                                 
229901        IF SW-REC-UPPD = JA                                               
230001           MOVE INF-UPDATE-DONE TO MED-IDMFSFEL                           
230101           CALL WMEDKONV USING MED-WMEDAREA                               
230201           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
230301           PERFORM MFS-RENSA-FAELT-IN                                     
230401        END-IF                                                            
230501     END-IF                                                               
230601                                                                          
230701     IF SW-REC-UPPD = JA                                                  
230801        PERFORM IMS-GNP-WL630111                                          
230901        MOVE MID-ADINLOMR(INDX) TO 6302-ADINLOMR                          
231001        PERFORM IMS-REPL-WL630111                                         
231101     END-IF                                                               
231201     .                                                                    
231301     EJECT                                                                
231401 I-EDIT-MID-W6I30201 SECTION.                                             
231501                                                                          
231601     MOVE LOW-VALUE TO 6302-MID-W6I30201                                  
231701     MOVE +1        TO INDX                                               
231801                                                                          
231901     PERFORM UNTIL INDX > MAX-INDX                                        
232001         IF  MID-CMD-IN (INDX) = 'S  '                                    
232101         OR  MID-CMD-IN (INDX) = 'X  '                                    
232201             MOVE MID-IDFAKT (INDX) TO 6302-MID-IDFAKT-IN                 
232301             INSPECT 6302-MID-IDFAKT-IN REPLACING LEADING                 
232401                                        SPACE BY ZERO                     
232501         END-IF                                                           
232601         ADD 1 TO INDX                                                    
232701     END-PERFORM                                                          
232801     .                                                                    
232901     EJECT                                                                
233001 S97-CALL-W218ETA SECTION.                                                
233101                                                                          
233201     CALL W218ETA USING LETA-W218LETA                                     
233301                         ARTC-PCB                                         
233401                         WDK7-PCB                                         
233501                         INLC-PCB                                         
233601                         LEVA-PCB                                         
233701                         WDB6-PCB                                         
233801                                                                          
233901     IF LETA-SVAR-OK = 'F' OR 'N'                                         
234001        MOVE 'FEL RETURKOD FRÅN ETA' TO FELTEXT                           
234101        DISPLAY FELTEXT                                                   
234201        CALL FELLOG                                                       
234301     END-IF                                                               
234401     .                                                                    
234501     EJECT                                                                
234601 S98-FIXA-LOKAL-TID SECTION.                                              
234701                                                                          
234801     MOVE '011'            TO MSGI-KDCALL                                 
234901     MOVE IDDC-WS          TO WS-IDDC-TID                                 
235001     MOVE WS-IDDC-KOLL     TO MSGI-IDUSER                                 
235101     MOVE DAGENS-DATUM     TO MSGI-TILOKDAT                               
235201     MOVE AKTUELL-TTMM     TO MSGI-TILOKTID                               
235301                                                                          
235401     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
235501     IF MSGI-KDSVAR = 'F'                                                 
235601        MOVE 'FEL RETURKOD FRÅN WMSGINIT' TO FELTEXT                      
235701        DISPLAY FELTEXT                                                   
235801        CALL FELLOG                                                       
235901     END-IF                                                               
236001                                                                          
236101     IF 6302-IDDC-LEV NOT = SPACE                                         
236201       MOVE 6302-IDDC-LEV  TO LETA-IDDC-SEND                              
236301     ELSE                                                                 
236401       MOVE 6302-IDDC-SEND TO LETA-IDDC-SEND                              
236501     END-IF                                                               
236601     MOVE W-IDDC         TO LETA-IDDC-REC                                 
236701     MOVE ZERO           TO LETA-KDFRAKT                                  
236801     MOVE MSGI-TILOKDAT  TO LETA-TIAAMMDD-ANROP                           
236901                            WS-LOKALTID                                   
237001                            WS-LOKALTID-NUM                               
237101     IF WS-LOKAL-SEKEL = '9'                                              
237201        MOVE 19          TO LETA-TISEKEL-ANROP                            
237301     ELSE                                                                 
237401        MOVE 20          TO LETA-TISEKEL-ANROP                            
237501     END-IF                                                               
237601     MOVE MSGI-TILOKTID TO WS-LOKALCLOCK-NUM                              
237701     .                                                                    
237801     EJECT                                                                
237901                                                                          
238001 S98-SAP-TRANS-VCCS SECTION.                                              
238101***** MAPPING OF SAP TRANSACTIONS TO CDC                                  
238201                                                                          
238301     MOVE 'W6030100'                  TO FIL-IDPGM                        
238401     MOVE FUNCTION CURRENT-DATE(1:8)  TO FIL-DAREGDAT                     
238501     ACCEPT FIL-TIKLOCK               FROM TIME                           
238601     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
238701     MOVE WS-SAP-AAAAMMDD             TO EKH-DAVERDAT                     
238801     ADD +1                           TO W-IDSEKVNR-SAP                   
238901     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
239001     MOVE W-IDSEKVNR-SAP              TO FIL-IDSEKVNR                     
239101     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
239201     MOVE '102'                       TO EKH-KDEKHHT                      
239301     MOVE '121'                       TO EKH-KDEKSHT                      
239401     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
239501     MOVE 6302-IDDC-SEND              TO EKH-IDDC-SEND                    
239601     MOVE WS-IDDC-REC                 TO EKH-IDDC-REC                     
239701     MOVE WS-SAP-IDDISTR              TO EKH-IDDISTR                      
239801     MOVE WS-SAP-IDKUNDNR             TO EKH-IDKUNDNR                     
239901*******************************                                           
240001     MOVE ZERO TO NOLL-RAKNARE                                            
240101     MOVE W-IDFAKT                   TO WS-SAP-IDFAKT                     
240201     MOVE WS-SAP-IDFAKT              TO WS-SAP-X-IDFAKT                   
240301     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
240401          FOR LEADING ZERO                                                
240501     ADD +1 TO NOLL-RAKNARE                                               
240601     UNSTRING WS-SAP-X-IDFAKT         INTO EKH-IDVERGL                    
240701          WITH POINTER NOLL-RAKNARE                                       
240801     MOVE ZERO                        TO EKH-KDPSLLOC                     
240901     MOVE WS-SAP-PRARTNTO             TO EKH-PRARTNTO                     
241001     MOVE WS-SAP-PRARTSTD             TO EKH-PRARTSTD                     
241101     MOVE ART-KDPRODSL                TO EKH-KDPRODSL                     
241201     MOVE ZERO                        TO EKH-PRARTSJK                     
241301                                         EKH-PRHEMTAG                     
241401                                         EKH-PRINK                        
241501                                         EKH-PRDIRLON                     
241601                                         EKH-PRDMTRL                      
241701                                         EKH-PROVRPAL                     
241801                                         EKH-SUBEL                        
241901     MOVE W-IDARTNR                   TO EKH-IDARTNR                      
242001     MOVE SPACE                       TO EKH-FLLSBOK                      
242101     MOVE 'SEK'                       TO EKH-KDVALISO                     
242201********** EV ÄNDRING FÖR PRKURS                                          
242301     MOVE 1.00                        TO EKH-PRKURS                       
242401**********                                                                
242501     MOVE W-KVAVIS                    TO EKH-KVANTAL                      
242601     MOVE '6301'                      TO EKH-IDTRANS                      
242701     MOVE ZERO                        TO EKH-BEVAT                        
242801                                         EKH-IDANALYS                     
242901                                         EKH-IDKONTO                      
243001                                         EKH-KDANMORS                     
243101                                         EKH-SUVAT                        
243201                                         EKH-KDFRAKT                      
243301                                         EKH-PRLANDCO                     
243401                                         EKH-DAAVIDAT                     
243501                                         EKH-IDAVINR                      
243601                                         EKH-KDAVVTYP                     
243701                                         EKH-KDRT                         
243801                                         EKH-KVANTMOT                     
243901                                         EKH-KVAVIS                       
244001     MOVE SPACE                      TO  EKH-KDSORT                       
244101                                         EKH-IDKST                        
244201     MOVE SPACE                      TO  EKH-IDLEVNR                      
244301     MOVE 'SEPV'                     TO  EKH-KDTRADP                      
244401     MOVE SPACE                      TO  EKH-FLDCET                       
244501     MOVE SPACE                      TO  EKH-IDKUNDRF                     
244601     MOVE SPACE                      TO  EKH-IDFAKT-EXP                   
244701                                                                          
244801     PERFORM IMS-ISRT-WDR9                                                
244901                                                                          
245001     IF SEGMENT-FINNS-REDAN                                               
245101       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
245201          ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                            
245301          PERFORM IMS-ISRT-WDR9                                           
245401       END-PERFORM                                                        
245501     END-IF                                                               
245601     .                                                                    
245701     EJECT                                                                
245801                                                                          
245901 S98-SAP-TRANS-VCUS SECTION.                                              
246001***** MAPPING OF SAP TRANSACTIONS TO USA                                  
246101                                                                          
246201     MOVE 'W6030100'                  TO EKO-FIL-IDPGM                    
246301     MOVE FUNCTION CURRENT-DATE(1:8)  TO EKO-FIL-TIREGDAT                 
246401     ACCEPT EKO-FIL-TIKLOCK         FROM TIME                             
246501     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
246601     MOVE WS-SAP-AAAAMMDD             TO EKO-EKH-DAVERDAT                 
246701     ADD +1                           TO W-IDSEKVNR-SAP                   
246801     MOVE 'W561EKHA'                  TO EKO-FIL-IDCPYTXT                 
246901     MOVE W-IDSEKVNR-SAP              TO EKO-FIL-IDSEKVNR                 
247001     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
247101     MOVE '131'                       TO EKO-EKH-KDEKSHT                  
247201     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
247301     MOVE 6302-IDDC-SEND              TO EKO-EKH-IDDC-SEND                
247401     MOVE WS-IDDC-REC                 TO EKO-EKH-IDDC-REC                 
247501     MOVE WS-SAP-IDDISTR              TO EKO-EKH-IDDISTR                  
247601     MOVE WS-SAP-IDKUNDNR             TO EKO-EKH-IDKUNDNR                 
247701*******************************                                           
247801     MOVE ZERO TO NOLL-RAKNARE                                            
247901     MOVE W-IDFAKT                    TO WS-SAP-IDFAKT                    
248001     MOVE WS-SAP-IDFAKT               TO WS-SAP-X-IDFAKT                  
248101     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
248201          FOR LEADING ZERO                                                
248301     ADD +1 TO NOLL-RAKNARE                                               
248401     UNSTRING WS-SAP-X-IDFAKT       INTO EKO-EKH-IDVERGL                  
248501          WITH POINTER NOLL-RAKNARE                                       
248601     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
248701     MOVE WS-SAP-PRARTNTO             TO EKO-EKH-PRARTNTO                 
248801     MOVE WS-SAP-PRAVCOST             TO EKO-EKH-PRARTSTD                 
248901     MOVE ZERO                        TO EKO-EKH-KDPRODSL                 
249001                                         EKO-EKH-PRARTSJK                 
249101                                         EKO-EKH-PRHEMTAG                 
249201                                         EKO-EKH-PRINK                    
249301                                         EKO-EKH-PRDIRLON                 
249401                                         EKO-EKH-PRDMTRL                  
249501                                         EKO-EKH-PROVRPAL                 
249601                                         EKO-EKH-SUBEL                    
249701     MOVE W-IDARTNR                   TO EKO-EKH-IDARTNR                  
249801     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
249901     MOVE 'USD'                       TO EKO-EKH-KDVALISO                 
250001********** EV ÄNDRING FÖR PRKURS                                          
250101     MOVE 1.00                        TO EKO-EKH-PRKURS                   
250201**********                                                                
250301     MOVE W-KVAVIS                    TO EKO-EKH-KVANTAL                  
250401     MOVE '6301'                      TO EKO-EKH-IDTRANS                  
250501     MOVE INL-KDFRAKT                 TO EKO-EKH-KDFRAKT                  
250601     MOVE ZERO                        TO EKO-EKH-BEVAT                    
250701                                         EKO-EKH-IDANALYS                 
250801                                         EKO-EKH-IDKONTO                  
250901                                         EKO-EKH-KDANMORS                 
251001                                         EKO-EKH-SUVAT                    
251101                                         EKO-EKH-PRLANDCO                 
251201                                         EKO-EKH-DAAVIDAT                 
251301                                         EKO-EKH-IDAVINR                  
251401                                         EKO-EKH-KDAVVTYP                 
251501                                         EKO-EKH-KDRT                     
251601                                         EKO-EKH-KVANTMOT                 
251701                                         EKO-EKH-KVAVIS                   
251801     MOVE SPACE                      TO  EKO-EKH-KDSORT                   
251901                                         EKO-EKH-IDKST                    
252001     MOVE SPACE                      TO  EKO-EKH-IDLEVNR                  
252101     MOVE 'US01'                     TO  EKO-EKH-KDTRADP                  
252201     MOVE SPACE                      TO  EKO-EKH-FLDCET                   
252301     MOVE SPACE                      TO  EKO-EKH-IDKUNDRF                 
252401     MOVE SPACE                      TO  EKO-EKH-IDFAKT-EXP               
252501                                                                          
252601     PERFORM IMS-ISRT-WDR8                                                
252701                                                                          
252801     IF SEGMENT-FINNS-REDAN                                               
252901       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
253001          ADD +1 TO EKO-FIL-IDSEKVNR                                      
253101          PERFORM IMS-ISRT-WDR8                                           
253201       END-PERFORM                                                        
253301     END-IF                                                               
253401     .                                                                    
253501     EJECT                                                                
253601                                                                          
253701 MFS-RENSA-FAELT-UT SECTION.                                              
253801                                                                          
253901*    --- ALLA UTDATA-FÄLT                                                 
254001*    --- INKL. BLÄDDRINGSNYCKLAR                                          
254101     MOVE MFS-RENSA-FAELT TO MOD-TIFAKT-UT                                
254201                             MOD-IDLBBET-UT                               
254301                             MOD-IDDC-UT                                  
254401                             MOD-IDDC-SEND-UT                             
254501                             MOD-IDDC-ENTER                               
254601                             MOD-IDDC-NEXT                                
254701                             MOD-TIBERANK-ENTER                           
254801                             MOD-TIBERANK-NEXT                            
254901                             MOD-IDFAKT-ENTER                             
255001                             MOD-IDFAKT-NEXT                              
255101     .                                                                    
255201     EJECT                                                                
255301 MFS-RENSA-FAELT-IN SECTION.                                              
255401                                                                          
255501*    --- ALLA INDATA-FÄLT                                                 
255601     MOVE MFS-RENSA-FAELT TO MOD-TIFAKT-IN                                
255701                             MOD-IDLBBET-IN                               
255801                             MOD-IDDC-SEND-IN                             
255901                             MOD-IDDC-IN                                  
256001     .                                                                    
256101     EJECT                                                                
256201 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
256301                                                                          
256401*    --- ALLA UTDATA-FÄLT                                                 
256501*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
256601     MOVE MFS-ROER-EJ-FAELT TO MOD-TIFAKT-UT                              
256701                               MOD-IDLBBET-UT                             
256801                               MOD-IDDC-SEND-UT                           
256901                               MOD-IDDC-UT                                
257001                               MOD-IDDC-ENTER                             
257101                               MOD-IDDC-NEXT                              
257201                               MOD-TIBERANK-ENTER                         
257301                               MOD-TIBERANK-NEXT                          
257401                               MOD-IDFAKT-ENTER                           
257501                               MOD-IDFAKT-NEXT                            
257601                               MOD-ADINLOMR-PRT                           
257701     MOVE +1 TO INDX                                                      
257801     PERFORM UNTIL INDX > MAX-INDX                                        
257901       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
258001       ADD +1 TO INDX                                                     
258101     END-PERFORM                                                          
258201     .                                                                    
258301     EJECT                                                                
258401 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
258501                                                                          
258601*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
258701     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-IN(INDX)                           
258801                               MOD-IDDC-SEND(INDX)                        
258901                               MOD-IDDC-LEV(INDX)                         
259001                               MOD-IDFAKT(INDX)                           
259101                               MOD-TIFAKT(INDX)                           
259201                               MOD-TIBERANK(INDX)                         
259301                               MOD-ADINLOMR(INDX)                         
259401                               MOD-IDLBBET-KDTRPSTA(INDX)                 
259601                               MOD-KVKOLLI-FAKT (INDX)                    
259701                               MOD-KVKOLLI-MOT(INDX)                      
259801                               MOD-KVRADER-FAKT(INDX)                     
259901                               MOD-KVRADER-PRIO(INDX)                     
260001     .                                                                    
260101     EJECT                                                                
260201 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
260301                                                                          
260401*    --- ALLA INDATA-FÄLT                                                 
260501     MOVE MFS-ROER-EJ-FAELT TO MOD-TIFAKT-IN                              
260601                               MOD-IDLBBET-IN                             
260701                               MOD-IDDC-IN                                
260801                               MOD-IDDC-SEND-IN                           
260901                               MOD-ADINLOMR-PRT                           
261001                                                                          
261101     MOVE +1 TO INDX                                                      
261201     PERFORM UNTIL INDX > MAX-INDX                                        
261301       IF MID-TIBERANK(INDX) NOT = ALL '+'                                
261401          MOVE MFS-ROER-EJ-FAELT TO MOD-TIBERANK(INDX)                    
261501       END-IF                                                             
261601       IF MID-ADINLOMR(INDX) NOT = ALL '+'                                
261701          MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR(INDX)                    
261801       END-IF                                                             
261901       ADD +1 TO INDX                                                     
262001     END-PERFORM                                                          
262101     .                                                                    
262201     EJECT                                                                
262301* --- IMS SEKTIONER ---                                                   
262401     SKIP3                                                                
262501 IMS-GET-MSG SECTION.                                                     
262601                                                                          
262701     MOVE '  QC' TO GODK-STATUSKODER                                      
262801     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
262901     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
263001     PERFORM IMS-STATUSKONTROLL                                           
263101     .                                                                    
263201     SKIP3                                                                
263301 IMS-INSERT-MSG SECTION.                                                  
263401                                                                          
263501     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
263601     MOVE SPACE TO GODK-STATUSKODER                                       
263701     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
263801                                                                          
263901     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
264001     PERFORM IMS-STATUSKONTROLL                                           
264101     .                                                                    
264201     EJECT                                                                
264301 IMS-ISRT-ALT1-PCB-6301 SECTION.                                          
264401                                                                          
264501     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
264601     MOVE SPACE TO GODK-STATUSKODER                                       
264701     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-AREA                         
264801     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
264901     PERFORM IMS-STATUSKONTROLL                                           
265001     .                                                                    
265101     SKIP3                                                                
265201 IMS-ISRT-ALT2-PCB-6302 SECTION.                                          
265301                                                                          
265401     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
265501     MOVE SPACE TO GODK-STATUSKODER                                       
265601     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-AREA                         
265701     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
265801     PERFORM IMS-STATUSKONTROLL                                           
265901     .                                                                    
266001     SKIP3                                                                
266101 IMS-INSERT-ALTMSG SECTION.                                               
266201                                                                          
266301     MOVE SPACE TO GODK-STATUSKODER                                       
266401     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
266501     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
266601     PERFORM IMS-STATUSKONTROLL                                           
266701     .                                                                    
266801     EJECT                                                                
266901 IMS-GU-WL630101 SECTION.                                                 
267001                                                                          
267101     STRING 'WL630101(WDGXKEY = ' W-WDGXKEY-6301 ')'                      
267201          DELIMITED BY SIZE INTO SSA1                                     
267301     MOVE '  GE' TO GODK-STATUSKODER                                      
267401     CALL CBLTDLI USING GU GX63-PCB DLI-IO-AREA-6301 SSA1                 
267501                                                                          
267601     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
267701     PERFORM IMS-STATUSKONTROLL                                           
267801     .                                                                    
267901     SKIP3                                                                
268001 IMS-GNP-WL630111-A SECTION.                                              
268101                                                                          
268201     STRING 'WL630111(KEY6302 =>' W-WDGXKEY-6302 ')'                      
268301          DELIMITED BY SIZE INTO SSA1                                     
268401     MOVE '  GE' TO GODK-STATUSKODER                                      
268501     CALL CBLTDLI USING GNP GX63-PCB DLI-IO-AREA-6302 SSA1                
268601                                                                          
268701     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
268801     PERFORM IMS-STATUSKONTROLL                                           
268901     .                                                                    
269001     SKIP3                                                                
269101 IMS-GNP-WL630111-B SECTION.                                              
269201                                                                          
269301     STRING 'WL630111(KEY6302 =>' W-WDGXKEY-6302                          
269401                    '&IDLBBET = ' W-IDLBBET    ')'                        
269501          DELIMITED BY SIZE INTO SSA1                                     
269601     MOVE '  GE' TO GODK-STATUSKODER                                      
269701     CALL CBLTDLI USING GNP GX63-PCB DLI-IO-AREA-6302 SSA1                
269801                                                                          
269901     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
270001     PERFORM IMS-STATUSKONTROLL                                           
270101     .                                                                    
270201     EJECT                                                                
270301 IMS-GNP-WL630111 SECTION.                                                
270401                                                                          
270501     STRING 'WL630111*F(IDFAKT   =' W-IDFAKT-X ')'                        
270601          DELIMITED BY SIZE INTO SSA1                                     
270701     MOVE '  GE' TO GODK-STATUSKODER                                      
270801     CALL CBLTDLI USING GHNP GX63-PCB DLI-IO-AREA-6302 SSA1               
270901     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
271001     PERFORM IMS-STATUSKONTROLL                                           
271101     .                                                                    
271201     SKIP3                                                                
271301 IMS-REPL-WL630111 SECTION.                                               
271401                                                                          
271501     MOVE '  ' TO GODK-STATUSKODER                                        
271601     CALL CBLTDLI USING REPL GX63-PCB DLI-IO-AREA-6302                    
271701     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
271801     PERFORM IMS-STATUSKONTROLL                                           
271901     .                                                                    
272001     SKIP3                                                                
272101 IMS-DLET-WL630111 SECTION.                                               
272201                                                                          
272301     MOVE '  ' TO GODK-STATUSKODER                                        
272401     CALL CBLTDLI USING DLET GX63-PCB DLI-IO-AREA-6302                    
272501     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
272601     PERFORM IMS-STATUSKONTROLL                                           
272701     .                                                                    
272801     EJECT                                                                
272901 IMS-ISRT-WL630111 SECTION.                                               
273001                                                                          
273101     STRING 'WL630101(WDGXKEY = ' W-WDGXKEY-6301 ')'                      
273201          DELIMITED BY SIZE INTO SSA1                                     
273301     MOVE 'WL630111 ' TO SSA2                                             
273401     MOVE '  GE' TO GODK-STATUSKODER                                      
273501     CALL CBLTDLI USING ISRT GX63-PCB DLI-IO-AREA-6302 SSA1 SSA2          
273601     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
273701     PERFORM IMS-STATUSKONTROLL                                           
273801     .                                                                    
273901     SKIP3                                                                
274001 IMS-GN-WDL6A1  SECTION.                                                  
274101                                                                          
274201     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
274301                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
274401                    '&IDPTYP  = ' W-IDPTYP ')'                            
274501          DELIMITED BY SIZE INTO SSA1                                     
274601     MOVE '  GE' TO GODK-STATUSKODER                                      
274701     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA-WDL6A1 SSA1               
274801                                                                          
274901     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
275001     PERFORM IMS-STATUSKONTROLL                                           
275101     .                                                                    
275201     EJECT                                                                
275301 IMS-GU-WDL6A1  SECTION.                                                  
275401                                                                          
275501     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
275601                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
275701                    '&IDPTYP  = ' W-IDPTYP ')'                            
275801          DELIMITED BY SIZE INTO SSA1                                     
275901     MOVE '  GE' TO GODK-STATUSKODER                                      
276001     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA-WDL6A1 SSA1               
276101                                                                          
276201     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
276301     PERFORM IMS-STATUSKONTROLL                                           
276401     .                                                                    
276501     SKIP3                                                                
276601 IMS-GN-WDL6A1-R30-310 SECTION.                                           
276701                                                                          
276801     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
276901                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
277001          DELIMITED BY SIZE INTO SSA1                                     
277101     MOVE '  GE' TO GODK-STATUSKODER                                      
277201     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA-WDL6A1 SSA1               
277301                                                                          
277401     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
277501     PERFORM IMS-STATUSKONTROLL                                           
277601     .                                                                    
277701     EJECT                                                                
277801 IMS-GHU-WDK711   SECTION.                                                
277901                                                                          
278001     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
278101          DELIMITED BY SIZE INTO SSA1                                     
278201     STRING 'WDK711  (IDDC     =' W-IDDC  ')'                             
278301          DELIMITED BY SIZE INTO SSA2                                     
278401     MOVE '  ' TO GODK-STATUSKODER                                        
278501     CALL CBLTDLI USING GHU  WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2        
278601     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
278701     PERFORM IMS-STATUSKONTROLL                                           
278801     .                                                                    
278901     EJECT                                                                
279001 IMS-REPL-WDK711 SECTION.                                                 
279101                                                                          
279201     MOVE '  ' TO GODK-STATUSKODER                                        
279301     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK711                  
279401                                                                          
279501     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
279601     PERFORM IMS-STATUSKONTROLL                                           
279701     .                                                                    
279801     SKIP3                                                                
279901 IMS-GU-WDK601   SECTION.                                                 
280001                                                                          
280101     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
280201          DELIMITED BY SIZE INTO SSA1                                     
280301     MOVE '  ' TO GODK-STATUSKODER                                        
280401     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA-WDK601 SSA1              
280501     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
280601     PERFORM IMS-STATUSKONTROLL                                           
280701     .                                                                    
280801     EJECT                                                                
280901 IMS-GHU-WDK611   SECTION.                                                
281001                                                                          
281101     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
281201          DELIMITED BY SIZE INTO SSA1                                     
281301     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
281401          DELIMITED BY SIZE INTO SSA2                                     
281501     MOVE '  ' TO GODK-STATUSKODER                                        
281601     CALL CBLTDLI USING GHU  ARTC-PCB DLI-IO-AREA-WDK611 SSA1 SSA2        
281701     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
281801     PERFORM IMS-STATUSKONTROLL                                           
281901     .                                                                    
282001     EJECT                                                                
282101 IMS-REPL-WDK611 SECTION.                                                 
282201                                                                          
282301     MOVE '  ' TO GODK-STATUSKODER                                        
282401     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-WDK611                  
282501                                                                          
282601     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
282701     PERFORM IMS-STATUSKONTROLL                                           
282801     .                                                                    
282901     SKIP3                                                                
283001 IMS-GHU-INLC11   SECTION.                                                
283101                                                                          
283201     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
283301          DELIMITED BY SIZE INTO SSA1                                     
283401     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
283501          DELIMITED BY SIZE INTO SSA2                                     
283601     MOVE SPACE  TO GODK-STATUSKODER                                      
283701     CALL CBLTDLI USING GHU  INLC-PCB DLI-IO-AREA-WDL611 SSA1 SSA2        
283801     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
283901     PERFORM IMS-STATUSKONTROLL                                           
284001     .                                                                    
284101     SKIP3                                                                
284201 IMS-REPL-INLC11 SECTION.                                                 
284301                                                                          
284401     MOVE '  ' TO GODK-STATUSKODER                                        
284501     CALL CBLTDLI USING REPL INLC-PCB DLI-IO-AREA-WDL611                  
284601                                                                          
284701     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
284801     PERFORM IMS-STATUSKONTROLL                                           
284901     .                                                                    
285001     EJECT                                                                
285101 IMS-GHU-WL630111-GE SECTION.                                             
285201     STRING 'WL630101(WDGXKEY = ' W-WDGXKEY-6301 ')'                      
285301          DELIMITED BY SIZE INTO SSA1                                     
285401     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
285501          DELIMITED BY SIZE INTO SSA2                                     
285601     MOVE 'GE  ' TO GODK-STATUSKODER                                      
285701     CALL CBLTDLI USING GHU GX63-PCB DLI-IO-AREA-6302 SSA1 SSA2           
285801     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
285901     PERFORM IMS-STATUSKONTROLL                                           
286001     .                                                                    
286101     EJECT                                                                
286201 IMS-ISRT-WDL901 SECTION.                                                 
286301     MOVE 'WLLOGA01 ' TO SSA1                                             
286401     MOVE '  II' TO GODK-STATUSKODER                                      
286501     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
286601     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
286701     PERFORM IMS-STATUSKONTROLL                                           
286801     .                                                                    
286901     EJECT                                                                
287001                                                                          
287101 IMS-GU-WDB601    SECTION.                                                
287201     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
287301          DELIMITED BY SIZE INTO SSA1                                     
287401     MOVE '  GE' TO GODK-STATUSKODER                                      
287501     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
287601     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
287701     PERFORM IMS-STATUSKONTROLL                                           
287801     IF SEGMENT-SAKNAS                                                    
287901        MOVE SPACE TO DCS-IDDC                                            
288001                      DCS-KDDC                                            
288101     END-IF                                                               
288201     .                                                                    
288301     EJECT                                                                
288401                                                                          
288501 IMS-GU-WDB601-REC SECTION.                                               
288601     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
288701          DELIMITED BY SIZE INTO SSA1                                     
288801     MOVE '  GE' TO GODK-STATUSKODER                                      
288901     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-REC  SSA1            
289001     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
289101     PERFORM IMS-STATUSKONTROLL                                           
289201     .                                                                    
289301     SKIP3                                                                
289401                                                                          
289501 IMS-GU-WDB601-TEST    SECTION.                                           
289601     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
289701          DELIMITED BY SIZE INTO SSA1                                     
289801     MOVE '  GE' TO GODK-STATUSKODER                                      
289901     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-TEST SSA1            
290001     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
290101     PERFORM IMS-STATUSKONTROLL                                           
290201     IF SEGMENT-SAKNAS                                                    
290301        MOVE SPACE TO TEST-DCS-IDDC                                       
290401                      TEST-DCS-KDDC                                       
290501     END-IF                                                               
290601     .                                                                    
290701     EJECT                                                                
290801                                                                          
290901 IMS-GU-W6G130 SECTION.                                                   
291001                                                                          
291101     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
291201          DELIMITED BY SIZE INTO SSA1                                     
291301     STRING 'W6G130  (W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
291401          DELIMITED BY SIZE INTO SSA2                                     
291501     MOVE '  GE' TO GODK-STATUSKODER                                      
291601     CALL CBLTDLI USING GU W6G1-PCB DLI-IO-AREA-W6G130 SSA1 SSA2          
291701     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
291801     PERFORM IMS-STATUSKONTROLL                                           
291901     .                                                                    
292001     SKIP3                                                                
292101                                                                          
292201 IMS-ISRT-WDR9   SECTION.                                                 
292301     MOVE 'WDR901   ' TO SSA1                                             
292401     MOVE '  II' TO GODK-STATUSKODER                                      
292501     CALL CBLTDLI USING ISRT WDR9-PCB DLI-IO-WDR901 SSA1                  
292601     MOVE WDR9-STATUS-CODE TO STATUS-WS                                   
292701     PERFORM IMS-STATUSKONTROLL                                           
292801     .                                                                    
292901     EJECT                                                                
293001                                                                          
293101 IMS-ISRT-WDR8   SECTION.                                                 
293201     MOVE 'WDR801   ' TO SSA1                                             
293301     MOVE '  II' TO GODK-STATUSKODER                                      
293401     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
293501     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
293601     PERFORM IMS-STATUSKONTROLL                                           
293701     .                                                                    
293801     EJECT                                                                
293901                                                                          
294001 IMS-STATUSKONTROLL SECTION.                                              
294101     SET STATUS-IX TO 1                                                   
294201     SEARCH GODK-STATUS                                                   
294301       AT END                                                             
294401         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
294501         DELIMITED BY SIZE INTO FELTEXT                                   
294601         CALL FELLOG                                                      
294701       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
294801         CONTINUE                                                         
294901     END-SEARCH                                                           
295001     .                                                                    
295101     EJECT                                                                
296001*    -COPY WY2000P1                                                       
