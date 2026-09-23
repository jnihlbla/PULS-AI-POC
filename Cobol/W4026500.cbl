000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4026500.                                                
000400 AUTHOR.         ROGER OLSSON.                                            
000500 DATE-WRITTEN.   APRIL-91.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR ANNULLATION/ÄNDRING AV ORDER-                
001100*        RADER I PROFORMAREGISTER.                                        
001200*                                                                         
001300*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001400*        PROGRAMMET UPPDATERAR WLPROC (WDE8)                              
001500*        PROGRAMMET UPPDATERAR WLPROD (WDE9)                              
001600*        PROGRAMMET UPPDATERAR WLARTM (WDK9)                              
001700*        PROGRAMMET UPPDATERAR WLORQM (WDQ1)                              
001800*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001900*        PROGRAMMET LÄSER      WLARTC (WDD6)                              
002000*        PROGRAMMET LÄSER              WDB1                               
002100*        PROGRAMMET LÄSER              WDB2                               
002200*                                                                         
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T265                                              
002600*        MID:         W4I26501                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O26501                                            
003000*        TRANSAKTION: W4T265U                                             
003100*                                                                         
003200*    E-TRACKER : 7450328  2008-HÖST  VOHF                                 
003300*    E-TRACKER : 10254592 2015       DECOMISSION VOHF                     
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900*    -COPY WY2000W1                                                       
004000     SKIP3                                                                
004100 77  IDPGM                       PIC X(08)   VALUE 'W4026500'.            
004200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  YES                         PIC X       VALUE 'Y'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005000 77  MAX-INDX                    PIC S9(4)  VALUE +7    COMP SYNC.        
005100                                                                          
005200 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +736  COMP SYNC.        
005400                                                                          
005500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005600 01  WS-KLOCKAN                  PIC 9(8).                                
005700 01  FILLER REDEFINES WS-KLOCKAN.                                         
005800     03 WS-TIHHMMSS              PIC 9(6).                                
005900     03 FILLER                   PIC X(2).                                
006000 01  WS-9KOMPL-DATUM             PIC 9(8).                                
006100 01  FILLER REDEFINES WS-9KOMPL-DATUM.                                    
006200     03 WS-CENTURY               PIC 9(2).                                
006300     03 WS-AAMMDD                PIC 9(6).                                
006400 77  WS-DATUM                    PIC 9(6).                                
006500 77  WS-TIFORDAT                 PIC 9(6).                                
006600 77  WS-9KOMPL                   PIC 9(9)    VALUE 999999999.             
006700 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
006800 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
006900 77  WS-IDKUNDRF                 PIC X(7)    VALUE SPACE.                 
007000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
007100 01  WS-IDKUNDRF-RED.                                                     
007200     03 WS-IDKUNDRF-1-7          PIC X(7)    VALUE SPACE.                 
007300     03 WS-IDKUNDRF-8-10         PIC X(3)    VALUE SPACE.                 
007400                                                                          
007500 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
007600 77  MAX-IX                      PIC S9(3)  VALUE +7   COMP-3.            
008000                                                                          
008100 01 TABELL.                                                               
008200     03 PRARTNTO-TABELL OCCURS 7.                                         
008300        05 WS-PRARTNTO           PIC S9(7)V9(2) VALUE +0 COMP-3.          
008400                                                                          
008500 01 TABELL.                                                               
008600     03 KVBEART-Q-TABELL OCCURS 7.                                        
008700        05 WS-KVBEART-Q          PIC S9(7)    VALUE +0  COMP-3.           
008800                                                                          
008900 77  WS-SUORDV                   PIC S9(9)V99           COMP-3.           
009000 77  WS-SUORDV-LOC               PIC S9(9)V99           COMP-3.           
009100 77  WS-SUORDV-LOCPREL           PIC S9(9)V99           COMP-3.           
009200 77  WS-VKORDNTO                 PIC S9(6)V9            COMP-3.           
009300 77  WS-VLORDNTO                 PIC S9(4)V999          COMP-3.           
009400 77  WS-KVOFFERT                 PIC S9(7)              COMP-3.           
009500 77  WS-KVBEART-Q-URS            PIC S9(7)              COMP-3.           
009600 77  WS-PRARTNTO-URS             PIC S9(7)V99           COMP-3.           
009700 77  WS-PRARTNTO-URS-LOC         PIC S9(7)V99           COMP-3.           
009800 77  WS-PRARTNTO-URS-LOCPREL     PIC S9(7)V99           COMP-3.           
009900 77  WS-SUORDV-URS               PIC S9(9)V99           COMP-3.           
010000 77  WS-SUORDV-URS-LOC           PIC S9(9)V99           COMP-3.           
010100 77  WS-SUORDV-URS-LOCPREL       PIC S9(9)V99           COMP-3.           
010200 77  WS-SUORDV-DIFF              PIC S9(9)V99           COMP-3.           
010300 77  WS-SUORDV-DIFF-LOC          PIC S9(9)V99           COMP-3.           
010400 77  WS-SUORDV-DIFF-LOCPREL      PIC S9(9)V99           COMP-3.           
010500                                                                          
010600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010700     88  INDATA-OK                           VALUE 'J'.                   
010800     88  INDATA-FEL                          VALUE 'N'.                   
010900                                                                          
011000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
011100     88  NYCKLAR-OK                          VALUE 'J'.                   
011200     88  NYCKLAR-FEL                         VALUE 'N'.                   
011300                                                                          
011400 77  ALLT-SW                     PIC X       VALUE 'J'.                   
011500     88  ALLT-OK                             VALUE 'J'.                   
011600     88  ALLT-FEL                            VALUE 'N'.                   
011700                                                                          
011800 77  IDARTNR-SW                  PIC X       VALUE 'N'.                   
011900     88  IDARTNR-IFYLLT                      VALUE 'J'.                   
012000                                                                          
012100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012200     88  EGEN-MID                            VALUE '4265'.                
012300     88  GODK-MID                            VALUE '4262' '4264'          
012400                                                   '4265' '4266'          
012500                                                   '4267' '4268'.         
012600*    --- VALID IDDC CODES                                                 
012700*                                                                         
012800*01  -COPY WWDCKONS                                                       
012900*                                                                         
013000     EJECT                                                                
013300                                                                          
013400*    ----DISTR-DEALER-PRICE----                                           
013500*01  -COPY WWDIST79                                                       
013600     EJECT                                                                
013700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013800 01  GENERELLA-SUBPROGRAM.                                                
013900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
014100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014400*                                                                         
014500 01  GEMENSAMMA-SUBRUTINER.                                               
014600     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014900*   -COPY WMEDAREA                                                        
015000     SKIP3                                                                
015100*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
015200*   -COPY WDECAREA                                                        
015300     SKIP3                                                                
015400*    ----PARAMETRAR TILL SUBRUT W411EXCH---                               
015500 01  FILLER                      PIC X(16)   VALUE 'W411EXCH   '.         
015600*   -COPY W411EXCH                                                        
015700 01  MESSAGE-CODES.                                                       
015800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
015900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
016000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016100     03  ERR-LINES-MISSING       PIC X(3)    VALUE '029'.                 
016200     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
016300     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '701'.                 
016400     03  ERR-ORDER-FINISH        PIC X(3)    VALUE '057'.                 
016500     03  ERR-ORDER-ANNULLERAD    PIC X(3)    VALUE '052'.                 
016600     03  ERR-OTILL-UPPDAT        PIC X(3)    VALUE '007'.                 
016700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
016800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
017000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
017100     03  INF-PART-MISSING        PIC X(3)    VALUE '017'.                 
017200     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
017300     03  INF-LAST-PAGE-SHOW      PIC X(3)    VALUE '115'.                 
017400     SKIP3                                                                
017500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
017600*01 -COPY WMSGINIT                                                        
017700     EJECT                                                                
017800*                                                                         
017900*                                                                         
018000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018100*                                                                         
018200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018300     SKIP3                                                                
018400*01  MID -COPY W4I26501                                                   
018500     EJECT                                                                
018600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018700     SKIP3                                                                
018800*01  -COPY WMSGAREA                                                       
018900     EJECT                                                                
019000     03  MOD REDEFINES MSG-AREA.                                          
019100*      05  -COPY W4O26501                                                 
019200     EJECT                                                                
019300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019400     SKIP3                                                                
019500*01  -COPY WMFSAREA                                                       
019600     EJECT                                                                
019700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019800*                                                                         
019900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020000     SKIP3                                                                
020100 01  NYCKLAR-TILL-DLI.                                                    
020200                                                                          
020300* ----> PROFORMAHUVUD                                                     
020400                                                                          
020500     03  W-PROC-WDE801KY-X.                                               
020600         05  W-PROC-IDDISTR       PIC S9(5)   COMP-3.                     
020700         05  W-PROC-IDKUNDNR      PIC S9(7)   COMP-3.                     
020800         05  W-PROC-IDKUNDRF      PIC  X(10).                             
020900                                                                          
021000* ----> PROFORMARAD                                                       
021100                                                                          
021200     03  W-PROD-WDE901KY-X.                                               
021300         05  W-PROD-IDORDER       PIC S9(7)   COMP-3.                     
021400         05  W-PROD-IDARTNR       PIC S9(9)   COMP-3.                     
021500         05  W-PROD-IDLOPNR       PIC S9(3)   COMP-3.                     
021600                                                                          
021700     03  W-PROD-WDE901KY-MIN-X.                                           
021800         05  W-PROD-MIN-IDORDER   PIC S9(7)   COMP-3.                     
021900         05  W-PROD-MIN-IDARTNR   PIC S9(9)   COMP-3.                     
022000         05  W-PROD-MIN-IDLOPNR   PIC S9(3)   COMP-3.                     
022100                                                                          
022200     03  W-PROD-WDE901KY-MAX-X.                                           
022300         05  W-PROD-MAX-IDORDER   PIC S9(7)   COMP-3.                     
022400         05  W-PROD-MAX-IDARTNR   PIC S9(9)   COMP-3.                     
022500         05  W-PROD-MAX-IDLOPNR   PIC S9(3)   COMP-3.                     
022600                                                                          
022700* ----> ARTIKELREGISTER ARTM                                              
022800                                                                          
022900     03  W-ARTM-IDARTNR-X.                                                
023000         05  W-ARTM-IDARTNR       PIC S9(9)   COMP-3.                     
023100                                                                          
023200* ----> ARTIKELREGISTER BENA                                              
023300                                                                          
023400     03  W-BENA-IDARTNR-X.                                                
023500         05  W-BENA-IDARTNR       PIC S9(9)   COMP-3.                     
023600                                                                          
023700     03  W-BENA-IDSKYLT-X         PIC X(3).                               
023800                                                                          
023900* ----> ARTIKELREGISTER ARTC                                              
024000                                                                          
024100     03  W-ARTC-IDARTNR-X.                                                
024200         05  W-ARTC-IDARTNR       PIC S9(9)   COMP-3.                     
024300                                                                          
024400* ----> ORDERBEKRÄFTELSE ORQM                                             
024500                                                                          
024600     03  W-ORQM-WDQ101KY-MIN-X.                                           
024700         05  W-ORQM-MIN-IDORDER   PIC S9(7)   COMP-3.                     
024800         05  W-ORQM-MIN-IDARTNR   PIC S9(9)   COMP-3.                     
024900         05  W-ORQM-MIN-IDLOPNR   PIC S9(3)   COMP-3.                     
025000         05  W-ORQM-MIN-IDSEKVNR  PIC S9(3)   COMP-3.                     
025100         05  W-ORQM-MIN-FILLER    PIC  X(4)   VALUE LOW-VALUE.            
025200                                                                          
025300     03  W-ORQM-WDQ101KY-MAX-X.                                           
025400         05  W-ORQM-MAX-IDORDER   PIC S9(7)   COMP-3.                     
025500         05  W-ORQM-MAX-IDARTNR   PIC S9(9)   COMP-3.                     
025600         05  W-ORQM-MAX-IDLOPNR   PIC S9(3)   COMP-3.                     
025700         05  W-ORQM-MAX-IDSEKVNR  PIC S9(3)   COMP-3.                     
025800         05  W-ORQM-MAX-FILLER    PIC  X(4)   VALUE HIGH-VALUE.           
025900                                                                          
026000* -----> KUNDREGISTER  WDB2, WDB1                                         
026100     03   W-IDGMT-WDB2-X.                                                 
026200         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
026300         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
026400                                                                          
026500     03  W-IDGMT-MIN-X.                                                   
026600         05  W-IDDISTR-WDB2-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
026700         05  W-IDKUNDNR-WDB2-MIN PIC S9(7)   VALUE ZERO COMP-3.           
026800                                                                          
026900     03  W-IDGMT-MAX-X.                                                   
027000         05  W-IDDISTR-WDB2-MAX  PIC S9(5)   VALUE ZERO COMP-3.           
027100         05  W-IDKUNDNR-WDB2-MAX PIC S9(7)   VALUE ZERO COMP-3.           
027200                                                                          
027300     03  W-WDB101KY-X.                                                    
027400         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
027500         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
027600                                                                          
027700* TILL VALUTAREG.                                                         
027800     03  W-WDGXKEY-9303-X.                                                
027900       05  W-IDHTYP              PIC X(4)  VALUE '9303'.                  
028000       05  W-TIMM                PIC 9(2)  VALUE 01.                      
028100       05  FILLER                PIC X(24) VALUE LOW-VALUE.               
028200*                                                                         
028800*    --- STATUS-KOD FRÅN IMS                                              
028900 01  STATUS-WS                   PIC XX.                                  
029000     88  SEGMENT-FINNS                       VALUE '  '.                  
029100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
029200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029300     88  END-OF-DATA                         VALUE 'GB'.                  
029400     SKIP2                                                                
029500 01  GODK-STATUSKODER.                                                    
029600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029700     SKIP3                                                                
029800 01  SSA1                        PIC X(160).                              
029900 01  SSA2                        PIC X(64).                               
030000     EJECT                                                                
030100*    --- IMS FUNKTIONSKODER                                               
030200*01  -COPY W0003                                                          
030300     EJECT                                                                
030400*    ---  DLI INPUT-OUTPUT AREA                                           
030500 01  FILLER                   PIC X(16)  VALUE 'IO-AREA-PROC01'.          
030600 01  DLI-IO-PROC01.                                                       
030700*    03  WLPROC01     -COPY WDE801                                        
030800     EJECT                                                                
030900                                                                          
031000 01  FILLER                     PIC X(16)  VALUE 'IO-AREA-PROD01'.        
031100 01  DLI-IO-PROD01.                                                       
031200*    03  WLPROD01     -COPY WDE901                                        
031300     EJECT                                                                
031400                                                                          
031500 01  FILLER                     PIC X(16)  VALUE 'IO-AREA-ARTM01'.        
031600 01  DLI-IO-ARTM01.                                                       
031700*    03  WLARTM01     -COPY WDK901                                        
031800     EJECT                                                                
031900                                                                          
032000 01  FILLER                     PIC X(16)  VALUE 'IO-AREA-ORQM01'.        
032100 01  DLI-IO-ORQM01.                                                       
032200*    03  WLORQM01     -COPY WDQ101                                        
032300     EJECT                                                                
032400                                                                          
032500 01  FILLER                     PIC X(16)  VALUE 'IO-AREA-ARTC01'.        
032600 01  DLI-IO-ARTC01.                                                       
032700*    03  WLARTC01     -COPY WDK601                                        
032800     EJECT                                                                
032900                                                                          
033000 01  FILLER                     PIC X(16)  VALUE 'IO-AREA-BENA11'.        
033100 01  DLI-IO-BENA11.                                                       
033200*    03  WLBENA11     -COPY WDD311                                        
033300     EJECT                                                                
033400                                                                          
033500 01  FILLER                     PIC X(16)  VALUE 'IO-AREA-WDB101'.        
033600 01  DLI-IO-WDB101.                                                       
033700*    03  WDB101       -COPY WDB101                                        
033800     EJECT                                                                
033900                                                                          
034000 01  FILLER                     PIC X(16)  VALUE 'IO-AREA-WDB201'.        
034100 01  DLI-IO-WDB201.                                                       
034200*    03  WDB201       -COPY WDB201                                        
034300     EJECT                                                                
034400                                                                          
034500 LINKAGE SECTION.                                                         
034600                                                                          
034700*01  -COPY W0009      -PRE MSG-                                           
034800     EJECT                                                                
034900*01  -COPY W0008      -PRE USEA-                                          
035000     05  FILLER                  PIC X.                                   
035100     EJECT                                                                
035200*01  -COPY W0008      -PRE PROC-                                          
035300     05  FILLER                  PIC X.                                   
035400     EJECT                                                                
035500*01  -COPY W0008      -PRE PROD-                                          
035600     05  FILLER                  PIC X.                                   
035700     EJECT                                                                
035800*01  -COPY W0008      -PRE ARTM-                                          
035900     05  FILLER                  PIC X.                                   
036000     EJECT                                                                
036100*01  -COPY W0008      -PRE ORQM-                                          
036200     05  FILLER                  PIC X.                                   
036300     EJECT                                                                
036400*01  -COPY W0008      -PRE BENA-                                          
036500     05  FILLER                  PIC X.                                   
036600     EJECT                                                                
036700*01  -COPY W0008      -PRE ARTC-                                          
036800     05  FILLER                  PIC X.                                   
036900     EJECT                                                                
037000*01  -COPY W0008      -PRE WDB1-                                          
037100     05  FILLER                  PIC X.                                   
037200     EJECT                                                                
037300*01  -COPY W0008      -PRE WDB2-                                          
037400     05  FILLER                  PIC X.                                   
037500     EJECT                                                                
037600 PROCEDURE DIVISION  USING MSG-PCB                                        
037700                           USEA-PCB                                       
037800                           PROC-PCB                                       
037900                           PROD-PCB                                       
038000                           ARTM-PCB                                       
038100                           ORQM-PCB                                       
038200                           BENA-PCB                                       
038300                           ARTC-PCB                                       
038400                           WDB1-PCB                                       
038500                           WDB2-PCB.                                      
038600                                                                          
038700     ENTRY 'DLITCBL' USING MSG-PCB                                        
038800                           USEA-PCB                                       
038900                           PROC-PCB                                       
039000                           PROD-PCB                                       
039100                           ARTM-PCB                                       
039200                           ORQM-PCB                                       
039300                           BENA-PCB                                       
039400                           ARTC-PCB                                       
039500                           WDB1-PCB                                       
039600                           WDB2-PCB.                                      
039700                                                                          
039800     PERFORM IMS-GET-MSG                                                  
039900     IF SEGMENT-FINNS                                                     
040000        PERFORM A-INIT                                                    
040100        PERFORM B-KOLLA-NYCKLAR                                           
040200        IF NYCKLAR-OK                                                     
040300           PERFORM C-LAES-IN-PROFORMAHUVUD                                
040400           IF ALLT-OK                                                     
040500              IF MFS-UPDATE                                               
040600                 PERFORM G-KOLLA-INPUT                                    
040700                 IF INDATA-OK                                             
040800                    PERFORM H-UPPDATERA                                   
040900                 END-IF                                                   
041000              ELSE                                                        
041100                 IF MFS-FIRST                                             
041200                    PERFORM D-FOERSTA-SIDA                                
041300                 ELSE                                                     
041400                    IF MFS-NEXT                                           
041500                       PERFORM E-NAESTA-SIDA                              
041600                    ELSE                                                  
041700                       PERFORM F-SAMMA-SIDA                               
041800                    END-IF                                                
041900                 END-IF                                                   
042000              END-IF                                                      
042100              IF ALLT-OK                                                  
042200                 PERFORM I-LAES-VISA-INFO                                 
042300              END-IF                                                      
042400           END-IF                                                         
042500        END-IF                                                            
042600        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
042700        PERFORM IMS-INSERT-MSG                                            
042800     END-IF                                                               
042900                                                                          
043000     MOVE ZERO TO RETURN-CODE                                             
043100     GOBACK                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 A-INIT SECTION.                                                          
043500                                                                          
043600     IF MSG-DUBBLA-TRANSKODER                                             
043700        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I26501                
043800        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
043900        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
044000     ELSE                                                                 
044100        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I26501                
044200        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
044300        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
044400     END-IF                                                               
044500                                                                          
044600     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
044700     MOVE MSG-IDPFK            TO MFS-IDPFK                               
044800     MOVE MFS-IDTRANS          TO W-IDTRANS                               
044900                                                                          
045000     MOVE LOW-VALUE            TO MSG-AREA                                
045100     MOVE 'W4O26501'           TO MFS-IDMOD                               
045200     MOVE '4265'               TO MOD-IDTRANS                             
045300     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL                            
045400                                  MOD-TEMFSINF                            
045500                                                                          
045600     IF NOT EGEN-MID                                                      
045700       MOVE SPACE           TO MID-IDDISTR-IN                             
045800                               MID-IDKUNDNR-IN                            
045900                               MID-IDKUNDRF-IN                            
046000                               MID-IDARTNR-IN                             
046100                               MID-IDDISTR-UT                             
046200                               MID-IDKUNDNR-UT                            
046300                               MID-IDKUNDRF-UT                            
046400                               MID-IDARTNR-UT                             
046500        MOVE SPACE TO MFS-KDTRTYP                                         
046600        MOVE '7' TO MFS-IDPFK                                             
046700     END-IF                                                               
046800                                                                          
046900     IF ENGLISH-TEXT                                                      
047000        MOVE +2    TO SPRAK-IX                                            
047100        MOVE 'GB ' TO MED-IDSKYLT                                         
047200     ELSE                                                                 
047300        MOVE +1    TO SPRAK-IX                                            
047400        MOVE 'S  ' TO MED-IDSKYLT                                         
047500     END-IF                                                               
047600                                                                          
047700     MOVE  LOW-VALUE         TO W-IDGMT-MIN-X                             
047800                                                                          
047900     MOVE HIGH-VALUE         TO W-IDGMT-MAX-X                             
048000                                                                          
048100                                                                          
048200     ACCEPT WS-DATUM   FROM DATE                                          
048300     ACCEPT WS-KLOCKAN FROM TIME                                          
048400     MOVE WS-DATUM(3:2)  TO W-TIMM                                        
048500     .                                                                    
048600     EJECT                                                                
048700 B-KOLLA-NYCKLAR SECTION.                                                 
048800                                                                          
048900     MOVE JA TO NYCKLAR-SW                                                
049000                                                                          
049100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
049200     MOVE '001'             TO MSGI-KDCALL                                
049300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
049400     MOVE '4265'            TO MSGI-IDTRANS                               
049500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
049600     IF MFS-IDTRANS = '4265'                                              
049700        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
049800        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
049900                                                                          
050000        MOVE MID-IDKUNDRF-IN TO WS-IDKUNDRF-1-7                           
050100        IF WS-IDKUNDRF-1-7 = ALL '+'                                      
050200          MOVE '+++'         TO WS-IDKUNDRF-8-10                          
050300        ELSE                                                              
050400          MOVE SPACE         TO WS-IDKUNDRF-8-10                          
050500        END-IF                                                            
050600        MOVE WS-IDKUNDRF-RED TO MSGI-IDKUNDRF                             
050700        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
050800     END-IF                                                               
050900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
051000                                                                          
051100     MOVE LOW-VALUE TO       W-PROD-WDE901KY-MIN-X                        
051200                                                                          
051300     MOVE HIGH-VALUE TO      W-PROD-WDE901KY-MAX-X                        
051400                                                                          
051500     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
051600                             MOD-IDKUNDNR-IN                              
051700                             MOD-IDKUNDRF-IN                              
051800                             MOD-IDARTNR-IN                               
051900                                                                          
052000     PERFORM BA-KOLLA-DISTRIKT                                            
052100     PERFORM BB-KOLLA-KUNDNUMMER                                          
052200     PERFORM BC-KOLLA-ORDERNUMMER                                         
052300     PERFORM BD-KOLLA-ARTIKELNUMMER                                       
052400                                                                          
052500     MOVE WS-IDDISTR   TO MOD-IDDISTR-UT                                  
052600     MOVE WS-IDKUNDNR  TO MOD-IDKUNDNR-UT                                 
052700     MOVE WS-IDKUNDRF  TO MOD-IDKUNDRF-UT                                 
052800     MOVE WS-IDARTNR   TO MOD-IDARTNR-UT                                  
052900     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
053000     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
053100     IF MOD-IDKUNDNR-UT = ALL SPACE                                       
053200        MOVE '     0' TO MOD-IDKUNDNR-UT                                  
053300     END-IF                                                               
053400     INSPECT MOD-IDKUNDRF-UT REPLACING LEADING ZERO BY SPACE              
053500     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
053600                                                                          
053700     IF NYCKLAR-FEL                                                       
053800        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
053900        PERFORM S01-FEL-MEDDELANDE                                        
054000        PERFORM MFS-RENSA-FAELT-IN                                        
054100        PERFORM MFS-RENSA-FAELT-UT                                        
054200     END-IF                                                               
054300     PERFORM BE-HAMTA-KDVALISO                                            
054400     .                                                                    
054500     EJECT                                                                
054600 BA-KOLLA-DISTRIKT SECTION.                                               
054700                                                                          
054800     MOVE MSGI-IDDISTR   TO WS-IDDISTR                                    
054900     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
055000                                                                          
055100     IF MID-IDDISTR-IN = ALL '+'                                          
055200       CONTINUE                                                           
055300     ELSE                                                                 
055400       MOVE '7'         TO MFS-IDPFK                                      
055500       MOVE SPACE       TO MFS-KDTRTYP                                    
055600     END-IF                                                               
055700                                                                          
055800     IF WS-IDDISTR NUMERIC                                                
055900        AND                                                               
056000        WS-IDDISTR > ZERO                                                 
056100        MOVE WS-IDDISTR TO W-PROC-IDDISTR                                 
056200     ELSE                                                                 
056300        MOVE NEJ TO NYCKLAR-SW                                            
056400     END-IF                                                               
056500                                                                          
056600     MOVE WS-IDDISTR        TO DIST79-IDDISTR                             
056700     IF DIST79-DEALER-PRICE                                               
056800       IF ENGLISH-TEXT                                                    
056900         MOVE 'DEALERPRICE' TO MOD-TEDDI                                  
057000       ELSE                                                               
057100         MOVE '    ÅF PRIS' TO MOD-TEDDI                                  
057200       END-IF                                                             
057300     ELSE                                                                 
058100        MOVE SPACE           TO MOD-TEDDI                                 
058300     END-IF                                                               
058400     .                                                                    
058500     EJECT                                                                
058600 BB-KOLLA-KUNDNUMMER SECTION.                                             
058700                                                                          
058800     MOVE MSGI-IDKUNDNR    TO WS-IDKUNDNR                                 
058900     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
059000                                                                          
059100     IF MID-IDKUNDNR-IN = ALL '+'                                         
059200       CONTINUE                                                           
059300     ELSE                                                                 
059400       MOVE '7'         TO MFS-IDPFK                                      
059500       MOVE SPACE       TO MFS-KDTRTYP                                    
059600     END-IF                                                               
059700                                                                          
059800     IF WS-IDKUNDNR NUMERIC                                               
059900        MOVE WS-IDKUNDNR TO W-PROC-IDKUNDNR                               
060000     ELSE                                                                 
060100        MOVE NEJ         TO NYCKLAR-SW                                    
060200     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500 BC-KOLLA-ORDERNUMMER SECTION.                                            
060600                                                                          
060700                                                                          
060800     MOVE MSGI-IDKUNDRF    TO WS-IDKUNDRF                                 
060900     INSPECT WS-IDKUNDRF REPLACING LEADING SPACE BY ZERO                  
061000                                                                          
061100     IF MID-IDKUNDRF-IN = ALL '+'                                         
061200        CONTINUE                                                          
061300     ELSE                                                                 
061400        MOVE '7'             TO MFS-IDPFK                                 
061500        MOVE SPACE           TO MFS-KDTRTYP                               
061600     END-IF                                                               
061700                                                                          
061800     IF WS-IDKUNDRF NUMERIC                                               
061900        AND                                                               
062000        WS-IDKUNDRF > ZERO                                                
062100        MOVE WS-IDKUNDRF TO W-PROC-IDKUNDRF                               
062200     ELSE                                                                 
062300        MOVE NEJ         TO NYCKLAR-SW                                    
062400     END-IF                                                               
062500     .                                                                    
062600     EJECT                                                                
062700 BD-KOLLA-ARTIKELNUMMER SECTION.                                          
062800                                                                          
062900     IF MID-IDARTNR-IN = ALL '+'                                          
063000        IF EGEN-MID                                                       
063100           MOVE MID-IDARTNR-UT TO WS-IDARTNR                              
063200        ELSE                                                              
063300           MOVE SPACE          TO WS-IDARTNR                              
063400        END-IF                                                            
063500     ELSE                                                                 
063600        MOVE MID-IDARTNR-IN TO WS-IDARTNR                                 
063700        MOVE '7'            TO MFS-IDPFK                                  
063800        MOVE SPACE          TO MFS-KDTRTYP                                
063900     END-IF                                                               
064000                                                                          
064100     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
064200                                                                          
064300     IF WS-IDARTNR NUMERIC                                                
064400        IF WS-IDARTNR > ZERO                                              
064500           MOVE WS-IDARTNR TO W-PROD-MIN-IDARTNR                          
064600                              W-PROD-MAX-IDARTNR                          
064700           MOVE JA         TO IDARTNR-SW                                  
064800        END-IF                                                            
064900     ELSE                                                                 
065000        MOVE NEJ            TO NYCKLAR-SW                                 
065100     END-IF                                                               
065200     .                                                                    
065300     EJECT                                                                
065400 BE-HAMTA-KDVALISO  SECTION.                                              
065401                                                                          
065410     IF DIST79-DEALER-PRICE                                               
065500       MOVE WS-IDDISTR              TO W-IDDISTR-WDB2                     
065600                                       W-IDDISTR-WDB2-MIN                 
065700                                       W-IDDISTR-WDB2-MAX                 
065800       MOVE WS-IDKUNDNR             TO W-IDKUNDNR-WDB2                    
065900       PERFORM IMS-GU-WDB201                                              
066000       IF SEGMENT-FINNS                                                   
066100          CONTINUE                                                        
066200       ELSE                                                               
066300          PERFORM IMS-GET-WDB201                                          
066400       END-IF                                                             
066500       MOVE GMT-IDPARTNR            TO W-WDB1-IDPARTNR                    
066600       MOVE GMT-IDFTG               TO W-WDB1-IDFTG                       
066700                                                                          
066800       PERFORM IMS-GU-WDB101                                              
066900       IF SEGMENT-FINNS                                                   
067000          MOVE BET-KDVALISO         TO MOD-KDVALISO                       
067200       ELSE                                                               
067300          MOVE SPACE                TO MOD-KDVALISO                       
067400       END-IF                                                             
067410     ELSE                                                                 
067420       MOVE 'SEK'                   TO MOD-KDVALISO                       
067500     END-IF                                                               
067600                                                                          
068300     .                                                                    
068400     EJECT                                                                
068500 C-LAES-IN-PROFORMAHUVUD SECTION.                                         
068600                                                                          
068700     PERFORM IMS-GHU-PROC-WLPROC01                                        
068800                                                                          
068900     IF SEGMENT-FINNS                                                     
069000        PERFORM CA-KOLLA-FORFALLODATUM                                    
069100        PERFORM CB-KOLLA-BEHORIGHET                                       
069200        PERFORM CC-KOLLA-BORTTAGSMARKERAD                                 
069300        IF ALLT-OK                                                        
069400           MOVE PHUV-IDORDER  TO W-PROD-MIN-IDORDER                       
069500                                 W-PROD-MAX-IDORDER                       
069600                                 W-PROD-IDORDER                           
069700           MOVE PHUV-KDORDKL  TO MOD-KDORDKL                              
069800           MOVE PHUV-KDFRAKT  TO MOD-KDFRAKT                              
069900           MOVE PHUV-KDPROTYP TO MOD-KDPROTYP                             
070000        END-IF                                                            
070100     ELSE                                                                 
070200        MOVE ERR-ORDER-MISSING TO MED-IDMFSFEL                            
070300        MOVE NEJ               TO ALLT-SW                                 
070400     END-IF                                                               
070500                                                                          
070600     IF ALLT-FEL                                                          
070700        PERFORM S01-FEL-MEDDELANDE                                        
070800        IF NOT MFS-UPDATE                                                 
070900           PERFORM MFS-RENSA-FAELT-IN                                     
071000           PERFORM MFS-RENSA-FAELT-UT                                     
071100        END-IF                                                            
071200     END-IF                                                               
071300     .                                                                    
071400     EJECT                                                                
071500 CA-KOLLA-FORFALLODATUM SECTION.                                          
071600                                                                          
071700     MOVE PHUV-TIFORDAT TO WS-TIFORDAT                                    
071800     MOVE WS-TIFORDAT   TO TMP1-YYMMDD                                    
071900     MOVE WS-DATUM      TO TMP2-YYMMDD                                    
072000     PERFORM WY2000P1                                                     
072100     IF TMP1-YYMMDD <= TMP2-YYMMDD                                        
072200        OR                                                                
072300        PHUV-TIORDDAT > 0                                                 
072400        IF MFS-UPDATE                                                     
072500           PERFORM MFS-ROR-EJ-FAELT-IN                                    
072600           PERFORM MFS-ROR-EJ-FAELT-UT                                    
072700           MOVE ERR-OTILL-UPPDAT TO MED-IDMFSFEL                          
072800           MOVE NEJ TO ALLT-SW                                            
072900        END-IF                                                            
073000     END-IF                                                               
073100     .                                                                    
073200     EJECT                                                                
073300 CB-KOLLA-BEHORIGHET SECTION.                                             
073400                                                                          
073500     IF PHUV-KDPROTYP = 'L'                                               
073600        IF PHUV-IDUSER NOT = MSG-SIGNON-USERID                            
073700           MOVE ERR-OBEHORIG TO MED-IDMFSFEL                              
073800           MOVE NEJ          TO ALLT-SW                                   
073900        END-IF                                                            
074000     END-IF                                                               
074100     .                                                                    
074200     EJECT                                                                
074300 CC-KOLLA-BORTTAGSMARKERAD SECTION.                                       
074400                                                                          
074500     IF PHUV-FLBORT = JA                                                  
074600        MOVE ERR-ORDER-ANNULLERAD TO MED-IDMFSFEL                         
074700        MOVE NEJ                  TO ALLT-SW                              
074800     END-IF                                                               
074900     .                                                                    
075000     EJECT                                                                
075100 D-FOERSTA-SIDA SECTION.                                                  
075200                                                                          
075300     MOVE ZERO       TO  MOD-IDARTNR-ENTER                                
075400                         MOD-IDLOPNR-ENTER                                
075500     MOVE 999999999  TO  MOD-IDARTNR-NEXT                                 
075600     MOVE 999        TO  MOD-IDLOPNR-NEXT                                 
075700                                                                          
075800     PERFORM MFS-RENSA-FAELT-IN                                           
075900     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
076000     PERFORM S01-FEL-MEDDELANDE                                           
076100     .                                                                    
076200     EJECT                                                                
076300 E-NAESTA-SIDA SECTION.                                                   
076400                                                                          
076500     IF MID-IDARTNR-NEXT = 999999999                                      
076600        AND                                                               
076700        MID-IDLOPNR-NEXT = 999                                            
076800        MOVE MID-IDARTNR-NEXT   TO MOD-IDARTNR-NEXT                       
076900        MOVE MID-IDLOPNR-NEXT   TO MOD-IDLOPNR-NEXT                       
077000        MOVE INF-LAST-PAGE-SHOW TO MED-IDMFSINF                           
077100        PERFORM S02-INFO-MEDDELANDE                                       
077200        MOVE NEJ TO ALLT-SW                                               
077300     ELSE                                                                 
077400        MOVE MID-IDARTNR-NEXT    TO W-PROD-MIN-IDARTNR                    
077500        IF IDARTNR-IFYLLT                                                 
077600           MOVE MID-IDARTNR-NEXT TO W-PROD-MAX-IDARTNR                    
077700        END-IF                                                            
077800        MOVE MID-IDLOPNR-NEXT    TO W-PROD-MIN-IDLOPNR                    
077900     END-IF                                                               
078000     .                                                                    
078100     EJECT                                                                
078200 F-SAMMA-SIDA SECTION.                                                    
078300                                                                          
078400     MOVE MID-IDARTNR-ENTER    TO W-PROD-MIN-IDARTNR                      
078500     IF IDARTNR-IFYLLT                                                    
078600        MOVE MID-IDARTNR-ENTER TO W-PROD-MAX-IDARTNR                      
078700     END-IF                                                               
078800     MOVE MID-IDLOPNR-ENTER    TO W-PROD-MIN-IDLOPNR                      
078900                                                                          
079000     MOVE JA TO INDATA-SW                                                 
079100                                                                          
079200     MOVE +1 TO INDX                                                      
079300     PERFORM UNTIL INDX > MAX-INDX                                        
079400        IF MID-CMD-UPDATE  (INDX) NOT = ALL '+'                           
079500           OR                                                             
079600           MID-KVBEART-Q-U (INDX) NOT = ALL '+'                           
079700           OR                                                             
079800           MID-PRARTNTO-U  (INDX) NOT = ALL '+'                           
079900           OR                                                             
080000           MID-BEART-U     (INDX) NOT = ALL '+'                           
080100           OR                                                             
080200           MID-BERADREF-U  (INDX) NOT = ALL '+'                           
080300           MOVE NEJ TO INDATA-SW                                          
080400        END-IF                                                            
080500        ADD 1 TO INDX                                                     
080600     END-PERFORM                                                          
080700                                                                          
080800     IF INDATA-FEL                                                        
080900        MOVE NEJ TO ALLT-SW                                               
081000        MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                               
081100        PERFORM S01-FEL-MEDDELANDE                                        
081200        PERFORM MFS-ROR-EJ-FAELT-IN                                       
081300        PERFORM MFS-ROR-EJ-FAELT-UT                                       
081400        PERFORM MFS-LAS-IN-IGEN                                           
081500     END-IF                                                               
081600     .                                                                    
081700     EJECT                                                                
081800 G-KOLLA-INPUT SECTION.                                                   
081900                                                                          
082000     MOVE NEJ TO INDATA-SW                                                
082100                                                                          
082200     MOVE +1 TO INDX                                                      
082300     PERFORM UNTIL INDX > MAX-INDX                                        
082400        IF MID-CMD-UPDATE  (INDX) = ALL '+'                               
082500           AND                                                            
082600           MID-KVBEART-Q-U (INDX) = ALL '+'                               
082700           AND                                                            
082800           MID-PRARTNTO-U  (INDX) = ALL '+'                               
082900           AND                                                            
083000           MID-BEART-U     (INDX) = ALL '+'                               
083100           AND                                                            
083200           MID-BERADREF-U  (INDX) = ALL '+'                               
083300           CONTINUE                                                       
083400        ELSE                                                              
083500           MOVE JA  TO INDATA-SW                                          
083600        END-IF                                                            
083700        ADD 1 TO INDX                                                     
083800     END-PERFORM                                                          
083900                                                                          
084000     IF INDATA-FEL                                                        
084100        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
084200        PERFORM S01-FEL-MEDDELANDE                                        
084300        PERFORM MFS-ROR-EJ-FAELT-UT                                       
084400        PERFORM MFS-ROR-EJ-FAELT-IN                                       
084500        MOVE NEJ TO ALLT-SW                                               
084600     ELSE                                                                 
084700        MOVE +1 TO INDX                                                   
084800        PERFORM UNTIL INDX > MAX-INDX                                     
084900           PERFORM GA-KOLLA-CMD-KOD                                       
085000           PERFORM GB-KOLLA-BESTALLT-ANTAL                                
085100           PERFORM GC-KOLLA-PRIS                                          
085200           PERFORM GD-KOLLA-BENAMNING                                     
085300           PERFORM GE-KOLLA-RADREFERENS                                   
085400           ADD 1 TO INDX                                                  
085500        END-PERFORM                                                       
085600        IF INDATA-FEL                                                     
085700           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
085800           PERFORM S01-FEL-MEDDELANDE                                     
085900           PERFORM MFS-ROR-EJ-FAELT-UT                                    
086000           PERFORM MFS-ROR-EJ-FAELT-IN                                    
086100           MOVE NEJ TO ALLT-SW                                            
086200        END-IF                                                            
086300     END-IF                                                               
086400     .                                                                    
086500     EJECT                                                                
086600 GA-KOLLA-CMD-KOD SECTION.                                                
086700                                                                          
086800     IF MID-CMD-UPDATE(INDX) NOT = ALL '+'                                
086900        IF MID-CMD-UPDATE(INDX) = 'D' OR 'A'                              
087000           MOVE MFS-ALFA-FAELT-RAETT                                      
087100                        TO MOD-CMD-UPDATE-ATTR    (INDX)                  
087200        ELSE                                                              
087300           MOVE MFS-ALFA-FAELT-FEL                                        
087400                        TO MOD-CMD-UPDATE-ATTR    (INDX)                  
087500           MOVE NEJ TO INDATA-SW                                          
087600        END-IF                                                            
087700     ELSE                                                                 
087800        IF MID-CMD-UPDATE (INDX) = ALL '+'                                
087900           IF MID-KVBEART-Q-U  (INDX) NOT = ALL '+'                       
088000              OR                                                          
088100              MID-PRARTNTO-U   (INDX) NOT = ALL '+'                       
088200              OR                                                          
088300              MID-BEART-U      (INDX) NOT = ALL '+'                       
088400              OR                                                          
088500              MID-BERADREF-U   (INDX) NOT = ALL '+'                       
088600              MOVE MFS-ALFA-FAELT-FEL                                     
088700                           TO MOD-CMD-UPDATE-ATTR (INDX)                  
088800              MOVE NEJ TO INDATA-SW                                       
088900           END-IF                                                         
089000        END-IF                                                            
089100     END-IF                                                               
089200     .                                                                    
089300     EJECT                                                                
089400 GB-KOLLA-BESTALLT-ANTAL SECTION.                                         
089500                                                                          
089600     IF MID-KVBEART-Q-U (INDX) NOT = ALL '+'                              
089700        IF MID-KVBEART-Q-U (INDX) NOT NUMERIC                             
089800           MOVE MFS-NUM-FAELT-FEL                                         
089900                        TO MOD-KVBEART-Q-U-ATTR   (INDX)                  
090000           MOVE NEJ TO INDATA-SW                                          
090100        ELSE                                                              
090200           MOVE MFS-NUM-FAELT-RAETT                                       
090300                        TO MOD-KVBEART-Q-U-ATTR   (INDX)                  
090400           MOVE MID-KVBEART-Q-U                   (INDX)                  
090500                        TO WS-KVBEART-Q           (INDX)                  
090600        END-IF                                                            
090700     END-IF                                                               
090800     .                                                                    
090900     EJECT                                                                
091000 GC-KOLLA-PRIS SECTION.                                                   
091100                                                                          
091200     IF MID-PRARTNTO-U(INDX) NOT = ALL '+'                                
091300        MOVE MID-PRARTNTO-U (INDX) TO DEC-IDFRIDATA                       
091400        MOVE +7 TO DEC-KVHELTAL                                           
091500        MOVE +2 TO DEC-KVDECIMAL                                          
091600        CALL WDECEDIT USING DEC-WDECAREA                                  
091700        IF DEC-KDSVAR-FEL                                                 
091800           MOVE MFS-NUM-FAELT-FEL                                         
091900                               TO MOD-PRARTNTO-U-ATTR (INDX)              
092000           MOVE NEJ TO INDATA-SW                                          
092100        ELSE                                                              
092200           MOVE DEC-IDEDITDATA TO WS-PRARTNTO         (INDX)              
092300           IF WS-PRARTNTO (INDX) NOT > ZERO                               
092400              MOVE MFS-NUM-FAELT-FEL                                      
092500                               TO MOD-PRARTNTO-U-ATTR (INDX)              
092600              MOVE NEJ TO INDATA-SW                                       
092700           ELSE                                                           
092800              MOVE MFS-NUM-FAELT-RAETT                                    
092900                               TO MOD-PRARTNTO-U-ATTR (INDX)              
093000           END-IF                                                         
093100        END-IF                                                            
093200     END-IF                                                               
093300     .                                                                    
093400     EJECT                                                                
093500 GD-KOLLA-BENAMNING SECTION.                                              
093600                                                                          
093700     IF MID-BEART-U (INDX) NOT = ALL '+'                                  
093800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-U-ATTR (INDX)              
093900     END-IF                                                               
094000     .                                                                    
094100     EJECT                                                                
094200 GE-KOLLA-RADREFERENS SECTION.                                            
094300                                                                          
094400     IF MID-BERADREF-U (INDX) NOT = ALL '+'                               
094500        MOVE MFS-ALFA-FAELT-RAETT TO MOD-BERADREF-U-ATTR (INDX)           
094600     END-IF                                                               
094700     .                                                                    
094800     EJECT                                                                
094900 H-UPPDATERA SECTION.                                                     
095000                                                                          
095100     MOVE JA TO INDATA-SW                                                 
095200                                                                          
095300     PERFORM HA-UPPDAT-EN-SIDA                                            
095400                                                                          
095500     IF INDATA-OK                                                         
095600        MOVE INF-UPDATE-DONE TO MED-IDMFSINF                              
095700        PERFORM S02-INFO-MEDDELANDE                                       
095800        PERFORM MFS-FORM-ATTR                                             
095900        PERFORM MFS-RENSA-FAELT-IN                                        
096000        MOVE MID-IDARTNR-ENTER    TO W-PROD-MIN-IDARTNR                   
096100        IF IDARTNR-IFYLLT                                                 
096200           MOVE MID-IDARTNR-ENTER TO W-PROD-MAX-IDARTNR                   
096300        END-IF                                                            
096400        MOVE MID-IDLOPNR-ENTER    TO W-PROD-MIN-IDLOPNR                   
096500     ELSE                                                                 
096600        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
096700        PERFORM S01-FEL-MEDDELANDE                                        
096800        PERFORM MFS-ROR-EJ-FAELT-UT                                       
096900        PERFORM MFS-ROR-EJ-FAELT-IN                                       
097000        MOVE NEJ TO ALLT-SW                                               
097100     END-IF                                                               
097200     .                                                                    
097300     EJECT                                                                
097400 HA-UPPDAT-EN-SIDA SECTION.                                               
097500                                                                          
097600     MOVE +1 TO INDX                                                      
097700     PERFORM UNTIL INDX > MAX-INDX                                        
097800        IF MID-CMD-UPDATE(INDX) NOT = ALL '+'                             
097900           MOVE MID-IDARTNR (INDX) TO W-PROD-IDARTNR                      
098000           MOVE MID-IDLOPNR (INDX) TO W-PROD-IDLOPNR                      
098100           PERFORM IMS-GHU-PROD-WLPROD01                                  
098200           IF SEGMENT-FINNS                                               
098300              IF MID-CMD-UPDATE(INDX) = 'A'                               
098400                 IF MID-KVBEART-Q-U (INDX) NOT = ALL '+'                  
098500                    IF PRAD-KVBEART-Q < WS-KVBEART-Q (INDX)               
098600                       MOVE NEJ TO INDATA-SW                              
098700                       MOVE MFS-NUM-FAELT-FEL TO                          
098800                                   MOD-KVBEART-Q-U-ATTR(INDX)             
098900                    END-IF                                                
099000                 ELSE                                                     
099100                    IF (MID-KVBEART-Q-U (INDX) = ALL '+' AND              
099200                        MID-PRARTNTO-U  (INDX) = ALL '+' AND              
099300                        MID-BEART-U     (INDX) = ALL '+' AND              
099400                        MID-BERADREF-U  (INDX) = ALL '+')                 
099500                       MOVE NEJ TO INDATA-SW                              
099600                       MOVE MFS-NUM-FAELT-FEL TO                          
099700                                  MOD-KVBEART-Q-U-ATTR (INDX)             
099800                       MOVE MFS-NUM-FAELT-FEL TO                          
099900                                  MOD-PRARTNTO-U-ATTR  (INDX)             
100000                       MOVE MFS-ALFA-FAELT-FEL TO                         
100100                                  MOD-BEART-U-ATTR     (INDX)             
100200                       MOVE MFS-ALFA-FAELT-FEL TO                         
100300                                  MOD-BERADREF-U-ATTR  (INDX)             
100400                       MOVE MFS-ALFA-FAELT-FEL TO                         
100500                                  MOD-CMD-UPDATE-ATTR  (INDX)             
100600                    END-IF                                                
100700                 END-IF                                                   
100800              END-IF                                                      
100900           END-IF                                                         
101000        END-IF                                                            
101100        ADD +1 TO INDX                                                    
101200     END-PERFORM                                                          
101300                                                                          
101400     IF INDATA-OK                                                         
101500        MOVE +1 TO INDX                                                   
101600        PERFORM UNTIL INDX > MAX-INDX                                     
101700           IF MID-CMD-UPDATE(INDX) NOT = ALL '+'                          
101800              MOVE MID-IDARTNR (INDX) TO W-PROD-IDARTNR                   
101900              MOVE MID-IDLOPNR (INDX) TO W-PROD-IDLOPNR                   
102000              PERFORM IMS-GHU-PROD-WLPROD01                               
102100              IF SEGMENT-FINNS                                            
102200                 IF MID-CMD-UPDATE (INDX) = 'D'                           
102300                    PERFORM HAA-BORTTAG-RAD                               
102400                 ELSE                                                     
102500                    IF MID-CMD-UPDATE (INDX) = 'A'                        
102600                       IF MID-KVBEART-Q-U (INDX) NOT = ALL '+'            
102700                          AND                                             
102800                          WS-KVBEART-Q    (INDX) = ZERO                   
102900                          PERFORM HAA-BORTTAG-RAD                         
103000                       ELSE                                               
103100                          PERFORM HAB-AENDRA-RAD                          
103200                       END-IF                                             
103300                    END-IF                                                
103400                 END-IF                                                   
103500              END-IF                                                      
103600           END-IF                                                         
103700           ADD 1 TO INDX                                                  
103800        END-PERFORM                                                       
103900        PERFORM HAC-UPPDAT-PROFORMAHUVUD                                  
104000     END-IF                                                               
104100     .                                                                    
104200     EJECT                                                                
104300 HAA-BORTTAG-RAD SECTION.                                                 
104400                                                                          
104500     IF PHUV-KDPROTYP = 'O' OR 'L'                                        
104600        MOVE PRAD-KVBEART-Q TO WS-KVOFFERT                                
104700        PERFORM S03-UPPDAT-WLARTM01                                       
104800     END-IF                                                               
104900                                                                          
105000     PERFORM HAAA-UPPDAT-MOTOR-KAROSS                                     
105100                                                                          
105200     PERFORM HAAB-UPPDAT-VAERDE-VIKT-VOLYM                                
105300                                                                          
105400     PERFORM HAAC-SKAPA-ORDERBEKRAFTELSE                                  
105500                                                                          
105600     PERFORM IMS-DLET-PROD-WLPROD01                                       
105700     .                                                                    
105800     EJECT                                                                
105900 HAAA-UPPDAT-MOTOR-KAROSS SECTION.                                        
106000                                                                          
106100     MOVE PRAD-IDARTNR TO W-ARTC-IDARTNR                                  
106200                                                                          
106300     PERFORM IMS-GU-ARTC-WLARTC01                                         
106400                                                                          
106500     IF SEGMENT-FINNS                                                     
106600        IF ART-IDFKNGRP = 2101 OR 2102                                    
106700           IF PHUV-KVMOTOR > 0                                            
106800              SUBTRACT 1 FROM PHUV-KVMOTOR                                
106900           END-IF                                                         
107000        ELSE                                                              
107100           IF ART-IDFKNGRP = 8001 OR 8002                                 
107200              IF PHUV-KVKAROSS > 0                                        
107300                 SUBTRACT 1 FROM PHUV-KVKAROSS                            
107400              END-IF                                                      
107500           END-IF                                                         
107600        END-IF                                                            
107700     END-IF                                                               
107800     .                                                                    
107900     EJECT                                                                
108000 HAAB-UPPDAT-VAERDE-VIKT-VOLYM SECTION.                                   
108100                                                                          
108200     IF NOT DIST79-DEALER-PRICE                                           
108300       COMPUTE WS-SUORDV   =                                              
108400                 PRAD-KVBEART-Q * PRAD-PRARTNTO                           
108500     ELSE                                                                 
108600       COMPUTE WS-SUORDV-LOC =                                            
108700                 PRAD-KVBEART-Q * PRAD-PRARTNTO-LOC                       
108800       COMPUTE WS-SUORDV-LOCPREL =                                        
108900                 PRAD-KVBEART-Q * PRAD-PRARTNTO-LOCPREL                   
109000     END-IF                                                               
109010                                                                          
109100     COMPUTE WS-VKORDNTO =                                                
109200               ((PRAD-KVBEART-Q * PRAD-VKART) / 1000)                     
109300     COMPUTE WS-VLORDNTO =                                                
109400               ((PRAD-KVBEART-Q * PRAD-VLARTNTO) / 1000000)               
109500                                                                          
109600     IF NOT DIST79-DEALER-PRICE                                           
109700       SUBTRACT WS-SUORDV         FROM PHUV-SUORDV                        
109800     ELSE                                                                 
109900       SUBTRACT WS-SUORDV-LOC     FROM PHUV-SUORDV-LOC                    
110000       SUBTRACT WS-SUORDV-LOCPREL FROM PHUV-SUORDV-LOCPREL                
110100     END-IF                                                               
110110                                                                          
110200     SUBTRACT WS-VKORDNTO   FROM PHUV-VKORDNTO                            
110300     SUBTRACT WS-VLORDNTO   FROM PHUV-VLORDBTO                            
110400                                                                          
110500     IF PHUV-SUORDV NEGATIVE                                              
110600        MOVE 0 TO PHUV-SUORDV                                             
110700     END-IF                                                               
110800     IF PHUV-SUORDV-LOC NEGATIVE                                          
110900        MOVE 0 TO PHUV-SUORDV-LOC                                         
111000     END-IF                                                               
111100     IF PHUV-SUORDV-LOCPREL NEGATIVE                                      
111200        MOVE 0 TO PHUV-SUORDV-LOCPREL                                     
111300     END-IF                                                               
111400                                                                          
111500     IF PHUV-VKORDNTO NEGATIVE                                            
111600        MOVE 0 TO PHUV-VKORDNTO                                           
111700     END-IF                                                               
111800                                                                          
111900     IF PHUV-VLORDBTO NEGATIVE                                            
112000        MOVE 0 TO PHUV-VLORDBTO                                           
112100     END-IF                                                               
112200     .                                                                    
112300     EJECT                                                                
112400 HAAC-SKAPA-ORDERBEKRAFTELSE SECTION.                                     
112500                                                                          
112600     PERFORM HAACA-KOLLA-LOPNR-OBKR                                       
112700                                                                          
112800     MOVE PHUV-IDORDER           TO   OBKR-IDORDER                        
112900     MOVE PRAD-IDARTNR           TO   OBKR-IDARTNR                        
112910     MOVE PRAD-KDVALISO          TO   OBKR-KDVALISO                       
113000     MOVE W-ORQM-MIN-IDLOPNR     TO   OBKR-IDLOPNR                        
113100     MOVE 1                      TO   OBKR-IDSEKVNR                       
113200     MOVE WC-CDC-SE              TO   OBKR-IDDC                           
113300     MOVE SPACE                  TO   OBKR-BEERS                          
113400     MOVE SPACE                  TO   OBKR-IDBIL                          
113500     MOVE PHUV-BEKUNDRF          TO   OBKR-BEKUNDRF                       
113600     MOVE PRAD-BERADREF          TO   OBKR-BERADREF                       
113700     MOVE SPACE                  TO   OBKR-BEVOLREF                       
113800     MOVE ZERO                   TO   OBKR-IDKAMPRF                       
113900     MOVE ZERO                   TO   OBKR-DIERS-KVOT                     
114000     MOVE NEJ                    TO   OBKR-FLAKPLOC                       
114100     MOVE PRAD-FLINVEST          TO   OBKR-FLINVEST                       
114200     MOVE JA                     TO   OBKR-FLOBOK                         
114300     MOVE NEJ                    TO   OBKR-FLOBTRAN                       
114400     MOVE NEJ                    TO   OBKR-FLOBPRT                        
114500     MOVE PRAD-FLPRTILL          TO   OBKR-FLPRTILL                       
114600     MOVE PRAD-FLRESTN           TO   OBKR-FLRESTN                        
114700     MOVE JA                     TO   OBKR-FLSLATT                        
114800     MOVE NEJ                    TO   OBKR-FLTILLK                        
114900     MOVE ZERO                   TO   OBKR-IDARTNR-TILLK                  
115000     MOVE PHUV-IDDISTR           TO   OBKR-IDDISTR                        
115100     MOVE PHUV-IDKUNDNR          TO   OBKR-IDKUNDNR                       
115200     MOVE PHUV-IDKUNDRF          TO   OBKR-IDKUNDRF                       
115300     MOVE '0000000   '           TO   OBKR-IDKUNDRF-RO                    
115400     MOVE PRAD-IDLEVNR           TO   OBKR-IDLEVNR                        
115500     MOVE ZERO                   TO   OBKR-IDLOPNR-RO                     
115600     MOVE PRAD-IDSYSTEM          TO   OBKR-IDSYSTEM                       
115700     MOVE ZERO                   TO   OBKR-KDDSP                          
115800     MOVE ZERO                   TO   OBKR-KDERS                          
115900     MOVE PRAD-KDKVBRYT          TO   OBKR-KDKVBRYT                       
116000     MOVE PRAD-KDPRTYP           TO   OBKR-KDPRTYP                        
116100     MOVE PRAD-KDTPOTYP          TO   OBKR-KDTPOTYP                       
116200     MOVE ZERO                   TO   OBKR-KDVRINFO                       
116300     MOVE PRAD-KVBEART-Q         TO   OBKR-KVANNANT                       
116400     MOVE ZERO                   TO   OBKR-KVAVBART                       
116500     MOVE PRAD-KVBEART           TO   OBKR-KVBEART                        
116600     MOVE PRAD-KVBEART-Q         TO   OBKR-KVBEART-Q                      
116700     MOVE ZERO                   TO   OBKR-KVBEART-TILLK                  
116800     MOVE ZERO                   TO   OBKR-KVPREAVB                       
116900     MOVE ZERO                   TO   OBKR-KVPRERO                        
117000     MOVE ZERO                   TO   OBKR-KVQPACK                        
117100     MOVE ZERO                   TO   OBKR-KVRO                           
117200     MOVE ZERO                   TO   OBKR-KVSLATT                        
117300     MOVE PRAD-PRARTNTO          TO   OBKR-PRARTNTO                       
117400     MOVE PRAD-DEAL-PR-LINE      TO   OBKR-DEAL-PR-LINE                   
117500     MOVE PRAD-PRBPRIS           TO   OBKR-PRBPRIS                        
117600     MOVE PRAD-REKSIFFR          TO   OBKR-REKSIFFR                       
117700     MOVE ZERO                   TO   OBKR-REKSIFFR-TILLK                 
117800     MOVE ZERO                   TO   OBKR-RERF-RAD                       
117900     MOVE ZERO                   TO   OBKR-TIDISPIN                       
118000     MOVE PHUV-TIREGDAT          TO   OBKR-TIORDREG                       
118100     MOVE PRAD-TIPRIS            TO   OBKR-TIPRIS                         
118200     MOVE WS-DATUM               TO   OBKR-TIREGDAT                       
118300     MOVE WS-TIHHMMSS            TO   OBKR-TIREGTID                       
118400     MOVE ZERO                   TO   OBKR-TIRODAT                        
118500                                                                          
118600     MOVE PHUV-TIREGDAT TO WS-AAMMDD                                      
118700     IF WS-AAMMDD(1:2) < 50                                               
118800       MOVE 20                 TO WS-CENTURY                              
118900     ELSE                                                                 
119000       MOVE 19                 TO WS-CENTURY                              
119100     END-IF                                                               
119200     COMPUTE OBKR-TITIORDD-9KOMPL = WS-9KOMPL - WS-9KOMPL-DATUM           
119300                                                                          
119400     MOVE ZERO                   TO   OBKR-TITPO                          
119500                                                                          
119600     MOVE WS-DATUM TO WS-AAMMDD                                           
119700     IF WS-AAMMDD(1:2) < 50                                               
119800       MOVE 20                 TO WS-CENTURY                              
119900     ELSE                                                                 
120000       MOVE 19                 TO WS-CENTURY                              
120100     END-IF                                                               
120200     COMPUTE OBKR-TITIREGD-9KOMPL = WS-9KOMPL - WS-9KOMPL-DATUM           
120300                                                                          
120400     MOVE PHUV-KDFRAKT           TO   OBKR-KDFRAKT                        
120500     MOVE PHUV-KDORDKL           TO   OBKR-KDORDKL                        
120600     MOVE 83                     TO   OBKR-KDORDBEK                       
120700     MOVE IDPGM                  TO   OBKR-IDPGM                          
120800                                                                          
120900     MOVE SPACE                  TO OBKR-KDORDTYP-LDC                     
121000     MOVE ZERO                   TO OBKR-TIREPDAT                         
121100     MOVE SPACE                  TO OBKR-IDKUNDRF-WIP                     
121210     MOVE ZERO                   TO OBKR-TIDLEVDAT                        
121220     MOVE +0                     TO OBKR-PRAVCOST                         
121300                                                                          
121400     PERFORM IMS-ISRT-ORQM-WLORQM01                                       
121500                                                                          
121600     PERFORM UNTIL SEGMENT-FINNS                                          
121700        ADD 1 TO OBKR-IDSEKVNR                                            
121800        PERFORM IMS-ISRT-ORQM-WLORQM01                                    
121900     END-PERFORM                                                          
122000     .                                                                    
122100     EJECT                                                                
122200 HAACA-KOLLA-LOPNR-OBKR SECTION.                                          
122300                                                                          
122400     MOVE PHUV-IDORDER         TO   W-ORQM-MIN-IDORDER                    
122500                                    W-ORQM-MAX-IDORDER                    
122600     MOVE PRAD-IDARTNR         TO   W-ORQM-MIN-IDARTNR                    
122700                                    W-ORQM-MAX-IDARTNR                    
122800     MOVE +1                   TO   W-ORQM-MIN-IDLOPNR                    
122900                                    W-ORQM-MAX-IDLOPNR                    
123000     MOVE +1                   TO   W-ORQM-MIN-IDSEKVNR                   
123100                                    W-ORQM-MAX-IDSEKVNR                   
123200     PERFORM IMS-GU-ORQM-WLORQM01                                         
123300                                                                          
123400     PERFORM UNTIL SEGMENT-SAKNAS                                         
123500        ADD 1 TO  W-ORQM-MIN-IDLOPNR                                      
123600                  W-ORQM-MAX-IDLOPNR                                      
123700        PERFORM IMS-GU-ORQM-WLORQM01                                      
123800     END-PERFORM                                                          
123900     .                                                                    
124000     EJECT                                                                
124100 HAB-AENDRA-RAD SECTION.                                                  
124200                                                                          
124300     PERFORM HABA-AENDRA-KVBEART-Q                                        
124400                                                                          
124500     PERFORM HABB-AENDRA-PRARTNTO                                         
124600                                                                          
124700     PERFORM HABC-AENDRA-BEART                                            
124800                                                                          
124900     PERFORM HABD-AENDRA-BERADREF                                         
125000                                                                          
125100     PERFORM HABE-UPPDAT-VAERDE-VIKT-VOLYM                                
125200                                                                          
125300     PERFORM IMS-REPL-PROD-WLPROD01                                       
125400     .                                                                    
125500     EJECT                                                                
125600 HABA-AENDRA-KVBEART-Q SECTION.                                           
125700                                                                          
125800     MOVE 0              TO WS-KVOFFERT                                   
125900     MOVE PRAD-KVBEART-Q TO WS-KVBEART-Q-URS                              
126000                                                                          
126100     IF MID-KVBEART-Q-U (INDX) NOT = ALL '+'                              
126200        COMPUTE WS-KVOFFERT = PRAD-KVBEART-Q - WS-KVBEART-Q (INDX)        
126300        IF WS-KVOFFERT > 0                                                
126400           IF PHUV-KDPROTYP = 'O' OR 'L'                                  
126500              PERFORM S03-UPPDAT-WLARTM01                                 
126600           END-IF                                                         
126700        END-IF                                                            
126800        MOVE WS-KVBEART-Q (INDX) TO PRAD-KVBEART-Q                        
126900                                    PRAD-KVBEART                          
127000     END-IF                                                               
127100     .                                                                    
127200     EJECT                                                                
127300 HABB-AENDRA-PRARTNTO SECTION.                                            
127400                                                                          
127500     MOVE PRAD-PRARTNTO TO WS-PRARTNTO-URS                                
127600     MOVE PRAD-PRARTNTO-LOC TO WS-PRARTNTO-URS-LOC                        
127700     MOVE PRAD-PRARTNTO-LOCPREL TO WS-PRARTNTO-URS-LOCPREL                
127800                                                                          
127900     IF MID-PRARTNTO-U (INDX) NOT = ALL '+'                               
128000        IF DIST79-DEALER-PRICE                                            
128100          MOVE WS-PRARTNTO(INDX)  TO PRAD-PRARTNTO-LOC                    
128200          MOVE +0                 TO PRAD-PRARTNTO-LOCPREL                
128300        ELSE                                                              
128400          MOVE WS-PRARTNTO (INDX) TO PRAD-PRARTNTO                        
128500        END-IF                                                            
128600        MOVE 'P'                TO PRAD-KDPRTYP                           
128700        MOVE WS-DATUM           TO PRAD-TIPRIS                            
128800        MOVE NEJ                TO PRAD-FLPRTILL                          
128900     END-IF                                                               
129000     .                                                                    
129100     EJECT                                                                
129200 HABC-AENDRA-BEART SECTION.                                               
129300                                                                          
129400     IF MID-BEART-U (INDX) NOT = ALL '+'                                  
129500        MOVE MID-BEART-U (INDX) TO PRAD-BEART                             
129600     END-IF                                                               
129700     .                                                                    
129800     EJECT                                                                
129900 HABD-AENDRA-BERADREF SECTION.                                            
130000                                                                          
130100     IF MID-BERADREF-U (INDX) NOT = ALL '+'                               
130200        MOVE MID-BERADREF-U (INDX) TO PRAD-BERADREF                       
130300     END-IF                                                               
130400     .                                                                    
130500     EJECT                                                                
130600 HABE-UPPDAT-VAERDE-VIKT-VOLYM SECTION.                                   
130700                                                                          
130800     IF WS-KVOFFERT > 0                                                   
130900        COMPUTE WS-VKORDNTO =                                             
131000               ((WS-KVOFFERT * PRAD-VKART) / 1000)                        
131100        COMPUTE WS-VLORDNTO =                                             
131200               ((WS-KVOFFERT * PRAD-VLARTNTO) / 1000000)                  
131300        SUBTRACT WS-VKORDNTO   FROM PHUV-VKORDNTO                         
131400        SUBTRACT WS-VLORDNTO   FROM PHUV-VLORDBTO                         
131500     END-IF                                                               
131600                                                                          
131700     IF DIST79-DEALER-PRICE                                               
131800       COMPUTE WS-SUORDV-URS-LOC = WS-KVBEART-Q-URS *                     
131900                                   WS-PRARTNTO-URS-LOC                    
132000       COMPUTE WS-SUORDV-LOC     = PRAD-KVBEART-Q   *                     
132100                                   PRAD-PRARTNTO-LOC                      
132200                                                                          
132300       IF WS-SUORDV-URS-LOC NOT = WS-SUORDV-LOC                           
132400         COMPUTE WS-SUORDV-DIFF-LOC = WS-SUORDV-URS-LOC -                 
132500                                      WS-SUORDV-LOC                       
132600         COMPUTE PHUV-SUORDV-LOC    = PHUV-SUORDV-LOC -                   
132700                                      WS-SUORDV-DIFF-LOC                  
132800       END-IF                                                             
132900                                                                          
133000       IF PHUV-SUORDV-LOC NEGATIVE                                        
133100         MOVE 0 TO PHUV-SUORDV-LOC                                        
133200       END-IF                                                             
133300       COMPUTE WS-SUORDV-URS-LOCPREL = WS-KVBEART-Q-URS *                 
133400                                       WS-PRARTNTO-URS-LOCPREL            
133500       COMPUTE WS-SUORDV-LOCPREL     = PRAD-KVBEART-Q   *                 
133600                                       PRAD-PRARTNTO-LOCPREL              
133700                                                                          
133800       IF WS-SUORDV-URS-LOCPREL NOT  = WS-SUORDV-LOCPREL                  
133900         COMPUTE WS-SUORDV-DIFF-LOCPREL = WS-SUORDV-URS-LOCPREL -         
134000                                          WS-SUORDV-LOCPREL               
134100         COMPUTE PHUV-SUORDV-LOCPREL = PHUV-SUORDV-LOCPREL -              
134200                                       WS-SUORDV-DIFF-LOCPREL             
134300       END-IF                                                             
134400                                                                          
134500       IF PHUV-SUORDV-LOCPREL NEGATIVE                                    
134600         MOVE 0 TO PHUV-SUORDV-LOCPREL                                    
134700       END-IF                                                             
134800     ELSE                                                                 
134900                                                                          
135000       COMPUTE WS-SUORDV-URS = WS-KVBEART-Q-URS * WS-PRARTNTO-URS         
135100       COMPUTE WS-SUORDV     = PRAD-KVBEART-Q    * PRAD-PRARTNTO          
135200                                                                          
135300       IF WS-SUORDV-URS NOT = WS-SUORDV                                   
135400         COMPUTE WS-SUORDV-DIFF = WS-SUORDV-URS - WS-SUORDV               
135500         COMPUTE PHUV-SUORDV = PHUV-SUORDV - WS-SUORDV-DIFF               
135600       END-IF                                                             
135700                                                                          
135800       IF PHUV-SUORDV NEGATIVE                                            
135900         MOVE 0 TO PHUV-SUORDV                                            
136000       END-IF                                                             
136100     END-IF                                                               
136200                                                                          
136300     IF PHUV-VKORDNTO NEGATIVE                                            
136400        MOVE 0 TO PHUV-VKORDNTO                                           
136500     END-IF                                                               
136600                                                                          
136700     IF PHUV-VLORDBTO NEGATIVE                                            
136800        MOVE 0 TO PHUV-VLORDBTO                                           
136900     END-IF                                                               
137000     .                                                                    
137100     EJECT                                                                
137200 HAC-UPPDAT-PROFORMAHUVUD SECTION.                                        
137300                                                                          
137400     MOVE WS-DATUM    TO PHUV-TIUPPDAT                                    
137500     MOVE WS-KLOCKAN  TO PHUV-TIUPPTID                                    
137600                                                                          
137700     COMPUTE PHUV-PREMBHNT ROUNDED =                                      
137800                        (((PHUV-SUORDV - PHUV-PRAVDRAG) *                 
137900                           PHUV-REEMBHNT) / 100)                          
138000     END-COMPUTE                                                          
138100                                                                          
138200     PERFORM IMS-REPL-PROC-WLPROC01                                       
138300     .                                                                    
138400     EJECT                                                                
138500 I-LAES-VISA-INFO SECTION.                                                
138600                                                                          
138700     MOVE +1 TO INDX                                                      
138800                                                                          
138900     PERFORM IMS-GU-PROD-WLPROD01                                         
139000                                                                          
139100     IF SEGMENT-FINNS                                                     
139200        MOVE INF-LAST-PAGE TO MED-IDMFSINF                                
139300        PERFORM S02-INFO-MEDDELANDE                                       
139400        PERFORM IA-SPAR-ENTER-NYCKLAR                                     
139500        PERFORM UNTIL INDX > MAX-INDX                                     
139600           IF SEGMENT-FINNS                                               
139700              PERFORM IB-FLYTTA-TILL-MOD                                  
139800              PERFORM IMS-GN-PROD-WLPROD01                                
139900           ELSE                                                           
140000              MOVE MFS-RENSA-FAELT TO MOD-CMD-UPDATE     (INDX)           
140100                                      MOD-IDARTNR        (INDX)           
140200                                      MOD-KVART          (INDX)           
140300                                      MOD-PRARTNTO       (INDX)           
140400                                      MOD-TEASTRIX       (INDX)           
140500                                      MOD-IDARTNR        (INDX)           
140600                                      MOD-BEART          (INDX)           
140700                                      MOD-BERADREF       (INDX)           
140800                                      MOD-KDKVBRYT       (INDX)           
140900                                      MOD-KVVECKOR-TPO5  (INDX)           
141000                                      MOD-KDTPOTYP       (INDX)           
141100                                      MOD-IDLOPNR        (INDX)           
141200                                      MOD-KVBEART-Q-U    (INDX)           
141300                                      MOD-PRARTNTO-U     (INDX)           
141400                                      MOD-BEART-U        (INDX)           
141500                                      MOD-BERADREF-U     (INDX)           
141600           END-IF                                                         
141700        ADD +1 TO INDX                                                    
141800        END-PERFORM                                                       
141900     ELSE                                                                 
142000        IF MFS-NEXT                                                       
142100           MOVE INF-LAST-PAGE-SHOW TO MED-IDMFSFEL                        
142200           PERFORM S01-FEL-MEDDELANDE                                     
142300        ELSE                                                              
142400           IF IDARTNR-IFYLLT                                              
142500              MOVE INF-PART-MISSING TO MED-IDMFSINF                       
142600              PERFORM S02-INFO-MEDDELANDE                                 
142700           ELSE                                                           
142800              MOVE ERR-LINES-MISSING TO MED-IDMFSINF                      
142900              PERFORM S02-INFO-MEDDELANDE                                 
143000           END-IF                                                         
143100        END-IF                                                            
143200        PERFORM MFS-RENSA-FAELT-IN                                        
143300        PERFORM MFS-RENSA-FAELT-UT                                        
143400     END-IF                                                               
143500                                                                          
143600     PERFORM IC-SPAR-NEXT-NYCKLAR                                         
143700                                                                          
143800     IF SEGMENT-FINNS                                                     
143900        IF NOT MFS-UPDATE                                                 
144000           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
144100           PERFORM S02-INFO-MEDDELANDE                                    
144200        END-IF                                                            
144300     ELSE                                                                 
144400        MOVE MFS-RENSA-FAELT         TO MOD-TEMFSFEL                      
144500     END-IF                                                               
144600     .                                                                    
144700     EJECT                                                                
144800 IA-SPAR-ENTER-NYCKLAR SECTION.                                           
144900                                                                          
145000     MOVE PRAD-IDARTNR  TO MOD-IDARTNR-ENTER                              
145100     MOVE PRAD-IDLOPNR  TO MOD-IDLOPNR-ENTER                              
145200     .                                                                    
145300     EJECT                                                                
145400 IB-FLYTTA-TILL-MOD SECTION.                                              
145500                                                                          
145600     MOVE PRAD-IDARTNR       TO MOD-IDARTNR         (INDX)                
145700     MOVE PRAD-KVBEART-Q     TO MOD-KVART           (INDX)                
145800     IF DIST79-DEALER-PRICE                                               
145900      IF PRAD-PRARTNTO-LOC > +0                                           
146000       MOVE PRAD-PRARTNTO-LOC     TO MOD-PRARTNTO   (INDX)                
146100       MOVE ' '                   TO MOD-TEASTRIX   (INDX)                
146200      ELSE                                                                
146300       MOVE PRAD-PRARTNTO-LOCPREL TO MOD-PRARTNTO   (INDX)                
146400       MOVE '*'                   TO MOD-TEASTRIX   (INDX)                
146500      END-IF                                                              
146600     ELSE                                                                 
146700       MOVE PRAD-PRARTNTO         TO MOD-PRARTNTO   (INDX)                
146800       MOVE ' '                   TO MOD-TEASTRIX   (INDX)                
146900     END-IF                                                               
147000     MOVE PRAD-BERADREF      TO MOD-BERADREF        (INDX)                
147100     MOVE PRAD-KDKVBRYT      TO MOD-KDKVBRYT        (INDX)                
147200     MOVE PRAD-KVVECKOR-TPO5 TO MOD-KVVECKOR-TPO5   (INDX)                
147300     MOVE PRAD-KDTPOTYP      TO MOD-KDTPOTYP        (INDX)                
147400     MOVE PRAD-IDLOPNR       TO MOD-IDLOPNR         (INDX)                
147500     IF PRAD-BEART = SPACE                                                
147600        PERFORM IBA-LAES-BENAEMNING                                       
147700     ELSE                                                                 
147800        MOVE PRAD-BEART      TO MOD-BEART           (INDX)                
147900     END-IF                                                               
148000     .                                                                    
148100     EJECT                                                                
148200 IBA-LAES-BENAEMNING SECTION.                                             
148300                                                                          
148400     MOVE PRAD-IDARTNR TO W-BENA-IDARTNR                                  
148500     MOVE PHUV-IDSKYLT TO W-BENA-IDSKYLT-X                                
148600                                                                          
148700     PERFORM IMS-GU-BENA-WLBENA11                                         
148800     IF SEGMENT-FINNS                                                     
148900        MOVE TEXT-BEART TO MOD-BEART (INDX)                               
149000     ELSE                                                                 
149100        MOVE SPACE      TO MOD-BEART (INDX)                               
149200     END-IF                                                               
149300     .                                                                    
149400     EJECT                                                                
149500 IC-SPAR-NEXT-NYCKLAR SECTION.                                            
149600                                                                          
149700     IF SEGMENT-FINNS                                                     
149800        MOVE PRAD-IDARTNR  TO MOD-IDARTNR-NEXT                            
149900        MOVE PRAD-IDLOPNR  TO MOD-IDLOPNR-NEXT                            
150000        PERFORM IMS-GU-PROD-WLPROD01                                      
150100     ELSE                                                                 
150200        MOVE 999999999     TO MOD-IDARTNR-NEXT                            
150300        MOVE 999           TO MOD-IDLOPNR-NEXT                            
150400     END-IF                                                               
150500     .                                                                    
150600     EJECT                                                                
150700 S01-FEL-MEDDELANDE SECTION.                                              
150800                                                                          
150900     CALL WMEDKONV USING MED-WMEDAREA                                     
151000     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
151100     .                                                                    
151200     EJECT                                                                
151300 S02-INFO-MEDDELANDE SECTION.                                             
151400                                                                          
151500     CALL WMEDKONV USING MED-WMEDAREA                                     
151600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
151700     .                                                                    
151800     EJECT                                                                
151900 S03-UPPDAT-WLARTM01 SECTION.                                             
152000                                                                          
152100     MOVE PRAD-IDARTNR TO W-ARTM-IDARTNR                                  
152200                                                                          
152300     PERFORM IMS-GHU-ARTM-WLARTM01                                        
152400                                                                          
152500     IF SEGMENT-FINNS                                                     
152600        COMPUTE ART-KVOFFERT =                                            
152700                ART-KVOFFERT - WS-KVOFFERT                                
152800        PERFORM IMS-REPL-ARTM-WLARTM01                                    
152900     END-IF                                                               
153000     .                                                                    
153100     EJECT                                                                
154800     .                                                                    
154900     EJECT                                                                
155000                                                                          
155100 MFS-RENSA-FAELT-UT SECTION.                                              
155200                                                                          
155300*    --- ALLA UTDATA-FÄLT                                                 
155400                                                                          
155500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ENTER                            
155600                             MOD-IDLOPNR-ENTER                            
155700                             MOD-IDARTNR-NEXT                             
155800                             MOD-IDLOPNR-NEXT                             
155900     MOVE +1 TO INDX                                                      
156000     PERFORM UNTIL INDX > MAX-INDX                                        
156100        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR        (INDX)                 
156200                                MOD-KVART          (INDX)                 
156300                                MOD-PRARTNTO       (INDX)                 
156400                                MOD-BEART          (INDX)                 
156500                                MOD-BERADREF       (INDX)                 
156600                                MOD-KDKVBRYT       (INDX)                 
156700                                MOD-KVVECKOR-TPO5  (INDX)                 
156800                                MOD-KDTPOTYP       (INDX)                 
156900                                MOD-IDLOPNR        (INDX)                 
157000        ADD +1 TO INDX                                                    
157100     END-PERFORM                                                          
157200     .                                                                    
157300     EJECT                                                                
157400 MFS-RENSA-FAELT-IN SECTION.                                              
157500                                                                          
157600*    --- ALLA INDATA-FÄLT                                                 
157700                                                                          
157800     MOVE +1 TO INDX                                                      
157900     PERFORM UNTIL INDX > MAX-INDX                                        
158000        MOVE MFS-RENSA-FAELT TO MOD-CMD-UPDATE  (INDX)                    
158100                                MOD-KVBEART-Q-U (INDX)                    
158200                                MOD-PRARTNTO-U  (INDX)                    
158300                                MOD-BEART-U     (INDX)                    
158400                                MOD-BERADREF-U  (INDX)                    
158500        ADD +1 TO INDX                                                    
158600     END-PERFORM                                                          
158700     .                                                                    
158800     EJECT                                                                
158900 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
159000                                                                          
159100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-ENTER                          
159200                               MOD-IDLOPNR-ENTER                          
159300                               MOD-IDARTNR-NEXT                           
159400                               MOD-IDLOPNR-NEXT                           
159500     MOVE +1 TO INDX                                                      
159600     PERFORM UNTIL INDX > MAX-INDX                                        
159700        MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR        (INDX)               
159800                                  MOD-KVART          (INDX)               
159900                                  MOD-PRARTNTO       (INDX)               
160000                                  MOD-KDKVBRYT       (INDX)               
160100                                  MOD-BEART          (INDX)               
160200                                  MOD-BERADREF       (INDX)               
160300                                  MOD-KVVECKOR-TPO5  (INDX)               
160400                                  MOD-KDTPOTYP       (INDX)               
160500                                  MOD-IDLOPNR        (INDX)               
160600        ADD +1 TO INDX                                                    
160700     END-PERFORM                                                          
160800     .                                                                    
160900     EJECT                                                                
161000 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
161100                                                                          
161200     MOVE +1 TO INDX                                                      
161300     PERFORM UNTIL INDX > MAX-INDX                                        
161400        MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-UPDATE  (INDX)                  
161500                                  MOD-KVBEART-Q-U (INDX)                  
161600                                  MOD-PRARTNTO-U  (INDX)                  
161700                                  MOD-BEART-U     (INDX)                  
161800                                  MOD-BERADREF-U  (INDX)                  
161900        ADD +1 TO INDX                                                    
162000     END-PERFORM                                                          
162100     .                                                                    
162200     EJECT                                                                
162300 MFS-FORM-ATTR SECTION.                                                   
162400                                                                          
162500     MOVE +1 TO INDX                                                      
162600     PERFORM UNTIL INDX > MAX-INDX                                        
162700        MOVE MFS-FORMATETS-ATTR TO MOD-CMD-UPDATE-ATTR  (INDX)            
162800                                   MOD-KVBEART-Q-U-ATTR (INDX)            
162900                                   MOD-PRARTNTO-U-ATTR  (INDX)            
163000                                   MOD-BEART-U-ATTR     (INDX)            
163100                                   MOD-BERADREF-U-ATTR  (INDX)            
163200        ADD +1 TO INDX                                                    
163300     END-PERFORM                                                          
163400     .                                                                    
163500     SKIP2                                                                
163600 MFS-LAS-IN-IGEN SECTION.                                                 
163700                                                                          
163800     MOVE +1 TO INDX                                                      
163900     PERFORM UNTIL INDX > MAX-INDX                                        
164000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-UPDATE-ATTR  (INDX)         
164100                                      MOD-KVBEART-Q-U-ATTR (INDX)         
164200                                      MOD-PRARTNTO-U-ATTR  (INDX)         
164300                                      MOD-BEART-U-ATTR     (INDX)         
164400                                      MOD-BERADREF-U-ATTR  (INDX)         
164500        ADD +1 TO INDX                                                    
164600     END-PERFORM                                                          
164700     .                                                                    
164800     EJECT                                                                
164900* --- IMS SEKTIONER ---                                                   
165000     SKIP3                                                                
165100 IMS-GET-MSG SECTION.                                                     
165200                                                                          
165300     MOVE '  QC' TO GODK-STATUSKODER                                      
165400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
165500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
165600     PERFORM IMS-STATUSKONTROLL                                           
165700     .                                                                    
165800     SKIP3                                                                
165900 IMS-INSERT-MSG SECTION.                                                  
166000                                                                          
166100     IF ENGLISH-TEXT                                                      
166200       MOVE 'N' TO MFS-KDHUVOMR                                           
166300     END-IF                                                               
166400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
166500     MOVE SPACE TO GODK-STATUSKODER                                       
166600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
166700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
166800     PERFORM IMS-STATUSKONTROLL                                           
166900     .                                                                    
167000     EJECT                                                                
167100 IMS-GHU-PROC-WLPROC01 SECTION.                                           
167200                                                                          
167300     STRING 'WLPROC01(WDE801KY =' W-PROC-WDE801KY-X ')'                   
167400          DELIMITED BY SIZE INTO SSA1                                     
167500     MOVE '  GE' TO GODK-STATUSKODER                                      
167600     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-PROC01 SSA1                   
167700     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
167800     PERFORM IMS-STATUSKONTROLL                                           
167900     .                                                                    
168000                                                                          
168100 IMS-REPL-PROC-WLPROC01 SECTION.                                          
168200                                                                          
168300     MOVE '    ' TO GODK-STATUSKODER                                      
168400     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-PROC01                       
168500     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
168600     PERFORM IMS-STATUSKONTROLL                                           
168700     .                                                                    
168800     EJECT                                                                
168900 IMS-GN-PROD-WLPROD01 SECTION.                                            
169000                                                                          
169100     STRING 'WLPROD01(WDE901KY>=' W-PROD-WDE901KY-MIN-X                   
169200                    '&WDE901KY<=' W-PROD-WDE901KY-MAX-X ')'               
169300          DELIMITED BY SIZE INTO SSA1                                     
169400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
169500     CALL CBLTDLI USING GN PROD-PCB DLI-IO-PROD01 SSA1                    
169600     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
169700     PERFORM IMS-STATUSKONTROLL                                           
169800     .                                                                    
169900                                                                          
170000 IMS-GU-PROD-WLPROD01 SECTION.                                            
170100                                                                          
170200     STRING 'WLPROD01(WDE901KY>=' W-PROD-WDE901KY-MIN-X                   
170300                    '&WDE901KY<=' W-PROD-WDE901KY-MAX-X ')'               
170400          DELIMITED BY SIZE INTO SSA1                                     
170500     MOVE '  GE'   TO GODK-STATUSKODER                                    
170600     CALL CBLTDLI USING GU PROD-PCB DLI-IO-PROD01 SSA1                    
170700     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
170800     PERFORM IMS-STATUSKONTROLL                                           
170900     .                                                                    
171000                                                                          
171100 IMS-GHU-PROD-WLPROD01 SECTION.                                           
171200                                                                          
171300     STRING 'WLPROD01(WDE901KY =' W-PROD-WDE901KY-X ')'                   
171400          DELIMITED BY SIZE INTO SSA1                                     
171500     MOVE '  GE' TO GODK-STATUSKODER                                      
171600     CALL CBLTDLI USING GHU PROD-PCB DLI-IO-PROD01 SSA1                   
171700     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
171800     PERFORM IMS-STATUSKONTROLL                                           
171900     .                                                                    
172000     EJECT                                                                
172100 IMS-REPL-PROD-WLPROD01 SECTION.                                          
172200                                                                          
172300     MOVE '    ' TO GODK-STATUSKODER                                      
172400     CALL CBLTDLI USING REPL PROD-PCB DLI-IO-PROD01                       
172500     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
172600     PERFORM IMS-STATUSKONTROLL                                           
172700     .                                                                    
172800                                                                          
172900 IMS-DLET-PROD-WLPROD01 SECTION.                                          
173000                                                                          
173100     MOVE '    ' TO GODK-STATUSKODER                                      
173200     CALL CBLTDLI USING DLET PROD-PCB DLI-IO-PROD01                       
173300     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
173400     PERFORM IMS-STATUSKONTROLL                                           
173500     .                                                                    
173600     EJECT                                                                
173700 IMS-GU-BENA-WLBENA11 SECTION.                                            
173800                                                                          
173900     STRING 'WLBENA01(WDD3BSEQ =' W-BENA-IDARTNR-X ')'                    
174000          DELIMITED BY SIZE INTO SSA1                                     
174100     STRING 'WLBENA11(IDSKYLT  =' W-BENA-IDSKYLT-X ')'                    
174200          DELIMITED BY SIZE INTO SSA2                                     
174300     MOVE '  GE' TO GODK-STATUSKODER                                      
174400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1 SSA2               
174500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
174600     PERFORM IMS-STATUSKONTROLL                                           
174700     .                                                                    
174800     EJECT                                                                
174900 IMS-GU-ARTC-WLARTC01 SECTION.                                            
175000                                                                          
175100     STRING 'WLARTC01(IDARTNR  =' W-ARTC-IDARTNR-X ')'                    
175200          DELIMITED BY SIZE INTO SSA1                                     
175300     MOVE '  GE' TO GODK-STATUSKODER                                      
175400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC01 SSA1                    
175500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
175600     PERFORM IMS-STATUSKONTROLL                                           
175700     .                                                                    
175800     EJECT                                                                
175900 IMS-GHU-ARTM-WLARTM01 SECTION.                                           
176000                                                                          
176100     STRING 'WLARTM01(IDARTNR  =' W-ARTM-IDARTNR-X ')'                    
176200          DELIMITED BY SIZE INTO SSA1                                     
176300     MOVE '  GE' TO GODK-STATUSKODER                                      
176400     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-ARTM01 SSA1                   
176500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
176600     PERFORM IMS-STATUSKONTROLL                                           
176700     .                                                                    
176800                                                                          
176900 IMS-REPL-ARTM-WLARTM01 SECTION.                                          
177000                                                                          
177100     MOVE '  ' TO GODK-STATUSKODER                                        
177200     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-ARTM01                       
177300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
177400     PERFORM IMS-STATUSKONTROLL                                           
177500     .                                                                    
177600     EJECT                                                                
177700 IMS-GU-ORQM-WLORQM01 SECTION.                                            
177800                                                                          
177900     STRING 'WLORQM01(WDQ101KY>=' W-ORQM-WDQ101KY-MIN-X                   
178000                    '&WDQ101KY<=' W-ORQM-WDQ101KY-MAX-X ')'               
178100          DELIMITED BY SIZE INTO SSA1                                     
178200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
178300     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-ORQM01 SSA1                    
178400     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
178500     PERFORM IMS-STATUSKONTROLL                                           
178600     .                                                                    
178700                                                                          
178800 IMS-ISRT-ORQM-WLORQM01 SECTION.                                          
178900                                                                          
179000     MOVE 'WLORQM01 ' TO SSA1                                             
179100     MOVE '  II' TO GODK-STATUSKODER                                      
179200     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-ORQM01 SSA1                  
179300     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
179400     PERFORM IMS-STATUSKONTROLL                                           
179500     .                                                                    
179600     EJECT                                                                
179700 IMS-GU-WDB201 SECTION.                                                   
179800                                                                          
179900     STRING 'WDB201  (IDGMT    =' W-IDGMT-WDB2-X ')'                      
180000          DELIMITED BY SIZE INTO SSA1                                     
180100     MOVE '  GE'               TO GODK-STATUSKODER                        
180200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
180300     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
180400     PERFORM IMS-STATUSKONTROLL                                           
180500     .                                                                    
180600     SKIP3                                                                
180700 IMS-GET-WDB201 SECTION.                                                  
180800                                                                          
180900     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
181000                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
181100            DELIMITED BY SIZE INTO SSA1                                   
181200     MOVE '  GE'               TO GODK-STATUSKODER                        
181300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
181400     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
181500     PERFORM IMS-STATUSKONTROLL                                           
181600     .                                                                    
181700     SKIP2                                                                
181800 IMS-GU-WDB101 SECTION.                                                   
181900                                                                          
182000     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
182100          DELIMITED BY SIZE INTO SSA1                                     
182200     MOVE '  GE'               TO GODK-STATUSKODER                        
182300     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
182400     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
182500     PERFORM IMS-STATUSKONTROLL                                           
182600     .                                                                    
182700     SKIP2                                                                
182800 IMS-STATUSKONTROLL SECTION.                                              
182900                                                                          
183000     SET STATUS-IX TO 1                                                   
183100     SEARCH GODK-STATUS                                                   
183200       AT END CALL FELLOG                                                 
183300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
183400     END-SEARCH                                                           
183500     .                                                                    
183600     EJECT                                                                
183700*    -COPY WY2000P1                                                       
