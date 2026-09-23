000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL010100.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   03/10/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN:CARPARTS.LDC.WL0101                                             
000800*    WEB-LDC: WL010100 PROGRAM IS A REPLICA OF W6030100 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        LDC LOSSNING                                                     
001300*        BETÄLLNING AV LISTA SAMTLIGA KOLLIN I FAKTURA                    
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: WL0101U                                             
001700*                     WL0101X                                             
001800*                     WLA101                                              
001900*        REQUEST:     WZ01REQU                                            
002000*                     WL0101I1                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        RESPONSE:    WZ01RESP / WZ01RES2                                 
002400*                     WL0101O1 / WL0101O2                                 
002500*                                                                         
002600*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
002700*                              WLINLC (WDL6)                              
002800*                              WL6301 (WDR5)                              
002900*                              WLLOGA (WDL9)                              
003000*                              WDR8                                       
003100*                              WDR9                                       
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500*   -COPY WY2000W1                                                        
004600*   -COPY WWDC99                                                          
004700 77  IDPGM                       PIC X(08)   VALUE 'WL010100'.            
004800 77  WS-ADRESS-DP                PIC X(50)                                
004900         VALUE 'CARPARTS.DAP.DISTRDOCWEB'.                                
005000 77  WS-ADDRESS-WHSTOCKA          PIC X(50)                               
005100         VALUE 'CARPARTS.PULS.WHSTOCKADJ'.                                
005200 77  WS-ADDRESS-MQASYNC           PIC X(50)                               
005300       VALUE 'CARPARTS.PULS.MQASYNC'.                                     
005400 77  WS-RESP-AREA                PIC S9(5) VALUE ZERO COMP-3.             
005500 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
005600 77  KDRC-DISPLAY                PIC Z(5).                                
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  YES                         PIC X       VALUE 'Y'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000 77  SW-REQU-RAD-IFYLLD          PIC X       VALUE 'N'.                   
006100 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
006200 77  SW-TRAEFF                   PIC X       VALUE 'N'.                   
006300 77  SW-IDTRACK-OLD              PIC X       VALUE 'N'.                   
006400 77  WS-TINL-IDTRACK             PIC X(25)   VALUE SPACE.                 
006500 77  WS-FROM-PARA                PIC X(4)    VALUE SPACE.                 
006600 77  SW-REC-UPPD                 PIC X       VALUE 'N'.                   
006700 77  WS-KVANT                    PIC S9(3)   VALUE ZERO COMP-3.           
006800 77  WS-ADRESS                   PIC X(50)                                
006900            VALUE 'CARPARTS.LDC.DCGOODSRECEIVING'.                        
007000 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
007100 77  INDX-DISPLAY                PIC 9999    VALUE ZERO.                  
007200 77  MAX-INDX                    PIC S9(4)   VALUE 500  COMP SYNC.        
007300 77  MSG-IX                      PIC S9(9)   VALUE +0  COMP SYNC.         
007400 77  WS-IDDC-SEND-KEY            PIC X(2)    VALUE SPACE.                 
007500 77  WS-IDDC-REC                 PIC X(2)    VALUE SPACE.                 
007600 77  WS-FLTRACK                  PIC X(1)    VALUE 'N'.                   
007700 77  WS-TIFAKT                   PIC X(6).                                
007800 77  TIFAKT-WS REDEFINES WS-TIFAKT PIC 9(6).                              
007900                                                                          
008000 77  WS-SAP-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
008100 77  WS-SAP-TTMMSSTH             PIC 9(8)    VALUE ZERO.                  
008200 77  WS-SAP-PRAVCOST             PIC S9(7)V99 VALUE ZERO COMP-3.          
008300 77  WS-SAP-IDFAKT               PIC 9(7)  VALUE ZERO.                    
008400 77  WS-SAP-X-IDFAKT             PIC X(7)  VALUE ZERO.                    
008500 77  WS-SAP-IDDISTR              PIC S9(5)  VALUE ZERO COMP-3.            
008600 77  WS-SAP-IDKUNDNR             PIC S9(7)  VALUE ZERO COMP-3.            
008700 77  WS-SAP-PRARTNTO             PIC S9(7)V99 VALUE ZERO COMP-3.          
008800 77  WS-SAP-PRARTSTD             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
008900 77  W-IDSEKVNR-SAP              PIC S9(3) VALUE 0   COMP-3.              
009000 77  NOLL-RAKNARE                PIC S9(5)   VALUE ZERO COMP-3.           
009100 77  W-ANTAL-LASN                PIC S9(7)   VALUE ZERO.                  
009200 77  W-SPAR-IDKUNDRF             PIC X(10)   VALUE SPACE.                 
009300 77  W-SPAR-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.           
009400 77  W-SPAR-IDKOLLI              PIC S9(5)   VALUE ZERO COMP-3.           
009500 77  W-KVBO                      PIC S9(7)   VALUE ZERO.                  
009600 77  W-KVRADER                   PIC S9(7)   VALUE ZERO.                  
009700 77  W-KVNYART                   PIC S9(7)   VALUE ZERO.                  
009800 77  W-KVPRIOART                 PIC S9(7)   VALUE ZERO.                  
009900 77  WS-KVROS                    PIC S9(7)            VALUE ZERO.         
010000 77  ETA-IDKUNDNR                PIC 9(7)  VALUE ZERO.                    
010100 77  ETA-IDKUNDNR-SORD           PIC 9(7)  VALUE ZERO.                    
010200 77  ETA-IDKUNDNR-SBPS           PIC 9(7)  VALUE ZERO.                    
010300 77  W-IDARTNR-Z                 PIC Z(9).                                
010400 77  W-IDORDER                   PIC 9(7)  VALUE ZERO.                    
010500 77  W-IDKOLLI                   PIC 9(5)  VALUE ZERO.                    
010600                                                                          
010700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010800 01  FILLER REDEFINES DAGENS-DATUM.                                       
010900     03  DAGENS-AA               PIC 9(2).                                
011000     03  DAGENS-MM               PIC 9(2).                                
011100     03  DAGENS-DD               PIC 9(2).                                
011200                                                                          
011300 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
011400                                                                          
011500 77  LAND-SW                     PIC X       VALUE 'J'.                   
011600     88  AKTUELLT-LAND-SVERIGE               VALUE 'J'.                   
011700     88  AKTUELLT-EJ-SVERIGE                 VALUE 'N'.                   
011800                                                                          
011900 77  WS-TRACKID-MISSING          PIC X       VALUE 'N'.                   
012000     88  TRACKID-MISSING                     VALUE 'J'.                   
012100     88  NOT-TRACKID-MISSING                 VALUE 'N'.                   
012200                                                                          
012300 77  WS-WRITE-IDTRACK            PIC X       VALUE 'N'.                   
012400     88  WRITE-IDTRACK                       VALUE 'J'.                   
012500     88  NOT-WRITE-IDTRACK                   VALUE 'N'.                   
012600                                                                          
012700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
012800     88  INDATA-OK                           VALUE 'J'.                   
012900     88  INDATA-FEL                          VALUE 'N'.                   
013000                                                                          
013100 77  INLEV-SW                    PIC X       VALUE 'J'.                   
013200     88  INLEV-JA                            VALUE 'J'.                   
013300     88  INLEV-NEJ                           VALUE 'N'.                   
013400                                                                          
013500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013600     88  NYCKLAR-OK                          VALUE 'J'.                   
013700     88  NYCKLAR-FEL                         VALUE 'N'.                   
013800                                                                          
013900 77  ATERHOPP-SW                 PIC X       VALUE 'N'.                   
014000     88  ATERHOPP                            VALUE 'J'.                   
014100     88  EJ-ATERHOPP                         VALUE 'N'.                   
014200                                                                          
014300 77  PRINT-SW                    PIC X       VALUE 'N'.                   
014400     88  PRINT                               VALUE 'J'.                   
014500     88  EJ-PRINT                            VALUE 'N'.                   
014600                                                                          
014700 77  FORSTA-POST-SW              PIC X       VALUE 'J'.                   
014800     88  FORSTA-POST                         VALUE 'J'.                   
014900     88  EJ-FORSTA-POST                      VALUE 'N'.                   
015000                                                                          
015100 77  ETA-UPD-SW                  PIC X       VALUE 'J'.                   
015200     88  ETA-UPD-OK                          VALUE 'J'.                   
015300     88  ETA-UPD-NOT-OK                      VALUE 'N'.                   
015400                                                                          
015500 77  FIRST-REC-TRANS-SW          PIC X       VALUE 'N'.                   
015600     88  NOT-FIRST-REC-TRANS                 VALUE 'N'.                   
015700     88  FIRST-REC-TRANS                     VALUE 'J'.                   
015800                                                                          
015900 01  WLOGG-TID                   PIC S9(9)   VALUE ZERO.                  
016000 01  LOGG-DATUM                  PIC S9(8)   VALUE ZERO.                  
016100                                                                          
016200 01  LOCAL-DATE                  PIC 9(6)    VALUE ZERO.                  
016300 01  FILLER REDEFINES LOCAL-DATE.                                         
016400     03  LOCAL-AA                PIC 9(2).                                
016500     03  LOCAL-MM                PIC 9(2).                                
016600     03  LOCAL-DD                PIC 9(2).                                
016700                                                                          
016800 77  LOCAL-TIME                  PIC 9(8)    VALUE ZERO.                  
016900                                                                          
017000                                                                          
017100 01  W.                                                                   
017200     05  W-KVANT-UPD             PIC S9(3).                               
017300     05  W-KVAVIS                PIC S9(7)   VALUE ZERO COMP-3.           
017400     05  W-CMD                   PIC X(3).                                
017500                                                                          
017600 01  TABENTRY-PARM.                                                       
017700     03  STEGLAANGD              PIC S9(9) COMP.                          
017800     03  ANTAL                   PIC S9(9) COMP.                          
017900     03  NYCKELLAANGD            PIC S9(9) COMP.                          
018000                                                                          
018100 01  TAB-RAD-IX                 PIC 9(3).                                 
018200 01  MAX-TAB-RAD-IX             PIC 9(3)  VALUE 500.                      
018300 01  SORT-TABELL.                                                         
018400     03  TAB-RAD OCCURS 500.                                              
018500        05  TAB-SORTBEGREPP.                                              
018600            07 TAB-KVANTAL-SORT PIC S9(7) COMP-3.                         
018700            07 TAB-IDKUNDRF     PIC X(10).                                
018800            07 TAB-IDKUNDNR     PIC S9(7) COMP-3.                         
018900            07 TAB-IDKOLLI      PIC S9(5) COMP-3.                         
019000        05  TAB-KVANTAL-PRIO    PIC S9(7) COMP-3.                         
019100        05  TAB-IDUSER          PIC X(8).                                 
019200        05  TAB-IDDC            PIC X(2).                                 
019300        05  TAB-IDFAKT          PIC S9(7) COMP-3.                         
019400        05  TAB-IDLBBET         PIC X(12).                                
019500        05  TAB-KVANTAL-KOLLI   PIC S9(7) COMP-3.                         
019600        05  TAB-KVANTAL-NEW     PIC S9(7) COMP-3.                         
019700        05  TAB-KVANTAL-BO      PIC S9(7) COMP-3.                         
019800        05  TAB-TEINFO          PIC X(7).                                 
019900        05  TAB-DABERANK        PIC 9(8).                                 
020000                                                                          
020100                                                                          
020200*    --- PARAMETRAR TILL ABEND                                            
020300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
020400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
020500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
020600     EJECT                                                                
020700 01  SUBPROGRAM.                                                          
020800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
020900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
021000     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
021100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021300     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
021400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
021500     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
021600     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
021700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021800     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
021900     EJECT                                                                
022000 01  MESSAGE-CODES.                                                       
022100     05  NO-DATA-ENTERED         PIC X(3)   VALUE '014'.                  
022200     05  INVALID-KEY-FIELDS      PIC X(3)   VALUE '022'.                  
022300     05  IS-INVALID              PIC X(3)   VALUE '023'.                  
022400     05  TOO-MANY-LINES          PIC X(3)   VALUE '028'.                  
022500     05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.                  
022600     05  KEYS-ARE-MISSING        PIC X(3)   VALUE '041'.                  
022700     05  LINES-NOT-FOUND         PIC X(3)   VALUE '027'.                  
022800     05  PRINTING-REQUESTED      PIC X(3)   VALUE '015'.                  
022900     05  TRAILER-RECEIVED        PIC X(3)   VALUE '001'.                  
023000     05  TRACKING-ID-MISSING     PIC X(3)   VALUE '422'.                  
023100     EJECT                                                                
023200*    --- PARAMETRAR TILL SUBPROGRAM W218ETA                               
023300*01 -COPY W218LETA              -PRE LETA-                                
023400     EJECT                                                                
023500 01  FILLER                      PIC X(16)   VALUE 'W006PRT'.             
023600*01  -COPY W006PRT                                                        
023700     EJECT                                                                
023800 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
023900*01  -COPY WDATAREA                                                       
024000     EJECT                                                                
024100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
024200*01  -COPY WZ01SUB                                                        
024300     EJECT                                                                
024400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
024500*01  -COPY WZ01SEND                                                       
024600     EJECT                                                                
024700                                                                          
024800 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
024900*01  -COPY WMSGCONV                                                       
025000                                                                          
025100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
025200 01  SEND-AREA.                                                           
025300*    03  -COPY WZ01REQU -PRE SEND-                                        
025400*    03  -COPY WL0101I1 -PRE SEND-                                        
025500     03  SEND-REQU-REC-TAB.                                               
025600         05 SEND-REQU-FLATERHOPP PIC X(1).                                
025700         05 SEND-REQU-REC-RAD OCCURS 500.                                 
025800            07 SEND-REQU-FLREC   PIC X(1).                                
025900                                                                          
026000     EJECT                                                                
026100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
026200 01  REQU-AREA.                                                           
026300*    03  -COPY WZ01REQU                                                   
026400*    03  -COPY WL0101I1                                                   
026500     03  REQU-REC-TAB.                                                    
026600         05  REQU-FLATERHOPP     PIC X(1).                                
026700         05  REQU-REC-RAD OCCURS 500.                                     
026800             07 REQU-FLREC       PIC X(1).                                
026900     EJECT                                                                
027000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA-V1'.        
027100 01  RESP-AREA-V1.                                                        
027200*    03  -COPY WZ01RESP -PRE V1-                                          
027300*    03  -COPY WL0101O1 -PRE V1-                                          
027400                                                                          
027500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA-V2'.        
027600 01  RESP-AREA-V2.                                                        
027700*    03  -COPY WZ01RES2                                                   
027800*    03  -COPY WL0101O2                                                   
027900                                                                          
028000 01  PRINTAREA-START             PIC X(24)   VALUE                        
028100                                 'PRINTAREA-START'.                       
028200 01  HDR-AREA.                                                            
028300*    03  -COPY WZ01REQU  -PRE HDR-                                        
028400*    03  -COPY WZ04HDR                                                    
028500     EJECT                                                                
028600 01  DOC-HEADER-START            PIC X(24)   VALUE                        
028700                                 'DOC-HEAD-START '.                       
028800 01  DOC-HEAD-AREA.                                                       
028900*    03  -COPY WL10021   -PRE UT1-                                        
029000     EJECT                                                                
029100 01  DOC-LINE-START              PIC X(24)   VALUE                        
029200                                 'DOC-LINE-START '.                       
029300 01  DOC-LINE-AREA.                                                       
029400*    03  -COPY WL10022   -PRE UT2-                                        
029500     EJECT                                                                
029600*01  -COPY WL01TIDZ                                                       
029700     EJECT                                                                
029800*01  -COPY WWDIST35                                                       
029900     EJECT                                                                
030000                                                                          
030100*    NOTAFISCAL                                                           
030200 01  NOTF-AREA.                                                           
030300*    03  -COPY W611NOTF                                                   
030400                                                                          
030500 01  FILLER                      PIC X(16)   VALUE 'MSG PROP'.            
030600*01  -COPY WZ04PROP                                                       
030700                                                                          
030800 01  WS-PARAMETRAR.                                                       
030900     03  WS-URVAL.                                                        
031000         05  URV-IDFAKT          PIC 9(7) VALUE ZERO.                     
031100         05  URV-IDUSER          PIC X(8) VALUE ZERO.                     
031200     03  WS-PRINTER.                                                      
031300         05  URV-IDPRINTER       PIC X(8) VALUE SPACE.                    
031400     EJECT                                                                
031500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031700     SKIP3                                                                
031800 01  NYCKLAR-TILL-DLI.                                                    
031900     03  W-6301KEY-X.                                                     
032000         05  W-R5-IDHTYP         PIC X(4)      VALUE '6301'.              
032100         05  W-R5-IDDC           PIC X(2)      VALUE SPACE.               
032200         05  FILLER              PIC X(24)     VALUE LOW-VALUE.           
032300     03  W-WDGXKEY-6301.                                                  
032400         05  W-6301-IDHTYP       PIC X(4).                                
032500         05  W-6301-IDDC         PIC X(2).                                
032600         05  W-6301-LOWVALUE     PIC X(24).                               
032700                                                                          
032800     03  W-WDGXKEY-6302.                                                  
032900         05  W-6302-DABERANK     PIC 9(8).                                
033000         05  W-6302-IDFAKT       PIC S9(7)              COMP-3.           
033100                                                                          
033200     03  W-IDARTNR-X.                                                     
033300         05  W-IDARTNR           PIC S9(9)              COMP-3.           
033400                                                                          
033500     03  W-DAINLEV-X.                                                     
033600         05  W-DAINLEV           PIC 9(16).                               
033700                                                                          
033800     03  W-KDSEGKEY-X.                                                    
033900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
034000                                                                          
034100     03  W-IDDC                  PIC X(2).                                
034200     03  W-IDPTYP                PIC X(3).                                
034300                                                                          
034400     03  W-WDL6ASEQ-MIN.                                                  
034500         05  W-IDFAKT-MIN        PIC S9(7)             COMP-3.            
034600         05  W-IDKUNDRF-MIN      PIC X(10).                               
034700         05  W-IDKUNDNR-MIN      PIC S9(7)             COMP-3.            
034800         05  W-IDKOLLI-MIN       PIC S9(5)             COMP-3.            
034900     03  W-WDL6ASEQ-MAX.                                                  
035000         05  W-IDFAKT-MAX        PIC S9(7)             COMP-3.            
035100         05  W-IDKUNDRF-MAX      PIC X(10).                               
035200         05  W-IDKUNDNR-MAX      PIC S9(7)             COMP-3.            
035300         05  W-IDKOLLI-MAX       PIC S9(5)             COMP-3.            
035400                                                                          
035500     03  W-WDL6A1KY-MIN.                                                  
035600         05  W-SEQA-IDFAKT-MIN    PIC S9(7)             COMP-3.           
035700         05  W-SEQA-IDKUNDRF-MIN  PIC X(10).                              
035800         05  W-SEQA-IDKUNDNR-MIN  PIC S9(7)             COMP-3.           
035900         05  W-SEQA-IDKOLLI-MIN   PIC S9(5)             COMP-3.           
036000         05  W-SEQA-IDARTNR-MIN   PIC S9(9)             COMP-3.           
036100         05  W-SEQA-DAINLEV-MIN   PIC 9(16).                              
036200                                                                          
036300     03  W-WDL6A1KY-MAX.                                                  
036400         05  W-SEQA-IDFAKT-MAX    PIC S9(7)             COMP-3.           
036500         05  W-SEQA-IDKUNDRF-MAX  PIC X(10).                              
036600         05  W-SEQA-IDKUNDNR-MAX  PIC S9(7)             COMP-3.           
036700         05  W-SEQA-IDKOLLI-MAX   PIC S9(5)             COMP-3.           
036800         05  W-SEQA-IDARTNR-MAX   PIC S9(9)             COMP-3.           
036900         05  W-SEQA-DAINLEV-MAX   PIC 9(16).                              
037000                                                                          
037100     03  W-IDFAKT-X.                                                      
037200         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
037300                                                                          
037400     03  W-DABERANK-X.                                                    
037500         05  W-DABERANK          PIC 9(8)   VALUE ZERO.                   
037600                                                                          
037700     03  W-IDLBBET               PIC X(12)   VALUE SPACE.                 
037800                                                                          
037900     03  W-IDDC-B6-X.                                                     
038000         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
038100     03  W-IDDC-B6-DDC-X.                                                 
038200         05 W-IDDC-B6-DDC        PIC X(2)    VALUE SPACE.                 
038300                                                                          
038400*--------W6G1                                                             
038500     03  W-W6GXKEY-6005-X.                                                
038600         05  W-IDHTYP-6005         PIC X(4)    VALUE '6005'.              
038700         05  W-IDDC-6005           PIC X(2)    VALUE '11'.                
038800         05  FILLER                PIC X(24)   VALUE LOW-VALUE.           
038900                                                                          
039000     03  W-W6GXKEY-6006-X.                                                
039100         05  W-ADINLOMR-6006       PIC X(4)    VALUE SPACE.               
039200         05  FILLER                PIC X(1)    VALUE LOW-VALUE.           
039300     EJECT                                                                
039400*    --- STATUS-KOD FRÅN IMS                                              
039500 01  STATUS-WS                   PIC XX.                                  
039600     88  SEGMENT-FINNS                       VALUE '  '.                  
039700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
039800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
039900                                                                          
040000 01  GODK-STATUSKODER.                                                    
040100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040200     SKIP3                                                                
040300 01  SSA1                        PIC X(160).                              
040400 01  SSA2                        PIC X(128).                              
040500 01  SSA3                        PIC X(128).                              
040600     EJECT                                                                
040700*    --- IMS FUNKTIONSKODER                                               
040800*01  -COPY W0003                                                          
040900     EJECT                                                                
041000*    ---  DLI INPUT-OUTPUT AREA                                           
041100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
041200     SKIP3                                                                
041300 01  DLI-IO-AREA-WL6301.                                                  
041400     03  IO-AREA-WL6301          PIC X(260)  VALUE SPACE.                 
041500     SKIP3                                                                
041600     03  WDGX6301 REDEFINES IO-AREA-WL6301.                               
041700*        05  -COPY WDGX6301                                               
041800     EJECT                                                                
041900     03  WDGX6302 REDEFINES IO-AREA-WL6301.                               
042000*        05  -COPY WDGX6302                                               
042100 01  DLI-IO-AREA.                                                         
042200     03  IO-AREA                 PIC X(360)  VALUE SPACE.                 
042300     SKIP3                                                                
042400     03  WLINLC01 REDEFINES IO-AREA.                                      
042500*        05  -COPY WDL601                                                 
042600     EJECT                                                                
042700     03  WLINLC11 REDEFINES IO-AREA.                                      
042800*        05  -COPY WDL611                                                 
042900     EJECT                                                                
043000     03  WDL623   REDEFINES IO-AREA.                                      
043100*        05  -COPY WDL623                                                 
043200     EJECT                                                                
043300     03  WLINLD01 REDEFINES IO-AREA.                                      
043400*        05  -COPY WDL6A1                                                 
043500     EJECT                                                                
043600     03  WLARTS01 REDEFINES IO-AREA.                                      
043700*        05  -COPY WDK701                                                 
043800     EJECT                                                                
043900     03  WLARTS11 REDEFINES IO-AREA.                                      
044000*        05  -COPY WDK711                                                 
044100     EJECT                                                                
044200*01  WLLOGA01  -COPY WDL901                                               
044300     EJECT                                                                
044400 01  FILLER               PIC X(16)   VALUE 'WDB601 SEND AREA'.           
044500 01   DLI-IO-AREA-B601-SEND.                                              
044600*     03  -COPY WDB601 -PRE SEND-                                         
044700*                                                                         
044800 01  FILLER               PIC X(16)   VALUE 'WDB601 REC AREA'.            
044900 01   DLI-IO-AREA-B601-REC.                                               
045000*     03  -COPY WDB601 -PRE REC-                                          
045100     EJECT                                                                
045200*                                                                         
045300 01  FILLER               PIC X(16)   VALUE 'WDB601 DDC AREA'.            
045400 01   DLI-IO-AREA-B601-DDC.                                               
045500*     03  -COPY WDB601 -PRE DDC-                                          
045600     EJECT                                                                
045700 01  FILLER               PIC X(16)   VALUE 'WDL6A AREA'.                 
045800 01  DLI-IO-AREA-WDL6A.                                                   
045900*    03  -COPY WDL611  -PRE L6-                                           
046000     EJECT                                                                
046100 01  FILLER               PIC X(16)   VALUE 'WDR5  AREA'.                 
046200 01  DLI-IO-AREA-WDR5.                                                    
046300*    03  -COPY WDGX6302   -PRE R5-                                        
046400     EJECT                                                                
046500 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDR801'.            
046600 01  DLI-IO-WDR801.                                                       
046700*    03  WDR801    -COPY WDR801                                           
046800*    07  -COPY W510EKHA  -RED FIL-WDR801-DATA                             
046900     EJECT                                                                
047000 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDR901'.            
047100 01  DLI-IO-WDR901.                                                       
047200*    03  WDR901    -COPY WDR901                                           
047300*    07  -COPY W510EKHA -PRE R9- -RED FIL-WDR901-DATA                     
047400     EJECT                                                                
047500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK6'.                        
047600 01  DLI-IO-WDK601.                                                       
047700*    03  -COPY WDK601                                                     
047800     EJECT                                                                
047900 01  DLI-IO-WDK611.                                                       
048000*    03  -COPY WDK611                                                     
048100*                                                                         
048200 01  FILLER               PIC X(16)   VALUE 'DLI-IO-W6G130'.              
048300 01  DLI-IO-AREA-W6G130.                                                  
048400*    03  -COPY W6GX6006                                                   
048500     EJECT                                                                
048600                                                                          
048700 LINKAGE SECTION.                                                         
048800                                                                          
048900*01  -COPY W0009   -PRE MSG-                                              
049000                                                                          
049100 01  DISTRDOC-PCB                PIC X.                                   
049200     EJECT                                                                
049300 01  MQASYNC-PCB                 PIC X.                                   
049400     EJECT                                                                
049500*01  -COPY W0009   -PRE ALT-                                              
049600     EJECT                                                                
049700*01  -COPY W0009   -PRE DETTAPGM-                                         
049800     EJECT                                                                
049900*01  -COPY W0008  -PRE GX63-                                              
050000     05  FILLER                  PIC X.                                   
050100     EJECT                                                                
050200*01  -COPY W0008  -PRE INLC-                                              
050300     05  FILLER                  PIC X.                                   
050400     EJECT                                                                
050500*01  -COPY W0008  -PRE INLD-                                              
050600     05  FILLER                  PIC X.                                   
050700     EJECT                                                                
050800*01  -COPY W0008  -PRE ARTS-                                              
050900     05  FILLER                  PIC X.                                   
051000     EJECT                                                                
051100*01  -COPY W0008  -PRE LOGA-                                              
051200     05  FILLER                  PIC X.                                   
051300     EJECT                                                                
051400*01  MSG-PCB                     PIC X.                                   
051500                                                                          
051600*01  -COPY W0008  -PRE 01-                                                
051700     05  FILLER                  PIC X.                                   
051800     EJECT                                                                
051900*01  -COPY W0008  -PRE WDB6-                                              
052000     05  FILLER                  PIC X.                                   
052100     EJECT                                                                
052200*01  -COPY W0008  -PRE WDL6A-                                             
052300     05  FILLER                  PIC X.                                   
052400     EJECT                                                                
052500*01  -COPY W0008  -PRE WDR5-                                              
052600     05  FILLER                  PIC X.                                   
052700     EJECT                                                                
052800*01  -COPY W0008  -PRE WDR8-                                              
052900     05  FILLER                  PIC X.                                   
053000     EJECT                                                                
053100*01  -COPY W0008  -PRE WDR9-                                              
053200     05  FILLER                  PIC X.                                   
053300     EJECT                                                                
053400*01  -COPY W0008  -PRE WDK6-                                              
053500     05  FILLER                  PIC X.                                   
053600     EJECT                                                                
053700*01  -COPY W0008  -PRE WDL6-                                              
053800     05  FILLER                  PIC X.                                   
053900     EJECT                                                                
054000*01  -COPY W0008  -PRE W6G1-                                              
054100     05  FILLER                  PIC X.                                   
054200     EJECT                                                                
054300 01  W218-WDK6-PCB               PIC X.                                   
054400 01  W218-WDK7-PCB               PIC X.                                   
054500 01  W218-WDL6-PCB               PIC X.                                   
054600 01  W218-WDF1-PCB               PIC X.                                   
054700 01  W218-WDB6-PCB               PIC X.                                   
054800 01  W218-WDD9-PCB               PIC X.                                   
054900 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB MQASYNC-PCB               
055000                           ALT-PCB DETTAPGM-PCB                           
055100                           GX63-PCB INLC-PCB INLD-PCB ARTS-PCB            
055200                           LOGA-PCB WDB6-PCB WDL6A-PCB                    
055300                           WDR5-PCB WDR8-PCB WDR9-PCB WDK6-PCB            
055400                           WDL6-PCB W6G1-PCB                              
055500                           W218-WDK6-PCB W218-WDK7-PCB                    
055600                           W218-WDL6-PCB W218-WDF1-PCB                    
055700                           W218-WDB6-PCB W218-WDD9-PCB.                   
055800 MAIN SECTION.                                                            
055900     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB MQASYNC-PCB               
056000                           ALT-PCB DETTAPGM-PCB                           
056100                           GX63-PCB INLC-PCB INLD-PCB ARTS-PCB            
056200                           LOGA-PCB WDB6-PCB WDL6A-PCB                    
056300                           WDR5-PCB WDR8-PCB WDR9-PCB WDK6-PCB            
056400                           WDL6-PCB W6G1-PCB                              
056500                           W218-WDK6-PCB W218-WDK7-PCB                    
056600                           W218-WDL6-PCB W218-WDF1-PCB                    
056700                           W218-WDB6-PCB W218-WDD9-PCB.                   
056800                                                                          
056900     PERFORM S01-HAEMTA-ANROPSDATA                                        
057000     IF SUB-KDRC = 0                                                      
057100       PERFORM A-INIT                                                     
057200       PERFORM B-KOLLA-NYCKLAR                                            
057300       IF NYCKLAR-OK                                                      
057400          IF REQU-KDPGMACT = 'E' OR 'X'                                   
057500             PERFORM G-KOLLA-INPUT                                        
057600             IF INDATA-OK                                                 
057700                PERFORM H-UPPDATERA                                       
057800             END-IF                                                       
057900          END-IF                                                          
058000       END-IF                                                             
058100                                                                          
058200       IF ATERHOPP                                                        
058300          MOVE YES   TO REQU-FLATERHOPP                                   
058400          MOVE REQU-WL0101I1 TO SEND-REQU-WL0101I1                        
058500          MOVE REQU-REC-TAB  TO SEND-REQU-REC-TAB                         
058700          MOVE ZERO  TO SEND-REQU-IDMSGVER                                
058800          MOVE SPACE TO SEND-REQU-IDUSER                                  
058900          MOVE 'X'   TO SEND-REQU-KDPGMACT                                
059000          PERFORM K-OMSTART-EGEN-TRANS                                    
059100       ELSE                                                               
059200          IF REC-DCS-KDTRADP = 'BR12' AND                                 
059300            (REQU-KDPGMACT = 'E' OR 'X')                                  
059400            PERFORM I-TRIGGER-BR-TRANS                                    
059500          END-IF                                                          
059600          IF EJ-PRINT                                                     
059700            IF NYCKLAR-OK                                                 
059800              PERFORM F-LAES-VISA-INFO                                    
059900            END-IF                                                        
060000            PERFORM S02-RETURNERA-SVAR                                    
060100          ELSE                                                            
060200            PERFORM S94-SEND-CLOSE                                        
060300          END-IF                                                          
060400       END-IF                                                             
060500     END-IF                                                               
060600     MOVE ZERO TO RETURN-CODE                                             
060700     GOBACK                                                               
060800     .                                                                    
060900     EJECT                                                                
061000 A-INIT SECTION.                                                          
061100                                                                          
061200     MOVE ALL '+'                TO RESP-AREA-V1                          
061300     MOVE LOW-VALUES             TO RESP-AREA-V2                          
061400     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
061500                     RESP-IDMSG-INFO                                      
061600                     RESP-IDELMT-ERROR                                    
061700     MOVE ZERO    TO RESP-KVRADER                                         
061800     ACCEPT DAGENS-TID   FROM TIME                                        
061900     ACCEPT DAGENS-DATUM FROM DATE                                        
062000                                                                          
062100     MOVE LOW-VALUE  TO W-WDL6ASEQ-MIN                                    
062200                        W-WDL6A1KY-MIN                                    
062300     MOVE HIGH-VALUE TO W-WDL6ASEQ-MAX                                    
062400                        W-WDL6A1KY-MAX                                    
062700     IF REQU-FLATERHOPP = YES                                             
062800       CONTINUE                                                           
062900     ELSE                                                                 
063000       MOVE SPACE    TO REQU-REC-TAB                                      
063100     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400 B-KOLLA-NYCKLAR SECTION.                                                 
063500                                                                          
063600     MOVE JA             TO NYCKLAR-SW                                    
063700                                                                          
063800***  KONTROLL AV REQU-KDPGMACT                                            
063900     IF REQU-KDPGMACT = 'S' OR 'E' OR 'X'                                 
064000        CONTINUE                                                          
064100     ELSE                                                                 
064200        MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                             
064300        MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                            
064400        MOVE NEJ TO NYCKLAR-SW                                            
064500     END-IF                                                               
064600                                                                          
064700***  KONTROLL AV TIFAKT                                                   
064800                                                                          
064900     IF REQU-TIFAKT-KEY = ALL '+'                                         
065000        MOVE ZERO TO WS-TIFAKT                                            
065100     ELSE                                                                 
065200        MOVE REQU-TIFAKT-KEY TO WS-TIFAKT                                 
065300     END-IF                                                               
065400     INSPECT WS-TIFAKT REPLACING LEADING SPACE BY ZERO                    
065500     IF WS-TIFAKT NUMERIC                                                 
065600        CONTINUE                                                          
065700     ELSE                                                                 
065800        MOVE 'TIFAKT' TO RESP-IDELMT-ERROR                                
065900        MOVE NEJ      TO NYCKLAR-SW                                       
066000     END-IF                                                               
066100                                                                          
066200***  KONTROLL AV IDDC-SEND                                                
066300     IF REQU-IDDC-SEND-KEY = ALL '+'                                      
066400        MOVE SPACE              TO WS-IDDC-SEND-KEY                       
066500     ELSE                                                                 
066600        MOVE REQU-IDDC-SEND-KEY TO WS-IDDC-SEND-KEY                       
066700     END-IF                                                               
066800*                                                                         
066900     IF WS-IDDC-SEND-KEY = SPACE                                          
067000        CONTINUE                                                          
067100     ELSE                                                                 
067200        MOVE WS-IDDC-SEND-KEY TO W-IDDC-B6                                
067300        PERFORM IMS-GU-WDB601-SEND                                        
067400        IF SEGMENT-SAKNAS                                                 
067500           MOVE 'IDDC-SEND' TO RESP-IDELMT-ERROR                          
067600           MOVE NEJ         TO NYCKLAR-SW                                 
067700        END-IF                                                            
067800     END-IF                                                               
067900                                                                          
068000***  KONTROLL AV IDLBBET                                                  
068100     IF REQU-IDLBBET-KEY = ALL '+'                                        
068200        MOVE SPACE            TO W-IDLBBET                                
068300     ELSE                                                                 
068400        MOVE REQU-IDLBBET-KEY TO W-IDLBBET                                
068500     END-IF                                                               
068600                                                                          
068700***  KONTROLL RECEIVING DC                                                
068800     MOVE REQU-IDDC-KEY   TO W-IDDC                                       
068900                             WS-IDDC-REC                                  
069000     IF W-IDDC = ALL '+' OR SPACE                                         
069100       MOVE 'IDDC'   TO RESP-IDELMT-ERROR                                 
069200       MOVE NEJ      TO NYCKLAR-SW                                        
069300     ELSE                                                                 
069400       MOVE W-IDDC   TO W-IDDC-B6                                         
069500       PERFORM IMS-GU-WDB601-REC                                          
069600       IF SEGMENT-FINNS                                                   
069700         IF REC-DCS-SWEDEN                                                
069800            SET AKTUELLT-LAND-SVERIGE TO TRUE                             
069900         ELSE                                                             
070000            SET AKTUELLT-EJ-SVERIGE   TO TRUE                             
070100         END-IF                                                           
070200         IF REC-DCS-DDC                                                   
070300           MOVE 'IDDC'    TO RESP-IDELMT-ERROR                            
070400           MOVE NEJ       TO NYCKLAR-SW                                   
070500         END-IF                                                           
070600***  VALIDATE IDTRACK IF EXITS OR NOT                                     
070700         MOVE REC-DCS-FLTRACK TO RESP-FLTRACK                             
070800         IF REC-DCS-FLTRACK = 'J'                                         
070900            MOVE 'J'  TO WS-FLTRACK                                       
071000         ELSE                                                             
071100            MOVE 'N'  TO WS-FLTRACK                                       
071200         END-IF                                                           
071300       END-IF                                                             
071400     END-IF                                                               
071500                                                                          
071600     MOVE WS-TIFAKT        TO RESP-TIFAKT-KEY                             
071700     MOVE WS-IDDC-SEND-KEY TO RESP-IDDC-SEND-KEY                          
071800     MOVE W-IDLBBET       TO RESP-IDLBBET-KEY                             
071900     MOVE W-IDDC          TO RESP-IDDC-KEY                                
072000******** ADAPT DATE AND TIME FOR TIMEZONES                                
072100                                                                          
072200     MOVE '011'                TO MSGI-KDCALL                             
072300     MOVE REC-DCS-IDTIDZON     TO MSGI-IDTIDZON                           
072400     MOVE REC-DCS-IDDC         TO MSGI-IDDC                               
072500     MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                           
072600     MOVE DAGENS-TID           TO MSGI-TILOKTID                           
072700                                  LOCAL-TIME                              
072800     CALL WL01TIDZ USING          MSGI-WL01TIDZ                           
072900     MOVE MSGI-TILOKDAT(1:6)   TO LOCAL-DATE                              
073000     MOVE MSGI-TILOKTID(1:4)   TO LOCAL-TIME(1:4)                         
073100********                                                                  
073200                                                                          
073300     IF RESP-TIFAKT-KEY = ZERO                                            
073400        INSPECT RESP-TIFAKT-KEY REPLACING LEADING ZERO BY SPACE           
073500     END-IF                                                               
073600                                                                          
073700     IF NYCKLAR-FEL                                                       
073800        IF RESP-IDELMT-ERROR = 'KDPGMACT'                                 
073900           MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                          
074000        ELSE                                                              
074100           MOVE INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                    
074200        END-IF                                                            
074300     END-IF                                                               
074400     .                                                                    
074500     EJECT                                                                
074600 F-LAES-VISA-INFO SECTION.                                                
074700                                                                          
074800     MOVE 500 TO MAX-INDX                                                 
074900     PERFORM FA-LAES-GRUNDDATA                                            
075000                                                                          
075100     IF SEGMENT-SAKNAS                                                    
075200        MOVE LINES-NOT-FOUND  TO RESP-IDMSG-ERROR                         
075300        MOVE 'KEY' TO RESP-IDELMT-ERROR                                   
075400     ELSE                                                                 
075500                                                                          
075600****************** FÖRSTA SÖKTA                                           
075700        MOVE ZERO TO INDX                                                 
075800                     WS-KVANT                                             
075900        MOVE NEJ  TO SW-TRAEFF                                            
076000        PERFORM FB-LAES-RADDATA                                           
076100                                                                          
076200        IF SEGMENT-FINNS                                                  
076300           PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = JA                 
076400             IF WS-IDDC-SEND-KEY = SPACE                                  
076500               MOVE 6302-TIFAKT   TO TMP1-YYMMDD                          
076600               MOVE TIFAKT-WS     TO TMP2-YYMMDD                          
076700               PERFORM WY2000P1                                           
076800               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
076900                  MOVE JA TO SW-TRAEFF                                    
077000               END-IF                                                     
077100             ELSE                                                         
077200               MOVE 6302-TIFAKT   TO TMP1-YYMMDD                          
077300               MOVE TIFAKT-WS     TO TMP2-YYMMDD                          
077400               PERFORM WY2000P1                                           
077500               IF (WS-IDDC-SEND-KEY = 6302-IDDC-SEND) AND                 
077600                  (TMP1-YYMMDD >= TMP2-YYMMDD)                            
077700                  MOVE JA TO SW-TRAEFF                                    
077800               ELSE                                                       
077900                 IF 6302-IDDC-SEND NOT = DDC-DCS-IDDC                     
078000                    MOVE 6302-IDDC-SEND TO W-IDDC-B6-DDC                  
078100                    PERFORM IMS-GU-WDB601-DDC-CHECK                       
078200                 END-IF                                                   
078300                 IF SEND-DCS-CDC AND                                      
078400                    DDC-DCS-DDC AND                                       
078500                    TMP1-YYMMDD >= TMP2-YYMMDD                            
078600                    MOVE JA TO SW-TRAEFF                                  
078700                 END-IF                                                   
078800               END-IF                                                     
078900             END-IF                                                       
079000             IF SW-TRAEFF = NEJ                                           
079100                PERFORM FB-LAES-RADDATA                                   
079200             END-IF                                                       
079300          END-PERFORM                                                     
079400          IF SW-TRAEFF = NEJ                                              
079500             MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
079600          END-IF                                                          
079700       ELSE                                                               
079800          MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                        
079900       END-IF                                                             
080000                                                                          
080100****************** SÖKTA FAKTUROR                                         
080200       PERFORM UNTIL SEGMENT-SAKNAS                                       
080300          IF WS-IDDC-SEND-KEY NOT = SEND-DCS-IDDC AND                     
080400             WS-IDDC-SEND-KEY NOT = SPACE                                 
080500             MOVE WS-IDDC-SEND-KEY TO W-IDDC-B6                           
080600             PERFORM IMS-GU-WDB601-SEND                                   
080700          END-IF                                                          
080800          IF 6302-IDDC-SEND NOT = DDC-DCS-IDDC                            
080900             MOVE 6302-IDDC-SEND TO W-IDDC-B6-DDC                         
081000             PERFORM IMS-GU-WDB601-DDC-CHECK                              
081100          END-IF                                                          
081200          IF (WS-IDDC-SEND-KEY = SPACE) OR                                
081300             (WS-IDDC-SEND-KEY = 6302-IDDC-SEND) OR                       
081400             (SEND-DCS-CDC AND DDC-DCS-DDC)                               
081500             MOVE 6302-TIFAKT   TO TMP1-YYMMDD                            
081600             MOVE TIFAKT-WS     TO TMP2-YYMMDD                            
081700             PERFORM WY2000P1                                             
081800             IF TMP1-YYMMDD >= TMP2-YYMMDD                                
081900                IF INDX < MAX-INDX                                        
082000                 ADD +1 TO INDX                                           
082100                 MOVE 6302-IDDC-SEND    TO RESP-IDDC-SEND(INDX)           
082200                 MOVE 6302-IDDC-LEV     TO RESP-IDDC-LEV(INDX)            
082300                 MOVE 6302-IDFAKT       TO RESP-IDFAKT(INDX)              
082400                 INSPECT RESP-IDFAKT(INDX) REPLACING LEADING              
082500                      SPACE BY ZERO                                       
082600                 MOVE 6302-IDSHIPM      TO RESP-IDSHIPM(INDX)             
082700                 MOVE 6302-TIFAKT       TO RESP-TIFAKT(INDX)              
082800                 IF INDATA-OK                                             
082900                   MOVE 6302-DABERANK(3:6) TO RESP-TIBERANK(INDX)         
083000                   MOVE 6302-ADINLOMR      TO RESP-ADINLOMR(INDX)         
083100                 ELSE                                                     
083200                   MOVE REQU-TIBERANK(INDX) TO RESP-TIBERANK(INDX)        
083300                   MOVE REQU-ADINLOMR(INDX) TO RESP-ADINLOMR(INDX)        
083400                 END-IF                                                   
083500                 MOVE 6302-IDLBBET      TO RESP-IDLBBET(INDX)             
083600                 MOVE 6302-KDTRPSTA     TO RESP-KDTRPSTA(INDX)            
083700                 MOVE 6302-KVKOLLI-FAKT TO RESP-KVKOLLI-FAKT(INDX)        
083800                 MOVE 6302-KVKOLLI-MOT  TO RESP-KVKOLLI-MOT(INDX)         
083900                 MOVE 6302-KVRADER-MOT  TO RESP-KVRADER-MOT(INDX)         
084000                 MOVE 6302-KVRADER-FAKT TO RESP-KVRADER-FAKT(INDX)        
084100                 MOVE 6302-KVRADER-PRIO TO RESP-KVRADER-PRIO(INDX)        
084200                 MOVE 6302-DABERANK-PROP TO                               
084300                                         RESP-DABERANK-PROP(INDX)         
084400                 MOVE 6302-DABERANK-DISCH TO                              
084500                                         RESP-DABERANK-DISCH(INDX)        
084600                 MOVE 6302-FLMANETA     TO RESP-FLMANETA(INDX)            
084700                 MOVE RESP-IDFAKT(INDX) TO REQU-IDFAKT(INDX)              
084800                 MOVE 'VISA' TO WS-FROM-PARA                              
084900                 IF WS-FLTRACK = 'J'                                      
085000                   PERFORM FC-CHECK-IDTRACK                               
085100                   IF 6302-KVRADER-MOT > 0                                
085200                      MOVE 'N' TO RESP-FLOLD-IDTRACK(INDX)                
085300                   ELSE                                                   
085400                      MOVE 'J' TO RESP-FLOLD-IDTRACK(INDX)                
085500                   END-IF                                                 
085600                 ELSE                                                     
085700                   MOVE ' ' TO RESP-FLOLD-IDTRACK(INDX)                   
085800                 END-IF                                                   
085900                 IF REQU-CMD-IN(INDX) = ALL '+' OR SPACE OR               
086000                   LOW-VALUE                                              
086100                   MOVE SPACE TO RESP-CMD-IN(INDX)                        
086200                                    RESP-IDMSG-ERROR-LINE(INDX)           
086300                 END-IF                                                   
086400                                                                          
086500                 IF REQU-KDPGMACT = 'S' OR ((REQU-KDPGMACT = 'E'          
086600                            OR 'X') AND INDATA-OK)                        
086700                   MOVE SPACE      TO RESP-IDMSG-ERROR-LINE(INDX)         
086800                                      RESP-CMD-IN(INDX)                   
086900                 END-IF                                                   
087000                                                                          
087100                END-IF                                                    
087200                ADD +1 TO WS-KVANT                                        
087300             END-IF                                                       
087400          END-IF                                                          
087500                                                                          
087600          PERFORM FB-LAES-RADDATA                                         
087700       END-PERFORM                                                        
087800       MOVE WS-KVANT TO RESP-KVRADER                                      
087900       IF WS-KVANT > 500                                                  
088000          MOVE 500 TO RESP-KVRADER                                        
088100          MOVE TOO-MANY-LINES TO RESP-IDMSG-ERROR                         
088200       END-IF                                                             
088300     END-IF                                                               
088400     .                                                                    
088500     EJECT                                                                
088600 FA-LAES-GRUNDDATA SECTION.                                               
088700                                                                          
088800     MOVE '6301'          TO W-6301-IDHTYP                                
088900     MOVE WS-IDDC-REC     TO W-6301-IDDC                                  
089000     MOVE LOW-VALUE       TO W-6301-LOWVALUE                              
089100                                                                          
089200     PERFORM IMS-GU-WL630101                                              
089300     .                                                                    
089400     EJECT                                                                
089500 FB-LAES-RADDATA SECTION.                                                 
089600                                                                          
089700     MOVE ZERO     TO W-6302-DABERANK                                     
089800                      W-6302-IDFAKT                                       
089900     IF W-IDLBBET = SPACE                                                 
090000        PERFORM IMS-GNP-WL630111-A                                        
090100     ELSE                                                                 
090200        PERFORM IMS-GNP-WL630111-B                                        
090300     END-IF                                                               
090400     .                                                                    
090500     EJECT                                                                
090600                                                                          
090700 FC-CHECK-IDTRACK SECTION.                                                
090800                                                                          
090900     MOVE RESP-IDFAKT(INDX)  TO W-SEQA-IDFAKT-MIN                         
091000                                W-SEQA-IDFAKT-MAX                         
091100     MOVE SPACE              TO W-SEQA-IDKUNDRF-MIN                       
091200     MOVE ZERO               TO W-SEQA-IDKUNDNR-MIN                       
091300                                W-SEQA-IDKOLLI-MIN                        
091400                                W-SEQA-IDARTNR-MIN                        
091500                                W-SEQA-DAINLEV-MIN                        
091600     MOVE '9999999999'       TO W-SEQA-IDKUNDRF-MAX                       
091700     MOVE 9999999            TO W-SEQA-IDKUNDNR-MAX                       
091800     MOVE 99999              TO W-SEQA-IDKOLLI-MAX                        
091900     MOVE 999999999          TO W-SEQA-IDARTNR-MAX                        
092000     MOVE 9999999999999999   TO W-SEQA-DAINLEV-MAX                        
092100                                                                          
092200     PERFORM IMS-GU-WLINLD01                                              
092300     MOVE SEQA-IDARTNR TO W-IDARTNR                                       
092400     MOVE SEQA-DAINLEV TO W-DAINLEV                                       
092500     MOVE SEQA-IDDC    TO W-IDDC                                          
092600     IF WS-FLTRACK = 'J'                                                  
092700       PERFORM IMS-GU-WDL623                                              
092800       IF SEGMENT-FINNS                                                   
092900          IF WS-FROM-PARA = 'VISA'                                        
093000            MOVE TINL-IDTRACK TO RESP-IDTRACK(INDX)                       
093100                                 REQU-IDTRACK(INDX)                       
093200          ELSE                                                            
093300            MOVE TINL-IDTRACK TO WS-TINL-IDTRACK                          
093400          END-IF                                                          
093500       END-IF                                                             
093600     END-IF                                                               
093700                                                                          
093800     .                                                                    
093900     EJECT                                                                
094000                                                                          
094100 G-KOLLA-INPUT SECTION.                                                   
094200                                                                          
094300     MOVE JA TO INDATA-SW                                                 
094400     MOVE ZERO TO W-IDFAKT                                                
094500                  W-DABERANK                                              
094600     IF REQU-KVRADER NUMERIC                                              
094700     AND REQU-KVRADER > ZERO                                              
094800        MOVE REQU-KVRADER TO MAX-INDX                                     
094900     END-IF                                                               
095000                                                                          
095100     MOVE +1 TO INDX                                                      
095200     MOVE NEJ TO SW-REQU-RAD-IFYLLD                                       
095300     PERFORM UNTIL INDX > MAX-INDX                                        
095400        IF REQU-CMD-IN (INDX) = ALL '+'                                   
095500           CONTINUE                                                       
095600        ELSE                                                              
095700           MOVE JA TO SW-REQU-RAD-IFYLLD                                  
095800           MOVE MAX-INDX TO INDX                                          
095900        END-IF                                                            
096000        ADD +1 TO INDX                                                    
096100     END-PERFORM                                                          
096200                                                                          
096300     IF SW-REQU-RAD-IFYLLD = NEJ                                          
096400        MOVE NO-DATA-ENTERED TO RESP-IDMSG-ERROR                          
096500        MOVE NEJ TO INDATA-SW                                             
096600     ELSE                                                                 
096700        MOVE SPACE TO W-CMD                                               
096800        MOVE +1 TO INDX                                                   
096900        PERFORM UNTIL INDX > MAX-INDX                                     
097000           IF  REQU-CMD-IN(INDX) NOT = ALL '+'                            
097100           AND REQU-CMD-IN(INDX) NOT = SPACE                              
097200              MOVE REQU-CMD-IN(INDX) TO RESP-CMD-IN(INDX)                 
097300              IF REQU-IDFAKT(INDX) = SPACE OR ZERO                        
097400                  MOVE NEJ  TO INDATA-SW                                  
097500                  MOVE INDX TO INDX-DISPLAY                               
097600                  STRING 'IDFAKT*' INDX-DISPLAY DELIMITED BY SIZE         
097700                    INTO RESP-IDELMT-ERROR                                
097800                  MOVE 'INV'  TO RESP-IDMSG-ERROR-LINE(INDX)              
097900              END-IF                                                      
098000*                                                                         
098100              IF REQU-CMD-IN(INDX) NOT = 'REC' AND                        
098200                 REQU-CMD-IN(INDX) NOT = 'PR ' AND                        
098300                 REQU-CMD-IN(INDX) NOT = 'DAT' AND                        
098400                 REQU-CMD-IN(INDX) NOT = 'LOC' AND                        
098500                 REQU-CMD-IN(INDX) NOT = 'DEC' AND                        
098600                 REQU-CMD-IN(INDX) NOT = '   ' AND                        
098700                 REQU-CMD-IN(INDX) NOT = ALL '+'                          
098800                 IF REQU-CMD-IN(INDX) = 'C  ' OR                          
098900                    REQU-CMD-IN(INDX) = 'A  ' OR                          
099000                    REQU-CMD-IN(INDX) = 'I  '                             
099100                    CONTINUE                                              
099200                 ELSE                                                     
099300                    MOVE NEJ TO INDATA-SW                                 
099400                    MOVE INDX TO INDX-DISPLAY                             
099500                    STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE          
099600                    INTO RESP-IDELMT-ERROR                                
099700                    MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)             
099800                 END-IF                                                   
099900              END-IF                                                      
100000**            VALIDATION FOR TIBERANK                                     
100100              IF  REQU-CMD-IN(INDX) = 'DAT'                               
100200                MOVE 'AAMMDD'            TO DAT-KDDATFORM                 
100300                MOVE REQU-TIBERANK(INDX) TO DAT-I-TIDATUM                 
100400                CALL WDATKONV         USING DAT-KDDATFORM                 
100500                                            DAT-I-TIDATUM                 
100600                                            DAT-O-TIDATUM                 
100700                                            DAT-KDSVAR                    
100800                                                                          
100900                IF DAT-KDSVAR NOT = SPACE OR                              
101000                   REQU-TIBERANK(INDX) < LOCAL-DATE                       
101100                    MOVE NEJ  TO INDATA-SW                                
101200                    MOVE INDX TO INDX-DISPLAY                             
101300                    STRING 'TIBERANK*' INDX-DISPLAY                       
101400                      DELIMITED BY SIZE INTO RESP-IDELMT-ERROR            
101500                    MOVE 'ETA' TO  RESP-IDMSG-ERROR-LINE(INDX)            
101600                END-IF                                                    
101700              END-IF                                                      
101800**            VALIDATION FOR ADINLOMR                                     
101900              IF  REQU-CMD-IN(INDX) = 'LOC'                               
102000                IF REQU-ADINLOMR (INDX) = ALL '+'                         
102100                  MOVE NEJ TO INDATA-SW                                   
102200                  MOVE INDX TO INDX-DISPLAY                               
102300                  STRING 'ADINLOMR-UL*' INDX-DISPLAY                      
102400                    DELIMITED BY SIZE INTO RESP-IDELMT-ERROR              
102500                  MOVE 'UNL' TO  RESP-IDMSG-ERROR-LINE(INDX)              
102600                ELSE                                                      
102700                  IF REC-DCS-CDC                                          
102800                     MOVE REQU-ADINLOMR(INDX)                             
102900                                  TO W-ADINLOMR-6006                      
103000                     PERFORM IMS-GU-W6G130                                
103100                     IF SEGMENT-SAKNAS                                    
103200                       MOVE NEJ TO INDATA-SW                              
103300                       MOVE INDX TO INDX-DISPLAY                          
103400                       STRING 'ADINLOMR-UL*' INDX-DISPLAY                 
103500                         DELIMITED BY SIZE INTO RESP-IDELMT-ERROR         
103600                       MOVE 'UNL' TO  RESP-IDMSG-ERROR-LINE(INDX)         
103700                     END-IF                                               
103800                  END-IF                                                  
103900                END-IF                                                    
104000              END-IF                                                      
104100**            VALIDATION FOR IDTRACK                                      
104200              IF  REQU-CMD-IN(INDX) = 'DEC'                               
104300                                                                          
104400                IF REQU-IDTRACK(INDX) = SPACE OR ALL '+'                  
104500                    MOVE NEJ  TO INDATA-SW                                
104600                    MOVE INDX TO INDX-DISPLAY                             
104700                    STRING 'IDTRACK*' INDX-DISPLAY                        
104800                      DELIMITED BY SIZE INTO RESP-IDELMT-ERROR            
104900                    MOVE 'DEC' TO  RESP-IDMSG-ERROR-LINE(INDX)            
105000                ELSE                                                      
105100                   MOVE 'N' TO SW-IDTRACK-OLD                             
105200                   PERFORM GA-CHECK-IDTRACK-OLD                           
105300                   IF SW-IDTRACK-OLD = 'J'                                
105400                      MOVE NEJ  TO INDATA-SW                              
105500                      MOVE INDX TO INDX-DISPLAY                           
105600                      STRING 'IDTRACK ALREADY EXISTS*'                    
105700                        INDX-DISPLAY                                      
105800                        DELIMITED BY SIZE INTO RESP-IDELMT-ERROR          
105900                      MOVE 'DEC' TO  RESP-IDMSG-ERROR-LINE(INDX)          
106000                   END-IF                                                 
106100                END-IF                                                    
106200              END-IF                                                      
106300              IF  REQU-CMD-IN(INDX) = 'REC'                               
106400               IF (REQU-ADINLOMR (INDX) = ALL '+' OR SPACE) AND           
106500                  REC-DCS-CDC                                             
106600                  MOVE NEJ TO INDATA-SW                                   
106700                  MOVE INDX TO INDX-DISPLAY                               
106800                  STRING 'ADINLOMR-UL*' INDX-DISPLAY                      
106900                    DELIMITED BY SIZE INTO RESP-IDELMT-ERROR              
107000                  MOVE 'UNL' TO  RESP-IDMSG-ERROR-LINE(INDX)              
107100               ELSE                                                       
107200                 IF REC-DCS-CDC                                           
107300                    MOVE REQU-ADINLOMR(INDX)                              
107400                                 TO W-ADINLOMR-6006                       
107500                    PERFORM IMS-GU-W6G130                                 
107600                    IF SEGMENT-SAKNAS                                     
107700                      MOVE NEJ TO INDATA-SW                               
107800                      MOVE INDX TO INDX-DISPLAY                           
107900                      STRING 'ADINLOMR-UL*' INDX-DISPLAY                  
108000                        DELIMITED BY SIZE INTO RESP-IDELMT-ERROR          
108100                      MOVE 'UNL' TO  RESP-IDMSG-ERROR-LINE(INDX)          
108200                    END-IF                                                
108300                 END-IF                                                   
108400               END-IF                                                     
108500                                                                          
108600               MOVE SPACE  TO WS-FROM-PARA                                
108700                              WS-TINL-IDTRACK                             
108800               MOVE REQU-IDFAKT(INDX) TO RESP-IDFAKT(INDX)                
108900               IF WS-FLTRACK = 'J'                                        
109000                 PERFORM FC-CHECK-IDTRACK                                 
109100                 IF WS-TINL-IDTRACK = SPACE                               
109200                      MOVE NEJ  TO INDATA-SW                              
109300                      MOVE INDX TO INDX-DISPLAY                           
109400                      STRING 'IDTRACK MISSING*' INDX-DISPLAY              
109500                        DELIMITED BY SIZE INTO RESP-IDELMT-ERROR          
109600                      MOVE 'REC' TO  RESP-IDMSG-ERROR-LINE(INDX)          
109700                  END-IF                                                  
109800               END-IF                                                     
109900              END-IF                                                      
110000           ELSE                                                           
110100              MOVE SPACE TO RESP-CMD-IN(INDX)                             
110200           END-IF                                                         
110300           ADD 1 TO INDX                                                  
110400        END-PERFORM                                                       
110500        IF INDATA-FEL                                                     
110600           MOVE IS-INVALID TO RESP-IDMSG-ERROR                            
110700           IF TRACKID-MISSING                                             
110800              MOVE TRACKING-ID-MISSING TO RESP-IDMSG-ERROR                
110900           END-IF                                                         
111000        END-IF                                                            
111100     END-IF                                                               
111200     .                                                                    
111300     EJECT                                                                
111400 GA-CHECK-IDTRACK-OLD SECTION.                                            
111500                                                                          
111600     PERFORM FA-LAES-GRUNDDATA                                            
111700     IF SEGMENT-FINNS                                                     
111800        MOVE REQU-IDFAKT(INDX) TO W-IDFAKT                                
111900        PERFORM IMS-GNP-WL630111                                          
112000        IF SEGMENT-FINNS                                                  
112100           IF 6302-KVRADER-MOT > 0                                        
112200              MOVE 'J' TO SW-IDTRACK-OLD                                  
112300           END-IF                                                         
112400        END-IF                                                            
112500     END-IF                                                               
112600     .                                                                    
112700     EJECT                                                                
112800                                                                          
112900 H-UPPDATERA SECTION.                                                     
113000     MOVE +1        TO INDX                                               
113100     MOVE ZERO      TO W-KVANT-UPD                                        
113200     MOVE W-IDDC    TO W-6301-IDDC                                        
113300     MOVE '6301'    TO W-6301-IDHTYP                                      
113400     MOVE LOW-VALUE TO W-6301-LOWVALUE                                    
113500     PERFORM IMS-GU-WL630101                                              
113600                                                                          
113700     PERFORM UNTIL INDX  > MAX-INDX OR W-KVANT-UPD > 5                    
113800** BELOW LINE IS A TEMPORARY FIX TO HANDLE THAT INCORRECT KVRADER         
113900** OCCASIONALLY COMES IN AND THE PROGRAM THEN ABENDS.                     
114000                   OR REQU-IDFAKT(INDX) NOT NUMERIC                       
114100       MOVE REQU-IDFAKT(INDX) TO W-IDFAKT                                 
114200       IF REQU-CMD-IN(INDX) = 'PR'                                        
114300         MOVE JA             TO PRINT-SW                                  
114400         PERFORM HB-SKAPA-PRINTER-TRANS                                   
114500       ELSE                                                               
114600         IF REQU-CMD-IN(INDX) = 'REC'                                     
114700           MOVE NEJ TO ETA-UPD-SW                                         
114800           PERFORM IMS-GNP-WL630111                                       
114900           IF REC-DCS-LAND-NON-VCC-OWNED OR REC-DCS-NDC-NA                
115000           OR REC-DCS-AUSTRALIA OR REC-DCS-JAPAN OR REC-DCS-CDC           
115100             IF 6302-KDTRPSTA NOT = 'R' AND                               
115200                6302-KDTRPSTA NOT = 'M' AND                               
115300                6302-KDTRPSTA NOT = 'L'                                   
115400               MOVE 6302-IDKUNDNR TO ETA-IDKUNDNR                         
115500               IF REC-DCS-CDC                                             
115600                 IF ETA-IDKUNDNR(6:2) = 01 OR 02 OR 17                    
115700                   MOVE '613' TO LETA-KDCALL                              
115800                 ELSE                                                     
115900                   MOVE '608' TO LETA-KDCALL                              
116000                 END-IF                                                   
116100               ELSE                                                       
116200                 MOVE REC-DCS-IDKUNDNR-SORD TO ETA-IDKUNDNR-SORD          
116300                 MOVE REC-DCS-IDKUNDNR-SBPS TO ETA-IDKUNDNR-SBPS          
116400                 IF ETA-IDKUNDNR(6:2) = (ETA-IDKUNDNR-SORD(6:2) OR        
116500                                         ETA-IDKUNDNR-SBPS(6:2))          
116600                   MOVE '613' TO LETA-KDCALL                              
116700                 ELSE                                                     
116800                   MOVE '608' TO LETA-KDCALL                              
116900                 END-IF                                                   
117000               END-IF                                                     
117100               PERFORM S97-CALL-W218ETA                                   
117200             END-IF                                                       
117300           END-IF                                                         
117400           PERFORM HA-UPPDATERA-REC                                       
117500         ELSE                                                             
117600           IF REQU-CMD-IN(INDX) = 'DEC'                                   
117700             IF WS-FLTRACK = 'J'                                          
117800              IF  REQU-IDTRACK(INDX) > SPACE                              
117900                MOVE REQU-IDTRACK(INDX) TO TINL-IDTRACK                   
118000                MOVE 'J' TO WS-WRITE-IDTRACK                              
118100              END-IF                                                      
118200             END-IF                                                       
118300             IF WRITE-IDTRACK                                             
118400               PERFORM HA-UPPDATERA-DEC                                   
118500             END-IF                                                       
118600           ELSE                                                           
118700             IF REQU-CMD-IN(INDX) = 'DAT'                                 
118800               PERFORM HC-UPPDATERA-DAT                                   
118900             ELSE                                                         
119000               IF REQU-CMD-IN(INDX) = 'LOC'                               
119100                 PERFORM HE-UPPDATERA-LOC                                 
119200               ELSE                                                       
119300                 IF REQU-CMD-IN(INDX) = 'C  ' OR 'A  ' OR 'I  '           
119400                   IF REQU-CMD-IN(INDX) = 'C  '                           
119500                      MOVE '605'           TO LETA-KDCALL                 
119600                   ELSE                                                   
119700                      IF REQU-CMD-IN(INDX) = 'A  '                        
119800                         MOVE '606'        TO LETA-KDCALL                 
119900                      ELSE                                                
120000                         IF REQU-CMD-IN(INDX) = 'I  '                     
120100                            MOVE '607' TO LETA-KDCALL                     
120200                         END-IF                                           
120300                      END-IF                                              
120400                   END-IF                                                 
120500                   PERFORM S97-CALL-W218ETA                               
120600                   PERFORM HD-UPPDATERA-CAI                               
120700                 END-IF                                                   
120800               END-IF                                                     
120900             END-IF                                                       
121000           END-IF                                                         
121100         END-IF                                                           
121200       END-IF                                                             
121300       ADD +1 TO INDX                                                     
121400                                                                          
121500     END-PERFORM                                                          
121600     PERFORM UNTIL INDX  > MAX-INDX                                       
121700                OR ATERHOPP                                               
121800       IF  REQU-CMD-IN(INDX) NOT = ALL '+'                                
121900       AND REQU-CMD-IN(INDX) NOT = SPACE                                  
122000         MOVE JA TO ATERHOPP-SW                                           
122100       END-IF                                                             
122200       ADD +1 TO INDX                                                     
122300     END-PERFORM                                                          
122400     .                                                                    
122500     EJECT                                                                
122600 HA-UPPDATERA-DEC SECTION.                                                
122700                                                                          
122800     MOVE REQU-IDFAKT(INDX)  TO W-SEQA-IDFAKT-MIN                         
122900                                W-SEQA-IDFAKT-MAX                         
123000     MOVE SPACE              TO W-SEQA-IDKUNDRF-MIN                       
123100     MOVE ZERO               TO W-SEQA-IDKUNDNR-MIN                       
123200                                W-SEQA-IDKOLLI-MIN                        
123300                                W-SEQA-IDARTNR-MIN                        
123400                                W-SEQA-DAINLEV-MIN                        
123500     MOVE '9999999999'       TO W-SEQA-IDKUNDRF-MAX                       
123600     MOVE 9999999            TO W-SEQA-IDKUNDNR-MAX                       
123700     MOVE 99999              TO W-SEQA-IDKOLLI-MAX                        
123800     MOVE 999999999          TO W-SEQA-IDARTNR-MAX                        
123900     MOVE 9999999999999999   TO W-SEQA-DAINLEV-MAX                        
124000                                                                          
124100     PERFORM IMS-GU-WLINLD01                                              
124200     PERFORM UNTIL SEGMENT-SAKNAS                                         
124300        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
124400        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
124500        IF SEQA-IDPTYP = 'R30' OR '310'                                   
124600          PERFORM IMS-GHU-WDL623                                          
124700          IF SEGMENT-SAKNAS                                               
124800             MOVE REQU-IDTRACK(INDX) TO TINL-IDTRACK                      
124900             PERFORM IMS-ISRT-WDL623                                      
125000          ELSE                                                            
125100             MOVE REQU-IDTRACK(INDX) TO TINL-IDTRACK                      
125200             PERFORM IMS-REPL-WDL623                                      
125300          END-IF                                                          
125400        END-IF                                                            
125500        PERFORM IMS-GN-WLINLD01                                           
125600     END-PERFORM                                                          
125700                                                                          
125800     .                                                                    
125900     EJECT                                                                
126000 HA-UPPDATERA-REC SECTION.                                                
126100                                                                          
126200     MOVE NEJ TO SW-REC-UPPD                                              
126300                 ATERHOPP-SW                                              
126400     MOVE REQU-IDFAKT(INDX)  TO W-SEQA-IDFAKT-MIN                         
126500                                W-SEQA-IDFAKT-MAX                         
126600     MOVE SPACE              TO W-SEQA-IDKUNDRF-MIN                       
126700     MOVE ZERO               TO W-SEQA-IDKUNDNR-MIN                       
126800                                W-SEQA-IDKOLLI-MIN                        
126900                                W-SEQA-IDARTNR-MIN                        
127000                                W-SEQA-DAINLEV-MIN                        
127100     MOVE '9999999999'       TO W-SEQA-IDKUNDRF-MAX                       
127200     MOVE 9999999            TO W-SEQA-IDKUNDNR-MAX                       
127300     MOVE 99999              TO W-SEQA-IDKOLLI-MAX                        
127400     MOVE 999999999          TO W-SEQA-IDARTNR-MAX                        
127500     MOVE 9999999999999999   TO W-SEQA-DAINLEV-MAX                        
127600     MOVE 'R30'              TO W-IDPTYP                                  
127700                                                                          
127800     PERFORM IMS-GU-WDL6A1                                                
127900     PERFORM UNTIL SEGMENT-SAKNAS OR W-KVANT-UPD > 5                      
128000                                                                          
128100        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
128200        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
128300        MOVE NEJ          TO INLEV-SW                                     
128400        PERFORM IMS-GHU-INLC11                                            
128500        IF INL-TIINLMOT > ZERO                                            
128600          MOVE JA TO INLEV-SW                                             
128700        END-IF                                                            
128800        MOVE INL-IDDISTR              TO WS-SAP-IDDISTR                   
128900        MOVE INL-IDKUNDNR             TO WS-SAP-IDKUNDNR                  
129000        MOVE INL-PRARTNTO             TO WS-SAP-PRARTNTO                  
129100        MOVE INL-KVAVIS               TO W-KVAVIS                         
129200        MOVE '310'                    TO INL-IDPTYP                       
129300        IF REC-DCS-LAND-NON-VCC-OWNED OR REC-DCS-NDC-NA                   
129400        OR REC-DCS-AUSTRALIA OR REC-DCS-JAPAN OR REC-DCS-CDC              
129500        OR REC-DCS-SDC                                                    
129600                                                                          
129700           IF ETA-UPD-OK                                                  
129800             MOVE LETA-TIAAMMDD-SVAR    TO INL-TIBERANK                   
129900           END-IF                                                         
130000           IF REQU-ADINLOMR(INDX) NOT = ALL '+'                           
130100             MOVE REQU-ADINLOMR (INDX) TO INL-ADINLOMR                    
130200           END-IF                                                         
130300           MOVE LOCAL-DATE            TO INL-TIINLMOT                     
130400           MOVE LOCAL-TIME(1:4)       TO INL-TIINLMTI                     
130500        ELSE                                                              
130600           MOVE LOCAL-DATE               TO INL-TIINLMOT                  
130700           MOVE LOCAL-TIME(1:4)          TO INL-TIINLMTI                  
130800        END-IF                                                            
130900        PERFORM IMS-REPL-INLC11                                           
131000        IF REC-DCS-CDC                                                    
131100         PERFORM IMS-GHU-WDK611                                           
131200         ADD W-KVAVIS       TO CLAG-KVAKS-CDC                             
131300         SUBTRACT W-KVAVIS                                                
131400                          FROM CLAG-KVAKS-PAV                             
131500         MOVE CLAG-PRARTSTD TO WS-SAP-PRARTSTD                            
131600         PERFORM IMS-REPL-WDK611                                          
131700         PERFORM HAA-SKAPA-SALDOLOGG                                      
131800         MOVE 6302-IDDC-SEND TO WS-IDDC                                   
131900         IF (NDC-CN OR NDC-US)                                            
132000         AND AKTUELLT-LAND-SVERIGE                                        
132100         AND INLEV-NEJ                                                    
132200           PERFORM S99-SAP-TRANS                                          
132300         END-IF                                                           
132400        ELSE                                                              
132500         PERFORM IMS-GHU-ARTS11                                           
132600         IF SEGMENT-FINNS                                                 
132700            ADD W-KVAVIS        TO SLAG-KVAKS-SDC                         
132800            SUBTRACT W-KVAVIS FROM SLAG-KVAKS-PAV                         
132900            MOVE SLAG-PRAVCOST  TO WS-SAP-PRAVCOST                        
133000            PERFORM IMS-REPL-ARTS11                                       
133100            PERFORM HAA-SKAPA-SALDOLOGG                                   
133200            MOVE 6302-IDDC-SEND TO WS-IDDC                                
133300            MOVE 6302-IDDISTR   TO DIST35-IDDISTR                         
133400**** SENDING DC IS CDC                                                    
133500            IF  (CDC-SE                                                   
133600            AND REC-DCS-LAND-NON-VCC-OWNED                                
133700            AND INLEV-NEJ)                                                
133800            OR  (CDC-SE                                                   
133900            AND REC-DCS-USA                                               
134000**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
134100**** REST OF FLOWS TO US IS IN LAB.                                       
134200**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
134300****             DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                   
134400            AND DIST35-NDCCN-NDCUS-REFILL                                 
134500            AND INLEV-NEJ)                                                
134600**** SENDING DC IS COUNTRY CODE (DIRECT DELIVERY)                         
134700            OR  (GOOD-DDC                                                 
134800            AND REC-DCS-LAND-NON-VCC-OWNED                                
134900            AND INLEV-NEJ)                                                
135000**** WHEN A COLLI IS REPORTED MIS, NO BOOKING SHOULD BE DONE              
135100**** WHEN FOUND                                                           
135200              IF 6302-KDTRPSTA = 'M'                                      
135300                CONTINUE                                                  
135400              ELSE                                                        
135500                IF REC-DCS-KDTRADP = 'BR12'                               
135700                  MOVE YES TO REQU-FLREC(INDX)                            
135800                ELSE                                                      
135900                  PERFORM S98-SAP-TRANS                                   
136000                END-IF                                                    
136100              END-IF                                                      
136200            ELSE                                                          
136300              IF (DIST35-NONVCC-VCC-REFILL OR                             
136400                 DIST35-NONVCC-VCC-TRANSFER) AND                          
136500                INLEV-NEJ                                                 
136600                IF 6302-KDTRPSTA = 'M'                                    
136700                  CONTINUE                                                
136800                ELSE                                                      
136900                  PERFORM S99-SAP-TRANS                                   
137000                END-IF                                                    
137100              END-IF                                                      
137200            END-IF                                                        
137300         END-IF                                                           
137400        END-IF                                                            
137500        MOVE JA TO SW-REC-UPPD                                            
137600        ADD 1 TO W-KVANT-UPD                                              
137700        PERFORM IMS-GN-WDL6A1                                             
137800     END-PERFORM                                                          
137900                                                                          
138000     IF SEGMENT-FINNS                                                     
138100        MOVE JA TO ATERHOPP-SW                                            
138200     ELSE                                                                 
138300        IF SW-REC-UPPD = JA                                               
138400           MOVE TRAILER-RECEIVED TO RESP-IDMSG-INFO                       
138500        END-IF                                                            
138600     END-IF                                                               
138700     IF SW-REC-UPPD = JA                                                  
138800        IF REC-DCS-LAND-NON-VCC-OWNED OR REC-DCS-NDC-NA                   
138900        OR REC-DCS-AUSTRALIA OR REC-DCS-JAPAN OR REC-DCS-CDC              
139000        OR REC-DCS-SDC                                                    
139100                                                                          
139200           IF ETA-UPD-OK                                                  
139300             PERFORM IMS-GNP-WL630111                                     
139400             PERFORM IMS-DLET-WL630111                                    
139500             MOVE 'R'                     TO 6302-KDTRPSTA                
139600             MOVE LETA-TIAAMMDD-SVAR      TO 6302-DABERANK                
139700             MOVE 20                      TO 6302-DABERANK (1:2)          
139800             IF REQU-ADINLOMR(INDX) NOT = ALL '+'                         
139900               MOVE REQU-ADINLOMR (INDX)  TO 6302-ADINLOMR                
140000             END-IF                                                       
140100             MOVE NEJ   TO 6302-FLMANETA                                  
140200             MOVE SPACE TO 6302-IDUSER-MANETA                             
140300             PERFORM IMS-ISRT-WL630111                                    
140400           ELSE                                                           
140500             PERFORM IMS-GHNP-WL630111                                    
140600             MOVE 'R'                     TO 6302-KDTRPSTA                
140700             PERFORM IMS-REPL-WL630111                                    
140800           END-IF                                                         
140900        ELSE                                                              
141000           PERFORM IMS-GHNP-WL630111                                      
141100           MOVE REQU-CMD-IN(INDX) TO 6302-KDTRPSTA                        
141200           PERFORM IMS-REPL-WL630111                                      
141300        END-IF                                                            
141400     END-IF                                                               
141500     .                                                                    
141600     EJECT                                                                
141700 HAA-SKAPA-SALDOLOGG SECTION.                                             
141800                                                                          
141900     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
142000     MOVE 9                         TO LOGG-IDSEKVNR                      
142100     MOVE W-IDDC                    TO LOGG-IDDC                          
142200     MOVE 'INBO'                    TO LOGG-IDHUVTYP                      
142300     MOVE '310'                     TO LOGG-IDSUBTYP                      
142400     MOVE 'WL010100'                TO LOGG-IDPGM                         
142500     MOVE 'L101'                    TO LOGG-IDTRANS                       
142600     MOVE MSG-SIGNON-USERID         TO LOGG-IDUSER                        
142700     MOVE SPACE                     TO LOGG-REF                           
142800     MOVE W-IDFAKT                  TO LOGG-IDFAKT                        
142900     MOVE W-IDLBBET                 TO LOGG-IDLBBET                       
143000     MOVE WS-TIFAKT                 TO LOGG-TIFAKT                        
143100     MOVE '+'                       TO LOGG-IDTECKEN-KVAKS                
143200     MOVE '-'                       TO LOGG-IDTECKEN-KVAKS-PAV            
143300     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
143400     MOVE SPACE                     TO LOGG-IDTECKEN-KVLS                 
143500     MOVE W-KVAVIS                  TO LOGG-KVART-SALDO                   
143600     IF REC-DCS-CDC                                                       
143700        COMPUTE LOGG-KVAKS          =  CLAG-KVAKS-CDC                     
143800                                    +  CLAG-KVAKS-T                       
143900        MOVE CLAG-KVAKS-PAV         TO LOGG-KVAKS-PAV                     
144000        MOVE CLAG-KVEFRS            TO LOGG-KVEFRS                        
144100        MOVE CLAG-KVLS              TO LOGG-KVLS                          
144200     ELSE                                                                 
144300        MOVE SLAG-KVAKS-SDC         TO LOGG-KVAKS                         
144400        MOVE SLAG-KVAKS-PAV         TO LOGG-KVAKS-PAV                     
144500        MOVE SLAG-KVEFRS            TO LOGG-KVEFRS                        
144600        MOVE SLAG-KVLS              TO LOGG-KVLS                          
144700     END-IF                                                               
144800     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
144900     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
145000     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
145100     ACCEPT WLOGG-TID FROM TIME                                           
145200     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
145300                                                                          
145400     PERFORM IMS-ISRT-WDL901                                              
145500     IF SEGMENT-FINNS-REDAN                                               
145600       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
145700          ADD -1 TO LOGG-IDSEKVNR                                         
145800          PERFORM IMS-ISRT-WDL901                                         
145900       END-PERFORM                                                        
146000     END-IF                                                               
146100     .                                                                    
146200     EJECT                                                                
146300 HB-SKAPA-PRINTER-TRANS SECTION.                                          
146400                                                                          
146500     PERFORM HBA-BYGG-PRINTTABELL                                         
146600     IF TAB-RAD-IX > MAX-TAB-RAD-IX                                       
146700        MOVE 'SORTTABELLEN FULL' TO FELTEXT                               
146800     END-IF                                                               
146900                                                                          
147000     MOVE +78 TO STEGLAANGD                                               
147100     COMPUTE ANTAL = TAB-RAD-IX - 1                                       
147200     MOVE +21 TO NYCKELLAANGD                                             
147300                                                                          
147400     IF ANTAL > 1                                                         
147500        CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                   
147600        TAB-SORTBEGREPP(1) NYCKELLAANGD                                   
147700     END-IF                                                               
147800                                                                          
147900     MOVE 1                TO TAB-RAD-IX                                  
148000       MOVE 1                TO HDR-REQU-IDMSGVER                         
148100       MOVE 'R'              TO HDR-REQU-KDPGMACT                         
148200       MOVE REQU-IDUSER      TO HDR-REQU-IDUSER                           
148300                                                                          
148400       MOVE SPACE            TO HDR-IDOUTREC                              
148500       MOVE 'UNLOADING-LIST' TO HDR-IDOUTTYPE                             
148600       MOVE TAB-IDDC(1)      TO HDR-IDOUTREC(1:2)                         
148700       MOVE TAB-IDUSER(1)    TO HDR-IDOUTREC(3:8)                         
148800       MOVE TAB-IDFAKT(1)    TO HDR-IDLIST                                
148900                                                                          
149000     IF FORSTA-POST                                                       
149100       PERFORM S90-SEND-OPEN                                              
149200       MOVE NEJ TO FORSTA-POST-SW                                         
149300     END-IF                                                               
149400                                                                          
149500       PERFORM S91-PUT-HEADER                                             
149600     MOVE TAB-IDLBBET(1)   TO UT1-IDLBBET                                 
149700     MOVE TAB-IDFAKT(1)    TO UT1-IDLIST                                  
149800     MOVE TAB-IDUSER(1)    TO UT1-IDUSER                                  
149900     MOVE TAB-IDDC(1)      TO UT1-IDDC                                    
150000     MOVE '1'              TO UT1-IDAFPRCD                                
150100     PERFORM S92-PUT-DOC-HEAD                                             
150200                                                                          
150300     MOVE '2'              TO UT2-IDAFPRCD                                
150400     PERFORM UNTIL TAB-RAD-IX > ANTAL                                     
150500       IF TAB-RAD-IX > 1                                                  
150600         IF TAB-IDLBBET(TAB-RAD-IX)  = TAB-IDLBBET(TAB-RAD-IX - 1)        
150700         AND TAB-IDFAKT (TAB-RAD-IX) = TAB-IDFAKT (TAB-RAD-IX - 1)        
150800         AND TAB-IDUSER (TAB-RAD-IX) = TAB-IDUSER (TAB-RAD-IX - 1)        
150900         AND TAB-IDDC   (TAB-RAD-IX) = TAB-IDDC   (TAB-RAD-IX - 1)        
151000             CONTINUE                                                     
151100         ELSE                                                             
151200             MOVE TAB-IDLBBET(TAB-RAD-IX) TO UT1-IDLBBET                  
151300             MOVE TAB-IDFAKT (TAB-RAD-IX) TO UT1-IDLIST                   
151400             MOVE TAB-IDUSER (TAB-RAD-IX) TO UT1-IDUSER                   
151500             MOVE TAB-IDDC   (TAB-RAD-IX) TO UT1-IDDC                     
151600             PERFORM S92-PUT-DOC-HEAD                                     
151700         END-IF                                                           
151800       END-IF                                                             
151900       MOVE TAB-IDKUNDRF     (TAB-RAD-IX) TO UT2-IDKUNDRF                 
152000       MOVE TAB-IDKOLLI      (TAB-RAD-IX) TO UT2-IDKOLLI                  
152100       MOVE TAB-IDKUNDNR     (TAB-RAD-IX) TO UT2-IDKUNDNR                 
152200       MOVE TAB-KVANTAL-KOLLI(TAB-RAD-IX) TO UT2-KVANTAL-KOLLI            
152300       MOVE TAB-KVANTAL-PRIO (TAB-RAD-IX) TO UT2-KVANTAL-PRIO             
152400       MOVE TAB-KVANTAL-BO   (TAB-RAD-IX) TO UT2-KVANTAL-BO               
152500       MOVE TAB-KVANTAL-NEW  (TAB-RAD-IX) TO UT2-KVANTAL-NEW              
152600       MOVE TAB-TEINFO       (TAB-RAD-IX) TO UT2-TEINFO                   
152700                                                                          
152800       PERFORM S93-PUT-DOC-LINE                                           
152900       ADD +1 TO TAB-RAD-IX                                               
153000     END-PERFORM                                                          
153100     MOVE JA TO PRINT-SW                                                  
153200     .                                                                    
153300     EJECT                                                                
153400 HBA-BYGG-PRINTTABELL SECTION.                                            
153500                                                                          
153600     INITIALIZE SORT-TABELL                                               
153700                                                                          
153800     MOVE 1 TO TAB-RAD-IX                                                 
153900                                                                          
154000     MOVE REQU-IDFAKT(INDX) TO W-IDFAKT-MIN                               
154100                               W-IDFAKT-MAX                               
154200                               W-IDFAKT                                   
154300                                                                          
154400     PERFORM IMS-GU-WDL6A                                                 
154500     IF SEGMENT-FINNS                                                     
154600                                                                          
154700        PERFORM HBAB-SKAPA-TABPOST                                        
154800                                                                          
154900        PERFORM UNTIL SEGMENT-SAKNAS                                      
155000                                                                          
155100           IF  W-SPAR-IDKUNDRF = L6-INL-IDKUNDRF                          
155200           AND W-SPAR-IDKUNDNR = L6-INL-IDKUNDNR                          
155300           AND W-SPAR-IDKOLLI  = L6-INL-IDKOLLI                           
155400                                                                          
155500             PERFORM HBAA-KOLLA-ARTIKEL                                   
155600                                                                          
155700           ELSE                                                           
155800                                                                          
155900              COMPUTE TAB-KVANTAL-SORT(TAB-RAD-IX)                        
156000                      = 9999999 - W-KVPRIOART                             
156100                                                                          
156200              MOVE W-KVRADER    TO TAB-KVANTAL-KOLLI (TAB-RAD-IX)         
156300              MOVE W-KVNYART    TO TAB-KVANTAL-NEW   (TAB-RAD-IX)         
156400              MOVE W-KVPRIOART  TO TAB-KVANTAL-PRIO  (TAB-RAD-IX)         
156500              MOVE W-KVBO       TO TAB-KVANTAL-BO    (TAB-RAD-IX)         
156600                                                                          
156700              PERFORM IMS-GU-WDR5                                         
156800              MOVE R5-6302-IDLBBET TO TAB-IDLBBET    (TAB-RAD-IX)         
156900              MOVE ZERO         TO TAB-DABERANK      (TAB-RAD-IX)         
157000              ADD +1 TO TAB-RAD-IX                                        
157100                                                                          
157200              PERFORM HBAB-SKAPA-TABPOST                                  
157300              PERFORM HBAA-KOLLA-ARTIKEL                                  
157400           END-IF                                                         
157500                                                                          
157600           PERFORM IMS-GN-WDL6A                                           
157700        END-PERFORM                                                       
157800                                                                          
157900        IF TAB-IDKUNDRF(TAB-RAD-IX) NOT = SPACE                           
158000           COMPUTE TAB-KVANTAL-SORT(TAB-RAD-IX)                           
158100                   = 9999999 - W-KVPRIOART                                
158200                                                                          
158300           MOVE W-KVRADER       TO TAB-KVANTAL-KOLLI (TAB-RAD-IX)         
158400           MOVE W-KVNYART       TO TAB-KVANTAL-NEW   (TAB-RAD-IX)         
158500           MOVE W-KVPRIOART     TO TAB-KVANTAL-PRIO  (TAB-RAD-IX)         
158600           MOVE W-KVBO          TO TAB-KVANTAL-BO    (TAB-RAD-IX)         
158700                                                                          
158800           PERFORM IMS-GU-WDR5                                            
158900           MOVE R5-6302-IDLBBET TO TAB-IDLBBET       (TAB-RAD-IX)         
159000           MOVE ZERO            TO TAB-DABERANK      (TAB-RAD-IX)         
159100           ADD +1 TO TAB-RAD-IX                                           
159200        END-IF                                                            
159300                                                                          
159400     END-IF                                                               
159500     .                                                                    
159600     EJECT                                                                
159700 HBAA-KOLLA-ARTIKEL SECTION.                                              
159800                                                                          
159900     ADD 1      TO W-KVRADER                                              
160000     PERFORM HBAAA-LAS-FRAM-ARTIKEL                                       
160100     ADD +1     TO W-ANTAL-LASN                                           
160200                                                                          
160300     IF REC-DCS-CDC                                                       
160400       PERFORM IMS-GU-WDK601                                              
160500       PERFORM IMS-GN-WDK611                                              
160600                                                                          
160700       IF CLAG-ADLAGOMR = ZERO                                            
160800          ADD 1 TO W-KVNYART                                              
160900       END-IF                                                             
161000       IF CLAG-KVROS >= L6-INL-KVAVIS                                     
161100          ADD +1 TO W-KVBO                                                
161200       END-IF                                                             
161300                                                                          
161400       IF L6-INL-FLPRIO = 'J' OR 'Y'                                      
161500          ADD +1 TO W-KVPRIOART                                           
161600       END-IF                                                             
161700                                                                          
161800     ELSE                                                                 
161900       PERFORM IMS-GU-ARTS11                                              
162000                                                                          
162100       IF SLAG-ADLAGOMR = ZERO                                            
162200          ADD 1 TO W-KVNYART                                              
162300       END-IF                                                             
162400       COMPUTE WS-KVROS = SLAG-KVROS-DAG + SLAG-KVROS-BULK                
162500       IF WS-KVROS >= L6-INL-KVAVIS                                       
162600          ADD +1 TO W-KVBO                                                
162700       END-IF                                                             
162800                                                                          
162900       IF L6-INL-FLPRIO = 'J' OR 'Y'                                      
163000          ADD +1 TO W-KVPRIOART                                           
163100       END-IF                                                             
163200     END-IF                                                               
163300     .                                                                    
163400     EJECT                                                                
163500 HBAAA-LAS-FRAM-ARTIKEL SECTION.                                          
163600                                                                          
163700     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
163800     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
163900     MOVE L6-INL-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
164000                             W-SEQA-IDFAKT-MAX                            
164100     MOVE L6-INL-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
164200                             W-SEQA-IDKUNDRF-MAX                          
164300     MOVE L6-INL-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
164400                             W-SEQA-IDKUNDNR-MAX                          
164500     MOVE L6-INL-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
164600                             W-SEQA-IDKOLLI-MAX                           
164700                                                                          
164800     IF W-ANTAL-LASN = ZERO                                               
164900        PERFORM IMS-GU-WLINLD01                                           
165000     ELSE                                                                 
165100        PERFORM IMS-GN-WLINLD01                                           
165200     END-IF                                                               
165300                                                                          
165400     MOVE SEQA-IDARTNR    TO W-IDARTNR                                    
165500     .                                                                    
165600     EJECT                                                                
165700 HBAB-SKAPA-TABPOST SECTION.                                              
165800                                                                          
165900     MOVE REQU-IDUSER      TO TAB-IDUSER      (TAB-RAD-IX)                
166000     MOVE L6-INL-IDFAKT    TO TAB-IDFAKT      (TAB-RAD-IX)                
166100     MOVE L6-INL-IDDC      TO TAB-IDDC        (TAB-RAD-IX)                
166200                              W-R5-IDDC                                   
166300     MOVE L6-INL-IDKUNDRF  TO TAB-IDKUNDRF    (TAB-RAD-IX)                
166400                              W-SPAR-IDKUNDRF                             
166500     MOVE L6-INL-IDKUNDNR  TO TAB-IDKUNDNR    (TAB-RAD-IX)                
166600                              W-SPAR-IDKUNDNR                             
166700     MOVE L6-INL-IDKOLLI   TO TAB-IDKOLLI     (TAB-RAD-IX)                
166800                              W-SPAR-IDKOLLI                              
166900                                                                          
167000     IF L6-INL-IDPTYP      =  'R30'                                       
167100     AND L6-INL-TIINLMOT   >   ZERO                                       
167200         MOVE 'MISSING' TO TAB-TEINFO      (TAB-RAD-IX)                   
167300     END-IF                                                               
167400                                                                          
167500     MOVE ZERO TO  W-KVRADER                                              
167600                   W-KVNYART                                              
167700                   W-KVPRIOART                                            
167800                   W-ANTAL-LASN                                           
167900                   W-KVBO                                                 
168000     .                                                                    
168100     EJECT                                                                
168200 HC-UPPDATERA-DAT SECTION.                                                
168300                                                                          
168400     IF REC-DCS-LAND-NON-VCC-OWNED OR REC-DCS-NDC-NA                      
168500     OR REC-DCS-AUSTRALIA OR REC-DCS-JAPAN OR REC-DCS-CDC                 
168600       PERFORM IMS-GNP-WL630111                                           
168700       IF SEGMENT-FINNS                                                   
168800         PERFORM IMS-DLET-WL630111                                        
168900         IF SEGMENT-FINNS                                                 
169000           MOVE 20                  TO 6302-DABERANK(1:2)                 
169100           MOVE REQU-TIBERANK(INDX) TO 6302-DABERANK(3:6)                 
169200           MOVE YES TO 6302-FLMANETA                                      
169300           MOVE REQU-IDUSER TO 6302-IDUSER-MANETA                         
169400           PERFORM IMS-ISRT-WL630111                                      
169500         END-IF                                                           
169600       END-IF                                                             
169700                                                                          
169800       MOVE NEJ TO SW-REC-UPPD                                            
169900                   ATERHOPP-SW                                            
170000       MOVE REQU-IDFAKT(INDX)  TO W-SEQA-IDFAKT-MIN                       
170100                                  W-SEQA-IDFAKT-MAX                       
170200       MOVE SPACE              TO W-SEQA-IDKUNDRF-MIN                     
170300       MOVE ZERO               TO W-SEQA-IDKUNDNR-MIN                     
170400                                  W-SEQA-IDKOLLI-MIN                      
170500                                  W-SEQA-IDARTNR-MIN                      
170600                                  W-SEQA-DAINLEV-MIN                      
170700       MOVE '9999999999'       TO W-SEQA-IDKUNDRF-MAX                     
170800       MOVE 9999999            TO W-SEQA-IDKUNDNR-MAX                     
170900       MOVE 99999              TO W-SEQA-IDKOLLI-MAX                      
171000       MOVE 999999999          TO W-SEQA-IDARTNR-MAX                      
171100       MOVE 9999999999999999   TO W-SEQA-DAINLEV-MAX                      
171200       MOVE 'R30'              TO W-IDPTYP                                
171300                                                                          
171400       PERFORM IMS-GU-WDL6A1                                              
171500       PERFORM UNTIL SEGMENT-SAKNAS                                       
171600          MOVE SEQA-IDARTNR TO W-IDARTNR                                  
171700          MOVE SEQA-DAINLEV TO W-DAINLEV                                  
171800          PERFORM IMS-GHU-INLC11                                          
171900                                                                          
172000          MOVE INL-KVAVIS               TO W-KVAVIS                       
172100          MOVE REQU-TIBERANK(INDX)   TO INL-TIBERANK                      
172200          PERFORM IMS-REPL-INLC11                                         
172300          MOVE JA TO SW-REC-UPPD                                          
172400                                                                          
172500          PERFORM IMS-GN-WDL6A1                                           
172600       END-PERFORM                                                        
172700                                                                          
172800       IF SW-REC-UPPD = JA                                                
172900          MOVE TRAILER-RECEIVED TO RESP-IDMSG-INFO                        
173000       END-IF                                                             
173100                                                                          
173200     END-IF                                                               
173300     .                                                                    
173400     EJECT                                                                
173500 HD-UPPDATERA-CAI SECTION.                                                
173600     MOVE NEJ TO SW-REC-UPPD                                              
173700*                SW-LOC-UPPD                                              
173800                                                                          
173900     MOVE REQU-IDFAKT(INDX) TO W-SEQA-IDFAKT-MIN                          
174000                               W-SEQA-IDFAKT-MAX                          
174100     MOVE SPACE             TO W-SEQA-IDKUNDRF-MIN                        
174200     MOVE ZERO              TO W-SEQA-IDKUNDNR-MIN                        
174300                               W-SEQA-IDKOLLI-MIN                         
174400                               W-SEQA-IDARTNR-MIN                         
174500                               W-SEQA-DAINLEV-MIN                         
174600     MOVE '9999999999'      TO W-SEQA-IDKUNDRF-MAX                        
174700     MOVE 9999999           TO W-SEQA-IDKUNDNR-MAX                        
174800     MOVE 99999             TO W-SEQA-IDKOLLI-MAX                         
174900     MOVE 999999999         TO W-SEQA-IDARTNR-MAX                         
175000     MOVE 9999999999999999  TO W-SEQA-DAINLEV-MAX                         
175100     MOVE 'R30'             TO W-IDPTYP                                   
175200                                                                          
175300     PERFORM IMS-GN-WDL6A1                                                
175400                                                                          
175500     PERFORM UNTIL SEGMENT-SAKNAS OR W-KVANT-UPD > 100                    
175600        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
175700        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
175800                                                                          
175900        PERFORM IMS-GHU-INLC11                                            
176000        IF LETA-TIAAMMDD-SVAR = INL-TIBERANK                              
176100          CONTINUE                                                        
176200        ELSE                                                              
176300          MOVE LETA-TIAAMMDD-SVAR TO INL-TIBERANK                         
176400          ADD 1 TO W-KVANT-UPD                                            
176500        END-IF                                                            
176600                                                                          
176700        PERFORM IMS-REPL-INLC11                                           
176800        MOVE JA TO SW-REC-UPPD                                            
176900                                                                          
177000        PERFORM IMS-GN-WDL6A1                                             
177100     END-PERFORM                                                          
177200                                                                          
177300     IF  SEGMENT-FINNS                                                    
177400        MOVE JA TO ATERHOPP-SW                                            
177500     ELSE                                                                 
177600        IF SW-REC-UPPD = JA                                               
177700           MOVE TRAILER-RECEIVED TO RESP-IDMSG-INFO                       
177800        END-IF                                                            
177900     END-IF                                                               
178000                                                                          
178100     IF SW-REC-UPPD = JA                                                  
178200        PERFORM IMS-GNP-WL630111                                          
178300        PERFORM IMS-DLET-WL630111                                         
178400        IF REQU-CMD-IN(INDX) = 'A' OR 'I' OR 'C'                          
178500           MOVE REQU-CMD-IN(INDX)   TO 6302-KDTRPSTA                      
178600        END-IF                                                            
178700        MOVE LETA-TIAAMMDD-SVAR TO 6302-DABERANK                          
178800        IF LETA-TIAAMMDD-SVAR NOT = ZERO                                  
178900          IF LETA-TIAAMMDD-SVAR < 500000                                  
179000            MOVE 20                 TO 6302-DABERANK (1:2)                
179100          ELSE                                                            
179200            IF LETA-TIAAMMDD-SVAR < 999999                                
179300              MOVE 19               TO 6302-DABERANK (1:2)                
179400            ELSE                                                          
179500              MOVE 99999999         TO 6302-DABERANK                      
179600            END-IF                                                        
179700          END-IF                                                          
179800        END-IF                                                            
179900        MOVE NEJ   TO 6302-FLMANETA                                       
180000        MOVE SPACE TO 6302-IDUSER-MANETA                                  
180100        PERFORM IMS-ISRT-WL630111                                         
180200     END-IF                                                               
180300     .                                                                    
180400     EJECT                                                                
180500 HE-UPPDATERA-LOC SECTION.                                                
180600                                                                          
180700     IF REC-DCS-LAND-NON-VCC-OWNED OR REC-DCS-NDC-NA                      
180800     OR REC-DCS-AUSTRALIA OR REC-DCS-JAPAN OR REC-DCS-CDC                 
180900     OR REC-DCS-SDC                                                       
181000       PERFORM IMS-GHNP-WL630111                                          
181100       IF SEGMENT-FINNS                                                   
181200         MOVE REQU-ADINLOMR(INDX) TO 6302-ADINLOMR                        
181300         PERFORM IMS-REPL-WL630111                                        
181400       END-IF                                                             
181500                                                                          
181600       MOVE NEJ TO SW-REC-UPPD                                            
181700                   ATERHOPP-SW                                            
181800       MOVE REQU-IDFAKT(INDX)  TO W-SEQA-IDFAKT-MIN                       
181900                                  W-SEQA-IDFAKT-MAX                       
182000       MOVE SPACE              TO W-SEQA-IDKUNDRF-MIN                     
182100       MOVE ZERO               TO W-SEQA-IDKUNDNR-MIN                     
182200                                  W-SEQA-IDKOLLI-MIN                      
182300                                  W-SEQA-IDARTNR-MIN                      
182400                                  W-SEQA-DAINLEV-MIN                      
182500       MOVE '9999999999'       TO W-SEQA-IDKUNDRF-MAX                     
182600       MOVE 9999999            TO W-SEQA-IDKUNDNR-MAX                     
182700       MOVE 99999              TO W-SEQA-IDKOLLI-MAX                      
182800       MOVE 999999999          TO W-SEQA-IDARTNR-MAX                      
182900       MOVE 9999999999999999   TO W-SEQA-DAINLEV-MAX                      
183000       MOVE 'R30'              TO W-IDPTYP                                
183100                                                                          
183200       PERFORM IMS-GU-WDL6A1                                              
183300       PERFORM UNTIL SEGMENT-SAKNAS                                       
183400          MOVE SEQA-IDARTNR TO W-IDARTNR                                  
183500          MOVE SEQA-DAINLEV TO W-DAINLEV                                  
183600          PERFORM IMS-GHU-INLC11                                          
183700                                                                          
183800          MOVE REQU-ADINLOMR(INDX)   TO INL-ADINLOMR                      
183900          MOVE JA TO SW-REC-UPPD                                          
184000          PERFORM IMS-REPL-INLC11                                         
184100                                                                          
184200          PERFORM IMS-GN-WDL6A1                                           
184300       END-PERFORM                                                        
184400                                                                          
184500       IF SW-REC-UPPD = JA                                                
184600          MOVE TRAILER-RECEIVED TO RESP-IDMSG-INFO                        
184700       END-IF                                                             
184800                                                                          
184900     END-IF                                                               
185000     .                                                                    
185100     EJECT                                                                
185200 I-TRIGGER-BR-TRANS SECTION.                                              
185300                                                                          
185400     MOVE +1        TO INDX                                               
185500     PERFORM UNTIL INDX  > MAX-INDX                                       
185600       IF REQU-FLREC(INDX) = YES                                          
185700         MOVE REQU-IDFAKT(INDX) TO W-IDFAKT                               
185900         MOVE W-IDDC    TO W-6301-IDDC                                    
186000         MOVE '6301'    TO W-6301-IDHTYP                                  
186100         MOVE LOW-VALUE TO W-6301-LOWVALUE                                
186200         PERFORM IMS-GU-WL630101                                          
186300         PERFORM IMS-GNP-WL630111                                         
186400         MOVE REQU-IDFAKT(INDX)  TO W-SEQA-IDFAKT-MIN                     
186500                                    W-SEQA-IDFAKT-MAX                     
186600         MOVE SPACE              TO W-SEQA-IDKUNDRF-MIN                   
186700         MOVE ZERO               TO W-SEQA-IDKUNDNR-MIN                   
186800                                    W-SEQA-IDKOLLI-MIN                    
186900                                    W-SEQA-IDARTNR-MIN                    
187000                                    W-SEQA-DAINLEV-MIN                    
187100         MOVE '9999999999'       TO W-SEQA-IDKUNDRF-MAX                   
187200         MOVE 9999999            TO W-SEQA-IDKUNDNR-MAX                   
187300         MOVE 99999              TO W-SEQA-IDKOLLI-MAX                    
187400         MOVE 999999999          TO W-SEQA-IDARTNR-MAX                    
187500         MOVE 9999999999999999   TO W-SEQA-DAINLEV-MAX                    
187600         MOVE '310'              TO W-IDPTYP                              
187700         PERFORM IMS-GU-WDL6A1                                            
187800         PERFORM UNTIL SEGMENT-SAKNAS                                     
187900                                                                          
188000            MOVE SEQA-IDARTNR TO W-IDARTNR                                
188100            MOVE SEQA-DAINLEV TO W-DAINLEV                                
188200            PERFORM IMS-GU-INLC11                                         
188300            MOVE INL-IDDISTR              TO WS-SAP-IDDISTR               
188400            MOVE INL-IDKUNDNR             TO WS-SAP-IDKUNDNR              
188500            MOVE INL-PRARTNTO             TO WS-SAP-PRARTNTO              
188600            MOVE INL-KVAVIS               TO W-KVAVIS                     
188700            MOVE INL-IDORDNR5             TO W-IDORDER                    
188800            MOVE INL-IDKOLLI              TO W-IDKOLLI                    
188900            PERFORM IMS-GU-ARTS11                                         
189000            MOVE SLAG-PRAVCOST  TO WS-SAP-PRAVCOST                        
189100            MOVE 6302-IDDC-SEND TO WS-IDDC                                
189200            MOVE 6302-IDDISTR   TO DIST35-IDDISTR                         
189300            PERFORM S98-SAP-TRANS                                         
189400            PERFORM IMS-GN-WDL6A1                                         
189500         END-PERFORM                                                      
189600       END-IF                                                             
189700       ADD +1 TO INDX                                                     
189800     END-PERFORM                                                          
189900     IF FIRST-REC-TRANS                                                   
190100       PERFORM S13-SEND-CLOSE                                             
190200     END-IF                                                               
190300     .                                                                    
190400     EJECT                                                                
190500 K-OMSTART-EGEN-TRANS SECTION.                                            
190600                                                                          
190700     PERFORM S03-SEND-OPEN                                                
190800     PERFORM S04-SEND-PUT                                                 
190900     PERFORM S05-SEND-CLOSE                                               
191000     .                                                                    
191100     EJECT                                                                
191200 S01-HAEMTA-ANROPSDATA SECTION.                                           
191300                                                                          
191400     MOVE 'GETARG'               TO SUB-KDFUNC                            
191500     MOVE WS-ADRESS              TO SUB-ADDISPABS                         
191600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
191700                                                                          
191800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
191900                                                                          
192000     IF SUB-KDRC > 0                                                      
192100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
192200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
192300       DELIMITED BY SIZE INTO FELTEXT                                     
192400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
192500     END-IF                                                               
192600     .                                                                    
192700     SKIP3                                                                
192800 S02-RETURNERA-SVAR SECTION.                                              
192900                                                                          
193000     IF SUB-KDTRANS = 'WLA101'                                            
193100       MOVE '002'                TO RESP-IDRESVER                         
193200                                                                          
193300       PERFORM S02A-MSG-CONV                                              
193400                                                                          
193500       COMPUTE WS-RESP-AREA = LENGTH OF RESP-AREA-V2                      
193600                                                                          
193700       MOVE 'RETURN'             TO SUB-KDFUNC                            
193800       MOVE WS-RESP-AREA         TO SUB-KVDLEN                            
193900                                                                          
194000       CALL WZ01SUB           USING SUB-CONTROL-AREA                      
194100                                    SUB-KVDLEN                            
194200                                    RESP-AREA-V2                          
194300                                                                          
194400     ELSE                                                                 
194500       MOVE '001'                TO RESP-IDRESVER                         
194600       MOVE RESP-IDMSG-INFO      TO V1-RESP-IDMSG-INFO                    
194700       MOVE RESP-IDMSG-ERROR     TO V1-RESP-IDMSG-ERROR                   
194800       MOVE RESP-IDELMT-ERROR    TO V1-RESP-IDELMT-ERROR                  
194900                                                                          
195000       MOVE RESP-IDDC-KEY        TO V1-RESP-IDDC-KEY                      
195100       MOVE RESP-TIFAKT-KEY      TO V1-RESP-TIFAKT-KEY                    
195200       MOVE RESP-IDLBBET-KEY     TO V1-RESP-IDLBBET-KEY                   
195300       MOVE RESP-IDDC-SEND-KEY   TO V1-RESP-IDDC-SEND-KEY                 
195400       MOVE RESP-FLTRACK         TO V1-RESP-FLTRACK                       
195500       MOVE RESP-KVRADER         TO V1-RESP-KVRADER                       
195600                                                                          
195700       PERFORM                                                            
195800       VARYING INDX FROM 1 BY 1                                           
195900         UNTIL INDX > RESP-KVRADER                                        
196000         MOVE RESP-CMD-IN    (INDX)                                       
196100                                 TO V1-RESP-CMD-IN        (INDX)          
196200         MOVE RESP-IDDC-SEND (INDX)                                       
196300                                 TO V1-RESP-IDDC-SEND     (INDX)          
196400         MOVE RESP-IDDC-LEV  (INDX)                                       
196500                                 TO V1-RESP-IDDC-LEV      (INDX)          
196600         MOVE RESP-IDFAKT    (INDX)                                       
196700                                 TO V1-RESP-IDFAKT        (INDX)          
196800         MOVE RESP-IDSHIPM   (INDX)                                       
196900                                 TO V1-RESP-IDSHIPM       (INDX)          
197000         MOVE RESP-TIFAKT    (INDX)                                       
197100                                 TO V1-RESP-TIFAKT        (INDX)          
197200         MOVE RESP-TIBERANK  (INDX)                                       
197300                                 TO V1-RESP-TIBERANK      (INDX)          
197400         MOVE RESP-DABERANK-PROP  (INDX)                                  
197500                                 TO V1-RESP-DABERANK-PROP (INDX)          
197600         MOVE RESP-DABERANK-DISCH (INDX)                                  
197700                                 TO V1-RESP-DABERANK-DISCH(INDX)          
197800         MOVE RESP-FLMANETA  (INDX)                                       
197900                                 TO V1-RESP-FLMANETA      (INDX)          
198000         MOVE RESP-IDLBBET   (INDX)                                       
198100                                 TO V1-RESP-IDLBBET       (INDX)          
198200         MOVE RESP-KDTRPSTA  (INDX)                                       
198300                                 TO V1-RESP-KDTRPSTA      (INDX)          
198400         MOVE RESP-ADINLOMR  (INDX)                                       
198500                                 TO V1-RESP-ADINLOMR      (INDX)          
198600         MOVE RESP-KVKOLLI-FAKT     (INDX)                                
198700                                 TO V1-RESP-KVKOLLI-FAKT  (INDX)          
198800         MOVE RESP-KVKOLLI-MOT      (INDX)                                
198900                                 TO V1-RESP-KVKOLLI-MOT   (INDX)          
199000         MOVE RESP-KVRADER-FAKT     (INDX)                                
199100                                 TO V1-RESP-KVRADER-FAKT  (INDX)          
199200         MOVE RESP-KVRADER-MOT      (INDX)                                
199300                                 TO V1-RESP-KVRADER-MOT   (INDX)          
199400         MOVE RESP-KVRADER-PRIO     (INDX)                                
199500                                 TO V1-RESP-KVRADER-PRIO  (INDX)          
199600         MOVE RESP-IDMSG-ERROR-LINE (INDX)                                
199700                                 TO V1-RESP-IDMSG-ERROR-LINE(INDX)        
199800         MOVE RESP-IDTRACK          (INDX)                                
199900                                 TO V1-RESP-IDTRACK       (INDX)          
200000         MOVE RESP-FLOLD-IDTRACK    (INDX)                                
200100                                 TO V1-RESP-FLOLD-IDTRACK (INDX)          
200200       END-PERFORM                                                        
200300                                                                          
200400       COMPUTE WS-RESP-AREA = LENGTH OF RESP-AREA-V1                      
200500                                                                          
200600       MOVE 'RETURN'             TO SUB-KDFUNC                            
200700       MOVE WS-RESP-AREA         TO SUB-KVDLEN                            
200800                                                                          
200900       CALL WZ01SUB           USING SUB-CONTROL-AREA                      
201000                                    SUB-KVDLEN                            
201100                                    RESP-AREA-V1                          
201200     END-IF                                                               
201300     IF SUB-KDRC > 0                                                      
201400       MOVE SUB-KDRC             TO KDRC-DISPLAY                          
201500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
201600             DELIMITED BY SIZE INTO FELTEXT                               
201700       CALL ABEND             USING RKOD-ABEND-MED-DUMP                   
201800     END-IF                                                               
201900     .                                                                    
202000                                                                          
202100 S02A-MSG-CONV SECTION.                                                   
202200                                                                          
202300     MOVE SPACES                 TO RESP-MESSAGES (1)                     
202400                                    RESP-MESSAGES (2)                     
202500     MOVE 1                      TO MSG-IX                                
202600*    REQUEST OK                                                           
202700     MOVE 200                    TO RESP-KDSTATUS-API                     
202800     IF RESP-IDMSG-INFO > SPACE                                           
202900       MOVE SPACES               TO MSG-CONV-AREA                         
203000       MOVE RESP-IDMSG-INFO      TO MSG-CONV-IDMSG-IN                     
203100       CALL WMSGCONV          USING MSG-CONV-AREA                         
203200       MOVE MSG-CONV-IDMSG-OUT   TO RESP-IDMSG   (MSG-IX)                 
203300       MOVE MSG-CONV-MESSAGE     TO RESP-MESSAGE (MSG-IX)                 
203400       ADD 1                     TO MSG-IX                                
203500     END-IF                                                               
203600     IF RESP-IDMSG-ERROR > SPACE                                          
203700*      BAD REQUEST                                                        
203800       MOVE 400                  TO RESP-KDSTATUS-API                     
203900       MOVE SPACES               TO MSG-CONV-AREA                         
204000       MOVE RESP-IDMSG-ERROR     TO MSG-CONV-IDMSG-IN                     
204100       MOVE RESP-IDELMT-ERROR    TO MSG-CONV-IDELMT                       
204200       CALL WMSGCONV          USING MSG-CONV-AREA                         
204300       MOVE MSG-CONV-IDMSG-OUT   TO RESP-IDMSG   (MSG-IX)                 
204400       MOVE MSG-CONV-MESSAGE     TO RESP-MESSAGE (MSG-IX)                 
204500     END-IF                                                               
204600     .                                                                    
204700                                                                          
204800 S03-SEND-OPEN SECTION.                                                   
204900                                                                          
205000     MOVE WS-ADRESS                  TO SEND-ADDISPABS                    
205100     MOVE 'OPEN'                     TO SEND-KDFUNC                       
205200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
205300                         SEND-OPEN-AREA                                   
205400     IF SEND-KDRC > ZERO                                                  
205500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
205600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
205700       DELIMITED BY SIZE INTO FELTEXT                                     
205800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
205900     END-IF                                                               
206000     .                                                                    
206100     EJECT                                                                
206200 S04-SEND-PUT SECTION.                                                    
206300                                                                          
206400     MOVE 'PUT'                            TO SEND-KDFUNC                 
206500     MOVE LENGTH OF SEND-AREA              TO SEND-KVDLEN                 
206600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
206700                         SEND-KVDLEN                                      
206800                         SEND-AREA                                        
206900     IF SEND-KDRC > 1                                                     
207000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
207100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
207200       DELIMITED BY SIZE INTO FELTEXT                                     
207300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
207400     END-IF                                                               
207500     .                                                                    
207600     EJECT                                                                
207700 S05-SEND-CLOSE SECTION.                                                  
207800                                                                          
207900     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
208000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
208100                                                                          
208200     IF SEND-KDRC > 0                                                     
208300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
208400       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
208500       DELIMITED BY SIZE INTO FELTEXT                                     
208600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
208700     END-IF                                                               
208800     .                                                                    
208900     EJECT                                                                
209000                                                                          
209100 S10-SEND-OPEN SECTION.                                                   
209200     MOVE 'OPEN'                        TO SEND-KDFUNC                    
209300     MOVE WS-ADDRESS-MQASYNC             TO SEND-ADDISPABS                
209400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
209500                         SEND-OPEN-AREA                                   
209600     IF SEND-KDRC > 0                                                     
209700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
209800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
209900       DELIMITED BY SIZE INTO FELTEXT                                     
210000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
210100     END-IF                                                               
210200     .                                                                    
210300     EJECT                                                                
210400                                                                          
210500 S11-SEND-PUT-PROP SECTION.                                               
210600                                                                          
210700     SET PROP-IX                 TO +1                                    
210800*    MANDATORY PROPERTY THAT SPECIFIES THE ACTUAL DESTINATION             
210900     MOVE 'ADDRESS'              TO PROP-IDPROPTYPE  (PROP-IX)            
211000     MOVE 'ADDISPABS'            TO PROP-IDPROPNAME  (PROP-IX)            
211100     MOVE WS-ADDRESS-WHSTOCKA    TO PROP-BEPROPVALUE (PROP-IX)            
211200                                                                          
211300     SET PROP-IX              UP BY +1                                    
211400*    OPTIONAL MQ MESSAGE PROPERTIES. CAN BE CASE-SENSITIVE                
211500     MOVE 'MQMPROP'              TO PROP-IDPROPTYPE  (PROP-IX)            
211600     MOVE 'CountryCode'          TO PROP-IDPROPNAME  (PROP-IX)            
211700     MOVE 'BR'                   TO PROP-BEPROPVALUE (PROP-IX)            
211800                                                                          
211900*    SET THE NUMBER OF PROPERTIES (KVANTAL) SO CORRECT LENGTH             
212000*    IS CALCULATED.                                                       
212100     SET PROP-KVANTAL            TO PROP-IX                               
212200                                                                          
212300     MOVE 'PUT'                            TO SEND-KDFUNC                 
212400     MOVE LENGTH OF PROP-WZ04PROP          TO SEND-KVDLEN                 
212500     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
212600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
212700                         SEND-KVDLEN                                      
212800                         PROP-WZ04PROP                                    
212900     IF SEND-KDRC > 1                                                     
213000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
213100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
213200       DELIMITED BY SIZE INTO FELTEXT                                     
213300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
213400     END-IF                                                               
213500     .                                                                    
213600     EJECT                                                                
213700 S12-SEND-PUT SECTION.                                                    
213800                                                                          
213900     MOVE 'PUT'                            TO SEND-KDFUNC                 
214000     MOVE LENGTH OF NOTF-AREA              TO SEND-KVDLEN                 
214100     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
214200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
214300                         SEND-KVDLEN                                      
214400                         NOTF-AREA                                        
214500     IF SEND-KDRC > 1                                                     
214600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
214700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
214800       DELIMITED BY SIZE INTO FELTEXT                                     
214900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
215000     END-IF                                                               
215100     .                                                                    
215200     EJECT                                                                
215300                                                                          
215400 S13-SEND-CLOSE SECTION.                                                  
215500     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
215600     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
215700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
215800                                                                          
215900     IF SEND-KDRC > 0                                                     
216000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
216100       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
216200       DELIMITED BY SIZE INTO FELTEXT                                     
216300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
216400     END-IF                                                               
216500     .                                                                    
216600     EJECT                                                                
216700                                                                          
216800 S90-SEND-OPEN SECTION.                                                   
216900                                                                          
217000     MOVE WS-ADRESS-DP                    TO SEND-ADDISPABS               
217100     MOVE 'OPEN'                          TO SEND-KDFUNC                  
217200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
217300                         SEND-OPEN-AREA                                   
217400     IF SEND-KDRC > ZERO                                                  
217500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
217600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
217700       DELIMITED BY SIZE INTO FELTEXT                                     
217800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
217900     END-IF                                                               
218000     .                                                                    
218100     SKIP3                                                                
218200 S91-PUT-HEADER SECTION.                                                  
218300                                                                          
218400     MOVE 'PUT'                           TO SEND-KDFUNC                  
218500     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
218600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
218700                         SEND-KVDLEN                                      
218800                         HDR-AREA                                         
218900     IF SEND-KDRC > ZERO                                                  
219000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
219100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
219200       DELIMITED BY SIZE INTO FELTEXT                                     
219300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
219400     END-IF                                                               
219500     .                                                                    
219600     EJECT                                                                
219700 S92-PUT-DOC-HEAD SECTION.                                                
219800                                                                          
219900     MOVE 'PUT'                           TO SEND-KDFUNC                  
220000     MOVE LENGTH OF DOC-HEAD-AREA         TO SEND-KVDLEN                  
220100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
220200                         SEND-KVDLEN                                      
220300                         DOC-HEAD-AREA                                    
220400     IF SEND-KDRC > ZERO                                                  
220500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
220600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
220700       DELIMITED BY SIZE INTO FELTEXT                                     
220800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
220900     END-IF                                                               
221000     .                                                                    
221100     SKIP3                                                                
221200 S93-PUT-DOC-LINE SECTION.                                                
221300                                                                          
221400     MOVE 'PUT'                           TO SEND-KDFUNC                  
221500     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
221600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
221700                         SEND-KVDLEN                                      
221800                         DOC-LINE-AREA                                    
221900     IF SEND-KDRC > ZERO                                                  
222000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
222100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
222200       DELIMITED BY SIZE INTO FELTEXT                                     
222300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
222400     END-IF                                                               
222500     .                                                                    
222600     EJECT                                                                
222700 S94-SEND-CLOSE SECTION.                                                  
222800                                                                          
222900     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
223000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
223100     .                                                                    
223200     EJECT                                                                
223300 S97-CALL-W218ETA SECTION.                                                
223400                                                                          
223500     IF 6302-IDDC-LEV NOT = SPACE                                         
223600       MOVE 6302-IDDC-LEV  TO LETA-IDDC-SEND                              
223700     ELSE                                                                 
223800       MOVE 6302-IDDC-SEND TO LETA-IDDC-SEND                              
223900     END-IF                                                               
224000     MOVE W-IDDC         TO LETA-IDDC-REC                                 
224100     MOVE ZERO           TO LETA-KDFRAKT                                  
224200     MOVE LOCAL-DATE     TO LETA-TIAAMMDD-ANROP                           
224300     MOVE 20             TO LETA-TISEKEL-ANROP                            
224400     CALL W218ETA USING LETA-W218LETA                                     
224500                         W218-WDK6-PCB                                    
224600                         W218-WDK7-PCB                                    
224700                         W218-WDL6-PCB                                    
224800                         W218-WDF1-PCB                                    
224900                         W218-WDB6-PCB                                    
225000                         W218-WDD9-PCB                                    
225100                                                                          
225200     IF LETA-SVAR-OK = 'F' OR 'N'                                         
225300        MOVE 'FEL RETURKOD FRÅN ETA' TO FELTEXT                           
225400        DISPLAY FELTEXT                                                   
225500        CALL FELLOG                                                       
225600     END-IF                                                               
225700     MOVE JA TO ETA-UPD-SW                                                
225800     .                                                                    
225900     EJECT                                                                
226000 S98-SAP-TRANS SECTION.                                                   
226100***** MAPPING OF SAP TRANSACTIONS                                         
226200                                                                          
226300     MOVE 'WL010100'                TO FIL-IDPGM IN FIL-WDR801            
226400     ACCEPT FIL-TIREGDAT IN FIL-WDR801 FROM DATE                          
226500     ACCEPT FIL-TIKLOCK  IN FIL-WDR801 FROM TIME                          
226600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
226700     MOVE WS-SAP-AAAAMMDD             TO EKH-DAVERDAT                     
226800     ADD +1                           TO W-IDSEKVNR-SAP                   
226900     MOVE W-IDSEKVNR-SAP          TO FIL-IDSEKVNR IN FIL-WDR801           
227000     MOVE '102'                       TO EKH-KDEKHHT                      
227100     IF DIST35-NONVCC-NONVCC-REFILL OR                                    
227200        DIST35-NONVCC-NONVCC-TRANSFER                                     
227300       MOVE '131'                     TO EKH-KDEKSHT                      
227400     ELSE                                                                 
227500       MOVE '121'                     TO EKH-KDEKSHT                      
227600     END-IF                                                               
227700     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
227800     MOVE 6302-IDDC-SEND              TO EKH-IDDC-SEND                    
227900     MOVE WS-IDDC-REC                 TO EKH-IDDC-REC                     
228000     MOVE WS-SAP-IDDISTR              TO EKH-IDDISTR                      
228100     MOVE WS-SAP-IDKUNDNR             TO EKH-IDKUNDNR                     
228200*******************************                                           
228300     MOVE ZERO TO NOLL-RAKNARE                                            
228400     MOVE W-IDFAKT                   TO WS-SAP-IDFAKT                     
228500     MOVE WS-SAP-IDFAKT              TO WS-SAP-X-IDFAKT                   
228600     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
228700          FOR LEADING ZERO                                                
228800     ADD +1 TO NOLL-RAKNARE                                               
228900     UNSTRING WS-SAP-X-IDFAKT         INTO EKH-IDVERGL                    
229000          WITH POINTER NOLL-RAKNARE                                       
229100     MOVE ZERO                        TO EKH-KDPSLLOC                     
229200     MOVE WS-SAP-PRARTNTO             TO EKH-PRARTNTO                     
229300     MOVE WS-SAP-PRAVCOST             TO EKH-PRARTSTD                     
229400     MOVE ZERO                        TO EKH-KDPRODSL                     
229500                                         EKH-PRARTSJK                     
229600                                         EKH-PRHEMTAG                     
229700                                         EKH-PRINK                        
229800                                         EKH-PRDIRLON                     
229900                                         EKH-PRDMTRL                      
230000                                         EKH-PROVRPAL                     
230100                                         EKH-SUBEL                        
230200     MOVE W-IDARTNR                   TO EKH-IDARTNR                      
230300     MOVE SPACE                       TO EKH-FLLSBOK                      
230400     MOVE 1.00                        TO EKH-PRKURS                       
230500********** EV ÄNDRING FÖR PRKURS                                          
230600**********                                                                
230700     MOVE W-KVAVIS                    TO EKH-KVANTAL                      
230800     MOVE 'L101'                      TO EKH-IDTRANS                      
230900     MOVE INL-KDFRAKT                 TO EKH-KDFRAKT                      
231000     MOVE ZERO                        TO EKH-BEVAT                        
231100                                         EKH-IDANALYS                     
231200                                         EKH-IDKONTO                      
231300                                         EKH-KDANMORS                     
231400                                         EKH-SUVAT                        
231500                                         EKH-PRLANDCO                     
231600                                         EKH-DAAVIDAT                     
231700                                         EKH-IDAVINR                      
231800                                         EKH-KDAVVTYP                     
231900                                         EKH-KDRT                         
232000                                         EKH-KVANTMOT                     
232100                                         EKH-KVAVIS                       
232200     MOVE SPACE                      TO  EKH-KDSORT                       
232300                                         EKH-IDKST                        
232400     MOVE SPACE                      TO  EKH-IDLEVNR                      
232500     MOVE SPACE                      TO  EKH-FLDCET                       
232600     MOVE SPACE                      TO  EKH-IDKUNDRF                     
232700     MOVE SPACE                      TO  EKH-IDFAKT-EXP                   
232800                                                                          
232900     MOVE REC-DCS-KDVALISO           TO EKH-KDVALISO                      
233000     MOVE REC-DCS-KDTRADP            TO EKH-KDTRADP                       
233100     EVALUATE TRUE                                                        
233200      WHEN REC-DCS-NDC-CN                                                 
233300        MOVE 'W570'          TO FIL-IDCPYTXT IN FIL-WDR801(1:4)           
233400      WHEN REC-DCS-INDIA                                                  
233500        MOVE 'W515'          TO FIL-IDCPYTXT IN FIL-WDR801(1:4)           
233600      WHEN REC-DCS-USA                                                    
233700        MOVE 'W561'          TO FIL-IDCPYTXT IN FIL-WDR801(1:4)           
233800      WHEN OTHER                                                          
233900        MOVE REC-DCS-KDTRADP TO FIL-IDCPYTXT IN FIL-WDR801(1:4)           
234000     END-EVALUATE                                                         
234100     MOVE 'EKHA'             TO FIL-IDCPYTXT IN FIL-WDR801(5:4)           
234200                                                                          
234300     PERFORM IMS-ISRT-WDR8                                                
234400                                                                          
234500     IF SEGMENT-FINNS-REDAN                                               
234600       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
234700          ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                            
234800          PERFORM IMS-ISRT-WDR8                                           
234900       END-PERFORM                                                        
235000     END-IF                                                               
235100                                                                          
235200     IF REC-DCS-KDTRADP = 'BR12'                                          
235300        IF NOT-FIRST-REC-TRANS                                            
235400           MOVE ZEROES       TO NOTF-IDSEKVNR                             
235500           PERFORM S10-SEND-OPEN                                          
235600           MOVE SEND-IDCOM   TO WZ04-SEND-IDCOM                           
235700           PERFORM S11-SEND-PUT-PROP                                      
235800           MOVE JA           TO FIRST-REC-TRANS-SW                        
235900        END-IF                                                            
236000                                                                          
236100        MOVE EKH-KDEKHHT     TO NOTF-KDEKHHT                              
236200        MOVE EKH-KDEKSHT     TO NOTF-KDEKSHT                              
236300        MOVE EKH-DAVERDAT    TO NOTF-DAVERDAT                             
236400        MOVE LOCAL-TIME(1:6) TO NOTF-TIREGTID                             
236500        MOVE EKH-IDVERGL     TO NOTF-IDVERGL                              
236600        MOVE EKH-IDDC-SEND   TO NOTF-IDDC                                 
236700        MOVE WS-SAP-IDFAKT   TO NOTF-IDFAKT                               
236800        MOVE EKH-IDKUNDNR    TO NOTF-IDKUNDNR                             
236900        MOVE W-IDORDER       TO NOTF-IDORDER                              
237000        MOVE W-IDKOLLI       TO NOTF-IDKOLLI                              
237100        MOVE EKH-IDARTNR     TO W-IDARTNR-Z                               
237200        MOVE FUNCTION TRIM(W-IDARTNR-Z LEADING)                           
237300                             TO NOTF-IDARTNR20                            
237400        MOVE EKH-KVANTAL     TO NOTF-KVANTAL                              
237500        ADD  +1              TO NOTF-IDSEKVNR                             
237700                                                                          
237800        PERFORM S12-SEND-PUT                                              
237900                                                                          
238000     END-IF                                                               
238100     .                                                                    
238200     EJECT                                                                
238300                                                                          
238400 S99-SAP-TRANS SECTION.                                                   
238500***** MAPPING OF SAP TRANSACTIONS                                         
238600                                                                          
238700     PERFORM IMS-GU-WDK601                                                
238800     PERFORM IMS-GN-WDK611                                                
238900                                                                          
239000     MOVE 'WL010100'                 TO FIL-IDPGM IN FIL-WDR901           
239100     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
239200     MOVE WS-SAP-AAAAMMDD            TO R9-EKH-DAVERDAT                   
239300                                        FIL-DAREGDAT                      
239400     MOVE 'W510EKHA'                 TO FIL-IDCPYTXT IN FIL-WDR901        
239500     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
239600     MOVE WS-SAP-TTMMSSTH            TO FIL-TIKLOCK  IN FIL-WDR901        
239700     MOVE MSG-SIGNON-USERID          TO FIL-IDUSER   IN FIL-WDR901        
239800     ADD +1                          TO W-IDSEKVNR-SAP                    
239900     MOVE W-IDSEKVNR-SAP             TO FIL-IDSEKVNR IN FIL-WDR901        
240000     MOVE '102'                       TO R9-EKH-KDEKHHT                   
240100     MOVE '121'                       TO R9-EKH-KDEKSHT                   
240200     MOVE 'DET  '                     TO R9-EKH-KDEKNIVA                  
240300     MOVE 6302-IDDC-SEND              TO R9-EKH-IDDC-SEND                 
240400     MOVE WS-IDDC-REC                 TO R9-EKH-IDDC-REC                  
240500     MOVE WS-SAP-IDDISTR              TO R9-EKH-IDDISTR                   
240600     MOVE WS-SAP-IDKUNDNR             TO R9-EKH-IDKUNDNR                  
240700*******************************                                           
240800     MOVE ZERO TO NOLL-RAKNARE                                            
240900     MOVE W-IDFAKT                   TO WS-SAP-IDFAKT                     
241000     MOVE WS-SAP-IDFAKT              TO WS-SAP-X-IDFAKT                   
241100     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
241200          FOR LEADING ZERO                                                
241300     ADD +1 TO NOLL-RAKNARE                                               
241400     UNSTRING WS-SAP-X-IDFAKT         INTO R9-EKH-IDVERGL                 
241500          WITH POINTER NOLL-RAKNARE                                       
241600     MOVE ZERO                        TO R9-EKH-KDPSLLOC                  
241700     MOVE WS-SAP-PRARTNTO             TO R9-EKH-PRARTNTO                  
241800     MOVE CLAG-PRARTSTD               TO R9-EKH-PRARTSTD                  
241900     MOVE ART-KDPRODSL                TO R9-EKH-KDPRODSL                  
242000     MOVE ZERO                        TO R9-EKH-PRARTSJK                  
242100                                         R9-EKH-PRHEMTAG                  
242200                                         R9-EKH-PRINK                     
242300                                         R9-EKH-PRDIRLON                  
242400                                         R9-EKH-PRDMTRL                   
242500                                         R9-EKH-PROVRPAL                  
242600                                         R9-EKH-SUBEL                     
242700     MOVE W-IDARTNR                   TO R9-EKH-IDARTNR                   
242800     MOVE SPACE                       TO R9-EKH-FLLSBOK                   
242900     MOVE 1.00                        TO R9-EKH-PRKURS                    
243000********** EV ÄNDRING FÖR PRKURS                                          
243100**********                                                                
243200     MOVE W-KVAVIS                    TO R9-EKH-KVANTAL                   
243300     MOVE 'L101'                      TO R9-EKH-IDTRANS                   
243400     MOVE INL-KDFRAKT                 TO R9-EKH-KDFRAKT                   
243500     MOVE ZERO                        TO R9-EKH-BEVAT                     
243600                                         R9-EKH-IDANALYS                  
243700                                         R9-EKH-IDKONTO                   
243800                                         R9-EKH-KDANMORS                  
243900                                         R9-EKH-SUVAT                     
244000                                         R9-EKH-PRLANDCO                  
244100                                         R9-EKH-DAAVIDAT                  
244200                                         R9-EKH-IDAVINR                   
244300                                         R9-EKH-KDAVVTYP                  
244400                                         R9-EKH-KDRT                      
244500                                         R9-EKH-KVANTMOT                  
244600                                         R9-EKH-KVAVIS                    
244700     MOVE SPACE                      TO  R9-EKH-KDSORT                    
244800                                         R9-EKH-IDKST                     
244900     MOVE SPACE                      TO  R9-EKH-IDLEVNR                   
245000     MOVE SPACE                      TO  R9-EKH-FLDCET                    
245100     MOVE SPACE                      TO  R9-EKH-IDKUNDRF                  
245200     MOVE SPACE                      TO  R9-EKH-IDFAKT-EXP                
245300                                                                          
245400     MOVE 'SEK'                      TO  R9-EKH-KDVALISO                  
245500     MOVE 'SEPV'                     TO  R9-EKH-KDTRADP                   
245600     MOVE SPACE                      TO  R9-EKH-IDFAKT-EXP                
245700                                                                          
245800     PERFORM IMS-ISRT-WDR9                                                
245900                                                                          
246000     IF SEGMENT-FINNS-REDAN                                               
246100       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
246200          ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                            
246300          PERFORM IMS-ISRT-WDR9                                           
246400       END-PERFORM                                                        
246500     END-IF                                                               
246600     .                                                                    
246700     EJECT                                                                
246800* IMS-SEKTIONER                                                           
246900 IMS-GU-WL630101 SECTION.                                                 
247000     STRING 'WL630101(WDGXKEY = ' W-WDGXKEY-6301 ')'                      
247100          DELIMITED BY SIZE INTO SSA1                                     
247200     MOVE '  GE' TO GODK-STATUSKODER                                      
247300     CALL CBLTDLI USING GU GX63-PCB DLI-IO-AREA-WL6301 SSA1               
247400                                                                          
247500     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
247600     PERFORM IMS-STATUSKONTROLL                                           
247700     .                                                                    
247800     SKIP3                                                                
247900 IMS-GNP-WL630111-A SECTION.                                              
248000     STRING 'WL630111(KEY6302 =>' W-WDGXKEY-6302 ')'                      
248100          DELIMITED BY SIZE INTO SSA1                                     
248200     MOVE '  GE' TO GODK-STATUSKODER                                      
248300     CALL CBLTDLI USING GNP GX63-PCB DLI-IO-AREA-WL6301 SSA1              
248400                                                                          
248500     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
248600     PERFORM IMS-STATUSKONTROLL                                           
248700     .                                                                    
248800     EJECT                                                                
248900 IMS-GNP-WL630111-B SECTION.                                              
249000     STRING 'WL630111(KEY6302 =>' W-WDGXKEY-6302                          
249100                    '&IDLBBET = ' W-IDLBBET    ')'                        
249200          DELIMITED BY SIZE INTO SSA1                                     
249300     MOVE '  GE' TO GODK-STATUSKODER                                      
249400     CALL CBLTDLI USING GNP GX63-PCB DLI-IO-AREA-WL6301 SSA1              
249500                                                                          
249600     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
249700     PERFORM IMS-STATUSKONTROLL                                           
249800     .                                                                    
249900     SKIP3                                                                
250000 IMS-GNP-WL630111 SECTION.                                                
250100                                                                          
250200     STRING 'WL630111*F(IDFAKT   =' W-IDFAKT-X ')'                        
250300          DELIMITED BY SIZE INTO SSA1                                     
250400     MOVE '  GE' TO GODK-STATUSKODER                                      
250500     CALL CBLTDLI USING GHNP GX63-PCB DLI-IO-AREA-WL6301 SSA1             
250600     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
250700     PERFORM IMS-STATUSKONTROLL                                           
250800     .                                                                    
250900     SKIP3                                                                
251000 IMS-GHNP-WL630111 SECTION.                                               
251100     STRING 'WL630111*F(IDFAKT   =' W-IDFAKT-X ')'                        
251200          DELIMITED BY SIZE INTO SSA1                                     
251300     MOVE '  GE' TO GODK-STATUSKODER                                      
251400     CALL CBLTDLI USING GHNP GX63-PCB DLI-IO-AREA-WL6301 SSA1             
251500     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
251600     PERFORM IMS-STATUSKONTROLL                                           
251700     .                                                                    
251800     SKIP3                                                                
251900 IMS-REPL-WL630111 SECTION.                                               
252000     MOVE '  ' TO GODK-STATUSKODER                                        
252100     CALL CBLTDLI USING REPL GX63-PCB DLI-IO-AREA-WL6301                  
252200     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
252300     PERFORM IMS-STATUSKONTROLL                                           
252400     .                                                                    
252500     EJECT                                                                
252600 IMS-DLET-WL630111 SECTION.                                               
252700     MOVE '  ' TO GODK-STATUSKODER                                        
252800     CALL CBLTDLI USING DLET GX63-PCB DLI-IO-AREA-WL6301                  
252900     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
253000     PERFORM IMS-STATUSKONTROLL                                           
253100     .                                                                    
253200     EJECT                                                                
253300 IMS-ISRT-WL630111 SECTION.                                               
253400     MOVE '  II' TO GODK-STATUSKODER                                      
253500     MOVE 'WL630111 ' TO SSA1                                             
253600     CALL CBLTDLI USING ISRT GX63-PCB DLI-IO-AREA-WL6301 SSA1             
253700     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
253800     PERFORM IMS-STATUSKONTROLL                                           
253900     .                                                                    
254000     EJECT                                                                
254100 IMS-GN-WDL6A1  SECTION.                                                  
254200     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
254300                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
254400                    '&IDPTYP  = ' W-IDPTYP ')'                            
254500          DELIMITED BY SIZE INTO SSA1                                     
254600     MOVE '  GE' TO GODK-STATUSKODER                                      
254700     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA SSA1                      
254800                                                                          
254900     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
255000     PERFORM IMS-STATUSKONTROLL                                           
255100     .                                                                    
255200     SKIP3                                                                
255300 IMS-GU-WDL6A1  SECTION.                                                  
255400     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
255500                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
255600                    '&IDPTYP  = ' W-IDPTYP ')'                            
255700          DELIMITED BY SIZE INTO SSA1                                     
255800     MOVE '  GE' TO GODK-STATUSKODER                                      
255900     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA SSA1                      
256000                                                                          
256100     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
256200     PERFORM IMS-STATUSKONTROLL                                           
256300     .                                                                    
256400     SKIP3                                                                
256500 IMS-GHU-ARTS11   SECTION.                                                
256600     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
256700          DELIMITED BY SIZE INTO SSA1                                     
256800     STRING 'WLARTS11(IDDC     =' W-IDDC  ')'                             
256900          DELIMITED BY SIZE INTO SSA2                                     
257000     MOVE '  ' TO GODK-STATUSKODER                                        
257100     CALL CBLTDLI USING GHU  ARTS-PCB DLI-IO-AREA SSA1 SSA2               
257200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
257300     PERFORM IMS-STATUSKONTROLL                                           
257400     .                                                                    
257500     EJECT                                                                
257600 IMS-GU-ARTS11   SECTION.                                                 
257700     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
257800          DELIMITED BY SIZE INTO SSA1                                     
257900     STRING 'WLARTS11(IDDC     =' W-IDDC  ')'                             
258000          DELIMITED BY SIZE INTO SSA2                                     
258100     MOVE '  ' TO GODK-STATUSKODER                                        
258200     CALL CBLTDLI USING GU  ARTS-PCB DLI-IO-AREA SSA1 SSA2                
258300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
258400     PERFORM IMS-STATUSKONTROLL                                           
258500     .                                                                    
258600     EJECT                                                                
258700 IMS-REPL-ARTS11 SECTION.                                                 
258800     MOVE '  ' TO GODK-STATUSKODER                                        
258900     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA                         
259000                                                                          
259100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
259200     PERFORM IMS-STATUSKONTROLL                                           
259300     .                                                                    
259400     SKIP3                                                                
259500 IMS-GU-INLC11   SECTION.                                                 
259600     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
259700          DELIMITED BY SIZE INTO SSA1                                     
259800     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
259900          DELIMITED BY SIZE INTO SSA2                                     
260000     MOVE SPACE  TO GODK-STATUSKODER                                      
260100     CALL CBLTDLI USING GU  INLC-PCB DLI-IO-AREA SSA1 SSA2                
260200     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
260300     PERFORM IMS-STATUSKONTROLL                                           
260400     .                                                                    
260500     SKIP3                                                                
260600 IMS-GHU-INLC11   SECTION.                                                
260700     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
260800          DELIMITED BY SIZE INTO SSA1                                     
260900     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
261000          DELIMITED BY SIZE INTO SSA2                                     
261100     MOVE SPACE  TO GODK-STATUSKODER                                      
261200     CALL CBLTDLI USING GHU  INLC-PCB DLI-IO-AREA SSA1 SSA2               
261300     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
261400     PERFORM IMS-STATUSKONTROLL                                           
261500     .                                                                    
261600     SKIP3                                                                
261700 IMS-REPL-INLC11 SECTION.                                                 
261800     MOVE '  ' TO GODK-STATUSKODER                                        
261900     CALL CBLTDLI USING REPL INLC-PCB DLI-IO-AREA                         
262000                                                                          
262100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
262200     PERFORM IMS-STATUSKONTROLL                                           
262300     .                                                                    
262400     EJECT                                                                
262500 IMS-ISRT-WDL901 SECTION.                                                 
262600     MOVE 'WLLOGA01 ' TO SSA1                                             
262700     MOVE '  II' TO GODK-STATUSKODER                                      
262800     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
262900     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
263000     PERFORM IMS-STATUSKONTROLL                                           
263100     .                                                                    
263200     EJECT                                                                
263300 IMS-GU-WDB601-SEND SECTION.                                              
263400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
263500          DELIMITED BY SIZE INTO SSA1                                     
263600     MOVE '  GE' TO GODK-STATUSKODER                                      
263700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
263800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
263900     PERFORM IMS-STATUSKONTROLL                                           
264000     .                                                                    
264100     SKIP3                                                                
264200                                                                          
264300 IMS-GU-WDB601-REC SECTION.                                               
264400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
264500          DELIMITED BY SIZE INTO SSA1                                     
264600     MOVE '  GE' TO GODK-STATUSKODER                                      
264700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-REC  SSA1            
264800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
264900     PERFORM IMS-STATUSKONTROLL                                           
265000     .                                                                    
265100     SKIP3                                                                
265200                                                                          
265300 IMS-GU-WDB601-DDC-CHECK SECTION.                                         
265400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-DDC-X ')'                     
265500          DELIMITED BY SIZE INTO SSA1                                     
265600     MOVE '  GE' TO GODK-STATUSKODER                                      
265700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-DDC SSA1             
265800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
265900     PERFORM IMS-STATUSKONTROLL                                           
266000     .                                                                    
266100     SKIP3                                                                
266200                                                                          
266300 IMS-GU-WDL6A  SECTION.                                                   
266400     STRING 'WLINLC11(WDL6ASEQ=>' W-WDL6ASEQ-MIN                          
266500                    '&WDL6ASEQ=<' W-WDL6ASEQ-MAX ')'                      
266600          DELIMITED BY SIZE INTO SSA1                                     
266700     MOVE '  GE' TO GODK-STATUSKODER                                      
266800     CALL CBLTDLI USING GU WDL6A-PCB DLI-IO-AREA-WDL6A SSA1               
266900     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
267000     PERFORM IMS-STATUSKONTROLL                                           
267100     .                                                                    
267200     SKIP3                                                                
267300 IMS-GN-WDL6A  SECTION.                                                   
267400     STRING 'WLINLC11(WDL6ASEQ=>' W-WDL6ASEQ-MIN                          
267500                    '&WDL6ASEQ=<' W-WDL6ASEQ-MAX ')'                      
267600          DELIMITED BY SIZE INTO SSA1                                     
267700     MOVE '  GE' TO GODK-STATUSKODER                                      
267800     CALL CBLTDLI USING GN WDL6A-PCB DLI-IO-AREA-WDL6A SSA1               
267900     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
268000     PERFORM IMS-STATUSKONTROLL                                           
268100     .                                                                    
268200     EJECT                                                                
268300 IMS-GU-WDL623    SECTION.                                                
268400     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
268500          DELIMITED BY SIZE INTO SSA1                                     
268600     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
268700          DELIMITED BY SIZE INTO SSA2                                     
268800     MOVE 'WDL623 ' TO SSA3                                               
268900     MOVE '  GE' TO GODK-STATUSKODER                                      
269000     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA                           
269100                             SSA1 SSA2 SSA3                               
269200     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
269300     PERFORM IMS-STATUSKONTROLL                                           
269400     .                                                                    
269500     EJECT                                                                
269600 IMS-GHU-WDL623    SECTION.                                               
269700     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
269800          DELIMITED BY SIZE INTO SSA1                                     
269900     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
270000          DELIMITED BY SIZE INTO SSA2                                     
270100     MOVE 'WDL623 ' TO SSA3                                               
270200     MOVE '  GE' TO GODK-STATUSKODER                                      
270300     CALL CBLTDLI USING GHU WDL6-PCB DLI-IO-AREA                          
270400                             SSA1 SSA2 SSA3                               
270500     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
270600     PERFORM IMS-STATUSKONTROLL                                           
270700     .                                                                    
270800     EJECT                                                                
270900 IMS-ISRT-WDL623    SECTION.                                              
271000     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
271100          DELIMITED BY SIZE INTO SSA1                                     
271200     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
271300          DELIMITED BY SIZE INTO SSA2                                     
271400     MOVE 'WDL623 ' TO SSA3                                               
271500     MOVE '  II' TO GODK-STATUSKODER                                      
271600     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-AREA                         
271700                             SSA1 SSA2 SSA3                               
271800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
271900     PERFORM IMS-STATUSKONTROLL                                           
272000     .                                                                    
272100     EJECT                                                                
272200 IMS-REPL-WDL623    SECTION.                                              
272300     MOVE 'WDL623   ' TO SSA1                                             
272400     MOVE '  ' TO GODK-STATUSKODER                                        
272500     CALL CBLTDLI USING REPL WDL6-PCB DLI-IO-AREA SSA1                    
272600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
272700     PERFORM IMS-STATUSKONTROLL                                           
272800     .                                                                    
272900     EJECT                                                                
273000 IMS-GU-WDR5  SECTION.                                                    
273100     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
273200          DELIMITED BY SIZE INTO SSA1                                     
273300     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
273400          DELIMITED BY SIZE INTO SSA2                                     
273500     MOVE SPACE TO GODK-STATUSKODER                                       
273600     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-AREA-WDR5 SSA1 SSA2            
273700     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
273800     PERFORM IMS-STATUSKONTROLL                                           
273900     EJECT                                                                
274000     .                                                                    
274100 IMS-GU-WLINLD01 SECTION.                                                 
274200     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
274300                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
274400          DELIMITED BY SIZE INTO SSA1                                     
274500     MOVE '  GE' TO GODK-STATUSKODER                                      
274600     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA SSA1                      
274700                                                                          
274800     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
274900     PERFORM IMS-STATUSKONTROLL                                           
275000     .                                                                    
275100     SKIP3                                                                
275200 IMS-GN-WLINLD01 SECTION.                                                 
275300     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
275400                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
275500          DELIMITED BY SIZE INTO SSA1                                     
275600     MOVE '  GE' TO GODK-STATUSKODER                                      
275700     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA SSA1                      
275800     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
275900     PERFORM IMS-STATUSKONTROLL                                           
276000     .                                                                    
276100     EJECT                                                                
276200 IMS-ISRT-WDR8   SECTION.                                                 
276300                                                                          
276400     MOVE 'WDR801   ' TO SSA1                                             
276500     MOVE '  II' TO GODK-STATUSKODER                                      
276600     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
276700     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
276800     PERFORM IMS-STATUSKONTROLL                                           
276900     .                                                                    
277000     EJECT                                                                
277100                                                                          
277200 IMS-ISRT-WDR9   SECTION.                                                 
277300     MOVE 'WDR901   ' TO SSA1                                             
277400     MOVE '  II' TO GODK-STATUSKODER                                      
277500     CALL CBLTDLI USING ISRT WDR9-PCB DLI-IO-WDR901 SSA1                  
277600     MOVE WDR9-STATUS-CODE TO STATUS-WS                                   
277700     PERFORM IMS-STATUSKONTROLL                                           
277800     .                                                                    
277900     EJECT                                                                
278000                                                                          
278100 IMS-GU-WDK601   SECTION.                                                 
278200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
278300          DELIMITED BY SIZE INTO SSA1                                     
278400     MOVE '  ' TO GODK-STATUSKODER                                        
278500     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK601 SSA1                   
278600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
278700     PERFORM IMS-STATUSKONTROLL                                           
278800     .                                                                    
278900     EJECT                                                                
279000                                                                          
279100 IMS-GN-WDK611   SECTION.                                                 
279200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
279300          DELIMITED BY SIZE INTO SSA1                                     
279400     MOVE '  GE' TO GODK-STATUSKODER                                      
279500     CALL CBLTDLI USING GNP  WDK6-PCB DLI-IO-WDK611 SSA1                  
279600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
279700     PERFORM IMS-STATUSKONTROLL                                           
279800     .                                                                    
279900     EJECT                                                                
280000 IMS-GHU-WDK611   SECTION.                                                
280100                                                                          
280200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
280300          DELIMITED BY SIZE INTO SSA1                                     
280400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
280500          DELIMITED BY SIZE INTO SSA2                                     
280600     MOVE '  ' TO GODK-STATUSKODER                                        
280700     CALL CBLTDLI USING GHU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2             
280800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
280900     PERFORM IMS-STATUSKONTROLL                                           
281000     .                                                                    
281100     EJECT                                                                
281200 IMS-REPL-WDK611 SECTION.                                                 
281300                                                                          
281400     MOVE '  ' TO GODK-STATUSKODER                                        
281500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
281600                                                                          
281700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
281800     PERFORM IMS-STATUSKONTROLL                                           
281900     .                                                                    
282000     EJECT                                                                
282100                                                                          
282200 IMS-GU-W6G130 SECTION.                                                   
282300                                                                          
282400     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
282500          DELIMITED BY SIZE INTO SSA1                                     
282600     STRING 'W6G130  (W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
282700          DELIMITED BY SIZE INTO SSA2                                     
282800     MOVE '  GE' TO GODK-STATUSKODER                                      
282900     CALL CBLTDLI USING GU W6G1-PCB DLI-IO-AREA-W6G130 SSA1 SSA2          
283000     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
283100     PERFORM IMS-STATUSKONTROLL                                           
283200     .                                                                    
283300     SKIP3                                                                
283400                                                                          
283500 IMS-STATUSKONTROLL SECTION.                                              
283600     SET STATUS-IX TO 1                                                   
283700     SEARCH GODK-STATUS                                                   
283800       AT END                                                             
283900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
284000         DELIMITED BY SIZE INTO FELTEXT                                   
284100         CALL FELLOG                                                      
284200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
284300         CONTINUE                                                         
284400     END-SEARCH                                                           
284500     .                                                                    
284600     EJECT                                                                
284700*    -COPY WY2000P1                                                       
