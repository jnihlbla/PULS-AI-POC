000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2012600.                                                
000300 AUTHOR.         ANN JORDEBO.                                             
000400 DATE-WRITTEN.   90/07/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        VISA OCH UPPDATERA (BEKRÄFTA) TPO:ER (2 OCH 6) SOM               
000900*        SKAPAT LARM TILL ANSKAFFARE. RADEN KAN BEKRÄFTAS SOM             
001000*        DEN ÄR ELLER SPLITTAS UPP I FLERA RADER. ENDAST RADER            
001100*        MED DAGENS DATUM ORDERBEKRÄFTAS.                                 
001200*        PRISTILLÄMPNING GÖRS OM DETTA INTE ÄR GJORT.                     
001300*        EFTER UPPDATERING AV RADEN TAS                                   
001400*        LARMET BORT OCH DÄREFTER KAN INTE NÅGON UPPDATERING SKE          
001500*        HÄR. ALL LÄSNING AV RAD SKER VIA LARMBASEN, M A O KAN            
001600*        INTE BEKRÄFTAD RAD VISAS IGEN.                                   
001700*                                                                         
001800*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001900*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
002000*                              WLARTC (WDK6)                              
002100*                              WLORQI (WDQ2)    WLXXBX (WDR2)             
002200*        PROGRAMMET UPPDATERAR WLARTM (WDK9)    WLORDP (WDA5)             
002300*                              WLORQM (WDQ1)    WLXXBU (WDR5)             
002400*                              WLXXBJ (WDG3)    WLZZAC (WDG6)             
002500*                              WDR1   WDC7                                
002600*                                                                         
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W2T126                                              
003000*        MID:         W2I12601                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        MOD:         W2O12601                                            
003400*                                                                         
003500*   ÄNDRINGAR:                                                            
003600*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
003700*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
003800*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
003900*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
004000*                                                                         
004100*                                                                         
004200*    E'TRACKER: 5444132 DATED 2007-08-21                                  
004300*    E'TRACKER: 7450328 DATED 2008-HÖST  VOHF                             
004400*    E'TRACKER: 10254592  2015 DECOMISSION VOHF                           
004500*                                                                         
004600                                                                          
004700     SKIP3                                                                
004800 ENVIRONMENT DIVISION.                                                    
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -COPY WY2000W1                                                       
005400     SKIP3                                                                
005500*    -COPY WY2000W2                                                       
005600     SKIP3                                                                
005700 77  IDPGM                       PIC X(08)   VALUE 'W2012600'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006200 77  MAX-INDX                    PIC S9(4)  VALUE +6    COMP SYNC.        
006300 77  MOD-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
006400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006500                                                                          
006600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006700 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
006800                                                                          
006900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007000     88  INDATA-OK                           VALUE 'J'.                   
007100     88  INDATA-FEL                          VALUE 'N'.                   
007200                                                                          
007300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007400     88  NYCKLAR-OK                          VALUE 'J'.                   
007500     88  NYCKLAR-FEL                         VALUE 'N'.                   
007600                                                                          
007700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007800     88  ALLT-OK                             VALUE 'J'.                   
007900                                                                          
008000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008100     88  EGEN-MID                            VALUE '2126'.                
008200     88  GODK-MID                            VALUE '2121' '2122'          
008300                                                   '2123' '2124'          
008400                                                   '2125' '2126'          
008500                                                   '2127' '2128'          
008600                                                   '2129'.                
008700                                                                          
008800 77  2171-TRANS-SW               PIC X       VALUE SPACE.                 
008900     88  2171-TRANS                          VALUE 'J'.                   
009000                                                                          
009100 77  TRAFF-SW                    PIC X       VALUE SPACE.                 
009200     88  TRAFF                               VALUE 'J'.                   
009300                                                                          
009400 77  KVANTANPASSA-SW             PIC X       VALUE SPACE.                 
009500     88  KVANTANPASSA                        VALUE 'J'.                   
009600                                                                          
009700 77  SPLITTAD-RAD-SW             PIC X       VALUE 'N'.                   
009800     88  SPLITTAD-RAD                        VALUE 'J'.                   
009900                                                                          
010000 77  2204-SW                     PIC X       VALUE 'N'.                   
010100     88  2204-ISRT                           VALUE 'J'.                   
010200                                                                          
010300 77  ANNAN-NYCKEL-SW             PIC X       VALUE 'N'.                   
010400     88  ANNAN-NYCKEL                        VALUE 'J'.                   
010500                                                                          
010600 77  PRISTILL-SW                 PIC X       VALUE 'N'.                   
010700     88  PRISTILLAEMPA                       VALUE 'J'.                   
010800                                                                          
010900 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
011000                                                                          
011100 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
011200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
011300                                                                          
011400 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
011500                                                                          
011600 01  FELTEXT.                                                             
011700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011900     EJECT                                                                
012000                                                                          
012100 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
012200*01  FILLER  -COPY WWDIST79   -RED TEST-IDDISTR.                          
012300     EJECT                                                                
012400*    --- ARBETSFÄLT                                                       
012500 01  ARBETSFAELT.                                                         
012600     03  WS-IDLEVNR              PIC X(5)   VALUE SPACE.                  
012700     03  WS-TITPO OCCURS 6       PIC 9(6)   VALUE ZERO.                   
012800     03  SPAR-RAD-TITPO          PIC 9(6)   VALUE ZERO.                   
012900     03  WS-TIBEHOV.                                                      
013000       05 WS-TIAAVV              PIC 9(4)   VALUE ZERO.                   
013100       05 WS-TID                 PIC 9(1)   VALUE ZERO.                   
013200     03  WS-DABEHOV.                                                      
013300       05 WS-DASEKEL             PIC 9(2)   VALUE ZERO.                   
013400       05 WS-DAAAVV              PIC 9(4)   VALUE ZERO.                   
013500     03  WS-KDSORT               PIC X(2)   VALUE SPACE.                  
013800     03  WS-KVQPACK-1            PIC S9(5)  VALUE ZERO COMP-3.            
013900     03  WS-ANTAL-FORP           PIC S9(7)  VALUE ZERO.                   
014000     03  WS-KVANT                PIC S9(7)  VALUE ZERO.                   
014100     03  WS-KVANT-SUM            PIC S9(9)  VALUE ZERO.                   
014200     03  WS-IDORDER              PIC S9(7)  VALUE ZERO COMP-3.            
014300     03  WS-IDKUNDRF             PIC X(10)  VALUE SPACE.                  
014400     03  WS-IDKUNDRF-FIX.                                                 
014500       05 FILLER                 PIC X(2).                                
014600       05 WS-IDORDER5            PIC X(5).                                
014700       05 FILLER                 PIC X(3).                                
014800     03  WS-IDLEVNR-8            PIC X(8)  VALUE SPACE.                   
014900     03  WS-TIDISPIN             PIC S9(7)  VALUE ZERO COMP-3.            
015000     03  WS-FLNC                 PIC X.                                   
015100     03  WS-FLVR                 PIC X      VALUE 'N'.                    
015200*    --- DATUM- OCH TIDFÄLT                                               
015300 01  DATUM-OCH-TID-FAELT.                                                 
015400     03  DAGENS-TIAAVVD          PIC  9(5)  VALUE ZERO.                   
015500     03  DAGENS-DATUM            PIC  9(6)  VALUE ZERO.                   
015600     03  AKTUELL-TID             PIC  9(8)  VALUE ZERO.                   
015700     03  DATUM-MED-ARHUNDR       PIC  9(8)  VALUE ZERO.                   
015800     03  SEKEL-TAL               PIC S9(9)  VALUE ZERO COMP-3.            
015900*      --- VALID IDDC CODES                                               
016000*                                                                         
016100*01    -COPY WWDCKONS                                                     
016200       EJECT                                                              
016300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016400 01  GENERELLA-SUBPROGRAM.                                                
016500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016900     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
017000     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
017100     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
017200*        HÄMTA PRISFRÅGENR                                                
017300     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
017400*        DEALER PRISFRÅGABEHANDLING                                       
017500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
017700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017800     EJECT                                                                
017900*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
018000*   -COPY WDATAREA                                                        
018100     EJECT                                                                
018200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
018300*   -COPY WMSGINIT                                                        
018400     EJECT                                                                
018500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018600*   -COPY WMEDAREA                                                        
018700     EJECT                                                                
018800*    --- PARAMETRAR TILL SUBPROGRAM W411ARTM, SKAPA WDK901-SEGM           
018900*   -COPY W411ARTM                                                        
019000     EJECT                                                                
019100*    --- PARAMETRAR TILL SUBPROGRAM W335PRIS, PRISTILLÄMPNING             
019200 01  FILLER                      PIC X(24)  VALUE 'PRISAREA'.             
019300*01  PRIS-AREA -COPY W335PRIS                                             
019400     EJECT                                                                
019500 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
019600*   -COPY W335PRNO                                                        
019700     EJECT                                                                
019800 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
019900*   -COPY W335PRQU                                                        
020000     EJECT                                                                
020100 01  MESSAGE-CODES.                                                       
020200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
020300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
020400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
020500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
020600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
020700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
020800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
020900     03  ERR-ARTICLE-MISSING     PIC X(3)    VALUE '017'.                 
021000     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
021100     EJECT                                                                
021200 01  MEDDELANDEN.                                                         
021300     03  MED-1                   PIC X(11)   VALUE                        
021400         'SISTA SIDAN'.                                                   
021500     03  MED-2                   PIC X(19)   VALUE                        
021600         'KVANT FÅR EJ BRYTAS'.                                           
021700     03  MED-3                   PIC X(23)   VALUE                        
021800         'LARM SAKNAS FÖR ARTIKEL'.                                       
021900     03  MED-4                   PIC X(23)   VALUE                        
022000         'FYLL I RADERNA I FÖLJD!'.                                       
022100*    --- LOGGPOSTER, TRANSAR                                              
022200*01     -COPY W440300  -PRE RY4-.                                         
022300     EJECT                                                                
022400*01  WDGZRY9  -COPY WDGZRY9                                               
022500     EJECT                                                                
022600*01  WDGZRY9S -COPY WDGZRY9S                                              
022700     EJECT                                                                
022800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
022900*                                                                         
023000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023100     SKIP3                                                                
023200*01  MID -COPY W2I12601                                                   
023300     EJECT                                                                
023400*01  MID -COPY W2I17101   -PRE 2171-.                                     
023500     EJECT                                                                
023600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023700     SKIP3                                                                
023800*01  -COPY WMSGAREA                                                       
023900     EJECT                                                                
024000     03  MOD REDEFINES MSG-AREA.                                          
024100*      05  -COPY W2O12601                                                 
024200     EJECT                                                                
024300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024400     SKIP3                                                                
024500*01  -COPY WMFSAREA                                                       
024600     EJECT                                                                
024700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024800*                                                                         
024900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025000     SKIP3                                                                
025100 01  NYCKLAR-TILL-DLI.                                                    
025200     03  W-IDARTNR-X.                                                     
025300         05  W-IDARTNR-A         PIC S9(9)   VALUE ZERO COMP-3.           
025400     03  W-IDDC-X.                                                        
025500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
025600     03  W-KDSEGKEY-X.                                                    
025700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
025800     03  W-DABEHOV-X.                                                     
025900         05  W-DABEHOV-A         PIC  9(6)   VALUE ZERO.                  
026000     03  W-IDDISTR-X.                                                     
026100         05  W-IDDISTR-K         PIC S9(5)   VALUE ZERO COMP-3.           
026200     03  W-IDKUNDNR-X.                                                    
026300         05  W-IDKUNDNR-K        PIC S9(7)   VALUE ZERO COMP-3.           
026400     03  W-IDGMT-X.                                                       
026500         05  W-IDDISTR-GMTA      PIC S9(5)   VALUE ZERO COMP-3.           
026600         05  W-IDKUNDNR-GMTA     PIC S9(7)   VALUE ZERO COMP-3.           
026700     03  W-IDGMTREF-X.                                                    
026800         05  W-IDDISTR-XX.                                                
026900             07  W-IDDISTR-R     PIC S9(5)   VALUE ZERO COMP-3.           
027000         05  W-IDKUNDNR-XX.                                               
027100             07  W-IDKUNDNR-R    PIC S9(7)   VALUE ZERO COMP-3.           
027200         05  W-IDKUNDRF-XX.                                               
027300             07  W-IDKUNDRF-R    PIC X(10)   VALUE SPACE.                 
027400     03  W-WDA501KY-X.                                                    
027500         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
027600         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
027700         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
027800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
027900         05  W-IDLOPNR           PIC S9(3)   VALUE ZERO COMP-3.           
028000     03  W-IDORDER-X.                                                     
028100         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
028200     03  W-WDGXKEY-2223-X.                                                
028300         05  FILLER              PIC X(4)    VALUE '2223'.                
028400         05  W-IDANSK            PIC S9(3)   VALUE ZERO COMP-3.           
028500         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
028600     03  W-WDGXKEY-2224-X.                                                
028700         05  W-TISENBEK.                                                  
028800             07  W-TISENBEK-DAG  PIC S9(7)   VALUE ZERO COMP-3.           
028900             07  W-TISENBEK-KL   PIC S9(7)   VALUE ZERO COMP-3.           
029000         05  W-KDLARM            PIC S9(3)   VALUE ZERO COMP-3.           
029100     03  W-KDLARM-MIN-X.                                                  
029200         05  W-KDLARM-MIN        PIC S9(3)   VALUE +100 COMP-3.           
029300     03  W-KDLARM-MAX-X.                                                  
029400         05  W-KDLARM-MAX        PIC S9(3)   VALUE +110 COMP-3.           
029500     03  W-WDGXKEY-2231-X.                                                
029600         05  FILLER              PIC X(4)    VALUE '2231'.                
029700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
029800     03  W-WDGXKEY-2232-X.                                                
029900         05  W-IDANSK-L          PIC S9(3)   VALUE ZERO COMP-3.           
030000         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
030100     03  W-WDGXKEY-2203-X.                                                
030200         05  FILLER              PIC X(4)    VALUE '2203'.                
030300         05  W-IDDC-2203         PIC X(2)    VALUE '  '.                  
030400         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
030500*    --- STATUS-KOD FRÅN IMS                                              
030600 01  STATUS-WS                   PIC XX.                                  
030700     88  SEGMENT-FINNS                       VALUE '  '.                  
030800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031000     SKIP2                                                                
031100 01  GODK-STATUSKODER.                                                    
031200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031300     SKIP3                                                                
031400 01  SSA1                        PIC X(64).                               
031500 01  SSA2                        PIC X(64).                               
031600 01  SSA3                        PIC X(64).                               
031700     EJECT                                                                
031800*    --- IMS FUNKTIONSKODER                                               
031900*01  -COPY W0003                                                          
032000     EJECT                                                                
032100*    ---  DLI INPUT-OUTPUT AREA                                           
032200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
032300                                                                          
032400 01  DLI-IO-AREA.                                                         
032500*  03  -COPY WDA501     -PRE ORDP-                                        
032600     EJECT                                                                
032700 01  DLI-IO-AREA-01.                                                      
032800*  03  -COPY WDK601                                                       
032900     EJECT                                                                
033000 01  DLI-IO-AREA-11.                                                      
033100*  03  -COPY WDK611                                                       
033200     EJECT                                                                
033210 01  FILLER               PIC X(16)   VALUE 'WDB201 AREA'.                
033220 01  DLI-IO-B201.                                                         
033230*    03  -COPY WDB201     -PRE GMTA-                                      
033240     EJECT                                                                
033300 01  DLI-IO-AREA2.                                                        
033400     03  IO-AREA2                PIC X(700)  VALUE SPACE.                 
033500     SKIP3                                                                
033600     03  WLARTM01 REDEFINES IO-AREA2.                                     
033700*        05  -COPY WDK901     -PRE ARTM-                                  
033800     EJECT                                                                
033900     03  WLARTM11 REDEFINES IO-AREA2.                                     
034000*        05  -COPY WDK911     -PRE ARTM-                                  
034100     SKIP3                                                                
034500     EJECT                                                                
034600     03  WLORQM01 REDEFINES IO-AREA2.                                     
034700*        05  -COPY WDQ101     -PRE ORQM-                                  
034800     EJECT                                                                
034900     03  WLORQI01 REDEFINES IO-AREA2.                                     
035000*        05  -COPY WDQ201     -PRE ORQI-                                  
035100     EJECT                                                                
035200     03  WLXXBU01 REDEFINES IO-AREA2.                                     
035300*        05  -COPY WDGX2223   -PRE XXBU-                                  
035400     EJECT                                                                
035500     03  WLXXBU11 REDEFINES IO-AREA2.                                     
035600*        05  -COPY WDGX2224   -PRE XXBU-                                  
035700     EJECT                                                                
035800     03  WLXXBX01 REDEFINES IO-AREA2.                                     
035900*        05  -COPY WDGX01     -PRE XXBX-                                  
036000     EJECT                                                                
036100     03  WLXXBX11 REDEFINES IO-AREA2.                                     
036200*        05  -COPY WDGX2232   -PRE XXBX-                                  
036300     EJECT                                                                
036400     03  WLXXBJ01 REDEFINES IO-AREA2.                                     
036500*        05  -COPY WDGX01     -PRE XXBJ-                                  
036600     EJECT                                                                
036700     03  WLXXBJ11 REDEFINES IO-AREA2.                                     
036800*        05  -COPY WDGX2204   -PRE XXBJ-                                  
036900     EJECT                                                                
037000     03  WLZZAC01 REDEFINES IO-AREA2.                                     
037100*        05  -COPY WDGZ01     -PRE ZZAC-                                  
037200     EJECT                                                                
037300 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
037400     SKIP3                                                                
037500 01  -COPY WZ01SEND                                                       
037600     EJECT                                                                
037700 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
037800     SKIP3                                                                
037900 01  SEND-AREA.                                                           
038000*    03  -COPY WZ01REQU  -PRE 3039-                                       
038100*    03  -COPY W30391I1  -PRE 3039-                                       
038200     EJECT                                                                
038300 LINKAGE SECTION.                                                         
038400                                                                          
038500*01  -COPY W0009      -PRE MSG-                                           
038600     EJECT                                                                
038700*01  -COPY W0009      -PRE PRQRY-                                         
038800     SKIP2                                                                
038900*01  -COPY W0008      -PRE USEA-                                          
039000     05  FILLER                  PIC X.                                   
039100     EJECT                                                                
039200*01  -COPY W0008      -PRE ARTC-                                          
039300     05  FILLER                  PIC X.                                   
039400     EJECT                                                                
039500*01  -COPY W0008      -PRE ARTM-                                          
039600     05  FILLER                  PIC X.                                   
039700     EJECT                                                                
039800*01  -COPY W0008      -PRE ORDP-                                          
039900     05  FILLER                  PIC X.                                   
040000     EJECT                                                                
040100*01  -COPY W0008      -PRE GMTA-                                          
040200     05  FILLER                  PIC X.                                   
040300     EJECT                                                                
040400*01  -COPY W0008      -PRE ORQM-                                          
040500     05  FILLER                  PIC X.                                   
040600     EJECT                                                                
040700*01  -COPY W0008      -PRE ORQI-                                          
040800     05  FILLER                  PIC X.                                   
040900     EJECT                                                                
041000*01  -COPY W0008      -PRE XXBU-                                          
041100     05  FILLER                  PIC X.                                   
041200     EJECT                                                                
041300*01  -COPY W0008      -PRE XXBX-                                          
041400     05  FILLER                  PIC X.                                   
041500     EJECT                                                                
041600*01  -COPY W0008      -PRE XXBJ-                                          
041700     05  FILLER                  PIC X.                                   
041800     EJECT                                                                
041900*01  -COPY W0008      -PRE ZZAC-                                          
042000     05  FILLER                  PIC X.                                   
042100     EJECT                                                                
042200*01  -COPY W0008      -PRE GMTB-                                          
042300     05  FILLER                  PIC X.                                   
042400     EJECT                                                                
042500*01  -COPY W0008      -PRE BETA-                                          
042600     05  FILLER                  PIC X.                                   
042700     EJECT                                                                
042800*01  -COPY W0008      -PRE PRIA-                                          
042900     05  FILLER                  PIC X.                                   
043000     EJECT                                                                
043100*01  -COPY W0008      -PRE PRIB-                                          
043200     05  FILLER                  PIC X.                                   
043300     EJECT                                                                
043400 01  PRIS-WDK7-PCB               PIC X.                                   
043500 01  PRIS-COST-WDK6-PCB          PIC X.                                   
043600 01  PRIS-COST-WDK7-PCB          PIC X.                                   
043700 01  PRIS-COST-WDF1-PCB          PIC X.                                   
043800 01  PRIS-COST-9305-PCB          PIC X.                                   
043900 01  PRIS-COST-WDK72-PCB         PIC X.                                   
044000 01  PRIS-COST-WDB6-PCB          PIC X.                                   
044200 01  PRNO-3107-PCB               PIC X.                                   
044300 01  PRQU-WDG2-PCB               PIC X.                                   
044400 01  PRQU-WDC7-PCB               PIC X.                                   
044500 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
044600                                                                          
044700 PROCEDURE DIVISION  USING MSG-PCB                                        
044800                  PRQRY-PCB USEA-PCB                                      
044900                  ARTC-PCB ARTM-PCB                                       
045000                  ORDP-PCB GMTA-PCB ORQM-PCB ORQI-PCB                     
045100                  XXBU-PCB XXBX-PCB XXBJ-PCB ZZAC-PCB GMTB-PCB            
045200                  BETA-PCB PRIA-PCB                                       
045300                  PRIB-PCB                                                
045400                  PRIS-WDK7-PCB                                           
045500                  PRIS-COST-WDK6-PCB                                      
045600                  PRIS-COST-WDK7-PCB                                      
045700                  PRIS-COST-WDF1-PCB                                      
045800                  PRIS-COST-9305-PCB                                      
045900                  PRIS-COST-WDK72-PCB                                     
046000                  PRIS-COST-WDB6-PCB                                      
046200                  PRNO-3107-PCB                                           
046300                  PRQU-WDG2-PCB                                           
046400                  PRQU-WDC7-PCB                                           
046500                  PRQU-SJKO-WDK6-PCB.                                     
046600 MAIN SECTION.                                                            
046700     ENTRY 'DLITCBL' USING MSG-PCB                                        
046800                  PRQRY-PCB USEA-PCB                                      
046900                  ARTC-PCB ARTM-PCB                                       
047000                  ORDP-PCB GMTA-PCB ORQM-PCB ORQI-PCB                     
047100                  XXBU-PCB XXBX-PCB XXBJ-PCB ZZAC-PCB GMTB-PCB            
047200                  BETA-PCB PRIA-PCB                                       
047300                  PRIB-PCB                                                
047400                  PRIS-WDK7-PCB                                           
047500                  PRIS-COST-WDK6-PCB                                      
047600                  PRIS-COST-WDK7-PCB                                      
047700                  PRIS-COST-WDF1-PCB                                      
047800                  PRIS-COST-9305-PCB                                      
047900                  PRIS-COST-WDK72-PCB                                     
048000                  PRIS-COST-WDB6-PCB                                      
048200                  PRNO-3107-PCB                                           
048300                  PRQU-WDG2-PCB                                           
048400                  PRQU-WDC7-PCB                                           
048500                  PRQU-SJKO-WDK6-PCB.                                     
048600                                                                          
048700     PERFORM IMS-GET-MSG                                                  
048800     IF SEGMENT-FINNS                                                     
048900       PERFORM A-INIT                                                     
049000       PERFORM B-KOLLA-NYCKLAR                                            
049100       IF NYCKLAR-OK                                                      
049200         IF MFS-UPDATE                                                    
049300           PERFORM G-KOLLA-INPUT                                          
049400           IF INDATA-OK                                                   
049500             PERFORM H-UPPDATERA                                          
049600           END-IF                                                         
049700         ELSE                                                             
049800           IF MFS-FIRST                                                   
049900             PERFORM C-FOERSTA-SIDA                                       
050000           ELSE                                                           
050100             IF MFS-NEXT                                                  
050200               PERFORM D-NAESTA-SIDA                                      
050300             ELSE                                                         
050400               PERFORM E-SAMMA-SIDA                                       
050500             END-IF                                                       
050600           END-IF                                                         
050700           IF ALLT-OK                                                     
050800             PERFORM F-LAES-VISA-INFO                                     
050900           END-IF                                                         
051000         END-IF                                                           
051100       END-IF                                                             
051200       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O12601 + 4                      
051300       PERFORM IMS-INSERT-MSG                                             
051400     END-IF                                                               
051500                                                                          
051600     MOVE ZERO TO RETURN-CODE                                             
051700     GOBACK                                                               
051800     .                                                                    
051900     EJECT                                                                
052000 A-INIT SECTION.                                                          
052100                                                                          
052200     IF MSG-DUBBLA-TRANSKODER                                             
052300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I12601                 
052400                                             2171-MID-W2I17101            
052500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
052600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
052700     ELSE                                                                 
052800       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I12601                 
052900                                             2171-MID-W2I17101            
053000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
053100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
053200     END-IF                                                               
053300                                                                          
053400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
053500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
053600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
053700                                                                          
053800     MOVE LOW-VALUE TO MSG-AREA                                           
053900     MOVE 'W2O12601' TO MFS-IDMOD                                         
054000     MOVE '2126' TO MOD-IDTRANS                                           
054100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
054200                                                                          
054300     IF NOT EGEN-MID                                                      
054400       MOVE SPACE TO MFS-KDTRTYP                                          
054500       MOVE '7' TO MFS-IDPFK                                              
054600     END-IF                                                               
054700                                                                          
054800     IF ENGLISH-TEXT                                                      
054900       MOVE +2 TO SPRAK-IX                                                
055000       MOVE 'GB ' TO MED-IDSKYLT                                          
055100     ELSE                                                                 
055200       MOVE +1 TO SPRAK-IX                                                
055300       MOVE 'S  ' TO MED-IDSKYLT                                          
055400     END-IF                                                               
055500                                                                          
055600     IF MFS-IDTRANS = '2171'                                              
055700       PERFORM AA-FIXA-2171                                               
055800     END-IF                                                               
055900                                                                          
056000     ACCEPT DAGENS-DATUM FROM DATE                                        
056100     ACCEPT AKTUELL-TID  FROM TIME                                        
056200                                                                          
056300     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
056400     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
056500     CALL WDATKONV USING DAT-KDDATFORM                                    
056600                         DAT-I-TIDATUM                                    
056700                         DAT-O-TIDATUM                                    
056800                         DAT-KDSVAR                                       
056900     MOVE DAT-TIAAVVD TO DAGENS-TIAAVVD                                   
057000     COMPUTE SEKEL-TAL = DAT-TISEKEL * 1000000                            
057100                                                                          
057200     MOVE SPACE TO ZZAC-SORTPOST                                          
057300                                                                          
057400     .                                                                    
057500     SKIP3                                                                
057600 AA-FIXA-2171 SECTION.                                                    
057700                                                                          
057800     MOVE JA TO 2171-TRANS-SW                                             
057900     MOVE 2171-MID-IDARTNR-UT     TO MID-IDARTNR-UT                       
058000                                     MOD-IDARTNR-UT                       
058100     INSPECT 2171-MID-IDANSK-UT REPLACING ALL SPACE BY ZERO               
058200     IF 2171-MID-IDANSK-UT NUMERIC                                        
058300       MOVE 2171-MID-IDANSK-UT TO W-IDANSK                                
058400     END-IF                                                               
058500                                                                          
058600     MOVE '+++++++++'             TO MID-IDARTNR-IN                       
058700                                                                          
058800     MOVE MFS-ADD-SAETT-CURSOR TO MOD-KVART-IN-ATTR(1)                    
058900     .                                                                    
059000     EJECT                                                                
059100 B-KOLLA-NYCKLAR SECTION.                                                 
059200                                                                          
059300     MOVE JA TO NYCKLAR-SW                                                
059400                                                                          
059500*    -- KONTROLL AV IDARTNR                                               
059600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
059700                                                                          
059800     MOVE ALL '+' TO MSGI-WMSGINIT                                        
059900     MOVE '001'             TO MSGI-KDCALL                                
060000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
060100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
060200     MOVE '2126'            TO MSGI-IDTRANS                               
060300     IF MFS-IDTRANS = '2126'                                              
060400     OR (MID-IDARTNR-IN NUMERIC                                           
060500     AND MID-IDARTNR-IN > ZERO)                                           
060600         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
060700     END-IF                                                               
060800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
060900     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
061000     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
061100                                                                          
061200     IF MID-IDARTNR-IN = ALL '+'                                          
061300       CONTINUE                                                           
061400     ELSE                                                                 
061500       MOVE '7'         TO MFS-IDPFK                                      
061600       MOVE SPACE       TO MFS-KDTRTYP                                    
061700     END-IF                                                               
061800     IF WS-IDARTNR NUMERIC                                                
061900       IF WS-IDARTNR > '000000000'                                        
062000         MOVE WS-IDARTNR TO W-IDARTNR-A                                   
062100         PERFORM IMS-GU-ARTC-ART                                          
062200         IF SEGMENT-SAKNAS                                                
062300           MOVE NEJ TO NYCKLAR-SW                                         
062400           MOVE ERR-ARTICLE-MISSING TO MED-IDMFSFEL                       
062500         ELSE                                                             
062600           MOVE ART-IDLEVNR TO WS-IDLEVNR   WS-IDLEVNR-8                  
062700                                                                          
062800*          --- KOLLA OM BEHÖRIG ANVÄNDARE                                 
062900           IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                      
063000           OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                
063100*   *         --- BEHÖRIG !                                               
063200              CONTINUE                                                    
063300           ELSE                                                           
063400              MOVE NEJ TO NYCKLAR-SW                                      
063500              MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                     
063600           END-IF                                                         
063700         END-IF                                                           
063800       ELSE                                                               
063900         IF NOT 2171-TRANS                                                
064000           MOVE NEJ TO NYCKLAR-SW                                         
064100         END-IF                                                           
064200       END-IF                                                             
064300     ELSE                                                                 
064400       MOVE NEJ TO NYCKLAR-SW                                             
064500     END-IF                                                               
064600     IF GODK-MID OR NYCKLAR-OK                                            
064700       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
064800       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
064900     ELSE                                                                 
065000       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
065100     END-IF                                                               
065200                                                                          
065300     IF  NYCKLAR-OK                                                       
065400     AND NOT MFS-NEXT                                                     
065500     AND EGEN-MID                                                         
065600       IF MID-TISENBEK-DAG-ENTER > 0                                      
065700       OR MID-TISENBEK-KL-ENTER  > 0                                      
065800       OR MID-KDLARM-ENTER       > 0                                      
065900         MOVE MID-IDANSK-DOLD        TO W-IDANSK                          
066000         MOVE MID-TISENBEK-DAG-ENTER TO W-TISENBEK-DAG                    
066100         MOVE MID-TISENBEK-KL-ENTER  TO W-TISENBEK-KL                     
066200         MOVE MID-KDLARM-ENTER       TO W-KDLARM                          
066300         PERFORM IMS-GET-XXBU-2223                                        
066400         IF SEGMENT-FINNS                                                 
066500           MOVE WC-CDC-SE            TO W-IDDC                            
066600           PERFORM IMS-GET-XXBU-2224                                      
066700           IF SEGMENT-FINNS                                               
066800             MOVE XXBU-2224-IDDISTR  TO W-IDDISTR                         
066900             MOVE XXBU-2224-IDKUNDNR TO W-IDKUNDNR                        
067000             MOVE XXBU-2224-IDKUNDRF TO WS-IDKUNDRF-FIX                   
067100             MOVE WS-IDORDER5        TO W-IDKUNDRF                        
067200             MOVE XXBU-2224-IDARTNR  TO W-IDARTNR                         
067300             MOVE XXBU-2224-IDLOPNR  TO W-IDLOPNR                         
067400           ELSE                                                           
067500             MOVE NEJ TO NYCKLAR-SW                                       
067600             MOVE ZERO TO MOD-TISENBEK-DAG-ENTER                          
067700                          MOD-TISENBEK-KL-ENTER                           
067800                          MOD-KDLARM-ENTER                                
067900           END-IF                                                         
068000         ELSE                                                             
068100           MOVE NEJ TO NYCKLAR-SW                                         
068200           MOVE ZERO TO MOD-TISENBEK-DAG-ENTER                            
068300                        MOD-TISENBEK-KL-ENTER                             
068400                        MOD-KDLARM-ENTER                                  
068500                        MOD-IDANSK-DOLD                                   
068600         END-IF                                                           
068700       END-IF                                                             
068800     END-IF                                                               
068900                                                                          
069000     IF NYCKLAR-FEL                                                       
069100       IF MED-IDMFSFEL NOT = ERR-ARTICLE-MISSING                          
069200                         AND ERR-NOT-AUTHORIZED                           
069300         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
069400       END-IF                                                             
069500       CALL WMEDKONV USING MED-WMEDAREA                                   
069600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
069700       PERFORM MFS-RENSA-FAELT-IN                                         
069800       PERFORM MFS-RENSA-FAELT-UT                                         
069900     END-IF                                                               
070000     .                                                                    
070100     EJECT                                                                
070200 C-FOERSTA-SIDA SECTION.                                                  
070300                                                                          
070400     MOVE JA TO ALLT-SW                                                   
070500                                                                          
070600*    --- HÄMTA NYCKLAR TILL LARMET                                        
070700     IF 2171-TRANS                                                        
070800       MOVE +1 TO INDX                                                    
070900       MOVE NEJ TO TRAFF-SW                                               
071000       PERFORM UNTIL TRAFF OR INDX > 14                                   
071100         IF 2171-MID-SELECT-ARTIKEL(INDX) = 'S'                           
071200           MOVE JA TO TRAFF-SW                                            
071300           PERFORM S05-CONVERT-TISENBEK-DAG-IN                            
071400           MOVE 2171-MID-TISENBEK-KL-IN(INDX) TO W-TISENBEK-KL            
071500           MOVE 2171-MID-KDLARM(INDX)         TO W-KDLARM                 
071600         ELSE                                                             
071700           ADD +1 TO INDX                                                 
071800         END-IF                                                           
071900       END-PERFORM                                                        
072000       PERFORM IMS-GET-XXBU-2223                                          
072100       IF SEGMENT-FINNS                                                   
072200         MOVE WC-CDC-SE           TO W-IDDC                               
072300         PERFORM IMS-GET-XXBU-2224                                        
072400         IF SEGMENT-FINNS AND WS-IDARTNR = '000000000'                    
072500           MOVE XXBU-2224-IDARTNR TO WS-IDARTNR                           
072600                                     W-IDARTNR-A                          
072700                                     MOD-IDARTNR-UT                       
072800           INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO                  
072900                   BY SPACE                                               
073000           MOVE ALL '+' TO MSGI-WMSGINIT                                  
073100           MOVE XXBU-2224-IDARTNR TO MSGI-IDARTNR                         
073200           MOVE '001'             TO MSGI-KDCALL                          
073300           MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                          
073400           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
073500         END-IF                                                           
073600       ELSE                                                               
073700         MOVE NEJ TO ALLT-SW                                              
073800       END-IF                                                             
073900     ELSE                                                                 
074000       PERFORM IMS-GET-ARTC-CLAG                                          
074100       IF SEGMENT-FINNS                                                   
074200         MOVE CLAG-IDANSK TO W-IDANSK-L                                   
074300         PERFORM IMS-GET-XXBX-2231                                        
074400         PERFORM IMS-GET-XXBX-2232                                        
074500         IF SEGMENT-FINNS                                                 
074600           MOVE XXBX-2232-IDANSK-LARM TO W-IDANSK                         
074700           PERFORM IMS-GET-XXBU-2223                                      
074800           IF SEGMENT-FINNS                                               
074900             MOVE WC-CDC-SE      TO W-IDDC                                
075000             PERFORM IMS-GET-XXBU-2224-IDART                              
075100             MOVE INF-FIRST-PAGE TO MED-IDMFSINF                          
075200             CALL WMEDKONV USING MED-WMEDAREA                             
075300             MOVE MED-MFSINF TO MOD-TEMFSINF                              
075400           ELSE                                                           
075500             MOVE NEJ TO ALLT-SW                                          
075600           END-IF                                                         
075700         ELSE                                                             
075800           MOVE ZERO TO W-IDANSK                                          
075900           PERFORM IMS-GET-XXBU-2223                                      
076000           IF SEGMENT-FINNS                                               
076100             MOVE WC-CDC-SE      TO W-IDDC                                
076200             PERFORM IMS-GET-XXBU-2224-IDART                              
076300             MOVE INF-FIRST-PAGE TO MED-IDMFSINF                          
076400             CALL WMEDKONV USING MED-WMEDAREA                             
076500             MOVE MED-MFSINF TO MOD-TEMFSINF                              
076600           ELSE                                                           
076700             MOVE NEJ TO ALLT-SW                                          
076800           END-IF                                                         
076900         END-IF                                                           
077000       ELSE                                                               
077100         MOVE NEJ TO ALLT-SW                                              
077200       END-IF                                                             
077300     END-IF                                                               
077400     .                                                                    
077500     EJECT                                                                
077600 D-NAESTA-SIDA SECTION.                                                   
077700                                                                          
077800     MOVE MID-IDANSK-DOLD       TO W-IDANSK                               
077900     IF  MID-TISENBEK-DAG-NEXT = ZERO                                     
078000     AND MID-TISENBEK-KL-NEXT  = ZERO                                     
078100       MOVE MID-TISENBEK-DAG-ENTER TO W-TISENBEK-DAG                      
078200       MOVE MID-TISENBEK-KL-ENTER  TO W-TISENBEK-KL                       
078300       MOVE MID-KDLARM-ENTER       TO W-KDLARM                            
078400     ELSE                                                                 
078500       MOVE MID-TISENBEK-DAG-NEXT  TO W-TISENBEK-DAG                      
078600       MOVE MID-TISENBEK-KL-NEXT   TO W-TISENBEK-KL                       
078700       MOVE MID-KDLARM-NEXT        TO W-KDLARM                            
078800     END-IF                                                               
078900                                                                          
079000     PERFORM IMS-GET-XXBU-2223                                            
079100     IF SEGMENT-FINNS                                                     
079200       MOVE WC-CDC-SE              TO W-IDDC                              
079300       PERFORM IMS-GET-XXBU-2224                                          
079400     ELSE                                                                 
079500       MOVE NEJ TO ALLT-SW                                                
079600     END-IF                                                               
079700     .                                                                    
079800     EJECT                                                                
079900 E-SAMMA-SIDA SECTION.                                                    
080000                                                                          
080100     MOVE JA TO ALLT-SW                                                   
080200                                                                          
080300     IF MID-INPUT = ALL '+'                                               
080400       MOVE MID-IDANSK-DOLD        TO W-IDANSK                            
080500       MOVE MID-TISENBEK-DAG-ENTER TO W-TISENBEK-DAG                      
080600       MOVE MID-TISENBEK-KL-ENTER  TO W-TISENBEK-KL                       
080700       MOVE MID-KDLARM-ENTER       TO W-KDLARM                            
080800       PERFORM IMS-GET-XXBU-2223                                          
080900       IF SEGMENT-FINNS                                                   
081000         MOVE WC-CDC-SE            TO W-IDDC                              
081100         PERFORM IMS-GET-XXBU-2224                                        
081200       END-IF                                                             
081300     ELSE                                                                 
081400       MOVE NEJ TO ALLT-SW                                                
081500       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
081600       CALL WMEDKONV USING MED-WMEDAREA                                   
081700       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
081800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
081900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
082000       PERFORM MFS-LAS-IN-IGEN                                            
082100     END-IF                                                               
082200     .                                                                    
082300     EJECT                                                                
082400 F-LAES-VISA-INFO SECTION.                                                
082500                                                                          
082600     IF SEGMENT-SAKNAS                                                    
082700       MOVE MED-3 TO MOD-TEMFSFEL                                         
082800       MOVE MFS-RENSA-FAELT          TO MOD-TEMFSINF                      
082900     ELSE                                                                 
083000       MOVE XXBU-2224-IDDISTR        TO W-IDDISTR                         
083100       MOVE XXBU-2224-IDKUNDNR       TO W-IDKUNDNR                        
083200       MOVE XXBU-2224-IDKUNDRF       TO WS-IDKUNDRF-FIX                   
083300       MOVE WS-IDORDER5              TO W-IDKUNDRF                        
083400       MOVE XXBU-2224-IDARTNR        TO W-IDARTNR                         
083500       MOVE XXBU-2224-IDLOPNR        TO W-IDLOPNR                         
083600       MOVE XXBU-2224-TISENBEK-DAG   TO MOD-TISENBEK-DAG-ENTER            
083700       MOVE XXBU-2224-TISENBEK-KL    TO MOD-TISENBEK-KL-ENTER             
083800       MOVE XXBU-2224-KDLARM         TO MOD-KDLARM-ENTER                  
083900       MOVE W-IDANSK                 TO MOD-IDANSK-DOLD                   
084000       MOVE WC-CDC-SE                TO W-IDDC                            
084100       PERFORM IMS-GET-XXBU-2224-IDART                                    
084200       IF SEGMENT-FINNS                                                   
084300         MOVE XXBU-2224-TISENBEK-DAG TO MOD-TISENBEK-DAG-NEXT             
084400         MOVE XXBU-2224-TISENBEK-KL  TO MOD-TISENBEK-KL-NEXT              
084500         MOVE XXBU-2224-KDLARM       TO MOD-KDLARM-NEXT                   
084600         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
084700         CALL WMEDKONV USING MED-WMEDAREA                                 
084800         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
084900       ELSE                                                               
085000         MOVE ZERO                   TO MOD-TISENBEK-DAG-NEXT             
085100                                        MOD-TISENBEK-KL-NEXT              
085200                                        MOD-KDLARM-NEXT                   
085300         MOVE MED-1 TO MOD-TEMFSINF                                       
085400       END-IF                                                             
085500       IF 2171-TRANS                                                      
085600         PERFORM IMS-GET-ORDP-RAD                                         
085700         PERFORM S12-GRUNDRAD-TILL-MOD                                    
085800       ELSE                                                               
085900         PERFORM IMS-GET-ORDP-RAD-GE                                      
086000         IF SEGMENT-FINNS                                                 
086100           PERFORM S12-GRUNDRAD-TILL-MOD                                  
086200         ELSE                                                             
086300           MOVE ZERO TO MOD-TISENBEK-DAG-ENTER                            
086400                        MOD-TISENBEK-KL-ENTER                             
086500                        MOD-KDLARM-ENTER                                  
086600           PERFORM MFS-RENSA-RAD-FAELT                                    
086700         END-IF                                                           
086800       END-IF                                                             
086900       PERFORM MFS-RENSA-FAELT-IN                                         
087000     END-IF                                                               
087100     .                                                                    
087200     EJECT                                                                
087300 G-KOLLA-INPUT SECTION.                                                   
087400                                                                          
087500     MOVE JA  TO INDATA-SW                                                
087600     IF MID-INPUT = ALL '+'                                               
087700       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
087800       CALL WMEDKONV USING MED-WMEDAREA                                   
087900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
088000       PERFORM MFS-ROER-EJ-FAELT-IN                                       
088100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
088200       MOVE NEJ TO INDATA-SW                                              
088300     ELSE                                                                 
088400       PERFORM GA-FORMELL-KONTROLL                                        
088500       IF INDATA-OK                                                       
088600         PERFORM GB-RELATIONS-KONTROLL                                    
088700         IF INDATA-OK                                                     
088800           PERFORM GC-DATABAS-KONTROLL                                    
088900         END-IF                                                           
089000       END-IF                                                             
089100                                                                          
089200       IF INDATA-FEL                                                      
089300         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
089400         CALL WMEDKONV USING MED-WMEDAREA                                 
089500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
089600         PERFORM MFS-ROER-EJ-FAELT-UT                                     
089700         PERFORM MFS-ROER-EJ-FAELT-IN                                     
089800       END-IF                                                             
089900     END-IF                                                               
090000     .                                                                    
090100     EJECT                                                                
090200 GA-FORMELL-KONTROLL SECTION.                                             
090300                                                                          
090400     MOVE +1 TO INDX                                                      
090500     PERFORM UNTIL INDX > MAX-INDX                                        
090600       IF MID-KVART-IN(INDX) = ALL '+'                                    
090700         CONTINUE                                                         
090800       ELSE                                                               
090900         IF  MID-KVART-IN(INDX) NUMERIC                                   
091000         AND MID-KVART-IN(INDX) > 0                                       
091100           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVART-IN-ATTR(INDX)            
091200           ADD MID-KVART-IN(INDX) TO WS-KVANT-SUM                         
091300         ELSE                                                             
091400           MOVE NEJ TO INDATA-SW                                          
091500           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVART-IN-ATTR(INDX)            
091600         END-IF                                                           
091700       END-IF                                                             
091800       IF MID-TITPO-IN(INDX) = ALL '+'                                    
091900         CONTINUE                                                         
092000       ELSE                                                               
092100         IF MID-TITPO-IN(INDX) NUMERIC                                    
092200           MOVE MID-TITPO-IN(INDX)   TO TMP1-YYWWD                        
092300           MOVE DAGENS-TIAAVVD       TO TMP2-YYWWD                        
092400           PERFORM WY2000P2                                               
092500           IF TMP1-YYWWD  < TMP2-YYWWD                                    
092600             MOVE NEJ TO INDATA-SW                                        
092700             MOVE MFS-NUM-FAELT-FEL TO MOD-TITPO-IN-ATTR(INDX)            
092800           ELSE                                                           
092900             PERFORM S02-KONV-TITPO-2                                     
093000             IF DAT-KDSVAR-OK                                             
093100               MOVE DAT-TIAAMMDD TO WS-TITPO(INDX)                        
093200               MOVE MFS-NUM-FAELT-RAETT TO                                
093300                                       MOD-TITPO-IN-ATTR(INDX)            
093400             ELSE                                                         
093500               MOVE NEJ TO INDATA-SW                                      
093600               MOVE MFS-NUM-FAELT-FEL   TO                                
093700                                       MOD-TITPO-IN-ATTR(INDX)            
093800             END-IF                                                       
093900           END-IF                                                         
094000         ELSE                                                             
094100           MOVE NEJ TO INDATA-SW                                          
094200           MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-IN-ATTR(INDX)            
094300         END-IF                                                           
094400       END-IF                                                             
094500       ADD +1 TO INDX                                                     
094600     END-PERFORM                                                          
094700     IF INDATA-OK                                                         
094800       IF  MID-KVART-IN(1) NUMERIC                                        
094900       AND MID-TITPO-IN(1) NUMERIC                                        
095000         MOVE JA TO SPLITTAD-RAD-SW                                       
095100       END-IF                                                             
095200     END-IF                                                               
095300     .                                                                    
095400     EJECT                                                                
095500 GB-RELATIONS-KONTROLL SECTION.                                           
095600                                                                          
095700     MOVE +1 TO INDX                                                      
095800     PERFORM UNTIL INDX > MAX-INDX                                        
095900       IF INDX = MAX-INDX                                                 
096000         CONTINUE                                                         
096100       ELSE                                                               
096200         IF  MID-INFO-RAD(INDX) = ALL '+'                                 
096300           IF MID-INFO-RAD(INDX + 1) NOT = ALL '+'                        
096400             MOVE NEJ TO INDATA-SW                                        
096500             MOVE MFS-NUM-FAELT-FEL TO MOD-KVART-IN-ATTR(INDX)            
096600             MOVE MFS-NUM-FAELT-FEL TO MOD-TITPO-IN-ATTR(INDX)            
096700             MOVE MFS-NUM-FAELT-FEL TO                                    
096800                                   MOD-KVART-IN-ATTR(INDX + 1)            
096900             MOVE MFS-NUM-FAELT-FEL TO                                    
097000                                   MOD-TITPO-IN-ATTR(INDX + 1)            
097100             MOVE MED-4 TO MOD-TEMFSINF                                   
097200           END-IF                                                         
097300         ELSE                                                             
097400           IF MID-TITPO-IN(INDX) = ALL '+'                                
097500             MOVE NEJ TO INDATA-SW                                        
097600             MOVE MFS-NUM-FAELT-FEL TO MOD-TITPO-IN-ATTR(INDX)            
097700           ELSE                                                           
097800             IF MID-KVART-IN(INDX) = ALL '+'                              
097900               IF INDX = 1                                                
098000                 IF MID-INFO-RAD(INDX + 1) = ALL '+'                      
098100                   CONTINUE                                               
098200                 ELSE                                                     
098300                   MOVE NEJ TO INDATA-SW                                  
098400                   MOVE MFS-NUM-FAELT-FEL TO                              
098500                                        MOD-KVART-IN-ATTR(INDX)           
098600                 END-IF                                                   
098700               ELSE                                                       
098800                 MOVE NEJ TO INDATA-SW                                    
098900                 MOVE MFS-NUM-FAELT-FEL TO                                
099000                                        MOD-KVART-IN-ATTR(INDX)           
099100               END-IF                                                     
099200             END-IF                                                       
099300           END-IF                                                         
099400         END-IF                                                           
099500       END-IF                                                             
099600       ADD +1 TO INDX                                                     
099700     END-PERFORM                                                          
099800     .                                                                    
099900     EJECT                                                                
100000 GC-DATABAS-KONTROLL SECTION.                                             
100100                                                                          
100200     MOVE ART-KDSORT TO WS-KDSORT                                         
100500     PERFORM IMS-GET-ARTC-CLAG                                            
100600     IF SEGMENT-FINNS                                                     
100700       MOVE CLAG-KVQPACK-1 TO WS-KVQPACK-1                                
100800     END-IF                                                               
100900     PERFORM IMS-GET-ORDP-RAD-GE                                          
101000     IF SEGMENT-FINNS                                                     
101100       PERFORM GCA-KOLLA-OM-KVANTANPASSA                                  
101200       MOVE +1 TO INDX                                                    
101300       PERFORM UNTIL INDX > MAX-INDX OR                                   
101400                     MID-INFO-RAD(INDX) = ALL '+'                         
101500         IF MID-KVART-IN(INDX) = ALL '+'                                  
101600           CONTINUE                                                       
101700         ELSE                                                             
101800           PERFORM GCB-KOLLA-RADENS-KVANT                                 
101900           IF MID-KVART-IN(INDX) = ORDP-RAD-KVART                         
102000             MOVE NEJ TO SPLITTAD-RAD-SW                                  
102100           END-IF                                                         
102200         END-IF                                                           
102300         ADD +1 TO INDX                                                   
102400       END-PERFORM                                                        
102500       IF INDATA-OK                                                       
102600         IF WS-KVANT-SUM = 0                                              
102700         OR WS-KVANT-SUM <= ORDP-RAD-KVART                                
102800           CONTINUE                                                       
102900         ELSE                                                             
103000           MOVE NEJ TO INDATA-SW                                          
103100           MOVE +1 TO INDX                                                
103200           PERFORM UNTIL INDX > MAX-INDX                                  
103300             IF MID-KVART-IN(INDX) = ALL '+'                              
103400               CONTINUE                                                   
103500             ELSE                                                         
103600               MOVE MFS-NUM-FAELT-FEL TO MOD-KVART-IN-ATTR(INDX)          
103700             END-IF                                                       
103800             ADD +1 TO INDX                                               
103900           END-PERFORM                                                    
104000         END-IF                                                           
104100       END-IF                                                             
104200     ELSE                                                                 
104300       MOVE NEJ TO INDATA-SW                                              
104400     END-IF                                                               
104500     .                                                                    
104600     EJECT                                                                
104700 GCA-KOLLA-OM-KVANTANPASSA SECTION.                                       
104800                                                                          
104900     IF (WS-KDSORT = 'KG' OR = 'L' OR = 'M')                              
105000     OR (ORDP-RAD-KDKVBRYT = 0 AND WS-KVQPACK-1 > +1)                     
105100       MOVE JA  TO KVANTANPASSA-SW                                        
105200     ELSE                                                                 
105300       MOVE NEJ TO KVANTANPASSA-SW                                        
105400     END-IF                                                               
105500     .                                                                    
105600     SKIP3                                                                
105700 GCB-KOLLA-RADENS-KVANT SECTION.                                          
105800                                                                          
105900     IF WS-KVQPACK-1 > 0                                                  
106000       COMPUTE WS-ANTAL-FORP =                                            
106100                        MID-KVART-IN(INDX) / WS-KVQPACK-1                 
106200       COMPUTE WS-KVANT = WS-ANTAL-FORP * WS-KVQPACK-1                    
106300       IF KVANTANPASSA                                                    
106400         IF WS-KVANT = MID-KVART-IN(INDX)                                 
106500           CONTINUE                                                       
106600         ELSE                                                             
106700           MOVE NEJ TO INDATA-SW                                          
106800           MOVE MFS-NUM-FAELT-FEL TO MOD-KVART-IN-ATTR(INDX)              
106900           MOVE MED-2 TO MOD-TEMFSINF                                     
107000         END-IF                                                           
107100       ELSE                                                               
107200         IF  ORDP-RAD-KDKVBRYT = +2                                       
107300         AND WS-KVANT NOT = MID-KVART-IN(INDX)                            
107400           MOVE +1 TO ORDP-RAD-KDKVBRYT                                   
107500         END-IF                                                           
107600       END-IF                                                             
107700     END-IF                                                               
107800     .                                                                    
107900     EJECT                                                                
108000 H-UPPDATERA SECTION.                                                     
108100                                                                          
108200     MOVE ORDP-RAD-IDDISTR    TO TEST-IDDISTR                             
108300     IF SPLITTAD-RAD                                                      
108400       PERFORM HA-BEARBETA-SPLIT                                          
108500     ELSE                                                                 
108600       PERFORM HB-BEARB-GODK-AENDR-TITPO                                  
108700     END-IF                                                               
108800     PERFORM HC-VISA-BILD-EFTER-UPPDAT                                    
108900                                                                          
109000     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
109100     CALL WMEDKONV USING MED-WMEDAREA                                     
109200     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
109300     MOVE MFS-ROER-EJ-FAELT TO MOD-TEMFSINF                               
109400     .                                                                    
109500     EJECT                                                                
109600 HA-BEARBETA-SPLIT SECTION.                                               
109700                                                                          
109800     IF WS-KVANT-SUM < ORDP-RAD-KVART                                     
109900* GAMMAL RAD BEARBETAS                                                    
110000       PERFORM HAA-UPPD-ORDERRAD-1                                        
110100       IF ORDP-RAD-KDTPOTYP = 6                                           
110200         PERFORM S15-SKAPA-RY4-TRANS                                      
110300         PERFORM S03-UPPD-ORDK-SALDO                                      
110400         PERFORM S04-ORDER-BEKR                                           
110500         PERFORM S16-SKAPA-RY9-TRANS                                      
110600       ELSE                                                               
110700         PERFORM S08-HAEMTA-KUNDENS-CL                                    
110800         PERFORM S06-UPPD-SUTPO-TOT                                       
110900         MOVE JA TO ANNAN-NYCKEL-SW                                       
111000         PERFORM S11-UPPD-SUTPO-PB-EJPB                                   
111100         PERFORM S04-ORDER-BEKR                                           
111200         PERFORM S16-SKAPA-RY9-TRANS                                      
111300       END-IF                                                             
111400       MOVE ORDP-RAD-TITPO TO SPAR-RAD-TITPO                              
111500     ELSE                                                                 
111600       PERFORM IMS-DLET-ORDP                                              
111700       MOVE ZERO TO ORDP-RAD-IDLOPNR                                      
111800     END-IF                                                               
111900     MOVE +1 TO INDX                                                      
112000                MOD-IX                                                    
112100* NYA RADER BEARBETAS                                                     
112310     PERFORM UNTIL INDX > MAX-INDX OR                                     
112320                   MID-INFO-RAD(INDX) = ALL '+'                           
112400       IF ORDP-RAD-KDTPOTYP = 6                                           
112500       AND MID-TITPO-IN(INDX) = DAGENS-TIAAVVD                            
112600         PERFORM HAB-UPPD-ORDERRAD-2                                      
112700         PERFORM S15-SKAPA-RY4-TRANS                                      
112800         PERFORM S03-UPPD-ORDK-SALDO                                      
112900         PERFORM S04-ORDER-BEKR                                           
113000         PERFORM S16-SKAPA-RY9-TRANS                                      
113100       ELSE                                                               
113200         PERFORM HAC-UPPD-ORDERRAD-3                                      
113300         PERFORM S08-HAEMTA-KUNDENS-CL                                    
113400         PERFORM S06-UPPD-SUTPO-TOT                                       
113500         PERFORM S11-UPPD-SUTPO-PB-EJPB                                   
113600         IF ( (ORDP-RAD-KDTPOTYP = 2) AND                                 
113700              (WS-TITPO(INDX) NOT = SPAR-RAD-TITPO) )                     
113800         OR  ORDP-RAD-KDTPOTYP = 6                                        
113900           PERFORM S04-ORDER-BEKR                                         
114000           PERFORM S16-SKAPA-RY9-TRANS                                    
114100         END-IF                                                           
114200       END-IF                                                             
114300       MOVE ORDP-RAD-TITPO   TO TMP1-YYMMDD                               
114400       MOVE DAGENS-DATUM     TO TMP2-YYMMDD                               
114500       PERFORM WY2000P1                                                   
114600       IF  ORDP-RAD-KDTPOTYP = 6                                          
114700       AND TMP1-YYMMDD > TMP2-YYMMDD                                      
114800       AND 2204-SW = NEJ                                                  
114900         MOVE JA TO 2204-SW                                               
115000       END-IF                                                             
115100       ADD +1 TO INDX                                                     
115200                 MOD-IX                                                   
115300     END-PERFORM                                                          
115400     PERFORM S14-DLET-LARM                                                
115500     IF 2204-ISRT                                                         
115600       PERFORM S07-SKAPA-2204                                             
115700     END-IF                                                               
115800     .                                                                    
115900     EJECT                                                                
116000 HAA-UPPD-ORDERRAD-1 SECTION.                                             
116100                                                                          
116200     IF ORDP-RAD-KDTPOTYP = 6                                             
116300       MOVE 3 TO ORDP-RAD-KDSTARAD                                        
116400       MOVE DAGENS-DATUM TO ORDP-RAD-TIRES                                
116500     ELSE                                                                 
116600       MOVE 1 TO ORDP-RAD-KDSTARAD                                        
116700     END-IF                                                               
116800     MOVE 'J' TO ORDP-RAD-FLTPOBEK                                        
116900     IF ORDP-RAD-TITPO = ZERO                                             
117000       MOVE DAGENS-DATUM TO ORDP-RAD-TITPO                                
117100     END-IF                                                               
117200     SUBTRACT WS-KVANT-SUM FROM ORDP-RAD-KVART                            
117300     IF DIST79-DEALER-PRICE                                               
117400       IF ORDP-RAD-KDSTARAD = '3'                                         
117500*        PERFORM S20-KOMPLETTERA-PRIS                                     
117600         CONTINUE                                                         
117700       END-IF                                                             
117800       PERFORM IMS-REPL-ORDP                                              
117900     ELSE                                                                 
118000       IF  ORDP-RAD-KDSTARAD = '3'                                        
118100       AND (PRISTILLAEMPA OR                                              
118200        ORDP-RAD-PRARTNTO = ZERO)                                         
118500         PERFORM S18-PRISTILLAEMPA                                        
118600         PERFORM IMS-REPL-ORDP                                            
118700         MOVE ZERO TO ORDP-RAD-PRARTNTO                                   
118800       ELSE                                                               
118900         PERFORM IMS-REPL-ORDP                                            
119000       END-IF                                                             
119100     END-IF                                                               
119200                                                                          
119300     PERFORM S12-GRUNDRAD-TILL-MOD                                        
119400     .                                                                    
119500     SKIP3                                                                
119600 HAB-UPPD-ORDERRAD-2 SECTION.                                             
119700                                                                          
119800     MOVE MID-KVART-IN(INDX) TO ORDP-RAD-KVART                            
119900     MOVE 'J'                TO ORDP-RAD-FLTPOBEK                         
120000     MOVE 3                  TO ORDP-RAD-KDSTARAD                         
120100     MOVE DAGENS-DATUM       TO ORDP-RAD-TIRES                            
120200     ADD +1                  TO ORDP-RAD-IDLOPNR                          
120300     MOVE WS-TITPO(INDX)     TO ORDP-RAD-TITPO                            
120500     IF DIST79-DEALER-PRICE                                               
120600       PERFORM S20-KOMPLETTERA-PRIS                                       
120700       PERFORM IMS-ISRT-ORDP-RAD                                          
120800       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
120900         ADD +1 TO ORDP-RAD-IDLOPNR                                       
121000         PERFORM IMS-ISRT-ORDP-RAD                                        
121100       END-PERFORM                                                        
121200     ELSE                                                                 
121300     IF ORDP-RAD-PRARTNTO = ZERO OR                                       
121400        PRISTILLAEMPA                                                     
121600       PERFORM S18-PRISTILLAEMPA                                          
121700       PERFORM IMS-ISRT-ORDP-RAD                                          
121800       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
121900         ADD +1 TO ORDP-RAD-IDLOPNR                                       
122000         PERFORM IMS-ISRT-ORDP-RAD                                        
122100       END-PERFORM                                                        
122200       MOVE ZERO TO ORDP-RAD-PRARTNTO                                     
122300     ELSE                                                                 
122400       PERFORM IMS-ISRT-ORDP-RAD                                          
122500       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
122600         ADD +1 TO ORDP-RAD-IDLOPNR                                       
122700         PERFORM IMS-ISRT-ORDP-RAD                                        
122800       END-PERFORM                                                        
122900     END-IF                                                               
123000     END-IF                                                               
123100                                                                          
123200     IF ORDP-RAD-IDLOPNR = +1 AND MOD-IX = +1                             
123300       PERFORM S12-GRUNDRAD-TILL-MOD                                      
123400       MOVE ZERO TO MOD-IX                                                
123500     ELSE                                                                 
123600       PERFORM S13-SPLITRAD-TILL-MOD                                      
123700     END-IF                                                               
123800     .                                                                    
123900     EJECT                                                                
124000 HAC-UPPD-ORDERRAD-3 SECTION.                                             
124100                                                                          
124200     MOVE WS-TITPO(INDX)     TO ORDP-RAD-TITPO                            
124300     MOVE MID-KVART-IN(INDX) TO ORDP-RAD-KVART                            
124400     MOVE 'J'                TO ORDP-RAD-FLTPOBEK                         
124500     MOVE 1                  TO ORDP-RAD-KDSTARAD                         
124600     ADD +1                  TO ORDP-RAD-IDLOPNR                          
124700     MOVE ZERO               TO ORDP-RAD-TIRES                            
124900     PERFORM IMS-ISRT-ORDP-RAD                                            
125000     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
125100       ADD +1 TO ORDP-RAD-IDLOPNR                                         
125200       PERFORM IMS-ISRT-ORDP-RAD                                          
125300     END-PERFORM                                                          
125400                                                                          
125500     IF ORDP-RAD-IDLOPNR = +1 AND MOD-IX = +1                             
125600       PERFORM S12-GRUNDRAD-TILL-MOD                                      
125700       MOVE ZERO TO MOD-IX                                                
125800     ELSE                                                                 
125900       PERFORM S13-SPLITRAD-TILL-MOD                                      
126000     END-IF                                                               
126100     .                                                                    
126200     EJECT                                                                
126300 HB-BEARB-GODK-AENDR-TITPO SECTION.                                       
126400                                                                          
126500     MOVE +1 TO INDX                                                      
126600     IF ORDP-RAD-KDTPOTYP = 6                                             
126700     AND WS-TITPO(1) = DAGENS-DATUM                                       
126800* ALT1                                                                    
126900       PERFORM HBA-UPPD-ORDERRAD-4                                        
127000       PERFORM S15-SKAPA-RY4-TRANS                                        
127100       PERFORM S03-UPPD-ORDK-SALDO                                        
127200       PERFORM S04-ORDER-BEKR                                             
127300       PERFORM S16-SKAPA-RY9-TRANS                                        
127400     ELSE                                                                 
127500* ALT2                                                                    
127600       PERFORM HBB-UPPD-ORDERRAD-5                                        
127700       PERFORM S08-HAEMTA-KUNDENS-CL                                      
127800       PERFORM S06-UPPD-SUTPO-TOT                                         
127900       PERFORM S11-UPPD-SUTPO-PB-EJPB                                     
128000       IF ORDP-RAD-KDTPOTYP = 6                                           
128100         PERFORM S07-SKAPA-2204                                           
128200       END-IF                                                             
128300       IF ( (ORDP-RAD-KDTPOTYP  = 2) AND                                  
128400            (WS-TITPO(INDX) NOT = SPAR-RAD-TITPO) )                       
128500       OR  ORDP-RAD-KDTPOTYP = 6                                          
128600         PERFORM S04-ORDER-BEKR                                           
128700         PERFORM S16-SKAPA-RY9-TRANS                                      
128800         IF ORDP-RAD-KDTPOTYP = 6                                         
128900             CONTINUE                                                     
129000         ELSE                                                             
129100           MOVE ZERO           TO RY9-KVART                               
129200           MOVE SPAR-RAD-TITPO TO RY9-TITPO                               
129300           MOVE RY9-WDGZRY9    TO ZZAC-LOGGPOST                           
129400           MOVE WDGZRY9S       TO ZZAC-SORTPOST                           
129500           ADD +1              TO ZZAC-IDLOGLOP                           
129600           PERFORM IMS-ISRT-ZZAC                                          
129700         END-IF                                                           
129800       END-IF                                                             
129900     END-IF                                                               
130000     MOVE MID-TITPO-IN(INDX) TO MOD-TITPO                                 
130100     PERFORM HBC-FLYTTA-RAD-TILL-MOD                                      
130200     PERFORM S14-DLET-LARM                                                
130300     .                                                                    
130400     EJECT                                                                
130500 HBA-UPPD-ORDERRAD-4 SECTION.                                             
130600                                                                          
130700     MOVE JA           TO ORDP-RAD-FLTPOBEK                               
130800     MOVE 3            TO ORDP-RAD-KDSTARAD                               
130900     MOVE DAGENS-DATUM TO ORDP-RAD-TIRES                                  
131000     IF ORDP-RAD-TITPO = ZERO                                             
131100       MOVE DAGENS-DATUM TO ORDP-RAD-TITPO                                
131200     END-IF                                                               
131300     IF DIST79-DEALER-PRICE                                               
131400       PERFORM S20-KOMPLETTERA-PRIS                                       
131500     ELSE                                                                 
131600     IF ORDP-RAD-PRARTNTO = 0                                             
131800       PERFORM S18-PRISTILLAEMPA                                          
131900       MOVE JA TO PRISTILL-SW                                             
132000     END-IF                                                               
132100     END-IF                                                               
132200     PERFORM IMS-REPL-ORDP                                                
132300     .                                                                    
132400     SKIP3                                                                
132500 HBB-UPPD-ORDERRAD-5 SECTION.                                             
132600                                                                          
132700     MOVE ORDP-RAD-TITPO TO SPAR-RAD-TITPO                                
132800                                                                          
132900     MOVE JA          TO ORDP-RAD-FLTPOBEK                                
133000     MOVE WS-TITPO(1) TO ORDP-RAD-TITPO                                   
133100                                                                          
133200     PERFORM IMS-REPL-ORDP                                                
133300     .                                                                    
133400     EJECT                                                                
133500 HBC-FLYTTA-RAD-TILL-MOD SECTION.                                         
133600                                                                          
133700     PERFORM S12-GRUNDRAD-TILL-MOD                                        
133800                                                                          
133900     MOVE +1 TO INDX                                                      
134000     PERFORM UNTIL INDX > MAX-INDX                                        
134100       MOVE MFS-RENSA-FAELT TO MOD-KVART-UT(INDX)                         
134200                               MOD-TITPO-UT(INDX)                         
134300       ADD +1 TO INDX                                                     
134400     END-PERFORM                                                          
134500     .                                                                    
134600     EJECT                                                                
134700 HC-VISA-BILD-EFTER-UPPDAT SECTION.                                       
134800                                                                          
134900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSK-DOLD                            
135000                               MOD-TISENBEK-DAG-ENTER                     
135100                               MOD-TISENBEK-KL-ENTER                      
135200                               MOD-KDLARM-ENTER                           
135300                               MOD-TISENBEK-DAG-NEXT                      
135400                               MOD-TISENBEK-KL-NEXT                       
135500                               MOD-KDLARM-NEXT                            
135600     MOVE +1 TO INDX                                                      
135700     PERFORM UNTIL INDX > MAX-INDX                                        
135800       MOVE MFS-RENSA-FAELT  TO MOD-KVART-IN(INDX)                        
135900                                MOD-TITPO-IN(INDX)                        
136000       MOVE MFS-STAENG-FAELT TO MOD-KVART-IN-ATTR(INDX)                   
136100                                MOD-TITPO-IN-ATTR(INDX)                   
136200       ADD +1 TO INDX                                                     
136300     END-PERFORM                                                          
136400                                                                          
136500     .                                                                    
136600     EJECT                                                                
136700 S01-KONV-TITPO-1 SECTION.                                                
136800                                                                          
136900     MOVE ORDP-RAD-TITPO    TO DAT-I-TIDATUM                              
137000     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
137100     CALL WDATKONV USING       DAT-KDDATFORM                              
137200                               DAT-I-TIDATUM                              
137300                               DAT-O-TIDATUM                              
137400                               DAT-KDSVAR                                 
137500     .                                                                    
137600     EJECT                                                                
137700 S02-KONV-TITPO-2 SECTION.                                                
137800                                                                          
137900     MOVE MID-TITPO-IN(INDX) TO DAT-I-TIDATUM                             
138000     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
138100     CALL WDATKONV USING        DAT-KDDATFORM                             
138200                                DAT-I-TIDATUM                             
138300                                DAT-O-TIDATUM                             
138400                                DAT-KDSVAR                                
138500     .                                                                    
138600     EJECT                                                                
138700 S03-UPPD-ORDK-SALDO SECTION.                                             
138800                                                                          
138900     PERFORM S09-LAES-ELLER-SKAPA-WDK901                                  
139000     IF ORDP-RAD-KDORDKL = 0                                              
139100       ADD ORDP-RAD-KVART TO ARTM-ART-KVOKS-VOR                           
139200     ELSE                                                                 
139300       IF ORDP-RAD-KDORDKL = 1                                            
139400         ADD ORDP-RAD-KVART TO ARTM-ART-KVOKS-DAG                         
139500       ELSE                                                               
139600         IF ORDP-RAD-KDORDKL = 2 OR 3 OR 4                                
139700           ADD ORDP-RAD-KVART TO ARTM-ART-KVOKS-BULK                      
139800         END-IF                                                           
139900       END-IF                                                             
140000     END-IF                                                               
140100     PERFORM IMS-REPL-ARTM                                                
140200     .                                                                    
140300     EJECT                                                                
140400 S04-ORDER-BEKR SECTION.                                                  
140500                                                                          
140600     MOVE ORDP-RAD-IDDISTR  TO W-IDDISTR-R                                
140700     MOVE ORDP-RAD-IDKUNDNR TO W-IDKUNDNR-R                               
140800     MOVE '00'              TO W-IDKUNDRF-R(1:2)                          
140900     MOVE ORDP-RAD-IDKUNDRF TO W-IDKUNDRF-R(3:5)                          
141000     PERFORM IMS-GET-ORQI-CSEQ                                            
141100     MOVE ORQI-OHUV-IDORDER         TO WS-IDORDER                         
141200       MOVE W-IDKUNDRF-R            TO WS-IDKUNDRF                        
141300     PERFORM IMS-GU-ARTC-CLAG                                             
141400     IF SEGMENT-FINNS                                                     
141500       MOVE CLAG-TIDISPIN           TO WS-TIDISPIN                        
141600     END-IF                                                               
141700     MOVE WS-IDORDER                TO ORQM-OBKR-IDORDER                  
141800     MOVE ORDP-RAD-IDARTNR          TO ORQM-OBKR-IDARTNR                  
141900     MOVE +1                        TO ORQM-OBKR-IDLOPNR                  
142000                                       ORQM-OBKR-IDSEKVNR                 
142100     MOVE WC-CDC-SE                 TO ORQM-OBKR-IDDC                     
142200     MOVE 71                        TO ORQM-OBKR-KDORDBEK                 
142300     MOVE SPACE                     TO ORQM-OBKR-BEERS                    
142400     MOVE ORDP-RAD-BEKUNDRF         TO ORQM-OBKR-BEKUNDRF                 
142500     MOVE ORDP-RAD-BERADREF         TO ORQM-OBKR-BERADREF                 
142600     MOVE ORDP-RAD-BEVOLREF         TO ORQM-OBKR-BEVOLREF                 
142700     MOVE ORDP-RAD-IDKAMPRF         TO ORQM-OBKR-IDKAMPRF                 
142800     MOVE ZERO                      TO ORQM-OBKR-DIERS-KVOT               
142900     MOVE NEJ                       TO ORQM-OBKR-FLAKPLOC                 
143000     MOVE ORDP-RAD-FLINVEST         TO ORQM-OBKR-FLINVEST                 
143100     MOVE JA                        TO ORQM-OBKR-FLOBOK                   
143200     MOVE NEJ                       TO ORQM-OBKR-FLOBTRAN                 
143300                                       ORQM-OBKR-FLOBPRT                  
143400     MOVE ORDP-RAD-FLPRTILL         TO ORQM-OBKR-FLPRTILL                 
143500     MOVE JA                        TO ORQM-OBKR-FLRESTN                  
143600     MOVE NEJ                       TO ORQM-OBKR-FLSLATT                  
143700     MOVE ORDP-RAD-FLERS            TO ORQM-OBKR-FLTILLK                  
143800     MOVE ZERO                      TO ORQM-OBKR-IDARTNR-TILLK            
143900     MOVE ORDP-RAD-IDDISTR          TO ORQM-OBKR-IDDISTR                  
144000     MOVE ORDP-RAD-IDKUNDNR         TO ORQM-OBKR-IDKUNDNR                 
144100     MOVE WS-IDKUNDRF               TO ORQM-OBKR-IDKUNDRF                 
144200     MOVE '0000000   '              TO ORQM-OBKR-IDKUNDRF-RO              
144300     MOVE ORDP-RAD-IDLEVNR          TO ORQM-OBKR-IDLEVNR                  
144400     MOVE ORDP-RAD-IDLOPNR          TO ORQM-OBKR-IDLOPNR-RO               
144500     MOVE ORDP-RAD-IDSYSTEM         TO ORQM-OBKR-IDSYSTEM                 
144600     MOVE ORDP-RAD-IDDC-RO          TO ORQM-OBKR-IDDC-RO                  
144700     MOVE IDPGM                     TO ORQM-OBKR-IDPGM                    
144800     MOVE ORDP-RAD-KDDSP            TO ORQM-OBKR-KDDSP                    
144900     MOVE ZERO                      TO ORQM-OBKR-KDERS                    
145000     MOVE ORDP-RAD-KDOI             TO ORQM-OBKR-KDOI                     
145100     MOVE ORDP-RAD-CLEARGROUP       TO ORQM-OBKR-CLEARGROUP               
145200     MOVE ORDP-RAD-KDKVBRYT         TO ORQM-OBKR-KDKVBRYT                 
145300     MOVE ORDP-RAD-KDPRTYP          TO ORQM-OBKR-KDPRTYP                  
145400     MOVE ORDP-RAD-KDTPOTYP         TO ORQM-OBKR-KDTPOTYP                 
145500     MOVE ORDP-RAD-KDVRINFO         TO ORQM-OBKR-KDVRINFO                 
145600     MOVE ZERO                      TO ORQM-OBKR-KVANNANT                 
145700                                       ORQM-OBKR-KVAVBART                 
145800     MOVE ORDP-RAD-KVART            TO ORQM-OBKR-KVBEART                  
145900                                       ORQM-OBKR-KVBEART-Q                
146000     MOVE ZERO                      TO ORQM-OBKR-KVBEART-TILLK            
146100                                       ORQM-OBKR-KVPREAVB                 
146200                                       ORQM-OBKR-KVPRERO                  
146300                                       ORQM-OBKR-KVQPACK                  
146400                                       ORQM-OBKR-KVRO                     
146500                                       ORQM-OBKR-KVSLATT                  
146600     MOVE ORDP-RAD-PRARTNTO         TO ORQM-OBKR-PRARTNTO                 
146700     MOVE ZERO                      TO ORQM-OBKR-PRBPRIS                  
146800     MOVE ORDP-RAD-REKSIFFR         TO ORQM-OBKR-REKSIFFR                 
146900     MOVE ZERO                      TO ORQM-OBKR-REKSIFFR-TILLK           
147000                                       ORQM-OBKR-RERF-RAD                 
147100     MOVE WS-TIDISPIN               TO ORQM-OBKR-TIDISPIN                 
147200     MOVE ORDP-RAD-TIREGDAT         TO ORQM-OBKR-TIORDREG                 
147300     MOVE ZERO                      TO ORQM-OBKR-TIPRIS                   
147400                                       ORQM-OBKR-TIRODAT                  
147500     MOVE DAGENS-DATUM              TO ORQM-OBKR-TIREGDAT                 
147600     MOVE AKTUELL-TID (1:7)         TO ORQM-OBKR-TIREGTID                 
147700     ADD SEKEL-TAL TO ORQM-OBKR-TIREGDAT GIVING                           
147800                                     DATUM-MED-ARHUNDR                    
147900     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
148000                                     ORQM-OBKR-TITIREGD-9KOMPL            
148100     MOVE ORDP-RAD-TITPO            TO ORQM-OBKR-TITPO                    
148200     ADD SEKEL-TAL TO ORQM-OBKR-TIORDREG GIVING                           
148300                                     DATUM-MED-ARHUNDR                    
148400     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
148500                                     ORQM-OBKR-TITIORDD-9KOMPL            
148600     MOVE ORDP-RAD-KDFRAKT          TO ORQM-OBKR-KDFRAKT                  
148700     MOVE ORDP-RAD-KDORDKL          TO ORQM-OBKR-KDORDKL                  
148800     MOVE ORDP-RAD-DEAL-PR-LINE     TO ORQM-OBKR-DEAL-PR-LINE             
148900                                                                          
149000     MOVE ORQI-OHUV-KDORDTYP-LDC    TO ORQM-OBKR-KDORDTYP-LDC             
149100     MOVE ORQI-OHUV-TIREPDAT        TO ORQM-OBKR-TIREPDAT                 
149200     MOVE ORDP-RAD-IDKUNDRF-WIP     TO ORQM-OBKR-IDKUNDRF-WIP             
149310     MOVE ZERO                      TO ORQM-OBKR-TIDLEVDAT                
149320     MOVE ORDP-RAD-PRAVCOST         TO ORQM-OBKR-PRAVCOST                 
149400                                                                          
149500     PERFORM IMS-ISRT-ORQM                                                
149600     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
149700       ADD +1 TO ORQM-OBKR-IDLOPNR                                        
149800       PERFORM IMS-ISRT-ORQM                                              
149900     END-PERFORM                                                          
150000     .                                                                    
150100     EJECT                                                                
150200 S05-CONVERT-TISENBEK-DAG-IN SECTION.                                     
150300                                                                          
150400     MOVE 'AAVVD'                        TO DAT-KDDATFORM                 
150500     MOVE 2171-MID-TISENBEK-DAG-IN(INDX) TO DAT-I-TIDATUM                 
150600                                                                          
150700     CALL WDATKONV USING DAT-KDDATFORM                                    
150800                         DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR           
150900                                                                          
151000     IF DAT-KDSVAR-OK                                                     
151100        MOVE DAT-TIAAMMDD                TO W-TISENBEK-DAG                
151200     ELSE                                                                 
151300        MOVE ZERO                        TO W-TISENBEK-DAG                
151400     END-IF                                                               
151500     .                                                                    
151600     EJECT                                                                
151700 S06-UPPD-SUTPO-TOT SECTION.                                              
151800                                                                          
151900     PERFORM S09-LAES-ELLER-SKAPA-WDK901                                  
152000     ADD ORDP-RAD-KVART TO ARTM-ART-SUTPO-TOT                             
152100     PERFORM IMS-REPL-ARTM                                                
152200     .                                                                    
152300     EJECT                                                                
152400 S07-SKAPA-2204 SECTION.                                                  
152500                                                                          
152600     MOVE SPACE TO IO-AREA2                                               
152700                                                                          
152800     MOVE WC-CDC-SE  TO W-IDDC-2203                                       
152900     MOVE WS-IDARTNR TO XXBJ-2204-IDARTNR                                 
153000     MOVE +50        TO XXBJ-2204-KDLPORS                                 
153100     PERFORM IMS-ISRT-XXBJ-2204                                           
153200     .                                                                    
153300     EJECT                                                                
153400 S08-HAEMTA-KUNDENS-CL SECTION.                                           
153500                                                                          
153600     MOVE ORDP-RAD-IDDISTR TO W-IDDISTR-K                                 
153700     MOVE ORDP-RAD-IDKUNDNR TO W-IDKUNDNR-K                               
153800     .                                                                    
153900     EJECT                                                                
154000 S09-LAES-ELLER-SKAPA-WDK901 SECTION.                                     
154100                                                                          
154200     PERFORM IMS-GET-ARTM-ART                                             
154300     IF SEGMENT-SAKNAS                                                    
154400       MOVE WS-IDARTNR TO ARTM-IDARTNR-IN                                 
154500       CALL W411ARTM USING ARTM-W411ARTM ARTM-PCB                         
154600       PERFORM IMS-GET-ARTM-ART                                           
154700     END-IF                                                               
154800     .                                                                    
154900     EJECT                                                                
155000 S11-UPPD-SUTPO-PB-EJPB SECTION.                                          
155100                                                                          
155200     IF ANNAN-NYCKEL                                                      
155300       PERFORM S01-KONV-TITPO-1                                           
155400       IF DAT-KDSVAR-OK                                                   
155500         MOVE DAT-TIAAVV-GRP   TO WS-DAAAVV                               
155600         MOVE DAT-TISEKEL      TO WS-DASEKEL                              
155700         MOVE WS-DABEHOV       TO W-DABEHOV-A                             
155800       END-IF                                                             
155900     ELSE                                                                 
156000       MOVE MID-TITPO-IN(INDX) TO WS-TIBEHOV                              
156100       MOVE WS-TIAAVV          TO W-DABEHOV-A                             
156200       IF WS-TIAAVV NOT = ZERO                                            
156300         IF WS-TIAAVV < 5000                                              
156400           MOVE 20             TO W-DABEHOV-A (1:2)                       
156500         ELSE                                                             
156600           IF WS-TIAAVV < 9999                                            
156700             MOVE 19           TO W-DABEHOV-A (1:2)                       
156800           ELSE                                                           
156900             MOVE 9999         TO W-DABEHOV-A                             
157000           END-IF                                                         
157100         END-IF                                                           
157200       END-IF                                                             
157300     END-IF                                                               
157400     PERFORM IMS-GET-ARTM-ANT                                             
157500     IF SEGMENT-FINNS                                                     
157600       IF ORDP-RAD-KDTPOTYP = 2                                           
157700         ADD ORDP-RAD-KVART TO ARTM-ANT-SUTPO-PB                          
157800       ELSE                                                               
157900         ADD ORDP-RAD-KVART TO ARTM-ANT-SUTPO-EJPB                        
158000       END-IF                                                             
158100       PERFORM IMS-REPL-ARTM                                              
158200     ELSE                                                                 
158300       MOVE W-DABEHOV-A    TO ARTM-ANT-DABEHOV                            
158400       IF ORDP-RAD-KDTPOTYP = 2                                           
158500         MOVE ORDP-RAD-KVART TO ARTM-ANT-SUTPO-PB                         
158600         MOVE ZERO           TO ARTM-ANT-SUTPO-EJPB                       
158700       ELSE                                                               
158800         MOVE ORDP-RAD-KVART TO ARTM-ANT-SUTPO-EJPB                       
158900         MOVE ZERO           TO ARTM-ANT-SUTPO-PB                         
159000       END-IF                                                             
159100       PERFORM IMS-ISRT-ARTM-ANT                                          
159200     END-IF                                                               
159300     .                                                                    
159400     EJECT                                                                
159500 S12-GRUNDRAD-TILL-MOD SECTION.                                           
159600                                                                          
159700* OM RADEN INTE HAR LEVERANTÖR HÄMTAS DEN FRÅN WDK601                     
159800* (DVS OM DET INTE ÄR GJORT TIDIGARE)                                     
159900     IF ORDP-RAD-IDLEVNR NOT = SPACE                                      
160000       MOVE ORDP-RAD-IDLEVNR    TO MOD-IDLEVNR                            
160100     ELSE                                                                 
160200       IF WS-IDLEVNR NOT = SPACE                                          
160300         MOVE WS-IDLEVNR        TO MOD-IDLEVNR                            
160400       ELSE                                                               
160500         PERFORM IMS-GU-ARTC-ART                                          
160600         IF SEGMENT-FINNS                                                 
160700           MOVE ART-IDLEVNR    TO MOD-IDLEVNR                             
160800         END-IF                                                           
160900       END-IF                                                             
161000     END-IF                                                               
161100     MOVE ORDP-RAD-IDANSK       TO MOD-IDANSK                             
161200     MOVE ORDP-RAD-IDARTNR      TO MOD-IDARTNR                            
161300     MOVE ORDP-RAD-IDDISTR      TO MOD-IDDISTR                            
161400     MOVE ORDP-RAD-IDKUNDNR     TO MOD-IDKUNDNR                           
161500     MOVE 1                     TO MOD-KDCLAGER                           
161600     IF ORDP-RAD-IDORDNR7 NUMERIC                                         
161700       MOVE ORDP-RAD-IDORDNR7   TO MOD-IDORDNR7                           
161800     ELSE                                                                 
161900       MOVE ORDP-RAD-IDORDNR5   TO MOD-IDORDNR7                           
162000     END-IF                                                               
162100     MOVE ORDP-RAD-KVART        TO MOD-KVART                              
162200     MOVE ORDP-RAD-KDTPOTYP     TO MOD-KDTPOTYP                           
162300     PERFORM S01-KONV-TITPO-1                                             
162400     IF DAT-KDSVAR-OK                                                     
162500       MOVE DAT-TIAAVVD         TO MOD-TITPO                              
162600     END-IF                                                               
162700     .                                                                    
162800     SKIP3                                                                
162900 S13-SPLITRAD-TILL-MOD SECTION.                                           
163000                                                                          
163100     MOVE ORDP-RAD-KVART        TO MOD-KVART-UT(MOD-IX)                   
163200     PERFORM S01-KONV-TITPO-1                                             
163300     IF DAT-KDSVAR-OK                                                     
163400       MOVE DAT-TIAAVVD         TO MOD-TITPO-UT(MOD-IX)                   
163500     END-IF                                                               
163600     .                                                                    
163700     EJECT                                                                
163800 S14-DLET-LARM SECTION.                                                   
163900                                                                          
164000     PERFORM IMS-GET-XXBU-2223                                            
164100     IF SEGMENT-FINNS                                                     
164200       MOVE WC-CDC-SE           TO W-IDDC                                 
164300       PERFORM IMS-GET-XXBU-2224                                          
164400       IF SEGMENT-FINNS                                                   
164500         PERFORM IMS-DLET-XXBU                                            
164600       END-IF                                                             
164700     END-IF                                                               
164800     .                                                                    
164900     EJECT                                                                
165000 S15-SKAPA-RY4-TRANS SECTION.                                             
165100                                                                          
165200     MOVE SPACE                   TO ZZAC-LOGGPOST                        
165300                                                                          
165400     MOVE 'RY4'                   TO RY4-IDPTYP                           
165500     MOVE ORDP-RAD-IDDC           TO RY4-IDDC                             
165600     MOVE ORDP-RAD-IDDISTR        TO RY4-IDDISTR                          
165700     MOVE ORDP-RAD-IDKUNDNR       TO RY4-IDKUNDNR                         
165800     MOVE ORDP-RAD-IDKUNDRF (1:5) TO RY4-IDRONR                           
165900     MOVE ORDP-RAD-IDARTNR        TO RY4-IDARTNR                          
166000     MOVE ORDP-RAD-KVART          TO RY4-KVRO                             
166100     MOVE ORDP-RAD-KDORDKL        TO RY4-KDORDKL                          
166200     MOVE ORDP-RAD-KDFAKTYP       TO RY4-KDFAKTYP                         
166300     MOVE ORDP-RAD-KDVRINFO       TO RY4-KDVRINFO                         
166400                                                                          
166500     MOVE DAGENS-DATUM       TO ZZAC-TIAAMMDD                             
166600     MOVE AKTUELL-TID        TO ZZAC-TIKLOCK                              
166700     ADD +100                TO ZZAC-TIKLOCK                              
166800     MOVE  +1                TO ZZAC-IDLOGLOP                             
166900     MOVE 'RY4'              TO ZZAC-IDPTYP                               
167000     MOVE RY4-W440300        TO ZZAC-LOGGPOST                             
167100                                                                          
167200     PERFORM IMS-ISRT-ZZAC                                                
167300     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
167400       ADD +1 TO ZZAC-IDLOGLOP                                            
167500       PERFORM IMS-ISRT-ZZAC                                              
167600     END-PERFORM                                                          
167700     .                                                                    
167800     EJECT                                                                
167900 S16-SKAPA-RY9-TRANS SECTION.                                             
168000     MOVE ORDP-RAD-IDDISTR   TO W-IDDISTR-GMTA                            
168100     MOVE ORDP-RAD-IDKUNDNR  TO W-IDKUNDNR-GMTA                           
168200     PERFORM IMS-GET-GMTA                                                 
168300     MOVE GMTA-GMT-FLVR      TO WS-FLVR                                   
168400     MOVE NEJ                TO WS-FLVR                                   
168500     MOVE GMTA-GMT-FLNC      TO WS-FLNC                                   
168600     MOVE SPACE              TO ZZAC-LOGGPOST                             
168700                                                                          
168800     MOVE 'RY9'              TO RY9-IDPTYP                                
168900     MOVE ORDP-RAD-BERADREF  TO RY9-BERADREF                              
169000     MOVE ORDP-RAD-BEVOLREF  TO RY9-BEVOLREF                              
169100     MOVE ORDP-RAD-FLERS     TO RY9-FLERS                                 
169200     MOVE WS-FLNC            TO RY9-FLNC                                  
169300     MOVE ORDP-RAD-IDARTNR   TO RY9-IDARTNR                               
169400     MOVE ZERO               TO RY9-IDDIVORD                              
169500     MOVE ORDP-RAD-IDKUNDRF  TO RY9-IDKUNDRF                              
169600     MOVE ORDP-RAD-IDLOPNR   TO RY9-IDLOPNR                               
169700     MOVE MSG-LTERM-NAME     TO RY9-IDUSER                                
169800     MOVE ORDP-RAD-KDFAKTYP  TO RY9-KDFAKTYP                              
169900     MOVE ORDP-RAD-KDKVBRYT  TO RY9-KDKVBRYT                              
170000     MOVE ORDP-RAD-KDORDKL   TO RY9-KDORDKL                               
170100     MOVE ORDP-RAD-KVART     TO RY9-KVART                                 
170200     MOVE ORDP-RAD-KDDSP     TO RY9-KDDSP                                 
170300     MOVE ORDP-RAD-KDRAPRIO  TO RY9-KDRAPRIO                              
170400     MOVE ORDP-RAD-KDSTARAD  TO RY9-KDSTARAD                              
170500     MOVE ORDP-RAD-KDTPOTYP  TO RY9-KDTPOTYP                              
170600     MOVE ORDP-RAD-KDVRINFO  TO RY9-KDVRINFO                              
170700     IF ORDP-RAD-KDTPOTYP = 1                                             
170800       IF ORDP-RAD-IDSYSTEM = 'VR'                                        
170900         MOVE +1             TO RY9-KDVRTPO                               
171000       ELSE                                                               
171100         MOVE +2             TO RY9-KDVRTPO                               
171200       END-IF                                                             
171300     ELSE                                                                 
171400       MOVE ZERO             TO RY9-KDVRTPO                               
171500     END-IF                                                               
171600     MOVE ORDP-RAD-PRARTNTO  TO RY9-PRARTNTO                              
172000     MOVE ORDP-RAD-TIREGDAT  TO RY9-TIREGDAT                              
172100     MOVE ORDP-RAD-TIRES     TO RY9-TIRES                                 
172200     MOVE ORDP-RAD-TITPO     TO RY9-TITPO                                 
172300     MOVE ORDP-RAD-DARODAT (3:6) TO RY9-TIRODAT                           
172400     MOVE WS-FLVR            TO RY9-FLVR                                  
172500     MOVE RY9-WDGZRY9        TO ZZAC-LOGGPOST                             
172600                                                                          
172700     MOVE SPACE              TO ZZAC-SORTPOST                             
172800     MOVE ORDP-RAD-IDDISTR   TO RY9S-IDDISTR                              
172900     MOVE ORDP-RAD-IDKUNDNR  TO RY9S-IDKUNDNR                             
173000     MOVE WC-CDC-SE          TO RY9S-IDDC                                 
173100     MOVE ORDP-RAD-KDFRAKT   TO RY9S-KDFRAKT                              
173200     MOVE ORDP-RAD-KDORDKL   TO RY9S-KDORDKL                              
173300     MOVE 71                 TO RY9S-KDORDBEK                             
173400     MOVE WS-IDORDER         TO RY9S-IDORDER                              
173500     MOVE WDGZRY9S           TO ZZAC-SORTPOST                             
173600                                                                          
173700     MOVE DAGENS-DATUM       TO ZZAC-TIAAMMDD                             
173800     MOVE AKTUELL-TID        TO ZZAC-TIKLOCK                              
173900     MOVE  +1                TO ZZAC-IDLOGLOP                             
174000     MOVE 'RY9'              TO ZZAC-IDPTYP                               
174100                                                                          
174200     PERFORM IMS-ISRT-ZZAC                                                
174300     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
174400       ADD +1 TO ZZAC-IDLOGLOP                                            
174500       PERFORM IMS-ISRT-ZZAC                                              
174600     END-PERFORM                                                          
174700     .                                                                    
174800     EJECT                                                                
174900 S18-PRISTILLAEMPA SECTION.                                               
175000                                                                          
175100     MOVE 1                   TO PRIS-KDCALL                              
175110     MOVE 'W2012600'          TO PRIS-IDPGM                               
175200     MOVE ORDP-RAD-IDARTNR    TO PRIS-IDARTNR                             
175300     MOVE ORDP-RAD-IDDISTR    TO PRIS-IDDISTR                             
175400                                 TEST-IDDISTR                             
175500     MOVE ORDP-RAD-IDKUNDNR   TO PRIS-IDKUNDNR                            
175600     MOVE WC-CDC-SE           TO PRIS-IDDC                                
175700     MOVE ORDP-RAD-KDORDKL    TO PRIS-KDORDKL                             
175800     MOVE ORDP-RAD-KVART      TO PRIS-KVBEART                             
175900     MOVE ORDP-RAD-FLINVEST   TO PRIS-FLINVEST                            
176000                                                                          
176100     CALL W335PRIS USING PRIS-AREA ARTC-PCB PRIS-WDK7-PCB                 
176200                                   GMTA-PCB                               
176300                                   BETA-PCB PRIA-PCB PRIB-PCB             
176400                                   PRIS-COST-WDK6-PCB                     
176500                                   PRIS-COST-WDK7-PCB                     
176600                                   PRIS-COST-WDF1-PCB                     
176700                                   PRIS-COST-9305-PCB                     
176800                                   PRIS-COST-WDK72-PCB                    
176900                                   PRIS-COST-WDB6-PCB                     
177100     IF PRIS-KDSVAR = SPACE                                               
177200       MOVE PRIS-PRARTNTO     TO ORDP-RAD-PRARTNTO                        
177300       MOVE PRIS-FLPRTILL     TO ORDP-RAD-FLPRTILL                        
177400       MOVE PRIS-KDPRTYP      TO ORDP-RAD-KDPRTYP                         
178000       MOVE PRIS-KDVALISO     TO ORDP-RAD-KDVALISO                        
178200       MOVE PRIS-PRAVCOST     TO ORDP-RAD-PRAVCOST                        
178300     ELSE                                                                 
178400       MOVE 'FEL RETURKOD FRÅN W335PRIS' TO FELTEXT-STR                   
178500       DISPLAY FELTEXT                                                    
178600       CALL FELLOG                                                        
178700     END-IF                                                               
178800     .                                                                    
178900     EJECT                                                                
179000 S20-KOMPLETTERA-PRIS  SECTION.                                           
179100                                                                          
179200     IF DIST79-DEALER-PRICE                                               
179300      IF ORDP-RAD-PRARTNTO-LOC = ZERO AND                                 
179400         ORDP-RAD-PRARTNTO-LOCPREL = ZERO                                 
179500       IF ORDP-RAD-IDPRQUES > ZERO                                        
179600*         TA BORT GAMLA PRISFRÅGAN                                        
179700         INITIALIZE PRQU-W335PRQU                                         
179800         MOVE ORDP-RAD-IDDISTR       TO PRQU-IDDISTR                      
179900         MOVE ORDP-RAD-IDKUNDNR      TO PRQU-IDKUNDNR                     
180000         MOVE ORDP-RAD-IDKUNDRF(1:5) TO PRQU-IDKUNDRF(3:5)                
180100         MOVE '00'                   TO PRQU-IDKUNDRF(1:2)                
180200         MOVE ORDP-RAD-IDPRQUES      TO PRQU-IDPRQUES                     
180300         MOVE 4                      TO PRQU-KDCALL                       
180400         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
180500                                            PRQU-WDC7-PCB                 
180600                                            PRQU-SJKO-WDK6-PCB            
180700                                                                          
180800       END-IF                                                             
180900       IF WS-IDPRQUES                = +0                                 
181000          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
181100          MOVE +1                    TO PRNO-KDCALL                       
181200                                                                          
181300          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
181400                                                                          
181500          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
181600                                        WS-IDPRQUES                       
181700          MOVE +1                    TO PRQU-KDCALL                       
181800       ELSE                                                               
181900          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
182000          MOVE +2                    TO PRNO-KDCALL                       
182100                                                                          
182200          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
182300                                                                          
182400          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
182500                                      WS-IDPRQUES                         
182600          MOVE +2                    TO PRQU-KDCALL                       
182700       END-IF                                                             
182800                                                                          
182900*         LÄGG IN NY PRISFRÅGA                                            
183000       MOVE ORDP-RAD-IDDISTR         TO PRQU-IDDISTR                      
183100                                        W-IDDISTR-GMTA                    
183200       MOVE ORDP-RAD-IDKUNDNR        TO PRQU-IDKUNDNR                     
183300                                        W-IDKUNDNR-GMTA                   
183400       MOVE ORDP-RAD-IDKUNDRF(1:5)   TO PRQU-IDKUNDRF(3:5)                
183500       MOVE '00'                     TO PRQU-IDKUNDRF(1:2)                
183600       MOVE ZERO                     TO PRQU-IDORDER                      
183700       MOVE ORDP-RAD-KDORDKL         TO PRQU-KDORDKL                      
183800       MOVE 'N'                      TO PRQU-KDPRSTA                      
183900       MOVE ORDP-RAD-IDARTNR         TO PRQU-IDARTNR                      
184000       MOVE ORDP-RAD-KVBEART-Q       TO PRQU-KVBEART-Q                    
184100       MOVE ORDP-RAD-KDVALISO        TO PRQU-KDVALISO                     
184200       MOVE ORDP-RAD-PRARTNTO-LOC    TO PRQU-PRARTNTO-LOC                 
184300       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
184400       MOVE ORDP-RAD-IDSYSTEM        TO PRQU-IDSYSTEM                     
184500                                                                          
184600       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
184700                                          PRQU-WDC7-PCB                   
184800                                          PRQU-SJKO-WDK6-PCB              
184900                                                                          
185000       MOVE PRQU-IDPRQUES            TO  ORDP-RAD-IDPRQUES                
185100                                         WS-IDPRQUES                      
185200       MOVE PRQU-FLPRTILL            TO  ORDP-RAD-FLPRTILL                
185300                                                                          
185400       IF ORDP-RAD-PRARTNTO-LOC = +0                                      
185500          MOVE PRQU-PRARTNTO-LOCPREL TO                                   
185600                     ORDP-RAD-PRARTNTO-LOCPREL                            
185700       END-IF                                                             
185800                                                                          
185900       IF ORDP-RAD-PRARTNTO-LOC NOT = +0                                  
186000         IF ORDP-RAD-KDPRTYP = SPACE                                      
186100           MOVE 'P'                TO ORDP-RAD-KDPRTYP                    
186200*          MOVE ORDP-RAD-TIREGDAT  TO ORDP-RAD-TIPRIS                     
186300         END-IF                                                           
186400       END-IF                                                             
186500       PERFORM S21-SKICKA-PRISFRAGA                                       
186600      END-IF                                                              
186700     END-IF                                                               
186800     .                                                                    
186900     EJECT                                                                
187000 S21-SKICKA-PRISFRAGA  SECTION.                                           
187100                                                                          
187200     MOVE 1                          TO 3039-REQU-IDMSGVER                
187300     MOVE SPACE                      TO 3039-REQU-KDPGMACT                
187400     MOVE 'W2012600'                 TO 3039-REQU-IDUSER                  
187500                                                                          
187600     MOVE SPACE                      TO 3039-MID-IDBUNDLE                 
187700     MOVE ORDP-RAD-IDDISTR           TO 3039-MID-IDDISTR                  
187800     MOVE ORDP-RAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                 
187900     MOVE ORDP-RAD-IDORDNR7          TO 3039-MID-IDBUNDLE                 
188000     MOVE WS-IDPRQUES                TO 3039-MID-IDPRQUES                 
188100                                                                          
188200     PERFORM S24-SKICKA-OPEN                                              
188300     PERFORM S24-SKICKA-MEDDELANDE                                        
188400     PERFORM S24-SKICKA-CLOSE                                             
188500                                                                          
188600     .                                                                    
188700     EJECT                                                                
188800 S24-SKICKA-OPEN SECTION.                                                 
188900                                                                          
189000     MOVE 'OPEN'                     TO SEND-KDFUNC                       
189100     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
189200     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
189300                                                                          
189400     IF SEND-KDRC > 0                                                     
189500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
189600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
189700       DELIMITED BY SIZE INTO FELTEXT                                     
189800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
189900     END-IF                                                               
190000     .                                                                    
190100     SKIP3                                                                
190200 S24-SKICKA-MEDDELANDE  SECTION.                                          
190300                                                                          
190400     MOVE 'PUT'                      TO SEND-KDFUNC                       
190500     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
190600     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
190700                                                                          
190800     IF SEND-KDRC > 0                                                     
190900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
191000       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
191100       DELIMITED BY SIZE INTO FELTEXT                                     
191200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
191300     END-IF                                                               
191400     .                                                                    
191500     SKIP3                                                                
191600 S24-SKICKA-CLOSE SECTION.                                                
191700                                                                          
191800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
191900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
192000                                                                          
192100     IF SEND-KDRC > 0                                                     
192200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
192300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
192400       DELIMITED BY SIZE INTO FELTEXT                                     
192500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
192600     END-IF                                                               
192700     .                                                                    
192800     EJECT                                                                
192900                                                                          
193000 MFS-RENSA-FAELT-UT SECTION.                                              
193100                                                                          
193200*    --- ALLA UTDATA-FÄLT                                                 
193300*    --- INKL. BLÄDDRINGSNYCKLAR                                          
193400     MOVE MFS-RENSA-FAELT TO MOD-TISENBEK-DAG-ENTER                       
193500                             MOD-TISENBEK-KL-ENTER                        
193600                             MOD-TISENBEK-DAG-NEXT                        
193700                             MOD-TISENBEK-KL-NEXT                         
193800                             MOD-KDLARM-ENTER                             
193900                             MOD-KDLARM-NEXT                              
194000                             MOD-IDANSK                                   
194100                             MOD-IDLEVNR                                  
194200                             MOD-IDARTNR                                  
194300                             MOD-IDDISTR                                  
194400                             MOD-IDKUNDNR                                 
194500                             MOD-KDCLAGER                                 
194600                             MOD-IDORDNR7                                 
194700                             MOD-KVART                                    
194800                             MOD-TITPO                                    
194900                             MOD-KDTPOTYP                                 
195000     MOVE +1 TO INDX                                                      
195100     PERFORM UNTIL INDX > MAX-INDX                                        
195200       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
195300       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
195400       ADD +1 TO INDX                                                     
195500     END-PERFORM                                                          
195600     .                                                                    
195700     SKIP2                                                                
195800 MFS-RENSA-RAD-FAELT SECTION.                                             
195900                                                                          
196000     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR                                  
196100                             MOD-IDARTNR                                  
196200                             MOD-IDDISTR                                  
196300                             MOD-IDKUNDNR                                 
196400                             MOD-KDCLAGER                                 
196500                             MOD-IDORDNR7                                 
196600                             MOD-KVART                                    
196700                             MOD-TITPO                                    
196800                             MOD-KDTPOTYP                                 
196900     MOVE +1 TO INDX                                                      
197000     PERFORM UNTIL INDX > MAX-INDX                                        
197100       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
197200       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
197300       ADD +1 TO INDX                                                     
197400     END-PERFORM                                                          
197500     .                                                                    
197600     SKIP2                                                                
197700 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
197800                                                                          
197900*    --- UTDATA-FÄLT PÅ VISNINGSRADER                                     
198000     MOVE MFS-RENSA-FAELT TO MOD-KVART-UT (INDX)                          
198100                             MOD-TITPO-UT (INDX)                          
198200     .                                                                    
198300     SKIP2                                                                
198400 MFS-RENSA-FAELT-IN SECTION.                                              
198500                                                                          
198600     MOVE +1 TO INDX                                                      
198700     PERFORM UNTIL INDX > MAX-INDX                                        
198800       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
198900       ADD +1 TO INDX                                                     
199000     END-PERFORM                                                          
199100     .                                                                    
199200     SKIP2                                                                
199300 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
199400                                                                          
199500*    --- ALLA INDATA-FÄLT                                                 
199600     MOVE MFS-RENSA-FAELT TO MOD-KVART-IN(INDX)                           
199700                             MOD-TITPO-IN(INDX)                           
199800     .                                                                    
199900     EJECT                                                                
200000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
200100                                                                          
200200*    --- ALLA UTDATA-FÄLT                                                 
200300*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
200400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSK-DOLD                            
200500                               MOD-TISENBEK-DAG-ENTER                     
200600                               MOD-TISENBEK-KL-ENTER                      
200700                               MOD-KDLARM-ENTER                           
200800                               MOD-TISENBEK-DAG-NEXT                      
200900                               MOD-TISENBEK-KL-NEXT                       
201000                               MOD-KDLARM-NEXT                            
201100                               MOD-IDANSK                                 
201200                               MOD-IDLEVNR                                
201300                               MOD-IDARTNR                                
201400                               MOD-IDDISTR                                
201500                               MOD-IDKUNDNR                               
201600                               MOD-KDCLAGER                               
201700                               MOD-IDORDNR7                               
201800                               MOD-KVART                                  
201900                               MOD-TITPO                                  
202000                               MOD-KDTPOTYP                               
202100                                                                          
202200     MOVE +1 TO INDX                                                      
202300     PERFORM UNTIL INDX > MAX-INDX                                        
202400       MOVE MFS-ROER-EJ-FAELT TO MOD-KVART-UT (INDX)                      
202500                                 MOD-TITPO-UT (INDX)                      
202600       ADD +1 TO INDX                                                     
202700     END-PERFORM                                                          
202800     .                                                                    
202900     SKIP2                                                                
203000 MFS-ROER-EJ-FAELT-IN SECTION.                                            
203100                                                                          
203200     MOVE +1 TO INDX                                                      
203300     PERFORM UNTIL INDX > MAX-INDX                                        
203400       IF MID-KVART-IN(INDX) = ALL '+'                                    
203500         MOVE MFS-RENSA-FAELT   TO MOD-KVART-IN(INDX)                     
203600       ELSE                                                               
203700         MOVE MFS-ROER-EJ-FAELT TO MOD-KVART-IN(INDX)                     
203800       END-IF                                                             
203900       IF MID-TITPO-IN(INDX) = ALL '+'                                    
204000         MOVE MFS-RENSA-FAELT   TO MOD-TITPO-IN(INDX)                     
204100       ELSE                                                               
204200         MOVE MFS-ROER-EJ-FAELT TO MOD-TITPO-IN(INDX)                     
204300       END-IF                                                             
204400       ADD +1 TO INDX                                                     
204500     END-PERFORM                                                          
204600     .                                                                    
204700     EJECT                                                                
204800 MFS-LAS-IN-IGEN SECTION.                                                 
204900                                                                          
205000*    --- ALLA INDATA-FÄLT                                                 
205100     MOVE +1 TO INDX                                                      
205200     PERFORM UNTIL INDX > MAX-INDX                                        
205300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVART-IN-ATTR(INDX)              
205400                                     MOD-TITPO-IN-ATTR(INDX)              
205500       ADD +1 TO INDX                                                     
205600     END-PERFORM                                                          
205700     .                                                                    
205800     EJECT                                                                
205900* --  IMS-SEKTIONER ---                                                   
206000                                                                          
206100 IMS-GET-MSG SECTION.                                                     
206200                                                                          
206300     MOVE '  QC' TO GODK-STATUSKODER                                      
206400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
206500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
206600     PERFORM IMS-STATUSKONTROLL                                           
206700     .                                                                    
206800     SKIP3                                                                
206900 IMS-INSERT-MSG SECTION.                                                  
207000                                                                          
207100     IF ENGLISH-TEXT                                                      
207200       MOVE 'N' TO MFS-KDHUVOMR                                           
207300     END-IF                                                               
207400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
207500     MOVE SPACE TO GODK-STATUSKODER                                       
207600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
207700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
207800     PERFORM IMS-STATUSKONTROLL                                           
207900     .                                                                    
208000                                                                          
208100     EJECT                                                                
208200 IMS-GU-ARTC-ART  SECTION.                                                
208300                                                                          
208400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
208500          DELIMITED BY SIZE INTO SSA1                                     
208600     MOVE '  GE' TO GODK-STATUSKODER                                      
208700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
208800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
208900     PERFORM IMS-STATUSKONTROLL                                           
209000     .                                                                    
209100                                                                          
209200                                                                          
209300 IMS-GET-ARTC-CLAG SECTION.                                               
209400                                                                          
209500     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
209600          DELIMITED BY SIZE INTO SSA1                                     
209700     MOVE '  GE' TO GODK-STATUSKODER                                      
209800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
209900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
210000     PERFORM IMS-STATUSKONTROLL                                           
210100     .                                                                    
210200                                                                          
210300 IMS-GU-ARTC-CLAG SECTION.                                                
210400                                                                          
210500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
210600          DELIMITED BY SIZE INTO SSA1                                     
210700     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
210800          DELIMITED BY SIZE INTO SSA2                                     
210900     MOVE '  GE' TO GODK-STATUSKODER                                      
211000     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA-11 SSA1 SSA2             
211100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
211200     PERFORM IMS-STATUSKONTROLL                                           
211300     .                                                                    
211400                                                                          
211500     EJECT                                                                
211600 IMS-GET-ARTM-ART  SECTION.                                               
211700                                                                          
211800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
211900          DELIMITED BY SIZE INTO SSA1                                     
212000     MOVE '  GE' TO GODK-STATUSKODER                                      
212100     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA2 SSA1                    
212200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
212300     PERFORM IMS-STATUSKONTROLL                                           
212400     .                                                                    
212500                                                                          
212600                                                                          
212700 IMS-GET-ARTM-ANT  SECTION.                                               
212800                                                                          
212900     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
213000          DELIMITED BY SIZE INTO SSA1                                     
213100     MOVE '  GE' TO GODK-STATUSKODER                                      
213200     CALL CBLTDLI USING GHNP ARTM-PCB DLI-IO-AREA2 SSA1                   
213300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
213400     PERFORM IMS-STATUSKONTROLL                                           
213500     .                                                                    
213600                                                                          
213700 IMS-ISRT-ARTM-ANT SECTION.                                               
213800                                                                          
213900     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
214000          DELIMITED BY SIZE INTO SSA1                                     
214100     MOVE 'WLARTM11 ' TO SSA2                                             
214200     MOVE '  ' TO GODK-STATUSKODER                                        
214300     CALL CBLTDLI USING ISRT ARTM-PCB DLI-IO-AREA2 SSA1 SSA2              
214400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
214500     PERFORM IMS-STATUSKONTROLL                                           
214600     .                                                                    
214700                                                                          
214800 IMS-REPL-ARTM SECTION.                                                   
214900                                                                          
215000     MOVE '  ' TO GODK-STATUSKODER                                        
215100     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA2                        
215200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
215300     PERFORM IMS-STATUSKONTROLL                                           
215400     .                                                                    
215500     EJECT                                                                
215600 IMS-GET-GMTA      SECTION.                                               
215700                                                                          
215800     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
215900          DELIMITED BY SIZE INTO SSA1                                     
216000     MOVE '  ' TO GODK-STATUSKODER                                        
216100     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-B201 SSA1                      
216200     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
216300     PERFORM IMS-STATUSKONTROLL                                           
216400     .                                                                    
216500                                                                          
216600 IMS-GET-ORDP-RAD  SECTION.                                               
216700                                                                          
216800     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
216900          DELIMITED BY SIZE INTO SSA1                                     
217000     MOVE '  ' TO GODK-STATUSKODER                                        
217100     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA SSA1                     
217200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
217300     PERFORM IMS-STATUSKONTROLL                                           
217400     .                                                                    
217500                                                                          
217600 IMS-GET-ORDP-RAD-GE SECTION.                                             
217700                                                                          
217800     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
217900          DELIMITED BY SIZE INTO SSA1                                     
218000     MOVE '  GE' TO GODK-STATUSKODER                                      
218100     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA SSA1                     
218200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
218300     PERFORM IMS-STATUSKONTROLL                                           
218400     .                                                                    
218500                                                                          
218600 IMS-ISRT-ORDP-RAD SECTION.                                               
218700                                                                          
218800     MOVE 'WLORDP01 ' TO SSA1                                             
218900     MOVE '  II' TO GODK-STATUSKODER                                      
219000     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA SSA1                    
219100     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
219200     PERFORM IMS-STATUSKONTROLL                                           
219300     .                                                                    
219400                                                                          
219500 IMS-REPL-ORDP SECTION.                                                   
219600                                                                          
219700     MOVE '  ' TO GODK-STATUSKODER                                        
219800     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-AREA                         
219900     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
220000     PERFORM IMS-STATUSKONTROLL                                           
220100     .                                                                    
220200                                                                          
220300 IMS-DLET-ORDP SECTION.                                                   
220400                                                                          
220500     MOVE '  ' TO GODK-STATUSKODER                                        
220600     CALL CBLTDLI USING DLET ORDP-PCB DLI-IO-AREA                         
220700     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
220800     PERFORM IMS-STATUSKONTROLL                                           
220900     .                                                                    
221000     EJECT                                                                
221100 IMS-ISRT-ORQM    SECTION.                                                
221200                                                                          
221300     MOVE 'WLORQM01 ' TO SSA1                                             
221400     MOVE '  II' TO GODK-STATUSKODER                                      
221500     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA2 SSA1                   
221600     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
221700     PERFORM IMS-STATUSKONTROLL                                           
221800     .                                                                    
221900                                                                          
222000 IMS-GET-ORQI-CSEQ SECTION.                                               
222100                                                                          
222200     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
222300          DELIMITED BY SIZE INTO SSA1                                     
222400     MOVE '  ' TO GODK-STATUSKODER                                        
222500     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA2 SSA1                     
222600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
222700     PERFORM IMS-STATUSKONTROLL                                           
222800     .                                                                    
222900                                                                          
223000                                                                          
223100 IMS-GET-XXBU-2223 SECTION.                                               
223200                                                                          
223300     STRING 'WLXXBU01(WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
223400          DELIMITED BY SIZE INTO SSA1                                     
223500     MOVE '  GE' TO GODK-STATUSKODER                                      
223600     CALL CBLTDLI USING GU XXBU-PCB DLI-IO-AREA2 SSA1                     
223700     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
223800     PERFORM IMS-STATUSKONTROLL                                           
223900     .                                                                    
224000                                                                          
224100                                                                          
224200 IMS-GET-XXBU-2224 SECTION.                                               
224300                                                                          
224400     STRING 'WLXXBU11*F(WDGXKEY  =' W-WDGXKEY-2224-X                      
224500                      '&IDDC     =' W-IDDC-X ')'                          
224600          DELIMITED BY SIZE INTO SSA1                                     
224700     MOVE '  GE' TO GODK-STATUSKODER                                      
224800     CALL CBLTDLI USING GHNP XXBU-PCB DLI-IO-AREA2 SSA1                   
224900     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
225000     PERFORM IMS-STATUSKONTROLL                                           
225100     .                                                                    
225200                                                                          
225300 IMS-GET-XXBU-2224-IDART SECTION.                                         
225400                                                                          
225500     STRING 'WLXXBU11(IDARTNR  =' W-IDARTNR-X                             
225600                    '&IDDC     =' W-IDDC-X                                
225700                    '&KDLARM  >=' W-KDLARM-MIN-X                          
225800                    '&KDLARM  <=' W-KDLARM-MAX-X ')'                      
225900          DELIMITED BY SIZE INTO SSA1                                     
226000     MOVE '  GE' TO GODK-STATUSKODER                                      
226100     CALL CBLTDLI USING GNP XXBU-PCB DLI-IO-AREA2 SSA1                    
226200     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
226300     PERFORM IMS-STATUSKONTROLL                                           
226400     .                                                                    
226500                                                                          
226600 IMS-DLET-XXBU SECTION.                                                   
226700                                                                          
226800     MOVE '  ' TO GODK-STATUSKODER                                        
226900     CALL CBLTDLI USING DLET XXBU-PCB DLI-IO-AREA2                        
227000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
227100     PERFORM IMS-STATUSKONTROLL                                           
227200     .                                                                    
227300                                                                          
227400 IMS-GET-XXBX-2231 SECTION.                                               
227500                                                                          
227600     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
227700          DELIMITED BY SIZE INTO SSA1                                     
227800     MOVE '  ' TO GODK-STATUSKODER                                        
227900     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA2 SSA1                     
228000     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
228100     PERFORM IMS-STATUSKONTROLL                                           
228200     .                                                                    
228300                                                                          
228400                                                                          
228500 IMS-GET-XXBX-2232 SECTION.                                               
228600                                                                          
228700     STRING 'WLXXBX11(WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
228800          DELIMITED BY SIZE INTO SSA1                                     
228900     MOVE '  GE' TO GODK-STATUSKODER                                      
229000     CALL CBLTDLI USING GNP XXBX-PCB DLI-IO-AREA2 SSA1                    
229100     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
229200     PERFORM IMS-STATUSKONTROLL                                           
229300     .                                                                    
229400                                                                          
229500 IMS-ISRT-XXBJ-2204 SECTION.                                              
229600                                                                          
229700     STRING 'WLXXBJ01(WDG3KEY  =' W-WDGXKEY-2203-X ')'                    
229800          DELIMITED BY SIZE INTO SSA1                                     
229900     MOVE 'WLXXBJ11 ' TO SSA2                                             
230000     MOVE '  ' TO GODK-STATUSKODER                                        
230100     CALL CBLTDLI USING ISRT XXBJ-PCB DLI-IO-AREA2 SSA1 SSA2              
230200     MOVE XXBJ-STATUS-CODE TO STATUS-WS                                   
230300     PERFORM IMS-STATUSKONTROLL                                           
230400     .                                                                    
230500                                                                          
230600 IMS-ISRT-ZZAC   SECTION.                                                 
230700                                                                          
230800     MOVE 'WLZZAC01 ' TO SSA1                                             
230900     MOVE '  II' TO GODK-STATUSKODER                                      
231000     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA2 SSA1                   
231100     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
231200     PERFORM IMS-STATUSKONTROLL                                           
231300     .                                                                    
231400     EJECT                                                                
231500 IMS-STATUSKONTROLL SECTION.                                              
231600                                                                          
231700     SET STATUS-IX TO 1                                                   
231800     SEARCH GODK-STATUS                                                   
231900       AT END                                                             
232000         CALL FELLOG                                                      
232100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
232200         CONTINUE                                                         
232300     END-SEARCH                                                           
232400     .                                                                    
232500     EJECT                                                                
232600*    -COPY WY2000P1                                                       
232700     EJECT                                                                
232800*    -COPY WY2000P2                                                       
