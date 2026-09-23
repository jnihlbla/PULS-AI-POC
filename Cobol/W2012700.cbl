000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2012700.                                                
000400 AUTHOR.         ANN JORDEBO.                                             
000500 DATE-WRITTEN.   91/03/16.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        VISA OCH UPPDATERA (BEKRÄFTA) PROFORMARADER (TPO 5)              
001100*        SOM SKAPAT LARM TILL ANSKAFFARE.                                 
001200*        RADEN KAN BEKRÄFTAS SOM DEN ÄR, DÅ SÄTTS BARA VEO                
001300*        (VECKOR EFTER ORDER), ELLER SPLITTAS I TRE DELAR.                
001400*        DÅ MÅSTE, FÖR VARJE RAD, BÅDE ANTAL OCH VEO ANGES.               
001500*        PROFORMAREGISTRET UPPDATERAS OCH EN ORDERBEKRÄFTELSE             
001600*        SKAPAS. LARMET TAS SEDAN BORT OCH DÄREFTER KAN INTE              
001700*        UPPDATERAS IGEN.                                                 
001800*        ALL LÄSNING AV RAD SKER VIA LARMBASEN, M A O KAN                 
001900*        INTE BEKRÄFTAD RAD VISAS IGEN.                                   
002000*                                                                         
002100*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002200*        PROGRAMMET LÄSER                                                 
002300*                              WLARTC (WDK6)                              
002400*                              WLXXBX (WDR2)                              
002500*                              WLORQI (WDQ2)                              
002600*        PROGRAMMET UPPDATERAR WLPROC (WDE8)                              
002700*                              WLPROD (WDE9)                              
002800*                              WLXXBU (WDR5)                              
002900*                              WLORQM (WDQ1)                              
003000*                                                                         
003100*                                                                         
003200*    INDATA.                                                              
003300*        TRANSAKTION: W2T127                                              
003400*        MID:         W2I12701                                            
003500*                                                                         
003600*    UTDATA.                                                              
003700*        MOD:         W2O12701                                            
003800*                                                                         
003900*   ÄNDRINGAR:                                                            
004000*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
004100*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
004200*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
004300*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
004400*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
004410*    E-TRACKER: 10254592 2015 DECOMISSION VOHF                            
004500*                                                                         
004600                                                                          
004700     SKIP3                                                                
004800 ENVIRONMENT DIVISION.                                                    
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400 77  IDPGM                       PIC X(08)   VALUE 'W2012700'.            
005500                                                                          
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800                                                                          
005900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006100 77  MAX-INDX                    PIC S9(4)  VALUE +3    COMP SYNC.        
006200 77  MOD-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
006300                                                                          
006400 77  CL                          PIC S9(4)  VALUE +0    COMP SYNC.        
006500                                                                          
006600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +267  COMP SYNC.        
006800                                                                          
006900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
007100                                                                          
007200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007300     88  INDATA-OK                           VALUE 'J'.                   
007400     88  INDATA-FEL                          VALUE 'N'.                   
007500                                                                          
007600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007700     88  NYCKLAR-OK                          VALUE 'J'.                   
007800     88  NYCKLAR-FEL                         VALUE 'N'.                   
007900                                                                          
008000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
008100     88  ALLT-OK                             VALUE 'J'.                   
008200                                                                          
008300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008400     88  EGEN-MID                            VALUE '2127'.                
008500     88  GODK-MID                            VALUE '2121' '2122'          
008600                                                   '2123' '2124'          
008700                                                   '2125' '2126'          
008800                                                   '2127' '2128'          
008900                                                   '2129'.                
009000                                                                          
009100 77  2171-TRANS-SW               PIC X       VALUE SPACE.                 
009200     88  2171-TRANS                          VALUE 'J'.                   
009300                                                                          
009400 77  TRAFF-SW                    PIC X       VALUE SPACE.                 
009500     88  TRAFF                               VALUE 'J'.                   
009600                                                                          
009700 77  ARTC-CLAG-LAEST-SW          PIC X       VALUE SPACE.                 
009800     88  ARTC-CLAG-LAEST                     VALUE 'J'.                   
009900                                                                          
010000 77  SPLITTAD-RAD-SW             PIC X       VALUE 'N'.                   
010100     88  SPLITTAD-RAD                        VALUE 'J'.                   
010200                                                                          
010300     EJECT                                                                
010400*      --- VALID IDDC CODES                                               
010500*                                                                         
010600*01    -COPY WWDCKONS                                                     
010700       EJECT                                                              
010800*    --- ARBETSFÄLT                                                       
010900 01  ARBETSFAELT.                                                         
011000     03  WS-IDANSK               PIC S9(3)  VALUE ZERO COMP-3.            
011100     03  WS-IDLEVNR              PIC X(5)   VALUE SPACE.                  
011200     03  WS-TIDISPIN             PIC S9(7)  VALUE ZERO COMP-3.            
011300     03  WS-IDKUNDRF             PIC X(10).                               
011400     03  WS-IDORDNR7-FILLER REDEFINES WS-IDKUNDRF.                        
011500       05  WS-IDORDNR7           PIC 9(7).                                
011600       05  FILLER                PIC X(3).                                
011700     03  WS-KVVECKOR-TPO5 OCCURS 3  PIC S9(3) VALUE ZERO.                 
011800     03  WS-KVART         OCCURS 3  PIC S9(7) VALUE ZERO.                 
011900     03  WS-SUMMA-INMATAT        PIC S9(9) VALUE ZERO.                    
012000     03  WS-IDLEVNR-8            PIC X(8)  VALUE SPACE.                   
012100     03  KVMOTOR                 PIC X     VALUE 'N'.                     
012200     03  KVKAROSS                PIC X     VALUE 'N'.                     
012300*    --- DATUM- OCH TIDFÄLT                                               
012400 01  DATUM-OCH-TID-FAELT.                                                 
012500     03  DAGENS-DATUM            PIC  9(6)  VALUE ZERO.                   
012600     03  AKT-TID.                                                         
012700       05  AKTUELL-TID           PIC  9(6)  VALUE ZERO.                   
012800       05  FILLER                PIC  9(2)  VALUE ZERO.                   
012900     03  DATUM-MED-ARHUNDR       PIC  9(8)  VALUE ZERO.                   
013000     03  SEKEL                   PIC  9(2).                               
013100     03  SEKEL-TAL               PIC S9(9)  COMP-3.                       
013200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013300 01  GENERELLA-SUBPROGRAM.                                                
013400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013900     EJECT                                                                
014000*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
014100*   -COPY WDATAREA                                                        
014200     EJECT                                                                
014300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014400*   -COPY WMEDAREA                                                        
014500     EJECT                                                                
014600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014700*   -COPY WMSGINIT                                                        
014800     EJECT                                                                
014900 01  MESSAGE-CODES.                                                       
015000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
015100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
015200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
015500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
015600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015700     03  ERR-ARTICLE-MISSING     PIC X(3)    VALUE '017'.                 
015800     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
015900     EJECT                                                                
016000 01  MEDDELANDEN.                                                         
016100     03  MED-1                   PIC X(11)   VALUE                        
016200         'SISTA SIDAN'.                                                   
016300     03  MED-3                   PIC X(23)   VALUE                        
016400         'LARM SAKNAS FÖR ARTIKEL'.                                       
016500     03  MED-4                   PIC X(38)   VALUE                        
016600         'LARM EJ AKTUELLT, TA BORT DET VIA 2171'.                        
016700     03  MED-5                   PIC X(49)   VALUE                        
016800         'PROFORMA-RAD ORDERSLÄPPT, TRYCK PF8 FÖR NÄSTA RAD'.             
016900     03  MED-6                   PIC X(24)   VALUE                        
017000         'PROFORMA-RAD ORDERSLÄPPT'.                                      
017100     03  MED-7                   PIC X(24)   VALUE                        
017200         'FYLL I RADERNA I ORDNING'.                                      
017300     03  FEL-1                   PIC X(13)   VALUE                        
017400         'MINVÄRDE = 1V'.                                                 
017500     03  FEL-2                   PIC X(41)   VALUE                        
017600         'SUMMA INMATAT SKA STÄMMA MED URSPR. ANTAL'.                     
017700     EJECT                                                                
017800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017900*                                                                         
018000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018100     SKIP3                                                                
018200*01  MID -COPY W2I12701                                                   
018300     EJECT                                                                
018400*01  MID -COPY W2I17101   -PRE 2171-.                                     
018500     EJECT                                                                
018600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018700     SKIP3                                                                
018800*01  -COPY WMSGAREA                                                       
018900     EJECT                                                                
019000     03  MOD REDEFINES MSG-AREA.                                          
019100*      05  -COPY W2O12701                                                 
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
020200     03  W-IDARTNR-X.                                                     
020300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
020310     03  W-IDDC-X.                                                        
020320         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
020400     03  W-KDSEGKEY-X.                                                    
020500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
020600     03  W-WDGXKEY-2223-X.                                                
020700         05  FILLER              PIC X(4)    VALUE '2223'.                
020800         05  W-IDANSK            PIC S9(3)   VALUE ZERO COMP-3.           
020900         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
021000     03  W-WDGXKEY-2224-X.                                                
021100         05  W-TISENBEK.                                                  
021200             07  W-TISENBEK-DAG  PIC S9(7)   VALUE ZERO COMP-3.           
021300             07  W-TISENBEK-KL   PIC S9(7)   VALUE ZERO COMP-3.           
021400         05  W-KDLARM            PIC S9(3)   VALUE ZERO COMP-3.           
021500     03  W-KDLARM-X.                                                      
021600         05  W-KDLARM-S          PIC S9(3)   VALUE +150 COMP-3.           
021700     03  W-WDGXKEY-2231-X.                                                
021800         05  FILLER              PIC X(4)    VALUE '2231'.                
021900         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
022000     03  W-WDGXKEY-2232-X.                                                
022100         05  W-IDANSK-L          PIC S9(3)   VALUE ZERO COMP-3.           
022200         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
022300     03  W-WDE801KY-X.                                                    
022400         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
022500         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
022600         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
022700     03  W-WDE901KY-X.                                                    
022800         05  W-IDORDER-RAD       PIC S9(7)   VALUE ZERO COMP-3.           
022900         05  W-IDARTNR-RAD       PIC S9(9)   VALUE ZERO COMP-3.           
023000         05  W-IDLOPNR-RAD       PIC S9(3)   VALUE ZERO COMP-3.           
023100*    --- STATUS-KOD FRÅN IMS                                              
023200 01  STATUS-WS                   PIC XX.                                  
023300     88  SEGMENT-FINNS                       VALUE '  '.                  
023400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023600     SKIP2                                                                
023700 01  GODK-STATUSKODER.                                                    
023800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023900     SKIP3                                                                
024000 01  SSA1                        PIC X(64).                               
024100 01  SSA2                        PIC X(64).                               
024200 01  SSA3                        PIC X(64).                               
024300     EJECT                                                                
024400*    --- IMS FUNKTIONSKODER                                               
024500*01  -COPY W0003                                                          
024600     EJECT                                                                
024700*    ---  DLI INPUT-OUTPUT AREA                                           
024800 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA1'.         
024900                                                                          
025000 01  DLI-IO-AREA1.                                                        
025100     03  IO-AREA1               PIC X(2000) VALUE SPACE.                  
025200     SKIP3                                                                
025300     03  WLPROC01 REDEFINES IO-AREA1.                                     
025400*        05  -COPY WDE801     -PRE PROC-                                  
025500     EJECT                                                                
025600 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA2'.         
025700                                                                          
025800 01  DLI-IO-AREA2.                                                        
025900     03  IO-AREA2               PIC X(200)  VALUE SPACE.                  
026000     SKIP3                                                                
026100     03  WLPROD01 REDEFINES IO-AREA2.                                     
026200*        05  -COPY WDE901     -PRE PROD-                                  
026300     EJECT                                                                
026400 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA3'.         
026500                                                                          
026600 01  DLI-IO-AREA-01.                                                      
026700     03  IO-AREA-01             PIC X(200)  VALUE SPACE.                  
026800     SKIP3                                                                
026900     03  WLARTC01 REDEFINES IO-AREA-01.                                   
027000*        05  -COPY WDK601                                                 
027100     EJECT                                                                
027200 01  DLI-IO-AREA-11.                                                      
027300     03  IO-AREA-11             PIC X(900)  VALUE SPACE.                  
027400     SKIP3                                                                
027500     03  WLARTC11 REDEFINES IO-AREA-11.                                   
027600*        05  -COPY WDK611                                                 
027700     EJECT                                                                
027800 01  DLI-IO-AREA3.                                                        
027900     03  IO-AREA3               PIC X(400)  VALUE SPACE.                  
028000     SKIP3                                                                
028100     03  WLORQM01 REDEFINES IO-AREA3.                                     
028200*        05  -COPY WDQ101     -PRE ORQM-                                  
028300     EJECT                                                                
028400     03  WLXXBU01 REDEFINES IO-AREA3.                                     
028500*        05  -COPY WDGX2223   -PRE XXBU-                                  
028600     EJECT                                                                
028700     03  WLXXBU11 REDEFINES IO-AREA3.                                     
028800*        05  -COPY WDGX2224   -PRE XXBU-                                  
028900     EJECT                                                                
029000     03  WLXXBX01 REDEFINES IO-AREA3.                                     
029100*        05  -COPY WDGX01     -PRE XXBX-                                  
029200     EJECT                                                                
029300     03  WLXXBX11 REDEFINES IO-AREA3.                                     
029400*        05  -COPY WDGX2232   -PRE XXBX-                                  
029500     EJECT                                                                
029600 LINKAGE SECTION.                                                         
029700                                                                          
029800*01  -COPY W0009      -PRE MSG-                                           
029900     EJECT                                                                
030000*01  -COPY W0008      -PRE USEA-                                          
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008      -PRE PROC-                                          
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008      -PRE PROD-                                          
030700     05  FILLER                  PIC X.                                   
030800     EJECT                                                                
030900*01  -COPY W0008      -PRE ARTC-                                          
031000     05  FILLER                  PIC X.                                   
031100     EJECT                                                                
031200*01  -COPY W0008      -PRE ORQM-                                          
031300     05  FILLER                  PIC X.                                   
031400     EJECT                                                                
031500*01  -COPY W0008      -PRE XXBU-                                          
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800*01  -COPY W0008      -PRE XXBX-                                          
031900     05  FILLER                  PIC X.                                   
032000     EJECT                                                                
032100 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
032200                                   PROC-PCB PROD-PCB                      
032300                  ARTC-PCB ORQM-PCB XXBU-PCB XXBX-PCB.                    
032400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
032500                                   PROC-PCB PROD-PCB                      
032600                  ARTC-PCB ORQM-PCB XXBU-PCB XXBX-PCB.                    
032700                                                                          
032800     PERFORM IMS-GET-MSG                                                  
032900     IF SEGMENT-FINNS                                                     
033000       PERFORM A-INIT                                                     
033100       PERFORM B-KOLLA-NYCKLAR                                            
033200       IF NYCKLAR-OK                                                      
033300         IF MFS-UPDATE                                                    
033400           PERFORM G-KOLLA-INPUT                                          
033500           IF INDATA-OK                                                   
033600             PERFORM H-UPPDATERA                                          
033700           END-IF                                                         
033800         ELSE                                                             
033900           IF MFS-FIRST                                                   
034000             PERFORM C-FOERSTA-SIDA                                       
034100           ELSE                                                           
034200             IF MFS-NEXT                                                  
034300               PERFORM D-NAESTA-SIDA                                      
034400             ELSE                                                         
034500               PERFORM E-SAMMA-SIDA                                       
034600             END-IF                                                       
034700           END-IF                                                         
034800           IF ALLT-OK                                                     
034900             PERFORM F-LAES-VISA-INFO                                     
035000           END-IF                                                         
035100         END-IF                                                           
035200       END-IF                                                             
035300       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
035400       PERFORM IMS-INSERT-MSG                                             
035500     END-IF                                                               
035600                                                                          
035700     MOVE ZERO TO RETURN-CODE                                             
035800     GOBACK                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 A-INIT SECTION.                                                          
036200                                                                          
036300     IF MSG-DUBBLA-TRANSKODER                                             
036400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I12701                 
036500                                             2171-MID-W2I17101            
036600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
036700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036800     ELSE                                                                 
036900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I12701                 
037000                                             2171-MID-W2I17101            
037100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
037200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
037300     END-IF                                                               
037400                                                                          
037500     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
037600     MOVE MSG-IDPFK TO MFS-IDPFK                                          
037700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
037800                                                                          
037900     MOVE LOW-VALUE TO MSG-AREA                                           
038000     MOVE 'W2O12701' TO MFS-IDMOD                                         
038100     MOVE '2127' TO MOD-IDTRANS                                           
038200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
038300                                                                          
038400     IF NOT EGEN-MID                                                      
038500       MOVE SPACE TO MFS-KDTRTYP                                          
038600       MOVE '7' TO MFS-IDPFK                                              
038700     END-IF                                                               
038800                                                                          
038900     IF ENGLISH-TEXT                                                      
039000       MOVE +2 TO SPRAK-IX                                                
039100       MOVE 'GB ' TO MED-IDSKYLT                                          
039200     ELSE                                                                 
039300       MOVE +1 TO SPRAK-IX                                                
039400       MOVE 'S  ' TO MED-IDSKYLT                                          
039500     END-IF                                                               
039600                                                                          
039700     IF MFS-IDTRANS = '2171'                                              
039800       PERFORM AA-FIXA-2171                                               
039900     END-IF                                                               
040000                                                                          
040100     ACCEPT DAGENS-DATUM FROM DATE                                        
040200     ACCEPT AKT-TID      FROM TIME                                        
040300     MOVE FUNCTION CURRENT-DATE (1:2) TO SEKEL                            
040400     COMPUTE SEKEL-TAL = SEKEL * 1000000                                  
040500                                                                          
040600     .                                                                    
040700     SKIP3                                                                
040800 AA-FIXA-2171 SECTION.                                                    
040900                                                                          
041000     MOVE JA TO 2171-TRANS-SW                                             
041100     MOVE 2171-MID-IDARTNR-UT     TO MID-IDARTNR-UT                       
041200                                     MOD-IDARTNR-UT                       
041300     INSPECT 2171-MID-IDANSK-UT REPLACING ALL SPACE BY ZERO               
041400     IF 2171-MID-IDANSK-UT NUMERIC                                        
041500       MOVE 2171-MID-IDANSK-UT TO W-IDANSK                                
041600     END-IF                                                               
041700                                                                          
041800     MOVE '+++++++++'             TO MID-IDARTNR-IN                       
041900                                                                          
042000     MOVE MFS-ADD-SAETT-CURSOR TO MOD-KVART-IN-ATTR(1)                    
042100     .                                                                    
042200     EJECT                                                                
042300 B-KOLLA-NYCKLAR SECTION.                                                 
042400                                                                          
042500     MOVE JA TO NYCKLAR-SW                                                
042600                                                                          
042700*    -- KONTROLL AV IDARTNR                                               
042800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
042900                                                                          
043000     MOVE ALL '+' TO MSGI-WMSGINIT                                        
043100     MOVE '001'             TO MSGI-KDCALL                                
043200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
043300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
043400     MOVE '2127'            TO MSGI-IDTRANS                               
043500     IF MFS-IDTRANS = '2127'                                              
043600     OR (MID-IDARTNR-IN NUMERIC                                           
043700     AND MID-IDARTNR-IN > ZERO)                                           
043800         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
043900     END-IF                                                               
044000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
044100     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
044200     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
044300                                                                          
044400     IF MID-IDARTNR-IN = ALL '+'                                          
044500       CONTINUE                                                           
044600     ELSE                                                                 
044700       MOVE '7'         TO MFS-IDPFK                                      
044800       MOVE SPACE       TO MFS-KDTRTYP                                    
044900     END-IF                                                               
045000                                                                          
045100     IF WS-IDARTNR NUMERIC                                                
045200       IF WS-IDARTNR > '000000000'                                        
045300         MOVE WS-IDARTNR TO W-IDARTNR                                     
045400         PERFORM IMS-GU-ARTC-ART                                          
045500         IF SEGMENT-SAKNAS                                                
045600           MOVE NEJ TO NYCKLAR-SW                                         
045700           MOVE ERR-ARTICLE-MISSING TO MED-IDMFSFEL                       
045800         ELSE                                                             
045900           MOVE ART-IDLEVNR TO WS-IDLEVNR   WS-IDLEVNR-8                  
046000                                                                          
046100*          --- KOLLA OM BEHÖRIG ANVÄNDARE                                 
046200           IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                      
046300           OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                
046400*   *         --- BEHÖRIG !                                               
046500              CONTINUE                                                    
046600           ELSE                                                           
046700              MOVE NEJ TO NYCKLAR-SW                                      
046800              MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                     
046900           END-IF                                                         
047000         END-IF                                                           
047100       ELSE                                                               
047200         IF NOT 2171-TRANS                                                
047300           MOVE NEJ TO NYCKLAR-SW                                         
047400         END-IF                                                           
047500       END-IF                                                             
047600     ELSE                                                                 
047700       MOVE NEJ TO NYCKLAR-SW                                             
047800     END-IF                                                               
047900                                                                          
048000     IF GODK-MID OR NYCKLAR-OK                                            
048100       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
048200       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
048300     ELSE                                                                 
048400       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
048500     END-IF                                                               
048600                                                                          
048700     IF  NYCKLAR-OK                                                       
048800     AND NOT MFS-NEXT                                                     
048900     AND EGEN-MID                                                         
049000       IF MID-TISENBEK-DAG-ENTER > 0                                      
049100       OR MID-TISENBEK-KL-ENTER  > 0                                      
049200       OR MID-KDLARM-ENTER       > 0                                      
049300         MOVE MID-IDANSK-DOLD        TO W-IDANSK                          
049400         MOVE MID-TISENBEK-DAG-ENTER TO W-TISENBEK-DAG                    
049500         MOVE MID-TISENBEK-KL-ENTER  TO W-TISENBEK-KL                     
049600         MOVE MID-KDLARM-ENTER       TO W-KDLARM                          
049700         PERFORM IMS-GET-XXBU-2223                                        
049800         IF SEGMENT-FINNS                                                 
049810           MOVE WC-CDC-SE            TO W-IDDC                            
049900           PERFORM IMS-GET-XXBU-2224                                      
050000           IF SEGMENT-FINNS                                               
050100             MOVE XXBU-2224-IDDISTR  TO W-IDDISTR                         
050200             MOVE XXBU-2224-IDKUNDNR TO W-IDKUNDNR                        
050300             MOVE XXBU-2224-IDKUNDRF TO W-IDKUNDRF                        
050400             MOVE XXBU-2224-IDARTNR  TO W-IDARTNR-RAD                     
050500             MOVE XXBU-2224-IDLOPNR  TO W-IDLOPNR-RAD                     
050700           ELSE                                                           
050800             MOVE NEJ TO NYCKLAR-SW                                       
050900             MOVE ZERO TO MOD-TISENBEK-DAG-ENTER                          
051000                          MOD-TISENBEK-KL-ENTER                           
051100                          MOD-KDLARM-ENTER                                
051200           END-IF                                                         
051300         ELSE                                                             
051400           MOVE NEJ TO NYCKLAR-SW                                         
051500           MOVE ZERO TO MOD-TISENBEK-DAG-ENTER                            
051600                        MOD-TISENBEK-KL-ENTER                             
051700                        MOD-KDLARM-ENTER                                  
051800                        MOD-IDANSK-DOLD                                   
051900         END-IF                                                           
052000       END-IF                                                             
052100     END-IF                                                               
052200                                                                          
052300     IF NYCKLAR-FEL                                                       
052400       IF MED-IDMFSFEL NOT = ERR-ARTICLE-MISSING                          
052500                         AND ERR-NOT-AUTHORIZED                           
052600         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
052700       END-IF                                                             
052800       CALL WMEDKONV USING MED-WMEDAREA                                   
052900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
053000       PERFORM S02-RENSA-ICKE-GRUNDRAD                                    
053100       PERFORM MFS-RENSA-FAELT-UT                                         
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 C-FOERSTA-SIDA SECTION.                                                  
053600                                                                          
053700     MOVE JA TO ALLT-SW                                                   
053800                                                                          
053900*    --- HÄMTA NYCKLAR TILL LARMET                                        
054000     IF 2171-TRANS                                                        
054100       MOVE +1 TO INDX                                                    
054200       MOVE NEJ TO TRAFF-SW                                               
054300       PERFORM UNTIL TRAFF OR INDX > 14                                   
054400         IF 2171-MID-SELECT-ARTIKEL(INDX) = 'S'                           
054500           MOVE JA TO TRAFF-SW                                            
054510           PERFORM S05-CONVERT-TISENBEK-DAG-IN                            
054800           MOVE 2171-MID-TISENBEK-KL-IN(INDX) TO W-TISENBEK-KL            
054900           MOVE 2171-MID-KDLARM(INDX)         TO W-KDLARM                 
055000         ELSE                                                             
055100           ADD +1 TO INDX                                                 
055200         END-IF                                                           
055300       END-PERFORM                                                        
055400       PERFORM IMS-GET-XXBU-2223                                          
055500       IF SEGMENT-FINNS                                                   
055510         MOVE WC-CDC-SE           TO W-IDDC                               
055600         PERFORM IMS-GET-XXBU-2224                                        
055700         IF SEGMENT-FINNS AND WS-IDARTNR = '000000000'                    
055800           MOVE XXBU-2224-IDARTNR TO WS-IDARTNR                           
055900                                     W-IDARTNR                            
056000                                     MOD-IDARTNR-UT                       
056100           INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO                  
056200                   BY SPACE                                               
056300           MOVE ALL '+' TO MSGI-WMSGINIT                                  
056400           MOVE XXBU-2224-IDARTNR TO MSGI-IDARTNR                         
056500           MOVE '001'             TO MSGI-KDCALL                          
056600           MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                          
056700           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
056800         END-IF                                                           
056900       ELSE                                                               
057000         MOVE NEJ TO ALLT-SW                                              
057100       END-IF                                                             
057200     ELSE                                                                 
057300       PERFORM IMS-GNP-ARTC-CLAG                                          
057400       IF SEGMENT-FINNS                                                   
057500         MOVE CLAG-IDANSK TO W-IDANSK-L                                   
057600                             WS-IDANSK                                    
057700         MOVE JA TO ARTC-CLAG-LAEST-SW                                    
057800         PERFORM IMS-GET-XXBX-2231                                        
057900         PERFORM IMS-GET-XXBX-2232                                        
058000         IF SEGMENT-FINNS                                                 
058100           MOVE XXBX-2232-IDANSK-LARM TO W-IDANSK                         
058200           PERFORM IMS-GET-XXBU-2223                                      
058300           IF SEGMENT-FINNS                                               
058310             MOVE WC-CDC-SE      TO W-IDDC                                
058400             PERFORM IMS-GET-XXBU-2224-IDART                              
058500             MOVE INF-FIRST-PAGE TO MED-IDMFSINF                          
058600             CALL WMEDKONV USING MED-WMEDAREA                             
058700             MOVE MED-MFSINF TO MOD-TEMFSINF                              
058800           ELSE                                                           
058900             MOVE NEJ TO ALLT-SW                                          
059000           END-IF                                                         
059100         ELSE                                                             
059200           MOVE ZERO TO W-IDANSK                                          
059300           PERFORM IMS-GET-XXBU-2223                                      
059400           IF SEGMENT-FINNS                                               
059410             MOVE WC-CDC-SE      TO W-IDDC                                
059500             PERFORM IMS-GET-XXBU-2224-IDART                              
059600             MOVE INF-FIRST-PAGE TO MED-IDMFSINF                          
059700             CALL WMEDKONV USING MED-WMEDAREA                             
059800             MOVE MED-MFSINF TO MOD-TEMFSINF                              
059900           ELSE                                                           
060000             MOVE NEJ TO ALLT-SW                                          
060100           END-IF                                                         
060200         END-IF                                                           
060300       ELSE                                                               
060400         MOVE NEJ TO ALLT-SW                                              
060500       END-IF                                                             
060600     END-IF                                                               
060700     .                                                                    
060800     EJECT                                                                
060900 D-NAESTA-SIDA SECTION.                                                   
061000                                                                          
061100     MOVE MID-IDANSK-DOLD       TO W-IDANSK                               
061200     IF  MID-TISENBEK-DAG-NEXT = ZERO                                     
061300     AND MID-TISENBEK-KL-NEXT  = ZERO                                     
061400       MOVE MID-TISENBEK-DAG-ENTER TO W-TISENBEK-DAG                      
061500       MOVE MID-TISENBEK-KL-ENTER  TO W-TISENBEK-KL                       
061600       MOVE MID-KDLARM-ENTER       TO W-KDLARM                            
061700     ELSE                                                                 
061800       MOVE MID-TISENBEK-DAG-NEXT TO W-TISENBEK-DAG                       
061900       MOVE MID-TISENBEK-KL-NEXT  TO W-TISENBEK-KL                        
062000       MOVE MID-KDLARM-NEXT       TO W-KDLARM                             
062100     END-IF                                                               
062200                                                                          
062300     PERFORM IMS-GET-XXBU-2223                                            
062400     IF SEGMENT-FINNS                                                     
062410       MOVE WC-CDC-SE             TO W-IDDC                               
062500       PERFORM IMS-GET-XXBU-2224                                          
062600     ELSE                                                                 
062700       MOVE NEJ TO ALLT-SW                                                
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 E-SAMMA-SIDA SECTION.                                                    
063200                                                                          
063300     MOVE JA TO ALLT-SW                                                   
063400                                                                          
063500     IF MID-INPUT = ALL '+'                                               
063600       MOVE MID-IDANSK-DOLD        TO W-IDANSK                            
063700       MOVE MID-TISENBEK-DAG-ENTER TO W-TISENBEK-DAG                      
063800       MOVE MID-TISENBEK-KL-ENTER  TO W-TISENBEK-KL                       
063900       MOVE MID-KDLARM-ENTER       TO W-KDLARM                            
064000       PERFORM IMS-GET-XXBU-2223                                          
064100       IF SEGMENT-FINNS                                                   
064110         MOVE WC-CDC-SE            TO W-IDDC                              
064200         PERFORM IMS-GET-XXBU-2224                                        
064300       END-IF                                                             
064400     ELSE                                                                 
064500       MOVE NEJ TO ALLT-SW                                                
064600       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
064700       CALL WMEDKONV USING MED-WMEDAREA                                   
064800       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
064900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
065000       PERFORM MFS-ADD-ROER-EJ-ICKE-GRUNDRAD                              
065100     END-IF                                                               
065200     .                                                                    
065300     EJECT                                                                
065400 F-LAES-VISA-INFO SECTION.                                                
065500                                                                          
065600     IF SEGMENT-SAKNAS                                                    
065700***    DVS OM LARM-SEGMENT SAKNAS                                         
065800       MOVE MED-3 TO MOD-TEMFSFEL                                         
065900       MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                               
066000     ELSE                                                                 
066100       MOVE XXBU-2224-IDDISTR   TO W-IDDISTR                              
066200       MOVE XXBU-2224-IDKUNDNR  TO W-IDKUNDNR                             
066300       MOVE XXBU-2224-IDKUNDRF  TO W-IDKUNDRF                             
066400       MOVE XXBU-2224-IDARTNR   TO W-IDARTNR-RAD                          
066500       MOVE XXBU-2224-IDLOPNR   TO W-IDLOPNR-RAD                          
066700       MOVE XXBU-2224-TISENBEK-DAG TO MOD-TISENBEK-DAG-ENTER              
066800       MOVE XXBU-2224-TISENBEK-KL  TO MOD-TISENBEK-KL-ENTER               
066900       MOVE XXBU-2224-KDLARM       TO MOD-KDLARM-ENTER                    
067000       MOVE W-IDANSK               TO MOD-IDANSK-DOLD                     
067010       MOVE WC-CDC-SE              TO W-IDDC                              
067100       PERFORM IMS-GET-XXBU-2224-IDART                                    
067200       IF SEGMENT-FINNS                                                   
067300         MOVE XXBU-2224-TISENBEK-DAG TO MOD-TISENBEK-DAG-NEXT             
067400         MOVE XXBU-2224-TISENBEK-KL  TO MOD-TISENBEK-KL-NEXT              
067500         MOVE XXBU-2224-KDLARM       TO MOD-KDLARM-NEXT                   
067600         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
067700         CALL WMEDKONV USING MED-WMEDAREA                                 
067800         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
067900       ELSE                                                               
068000         MOVE ZERO                   TO MOD-TISENBEK-DAG-NEXT             
068100                                        MOD-TISENBEK-KL-NEXT              
068200                                        MOD-KDLARM-NEXT                   
068300         MOVE MED-1 TO MOD-TEMFSINF                                       
068400       END-IF                                                             
068500       PERFORM IMS-GET-PROC-PHUV                                          
068600       IF SEGMENT-FINNS                                                   
068700         IF  PROC-PHUV-TIORDDAT = ZERO                                    
068800         AND PROC-PHUV-FLBORT   = NEJ                                     
068900           MOVE PROC-PHUV-IDORDER TO W-IDORDER-RAD                        
069000           PERFORM IMS-GU-ARTC-ART                                        
069100           IF SEGMENT-FINNS                                               
069200             MOVE ART-IDLEVNR TO WS-IDLEVNR                               
069300           END-IF                                                         
069400           PERFORM IMS-GET-PROD-PRAD                                      
069500           IF SEGMENT-FINNS                                               
069600             PERFORM S01-GRUNDRAD-TILL-MOD                                
069700           ELSE                                                           
069800             PERFORM FA-VISA-ATT-RAD-SAKNAS                               
069900           END-IF                                                         
070000         ELSE                                                             
070100           PERFORM FA-VISA-ATT-RAD-SAKNAS                                 
070200         END-IF                                                           
070300       ELSE                                                               
070400         PERFORM FA-VISA-ATT-RAD-SAKNAS                                   
070500       END-IF                                                             
070600       PERFORM S02-RENSA-ICKE-GRUNDRAD                                    
070700     END-IF                                                               
070800     .                                                                    
070900     EJECT                                                                
071000 FA-VISA-ATT-RAD-SAKNAS SECTION.                                          
071100                                                                          
071200     PERFORM MFS-RENSA-FAELT-RAD                                          
071300     MOVE MED-4 TO MOD-TEMFSFEL                                           
071400     IF MOD-TEMFSINF = INF-MORE-INFO-EXISTS                               
071500       MOVE MED-5 TO MOD-TEMFSINF                                         
071600     ELSE                                                                 
071700       MOVE MED-6 TO MOD-TEMFSINF                                         
071800     END-IF                                                               
071900     .                                                                    
072000     EJECT                                                                
072100 G-KOLLA-INPUT SECTION.                                                   
072200                                                                          
072300     MOVE JA  TO INDATA-SW                                                
072400                                                                          
072500     IF MID-INPUT = ALL '+'                                               
072600       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
072700       CALL WMEDKONV USING MED-WMEDAREA                                   
072800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
072900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
073000       PERFORM S02-RENSA-ICKE-GRUNDRAD                                    
073100       MOVE NEJ TO INDATA-SW                                              
073200     ELSE                                                                 
073300       PERFORM GA-FORMELL-KONTROLL                                        
073400       IF INDATA-OK                                                       
073500         PERFORM GB-RELATIONS-KONTROLL                                    
073600         IF INDATA-OK                                                     
073700           PERFORM GC-DATABAS-KONTROLL                                    
073800         END-IF                                                           
073900       END-IF                                                             
074000                                                                          
074100       IF INDATA-FEL                                                      
074200         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
074300         CALL WMEDKONV USING MED-WMEDAREA                                 
074400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
074500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
074600         PERFORM MFS-ROER-EJ-ICKE-GRUNDRAD                                
074700       END-IF                                                             
074800     END-IF                                                               
074900     .                                                                    
075000     EJECT                                                                
075100 GA-FORMELL-KONTROLL SECTION.                                             
075200                                                                          
075300     MOVE +0 TO WS-SUMMA-INMATAT                                          
075400     MOVE +1 TO INDX                                                      
075500     PERFORM UNTIL INDX > MAX-INDX                                        
075600                                                                          
075700       IF MID-KVART(INDX) = ALL '+'                                       
075800         CONTINUE                                                         
075900       ELSE                                                               
076000         IF MID-KVART(INDX) NUMERIC                                       
076100           MOVE MID-KVART(INDX) TO WS-KVART(INDX)                         
076200           IF WS-KVART(INDX) > 0                                          
076300             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVART-IN-ATTR(INDX)          
076400             ADD WS-KVART(INDX) TO WS-SUMMA-INMATAT                       
076500           ELSE                                                           
076600             MOVE NEJ TO INDATA-SW                                        
076700             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVART-IN-ATTR(INDX)          
076800           END-IF                                                         
076900         ELSE                                                             
077000           MOVE NEJ TO INDATA-SW                                          
077100           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVART-IN-ATTR(INDX)          
077200         END-IF                                                           
077300       END-IF                                                             
077400                                                                          
077500       IF MID-KVVECKOR-TPO5(INDX) = ALL '+'                               
077600         CONTINUE                                                         
077700       ELSE                                                               
077800         IF MID-KVVECKOR-TPO5(INDX) NUMERIC                               
077900           MOVE MID-KVVECKOR-TPO5(INDX) TO WS-KVVECKOR-TPO5(INDX)         
078000           IF WS-KVVECKOR-TPO5(INDX) > 0                                  
078100             MOVE MFS-NUM-FAELT-RAETT TO                                  
078200                          MOD-KVVECKOR-TPO5-IN-ATTR(INDX)                 
078300           ELSE                                                           
078400             MOVE NEJ TO INDATA-SW                                        
078500             MOVE MFS-NUM-FAELT-FEL   TO                                  
078600                          MOD-KVVECKOR-TPO5-IN-ATTR(INDX)                 
078700             MOVE FEL-1 TO MOD-TEMFSINF                                   
078800           END-IF                                                         
078900         ELSE                                                             
079000           MOVE NEJ TO INDATA-SW                                          
079100           MOVE MFS-NUM-FAELT-FEL     TO                                  
079200                          MOD-KVVECKOR-TPO5-IN-ATTR(INDX)                 
079300         END-IF                                                           
079400       END-IF                                                             
079500       ADD +1 TO INDX                                                     
079600     END-PERFORM                                                          
079700     .                                                                    
079800     EJECT                                                                
079900 GB-RELATIONS-KONTROLL SECTION.                                           
080000                                                                          
080100     MOVE +1 TO INDX                                                      
080200     PERFORM UNTIL INDX > MAX-INDX                                        
080300****   TOMRAD MITT I EJ TILLÅTET                                          
080400       IF INDX = MAX-INDX                                                 
080500         CONTINUE                                                         
080600       ELSE                                                               
080700         IF  MID-INFO-RAD(INDX) = ALL '+'                                 
080800           IF MID-INFO-RAD(INDX + 1) NOT = ALL '+'                        
080900             MOVE NEJ TO INDATA-SW                                        
081000             MOVE MFS-NUM-FAELT-FEL TO MOD-KVART-IN-ATTR(INDX)            
081100             MOVE MFS-NUM-FAELT-FEL TO                                    
081200                             MOD-KVVECKOR-TPO5-IN-ATTR(INDX)              
081300             MOVE MFS-NUM-FAELT-FEL TO                                    
081400                             MOD-KVART-IN-ATTR(INDX + 1)                  
081500             MOVE MFS-NUM-FAELT-FEL TO                                    
081600                             MOD-KVVECKOR-TPO5-IN-ATTR(INDX + 1)          
081700             MOVE MED-7 TO MOD-TEMFSINF                                   
081800           END-IF                                                         
081900         END-IF                                                           
082000       END-IF                                                             
082100****   KVVECKOR-TPO5 ÄR OBLIGATORISK                                      
082200       IF  MID-KVART(INDX) NOT = ALL '+'                                  
082300       AND MID-KVVECKOR-TPO5(INDX) = ALL '+'                              
082400         MOVE NEJ TO INDATA-SW                                            
082500         MOVE MFS-NUM-FAELT-FEL TO                                        
082600                               MOD-KVVECKOR-TPO5-IN-ATTR(INDX)            
082700       ELSE                                                               
082800         IF  MID-KVART(INDX) = ALL '+'                                    
082900         AND MID-KVVECKOR-TPO5(INDX) NOT = ALL '+'                        
083000****       VID SPLIT ÄR KVART OBLIGATORISK PÅ ALLA RADER                  
083100           IF INDX = 1                                                    
083200             IF MID-INFO-RAD(2) = ALL '+'                                 
083300               CONTINUE                                                   
083400             ELSE                                                         
083500               MOVE NEJ TO INDATA-SW                                      
083600               MOVE MFS-NUM-FAELT-FEL TO MOD-KVART-IN-ATTR(INDX)          
083700             END-IF                                                       
083800           ELSE                                                           
083900             MOVE NEJ TO INDATA-SW                                        
084000             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVART-IN-ATTR(INDX)          
084100           END-IF                                                         
084200         END-IF                                                           
084300       END-IF                                                             
084400       ADD +1 TO INDX                                                     
084500     END-PERFORM                                                          
084600     IF MID-INFO-RAD(2) NOT = ALL '+'                                     
084700       MOVE JA TO SPLITTAD-RAD-SW                                         
084800     END-IF                                                               
084900     .                                                                    
085000     EJECT                                                                
085100 GC-DATABAS-KONTROLL SECTION.                                             
085200                                                                          
085300     PERFORM IMS-GET-PROC-PHUV                                            
085400     IF SEGMENT-FINNS                                                     
085500       MOVE PROC-PHUV-IDORDER TO W-IDORDER-RAD                            
085600       PERFORM IMS-GET-PROD-PRAD                                          
085700       IF SEGMENT-FINNS                                                   
085800****   INMATAD SUMMA SKA VARA LIKA STOR SOM URSPRUNGLIGT ANTAL            
085900       IF WS-SUMMA-INMATAT > 0                                            
086000         IF WS-SUMMA-INMATAT NOT = PROD-PRAD-KVBEART                      
086100           MOVE NEJ TO INDATA-SW                                          
086200           MOVE +1 TO INDX                                                
086300           PERFORM UNTIL INDX > MAX-INDX                                  
086400             IF MID-KVART(INDX) = ALL '+'                                 
086500               CONTINUE                                                   
086600             ELSE                                                         
086700               MOVE MFS-NUM-FAELT-FEL TO MOD-KVART-IN-ATTR(INDX)          
086800             END-IF                                                       
086900             ADD +1 TO INDX                                               
087000           END-PERFORM                                                    
087100           MOVE FEL-2 TO MOD-TEMFSINF                                     
087200         END-IF                                                           
087300       END-IF                                                             
087400       ELSE                                                               
087500          MOVE NEJ TO INDATA-SW                                           
087600* OBS     HÄR BÖR DET LIGGA ETT FELMEDDELANDE                             
087700       END-IF                                                             
087800     ELSE                                                                 
087900       MOVE NEJ TO INDATA-SW                                              
088000     END-IF                                                               
088100     .                                                                    
088200     EJECT                                                                
088300 H-UPPDATERA SECTION.                                                     
088400                                                                          
088500     IF SPLITTAD-RAD                                                      
088600       PERFORM HA-BEARBETA-SPLIT                                          
088700     ELSE                                                                 
088800       PERFORM HB-UPPDATERA-RAD                                           
088900     END-IF                                                               
089000     PERFORM HC-DLET-LARM                                                 
089100                                                                          
089200     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
089300     CALL WMEDKONV USING MED-WMEDAREA                                     
089400     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
089500     .                                                                    
089600     EJECT                                                                
089700 HA-BEARBETA-SPLIT SECTION.                                               
089800                                                                          
089900**** KOLLA OM KVMOTOR/KVKAROSS SKA UPPDATERAS                             
090000                                                                          
090100     PERFORM IMS-GU-ARTC-ART                                              
090200     IF SEGMENT-FINNS                                                     
090300       IF ART-IDFKNGRP = 2101 OR 2102                                     
090400         MOVE JA TO KVMOTOR                                               
090500       ELSE                                                               
090600         IF ART-IDFKNGRP = 8001 OR 8002                                   
090700           MOVE JA TO KVKAROSS                                            
090800         END-IF                                                           
090900       END-IF                                                             
091000     END-IF                                                               
091100                                                                          
091200**** UPPDATERA GAMMAL RAD                                                 
091300                                                                          
091400     MOVE +1 TO INDX                                                      
091500     MOVE WS-KVART(INDX) TO PROD-PRAD-KVBEART                             
091600                            PROD-PRAD-KVBEART-Q                           
091700     MOVE WS-KVVECKOR-TPO5(INDX) TO PROD-PRAD-KVVECKOR-TPO5               
091800     PERFORM IMS-REPL-PROD                                                
091900     PERFORM S01-GRUNDRAD-TILL-MOD                                        
092000     PERFORM S03-FYLL-I-ORDERBEKR                                         
092100     PERFORM S04-ORDERBEKR                                                
092200     PERFORM HAA-EV-ADD-KVMOTOR-KVKAROSS                                  
092300                                                                          
092400**** SKAPA NYA RADER                                                      
092500                                                                          
092600     ADD +1 TO INDX                                                       
092700     PERFORM UNTIL INDX > MAX-INDX OR                                     
092800                   MID-INFO-RAD(INDX) = ALL '+'                           
092900       MOVE WS-KVART(INDX) TO PROD-PRAD-KVBEART                           
093000                              PROD-PRAD-KVBEART-Q                         
093100       MOVE WS-KVVECKOR-TPO5(INDX) TO PROD-PRAD-KVVECKOR-TPO5             
093200       ADD +1 TO PROD-PRAD-IDLOPNR                                        
093300       PERFORM IMS-ISRT-PROD                                              
093400       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
093500         ADD +1 TO PROD-PRAD-IDLOPNR                                      
093600         PERFORM IMS-ISRT-PROD                                            
093700       END-PERFORM                                                        
093800       PERFORM S04-ORDERBEKR                                              
093900       PERFORM HAA-EV-ADD-KVMOTOR-KVKAROSS                                
094000       ADD +1 TO INDX                                                     
094100     END-PERFORM                                                          
094200                                                                          
094300**** EV. UPPDATERING AV PROFORMA-HUVUD                                    
094400                                                                          
094500     IF KVMOTOR = JA OR KVKAROSS = JA                                     
094600       PERFORM IMS-REPL-PROC                                              
094700     END-IF                                                               
094800                                                                          
094900     PERFORM HAB-VISA-EFTER-UPPDATERING                                   
095000     .                                                                    
095100     EJECT                                                                
095200 HAA-EV-ADD-KVMOTOR-KVKAROSS SECTION.                                     
095300                                                                          
095400     IF KVMOTOR = JA                                                      
095500       ADD +1 TO PROC-PHUV-KVMOTOR                                        
095600     ELSE                                                                 
095700       IF KVKAROSS = JA                                                   
095800         ADD +1 TO PROC-PHUV-KVKAROSS                                     
095900       END-IF                                                             
096000     END-IF                                                               
096100     .                                                                    
096200     EJECT                                                                
096300 HAB-VISA-EFTER-UPPDATERING SECTION.                                      
096400                                                                          
096500     MOVE MFS-RENSA-FAELT            TO MOD-KVART-IN(1)                   
096600                                        MOD-KVVECKOR-TPO5-IN(1)           
096700     MOVE +2 TO INDX                                                      
096800     PERFORM UNTIL INDX > MAX-INDX                                        
096900       IF WS-KVART(INDX) > 0                                              
097000         MOVE WS-KVART(INDX)         TO MOD-KVART-UT(INDX)                
097100         MOVE WS-KVVECKOR-TPO5(INDX) TO MOD-KVVECKOR-TPO5-UT(INDX)        
097200         MOVE MFS-RENSA-FAELT        TO MOD-KVART-IN(INDX)                
097300                                        MOD-KVVECKOR-TPO5-IN(INDX)        
097400       END-IF                                                             
097500       ADD +1 TO INDX                                                     
097600     END-PERFORM                                                          
097700     .                                                                    
097800     EJECT                                                                
097900 HB-UPPDATERA-RAD SECTION.                                                
098000                                                                          
098100     MOVE WS-KVVECKOR-TPO5(1) TO PROD-PRAD-KVVECKOR-TPO5                  
098200     PERFORM IMS-REPL-PROD                                                
098300     PERFORM S03-FYLL-I-ORDERBEKR                                         
098400     PERFORM S04-ORDERBEKR                                                
098500     PERFORM S01-GRUNDRAD-TILL-MOD                                        
098600     PERFORM S02-RENSA-ICKE-GRUNDRAD                                      
098700     .                                                                    
098800     EJECT                                                                
098900 HC-DLET-LARM  SECTION.                                                   
099000                                                                          
099100     PERFORM IMS-GET-XXBU-2223                                            
099200     IF SEGMENT-FINNS                                                     
099210       MOVE WC-CDC-SE              TO W-IDDC                              
099300       PERFORM IMS-GET-XXBU-2224                                          
099400       IF SEGMENT-FINNS                                                   
099500         PERFORM IMS-DLET-XXBU                                            
099600       END-IF                                                             
099700     END-IF                                                               
099800     .                                                                    
099900     EJECT                                                                
100000 S01-GRUNDRAD-TILL-MOD SECTION.                                           
100100                                                                          
100200     IF PROD-PRAD-IDLEVNR NOT = SPACE                                     
100300       MOVE PROD-PRAD-IDLEVNR     TO MOD-IDLEVNR                          
100400     ELSE                                                                 
100500       MOVE WS-IDLEVNR            TO MOD-IDLEVNR                          
100600     END-IF                                                               
100700     MOVE PROD-PRAD-IDARTNR       TO MOD-IDARTNR                          
100800     MOVE W-IDDISTR               TO MOD-IDDISTR                          
100900     MOVE W-IDKUNDNR              TO MOD-IDKUNDNR                         
101000***  MOVE PROD-PRAD-KDCLAGER      TO MOD-KDCLAGER                         
101100     MOVE 1                       TO MOD-KDCLAGER                         
101200     MOVE W-IDKUNDRF              TO WS-IDKUNDRF                          
101300     MOVE WS-IDORDNR7             TO MOD-IDORDNR7                         
101400     MOVE PROC-PHUV-TIFORDAT      TO MOD-TIFORDAT                         
101500     MOVE PROD-PRAD-KVBEART       TO MOD-KVART-UT(1)                      
101600     MOVE PROD-PRAD-KVVECKOR-TPO5 TO MOD-KVVECKOR-TPO5-UT(1)              
101700     IF ARTC-CLAG-LAEST                                                   
101800       MOVE WS-IDANSK             TO MOD-IDANSK                           
101900     ELSE                                                                 
102000       PERFORM IMS-GU-ARTC-CLAG                                           
102100       IF SEGMENT-FINNS                                                   
102200         MOVE CLAG-IDANSK         TO MOD-IDANSK                           
102300       ELSE                                                               
102400         MOVE ZERO                TO MOD-IDANSK                           
102500       END-IF                                                             
102600     END-IF                                                               
102700     .                                                                    
102800     EJECT                                                                
102900 S02-RENSA-ICKE-GRUNDRAD SECTION.                                         
103000                                                                          
103100     MOVE MFS-RENSA-FAELT TO MOD-KVART-IN(1)                              
103200                             MOD-KVVECKOR-TPO5-IN(1)                      
103300     MOVE +2 TO INDX                                                      
103400     PERFORM UNTIL INDX > MAX-INDX                                        
103500       MOVE MFS-RENSA-FAELT TO MOD-KVART-UT(INDX)                         
103600                               MOD-KVVECKOR-TPO5-UT(INDX)                 
103700                               MOD-KVART-IN(INDX)                         
103800                               MOD-KVVECKOR-TPO5-IN(INDX)                 
103900       ADD +1 TO INDX                                                     
104000     END-PERFORM                                                          
104100     .                                                                    
104200     EJECT                                                                
104300 S03-FYLL-I-ORDERBEKR SECTION.                                            
104400                                                                          
104500**** ALLT UTOM KVBEART OCH TITPO FYLLS I                                  
104600                                                                          
104700     PERFORM IMS-GU-ARTC-CLAG                                             
104800     IF SEGMENT-FINNS                                                     
104900       MOVE CLAG-TIDISPIN           TO WS-TIDISPIN                        
105000     END-IF                                                               
105100     MOVE PROC-PHUV-IDORDER         TO ORQM-OBKR-IDORDER                  
105200     MOVE PROD-PRAD-IDARTNR         TO ORQM-OBKR-IDARTNR                  
105300     MOVE +1                        TO ORQM-OBKR-IDLOPNR                  
105400                                       ORQM-OBKR-IDSEKVNR                 
105500***  MOVE PROD-PRAD-KDCLAGER        TO ORQM-OBKR-KDCLAGER                 
105600     MOVE WC-CDC-SE                 TO ORQM-OBKR-IDDC                     
105700     MOVE 71                        TO ORQM-OBKR-KDORDBEK                 
105800     MOVE SPACE                     TO ORQM-OBKR-BEERS                    
105900     MOVE PROC-PHUV-BEKUNDRF        TO ORQM-OBKR-BEKUNDRF                 
106000     MOVE PROD-PRAD-BERADREF        TO ORQM-OBKR-BERADREF                 
106100     MOVE SPACE                     TO ORQM-OBKR-BEVOLREF                 
106200     MOVE ZERO                      TO ORQM-OBKR-IDKAMPRF                 
106300                                       ORQM-OBKR-DIERS-KVOT               
106400     MOVE NEJ                       TO ORQM-OBKR-FLAKPLOC                 
106500     MOVE PROD-PRAD-FLINVEST        TO ORQM-OBKR-FLINVEST                 
106600     MOVE JA                        TO ORQM-OBKR-FLOBOK                   
106700     MOVE NEJ                       TO ORQM-OBKR-FLOBTRAN                 
106800                                       ORQM-OBKR-FLOBPRT                  
106900     MOVE PROD-PRAD-FLPRTILL        TO ORQM-OBKR-FLPRTILL                 
107000     MOVE PROD-PRAD-FLRESTN         TO ORQM-OBKR-FLRESTN                  
107100     MOVE JA                        TO ORQM-OBKR-FLSLATT                  
107200     MOVE NEJ                       TO ORQM-OBKR-FLTILLK                  
107300     MOVE SPACE                     TO ORQM-OBKR-IDDC-RO                  
107400     MOVE ZERO                      TO ORQM-OBKR-IDARTNR-TILLK            
107500     MOVE PROC-PHUV-IDDISTR         TO ORQM-OBKR-IDDISTR                  
107600     MOVE PROC-PHUV-IDKUNDNR        TO ORQM-OBKR-IDKUNDNR                 
107700     MOVE PROC-PHUV-IDKUNDRF        TO ORQM-OBKR-IDKUNDRF                 
107800     MOVE '0000000   '              TO ORQM-OBKR-IDKUNDRF-RO              
107900     MOVE PROD-PRAD-IDLEVNR         TO ORQM-OBKR-IDLEVNR                  
108000     MOVE ZERO                      TO ORQM-OBKR-IDLOPNR-RO               
108100     MOVE IDPGM                     TO ORQM-OBKR-IDPGM                    
108200     MOVE PROD-PRAD-IDSYSTEM        TO ORQM-OBKR-IDSYSTEM                 
108300***  MOVE PROD-PRAD-KDCLAGER-TVS    TO ORQM-OBKR-KDCLAGER-TVS             
108400     MOVE ZERO                      TO ORQM-OBKR-KDDSP                    
108500                                       ORQM-OBKR-KDERS                    
108600     MOVE SPACE                     TO ORQM-OBKR-KDOI                     
108700     MOVE PROD-PRAD-KDKVBRYT        TO ORQM-OBKR-KDKVBRYT                 
108800     MOVE PROD-PRAD-KDPRTYP         TO ORQM-OBKR-KDPRTYP                  
108900     MOVE PROD-PRAD-KDTPOTYP        TO ORQM-OBKR-KDTPOTYP                 
109000     MOVE ZERO                      TO ORQM-OBKR-KDVRINFO                 
109100                                       ORQM-OBKR-KVANNANT                 
109200                                       ORQM-OBKR-KVAVBART                 
109300     MOVE ZERO                      TO ORQM-OBKR-KVBEART-TILLK            
109400                                       ORQM-OBKR-KVPREAVB                 
109500                                       ORQM-OBKR-KVPRERO                  
109600                                       ORQM-OBKR-KVQPACK                  
109700                                       ORQM-OBKR-KVRO                     
109800                                       ORQM-OBKR-KVSLATT                  
109900     MOVE PROD-PRAD-PRARTNTO        TO ORQM-OBKR-PRARTNTO                 
110000     MOVE PROD-PRAD-PRBPRIS         TO ORQM-OBKR-PRBPRIS                  
110100     MOVE PROD-PRAD-REKSIFFR        TO ORQM-OBKR-REKSIFFR                 
110200     MOVE ZERO                      TO ORQM-OBKR-REKSIFFR-TILLK           
110300                                       ORQM-OBKR-RERF-RAD                 
110400     MOVE WS-TIDISPIN               TO ORQM-OBKR-TIDISPIN                 
110500     MOVE PROD-PRAD-TIREGDAT        TO ORQM-OBKR-TIORDREG                 
110600     MOVE PROD-PRAD-TIPRIS          TO ORQM-OBKR-TIPRIS                   
110700     MOVE DAGENS-DATUM              TO ORQM-OBKR-TIREGDAT                 
110800     MOVE AKTUELL-TID               TO ORQM-OBKR-TIREGTID                 
110900     MOVE ZERO                      TO ORQM-OBKR-TIRODAT                  
111000     ADD SEKEL-TAL TO ORQM-OBKR-TIREGDAT GIVING                           
111100                                       DATUM-MED-ARHUNDR                  
111200     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
111300                                       ORQM-OBKR-TITIREGD-9KOMPL          
111400     ADD SEKEL-TAL TO ORQM-OBKR-TIORDREG GIVING                           
111500                                       DATUM-MED-ARHUNDR                  
111600     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
111700                                       ORQM-OBKR-TITIORDD-9KOMPL          
111800     MOVE PROC-PHUV-KDFRAKT         TO ORQM-OBKR-KDFRAKT                  
111900     MOVE PROC-PHUV-KDORDKL         TO ORQM-OBKR-KDORDKL                  
112000     MOVE SPACE                     TO ORQM-OBKR-IDBIL                    
112100     MOVE PROD-PRAD-IDPRQUES        TO ORQM-OBKR-IDPRQUES                 
112200     MOVE PROD-PRAD-PRARTNTO-LOC    TO ORQM-OBKR-PRARTNTO-LOC             
112300     MOVE PROD-PRAD-PRARTNTO-LOCPREL                                      
112400                          TO ORQM-OBKR-PRARTNTO-LOCPREL                   
112500     MOVE PROD-PRAD-PRARTBTO-LOC    TO ORQM-OBKR-PRARTBTO-LOC             
112600     MOVE PROD-PRAD-KDVALISO        TO ORQM-OBKR-KDVALISO                 
112700     MOVE PROD-PRAD-KDVAT           TO ORQM-OBKR-KDVAT                    
112800     MOVE PROD-PRAD-RERAB           TO ORQM-OBKR-RERAB                    
112900     MOVE PROD-PRAD-KDRAB           TO ORQM-OBKR-KDRAB                    
113000     MOVE PROD-PRAD-BEART-VIPS      TO ORQM-OBKR-BEART-VIPS               
113100     MOVE SPACE                     TO ORQM-OBKR-CLEARGROUP               
113200     MOVE ZERO                      TO ORQM-OBKR-TIDLEVDAT                
113210     MOVE +0                        TO ORQM-OBKR-PRAVCOST                 
113300     .                                                                    
113400     EJECT                                                                
113500 S04-ORDERBEKR SECTION.                                                   
113600                                                                          
113700     MOVE PROD-PRAD-KVBEART         TO ORQM-OBKR-KVBEART                  
113800     MOVE PROD-PRAD-KVBEART-Q       TO ORQM-OBKR-KVBEART-Q                
113900     MOVE PROD-PRAD-KVVECKOR-TPO5   TO ORQM-OBKR-TITPO                    
114000                                                                          
114100     MOVE SPACE                     TO ORQM-OBKR-KDORDTYP-LDC             
114200     MOVE ZERO                      TO ORQM-OBKR-TIREPDAT                 
114300     MOVE SPACE                     TO ORQM-OBKR-IDKUNDRF-WIP             
114500                                                                          
114600     PERFORM IMS-ISRT-ORQM                                                
114700     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
114800       ADD +1 TO ORQM-OBKR-IDLOPNR                                        
114900       PERFORM IMS-ISRT-ORQM                                              
115000     END-PERFORM                                                          
115100     .                                                                    
115200     EJECT                                                                
115300                                                                          
115310 S05-CONVERT-TISENBEK-DAG-IN SECTION.                                     
115330                                                                          
115340     MOVE 'AAVVD'                        TO DAT-KDDATFORM                 
115350     MOVE 2171-MID-TISENBEK-DAG-IN(INDX) TO DAT-I-TIDATUM                 
115360                                                                          
115370     CALL WDATKONV USING DAT-KDDATFORM                                    
115380                         DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR           
115390                                                                          
115391     IF DAT-KDSVAR-OK                                                     
115392        MOVE DAT-TIAAMMDD                TO W-TISENBEK-DAG                
115393     ELSE                                                                 
115394        MOVE ZERO                        TO W-TISENBEK-DAG                
115395     END-IF                                                               
115396     .                                                                    
115397     EJECT                                                                
115400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
115500                                                                          
115600*    --- ALLA UTDATA-FÄLT                                                 
115700*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
115800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSK-DOLD                            
115900                               MOD-TISENBEK-DAG-ENTER                     
116000                               MOD-TISENBEK-KL-ENTER                      
116100                               MOD-KDLARM-ENTER                           
116200                               MOD-TISENBEK-DAG-NEXT                      
116300                               MOD-TISENBEK-KL-NEXT                       
116400                               MOD-KDLARM-NEXT                            
116500                               MOD-IDANSK                                 
116600                               MOD-IDLEVNR                                
116700                               MOD-IDARTNR                                
116800                               MOD-IDDISTR                                
116900                               MOD-IDKUNDNR                               
117000                               MOD-KDCLAGER                               
117100                               MOD-IDORDNR7                               
117200                               MOD-TIFORDAT                               
117300                               MOD-KVART-UT(1)                            
117400                               MOD-KVVECKOR-TPO5-UT(1)                    
117500                                                                          
117600     .                                                                    
117700     EJECT                                                                
117800 MFS-ROER-EJ-ICKE-GRUNDRAD SECTION.                                       
117900                                                                          
118000*    --- INMATNINGSFÄLTEN OCH DERAS UT-FÄLT                               
118100     MOVE MFS-ROER-EJ-FAELT TO MOD-KVART-IN(1)                            
118200                               MOD-KVVECKOR-TPO5-IN(1)                    
118300     MOVE +2 TO INDX                                                      
118400     PERFORM UNTIL INDX > MAX-INDX                                        
118500       MOVE MFS-ROER-EJ-FAELT TO MOD-KVART-IN(INDX)                       
118600                                 MOD-KVVECKOR-TPO5-IN(INDX)               
118700       MOVE MFS-RENSA-FAELT   TO MOD-KVART-UT(INDX)                       
118800                                 MOD-KVVECKOR-TPO5-UT(INDX)               
118900       ADD +1 TO INDX                                                     
119000     END-PERFORM                                                          
119100     .                                                                    
119200     EJECT                                                                
119300 MFS-ADD-ROER-EJ-ICKE-GRUNDRAD SECTION.                                   
119400                                                                          
119500*    --- INMATNINGSFÄLTEN                                                 
119600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVART-IN-ATTR(1)                   
119700                                   MOD-KVVECKOR-TPO5-IN-ATTR(1)           
119800     MOVE MFS-ROER-EJ-FAELT     TO MOD-KVART-IN(1)                        
119900                                   MOD-KVVECKOR-TPO5-IN(1)                
120000     MOVE +2 TO INDX                                                      
120100     PERFORM UNTIL INDX > MAX-INDX                                        
120200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVART-IN-ATTR(INDX)              
120300                             MOD-KVVECKOR-TPO5-IN-ATTR(INDX)              
120400       MOVE MFS-ROER-EJ-FAELT     TO MOD-KVART-IN(INDX)                   
120500                                     MOD-KVVECKOR-TPO5-IN(INDX)           
120600       MOVE MFS-RENSA-FAELT       TO MOD-KVART-UT(INDX)                   
120700                                     MOD-KVVECKOR-TPO5-UT(INDX)           
120800       ADD +1 TO INDX                                                     
120900     END-PERFORM                                                          
121000     .                                                                    
121100     EJECT                                                                
121200 MFS-RENSA-FAELT-UT  SECTION.                                             
121300                                                                          
121400*    --- ALLA UTDATA-FÄLT                                                 
121500*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
121600     MOVE MFS-RENSA-FAELT   TO MOD-IDANSK-DOLD                            
121700                               MOD-TISENBEK-DAG-ENTER                     
121800                               MOD-TISENBEK-KL-ENTER                      
121900                               MOD-KDLARM-ENTER                           
122000                               MOD-TISENBEK-DAG-NEXT                      
122100                               MOD-TISENBEK-KL-NEXT                       
122200                               MOD-KDLARM-NEXT                            
122300                               MOD-IDANSK                                 
122400                               MOD-IDLEVNR                                
122500                               MOD-IDARTNR                                
122600                               MOD-IDDISTR                                
122700                               MOD-IDKUNDNR                               
122800                               MOD-KDCLAGER                               
122900                               MOD-IDORDNR7                               
123000                               MOD-TIFORDAT                               
123100                               MOD-KVART-UT(1)                            
123200                               MOD-KVVECKOR-TPO5-UT(1)                    
123300                                                                          
123400     .                                                                    
123500     EJECT                                                                
123600 MFS-RENSA-FAELT-RAD SECTION.                                             
123700                                                                          
123800*    --- ALLA UTDATA-FÄLT                                                 
123900*    --- BARA RAD-DATA                                                    
124000     MOVE MFS-RENSA-FAELT   TO MOD-IDANSK                                 
124100                               MOD-IDLEVNR                                
124200                               MOD-IDARTNR                                
124300                               MOD-IDDISTR                                
124400                               MOD-IDKUNDNR                               
124500                               MOD-KDCLAGER                               
124600                               MOD-IDORDNR7                               
124700                               MOD-TIFORDAT                               
124800                               MOD-KVART-UT(1)                            
124900                               MOD-KVVECKOR-TPO5-UT(1)                    
125000                                                                          
125100     .                                                                    
125200     EJECT                                                                
125300* --- IMS-SEKTIONER ---                                                   
125400     SKIP3                                                                
125500 IMS-GET-MSG SECTION.                                                     
125600                                                                          
125700     MOVE '  QC' TO GODK-STATUSKODER                                      
125800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
125900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
126000     PERFORM IMS-STATUSKONTROLL                                           
126100     .                                                                    
126200     SKIP3                                                                
126300 IMS-INSERT-MSG SECTION.                                                  
126400                                                                          
126500     IF ENGLISH-TEXT                                                      
126600       MOVE 'N' TO MFS-KDHUVOMR                                           
126700     END-IF                                                               
126800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
126900     MOVE SPACE TO GODK-STATUSKODER                                       
127000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
127100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
127200     PERFORM IMS-STATUSKONTROLL                                           
127300     .                                                                    
127400     EJECT                                                                
127500 IMS-GU-ARTC-ART  SECTION.                                                
127600                                                                          
127700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
127800          DELIMITED BY SIZE INTO SSA1                                     
127900     MOVE '  GE' TO GODK-STATUSKODER                                      
128000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
128100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
128200     PERFORM IMS-STATUSKONTROLL                                           
128300     .                                                                    
128400                                                                          
128500 IMS-GNP-ARTC-CLAG SECTION.                                               
128600                                                                          
128700     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
128800          DELIMITED BY SIZE INTO SSA1                                     
128900     MOVE '  GE' TO GODK-STATUSKODER                                      
129000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
129100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
129200     PERFORM IMS-STATUSKONTROLL                                           
129300     .                                                                    
129400                                                                          
129500 IMS-GU-ARTC-CLAG SECTION.                                                
129600                                                                          
129700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
129800          DELIMITED BY SIZE INTO SSA1                                     
129900     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
130000          DELIMITED BY SIZE INTO SSA2                                     
130100     MOVE '  GE' TO GODK-STATUSKODER                                      
130200     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA-11 SSA1 SSA2             
130300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
130400     PERFORM IMS-STATUSKONTROLL                                           
130500     .                                                                    
130600                                                                          
130700 IMS-GET-PROC-PHUV SECTION.                                               
130800                                                                          
130900     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
131000          DELIMITED BY SIZE INTO SSA1                                     
131100     MOVE '  GE' TO GODK-STATUSKODER                                      
131200     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-AREA1 SSA1                    
131300     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
131400     PERFORM IMS-STATUSKONTROLL                                           
131500     .                                                                    
131600                                                                          
131700 IMS-REPL-PROC SECTION.                                                   
131800                                                                          
131900     MOVE '  ' TO GODK-STATUSKODER                                        
132000     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-AREA1                        
132100     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
132200     PERFORM IMS-STATUSKONTROLL                                           
132300     .                                                                    
132400                                                                          
132500 IMS-GET-PROD-PRAD SECTION.                                               
132600                                                                          
132700     STRING 'WLPROD01(WDE901KY =' W-WDE901KY-X ')'                        
132800          DELIMITED BY SIZE INTO SSA1                                     
132900     MOVE '  GE' TO GODK-STATUSKODER                                      
133000     CALL CBLTDLI USING GHU PROD-PCB DLI-IO-AREA2 SSA1                    
133100     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
133200     PERFORM IMS-STATUSKONTROLL                                           
133300     .                                                                    
133400                                                                          
133500 IMS-REPL-PROD SECTION.                                                   
133600                                                                          
133700     MOVE '  ' TO GODK-STATUSKODER                                        
133800     CALL CBLTDLI USING REPL PROD-PCB DLI-IO-AREA2                        
133900     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
134000     PERFORM IMS-STATUSKONTROLL                                           
134100     .                                                                    
134200                                                                          
134300 IMS-ISRT-PROD SECTION.                                                   
134400                                                                          
134500     MOVE 'WLPROD01 ' TO SSA1                                             
134600     MOVE '  II' TO GODK-STATUSKODER                                      
134700     CALL CBLTDLI USING ISRT PROD-PCB DLI-IO-AREA2 SSA1                   
134800     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
134900     PERFORM IMS-STATUSKONTROLL                                           
135000     .                                                                    
135100                                                                          
135200 IMS-ISRT-ORQM SECTION.                                                   
135300                                                                          
135400     MOVE 'WLORQM01 ' TO SSA1                                             
135500     MOVE '  II' TO GODK-STATUSKODER                                      
135600     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA3 SSA1                   
135700     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
135800     PERFORM IMS-STATUSKONTROLL                                           
135900     .                                                                    
136000                                                                          
136100 IMS-GET-XXBU-2223 SECTION.                                               
136200                                                                          
136300     STRING 'WLXXBU01(WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
136400          DELIMITED BY SIZE INTO SSA1                                     
136500     MOVE '  GE' TO GODK-STATUSKODER                                      
136600     CALL CBLTDLI USING GU XXBU-PCB DLI-IO-AREA3 SSA1                     
136700     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
136800     PERFORM IMS-STATUSKONTROLL                                           
136900     .                                                                    
137000                                                                          
137100 IMS-GET-XXBU-2224 SECTION.                                               
137200                                                                          
137300     STRING 'WLXXBU11*F(WDGXKEY  =' W-WDGXKEY-2224-X                      
137310                      '&IDDC     =' W-IDDC-X ')'                          
137400          DELIMITED BY SIZE INTO SSA1                                     
137500     MOVE '  GE' TO GODK-STATUSKODER                                      
137600     CALL CBLTDLI USING GHNP XXBU-PCB DLI-IO-AREA3 SSA1                   
137700     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
137800     PERFORM IMS-STATUSKONTROLL                                           
137900     .                                                                    
138000                                                                          
138100 IMS-GET-XXBU-2224-IDART SECTION.                                         
138200                                                                          
138300     STRING 'WLXXBU11(IDARTNR  =' W-IDARTNR-X                             
138310                    '&IDDC     =' W-IDDC-X                                
138400                    '&KDLARM   =' W-KDLARM-X ')'                          
138500          DELIMITED BY SIZE INTO SSA1                                     
138600     MOVE '  GE' TO GODK-STATUSKODER                                      
138700     CALL CBLTDLI USING GNP XXBU-PCB DLI-IO-AREA3 SSA1                    
138800     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
138900     PERFORM IMS-STATUSKONTROLL                                           
139000     .                                                                    
139100                                                                          
139200 IMS-DLET-XXBU SECTION.                                                   
139300                                                                          
139400     MOVE '  ' TO GODK-STATUSKODER                                        
139500     CALL CBLTDLI USING DLET XXBU-PCB DLI-IO-AREA3                        
139600     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
139700     PERFORM IMS-STATUSKONTROLL                                           
139800     .                                                                    
139900                                                                          
140000 IMS-GET-XXBX-2231 SECTION.                                               
140100                                                                          
140200     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
140300          DELIMITED BY SIZE INTO SSA1                                     
140400     MOVE '  ' TO GODK-STATUSKODER                                        
140500     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA3 SSA1                     
140600     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
140700     PERFORM IMS-STATUSKONTROLL                                           
140800     .                                                                    
140900                                                                          
141000 IMS-GET-XXBX-2232 SECTION.                                               
141100                                                                          
141200     STRING 'WLXXBX11(WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
141300          DELIMITED BY SIZE INTO SSA1                                     
141400     MOVE '  GE' TO GODK-STATUSKODER                                      
141500     CALL CBLTDLI USING GNP XXBX-PCB DLI-IO-AREA3 SSA1                    
141600     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
141700     PERFORM IMS-STATUSKONTROLL                                           
141800     .                                                                    
141900     EJECT                                                                
142000 IMS-STATUSKONTROLL SECTION.                                              
142100                                                                          
142200     SET STATUS-IX TO 1                                                   
142300     SEARCH GODK-STATUS                                                   
142400       AT END CALL FELLOG                                                 
142500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
142600     END-SEARCH                                                           
142700     .                                                                    
