000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2017100.                                                
000400 AUTHOR.         ANN JORDEBO.                                             
000500 DATE-WRITTEN.   90/06/27.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*                                                                         
001000*        LARMKÖ-BILD SOM VISAR ALLA NYA TPO-LARM (GÅR ATT UTÖKA           
001100*        MED ANDRA TYPER AV LARM T.EX SÄKERHETSLAGER UNDERSKRIDET)        
001200*                                                                         
001300*        PROGRAMMET LÄSER LARMKÖBASEN (WDR5) SOM ÄR EN HÄNDELSE-          
001400*        BAS, OCH LISTAR ALLA 2224-SEGMENT FÖR ANSKAFFARE.                
001500*        ÄR INTE ANSKAFFARE ANGIVEN SOM NYCKEL, HÄMTAS DEN UPP-           
001600*        GIFTEN FRÅN WDK6 FÖRST.                                          
001700*   (ÄT01:19) ÄR NYCKEL LEVNR ANGIVEN, HÄMTAS ENBART DE HÄNDELSER         
001800*        SOM MATCHAR DETTA, EV. I KOMBINATION MED ANGIVET ANSKNR.         
001900*        (KOLLAS PÅ WDK6). /CONNY E                                       
002000*                                                                         
002100*        ETT LARM KAN SELEKTERAS FÖR VIDARE BEARBETNING, HOPP MHA         
002200*        PROGRAM-TO-PROGRAM-SWITCH TILL ANDRA PROGRAM GÖRS.               
002300*                                                                         
002400*        FÖLJANDE UPPDATERING GÖRS:                                       
002500*        - SLÄCKNING AV NYTT LARM UTAN ATT TA BORT DET                    
002600*        - BORTTAG AV LARM (END TPO5 OCH BYTES)                           
002700*                                                                         
002800*        LARMKÖN KAN ÄVEN TAS UT PÅ PAPPER (PF4).                         
002900*                                                                         
003000*        HOPP TILL BILDER I ANDRA SYSTEMOMRÅDEN:                          
003100*                                                                         
003200*           VID KVALITETSLARM LARMORSAK 300, 301 OCH 302                  
003300*           KAN MAN GENOM ATT SKRIVA S I RADKANTEN HOPPA                  
003400*           TILL BILD 6202. DETTA GÅR TILL SÅ ATT TRANS                   
003500*           OCH NYCKEL TILL DEN 'FRÄMMANDE' BILDEN HÄMTAS                 
003600*           FRÅN WDR550.                                                  
003700*           DET FINNS INGEN GENVÄG TILLBAKA TILL 2171 FÖR                 
003800*           ATT TA BORT LARM, UTAN MAN MÅSTE HÄMTA UPP BILDEN             
003900*           PÅ NYTT.                                                      
004000*                                                                         
004100*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
004200*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
004300*                              WLXXBX (WDR2)                              
004400*        PROGRAMMET UPPDATERAR WLXXBU (WDR5)                              
004500*                                                                         
004600*                                                                         
004700*    INDATA.                                                              
004800*        TRANSAKTION: W2T171                                              
004900*        MID:         W2I17101                                            
005000*                                                                         
005100*    UTDATA.                                                              
005200*        MOD:         W2O17101                                            
005300*                                                                         
005400*        MID:         W6I20201 (ALT.MSG-3)                                
005500*        MID:         W2I10201 (ALT.MSG-4)                                
005600*        MID:         W3I16501 (ALT.MSG-5)                                
005700*        MID:         W2I10301 (ALT.MSG-6)                                
005800*                                                                         
005900*   ÄNDRINGAR:                                                            
006000*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
006100*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
006200*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
006300*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
006400*                                                                         
006500*    2013-03-20    E'TRACKER 10143273 CHINA  LOCAL SOURCING               
006600*                  LAGT TILL IDDC PÅ WDR550/WDGX2224.                     
006700*                                                                         
006800*    2014-03-31    E'TRACKER 8403120  BLOCKADE AVROP, BILD 2149           
006900*                                                                         
007000*                                                                         
007100*    2016-07-05    E'TRACKER 10273773                                     
007200*                  223-ALARM BACKORDER FROM WHEN                          
007300*                                                                         
007400                                                                          
007500     SKIP3                                                                
007600 ENVIRONMENT DIVISION.                                                    
007700 DATA DIVISION.                                                           
007800     EJECT                                                                
007900 WORKING-STORAGE SECTION.                                                 
008000                                                                          
008100*    -- CHECKED BY WY2000                                                 
008200 77  IDPGM                       PIC X(08)   VALUE 'W2017100'.            
008300 77  JA                          PIC X       VALUE 'J'.                   
008400 77  NEJ                         PIC X       VALUE 'N'.                   
008500                                                                          
008600 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
008700 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
008800                                                                          
008900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
009000 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
009100 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
009200 77  MAX-INDX                    PIC S9(9)  VALUE +14   COMP SYNC.        
009300 77  SPIND                       PIC S9(3)  VALUE +0    COMP SYNC.        
009400                                                                          
009500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009600                                                                          
009700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009800     88  INDATA-OK                           VALUE 'J'.                   
009900     88  INDATA-FEL                          VALUE 'N'.                   
010000                                                                          
010100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010200     88  NYCKLAR-OK                          VALUE 'J'.                   
010300     88  NYCKLAR-FEL                         VALUE 'N'.                   
010400                                                                          
010500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010600     88  ALLT-OK                             VALUE 'J'.                   
010700                                                                          
010800 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
010900     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
011000     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
011100                                                                          
011200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011300     88  EGEN-MID                            VALUE '2171'.                
011400     88  GODK-MID                            VALUE '2171' '2172'          
011500                                                   '2173' '2174'          
011600                                                   '2175' '2176'          
011700                                                   '2177' '2178'          
011800                                                   '2179' '2126'.         
011900                                                                          
012000 77  S-MARKERAT-SW               PIC X(1)    VALUE SPACE.                 
012100     88  S-MARKERAT                          VALUE 'J'.                   
012200                                                                          
012300 77  ENTER-KEY-SW                PIC X(1)    VALUE SPACE.                 
012400     88  ENTER-KEY-IFYLLD                    VALUE 'J'.                   
012500                                                                          
012600 77  TRANS-TILL-UPPD-PGM-SW      PIC X(1)    VALUE SPACE.                 
012700     88  TRANS-TILL-UPPD-PGM                 VALUE 'J'.                   
012800     EJECT                                                                
012900*    --- ARBETSFÄLT                                                       
013000 01  ARBETSFAELT.                                                         
013100     03  WS-IDARTNR-8            PIC 9(8)    VALUE ZERO.                  
013200     03  WS-IDARTNR              PIC X(9)    VALUE SPACE.                 
013300     03  WS-IDLEVNR              PIC X(5)    VALUE ZERO.                  
013400     03  WS-IDLEVNR-NUM          PIC X(5)    VALUE SPACE.                 
013500     03  WS-IDLEVNR-RAD          PIC X(5)    VALUE SPACE.                 
013600     03  WS-IDLEVNR-8            PIC X(8)    VALUE SPACE.                 
013700     03  WS-IDANSK               PIC X(3)    VALUE SPACE.                 
013800     03  WS-IDANSK-NUM           PIC S9(3)   VALUE ZERO COMP-3.           
013900     03  WS-KDLARM               PIC X(3)    VALUE SPACE.                 
014000     03  WS-KDLARM-NUM           PIC S9(3)   VALUE ZERO.                  
014100     03  WS-KDLARM-VAL           PIC S9(3)   VALUE ZERO.                  
014110     03  WS-KDOTFREK             PIC X(1)    VALUE SPACE.                 
014200     03  WS-INPUT                PIC X(1)    VALUE '+'.                   
014300     03  WS-IDANSK-LARM          PIC S9(3)   VALUE ZERO.                  
014400     03  W-TIKLOCK               PIC S9(9)   VALUE ZERO COMP-3.           
014500     03  WS-TISENBEK-DAG-YYMMDD  PIC S9(06)  VALUE ZERO.                  
014600     03  WS-TISENBEK-DAG-YYWWD   PIC S9(05)  VALUE ZERO.                  
014700     03  WS-TISENBEK.                                                     
014800         05  WS-TISENBEK-DAG     PIC S9(7)   VALUE ZERO.                  
014900         05  WS-TISENBEK-KL      PIC S9(7)   VALUE ZERO.                  
015000     03  WS-TISENBEK-KL-X        PIC 9(6)    VALUE ZERO.                  
015100     03  WS-TISENBEK-KL-PRT.                                              
015200         05  WS-TISENBEK-HH      PIC 9(2)    VALUE ZERO.                  
015300         05  WS-TISENBEK-MM      PIC 9(2)    VALUE ZERO.                  
015400         05  WS-TISENBEK-SS      PIC 9(2)    VALUE ZERO.                  
015500     03  WS-DUMMY                PIC X(1)    VALUE SPACE.                 
015600                                                                          
015700***  IF INCLUDING NEW ALARM,KDLARM, MESSAGES HERE, INCLUDE                
015800***  TEORSLRM MESSAGES IN THE PROGRAM WXTR2100 ALSO                       
015900     03  WS-TEORSLRM-1.                                                   
016000         05 FILLER   PIC X(25) VALUE 'TPO-LARM                '.          
016100         05 FILLER   PIC X(25) VALUE 'TPO-ALERT               '.          
016200     03  FILLER REDEFINES WS-TEORSLRM-1.                                  
016300         05 TEORSLRM-1 PIC X(25) OCCURS 2.                                
016400                                                                          
016500     03  WS-TEORSLRM-2.                                                   
016600         05 FILLER   PIC X(25) VALUE 'PROFORMA                '.          
016700         05 FILLER   PIC X(25) VALUE 'PRO FORMA               '.          
016800     03  FILLER REDEFINES WS-TEORSLRM-2.                                  
016900         05 TEORSLRM-2 PIC X(25) OCCURS 2.                                
017000                                                                          
017100     03  WS-TEORSLRM-3.                                                   
017200         05 FILLER   PIC X(25) VALUE 'KONTROLLRAPP REGISTRERAD'.          
017300         05 FILLER   PIC X(25) VALUE 'IR ENTERED              '.          
017400     03  FILLER REDEFINES WS-TEORSLRM-3.                                  
017500         05 TEORSLRM-3 PIC X(25) OCCURS 2.                                
017600                                                                          
017700     03  WS-TEORSLRM-4.                                                   
017800         05 FILLER   PIC X(25) VALUE 'KONTROLLRAPP ÄNDRAD     '.          
017900         05 FILLER   PIC X(25) VALUE 'IR CHANGED              '.          
018000     03  FILLER REDEFINES WS-TEORSLRM-4.                                  
018100         05 TEORSLRM-4 PIC X(25) OCCURS 2.                                
018200                                                                          
018300     03  WS-TEORSLRM-5.                                                   
018400         05 FILLER   PIC X(25) VALUE 'KONTROLLRAPP ANNULLERAD '.          
018500         05 FILLER   PIC X(25) VALUE 'IR DELETED              '.          
018600     03  FILLER REDEFINES WS-TEORSLRM-5.                                  
018700         05 TEORSLRM-5 PIC X(25) OCCURS 2.                                
018800                                                                          
018900     03  WS-TEORSLRM-6.                                                   
019000         05 FILLER   PIC X(25) VALUE 'UTREDNINGSSALDO         '.          
019100         05 FILLER   PIC X(25) VALUE 'INVESTIGATION BALANCE   '.          
019200     03  FILLER REDEFINES WS-TEORSLRM-6.                                  
019300         05 TEORSLRM-6 PIC X(25) OCCURS 2.                                
019400                                                                          
019500     03  WS-TEORSLRM-7.                                                   
019600         05 FILLER   PIC X(25) VALUE 'RESTNOTERAD             '.          
019700         05 FILLER   PIC X(25) VALUE 'BACKORDER               '.          
019800     03  FILLER REDEFINES WS-TEORSLRM-7.                                  
019900         05 TEORSLRM-7 PIC X(25) OCCURS 2.                                
020000                                                                          
020100     03  WS-TEORSLRM-8.                                                   
020200         05 FILLER   PIC X(25) VALUE 'OBJEKT LAGERNIVÅ        '.          
020300         05 FILLER   PIC X(25) VALUE 'CORE BALANCE LEVEL      '.          
020400     03  FILLER REDEFINES WS-TEORSLRM-8.                                  
020500         05 TEORSLRM-8 PIC X(25) OCCURS 2.                                
020600                                                                          
020700     03  WS-TEORSLRM-9.                                                   
020800         05 FILLER   PIC X(25) VALUE 'PASSERAT BESKED         '.          
020900         05 FILLER   PIC X(25) VALUE 'DELIVERY INFO EXPIRED   '.          
021000     03  FILLER REDEFINES WS-TEORSLRM-9.                                  
021100         05 TEORSLRM-9 PIC X(25) OCCURS 2.                                
021200                                                                          
021300     03  WS-TEORSLRM-10.                                                  
021400         05 FILLER   PIC X(25) VALUE 'SÄK.LAGER UNDERSKRIDET  '.          
021500         05 FILLER   PIC X(25) VALUE 'BELOW SAFETY STOCK      '.          
021600     03  FILLER REDEFINES WS-TEORSLRM-10.                                 
021700         05 TEORSLRM-10 PIC X(25) OCCURS 2.                               
021800                                                                          
021900     03  WS-TEORSLRM-11.                                                  
022000         05 FILLER   PIC X(25) VALUE 'TILL VOR-KÖ             '.          
022100         05 FILLER   PIC X(25) VALUE 'VOR-QUEUE               '.          
022200     03  FILLER REDEFINES WS-TEORSLRM-11.                                 
022300         05 TEORSLRM-11 PIC X(25) OCCURS 2.                               
022400                                                                          
022500     03  WS-TEORSLRM-12.                                                  
022600         05 FILLER   PIC X(25) VALUE 'PUBV ÄNDRAD             '.          
022700         05 FILLER   PIC X(25) VALUE 'PUBLICATION WEEK CHANGED'.          
022800     03  FILLER REDEFINES WS-TEORSLRM-12.                                 
022900         05 TEORSLRM-12 PIC X(25) OCCURS 2.                               
023000                                                                          
023100     03  WS-TEORSLRM-223.                                                 
023200         05 FILLER   PIC X(25) VALUE 'TÄCKNING SAKNAS I PLAN  '.          
023300         05 FILLER   PIC X(25) VALUE 'NOT SUFFICIENT CALL OFFS'.          
023400     03  FILLER REDEFINES WS-TEORSLRM-223.                                
023500         05 TEORSLRM-223 PIC X(25) OCCURS 2.                              
023600                                                                          
023700     03  WS-TEORSLRM-601.                                                 
023800         05 FILLER   PIC X(25) VALUE 'EOP UPD FRM KDP - PG 15 '.          
023900         05 FILLER   PIC X(25) VALUE 'EOP UPD FRM KDP - PG 15 '.          
024000     03  FILLER REDEFINES WS-TEORSLRM-601.                                
024100         05 TEORSLRM-601 PIC X(25) OCCURS 2.                              
024200                                                                          
024300     03  WS-TEORSLRM-610.                                                 
024400         05 FILLER   PIC X(25) VALUE 'ERSÄTTNINGSKOD BORTTAGEN'.          
024500         05 FILLER   PIC X(25) VALUE 'SS CODE REMOVED         '.          
024600     03  FILLER REDEFINES WS-TEORSLRM-610.                                
024700         05 TEORSLRM-610 PIC X(25) OCCURS 2.                              
024800                                                                          
024900     03  WS-TEORSLRM-708.                                                 
025000         05 FILLER   PIC X(25) VALUE 'SI+:KÖPANMODAN AVVISAD  '.          
025100         05 FILLER   PIC X(25) VALUE 'SI+:PURCH.REQ.REJECTED  '.          
025200     03  FILLER REDEFINES WS-TEORSLRM-708.                                
025300         05 TEORSLRM-708 PIC X(25) OCCURS 2.                              
025400                                                                          
025500     03  WS-TEORSLRM-710.                                                 
025600         05 FILLER   PIC X(25) VALUE 'SI+:ANNULLATION EJ MÖJLIG'.         
025700         05 FILLER   PIC X(25) VALUE 'SI+:CANCELLATION REJECTED'.         
025800     03  FILLER REDEFINES WS-TEORSLRM-710.                                
025900         05 TEORSLRM-710 PIC X(25) OCCURS 2.                              
026000                                                                          
026100     03  WS-TEORSLRM-712.                                                 
026200         05 FILLER   PIC X(25) VALUE 'SI+:ORDER REDAN LAGD     '.         
026300         05 FILLER   PIC X(25) VALUE 'SI+:ALREADY ON ORDER     '.         
026400     03  FILLER REDEFINES WS-TEORSLRM-712.                                
026500         05 TEORSLRM-712 PIC X(25) OCCURS 2.                              
026600                                                                          
026700     03  WS-TEORSLRM-720.                                                 
026800         05 FILLER   PIC X(25) VALUE 'SI+:INKÖP NEKAR ANMODAN  '.         
026900         05 FILLER   PIC X(25) VALUE 'SI+:PURCH REJECTED REQ.  '.         
027000     03  FILLER REDEFINES WS-TEORSLRM-720.                                
027100         05 TEORSLRM-720 PIC X(25) OCCURS 2.                              
027200                                                                          
027300     03  WS-TEORSLRM-761.                                                 
027400         05 FILLER   PIC X(25) VALUE 'LEVERANTÖR SAKNAS PÅ 2114'.         
027500         05 FILLER   PIC X(25) VALUE 'SUPPL IS MISSING ON 2114 '.         
027600     03  FILLER REDEFINES WS-TEORSLRM-761.                                
027700         05 TEORSLRM-761 PIC X(25) OCCURS 2.                              
027800                                                                          
027900     03  WS-TEORSLRM-777.                                                 
028000         05 FILLER   PIC X(25) VALUE 'NAP:ANNULLATION INOM 6 V '.         
028100         05 FILLER   PIC X(25) VALUE 'NAP:CANCELLATION 6 WEEKS '.         
028200     03  FILLER REDEFINES WS-TEORSLRM-777.                                
028300         05 TEORSLRM-777 PIC X(25) OCCURS 2.                              
028400                                                                          
028500     03  WS-TEORSLRM-240.                                                 
028600         05 FILLER   PIC X(25) VALUE 'DDGS 1.0 KUNDE EJ UPPDAT '.         
028700         05 FILLER   PIC X(25) VALUE 'DDGS 1.0 COULD NOT BE UPD'.         
028800     03  FILLER REDEFINES WS-TEORSLRM-240.                                
028900         05 TEORSLRM-240 PIC X(25) OCCURS 2.                              
029000                                                                          
029100     03  WS-TEORSLRM-999.                                                 
029200         05 FILLER   PIC X(25) VALUE 'BLOCKAD PL.FLYTT EJ MÖJL '.         
029300         05 FILLER   PIC X(25) VALUE 'BLOCKED DEL. MOVE NOT OK '.         
029400     03  FILLER REDEFINES WS-TEORSLRM-999.                                
029500         05 TEORSLRM-999 PIC X(25) OCCURS 2.                              
029600                                                                          
029700     03  WS-VALD-RAD             PIC S9(4) VALUE +0 COMP SYNC.            
029800*    --- DATUMFÄLT                                                        
029900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
030000 01  DAGENS-AAVV                 PIC 9(4)    VALUE ZERO.                  
030100 01  WS-ALARM-AAVV               PIC 9(4)    VALUE ZERO.                  
030200     EJECT                                                                
030300                                                                          
030400*01  -COPY WWDCKONS                                                       
030500     EJECT                                                                
030600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
030700 01  GENERELLA-SUBPROGRAM.                                                
030800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
030900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
031000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
031100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
031200     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
031300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
031400     EJECT                                                                
031500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
031600*   -COPY WMEDAREA                                                        
031700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
031800*01 -COPY WMSGINIT                                                        
031900                                                                          
032000     EJECT                                                                
032100*    --- PARAMETRAR TILL WZ20DAYS                                         
032200 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
032300*   -COPY WZ20DAYS                                                        
032400     EJECT                                                                
032500*    --- PARAMETRAR TILL WDATKONV                                         
032600*01 -COPY WDATAREA                                                        
032700     EJECT                                                                
032800 01  MESSAGE-CODES.                                                       
032900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
033000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
033100     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
033200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
033300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
033400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
033500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
033600     03  ERR-PARTNO-MISSING      PIC X(3)    VALUE '017'.                 
033700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
033800     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
033900*    --- ÖVRIGA MEDDELANDEN                                               
034000 01  MEDDELANDEN.                                                         
034100     03  FEL-1                   PIC X(33)   VALUE                        
034200         'TRYCK F9 FÖR SELEKTION AV LARM'.                                
034300     03  FEL-2                   PIC X(19)   VALUE                        
034400         'BORTTAG EJ TILLÅTET'.                                           
034500     03  FEL-4                   PIC X(21)   VALUE                        
034600         'FELAKTIG PRINTER VALD'.                                         
034700     03  FEL-5                   PIC X(23)   VALUE                        
034800         'ENDAST BORTTAG TILLÅTET'.                                       
034900     03  FEL-6                   PIC X(23)   VALUE                        
035000         'HOPP TILL 2102 GICK EJ '.                                       
035100     03  FEL-7                   PIC X(40)   VALUE                        
035200         'BEHÖRIGHETSKONTROLL AKTIVERAD '.                                
035300     03  MED-1                   PIC X(11)   VALUE                        
035400         'LARM SAKNAS'.                                                   
035500     03  MED-2                   PIC X(23)   VALUE                        
035600         'LISTA KÖAD FÖR UTSKRIFT'.                                       
035700     EJECT                                                                
035800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
035900*                                                                         
036000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
036100     SKIP3                                                                
036200*01  MID -COPY W2I17101                                                   
036300     EJECT                                                                
036400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
036500     SKIP3                                                                
036600*01  -COPY WMSGAREA                                                       
036700     EJECT                                                                
036800     03  MOD REDEFINES MSG-AREA.                                          
036900*      05  -COPY W2O17101                                                 
037000     EJECT                                                                
037100 01  FILLER                      PIC X(16)  VALUE                         
037200                                            'WDR301-DATA-AREA'.           
037300     SKIP3                                                                
037400*01  -COPY W214ALOG                                                       
037500     EJECT                                                                
037600 01  FILLER                PIC X(16) VALUE 'PROG-TO-PROG-SW'.             
037700 01  W-PROG-TO-PROG-SW.                                                   
037800     03  P-WS-LL           PIC S9(4) VALUE +1063 COMP SYNC.               
037900     03  FILLER            PIC X(2)  VALUE LOW-VALUE.                     
038000     03  KDTRANS-WS        PIC X(8)  VALUE 'W2T171  '.                    
038100     03  FILLER            PIC X(5)  VALUE '21711'.                       
038200*    03  MID  -COPY W2I17101    -PRE PRGSW-.                              
038300*    03  MOD  -COPY W2O17101    -PRE PRGSW-.                              
038400     EJECT                                                                
038500 01  FILLER                PIC X(16)   VALUE 'MID TILL 6102 '.            
038600 01  ALT3-MSG-AREA.                                                       
038700                                                                          
038800     03  ALT3-LL           PIC S9(4)   VALUE +182 COMP SYNC.              
038900** W6I20201 + 17                                                          
039000     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
039100     03  ALT3-KDTRANS      PIC X(8)    VALUE SPACE.                       
039200     03  ALT3-IDTRANS      PIC X(4)    VALUE SPACE.                       
039300     03  ALT3-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
039400     03  MID -COPY W6I20201    -PRE ALT3-                                 
039500     EJECT                                                                
039600 01  FILLER                PIC X(16)   VALUE 'MID TILL 2102 '.            
039700 01  ALT4-MSG-AREA.                                                       
039800                                                                          
039900     03  ALT4-LL           PIC S9(4)   VALUE +35  COMP SYNC.              
040000     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
040100     03  ALT4-KDTRANS      PIC X(8)    VALUE SPACE.                       
040200     03  ALT4-IDTRANS      PIC X(4)    VALUE SPACE.                       
040300     03  ALT4-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
040400     03  MID -COPY W2I10201    -PRE ALT4-                                 
040500     EJECT                                                                
040600 01  FILLER                PIC X(16)   VALUE 'MID TILL 3165 '.            
040700 01  ALT5-MSG-AREA.                                                       
040800                                                                          
040900     03  ALT5-LL           PIC S9(4)   VALUE +40  COMP SYNC.              
041000     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
041100     03  ALT5-KDTRANS      PIC X(8)    VALUE SPACE.                       
041200     03  ALT5-IDTRANS      PIC X(4)    VALUE SPACE.                       
041300     03  ALT5-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
041400     03  MID -COPY W3I16501    -PRE ALT5-                                 
041500     EJECT                                                                
041600                                                                          
041700 01  FILLER                PIC X(16)   VALUE 'MID TILL 2103 '.            
041800 01  ALT6-MSG-AREA.                                                       
041900                                                                          
042000     03  ALT6-LL           PIC S9(4)   VALUE +185 COMP SYNC.              
042100     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
042200     03  ALT6-KDTRANS      PIC X(8)    VALUE SPACE.                       
042300     03  ALT6-IDTRANS      PIC X(4)    VALUE SPACE.                       
042400     03  ALT6-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
042500     03  MID -COPY W2I10301    -PRE ALT6-                                 
042600     EJECT                                                                
042700                                                                          
042800 01  FILLER                PIC X(16)   VALUE 'MID TILL 2114 '.            
042900 01  ALT7-MSG-AREA.                                                       
043000                                                                          
043100     03  ALT7-LL           PIC S9(4)   VALUE +103 COMP SYNC.              
043200     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
043300     03  ALT7-KDTRANS      PIC X(8)    VALUE SPACE.                       
043400     03  ALT7-IDTRANS      PIC X(4)    VALUE SPACE.                       
043500     03  ALT7-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
043600     03  MID -COPY W2I11401    -PRE ALT7-                                 
043700     EJECT                                                                
043800                                                                          
043900 01  FILLER                PIC X(16)   VALUE 'MFS-AREA'.                  
044000     SKIP3                                                                
044100*01  -COPY WMFSAREA                                                       
044200     EJECT                                                                
044300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
044400*                                                                         
044500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
044600     SKIP3                                                                
044700 01  NYCKLAR-TILL-DLI.                                                    
044800     03  W-IDDC-X.                                                        
044900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
045000                                                                          
045100     03  W-IDARTNR-X.                                                     
045200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
045300     03  W-IDARTNR-RAD-X.                                                 
045400         05  W-IDARTNR-RAD       PIC S9(9)   VALUE ZERO COMP-3.           
045500     03  W-KDSEGKEY-X.                                                    
045600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
045700     03  W-TISENBEK-MIN-X.                                                
045800         05  W-TISENBEK-DAG-MIN  PIC S9(7)   VALUE ZERO COMP-3.           
045900         05  W-TISENBEK-KL-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
046000     03  W-TISENBEK-MAX-X.                                                
046100         05  W-TISENBEK-DAG-MAX  PIC S9(7)   VALUE +9999999               
046200                                               COMP-3.                    
046300         05  W-TISENBEK-KL-MAX   PIC S9(7)   VALUE +9999999               
046400                                               COMP-3.                    
046500     03  W-KDLARM-X.                                                      
046600         05  W-KDLARM-XX         PIC S9(3)   VALUE ZERO COMP-3.           
046700     03  W-WDGXKEY-2223-X.                                                
046800         05  FILLER              PIC X(4)    VALUE '2223'.                
046900         05  W-IDANSK            PIC S9(3)   VALUE ZERO COMP-3.           
047000         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
047100     03  W-WDGXKEY-2224-X.                                                
047200         05  W-TISENBEK.                                                  
047300             07  W-TISENBEK-DAG  PIC S9(7)   COMP-3.                      
047400             07  W-TISENBEK-KL   PIC S9(7)   COMP-3.                      
047500         05  W-KDLARM            PIC S9(3)   COMP-3.                      
047600     03  W-WDGXKEY-2231-X.                                                
047700         05  FILLER              PIC X(4)    VALUE '2231'.                
047800         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
047900     03  W-WDGXKEY-2232-X.                                                
048000         05  W-IDANSK-L          PIC S9(3)   VALUE ZERO COMP-3.           
048100         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
048200     EJECT                                                                
048300*    --- STATUS-KOD FRÅN IMS                                              
048400 01  STATUS-WS                   PIC XX.                                  
048500     88  SEGMENT-FINNS                       VALUE '  '.                  
048600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
048700     88  SEGMENT-SAKNAS                      VALUE 'GE' 'GB'.             
048800     SKIP2                                                                
048900 01  GODK-STATUSKODER.                                                    
049000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
049100     SKIP3                                                                
049200 01  SSA1                        PIC X(64).                               
049300 01  SSA2                        PIC X(64).                               
049400     EJECT                                                                
049500*    --- IMS FUNKTIONSKODER                                               
049600*01  -COPY W0003                                                          
049700     EJECT                                                                
049800*    ---  DLI INPUT-OUTPUT AREA                                           
049900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
050000 01  DLI-IO-WDK601.                                                       
050100*    03  -COPY WDK601                                                     
050200     EJECT                                                                
050300 01  DLI-IO-WDK611.                                                       
050400*    03  -COPY WDK611                                                     
050500     EJECT                                                                
050600 01  DLI-IO-AREA.                                                         
050700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
050800     SKIP3                                                                
050900     03  WLXXBU01 REDEFINES IO-AREA.                                      
051000*        05  -COPY WDGX2223   -PRE XXBU-                                  
051100     EJECT                                                                
051200     03  WLXXBU10 REDEFINES IO-AREA.                                      
051300*        05  -COPY WDGX2224   -PRE XXBU-                                  
051400     SKIP3                                                                
051500     03  WLXXBX01 REDEFINES IO-AREA.                                      
051600*        05  -COPY WDGX01     -PRE XXBX-                                  
051700     EJECT                                                                
051800     03  WLXXBX20 REDEFINES IO-AREA.                                      
051900*        05  -COPY WDGX2232   -PRE XXBX-                                  
052000     EJECT                                                                
052100*    ---  DLI INPUT-OUTPUT AREA WDR301                                    
052200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDR301'.         
052300 01  DLI-IO-WDR301.                                                       
052400*    03  -COPY WDR301                                                     
052500                                                                          
052600 LINKAGE SECTION.                                                         
052700*01  -COPY W0009      -PRE MSG-                                           
052800                                                                          
052900*  PSB FÖR HOPP TILL 2126                                                 
053000*01  -COPY W0009      -PRE ALT-                                           
053100     EJECT                                                                
053200*  PSB FÖR HOPP TILL 2127                                                 
053300*01  -COPY W0009      -PRE ALT1-                                          
053400                                                                          
053500*  PSB FÖR IMS-PRINTNING                                                  
053600*01  -COPY W0009      -PRE ALT2-                                          
053700     EJECT                                                                
053800*  PSB FÖR HOPP TILL 6202                                                 
053900*01  -COPY W0009      -PRE ALT3-                                          
054000                                                                          
054100*  PSB FÖR HOPP TILL 2102                                                 
054200*01  -COPY W0009      -PRE ALT4-                                          
054300     EJECT                                                                
054400*  PSB FÖR HOPP TILL 3165                                                 
054500*01  -COPY W0009      -PRE ALT5-                                          
054600     EJECT                                                                
054700*  PSB FÖR HOPP TILL 2103                                                 
054800*01  -COPY W0009      -PRE ALT6-                                          
054900     EJECT                                                                
055000*  PSB FÖR HOPP TILL 2114                                                 
055100*01  -COPY W0009      -PRE ALT7-                                          
055200     EJECT                                                                
055300*01  -COPY W0008      -PRE USEA-                                          
055400     05  FILLER                  PIC X.                                   
055500     EJECT                                                                
055600*01  -COPY W0008      -PRE ARTC-                                          
055700     05  FILLER                  PIC X.                                   
055800                                                                          
055900*01  -COPY W0008      -PRE XXBU-                                          
056000     05  FILLER                  PIC X.                                   
056100     EJECT                                                                
056200*01  -COPY W0008      -PRE XXBX-                                          
056300     05  FILLER                  PIC X.                                   
056400     EJECT                                                                
056500*01  -COPY W0008      -PRE WDR3-                                          
056600     05  FILLER                  PIC X.                                   
056700     EJECT                                                                
056800 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB  ALT1-PCB ALT3-PCB            
056900                           ALT4-PCB ALT5-PCB ALT6-PCB ALT7-PCB            
057000                           USEA-PCB                                       
057100                           ARTC-PCB XXBU-PCB XXBX-PCB WDR3-PCB.           
057200 MAIN SECTION.                                                            
057300     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  ALT1-PCB ALT3-PCB            
057400                           ALT4-PCB ALT5-PCB ALT6-PCB ALT7-PCB            
057500                           USEA-PCB                                       
057600                           ARTC-PCB XXBU-PCB XXBX-PCB WDR3-PCB.           
057700                                                                          
057800     PERFORM IMS-GET-MSG                                                  
057900     IF SEGMENT-FINNS                                                     
058000       PERFORM A-INIT                                                     
058100       PERFORM B-KOLLA-NYCKLAR                                            
058200       IF NYCKLAR-OK                                                      
058300         IF MFS-UPDATE                                                    
058400           PERFORM G-KOLLA-INPUT                                          
058500           IF INDATA-OK                                                   
058600             PERFORM H-UPPDATERA-VISA                                     
058700           END-IF                                                         
058800         ELSE                                                             
058900           IF MFS-FIRST                                                   
059000             PERFORM C-FOERSTA-SIDA                                       
059100           ELSE                                                           
059200             IF MFS-NEXT                                                  
059300               PERFORM D-NAESTA-SIDA                                      
059400             ELSE                                                         
059500               PERFORM I-KOLLA-S-MARKERING                                
059600               IF S-MARKERAT                                              
059700                 MOVE JA TO TRANS-TILL-UPPD-PGM-SW                        
059800               ELSE                                                       
059900                 PERFORM E-SAMMA-SIDA                                     
060000               END-IF                                                     
060100             END-IF                                                       
060200           END-IF                                                         
060300           IF ALLT-OK AND NOT TRANS-TILL-UPPD-PGM                         
060400             PERFORM F-LAES-VISA-INFO                                     
060500           END-IF                                                         
060600         END-IF                                                           
060700       END-IF                                                             
060800***    IF INCLUDING NEW ALARM, KDLARM, HERE, PLEASE INCLUDE               
060900***    CORR. TEORSLRM MESSAGES IN THE PROGRAM WXTR2100 ALSO               
061000       IF TRANS-TILL-UPPD-PGM                                             
061100         MOVE MID-W2I17101 TO PRGSW-MID-W2I17101                          
061200         MOVE MOD-W2O17101 TO PRGSW-MOD-W2O17101                          
061300         EVALUATE WS-KDLARM-VAL                                           
061400           WHEN '100' PERFORM N-BEHANDLA-1XX-LARM                         
061500           WHEN '110' PERFORM N-BEHANDLA-1XX-LARM                         
061600           WHEN '150' PERFORM O-BEHANDLA-150-LARM                         
061700           WHEN '200' PERFORM L-BEHANDLA-2XX-LARM                         
061800           WHEN '210' PERFORM L-BEHANDLA-2XX-LARM                         
061900           WHEN '221' PERFORM L-BEHANDLA-2XX-LARM                         
062000           WHEN '222' PERFORM L-BEHANDLA-2XX-LARM                         
062100           WHEN '223' PERFORM L-BEHANDLA-2XX-LARM                         
062200           WHEN '240' PERFORM L-BEHANDLA-2XX-LARM                         
062300           WHEN '300' PERFORM K-BEHANDLA-KVAL-LARM                        
062400           WHEN '301' PERFORM K-BEHANDLA-KVAL-LARM                        
062500           WHEN '302' PERFORM K-BEHANDLA-KVAL-LARM                        
062600           WHEN '400' PERFORM M-BEHANDLA-EXCH-LARM                        
062700           WHEN '500' PERFORM L-BEHANDLA-2XX-LARM                         
062800           WHEN '600' PERFORM L-BEHANDLA-2XX-LARM                         
062900           WHEN '601' PERFORM L-BEHANDLA-2XX-LARM                         
063000           WHEN '610' PERFORM L-BEHANDLA-2XX-LARM                         
063100           WHEN '708' PERFORM L-BEHANDLA-2XX-LARM                         
063200           WHEN '712' PERFORM L-BEHANDLA-2XX-LARM                         
063300           WHEN '720' PERFORM L-BEHANDLA-2XX-LARM                         
063400           WHEN '761' PERFORM P-BEHANDLA-761-LARM                         
063500           WHEN '999' PERFORM L-BEHANDLA-2XX-LARM                         
063600           WHEN OTHER PERFORM S09-VISA-EFTER-FELAKTIGT-VAL                
063700                     COMPUTE MSG-KVLL = LENGTH OF MOD-W2O17101 + 4        
063800                              PERFORM IMS-INSERT-MSG                      
063900         END-EVALUATE                                                     
064000       ELSE                                                               
064100         COMPUTE MSG-KVLL = LENGTH OF MOD-W2O17101 + 4                    
064200         PERFORM IMS-INSERT-MSG                                           
064300       END-IF                                                             
064400     END-IF                                                               
064500                                                                          
064600     MOVE ZERO TO RETURN-CODE                                             
064700     GOBACK                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 A-INIT SECTION.                                                          
065100     MOVE 'A-INIT                   ' TO CURRENT-SECTION                  
065200                                                                          
065300     IF MSG-DUBBLA-TRANSKODER                                             
065400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I17101                 
065500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
065600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
065700     ELSE                                                                 
065800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I17101                  
065900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
066000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
066100     END-IF                                                               
066200                                                                          
066300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
066400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
066500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
066600                                                                          
066700     MOVE LOW-VALUE TO MSG-AREA                                           
066800     MOVE 'W2O17101' TO MFS-IDMOD                                         
066900     MOVE '2171' TO MOD-IDTRANS                                           
067000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
067100                                                                          
067200     IF ENGLISH-TEXT                                                      
067300       MOVE 'GB ' TO MED-IDSKYLT                                          
067400       MOVE +2    TO SPIND                                                
067500     ELSE                                                                 
067600       MOVE 'S  ' TO MED-IDSKYLT                                          
067700       MOVE +1    TO SPIND                                                
067800     END-IF                                                               
067900                                                                          
068000     MOVE WC-CDC-SE     TO W-IDDC                                         
068100                                                                          
068200     ACCEPT DAGENS-DATUM FROM DATE                                        
068300     ACCEPT W-TIKLOCK    FROM TIME                                        
068400     MOVE   DAGENS-DATUM   TO DAT-I-TIDATUM                               
068500*                                                                         
068600     PERFORM S10-CONVERT-DATE-FMT                                         
068700*                                                                         
068800     MOVE DAT-TIAAVV-GRP   TO DAGENS-AAVV                                 
068900     .                                                                    
069000     EJECT                                                                
069100 B-KOLLA-NYCKLAR SECTION.                                                 
069200     MOVE 'B-KOLLA-NYCKLAR          ' TO CURRENT-SECTION                  
069300     SKIP2                                                                
069400     MOVE JA TO NYCKLAR-SW                                                
069500                                                                          
069600*    -- KONTROLL AV IDARTNR                                               
069700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
069800                                                                          
070000     IF  MID-IDARTNR-IN = ALL '+'                                         
070100     AND MID-IDLEVNR-IN = ALL '+'                                         
070200     AND MID-IDANSK-IN = ALL '+'                                          
070300     AND MID-KDLARM-IN = ALL '+'                                          
070310     AND MID-KDOTFREK-IN = ALL '+'                                        
070400       CONTINUE                                                           
070500     ELSE                                                                 
071000       MOVE '7'              TO MFS-IDPFK                                 
071100       MOVE SPACE            TO MFS-KDTRTYP                               
071200     END-IF                                                               
071210     IF  MID-IDARTNR-IN = '00000000 '                                     
071220       MOVE ZERO TO MID-IDARTNR-IN                                        
071230     END-IF                                                               
071270     IF  MID-IDANSK-IN = '00 '                                            
071280       MOVE ZERO TO MID-IDANSK-IN                                         
071290     END-IF                                                               
071291     IF  MID-KDLARM-IN = '00 '                                            
071293       MOVE ZERO TO MID-KDLARM-IN                                         
071294     END-IF                                                               
071300                                                                          
071400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
071500     MOVE '001'             TO MSGI-KDCALL                                
071600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
071700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
071800     MOVE '2171'            TO MSGI-IDTRANS                               
071900     IF EGEN-MID                                                          
072010       IF MID-IDARTNR-IN NUMERIC                                          
072011         MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                            
072012       END-IF                                                             
072020       MOVE MID-IDANSK-IN    TO MSGI-IDPERSON                             
072100       IF MID-IDANSK-IN NOT = ALL '+'                                     
072300         MOVE 'ANSK'         TO MSGI-KDARBTYP                             
072400       ELSE                                                               
072500         MOVE SPACE          TO MSGI-KDARBTYP                             
072600       END-IF                                                             
072610       MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                              
072711       MOVE MID-KDLARM-IN    TO MSGI-KDLARM                               
072730       MOVE MID-KDOTFREK-IN  TO MSGI-KDOTFREK                             
072800     ELSE                                                                 
072900       MOVE ZERO             TO MSGI-IDARTNR                              
073100       MOVE '7'              TO MFS-IDPFK                                 
073200       MOVE SPACE            TO MFS-KDTRTYP                               
073300     END-IF                                                               
073400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
073500     MOVE MSGI-IDARTNR     TO WS-IDARTNR                                  
073600     MOVE MSGI-IDPERSON    TO WS-IDANSK                                   
073700     MOVE MSGI-IDLEVNR     TO WS-IDLEVNR                                  
073912     MOVE MSGI-KDLARM      TO WS-KDLARM                                   
073920     MOVE MSGI-KDOTFREK    TO WS-KDOTFREK                                 
073930     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
073940     INSPECT WS-IDANSK  REPLACING LEADING SPACE BY ZERO                   
073950     INSPECT WS-KDLARM  REPLACING LEADING SPACE BY ZERO                   
073951                                                                          
073960     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
073970     MOVE WS-IDANSK        TO MOD-IDANSK-UT                               
073980     MOVE WS-IDLEVNR       TO MOD-IDLEVNR-UT                              
073990     MOVE WS-KDLARM        TO MOD-KDLARM-UT                               
073991     MOVE WS-KDOTFREK      TO MOD-KDOTFREK-UT                             
073992     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
073993     INSPECT MOD-IDANSK-UT  REPLACING LEADING ZERO BY SPACE               
073994     INSPECT MOD-KDLARM-UT  REPLACING LEADING ZERO BY SPACE               
074000                                                                          
074100*    -- KONTROLL AV IDARTNR                                               
074200     IF WS-IDARTNR NUMERIC                                                
074300       MOVE WS-IDARTNR TO W-IDARTNR                                       
074400     ELSE                                                                 
074500       MOVE NEJ TO NYCKLAR-SW                                             
074600     END-IF                                                               
074700                                                                          
074800*    -- KONTROLL AV IDANSK                                                
074900     MOVE MFS-RENSA-FAELT TO MOD-IDANSK-IN                                
075000                                                                          
075100     IF WS-IDANSK NUMERIC                                                 
075200       MOVE WS-IDANSK TO W-IDANSK                                         
075300     ELSE                                                                 
075400       MOVE NEJ TO NYCKLAR-SW                                             
075500     END-IF                                                               
075600                                                                          
075700*    -- KONTROLL AV IDLEVNR                                               
075800     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
075900*      --  EJ NYCKEL FÖR BASLÄSNING, ENDAST FÖR SELEKTERING               
076000       MOVE WS-IDLEVNR TO WS-IDLEVNR-NUM                                  
076100                                                                          
076200*    -- KONTROLL AV KDLARM                                                
076300     MOVE MFS-RENSA-FAELT TO MOD-KDLARM-IN                                
076400                                                                          
076500     IF EGEN-MID                                                          
076600       IF MID-KDLARM-IN = ALL '+'                                         
076800         CONTINUE                                                         
076900       ELSE                                                               
077100         MOVE '7'              TO MFS-IDPFK                               
077200         MOVE SPACE            TO MFS-KDTRTYP                             
077300       END-IF                                                             
077600     END-IF                                                               
077800     IF WS-KDLARM NUMERIC                                                 
077900       MOVE WS-KDLARM TO W-KDLARM                                         
078000                         W-KDLARM-XX                                      
078100     ELSE                                                                 
078200       MOVE NEJ TO NYCKLAR-SW                                             
078300     END-IF                                                               
078400                                                                          
078410*    -- KONTROLL AV KDOTFREK                                              
078420     MOVE MFS-RENSA-FAELT TO MOD-KDOTFREK-IN                              
078430                                                                          
078440     IF EGEN-MID                                                          
078497       IF MID-KDOTFREK-IN = ALL '+'                                       
078499         CONTINUE                                                         
078500       ELSE                                                               
078501         IF MID-KDOTFREK-IN = 'H' OR 'L' OR ' '                           
078502           CONTINUE                                                       
078504         ELSE                                                             
078505           MOVE NEJ TO NYCKLAR-SW                                         
078506         END-IF                                                           
078507       END-IF                                                             
078510     END-IF                                                               
078511                                                                          
078520*    -- KONTROLL ATT RÄTT KOMBINATION AV NYCKLAR ANVÄNTS                  
078600                                                                          
078700*********** FÖR ATT KUNNA SÖKA PÅ LARMANSKAFFARE 0                        
078800*??* IF NOT EGEN-MID                                                      
078900*??*    IF  WS-IDARTNR = ZERO                                             
079000*??*    AND WS-IDANSK  = ZERO                                             
079100*??*    AND WS-KDLARM  = ZERO                                             
079200*??*        MOVE NEJ TO NYCKLAR-SW                                        
079300*??*    END-IF                                                            
079400*??* END-IF                                                               
079500                                                                          
079600*??* IF EGEN-MID                                                          
079700*??*    IF  WS-IDARTNR = ZERO                                             
079800*??*    AND WS-IDANSK  = ZERO                                             
079900*??*    AND WS-KDLARM  = ZERO                                             
080000*??*    AND MID-IDANSK-IN NOT = ALL '+'                                   
080100*??*    OR  MID-KDLARM-ENTER > 0                                          
080200*??*    OR  MID-KDLARM-NEXT > 0                                           
080300*??*       CONTINUE                                                       
080400*??*    ELSE                                                              
080500*??*       IF  WS-IDARTNR = ZERO                                          
080600*??*       AND WS-IDANSK  = ZERO                                          
080700*??*       AND WS-KDLARM  = ZERO                                          
080800*??*           MOVE NEJ TO NYCKLAR-SW                                     
080900*??*       ELSE                                                           
081000*??*          IF  WS-KDLARM > ZERO                                        
081100*??*          AND WS-IDANSK = ZERO                                        
081200*??*             MOVE NEJ TO NYCKLAR-SW                                   
081300*??*          END-IF                                                      
081400*??*       END-IF                                                         
081500*??*    END-IF                                                            
081600*??* END-IF                                                               
081700                                                                          
081800*    -- GÖR IORDNING NYCKLAR-UT                                           
081900                                                                          
082000     IF GODK-MID OR NYCKLAR-OK                                            
082020       CONTINUE                                                           
082701                                                                          
082800     ELSE                                                                 
082900       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
083000                               MOD-IDANSK-UT                              
083100                               MOD-IDLEVNR-UT                             
083200                               MOD-KDLARM-UT                              
083210                               MOD-KDOTFREK-UT                            
083300     END-IF                                                               
083400                                                                          
083500     IF NYCKLAR-OK                                                        
083600       IF W-IDARTNR = ZERO                                                
083700         CONTINUE                                                         
083800       ELSE                                                               
083900*                  --- MAN HAR VALT EN SPECIFIK ARTIKEL IN                
084000         PERFORM IMS-GU-ARTC                                              
084100         IF SEGMENT-FINNS                                                 
084200           IF WS-IDANSK = ZERO                                            
084300             PERFORM IMS-GNP-ARTC-CLAG                                    
084400             IF SEGMENT-FINNS                                             
084500               MOVE CLAG-IDANSK TO WS-IDANSK                              
084600                                  W-IDANSK-L                              
084700               PERFORM S04-HAMTA-IDANSK-LARM                              
084800               MOVE WS-IDANSK-LARM TO W-IDANSK                            
084900                                      MOD-IDANSK-UT                       
084910               INSPECT MOD-IDANSK-UT                                      
084920                 REPLACING LEADING ZERO BY SPACE                          
085000             ELSE                                                         
085100               MOVE NEJ TO NYCKLAR-SW                                     
085200             END-IF                                                       
085300           ELSE                                                           
085400             MOVE WS-IDANSK TO W-IDANSK                                   
085500           END-IF                                                         
085600         ELSE                                                             
085700           MOVE ERR-PARTNO-MISSING TO MED-IDMFSFEL                        
085800           MOVE NEJ TO NYCKLAR-SW                                         
085900         END-IF                                                           
086000       END-IF                                                             
086100       IF NOT NYCKLAR-FEL                                                 
086200         PERFORM IMS-GU-XXBU-2223                                         
086300         IF SEGMENT-FINNS                                                 
086400           CONTINUE                                                       
086500         ELSE                                                             
086600           MOVE NEJ TO NYCKLAR-SW                                         
086700           MOVE MED-1 TO MOD-TEMFSINF                                     
086800         END-IF                                                           
086900       END-IF                                                             
087000     END-IF                                                               
087100                                                                          
087200     IF NYCKLAR-FEL                                                       
087300       IF MED-IDMFSFEL NOT = ERR-PARTNO-MISSING AND                       
087400                             ERR-NOT-AUTHORIZED                           
087500         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
087600       END-IF                                                             
087700       CALL WMEDKONV USING MED-WMEDAREA                                   
087800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
087900       PERFORM MFS-RENSA-FAELT-IN                                         
088000       PERFORM MFS-RENSA-FAELT-UT                                         
088100     END-IF                                                               
088200     .                                                                    
088300     EJECT                                                                
088400 C-FOERSTA-SIDA SECTION.                                                  
088500     MOVE 'C-FOERSTA-SIDA           ' TO CURRENT-SECTION                  
088600     SKIP2                                                                
088700*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
088800     MOVE ZERO TO W-TISENBEK-DAG                                          
088900                  W-TISENBEK-KL                                           
089000                  W-KDLARM                                                
089100     MOVE JA TO ALLT-SW                                                   
089200                                                                          
089300     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
089400     CALL WMEDKONV USING MED-WMEDAREA                                     
089500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
089600                                                                          
089700     PERFORM S03-LAES-RADDATA                                             
089800     .                                                                    
089900     EJECT                                                                
090000 D-NAESTA-SIDA SECTION.                                                   
090100     MOVE 'D-NAESTA-SIDA            ' TO CURRENT-SECTION                  
090200     SKIP2                                                                
090300     MOVE MID-TISENBEK-DAG-NEXT TO W-TISENBEK-DAG                         
090400     MOVE MID-TISENBEK-KL-NEXT  TO W-TISENBEK-KL                          
090500     MOVE MID-KDLARM-NEXT       TO W-KDLARM                               
090600                                                                          
090700     IF MID-KDLARM-NEXT = ZERO AND ( MID-KDLARM-UT NOT = ZERO )           
090800       MOVE MID-KDLARM-UT       TO W-KDLARM                               
090900     END-IF                                                               
091000     MOVE JA TO ALLT-SW                                                   
091100                                                                          
091200     PERFORM IMS-GNP-XXBU-2224                                            
091300     IF SEGMENT-FINNS                                                     
091400*    -- LÄS IDLEVNR FÖR DEN FÖRSTA RADEN                                  
091500       MOVE XXBU-2224-IDARTNR TO W-IDARTNR-RAD                            
091600       PERFORM IMS-GU-ARTC-RAD                                            
091700       MOVE ART-IDLEVNR TO WS-IDLEVNR-RAD                                 
091800     END-IF                                                               
091900     .                                                                    
092000     EJECT                                                                
092100 E-SAMMA-SIDA SECTION.                                                    
092200     MOVE 'E-SAMMA-SIDA             ' TO CURRENT-SECTION                  
092300                                                                          
092400     PERFORM S02-KOLLA-IFYLLDA-FAELT                                      
092500     IF WS-INPUT = ALL '+' OR INDATA-FEL                                  
092600       CONTINUE                                                           
092700     ELSE                                                                 
092800       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
092900       CALL WMEDKONV USING MED-WMEDAREA                                   
093000       MOVE MED-MFSINF TO MOD-TEMFSFEL                                    
093100     END-IF                                                               
093200     MOVE NEJ TO ALLT-SW                                                  
093300     PERFORM MFS-ROER-EJ-FAELT-IN                                         
093400     PERFORM MFS-ROER-EJ-FAELT-UT                                         
093500     PERFORM MFS-LAS-IN-IGEN                                              
093600     .                                                                    
093700     EJECT                                                                
093800 F-LAES-VISA-INFO SECTION.                                                
093900     MOVE 'F-LAES-VISA-INFO         ' TO CURRENT-SECTION                  
094000                                                                          
094100     IF SEGMENT-FINNS                                                     
094200       MOVE XXBU-2224-TISENBEK-DAG TO MOD-TISENBEK-DAG-ENTER              
094300       MOVE XXBU-2224-TISENBEK-KL  TO MOD-TISENBEK-KL-ENTER               
094400       MOVE XXBU-2224-KDLARM       TO MOD-KDLARM-ENTER                    
094500     ELSE                                                                 
094600       MOVE ZERO                   TO MOD-TISENBEK-DAG-ENTER              
094700                                      MOD-TISENBEK-KL-ENTER               
094800                                      MOD-KDLARM-ENTER                    
094900       MOVE MED-1                  TO MOD-TEMFSINF                        
095000     END-IF                                                               
095100                                                                          
095200     PERFORM S08-FYLL-BILDEN                                              
095300     .                                                                    
095400     EJECT                                                                
095500 G-KOLLA-INPUT SECTION.                                                   
095600     MOVE 'G-KOLLA-INPUT            ' TO CURRENT-SECTION                  
095700                                                                          
095800     MOVE JA  TO INDATA-SW                                                
095900     PERFORM S02-KOLLA-IFYLLDA-FAELT                                      
096000     IF WS-INPUT = ALL '+'                                                
096100       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
096200       CALL WMEDKONV USING MED-WMEDAREA                                   
096300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
096400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
096500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
096600       MOVE NEJ TO INDATA-SW                                              
096700     ELSE                                                                 
096800*  -- FORMELL KONTROLL OCH RELATIONSKONTROLL                              
096900       MOVE +1 TO INDX                                                    
097000       PERFORM UNTIL INDX > MAX-INDX                                      
097100                                                                          
097200         IF  (MID-SELECT-ARTIKEL(INDX) NOT = ALL '+')                     
097300         AND (MID-FLNYLARM(INDX)       NOT = ALL '+')                     
097400           MOVE MFS-ALFA-FAELT-FEL TO                                     
097500                               MOD-SELECT-ARTIKEL-ATTR(INDX)              
097600                               MOD-FLNYLARM-ATTR(INDX)                    
097700           MOVE NEJ TO INDATA-SW                                          
097800         END-IF                                                           
097900                                                                          
098000         IF MID-SELECT-ARTIKEL(INDX) = ALL '+'                            
098100           CONTINUE                                                       
098200         ELSE                                                             
098300           IF MID-SELECT-ARTIKEL(INDX) = 'S'                              
098400             MOVE FEL-1 TO MOD-TEMFSINF                                   
098500             MOVE MFS-ALFA-FAELT-FEL TO                                   
098600                           MOD-SELECT-ARTIKEL-ATTR(INDX)                  
098700             MOVE NEJ TO INDATA-SW                                        
098800           ELSE                                                           
098900             IF MID-SELECT-ARTIKEL(INDX) = 'B' OR 'D'                     
099000               IF MID-KDLARM(INDX) = 100 OR 110                           
099100                 MOVE FEL-2 TO MOD-TEMFSINF                               
099200                 MOVE MFS-ALFA-FAELT-FEL TO                               
099300                         MOD-SELECT-ARTIKEL-ATTR(INDX)                    
099400                 MOVE NEJ TO INDATA-SW                                    
099500               ELSE                                                       
099600                 MOVE MFS-ALFA-FAELT-RAETT TO                             
099700                         MOD-SELECT-ARTIKEL-ATTR(INDX)                    
099800               END-IF                                                     
099900             ELSE                                                         
100000                MOVE MFS-ALFA-FAELT-FEL TO                                
100100                        MOD-SELECT-ARTIKEL-ATTR(INDX)                     
100200                MOVE NEJ TO INDATA-SW                                     
100300             END-IF                                                       
100400           END-IF                                                         
100500         END-IF                                                           
100600                                                                          
100700         IF MID-FLNYLARM(INDX) = ALL '+'                                  
100800           CONTINUE                                                       
100900         ELSE                                                             
101000           IF MID-FLNYLARM(INDX) = SPACE                                  
101100             MOVE MFS-ALFA-FAELT-RAETT TO                                 
101200                                   MOD-FLNYLARM-ATTR(INDX)                
101300           ELSE                                                           
101400             MOVE MFS-ALFA-FAELT-FEL TO                                   
101500                                   MOD-FLNYLARM-ATTR(INDX)                
101600             MOVE NEJ TO INDATA-SW                                        
101700           END-IF                                                         
101800         END-IF                                                           
101900                                                                          
102000         ADD +1 TO INDX                                                   
102100       END-PERFORM                                                        
102200                                                                          
102300*  -- KONTROLL MOT DATABAS                                                
102400       MOVE +1 TO INDX                                                    
102500       PERFORM UNTIL INDX > MAX-INDX                                      
102600         IF MID-SELECT-ARTIKEL(INDX) = 'B'                                
102700         OR MID-SELECT-ARTIKEL(INDX) = 'D'                                
102800         OR MID-FLNYLARM(INDX)       = SPACE                              
102900           MOVE MID-TISENBEK-DAG-IN(INDX) TO WS-TISENBEK-DAG-YYWWD        
103000           PERFORM S05-CONVERT-TISENBEK-DAG-IN                            
103100           MOVE MID-TISENBEK-KL-IN (INDX) TO W-TISENBEK-KL                
103200           MOVE MID-KDLARM         (INDX) TO W-KDLARM                     
103300           PERFORM IMS-GHNP-XXBU-2224-KVAL                                
103400           IF SEGMENT-FINNS                                               
103500             CONTINUE                                                     
103600           ELSE                                                           
103700             MOVE NEJ TO INDATA-SW                                        
103800           END-IF                                                         
103900         END-IF                                                           
104000         ADD +1 TO INDX                                                   
104100       END-PERFORM                                                        
104200                                                                          
104300       IF INDATA-FEL                                                      
104400         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
104500         CALL WMEDKONV USING MED-WMEDAREA                                 
104600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
104700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
104800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
104900       END-IF                                                             
105000     END-IF                                                               
105100     .                                                                    
105200     EJECT                                                                
105300 H-UPPDATERA-VISA SECTION.                                                
105400     MOVE 'C-UPPDATERA-VISA         ' TO CURRENT-SECTION                  
105500                                                                          
105600     MOVE +1 TO INDX                                                      
105700     MOVE NEJ TO ENTER-KEY-SW                                             
105800     PERFORM UNTIL INDX > MAX-INDX                                        
105900       IF MID-SELECT-ARTIKEL(INDX) = 'B' OR 'D'                           
106000         MOVE MID-TISENBEK-DAG-IN(INDX) TO WS-TISENBEK-DAG-YYWWD          
106100         PERFORM S05-CONVERT-TISENBEK-DAG-IN                              
106200         MOVE MID-TISENBEK-KL-IN (INDX) TO W-TISENBEK-KL                  
106300         MOVE MID-KDLARM         (INDX) TO W-KDLARM                       
106400         PERFORM IMS-GHNP-XXBU-2224-KVAL                                  
106500         IF NOT ENTER-KEY-IFYLLD                                          
106600           MOVE XXBU-2224-TISENBEK-DAG  TO                                
106700                                        MOD-TISENBEK-DAG-ENTER            
106800           MOVE XXBU-2224-TISENBEK-KL   TO                                
106900                                        MOD-TISENBEK-KL-ENTER             
107000           MOVE XXBU-2224-KDLARM        TO MOD-KDLARM-ENTER               
107100           MOVE JA                      TO ENTER-KEY-SW                   
107200         END-IF                                                           
107300         PERFORM IMS-DLET-XXBU                                            
107400         PERFORM HA-SAVE-ALARM                                            
107500       END-IF                                                             
107600                                                                          
107700       IF MID-FLNYLARM(INDX) = SPACE                                      
107800         MOVE MID-TISENBEK-DAG-IN(INDX) TO WS-TISENBEK-DAG-YYWWD          
107900         PERFORM S05-CONVERT-TISENBEK-DAG-IN                              
108000         MOVE MID-TISENBEK-KL-IN (INDX) TO W-TISENBEK-KL                  
108100         MOVE MID-KDLARM         (INDX) TO W-KDLARM                       
108200         PERFORM IMS-GHNP-XXBU-2224-KVAL                                  
108300         IF SEGMENT-FINNS                                                 
108400           MOVE 'N' TO XXBU-2224-FLNYLARM                                 
108500           PERFORM IMS-REPL-XXBU                                          
108600           IF NOT ENTER-KEY-IFYLLD                                        
108700             MOVE XXBU-2224-TISENBEK-DAG                                  
108800                                        TO MOD-TISENBEK-DAG-ENTER         
108900             MOVE XXBU-2224-TISENBEK-KL                                   
109000                                        TO MOD-TISENBEK-KL-ENTER          
109100             MOVE XXBU-2224-KDLARM      TO MOD-KDLARM-ENTER               
109200             MOVE JA                    TO ENTER-KEY-SW                   
109300           END-IF                                                         
109400         END-IF                                                           
109500       END-IF                                                             
109600       ADD +1 TO INDX                                                     
109700     END-PERFORM                                                          
109800                                                                          
109900     MOVE MOD-TISENBEK-DAG-ENTER TO W-TISENBEK-DAG                        
110000     MOVE MOD-TISENBEK-KL-ENTER  TO W-TISENBEK-KL                         
110100     MOVE MOD-KDLARM-ENTER       TO W-KDLARM                              
110200     PERFORM IMS-GNP-XXBU-2224                                            
110300     PERFORM S08-FYLL-BILDEN                                              
110400                                                                          
110500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
110600     CALL WMEDKONV USING MED-WMEDAREA                                     
110700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
110800     PERFORM MFS-FORM-ATTR                                                
110900     PERFORM MFS-RENSA-FAELT-IN                                           
111000     .                                                                    
111100     EJECT                                                                
111200 HA-SAVE-ALARM SECTION.                                                   
111300     MOVE XXBU-2224-TIREGDAT                                              
111400                            TO DAT-I-TIDATUM                              
111500     PERFORM S10-CONVERT-DATE-FMT                                         
111600     IF DAT-KDSVAR = ' '                                                  
111700        MOVE DAT-TIAAVV-GRP    TO WS-ALARM-AAVV                           
111800        IF WS-ALARM-AAVV  = DAGENS-AAVV                                   
111900           PERFORM HAA-LOG-WDR3                                           
112000        END-IF                                                            
112100     END-IF                                                               
112200     .                                                                    
112300     EJECT                                                                
112400 HAA-LOG-WDR3 SECTION.                                                    
112500     MOVE 'W2017100'        TO FIL-IDPGM                                  
112600     MOVE DAGENS-DATUM      TO FIL-TIREGDAT                               
112700     ACCEPT W-TIKLOCK     FROM TIME                                       
112800     IF FIL-TIKLOCK NUMERIC                                               
112810        IF W-TIKLOCK  =  FIL-TIKLOCK                                      
112900           ADD +1           TO FIL-IDSEKVNR                               
113000        ELSE                                                              
113100           MOVE W-TIKLOCK   TO FIL-TIKLOCK                                
113200           MOVE +1          TO FIL-IDSEKVNR                               
113300        END-IF                                                            
113310     ELSE                                                                 
113320        MOVE W-TIKLOCK      TO FIL-TIKLOCK                                
113330        MOVE +1             TO FIL-IDSEKVNR                               
113340     END-IF                                                               
113400     MOVE 'W214ALOG'        TO FIL-IDCPYTXT                               
113500*                                                                         
113600     MOVE XXBU-2224-TISENBEK-DAG                                          
113700                            TO DAT-I-TIDATUM                              
113800     PERFORM S10-CONVERT-DATE-FMT                                         
113900     IF DAT-KDSVAR = ' '                                                  
114000        MOVE DAT-TIAAVVD    TO ALOG-TIAAVVD                               
114100     ELSE                                                                 
114200        MOVE ZERO           TO ALOG-TIAAVVD                               
114300     END-IF                                                               
114400*                                                                         
114500     MOVE 'WDR5'            TO ALOG-IDSYSTEM                              
114600     MOVE XXBU-2224-KDLARM  TO ALOG-KDLARM                                
114700     MOVE XXBU-2224-IDARTNR TO ALOG-IDARTNR                               
114800     MOVE W-IDANSK          TO ALOG-IDANSK                                
114900     MOVE XXBU-2224-IDLEVNR TO ALOG-IDLEVNR                               
115000     MOVE XXBU-2224-IDDISTR TO ALOG-IDDISTR                               
115100     MOVE XXBU-2224-TIREGDAT                                              
115200                            TO ALOG-TIREGDAT                              
115300     MOVE ZERO              TO ALOG-TIPLANDAT                             
115400                               ALOG-KVAVIS                                
115500                               ALOG-KVAVROP                               
115600*                                                                         
115700     MOVE ALOG-W214ALOG     TO FIL-WDR301-DATA                            
115800     PERFORM IMS-ISRT-WDR301                                              
115900     .                                                                    
116000     EJECT                                                                
116100 I-KOLLA-S-MARKERING SECTION.                                             
116200     MOVE 'I-KOLLA-S-MARKERING      ' TO CURRENT-SECTION                  
116300                                                                          
116400     MOVE +1 TO INDX                                                      
116500     PERFORM UNTIL INDX > MAX-INDX OR S-MARKERAT                          
116600       IF MID-SELECT-ARTIKEL(INDX) = 'S'                                  
116700        IF MFS-SPLIT                                                      
116800         MOVE JA TO S-MARKERAT-SW                                         
116900         MOVE MID-KDLARM(INDX) TO WS-KDLARM-VAL                           
117000         MOVE INDX TO WS-VALD-RAD                                         
117100        ELSE                                                              
117200         MOVE FEL-1 TO MOD-TEMFSINF                                       
117300         MOVE MFS-ALFA-FAELT-FEL TO                                       
117400              MOD-SELECT-ARTIKEL-ATTR(INDX)                               
117500         MOVE NEJ TO INDATA-SW                                            
117600         MOVE MAX-INDX TO INDX                                            
117700         ADD  +1       TO INDX                                            
117800        END-IF                                                            
117900       ELSE                                                               
118000         ADD +1 TO INDX                                                   
118100       END-IF                                                             
118200     END-PERFORM                                                          
118300     .                                                                    
118400     EJECT                                                                
118500 K-BEHANDLA-KVAL-LARM SECTION.                                            
118600     MOVE 'K-BEHANDLA-KVAL-LARM     ' TO CURRENT-SECTION                  
118700                                                                          
118800     PERFORM S04-HAMTA-IDANSK-LARM                                        
118900     MOVE MID-TISENBEK-DAG-IN(WS-VALD-RAD) TO                             
119000                                           WS-TISENBEK-DAG-YYWWD          
119100     PERFORM S05-CONVERT-TISENBEK-DAG-IN                                  
119200     MOVE MID-TISENBEK-KL-IN(WS-VALD-RAD)  TO W-TISENBEK-KL               
119300     MOVE MID-KDLARM        (WS-VALD-RAD)  TO W-KDLARM                    
119400     PERFORM IMS-GHU-XXBU-2223-2224                                       
119500     IF SEGMENT-FINNS                                                     
119600        MOVE 'N' TO XXBU-2224-FLNYLARM                                    
119700        PERFORM IMS-REPL-XXBU                                             
119800        IF XXBU-2224-IDTRANS = '6203'                                     
119900           MOVE 'W6T202  '        TO ALT3-KDTRANS                         
120000        END-IF                                                            
120100        COMPUTE ALT3-LL  = LENGTH OF ALT3-MID-W6I20201 + 17               
120200        MOVE '2171'               TO ALT3-IDTRANS                         
120300        MOVE XXBU-2224-KDMFSFOR   TO ALT3-KDMFSFOR                        
120400        MOVE XXBU-2224-IDKR       TO ALT3-MID-IDKR-IN                     
120500        MOVE SPACE                TO ALT3-MID-IDKR-UT                     
120600        MOVE ALL '+'              TO ALT3-MID-INPUT                       
120700        PERFORM IMS-INSERT-ALT3-6202                                      
120800     END-IF                                                               
120900     .                                                                    
121000     EJECT                                                                
121100 L-BEHANDLA-2XX-LARM SECTION.                                             
121200     MOVE 'L-BEHANDLA-2XX-LARM      ' TO CURRENT-SECTION                  
121300                                                                          
121400     MOVE MID-TISENBEK-DAG-IN(WS-VALD-RAD) TO                             
121500                                           WS-TISENBEK-DAG-YYWWD          
121600     PERFORM S05-CONVERT-TISENBEK-DAG-IN                                  
121700     MOVE MID-TISENBEK-KL-IN (WS-VALD-RAD) TO W-TISENBEK-KL               
121800     MOVE MID-KDLARM         (WS-VALD-RAD) TO W-KDLARM                    
121900     PERFORM IMS-GHU-XXBU-2223-2224                                       
122000     IF SEGMENT-FINNS                                                     
122100        IF MID-KDLARM (WS-VALD-RAD) = 223                                 
122200           MOVE 'N'                  TO XXBU-2224-FLNYLARM                
122300           PERFORM IMS-REPL-XXBU                                          
122400           MOVE '2171'               TO ALT6-IDTRANS                      
122500           MOVE XXBU-2224-KDMFSFOR   TO ALT6-KDMFSFOR                     
122600           MOVE ALL '+'              TO ALT6-MID                          
122700           MOVE XXBU-2224-IDARTNR    TO ALT6-IDARTNR-IN                   
122800*          MOVE SPACE                TO ALT6-IDARTNR-UT                   
122900           PERFORM IMS-INSERT-ALT6-2103                                   
123000        ELSE                                                              
123100           MOVE 'N'                  TO XXBU-2224-FLNYLARM                
123200           PERFORM IMS-REPL-XXBU                                          
123300           MOVE '2171'               TO ALT4-IDTRANS                      
123400           MOVE XXBU-2224-KDMFSFOR   TO ALT4-KDMFSFOR                     
123500           MOVE ALL '+'              TO ALT4-MID                          
123600           MOVE XXBU-2224-IDARTNR    TO ALT4-IDARTNR-IN                   
123700*          MOVE SPACE                TO ALT4-IDARTNR-UT                   
123800           PERFORM IMS-INSERT-ALT4-2102                                   
123900        END-IF                                                            
124000     ELSE                                                                 
124100        MOVE FEL-6 TO MOD-TEMFSFEL                                        
124200        PERFORM MFS-ROER-EJ-FAELT-UT                                      
124300        PERFORM MFS-ROER-EJ-FAELT-IN                                      
124400        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O17101 + 4                     
124500        PERFORM IMS-INSERT-MSG                                            
124600     END-IF                                                               
124700     .                                                                    
124800     EJECT                                                                
124900                                                                          
125000 M-BEHANDLA-EXCH-LARM SECTION.                                            
125100     MOVE 'M-BEHANDLA-EXCH-LARM     ' TO CURRENT-SECTION                  
125200                                                                          
125300     MOVE MID-TISENBEK-DAG-IN(WS-VALD-RAD) TO                             
125400                                           WS-TISENBEK-DAG-YYWWD          
125500     PERFORM S05-CONVERT-TISENBEK-DAG-IN                                  
125600     MOVE MID-TISENBEK-KL-IN (WS-VALD-RAD) TO W-TISENBEK-KL               
125700     MOVE MID-KDLARM         (WS-VALD-RAD) TO W-KDLARM                    
125800     PERFORM IMS-GHU-XXBU-2223-2224                                       
125900     IF SEGMENT-FINNS                                                     
126000        MOVE 'N' TO XXBU-2224-FLNYLARM                                    
126100        PERFORM IMS-REPL-XXBU                                             
126200        MOVE '2171'               TO ALT5-IDTRANS                         
126300        MOVE 'N'                  TO ALT5-KDMFSFOR                        
126400        MOVE ALL '+'              TO ALT5-MID                             
126500        MOVE XXBU-2224-IDARTNR    TO WS-IDARTNR-8                         
126600        MOVE WS-IDARTNR-8         TO ALT5-MID-IDARTNR-IN                  
126700        PERFORM IMS-INSERT-ALT5-3165                                      
126800     ELSE                                                                 
126900        MOVE FEL-6 TO MOD-TEMFSFEL                                        
127000        PERFORM MFS-ROER-EJ-FAELT-UT                                      
127100        PERFORM MFS-ROER-EJ-FAELT-IN                                      
127200        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O17101 + 4                     
127300        PERFORM IMS-INSERT-MSG                                            
127400     END-IF                                                               
127500     .                                                                    
127600     EJECT                                                                
127700 N-BEHANDLA-1XX-LARM SECTION.                                             
127800     MOVE 'N-BEHANDLA-1XX-LARM      ' TO CURRENT-SECTION                  
127900                                                                          
128000     MOVE MID-TISENBEK-DAG-IN(WS-VALD-RAD) TO                             
128100                                           WS-TISENBEK-DAG-YYWWD          
128200     PERFORM S05-CONVERT-TISENBEK-DAG-IN                                  
128300     MOVE MID-TISENBEK-KL-IN (WS-VALD-RAD) TO W-TISENBEK-KL               
128400     MOVE MID-KDLARM         (WS-VALD-RAD) TO W-KDLARM                    
128500     PERFORM IMS-GHU-XXBU-2223-2224                                       
128600     IF SEGMENT-FINNS                                                     
128700        MOVE 'N' TO XXBU-2224-FLNYLARM                                    
128800        PERFORM IMS-REPL-XXBU                                             
128900     END-IF                                                               
129000                                                                          
129100     PERFORM IMS-INSERT-ALT-2126                                          
129200     .                                                                    
129300     EJECT                                                                
129400 O-BEHANDLA-150-LARM SECTION.                                             
129500     MOVE 'O-BEHANDLA-150-LARM      ' TO CURRENT-SECTION                  
129600                                                                          
129700     MOVE MID-TISENBEK-DAG-IN(WS-VALD-RAD) TO                             
129800                                           WS-TISENBEK-DAG-YYWWD          
129900     PERFORM S05-CONVERT-TISENBEK-DAG-IN                                  
130000     MOVE MID-TISENBEK-KL-IN(WS-VALD-RAD)  TO W-TISENBEK-KL               
130100     MOVE MID-KDLARM        (WS-VALD-RAD)  TO W-KDLARM                    
130200     PERFORM IMS-GHU-XXBU-2223-2224                                       
130300     IF SEGMENT-FINNS                                                     
130400        MOVE 'N' TO XXBU-2224-FLNYLARM                                    
130500        PERFORM IMS-REPL-XXBU                                             
130600     END-IF                                                               
130700                                                                          
130800     PERFORM IMS-INSERT-ALT1-2127                                         
130900     .                                                                    
131000     EJECT                                                                
131100                                                                          
131200 P-BEHANDLA-761-LARM SECTION.                                             
131300     MOVE 'P-BEHANDLA-761-LARM      ' TO CURRENT-SECTION                  
131400                                                                          
131500     MOVE MID-TISENBEK-DAG-IN(WS-VALD-RAD) TO                             
131600                                           WS-TISENBEK-DAG-YYWWD          
131700     PERFORM S05-CONVERT-TISENBEK-DAG-IN                                  
131800     MOVE MID-TISENBEK-KL-IN(WS-VALD-RAD)  TO W-TISENBEK-KL               
131900     MOVE MID-KDLARM        (WS-VALD-RAD)  TO W-KDLARM                    
132000     PERFORM IMS-GHU-XXBU-2223-2224                                       
132100     IF SEGMENT-FINNS                                                     
132200        MOVE 'N' TO XXBU-2224-FLNYLARM                                    
132300        PERFORM IMS-REPL-XXBU                                             
132400     END-IF                                                               
132500                                                                          
132600     PERFORM IMS-INSERT-ALT7-2114                                         
132700     .                                                                    
132800     EJECT                                                                
132900                                                                          
133000 S01-FLYTTA-TILL-MOD SECTION.                                             
133100     MOVE 'S01-FLYTTA-TILL-MOD      ' TO CURRENT-SECTION                  
133200                                                                          
133300     IF XXBU-2224-KDLARM = 100 OR 110 OR 150 OR 223                       
133400        MOVE MFS-FORMATETS-ATTR TO MOD-TISENBEK-DAG-UT-ATTR(INDX)         
133500     ELSE                                                                 
133600        MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                 
133700                              MOD-TISENBEK-DAG-UT-ATTR(INDX)              
133800     END-IF                                                               
133900                                                                          
134000     IF XXBU-2224-KDLARM = 100 OR 110 OR 150                              
134100        MOVE MFS-FORMATETS-ATTR TO MOD-TISENBEK-KL-UT-ATTR(INDX)          
134200     ELSE                                                                 
134300        MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                 
134400                              MOD-TISENBEK-KL-UT-ATTR(INDX)               
134500     END-IF                                                               
134600                                                                          
134700     IF XXBU-2224-TISENBEK-DAG > +0                                       
134800        MOVE XXBU-2224-TISENBEK-DAG TO WS-TISENBEK-DAG-YYMMDD             
134900        MOVE WS-TISENBEK-DAG-YYMMDD TO DAYS-TIDATE1                       
135000        MOVE 'YYMMDD'            TO DAYS-KDDATFMT1                        
135100        MOVE 'YYWWD'             TO DAYS-KDDATFMT2                        
135200        MOVE 0                   TO DAYS-KVDAYS                           
135300        MOVE SPACE               TO DAYS-TIDATE2                          
135400                                    DAYS-IDCALEND                         
135500        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
135600                                                                          
135700        IF DAYS-KDRC = 8                                                  
135800          MOVE ZERO              TO MOD-TISENBEK-DAG-UT(INDX)             
135900        ELSE                                                              
136000          MOVE DAYS-TIDATE2(1:5) TO MOD-TISENBEK-DAG-UT(INDX)             
136100        END-IF                                                            
136200     END-IF                                                               
136300                                                                          
136400     MOVE XXBU-2224-TISENBEK-KL  TO MOD-TISENBEK-KL-UT(INDX)              
136500     MOVE XXBU-2224-KDLARM       TO MOD-KDLARM(INDX)                      
136600     MOVE XXBU-2224-IDARTNR      TO MOD-IDARTNR(INDX)                     
136700     MOVE WS-IDLEVNR-RAD         TO MOD-IDLEVNR(INDX)                     
136800     MOVE XXBU-2224-IDDISTR      TO MOD-IDDISTR(INDX)                     
136900     MOVE XXBU-2224-TIREGDAT     TO MOD-TIREGDAT(INDX)                    
137000                                                                          
137100     IF XXBU-2224-FLNYLARM = JA                                           
137200       MOVE '*'                  TO MOD-FLNYLARM(INDX)                    
137300     ELSE                                                                 
137400       MOVE MFS-RENSA-FAELT      TO MOD-FLNYLARM(INDX)                    
137500     END-IF                                                               
137600***                                                                       
137700***  IF INCLUDING NEW ALARM,KDLARM, MESSAGES HERE, INCLUDE                
137800***  TEORSLRM MESSAGES IN THE PROGRAM WXTR2100 ALSO                       
137900***                                                                       
138000     EVALUATE XXBU-2224-KDLARM                                            
138100        WHEN 100                                                          
138200           MOVE TEORSLRM-1  (SPIND) TO MOD-TEORSLRM(INDX)                 
138300        WHEN 110                                                          
138400           MOVE TEORSLRM-1  (SPIND) TO MOD-TEORSLRM(INDX)                 
138500        WHEN 150                                                          
138600           MOVE TEORSLRM-2  (SPIND) TO MOD-TEORSLRM(INDX)                 
138700        WHEN 200                                                          
138800           MOVE TEORSLRM-6  (SPIND) TO MOD-TEORSLRM(INDX)                 
138900        WHEN 210                                                          
139000           MOVE TEORSLRM-7  (SPIND) TO MOD-TEORSLRM(INDX)                 
139100        WHEN 221                                                          
139200           MOVE TEORSLRM-9  (SPIND) TO MOD-TEORSLRM(INDX)                 
139300        WHEN 222                                                          
139400           MOVE TEORSLRM-10 (SPIND) TO MOD-TEORSLRM(INDX)                 
139500        WHEN 223                                                          
139600           MOVE TEORSLRM-223(SPIND) TO MOD-TEORSLRM(INDX)                 
139700        WHEN 240                                                          
139800           MOVE TEORSLRM-240(SPIND) TO MOD-TEORSLRM(INDX)                 
139900        WHEN 300                                                          
140000           MOVE TEORSLRM-3  (SPIND) TO MOD-TEORSLRM(INDX)                 
140100        WHEN 301                                                          
140200           MOVE TEORSLRM-4  (SPIND) TO MOD-TEORSLRM(INDX)                 
140300        WHEN 302                                                          
140400           MOVE TEORSLRM-5  (SPIND) TO MOD-TEORSLRM(INDX)                 
140500        WHEN 400                                                          
140600           MOVE TEORSLRM-8  (SPIND) TO MOD-TEORSLRM(INDX)                 
140700        WHEN 500                                                          
140800           MOVE TEORSLRM-11 (SPIND) TO MOD-TEORSLRM(INDX)                 
140900        WHEN 600                                                          
141000           MOVE TEORSLRM-12 (SPIND) TO MOD-TEORSLRM(INDX)                 
141100        WHEN 601                                                          
141200           MOVE TEORSLRM-601(SPIND) TO MOD-TEORSLRM(INDX)                 
141300        WHEN 610                                                          
141400           MOVE TEORSLRM-610(SPIND) TO MOD-TEORSLRM(INDX)                 
141500        WHEN 708                                                          
141600           MOVE TEORSLRM-708(SPIND) TO MOD-TEORSLRM(INDX)                 
141700        WHEN 710                                                          
141800           MOVE TEORSLRM-710(SPIND) TO MOD-TEORSLRM(INDX)                 
141900        WHEN 712                                                          
142000           MOVE TEORSLRM-712(SPIND) TO MOD-TEORSLRM(INDX)                 
142100        WHEN 720                                                          
142200           MOVE TEORSLRM-720(SPIND) TO MOD-TEORSLRM(INDX)                 
142300        WHEN 761                                                          
142400           MOVE TEORSLRM-761(SPIND) TO MOD-TEORSLRM(INDX)                 
142500        WHEN 777                                                          
142600           MOVE TEORSLRM-777(SPIND) TO MOD-TEORSLRM(INDX)                 
142700        WHEN 999                                                          
142800           MOVE TEORSLRM-999(SPIND) TO MOD-TEORSLRM(INDX)                 
142900        WHEN OTHER                                                        
143000            MOVE MFS-RENSA-FAELT    TO MOD-TEORSLRM(INDX)                 
143100     END-EVALUATE                                                         
143102                                                                          
143199     MOVE CLAG-KDOTFREK          TO MOD-KDOTFREK(INDX)                    
143200     .                                                                    
143300     EJECT                                                                
143400 S02-KOLLA-IFYLLDA-FAELT SECTION.                                         
143500     MOVE 'S02-KOLLA-IFYLLDA-FAELT  ' TO CURRENT-SECTION                  
143600                                                                          
143700     MOVE +1 TO INDX                                                      
143800     PERFORM UNTIL INDX > MAX-INDX OR WS-INPUT = SPACE                    
143900       IF  MID-SELECT-ARTIKEL(INDX) = ALL '+'                             
144000       AND MID-FLNYLARM(INDX)       = ALL '+'                             
144100         CONTINUE                                                         
144200       ELSE                                                               
144300         MOVE SPACE TO WS-INPUT                                           
144400       END-IF                                                             
144500       ADD +1 TO INDX                                                     
144600     END-PERFORM                                                          
144700     .                                                                    
144800     EJECT                                                                
144900 S03-LAES-RADDATA SECTION.                                                
145000     MOVE 'S03-LAES-RADDATA         ' TO CURRENT-SECTION                  
145100     SKIP2                                                                
145200     IF WS-IDARTNR > '000000000'                                          
145300*                                 LÄS LARM VIA SÖKNYCKEL IDARTNR          
145400       PERFORM IMS-GNP-XXBU-2224-IDART                                    
145500       IF SEGMENT-FINNS                                                   
145600         MOVE XXBU-2224-IDARTNR TO W-IDARTNR-RAD                          
145700         PERFORM IMS-GU-ARTC-RAD                                          
145800         MOVE ART-IDLEVNR TO WS-IDLEVNR-RAD                               
145900       END-IF                                                             
146000     ELSE                                                                 
146100       IF WS-IDANSK > ZERO                                                
146200         IF WS-KDLARM > ZERO                                              
146300*                               LÄS LARM VIA NYCKEL KDLARM                
146400           PERFORM S03A-LAES-XXBU-LARM                                    
146500         ELSE                                                             
146600*                                 LÄS ALLA SEGMENT                        
146700           PERFORM S03B-LAES-XXBU-OKVAL                                   
146800         END-IF                                                           
146900       ELSE                                                               
147000*??*     IF NOT EGEN-MID                                                  
147100*??*       PERFORM IMS-GNP-XXBU-2224-OKVAL                                
147200*??*       IF SEGMENT-FINNS                                               
147300*??*         MOVE XXBU-2224-IDARTNR TO W-IDARTNR-RAD                      
147400*??*         PERFORM IMS-GU-ARTC-RAD                                      
147500*??*         MOVE ART-IDLEVNR TO WS-IDLEVNR-RAD                           
147600*??*       END-IF                                                         
147700*??*     ELSE                                                             
147800           IF WS-KDLARM > ZERO                                            
147900*                                 LÄS LARM VIA NYCKEL KDLARM              
148000             PERFORM S03A-LAES-XXBU-LARM                                  
148100           ELSE                                                           
148200*                                 LÄS ALLA SEGMENT                        
148300             PERFORM S03B-LAES-XXBU-OKVAL                                 
148400           END-IF                                                         
148500*??*     END-IF                                                           
148600       END-IF                                                             
148700     END-IF                                                               
148701                                                                          
148702     IF SEGMENT-FINNS                                                     
148740        PERFORM IMS-GNP-ARTC-CLAG                                         
148750        IF SEGMENT-FINNS                                                  
148760           CONTINUE                                                       
148770        ELSE                                                              
148771           CALL FELLOG                                                    
148772        END-IF                                                            
148773     END-IF                                                               
148800     .                                                                    
148900     EJECT                                                                
149000 S03A-LAES-XXBU-LARM  SECTION.                                            
149100     MOVE 'S03A-LAES-XXBU-LARM      ' TO CURRENT-SECTION                  
149200*                               LÄS NÄSTA LARM VIA NYCKEL KDLARM          
149300     PERFORM IMS-GNP-XXBU-2224-LARM                                       
149400     IF SEGMENT-FINNS                                                     
149500       MOVE XXBU-2224-IDARTNR TO W-IDARTNR-RAD                            
149600       PERFORM IMS-GU-ARTC-RAD                                            
149700                                                                          
149800       IF WS-IDLEVNR-NUM NOT = SPACE                                      
149900*                               MAN VILL BARA SE VISST LEVNR              
150000         PERFORM UNTIL ART-IDLEVNR = WS-IDLEVNR-NUM                       
150100                 OR SEGMENT-SAKNAS                                        
150200           PERFORM IMS-GNP-XXBU-2224-LARM                                 
150300           IF SEGMENT-FINNS                                               
150400             MOVE XXBU-2224-IDARTNR TO W-IDARTNR-RAD                      
150500             PERFORM IMS-GU-ARTC-RAD                                      
150600           END-IF                                                         
150700         END-PERFORM                                                      
150800       END-IF                                                             
150900       MOVE ART-IDLEVNR TO WS-IDLEVNR-RAD                                 
151000     END-IF                                                               
151100     .                                                                    
151200     EJECT                                                                
151300 S03B-LAES-XXBU-OKVAL  SECTION.                                           
151400     MOVE 'S03B-LAES-XXBU-OKVAL     ' TO CURRENT-SECTION                  
151500     SKIP2                                                                
151600     PERFORM IMS-GNP-XXBU-2224-OKVAL                                      
151700*                          LÄS ALLA SEGMENT                               
151800     IF SEGMENT-FINNS                                                     
151900       MOVE XXBU-2224-IDARTNR TO W-IDARTNR-RAD                            
152000       PERFORM IMS-GU-ARTC-RAD                                            
152100                                                                          
152200       IF WS-IDLEVNR-NUM NOT = SPACE                                      
152300*                         BARA VISA ARTIKLAR MED VISST IDLEVNR            
152400         PERFORM UNTIL ART-IDLEVNR = WS-IDLEVNR-NUM                       
152500                 OR SEGMENT-SAKNAS                                        
152600           PERFORM IMS-GNP-XXBU-2224-OKVAL                                
152700           IF SEGMENT-FINNS                                               
152800             MOVE XXBU-2224-IDARTNR TO W-IDARTNR-RAD                      
152900             PERFORM IMS-GU-ARTC-RAD                                      
153000           END-IF                                                         
153100         END-PERFORM                                                      
153200       END-IF                                                             
153300       MOVE ART-IDLEVNR TO WS-IDLEVNR-RAD                                 
153400     END-IF                                                               
153500     .                                                                    
153600     EJECT                                                                
153700 S04-HAMTA-IDANSK-LARM SECTION.                                           
153800     MOVE 'S04-HAMTA-IDANSK-LARM    ' TO CURRENT-SECTION                  
153900                                                                          
154000     PERFORM IMS-GET-XXBX-2231                                            
154100                                                                          
154200     PERFORM IMS-GET-XXBX-2232                                            
154300     IF SEGMENT-FINNS                                                     
154400       MOVE XXBX-2232-IDANSK-LARM TO WS-IDANSK-LARM                       
154500     ELSE                                                                 
154600       MOVE ZERO                  TO WS-IDANSK-LARM                       
154700     END-IF                                                               
154800     .                                                                    
154900     EJECT                                                                
155000 S05-CONVERT-TISENBEK-DAG-IN SECTION.                                     
155100     MOVE 'S05-CONVERT-TISENBEK-DAG-IN' TO CURRENT-SECTION                
155200                                                                          
155300     MOVE 'YYWWD'                  TO DAYS-KDDATFMT1                      
155400     MOVE 'YYMMDD'                 TO DAYS-KDDATFMT2                      
155500     MOVE WS-TISENBEK-DAG-YYWWD    TO DAYS-TIDATE1                        
155600     MOVE 0                        TO DAYS-KVDAYS                         
155700     MOVE SPACE                    TO DAYS-TIDATE2                        
155800                                      DAYS-IDCALEND                       
155900     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
156000                                                                          
156100     IF DAYS-KDRC = 8                                                     
156200       MOVE ZERO                   TO W-TISENBEK-DAG                      
156300     ELSE                                                                 
156400       MOVE DAYS-TIDATE2(1:6)      TO W-TISENBEK-DAG                      
156500     END-IF                                                               
156600     .                                                                    
156700     EJECT                                                                
156800 S08-FYLL-BILDEN SECTION.                                                 
156900     MOVE 'S08-FYLL-BILDEN          ' TO CURRENT-SECTION                  
157000     SKIP2                                                                
157100*                                                                         
157200     MOVE +1 TO INDX                                                      
157300     PERFORM UNTIL INDX > MAX-INDX                                        
157400             OR SEGMENT-SAKNAS                                            
157410                                                                          
157420       IF WS-KDOTFREK = CLAG-KDOTFREK OR SPACE                            
157500***      -- KOLLA OM ARTIKELINFO FÅR VISAS                                
157600         MOVE WS-IDLEVNR-RAD TO WS-IDLEVNR-8                              
157700         PERFORM S2-SECURITY-CHECK-SUPPLIER                               
157800                                                                          
157900         IF PASSED-SECURITY-CHECK                                         
158000           PERFORM S01-FLYTTA-TILL-MOD                                    
158100           MOVE MFS-RENSA-FAELT TO MOD-SELECT-ARTIKEL(INDX)               
158200           ADD +1 TO INDX                                                 
158300         ELSE                                                             
158400***          --- USER NOT AUTHORIZED TO SEE THIS DATA                     
158500             MOVE FEL-7              TO MOD-TEMFSFEL                      
158600         END-IF                                                           
158610       END-IF                                                             
158700       PERFORM S03-LAES-RADDATA                                           
158800                                                                          
158900     END-PERFORM                                                          
159000                                                                          
159100     IF SEGMENT-SAKNAS                                                    
159200*      --- BLANKA SKÄRMRADER SOM INTE FÅTT NÅGON OUTPUT                   
159300       PERFORM UNTIL INDX > MAX-INDX                                      
159400         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
159500         ADD +1 TO INDX                                                   
159600       END-PERFORM                                                        
159700     END-IF                                                               
159800                                                                          
159900     IF SEGMENT-FINNS                                                     
160000       MOVE XXBU-2224-TISENBEK-DAG TO WS-TISENBEK-DAG                     
160100       MOVE WS-TISENBEK-DAG        TO MOD-TISENBEK-DAG-NEXT               
160200       MOVE XXBU-2224-TISENBEK-KL  TO WS-TISENBEK-KL                      
160300       MOVE WS-TISENBEK-KL         TO MOD-TISENBEK-KL-NEXT                
160400       MOVE XXBU-2224-KDLARM       TO WS-KDLARM-NUM                       
160500       MOVE WS-KDLARM-NUM          TO MOD-KDLARM-NEXT                     
160600       IF MFS-PRINT OR MFS-UPDATE                                         
160700         CONTINUE                                                         
160800       ELSE                                                               
160900         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
161000         CALL WMEDKONV USING MED-WMEDAREA                                 
161100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
161200       END-IF                                                             
161300     ELSE                                                                 
161400       MOVE ZERO             TO MOD-TISENBEK-DAG-NEXT                     
161500                                MOD-TISENBEK-KL-NEXT                      
161600                                MOD-KDLARM-NEXT                           
161700     END-IF                                                               
161800     .                                                                    
161900     EJECT                                                                
162000                                                                          
162100 S09-VISA-EFTER-FELAKTIGT-VAL SECTION.                                    
162200     MOVE 'S09-VISA-EFTER-FELAKTIGT-VAL' TO CURRENT-SECTION               
162300                                                                          
162400     MOVE FEL-5 TO MOD-TEMFSFEL                                           
162500     PERFORM MFS-ROER-EJ-FAELT-UT                                         
162600     PERFORM MFS-ROER-EJ-FAELT-IN                                         
162700     .                                                                    
162800     EJECT                                                                
162900                                                                          
163000 S10-CONVERT-DATE-FMT SECTION.                                            
163100     MOVE 'S10-CONVERT-DATE-FMT' TO CURRENT-SECTION                       
163200                                                                          
163300     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
163400     CALL WDATKONV       USING DAT-KDDATFORM                              
163500                               DAT-I-TIDATUM                              
163600                               DAT-O-TIDATUM                              
163700                               DAT-KDSVAR                                 
163800     .                                                                    
163900     EJECT                                                                
164000                                                                          
164100 S2-SECURITY-CHECK-SUPPLIER SECTION.                                      
164200     MOVE 'S2-SECURITY-CHECK-SUPPLIER' TO CURRENT-SECTION                 
164300     SKIP2                                                                
164400*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
164500                                                                          
164600     IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                            
164700     OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                      
164800*      --- BEHÖRIG USER                                                   
164900       SET PASSED-SECURITY-CHECK TO TRUE                                  
165000     ELSE                                                                 
165100       SET BLOCKED-SECURITY-CHECK TO TRUE                                 
165200     END-IF                                                               
165300     .                                                                    
165400     EJECT                                                                
165500                                                                          
165600 MFS-RENSA-FAELT-UT SECTION.                                              
165700                                                                          
165800*    --- ALLA UTDATA-FÄLT                                                 
165900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
166000     MOVE MFS-RENSA-FAELT TO MOD-TISENBEK-DAG-ENTER                       
166100                             MOD-TISENBEK-KL-ENTER                        
166200                             MOD-KDLARM-ENTER                             
166300                             MOD-TISENBEK-DAG-NEXT                        
166400                             MOD-TISENBEK-KL-NEXT                         
166500                             MOD-KDLARM-NEXT                              
166600     MOVE +1 TO INDX                                                      
166700     PERFORM UNTIL INDX > MAX-INDX                                        
166800       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
166900       ADD +1 TO INDX                                                     
167000     END-PERFORM                                                          
167100     .                                                                    
167200     EJECT                                                                
167300 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
167400                                                                          
167500*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
167600     MOVE MFS-RENSA-FAELT TO MOD-SELECT-ARTIKEL(INDX)                     
167700                             MOD-FLNYLARM(INDX)                           
167800                             MOD-KDLARM(INDX)                             
167900                             MOD-IDARTNR(INDX)                            
168000                             MOD-IDLEVNR(INDX)                            
168100                             MOD-TISENBEK-DAG-UT(INDX)                    
168200                             MOD-TISENBEK-KL-UT(INDX)                     
168300                             MOD-IDDISTR(INDX)                            
168400                             MOD-TEORSLRM(INDX)                           
168500                             MOD-TIREGDAT(INDX)                           
168600     .                                                                    
168700     EJECT                                                                
168800 MFS-RENSA-FAELT-IN SECTION.                                              
168900                                                                          
169000*    --- ALLA INDATA-FÄLT                                                 
169100     MOVE +1 TO INDX                                                      
169200     PERFORM UNTIL INDX > MAX-INDX                                        
169300       MOVE MFS-RENSA-FAELT TO MOD-SELECT-ARTIKEL(INDX)                   
169400       ADD +1 TO INDX                                                     
169500     END-PERFORM                                                          
169600     .                                                                    
169700     EJECT                                                                
169800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
169900                                                                          
170000*    --- ALLA UTDATA-FÄLT                                                 
170100*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
170200     MOVE MFS-ROER-EJ-FAELT TO MOD-TISENBEK-DAG-ENTER                     
170300                               MOD-TISENBEK-KL-ENTER                      
170400                               MOD-KDLARM-ENTER                           
170500                               MOD-TISENBEK-DAG-NEXT                      
170600                               MOD-TISENBEK-KL-NEXT                       
170700                               MOD-KDLARM-NEXT                            
170800                                                                          
170900     MOVE +1 TO INDX                                                      
171000     PERFORM UNTIL INDX > MAX-INDX                                        
171100       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
171200       ADD +1 TO INDX                                                     
171300     END-PERFORM                                                          
171400     .                                                                    
171500     EJECT                                                                
171600 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
171700                                                                          
171800*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
171900     MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT-ARTIKEL(INDX)                   
172000                               MOD-FLNYLARM(INDX)                         
172100                               MOD-KDLARM(INDX)                           
172200                               MOD-IDARTNR(INDX)                          
172300                               MOD-IDLEVNR(INDX)                          
172400                               MOD-TISENBEK-DAG-UT(INDX)                  
172500                               MOD-TISENBEK-KL-UT(INDX)                   
172600                               MOD-IDDISTR(INDX)                          
172700                               MOD-TEORSLRM(INDX)                         
172800                               MOD-KDOTFREK(INDX)                         
172810                               MOD-TIREGDAT(INDX)                         
172900                                                                          
173000     IF MID-KDLARM (INDX) = 100 OR 150 OR 223                             
173100        MOVE MFS-FORMATETS-ATTR TO                                        
173200                          MOD-TISENBEK-DAG-UT-ATTR(INDX)                  
173300     ELSE                                                                 
173400        MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                 
173500                          MOD-TISENBEK-DAG-UT-ATTR(INDX)                  
173600     END-IF                                                               
173700                                                                          
173800     IF MID-KDLARM (INDX) = 100 OR 150                                    
173900        MOVE MFS-FORMATETS-ATTR TO                                        
174000                          MOD-TISENBEK-KL-UT-ATTR(INDX)                   
174100     ELSE                                                                 
174200        MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                 
174300                          MOD-TISENBEK-KL-UT-ATTR(INDX)                   
174400     END-IF                                                               
174500     .                                                                    
174600     EJECT                                                                
174700 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
174800                                                                          
174900*    --- ALLA INDATA-FÄLT                                                 
175000     MOVE +1 TO INDX                                                      
175100     PERFORM UNTIL INDX > MAX-INDX                                        
175200       MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT-ARTIKEL(INDX)                 
175300       ADD +1 TO INDX                                                     
175400     END-PERFORM                                                          
175500     .                                                                    
175600     EJECT                                                                
175700 MFS-FORM-ATTR SECTION.                                                   
175800                                                                          
175900*    --- ALLA INDATA-FÄLT                                                 
176000     MOVE +1 TO INDX                                                      
176100     PERFORM UNTIL INDX > MAX-INDX                                        
176200        MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ARTIKEL-ATTR(INDX)          
176300                                   MOD-FLNYLARM-ATTR(INDX)                
176400        ADD +1 TO INDX                                                    
176500     END-PERFORM                                                          
176600     .                                                                    
176700     EJECT                                                                
176800 MFS-LAS-IN-IGEN SECTION.                                                 
176900                                                                          
177000*    --- ALLA INDATA-FÄLT                                                 
177100     MOVE +1 TO INDX                                                      
177200     PERFORM UNTIL INDX > MAX-INDX                                        
177300        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
177400                                   MOD-SELECT-ARTIKEL-ATTR(INDX)          
177500        IF MID-FLNYLARM(INDX) = ALL '+'                                   
177600          MOVE MFS-FORMATETS-ATTR    TO MOD-FLNYLARM-ATTR(INDX)           
177700        ELSE                                                              
177800          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLNYLARM-ATTR(INDX)           
177900        END-IF                                                            
178000        ADD +1 TO INDX                                                    
178100     END-PERFORM                                                          
178200     .                                                                    
178300     EJECT                                                                
178400* --- IMS-SEKTIONER ---                                                   
178500                                                                          
178600 IMS-GET-MSG SECTION.                                                     
178700                                                                          
178800     MOVE '  QC' TO GODK-STATUSKODER                                      
178900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
179000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
179100     PERFORM IMS-STATUSKONTROLL                                           
179200     .                                                                    
179300                                                                          
179400                                                                          
179500 IMS-INSERT-MSG SECTION.                                                  
179600                                                                          
179700     IF ENGLISH-TEXT                                                      
179800       MOVE 'N' TO MFS-KDHUVOMR                                           
179900     END-IF                                                               
180000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
180100     MOVE SPACE TO GODK-STATUSKODER                                       
180200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
180300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
180400     PERFORM IMS-STATUSKONTROLL                                           
180500     .                                                                    
180600     EJECT                                                                
180700 IMS-GU-ARTC       SECTION.                                               
180800     MOVE 'IMS-GU-ARTC              ' TO CURRENT-IMS-SECTION              
180900                                                                          
181000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
181100          DELIMITED BY SIZE INTO SSA1                                     
181200     MOVE '  GE' TO GODK-STATUSKODER                                      
181300     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-WDK601  SSA1                  
181400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
181500     PERFORM IMS-STATUSKONTROLL                                           
181600     .                                                                    
181700 IMS-GU-ARTC-RAD   SECTION.                                               
181800     MOVE 'IMS-GU-ARTC-RAD          ' TO CURRENT-IMS-SECTION              
181900                                                                          
182000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-RAD-X ')'                     
182100          DELIMITED BY SIZE INTO SSA1                                     
182200     MOVE '  ' TO GODK-STATUSKODER                                        
182300     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-WDK601  SSA1                  
182400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
182500     PERFORM IMS-STATUSKONTROLL                                           
182600     .                                                                    
182700                                                                          
182800                                                                          
182900 IMS-GNP-ARTC-CLAG SECTION.                                               
183000     MOVE 'IMS-GNP-ARTC-CLAG         ' TO CURRENT-IMS-SECTION             
183100                                                                          
183200     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
183300          DELIMITED BY SIZE INTO SSA1                                     
183400     MOVE '  GE' TO GODK-STATUSKODER                                      
183500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WDK611 SSA1                   
183600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
183700     PERFORM IMS-STATUSKONTROLL                                           
183800     .                                                                    
183900     EJECT                                                                
184000 IMS-GU-XXBU-2223 SECTION.                                                
184100     MOVE 'IMS-GU-XXBU-2223          ' TO CURRENT-IMS-SECTION             
184200                                                                          
184300     STRING 'WLXXBU01(WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
184400          DELIMITED BY SIZE INTO SSA1                                     
184500     MOVE '  GE' TO GODK-STATUSKODER                                      
184600     CALL CBLTDLI USING GU XXBU-PCB DLI-IO-AREA SSA1                      
184700     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
184800     PERFORM IMS-STATUSKONTROLL                                           
184900     .                                                                    
185000                                                                          
185100                                                                          
185200 IMS-GNP-XXBU-2224 SECTION.                                               
185300     MOVE 'IMS-GNP-XXBU-2224         ' TO CURRENT-IMS-SECTION             
185400                                                                          
185500     STRING 'WLXXBU11*F(WDGXKEY >=' W-WDGXKEY-2224-X                      
185600                      '&IDDC     =' W-IDDC-X ')'                          
185700          DELIMITED BY SIZE INTO SSA1                                     
185800     MOVE '  GE' TO GODK-STATUSKODER                                      
185900     CALL CBLTDLI USING GNP XXBU-PCB DLI-IO-AREA SSA1                     
186000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
186100     PERFORM IMS-STATUSKONTROLL                                           
186200     .                                                                    
186300                                                                          
186400 IMS-GHNP-XXBU-2224-KVAL SECTION.                                         
186500     MOVE 'IMS-GHNP-XXBU-2224-KVAL   ' TO CURRENT-IMS-SECTION             
186600                                                                          
186700     STRING 'WLXXBU11*F(WDGXKEY  =' W-WDGXKEY-2224-X                      
186800                      '&IDDC     =' W-IDDC-X ')'                          
186900          DELIMITED BY SIZE INTO SSA1                                     
187000     MOVE '  GE' TO GODK-STATUSKODER                                      
187100     CALL CBLTDLI USING GHNP XXBU-PCB DLI-IO-AREA SSA1                    
187200     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
187300     PERFORM IMS-STATUSKONTROLL                                           
187400     .                                                                    
187500     EJECT                                                                
187600 IMS-GNP-XXBU-2224-OKVAL SECTION.                                         
187700     MOVE 'IMS-GNP-XXBU-2224-OKVAL   ' TO CURRENT-IMS-SECTION             
187800                                                                          
187900     STRING 'WLXXBU11(IDDC     =' W-IDDC-X ')'                            
188000          DELIMITED BY SIZE INTO SSA1                                     
188100     MOVE '  GE' TO GODK-STATUSKODER                                      
188200     CALL CBLTDLI USING GNP XXBU-PCB DLI-IO-AREA SSA1                     
188300     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
188400     PERFORM IMS-STATUSKONTROLL                                           
188500     .                                                                    
188600                                                                          
188700 IMS-GNP-XXBU-2224-LARM  SECTION.                                         
188800     MOVE 'IMS-GNP-XXBU-2224-LARM    ' TO CURRENT-IMS-SECTION             
188900                                                                          
189000     STRING 'WLXXBU11(KDLARM   =' W-KDLARM-X                              
189100                    '&IDDC     =' W-IDDC-X ')'                            
189200          DELIMITED BY SIZE INTO SSA1                                     
189300     MOVE '  GE' TO GODK-STATUSKODER                                      
189400     CALL CBLTDLI USING GNP XXBU-PCB DLI-IO-AREA SSA1                     
189500     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
189600     PERFORM IMS-STATUSKONTROLL                                           
189700     .                                                                    
189800                                                                          
189900 IMS-GNP-XXBU-2224-IDART SECTION.                                         
190000     MOVE 'IMS-GNP-XXBU-2224-IDART   ' TO CURRENT-IMS-SECTION             
190100                                                                          
190200     STRING 'WLXXBU11(IDARTNR  =' W-IDARTNR-X                             
190300                    '&IDDC     =' W-IDDC-X ')'                            
190400          DELIMITED BY SIZE INTO SSA1                                     
190500     MOVE '  GE' TO GODK-STATUSKODER                                      
190600     CALL CBLTDLI USING GNP XXBU-PCB DLI-IO-AREA SSA1                     
190700     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
190800     PERFORM IMS-STATUSKONTROLL                                           
190900     .                                                                    
191000     EJECT                                                                
191100 IMS-GHU-XXBU-2223-2224 SECTION.                                          
191200     MOVE 'IMS-GHU-XXBU-2223-2224    ' TO CURRENT-IMS-SECTION             
191300                                                                          
191400     STRING 'WLXXBU01(WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
191500          DELIMITED BY SIZE INTO SSA1                                     
191600     STRING 'WLXXBU11(WDGXKEY  =' W-WDGXKEY-2224-X                        
191700                    '&IDDC     =' W-IDDC-X ')'                            
191800          DELIMITED BY SIZE INTO SSA2                                     
191900     MOVE '  GE' TO GODK-STATUSKODER                                      
192000     CALL CBLTDLI USING GHU XXBU-PCB DLI-IO-AREA SSA1 SSA2                
192100     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
192200     PERFORM IMS-STATUSKONTROLL                                           
192300     .                                                                    
192400                                                                          
192500 IMS-REPL-XXBU SECTION.                                                   
192600     MOVE 'IMS-REPL-XXBU             ' TO CURRENT-IMS-SECTION             
192700                                                                          
192800     MOVE '  ' TO GODK-STATUSKODER                                        
192900     CALL CBLTDLI USING REPL XXBU-PCB DLI-IO-AREA                         
193000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
193100     PERFORM IMS-STATUSKONTROLL                                           
193200     .                                                                    
193300                                                                          
193400                                                                          
193500 IMS-DLET-XXBU SECTION.                                                   
193600     MOVE 'IMS-DLET-XXBU             ' TO CURRENT-IMS-SECTION             
193700                                                                          
193800     MOVE '  ' TO GODK-STATUSKODER                                        
193900     CALL CBLTDLI USING DLET XXBU-PCB DLI-IO-AREA                         
194000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
194100     PERFORM IMS-STATUSKONTROLL                                           
194200     .                                                                    
194300     EJECT                                                                
194400 IMS-GET-XXBX-2231 SECTION.                                               
194500     MOVE 'IMS-GET-XXBX-2231         ' TO CURRENT-IMS-SECTION             
194600                                                                          
194700     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
194800          DELIMITED BY SIZE INTO SSA1                                     
194900     MOVE '  ' TO GODK-STATUSKODER                                        
195000     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA SSA1                      
195100     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
195200     PERFORM IMS-STATUSKONTROLL                                           
195300     .                                                                    
195400                                                                          
195500                                                                          
195600 IMS-GET-XXBX-2232 SECTION.                                               
195700     MOVE 'IMS-GET-XXBX-2232         ' TO CURRENT-IMS-SECTION             
195800                                                                          
195900     STRING 'WLXXBX11(WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
196000          DELIMITED BY SIZE INTO SSA1                                     
196100     MOVE '  GE' TO GODK-STATUSKODER                                      
196200     CALL CBLTDLI USING GNP XXBX-PCB DLI-IO-AREA SSA1                     
196300     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
196400     PERFORM IMS-STATUSKONTROLL                                           
196500     .                                                                    
196600                                                                          
196700 IMS-ISRT-WDR301 SECTION.                                                 
196800     MOVE 'IMS-ISRT-WDR301           ' TO CURRENT-IMS-SECTION             
196900                                                                          
197000     STRING 'WDR301      '                                                
197100          DELIMITED BY SIZE INTO SSA1                                     
197200     MOVE '   ' TO GODK-STATUSKODER                                       
197300     CALL CBLTDLI USING ISRT WDR3-PCB DLI-IO-WDR301 SSA1                  
197400     MOVE WDR3-STATUS-CODE TO STATUS-WS                                   
197500     PERFORM IMS-STATUSKONTROLL                                           
197600     .                                                                    
197700                                                                          
197800 IMS-INSERT-ALT-2126  SECTION.                                            
197900     MOVE 'IMS-INSERT-ALT-2126       ' TO CURRENT-IMS-SECTION             
198000                                                                          
198100     MOVE SPACE TO GODK-STATUSKODER                                       
198200     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
198300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
198400     PERFORM IMS-STATUSKONTROLL                                           
198500     .                                                                    
198600     EJECT                                                                
198700 IMS-INSERT-ALT1-2127 SECTION.                                            
198800     MOVE 'IMS-INSERT-ALT1-2127      ' TO CURRENT-IMS-SECTION             
198900                                                                          
199000     MOVE SPACE TO GODK-STATUSKODER                                       
199100     CALL CBLTDLI USING ISRT ALT1-PCB W-PROG-TO-PROG-SW                   
199200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
199300     PERFORM IMS-STATUSKONTROLL                                           
199400     .                                                                    
199500                                                                          
199600                                                                          
199700 IMS-INSERT-ALT3-6202 SECTION.                                            
199800     MOVE 'IMS-INSERT-ALT3-6202      ' TO CURRENT-IMS-SECTION             
199900                                                                          
200000     MOVE SPACE TO GODK-STATUSKODER                                       
200100     CALL CBLTDLI USING ISRT ALT3-PCB ALT3-MSG-AREA                       
200200     MOVE ALT3-STATUS-CODE TO STATUS-WS                                   
200300     PERFORM IMS-STATUSKONTROLL                                           
200400     .                                                                    
200500                                                                          
200600 IMS-INSERT-ALT4-2102 SECTION.                                            
200700     MOVE 'IMS-INSERT-ALT4-2102      ' TO CURRENT-IMS-SECTION             
200800                                                                          
200900     MOVE SPACE TO GODK-STATUSKODER                                       
201000     CALL CBLTDLI USING ISRT ALT4-PCB ALT4-MSG-AREA                       
201100     MOVE ALT4-STATUS-CODE TO STATUS-WS                                   
201200     PERFORM IMS-STATUSKONTROLL                                           
201300     .                                                                    
201400                                                                          
201500 IMS-INSERT-ALT5-3165 SECTION.                                            
201600     MOVE 'IMS-INSERT-ALT5-3165      ' TO CURRENT-IMS-SECTION             
201700                                                                          
201800     MOVE SPACE TO GODK-STATUSKODER                                       
201900     CALL CBLTDLI USING ISRT ALT5-PCB ALT5-MSG-AREA                       
202000     MOVE ALT5-STATUS-CODE TO STATUS-WS                                   
202100     PERFORM IMS-STATUSKONTROLL                                           
202200     .                                                                    
202300                                                                          
202400 IMS-INSERT-ALT6-2103 SECTION.                                            
202500     MOVE 'IMS-INSERT-ALT6-2103      ' TO CURRENT-IMS-SECTION             
202600                                                                          
202700     MOVE SPACE TO GODK-STATUSKODER                                       
202800     CALL CBLTDLI USING ISRT ALT6-PCB ALT6-MSG-AREA                       
202900     MOVE ALT6-STATUS-CODE TO STATUS-WS                                   
203000     PERFORM IMS-STATUSKONTROLL                                           
203100     .                                                                    
203200                                                                          
203300                                                                          
203400 IMS-INSERT-ALT7-2114 SECTION.                                            
203500     MOVE 'IMS-INSERT-ALT7-2114      ' TO CURRENT-IMS-SECTION             
203600                                                                          
203700     MOVE SPACE TO GODK-STATUSKODER                                       
203800     CALL CBLTDLI USING ISRT ALT7-PCB ALT7-MSG-AREA                       
203900     MOVE ALT7-STATUS-CODE TO STATUS-WS                                   
204000     PERFORM IMS-STATUSKONTROLL                                           
204100     .                                                                    
204200     EJECT                                                                
204300 IMS-STATUSKONTROLL SECTION.                                              
204400                                                                          
204500     SET STATUS-IX TO 1                                                   
204600     SEARCH GODK-STATUS                                                   
204700       AT END                                                             
204800         CALL FELLOG                                                      
204900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
205000         CONTINUE                                                         
205100     END-SEARCH                                                           
205200     .                                                                    
