000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9031100.                                                
000400 AUTHOR.         KERSTIN JOHANSSON  GUIDE DATAKONSULT AB                  
000500 DATE-WRITTEN.   JAN 1991.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*        PROGRAMMET ÄR EN BAKGRUNDSTRANS FÖR KONVERTERING AV              
001000*        ORDERPOSTER FRÅN VDI.                                            
001100*        ORDERPOSTERNA UPPDATERAS PÅ KOMMUNIKATIONS DB.                   
001200*        DISPATCHER MPP 0693 STARTAS DÅ KOMMUNIKATIONS DB                 
001300*        UPPDATERAD.                                                      
001400*        VDI KAN ÄVEN FRÅGA PÅ ORDERPOSTENS STATUS PÅ KOMMUNIKA-          
001500*        TIONS DATABASEN.                                                 
001600*        FRÅN VDI KAN DET KOMMA POSTER FÖR:                               
001610*                                                                         
001620*        PROGRAMMET ÄNDRAT MARS 2007 SÅ KONTROLL SKER OM                  
001630*        TVINGANDE TILLÄGG SKALL GÖRAS.                                   
001640*        KONTROLL SKER MED HJÄLP AV W411OHKK OCH W411TVAG                 
001700*                                                                         
001800*             - NYUPPLÄGGNING AV ORDERHUVUD/POSTER - POSTTYP RHI          
001900*         NY  - NYUPPLÄGGNING AV ORDERHUVUD/POSTER - POSTTYP RHN          
002000*     PIE NY  - NYUPPLÄGGNING AV ORDERHUVUD/POSTER - POSTTYP RHP          
002100*     DDI*NY  - NYUPPLÄGGNING AV ORDERHUVUD/POSTER - POSTTYP RHP          
002200*     PIE NY  - NYUPPLÄGGNING AV ORDERHUVUD/POSTER - POSTTYP RHR          
002300*     DDI*NY  - NYUPPLÄGGNING AV ORDERHUVUD/POSTER - POSTTYP RHR          
002400*                                                                         
002500*             - ANNULLERING AV ORDERRADER          - POSTTYP RHO          
002600*                                                                         
002700*             - TILLÄGG AV ORDERRADER              - POSTTYP RHK          
002800*     DDI*    - TILLÄGG AV ORDERRADER              - POSTTYP RHK          
002900*                                                                         
003000*             - ANNULLERING AV RESTORDER/TPO       - POSTTYP RHM          
003100*                                                                         
003200*             - FRÅGA PÅ STATUS                                           
003300*                                                                         
003400*        * DDI-POSTERNA HAR IDVTYP = 3                                    
003500*          DDI-COPYTEXTEN HETER W903RHPB                                  
003600*        EN POST KAN ENDAST INNEHÅLLA UPPGIFTER FÖR EN OCH SAMMA          
003700*        IDENTITET (IDDISTR, IDKUNDNR, IDORDNR) OCH ENDAST EN             
003800*        POSTTYP.                                                         
003900*        ETT MEDDELANDE MED POSTENS STATUS SKICKAS TILLBAKA               
004000*        TILL VDI                                                         
004100*                                                                         
004200*                                                                         
004300*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
004400*        PROGRAMMET UPPDATERAR WLKOMA (WDP8) - KOOMUNIKATIONS DB          
004500*                                                                         
004600     EJECT                                                                
004700*                                                                         
004800*    INDATA.                                                              
004900*        TRANSAKTION: W90311U                                             
005000*        MID:         WMSGKOMI + W903RHIA                                 
005100*                     WMSGKOMI + W903RHKA                                 
005200*                     WMSGKOMI + W903RHKB                                 
005300*                     WMSGKOMI + W903RHMA                                 
005400*                     WMSGKOMI + W903RHNA                                 
005500*                     WMSGKOMI + W903RHOA                                 
005600*                     WMSGKOMI + W903RHPA                                 
005700*                     WMSGKOMI + W903RHPB                                 
005800*                     WMSGKOMI + W903RHRA                                 
005900*                     WMSGKOMI + W903RHRB                                 
006000*                                                                         
006100*    UTDATA:                                                              
006200*        MOD:         WMSGKOMI                                            
006300     EJECT                                                                
006400 ENVIRONMENT DIVISION.                                                    
006500 DATA DIVISION.                                                           
006600 WORKING-STORAGE SECTION.                                                 
006700*    -COPY WY2000W9                                                       
006800     SKIP3                                                                
006900*    -COPY WY2000W4                                                       
007000     SKIP3                                                                
007100 77  IDPGM                       PIC X(08)   VALUE 'W9031100'.            
007200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
007300 77  JA                          PIC X       VALUE 'J'.                   
007310 77  YES                         PIC X       VALUE 'Y'.                   
007400 77  NEJ                         PIC X       VALUE 'N'.                   
007500 77  MID-IX                      PIC S9(9)   COMP SYNC VALUE ZERO.        
007510 77  W-IX                        PIC S9(9)   COMP SYNC VALUE ZERO.        
007600 77  ORAD-IX                     PIC S9(9)   COMP SYNC VALUE ZERO.        
007700 77  ORAD-IX-MAX                 PIC S9(9)   COMP SYNC VALUE +5.          
007800 77  TRAD-IX                     PIC S9(9)   COMP SYNC VALUE ZERO.        
007900 77  TRAD-IX-MAX                 PIC S9(9)   COMP SYNC VALUE +8.          
       77  IDDC-IX                     PIC S9(3)   VALUE ZERO COMP SYNC.        
       77  IX-DCCLEAR-MAX              PIC S9(3)   VALUE +99  COMP SYNC.        
008000 77  WS-DATUM                    PIC 9(6).                                
008100 77  W-KVRADER                   PIC 9(5)     VALUE ZERO.                 
008101 77  W-IDORDNR                   PIC 9(5)     VALUE ZERO.                 
008110                                                                          
008120 01  KONSOLIDERING-SW            PIC X       VALUE 'N'.                   
008130     88 KONSOLIDERING                        VALUE 'J'.                   
008200                                                                          
008210 01  W-FLSOFT                    PIC X       VALUE 'N'.                   
008220     88 SOFTW                                VALUE 'J'.                   
008221     88 SOFTW-NEJ                            VALUE 'N'.                   
008230                                                                          
008210 01  DC11-PRESENT-SW             PIC X       VALUE 'N'.                   
008220     88  DC11-PRESENT                        VALUE 'J'.                   
008230                                                                          
008300 01  W-PRARTXXX-MED-PUNKT        PIC 9(7).9(2).                           
008400                                                                          
008500 01  WS-TIDAGNR.                                                          
008600     03 WS-TIAADDD               PIC 9(5).                                
008700     03 FILLER                   REDEFINES WS-TIAADDD.                    
008800        05 WS-TIAA               PIC 9(2).                                
008900        05 WS-TIDDD              PIC 9(3).                                
009000                                                                          
009100 01  MESSAGE-CODES.                                                       
009200     03 ERR-POST-FEL             PIC X(3)     VALUE '076'.                
009300     03 OK-POST-MOTTAGEN         PIC X(3)     VALUE '121'.                
009400     EJECT                                                                
009500                                                                          
009600 01  TEST-IDDISTR        PIC 9(5) COMP-3.                                 
009700*01  FILLER -COPY WWDIST20  -RED TEST-IDDISTR.                            
009800     EJECT                                                                
009900*01  FILLER -COPY WWDIST40  -RED TEST-IDDISTR.                            
010000     EJECT                                                                
010100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010200 01  GENERELLA-SUBPROGRAM.                                                
010300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010600     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
010700*    ----------------------------UNDERSÖK OM NOAC-DISTRIKT                
010800     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
010900*    ----------------------------UPPDATERA KOMMUNIKATIONS DB              
011000     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
011010     03  W411OHKK                PIC X(8)    VALUE 'W411OHKK'.            
011020*        KOSOLIDERINGSKONTROLL                                            
011100                                                                          
011200 01  KONTROLL-SIFFRA.                                                     
011300     03  REK-IDARTNR             PIC 9(9)    VALUE 0.                     
011400     03  REK-LNGD                PIC 9(1)    VALUE 9.                     
011500     03  REK-REKSIFFR            PIC 9(1)    VALUE 0.                     
011600                                                                          
011700 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
011800     SKIP3                                                                
011900 01  DAT-IO-AREA.                                                         
012000*    03  -COPY WDATAREA                                                   
012100     EJECT                                                                
012200*01  -COPY W460DIS1                                                       
012300     EJECT                                                                
012400*01  -COPY W460LISO                                                       
012410                                                                          
012420 01 FILLER                       PIC X(8)    VALUE 'W411OHKK'.            
012430*   -COPY W411OHKK                                                        
012500     EJECT                                                                
012600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012800 01  MID-IO-AREA.                                                         
012900*    03  -COPY WMSGKOMI                                                   
013000     EJECT                                                                
013100                                                                          
013200*    03  -COPY W9I31101                                                   
013300     EJECT                                                                
013400     05 MID-RHI    REDEFINES  MID-AREA-PTYP.                              
013500*       07  -COPY W903RHIA  -PRE MID-                                     
013600     EJECT                                                                
013700     05 MID-RHKA   REDEFINES  MID-AREA-PTYP.                              
013800*       07  -COPY W903RHKA  -PRE MID-                                     
013900     EJECT                                                                
014000     05 MID-RHKB   REDEFINES  MID-AREA-PTYP.                              
014100*       07  -COPY W903RHKB  -PRE MID-                                     
014200     EJECT                                                                
014300     05 MID-RHM    REDEFINES  MID-AREA-PTYP.                              
014400*       07  -COPY W903RHMA  -PRE MID-                                     
014500     EJECT                                                                
014600     05 MID-RHN    REDEFINES  MID-AREA-PTYP.                              
014700*       07  -COPY W903RHNA  -PRE MID-                                     
014800     EJECT                                                                
014900     05 MID-RHO    REDEFINES  MID-AREA-PTYP.                              
015000*       07  -COPY W903RHOA  -PRE MID-                                     
015100     EJECT                                                                
015200     05 MID-RHPA   REDEFINES  MID-AREA-PTYP.                              
015300*       07  -COPY W903RHPA  -PRE MID-                                     
015400     EJECT                                                                
015500     05 MID-RHPB   REDEFINES  MID-AREA-PTYP.                              
015600*       07  -COPY W903RHPB  -PRE MID-                                     
015700     EJECT                                                                
015800     05 MID-RHRA   REDEFINES  MID-AREA-PTYP.                              
015900*       07  -COPY W903RHRA  -PRE MID-                                     
016000     EJECT                                                                
016100     05 MID-RHRB   REDEFINES  MID-AREA-PTYP.                              
016200*       07  -COPY W903RHRB  -PRE MID-                                     
016300     EJECT                                                                
016400*                                                                         
016500 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
016600     SKIP3                                                                
016700*01  -COPY WMSGAREA                                                       
016800     EJECT                                                                
016900*                                                                         
017000*    --- AREOR FÖR W006KOM SUBMODUL                                       
017100*                                                                         
017200 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
017300 01  KOM-IO-AREA.                                                         
017400   03  KOM-AREA                     PIC X(2000) VALUE SPACE.              
017500   03  OHUV     REDEFINES KOM-AREA.                                       
017600*    05      -COPY W4I25101   -PRE OHUV-                                  
017700     EJECT                                                                
017800   03  ORAD     REDEFINES KOM-AREA.                                       
017900*    05      -COPY W4I25201   -PRE ORAD-                                  
018000     EJECT                                                                
018100   03  ARAD     REDEFINES KOM-AREA.                                       
018200*    05      -COPY W4I25401   -PRE ARAD-                                  
018300     EJECT                                                                
018400   03  TRAD     REDEFINES KOM-AREA.                                       
018500*    05      -COPY W4I25501   -PRE TRAD-                                  
018600     EJECT                                                                
018700   03  RORAD    REDEFINES KOM-AREA.                                       
018800*    05      -COPY W4I25601   -PRE RORAD-                                 
018900     EJECT                                                                
019000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019200                                                                          
019300 01  NYCKLAR-TILL-DLI.                                                    
019400     03  W-IDGMT-X.                                                       
019500         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
019600         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
019700*                                                                         
019800     03  W-IDGMT-MIN-X.                                                   
019900         05  W-IDDISTR-WDB2-MIN  PIC S9(5) VALUE ZERO COMP-3.             
020000         05  W-IDKUNDNR-WDB2-MIN PIC S9(7) VALUE ZERO COMP-3.             
020100*                                                                         
020200     03  W-IDGMT-MAX-X.                                                   
020300         05  W-IDDISTR-WDB2-MAX  PIC S9(5) VALUE ZERO COMP-3.             
020400         05  W-IDKUNDNR-WDB2-MAX PIC S9(7) VALUE ZERO COMP-3.             
020500                                                                          
020600 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
020700 01  DLI-IO-AREA-WDB201.                                                  
020800     03  WLGMTA01.                                                        
020900*        05  -COPY WDB201                                                 
021000                                                                          
021100 01  STATUS-WS                   PIC XX.                                  
021200     88 SEGMENT-FINNS                        VALUE '  '.                  
021300 01  GODK-STATUSKODER.                                                    
021400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021500                                                                          
021600 01  SSA1                        PIC X(64).                               
021700                                                                          
021800*    --- IMS FUNKTIONSKODER                                               
021900*01  -COPY W0003                                                          
022000     EJECT                                                                
022100 LINKAGE SECTION.                                                         
022200*01  -COPY W0009      -PRE MSG-                                           
022300     SKIP2                                                                
022400 01  DISP-PCB                    PIC X.                                   
022500     SKIP2                                                                
022600*01  -COPY W0008      -PRE WDB2-                                          
022700     05  FILLER                  PIC X.                                   
022800 01  KOMA-PCB                    PIC X.                                   
022810                                                                          
022820 01  OHKK-WDQ2-PCB               PIC X.                                   
022830 01  OHKK-WDQ2-UPD-PCB           PIC X.                                   
022831 01  OHKK-WDQ2C-PCB              PIC X.                                   
022840 01  OHKK-GMTA-PCB               PIC X.                                   
022850 01  OHKK-GMTB-PCB               PIC X.                                   
022860 01  OHKK-GMTC-PCB               PIC X.                                   
022870 01  OHKK-BETC-PCB               PIC X.                                   
022880 01  OHKK-WDB2-PCB               PIC X.                                   
022890 01  OHKK-WDB3-PCB               PIC X.                                   
022891 01  OHKK-WDB5-PCB               PIC X.                                   
022892 01  OHKK-WDP7-PCB               PIC X.                                   
022893 01  OHKK-XXKB-PCB               PIC X.                                   
022900     EJECT                                                                
023000 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB WDB2-PCB KOMA-PCB             
023100                     OHKK-WDQ2-PCB OHKK-WDQ2-UPD-PCB                      
023110                     OHKK-WDQ2C-PCB                                       
023200                     OHKK-GMTA-PCB OHKK-GMTB-PCB                          
023210                     OHKK-GMTC-PCB OHKK-BETC-PCB OHKK-WDB2-PCB            
023220                     OHKK-WDB3-PCB OHKK-WDB5-PCB OHKK-WDP7-PCB            
023230                     OHKK-XXKB-PCB.                                       
023240 MAIN SECTION.                                                            
023250     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB WDB2-PCB KOMA-PCB             
023260                     OHKK-WDQ2-PCB OHKK-WDQ2-UPD-PCB                      
023261                     OHKK-WDQ2C-PCB                                       
023270                     OHKK-GMTA-PCB OHKK-GMTB-PCB                          
023280                     OHKK-GMTC-PCB OHKK-BETC-PCB OHKK-WDB2-PCB            
023290                     OHKK-WDB3-PCB OHKK-WDB5-PCB OHKK-WDP7-PCB            
023291                     OHKK-XXKB-PCB.                                       
023292                                                                          
023300                                                                          
023400     PERFORM IMS-GET-MSG                                                  
023500     IF SEGMENT-FINNS                                                     
023600        PERFORM A-INIT                                                    
023700        PERFORM B-KONTROLLERA-POST                                        
023800        IF MSG-KOM-IDMFSMED = SPACE OR '000'                              
023900           IF MSG-KOM-IDMFSMED = '000'                                    
024000              PERFORM C-FRAGA-STATUS                                      
024100           ELSE                                                           
024600              IF MID-IDPTYP = 'RHI' OR 'RHN' OR 'RHP' OR 'RHR'            
024700                 PERFORM D-SKAPA-ORDERHUVUDTRANS                          
024800                 IF MSG-KOM-IDMFSMED = SPACE                              
024900                    PERFORM E-SKAPA-ORDERRADTRANS                         
025000                 END-IF                                                   
025100              END-IF                                                      
025200              IF MID-IDPTYP = 'RHO'                                       
025300                 PERFORM F-SKAPA-ANNULRADTRANS                            
025400              END-IF                                                      
025500              IF MID-IDPTYP = 'RHK'                                       
025600                 PERFORM G-SKAPA-TILLAGGRADTRANS                          
025700              END-IF                                                      
025800              IF MID-IDPTYP = 'RHM'                                       
025900                 PERFORM H-SKAPA-RO-TPO-ANNULLRADTRANS                    
026000              END-IF                                                      
026010              IF MSG-KOM-IDMFSMED = SPACE                                 
026020                 MOVE OK-POST-MOTTAGEN TO MSG-KOM-IDMFSMED                
026030              END-IF                                                      
026050           END-IF                                                         
026100        END-IF                                                            
026200*       SKICKA MEDDELANDE TILL VDI OM POSTENS STATUS                      
026300        MOVE MSG-KOM-WMSGKOMI    TO MSG-IO-AREA                           
026400        PERFORM IMS-INSERT-MSG                                            
026500     END-IF                                                               
026600     MOVE ZERO TO RETURN-CODE                                             
026700     GOBACK                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 A-INIT SECTION.                                                          
027100                                                                          
027200     MOVE LOW-VALUE TO MSG-AREA                                           
027300                                                                          
027400     ACCEPT WS-DATUM FROM DATE                                            
027500     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
027600     MOVE WS-DATUM       TO DAT-I-TIDATUM                                 
027700     CALL WDATKONV    USING DAT-KDDATFORM                                 
027800                            DAT-I-TIDATUM                                 
027900                            DAT-O-TIDATUM                                 
028000                            DAT-KDSVAR                                    
028100     IF DAT-KDSVAR NOT = SPACE                                            
028200        MOVE ' FELAKTIG RETURKOD FRÅN WDATKONV I A-INIT-'                 
028300                         TO FELTEXT                                       
028400        CALL FELLOG                                                       
028500     END-IF                                                               
028600     MOVE DAT-TIAADDD    TO WS-TIAADDD                                    
028700                                                                          
028800     MOVE MID-IDDISTR                                                     
028900                         TO TEST-IDDISTR DIS1-IDDISTR                     
029000     .                                                                    
029100     EJECT                                                                
029200                                                                          
029300 B-KONTROLLERA-POST SECTION.                                              
029400     SKIP2                                                                
029500     IF MSG-KOM-IDMFSMED = '000'                                          
029600        IF MSG-KOM-TIREGDAT NUMERIC AND                                   
029700           MSG-KOM-TIKLOCK NUMERIC                                        
029800           CONTINUE                                                       
029900        ELSE                                                              
030000*          DATUM, TID EJ NUMERISKT                                        
030100*          MOVE ERR-POST-FEL         TO MSG-KOM-IDMFSMED                  
030200           MOVE '001'                TO FELTEXT                           
030300                                                                          
030400        END-IF                                                            
030500     ELSE                                                                 
030600        IF MSG-KOM-IDMFSMED NOT = SPACE                                   
030700*          MEDDELANDEKOD FELAKTIG                                         
030800*          MOVE ERR-POST-FEL         TO MSG-KOM-IDMFSMED                  
030900           MOVE '002'                TO FELTEXT                           
031000        END-IF                                                            
031100                                                                          
031200        IF MSG-KOM-IDMFSMED = SPACE                                       
031300           MOVE 'AAMMDD'          TO DAT-KDDATFORM                        
031400           MOVE MSG-KOM-TIREGDAT                                          
031500                               TO DAT-I-TIDATUM                           
031600           CALL WDATKONV USING DAT-KDDATFORM                              
031700                               DAT-I-TIDATUM                              
031800                               DAT-O-TIDATUM                              
031900                               DAT-KDSVAR                                 
032000           MOVE WS-TIAA        TO TMP1-YY                                 
032100           MOVE DAT-TIAA-DAGNR TO TMP2-YY                                 
032200           PERFORM WY2000P9                                               
032300           IF DAT-KDSVAR = SPACE AND                                      
032400              TMP1-YY > TMP2-YY                                           
032500              ADD +634            TO DAT-TIAADDD                          
032600           END-IF                                                         
032700           MOVE WS-TIAADDD    TO TMP1-YYDDD                               
032800           MOVE DAT-TIAADDD   TO TMP2-YYDDD                               
032900           PERFORM WY2000P4                                               
033000           IF DAT-KDSVAR = SPACE AND                                      
033100             (TMP1-YYDDD - TMP2-YYDDD NOT > 7)                            
033200              CONTINUE                                                    
033300           ELSE                                                           
033400*             DATUM MER ÄN 7 DAGAR GAMMALT ELLER FELAKTIGT                
033500*             MOVE ERR-POST-FEL TO MSG-KOM-IDMFSMED                       
033600             MOVE '003'              TO FELTEXT                           
033700           END-IF                                                         
033800                                                                          
033900        END-IF                                                            
034000                                                                          
034100        IF MSG-KOM-IDMFSMED = SPACE                                       
034200           IF MID-IDPTYP NOT = 'RHI' AND                                  
034300              MID-IDPTYP NOT = 'RHK' AND                                  
034400              MID-IDPTYP NOT = 'RHM' AND                                  
034500              MID-IDPTYP NOT = 'RHN' AND                                  
034600              MID-IDPTYP NOT = 'RHO' AND                                  
034700              MID-IDPTYP NOT = 'RHP' AND                                  
034800              MID-IDPTYP NOT = 'RHR'                                      
034900*             POSTTYP EJ GODKÄND                                          
035000*             MOVE ERR-POST-FEL TO MSG-KOM-IDMFSMED                       
035100             MOVE '004'              TO FELTEXT                           
035200           END-IF                                                         
035300        END-IF                                                            
035400                                                                          
035500        IF MSG-KOM-IDMFSMED = SPACE                                       
035600           IF MID-RHI-KVRADER NUMERIC                                     
035700              CONTINUE                                                    
035800           ELSE                                                           
035900*             ANTAL RADER EJ NUMERISKT                                    
036000*             MOVE ERR-POST-FEL TO MSG-KOM-IDMFSMED                       
036100             MOVE '005'              TO FELTEXT                           
036200           END-IF                                                         
036300        END-IF                                                            
036400                                                                          
036500        IF MSG-KOM-IDMFSMED = SPACE                                       
036600           IF MID-IDPTYP = 'RHI'                                          
036700              IF MID-RHI-IDARTNR (MID-RHI-KVRADER) = SPACE     OR         
036800                 MID-RHI-IDARTNR (MID-RHI-KVRADER) = LOW-VALUE OR         
036900                 MID-RHI-IDARTNR (MID-RHI-KVRADER) NOT NUMERIC            
037000*                FEL LÄNGD PÅ POSTEN                                      
037100*                MOVE ERR-POST-FEL TO MSG-KOM-IDMFSMED                    
037200               MOVE '006'            TO FELTEXT                           
037300              END-IF                                                      
037400           END-IF                                                         
037500           IF MID-IDPTYP = 'RHK'                                          
037600              IF (MID-RHKA-IDARTNR(MID-RHKA-KVRADER) = SPACE  OR          
037700                 MID-RHKA-IDARTNR(MID-RHKA-KVRADER) = LOW-VALUE OR        
037800                  MID-RHKA-IDARTNR(MID-RHKA-KVRADER) NOT NUMERIC)         
037900                OR                                                        
038000                 (MID-RHKB-IDARTNR(MID-RHKB-KVRADER) = SPACE   OR         
038100                 MID-RHKB-IDARTNR(MID-RHKB-KVRADER) = LOW-VALUE OR        
038200                  MID-RHKB-IDARTNR(MID-RHKB-KVRADER) NOT NUMERIC)         
038300*                FEL LÄNGD PÅ POSTEN                                      
038400*                MOVE ERR-POST-FEL TO MSG-KOM-IDMFSMED                    
038500                 MOVE '006'          TO FELTEXT                           
038600              END-IF                                                      
038700           END-IF                                                         
038800           IF MID-IDPTYP = 'RHP'                                          
038900             IF (MID-RHPA-IDARTNR(MID-RHPA-KVRADER) = SPACE    OR         
039000                 MID-RHPA-IDARTNR(MID-RHPA-KVRADER) = LOW-VALUE OR        
039100                 MID-RHPA-IDARTNR(MID-RHPA-KVRADER) NOT NUMERIC)          
039200               OR                                                         
039300                (MID-RHPB-IDARTNR(MID-RHPB-KVRADER) = SPACE   OR          
039400                 MID-RHPB-IDARTNR(MID-RHPB-KVRADER) = LOW-VALUE OR        
039500                 MID-RHPB-IDARTNR(MID-RHPB-KVRADER) NOT NUMERIC)          
039600*                FEL LÄNGD PÅ POSTEN                                      
039700*                MOVE ERR-POST-FEL TO MSG-KOM-IDMFSMED                    
039800               MOVE '006'            TO FELTEXT                           
039900              END-IF                                                      
040000           END-IF                                                         
040100        END-IF                                                            
040200     END-IF                                                               
040300                                                                          
040400     IF MSG-KOM-IDMFSMED = SPACE                                          
040500        MOVE MID-IDDISTR            TO W-IDDISTR-WDB2                     
040600                                       W-IDDISTR-WDB2-MIN                 
040700                                       W-IDDISTR-WDB2-MAX                 
040800        MOVE MID-IDKUNDNR           TO W-IDKUNDNR-WDB2                    
040900        PERFORM IMS-GET-WDB201-UNIK                                       
041000        IF SEGMENT-FINNS                                                  
                 IF MID-RHPA-KDORDKL >= 2                                       
                   PERFORM BA-CHK-FOR-DC11                                      
                 END-IF                                                         
041200        ELSE                                                              
041300           PERFORM IMS-GU-WDB201                                          
041400        END-IF                                                            
041500     END-IF                                                               
041600     MOVE +54                    TO MSG-KOM-KVLL                          
041700     .                                                                    
041800     EJECT                                                                
041900                                                                          
      *CHECK IF DC11 IS PRESENT IN BULK IDDC CLEAR TABLE.                       
042000 BA-CHK-FOR-DC11 SECTION.                                                 
042100     SKIP2                                                                
           MOVE NEJ                    TO DC11-PRESENT-SW                       
042200     MOVE +1                     TO IDDC-IX                               
042300     PERFORM UNTIL IDDC-IX > IX-DCCLEAR-MAX OR                            
                         GMT-IDDC-BULK(IDDC-IX) = SPACE OR                      
                         DC11-PRESENT                                           
042400        IF GMT-IDDC-BULK(IDDC-IX) = '11'                                  
042500           SET DC11-PRESENT      TO TRUE                                  
              END-IF                                                            
042600        ADD +1                   TO   IDDC-IX                             
           END-PERFORM                                                          
042700     .                                                                    
042800     EJECT                                                                
042900                                                                          
041900                                                                          
042000 C-FRAGA-STATUS SECTION.                                                  
042100     SKIP2                                                                
042200     CALL W006KOM USING MSG-PCB                                           
042300                        DISP-PCB                                          
042400                        KOMA-PCB                                          
042500                        MSG-KOM-WMSGKOMI                                  
042600                        MSG-IO-AREA                                       
042700     .                                                                    
042800     EJECT                                                                
042900                                                                          
043000 D-SKAPA-ORDERHUVUDTRANS SECTION.                                         
043100     SKIP2                                                                
043200                                                                          
043300     MOVE SPACE                  TO KOM-AREA                              
043400                                                                          
043500     COMPUTE MSG-KVLL = LENGTH OF OHUV-MID-W4I25101 + 17                  
043600                                                                          
043700     MOVE LOW-VALUE              TO MSG-KDZ1                              
043800     MOVE LOW-VALUE              TO MSG-KDZ2                              
043900     MOVE 'W4T251X '             TO MSG-KDTRANS-1                         
044000     MOVE '4251'                 TO MSG-IDTRANS-1                         
044100     MOVE '1'                    TO MSG-KDMFSFOR-1                        
044200                                                                          
044300     IF MID-IDPTYP = 'RHI'                                                
044400       PERFORM DA-SKAPA-ORDERHUVUDTRANS-RHI                               
044500     ELSE                                                                 
044600       IF MID-IDPTYP = 'RHN'                                              
044700         PERFORM DB-SKAPA-ORDERHUVUDTRANS-RHN                             
044800       ELSE                                                               
044900         IF MID-IDPTYP = 'RHP'                                            
044910           PERFORM S02-KOLLA-SOFTWARE                                     
044910           PERFORM S04-SW-ORDER2-CHK-DC11                                 
                 IF MSG-KOM-IDMFSMED = SPACES                                   
045000             PERFORM DC-SKAPA-ORDERHUVUDTRANS-RHP                         
                 END-IF                                                         
045100         ELSE                                                             
045200           IF MID-IDPTYP = 'RHR'                                          
045210             PERFORM S03-KOLLA-SOFTWARE                                   
045210             PERFORM S04-SW-ORDER2-CHK-DC11                               
                   IF MSG-KOM-IDMFSMED = SPACES                                 
045300               PERFORM DD-SKAPA-ORDERHUVUDTRANS-RHR                       
045400             END-IF                                                       
045400           END-IF                                                         
045500         END-IF                                                           
045600       END-IF                                                             
045700     END-IF                                                               
                                                                                
           IF MSG-KOM-IDMFSMED = SPACES                                         
045900        MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD        
046000        CALL W006KOM USING MSG-PCB                                        
046100                           DISP-PCB                                       
046200                           KOMA-PCB                                       
046300                           MSG-KOM-WMSGKOMI                               
046400                           MSG-IO-AREA                                    
           END-IF                                                               
046500     .                                                                    
046600     EJECT                                                                
046700                                                                          
046800 DA-SKAPA-ORDERHUVUDTRANS-RHI SECTION.                                    
046900                                                                          
047000     MOVE 'VDI '                 TO OHUV-MID-IDSYSTEM                     
047100     MOVE MID-IDDISTR            TO OHUV-MID-IDDISTR                      
047200     MOVE MID-IDKUNDNR           TO OHUV-MID-IDKUNDNR                     
047210     PERFORM S01-KOLLA-KONSOLIDERING                                      
047215     IF KONSOLIDERING                                                     
047216        MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                      
047217        MOVE OHKK-IDORDNR-UT     TO W-IDORDNR                             
047218        STRING '9311 OC ' W-IDORDNR DELIMITED BY SIZE                     
047219                               INTO OHUV-MID-BEKUNDRF                     
047220     ELSE                                                                 
047221        MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                      
047222        MOVE MID-RHI-BEKUNDRF    TO OHUV-MID-BEKUNDRF                     
047230     END-IF                                                               
047400     MOVE MID-RHI-KDORDKL        TO OHUV-MID-KDORDKL                      
047410     MOVE OHKK-FLORDTIL          TO OHUV-MID-FLORDTIL                     
047500                                                                          
047600     CALL W460DIS1 USING DIS1-W460DIS1                                    
047700                                                                          
047800     IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                                 
047900        OHUV-MID-KDORDKL = '3'                                            
048000        MOVE SPACE               TO OHUV-MID-KDFRAKT                      
048100     ELSE                                                                 
048200        IF MID-RHI-KDFRAKT = ZERO                                         
048300           MOVE SPACE            TO OHUV-MID-KDFRAKT                      
048400        ELSE                                                              
048500           MOVE MID-RHI-KDFRAKT  TO OHUV-MID-KDFRAKT                      
048600        END-IF                                                            
048700     END-IF                                                               
048800     MOVE SPACE                  TO OHUV-MID-TIRFS                        
049000     MOVE SPACE                  TO OHUV-MID-KDFAKTYP                     
049100     MOVE MID-RHI-FLRESTN        TO OHUV-MID-FLRESTN                      
049200     MOVE MID-RHI-KDTPOTYP       TO OHUV-MID-KDTPOTYP                     
049300     MOVE SPACE                  TO OHUV-MID-TITPO                        
049400     MOVE SPACE                  TO OHUV-MID-BELAGINS                     
049500     MOVE SPACE                  TO OHUV-MID-BEGMT                        
049600     MOVE SPACE                  TO OHUV-MID-ADGMT-GATA                   
049700     MOVE SPACE                  TO OHUV-MID-ADGMT-PADR                   
049800     IF MID-RHI-KDFRAKT > ZERO AND (DIST40-NDC-NA                         
049810                                OR  DIST40-NDC-CN)                        
049900       MOVE ZERO                 TO OHUV-MID-KDROPACK                     
050000     ELSE                                                                 
050100       MOVE SPACE                TO OHUV-MID-KDROPACK                     
050200     END-IF                                                               
050300     MOVE SPACE                  TO OHUV-MID-IDKONTO                      
050400     MOVE SPACE                  TO OHUV-MID-IDKST                        
050500     MOVE SPACE                  TO OHUV-MID-IDANALYS                     
050600     MOVE MID-RHI-BEVARREF       TO OHUV-MID-BEVARREF                     
050700     MOVE SPACE                  TO OHUV-MID-KDTULLVE                     
050800     MOVE SPACE                  TO OHUV-MID-KDNOTES                      
050900     MOVE SPACE                  TO OHUV-MID-FLAUTFAK                     
051000     MOVE NEJ                    TO OHUV-MID-FLAUTPAC                     
051100     IF DIST20-EMBALLAGE                                                  
051200        MOVE JA                  TO OHUV-MID-FLEMBORD                     
051300     ELSE                                                                 
051400        MOVE NEJ                 TO OHUV-MID-FLEMBORD                     
051500     END-IF                                                               
051600     MOVE NEJ                    TO OHUV-MID-FLOVRLEV                     
051700     MOVE MID-RHI-IDKAMPRF       TO OHUV-MID-IDKAMPRF                     
051800     MOVE SPACE                  TO OHUV-MID-IDFTG                        
051900     MOVE SPACE                  TO OHUV-MID-ADBET                        
052000     MOVE SPACE                  TO OHUV-MID-BEBET                        
052100     MOVE SPACE                  TO OHUV-MID-IDSKYLT                      
052200     MOVE SPACE                  TO OHUV-MID-IDDC                         
052300     MOVE SPACE                  TO OHUV-MID-FLLSBOK                      
052400     MOVE ZERO                   TO OHUV-MID-IDDEPT                       
052500                                                                          
052600     MOVE SPACE                  TO OHUV-MID-KDORDTYP-LDC                 
052700     MOVE ZERO                   TO OHUV-MID-TIREPDAT                     
052710                                    OHUV-MID-IDGROSS                      
052800     MOVE NEJ                    TO OHUV-MID-FLFORBI                      
052900     MOVE SPACE                  TO OHUV-MID-IDBILREG                     
052910                                    OHUV-MID-IDVIN                        
052920                                    OHUV-MID-IDCISNR                      
053000                                                                          
053100     IF GMT-FLLDCKND = JA                                                 
053200        IF MID-RHI-KDORDKL = 1                                            
053300           MOVE 'FW'             TO OHUV-MID-KDORDTYP-LDC                 
053400        END-IF                                                            
053500     END-IF                                                               
053600     .                                                                    
053700     EJECT                                                                
053800                                                                          
053900 DB-SKAPA-ORDERHUVUDTRANS-RHN SECTION.                                    
054000                                                                          
054100     MOVE 'VDI '                 TO OHUV-MID-IDSYSTEM                     
054200     MOVE MID-IDDISTR            TO OHUV-MID-IDDISTR                      
054300     MOVE MID-IDKUNDNR           TO OHUV-MID-IDKUNDNR                     
054310     PERFORM S01-KOLLA-KONSOLIDERING                                      
054400     IF KONSOLIDERING                                                     
054401        MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                      
054402        MOVE OHKK-IDORDNR-UT     TO W-IDORDNR                             
054403        STRING '9311 OC ' W-IDORDNR DELIMITED BY SIZE                     
054404                               INTO OHUV-MID-BEKUNDRF                     
054460     ELSE                                                                 
054470        MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                      
054481        MOVE MID-RHN-BEKUNDRF    TO OHUV-MID-BEKUNDRF                     
054490     END-IF                                                               
054500     MOVE MID-RHN-KDORDKL        TO OHUV-MID-KDORDKL                      
054510     MOVE OHKK-FLORDTIL          TO OHUV-MID-FLORDTIL                     
054600                                                                          
054700     CALL W460DIS1 USING DIS1-W460DIS1                                    
054800                                                                          
054900     IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                                 
055000        OHUV-MID-KDORDKL = '3'                                            
055100        MOVE SPACE               TO OHUV-MID-KDFRAKT                      
055200     ELSE                                                                 
055300        IF MID-RHN-KDFRAKT = ZERO                                         
055400           MOVE SPACE            TO OHUV-MID-KDFRAKT                      
055500        ELSE                                                              
055600           MOVE MID-RHN-KDFRAKT  TO OHUV-MID-KDFRAKT                      
055700        END-IF                                                            
055800     END-IF                                                               
055900     MOVE SPACE                  TO OHUV-MID-TIRFS                        
056100     MOVE SPACE                  TO OHUV-MID-KDFAKTYP                     
056200     MOVE MID-RHN-FLRESTN        TO OHUV-MID-FLRESTN                      
056300     MOVE MID-RHN-KDTPOTYP       TO OHUV-MID-KDTPOTYP                     
056400     MOVE SPACE                  TO OHUV-MID-TITPO                        
056500     MOVE MID-RHN-BELAGINS-DEL   TO OHUV-MID-BELAGINS                     
056600     MOVE MID-RHN-BEGMT-RAD1     TO OHUV-MID-BEGMT-RAD1                   
056700     MOVE MID-RHN-BEGMT-RAD2     TO OHUV-MID-BEGMT-RAD2                   
056800     MOVE MID-RHN-ADGMT-GATA     TO OHUV-MID-ADGMT-GATA                   
056900     MOVE MID-RHN-ADGMT-PADR     TO OHUV-MID-ADGMT-PADR                   
057000     IF MID-RHN-BEGMT = SPACE       AND                                   
057100        MID-RHN-ADGMT-GATA = SPACE  AND                                   
057200        MID-RHN-ADGMT-PADR = SPACE                                        
057300       MOVE SPACE                TO OHUV-MID-KDROPACK                     
057400     ELSE                                                                 
057500       MOVE ZERO                 TO OHUV-MID-KDROPACK                     
057600     END-IF                                                               
057700     MOVE SPACE                  TO OHUV-MID-IDKONTO                      
057800     MOVE SPACE                  TO OHUV-MID-IDKST                        
057900     MOVE SPACE                  TO OHUV-MID-IDANALYS                     
058000     MOVE MID-RHN-BEVARREF       TO OHUV-MID-BEVARREF                     
058100     MOVE SPACE                  TO OHUV-MID-KDTULLVE                     
058200     MOVE SPACE                  TO OHUV-MID-KDNOTES                      
058300     MOVE SPACE                  TO OHUV-MID-FLAUTFAK                     
058400     MOVE NEJ                    TO OHUV-MID-FLAUTPAC                     
058500     IF DIST20-EMBALLAGE                                                  
058600        MOVE JA                  TO OHUV-MID-FLEMBORD                     
058700     ELSE                                                                 
058800        MOVE NEJ                 TO OHUV-MID-FLEMBORD                     
058900     END-IF                                                               
059000     MOVE NEJ                    TO OHUV-MID-FLOVRLEV                     
059100     MOVE MID-RHN-IDKAMPRF       TO OHUV-MID-IDKAMPRF                     
059200     MOVE SPACE                  TO OHUV-MID-IDFTG                        
059300     MOVE SPACE                  TO OHUV-MID-IDSKYLT                      
059400     MOVE SPACE                  TO OHUV-MID-IDDC                         
059500     MOVE SPACE                  TO OHUV-MID-FLLSBOK                      
059600     MOVE SPACE                  TO OHUV-MID-BEBETRAD-1                   
059700     MOVE SPACE                  TO OHUV-MID-BEBETRAD-2                   
059800     MOVE SPACE                  TO OHUV-MID-ADBETRAD-1                   
059900     MOVE SPACE                  TO OHUV-MID-ADBETRAD-2                   
060000     MOVE ZERO                   TO OHUV-MID-IDDEPT                       
060100                                                                          
060200     MOVE SPACE                  TO OHUV-MID-KDORDTYP-LDC                 
060300     MOVE ZERO                   TO OHUV-MID-TIREPDAT                     
060400     MOVE NEJ                    TO OHUV-MID-FLFORBI                      
060500                                                                          
060600     IF GMT-FLLDCKND = JA                                                 
060700        IF MID-RHN-KDORDKL = 1                                            
060800           MOVE 'FW'             TO OHUV-MID-KDORDTYP-LDC                 
060900        END-IF                                                            
061000     END-IF                                                               
061100     .                                                                    
061200     EJECT                                                                
061300                                                                          
061400 DC-SKAPA-ORDERHUVUDTRANS-RHP SECTION.                                    
061500                                                                          
061510     PERFORM S01-KOLLA-KONSOLIDERING                                      
061520                                                                          
061600     IF MID-IDVTYP = '3'                                                  
061700       MOVE 'VDI '                 TO OHUV-MID-IDSYSTEM                   
061800       MOVE MID-IDDISTR            TO OHUV-MID-IDDISTR                    
061900       MOVE MID-IDKUNDNR           TO OHUV-MID-IDKUNDNR                   
061902       IF KONSOLIDERING                                                   
061903          MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                    
061904          MOVE OHKK-IDORDNR-UT     TO W-IDORDNR                           
061905          STRING '9311 OC ' W-IDORDNR DELIMITED BY SIZE                   
061906                                 INTO OHUV-MID-BEKUNDRF                   
061960       ELSE                                                               
061970          MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                    
061980          MOVE MID-RHPB-BEKUNDRF   TO OHUV-MID-BEKUNDRF                   
061990       END-IF                                                             
062100       MOVE MID-RHPB-KDORDKL       TO OHUV-MID-KDORDKL                    
062110       MOVE OHKK-FLORDTIL          TO OHUV-MID-FLORDTIL                   
062200                                                                          
062300       CALL W460DIS1 USING DIS1-W460DIS1                                  
062400                                                                          
062500       IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                               
062600          OHUV-MID-KDORDKL = '3'                                          
062700          MOVE SPACE               TO OHUV-MID-KDFRAKT                    
062800       ELSE                                                               
062900          IF MID-RHPB-KDFRAKT = ZERO                                      
063000             MOVE SPACE            TO OHUV-MID-KDFRAKT                    
063100          ELSE                                                            
063200             MOVE MID-RHPB-KDFRAKT TO OHUV-MID-KDFRAKT                    
063300          END-IF                                                          
063400       END-IF                                                             
063500       MOVE SPACE                  TO OHUV-MID-TIRFS                      
063700       MOVE SPACE                  TO OHUV-MID-KDFAKTYP                   
063800       MOVE MID-RHPB-FLRESTN       TO OHUV-MID-FLRESTN                    
063900       MOVE MID-RHPB-KDTPOTYP      TO OHUV-MID-KDTPOTYP                   
064000       MOVE SPACE                  TO OHUV-MID-TITPO                      
064100       MOVE SPACE                  TO OHUV-MID-BELAGINS                   
064200       MOVE SPACE                  TO OHUV-MID-BEGMT                      
064300       MOVE SPACE                  TO OHUV-MID-ADGMT-GATA                 
064400       MOVE SPACE                  TO OHUV-MID-ADGMT-PADR                 
064500       IF MID-RHPB-KDFRAKT > ZERO AND (DIST40-NDC-NA                      
064510                                   OR  DIST40-NDC-CN)                     
064600         MOVE ZERO                 TO OHUV-MID-KDROPACK                   
064700       ELSE                                                               
064800         MOVE SPACE                TO OHUV-MID-KDROPACK                   
064900       END-IF                                                             
065000       MOVE SPACE                  TO OHUV-MID-IDKONTO                    
065100       MOVE SPACE                  TO OHUV-MID-IDKST                      
065200       MOVE SPACE                  TO OHUV-MID-IDANALYS                   
065300       MOVE MID-RHPB-BEVARREF       TO OHUV-MID-BEVARREF                  
065400       MOVE SPACE                  TO OHUV-MID-KDTULLVE                   
065500       MOVE SPACE                  TO OHUV-MID-KDNOTES                    
065600       MOVE SPACE                  TO OHUV-MID-FLAUTFAK                   
065700       MOVE NEJ                    TO OHUV-MID-FLAUTPAC                   
065800       IF DIST20-EMBALLAGE                                                
065900          MOVE JA                  TO OHUV-MID-FLEMBORD                   
066000       ELSE                                                               
066100          MOVE NEJ                 TO OHUV-MID-FLEMBORD                   
066200       END-IF                                                             
066300       MOVE NEJ                    TO OHUV-MID-FLOVRLEV                   
066400       MOVE MID-RHPB-IDKAMPRF       TO OHUV-MID-IDKAMPRF                  
066500       MOVE SPACE                  TO OHUV-MID-IDFTG                      
066600       MOVE SPACE                  TO OHUV-MID-ADBET                      
066700       MOVE SPACE                  TO OHUV-MID-BEBET                      
066800       MOVE SPACE                  TO OHUV-MID-IDSKYLT                    
066900       MOVE SPACE                  TO OHUV-MID-IDDC                       
067000       MOVE SPACE                  TO OHUV-MID-FLLSBOK                    
067100     ELSE                                                                 
067200       MOVE 'VDI '                 TO OHUV-MID-IDSYSTEM                   
067300       MOVE MID-IDDISTR            TO OHUV-MID-IDDISTR                    
067400       MOVE MID-IDKUNDNR           TO OHUV-MID-IDKUNDNR                   
067402       IF KONSOLIDERING                                                   
067403          MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                    
067404          MOVE OHKK-IDORDNR-UT     TO W-IDORDNR                           
067405          STRING '9311 OC ' W-IDORDNR DELIMITED BY SIZE                   
067406                                 INTO OHUV-MID-BEKUNDRF                   
067460       ELSE                                                               
067470          MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                    
067480          MOVE MID-RHPA-BEKUNDRF   TO OHUV-MID-BEKUNDRF                   
067490       END-IF                                                             
067600       MOVE MID-RHPA-KDORDKL       TO OHUV-MID-KDORDKL                    
067610       MOVE OHKK-FLORDTIL          TO OHUV-MID-FLORDTIL                   
067700                                                                          
067800       CALL W460DIS1 USING DIS1-W460DIS1                                  
067900                                                                          
068000       IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                               
068100          OHUV-MID-KDORDKL = '3'                                          
068200          MOVE SPACE               TO OHUV-MID-KDFRAKT                    
068300       ELSE                                                               
068400          IF MID-RHPA-KDFRAKT = ZERO                                      
068500             MOVE SPACE            TO OHUV-MID-KDFRAKT                    
068600          ELSE                                                            
068700             MOVE MID-RHPA-KDFRAKT TO OHUV-MID-KDFRAKT                    
068800          END-IF                                                          
068900       END-IF                                                             
069000       MOVE SPACE                  TO OHUV-MID-TIRFS                      
069200       MOVE SPACE                  TO OHUV-MID-KDFAKTYP                   
069300       MOVE MID-RHPA-FLRESTN       TO OHUV-MID-FLRESTN                    
069400       MOVE MID-RHPA-KDTPOTYP      TO OHUV-MID-KDTPOTYP                   
069500       MOVE SPACE                  TO OHUV-MID-TITPO                      
069600       MOVE SPACE                  TO OHUV-MID-BELAGINS                   
069700       MOVE SPACE                  TO OHUV-MID-BEGMT                      
069800       MOVE SPACE                  TO OHUV-MID-ADGMT-GATA                 
069900       MOVE SPACE                  TO OHUV-MID-ADGMT-PADR                 
070000       IF MID-RHPA-KDFRAKT > ZERO AND (DIST40-NDC-NA                      
070010                                   OR  DIST40-NDC-CN)                     
070100         MOVE ZERO                 TO OHUV-MID-KDROPACK                   
070200       ELSE                                                               
070300         MOVE SPACE                TO OHUV-MID-KDROPACK                   
070400       END-IF                                                             
070500       MOVE SPACE                  TO OHUV-MID-IDKONTO                    
070600       MOVE SPACE                  TO OHUV-MID-IDKST                      
070700       MOVE SPACE                  TO OHUV-MID-IDANALYS                   
070800       MOVE MID-RHPA-BEVARREF       TO OHUV-MID-BEVARREF                  
070900       MOVE SPACE                  TO OHUV-MID-KDTULLVE                   
071000       MOVE SPACE                  TO OHUV-MID-KDNOTES                    
071100       MOVE SPACE                  TO OHUV-MID-FLAUTFAK                   
071200       MOVE NEJ                    TO OHUV-MID-FLAUTPAC                   
071300       IF DIST20-EMBALLAGE                                                
071400          MOVE JA                  TO OHUV-MID-FLEMBORD                   
071500       ELSE                                                               
071600          MOVE NEJ                 TO OHUV-MID-FLEMBORD                   
071700       END-IF                                                             
071800       MOVE NEJ                    TO OHUV-MID-FLOVRLEV                   
071900       MOVE MID-RHPA-IDKAMPRF       TO OHUV-MID-IDKAMPRF                  
072000       MOVE SPACE                  TO OHUV-MID-IDFTG                      
072100       MOVE SPACE                  TO OHUV-MID-ADBET                      
072200       MOVE SPACE                  TO OHUV-MID-BEBET                      
072300       MOVE SPACE                  TO OHUV-MID-IDSKYLT                    
072400       MOVE SPACE                  TO OHUV-MID-IDDC                       
072500       MOVE SPACE                  TO OHUV-MID-FLLSBOK                    
072600     END-IF                                                               
072700     MOVE ZERO                   TO OHUV-MID-IDDEPT                       
072800                                                                          
072900     MOVE SPACE                  TO OHUV-MID-KDORDTYP-LDC                 
073000     MOVE ZERO                   TO OHUV-MID-TIREPDAT                     
073100     MOVE NEJ                    TO OHUV-MID-FLFORBI                      
073200                                                                          
073300     IF GMT-FLLDCKND = JA                                                 
073400        IF MID-RHPA-KDORDKL = 1                                           
073500           MOVE 'FW'             TO OHUV-MID-KDORDTYP-LDC                 
073600        END-IF                                                            
073700     END-IF                                                               
073800     .                                                                    
073900     EJECT                                                                
074000                                                                          
074100 DD-SKAPA-ORDERHUVUDTRANS-RHR SECTION.                                    
074200                                                                          
074202     PERFORM S01-KOLLA-KONSOLIDERING                                      
074210                                                                          
074300     IF MID-IDVTYP = '3'                                                  
074400       MOVE 'VDI '                 TO OHUV-MID-IDSYSTEM                   
074500       MOVE MID-IDDISTR            TO OHUV-MID-IDDISTR                    
074600       MOVE MID-IDKUNDNR           TO OHUV-MID-IDKUNDNR                   
074602       IF KONSOLIDERING                                                   
074603          MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                    
074604          MOVE OHKK-IDORDNR-UT     TO W-IDORDNR                           
074605          STRING '9311 OC ' W-IDORDNR DELIMITED BY SIZE                   
074606                                 INTO OHUV-MID-BEKUNDRF                   
074660       ELSE                                                               
074670          MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                    
074680          MOVE MID-RHRB-BEKUNDRF   TO OHUV-MID-BEKUNDRF                   
074690       END-IF                                                             
074800       MOVE MID-RHRB-KDORDKL        TO OHUV-MID-KDORDKL                   
074810       MOVE OHKK-FLORDTIL          TO OHUV-MID-FLORDTIL                   
074900                                                                          
075000       CALL W460DIS1 USING DIS1-W460DIS1                                  
075100                                                                          
075200       IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                               
075300          OHUV-MID-KDORDKL = '3'                                          
075400          MOVE SPACE               TO OHUV-MID-KDFRAKT                    
075500       ELSE                                                               
075600          IF MID-RHRB-KDFRAKT = ZERO                                      
075700             MOVE SPACE            TO OHUV-MID-KDFRAKT                    
075800          ELSE                                                            
075900             MOVE MID-RHRB-KDFRAKT  TO OHUV-MID-KDFRAKT                   
076000          END-IF                                                          
076100       END-IF                                                             
076200       MOVE SPACE                  TO OHUV-MID-TIRFS                      
076400       MOVE SPACE                  TO OHUV-MID-KDFAKTYP                   
076500       MOVE MID-RHRB-FLRESTN        TO OHUV-MID-FLRESTN                   
076600       MOVE MID-RHRB-KDTPOTYP       TO OHUV-MID-KDTPOTYP                  
076700       MOVE SPACE                  TO OHUV-MID-TITPO                      
076800       MOVE MID-RHRB-BELAGINS-DEL   TO OHUV-MID-BELAGINS                  
076900       MOVE MID-RHRB-BEGMT-RAD1     TO OHUV-MID-BEGMT-RAD1                
077000       MOVE MID-RHRB-BEGMT-RAD2     TO OHUV-MID-BEGMT-RAD2                
077100       MOVE MID-RHRB-ADGMT-GATA     TO OHUV-MID-ADGMT-GATA                
077200       MOVE MID-RHRB-ADGMT-PADR     TO OHUV-MID-ADGMT-PADR                
077300       IF MID-RHRB-BEGMT = SPACE       AND                                
077400          MID-RHRB-ADGMT-GATA = SPACE  AND                                
077500          MID-RHRB-ADGMT-PADR = SPACE                                     
077600         MOVE SPACE                TO OHUV-MID-KDROPACK                   
077700       ELSE                                                               
077800         MOVE ZERO                 TO OHUV-MID-KDROPACK                   
077900       END-IF                                                             
078000       MOVE SPACE                  TO OHUV-MID-IDKONTO                    
078100       MOVE SPACE                  TO OHUV-MID-IDKST                      
078200       MOVE SPACE                  TO OHUV-MID-IDANALYS                   
078300       MOVE MID-RHRB-BEVARREF       TO OHUV-MID-BEVARREF                  
078400       MOVE SPACE                  TO OHUV-MID-KDTULLVE                   
078500       MOVE SPACE                  TO OHUV-MID-KDNOTES                    
078600       MOVE SPACE                  TO OHUV-MID-FLAUTFAK                   
078700       MOVE NEJ                    TO OHUV-MID-FLAUTPAC                   
078800       IF DIST20-EMBALLAGE                                                
078900          MOVE JA                  TO OHUV-MID-FLEMBORD                   
079000       ELSE                                                               
079100          MOVE NEJ                 TO OHUV-MID-FLEMBORD                   
079200       END-IF                                                             
079300       MOVE NEJ                    TO OHUV-MID-FLOVRLEV                   
079400       MOVE MID-RHRB-IDKAMPRF       TO OHUV-MID-IDKAMPRF                  
079500       MOVE SPACE                  TO OHUV-MID-IDFTG                      
079600       MOVE SPACE                  TO OHUV-MID-IDSKYLT                    
079700       MOVE SPACE                  TO OHUV-MID-IDDC                       
079800       MOVE SPACE                  TO OHUV-MID-FLLSBOK                    
079900       MOVE SPACE                  TO OHUV-MID-BEBETRAD-1                 
080000       MOVE SPACE                  TO OHUV-MID-BEBETRAD-2                 
080100       MOVE SPACE                  TO OHUV-MID-ADBETRAD-1                 
080200       MOVE SPACE                  TO OHUV-MID-ADBETRAD-2                 
080300     ELSE                                                                 
080400       MOVE 'VDI '                 TO OHUV-MID-IDSYSTEM                   
080500       MOVE MID-IDDISTR            TO OHUV-MID-IDDISTR                    
080600       MOVE MID-IDKUNDNR           TO OHUV-MID-IDKUNDNR                   
080602       IF KONSOLIDERING                                                   
080603          MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                    
080604          MOVE OHKK-IDORDNR-UT     TO W-IDORDNR                           
080605          STRING '9311 OC ' W-IDORDNR DELIMITED BY SIZE                   
080606                                 INTO OHUV-MID-BEKUNDRF                   
080660       ELSE                                                               
080670          MOVE MID-IDORDNR         TO OHUV-MID-IDORDNR                    
080680          MOVE MID-RHRA-BEKUNDRF   TO OHUV-MID-BEKUNDRF                   
080690       END-IF                                                             
080800       MOVE MID-RHRA-KDORDKL        TO OHUV-MID-KDORDKL                   
080810       MOVE OHKK-FLORDTIL          TO OHUV-MID-FLORDTIL                   
080900                                                                          
081000       CALL W460DIS1 USING DIS1-W460DIS1                                  
081100                                                                          
081200       IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                               
081300          OHUV-MID-KDORDKL = '3'                                          
081400          MOVE SPACE               TO OHUV-MID-KDFRAKT                    
081500       ELSE                                                               
081600          IF MID-RHRA-KDFRAKT = ZERO                                      
081700             MOVE SPACE            TO OHUV-MID-KDFRAKT                    
081800          ELSE                                                            
081900             MOVE MID-RHRA-KDFRAKT  TO OHUV-MID-KDFRAKT                   
082000          END-IF                                                          
082100       END-IF                                                             
082200       MOVE SPACE                  TO OHUV-MID-TIRFS                      
082400       MOVE SPACE                  TO OHUV-MID-KDFAKTYP                   
082500       MOVE MID-RHRA-FLRESTN        TO OHUV-MID-FLRESTN                   
082600       MOVE MID-RHRA-KDTPOTYP       TO OHUV-MID-KDTPOTYP                  
082700       MOVE SPACE                  TO OHUV-MID-TITPO                      
082800       MOVE MID-RHRA-BELAGINS-DEL   TO OHUV-MID-BELAGINS                  
082900       MOVE MID-RHRA-BEGMT-RAD1     TO OHUV-MID-BEGMT-RAD1                
083000       MOVE MID-RHRA-BEGMT-RAD2     TO OHUV-MID-BEGMT-RAD2                
083100       MOVE MID-RHRA-ADGMT-GATA     TO OHUV-MID-ADGMT-GATA                
083200       MOVE MID-RHRA-ADGMT-PADR     TO OHUV-MID-ADGMT-PADR                
083300       IF MID-RHRA-BEGMT = SPACE       AND                                
083400          MID-RHRA-ADGMT-GATA = SPACE  AND                                
083500          MID-RHRA-ADGMT-PADR = SPACE                                     
083600         MOVE SPACE                TO OHUV-MID-KDROPACK                   
083700       ELSE                                                               
083800         MOVE ZERO                 TO OHUV-MID-KDROPACK                   
083900       END-IF                                                             
084000       MOVE SPACE                  TO OHUV-MID-IDKONTO                    
084100       MOVE SPACE                  TO OHUV-MID-IDKST                      
084200       MOVE SPACE                  TO OHUV-MID-IDANALYS                   
084300       MOVE MID-RHRA-BEVARREF       TO OHUV-MID-BEVARREF                  
084400       MOVE SPACE                  TO OHUV-MID-KDTULLVE                   
084500       MOVE SPACE                  TO OHUV-MID-KDNOTES                    
084600       MOVE SPACE                  TO OHUV-MID-FLAUTFAK                   
084700       MOVE NEJ                    TO OHUV-MID-FLAUTPAC                   
084800       IF DIST20-EMBALLAGE                                                
084900          MOVE JA                  TO OHUV-MID-FLEMBORD                   
085000       ELSE                                                               
085100          MOVE NEJ                 TO OHUV-MID-FLEMBORD                   
085200       END-IF                                                             
085300       MOVE NEJ                    TO OHUV-MID-FLOVRLEV                   
085400       MOVE MID-RHRA-IDKAMPRF       TO OHUV-MID-IDKAMPRF                  
085500       MOVE SPACE                  TO OHUV-MID-IDFTG                      
085600       MOVE SPACE                  TO OHUV-MID-IDSKYLT                    
085700       MOVE SPACE                  TO OHUV-MID-IDDC                       
085800       MOVE SPACE                  TO OHUV-MID-FLLSBOK                    
085900       MOVE SPACE                  TO OHUV-MID-BEBETRAD-1                 
086000       MOVE SPACE                  TO OHUV-MID-BEBETRAD-2                 
086100       MOVE SPACE                  TO OHUV-MID-ADBETRAD-1                 
086200       MOVE SPACE                  TO OHUV-MID-ADBETRAD-2                 
086300     END-IF                                                               
086400     MOVE ZERO                   TO OHUV-MID-IDDEPT                       
086500                                                                          
086600     MOVE SPACE                  TO OHUV-MID-KDORDTYP-LDC                 
086700     MOVE ZERO                   TO OHUV-MID-TIREPDAT                     
086800     MOVE NEJ                    TO OHUV-MID-FLFORBI                      
086900                                                                          
087000     IF GMT-FLLDCKND = JA                                                 
087100        IF MID-RHRA-KDORDKL = 1                                           
087200           MOVE 'FW'             TO OHUV-MID-KDORDTYP-LDC                 
087300        END-IF                                                            
087400     END-IF                                                               
087500     .                                                                    
087600     EJECT                                                                
087700                                                                          
087800                                                                          
087900 E-SKAPA-ORDERRADTRANS SECTION.                                           
088000                                                                          
088100     IF MID-IDPTYP = 'RHI'                                                
088200       PERFORM EA-SKAPA-ORDERTRANS-RHI                                    
088300     ELSE                                                                 
088400       IF MID-IDPTYP = 'RHN'                                              
088500         PERFORM EB-SKAPA-ORDERTRANS-RHN                                  
088600       ELSE                                                               
088700         IF MID-IDPTYP = 'RHP'                                            
088800           PERFORM EC-SKAPA-ORDERTRANS-RHP                                
088900         ELSE                                                             
089000           IF MID-IDPTYP = 'RHR'                                          
089100             PERFORM ED-SKAPA-ORDERTRANS-RHR                              
089200           END-IF                                                         
089300         END-IF                                                           
089400       END-IF                                                             
089500     END-IF                                                               
089600     .                                                                    
089700     EJECT                                                                
089800                                                                          
089900 EA-SKAPA-ORDERTRANS-RHI SECTION.                                         
090000                                                                          
090100     MOVE MID-RHI-KVRADER        TO W-KVRADER                             
090200     MOVE ZERO                   TO ORAD-IX                               
090300     MOVE SPACE                  TO KOM-AREA                              
090400                                                                          
090500     COMPUTE MSG-KVLL = LENGTH OF ORAD-MID-W4I25201 + 17                  
090600                                                                          
090700     MOVE LOW-VALUE              TO MSG-KDZ1                              
090800     MOVE LOW-VALUE              TO MSG-KDZ2                              
090900     MOVE 'W4T252Y '             TO MSG-KDTRANS-1                         
091000     MOVE '4252'                 TO MSG-IDTRANS-1                         
091100     MOVE '1'                    TO MSG-KDMFSFOR-1                        
091200                                                                          
091300     MOVE +1    TO MID-IX                                                 
091400     PERFORM UNTIL MID-IX > W-KVRADER OR                                  
091500                   MSG-KOM-IDMFSMED NOT = SPACE                           
091600                                                                          
091700        MOVE 'VDI '              TO ORAD-MID-IDSYSTEM                     
091800        MOVE MID-IDDISTR         TO ORAD-MID-IDDISTR                      
091900        MOVE MID-IDKUNDNR        TO ORAD-MID-IDKUNDNR                     
092000        IF KONSOLIDERING                                                  
092010           MOVE OHKK-IDORDNR-UT  TO ORAD-MID-IDORDNR                      
092011           MOVE MID-IDORDNR      TO ORAD-MID-IDKUNDRF-RO                  
092020        ELSE                                                              
092021           MOVE MID-IDORDNR      TO ORAD-MID-IDORDNR                      
092022           MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                  
092030        END-IF                                                            
092100        MOVE MID-RHI-BEKUNDRF    TO ORAD-MID-BEVOLREF                     
092200        MOVE 'N'                 TO ORAD-MID-FLSLUT                       
092300                                                                          
092400        MOVE +1     TO ORAD-IX                                            
092500        PERFORM UNTIL MID-IX > W-KVRADER OR                               
092600                      ORAD-IX > ORAD-IX-MAX                               
092700           MOVE MID-RHI-IDARTNR (MID-IX)                                  
092800                                 TO ORAD-MID-IDARTNR   (ORAD-IX)          
092900           IF MID-RHI-IDARTNR (MID-IX) NUMERIC                            
093000              MOVE MID-RHI-IDARTNR (MID-IX)                               
093100                                 TO REK-IDARTNR                           
093200              MOVE 9             TO REK-LNGD                              
093300              MOVE 0             TO REK-REKSIFFR                          
093400              CALL W009KSIF USING REK-IDARTNR                             
093500                                  REK-LNGD                                
093600                                  REK-REKSIFFR                            
093700              MOVE REK-REKSIFFR  TO ORAD-MID-REKSIFFR  (ORAD-IX)          
093800           END-IF                                                         
093900           MOVE MID-RHI-KVBEART (MID-IX)                                  
094000                                 TO ORAD-MID-KVBEART   (ORAD-IX)          
094100           MOVE SPACE            TO ORAD-MID-PRARTNTO  (ORAD-IX)          
094200           MOVE SPACE            TO ORAD-MID-TITPO     (ORAD-IX)          
094300           MOVE SPACE            TO ORAD-MID-FLRESTN   (ORAD-IX)          
094400           MOVE MID-RHI-KDKVBRYT (MID-IX)                                 
094500                                 TO ORAD-MID-KDKVBRYT  (ORAD-IX)          
094600           MOVE SPACE            TO ORAD-MID-FLINVEST  (ORAD-IX)          
094700           MOVE ZERO             TO ORAD-MID-KDVRINFO  (ORAD-IX)          
094800           MOVE SPACE            TO ORAD-MID-IDKONTO   (ORAD-IX)          
094900           MOVE SPACE            TO ORAD-MID-IDKST     (ORAD-IX)          
095000           MOVE MID-RHI-BERADREF (MID-IX)                                 
095100                                 TO ORAD-MID-BERADREF  (ORAD-IX)          
095200           MOVE MID-RHI-KDDSP (MID-IX)                                    
095300                                 TO ORAD-MID-KDDSP     (ORAD-IX)          
095400           MOVE MID-RHI-FLSLATT (MID-IX)                                  
095500                                 TO ORAD-MID-FLSLATT   (ORAD-IX)          
095600           MOVE SPACE            TO ORAD-MID-IDBIL     (ORAD-IX)          
095700                                                                          
095800           MOVE SPACE            TO                                       
095900                                ORAD-MID-PRARTNTO-LOC  (ORAD-IX)          
096000           MOVE SPACE            TO                                       
096100                               ORAD-MID-PRARTBTO-LOC   (ORAD-IX)          
096200           MOVE SPACE            TO                                       
096300                                    ORAD-MID-KDVALISO  (ORAD-IX)          
096400           MOVE SPACE            TO                                       
096500                                    ORAD-MID-KDVAT     (ORAD-IX)          
096600           MOVE 0                TO                                       
096700                                    ORAD-MID-RERAB     (ORAD-IX)          
096800           MOVE SPACE            TO                                       
096900                                    ORAD-MID-KDRAB     (ORAD-IX)          
097000           MOVE SPACE            TO                                       
097100                                ORAD-MID-BEART-VIPS    (ORAD-IX)          
097200                                                                          
097300           MOVE SPACE            TO ORAD-MID-IDKUNDRF-WIP(ORAD-IX)        
097400                                                                          
097500           IF GMT-FLLDCKND = JA                                           
097600              IF MID-RHI-KDORDKL = 1                                      
097700                 MOVE MID-RHI-BERADREF (MID-IX)                           
097800                                 TO ORAD-MID-IDKUNDRF-WIP(ORAD-IX)        
097900              END-IF                                                      
098000           END-IF                                                         
098100                                                                          
098200           ADD +1 TO MID-IX                                               
098300           ADD +1 TO ORAD-IX                                              
098400        END-PERFORM                                                       
098500                                                                          
098600        IF ORAD-IX > 0                                                    
098700           IF MID-IX > W-KVRADER                                          
098800              MOVE 'J'           TO ORAD-MID-FLSLUT                       
098900           END-IF                                                         
099000                                                                          
099100           MOVE KOM-AREA         TO MSG-INDATA-MINUS-1-TRANSKOD           
099200           CALL W006KOM USING MSG-PCB                                     
099300                              DISP-PCB                                    
099400                              KOMA-PCB                                    
099500                              MSG-KOM-WMSGKOMI                            
099600                              MSG-IO-AREA                                 
099700        END-IF                                                            
099800        MOVE ZERO  TO ORAD-IX                                             
099900        MOVE SPACE TO ORAD-MID-W4I25201                                   
100000                                                                          
100100     END-PERFORM                                                          
100200     .                                                                    
100300     EJECT                                                                
100400                                                                          
100500 EB-SKAPA-ORDERTRANS-RHN SECTION.                                         
100600                                                                          
100700     MOVE MID-RHN-KVRADER        TO W-KVRADER                             
100800     MOVE ZERO                   TO ORAD-IX                               
100900     MOVE SPACE                  TO KOM-AREA                              
101000                                                                          
101100     COMPUTE MSG-KVLL = LENGTH OF ORAD-MID-W4I25201 + 17                  
101200                                                                          
101300     MOVE LOW-VALUE              TO MSG-KDZ1                              
101400     MOVE LOW-VALUE              TO MSG-KDZ2                              
101500     MOVE 'W4T252Y '             TO MSG-KDTRANS-1                         
101600     MOVE '4252'                 TO MSG-IDTRANS-1                         
101700     MOVE '1'                    TO MSG-KDMFSFOR-1                        
101800                                                                          
101900     MOVE +1    TO MID-IX                                                 
102000     PERFORM UNTIL MID-IX > W-KVRADER OR                                  
102100                   MSG-KOM-IDMFSMED NOT = SPACE                           
102200                                                                          
102300        MOVE 'VDI '              TO ORAD-MID-IDSYSTEM                     
102400        MOVE MID-IDDISTR         TO ORAD-MID-IDDISTR                      
102500        MOVE MID-IDKUNDNR        TO ORAD-MID-IDKUNDNR                     
102600        IF KONSOLIDERING                                                  
102601           MOVE OHKK-IDORDNR-UT  TO ORAD-MID-IDORDNR                      
102602           MOVE MID-IDORDNR      TO ORAD-MID-IDKUNDRF-RO                  
102603        ELSE                                                              
102604           MOVE MID-IDORDNR      TO ORAD-MID-IDORDNR                      
102605           MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                  
102606        END-IF                                                            
102700        MOVE MID-RHN-BEKUNDRF    TO ORAD-MID-BEVOLREF                     
102800        MOVE 'N'                 TO ORAD-MID-FLSLUT                       
102900                                                                          
103000        MOVE +1     TO ORAD-IX                                            
103100        PERFORM UNTIL MID-IX > W-KVRADER OR                               
103200                      ORAD-IX > ORAD-IX-MAX                               
103300           MOVE MID-RHN-IDARTNR (MID-IX)                                  
103400                                 TO ORAD-MID-IDARTNR   (ORAD-IX)          
103500           IF MID-RHN-IDARTNR (MID-IX) NUMERIC                            
103600              MOVE MID-RHN-IDARTNR (MID-IX)                               
103700                                 TO REK-IDARTNR                           
103800              MOVE 9             TO REK-LNGD                              
103900              MOVE 0             TO REK-REKSIFFR                          
104000              CALL W009KSIF USING REK-IDARTNR                             
104100                                  REK-LNGD                                
104200                                  REK-REKSIFFR                            
104300              MOVE REK-REKSIFFR  TO ORAD-MID-REKSIFFR  (ORAD-IX)          
104400           END-IF                                                         
104500           MOVE MID-RHN-KVBEART (MID-IX)                                  
104600                                 TO ORAD-MID-KVBEART   (ORAD-IX)          
104700           MOVE SPACE            TO ORAD-MID-PRARTNTO  (ORAD-IX)          
104800           MOVE SPACE            TO ORAD-MID-TITPO     (ORAD-IX)          
104900           MOVE SPACE            TO ORAD-MID-FLRESTN   (ORAD-IX)          
105000           MOVE MID-RHN-KDKVBRYT (MID-IX)                                 
105100                                 TO ORAD-MID-KDKVBRYT  (ORAD-IX)          
105200           MOVE SPACE            TO ORAD-MID-FLINVEST  (ORAD-IX)          
105300           MOVE ZERO             TO ORAD-MID-KDVRINFO  (ORAD-IX)          
105400           MOVE SPACE            TO ORAD-MID-IDKONTO   (ORAD-IX)          
105500           MOVE SPACE            TO ORAD-MID-IDKST     (ORAD-IX)          
105600           MOVE MID-RHN-BERADREF (MID-IX)                                 
105700                                 TO ORAD-MID-BERADREF  (ORAD-IX)          
105800           MOVE MID-RHN-KDDSP (MID-IX)                                    
105900                                 TO ORAD-MID-KDDSP     (ORAD-IX)          
106000           MOVE MID-RHN-FLSLATT (MID-IX)                                  
106100                                 TO ORAD-MID-FLSLATT   (ORAD-IX)          
106200           MOVE SPACE            TO ORAD-MID-IDBIL     (ORAD-IX)          
106300                                                                          
106400           MOVE SPACE            TO                                       
106500                               ORAD-MID-PRARTNTO-LOC  (ORAD-IX)           
106600           MOVE SPACE            TO                                       
106700                               ORAD-MID-PRARTBTO-LOC   (ORAD-IX)          
106800           MOVE SPACE            TO                                       
106900                                    ORAD-MID-KDVALISO  (ORAD-IX)          
107000           MOVE SPACE            TO                                       
107100                                    ORAD-MID-KDVAT     (ORAD-IX)          
107200           MOVE ZERO             TO                                       
107300                                    ORAD-MID-RERAB     (ORAD-IX)          
107400           MOVE SPACE            TO                                       
107500                                    ORAD-MID-KDRAB     (ORAD-IX)          
107600           MOVE SPACE            TO                                       
107700                                ORAD-MID-BEART-VIPS    (ORAD-IX)          
107800                                                                          
107900           MOVE SPACE            TO ORAD-MID-IDKUNDRF-WIP(ORAD-IX)        
108000                                                                          
108100           IF GMT-FLLDCKND = JA                                           
108200              IF MID-RHN-KDORDKL = 1                                      
108300                 MOVE MID-RHN-BERADREF (MID-IX)                           
108400                                 TO ORAD-MID-IDKUNDRF-WIP(ORAD-IX)        
108500              END-IF                                                      
108600           END-IF                                                         
108700           ADD +1 TO MID-IX                                               
108800           ADD +1 TO ORAD-IX                                              
108900        END-PERFORM                                                       
109000                                                                          
109100        IF ORAD-IX > 0                                                    
109200           IF MID-IX > W-KVRADER                                          
109300              MOVE 'J'           TO ORAD-MID-FLSLUT                       
109400           END-IF                                                         
109500                                                                          
109600           MOVE KOM-AREA         TO MSG-INDATA-MINUS-1-TRANSKOD           
109700           CALL W006KOM USING MSG-PCB                                     
109800                              DISP-PCB                                    
109900                              KOMA-PCB                                    
110000                              MSG-KOM-WMSGKOMI                            
110100                              MSG-IO-AREA                                 
110200        END-IF                                                            
110300        MOVE ZERO  TO ORAD-IX                                             
110400        MOVE SPACE TO ORAD-MID-W4I25201                                   
110500                                                                          
110600     END-PERFORM                                                          
110700     .                                                                    
110800     EJECT                                                                
110900                                                                          
111000 EC-SKAPA-ORDERTRANS-RHP SECTION.                                         
111100                                                                          
111200     IF MID-IDVTYP = '3'                                                  
111300       MOVE MID-RHPB-KVRADER        TO W-KVRADER                          
111400       MOVE ZERO                   TO ORAD-IX                             
111500       MOVE SPACE                  TO KOM-AREA                            
111600                                                                          
111700       COMPUTE MSG-KVLL = LENGTH OF ORAD-MID-W4I25201 + 17                
111800                                                                          
111900       MOVE LOW-VALUE              TO MSG-KDZ1                            
112000       MOVE LOW-VALUE              TO MSG-KDZ2                            
112100       MOVE 'W4T252Y '             TO MSG-KDTRANS-1                       
112200       MOVE '4252'                 TO MSG-IDTRANS-1                       
112300       MOVE '1'                    TO MSG-KDMFSFOR-1                      
112400                                                                          
112500       MOVE +1    TO MID-IX                                               
112600       PERFORM UNTIL MID-IX > W-KVRADER OR                                
112700                     MSG-KOM-IDMFSMED NOT = SPACE                         
112800                                                                          
112900          MOVE 'VDI '              TO ORAD-MID-IDSYSTEM                   
113000          MOVE MID-IDDISTR         TO ORAD-MID-IDDISTR                    
113100          MOVE MID-IDKUNDNR        TO ORAD-MID-IDKUNDNR                   
113200          IF KONSOLIDERING                                                
113201             MOVE OHKK-IDORDNR-UT  TO ORAD-MID-IDORDNR                    
113202             MOVE MID-IDORDNR      TO ORAD-MID-IDKUNDRF-RO                
113203          ELSE                                                            
113204             MOVE MID-IDORDNR      TO ORAD-MID-IDORDNR                    
113205             MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                
113206          END-IF                                                          
113300          MOVE MID-RHPB-BEKUNDRF    TO ORAD-MID-BEVOLREF                  
113400          MOVE 'N'                 TO ORAD-MID-FLSLUT                     
113500                                                                          
113600          MOVE +1     TO ORAD-IX                                          
113700          PERFORM UNTIL MID-IX > W-KVRADER OR                             
113800                        ORAD-IX > ORAD-IX-MAX                             
113900             MOVE MID-RHPB-IDARTNR (MID-IX)                               
114000                                   TO ORAD-MID-IDARTNR   (ORAD-IX)        
114100             IF MID-RHPB-IDARTNR (MID-IX) NUMERIC                         
114200                MOVE MID-RHPB-IDARTNR (MID-IX)                            
114300                                   TO REK-IDARTNR                         
114400                MOVE 9             TO REK-LNGD                            
114500                MOVE 0             TO REK-REKSIFFR                        
114600                CALL W009KSIF USING REK-IDARTNR                           
114700                                    REK-LNGD                              
114800                                    REK-REKSIFFR                          
114900                MOVE REK-REKSIFFR  TO ORAD-MID-REKSIFFR  (ORAD-IX)        
115000             END-IF                                                       
115100             MOVE MID-RHPB-KVBEART (MID-IX)                               
115200                                   TO ORAD-MID-KVBEART   (ORAD-IX)        
115300             MOVE SPACE            TO ORAD-MID-PRARTNTO  (ORAD-IX)        
115400                                      ORAD-MID-TITPO     (ORAD-IX)        
115500             IF MID-RHPB-IDBIL (MID-IX) > SPACE                           
115600               MOVE JA             TO ORAD-MID-FLDIRLEV  (ORAD-IX)        
115700             ELSE                                                         
115800               MOVE SPACE          TO ORAD-MID-FLDIRLEV  (ORAD-IX)        
115900             END-IF                                                       
116000             MOVE MID-RHPB-KDKVBRYT (MID-IX)                              
116100                                   TO ORAD-MID-KDKVBRYT  (ORAD-IX)        
116200             MOVE SPACE            TO ORAD-MID-FLINVEST  (ORAD-IX)        
116300             MOVE ZERO             TO ORAD-MID-KDVRINFO  (ORAD-IX)        
116400             MOVE SPACE            TO ORAD-MID-IDKONTO   (ORAD-IX)        
116500             MOVE SPACE            TO ORAD-MID-IDKST     (ORAD-IX)        
116600             MOVE MID-RHPB-BERADREF (MID-IX)                              
116700                                   TO ORAD-MID-BERADREF  (ORAD-IX)        
116800             MOVE MID-RHPB-KDDSP (MID-IX)                                 
116900                                   TO ORAD-MID-KDDSP     (ORAD-IX)        
117000             MOVE MID-RHPB-FLSLATT (MID-IX)                               
117100                                   TO ORAD-MID-FLSLATT   (ORAD-IX)        
117200             MOVE MID-RHPB-IDBIL   (MID-IX)                               
117300                                   TO ORAD-MID-IDBIL     (ORAD-IX)        
117400             MOVE MID-RHPB-PRARTNTO-LOC (MID-IX)                          
117500                                    TO W-PRARTXXX-MED-PUNKT               
117600             MOVE W-PRARTXXX-MED-PUNKT TO                                 
117700                                   ORAD-MID-PRARTNTO-LOC (ORAD-IX)        
117800             MOVE MID-RHPB-PRARTBTO-LOC (MID-IX)                          
117900                                     TO W-PRARTXXX-MED-PUNKT              
118000             MOVE W-PRARTXXX-MED-PUNKT TO                                 
118100                                   ORAD-MID-PRARTBTO-LOC (ORAD-IX)        
118200             MOVE MID-RHPB-KDVALISO (MID-IX) TO                           
118300                                      ORAD-MID-KDVALISO  (ORAD-IX)        
118400             MOVE MID-RHPB-KDVAT (MID-IX)                                 
118500                                   TO ORAD-MID-KDVAT     (ORAD-IX)        
118600             MOVE MID-RHPB-RERAB (MID-IX)                                 
118700                                   TO ORAD-MID-RERAB     (ORAD-IX)        
118800             MOVE MID-RHPB-KDRAB (MID-IX)                                 
118900                                   TO ORAD-MID-KDRAB     (ORAD-IX)        
119000             MOVE MID-RHPB-BEART-VIPS (MID-IX)                            
119100                               TO ORAD-MID-BEART-VIPS    (ORAD-IX)        
119200                                                                          
119300             MOVE SPACE          TO ORAD-MID-IDKUNDRF-WIP(ORAD-IX)        
119400                                                                          
119500             IF GMT-FLLDCKND = JA                                         
119600                IF MID-RHPB-KDORDKL = 1                                   
119700                   MOVE MID-RHPB-BERADREF (MID-IX)                        
119800                                                                          
119900                                 TO ORAD-MID-IDKUNDRF-WIP(ORAD-IX)        
120000                END-IF                                                    
120100             END-IF                                                       
120200             ADD +1 TO MID-IX                                             
120300             ADD +1 TO ORAD-IX                                            
120400          END-PERFORM                                                     
120500                                                                          
120600          IF ORAD-IX > 0                                                  
120700             IF MID-IX > W-KVRADER                                        
120800                MOVE 'J'           TO ORAD-MID-FLSLUT                     
120900             END-IF                                                       
121000                                                                          
121100             MOVE KOM-AREA         TO MSG-INDATA-MINUS-1-TRANSKOD         
121200             CALL W006KOM USING MSG-PCB                                   
121300                                DISP-PCB                                  
121400                                KOMA-PCB                                  
121500                                MSG-KOM-WMSGKOMI                          
121600                                MSG-IO-AREA                               
121700          END-IF                                                          
121800          MOVE ZERO  TO ORAD-IX                                           
121900          MOVE SPACE TO ORAD-MID-W4I25201                                 
122000       END-PERFORM                                                        
122100     ELSE                                                                 
122200       MOVE MID-RHPA-KVRADER        TO W-KVRADER                          
122300       MOVE ZERO                   TO ORAD-IX                             
122400       MOVE SPACE                  TO KOM-AREA                            
122500                                                                          
122600       COMPUTE MSG-KVLL = LENGTH OF ORAD-MID-W4I25201 + 17                
122700                                                                          
122800       MOVE LOW-VALUE              TO MSG-KDZ1                            
122900       MOVE LOW-VALUE              TO MSG-KDZ2                            
123000       MOVE 'W4T252Y '             TO MSG-KDTRANS-1                       
123100       MOVE '4252'                 TO MSG-IDTRANS-1                       
123200       MOVE '1'                    TO MSG-KDMFSFOR-1                      
123300                                                                          
123400       MOVE +1    TO MID-IX                                               
123500       PERFORM UNTIL MID-IX > W-KVRADER OR                                
123600                     MSG-KOM-IDMFSMED NOT = SPACE                         
123700                                                                          
123800          MOVE 'VDI '              TO ORAD-MID-IDSYSTEM                   
123900          MOVE MID-IDDISTR         TO ORAD-MID-IDDISTR                    
124000          MOVE MID-IDKUNDNR        TO ORAD-MID-IDKUNDNR                   
124002          IF KONSOLIDERING                                                
124003             MOVE OHKK-IDORDNR-UT  TO ORAD-MID-IDORDNR                    
124004             MOVE MID-IDORDNR      TO ORAD-MID-IDKUNDRF-RO                
124005          ELSE                                                            
124006             MOVE MID-IDORDNR      TO ORAD-MID-IDORDNR                    
124007             MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                
124008          END-IF                                                          
124200          MOVE MID-RHPA-BEKUNDRF    TO ORAD-MID-BEVOLREF                  
124300          MOVE 'N'                 TO ORAD-MID-FLSLUT                     
124400                                                                          
124500          MOVE +1     TO ORAD-IX                                          
124600          PERFORM UNTIL MID-IX > W-KVRADER OR                             
124700                        ORAD-IX > ORAD-IX-MAX                             
124800             MOVE MID-RHPA-IDARTNR (MID-IX)                               
124900                                   TO ORAD-MID-IDARTNR   (ORAD-IX)        
125000             IF MID-RHPA-IDARTNR (MID-IX) NUMERIC                         
125100                MOVE MID-RHPA-IDARTNR (MID-IX)                            
125200                                   TO REK-IDARTNR                         
125300                MOVE 9             TO REK-LNGD                            
125400                MOVE 0             TO REK-REKSIFFR                        
125500                CALL W009KSIF USING REK-IDARTNR                           
125600                                    REK-LNGD                              
125700                                    REK-REKSIFFR                          
125800                MOVE REK-REKSIFFR  TO ORAD-MID-REKSIFFR  (ORAD-IX)        
125900             END-IF                                                       
126000             MOVE MID-RHPA-KVBEART (MID-IX)                               
126100                                   TO ORAD-MID-KVBEART   (ORAD-IX)        
126200             MOVE SPACE            TO ORAD-MID-PRARTNTO  (ORAD-IX)        
126300                                      ORAD-MID-TITPO     (ORAD-IX)        
126400             IF MID-RHPA-IDBIL (MID-IX) > SPACE                           
126500               MOVE JA             TO ORAD-MID-FLDIRLEV  (ORAD-IX)        
126600             ELSE                                                         
126700               MOVE SPACE          TO ORAD-MID-FLDIRLEV  (ORAD-IX)        
126800             END-IF                                                       
126900             MOVE MID-RHPA-KDKVBRYT (MID-IX)                              
127000                                   TO ORAD-MID-KDKVBRYT  (ORAD-IX)        
127100             MOVE SPACE            TO ORAD-MID-FLINVEST  (ORAD-IX)        
127200             MOVE ZERO             TO ORAD-MID-KDVRINFO  (ORAD-IX)        
127300             MOVE SPACE            TO ORAD-MID-IDKONTO   (ORAD-IX)        
127400             MOVE SPACE            TO ORAD-MID-IDKST     (ORAD-IX)        
127500             MOVE MID-RHPA-BERADREF (MID-IX)                              
127600                                   TO ORAD-MID-BERADREF  (ORAD-IX)        
127700             MOVE MID-RHPA-KDDSP (MID-IX)                                 
127800                                   TO ORAD-MID-KDDSP     (ORAD-IX)        
127900             MOVE MID-RHPA-FLSLATT (MID-IX)                               
128000                                   TO ORAD-MID-FLSLATT   (ORAD-IX)        
128100             MOVE MID-RHPA-IDBIL   (MID-IX)                               
128200                                   TO ORAD-MID-IDBIL     (ORAD-IX)        
128300             MOVE SPACE            TO                                     
128400                                  ORAD-MID-PRARTNTO-LOC  (ORAD-IX)        
128500             MOVE SPACE            TO                                     
128600                                 ORAD-MID-PRARTBTO-LOC   (ORAD-IX)        
128700             MOVE SPACE            TO                                     
128800                                      ORAD-MID-KDVALISO  (ORAD-IX)        
128900             MOVE SPACE            TO                                     
129000                                      ORAD-MID-KDVAT     (ORAD-IX)        
129100             MOVE 0                TO                                     
129200                                      ORAD-MID-RERAB     (ORAD-IX)        
129300             MOVE SPACE            TO                                     
129400                                      ORAD-MID-KDRAB     (ORAD-IX)        
129500             MOVE SPACE            TO                                     
129600                                  ORAD-MID-BEART-VIPS    (ORAD-IX)        
129700                                                                          
129800             MOVE SPACE          TO ORAD-MID-IDKUNDRF-WIP(ORAD-IX)        
129900                                                                          
130000             IF GMT-FLLDCKND = JA                                         
130100                IF MID-RHPA-KDORDKL = 1                                   
130200                   MOVE MID-RHPA-BERADREF (MID-IX)                        
130300                                 TO ORAD-MID-IDKUNDRF-WIP(ORAD-IX)        
130400                END-IF                                                    
130500             END-IF                                                       
130600             ADD +1 TO MID-IX                                             
130700             ADD +1 TO ORAD-IX                                            
130800          END-PERFORM                                                     
130900                                                                          
131000          IF ORAD-IX > 0                                                  
131100             IF MID-IX > W-KVRADER                                        
131200                MOVE 'J'           TO ORAD-MID-FLSLUT                     
131300             END-IF                                                       
131400                                                                          
131500             MOVE KOM-AREA         TO MSG-INDATA-MINUS-1-TRANSKOD         
131600             CALL W006KOM USING MSG-PCB                                   
131700                                DISP-PCB                                  
131800                                KOMA-PCB                                  
131900                                MSG-KOM-WMSGKOMI                          
132000                                MSG-IO-AREA                               
132100          END-IF                                                          
132200          MOVE ZERO  TO ORAD-IX                                           
132300          MOVE SPACE TO ORAD-MID-W4I25201                                 
132400       END-PERFORM                                                        
132500     END-IF                                                               
132600     .                                                                    
132700     EJECT                                                                
132800                                                                          
132900 ED-SKAPA-ORDERTRANS-RHR SECTION.                                         
133000                                                                          
133100     IF MID-IDVTYP = '3'                                                  
133200       MOVE MID-RHRB-KVRADER        TO W-KVRADER                          
133300       MOVE ZERO                   TO ORAD-IX                             
133400       MOVE SPACE                  TO KOM-AREA                            
133500                                                                          
133600       COMPUTE MSG-KVLL = LENGTH OF ORAD-MID-W4I25201 + 17                
133700                                                                          
133800       MOVE LOW-VALUE              TO MSG-KDZ1                            
133900       MOVE LOW-VALUE              TO MSG-KDZ2                            
134000       MOVE 'W4T252Y '             TO MSG-KDTRANS-1                       
134100       MOVE '4252'                 TO MSG-IDTRANS-1                       
134200       MOVE '1'                    TO MSG-KDMFSFOR-1                      
134300                                                                          
134400       MOVE +1    TO MID-IX                                               
134500       PERFORM UNTIL MID-IX > W-KVRADER OR                                
134600                     MSG-KOM-IDMFSMED NOT = SPACE                         
134700                                                                          
134800          MOVE 'VDI '              TO ORAD-MID-IDSYSTEM                   
134900          MOVE MID-IDDISTR         TO ORAD-MID-IDDISTR                    
135000          MOVE MID-IDKUNDNR        TO ORAD-MID-IDKUNDNR                   
135002          IF KONSOLIDERING                                                
135003             MOVE OHKK-IDORDNR-UT  TO ORAD-MID-IDORDNR                    
135004             MOVE MID-IDORDNR      TO ORAD-MID-IDKUNDRF-RO                
135005          ELSE                                                            
135006             MOVE MID-IDORDNR      TO ORAD-MID-IDORDNR                    
135007             MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                
135008          END-IF                                                          
135200          MOVE MID-RHRB-BEKUNDRF   TO ORAD-MID-BEVOLREF                   
135300          MOVE 'N'                 TO ORAD-MID-FLSLUT                     
135400                                                                          
135500          MOVE +1     TO ORAD-IX                                          
135600          PERFORM UNTIL MID-IX > W-KVRADER OR                             
135700                        ORAD-IX > ORAD-IX-MAX                             
135800             MOVE MID-RHRB-IDARTNR (MID-IX)                               
135900                                   TO ORAD-MID-IDARTNR   (ORAD-IX)        
136000             IF MID-RHRB-IDARTNR (MID-IX) NUMERIC                         
136100                MOVE MID-RHRB-IDARTNR (MID-IX)                            
136200                                   TO REK-IDARTNR                         
136300                MOVE 9             TO REK-LNGD                            
136400                MOVE 0             TO REK-REKSIFFR                        
136500                CALL W009KSIF USING REK-IDARTNR                           
136600                                    REK-LNGD                              
136700                                    REK-REKSIFFR                          
136800                MOVE REK-REKSIFFR  TO ORAD-MID-REKSIFFR  (ORAD-IX)        
136900             END-IF                                                       
137000             MOVE MID-RHRB-KVBEART (MID-IX)                               
137100                                   TO ORAD-MID-KVBEART   (ORAD-IX)        
137200             MOVE SPACE            TO ORAD-MID-PRARTNTO  (ORAD-IX)        
137300                                      ORAD-MID-TITPO     (ORAD-IX)        
137400             IF MID-RHRB-IDBIL (MID-IX) > SPACE                           
137500               MOVE JA             TO ORAD-MID-FLDIRLEV  (ORAD-IX)        
137600             ELSE                                                         
137700               MOVE SPACE          TO ORAD-MID-FLDIRLEV  (ORAD-IX)        
137800             END-IF                                                       
137900             MOVE MID-RHRB-KDKVBRYT (MID-IX)                              
138000                                   TO ORAD-MID-KDKVBRYT  (ORAD-IX)        
138100             MOVE SPACE            TO ORAD-MID-FLINVEST  (ORAD-IX)        
138200             MOVE ZERO             TO ORAD-MID-KDVRINFO  (ORAD-IX)        
138300             MOVE SPACE            TO ORAD-MID-IDKONTO   (ORAD-IX)        
138400             MOVE SPACE            TO ORAD-MID-IDKST     (ORAD-IX)        
138500             MOVE MID-RHRB-BERADREF (MID-IX)                              
138600                                   TO ORAD-MID-BERADREF  (ORAD-IX)        
138700             MOVE MID-RHRB-KDDSP (MID-IX)                                 
138800                                   TO ORAD-MID-KDDSP     (ORAD-IX)        
138900             MOVE MID-RHRB-FLSLATT (MID-IX)                               
139000                                   TO ORAD-MID-FLSLATT   (ORAD-IX)        
139100             MOVE MID-RHRB-IDBIL   (MID-IX)                               
139200                                   TO ORAD-MID-IDBIL     (ORAD-IX)        
139300                                                                          
139400             MOVE MID-RHRB-PRARTNTO-LOC (MID-IX)                          
139500                                        TO W-PRARTXXX-MED-PUNKT           
139600             MOVE W-PRARTXXX-MED-PUNKT TO                                 
139700                                   ORAD-MID-PRARTNTO-LOC (ORAD-IX)        
139800                                                                          
139900             MOVE MID-RHRB-PRARTBTO-LOC (MID-IX)                          
140000                                         TO W-PRARTXXX-MED-PUNKT          
140100             MOVE W-PRARTXXX-MED-PUNKT TO                                 
140200                                   ORAD-MID-PRARTBTO-LOC (ORAD-IX)        
140300                                                                          
140400             MOVE MID-RHRB-KDVALISO (MID-IX)                              
140500                                   TO ORAD-MID-KDVALISO  (ORAD-IX)        
140600             MOVE MID-RHRB-KDVAT (MID-IX)                                 
140700                                   TO ORAD-MID-KDVAT     (ORAD-IX)        
140800             MOVE MID-RHRB-RERAB (MID-IX)                                 
140900                                   TO ORAD-MID-RERAB     (ORAD-IX)        
141000             MOVE MID-RHRB-KDRAB (MID-IX)                                 
141100                                   TO ORAD-MID-KDRAB     (ORAD-IX)        
141200             MOVE MID-RHRB-BEART-VIPS (MID-IX)                            
141300                              TO  ORAD-MID-BEART-VIPS    (ORAD-IX)        
141400                                                                          
141500             MOVE SPACE       TO  ORAD-MID-IDKUNDRF-WIP(ORAD-IX)          
141600                                                                          
141700             IF GMT-FLLDCKND = JA                                         
141800                IF MID-RHRB-KDORDKL = 1                                   
141900                   MOVE MID-RHRB-BERADREF (MID-IX)                        
142000                              TO ORAD-MID-IDKUNDRF-WIP(ORAD-IX)           
142100                END-IF                                                    
142200             END-IF                                                       
142300             ADD +1 TO MID-IX                                             
142400             ADD +1 TO ORAD-IX                                            
142500          END-PERFORM                                                     
142600                                                                          
142700          IF ORAD-IX > 0                                                  
142800             IF MID-IX > W-KVRADER                                        
142900                MOVE 'J'           TO ORAD-MID-FLSLUT                     
143000             END-IF                                                       
143100                                                                          
143200             MOVE KOM-AREA         TO MSG-INDATA-MINUS-1-TRANSKOD         
143300             CALL W006KOM USING MSG-PCB                                   
143400                                DISP-PCB                                  
143500                                KOMA-PCB                                  
143600                                MSG-KOM-WMSGKOMI                          
143700                                MSG-IO-AREA                               
143800          END-IF                                                          
143900          MOVE ZERO  TO ORAD-IX                                           
144000          MOVE SPACE TO ORAD-MID-W4I25201                                 
144100                                                                          
144200       END-PERFORM                                                        
144300     ELSE                                                                 
144400       MOVE MID-RHRA-KVRADER        TO W-KVRADER                          
144500       MOVE ZERO                   TO ORAD-IX                             
144600       MOVE SPACE                  TO KOM-AREA                            
144700                                                                          
144800       COMPUTE MSG-KVLL = LENGTH OF ORAD-MID-W4I25201 + 17                
144900                                                                          
145000       MOVE LOW-VALUE              TO MSG-KDZ1                            
145100       MOVE LOW-VALUE              TO MSG-KDZ2                            
145200       MOVE 'W4T252Y '             TO MSG-KDTRANS-1                       
145300       MOVE '4252'                 TO MSG-IDTRANS-1                       
145400       MOVE '1'                    TO MSG-KDMFSFOR-1                      
145500                                                                          
145600       MOVE +1    TO MID-IX                                               
145700       PERFORM UNTIL MID-IX > W-KVRADER OR                                
145800                     MSG-KOM-IDMFSMED NOT = SPACE                         
145900                                                                          
146000          MOVE 'VDI '              TO ORAD-MID-IDSYSTEM                   
146100          MOVE MID-IDDISTR         TO ORAD-MID-IDDISTR                    
146200          MOVE MID-IDKUNDNR        TO ORAD-MID-IDKUNDNR                   
146202          IF KONSOLIDERING                                                
146203             MOVE OHKK-IDORDNR-UT  TO ORAD-MID-IDORDNR                    
146204             MOVE MID-IDORDNR      TO ORAD-MID-IDKUNDRF-RO                
146205          ELSE                                                            
146206             MOVE MID-IDORDNR      TO ORAD-MID-IDORDNR                    
146207             MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                
146208          END-IF                                                          
146400          MOVE MID-RHRA-BEKUNDRF   TO ORAD-MID-BEVOLREF                   
146500          MOVE 'N'                 TO ORAD-MID-FLSLUT                     
146600                                                                          
146700          MOVE +1     TO ORAD-IX                                          
146800          PERFORM UNTIL MID-IX > W-KVRADER OR                             
146900                        ORAD-IX > ORAD-IX-MAX                             
147000             MOVE MID-RHRA-IDARTNR (MID-IX)                               
147100                                   TO ORAD-MID-IDARTNR   (ORAD-IX)        
147200             IF MID-RHRA-IDARTNR (MID-IX) NUMERIC                         
147300                MOVE MID-RHRA-IDARTNR (MID-IX)                            
147400                                   TO REK-IDARTNR                         
147500                MOVE 9             TO REK-LNGD                            
147600                MOVE 0             TO REK-REKSIFFR                        
147700                CALL W009KSIF USING REK-IDARTNR                           
147800                                    REK-LNGD                              
147900                                    REK-REKSIFFR                          
148000                MOVE REK-REKSIFFR  TO ORAD-MID-REKSIFFR  (ORAD-IX)        
148100             END-IF                                                       
148200             MOVE MID-RHRA-KVBEART (MID-IX)                               
148300                                   TO ORAD-MID-KVBEART   (ORAD-IX)        
148400             MOVE SPACE            TO ORAD-MID-PRARTNTO  (ORAD-IX)        
148500                                      ORAD-MID-TITPO     (ORAD-IX)        
148600             IF MID-RHRA-IDBIL (MID-IX) > SPACE                           
148700               MOVE JA             TO ORAD-MID-FLDIRLEV  (ORAD-IX)        
148800             ELSE                                                         
148900               MOVE SPACE          TO ORAD-MID-FLDIRLEV  (ORAD-IX)        
149000             END-IF                                                       
149100             MOVE MID-RHRA-KDKVBRYT (MID-IX)                              
149200                                   TO ORAD-MID-KDKVBRYT  (ORAD-IX)        
149300             MOVE SPACE            TO ORAD-MID-FLINVEST  (ORAD-IX)        
149400             MOVE ZERO             TO ORAD-MID-KDVRINFO  (ORAD-IX)        
149500             MOVE SPACE            TO ORAD-MID-IDKONTO   (ORAD-IX)        
149600             MOVE SPACE            TO ORAD-MID-IDKST     (ORAD-IX)        
149700             MOVE MID-RHRA-BERADREF (MID-IX)                              
149800                                   TO ORAD-MID-BERADREF  (ORAD-IX)        
149900             MOVE MID-RHRA-KDDSP (MID-IX)                                 
150000                                   TO ORAD-MID-KDDSP     (ORAD-IX)        
150100             MOVE MID-RHRA-FLSLATT (MID-IX)                               
150200                                   TO ORAD-MID-FLSLATT   (ORAD-IX)        
150300             MOVE MID-RHRA-IDBIL   (MID-IX)                               
150400                                   TO ORAD-MID-IDBIL     (ORAD-IX)        
150500                                                                          
150600             MOVE SPACE            TO                                     
150700                                  ORAD-MID-PRARTNTO-LOC  (ORAD-IX)        
150800             MOVE SPACE            TO                                     
150900                                 ORAD-MID-PRARTBTO-LOC   (ORAD-IX)        
151000             MOVE SPACE            TO                                     
151100                                      ORAD-MID-KDVALISO  (ORAD-IX)        
151200             MOVE SPACE            TO                                     
151300                                      ORAD-MID-KDVAT     (ORAD-IX)        
151400             MOVE 0                TO                                     
151500                                      ORAD-MID-RERAB     (ORAD-IX)        
151600             MOVE SPACE            TO                                     
151700                                      ORAD-MID-KDRAB     (ORAD-IX)        
151800             MOVE SPACE            TO                                     
151900                                  ORAD-MID-BEART-VIPS    (ORAD-IX)        
152000                                                                          
152100             MOVE SPACE       TO  ORAD-MID-IDKUNDRF-WIP(ORAD-IX)          
152200                                                                          
152300             IF GMT-FLLDCKND = JA                                         
152400                IF MID-RHRA-KDORDKL = 1                                   
152500                   MOVE MID-RHRA-BERADREF (MID-IX)                        
152600                              TO ORAD-MID-IDKUNDRF-WIP(ORAD-IX)           
152700                END-IF                                                    
152800             END-IF                                                       
152900             ADD +1 TO MID-IX                                             
153000             ADD +1 TO ORAD-IX                                            
153100          END-PERFORM                                                     
153200                                                                          
153300          IF ORAD-IX > 0                                                  
153400             IF MID-IX > W-KVRADER                                        
153500                MOVE 'J'           TO ORAD-MID-FLSLUT                     
153600             END-IF                                                       
153700                                                                          
153800             MOVE KOM-AREA         TO MSG-INDATA-MINUS-1-TRANSKOD         
153900             CALL W006KOM USING MSG-PCB                                   
154000                                DISP-PCB                                  
154100                                KOMA-PCB                                  
154200                                MSG-KOM-WMSGKOMI                          
154300                                MSG-IO-AREA                               
154400          END-IF                                                          
154500          MOVE ZERO  TO ORAD-IX                                           
154600          MOVE SPACE TO ORAD-MID-W4I25201                                 
154700                                                                          
154800       END-PERFORM                                                        
154900     END-IF                                                               
155000     .                                                                    
155100     EJECT                                                                
155200                                                                          
155300 F-SKAPA-ANNULRADTRANS SECTION.                                           
155400     SKIP2                                                                
155500     MOVE SPACE                  TO KOM-AREA                              
155600                                                                          
155700     COMPUTE MSG-KVLL = LENGTH OF ARAD-MID-W4I25401-CTX + 17              
155800                                                                          
155900     MOVE LOW-VALUE              TO MSG-KDZ1                              
156000     MOVE LOW-VALUE              TO MSG-KDZ2                              
156100     MOVE 'W4T254X '             TO MSG-KDTRANS-1                         
156200     MOVE '4254'                 TO MSG-IDTRANS-1                         
156300     MOVE '1'                    TO MSG-KDMFSFOR-1                        
156400                                                                          
156500     PERFORM FA-SKAPA-ANNULRADTRANS-RHO                                   
156600                                                                          
156700     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
156800     CALL W006KOM USING MSG-PCB                                           
156900                        DISP-PCB                                          
157000                        KOMA-PCB                                          
157100                        MSG-KOM-WMSGKOMI                                  
157200                        MSG-IO-AREA                                       
157300     MOVE SPACE TO ARAD-MID-W4I25401-CTX                                  
157400                                                                          
157500     .                                                                    
157600     EJECT                                                                
157700                                                                          
157800                                                                          
157900 FA-SKAPA-ANNULRADTRANS-RHO SECTION.                                      
158000                                                                          
158100     MOVE 'VDI '                 TO ARAD-MID-IDSYSTEM                     
158200     MOVE MID-IDDISTR            TO ARAD-MID-IDDISTR                      
158300     MOVE MID-IDKUNDNR           TO ARAD-MID-IDKUNDNR                     
158400     MOVE MID-IDORDNR            TO ARAD-MID-IDORDNR                      
158500     MOVE 'J'                    TO ARAD-MID-FLSLUT                       
158600     MOVE MID-RHO-IDARTNR        TO ARAD-MID-IDARTNR                      
158700     MOVE MID-RHO-KVBEART        TO ARAD-MID-KVBEART                      
158800     MOVE MID-RHO-IDDC           TO ARAD-MID-IDDC                         
158900     .                                                                    
159000     EJECT                                                                
159100                                                                          
159200 G-SKAPA-TILLAGGRADTRANS SECTION.                                         
159300     SKIP2                                                                
159400     IF MID-IDVTYP = '3'                                                  
159500       MOVE MID-RHKB-KVRADER       TO W-KVRADER                           
159600       MOVE ZERO                   TO TRAD-IX                             
159700       MOVE SPACE                  TO KOM-AREA                            
159800                                                                          
159900       COMPUTE MSG-KVLL = LENGTH OF TRAD-MID-W4I25501-CTX + 17            
160000                                                                          
160100       MOVE LOW-VALUE              TO MSG-KDZ1                            
160200       MOVE LOW-VALUE              TO MSG-KDZ2                            
160300       MOVE 'W4T255X '             TO MSG-KDTRANS-1                       
160400       MOVE '4255'                 TO MSG-IDTRANS-1                       
160500       MOVE '1'                    TO MSG-KDMFSFOR-1                      
160600                                                                          
160700       MOVE +1    TO MID-IX                                               
160800       PERFORM UNTIL MID-IX > W-KVRADER OR                                
160900                     MSG-KOM-IDMFSMED NOT = SPACE                         
161000                                                                          
161100          MOVE 'VDI '              TO TRAD-MID-IDSYSTEM                   
161200          MOVE MID-IDDISTR         TO TRAD-MID-IDDISTR                    
161300          MOVE MID-IDKUNDNR        TO TRAD-MID-IDKUNDNR                   
161400          MOVE MID-IDORDNR         TO TRAD-MID-IDORDNR                    
161500          MOVE MID-RHKB-KDORDKL     TO TRAD-MID-KDORDKL                   
161600          MOVE MID-RHKB-BEKUNDRF    TO TRAD-MID-BEKUNDRF-001              
161700          MOVE SPACE               TO TRAD-MID-IDMFSMED                   
161800          MOVE 'N'                 TO TRAD-MID-FLSLUT                     
161900                                                                          
162000          MOVE +1     TO TRAD-IX                                          
162100          PERFORM UNTIL MID-IX > W-KVRADER OR                             
162200                        TRAD-IX > TRAD-IX-MAX                             
162300             MOVE MID-RHKB-IDARTNR (MID-IX)                               
162400                                   TO TRAD-MID-IDARTNR   (TRAD-IX)        
162500             MOVE MID-RHKB-KVBEART (MID-IX)                               
162600                                   TO TRAD-MID-KVBEART   (TRAD-IX)        
162700             MOVE MID-RHKB-KDKVBRYT (MID-IX)                              
162800                                   TO TRAD-MID-KDKVBRYT  (TRAD-IX)        
162900             MOVE MID-RHKB-BERADREF (MID-IX)                              
163000                                   TO TRAD-MID-BERADREF  (TRAD-IX)        
163100             MOVE MID-RHKB-KDDSP (MID-IX)                                 
163200                                   TO TRAD-MID-KDDSP     (TRAD-IX)        
163300             MOVE MID-RHKB-FLSLATT (MID-IX)                               
163400                                   TO TRAD-MID-FLSLATT   (TRAD-IX)        
163500                                                                          
163600             MOVE SPACE            TO TRAD-MID-IDPRQUES  (TRAD-IX)        
163700             MOVE SPACE            TO                                     
163800                              TRAD-MID-PRARTNTO-LOCPREL  (TRAD-IX)        
163900                                                                          
164000             MOVE MID-RHKB-PRARTNTO-LOC (MID-IX)                          
164100                                          TO W-PRARTXXX-MED-PUNKT         
164200             MOVE W-PRARTXXX-MED-PUNKT TO                                 
164300                                   TRAD-MID-PRARTNTO-LOC (TRAD-IX)        
164400                                                                          
164500             MOVE MID-RHKB-PRARTBTO-LOC (MID-IX)                          
164600                                           TO W-PRARTXXX-MED-PUNKT        
164700             MOVE W-PRARTXXX-MED-PUNKT TO                                 
164800                                 TRAD-MID-PRARTBTO-LOC (TRAD-IX)          
164900                                                                          
165000             MOVE MID-RHKB-KDVALISO (MID-IX)                              
165100                                   TO TRAD-MID-KDVALISO  (TRAD-IX)        
165200             MOVE MID-RHKB-KDVAT (MID-IX)                                 
165300                                   TO TRAD-MID-KDVAT     (TRAD-IX)        
165400             MOVE MID-RHKB-RERAB (MID-IX)                                 
165500                                   TO TRAD-MID-RERAB     (TRAD-IX)        
165600             MOVE MID-RHKB-KDRAB (MID-IX)                                 
165700                                   TO TRAD-MID-KDRAB     (TRAD-IX)        
165800             MOVE MID-RHKB-BEART-VIPS (MID-IX)                            
165900                               TO TRAD-MID-BEART-VIPS    (TRAD-IX)        
166000                                                                          
166100             MOVE SPACE        TO TRAD-MID-IDKUNDRF-WIP(TRAD-IX)          
166200                                                                          
166300             IF GMT-FLLDCKND = JA                                         
166400                IF MID-RHKB-KDORDKL = 1                                   
166500                   MOVE MID-RHKB-BERADREF (MID-IX)                        
166600                              TO TRAD-MID-IDKUNDRF-WIP(TRAD-IX)           
166700                END-IF                                                    
166800             END-IF                                                       
166900             ADD +1 TO MID-IX                                             
167000             ADD +1 TO TRAD-IX                                            
167100          END-PERFORM                                                     
167200                                                                          
167300          IF TRAD-IX > 0                                                  
167400             IF MID-IX > W-KVRADER                                        
167500                MOVE 'J'           TO TRAD-MID-FLSLUT                     
167600             END-IF                                                       
167700                                                                          
167800             MOVE KOM-AREA         TO MSG-INDATA-MINUS-1-TRANSKOD         
167900             CALL W006KOM USING MSG-PCB                                   
168000                                DISP-PCB                                  
168100                                KOMA-PCB                                  
168200                                MSG-KOM-WMSGKOMI                          
168300                                MSG-IO-AREA                               
168400          END-IF                                                          
168500          MOVE ZERO  TO TRAD-IX                                           
168600          MOVE SPACE TO TRAD-MID-W4I25501-CTX                             
168700                                                                          
168800       END-PERFORM                                                        
168900     ELSE                                                                 
169000       MOVE MID-RHKA-KVRADER        TO W-KVRADER                          
169100       MOVE ZERO                   TO TRAD-IX                             
169200       MOVE SPACE                  TO KOM-AREA                            
169300                                                                          
169400       COMPUTE MSG-KVLL = LENGTH OF TRAD-MID-W4I25501-CTX + 17            
169500                                                                          
169600       MOVE LOW-VALUE              TO MSG-KDZ1                            
169700       MOVE LOW-VALUE              TO MSG-KDZ2                            
169800       MOVE 'W4T255X '             TO MSG-KDTRANS-1                       
169900       MOVE '4255'                 TO MSG-IDTRANS-1                       
170000       MOVE '1'                    TO MSG-KDMFSFOR-1                      
170100                                                                          
170200       MOVE +1    TO MID-IX                                               
170300       PERFORM UNTIL MID-IX > W-KVRADER OR                                
170400                     MSG-KOM-IDMFSMED NOT = SPACE                         
170500                                                                          
170600          MOVE 'VDI '              TO TRAD-MID-IDSYSTEM                   
170700          MOVE MID-IDDISTR         TO TRAD-MID-IDDISTR                    
170800          MOVE MID-IDKUNDNR        TO TRAD-MID-IDKUNDNR                   
170900          MOVE MID-IDORDNR         TO TRAD-MID-IDORDNR                    
171000          MOVE MID-RHKA-KDORDKL     TO TRAD-MID-KDORDKL                   
171100          MOVE MID-RHKA-BEKUNDRF    TO TRAD-MID-BEKUNDRF-001              
171200          MOVE SPACE               TO TRAD-MID-IDMFSMED                   
171300          MOVE 'N'                 TO TRAD-MID-FLSLUT                     
171400                                                                          
171500          MOVE +1     TO TRAD-IX                                          
171600          PERFORM UNTIL MID-IX > W-KVRADER OR                             
171700                        TRAD-IX > TRAD-IX-MAX                             
171800             MOVE MID-RHKA-IDARTNR (MID-IX)                               
171900                                   TO TRAD-MID-IDARTNR   (TRAD-IX)        
172000             MOVE MID-RHKA-KVBEART (MID-IX)                               
172100                                   TO TRAD-MID-KVBEART   (TRAD-IX)        
172200             MOVE MID-RHKA-KDKVBRYT (MID-IX)                              
172300                                   TO TRAD-MID-KDKVBRYT  (TRAD-IX)        
172400             MOVE MID-RHKA-BERADREF (MID-IX)                              
172500                                   TO TRAD-MID-BERADREF  (TRAD-IX)        
172600             MOVE MID-RHKA-KDDSP (MID-IX)                                 
172700                                   TO TRAD-MID-KDDSP     (TRAD-IX)        
172800             MOVE MID-RHKA-FLSLATT (MID-IX)                               
172900                                   TO TRAD-MID-FLSLATT   (TRAD-IX)        
173000                                                                          
173100             MOVE SPACE            TO TRAD-MID-IDPRQUES  (TRAD-IX)        
173200             MOVE SPACE            TO                                     
173300                              TRAD-MID-PRARTNTO-LOCPREL  (TRAD-IX)        
173400             MOVE SPACE            TO                                     
173500                                  TRAD-MID-PRARTNTO-LOC  (TRAD-IX)        
173600             MOVE SPACE            TO                                     
173700                                 TRAD-MID-PRARTBTO-LOC   (TRAD-IX)        
173800             MOVE SPACE            TO                                     
173900                                      TRAD-MID-KDVALISO  (TRAD-IX)        
174000             MOVE SPACE            TO                                     
174100                                      TRAD-MID-KDVAT     (TRAD-IX)        
174200             MOVE 0                TO                                     
174300                                      TRAD-MID-RERAB     (TRAD-IX)        
174400             MOVE SPACE            TO                                     
174500                                      TRAD-MID-KDRAB     (TRAD-IX)        
174600             MOVE SPACE            TO                                     
174700                                  TRAD-MID-BEART-VIPS    (TRAD-IX)        
174800             MOVE SPACE            TO                                     
174900                                  TRAD-MID-IDKUNDRF-WIP  (TRAD-IX)        
175000                                                                          
175100             IF GMT-FLLDCKND = JA                                         
175200                IF MID-RHKB-KDORDKL = 1                                   
175300                   MOVE MID-RHKA-BERADREF (MID-IX)                        
175400                              TO TRAD-MID-IDKUNDRF-WIP(TRAD-IX)           
175500                END-IF                                                    
175600             END-IF                                                       
175700             ADD +1 TO MID-IX                                             
175800             ADD +1 TO TRAD-IX                                            
175900          END-PERFORM                                                     
176000                                                                          
176100          IF TRAD-IX > 0                                                  
176200             IF MID-IX > W-KVRADER                                        
176300                MOVE 'J'           TO TRAD-MID-FLSLUT                     
176400             END-IF                                                       
176500                                                                          
176600             MOVE KOM-AREA         TO MSG-INDATA-MINUS-1-TRANSKOD         
176700             CALL W006KOM USING MSG-PCB                                   
176800                                DISP-PCB                                  
176900                                KOMA-PCB                                  
177000                                MSG-KOM-WMSGKOMI                          
177100                                MSG-IO-AREA                               
177200          END-IF                                                          
177300          MOVE ZERO  TO TRAD-IX                                           
177400          MOVE SPACE TO TRAD-MID-W4I25501-CTX                             
177500                                                                          
177600       END-PERFORM                                                        
177700     END-IF                                                               
177800     .                                                                    
177900     EJECT                                                                
178000                                                                          
178100 H-SKAPA-RO-TPO-ANNULLRADTRANS SECTION.                                   
178200                                                                          
178300     MOVE SPACE                  TO KOM-AREA                              
178400                                                                          
178500     COMPUTE MSG-KVLL = LENGTH OF RORAD-MID-W4I25601-CTX + 17             
178600                                                                          
178700     MOVE LOW-VALUE              TO MSG-KDZ1                              
178800     MOVE LOW-VALUE              TO MSG-KDZ2                              
178900     MOVE 'W4T256X '             TO MSG-KDTRANS-1                         
179000     MOVE '4256'                 TO MSG-IDTRANS-1                         
179100     MOVE '1'                    TO MSG-KDMFSFOR-1                        
179200                                                                          
179300     PERFORM HA-SKAPA-ANNULLRADTRANS-RHM                                  
179400                                                                          
179500     MOVE KOM-AREA            TO MSG-INDATA-MINUS-1-TRANSKOD              
179600     CALL W006KOM USING MSG-PCB                                           
179700                        DISP-PCB                                          
179800                        KOMA-PCB                                          
179900                        MSG-KOM-WMSGKOMI                                  
180000                        MSG-IO-AREA                                       
180100                                                                          
180200     .                                                                    
180300     EJECT                                                                
180400                                                                          
180500                                                                          
180600 HA-SKAPA-ANNULLRADTRANS-RHM SECTION.                                     
180700                                                                          
180800     MOVE MID-IDDISTR         TO RORAD-MID-IDDISTR                        
180900     MOVE MID-IDKUNDNR        TO RORAD-MID-IDKUNDNR                       
181000     MOVE MID-IDORDNR         TO RORAD-MID-IDORDNR7                       
181100     MOVE MID-RHM-IDARTNR     TO RORAD-MID-IDARTNR                        
181200     MOVE MID-RHM-KVBEART     TO RORAD-MID-KVBEART                        
181300     MOVE MID-RHM-TITPO       TO RORAD-MID-TITPO                          
181400     MOVE MID-RHM-IDDC        TO RORAD-MID-IDDC                           
181500     .                                                                    
181600     EJECT                                                                
181700                                                                          
181800                                                                          
181894 S01-KOLLA-KONSOLIDERING   SECTION.                                       
181895                                                                          
181896                                                                          
181897     MOVE 1            TO OHKK-KDCALL                                     
181898     MOVE 'VDI'        TO OHKK-IDSYSTEM                                   
181899     MOVE MID-IDDISTR  TO OHKK-IDDISTR                                    
181900                          TEST-IDDISTR                                    
181901     MOVE MID-IDKUNDNR TO OHKK-IDKUNDNR                                   
181902     MOVE MID-IDORDNR  TO OHKK-IDORDNR-IN                                 
181903                                                                          
181904     MOVE ALL '+' TO OHKK-ADBETRAD-1                                      
181905                   OHKK-ADBETRAD-2                                        
181906                   OHKK-ADGMT                                             
181907                   OHKK-BEBETRAD-1                                        
181908                   OHKK-BEBETRAD-2                                        
181909                   OHKK-BEGMRK                                            
181910                   OHKK-BEGMT                                             
181911                   OHKK-KDFAKTYP                                          
181912     MOVE SPACE TO OHKK-IDANALYS                                          
181913                   OHKK-IDBIPREF                                          
181914                   OHKK-IDDC-TVS                                          
181915                   OHKK-IDDEPOT                                           
181916                   OHKK-IDRFTAB                                           
181917                   OHKK-IDROUTE                                           
181918                   OHKK-IDSKYLT                                           
181919                   OHKK-IDZON                                             
181920                   OHKK-IDKST                                             
181921                                                                          
181922     MOVE ZERO TO OHKK-IDDEPT                                             
181923                  OHKK-IDFTG                                              
181924                  OHKK-IDKONTO                                            
181925                  OHKK-KDORDING                                           
181926                  OHKK-KDVRINFO                                           
181927                  OHKK-KDTULLVE                                           
181928                  OHKK-KVDAGAR-DOW                                        
181929                  OHKK-RESLATT                                            
181930                  OHKK-TITPO                                              
181931                                                                          
181932     MOVE NEJ TO OHKK-FLAUTFAK                                            
181933                 OHKK-FLAUTPAC                                            
181934                 OHKK-FLFORBI                                             
181935                 OHKK-FLORDSPE                                            
181936                 OHKK-FLOVRLEV                                            
181937                 OHKK-FLVORKO                                             
181938                                                                          
181939     MOVE JA TO OHKK-FLLSBOK                                              
181940                                                                          
181941     IF DIST20-EMBALLAGE                                                  
181942        MOVE JA   TO OHKK-FLEMBORD                                        
181943     ELSE                                                                 
181944        MOVE NEJ  TO OHKK-FLEMBORD                                        
181945     END-IF                                                               
181946                                                                          
181947     MOVE GMT-FLPRELRO TO OHKK-FLPRELRO                                   
181948     MOVE GMT-FLPRERS  TO OHKK-FLPRERS                                    
181949     MOVE NEJ          TO OHKK-FLSOFT                                     
181950                                                                          
181951     CALL W460DIS1 USING DIS1-W460DIS1                                    
181952                                                                          
181953     IF MID-IDPTYP = 'RHI'                                                
181954        MOVE MID-RHI-KDORDKL                   TO OHKK-KDORDKL            
181955        MOVE MID-RHI-KDFRAKT                   TO OHKK-KDFRAKT            
181956        MOVE MID-RHI-FLRESTN                   TO OHKK-FLRESTN            
181957        MOVE MID-RHI-IDKAMPRF                  TO OHKK-IDKAMPRF           
181958        MOVE MID-RHI-KDTPOTYP                  TO OHKK-KDTPOTYP           
181959        IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                              
181960           OHKK-KDORDKL = 3                                               
181961           MOVE SPACE                          TO OHUV-MID-KDFRAKT        
181962        ELSE                                                              
181963           IF MID-RHI-KDFRAKT = ZERO                                      
181964              MOVE SPACE                       TO OHUV-MID-KDFRAKT        
181965           ELSE                                                           
181966              MOVE MID-RHI-KDFRAKT             TO OHUV-MID-KDFRAKT        
181967           END-IF                                                         
181968        END-IF                                                            
181969     ELSE                                                                 
181970        IF MID-IDPTYP = 'RHN'                                             
181971           MOVE MID-RHN-KDORDKL                TO OHKK-KDORDKL            
181973           MOVE MID-RHN-KDFRAKT                TO OHKK-KDFRAKT            
181974           MOVE MID-RHN-FLRESTN                TO OHKK-FLRESTN            
181975           MOVE MID-RHN-IDKAMPRF               TO OHKK-IDKAMPRF           
181976           MOVE MID-RHN-KDTPOTYP               TO OHKK-KDTPOTYP           
181977           IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                           
181978              OHKK-KDORDKL = 3                                            
181979              MOVE SPACE                       TO OHUV-MID-KDFRAKT        
181980           ELSE                                                           
181981              IF MID-RHN-KDFRAKT = ZERO                                   
181982                 MOVE SPACE                    TO OHUV-MID-KDFRAKT        
181983              ELSE                                                        
181984                 MOVE MID-RHN-KDFRAKT          TO OHUV-MID-KDFRAKT        
181985              END-IF                                                      
181986           END-IF                                                         
181987           MOVE MID-RHN-ADGMT-GATA             TO OHKK-ADGMT-GATA         
181988           MOVE MID-RHN-ADGMT-PADR             TO OHKK-ADGMT-PADR         
181989           MOVE MID-RHN-BEGMT-RAD1             TO OHKK-BEGMT-RAD1         
181990           MOVE MID-RHN-BEGMT-RAD2             TO OHKK-BEGMT-RAD2         
181991        ELSE                                                              
181992           IF MID-IDPTYP = 'RHP'                                          
181993              IF MID-IDVTYP = '3'                                         
181994                 MOVE MID-RHPB-KDORDKL         TO OHKK-KDORDKL            
181995                 MOVE MID-RHPB-KDFRAKT         TO OHKK-KDFRAKT            
181996                 MOVE MID-RHPB-FLRESTN         TO OHKK-FLRESTN            
181997                 MOVE MID-RHPB-IDKAMPRF        TO OHKK-IDKAMPRF           
181998                 MOVE MID-RHPB-KDTPOTYP        TO OHKK-KDTPOTYP           
181999                 MOVE W-FLSOFT                 TO OHKK-FLSOFT             
182000                 IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                     
182001                    OHKK-KDORDKL = 3                                      
182002                    MOVE SPACE                 TO OHUV-MID-KDFRAKT        
182003                 ELSE                                                     
182004                    IF MID-RHPB-KDFRAKT = ZERO                            
182005                       MOVE SPACE              TO OHUV-MID-KDFRAKT        
182006                    ELSE                                                  
182007                       MOVE MID-RHPB-KDFRAKT   TO OHUV-MID-KDFRAKT        
182008                    END-IF                                                
182009                 END-IF                                                   
182010              ELSE                                                        
182011                 MOVE MID-RHPA-KDORDKL         TO OHKK-KDORDKL            
182012                 MOVE MID-RHPA-KDFRAKT         TO OHKK-KDFRAKT            
182013                 MOVE MID-RHPA-FLRESTN         TO OHKK-FLRESTN            
182014                 MOVE MID-RHPA-IDKAMPRF        TO OHKK-IDKAMPRF           
182015                 MOVE MID-RHPA-KDTPOTYP        TO OHKK-KDTPOTYP           
182016                 MOVE W-FLSOFT                 TO OHKK-FLSOFT             
182017                 IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                     
182018                    OHKK-KDORDKL = 3                                      
182019                    MOVE SPACE                 TO OHUV-MID-KDFRAKT        
182020                 ELSE                                                     
182021                    IF MID-RHPA-KDFRAKT = ZERO                            
182022                       MOVE SPACE              TO OHUV-MID-KDFRAKT        
182023                    ELSE                                                  
182024                       MOVE MID-RHPA-KDFRAKT   TO OHUV-MID-KDFRAKT        
182025                    END-IF                                                
182026                 END-IF                                                   
182027              END-IF                                                      
182028           ELSE                                                           
182029              IF MID-IDPTYP = 'RHR'                                       
182030                 IF MID-IDVTYP = '3'                                      
182031                    MOVE MID-RHRB-KDORDKL      TO OHKK-KDORDKL            
182032                    MOVE MID-RHRB-KDFRAKT      TO OHKK-KDFRAKT            
182033                    MOVE MID-RHRB-FLRESTN      TO OHKK-FLRESTN            
182034                    MOVE MID-RHRB-IDKAMPRF     TO OHKK-IDKAMPRF           
182035                    MOVE MID-RHRB-KDTPOTYP     TO OHKK-KDTPOTYP           
182036                    MOVE W-FLSOFT              TO OHKK-FLSOFT             
182037                    IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                  
182038                       OHKK-KDORDKL = 3                                   
182039                       MOVE SPACE              TO OHUV-MID-KDFRAKT        
182040                    ELSE                                                  
182041                       IF MID-RHRB-KDFRAKT = ZERO                         
182042                          MOVE SPACE           TO OHUV-MID-KDFRAKT        
182043                       ELSE                                               
182044                         MOVE MID-RHRB-KDFRAKT TO OHUV-MID-KDFRAKT        
182045                       END-IF                                             
182046                    END-IF                                                
182047                    MOVE MID-RHRB-ADGMT-GATA   TO OHKK-ADGMT-GATA         
182048                    MOVE MID-RHRB-ADGMT-PADR   TO OHKK-ADGMT-PADR         
182049                    MOVE MID-RHRB-BEGMT-RAD1   TO OHKK-BEGMT-RAD1         
182050                    MOVE MID-RHRB-BEGMT-RAD2   TO OHKK-BEGMT-RAD2         
182051                 ELSE                                                     
182052                    MOVE MID-RHRA-KDORDKL      TO OHKK-KDORDKL            
182053                    MOVE MID-RHRA-KDFRAKT      TO OHKK-KDFRAKT            
182054                    MOVE MID-RHRA-FLRESTN      TO OHKK-FLRESTN            
182055                    MOVE MID-RHRA-IDKAMPRF     TO OHKK-IDKAMPRF           
182056                    MOVE MID-RHRA-KDTPOTYP     TO OHKK-KDTPOTYP           
182057                    MOVE W-FLSOFT              TO OHKK-FLSOFT             
182058                    IF DIS1-IDLANDX2 = ISO-FRANKRIKE AND                  
182059                       OHKK-KDORDKL = 3                                   
182060                       MOVE SPACE              TO OHUV-MID-KDFRAKT        
182061                    ELSE                                                  
182062                       IF MID-RHRA-KDFRAKT = ZERO                         
182063                          MOVE SPACE           TO OHUV-MID-KDFRAKT        
182064                       ELSE                                               
182065                         MOVE MID-RHRA-KDFRAKT TO OHUV-MID-KDFRAKT        
182066                       END-IF                                             
182067                    END-IF                                                
182068                    MOVE MID-RHRA-ADGMT-GATA   TO OHKK-ADGMT-GATA         
182069                    MOVE MID-RHRA-ADGMT-PADR   TO OHKK-ADGMT-PADR         
182070                    MOVE MID-RHRA-BEGMT-RAD1   TO OHKK-BEGMT-RAD1         
182071                    MOVE MID-RHRA-BEGMT-RAD2   TO OHKK-BEGMT-RAD2         
182072                 END-IF                                                   
182073              END-IF                                                      
182074           END-IF                                                         
182075        END-IF                                                            
182076     END-IF                                                               
182077                                                                          
182083                                                                          
182090     IF  OHKK-BEGMT        = ALL '+'      AND                             
182091         OHKK-ADGMT        = ALL '+'      AND                             
182093         OHKK-BEBETRAD-1   = ALL '+'      AND                             
182094         OHKK-BEBETRAD-2   = ALL '+'      AND                             
182095         OHKK-ADBETRAD-1   = ALL '+'      AND                             
182096         OHKK-ADBETRAD-2   = ALL '+'                                      
182097         CALL W411OHKK USING OHKK-W411OHKK OHKK-WDQ2-PCB                  
182098                                       OHKK-WDQ2-UPD-PCB                  
182099                                       OHKK-WDQ2C-PCB                     
182100                                       OHKK-GMTA-PCB                      
182101                                       OHKK-GMTB-PCB OHKK-GMTC-PCB        
182102                                       OHKK-BETC-PCB OHKK-WDB2-PCB        
182103                                       OHKK-WDB3-PCB OHKK-WDB5-PCB        
182104                                       OHKK-WDP7-PCB OHKK-XXKB-PCB        
182106     ELSE                                                                 
182107        MOVE ZERO      TO  OHKK-IDORDER                                   
182108     END-IF                                                               
182109                                                                          
182110     IF OHKK-IDORDER > ZERO                                               
182111        MOVE JA  TO KONSOLIDERING-SW                                      
182112     ELSE                                                                 
182113        MOVE NEJ TO KONSOLIDERING-SW                                      
182114     END-IF                                                               
182115     .                                                                    
182116                                                                          
182117 S02-KOLLA-SOFTWARE  SECTION.                                             
182118                                                                          
182119     MOVE NEJ                TO W-FLSOFT                                  
182120     MOVE +1    TO W-IX                                                   
182121     PERFORM UNTIL W-IX > MID-RHI-KVRADER                                 
182122       IF MID-IDVTYP = '3'                                                
182123         IF MID-RHPB-IDBIL(W-IX) > SPACE                                  
182124           MOVE JA              TO W-FLSOFT                               
182125           MOVE MID-RHI-KVRADER TO W-IX                                   
182126         END-IF                                                           
182127       ELSE                                                               
182128         IF MID-RHPA-IDBIL(W-IX) > SPACE                                  
182129           MOVE JA              TO W-FLSOFT                               
182130           MOVE MID-RHI-KVRADER TO W-IX                                   
182131         END-IF                                                           
182132       END-IF                                                             
182133       ADD +1 TO W-IX                                                     
182140     END-PERFORM                                                          
182200     .                                                                    
182300 S03-KOLLA-SOFTWARE  SECTION.                                             
182400                                                                          
182500     MOVE NEJ                TO W-FLSOFT                                  
182600     MOVE +1    TO W-IX                                                   
182610     PERFORM UNTIL W-IX > MID-RHI-KVRADER                                 
182611       IF MID-IDVTYP = '3'                                                
182620         IF MID-RHRB-IDBIL(W-IX) > SPACE                                  
182630           MOVE JA              TO W-FLSOFT                               
182640           MOVE MID-RHI-KVRADER TO W-IX                                   
182650         END-IF                                                           
182651       ELSE                                                               
182652         IF MID-RHRA-IDBIL(W-IX) > SPACE                                  
182653           MOVE JA              TO W-FLSOFT                               
182654           MOVE MID-RHI-KVRADER TO W-IX                                   
182655         END-IF                                                           
182656       END-IF                                                             
182660       ADD +1 TO W-IX                                                     
182670     END-PERFORM                                                          
182680     .                                                                    
182687                                                                          
      * FOR SW ORDER WITH ORDER CLASS 2,IF DC11 IS ABSENT IN IDDC CLEAR         
      * TABLE,SEND ERROR MESSAGE '076' TO VV                                    
182300 S04-SW-ORDER2-CHK-DC11 SECTION.                                          
182400                                                                          
           IF (MID-IDPTYP = 'RHP' OR 'RHR') AND                                 
045800         W-FLSOFT = JA                AND                                 
              (MID-RHPA-KDORDKL >= 2 )      AND                                 
               DC11-PRESENT-SW = NEJ                                            
              MOVE ERR-POST-FEL        TO MSG-KOM-IDMFSMED                      
           END-IF                                                               
182680     .                                                                    
182688                                                                          
182689* --- IMS SEKTIONER ---                                                   
182690     SKIP3                                                                
182691 IMS-GET-MSG SECTION.                                                     
182692                                                                          
182693     MOVE LOW-VALUE  TO MID-IO-AREA                                       
182694     MOVE '  QC' TO GODK-STATUSKODER                                      
182695     CALL CBLTDLI USING GU MSG-PCB MID-IO-AREA                            
182696     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
182700     PERFORM IMS-STATUSKONTROLL                                           
182800     .                                                                    
182900     SKIP3                                                                
183000 IMS-INSERT-MSG SECTION.                                                  
183100     MOVE +54                    TO MSG-KVLL                              
183200     MOVE LOW-VALUE              TO MSG-KDZ1                              
183300     MOVE LOW-VALUE              TO MSG-KDZ2                              
183400     MOVE SPACE TO GODK-STATUSKODER                                       
183500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA                          
183600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
183700     PERFORM IMS-STATUSKONTROLL                                           
183800     .                                                                    
183900                                                                          
184000 IMS-GET-WDB201-UNIK SECTION.                                             
184100                                                                          
184200     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
184300          DELIMITED BY SIZE INTO SSA1                                     
184400     MOVE '  GE'               TO GODK-STATUSKODER                        
184500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
184600     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
184700     PERFORM IMS-STATUSKONTROLL                                           
184800     .                                                                    
184900     SKIP2                                                                
185000 IMS-GU-WDB201 SECTION.                                                   
185100                                                                          
185200     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
185300                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
185400            DELIMITED BY SIZE INTO SSA1                                   
185500     MOVE '  GE'               TO GODK-STATUSKODER                        
185600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
185700     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
185800     PERFORM IMS-STATUSKONTROLL                                           
185900     .                                                                    
186000     SKIP2                                                                
186100 IMS-STATUSKONTROLL SECTION.                                              
186200                                                                          
186300     SET STATUS-IX TO 1                                                   
186400     SEARCH GODK-STATUS                                                   
186500       AT END CALL FELLOG                                                 
186600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
186700     END-SEARCH                                                           
186800     .                                                                    
186900     EJECT                                                                
187000*    -COPY WY2000P4                                                       
187100     EJECT                                                                
187200*    -COPY WY2000P9                                                       
