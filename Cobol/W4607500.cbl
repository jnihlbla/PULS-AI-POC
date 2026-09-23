000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W4607500.                                        
000300 AUTHOR.                 KERSTIN JOHANSSON  GUIDE DATAKONSULT AB          
000400 DATE-WRITTEN.           OKT 1990.                                        
000500                                                                          
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*            BMP KONVERTERING AV VIPS ORDERPOSTER (NOAC) TILL             
001000*            KOMMUNIKATIONS DB (TRANSAKTION 4251 OCH 4252).               
001100*            FÖR VARJE POST MED POSTTYP RHA SKAPAS EN ORDERHUVUD-         
001200*            TRANSAKTION.                                                 
001300*            ORDERRADPOSTERNA MED POSTTYP RHB GRUPPERAS MED 14            
001400*            RADER I VARJE ORDERRADTRANSAKTION.                           
001500*            OM FLER RADER FINNS PÅ ORDERN SÄTTS FLSLUT TILL N            
001600*            ANNARS SÄTTS FLSLUT TILL J PÅ ORDERRADTRANSAKTIONEN.         
001700*            ORDERTRANSAKTIONERNA SKRIVS PÅ KOMMUNIKATIONS DB.            
001800*            CHECKPOINT TAGES FÖR VARJE NY ORDER.                         
001900*                                                                         
002000*        PROGRAMMET ÄNDRAT NOV 2007 SÅ KONTROLL SKER OM                   
002100*        TVINGANDE TILLÄGG SKALL GÖRAS.                                   
002200*        KONTROLL SKER MED HJÄLP AV W411OHKK OCH W411TVAG                 
002300*                                                                         
002400*    DATABASER: UPPDATERAR   KOMMUNIKATIONS DB                            
002500*                            WLKOMA (WDP8)                                
002600*               UPPDATERAR   HÄNDELSE REGISTER (CHKPOINT)                 
002700*                            WLXXLF-(WDR4)                                
002800*               LÄSER        LDC KUNDREGISTER WDB2                        
002900     EJECT                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 INPUT-OUTPUT SECTION.                                                    
003300 FILE-CONTROL.                                                            
003400                                                                          
003500*                            INFIL:                                       
003600*                                           GODKÄNDA POSTER               
003700     SELECT  W46006                   ASSIGN TO    W46075D1.              
003800     SKIP3                                                                
003900 DATA DIVISION.                                                           
004000                                                                          
004100 FILE SECTION.                                                            
004200                                                                          
004300 FD  W46006                                                               
004400     LABEL RECORD STANDARD                                                
004500     RECORDING      V                                                     
004600     BLOCK CONTAINS 0.                                                    
004700                                                                          
004800*01  -COPY W460001 -L.                                                    
004900                                                                          
005000*01  -COPY W460002 -L.                                                    
005100                                                                          
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400*    -COPY WY2000W1                                                       
005500     SKIP3                                                                
005600 77  IDPGM                       PIC X(8)    VALUE 'W4607500'.            
005700 77  WS-TIME-WAIT                PIC S9(9)   COMP VALUE +500.             
005800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005900 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
006000 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
006100 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
006200 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
006300 77  DRAD-IX                     PIC S9(4)   COMP SYNC VALUE ZERO.        
006400 77  DRAD-IX-MAX                 PIC S9(4)   COMP SYNC VALUE +5.          
006500 77  RFS-IX                      PIC S9(4)   COMP SYNC VALUE ZERO.        
006600 77  MAX-RFS-IX                  PIC S9(4)   COMP SYNC VALUE +4.          
006700 77  W-ANT-ORDER-IN              PIC S9(7)   COMP-3.                      
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
007000 77  GMT-FINNS                   PIC X       VALUE SPACE.                 
007100 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
007200 77  W46006-EOF                  PIC X       VALUE 'N'.                   
007300 77  SPAR-SORT-IDDISTR           PIC 9(4).                                
007400 77  SPAR-SORT-TIFILDAT          PIC 9(6).                                
007500 77  SPAR-SORT-TIHHMMSS          PIC 9(6).                                
007600 77  SPAR-IDDISTR                PIC 9(4).                                
007700 77  SPAR-IDKUNDNR               PIC 9(6).                                
007800 77  SPAR-IDORDNR                PIC 9(7).                                
007900 77  SPAR-OHUV-BEVOLREF          PIC X(10)   VALUE SPACE.                 
008000 77  SPAR-IDSYSTEM               PIC X(4)    VALUE SPACE.                 
008100 77  SPAR-KDORDURS               PIC X       VALUE SPACE.                 
008200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
008300 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008400 77  W-IDORDNR                   PIC 9(5)    VALUE ZERO.                  
008500 77  WS-TILOKDAT                 PIC 9(6)    VALUE ZERO.                  
008600                                                                          
008700 01  KONSOLIDERING-SW            PIC X       VALUE 'N'.                   
008800     88 KONSOLIDERING                        VALUE 'J'.                   
008900                                                                          
009000 01  W-TIKLOCK.                                                           
009100   03 W-TIHHMMSS                 PIC 9(6).                                
009200   03 W-TITH                     PIC 9(2).                                
009300                                                                          
009400 01  W-PRARTXXX-MED-PUNKT        PIC 9(7).9(2).                           
009500                                                                          
009600 01  KLOCKAN.                                                             
009700   03  TIKLOCK                   PIC 9(8)    VALUE ZERO.                  
009800   03  FILLER                    REDEFINES TIKLOCK.                       
009900    05 TIHH                      PIC 9(2).                                
010000    05 TIMMS                     PIC 9(3).                                
010100    05 TISSS                     PIC 9(2).                                
010200 01  SPAR-BELAGINS.                                                       
010300   03  FIX-BELAGINS              PIC X(31)    VALUE SPACE.                
010400   03  FIX-TIHH                  PIC X(2)     VALUE SPACE.                
010500   03  FIX-TIMMS                 PIC X(3)     VALUE SPACE.                
010600 01  W-TITPO-TAB.                                                         
010700   03  W-TITPO-POS               PIC X(1) OCCURS 6 TIMES                  
010800                                 INDEXED W-TITPO-IX.                      
010900 01  KONTROLL-SIFFRA.                                                     
011000   03  REK-IDARTNR               PIC 9(9)    VALUE 0.                     
011100   03  REK-LNGD                  PIC 9(1)    VALUE 9.                     
011200   03  REK-REKSIFFR              PIC 9(1)    VALUE 0.                     
011300     EJECT                                                                
011400                                                                          
011500*    ---- AREA FÖR INFIL W46006                                           
011600 01  FILLER                      PIC X(8)    VALUE 'IN-AREA'.             
011700 01  IN-AREA                     PIC X(400).                              
011800*01  FILLER -COPY W460001 -PRE IN- -RED IN-AREA.                          
011900     EJECT                                                                
012000*01  FILLER -COPY W460002 -PRE IN- -RED IN-AREA.                          
012100     EJECT                                                                
012200                                                                          
012300 01  TEST-IDDISTR        PIC 9(5) COMP-3.                                 
012400*01  FILLER -COPY WWDIST20  -RED TEST-IDDISTR.                            
012500*01  FILLER -COPY WWDIST07  -RED TEST-IDDISTR.                            
012510*01  FILLER -COPY WWDIST14  -RED TEST-IDDISTR.                            
012600                                                                          
012700*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
012800 01  DYNAMISKA-SUBPROGRAM.                                                
012900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
013000   03  W009WAIT                  PIC X(8)    VALUE 'W009WAIT'.            
013100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
013200   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
013300   03  W009KSIF                  PIC X(8)    VALUE 'W009KSIF'.            
013400   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
013500   03  WORKDAY                   PIC X(8)    VALUE 'WORKDAY '.            
013600   03  W411OHKK                  PIC X(8)    VALUE 'W411OHKK'.            
013700*    ----------------------------UNDERSÖK OM NOAC-DISTRIKT                
013800   03  W460DIS1                  PIC X(8)    VALUE 'W460DIS1'.            
013900*    ----------------------------UPPDATERAR KOMMUNIKATIONS DB             
014000   03  W006KOM                   PIC X(8)    VALUE 'W006KOM '.            
014100   03  W005INIT                PIC X(8)       VALUE 'W005INIT'.           
014200     EJECT                                                                
014300                                                                          
014400     EJECT                                                                
014500*01  -COPY W0005        -PRE POSTSUM-                                     
014600     EJECT                                                                
014700 01  FILLER                  PIC X(16)   VALUE 'WORKAREA   '.             
014800*   -COPY WORKAREA                                                        
014900                                                                          
015000 01 FILLER                       PIC X(8)    VALUE 'W411OHKK'.            
015100*   -COPY W411OHKK                                                        
015200     EJECT                                                                
015300*01  -COPY W460DIS1                                                       
015400*01  -COPY W460LISO                                                       
015500     EJECT                                                                
015600*    --- PARAMETRAR TILL GENERELLA SUBPROGRAM                             
015700 01 FILLER                       PIC X(8) VALUE 'W005INIT'.               
015800*   -COPY WMSGINIT                                                        
015900                                                                          
016000*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
016100*                                                                         
016200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016300     SKIP3                                                                
016400 01  NYCKLAR-TILL-DLI.                                                    
016500    03 W-IDGMT-X.                                                         
016600       05 W-WDB2-IDDISTR         PIC S9(5)    VALUE ZERO COMP-3.          
016700       05 W-WDB2-IDKUNDNR        PIC S9(7)    VALUE ZERO COMP-3.          
016800     EJECT                                                                
016900*    ---- STATUSKOD FRÅN IMS                                              
017000                                                                          
017100 01  STATUS-WS                   PIC XX.                                  
017200     88  SEGMENT-FINNS                       VALUE '  '.                  
017300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017500     88  IMS-EJ-OK                           VALUE 'XD'.                  
017600     SKIP3                                                                
017700 01  GODK-STATUSKODER.                                                    
017800   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
017900     SKIP3                                                                
018000 01  SSA1                        PIC X(64).                               
018100 01  SSA2                        PIC X(64).                               
018200     EJECT                                                                
018300*    IMS FUNKTIONSKODER                                                   
018400*01  -COPY  W0003                                                         
018500     EJECT                                                                
018600 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA'.          
018700     SKIP3                                                                
018800                                                                          
018900 01  FILLER                      PIC X(16)  VALUE 'AREA FOR WDB2'.        
019000 01  DLI-IO-AREA-WDB2.                                                    
019100     03  DLI-IO-WDB201.                                                   
019200*        05  -COPY WDB201                                                 
019300     EJECT                                                                
019400*                                                                         
019500*01  -COPY WDGX01                                                         
019600 01  FILLER                  PIC X(16)   VALUE 'XXLF-IO-AREA'.            
019700 01  XXLF-IO-AREA.                                                        
019800*03  FILLER  -COPY WDGX4484                                               
019900     EJECT                                                                
020000*                                                                         
020100 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
020200*01  -COPY WMSGKOM                                                        
020300     EJECT                                                                
020400                                                                          
020500 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
020600     SKIP3                                                                
020700*01  -COPY WMSGAREA                                                       
020800     EJECT                                                                
020900*                                                                         
021000*    --- AREOR FÖR W006KOM SUBMODUL                                       
021100*                                                                         
021200 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
021300 01  KOM-IO-AREA.                                                         
021400   03  KOM-AREA                     PIC X(2400) VALUE SPACE.              
021500*03  FILLER  -COPY W4I25101 -PRE DHUV-  -RED KOM-AREA.                    
021600     EJECT                                                                
021700*03  FILLER  -COPY W4I25201 -PRE DRAD-  -RED KOM-AREA.                    
021800     EJECT                                                                
021900                                                                          
022000 LINKAGE SECTION.                                                         
022100*01  -COPY W0009         -PRE MSG-                                        
022200                                                                          
022300*01  -COPY W0009         -PRE DISP-                                       
022400     EJECT                                                                
022500*01  -COPY W0008         -PRE KOMA-                                       
022600     05  FILLER              PIC X.                                       
022700*01  -COPY W0008         -PRE XXLF-                                       
022800     05  FILLER              PIC X.                                       
022900*01  -COPY W0008         -PRE WDB2-                                       
023000     05  FILLER              PIC X.                                       
023100                                                                          
023200 01  OHKK-WDQ2-PCB               PIC X.                                   
023300 01  OHKK-WDQ2-UPD-PCB           PIC X.                                   
023400 01  OHKK-WDQ2C-PCB              PIC X.                                   
023500 01  OHKK-GMTA-PCB               PIC X.                                   
023600 01  OHKK-GMTB-PCB               PIC X.                                   
023700 01  OHKK-GMTC-PCB               PIC X.                                   
023800 01  OHKK-BETC-PCB               PIC X.                                   
023900 01  OHKK-WDB2-PCB               PIC X.                                   
024000 01  OHKK-WDB3-PCB               PIC X.                                   
024100 01  OHKK-WDB5-PCB               PIC X.                                   
024200 01  OHKK-WDP7-PCB               PIC X.                                   
024300 01  OHKK-XXKB-PCB               PIC X.                                   
024400     EJECT                                                                
024500                                                                          
024600 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB KOMA-PCB XXLF-PCB             
024700                     WDB2-PCB                                             
024800                     OHKK-WDQ2-PCB OHKK-WDQ2-UPD-PCB                      
024900                     OHKK-WDQ2C-PCB                                       
025000                     OHKK-GMTA-PCB OHKK-GMTB-PCB                          
025100                     OHKK-GMTC-PCB OHKK-BETC-PCB OHKK-WDB2-PCB            
025200                     OHKK-WDB3-PCB OHKK-WDB5-PCB OHKK-WDP7-PCB            
025300                     OHKK-XXKB-PCB.                                       
025400     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB KOMA-PCB XXLF-PCB             
025500                     WDB2-PCB                                             
025600                     OHKK-WDQ2-PCB OHKK-WDQ2-UPD-PCB                      
025700                     OHKK-WDQ2C-PCB                                       
025800                     OHKK-GMTA-PCB OHKK-GMTB-PCB                          
025900                     OHKK-GMTC-PCB OHKK-BETC-PCB OHKK-WDB2-PCB            
026000                     OHKK-WDB3-PCB OHKK-WDB5-PCB OHKK-WDP7-PCB            
026100                     OHKK-XXKB-PCB.                                       
026200                                                                          
026300     PERFORM A-INITIERA                                                   
026400                                                                          
026500     PERFORM IMS-RESTART                                                  
026600     DISPLAY 'W46075'                                                     
026600     PERFORM IMS-LAS-ATERSTART                                            
026700     IF 4484-KVPOST > +0                                                  
026800        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
026900     ELSE                                                                 
027000        PERFORM S01-LAS-W46006                                            
027100     END-IF                                                               
027200     IF W46006-EOF = NEJ                                                  
027300        PERFORM S02-SPARA-ORDERIDENT                                      
027400        PERFORM S03-SKAPA-MSG-KOM-AREA                                    
027500     END-IF                                                               
027600                                                                          
027700     PERFORM UNTIL W46006-EOF = JA                                        
027800                                                                          
027900        PERFORM C-BEARBETA                                                
028000        PERFORM S01-LAS-W46006                                            
028100     END-PERFORM                                                          
028200                                                                          
028300     PERFORM Z-FINIT                                                      
028400     MOVE ZERO TO RETURN-CODE                                             
028500     GOBACK                                                               
028600     .                                                                    
028700     EJECT                                                                
028800                                                                          
028900 A-INITIERA SECTION.                                                      
029000                                                                          
029100     OPEN INPUT W46006                                                    
029200                                                                          
029300     MOVE ZERO           TO W-ANT-ORDER-IN                                
029400                            DRAD-IX                                       
029500                                                                          
029600     MOVE IDPGM          TO POSTSUM-PROGNAMN                              
029700                                                                          
029800     ACCEPT DAGENS-DATUM       FROM  DATE                                 
029900     .                                                                    
030000     EJECT                                                                
030100 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
030200                                                                          
030300     PERFORM S01-LAS-W46006                                               
030400     PERFORM S02-SPARA-ORDERIDENT                                         
030500                                                                          
030600     PERFORM UNTIL W46006-EOF = JA  OR                                    
030700                     W-ANT-ORDER-IN = 4484-KVPOST                         
030800        PERFORM S01-LAS-W46006                                            
030900        IF SPAR-SORT-IDDISTR  = IN-OHUV-SORT-IDDISTR  AND                 
031000           SPAR-SORT-TIFILDAT = IN-OHUV-SORT-TIFILDAT AND                 
031100           SPAR-SORT-TIHHMMSS = IN-OHUV-SORT-TIHHMMSS AND                 
031200           SPAR-IDDISTR       = IN-OHUV-IDDISTR       AND                 
031300           SPAR-IDKUNDNR      = IN-OHUV-IDKUNDNR      AND                 
031400           SPAR-IDORDNR       = IN-OHUV-IDORDNR                           
031500           CONTINUE                                                       
031600        ELSE                                                              
031700           ADD +1        TO W-ANT-ORDER-IN                                
031800           PERFORM S02-SPARA-ORDERIDENT                                   
031900        END-IF                                                            
032000     END-PERFORM                                                          
032100     IF W46006-EOF = JA                                                   
032200        DISPLAY 'FEL VID ÅTERSTART'                                       
032300        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
032400     END-IF                                                               
032500     .                                                                    
032600     EJECT                                                                
032700 C-BEARBETA SECTION.                                                      
032800                                                                          
032900*    UNDERSÖK OM NY ORDER                                                 
033000     IF SPAR-SORT-IDDISTR  = IN-OHUV-SORT-IDDISTR  AND                    
033100        SPAR-SORT-TIFILDAT = IN-OHUV-SORT-TIFILDAT AND                    
033200        SPAR-SORT-TIHHMMSS = IN-OHUV-SORT-TIHHMMSS AND                    
033300        SPAR-IDDISTR       = IN-OHUV-IDDISTR       AND                    
033400        SPAR-IDKUNDNR      = IN-OHUV-IDKUNDNR      AND                    
033500        SPAR-IDORDNR       = IN-OHUV-IDORDNR                              
033600        CONTINUE                                                          
033700     ELSE                                                                 
033800        PERFORM S05-AVSLUTA-ORDERRADTRANS                                 
033900        PERFORM CA-TAG-CHECKPOINT                                         
033910        MOVE IN-OHUV-IDDISTR TO TEST-IDDISTR                              
033920        IF DIST14-ES                                                      
033930*         *ONLY FOR SPAIN.                                                
033970*         *WAIT IS NEEDED SO THAT IS POSSIBLE TO                          
033980*         *CONSOLIDATE ORDERS FOR SAME CUSTOMER.                          
034000          IF SPAR-IDDISTR   = IN-OHUV-IDDISTR  AND                        
034100             SPAR-IDKUNDNR  = IN-OHUV-IDKUNDNR                            
034200            CALL W009WAIT USING WS-TIME-WAIT                              
034300          END-IF                                                          
034310        END-IF                                                            
034400        PERFORM S02-SPARA-ORDERIDENT                                      
034500        MOVE IN-OHUV-SORT-IDDISTR   TO MSG-KOM-IDSNDNOD (5:4)             
034600        ADD +1                      TO MSG-KOM-TIKLOCK                    
034700     END-IF                                                               
034800                                                                          
034900     IF IN-OHUV-IDPTYP = 'RHA'                                            
034900         DISPLAY 'CB'                                                     
035000        PERFORM CB-SKAPA-ORDERHUVUDTRANS                                  
035100     ELSE                                                                 
035200        IF IN-ORAD-IDPTYP = 'RHB'                                         
034900         DISPLAY 'RHB'                                                    
035300           PERFORM CC-SKAPA-ORDERRADTRANS                                 
035400        ELSE                                                              
035500           DISPLAY 'FELAKTIG POSTTYP PÅ INFILEN W46006'                   
035600           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
035700        END-IF                                                            
035800     END-IF                                                               
035900     .                                                                    
036000     EJECT                                                                
036100                                                                          
036200 CA-TAG-CHECKPOINT SECTION.                                               
036300     SKIP2                                                                
036400*    UPPDATERA ÅTERSTARTREGISTRET                                         
036500     PERFORM IMS-LAS-ATERSTART                                            
036600                                                                          
036700     ADD +1               TO 4484-KVPOST                                  
036800     ACCEPT 4484-TIUPPDAT FROM DATE                                       
036900     ACCEPT 4484-TIUPPTID FROM TIME                                       
037000                                                                          
037100     PERFORM IMS-REPL-ATERSTART                                           
037200                                                                          
037300*    TAG CHECKPOINT                                                       
037400     PERFORM IMS-CHECKPOINT                                               
037500     .                                                                    
037600     EJECT                                                                
037700                                                                          
037800 CB-SKAPA-ORDERHUVUDTRANS SECTION.                                        
037900     SKIP2                                                                
038000                                                                          
038100     MOVE SPACE                  TO KOM-AREA                              
038200                                                                          
038300     COMPUTE MSG-KVLL = LENGTH OF DHUV-MID-W4I25101 + 17                  
038400                                                                          
038500     MOVE LOW-VALUE              TO MSG-KDZ1                              
038600     MOVE LOW-VALUE              TO MSG-KDZ2                              
038700     MOVE 'W4T251X '             TO MSG-KDTRANS-1                         
038800     MOVE '4251'                 TO MSG-IDTRANS-1                         
038900     MOVE '1'                    TO MSG-KDMFSFOR-1                        
039000                                                                          
039100     MOVE IN-OHUV-IDDISTR        TO W-WDB2-IDDISTR                        
039200     MOVE IN-OHUV-IDKUNDNR       TO W-WDB2-IDKUNDNR                       
039300     PERFORM IMS-GU-WDB201                                                
039400     IF SEGMENT-FINNS                                                     
039500       MOVE JA TO GMT-FINNS                                               
039600       IF GMT-FLLDCKND = JA                                               
039700         IF IN-OHUV-KDORDKL = 1                                           
039800            MOVE 'FW'          TO DHUV-MID-KDORDTYP-LDC                   
039900         ELSE                                                             
040000            IF IN-OHUV-KDTPOTYP = 2                                       
040100               MOVE SPACE      TO DHUV-MID-KDORDTYP-LDC                   
040200            ELSE                                                          
040300               MOVE 'PW'       TO DHUV-MID-KDORDTYP-LDC                   
040400            END-IF                                                        
040500         END-IF                                                           
040600         IF IN-OHUV-KDORDKL = 3                                           
040700            IF IN-OHUV-KDTPOTYP = 2                                       
040800               MOVE ZERO             TO DHUV-MID-TIREPDAT                 
040900               MOVE 'VIPS'           TO DHUV-MID-IDSYSTEM                 
041000                                        SPAR-IDSYSTEM                     
041100               MOVE SPACE            TO DHUV-MID-TIRFS                    
041200            ELSE                                                          
041300               MOVE GMT-IDDC-BULK(1) TO WS-IDDC                           
041400               MOVE 'LDC '           TO DHUV-MID-IDSYSTEM                 
041500                                        SPAR-IDSYSTEM                     
041600               MOVE IN-OHUV-TIBEGPAC TO DHUV-MID-TIREPDAT                 
041700               PERFORM CBA-SKAPA-RFSDATUM                                 
041800            END-IF                                                        
041900         ELSE                                                             
042000            IF IN-OHUV-KDORDKL = 2 AND                                    
042100               IN-OHUV-TIBEGPAC > ZERO                                    
042200               MOVE GMT-IDDC-BULK(1) TO WS-IDDC                           
042300               MOVE 'LDC '           TO DHUV-MID-IDSYSTEM                 
042400                                        SPAR-IDSYSTEM                     
042500               MOVE IN-OHUV-TIBEGPAC TO DHUV-MID-TIREPDAT                 
042600               PERFORM CBA-SKAPA-RFSDATUM                                 
042700            ELSE                                                          
042800               MOVE 'VIPS'           TO DHUV-MID-IDSYSTEM                 
042900                                        SPAR-IDSYSTEM                     
043000               MOVE SPACE            TO DHUV-MID-TIRFS                    
043100            END-IF                                                        
043200         END-IF                                                           
043300       ELSE                                                               
043400         MOVE 'VIPS'             TO DHUV-MID-IDSYSTEM                     
043500                                    SPAR-IDSYSTEM                         
043600         MOVE SPACE              TO DHUV-MID-TIRFS                        
043700       END-IF                                                             
043800     ELSE                                                                 
043900       MOVE NEJ TO GMT-FINNS                                              
044000       MOVE 'VIPS'               TO DHUV-MID-IDSYSTEM                     
044100                                    SPAR-IDSYSTEM                         
044200       MOVE SPACE                TO DHUV-MID-TIRFS                        
044300     END-IF                                                               
044400     MOVE IN-OHUV-IDDISTR        TO DHUV-MID-IDDISTR                      
044500     MOVE IN-OHUV-IDKUNDNR       TO DHUV-MID-IDKUNDNR                     
044600     MOVE IN-OHUV-IDORDNR        TO DHUV-MID-IDORDNR                      
044700     MOVE IN-OHUV-KDORDKL        TO DHUV-MID-KDORDKL                      
044710     MOVE IN-OHUV-IDDISTR        TO TEST-IDDISTR                          
044720     IF DIST07-KINA AND IN-OHUV-KDFRAKT > 0                               
044731        MOVE IN-OHUV-KDFRAKT     TO DHUV-MID-KDFRAKT                      
044740     ELSE                                                                 
044800        MOVE SPACE               TO DHUV-MID-KDFRAKT                      
044810     END-IF                                                               
044900     MOVE IN-OHUV-KDORDURS       TO SPAR-KDORDURS                         
045000*    MOVE SPACE                  TO DHUV-MID-TIRFS                        
045100     PERFORM CBB-KOLLA-KONSOLIDERING                                      
045200     IF KONSOLIDERING                                                     
045200       DISPLAY 'CONSOLIDATING YES'                                        
045300        MOVE OHKK-IDORDNR-UT     TO W-IDORDNR                             
045400        STRING 'NOAC OC ' W-IDORDNR DELIMITED BY SIZE                     
045500                               INTO DHUV-MID-BEKUNDRF                     
045600     ELSE                                                                 
045200       DISPLAY 'CONSOLIDATING NO '                                        
045700        MOVE IN-OHUV-BEVOLREF    TO DHUV-MID-BEKUNDRF                     
045800                                    SPAR-OHUV-BEVOLREF                    
045900     END-IF                                                               
045700        DISPLAY 'MID-BEKUND2'SPAR-OHUV-BEVOLREF                           
045700        DISPLAY 'MID-BEKUND3'DHUV-MID-BEKUNDRF                            
046000     MOVE SPACE                  TO DHUV-MID-KDFAKTYP                     
046100     MOVE IN-OHUV-FLRESTN        TO DHUV-MID-FLRESTN                      
046200     MOVE IN-OHUV-KDTPOTYP       TO DHUV-MID-KDTPOTYP                     
046400     MOVE SPACE                  TO DHUV-MID-TITPO                        
046500*IDBILREG                                                                 
046600     MOVE IN-OHUV-IDBILREG       TO DHUV-MID-IDBILREG                     
046700*    IF DHUV-MID-KDORDKL = '1' OR '2'                                     
046800*       ACCEPT TIKLOCK           FROM TIME                                
046900*       MOVE TIHH                TO FIX-TIHH                              
047000*       MOVE TIMMS               TO FIX-TIMMS                             
047100*       MOVE SPAR-BELAGINS       TO DHUV-MID-BELAGINS                     
047200*    ELSE                                                                 
047300     MOVE SPACE                  TO DHUV-MID-BELAGINS                     
047400*    END-IF                                                               
047500     MOVE IN-OHUV-BEGMT          TO DHUV-MID-BEGMT                        
047600     MOVE IN-OHUV-ADGMT-GATA     TO DHUV-MID-ADGMT-GATA                   
047700     MOVE IN-OHUV-ADGMT-PADR     TO DHUV-MID-ADGMT-PADR                   
047800     MOVE IN-OHUV-KDROPACK       TO DHUV-MID-KDROPACK                     
047900     MOVE SPACE                  TO DHUV-MID-IDKONTO                      
048000                                    DHUV-MID-IDKST                        
048100                                    DHUV-MID-IDANALYS                     
048200     MOVE IN-OHUV-BEVARREF       TO DHUV-MID-BEVARREF                     
048300     MOVE SPACE                  TO DHUV-MID-KDTULLVE                     
048400     MOVE SPACE                  TO DHUV-MID-KDNOTES                      
048500     MOVE SPACE                  TO DHUV-MID-FLAUTFAK                     
048600     MOVE NEJ                    TO DHUV-MID-FLAUTPAC                     
048700     MOVE NEJ                    TO DHUV-MID-FLEMBORD                     
048800     MOVE NEJ                    TO DHUV-MID-FLOVRLEV                     
048900     MOVE NEJ                    TO DHUV-MID-FLFORBI                      
049000     MOVE IN-OHUV-IDKAMPRF       TO DHUV-MID-IDKAMPRF                     
049100     MOVE SPACE                  TO DHUV-MID-IDFTG                        
049200     MOVE SPACE                  TO DHUV-MID-ADBET                        
049300     MOVE SPACE                  TO DHUV-MID-BEBET                        
049400     MOVE SPACE                  TO DHUV-MID-IDSKYLT                      
049500     MOVE SPACE                  TO DHUV-MID-IDDC                         
049600     MOVE SPACE                  TO DHUV-MID-FLLSBOK                      
049700     MOVE ZERO                   TO DHUV-MID-IDDEPT                       
049800     MOVE OHKK-FLORDTIL          TO DHUV-MID-FLORDTIL                     
050000     MOVE ZERO                   TO DHUV-MID-IDGROSS                      
050100     MOVE IN-OHUV-IDBILREG       TO DHUV-MID-IDBILREG                     
050200     MOVE SPACE                  TO DHUV-MID-IDVIN                        
050300                                    DHUV-MID-IDCISNR                      
050400                                                                          
050500     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
050600     CALL W006KOM USING MSG-PCB                                           
050700                        DISP-PCB                                          
050800                        KOMA-PCB                                          
050900                        MSG-KOM-WMSGKOM                                   
051000                        MSG-IO-AREA                                       
051100     PERFORM UNTIL MSG-KOM-IDMFSMED NOT = '120'                           
051200*       DUBBLETTPOST PÅ KOMMUNIKATIONS DB                                 
051300*       ADDERA 1 TILL TIDEN FÖR ATT GÖRA NYCKEL UNIK                      
051400        ADD +1                   TO MSG-KOM-TIKLOCK                       
051500        MOVE SPACE               TO MSG-KOM-IDMFSMED                      
051600        CALL W006KOM USING MSG-PCB                                        
051700                           DISP-PCB                                       
051800                           KOMA-PCB                                       
051900                           MSG-KOM-WMSGKOM                                
052000                           MSG-IO-AREA                                    
052100     END-PERFORM                                                          
052200     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
052300        DISPLAY ' DATUM/TID EJ NUM PÅ INPUTFIL W46006/60'                 
052400        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
052500     END-IF                                                               
052600                                                                          
052700     MOVE SPACE                  TO KOM-AREA                              
052800     .                                                                    
052900     EJECT                                                                
053000                                                                          
053100 CBA-SKAPA-RFSDATUM SECTION.                                              
053200     SKIP2                                                                
053300     PERFORM CBAA-GET-LOCALDATE                                           
053400     MOVE WS-IDDC                  TO WORK-IDDC                           
053500     MOVE +002                     TO WORK-KDCALL                         
053600     MOVE +001                     TO WORK-KVWORKD                        
053700     IF  IN-OHUV-TIBEGPAC = ZERO                                          
053800      OR IN-OHUV-TIBEGPAC < WS-TILOKDAT                                   
053900        MOVE WS-TILOKDAT           TO WORK-TIAAMMDD-FOM                   
054000     ELSE                                                                 
054100        MOVE IN-OHUV-TIBEGPAC      TO WORK-TIAAMMDD-FOM                   
054200     END-IF                                                               
054300     CALL WORKDAY                  USING WORK-KDCALL                      
054400                                         WORK-DATE-AREA                   
054500                                         WORK-KDSVAR                      
054600     IF WORK-KDSVAR-FEL                                                   
054700        MOVE 'SECT CBA-1, DATUM SAKNAS I WORKDAY'                         
054800                                   TO FELTEXT                             
054900        CALL ABEND                 USING RKOD-ABEND-UTAN-DUMP             
055000     ELSE                                                                 
055100       MOVE +003                   TO WORK-KDCALL                         
055200                                                                          
055300       MOVE GMT-KVDAGAR-RFS-DEF    TO WORK-KVWORKD                        
055400       PERFORM                                                            
055500       VARYING RFS-IX FROM 1 BY 1                                         
055600         UNTIL RFS-IX > MAX-RFS-IX                                        
055700         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
055800           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
055900                                   TO WORK-KVWORKD                        
056000         END-IF                                                           
056100       END-PERFORM                                                        
056200       ADD +1  TO WORK-KVWORKD                                            
056300*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
056400*      ANTAL DAGAR FÖRE RFS.                                              
056500*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
056600*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
056700*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
056800*                                                                         
056900       CALL WORKDAY                USING WORK-KDCALL                      
057000                                         WORK-DATE-AREA                   
057100                                         WORK-KDSVAR                      
057200       IF WORK-KDSVAR-FEL                                                 
057300          MOVE 'SECT CBA-2, DATUM SAKNAS I WORKDAY'                       
057400                                   TO FELTEXT                             
057500          CALL ABEND               USING RKOD-ABEND-UTAN-DUMP             
057600       ELSE                                                               
057700         IF WORK-TIAAMMDD-FOM < WS-TILOKDAT                               
057800           MOVE WS-IDDC            TO WORK-IDDC                           
057900           MOVE +002               TO WORK-KDCALL                         
058000           MOVE +001               TO WORK-KVWORKD                        
058100           MOVE WS-TILOKDAT        TO WORK-TIAAMMDD-FOM                   
058200           CALL WORKDAY            USING WORK-KDCALL                      
058300                                         WORK-DATE-AREA                   
058400                                         WORK-KDSVAR                      
058500           IF WORK-KDSVAR-FEL                                             
058600              MOVE 'SECT CBA-3, DATUM SAKNAS I WORKDAY'                   
058700                                   TO FELTEXT                             
058800              CALL ABEND           USING RKOD-ABEND-UTAN-DUMP             
058900           ELSE                                                           
059000              MOVE WORK-TIAAMMDD-TOM TO DHUV-MID-TIRFS                    
059100           END-IF                                                         
059200         ELSE                                                             
059300           MOVE WORK-TIAAMMDD-FOM  TO DHUV-MID-TIRFS                      
059400         END-IF                                                           
059500       END-IF                                                             
059600     END-IF                                                               
059700     .                                                                    
059800     EJECT                                                                
059900                                                                          
060000 CBAA-GET-LOCALDATE SECTION.                                              
060100                                                                          
060200     MOVE ALL '+'                 TO MSGI-WMSGINIT                        
060300     MOVE '013'                   TO MSGI-KDCALL                          
060400     MOVE 'WIDDC   '              TO MSGI-IDUSER                          
060500     MOVE WS-IDDC                 TO MSGI-IDUSER(6:2)                     
060600     MOVE 'W460'                  TO MSGI-IDTRANS                         
060700                                                                          
060800     CALL W005INIT USING MSGI-WMSGINIT OHKK-WDP7-PCB                      
060900                                                                          
061000     MOVE   MSGI-TILOKDAT         TO   WS-TILOKDAT                        
061100     .                                                                    
061200     EJECT                                                                
061300                                                                          
061400 CBB-KOLLA-KONSOLIDERING   SECTION.                                       
061500                                                                          
061550     IF  IN-OHUV-BEGMT        EQUAL SPACE AND                             
061560         IN-OHUV-ADGMT        EQUAL SPACE                                 
061600                                                                          
061700         MOVE 1                TO OHKK-KDCALL                             
061800         MOVE 'VIPS'           TO OHKK-IDSYSTEM                           
061900         MOVE IN-OHUV-IDDISTR  TO OHKK-IDDISTR                            
062000                              TEST-IDDISTR                                
062100         MOVE IN-OHUV-IDKUNDNR TO OHKK-IDKUNDNR                           
062200         MOVE IN-OHUV-IDORDNR  TO OHKK-IDORDNR-IN                         
062300                                                                          
062400         MOVE ALL '+' TO OHKK-ADBETRAD-1                                  
062500                         OHKK-ADBETRAD-2                                  
062600                         OHKK-ADGMT                                       
062700                         OHKK-BEBETRAD-1                                  
062800                         OHKK-BEBETRAD-2                                  
062900                         OHKK-BEGMRK                                      
063000                         OHKK-BEGMT                                       
063100                         OHKK-KDFAKTYP                                    
063200         MOVE SPACE   TO OHKK-IDANALYS                                    
063300                         OHKK-IDBIPREF                                    
063400                         OHKK-IDDC-TVS                                    
063500                         OHKK-IDDEPOT                                     
063600                         OHKK-IDRFTAB                                     
063700                         OHKK-IDROUTE                                     
063800                         OHKK-IDSKYLT                                     
063900                         OHKK-IDZON                                       
064000                         OHKK-IDKST                                       
064100                                                                          
064200         MOVE ZERO    TO OHKK-IDDEPT                                      
064300                         OHKK-IDFTG                                       
064400                         OHKK-IDKONTO                                     
064500                         OHKK-KDORDING                                    
064600                         OHKK-KDVRINFO                                    
064700                         OHKK-KDTULLVE                                    
064800                         OHKK-KVDAGAR-DOW                                 
064900                         OHKK-RESLATT                                     
065000                         OHKK-TITPO                                       
065100         MOVE NEJ     TO OHKK-FLAUTFAK                                    
065200                         OHKK-FLAUTPAC                                    
065300                         OHKK-FLFORBI                                     
065400                         OHKK-FLORDSPE                                    
065500                         OHKK-FLOVRLEV                                    
065600                         OHKK-FLVORKO                                     
065700                                                                          
065800         MOVE JA TO OHKK-FLLSBOK                                          
065900                                                                          
066000        IF DIST20-EMBALLAGE                                               
066100          MOVE JA   TO OHKK-FLEMBORD                                      
066200        ELSE                                                              
066300          MOVE NEJ  TO OHKK-FLEMBORD                                      
066400        END-IF                                                            
066500                                                                          
066600                                                                          
066700        MOVE GMT-FLPRELRO TO OHKK-FLPRELRO                                
066800        MOVE GMT-FLPRERS  TO OHKK-FLPRERS                                 
066900                                                                          
067000        CALL W460DIS1 USING DIS1-W460DIS1                                 
067100                                                                          
067200        MOVE IN-OHUV-KDORDKL          TO OHKK-KDORDKL                     
067300        MOVE IN-OHUV-FLRESTN          TO OHKK-FLRESTN                     
067400        MOVE IN-OHUV-IDKAMPRF         TO OHKK-IDKAMPRF                    
067500        MOVE IN-OHUV-KDTPOTYP         TO OHKK-KDTPOTYP                    
067600        MOVE NEJ                      TO OHKK-FLSOFT                      
067601        IF DHUV-MID-KDFRAKT NOT = SPACE                                   
067610           MOVE DHUV-MID-KDFRAKT      TO OHKK-KDFRAKT                     
067620        ELSE                                                              
068200           MOVE ZERO                  TO OHKK-KDFRAKT                     
068300        END-IF                                                            
068400                                                                          
068510        CALL W411OHKK USING OHKK-W411OHKK OHKK-WDQ2-PCB                   
068600                            OHKK-WDQ2-UPD-PCB                             
068700                            OHKK-WDQ2C-PCB                                
068800                            OHKK-GMTA-PCB                                 
068900                            OHKK-GMTB-PCB OHKK-GMTC-PCB                   
069000                            OHKK-BETC-PCB OHKK-WDB2-PCB                   
069100                            OHKK-WDB3-PCB OHKK-WDB5-PCB                   
069200                            OHKK-WDP7-PCB OHKK-XXKB-PCB                   
069300                                                                          
069310     ELSE                                                                 
069320        MOVE ZERO      TO  OHKK-IDORDER                                   
069330     END-IF                                                               
069400     IF OHKK-IDORDER > ZERO                                               
069500        MOVE JA  TO KONSOLIDERING-SW                                      
069600     ELSE                                                                 
069700        MOVE NEJ TO KONSOLIDERING-SW                                      
069800     END-IF                                                               
069900     .                                                                    
070000                                                                          
070100                                                                          
070200 CC-SKAPA-ORDERRADTRANS SECTION.                                          
070300     SKIP2                                                                
070300     DISPLAY 'CC'                                                         
070400                                                                          
070500*    ADDERA UPP INDEX TILL ORDERRADTRANSAKTION OCH UNDERSÖK               
070600*    OM ORDERRADTRANSAKTION FULL                                          
070700     ADD +1         TO DRAD-IX                                            
070800     IF DRAD-IX > DRAD-IX-MAX                                             
070900        PERFORM S05-AVSLUTA-ORDERRADTRANS                                 
071000        ADD +1      TO DRAD-IX                                            
071100     END-IF                                                               
071200                                                                          
071300     COMPUTE MSG-KVLL = LENGTH OF DRAD-MID-W4I25201 + 17                  
071400                                                                          
071500     MOVE LOW-VALUE              TO MSG-KDZ1                              
071600     MOVE LOW-VALUE              TO MSG-KDZ2                              
071700     MOVE 'W4T252X '             TO MSG-KDTRANS-1                         
071800     MOVE '4252'                 TO MSG-IDTRANS-1                         
071900     MOVE '1'                    TO MSG-KDMFSFOR-1                        
072000                                                                          
072100     MOVE SPAR-IDSYSTEM          TO DRAD-MID-IDSYSTEM                     
072200     MOVE IN-ORAD-IDDISTR        TO DRAD-MID-IDDISTR                      
072300     MOVE IN-ORAD-IDKUNDNR       TO DRAD-MID-IDKUNDNR                     
072400     IF KONSOLIDERING                                                     
072500        MOVE OHKK-IDORDNR-UT     TO DRAD-MID-IDORDNR                      
072600        MOVE IN-ORAD-IDORDNR     TO DRAD-MID-IDKUNDRF-RO                  
072700     ELSE                                                                 
072800        MOVE IN-ORAD-IDORDNR     TO DRAD-MID-IDORDNR                      
072900        MOVE SPACE               TO DRAD-MID-IDKUNDRF-RO                  
073000     END-IF                                                               
073100     MOVE SPAR-OHUV-BEVOLREF     TO DRAD-MID-BEVOLREF                     
073200     MOVE 'N'                    TO DRAD-MID-FLSLUT                       
073300     MOVE SPAR-KDORDURS          TO DRAD-MID-KDORDURS                     
073400                                                                          
073500     MOVE IN-ORAD-IDARTNR        TO DRAD-MID-IDARTNR   (DRAD-IX)          
073600     MOVE IN-ORAD-REKSIFFR       TO DRAD-MID-REKSIFFR  (DRAD-IX)          
073700     IF DRAD-MID-REKSIFFR (DRAD-IX) = SPACE OR '0'                        
073800        IF DRAD-MID-IDARTNR (DRAD-IX) NUMERIC                             
073900           MOVE IN-ORAD-IDARTNR  TO REK-IDARTNR                           
074000           MOVE 0                TO REK-REKSIFFR                          
074100           CALL W009KSIF USING REK-IDARTNR                                
074200                               REK-LNGD                                   
074300                               REK-REKSIFFR                               
074400           MOVE REK-REKSIFFR     TO DRAD-MID-REKSIFFR (DRAD-IX)           
074500        END-IF                                                            
074600     END-IF                                                               
074700     EJECT                                                                
074800     MOVE IN-ORAD-KVBEART        TO DRAD-MID-KVBEART   (DRAD-IX)          
074900     MOVE SPACE                  TO DRAD-MID-PRARTNTO  (DRAD-IX)          
075000                                                                          
075100     MOVE IN-ORAD-PRARTNTO-LOC  TO W-PRARTXXX-MED-PUNKT                   
075200     MOVE W-PRARTXXX-MED-PUNKT  TO DRAD-MID-PRARTNTO-LOC (DRAD-IX)        
075300                                                                          
075400     MOVE IN-ORAD-PRARTBTO-LOC  TO W-PRARTXXX-MED-PUNKT                   
075500     MOVE W-PRARTXXX-MED-PUNKT  TO DRAD-MID-PRARTBTO-LOC (DRAD-IX)        
075600                                                                          
075700     MOVE IN-ORAD-KDVALISO      TO DRAD-MID-KDVALISO     (DRAD-IX)        
075800     MOVE IN-ORAD-KDVAT         TO DRAD-MID-KDVAT        (DRAD-IX)        
075900     MOVE IN-ORAD-RERAB         TO DRAD-MID-RERAB        (DRAD-IX)        
076000     MOVE IN-ORAD-KDRAB         TO DRAD-MID-KDRAB        (DRAD-IX)        
076100     MOVE IN-ORAD-BEART-VIPS    TO DRAD-MID-BEART-VIPS   (DRAD-IX)        
076200     MOVE IN-ORAD-TITPO          TO W-TITPO-TAB                           
076300     SET W-TITPO-IX TO +1                                                 
076400     PERFORM UNTIL W-TITPO-IX > 6                                         
076500        IF W-TITPO-POS (W-TITPO-IX) = SPACE                               
076600           MOVE '0'              TO W-TITPO-POS (W-TITPO-IX)              
076700        ELSE                                                              
076800           SET W-TITPO-IX TO +6                                           
076900        END-IF                                                            
077000        SET W-TITPO-IX UP BY +1                                           
077100     END-PERFORM                                                          
077200     MOVE W-TITPO-TAB            TO IN-ORAD-TITPO                         
077300     IF IN-ORAD-TITPO = ZERO                                              
077400        MOVE SPACE               TO DRAD-MID-TITPO     (DRAD-IX)          
077500     ELSE                                                                 
077600        MOVE IN-ORAD-TITPO       TO DRAD-MID-TITPO     (DRAD-IX)          
077700     END-IF                                                               
077800     MOVE SPACE                  TO DRAD-MID-FLRESTN   (DRAD-IX)          
077900     MOVE '1'                    TO DRAD-MID-KDKVBRYT  (DRAD-IX)          
078000     MOVE SPACE                  TO DRAD-MID-FLINVEST  (DRAD-IX)          
078100     MOVE SPACE                  TO DRAD-MID-KDVRINFO  (DRAD-IX)          
078200     MOVE SPACE                  TO DRAD-MID-IDKONTO   (DRAD-IX)          
078300     MOVE SPACE                  TO DRAD-MID-IDKST     (DRAD-IX)          
078400     MOVE IN-ORAD-BERADREF       TO DRAD-MID-BERADREF  (DRAD-IX)          
078400     DISPLAY 'DRAD-MID'DRAD-MID-BERADREF  (DRAD-IX)                       
078500     MOVE IN-ORAD-KDDSP          TO DRAD-MID-KDDSP     (DRAD-IX)          
078600     MOVE IN-ORAD-FLSLATT        TO DRAD-MID-FLSLATT   (DRAD-IX)          
078700     MOVE IN-ORAD-FLDIRLEV       TO DRAD-MID-FLDIRLEV  (DRAD-IX)          
078800     MOVE SPACE                  TO DRAD-MID-IDBIL     (DRAD-IX)          
078900                                                                          
079000     IF   GMT-FINNS = JA                                                  
079100     AND  GMT-FLLDCKND = JA                                               
079200       MOVE IN-ORAD-BERADREF   TO DRAD-MID-IDKUNDRF-WIP(DRAD-IX)          
079300     ELSE                                                                 
079400       MOVE SPACE              TO DRAD-MID-IDKUNDRF-WIP(DRAD-IX)          
079500     END-IF                                                               
079600     .                                                                    
079700     EJECT                                                                
079800                                                                          
079900 Z-FINIT    SECTION.                                                      
080000                                                                          
080100     PERFORM S05-AVSLUTA-ORDERRADTRANS                                    
080200                                                                          
080300     CLOSE  W46006                                                        
080400                                                                          
080500*    NOLLA ÅTERSTARTINFORMATIONEN                                         
080600     PERFORM IMS-LAS-ATERSTART                                            
080700                                                                          
080800     MOVE +0       TO 4484-KVPOST                                         
080900     ACCEPT 4484-TIUPPDAT FROM DATE                                       
081000     ACCEPT 4484-TIUPPTID FROM TIME                                       
081100                                                                          
081200     PERFORM IMS-REPL-ATERSTART                                           
081300                                                                          
081400     MOVE 'S'      TO POSTSUM-OPKOD                                       
081500     CALL POSTSUM USING POSTSUM-PARM                                      
081600     .                                                                    
081700     EJECT                                                                
081800                                                                          
081900 S01-LAS-W46006 SECTION.                                                  
082000     SKIP2                                                                
082100     READ W46006 INTO IN-AREA                                             
082200       AT END                                                             
082300          MOVE JA TO W46006-EOF                                           
082400     END-READ                                                             
082500                                                                          
082600     IF W46006-EOF = NEJ                                                  
082700        MOVE 'W46006'          TO POSTSUM-FDNAMN                          
082800        MOVE 'W46075D1'        TO POSTSUM-DDNAMN2                         
082900        MOVE IN-OHUV-IDPTYP    TO POSTSUM-TRANSTYP                        
083000        CALL POSTSUM USING POSTSUM-PARM                                   
083100     END-IF                                                               
083200     .                                                                    
083300     EJECT                                                                
083400                                                                          
083500 S02-SPARA-ORDERIDENT SECTION.                                            
083600                                                                          
083700     MOVE IN-OHUV-SORT-IDDISTR      TO SPAR-SORT-IDDISTR                  
083800     MOVE IN-OHUV-SORT-TIFILDAT     TO SPAR-SORT-TIFILDAT                 
083900     MOVE IN-OHUV-SORT-TIHHMMSS     TO SPAR-SORT-TIHHMMSS                 
084000     MOVE IN-OHUV-IDDISTR           TO SPAR-IDDISTR                       
084100     MOVE IN-OHUV-IDKUNDNR          TO SPAR-IDKUNDNR                      
084200     MOVE IN-OHUV-IDORDNR           TO SPAR-IDORDNR                       
084300     .                                                                    
084400     EJECT                                                                
084500                                                                          
084600 S03-SKAPA-MSG-KOM-AREA SECTION.                                          
084700     SKIP2                                                                
084800     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
084900     MOVE +54                    TO MSG-KOM-KVLL                          
085000     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
085100     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
085200     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
085300     MOVE 'RHA/RHB'              TO MSG-KOM-IDCPYTXT                      
085400     MOVE 'NOAC'                 TO MSG-KOM-IDSNDNOD                      
085500     MOVE IN-OHUV-SORT-IDDISTR   TO MSG-KOM-IDSNDNOD (5:4)                
085600     MOVE 'W4607500'             TO MSG-KOM-IDSNDJOB                      
085700     MOVE IN-OHUV-SORT-TIFILDAT  TO MSG-KOM-TIREGDAT                      
085800     MOVE IN-OHUV-SORT-TIHHMMSS  TO W-TIHHMMSS                            
085900     MOVE +1                     TO W-TITH                                
086000     MOVE W-TIKLOCK              TO MSG-KOM-TIKLOCK                       
086100     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
086200     .                                                                    
086300     EJECT                                                                
086400 S05-AVSLUTA-ORDERRADTRANS SECTION.                                       
086500                                                                          
086600     IF DRAD-IX > 0                                                       
086700        IF DRAD-IX NOT > DRAD-IX-MAX                                      
086800           MOVE 'J'              TO DRAD-MID-FLSLUT                       
086900        END-IF                                                            
087000                                                                          
087100        MOVE KOM-AREA            TO MSG-INDATA-MINUS-1-TRANSKOD           
087200        CALL W006KOM USING MSG-PCB                                        
087300                           DISP-PCB                                       
087400                           KOMA-PCB                                       
087500                           MSG-KOM-WMSGKOM                                
087600                           MSG-IO-AREA                                    
087700        IF MSG-KOM-IDMFSMED NOT = SPACE                                   
087800           DISPLAY ' DATUM/TID EJ NUM PÅ INPUTFIL W46006/60'              
087900           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
088000        END-IF                                                            
088100     END-IF                                                               
088200                                                                          
088300     MOVE ZERO     TO DRAD-IX                                             
088400     MOVE SPACE    TO KOM-AREA                                            
088500     .                                                                    
088600     EJECT                                                                
088700* IMS SECTIONER                                                           
088800     SKIP3                                                                
088900                                                                          
089000 IMS-GU-WDB201      SECTION.                                              
089100                                                                          
089200     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
089300          DELIMITED BY SIZE INTO SSA1                                     
089400     MOVE '  GE' TO GODK-STATUSKODER                                      
089500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
089600     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
089700     PERFORM IMS-STATUSKONTROLL                                           
089800     .                                                                    
089900     EJECT                                                                
090000                                                                          
090100 IMS-RESTART SECTION.                                                     
090200     SKIP2                                                                
090300                                                                          
090400     MOVE SPACE TO MSG-IO-AREA-1                                          
090500     MOVE '  ' TO GODK-STATUSKODER                                        
090600     CALL CBLTDLI USING XRST MSG-PCB                                      
090700                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
090800                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
090900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
091000     PERFORM IMS-STATUSKONTROLL                                           
091100                                                                          
091200     .                                                                    
091300 IMS-CHECKPOINT SECTION.                                                  
091400                                                                          
091500     MOVE IDPGM  TO MSG-IO-AREA-1                                         
091600     MOVE '  XD' TO GODK-STATUSKODER                                      
091700     CALL CBLTDLI USING CHKP MSG-PCB                                      
091800                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
091900                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
092000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092100     PERFORM IMS-STATUSKONTROLL                                           
092200     IF IMS-EJ-OK                                                         
092300       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
092400       MOVE ' IMS-KONTROLLREGION EJ TILLGÄNGLIG '                         
092500                            TO FELTEXT                                    
092600       CALL FELLOG                                                        
092700     END-IF                                                               
092800     .                                                                    
092900     EJECT                                                                
093000 IMS-LAS-ATERSTART SECTION.                                               
093100     SKIP2                                                                
093200     MOVE '4483'         TO IDHTYP                                        
093300     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
093400     STRING 'WLXXLF01(WDGXKEY  =' WDGX01 ')'                              
093500                    DELIMITED BY SIZE INTO SSA1                           
093600     MOVE 'WLXXLF11 '    TO SSA2                                          
093700     MOVE '  '           TO GODK-STATUSKODER                              
093800     CALL CBLTDLI USING GHU XXLF-PCB XXLF-IO-AREA SSA1 SSA2               
093900     MOVE XXLF-STATUS-CODE TO STATUS-WS                                   
094000     PERFORM IMS-STATUSKONTROLL                                           
094100                                                                          
094200     .                                                                    
094300 IMS-REPL-ATERSTART SECTION.                                              
094400     SKIP2                                                                
094500     MOVE '  '             TO GODK-STATUSKODER                            
094600     CALL CBLTDLI USING REPL XXLF-PCB XXLF-IO-AREA                        
094700     MOVE XXLF-STATUS-CODE TO STATUS-WS                                   
094800     PERFORM IMS-STATUSKONTROLL                                           
094900     .                                                                    
095000     SKIP2                                                                
095100                                                                          
095200 IMS-STATUSKONTROLL SECTION.                                              
095300     SET STATUS-IX TO 1                                                   
095400     SEARCH GODK-STATUS                                                   
095500       AT END                                                             
095600         MOVE ' STATUSKOD FRÅN IMS EJ TILLÅTEN '                          
095700                            TO FELTEXT                                    
095800         CALL FELLOG                                                      
095900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
096000         CONTINUE                                                         
096100     END-SEARCH                                                           
096200     .                                                                    
097000     EJECT                                                                
