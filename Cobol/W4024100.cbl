000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4024100.                                                
000400 AUTHOR.         GÖRAN KJELLSON   GUIDE DATAKONSULT AB                    
000500 DATE-WRITTEN.   MARS -90.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR UPPLÄGGNING AV ORDERHUVUD I                  
001100*        ORDERKÖN. REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA            
001200*        HÄMTAS FRÅN KUNDREGISTRET.                                       
001300*        EFTER UPPLÄGGNING AV GODKÄNT ORDERHUVUD SKER UTHOPP TILL         
001400*        REGISTRERING AV ORDERRADER                                       
001500*                                                                         
001600*        PROGRAMMET ÄNDRAT MARS 2007 SÅ KONTROLL SKER OM                  
001700*        TVINGANDE TILLÄGG SKALL GÖRAS.                                   
001800*        KONTROLL SKER MED HJÄLP AV W411OHKK OCH W411TVAG                 
001900*        OM TILLÄGG SKALL GÖRAS SKER UTHOPP TILL 4206 FÖR                 
002000*        TILLÄGGSREGISTRERING                                             
002100*                                                                         
002200*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002300*        PROGRAMMET UPPDATERAR WLORQI (WDQ2)  ORDERHUVUD                  
002500*        PROGRAMMET LÄSER      WLGMTA (WDB2)  KUNDREGISTER                
002600*        PROGRAMMET LÄSER      WLGMTB (WDB3)  KUNDREGISTER                
002700*        PROGRAMMET LÄSER      WLGMTC (WDB5)  KUNDREGISTER                
002800*        PROGRAMMET UPPDATERAR WLXXKP (WDR1)  ORDERNUMMEREGISTER          
002900*        PROGRAMMET LÄSER              WDM2   KAMPANJREGISTER             
003000*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTREGISTER           
003100*                                                                         
003200*    INDATA.                                                              
003300*        TRANSAKTION: W4T241                                              
003400*        MID:         W4I24101                                            
003500*                                                                         
003600*    UTDATA.                                                              
003700*        MOD:         W4O24101                                            
003800                                                                          
003900     EJECT                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100                                                                          
004200 DATA DIVISION.                                                           
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP3                                                                
004500                                                                          
004600*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(08)   VALUE 'W4024100'.            
004800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  4206-MOD-LAENGD             PIC S9(4)  VALUE +83   COMP SYNC.        
005000 77  4242-MOD-LAENGD             PIC S9(4)  VALUE +104  COMP SYNC.        
005200 77  HOPP                        PIC X(1)   VALUE 'N'.                    
005300 77  YES                         PIC X(1)   VALUE 'Y'.                    
005400 77  WS-TEDDI                    PIC X(11)  VALUE SPACE.                  
005500                                                                          
006100*   -COPY WWDCKONS                                                        
006200                                                                          
006300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006400     88  ALLT-OK                             VALUE 'J'.                   
006500                                                                          
006610 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006700     88  EGEN-MID                            VALUE '4241'.                
006800                                                                          
006900 01  FILLER PIC X(16)   VALUE 'SPARAREA'.                                 
007000 01  SPAR-AREA.                                                           
007100     03  SPAR-IDTRANS            PIC X(4).                                
007110     03  SPAR-IDDC               PIC X(2).                                
007200                                                                          
007300 01  WS-DATUM-TID                PIC 9(10).                               
007400 01  FILLER REDEFINES WS-DATUM-TID.                                       
007500     03  WS-DATUM                PIC 9(6).                                
007600     03  WS-TID                  PIC 9(4).                                
007700                                                                          
007800 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
007900 01  FILLER REDEFINES WS-TIHHMMSS.                                        
008000     03  WS-TIHHMM               PIC 9(4).                                
008100     03  FILLER                  PIC 9(2).                                
008200     EJECT                                                                
008300                                                                          
008400 01  WS-ALFA-1.                                                           
008500     03  WS-NUM-1                PIC 9(1).                                
008600 01  WS-ALFA-2.                                                           
008700     03  WS-NUM-2                PIC 9(2).                                
008800 01  WS-ALFA-4.                                                           
008900     03  WS-NUM-4                PIC 9(4).                                
009000 01  WS-ALFA-5.                                                           
009100     03  WS-NUM-5                PIC 9(5).                                
009200 01  WS-ALFA-6.                                                           
009300     03  WS-NUM-6                PIC 9(6).                                
009400 01  WS-ALFA-7.                                                           
009500     03  WS-NUM-7                PIC 9(7).                                
009600 01  WS-ALFA-10.                                                          
009700     03  WS-NUM-10               PIC 9(10).                               
009800 01  WS-ALFA-1V3.                                                         
009900     03  WS-ALFA-HELTAL          PIC X(1).                                
010000     03  WS-ALFA-PUNKT           PIC X(1).                                
010100     03  WS-ALFA-DECIMAL         PIC X(3).                                
010200 01  WS-NUM-1V3                  PIC 9V9(3).                              
010300 01  FILLER REDEFINES WS-NUM-1V3.                                         
010400     03  WS-NUM-HELTAL           PIC 9(1).                                
010500     03  WS-NUM-DECIMAL          PIC 9(3).                                
010600     EJECT                                                                
010700                                                                          
010800 01  WS-TIREGDAT-9KOMPL          PIC 9(9)    VALUE ZERO.                  
010900                                                                          
011000 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
011100                                                                          
011200 01  FILLER REDEFINES TEST-IDDISTR.                                       
011300*    03 -COPY WWDIST03                                                    
011400     EJECT                                                                
011500 01  FILLER REDEFINES TEST-IDDISTR.                                       
011600*    03 -COPY WWDIST15                                                    
011700     EJECT                                                                
011800 01  FILLER REDEFINES TEST-IDDISTR.                                       
011900*    03 -COPY WWDIST20                                                    
012000 01  FILLER REDEFINES TEST-IDDISTR.                                       
012100*    ----DIST79-DEALER-PRICE----                                          
012200*    03 -COPY WWDIST79                                                    
012300     EJECT                                                                
012400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012500 01  GENERELLA-SUBPROGRAM.                                                
012600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012900     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
013000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013100     SKIP3                                                                
013200 01  GEMENSAMMA-SUBPROGRAM.                                               
013300     03  W411OHFK                PIC X(8)    VALUE 'W411OHFK'.            
013400*        FORMELLA KONTROLLER                                              
013500     03  W411KREG                PIC X(8)    VALUE 'W411KREG'.            
013600*        LÄSNING AV KUNDREGISTRET                                         
013700     03  W411OHLK                PIC X(8)    VALUE 'W411OHLK'.            
013800*        LOGISKA KONTROLLER                                               
013900     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
014000*        KONTROLL OCH UTTAG AV AUTOMATISKT ORDERNUMMER                    
014100     03  W411TRAN                PIC X(8)    VALUE 'W411TRAN'.            
014200*        BESTÄM TRANSPORT                                                 
014300     03  W411OHKK                PIC X(8)    VALUE 'W411OHKK'.            
014400*        KOSOLIDERINGSKONTROLL                                            
014500     EJECT                                                                
014600*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
014700*   -COPY WSECAREA                                                        
014800     EJECT                                                                
014900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015000*   -COPY WMSGINIT                                                        
015100     EJECT                                                                
015200*   -COPY W402W001                                                        
015300     EJECT                                                                
015400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015500*   -COPY WMEDAREA                                                        
015600     EJECT                                                                
015700 01  MESSAGE-CODES.                                                       
015800     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
015900     03  ERR-ORDER-FINNS         PIC X(3)    VALUE '065'.                 
016000     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
016100     03  ERR-SAKNAS-KREG         PIC X(3)    VALUE '063'.                 
016200     03  ERR-TRANSPORT-FEL       PIC X(3)    VALUE '087'.                 
016300     03  ERR-KAMPANJ-FEL         PIC X(3)    VALUE '157'.                 
016400     03  ERR-KUND-SPAERRAD       PIC X(3)    VALUE '213'.                 
016500     03  ERR-MOMS-REGNR-FEL      PIC X(3)    VALUE '223'.                 
016600     03  MED-ORDER-ANNULLERAD    PIC X(3)    VALUE '052'.                 
016700     03  MED-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
016800     03  ERR-SAKNAS-BET          PIC X(3)    VALUE '145'.                 
016900     EJECT                                                                
017000*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
017100 01 FILLER                       PIC X(8)    VALUE 'W411OHFK'.            
017200*   -COPY W411OHFK                                                        
017300     EJECT                                                                
017400 01 FILLER                       PIC X(8)    VALUE 'W411KREG'.            
017500*   -COPY W411KREG                                                        
017600     EJECT                                                                
017700 01 FILLER                       PIC X(8)    VALUE 'W411OHLK'.            
017800*   -COPY W411OHLK                                                        
017900     EJECT                                                                
018000 01 FILLER                       PIC X(8)    VALUE 'W411ORDN'.            
018100*   -COPY W411ORDN                                                        
018200     EJECT                                                                
018300 01 FILLER                       PIC X(8)    VALUE 'W411TRAN'.            
018400*   -COPY W411TRAN                                                        
018500     EJECT                                                                
018600 01 FILLER                       PIC X(8)    VALUE 'W411OHKK'.            
018700*   -COPY W411OHKK                                                        
018800     EJECT                                                                
018900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019100     SKIP3                                                                
019200*01  MID -COPY W4I24101                                                   
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019500     SKIP3                                                                
019600*01  -COPY WMSGAREA                                                       
019700     EJECT                                                                
019800*    03  MOD -COPY W4O24101 -RED MSG-AREA.                                
019900     EJECT                                                                
020000*    03  -COPY W4O24201 -PRE 4242- -RED MSG-AREA.                         
020100     EJECT                                                                
020200*    03  -COPY W4O20601 -PRE 4206- -RED MSG-AREA.                         
020300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020400     SKIP3                                                                
020500*01  -COPY WMFSAREA                                                       
020600     EJECT                                                                
020700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020900                                                                          
021000 01  NYCKLAR-TILL-DLI.                                                    
021100     03  W-WDQ2CSEQ-X.                                                    
021200         05  W-IDDISTR           PIC S9(5)   VALUE +0 COMP-3.             
021300         05  W-IDKUNDNR          PIC S9(7)   VALUE +0 COMP-3.             
021400         05  W-IDKUNDRF.                                                  
021500           07  W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
021600           07  FILLER            PIC X(3)    VALUE SPACE.                 
021700                                                                          
021800     03  W-IDORDER-X.                                                     
021900         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
022000                                                                          
022100     03  W-IDDC-X.                                                        
022200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
022300                                                                          
022400*                                                                         
022500     03  W-IDGMT-X.                                                       
022600         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
022700         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
022800*                                                                         
022900     03  W-IDGMT-MIN-X.                                                   
023000         05  W-IDDISTR-WDB2-MIN  PIC S9(5) VALUE ZERO COMP-3.             
023100         05  W-IDKUNDNR-WDB2-MIN PIC S9(7) VALUE ZERO COMP-3.             
023200*                                                                         
023300     03  W-IDGMT-MAX-X.                                                   
023400         05  W-IDDISTR-WDB2-MAX  PIC S9(5) VALUE ZERO COMP-3.             
023500         05  W-IDKUNDNR-WDB2-MAX PIC S9(7) VALUE ZERO COMP-3.             
023600                                                                          
023820     03  W-WDB301KY-X.                                                    
023850         05  W-IDDC-WDB3         PIC X(2).                                
023860         05  W-IDDISTR-WDB3      PIC S9(5)   COMP-3.                      
023870         05  W-IDKUNDNR-WDB3     PIC S9(7)   COMP-3.                      
023880                                                                          
023890     03  W-WDB301KY-DEF-X.                                                
023891         05  W-IDDC-WDB3-DEF     PIC X(2).                                
023892         05  W-IDDISTR-WDB3-DEF  PIC S9(5)   COMP-3.                      
023893         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999 COMP-3.         
023894                                                                          
023900*    --- STATUS-KOD FRÅN IMS                                              
024000 01  STATUS-WS                   PIC XX.                                  
024100     88  SEGMENT-FINNS                       VALUE '  '.                  
024200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024400     SKIP2                                                                
024500 01  GODK-STATUSKODER.                                                    
024600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024700     SKIP3                                                                
024800 01  SSA1                        PIC X(64).                               
024900 01  SSA2                        PIC X(64).                               
025000     EJECT                                                                
025100*    --- IMS FUNKTIONSKODER                                               
025200*01  -COPY W0003                                                          
025300     EJECT                                                                
025400*    ---  DLI INPUT-OUTPUT AREA                                           
025500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
025600     SKIP3                                                                
025700 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
025800 01  DLI-IO-AREA-OHUV.                                                    
025900     03  WLORQI01.                                                        
026000*        05  -COPY WDQ201                                                 
026100     EJECT                                                                
026200 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
026300 01  DLI-IO-AREA-ARB.                                                     
026400     03  WLORQI12.                                                        
026500*        05  -COPY WDQ212                                                 
026600     EJECT                                                                
026700 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
026800 01  DLI-IO-AREA-WDB201.                                                  
026900     03  WLGMTA01.                                                        
027000*        05  -COPY WDB201                                                 
027100     EJECT                                                                
027610 01  FILLER                      PIC X(16)   VALUE 'WDB301-AREA'.         
027620 01  DLI-IO-AREA-WDB301.                                                  
027640     03  -COPY WDB301                                                     
027650     EJECT                                                                
027700 LINKAGE SECTION.                                                         
027800                                                                          
027900*01  -COPY W0009      -PRE MSG-                                           
028000 01  USEA-PCB                    PIC X.                                   
028100     SKIP2                                                                
028200*01  -COPY W0008      -PRE ORQL-                                          
028300     05  FILLER                  PIC X.                                   
028400     EJECT                                                                
028500*01  -COPY W0008      -PRE ORQI-                                          
028600     05  FILLER                  PIC X.                                   
028700     EJECT                                                                
028800*01  -COPY W0008      -PRE WDB2-                                          
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029010*01  -COPY W0008      -PRE WDB3-                                          
029020     05  FILLER                  PIC X.                                   
029030     EJECT                                                                
029100 01  KREG-GMTA-PCB               PIC X.                                   
029200 01  KREG-GMTB-PCB               PIC X.                                   
029300 01  KREG-GMTC-PCB               PIC X.                                   
029400 01  KREG-BETC-PCB               PIC X.                                   
029500 01  OHLK-WDM2-PCB               PIC X.                                   
029600 01  OHLK-WDB6-PCB               PIC X.                                   
029700 01  ORDN-XXKP-PCB               PIC X.                                   
029800 01  ORDN-ORQL-PCB               PIC X.                                   
029900 01  ORDN-PROC-PCB               PIC X.                                   
030000 01  ORDN-ORQI-PCB               PIC X.                                   
030100 01  TRAN-XXKB-PCB               PIC X.                                   
030200 01  SAP-SAPC-PCB                PIC X.                                   
030300                                                                          
030400 01  OHKK-WDQ2-PCB               PIC X.                                   
030500 01  OHKK-WDQ2-UPD-PCB           PIC X.                                   
030510 01  OHKK-WDQ2C-PCB              PIC X.                                   
030600 01  OHKK-GMTA-PCB               PIC X.                                   
030700 01  OHKK-GMTB-PCB               PIC X.                                   
030800 01  OHKK-GMTC-PCB               PIC X.                                   
030900 01  OHKK-BETC-PCB               PIC X.                                   
031000 01  OHKK-WDB2-PCB               PIC X.                                   
031100 01  OHKK-WDB3-PCB               PIC X.                                   
031200 01  OHKK-WDB5-PCB               PIC X.                                   
031300 01  OHKK-WDP7-PCB               PIC X.                                   
031400 01  OHKK-XXKB-PCB               PIC X.                                   
031500     EJECT                                                                
031600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ORQL-PCB ORQI-PCB             
031700                     WDB2-PCB WDB3-PCB                                    
031800                     KREG-GMTA-PCB KREG-GMTB-PCB                          
031900                     KREG-GMTC-PCB KREG-BETC-PCB                          
032000                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
032100                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
032200                     ORDN-ORQI-PCB                                        
032300                     TRAN-XXKB-PCB                                        
032400                     SAP-SAPC-PCB                                         
032500                     OHKK-WDQ2-PCB OHKK-WDQ2-UPD-PCB                      
032510                     OHKK-WDQ2C-PCB                                       
032600                     OHKK-GMTA-PCB OHKK-GMTB-PCB                          
032700                     OHKK-GMTC-PCB OHKK-BETC-PCB OHKK-WDB2-PCB            
032800                     OHKK-WDB3-PCB OHKK-WDB5-PCB OHKK-WDP7-PCB            
032900                     OHKK-XXKB-PCB.                                       
033000 MAIN SECTION.                                                            
033100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ORQL-PCB ORQI-PCB             
033200                     WDB2-PCB WDB3-PCB                                    
033300                     KREG-GMTA-PCB KREG-GMTB-PCB                          
033400                     KREG-GMTC-PCB KREG-BETC-PCB                          
033500                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
033600                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
033700                     ORDN-ORQI-PCB                                        
033800                     TRAN-XXKB-PCB                                        
033900                     SAP-SAPC-PCB                                         
034000                     OHKK-WDQ2-PCB OHKK-WDQ2-UPD-PCB                      
034010                     OHKK-WDQ2C-PCB                                       
034100                     OHKK-GMTA-PCB OHKK-GMTB-PCB                          
034200                     OHKK-GMTC-PCB OHKK-BETC-PCB OHKK-WDB2-PCB            
034300                     OHKK-WDB3-PCB OHKK-WDB5-PCB OHKK-WDP7-PCB            
034400                     OHKK-XXKB-PCB.                                       
034500                                                                          
034600     EJECT                                                                
034700     PERFORM IMS-GET-MSG                                                  
034800     IF SEGMENT-FINNS                                                     
034900        PERFORM A-INIT                                                    
035000        IF ALLT-OK                                                        
035100           PERFORM B-KOLLA-O-KOMPLETTERA-INDATA                           
035200           IF ALLT-OK                                                     
035300              IF OHKK-IDORDER > ZERO                                      
035400                 PERFORM S02-HOPPA-TILL-TILLAGGSREG                       
035500              ELSE                                                        
035600                 PERFORM C-BESTAM-ORDERNUMMER                             
035700                 PERFORM D-SKAPA-ORDERHUVUD                               
035800                 IF ALLT-OK                                               
035900                    PERFORM S01-HOPPA-TILL-RADREGISTRERING                
036000                 END-IF                                                   
036100              END-IF                                                      
036200           END-IF                                                         
036300        END-IF                                                            
036400        IF HOPP = NEJ                                                     
036500           PERFORM Z-FINIT                                                
036600           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
036700           PERFORM IMS-INSERT-MSG                                         
036800        END-IF                                                            
036900     END-IF                                                               
037000                                                                          
037100     MOVE +0 TO RETURN-CODE                                               
037200     GOBACK                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 A-INIT SECTION.                                                          
037600                                                                          
037700     MOVE SPACE                TO MED-IDMFSFEL                            
037800     MOVE JA                   TO ALLT-SW                                 
037900     MOVE NEJ                  TO HOPP                                    
038000                                                                          
038100     IF MSG-DUBBLA-TRANSKODER                                             
038200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I24101                 
038300       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
038400       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
038500     ELSE                                                                 
038600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I24101                  
038700       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
038800       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
038900     END-IF                                                               
039000                                                                          
039100     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
039200     MOVE MSG-IDPFK            TO MFS-IDPFK                               
039300     MOVE MFS-IDTRANS          TO W-IDTRANS                               
039400                                                                          
039500     MOVE LOW-VALUE            TO MSG-AREA                                
039600     MOVE 'W4O24101'           TO MFS-IDMOD                               
039700     MOVE '4241'               TO MOD-IDTRANS                             
039800     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
039900                                                                          
040000     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O24101 + 4                  
040100     IF ENGLISH-TEXT                                                      
040200       MOVE 'GB '              TO MED-IDSKYLT                             
040300     ELSE                                                                 
040400       MOVE 'S  '              TO MED-IDSKYLT                             
040500     END-IF                                                               
040600     EJECT                                                                
040700     IF NOT EGEN-MID                                                      
040800        MOVE NEJ               TO ALLT-SW                                 
040900        PERFORM MFS-RENSA-BILD                                            
041000     ELSE                                                                 
041100        PERFORM AA-KOLLA-BEHORIGHET                                       
041200     END-IF                                                               
041210*    MOVE 'N'                  TO MOD-FLVORFK                             
041300     .                                                                    
041400     EJECT                                                                
041500                                                                          
041600 AA-KOLLA-BEHORIGHET SECTION.                                             
041700                                                                          
041800     IF MID-IDDISTR NUMERIC AND MID-IDDISTR > ZERO                        
041900        MOVE MSG-SIGNON-USERID    TO SEC-IDUSER                           
042000        MOVE '4241'               TO SEC-IDTRANS                          
042100        MOVE MID-IDDISTR          TO SEC-IDKEY                            
042200                                                                          
042300        CALL WSECURIT USING SEC-IDUSER                                    
042400                            SEC-IDTRANS                                   
042500                            SEC-IDKEY                                     
042600                            SEC-KDSVAR                                    
042700                                                                          
042800        IF SEC-KDSVAR = OBEHORIG                                          
042900           MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR                     
043000           MOVE ERR-OBEHORIG      TO MED-IDMFSFEL                         
043100           MOVE NEJ               TO ALLT-SW                              
043200        END-IF                                                            
043300     ELSE                                                                 
043400        MOVE OBEHORIG             TO SEC-KDSVAR                           
043500     END-IF                                                               
043600     .                                                                    
043700     EJECT                                                                
043800                                                                          
043900                                                                          
044000 B-KOLLA-O-KOMPLETTERA-INDATA SECTION.                                    
044100                                                                          
044200     PERFORM BA-KONTROLLERA-FORMELLA-FEL                                  
044300     IF ALLT-OK                                                           
044400                                                                          
044500        PERFORM BB-KONTROLLERA-OM-ORDER-FINNS                             
044600        IF ALLT-OK                                                        
044700                                                                          
044800           PERFORM BC-LAS-KUNDREGISTRET                                   
044900           IF ALLT-OK                                                     
045000                                                                          
045100              PERFORM BD-KONTROLLERA-LOGISKA-FEL                          
045200              IF ALLT-OK                                                  
045300                                                                          
045400                PERFORM BE-BESTAM-TRANSPORT                               
045500                 IF ALLT-OK                                               
045600                                                                          
045700                    PERFORM BF-KONTROLLERA-KOSOLIDERING                   
045800                 END-IF                                                   
045900              END-IF                                                      
046000           END-IF                                                         
046100        END-IF                                                            
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500                                                                          
046600 BA-KONTROLLERA-FORMELLA-FEL SECTION.                                     
046700                                                                          
046800     MOVE '4241'               TO OHFK-IDSYSTEM                           
046900     MOVE MID-IDDISTR          TO OHFK-IDDISTR                            
047000     MOVE MID-IDKUNDNR         TO OHFK-IDKUNDNR                           
047100     MOVE MID-IDORDNR          TO OHFK-IDORDNR                            
047200     MOVE MID-KDORDKL          TO OHFK-KDORDKL                            
047300     MOVE MID-KDFRAKT          TO OHFK-KDFRAKT                            
047310     MOVE MID-FLVORFK          TO MOD-FLVORFK                             
047400     MOVE NEJ                  TO OHFK-FLFORBI                            
047500     MOVE NEJ                  TO OHFK-FLORDSPE                           
047600     MOVE JA                   TO OHFK-FLLSBOK                            
047700     MOVE NEJ                  TO OHFK-FLAUTPAC                           
047800     MOVE NEJ                  TO OHFK-FLVORKO                            
047900     MOVE MID-FLRESTN          TO OHFK-FLRESTN                            
048000     IF MID-FLRESTN = 'Y'                                                 
048100        MOVE 'J'               TO MID-FLRESTN                             
048200     END-IF                                                               
048300     IF MID-FLRESTN = '+' AND MID-KDORDKL = '1'                           
048400        MOVE 'J'               TO MID-FLRESTN                             
048500     END-IF                                                               
048510     IF MID-FLVORFK = 'Y'                                                 
048520        MOVE 'J'               TO MID-FLVORFK                             
048530     END-IF                                                               
048600     MOVE MID-IDBIPREF         TO OHFK-IDBIPREF                           
048700     MOVE MID-IDKAMPRF         TO OHFK-IDKAMPRF                           
048800     MOVE MID-KDROPACK         TO OHFK-KDROPACK                           
048900     MOVE MID-KDTPOTYP         TO OHFK-KDTPOTYP                           
049000     MOVE MID-TITPO            TO OHFK-TITPO                              
049100     ACCEPT OHFK-TIREGDAT      FROM DATE                                  
049200     ACCEPT OHFK-TIHHMM        FROM TIME                                  
049300     MOVE ALL '+'              TO OHFK-IDDC                               
049400     MOVE ALL '+'              TO OHFK-FLAUTFAK                           
049500     MOVE ALL '+'              TO OHFK-IDFTG                              
049600     MOVE ALL '+'              TO OHFK-IDKONTO                            
049700     MOVE ALL '+'              TO OHFK-IDANALYS                           
049800     MOVE ALL '+'              TO OHFK-IDKST                              
049900     MOVE ALL '+'              TO OHFK-KDFAKTYP                           
050000     MOVE MID-TIRFS-DAT        TO OHFK-TIRFSDAT                           
050100     MOVE MID-TIRFS-TID        TO OHFK-TIRFSTID                           
050200     MOVE ALL '+'              TO OHFK-IDSKYLT                            
050300     MOVE ALL '+'              TO OHFK-KDTULLVE                           
050400     MOVE ALL '+'              TO OHFK-KDVRINFO                           
050500     MOVE ALL '+'              TO OHFK-TIFORDAT                           
050600     MOVE ALL '+'              TO OHFK-KDPROTYP                           
050700                                                                          
050800     CALL W411OHFK USING OHFK-W411OHFK                                    
050900     PERFORM BAA-KOLLA-FEL-FK                                             
051000     .                                                                    
051100     EJECT                                                                
051200 BAA-KOLLA-FEL-FK SECTION.                                                
051300                                                                          
051400     IF OHFK-IDDISTR-OK = NEJ                                             
051500        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
051600        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
051700        MOVE NEJ                 TO ALLT-SW                               
051800     ELSE                                                                 
051900        MOVE OHFK-IDDISTR        TO W-IDDISTR                             
052000                                    TEST-IDDISTR                          
052100     END-IF                                                               
052200                                                                          
052211     IF DIST79-ECOM-PRICE                                                 
052230        MOVE ERR-UPPLYSTA-FEL   TO MED-IDMFSFEL                           
052240        MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-ATTR                       
052250        MOVE NEJ                TO ALLT-SW                                
052260     END-IF                                                               
052270                                                                          
052300     IF DIST79-DEALER-PRICE                                               
052400        IF ENGLISH-TEXT                                                   
052500           MOVE 'DEALERPRICE'    TO MOD-TEDDI                             
052600                                    WS-TEDDI                              
052700        ELSE                                                              
052800           MOVE '    ÅF PRIS'    TO MOD-TEDDI                             
052900                                    WS-TEDDI                              
053000        END-IF                                                            
053100     ELSE                                                                 
053200        MOVE SPACES              TO MOD-TEDDI                             
053300                                    WS-TEDDI                              
053400     END-IF                                                               
053500                                                                          
053600     IF OHFK-IDKUNDNR-OK = NEJ                                            
053700        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
053800        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR                     
053900        MOVE NEJ                 TO ALLT-SW                               
054000     ELSE                                                                 
054100        IF MID-IDKUNDNR = ALL '+'                                         
054200           MOVE ZERO             TO MOD-IDKUNDNR                          
054300                                    W-IDKUNDNR                            
054400        ELSE                                                              
054500           MOVE OHFK-IDKUNDNR    TO W-IDKUNDNR                            
054600        END-IF                                                            
054700     END-IF                                                               
054800                                                                          
054900     IF OHFK-IDORDNR-OK = NEJ                                             
055000        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
055100        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDORDNR-ATTR                      
055200        MOVE NEJ                 TO ALLT-SW                               
055300     ELSE                                                                 
055400        IF MID-IDORDNR = ALL '+'                                          
055500           MOVE ZERO             TO W-IDORDNR                             
055600           MOVE MFS-RENSA-FAELT  TO MOD-IDORDNR                           
055700        ELSE                                                              
055800           MOVE OHFK-IDORDNR     TO W-IDORDNR                             
055900        END-IF                                                            
056000     END-IF                                                               
056100     EJECT                                                                
056200                                                                          
056300     IF OHFK-KDORDKL-OK = NEJ                                             
056400        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
056500        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDORDKL-ATTR                      
056600        MOVE NEJ                 TO ALLT-SW                               
056700     END-IF                                                               
056800                                                                          
056900     IF OHFK-KDFRAKT-OK = NEJ                                             
057000        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
057100        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFRAKT-ATTR                      
057200        MOVE NEJ                 TO ALLT-SW                               
057300     END-IF                                                               
057400                                                                          
057500     IF OHFK-FLRESTN-OK = NEJ                                             
057600        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
057700        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLRESTN-ATTR                      
057800        MOVE NEJ                 TO ALLT-SW                               
057900     END-IF                                                               
058000                                                                          
058100     IF OHFK-IDBIPREF-OK = NEJ                                            
058200        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
058300        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDBIPREF-ATTR                     
058400        MOVE NEJ                 TO ALLT-SW                               
058500     END-IF                                                               
058600                                                                          
058700     IF OHFK-IDKAMPRF-OK = NEJ                                            
058800        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
058900        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKAMPRF-ATTR                     
059000        MOVE NEJ                 TO ALLT-SW                               
059100     END-IF                                                               
059200     EJECT                                                                
059300                                                                          
059400     IF OHFK-KDROPACK-OK = NEJ                                            
059500        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
059600        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDROPACK-ATTR                     
059700        MOVE NEJ                 TO ALLT-SW                               
059800     END-IF                                                               
059900                                                                          
060000     IF OHFK-KDTPOTYP-OK = NEJ                                            
060100        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
060200        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDTPOTYP-ATTR                     
060300        MOVE NEJ                 TO ALLT-SW                               
060400     END-IF                                                               
060500                                                                          
060600     IF OHFK-TITPO-OK = NEJ                                               
060700        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
060800        MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-ATTR                        
060900        MOVE NEJ                 TO ALLT-SW                               
061000     END-IF                                                               
061100                                                                          
061200     IF OHFK-TIRFSDAT-OK = NEJ                                            
061300        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
061400        MOVE MFS-NUM-FAELT-FEL   TO MOD-TIRFS-DAT-ATTR                    
061500        MOVE NEJ                 TO ALLT-SW                               
061600     END-IF                                                               
061700                                                                          
061800     IF OHFK-TIRFSTID-OK = NEJ                                            
061900        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
062000        MOVE MFS-NUM-FAELT-FEL   TO MOD-TIRFS-TID-ATTR                    
062100        MOVE NEJ                 TO ALLT-SW                               
062200     END-IF                                                               
062210                                                                          
062220     IF MID-FLVORFK = ALL '+' OR JA OR NEJ                                
062221       IF  MID-FLVORFK = JA                                               
062223         IF MID-KDORDKL = 0 AND DIST15-FLVORFK                            
062224           CONTINUE                                                       
062225         ELSE                                                             
062226           MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                       
062227           MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLVORFK-ATTR                   
062228           MOVE NEJ                 TO ALLT-SW                            
062229         END-IF                                                           
062230       END-IF                                                             
062234     ELSE                                                                 
062235        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
062240        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLVORFK-ATTR                      
062250        MOVE NEJ                 TO ALLT-SW                               
062260     END-IF                                                               
062300     .                                                                    
062400     EJECT                                                                
062500 BB-KONTROLLERA-OM-ORDER-FINNS SECTION.                                   
062600                                                                          
062700     IF MID-IDORDNR NOT = ALL '+'                                         
062800                                                                          
062900        PERFORM IMS-GU-ORQL-WDQ201                                        
063000                                                                          
063100        IF SEGMENT-FINNS                                                  
063200           IF MID-KDTPOTYP = '1' AND OHUV-KDTPOTYP = +1                   
063300                                                                          
063400              MOVE NEJ            TO OHUV-FLKLAR                          
063500              MOVE OHUV-IDORDER   TO W-IDORDER                            
063600              PERFORM IMS-GHU-ORQI-WDQ201                                 
063700              PERFORM IMS-REPL-ORQI-WDQ201                                
063800                                                                          
063900              MOVE WC-CDC-SE      TO W-IDDC                               
064000              PERFORM IMS-GNP-ORQI-WDQ212                                 
064100                                                                          
064200              PERFORM S01-HOPPA-TILL-RADREGISTRERING                      
064300              MOVE NEJ            TO ALLT-SW                              
064400           ELSE                                                           
064500              MOVE MFS-NUM-FAELT-FEL TO MOD-IDORDNR-ATTR                  
064600              MOVE ERR-ORDER-FINNS   TO MED-IDMFSFEL                      
064700              MOVE NEJ               TO ALLT-SW                           
064800           END-IF                                                         
064900        END-IF                                                            
065000     END-IF                                                               
065100     .                                                                    
065200     EJECT                                                                
065300 BC-LAS-KUNDREGISTRET SECTION.                                            
065400                                                                          
065500     MOVE W-IDDISTR            TO KREG-IDDISTR                            
065600     MOVE W-IDKUNDNR           TO KREG-IDKUNDNR                           
065700     MOVE 'IMS '               TO KREG-IDSYSTEM                           
065800     IF MID-KDTPOTYP > ZERO OR MID-IDKAMPRF > ZERO                        
065900        MOVE WC-CDC-SE         TO KREG-IDDC-TVS                           
066000     ELSE                                                                 
066100        MOVE SPACE             TO KREG-IDDC-TVS                           
066200     END-IF                                                               
066300     IF MID-KDFRAKT = ALL '+'                                             
066400        MOVE +0                TO KREG-KDFRAKT-IN                         
066500     ELSE                                                                 
066600        MOVE MID-KDFRAKT       TO WS-ALFA-2                               
066700        MOVE WS-NUM-2          TO KREG-KDFRAKT-IN                         
066800     END-IF                                                               
066900     MOVE MID-KDORDKL          TO KREG-KDORDKL                            
067000     MOVE SPACE                TO KREG-KDFAKTYP-IN                        
067100     MOVE NEJ                  TO KREG-FLVORKO                            
117410     IF (MID-FLVORFK = JA OR YES)                                         
117420       AND MID-KDORDKL = 0 AND DIST15-FLVORFK                             
117431       MOVE JA                 TO KREG-FLVORFK                            
117440     ELSE                                                                 
117450       MOVE NEJ                TO KREG-FLVORFK                            
117451     END-IF                                                               
117452                                                                          
117453     CALL W411KREG USING KREG-W411KREG KREG-GMTA-PCB KREG-GMTB-PCB        
117454                                       KREG-GMTC-PCB KREG-BETC-PCB        
117455                                                                          
117456     IF KREG-KDKREDSP = '1'                                               
117457        MOVE ERR-KUND-SPAERRAD TO MED-IDMFSFEL                            
117458        MOVE NEJ                 TO ALLT-SW                               
117459     ELSE                                                                 
117460        IF KREG-IDVAT-OK = NEJ                                            
117461          MOVE ERR-MOMS-REGNR-FEL TO MED-IDMFSFEL                         
117462          MOVE NEJ                TO ALLT-SW                              
117463        ELSE                                                              
117464          IF KREG-IDPARTNR-OK = NEJ                                       
117465            MOVE ERR-SAKNAS-BET      TO MED-IDMFSFEL                      
117466            MOVE NEJ                 TO ALLT-SW                           
117467          END-IF                                                          
117468        END-IF                                                            
117469     END-IF                                                               
117470                                                                          
117471     PERFORM BCA-KOLLA-FEL-KREG                                           
117472     IF KREG-KDTRPKAT = 'C'                                               
117473        MOVE ZERO             TO KREG-IDTRP                               
117474     END-IF                                                               
117475     .                                                                    
117476     EJECT                                                                
117477 BCA-KOLLA-FEL-KREG SECTION.                                              
117478                                                                          
117479     IF KREG-IDDISTR-OK = NEJ                                             
117480        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
117481        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
117482        MOVE NEJ                 TO ALLT-SW                               
117483     END-IF                                                               
117484                                                                          
117485     IF KREG-IDKUNDNR-OK = NEJ                                            
117486        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
117487        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR                     
117488        MOVE NEJ                 TO ALLT-SW                               
117489     END-IF                                                               
117490                                                                          
117491     IF KREG-KDFRAKT-OK = NEJ                                             
117492        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
117493        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFRAKT-ATTR                      
117494        MOVE NEJ                 TO ALLT-SW                               
117495     END-IF                                                               
117496     .                                                                    
117497     EJECT                                                                
117498 BD-KONTROLLERA-LOGISKA-FEL SECTION.                                      
117499                                                                          
117500     MOVE 'IMS '               TO OHLK-IDSYSTEM                           
117501     MOVE W-IDDISTR            TO OHLK-IDDISTR                            
117502     MOVE W-IDKUNDNR           TO OHLK-IDKUNDNR                           
117503     MOVE W-IDORDNR            TO OHLK-IDORDNR                            
117504     MOVE MID-KDORDKL          TO WS-ALFA-1                               
117505     MOVE WS-NUM-1             TO OHLK-KDORDKL                            
117506     MOVE KREG-IDDC            TO OHLK-IDDC                               
117507     MOVE SPACE                TO OHLK-IDDC-TVS                           
117508     MOVE SEC-KDSVAR           TO OHLK-SEC-KDSVAR                         
117509     MOVE KREG-FLAUTORD        TO OHLK-FLAUTORD                           
117510     MOVE +0                   TO OHLK-IDKONTO                            
117511     MOVE SPACE                TO OHLK-IDANALYS                           
117512                                  OHLK-IDKST                              
117513     MOVE ZERO                 TO OHLK-IDFTG                              
117514     IF MID-IDKAMPRF = ALL '+'                                            
117515        MOVE +0                TO OHLK-IDKAMPRF                           
117516     ELSE                                                                 
117517        MOVE MID-IDKAMPRF      TO WS-ALFA-7                               
117518        MOVE WS-NUM-7          TO OHLK-IDKAMPRF                           
117519     END-IF                                                               
117520     MOVE KREG-FLOKFAK-G       TO OHLK-FLOKFAK-G                          
117521     MOVE KREG-FLOKFAK-N       TO OHLK-FLOKFAK-N                          
117522     MOVE KREG-FLOKFAK-R       TO OHLK-FLOKFAK-R                          
117523     MOVE KREG-FLOKFAK-K       TO OHLK-FLOKFAK-K                          
117524     MOVE KREG-KDGENFAK        TO OHLK-KDFAKTYP                           
117525     EJECT                                                                
117526     IF MID-KDTPOTYP = ALL '+'                                            
117527        MOVE +0                TO OHLK-KDTPOTYP                           
117528     ELSE                                                                 
117529        MOVE MID-KDTPOTYP      TO WS-ALFA-1                               
117530        MOVE WS-NUM-1          TO OHLK-KDTPOTYP                           
117531     END-IF                                                               
117532     IF MID-TITPO = ALL '+'                                               
117533        MOVE +0                TO OHLK-TITPO                              
117534     ELSE                                                                 
117535        MOVE MID-TITPO         TO WS-ALFA-6                               
117536        MOVE WS-NUM-6          TO OHLK-TITPO                              
117537     END-IF                                                               
117538                                                                          
117539     CALL W411OHLK USING OHLK-W411OHLK OHLK-WDM2-PCB ORDN-XXKP-PCB        
117540                                       KREG-GMTA-PCB                      
117541                                       SAP-SAPC-PCB                       
117542                                       OHLK-WDB6-PCB                      
117543                                                                          
117544     PERFORM BDA-KOLLA-FEL-LK                                             
117545     .                                                                    
117546     EJECT                                                                
117547 BDA-KOLLA-FEL-LK SECTION.                                                
117548                                                                          
117549     IF OHLK-IDDISTR-OK = NEJ                                             
117550        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
117551        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
117552        MOVE NEJ                 TO ALLT-SW                               
117553     END-IF                                                               
117554     IF OHLK-IDORDNR-OK = NEJ                                             
117555        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
117556        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDORDNR-ATTR                      
117557        MOVE NEJ                 TO ALLT-SW                               
117558     END-IF                                                               
117559     IF OHLK-KDORDKL-OK = NEJ                                             
117560        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
117561        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDORDKL-ATTR                      
117562        MOVE NEJ                 TO ALLT-SW                               
117563     END-IF                                                               
117564     IF OHLK-KDTPOTYP-OK = NEJ                                            
117565        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
117566        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDTPOTYP-ATTR                     
117567        MOVE NEJ                 TO ALLT-SW                               
117568     END-IF                                                               
117569     IF OHLK-TITPO-OK = NEJ                                               
117570        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
117571        MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-ATTR                        
117572        MOVE NEJ                 TO ALLT-SW                               
117573     END-IF                                                               
117574     IF OHLK-IDKAMPRF-OK = NEJ                                            
117575        MOVE ERR-KAMPANJ-FEL  TO MED-IDMFSFEL                             
117576        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKAMPRF-ATTR                     
117577        MOVE NEJ                 TO ALLT-SW                               
117578     END-IF                                                               
117579     .                                                                    
117580     EJECT                                                                
117581 BE-BESTAM-TRANSPORT SECTION.                                             
117582                                                                          
117583     MOVE 'IMS '               TO TRAN-IDSYSTEM                           
117584     MOVE KREG-IDTRP           TO TRAN-IDTRP                              
117585     MOVE OHLK-IDDC            TO TRAN-IDDC                               
117586     MOVE OHLK-KDORDKL         TO TRAN-KDORDKL                            
117587     MOVE KREG-KDTRPKAT        TO TRAN-KDTRPKAT                           
117588     MOVE KREG-KVLEDTIM-0      TO TRAN-KVLEDTIM-0                         
117589     MOVE KREG-KVLEDTIM-1      TO TRAN-KVLEDTIM-1                         
117590     MOVE KREG-KVLEDTIM-2      TO TRAN-KVLEDTIM-2                         
117591     MOVE KREG-KVLEDTIM-3      TO TRAN-KVLEDTIM-3                         
117592     MOVE KREG-KVLEDTIM-4      TO TRAN-KVLEDTIM-4                         
117593     MOVE NEJ                  TO TRAN-FLORDSPE                           
117594     MOVE NEJ                  TO TRAN-FLOVRLEV                           
117595     PERFORM BEA-FIXA-LOKAL-TID                                           
117596     MOVE MSGI-TILOKDAT        TO TRAN-TIREGDAT                           
117597     MOVE MSGI-TILOKTID        TO TRAN-TIHHMM-REG                         
117598     IF MID-TIRFS-DAT = ALL '+'                                           
117599        MOVE +0                TO TRAN-TIRFS                              
117600     ELSE                                                                 
117601        MOVE OHFK-TIRFSDAT     TO WS-DATUM                                
117602        MOVE OHFK-TIRFSTID     TO WS-TID                                  
117603        MOVE WS-DATUM-TID      TO TRAN-TIRFS                              
117604     END-IF                                                               
117605     IF OHLK-KDTPOTYP = +2                                                
117606        MOVE +0                TO TRAN-KDTPOTYP                           
117607     ELSE                                                                 
117608        MOVE OHLK-KDTPOTYP     TO TRAN-KDTPOTYP                           
117609     END-IF                                                               
117611                                                                          
117612     CALL W411TRAN USING TRAN-W411TRAN TRAN-XXKB-PCB                      
117613                                                                          
117614     IF TRAN-KDSVAR = '1'                                                 
117615        MOVE ERR-TRANSPORT-FEL     TO MED-IDMFSFEL                        
117616        MOVE NEJ                   TO ALLT-SW                             
117617     ELSE                                                                 
117618       IF TRAN-KDSVAR = '2' OR '3' OR '4'                                 
117619          MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                        
117620          MOVE MFS-NUM-FAELT-FEL   TO MOD-TIRFS-DAT-ATTR                  
117621                                      MOD-TIRFS-TID-ATTR                  
117622          MOVE NEJ                 TO ALLT-SW                             
117623       END-IF                                                             
117624     END-IF                                                               
117625     .                                                                    
117626     EJECT                                                                
117627 BEA-FIXA-LOKAL-TID SECTION.                                              
117628                                                                          
117629     MOVE ALL '+'            TO MSGI-WMSGINIT                             
117630     MOVE '001'              TO MSGI-KDCALL                               
117631     MOVE 'WIDDC   '         TO MSGI-IDUSER                               
117632     MOVE KREG-IDDC          TO MSGI-IDUSER(6:2)                          
117633     MOVE '4241'             TO MSGI-IDTRANS                              
117634     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
117635                                                                          
117636     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
117637     .                                                                    
117638     EJECT                                                                
117639 BF-KONTROLLERA-KOSOLIDERING SECTION.                                     
117640                                                                          
117641     IF MID-ADGMT         =  ALL '+' AND                                  
117642        MID-BEGMT         =  ALL '+'                                      
117646       MOVE 1                  TO OHKK-KDCALL                             
117647       MOVE 'IMS'              TO OHKK-IDSYSTEM                           
117648       MOVE W-IDDISTR          TO OHKK-IDDISTR                            
117649                                    TEST-IDDISTR                          
117650       MOVE W-IDKUNDNR         TO OHKK-IDKUNDNR                           
117651       MOVE ZERO               TO OHKK-IDORDNR-IN                         
117652       MOVE OHLK-KDORDKL       TO OHKK-KDORDKL                            
117653                                                                          
117654       MOVE KREG-ADBETRAD-1    TO OHKK-ADBETRAD-1                         
117655       MOVE KREG-ADBETRAD-2    TO OHKK-ADBETRAD-2                         
117656       MOVE KREG-ADGMT         TO OHKK-ADGMT                              
117657       MOVE KREG-BEBETRAD-1    TO OHKK-BEBETRAD-1                         
117658       MOVE KREG-BEBETRAD-2    TO OHKK-BEBETRAD-2                         
117659       MOVE KREG-BEGMRK        TO OHKK-BEGMRK                             
117660       MOVE KREG-BEGMT         TO OHKK-BEGMT                              
117661       IF OHFK-FLAUTFAK = '+'                                             
117662          MOVE NEJ             TO OHKK-FLAUTFAK                           
117663       ELSE                                                               
117664          MOVE OHFK-FLAUTFAK   TO OHKK-FLAUTFAK                           
117665       END-IF                                                             
117666       MOVE OHFK-FLAUTPAC      TO OHKK-FLAUTPAC                           
117667       IF DIST20-EMBALLAGE                                                
117668          MOVE JA              TO OHKK-FLEMBORD                           
117669       ELSE                                                               
117670          MOVE NEJ             TO OHKK-FLEMBORD                           
117671       END-IF                                                             
117672       MOVE OHFK-FLFORBI       TO OHKK-FLFORBI                            
117673       MOVE OHFK-FLLSBOK       TO OHKK-FLLSBOK                            
117674       MOVE OHFK-FLORDSPE      TO OHKK-FLORDSPE                           
117675       MOVE NEJ                TO OHKK-FLOVRLEV                           
117676       MOVE KREG-FLPRELRO      TO OHKK-FLPRELRO                           
117677       MOVE KREG-FLPRERS       TO OHKK-FLPRERS                            
117678       MOVE OHFK-FLRESTN       TO OHKK-FLRESTN                            
117679       IF MID-FLRESTN = ALL '+'                                           
117680          IF KREG-FLRESTN = YES                                           
117681             MOVE JA           TO OHKK-FLRESTN                            
117682          ELSE                                                            
117683             MOVE KREG-FLRESTN TO OHKK-FLRESTN                            
117684          END-IF                                                          
117685       ELSE                                                               
117686          MOVE MID-FLRESTN     TO OHKK-FLRESTN                            
117687       END-IF                                                             
117688       MOVE OHFK-FLVORKO       TO OHKK-FLVORKO                            
117689                                                                          
117690       IF OHFK-IDANALYS = ALL '+'                                         
117691          MOVE SPACE           TO OHKK-IDANALYS                           
117692       ELSE                                                               
117693          MOVE OHFK-IDANALYS   TO OHKK-IDANALYS                           
117694       END-IF                                                             
117695       IF OHFK-IDBIPREF = ALL '+'                                         
117696          MOVE SPACE           TO OHKK-IDBIPREF                           
117697       ELSE                                                               
117698          MOVE OHFK-IDBIPREF   TO OHKK-IDBIPREF                           
117699       END-IF                                                             
117700       MOVE OHLK-IDDC-TVS      TO OHKK-IDDC-TVS                           
117701       MOVE KREG-IDDEPOT       TO OHKK-IDDEPOT                            
117702       MOVE ZERO               TO OHKK-IDDEPT                             
117703       MOVE OHLK-IDFTG         TO OHKK-IDFTG                              
117704       MOVE OHLK-IDKAMPRF      TO OHKK-IDKAMPRF                           
117705       MOVE OHLK-IDKONTO       TO OHKK-IDKONTO                            
117706       MOVE OHLK-IDKST         TO OHKK-IDKST                              
117707       MOVE KREG-IDRFTAB       TO OHKK-IDRFTAB                            
117708       MOVE KREG-IDROUTE       TO OHKK-IDROUTE                            
117709       MOVE OHFK-IDSKYLT       TO OHKK-IDSKYLT                            
117710       MOVE KREG-IDZON         TO OHKK-IDZON                              
117711       MOVE OHLK-KDFAKTYP      TO OHKK-KDFAKTYP                           
117712       IF MID-KDFRAKT = ALL '+'                                           
117713          MOVE KREG-KDFRAKT    TO OHKK-KDFRAKT                            
117714       ELSE                                                               
117715          MOVE MID-KDFRAKT     TO OHKK-KDFRAKT                            
117716       END-IF                                                             
117717       IF KREG-KDORDING = +3                                              
117718          MOVE KREG-KDORDING       TO OHKK-KDORDING                       
117719       ELSE                                                               
117720          IF OHLK-KDTPOTYP > +0 OR OHLK-IDKAMPRF > +0                     
117721             MOVE +2               TO OHKK-KDORDING                       
117722          ELSE                                                            
117723             MOVE KREG-KDORDING    TO OHKK-KDORDING                       
117724          END-IF                                                          
117725       END-IF                                                             
117726       MOVE ZERO               TO OHKK-KDVRINFO                           
117727       MOVE OHLK-KDTPOTYP      TO OHKK-KDTPOTYP                           
117728       MOVE KREG-KDTULLVE      TO OHKK-KDTULLVE                           
117729       MOVE KREG-KVDAGAR-DOW   TO OHKK-KVDAGAR-DOW                        
117730       MOVE KREG-RESLATT       TO OHKK-RESLATT                            
117731       MOVE OHLK-TITPO         TO OHKK-TITPO                              
117732       MOVE NEJ                TO OHKK-FLSOFT                             
117733                                                                          
117735                                                                          
117736          CALL W411OHKK USING OHKK-W411OHKK OHKK-WDQ2-PCB                 
117737                              OHKK-WDQ2-UPD-PCB                           
117738                              OHKK-WDQ2C-PCB                              
117739                              OHKK-GMTA-PCB                               
117740                              OHKK-GMTB-PCB OHKK-GMTC-PCB                 
117741                              OHKK-BETC-PCB OHKK-WDB2-PCB                 
117742                              OHKK-WDB3-PCB OHKK-WDB5-PCB                 
117743                              OHKK-WDP7-PCB OHKK-XXKB-PCB                 
117744     ELSE                                                                 
117745        MOVE  ZERO             TO OHKK-IDORDER                            
117746     END-IF                                                               
117795     .                                                                    
117796     EJECT                                                                
117797                                                                          
117798 C-BESTAM-ORDERNUMMER SECTION.                                            
117799                                                                          
117800     MOVE 'IMS '               TO ORDN-IDSYSTEM                           
117801                                                                          
117802     MOVE W-IDDISTR            TO ORDN-IDDISTR                            
117803     MOVE W-IDKUNDNR           TO ORDN-IDKUNDNR                           
117804                                                                          
117805     IF MID-IDORDNR = ALL '+'                                             
117806        MOVE ZERO              TO ORDN-IDORDNR-IN                         
117807     ELSE                                                                 
117808        MOVE W-IDORDNR         TO ORDN-IDORDNR-IN                         
117809     END-IF                                                               
117810                                                                          
117811     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
117812                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
117813                                                                          
117814     MOVE ORDN-IDORDNR-UT           TO W-IDORDNR                          
117815     .                                                                    
117816     EJECT                                                                
117817 D-SKAPA-ORDERHUVUD SECTION.                                              
117818     PERFORM DA-REDIGERA-ORDERHUVUD                                       
117819     PERFORM IMS-ISRT-ORQI-WDQ201                                         
117820                                                                          
117821     PERFORM DB-REDIGERA-ARBETSTABELL                                     
117822     PERFORM IMS-ISRT-ORQI-WDQ212                                         
117823     .                                                                    
117824     EJECT                                                                
117825                                                                          
117826 DA-REDIGERA-ORDERHUVUD SECTION.                                          
117827                                                                          
117828     MOVE W-IDDISTR                TO W-IDDISTR-WDB2                      
117829                                      W-IDDISTR-WDB2-MIN                  
117830                                      W-IDDISTR-WDB2-MAX                  
117831     MOVE W-IDKUNDNR               TO W-IDKUNDNR-WDB2                     
117832     PERFORM IMS-GET-WDB201-UNIK                                          
117833     IF SEGMENT-SAKNAS                                                    
117834        PERFORM IMS-GU-WDB201                                             
117835     END-IF                                                               
117836                                                                          
117837     MOVE KREG-IDDEPOT             TO OHUV-IDDEPOT                        
117838     MOVE KREG-IDROUTE             TO OHUV-IDROUTE                        
117839     MOVE KREG-IDZON               TO OHUV-IDZON                          
117840                                                                          
117841     MOVE ORDN-IDORDER-UT          TO OHUV-IDORDER                        
117842     MOVE KREG-ADBETRAD-1          TO OHUV-ADBETRAD-1                     
117843     MOVE KREG-ADBETRAD-2          TO OHUV-ADBETRAD-2                     
117844     IF MID-ADGMT = ALL '+'                                               
117845       MOVE KREG-ADGMT             TO OHUV-ADGMT                          
117846     ELSE                                                                 
117847       IF MID-ADGMT-GATA = ALL '+'                                        
117848         MOVE SPACE                TO OHUV-ADGMT-GATA                     
117849       ELSE                                                               
117850         MOVE MID-ADGMT-GATA       TO OHUV-ADGMT-GATA                     
117851       END-IF                                                             
117852       IF MID-ADGMT-PADR = ALL '+'                                        
117853         MOVE SPACE                TO OHUV-ADGMT-PADR                     
117854       ELSE                                                               
117855         MOVE MID-ADGMT-PADR       TO OHUV-ADGMT-PADR                     
117856       END-IF                                                             
117857       IF MID-ADGMT-LAND = ALL '+'                                        
117858         MOVE SPACE                TO OHUV-ADGMT-LAND                     
117859       ELSE                                                               
117860         MOVE MID-ADGMT-LAND       TO OHUV-ADGMT-LAND                     
117861       END-IF                                                             
117862       INSPECT OHUV-ADGMT REPLACING ALL '+' BY SPACE                      
117863     END-IF                                                               
117864     MOVE KREG-BEBETRAD-1          TO OHUV-BEBETRAD-1                     
117865     MOVE KREG-BEBETRAD-2          TO OHUV-BEBETRAD-2                     
117866     MOVE SPACE                    TO OHUV-BEVARREF                       
117867     IF MID-BEGMT = ALL '+'                                               
117868       MOVE KREG-BEGMT             TO OHUV-BEGMT                          
117869     ELSE                                                                 
117870       MOVE MID-BEGMT              TO OHUV-BEGMT                          
117871       INSPECT OHUV-BEGMT REPLACING ALL '+' BY SPACE                      
117872     END-IF                                                               
117873                                                                          
117874     IF  MID-BEKUNDRF = ALL '+'                                           
117875       MOVE SPACE                  TO OHUV-BEKUNDRF                       
117876     ELSE                                                                 
117877       MOVE MID-BEKUNDRF           TO OHUV-BEKUNDRF                       
117878     END-IF                                                               
117879     MOVE ZERO                     TO OHUV-IDDEPT                         
117880                                                                          
117881     IF MID-BELAGINS-DEL1 = ALL '+'                                       
117882        MOVE SPACE                 TO OHUV-BELAGINS-DEL1                  
117883     ELSE                                                                 
117884        MOVE MID-BELAGINS-DEL1     TO OHUV-BELAGINS-DEL1                  
117885     END-IF                                                               
117886     IF MID-BELAGINS-DEL2 = ALL '+'                                       
117887        MOVE SPACE                 TO OHUV-BELAGINS-DEL2                  
117888     ELSE                                                                 
117889        MOVE MID-BELAGINS-DEL2     TO OHUV-BELAGINS-DEL2                  
117890     END-IF                                                               
117891                                                                          
117892     IF DIST03-SVERIGE-EJ-778                                             
117893       AND NOT DIST03-EJ-AUTFAK                                           
117894       AND NOT DIST03-S                                                   
117895       MOVE JA                     TO OHUV-FLAUTFAK                       
117896     ELSE                                                                 
117897       MOVE NEJ                    TO OHUV-FLAUTFAK                       
117898     END-IF                                                               
117899     MOVE NEJ                      TO OHUV-FLAUTPAC                       
117900     MOVE NEJ                      TO OHUV-FLBORT                         
117901     IF DIST20-EMBALLAGE                                                  
117902        MOVE JA                    TO OHUV-FLEMBORD                       
117903     ELSE                                                                 
117904        MOVE NEJ                   TO OHUV-FLEMBORD                       
117905     END-IF                                                               
117906     MOVE KREG-FLPRELRO            TO OHUV-FLPRELRO                       
117907     MOVE KREG-FLPRERS             TO OHUV-FLPRERS                        
117908     MOVE NEJ                      TO OHUV-FLFORBI                        
117909     MOVE NEJ                      TO OHUV-FLKLAR                         
117910     MOVE JA                       TO OHUV-FLLSBOK                        
117911     MOVE JA                       TO OHUV-FLOBTRAN                       
117912     MOVE NEJ                      TO OHUV-FLORDSPE                       
117913     MOVE NEJ                      TO OHUV-FLOVRLEV                       
117914     IF OHLK-KDORDKL = +0                                                 
117915        MOVE NEJ                   TO OHUV-FLRESTN                        
117916     ELSE                                                                 
117917        IF MID-FLRESTN = ALL '+'                                          
117918           IF KREG-FLRESTN = YES                                          
117919              MOVE JA              TO OHUV-FLRESTN                        
117920           ELSE                                                           
117921              MOVE KREG-FLRESTN    TO OHUV-FLRESTN                        
117922           END-IF                                                         
117923        ELSE                                                              
117924           MOVE MID-FLRESTN        TO OHUV-FLRESTN                        
117925        END-IF                                                            
117926     END-IF                                                               
117927     MOVE NEJ                      TO OHUV-FLVORKO                        
117928     IF OHUV-FLRESTN = NEJ                                                
117929        MOVE SPACE                 TO OHUV-IDBIPREF                       
117930     ELSE                                                                 
117931       IF MID-IDBIPREF = ALL '+'                                          
117932          MOVE SPACE               TO OHUV-IDBIPREF                       
117933       ELSE                                                               
117934          MOVE MID-IDBIPREF        TO OHUV-IDBIPREF                       
117935       END-IF                                                             
117936     END-IF                                                               
117937     MOVE KREG-IDDC                TO OHUV-IDDC-PRIM                      
117938     MOVE KREG-IDDC-TVS            TO OHUV-IDDC-TVS                       
117943     MOVE OHLK-IDFTG               TO OHUV-IDFTG                          
117944     MOVE '4241'                   TO OHUV-IDSYSTEM                       
117945     MOVE OHLK-IDDISTR             TO OHUV-IDDISTR                        
117946     MOVE OHLK-IDKUNDNR            TO OHUV-IDKUNDNR                       
117947     MOVE W-IDKUNDRF               TO OHUV-IDKUNDRF                       
117948     MOVE OHLK-IDKAMPRF            TO OHUV-IDKAMPRF                       
117949     MOVE KREG-IDRFTAB             TO OHUV-IDRFTAB                        
117950     MOVE OHLK-IDKONTO             TO OHUV-IDKONTO                        
117951     MOVE OHLK-IDANALYS            TO OHUV-IDANALYS                       
117952     MOVE OHLK-IDKST               TO OHUV-IDKST                          
117953     MOVE MSG-SIGNON-USERID        TO OHUV-IDUSER                         
117954     MOVE KREG-IDSKYLT             TO OHUV-IDSKYLT                        
117955     MOVE KREG-KDGENFAK            TO OHUV-KDFAKTYP                       
117956     IF KREG-KDORDING = +3                                                
117957        MOVE KREG-KDORDING         TO OHUV-KDORDING                       
117958     ELSE                                                                 
117959        IF OHLK-KDTPOTYP > +0 OR OHLK-IDKAMPRF > +0                       
117960           MOVE +2                 TO OHUV-KDORDING                       
117961        ELSE                                                              
117962           MOVE KREG-KDORDING      TO OHUV-KDORDING                       
117963        END-IF                                                            
117964     END-IF                                                               
117965     MOVE OHLK-KDORDKL             TO OHUV-KDORDKL                        
117969     MOVE OHLK-KDTPOTYP            TO OHUV-KDTPOTYP                       
117970     MOVE KREG-KDTULLVE            TO OHUV-KDTULLVE                       
117971     MOVE +0                       TO OHUV-KDVRINFO                       
117972     MOVE KREG-KVDAGAR-DOW         TO OHUV-KVDAGAR-DOW                    
117973     MOVE KREG-RESLATT             TO OHUV-RESLATT                        
117974     MOVE MSGI-TILOKDAT            TO OHUV-TIREGDAT                       
117975                                      WS-TIREGDAT-9KOMPL                  
117976     MOVE MSGI-TILOKTID            TO WS-TIHHMM                           
117977     MOVE WS-TIHHMMSS              TO OHUV-TIREGTID                       
117978     MOVE +0                       TO OHUV-TIREGDAT-STO                   
117979     MOVE +0                       TO OHUV-TIREGTID-STO                   
117980     IF MID-TITPO = ALL '+'                                               
117981        MOVE +0                    TO OHUV-TITPO                          
117982     ELSE                                                                 
117983        MOVE OHLK-TITPO            TO OHUV-TITPO                          
117984     END-IF                                                               
117985     IF OHUV-KDORDKL = 1 AND                                              
117986        GMT-FLLDCKND = JA                                                 
117987        MOVE 'FW'                  TO OHUV-KDORDTYP-LDC                   
117988        MOVE ZERO                  TO OHUV-TIREPDAT                       
117989     ELSE                                                                 
117990        MOVE SPACE                 TO OHUV-KDORDTYP-LDC                   
117991        MOVE ZERO                  TO OHUV-TIREPDAT                       
117992     END-IF                                                               
117993     COMPUTE OHUV-TIREGDAT-9KOMPL = 9999999 - WS-TIREGDAT-9KOMPL          
117994     MOVE OHKK-FLORDTIL            TO OHUV-FLORDTIL                       
117995     MOVE ZERO                     TO OHUV-KVORDTIL                       
117996     MOVE SPACE                    TO OHUV-IDLEVNR-EJLS                   
117997     MOVE NEJ                      TO OHUV-FLSOFT                         
117998     IF MID-FLVORFK = ALL '+'                                             
117999       MOVE NEJ                    TO OHUV-FLVORFK                        
118000     ELSE                                                                 
118001       IF (MID-FLVORFK = YES OR JA)                                       
118002         AND OHUV-KDORDKL = ZERO AND DIST15-FLVORFK                       
118003         MOVE JA                   TO OHUV-FLVORFK                        
118004       ELSE                                                               
118005         MOVE NEJ                  TO OHUV-FLVORFK                        
118006       END-IF                                                             
118007     END-IF                                                               
118008     MOVE ZERO                     TO OHUV-IDGROSS                        
118009     MOVE SPACE                    TO OHUV-IDBILREG                       
118010                                      OHUV-IDVIN                          
118011                                      OHUV-IDCISNR                        
118012     .                                                                    
118013     EJECT                                                                
118014 DB-REDIGERA-ARBETSTABELL SECTION.                                        
118015                                                                          
118016     MOVE KREG-IDDC            TO ARB-IDDC                                
118017     IF MID-BEGMRK = ALL '+'                                              
118018        MOVE KREG-BEGMRK       TO ARB-BEGMRK                              
118019     ELSE                                                                 
118020        MOVE MID-BEGMRK        TO ARB-BEGMRK                              
118021        INSPECT ARB-BEGMRK REPLACING ALL '+' BY SPACE                     
118022     END-IF                                                               
118023     MOVE NEJ                  TO ARB-FLODELUT                            
118024     MOVE +0                   TO ARB-IDRADNR-SISTA                       
118025     MOVE KREG-IDTRP           TO ARB-IDTRP                               
118026     MOVE KREG-IDTRP-ALT       TO ARB-IDTRP-ALT                           
118027     MOVE +0                   TO ARB-IDPLKLST-SISTA                      
118028     MOVE KREG-KDFDKRAV        TO ARB-KDFDKRAV                            
118029     IF MID-KDFRAKT = ALL '+'                                             
118030        MOVE KREG-KDFRAKT      TO ARB-KDFRAKT                             
118031     ELSE                                                                 
118032        MOVE MID-KDFRAKT       TO ARB-KDFRAKT                             
118033     END-IF                                                               
118034     IF (MID-FLVORFK = JA OR YES)                                         
118035       AND OHUV-KDORDKL = 0 AND DIST15-FLVORFK                            
118036       PERFORM S03-VORKDFRAKT                                             
118037     END-IF                                                               
118038     IF OHUV-FLRESTN = NEJ                                                
118039       MOVE +0                   TO ARB-KDROPACK                          
118040     ELSE                                                                 
118041       IF MID-KDROPACK = ALL '+'                                          
118042         IF OHUV-ADGMT    EQUAL GMT-ADGMT AND                             
118043            OHUV-BEGMT    EQUAL GMT-BEGMT                                 
118045            MOVE KREG-KDROPACK   TO ARB-KDROPACK                          
118046         ELSE                                                             
118047            MOVE '3'             TO ARB-KDROPACK                          
118048         END-IF                                                           
118049       ELSE                                                               
118050         MOVE MID-KDROPACK       TO ARB-KDROPACK                          
118070       END-IF                                                             
118310     END-IF                                                               
118400                                                                          
118500     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
118600     IF (DIST15-NA      AND (ARB-KDFRAKT > +11 AND < +20)) OR             
118700        (DIST15-ENGLAND AND OHUV-KDORDKL = +1                             
118800                        AND ARB-KDFRAKT = +16) OR                         
118900        (DIST15-JAPAN   AND OHUV-KDORDKL = +1                             
119000                        AND (ARB-KDFRAKT = +15 OR +25)) OR                
119100        (DIST15-AUSTRALIEN AND OHUV-KDORDKL = +1                          
119200                        AND ARB-KDFRAKT = +5)                             
119300       MOVE +0                 TO ARB-KDROPACK                            
119400     END-IF                                                               
119500                                                                          
119600     MOVE KREG-KDTRPKAT        TO ARB-KDTRPKAT                            
119700     MOVE +0                   TO ARB-KVSEMBRA                            
119800     MOVE TRAN-TIRFS           TO ARB-TIRFS                               
119900     MOVE TRAN-TIAAMMDD        TO ARB-DATRPAVD                            
120000     IF TRAN-TIAAMMDD NOT = ZERO                                          
120100       IF TRAN-TIAAMMDD < 500000                                          
120200         MOVE 20               TO ARB-DATRPAVD (1:2)                      
120300       ELSE                                                               
120400         IF TRAN-TIAAMMDD < 999999                                        
120500           MOVE 19             TO ARB-DATRPAVD (1:2)                      
120600         ELSE                                                             
120700           MOVE 99999999       TO ARB-DATRPAVD                            
120800         END-IF                                                           
120900       END-IF                                                             
121000     END-IF                                                               
121100     MOVE TRAN-TIHHMM          TO ARB-TIHHMM                              
121101     MOVE SPACE                TO ARB-KDORDSTA-O                          
121102     MOVE 'E'                  TO ARB-KDORDSTA                            
122500     .                                                                    
122600     EJECT                                                                
122700 S01-HOPPA-TILL-RADREGISTRERING SECTION.                                  
122710                                                                          
122720     MOVE '001'              TO MSGI-KDCALL                               
122730     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
122740     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
122750     MOVE '4241'             TO MSGI-IDTRANS                              
122760     MOVE MID-IDDISTR        TO MSGI-IDDISTR                              
122770     MOVE MID-IDKUNDNR       TO MSGI-IDKUNDNR                             
122780     MOVE W-IDORDNR          TO MSGI-IDKUNDRF                             
122790     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
122800                                                                          
122900     MOVE 'W4O24201'            TO MFS-IDMOD                              
123000                                                                          
123100*    PERFORM S11-HAMTA-KDVALISO                                           
123200                                                                          
123300     MOVE '4242'                TO 4242-MOD-IDTRANS                       
123400     MOVE MFS-RENSA-FAELT       TO 4242-MOD-TEMFSFEL                      
123700                                                                          
123800     MOVE W-IDDISTR             TO WS-NUM-4                               
123900     MOVE WS-NUM-4              TO 4242-MOD-IDDISTR                       
124000     INSPECT 4242-MOD-IDDISTR   REPLACING LEADING ZERO BY SPACE           
124100     MOVE W-IDKUNDNR            TO WS-NUM-6                               
124200     MOVE WS-NUM-6              TO 4242-MOD-IDKUNDNR                      
124300     INSPECT 4242-MOD-IDKUNDNR  REPLACING LEADING ZERO BY SPACE           
124400     MOVE W-IDORDNR             TO WS-NUM-5                               
124500     MOVE WS-NUM-5              TO 4242-MOD-IDORDNR5                      
124600     INSPECT 4242-MOD-IDORDNR5  REPLACING LEADING ZERO BY SPACE           
124700     MOVE MID-KDORDKL           TO 4242-MOD-KDORDKL                       
124800     MOVE ARB-KDFRAKT           TO 4242-MOD-KDFRAKT                       
124900     MOVE WS-TEDDI              TO 4242-MOD-TEDDI                         
125000     MOVE OHUV-BEKUNDRF         TO 4242-MOD-BEVOLREF                      
125100     MOVE SPACE                 TO 4242-MOD-KDTRTYP                       
125200     MOVE MFS-ADD-SAETT-CURSOR  TO 4242-MOD-IDARTNR-ATTR(1)               
125300                                                                          
125400     MOVE 4242-MOD-LAENGD       TO MSG-KVLL                               
125500     PERFORM IMS-INSERT-MSG                                               
125600                                                                          
125700     MOVE JA                    TO HOPP                                   
125800     .                                                                    
125900     EJECT                                                                
126000 S02-HOPPA-TILL-TILLAGGSREG     SECTION.                                  
126100                                                                          
126200     MOVE '002'              TO MSGI-KDCALL                               
126210     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
126220     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
126230     MOVE '4241'             TO SPAR-IDTRANS                              
126240     MOVE KREG-IDDC          TO SPAR-IDDC                                 
126250     MOVE SPAR-AREA          TO MSGI-SPAR-AREA                            
126260     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
126300                                                                          
126400     MOVE OHKK-IDORDER TO W-IDORDER                                       
126500     PERFORM IMS-GHU-ORQI-WDQ201                                          
126600                                                                          
126700     MOVE 'W4O20601'            TO MFS-IDMOD                              
126800                                                                          
126900     MOVE '4206'                TO 4206-MOD-IDTRANS                       
127000     MOVE OHUV-IDDISTR          TO 4206-MOD-IDDISTR                       
127100     INSPECT 4206-MOD-IDDISTR   REPLACING LEADING ZERO BY SPACE           
127200     MOVE OHUV-IDKUNDNR         TO 4206-MOD-IDKUNDNR                      
127300     INSPECT 4206-MOD-IDKUNDNR  REPLACING LEADING ZERO BY SPACE           
127400     MOVE OHUV-IDORDNR7(3:5)    TO 4206-MOD-IDORDNR5                      
127500     MOVE OHUV-KDORDKL          TO 4206-MOD-KDORDKL                       
127600     MOVE OHKK-KDFRAKT          TO 4206-MOD-KDFRAKT                       
127700     MOVE WS-TEDDI              TO 4206-MOD-TEDDI                         
127800     MOVE SPACE                 TO 4206-MOD-KDTRTYP                       
127900     MOVE MFS-ADD-SAETT-CURSOR  TO 4206-MOD-IDARTNR-ATTR(1)               
128000                                                                          
128100     IF MID-IDORDNR NOT = ALL '+'                                         
128200        STRING 'YOUR ORDER NO. HAS BEEN CHANGED TO '                      
128300               OHUV-IDORDNR7(3:5)                                         
128400            DELIMITED BY SIZE INTO 4206-MOD-TEMFSFEL                      
128500     END-IF                                                               
128600                                                                          
128700     MOVE 4206-MOD-LAENGD      TO MSG-KVLL                                
128800     PERFORM IMS-INSERT-MSG                                               
128900                                                                          
129000     MOVE JA                    TO HOPP                                   
129100     .                                                                    
129200     EJECT                                                                
129210 S03-VORKDFRAKT  SECTION.                                                 
129211     MOVE KREG-IDDC          TO W-IDDC-WDB3                               
129212                                W-IDDC-WDB3-DEF                           
129213     MOVE W-IDDISTR          TO W-IDDISTR-WDB3                            
129214                                W-IDDISTR-WDB3-DEF                        
129215     MOVE W-IDKUNDNR         TO W-IDKUNDNR-WDB3                           
129216     PERFORM IMS-GU-WDB301                                                
129217     IF SEGMENT-FINNS                                                     
129218       MOVE DC-KDGENFRA-DO   TO ARB-KDFRAKT                               
129219     END-IF                                                               
129220     .                                                                    
129230     EJECT                                                                
129300 Z-FINIT SECTION.                                                         
129400                                                                          
129500     IF MED-IDMFSFEL NOT = SPACE                                          
129600         CALL WMEDKONV USING MED-WMEDAREA                                 
129700         MOVE MED-MFSFEL    TO MOD-TEMFSFEL                               
129800     END-IF                                                               
129900     PERFORM MFS-ROER-EJ-BILD                                             
130000     .                                                                    
130100     EJECT                                                                
130200 MFS-RENSA-BILD SECTION.                                                  
130300                                                                          
130400     MOVE MFS-RENSA-FAELT  TO   MOD-IDDISTR                               
130500                                MOD-IDKUNDNR                              
130600                                MOD-IDORDNR                               
130700                                MOD-KDORDKL                               
130800                                MOD-KDFRAKT                               
130810                                MOD-FLVORFK                               
130900                                MOD-TIRFS-DAT                             
131000                                MOD-TIRFS-TID                             
131100                                MOD-BEKUNDRF                              
131200                                MOD-FLRESTN                               
131300                                MOD-KDTPOTYP                              
131400                                MOD-TITPO                                 
131500                                MOD-IDKAMPRF                              
131600                                MOD-BELAGINS-DEL1                         
131700                                MOD-BELAGINS-DEL2                         
131800                                MOD-BEGMT-RAD1                            
131900                                MOD-BEGMT-RAD2                            
132000                                MOD-ADGMT-GATA                            
132100                                MOD-ADGMT-PADR                            
132200                                MOD-ADGMT-LAND                            
132300                                MOD-BEGMRK-RAD1                           
132400                                MOD-BEGMRK-RAD2                           
132500                                MOD-KDROPACK                              
132600                                MOD-IDBIPREF                              
132700     .                                                                    
132800     EJECT                                                                
132900 MFS-ROER-EJ-BILD SECTION.                                                
133000                                                                          
133100     MOVE MFS-ROER-EJ-FAELT TO  MOD-IDDISTR                               
133200                                MOD-IDKUNDNR                              
133300                                MOD-IDORDNR                               
133400                                MOD-KDORDKL                               
133500                                MOD-KDFRAKT                               
133510                                MOD-FLVORFK                               
133600                                MOD-TIRFS-DAT                             
133700                                MOD-TIRFS-TID                             
133800                                MOD-BEKUNDRF                              
133900                                MOD-FLRESTN                               
134000                                MOD-KDTPOTYP                              
134100                                MOD-TITPO                                 
134200                                MOD-IDKAMPRF                              
134300                                MOD-BELAGINS-DEL1                         
134400                                MOD-BELAGINS-DEL2                         
134500                                MOD-BEGMT-RAD1                            
134600                                MOD-BEGMT-RAD2                            
134700                                MOD-ADGMT-GATA                            
134800                                MOD-ADGMT-PADR                            
134900                                MOD-ADGMT-LAND                            
135000                                MOD-BEGMRK-RAD1                           
135100                                MOD-BEGMRK-RAD2                           
135200                                MOD-KDROPACK                              
135300                                MOD-IDBIPREF                              
135400     .                                                                    
135500     EJECT                                                                
135600* --- IMS SEKTIONER ---                                                   
135700     SKIP2                                                                
135800 IMS-GET-MSG SECTION.                                                     
135900                                                                          
136000     MOVE '  QC' TO GODK-STATUSKODER                                      
136100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
136200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136300     PERFORM IMS-STATUSKONTROLL                                           
136400     .                                                                    
136500     SKIP2                                                                
136600 IMS-INSERT-MSG SECTION.                                                  
136700                                                                          
136710     IF ENGLISH-TEXT                                                      
136720       MOVE 'N' TO MFS-KDHUVOMR                                           
136730     END-IF                                                               
136800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
136900     MOVE SPACE TO GODK-STATUSKODER                                       
137000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
137100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
137200     PERFORM IMS-STATUSKONTROLL                                           
137300     .                                                                    
137400     EJECT                                                                
137500 IMS-GET-WDB201-UNIK SECTION.                                             
137600                                                                          
137700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
137800          DELIMITED BY SIZE INTO SSA1                                     
137900     MOVE '  GE'               TO GODK-STATUSKODER                        
138000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
138100     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
138200     PERFORM IMS-STATUSKONTROLL                                           
138300     .                                                                    
138400     SKIP2                                                                
138500 IMS-GU-WDB201 SECTION.                                                   
138600                                                                          
138700     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
138800                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
138900            DELIMITED BY SIZE INTO SSA1                                   
139000     MOVE '  GE'               TO GODK-STATUSKODER                        
139100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
139200     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
139300     PERFORM IMS-STATUSKONTROLL                                           
139400     .                                                                    
139500     SKIP2                                                                
139600 IMS-GU-ORQL-WDQ201 SECTION.                                              
139700                                                                          
139800     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
139900          DELIMITED BY SIZE INTO SSA1                                     
140000     MOVE '  GE'               TO GODK-STATUSKODER                        
140100     CALL CBLTDLI USING GHU ORQL-PCB DLI-IO-AREA-OHUV SSA1                
140200     MOVE ORQL-STATUS-CODE    TO STATUS-WS                                
140300     PERFORM IMS-STATUSKONTROLL                                           
140400     .                                                                    
140500     SKIP3                                                                
140600 IMS-GHU-ORQI-WDQ201 SECTION.                                             
140700                                                                          
140800     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
140900          DELIMITED BY SIZE INTO SSA1                                     
141000     MOVE '  '                 TO GODK-STATUSKODER                        
141100     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA-OHUV SSA1                
141200     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
141300     PERFORM IMS-STATUSKONTROLL                                           
141400     .                                                                    
141500     SKIP3                                                                
141600 IMS-GNP-ORQI-WDQ212 SECTION.                                             
141700                                                                          
141800     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
141900          DELIMITED BY SIZE INTO SSA1                                     
142000     MOVE '    '               TO GODK-STATUSKODER                        
142100     CALL CBLTDLI USING GNP  ORQI-PCB DLI-IO-AREA-ARB SSA1                
142200     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
142300     PERFORM IMS-STATUSKONTROLL                                           
142400     .                                                                    
142500     SKIP3                                                                
142600 IMS-REPL-ORQI-WDQ201 SECTION.                                            
142700                                                                          
142800     MOVE '    '               TO GODK-STATUSKODER                        
142900     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-OHUV                    
143000     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
143100     PERFORM IMS-STATUSKONTROLL                                           
143200     .                                                                    
143300     EJECT                                                                
143400 IMS-ISRT-ORQI-WDQ201 SECTION.                                            
143500                                                                          
143600     MOVE 'WLORQI01 '          TO SSA1                                    
143700     MOVE '    '               TO GODK-STATUSKODER                        
143800     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-OHUV SSA1               
143900     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
144000     PERFORM IMS-STATUSKONTROLL                                           
144100     .                                                                    
144200     SKIP3                                                                
144300 IMS-ISRT-ORQI-WDQ212 SECTION.                                            
144400                                                                          
144500     MOVE 'WLORQI12 '          TO SSA1                                    
144600     MOVE '    '               TO GODK-STATUSKODER                        
144700     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-ARB SSA1                
144800     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
144900     PERFORM IMS-STATUSKONTROLL                                           
145000     .                                                                    
145100     SKIP3                                                                
145110 IMS-GU-WDB301                 SECTION.                                   
145120                                                                          
145130     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
145140                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
145150            DELIMITED BY SIZE INTO SSA1                                   
145160     MOVE '  GE'              TO GODK-STATUSKODER                         
145170     CALL CBLTDLI USING GU  WDB3-PCB DLI-IO-AREA-WDB301 SSA1              
145180     MOVE WDB3-STATUS-CODE      TO STATUS-WS                              
145190     PERFORM IMS-STATUSKONTROLL                                           
145191     .                                                                    
145192     SKIP2                                                                
145200 IMS-STATUSKONTROLL SECTION.                                              
145300                                                                          
145400     SET STATUS-IX TO 1                                                   
145500     SEARCH GODK-STATUS                                                   
145600       AT END CALL FELLOG                                                 
145700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
145800     END-SEARCH                                                           
145900     .                                                                    
