000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4025100.                                                
000400 AUTHOR.         KERSTIN JOHANSSON   GUIDE DATAKONSULT AB                 
000500 DATE-WRITTEN.   OKT -90.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR EN BAKGRUNDSTRANS FÖR KONTROLL OCH                 
001100*        UPPDATERING AV ORDERHUVUD I ORDERKÖN.                            
001200*        INPUTTRANSAKTIONEN ÄR SKAPAD FRÅN EN BATCH SEKVENSFIL            
001300*        OCH UPPDATERAD PÅ KOMMUNIKATIONS DB. EN GENERELL MPP -           
001400*        DISPATCHERN HÄMTAR TRANSAKTIONEN PÅ KOMMUNIKATIONS DB            
001500*        OCH STARTAR DENNA BAKGRUNDSTRANS.                                
001600*        MOTTAGNA VÄRDEN KONTROLLERAS OCH ÖVRIGA VÄRDEN HÄMTAS            
001700*        FRÅN KUNDREGISTRET.                                              
001800*        DÅ BEHANDLINGEN ÄR KLAR SKICKAS EN FEL/RÄTT-SIGNAL PÅ            
001900*        IMSQ TILLBAKA TILL DISPATCHERN.                                  
002000*                                                                         
002100*        NOTERINGAR RÖRANDE RELEASADE PROFORMOR (IDSYSTEM = OREL)         
002200*        - FÖR ATT UNDVIKA MASKINELLT ORDERAVSLUT FÖR FD PROFORMOR        
002300*          HAR FÖLJANDE ÄNDRINGAR GJORTS I SECTION I-SKAPA-...            
002400*          OM IDSYSTEM=OREL OCH                                           
002500*             KDTRPKAT=A                                                  
002600*             SÄTT KDTRPKAT=B                                             
002700*          OM IDSYSTEM=OREL                                               
002800*             SÄTT TIRFS=0                                                
002900*                  TIAAMMDD=00                                            
003000*                  TIHHMM=00                                              
003100*                                                                         
003200*    PROGRAMMET KAN STARTAS FRÅN 4255 OCH SKA DÅ LÄGGA UPP ETT            
003300*    NYTT ORDERHUVUD ELLER KOMPLETTERA ETT BEFINTLIGT.                    
003400*                                                                         
003500*    EJECT                                                                
003600*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
003700*        PROGRAMMET UPPDATERAR WLORQI (WDQ2)  ORDERHUVUD                  
003800*        PROGRAMMET LÄSER      WLBETC (WDB1)  KUNDREGISTER                
003900*        PROGRAMMET LÄSER      WLKNDC (WDB2/3)KUNDREGISTER                
004000*        PROGRAMMET LÄSER      WLKNDG (WDB5)  KUNDREGISTER                
004100*        PROGRAMMET UPPDATERAR WLXXKP (WDR1)  ORDERNUMMEWREGISTER         
004200*        PROGRAMMET LÄSER              WDM2   KAMPANJREGISTER             
004300*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTREGISTER           
004400*                                                                         
004500*    INDATA.                                                              
004600*        TRANSAKTION: W4T251                                              
004700*        MID:         W4I25101                                            
004800*                     WMSGKOM                                             
004900*                                                                         
005000*    UTDATA.                                                              
005100*        MOD:         WMSGKOM                                             
005110*    STORY 2375089 ADD IDSYSTEM VOUI, ECOM                                
005120*                                                                         
005130                                                                          
005140     EJECT                                                                
005150 ENVIRONMENT DIVISION.                                                    
005160                                                                          
005170 DATA DIVISION.                                                           
005180 WORKING-STORAGE SECTION.                                                 
005190*    -- CHECKED BY WY2000                                                 
005200     SKIP3                                                                
005300 77  IDPGM                       PIC X(08)   VALUE 'W4025100'.            
005400 77  WS-CDC-11                   PIC X(02)   VALUE '11'.                  
005600 77  WS-FLRESTN                  PIC X      VALUE SPACE.                  
005700 77  WS-KDPERSON                 PIC S9(3)  VALUE +0    COMP-3.           
005800 77  YES                         PIC X(1)   VALUE 'Y'.                    
005900 77  JA                          PIC X(1)   VALUE 'J'.                    
006000 77  NEJ                         PIC X(1)   VALUE 'N'.                    
006100                                                                          
006200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006300     88  ALLT-OK                             VALUE 'J'.                   
006400                                                                          
006500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006600 77  RKOD-ABEND-MED-DUMP     PIC S9(4)   VALUE +33 COMP SYNC.             
006700                                                                          
006800 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
006900 01  FILLER REDEFINES WS-TIHHMMSS.                                        
007000     03  WS-TIHHMM               PIC 9(4).                                
007100     03  FILLER                  PIC 9(2).                                
007200                                                                          
007300 01  WS-TIRFS                    PIC 9(10).                               
007400 01  FILLER REDEFINES WS-TIRFS.                                           
007500     03  WS-TIRFS-DAT            PIC 9(6).                                
007600     03  WS-TIRFS-TID            PIC 9(4).                                
007700     EJECT                                                                
007800                                                                          
007900 01  WS-ALFA-1.                                                           
008000     03  WS-NUM-1                PIC 9(1).                                
008100 01  WS-ALFA-2.                                                           
008200     03  WS-NUM-2                PIC 9(2).                                
008300 01  WS-ALFA-3.                                                           
008400     03  WS-NUM-3                PIC 9(3).                                
008500 01  WS-ALFA-4.                                                           
008600     03  WS-NUM-4                PIC 9(4).                                
009600 01  WS-ALFA-6.                                                           
009700     03  WS-NUM-6                PIC 9(6).                                
009800 01  WS-ALFA-7.                                                           
009900     03  WS-NUM-7                PIC 9(7).                                
010000 01  WS-ALFA-10.                                                          
010100     03  WS-NUM-10               PIC 9(10).                               
010200 01  WS-ALFA-1V3.                                                         
010300     03  WS-ALFA-HELTAL          PIC X(1).                                
010400     03  WS-ALFA-PUNKT           PIC X(1).                                
010500     03  WS-ALFA-DECIMAL         PIC X(3).                                
010600 01  WS-NUM-1V3                  PIC 9V9(3).                              
010700 01  FILLER REDEFINES WS-NUM-1V3.                                         
010800     03  WS-NUM-HELTAL           PIC 9(1).                                
010900     03  WS-NUM-DECIMAL          PIC 9(3).                                
011000     EJECT                                                                
011100                                                                          
011200 01  WS-TIREGDAT-9KOMPL          PIC 9(9)    VALUE ZERO.                  
011300                                                                          
011400 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
011500*                                                                         
011600 01  FILLER REDEFINES TEST-IDDISTR.                                       
011700*    03 -COPY WWDIST03                                                    
011800 01  FILLER REDEFINES TEST-IDDISTR.                                       
011900*    03 -COPY WWDIST15                                                    
012000*                                                                         
012100*                                                                         
012200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012300 01  GENERELLA-SUBPROGRAM.                                                
012400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012800*                                                                         
012900*                                                                         
013000*    --- PARAMETRAR TILL GENERELLA SUBPROGRAM                             
013100 01 FILLER                       PIC X(8) VALUE 'W005INIT'.               
013200*   -COPY WMSGINIT                                                        
013300     EJECT                                                                
013400*                                                                         
013500 01  GEMENSAMMA-SUBPROGRAM.                                               
013600     03  W411OHFK                PIC X(8)    VALUE 'W411OHFK'.            
013700*        FORMELLA KONTROLLER                                              
013800     03  W411KREG                PIC X(8)    VALUE 'W411KREG'.            
013900*        LÄSNING AV KUNDREGISTRET                                         
014000     03  W411OHLK                PIC X(8)    VALUE 'W411OHLK'.            
014100*        LOGISKA KONTROLLER                                               
014200     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
014300*        KONTROLL OCH UTTAG AV AUTOMATISKT ORDERNUMMER                    
014400     03  W411TRAN                PIC X(8)    VALUE 'W411TRAN'.            
014500*        BESTÄM TRANSPORT                                                 
014600     EJECT                                                                
014700 01  MESSAGE-CODES.                                                       
014800     03  ERR-ORDER-FINNS         PIC X(3)    VALUE '703'.                 
014900     03  ERR-FORMELLT-FEL        PIC X(3)    VALUE '094'.                 
015000     03  ERR-FORM-IDDISTR-FEL    PIC X(3)    VALUE '94A'.                 
015100     03  ERR-FORM-IDKUNDNR-FEL   PIC X(3)    VALUE '94B'.                 
015200     03  ERR-FORM-IDORDNR-FEL    PIC X(3)    VALUE '94C'.                 
015300     03  ERR-FORM-KDORDKL-FEL    PIC X(3)    VALUE '94D'.                 
015400     03  ERR-FORM-KDFRAKT-FEL    PIC X(3)    VALUE '94E'.                 
015500     03  ERR-FORM-FLRESTN-FEL    PIC X(3)    VALUE '94F'.                 
015510     03  ERR-FORM-IDSKYLT-FEL    PIC X(3)    VALUE '94G'.                 
015520     03  ERR-FORM-IDDC-FEL       PIC X(3)    VALUE '028'.                 
015530     03  ERR-FORM-FLLSBOK-FEL    PIC X(3)    VALUE '94H'.                 
015540     03  ERR-FORM-IDKAMPRF-FEL   PIC X(3)    VALUE '94I'.                 
015550     03  ERR-FORM-IDKONTO-FEL    PIC X(3)    VALUE '940'.                 
015560     03  ERR-FORM-IDANALYS-FEL   PIC X(3)    VALUE '941'.                 
015570     03  ERR-FORM-IDKST-FEL      PIC X(3)    VALUE '942'.                 
015580     03  ERR-FORM-IDFTG-FEL      PIC X(3)    VALUE '943'.                 
015590     03  ERR-FORM-KDFAKTYP-FEL   PIC X(3)    VALUE '944'.                 
015600     03  ERR-FORM-KDROPACK-FEL   PIC X(3)    VALUE '945'.                 
015700     03  ERR-FORM-KDTPOTYP-FEL   PIC X(3)    VALUE '946'.                 
015800     03  ERR-FORM-TITPO-FEL      PIC X(3)    VALUE '947'.                 
015900     03  ERR-FORM-TIRFSDAT-FEL   PIC X(3)    VALUE '948'.                 
016000     03  ERR-FORM-KDTULLVE-FEL   PIC X(3)    VALUE '949'.                 
017000     03  ERR-IDDISTR-FEL         PIC X(3)    VALUE '95A'.                 
017100     03  ERR-IDORDNR-FEL         PIC X(3)    VALUE '95B'.                 
017200     03  ERR-KDORDKL-FEL         PIC X(3)    VALUE '95C'.                 
017300     03  ERR-IDKONTO-FEL         PIC X(3)    VALUE '95D'.                 
017400     03  ERR-IDANALYS-FEL        PIC X(3)    VALUE '95E'.                 
017500     03  ERR-IDKST-FEL           PIC X(3)    VALUE '95F'.                 
017600     03  ERR-IDFTG-FEL           PIC X(3)    VALUE '950'.                 
017700     03  ERR-IDKAMPRF-FEL        PIC X(3)    VALUE '157'.                 
017800     03  ERR-KDFAKTYP-FEL        PIC X(3)    VALUE '952'.                 
017900     03  ERR-KDTPOTYP-FEL        PIC X(3)    VALUE '953'.                 
018000     03  ERR-TITPO-FEL           PIC X(3)    VALUE '954'.                 
018100     03  ERR-SAKNAS-KREG         PIC X(3)    VALUE '063'.                 
018200     03  ERR-TRANSPORT-FEL       PIC X(3)    VALUE '087'.                 
018300     03  ERR-KUND-SPAERRAD       PIC X(3)    VALUE '213'.                 
018400     03  ERR-MOMS-REGNR-FEL      PIC X(3)    VALUE '223'.                 
018500     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
018600     03  ERR-SAKNAS-BET          PIC X(3)    VALUE '145'.                 
018700     EJECT                                                                
018800*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
018900 01 FILLER           PIC X(16) VALUE 'W411OHFK***'.                       
019000*   -COPY W411OHFK                                                        
019100     EJECT                                                                
019200 01 FILLER           PIC X(16) VALUE 'W411KREG***'.                       
019300*   -COPY W411KREG                                                        
019400     EJECT                                                                
019500 01 FILLER           PIC X(16) VALUE 'W411OHLK***'.                       
019600*   -COPY W411OHLK                                                        
019700     EJECT                                                                
019800 01 FILLER           PIC X(16) VALUE 'W411ORDN***'.                       
019900*   -COPY W411ORDN                                                        
020000     EJECT                                                                
020100 01 FILLER           PIC X(16) VALUE 'W411TRAN***'.                       
020200*   -COPY W411TRAN                                                        
020300     EJECT                                                                
020400*    --- AREOR FÖR MSG-IO                                                 
020500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020600     SKIP3                                                                
020700 01  MID-AREA.                                                            
020800*03  MID -COPY W4I25101                                                   
020900 01  FILLER                      PIC X(16)  VALUE 'MSG-IO-AREA '.         
021000     SKIP3                                                                
021100*01  -COPY WMSGAREA                                                       
021200     EJECT                                                                
021300*05  -COPY W4I25501  -PRE 4255-  -RED MSG-MID-OUT.                        
021400     EJECT                                                                
021500 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
021600 01  KOM-IO-AREA.                                                         
021700*03  -COPY WMSGKOM                                                        
021800     EJECT                                                                
021900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022000*                                                                         
022100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022200                                                                          
022300 01  NYCKLAR-TILL-DLI.                                                    
022400     03  W-IDORDER-X.                                                     
022500         05  W-IDORDER           PIC S9(7)   VALUE +0 COMP-3.             
022600                                                                          
022700     03  W-WDQ2CSEQ-X.                                                    
022800         05  W-IDDISTR           PIC S9(5)   VALUE +0 COMP-3.             
022900         05  W-IDKUNDNR          PIC S9(7)   VALUE +0 COMP-3.             
023000         05  W-IDKUNDRF.                                                  
023100           07  W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
023200           07  FILLER            PIC X(3)    VALUE SPACE.                 
023300     EJECT                                                                
023400*    --- STATUS-KOD FRÅN IMS                                              
023500 01  STATUS-WS                   PIC XX.                                  
023600     88  SEGMENT-FINNS                       VALUE '  '.                  
023700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023900     SKIP2                                                                
024000 01  GODK-STATUSKODER.                                                    
024100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024200     SKIP3                                                                
024300 01  SSA1                        PIC X(64).                               
024400 01  SSA2                        PIC X(64).                               
024500     EJECT                                                                
024600*    --- IMS FUNKTIONSKODER                                               
024700*01  -COPY W0003                                                          
024800     EJECT                                                                
024900*    ---  DLI INPUT-OUTPUT AREA                                           
025000 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
025100 01  DLI-IO-AREA-OHUV.                                                    
025200     03  WLORQI01.                                                        
025300*        05  -COPY WDQ201                                                 
025400     EJECT                                                                
025500 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
025600 01  DLI-IO-AREA-ARB.                                                     
025700     03  WLORQI12.                                                        
025800*        05  -COPY WDQ212                                                 
025900     EJECT                                                                
026000 01  FILLER                      PIC X(16)   VALUE 'R601-AREA'.           
026100 01    DLI-IO-AREA2.                                                      
026200*  03    WLFILA01  -COPY WDR601                                           
026300     EJECT                                                                
026400*    07  W414200   -COPY W414200A          -RED FIL-WDR601-DATA.          
026500     EJECT                                                                
026600 LINKAGE SECTION.                                                         
026700                                                                          
026800*01  -COPY W0009   -PRE MSG-                                              
026900     EJECT                                                                
027000*01  -COPY W0009   -PRE DISP-                                             
027100     EJECT                                                                
027200*01  -COPY W0009   -PRE 4255-                                             
027300     EJECT                                                                
027400*01  -COPY W0008   -PRE USEA-                                             
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01  -COPY W0008   -PRE ORQL-                                             
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000*01  -COPY W0008   -PRE ORQI-                                             
028100     05  FILLER                  PIC X.                                   
028200     EJECT                                                                
028300*01  -COPY W0008   -PRE FILA-                                             
028400     05  FILLER                  PIC X.                                   
028500     EJECT                                                                
028600 01  KREG-GMTA-PCB               PIC X.                                   
028700 01  KREG-GMTB-PCB               PIC X.                                   
028800 01  KREG-GMTC-PCB               PIC X.                                   
028900 01  KREG-BETC-PCB               PIC X.                                   
029000 01  OHLK-WDM2-PCB               PIC X.                                   
029100 01  OHLK-WDB6-PCB               PIC X.                                   
029200 01  ORDN-XXKP-PCB               PIC X.                                   
029300 01  ORDN-ORQL-PCB               PIC X.                                   
029400 01  ORDN-PROC-PCB               PIC X.                                   
029500 01  ORDN-ORQI-PCB               PIC X.                                   
029600 01  TRAN-XXKB-PCB               PIC X.                                   
029700 01  SAP-SAPC-PCB                PIC X.                                   
029800     EJECT                                                                
029900 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB 4255-PCB                      
030000                     USEA-PCB ORQL-PCB ORQI-PCB FILA-PCB                  
030100                     KREG-GMTA-PCB KREG-GMTB-PCB KREG-GMTC-PCB            
030200                     KREG-BETC-PCB                                        
030300                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
030400                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
030500                     ORDN-ORQI-PCB                                        
030600                     TRAN-XXKB-PCB                                        
030700                     SAP-SAPC-PCB.                                        
030800 MAIN SECTION.                                                            
030900     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB 4255-PCB                      
031000                     USEA-PCB ORQL-PCB ORQI-PCB FILA-PCB                  
031100                     KREG-GMTA-PCB KREG-GMTB-PCB KREG-GMTC-PCB            
031200                     KREG-BETC-PCB                                        
031300                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
031400                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
031500                     ORDN-ORQI-PCB                                        
031600                     TRAN-XXKB-PCB                                        
031700                     SAP-SAPC-PCB.                                        
031800                                                                          
031900     EJECT                                                                
032000     PERFORM IMS-GET-MSG                                                  
032100     IF SEGMENT-FINNS                                                     
032200        PERFORM IMS-GN-MSG                                                
032300        PERFORM A-INIT                                                    
032400        PERFORM B-KONTROLL-OM-ORDER-FINNS                                 
032500        IF ALLT-OK                                                        
032600          PERFORM C-FORMELL-KONTROLL                                      
032700          IF ALLT-OK                                                      
032800            PERFORM D-LAS-KUNDREGISTER                                    
032900            IF ALLT-OK                                                    
033000              PERFORM E-LOGISK-KONTROLL                                   
033100              IF ALLT-OK                                                  
033200                PERFORM F-BESTAM-TRANSPORT                                
033300                IF ALLT-OK                                                
033400                  PERFORM H-BESTAM-ORDERNUMMER                            
033500                  PERFORM I-SKAPA-ORDERHUVUD                              
033600                END-IF                                                    
033700              END-IF                                                      
033800            END-IF                                                        
033900          END-IF                                                          
034000        END-IF                                                            
034100        IF W-IDTRANS = '4255'                                             
034200           PERFORM J-STARTA-W4T255X                                       
034300        ELSE                                                              
034400           PERFORM Z-FINIT                                                
034500        END-IF                                                            
034600     END-IF                                                               
034700     MOVE +0 TO RETURN-CODE                                               
034800     GOBACK                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 A-INIT SECTION.                                                          
035200                                                                          
035300     MOVE JA                   TO ALLT-SW                                 
035400                                                                          
035500     MOVE MSG-IDTRANS-1        TO W-IDTRANS                               
035600     IF W-IDTRANS = '4255'                                                
035700        PERFORM AA-SKAPA-4251-MID                                         
035800     ELSE                                                                 
035900        MOVE MSG-INDATA-MINUS-1-TRANSKOD                                  
036000                               TO MID-AREA                                
036100     END-IF                                                               
036200     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
036300                                                                          
036400*    LITE FIX OM NU NÅGOT PROGRAM INTE SKICKAR MED FLAGGORNA              
036500     IF MID-FLORDTIL NOT = JA                                             
036600        MOVE NEJ TO MID-FLORDTIL                                          
036700     END-IF                                                               
036800*^                                                                        
036900*    KAN EVENTUELLT TAS BORT NÄR ALLA ANROPANDE PROGRAM ÄR FIXADE         
037000     .                                                                    
037100     EJECT                                                                
037200 AA-SKAPA-4251-MID SECTION.                                               
037300                                                                          
037400     MOVE 4255-MID-IDSYSTEM     TO MID-IDSYSTEM                           
037500     MOVE 4255-MID-IDDISTR      TO MID-IDDISTR                            
037600     MOVE 4255-MID-IDKUNDNR     TO MID-IDKUNDNR                           
037700     MOVE 4255-MID-IDORDNR      TO MID-IDORDNR                            
037800     MOVE 4255-MID-KDORDKL      TO MID-KDORDKL                            
037900     MOVE SPACE                 TO MID-KDFRAKT                            
038000                                   MID-IDDC                               
038100                                   MID-TIRFS                              
038200     MOVE 4255-MID-BEKUNDRF-001 TO MID-BEKUNDRF                           
038300     MOVE SPACE                 TO MID-KDFAKTYP                           
038400     MOVE JA                    TO MID-FLRESTN                            
038500     MOVE SPACE                 TO MID-KDTPOTYP                           
038600                                   MID-TITPO                              
038700                                   MID-BELAGINS                           
038800                                   MID-BEGMT                              
038900                                   MID-ADGMT-GATA                         
039000                                   MID-ADGMT-PADR                         
039100                                   MID-BEBET                              
039200                                   MID-ADBET                              
039300                                   MID-KDROPACK                           
039400                                   MID-IDKONTO                            
039500                                   MID-IDANALYS                           
039600                                   MID-IDKST                              
039700                                   MID-IDSKYLT                            
039800                                   MID-BEVARREF                           
039900                                   MID-KDTULLVE                           
040000                                   MID-KDNOTES                            
040100                                   MID-FLAUTFAK                           
040200     MOVE NEJ                   TO MID-FLAUTPAC                           
040300                                   MID-FLEMBORD                           
040400                                   MID-FLOVRLEV                           
040500                                   MID-FLFORBI                            
040600     MOVE SPACE                 TO MID-IDKAMPRF                           
040700                                   MID-IDFTG                              
040800                                   MID-FLLSBOK                            
040900                                   MID-IDBILREG                           
041000                                   MID-IDVIN                              
041100                                   MID-IDCISNR                            
041110     MOVE ZERO                  TO MID-IDGROSS                            
041120     .                                                                    
041130     EJECT                                                                
041140                                                                          
041150 B-KONTROLL-OM-ORDER-FINNS SECTION.                                       
041160                                                                          
041170     IF MID-IDDISTR NUMERIC                                               
041180        MOVE MID-IDDISTR          TO W-IDDISTR                            
041190     END-IF                                                               
041200     IF MID-IDKUNDNR = SPACE                                              
041300        MOVE ZERO                 TO W-IDKUNDNR                           
041400     ELSE                                                                 
041500        IF MID-IDKUNDNR NUMERIC                                           
041600           MOVE MID-IDKUNDNR      TO W-IDKUNDNR                           
041700        END-IF                                                            
041800     END-IF                                                               
041900     IF MID-IDORDNR NUMERIC                                               
042000        MOVE MID-IDORDNR          TO W-IDORDNR                            
042100     END-IF                                                               
042200                                                                          
042300     IF MID-IDORDNR NUMERIC AND MID-IDORDNR > ZERO                        
042400*FIX FÖR ATT STOPPA VR-ORDER SOM FORTF. SKICKAS ???                       
042500*IBLAND SKICKAR VIPS ORDERNR > 99999                                      
042600       IF (MID-IDSYSTEM = 'VR' AND W-IDDISTR = 7512 OR 7531 OR            
042700                                               7552)        OR            
042800          W-IDORDNR > 99999                                               
042900          MOVE '063'        TO MSG-KOM-IDMFSMED                           
043000          MOVE '4'          TO MSG-KOM-KDSVAR                             
043100          MOVE NEJ          TO ALLT-SW                                    
043200       ELSE                                                               
043300*FIX-SLUT LASSI. TA BORT NÄR DU SER ATT VD HAR SLUTAT MED DESSA VR        
043400*OBS!! FIXEN HAR EN ENDIF LÄNGRE NER !!                                   
043500       PERFORM IMS-GU-ORQL-WDQ201                                         
043600                                                                          
043700         IF SEGMENT-FINNS                                                 
043800            IF (MID-KDTPOTYP = '1' AND OHUV-KDTPOTYP = +1)                
043900               MOVE OHUV-IDORDER TO W-IDORDER                             
044000               PERFORM IMS-GHU-ORQI-WDQ201                                
044100               MOVE NEJ        TO OHUV-FLKLAR                             
044200               PERFORM IMS-REPL-ORQI-WDQ201                               
044300                                                                          
044400               IF W-IDTRANS NOT = '4255'                                  
044500                  MOVE NEJ     TO ALLT-SW                                 
044600               END-IF                                                     
044700            ELSE                                                          
044800               MOVE ERR-ORDER-FINNS TO MSG-KOM-IDMFSMED                   
044900               MOVE '4'             TO MSG-KOM-KDSVAR                     
045000               MOVE NEJ        TO ALLT-SW                                 
045100            END-IF                                                        
045200         END-IF                                                           
045300       END-IF                                                             
045400     END-IF                                                               
045500     .                                                                    
045600     EJECT                                                                
045700 C-FORMELL-KONTROLL         SECTION.                                      
045800                                                                          
045900     MOVE SPACE                TO OHFK-KDPROTYP                           
046000     MOVE MID-IDSYSTEM         TO OHFK-IDSYSTEM                           
046100     IF MID-IDDISTR = SPACE                                               
046200        MOVE ALL '+'           TO OHFK-IDDISTR                            
046300     ELSE                                                                 
046400        MOVE MID-IDDISTR       TO OHFK-IDDISTR                            
046500     END-IF                                                               
046600                                                                          
046700     IF MID-IDKUNDNR = SPACE                                              
046800        MOVE ALL '+'           TO OHFK-IDKUNDNR                           
046900     ELSE                                                                 
047000        MOVE MID-IDKUNDNR      TO OHFK-IDKUNDNR                           
047100     END-IF                                                               
047200                                                                          
047300     IF MID-IDORDNR  = SPACE                                              
047400        MOVE ALL '+'           TO OHFK-IDORDNR                            
047500     ELSE                                                                 
047600        MOVE MID-IDORDNR(3:5)  TO OHFK-IDORDNR                            
047700     END-IF                                                               
047800                                                                          
047900     IF MID-KDORDKL  = SPACE                                              
048000        MOVE ALL '+'           TO OHFK-KDORDKL                            
048100     ELSE                                                                 
048200        MOVE MID-KDORDKL       TO OHFK-KDORDKL                            
048300     END-IF                                                               
048400                                                                          
048500     IF MID-KDFRAKT  = SPACE                                              
048600        MOVE ALL '+'           TO OHFK-KDFRAKT                            
048700     ELSE                                                                 
048800        MOVE MID-KDFRAKT       TO OHFK-KDFRAKT                            
048900     END-IF                                                               
049000                                                                          
049100     IF MID-FLRESTN  = SPACE                                              
049200        MOVE ALL '+'           TO OHFK-FLRESTN                            
049300     ELSE                                                                 
049400        MOVE MID-FLRESTN       TO OHFK-FLRESTN                            
049500     END-IF                                                               
049600                                                                          
049700     IF MID-FLRESTN = 'Y'                                                 
049800        MOVE JA                TO MID-FLRESTN                             
049900     END-IF                                                               
050000                                                                          
050100     IF MID-IDKONTO  = SPACE                                              
050200        MOVE ALL '+'           TO OHFK-IDKONTO                            
050300     ELSE                                                                 
050400        MOVE MID-IDKONTO       TO OHFK-IDKONTO                            
050500     END-IF                                                               
050600                                                                          
050700     IF MID-IDANALYS = SPACE                                              
050800        MOVE ALL '+'           TO OHFK-IDANALYS                           
050900     ELSE                                                                 
051000        MOVE MID-IDANALYS      TO OHFK-IDANALYS                           
051100     END-IF                                                               
051200                                                                          
051300     IF MID-IDKST    = SPACE                                              
051400        MOVE ALL '+'           TO OHFK-IDKST                              
051500     ELSE                                                                 
051600        MOVE MID-IDKST         TO OHFK-IDKST                              
051700     END-IF                                                               
051800                                                                          
051900     IF MID-IDFTG    = SPACE                                              
052000        MOVE ALL '+'           TO OHFK-IDFTG                              
052100     ELSE                                                                 
052200        MOVE MID-IDFTG         TO OHFK-IDFTG                              
052300     END-IF                                                               
052400                                                                          
052500     IF MID-IDDC     = SPACE                                              
052600       MOVE ALL '+'            TO OHFK-IDDC                               
052700     ELSE                                                                 
052800       MOVE MID-IDDC           TO OHFK-IDDC                               
052900     END-IF                                                               
053000                                                                          
053100     IF MID-KDFAKTYP = SPACE                                              
053200        MOVE ALL '+'           TO OHFK-KDFAKTYP                           
053300     ELSE                                                                 
053400        MOVE MID-KDFAKTYP      TO OHFK-KDFAKTYP                           
053500     END-IF                                                               
053600                                                                          
053700     IF MID-KDROPACK = SPACE                                              
053800        MOVE ALL '+'           TO OHFK-KDROPACK                           
053900     ELSE                                                                 
054000        MOVE MID-KDROPACK      TO OHFK-KDROPACK                           
054100     END-IF                                                               
054200                                                                          
054300     IF MID-KDTPOTYP = SPACE                                              
054400        MOVE ALL '+'           TO OHFK-KDTPOTYP                           
054500     ELSE                                                                 
054600        MOVE MID-KDTPOTYP      TO OHFK-KDTPOTYP                           
054700     END-IF                                                               
054800                                                                          
054900     IF MID-TITPO    = SPACE                                              
055000        MOVE ALL '+'           TO OHFK-TITPO                              
055100     ELSE                                                                 
055200        MOVE MID-TITPO         TO OHFK-TITPO                              
055300     END-IF                                                               
055400                                                                          
055500     ACCEPT OHFK-TIREGDAT      FROM DATE                                  
055600     ACCEPT OHFK-TIHHMM        FROM TIME                                  
055700     EJECT                                                                
055800     IF MID-TIRFS    = SPACE                                              
055900        MOVE ALL '+'           TO OHFK-TIRFSDAT                           
056000        MOVE ALL '+'           TO OHFK-TIRFSTID                           
056100     ELSE                                                                 
056200        MOVE MID-TIRFS         TO OHFK-TIRFSDAT                           
056300        MOVE ALL '+'           TO OHFK-TIRFSTID                           
056400     END-IF                                                               
056500                                                                          
056600     IF MID-KDTULLVE = SPACE                                              
056700        MOVE ALL '+'           TO OHFK-KDTULLVE                           
056800     ELSE                                                                 
056900        MOVE MID-KDTULLVE      TO OHFK-KDTULLVE                           
057000     END-IF                                                               
058000     IF MID-IDKAMPRF = SPACE                                              
058100        MOVE ALL '+'           TO OHFK-IDKAMPRF                           
058200     ELSE                                                                 
058300        MOVE MID-IDKAMPRF      TO OHFK-IDKAMPRF                           
058400     END-IF                                                               
058500                                                                          
058600     IF MID-IDSKYLT = SPACE                                               
058700        MOVE ALL '+'           TO OHFK-IDSKYLT                            
058800     ELSE                                                                 
058900        MOVE MID-IDSKYLT       TO OHFK-IDSKYLT                            
059000     END-IF                                                               
059100                                                                          
059200     IF MID-FLLSBOK = SPACE                                               
059300        MOVE '+'               TO OHFK-FLLSBOK                            
059400        MOVE JA                TO MID-FLLSBOK                             
059500     ELSE                                                                 
059600        MOVE MID-FLLSBOK       TO OHFK-FLLSBOK                            
059700     END-IF                                                               
059800                                                                          
059900     MOVE MID-FLAUTFAK         TO OHFK-FLAUTFAK                           
060000     MOVE MID-FLAUTPAC         TO OHFK-FLAUTPAC                           
060100     IF MID-IDSYSTEM = 'W603' OR 'W407' OR 'W216' OR                      
060110                       'W371' OR 'W37A'                                   
060120        MOVE JA                TO OHFK-FLORDSPE                           
060130     ELSE                                                                 
060140        MOVE NEJ               TO OHFK-FLORDSPE                           
060150     END-IF                                                               
060160                                                                          
060170     MOVE NEJ                  TO OHFK-FLFORBI                            
060180     MOVE NEJ                  TO OHFK-FLVORKO                            
060190     MOVE ALL '+'              TO OHFK-KDVRINFO                           
060200     IF MID-IDSYSTEM = 'OREL' AND MID-KDROPACK = 'L'                      
060300        MOVE MID-IDORDNR       TO OHFK-IDBIPREF                           
060400     ELSE                                                                 
060500        MOVE ALL '+'           TO OHFK-IDBIPREF                           
060600     END-IF                                                               
060700     CALL W411OHFK USING OHFK-W411OHFK                                    
060800     PERFORM CA-KOLLA-FEL-FK                                              
060900     .                                                                    
061000     EJECT                                                                
061100 CA-KOLLA-FEL-FK SECTION.                                                 
061200                                                                          
061300     IF OHFK-IDDISTR-OK = NEJ                                             
061400        MOVE ERR-FORM-IDDISTR-FEL TO MSG-KOM-IDMFSMED                     
061500        MOVE '4'                  TO MSG-KOM-KDSVAR                       
061600        MOVE NEJ                  TO ALLT-SW                              
061700     ELSE                                                                 
061800        MOVE OHFK-IDDISTR        TO W-IDDISTR                             
061900                                    TEST-IDDISTR                          
062000     END-IF                                                               
062100                                                                          
062200     IF OHFK-IDKUNDNR-OK = NEJ                                            
062300        MOVE ERR-FORM-IDKUNDNR-FEL TO MSG-KOM-IDMFSMED                    
062400        MOVE '4'                   TO MSG-KOM-KDSVAR                      
062500        MOVE NEJ                   TO ALLT-SW                             
062600     ELSE                                                                 
062700        IF MID-IDKUNDNR = ALL '+'                                         
062800           MOVE ZERO             TO W-IDKUNDNR                            
062900        ELSE                                                              
063000           MOVE OHFK-IDKUNDNR    TO W-IDKUNDNR                            
063100        END-IF                                                            
063200     END-IF                                                               
063300                                                                          
063400     IF OHFK-IDORDNR-OK = NEJ                                             
063500        MOVE ERR-FORM-IDORDNR-FEL TO MSG-KOM-IDMFSMED                     
063600        MOVE '4'                  TO MSG-KOM-KDSVAR                       
063700        MOVE NEJ                  TO ALLT-SW                              
063800     ELSE                                                                 
063900        IF MID-IDORDNR = ALL '+'                                          
064000           MOVE ZERO             TO W-IDORDNR                             
064100        ELSE                                                              
064200           MOVE OHFK-IDORDNR     TO W-IDORDNR                             
064300        END-IF                                                            
064400     END-IF                                                               
064500     EJECT                                                                
064600     IF OHFK-KDORDKL-OK     = NEJ OR                                      
064700        OHFK-KDFRAKT-OK     = NEJ OR                                      
064800        OHFK-FLRESTN-OK     = NEJ OR                                      
064900        OHFK-IDKONTO-OK     = NEJ OR                                      
065000        OHFK-IDANALYS-OK    = NEJ OR                                      
065100        OHFK-IDKST-OK       = NEJ OR                                      
065200        OHFK-IDFTG-OK       = NEJ OR                                      
065300        OHFK-KDFAKTYP-OK    = NEJ OR                                      
065400        OHFK-KDROPACK-OK    = NEJ OR                                      
065500        OHFK-KDTPOTYP-OK    = NEJ OR                                      
065600        OHFK-TITPO-OK       = NEJ OR                                      
065700        OHFK-TIRFSDAT-OK    = NEJ OR                                      
065800        OHFK-KDTULLVE-OK    = NEJ OR                                      
065900        OHFK-IDSKYLT-OK     = NEJ OR                                      
066000        OHFK-IDDC-OK        = NEJ OR                                      
066100        OHFK-FLLSBOK-OK     = NEJ OR                                      
066200        OHFK-IDKAMPRF-OK    = NEJ                                         
066300        EVALUATE TRUE                                                     
066400        WHEN OHFK-KDORDKL-OK     = NEJ                                    
066500          MOVE ERR-FORM-KDORDKL-FEL      TO MSG-KOM-IDMFSMED              
066600        WHEN OHFK-KDFRAKT-OK     = NEJ                                    
066700          MOVE ERR-FORM-KDFRAKT-FEL      TO MSG-KOM-IDMFSMED              
066800        WHEN OHFK-FLRESTN-OK     = NEJ                                    
066900          MOVE ERR-FORM-FLRESTN-FEL      TO MSG-KOM-IDMFSMED              
067000        WHEN OHFK-IDKONTO-OK     = NEJ                                    
067100          MOVE ERR-FORM-IDKONTO-FEL      TO MSG-KOM-IDMFSMED              
067200        WHEN OHFK-IDANALYS-OK    = NEJ                                    
067300          MOVE ERR-FORM-IDANALYS-FEL     TO MSG-KOM-IDMFSMED              
067400        WHEN OHFK-IDKST-OK       = NEJ                                    
067500          MOVE ERR-FORM-IDKST-FEL        TO MSG-KOM-IDMFSMED              
067600        WHEN OHFK-IDFTG-OK       = NEJ                                    
067700          MOVE ERR-FORM-IDFTG-FEL        TO MSG-KOM-IDMFSMED              
067800        WHEN OHFK-KDFAKTYP-OK    = NEJ                                    
067900          MOVE ERR-FORM-KDFAKTYP-FEL     TO MSG-KOM-IDMFSMED              
068000        WHEN OHFK-KDROPACK-OK    = NEJ                                    
068100          MOVE ERR-FORM-KDROPACK-FEL     TO MSG-KOM-IDMFSMED              
068200        WHEN OHFK-KDTPOTYP-OK    = NEJ                                    
068300          MOVE ERR-FORM-KDTPOTYP-FEL     TO MSG-KOM-IDMFSMED              
068400        WHEN OHFK-TITPO-OK       = NEJ                                    
068500          MOVE ERR-FORM-TITPO-FEL        TO MSG-KOM-IDMFSMED              
068600        WHEN OHFK-TIRFSDAT-OK    = NEJ                                    
068700          MOVE ERR-FORM-TIRFSDAT-FEL     TO MSG-KOM-IDMFSMED              
068800        WHEN OHFK-KDTULLVE-OK    = NEJ                                    
068900          MOVE ERR-FORM-KDTULLVE-FEL     TO MSG-KOM-IDMFSMED              
069000        WHEN OHFK-IDSKYLT-OK     = NEJ                                    
069100          MOVE ERR-FORM-IDSKYLT-FEL      TO MSG-KOM-IDMFSMED              
069200        WHEN OHFK-IDDC-OK        = NEJ                                    
069300          MOVE ERR-FORM-IDDC-FEL         TO MSG-KOM-IDMFSMED              
069400        WHEN OHFK-FLLSBOK-OK     = NEJ                                    
069500          MOVE ERR-FORM-FLLSBOK-FEL      TO MSG-KOM-IDMFSMED              
069600        WHEN OHFK-IDKAMPRF-OK    = NEJ                                    
069700          MOVE ERR-FORM-IDKAMPRF-FEL     TO MSG-KOM-IDMFSMED              
069800        END-EVALUATE                                                      
069900        MOVE '4'                         TO MSG-KOM-KDSVAR                
070000        MOVE NEJ                         TO ALLT-SW                       
070100     END-IF                                                               
070200                                                                          
070300*  OM DET ÄR DIST 778 (SVERIGE) SÅ SKALL MAN RENSA BORT FELAKTIGA         
070400*  ORDER FRÅN DISPATCHKÖN AUTOMATISKT E. 5 DAGAR. TIDIGARE                
070500*  RENSADES DESSA MANUELLT.  TL 030401                                    
070600                                                                          
070700*    AUTOMATRENSNING AV TACDIS-ORDER. DETTA INGÅR NUMERA I OVAN-          
070800*    STÅENDE IF-SATS. TL 030401                                           
070900*                                                                         
071000*    IF MID-IDSYSTEM = 'LDC '                                             
071100*      IF OHFK-TIRFSDAT-OK = NEJ                                          
071200*        MOVE 'R'                TO MSG-KOM-KDSVAR                        
071300*      END-IF                                                             
071400*    END-IF                                                               
071500     .                                                                    
071600     EJECT                                                                
071700 D-LAS-KUNDREGISTER  SECTION.                                             
071800                                                                          
071900     MOVE W-IDDISTR            TO KREG-IDDISTR                            
072000     MOVE W-IDKUNDNR           TO KREG-IDKUNDNR                           
072100     MOVE MID-IDSYSTEM         TO KREG-IDSYSTEM                           
072200     IF MID-KDTPOTYP > ZERO OR MID-IDKAMPRF > ZERO                        
072300        MOVE WS-CDC-11         TO KREG-IDDC-TVS                           
072400     ELSE                                                                 
072500        MOVE MID-IDDC          TO KREG-IDDC-TVS                           
072600     END-IF                                                               
072700     IF MID-KDFRAKT = SPACE                                               
072800        MOVE +0                TO KREG-KDFRAKT-IN                         
072900     ELSE                                                                 
073000        MOVE MID-KDFRAKT       TO WS-ALFA-2                               
073100        MOVE WS-NUM-2          TO KREG-KDFRAKT-IN                         
073200     END-IF                                                               
073300     MOVE MID-KDORDKL          TO WS-ALFA-1                               
073400     MOVE WS-NUM-1             TO KREG-KDORDKL                            
073500     MOVE SPACE                TO KREG-KDFAKTYP-IN                        
073600     MOVE NEJ                  TO KREG-FLVORKO                            
073700                                  KREG-FLVORFK                            
073800     EJECT                                                                
073900     CALL W411KREG USING KREG-W411KREG KREG-GMTA-PCB KREG-GMTB-PCB        
074000                                       KREG-GMTC-PCB KREG-BETC-PCB        
074100                                                                          
074200     IF KREG-KDKREDSP = '1'                                               
074300        MOVE ERR-KUND-SPAERRAD   TO MSG-KOM-IDMFSMED                      
074400        MOVE '4'                 TO MSG-KOM-KDSVAR                        
074500        MOVE NEJ                 TO ALLT-SW                               
074600     ELSE                                                                 
074700        IF KREG-IDVAT-OK = NEJ                                            
074800          MOVE ERR-MOMS-REGNR-FEL  TO MSG-KOM-IDMFSMED                    
074900          MOVE '4'                 TO MSG-KOM-KDSVAR                      
075000          MOVE NEJ                 TO ALLT-SW                             
075100        ELSE                                                              
075200          IF KREG-IDPARTNR-OK = NEJ                                       
075300            MOVE ERR-SAKNAS-KREG     TO MSG-KOM-IDMFSMED                  
075400*           MOVE ERR-SAKNAS-BET      TO MSG-KOM-IDMFSMED                  
075500            MOVE '4'                 TO MSG-KOM-KDSVAR                    
075600            MOVE NEJ                 TO ALLT-SW                           
075700          END-IF                                                          
075800        END-IF                                                            
075900     END-IF                                                               
076000                                                                          
076100     IF KREG-IDDISTR-OK  = NEJ OR                                         
076200        KREG-IDKUNDNR-OK = NEJ OR                                         
076300        KREG-IDDC-OK = NEJ OR                                             
076400        KREG-KDFRAKT-OK  = NEJ                                            
076500        MOVE ERR-SAKNAS-KREG     TO MSG-KOM-IDMFSMED                      
076600        MOVE '4'                 TO MSG-KOM-KDSVAR                        
076700        MOVE NEJ                 TO ALLT-SW                               
076800     END-IF                                                               
076900     IF KREG-KDTRPKAT = 'C'                                               
077000        MOVE '00000'             TO KREG-IDTRP                            
077100                                    KREG-IDTRP-ALT                        
077200     END-IF                                                               
077300     .                                                                    
077400     EJECT                                                                
077500 E-LOGISK-KONTROLL  SECTION.                                              
077600                                                                          
077700     MOVE MID-IDSYSTEM         TO OHLK-IDSYSTEM                           
077800     MOVE W-IDDISTR            TO OHLK-IDDISTR                            
077900     MOVE W-IDKUNDNR           TO OHLK-IDKUNDNR                           
078000     MOVE W-IDORDNR            TO OHLK-IDORDNR                            
078100     MOVE MID-KDORDKL          TO OHLK-KDORDKL                            
078200     MOVE KREG-IDDC            TO OHLK-IDDC                               
078300     MOVE SPACE                TO OHLK-IDDC-TVS                           
078400     MOVE SPACE                TO OHLK-SEC-KDSVAR                         
078500     IF MID-IDKONTO = SPACE                                               
078600        MOVE +0                TO OHLK-IDKONTO                            
078700     ELSE                                                                 
078800        MOVE MID-IDKONTO       TO WS-ALFA-10                              
078900        MOVE WS-NUM-10         TO OHLK-IDKONTO                            
079000     END-IF                                                               
079100                                                                          
079200     IF MID-IDANALYS = SPACE                                              
079300        MOVE SPACE             TO OHLK-IDANALYS                           
079400     ELSE                                                                 
079500        MOVE MID-IDANALYS      TO OHLK-IDANALYS                           
079600     END-IF                                                               
079700                                                                          
079800     IF MID-IDKST = SPACE                                                 
079900        MOVE SPACE             TO OHLK-IDKST                              
080000     ELSE                                                                 
080100        MOVE MID-IDKST         TO OHLK-IDKST                              
081100     END-IF                                                               
081200                                                                          
081300     IF MID-IDFTG = SPACE                                                 
081400        MOVE +0                TO OHLK-IDFTG                              
081500     ELSE                                                                 
081600        MOVE MID-IDFTG         TO WS-ALFA-2                               
081700        MOVE WS-NUM-2          TO OHLK-IDFTG                              
081800     END-IF                                                               
081900                                                                          
082000     IF MID-IDKAMPRF = SPACE                                              
082100        MOVE +0                TO OHLK-IDKAMPRF                           
082200     ELSE                                                                 
082300        MOVE MID-IDKAMPRF      TO WS-ALFA-7                               
082400        MOVE WS-NUM-7          TO OHLK-IDKAMPRF                           
082500     END-IF                                                               
082600     MOVE KREG-FLOKFAK-G       TO OHLK-FLOKFAK-G                          
082700     MOVE KREG-FLOKFAK-N       TO OHLK-FLOKFAK-N                          
082800     MOVE KREG-FLOKFAK-R       TO OHLK-FLOKFAK-R                          
082900     MOVE KREG-FLOKFAK-K       TO OHLK-FLOKFAK-K                          
083000     MOVE NEJ                  TO OHLK-FLORDSPE                           
083100     MOVE NEJ                  TO OHLK-FLVORKO                            
083200     IF MID-KDFAKTYP = SPACE                                              
083300        MOVE KREG-KDGENFAK     TO OHLK-KDFAKTYP                           
083400     ELSE                                                                 
083500        MOVE MID-KDFAKTYP      TO OHLK-KDFAKTYP                           
083600     END-IF                                                               
083700                                                                          
083800     IF MID-KDTPOTYP = SPACE OR '4'                                       
083900        MOVE +0                TO OHLK-KDTPOTYP                           
084000*VV SKICKAR IN TPO4 + KAMPANJREFERENS MEN EJ TPODATUM PÅ HUVUDET.         
084100*411OHLK KRÄVER DATUM OM TPOTYP = 4. DÄRFÖR LÅTSAS VI ATT TPOTYP=0        
084200*FÖR ATT FÅ IGENOM ORDERN. DATUMKONTROLLEN GÖRS SENARE PÅ RADEN.          
084300* TINA 050223                                                             
084400     ELSE                                                                 
084500        MOVE MID-KDTPOTYP      TO WS-ALFA-1                               
084600        MOVE WS-NUM-1          TO OHLK-KDTPOTYP                           
084700     END-IF                                                               
084800                                                                          
084900     IF MID-TITPO = SPACE                                                 
085000        MOVE +0                TO OHLK-TITPO                              
085100     ELSE                                                                 
085200        MOVE MID-TITPO         TO WS-ALFA-6                               
085300        MOVE WS-NUM-6          TO OHLK-TITPO                              
085400     END-IF                                                               
085500     EJECT                                                                
085600                                                                          
085700     CALL W411OHLK USING OHLK-W411OHLK OHLK-WDM2-PCB ORDN-XXKP-PCB        
085800                                       KREG-GMTA-PCB                      
085900                                       SAP-SAPC-PCB                       
086000                                       OHLK-WDB6-PCB                      
086100     IF (MID-IDSYSTEM = 'LYNK' OR 'POLE' OR 'ECOM' OR 'VOUI'              
086110                               OR 'TAD ' OR 'ACC '                        
086120                               OR 'APA ' OR 'APB '                        
086130                               OR 'APC ' OR 'APD '                        
086140                               OR 'APE ' OR 'APF '                        
086150                               OR 'APG ' OR 'APH '                        
086151                               OR 'API ' OR 'APJ')                        
086152        MOVE YES               TO OHLK-IDORDNR-OK                         
086153     END-IF                                                               
086154                                                                          
086155     IF OHLK-IDDISTR-OK     = NEJ OR                                      
086160        OHLK-IDORDNR-OK     = NEJ OR                                      
086170        OHLK-KDORDKL-OK     = NEJ OR                                      
086180        OHLK-IDKONTO-OK     = NEJ OR                                      
086190        OHLK-IDANALYS-OK    = NEJ OR                                      
086200        OHLK-IDKST-OK       = NEJ OR                                      
086300        OHLK-IDFTG-OK       = NEJ OR                                      
086400        OHLK-IDKAMPRF-OK    = NEJ OR                                      
086500        OHLK-KDFAKTYP-OK    = NEJ OR                                      
086600        OHLK-KDTPOTYP-OK    = NEJ OR                                      
086700        OHLK-TITPO-OK       = NEJ                                         
086800        EVALUATE TRUE                                                     
086900        WHEN OHLK-IDDISTR-OK  = NEJ                                       
087000          MOVE ERR-IDDISTR-FEL   TO MSG-KOM-IDMFSMED                      
087100        WHEN OHLK-IDORDNR-OK  = NEJ                                       
087200          MOVE ERR-IDORDNR-FEL   TO MSG-KOM-IDMFSMED                      
087300        WHEN OHLK-KDORDKL-OK  = NEJ                                       
087400          MOVE ERR-KDORDKL-FEL   TO MSG-KOM-IDMFSMED                      
087500        WHEN OHLK-IDKONTO-OK  = NEJ                                       
087600          MOVE ERR-IDKONTO-FEL   TO MSG-KOM-IDMFSMED                      
087700        WHEN OHLK-IDANALYS-OK = NEJ                                       
087800          MOVE ERR-IDANALYS-FEL   TO MSG-KOM-IDMFSMED                     
087900        WHEN OHLK-IDKST-OK    = NEJ                                       
088000          MOVE ERR-IDKST-FEL   TO MSG-KOM-IDMFSMED                        
088100        WHEN OHLK-IDFTG-OK    = NEJ                                       
088200          MOVE ERR-IDFTG-FEL   TO MSG-KOM-IDMFSMED                        
088300        WHEN OHLK-IDKAMPRF-OK = NEJ                                       
088400          MOVE ERR-IDKAMPRF-FEL   TO MSG-KOM-IDMFSMED                     
088500        WHEN OHLK-KDFAKTYP-OK = NEJ                                       
088600          MOVE ERR-KDFAKTYP-FEL   TO MSG-KOM-IDMFSMED                     
088700        WHEN OHLK-KDTPOTYP-OK = NEJ                                       
088800          MOVE ERR-KDTPOTYP-FEL   TO MSG-KOM-IDMFSMED                     
088900        WHEN OHLK-TITPO-OK    = NEJ                                       
089000          MOVE ERR-TITPO-FEL   TO MSG-KOM-IDMFSMED                        
089100        END-EVALUATE                                                      
089200        MOVE '4'                 TO MSG-KOM-KDSVAR                        
089300        MOVE NEJ                 TO ALLT-SW                               
089400     END-IF                                                               
089500     .                                                                    
089600     EJECT                                                                
089700 F-BESTAM-TRANSPORT SECTION.                                              
089800                                                                          
089900     MOVE MID-IDSYSTEM         TO TRAN-IDSYSTEM                           
090000*GK  FIX FÖR WEB UPLOAD PREPLANNED                                        
090100     IF MID-IDSYSTEM = 'XCEL' AND                                         
090200        MID-TIREPDAT > ZERO                                               
090300        MOVE 'LDC '            TO TRAN-IDSYSTEM                           
090400     END-IF                                                               
090500     MOVE KREG-IDTRP           TO TRAN-IDTRP                              
090600     MOVE OHLK-IDDC            TO TRAN-IDDC                               
090700     MOVE OHLK-KDORDKL         TO TRAN-KDORDKL                            
090800     MOVE KREG-KDTRPKAT        TO TRAN-KDTRPKAT                           
090810     IF OHLK-KDTPOTYP = +2                                                
090820        MOVE +0                TO TRAN-KDTPOTYP                           
090830     ELSE                                                                 
090840        MOVE OHLK-KDTPOTYP     TO TRAN-KDTPOTYP                           
090850     END-IF                                                               
090860     IF MID-IDSYSTEM = 'W603' OR 'W407' OR 'W371' OR 'W37A'               
090870        MOVE JA                TO TRAN-FLORDSPE                           
090880     ELSE                                                                 
090890        MOVE NEJ               TO TRAN-FLORDSPE                           
090900     END-IF                                                               
091000     MOVE MID-FLOVRLEV         TO TRAN-FLOVRLEV                           
091100     MOVE KREG-KVLEDTIM-0      TO TRAN-KVLEDTIM-0                         
091200     MOVE KREG-KVLEDTIM-1      TO TRAN-KVLEDTIM-1                         
091300     MOVE KREG-KVLEDTIM-2      TO TRAN-KVLEDTIM-2                         
091400     MOVE KREG-KVLEDTIM-3      TO TRAN-KVLEDTIM-3                         
091500     MOVE KREG-KVLEDTIM-4      TO TRAN-KVLEDTIM-4                         
091600     PERFORM FA-FIXA-LOKAL-TID                                            
091700     MOVE MSGI-TILOKDAT        TO TRAN-TIREGDAT                           
091800     MOVE MSGI-TILOKTID        TO TRAN-TIHHMM-REG                         
091900     IF MID-TIRFS = SPACE                                                 
092000       MOVE +0                 TO TRAN-TIRFS                              
092100     ELSE                                                                 
092200       MOVE MID-TIRFS          TO WS-TIRFS-DAT                            
092300       MOVE ZERO               TO WS-TIRFS-TID                            
092400       MOVE WS-TIRFS           TO TRAN-TIRFS                              
092500     END-IF                                                               
092600                                                                          
092700     CALL W411TRAN USING TRAN-W411TRAN TRAN-XXKB-PCB                      
092800                                                                          
092900     IF TRAN-KDSVAR = '1' OR '2' OR '3' OR '4'                            
093000        MOVE ERR-TRANSPORT-FEL  TO MSG-KOM-IDMFSMED                       
093100        MOVE '4'                TO MSG-KOM-KDSVAR                         
093200        MOVE NEJ                TO ALLT-SW                                
093300     END-IF                                                               
093400     .                                                                    
093500     EJECT                                                                
093600 FA-FIXA-LOKAL-TID SECTION.                                               
093700                                                                          
093800     MOVE ALL '+'              TO MSGI-WMSGINIT                           
093900     MOVE '013'                TO MSGI-KDCALL                             
094000     MOVE 'WIDDC   '           TO MSGI-IDUSER                             
094100     MOVE KREG-IDDC            TO MSGI-IDUSER(6:2)                        
094200     MOVE '4251'               TO MSGI-IDTRANS                            
094300     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
094400                                                                          
094500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
094600     .                                                                    
094700     EJECT                                                                
094800                                                                          
094900                                                                          
095000 H-BESTAM-ORDERNUMMER SECTION.                                            
095100                                                                          
095200     MOVE MID-IDSYSTEM         TO ORDN-IDSYSTEM                           
095300     MOVE W-IDDISTR            TO ORDN-IDDISTR                            
095400     MOVE W-IDKUNDNR           TO ORDN-IDKUNDNR                           
095500                                                                          
095600     IF MID-IDORDNR = SPACE                                               
095700        MOVE ZERO              TO ORDN-IDORDNR-IN                         
095800     ELSE                                                                 
095900        MOVE W-IDORDNR         TO ORDN-IDORDNR-IN                         
096000     END-IF                                                               
096100                                                                          
096200     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB                      
096300                         ORDN-ORQL-PCB ORDN-PROC-PCB ORDN-ORQI-PCB        
096400                                                                          
096500     MOVE ORDN-IDORDNR-UT      TO W-IDORDNR                               
096600     .                                                                    
096700     EJECT                                                                
096800 I-SKAPA-ORDERHUVUD SECTION.                                              
096900                                                                          
097000     PERFORM IA-REDIGERA-ORDERHUVUD                                       
097100     PERFORM IMS-ISRT-ORQI-WDQ201                                         
097200                                                                          
097300     PERFORM IB-REDIGERA-ARBETSTABELL                                     
097400     PERFORM IMS-ISRT-ORQI-WDQ212                                         
097500                                                                          
097600     IF MID-BEKUNDRF(1:7) = '9311 OC' OR 'NOAC OC'                        
097700*    TOMT HUVUD SOM LÄGGS UPP VID TVINGANDE TILLÄGG                       
097800        PERFORM IC-SKAPA-WDR6-TRANS                                       
097900     END-IF                                                               
098000     .                                                                    
098100     EJECT                                                                
098200                                                                          
098300 IA-REDIGERA-ORDERHUVUD SECTION.                                          
098400                                                                          
098500     MOVE KREG-IDDEPOT             TO OHUV-IDDEPOT                        
098600     MOVE KREG-IDROUTE             TO OHUV-IDROUTE                        
098700     MOVE KREG-IDZON               TO OHUV-IDZON                          
098800                                                                          
098900     MOVE ORDN-IDORDER-UT          TO OHUV-IDORDER                        
099000                                                                          
099100     IF MID-ADBET = SPACE AND                                             
099200        (MID-IDSYSTEM (1:3) NOT = 'LYN' AND                               
099300         MID-IDSYSTEM (1:3) NOT = 'POL' AND                               
099400         MID-IDSYSTEM (1:3) NOT = 'ECO' AND                               
099500         MID-IDSYSTEM (1:3) NOT = 'VOU' AND                               
099600         MID-IDSYSTEM (1:3) NOT = 'TAD' AND                               
099610         MID-IDSYSTEM (1:3) NOT = 'ACC' AND                               
099620         MID-IDSYSTEM (1:3) NOT = 'APA' AND                               
099630         MID-IDSYSTEM (1:3) NOT = 'APB' AND                               
099640         MID-IDSYSTEM (1:3) NOT = 'APC' AND                               
099650         MID-IDSYSTEM (1:3) NOT = 'APD' AND                               
099660         MID-IDSYSTEM (1:3) NOT = 'APE' AND                               
099670         MID-IDSYSTEM (1:3) NOT = 'APF' AND                               
099680         MID-IDSYSTEM (1:3) NOT = 'APG' AND                               
099690         MID-IDSYSTEM (1:3) NOT = 'APH' AND                               
099700         MID-IDSYSTEM (1:3) NOT = 'API' AND                               
099701         MID-IDSYSTEM (1:3) NOT = 'APJ')                                  
099702        MOVE KREG-ADBETRAD-1       TO OHUV-ADBETRAD-1                     
099800        MOVE KREG-ADBETRAD-2       TO OHUV-ADBETRAD-2                     
099900     ELSE                                                                 
100000        MOVE MID-ADBET             TO OHUV-ADBET                          
100100     END-IF                                                               
100200                                                                          
100300     IF MID-ADGMT-GATA = SPACE AND MID-ADGMT-PADR = SPACE                 
100400        MOVE KREG-ADGMT-GATA       TO OHUV-ADGMT-GATA                     
100500        MOVE KREG-ADGMT-PADR       TO OHUV-ADGMT-PADR                     
100600     ELSE                                                                 
100700        MOVE MID-ADGMT-GATA        TO OHUV-ADGMT-GATA                     
100800        MOVE MID-ADGMT-PADR        TO OHUV-ADGMT-PADR                     
100900     END-IF                                                               
101000     MOVE KREG-ADGMT-LAND          TO OHUV-ADGMT-LAND                     
101100                                                                          
101200     IF MID-BEBET = SPACE AND                                             
101210        (MID-IDSYSTEM (1:3) NOT = 'LYN' AND                               
101220         MID-IDSYSTEM (1:3) NOT = 'POL' AND                               
101221         MID-IDSYSTEM (1:3) NOT = 'ECO' AND                               
101222         MID-IDSYSTEM (1:3) NOT = 'VOU' AND                               
101223         MID-IDSYSTEM (1:3) NOT = 'TAD' AND                               
101224         MID-IDSYSTEM (1:3) NOT = 'ACC' AND                               
101225         MID-IDSYSTEM (1:3) NOT = 'APA' AND                               
101226         MID-IDSYSTEM (1:3) NOT = 'APB' AND                               
101227         MID-IDSYSTEM (1:3) NOT = 'APC' AND                               
101228         MID-IDSYSTEM (1:3) NOT = 'APD' AND                               
101229         MID-IDSYSTEM (1:3) NOT = 'APE' AND                               
101230         MID-IDSYSTEM (1:3) NOT = 'APF' AND                               
101240         MID-IDSYSTEM (1:3) NOT = 'APG' AND                               
101250         MID-IDSYSTEM (1:3) NOT = 'APH' AND                               
101260         MID-IDSYSTEM (1:3) NOT = 'API' AND                               
101261         MID-IDSYSTEM (1:3) NOT = 'APJ')                                  
101262        MOVE KREG-BEBETRAD-1       TO OHUV-BEBETRAD-1                     
101263        MOVE KREG-BEBETRAD-2       TO OHUV-BEBETRAD-2                     
101264     ELSE                                                                 
101265        MOVE MID-BEBET             TO OHUV-BEBET                          
101266     END-IF                                                               
101267                                                                          
101268     IF MID-BEGMT = SPACE                                                 
101269        MOVE KREG-BEGMT            TO OHUV-BEGMT                          
101270     ELSE                                                                 
101271* FOR BEGMT WE ARE MOVING ELEMENTARY INSTEAD OF GROUP                     
101272       IF MID-BEGMT-RAD1 = SPACE                                          
101280          MOVE KREG-BEGMT-RAD1     TO OHUV-BEGMT-RAD1                     
101290       ELSE                                                               
101300          MOVE MID-BEGMT-RAD1      TO OHUV-BEGMT-RAD1                     
101400       END-IF                                                             
101500       IF MID-BEGMT-RAD2 = SPACE                                          
101600          MOVE KREG-BEGMT-RAD2     TO OHUV-BEGMT-RAD2                     
101700       ELSE                                                               
101800          MOVE MID-BEGMT-RAD2      TO OHUV-BEGMT-RAD2                     
101900       END-IF                                                             
102000     END-IF                                                               
102100                                                                          
102200                                                                          
102300     MOVE MID-BELAGINS             TO OHUV-BELAGINS-DEL1                  
102400     MOVE MID-IDDEPT               TO OHUV-IDDEPT                         
102500     MOVE SPACE                    TO OHUV-BELAGINS-DEL2                  
102600     MOVE MID-BEVARREF             TO OHUV-BEVARREF                       
102700                                                                          
102800     IF MID-KDNOTES = '21'                                                
102900        MOVE MID-FLAUTFAK          TO OHUV-FLAUTFAK                       
103000     ELSE                                                                 
103100        IF MID-FLAUTFAK = SPACE                                           
103200           IF DIST03-SVERIGE-EJ-778                                       
103300              AND NOT DIST03-EJ-AUTFAK                                    
103400              AND NOT DIST03-S                                            
103500              MOVE JA              TO OHUV-FLAUTFAK                       
103600           ELSE                                                           
103700              MOVE NEJ             TO OHUV-FLAUTFAK                       
103800           END-IF                                                         
103900        ELSE                                                              
104000           MOVE MID-FLAUTFAK       TO OHUV-FLAUTFAK                       
104100        END-IF                                                            
104200     END-IF                                                               
104300                                                                          
104400     MOVE MID-FLAUTPAC             TO OHUV-FLAUTPAC                       
104500     MOVE NEJ                      TO OHUV-FLBORT                         
104600     MOVE MID-FLEMBORD             TO OHUV-FLEMBORD                       
104700     IF MID-IDSYSTEM = 'REFB'                                             
104800        MOVE JA                    TO OHUV-FLFORBI                        
104900     ELSE                                                                 
105000        MOVE MID-FLFORBI           TO OHUV-FLFORBI                        
105100     END-IF                                                               
105200                                                                          
105300     MOVE MID-FLLSBOK              TO OHUV-FLLSBOK                        
105400     MOVE JA                       TO OHUV-FLOBTRAN                       
105500                                                                          
105600     IF MID-IDSYSTEM = 'W603' OR 'W407' OR 'W216' OR                      
105700                       'W371' OR 'W37A'                                   
105800        MOVE JA                    TO OHUV-FLORDSPE                       
105900     ELSE                                                                 
106000        MOVE NEJ                   TO OHUV-FLORDSPE                       
106100     END-IF                                                               
106200                                                                          
106300     MOVE MID-FLOVRLEV             TO OHUV-FLOVRLEV                       
106400                                                                          
106500     MOVE OHLK-KDORDKL             TO OHUV-KDORDKL                        
106600     IF OHUV-KDORDKL = +0 OR OHUV-FLORDSPE = JA                           
106700        MOVE NEJ                   TO WS-FLRESTN                          
106800     ELSE                                                                 
106900       IF MID-FLRESTN = SPACE                                             
107000          MOVE KREG-FLRESTN        TO WS-FLRESTN                          
107100       ELSE                                                               
107200          MOVE MID-FLRESTN         TO WS-FLRESTN                          
107300       END-IF                                                             
107400       IF OHUV-FLRESTN = YES                                              
107500          MOVE JA                  TO WS-FLRESTN                          
107600       END-IF                                                             
107700     END-IF                                                               
107800     MOVE WS-FLRESTN               TO OHUV-FLRESTN                        
107900                                                                          
108000     MOVE KREG-FLPRELRO            TO OHUV-FLPRELRO                       
108100     MOVE KREG-FLPRERS             TO OHUV-FLPRERS                        
108200     MOVE NEJ                      TO OHUV-FLVORKO                        
108300                                                                          
108400     IF MID-IDSYSTEM = 'OREL' AND MID-KDROPACK = 'L'                      
108500        MOVE MID-IDORDNR           TO OHUV-IDBIPREF                       
108600     ELSE                                                                 
108700       MOVE SPACE                  TO OHUV-IDBIPREF                       
108800     END-IF                                                               
108900                                                                          
109000     MOVE KREG-IDDC                TO OHUV-IDDC-PRIM                      
109100                                                                          
109200     MOVE KREG-IDDC-TVS            TO OHUV-IDDC-TVS                       
109600     IF MID-IDSYSTEM = 'SOFT'                                             
109700       MOVE JA                     TO OHUV-FLORDSPE                       
109800                                      OHUV-FLAUTPAC                       
109900       MOVE JA                     TO OHUV-FLAUTFAK                       
110000       MOVE NEJ                    TO OHUV-FLLSBOK                        
110100       MOVE WS-CDC-11              TO OHUV-IDDC-TVS                       
110200     END-IF                                                               
110300*TL FIX FÖR VOLYMTESTER                                                   
110400*    IF MID-IDSYSTEM = 'OVR '                                             
110500*      MOVE JA TO OHUV-FLORDSPE                                           
110600*      MOVE JA TO OHUV-FLAUTFAK                                           
110700*      MOVE JA TO OHUV-FLAUTPAC                                           
110800*    END-IF                                                               
110900*SLUT                                                                     
111000     MOVE OHLK-IDFTG               TO OHUV-IDFTG                          
111100     MOVE OHLK-IDDISTR             TO OHUV-IDDISTR                        
111200     MOVE OHLK-IDKUNDNR            TO OHUV-IDKUNDNR                       
111300                                                                          
111400     IF MID-IDORDNR = SPACE OR '0000000'                                  
111500        MOVE ORDN-IDORDNR-UT       TO OHUV-IDKUNDRF                       
111600     ELSE                                                                 
111700        MOVE OHLK-IDORDNR          TO OHUV-IDKUNDRF                       
111800     END-IF                                                               
111900                                                                          
112000     MOVE OHLK-IDKAMPRF            TO OHUV-IDKAMPRF                       
112100     MOVE OHLK-IDKONTO             TO OHUV-IDKONTO                        
112200     MOVE OHLK-IDANALYS            TO OHUV-IDANALYS                       
112300     MOVE OHLK-IDKST               TO OHUV-IDKST                          
112400     MOVE KREG-IDRFTAB             TO OHUV-IDRFTAB                        
112500                                                                          
112600     IF MID-IDSKYLT = SPACE                                               
112700        MOVE KREG-IDSKYLT          TO OHUV-IDSKYLT                        
112800     ELSE                                                                 
112900        MOVE MID-IDSKYLT           TO OHUV-IDSKYLT                        
113000     END-IF                                                               
113100                                                                          
113200     MOVE MID-IDSYSTEM             TO OHUV-IDSYSTEM                       
113300*GK  FIX FÖR WEB UPLOAD PREPLANNED                                        
113400     IF MID-IDSYSTEM = 'XCEL' AND                                         
113500        MID-TIREPDAT > ZERO                                               
113510        MOVE 'LDC '                TO OHUV-IDSYSTEM                       
113520     END-IF                                                               
113530     MOVE SPACE                    TO OHUV-IDUSER                         
113540                                                                          
113550     IF MID-KDFAKTYP = SPACE                                              
113560        MOVE KREG-KDGENFAK         TO OHUV-KDFAKTYP                       
113570     ELSE                                                                 
113580        MOVE MID-KDFAKTYP          TO OHUV-KDFAKTYP                       
113590     END-IF                                                               
113600                                                                          
113700     IF KREG-KDORDING = +3                                                
113800       MOVE KREG-KDORDING          TO OHUV-KDORDING                       
113900     ELSE                                                                 
114000       IF OHLK-KDTPOTYP > +0 OR OHLK-IDKAMPRF > +0                        
114100         MOVE +2                   TO OHUV-KDORDING                       
114200       ELSE                                                               
114300         IF (MID-IDSYSTEM = 'LDCB' OR 'LYNB' OR 'ECOB' OR 'VOUB'          
114400                                   OR 'TADB' OR 'ACCB' OR 'APAB'          
114500                                   OR 'APBB' OR 'APCB' OR 'APDB'          
114600                                   OR 'APEB' OR 'APFB' OR 'APGB'          
114601                                   OR 'APHB' OR 'APIB' OR 'APJB')         
114602            MOVE +3                TO OHUV-KDORDING                       
114603         ELSE                                                             
114700            MOVE KREG-KDORDING     TO OHUV-KDORDING                       
114800         END-IF                                                           
114900       END-IF                                                             
115000     END-IF                                                               
115100                                                                          
115300     IF MID-BEKUNDRF(1:7) = '9311 OC'  OR 'NOAC OC'                       
115500        MOVE MID-BEKUNDRF(6:10)    TO OHUV-BEKUNDRF                       
115600        MOVE NEJ                   TO OHUV-FLORDTIL                       
115700        MOVE JA                    TO OHUV-FLKLAR                         
115800     ELSE                                                                 
116000        IF MID-BEVARREF = 'TPO2REL'                                       
116100           MOVE 'DL REL'           TO OHUV-BEKUNDRF                       
116200           MOVE 'OVR '             TO OHUV-IDSYSTEM                       
116300        ELSE                                                              
116400           MOVE MID-BEKUNDRF       TO OHUV-BEKUNDRF                       
116500        END-IF                                                            
116600        MOVE MID-FLORDTIL          TO OHUV-FLORDTIL                       
116700                                                                          
116800        IF W-IDTRANS = '4255'                                             
116900           MOVE JA                 TO OHUV-FLKLAR                         
117000        ELSE                                                              
117100           MOVE NEJ                TO OHUV-FLKLAR                         
117200        END-IF                                                            
117300     END-IF                                                               
117400     MOVE OHLK-KDTPOTYP            TO OHUV-KDTPOTYP                       
117500                                                                          
117600     IF MID-KDTULLVE = SPACE                                              
117700        MOVE KREG-KDTULLVE         TO OHUV-KDTULLVE                       
117800     ELSE                                                                 
117900        MOVE MID-KDTULLVE          TO OHUV-KDTULLVE                       
118000     END-IF                                                               
118100                                                                          
118200     IF OHUV-FLOVRLEV = JA                                                
118300       MOVE +2                     TO OHUV-KDVRINFO                       
118400     ELSE                                                                 
118500       MOVE +0                     TO OHUV-KDVRINFO                       
118600     END-IF                                                               
118700                                                                          
118800     MOVE KREG-KVDAGAR-DOW         TO OHUV-KVDAGAR-DOW                    
118900     MOVE KREG-RESLATT             TO OHUV-RESLATT                        
119000     MOVE MSGI-TILOKDAT            TO OHUV-TIREGDAT                       
119100                                      WS-TIREGDAT-9KOMPL                  
119200     MOVE MSGI-TILOKTID            TO WS-TIHHMM                           
119300     MOVE WS-TIHHMMSS              TO OHUV-TIREGTID                       
119400     MOVE +0                       TO OHUV-TIREGDAT-STO                   
119500     MOVE +0                       TO OHUV-TIREGTID-STO                   
119600                                                                          
119700     IF MID-TITPO = SPACE                                                 
119800        MOVE +0                    TO OHUV-TITPO                          
119900     ELSE                                                                 
120000        MOVE OHLK-TITPO            TO OHUV-TITPO                          
120100     END-IF                                                               
120200     MOVE MID-KDORDTYP-LDC         TO OHUV-KDORDTYP-LDC                   
120300     MOVE MID-TIREPDAT             TO OHUV-TIREPDAT                       
120400     COMPUTE OHUV-TIREGDAT-9KOMPL = 9999999 - WS-TIREGDAT-9KOMPL          
120500     MOVE ZERO                     TO OHUV-KVORDTIL                       
120600     MOVE SPACE                    TO OHUV-IDLEVNR-EJLS                   
120700     MOVE NEJ                      TO OHUV-FLSOFT                         
120800     MOVE NEJ                      TO OHUV-FLVORFK                        
120900     IF OHUV-IDSYSTEM = 'SOFT'                                            
121000        MOVE JA                    TO OHUV-FLSOFT                         
121100     END-IF                                                               
121200     IF MID-IDGROSS = SPACE                                               
121300       MOVE ZERO                   TO OHUV-IDGROSS                        
121400     ELSE                                                                 
121500       MOVE MID-IDGROSS            TO OHUV-IDGROSS                        
121600     END-IF                                                               
121700     MOVE MID-IDBILREG             TO OHUV-IDBILREG                       
121800     MOVE MID-IDVIN                TO OHUV-IDVIN                          
121900     MOVE MID-IDCISNR              TO OHUV-IDCISNR                        
122000     .                                                                    
122100     EJECT                                                                
122200                                                                          
122300 IB-REDIGERA-ARBETSTABELL SECTION.                                        
122400                                                                          
122500     MOVE KREG-IDDC                TO ARB-IDDC                            
122600     MOVE KREG-BEGMRK              TO ARB-BEGMRK                          
122700                                                                          
122800     MOVE NEJ                      TO ARB-FLODELUT                        
122900     MOVE +0                       TO ARB-IDRADNR-SISTA                   
123000     MOVE KREG-IDTRP               TO ARB-IDTRP                           
123100     MOVE KREG-IDTRP-ALT           TO ARB-IDTRP-ALT                       
123200     MOVE +0                       TO ARB-IDPLKLST-SISTA                  
123300     MOVE KREG-KDFDKRAV            TO ARB-KDFDKRAV                        
123400                                                                          
123500     IF MID-KDFRAKT = SPACE                                               
123600        MOVE KREG-KDFRAKT          TO ARB-KDFRAKT                         
123700     ELSE                                                                 
123800        MOVE KREG-KDFRAKT-IN       TO ARB-KDFRAKT                         
123900     END-IF                                                               
124000                                                                          
124100     IF MID-IDSYSTEM = 'OREL' AND MID-KDROPACK = 'L'                      
124200       MOVE MID-KDROPACK          TO ARB-KDROPACK                         
124300     ELSE                                                                 
124400       IF OHUV-FLRESTN = NEJ                                              
124500         MOVE ZERO               TO ARB-KDROPACK                          
124600       ELSE                                                               
124700         IF MID-KDROPACK = SPACE                                          
124710           IF (OHUV-BEGMT        EQUAL KREG-BEGMT                         
124720           AND OHUV-ADGMT-GATA   EQUAL KREG-ADGMT-GATA                    
124730           AND OHUV-ADGMT-PADR   EQUAL KREG-ADGMT-PADR)                   
124731*>>>> RIGHT NOW WE TURN OFF AUTOMATIC BIPACK 3 FOR VIPS                   
124732           OR  OHUV-IDSYSTEM     EQUAL 'VIPS'                             
124733*>>>> RIGHT NOW WE TURN OFF AUTOMATIC BIPACK 3 FOR VIPS                   
124734             MOVE KREG-KDROPACK   TO ARB-KDROPACK                         
124735           ELSE                                                           
124736             MOVE '3'             TO ARB-KDROPACK                         
124737           END-IF                                                         
124738         ELSE                                                             
124739           MOVE MID-KDROPACK      TO ARB-KDROPACK                         
124740         END-IF                                                           
124750       END-IF                                                             
124760     END-IF                                                               
124770     IF MID-BEKUNDRF(1:7) = '9311 OC'  OR 'NOAC OC'                       
124780        MOVE ZERO                TO ARB-KDROPACK                          
124790     END-IF                                                               
124800                                                                          
124900     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
125000     IF (DIST15-NA      AND (ARB-KDFRAKT > +11 AND < +20)) OR             
125100        (DIST15-ENGLAND AND OHUV-KDORDKL = +1                             
125200                        AND ARB-KDFRAKT = +16) OR                         
125300        (DIST15-JAPAN   AND OHUV-KDORDKL = +1                             
125400                        AND (ARB-KDFRAKT = +15 OR +25)) OR                
125500        (DIST15-AUSTRALIEN AND OHUV-KDORDKL = +1                          
125600                        AND ARB-KDFRAKT = +5)                             
125700       MOVE +0                   TO ARB-KDROPACK                          
125800     END-IF                                                               
125900                                                                          
126000     MOVE KREG-KDTRPKAT            TO ARB-KDTRPKAT                        
126100                                                                          
126200     MOVE +0                       TO ARB-KVSEMBRA                        
126300                                                                          
126400     IF MID-IDSYSTEM = 'W603' OR 'W407' OR 'W371' OR 'W37A'               
126500       MOVE MSGI-TILOKDAT          TO WS-TIRFS-DAT                        
126600       MOVE MSGI-TILOKTID          TO WS-TIRFS-TID                        
126700       MOVE WS-TIRFS               TO ARB-TIRFS                           
126800       MOVE ZERO                   TO ARB-DATRPAVD                        
126900                                      ARB-TIHHMM                          
127000     ELSE                                                                 
127100       MOVE TRAN-TIRFS             TO ARB-TIRFS                           
127200       MOVE TRAN-TIHHMM            TO ARB-TIHHMM                          
127300       MOVE TRAN-TIAAMMDD          TO ARB-DATRPAVD                        
127400       IF TRAN-TIAAMMDD NOT = ZERO                                        
127500         IF TRAN-TIAAMMDD < 500000                                        
127600           MOVE 20                 TO ARB-DATRPAVD (1:2)                  
127700         ELSE                                                             
127800           IF TRAN-TIAAMMDD < 999999                                      
127900             MOVE 19               TO ARB-DATRPAVD (1:2)                  
128000           ELSE                                                           
128100             MOVE 99999999         TO ARB-DATRPAVD                        
128200           END-IF                                                         
128300         END-IF                                                           
128400       END-IF                                                             
128500     END-IF                                                               
128600                                                                          
128700     MOVE SPACE                    TO ARB-KDORDSTA-O                      
128800     IF MID-BEKUNDRF(1:7) = '9311 OC'  OR 'NOAC OC'                       
128900        MOVE 'R'                   TO ARB-KDORDSTA                        
129000     ELSE                                                                 
129100        MOVE 'E'                   TO ARB-KDORDSTA                        
129200     END-IF                                                               
130000     .                                                                    
130100     EJECT                                                                
130200 IC-SKAPA-WDR6-TRANS             SECTION.                                 
130300                                                                          
130400     MOVE IDPGM                TO  FIL-IDPGM                              
130500     ACCEPT FIL-TIREGDAT     FROM  DATE                                   
130600     ACCEPT FIL-TIKLOCK      FROM  TIME                                   
130700     MOVE ZERO                 TO  FIL-IDSEKVNR                           
130800     MOVE 'W414'               TO  FIL-CT-IDSYSTEM                        
130900     MOVE 'A'                  TO  FIL-CT-IDVTYP                          
131000     MOVE '200'                TO  FIL-CT-IDPTYP                          
131100     MOVE ZERO                 TO  FIL-IDSEKVNR                           
131200     MOVE SPACE                TO  FIL-WDR601-DATA                        
131300     MOVE OHUV-BEKUNDRF        TO  200-BEKUNDRF                           
131400     MOVE OHUV-BEVARREF        TO  200-BEVARREF                           
131500     MOVE OHUV-FLRESTN         TO  200-FLRESTN                            
131600     MOVE OHUV-IDDISTR         TO  200-IDDISTR                            
131700     MOVE OHUV-IDKONTO         TO  200-IDKONTO                            
131800     MOVE OHUV-IDKST           TO  200-IDKST                              
131900     MOVE OHUV-IDKUNDNR        TO  200-IDKUNDNR                           
132000     MOVE OHUV-IDKUNDRF        TO  200-IDKUNDRF                           
132100     MOVE OHUV-IDORDER         TO  200-IDORDER                            
132200     MOVE OHUV-IDSKYLT         TO  200-IDSKYLT                            
132300     MOVE OHUV-IDDC-PRIM       TO  200-IDDC                               
132400     MOVE OHUV-KDFAKTYP        TO  200-KDFAKTYP                           
132500     MOVE ARB-KDFRAKT          TO  200-KDFRAKT                            
132600     MOVE OHUV-KDORDKL         TO  200-KDORDKL                            
132700     MOVE ARB-KDROPACK         TO  200-KDROPACK                           
132800     MOVE OHUV-KDTULLVE        TO  200-KDTULLVE                           
132900     MOVE OHUV-TIREGDAT        TO  200-TIREGDAT                           
133000     MOVE ARB-TIRFS            TO  200-TIRFS                              
133100     MOVE ARB-DATRPAVD         TO  200-TIAAMMDD                           
133200     MOVE ARB-TIHHMM           TO  200-TIHHMM                             
133300                                                                          
133400     ADD +1                    TO  FIL-IDSEKVNR                           
133500     MOVE '200'                TO  FIL-CT-IDPTYP                          
133600     PERFORM IMS-ISRT-FILA01                                              
133700     PERFORM UNTIL SEGMENT-FINNS                                          
133800        ADD +1 TO FIL-IDSEKVNR                                            
133900        PERFORM IMS-ISRT-FILA01                                           
134000     END-PERFORM                                                          
134100     .                                                                    
134200     EJECT                                                                
134300                                                                          
134400 J-STARTA-W4T255X SECTION.                                                
134500                                                                          
134600     MOVE 'W4T255X '           TO MSG-KDTRANS-1                           
134700     MOVE '4251'               TO MSG-IDTRANS-1                           
134800     MOVE MSG-KOM-IDMFSMED     TO 4255-MID-IDMFSMED                       
134900                                                                          
135000     PERFORM IMS-INSERT-4255-MSG                                          
135100     .                                                                    
135200     EJECT                                                                
135300 Z-FINIT SECTION.                                                         
135400                                                                          
135500*    SKRIV FEL/KLAR MEDDELANDE TILL MPP DISPATCHERN                       
135600     IF MSG-KOM-IDMFSMED = SPACE                                          
135700        MOVE OK-BEHANDLAD      TO MSG-KOM-IDMFSMED                        
135800     END-IF                                                               
135900     PERFORM IMS-INSERT-DISP-MSG                                          
136000     .                                                                    
136100     EJECT                                                                
136200* --- IMS SEKTIONER ---                                                   
136300     SKIP1                                                                
136400 IMS-GET-MSG SECTION.                                                     
136500                                                                          
136600     MOVE '  QC' TO GODK-STATUSKODER                                      
136700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
136800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136900     PERFORM IMS-STATUSKONTROLL                                           
137000     .                                                                    
137100     SKIP2                                                                
137200 IMS-GN-MSG SECTION.                                                      
137300                                                                          
137400     MOVE '  '   TO GODK-STATUSKODER                                      
137500     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
137600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
137700     PERFORM IMS-STATUSKONTROLL                                           
137800     .                                                                    
137900     SKIP2                                                                
138000 IMS-INSERT-DISP-MSG SECTION.                                             
138100                                                                          
138200     MOVE SPACE TO GODK-STATUSKODER                                       
138300     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
138400     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
138500     PERFORM IMS-STATUSKONTROLL                                           
138600     .                                                                    
138700     SKIP2                                                                
138800 IMS-INSERT-4255-MSG SECTION.                                             
138900                                                                          
139000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
139100     MOVE SPACE TO GODK-STATUSKODER                                       
139200     CALL CBLTDLI USING ISRT 4255-PCB MSG-IO-AREA                         
139300     MOVE 4255-STATUS-CODE TO STATUS-WS                                   
139400     PERFORM IMS-STATUSKONTROLL                                           
139500     CALL CBLTDLI USING ISRT 4255-PCB KOM-IO-AREA                         
139600     MOVE 4255-STATUS-CODE TO STATUS-WS                                   
139700     PERFORM IMS-STATUSKONTROLL                                           
139800     .                                                                    
139900     EJECT                                                                
140000 IMS-GU-ORQL-WDQ201 SECTION.                                              
140100                                                                          
140200     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
140300          DELIMITED BY SIZE INTO SSA1                                     
140400     MOVE '  GE'               TO GODK-STATUSKODER                        
140500     CALL CBLTDLI USING GU ORQL-PCB DLI-IO-AREA-OHUV SSA1                 
140600     MOVE ORQL-STATUS-CODE    TO STATUS-WS                                
140700     PERFORM IMS-STATUSKONTROLL                                           
140800     .                                                                    
140900     SKIP2                                                                
141000 IMS-GHU-ORQI-WDQ201 SECTION.                                             
141100                                                                          
141200     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
141300          DELIMITED BY SIZE INTO SSA1                                     
141400     MOVE '    '               TO GODK-STATUSKODER                        
141500     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA-OHUV SSA1                
141600     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
141700     PERFORM IMS-STATUSKONTROLL                                           
141800     .                                                                    
141900     SKIP2                                                                
142000 IMS-ISRT-ORQI-WDQ201 SECTION.                                            
142100                                                                          
142200     MOVE 'WLORQI01 '          TO SSA1                                    
142300     MOVE '    '               TO GODK-STATUSKODER                        
142400     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-OHUV SSA1               
142500     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
142600     PERFORM IMS-STATUSKONTROLL                                           
142700     .                                                                    
142800     SKIP2                                                                
142900 IMS-REPL-ORQI-WDQ201 SECTION.                                            
143000                                                                          
143100     MOVE '    '               TO GODK-STATUSKODER                        
143200     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-OHUV                    
143300     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
143400     PERFORM IMS-STATUSKONTROLL                                           
143500     .                                                                    
143600     EJECT                                                                
143700 IMS-ISRT-ORQI-WDQ212 SECTION.                                            
143800                                                                          
143900     MOVE 'WLORQI12 '          TO SSA1                                    
144000     MOVE '    '               TO GODK-STATUSKODER                        
144100     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-ARB SSA1                
144200     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
144300     PERFORM IMS-STATUSKONTROLL                                           
144400     .                                                                    
144500     SKIP2                                                                
144600 IMS-ISRT-FILA01            SECTION.                                      
144700     MOVE   'WLFILA01'         TO SSA1                                    
144800     MOVE '  II' TO GODK-STATUSKODER                                      
144900     CALL  CBLTDLI  USING ISRT FILA-PCB DLI-IO-AREA2 SSA1                 
145000     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
145100     PERFORM IMS-STATUSKONTROLL                                           
145200     .                                                                    
145300                                                                          
145400     SKIP3                                                                
145500 IMS-STATUSKONTROLL SECTION.                                              
145600                                                                          
145700     SET STATUS-IX TO 1                                                   
145800     SEARCH GODK-STATUS                                                   
145900       AT END CALL FELLOG                                                 
146000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
146100     END-SEARCH                                                           
146200     .                                                                    
