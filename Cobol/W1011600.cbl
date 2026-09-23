000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1011600.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   APRIL 1988.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        REGISTRERING NY ARTIKEL.                                         
001100*        VÄCKNING GAMMAL ARTIKEL.                                         
001200*                                                                         
001300*    ÄNDRINGAR:                                                           
001400*        ÄT JAN 93. TILLAGT GODKÄNDA KDPRODSL 21-29 (EJ 22-23)            
001500*                                         CARPACK                         
001600*                       - TIFINLV TILLÅTS BAKÅT I TIDEN                   
001700*        ÄT FEB 93. YTBEH.KOD SKALL VARA 2-STÄLLIG, OCH                   
001800*                   EJ HA VÄRDEMÄSSIG SPÄRR.                              
001900*        ÄT NOV 93.                                                       
002000                                                                          
002100*        ÄT SOMMAREN 14 TISOP TILLAGT                                     
002200                                                                          
002300*        ÄT FEB 15.                                                       
002400*           CCID 10249871 - RESTRICTIONS ON PRODUCTGROUP                  
002500                                                                          
002600*       ******************************************************            
002700*       * X-TRANSAR  KAN KOMMA TILL DETTA PGM OCH            *            
002800*       * KOMMER FRÅN DISPATCHERN SOM FÅR INDATA FRÅN RUTIN  *            
002900*       * W100B1A. "MIDDAR" SKAPAS DÄR I PGM W1116100.       *            
003000*       * NÄR KONTROLLREGLER MOT INDATA ÄNDRAS I 1116, V.G.  *            
003100*       * UPPDATERA ÄVEN W1116100.                           *            
003200*       ******************************************************            
003300*                                                                         
003400*    INDATA.                                                              
003500*        TRANSAKTION: W1T116                                              
003600*                     W1T116U                                             
003700*                     W1T116X                                             
003800*        MID:         W1I11601                                            
003900*                                                                         
004000*    UTDATA.                                                              
004100*        MOD:         W1O11601                                            
004200*        TRANSAKTION: W0T693X                                             
004300*                                                                         
004400*    SUBPROGRAM:      CBLTDLI                                             
004500*                     FELLOG                                              
004600*                     WDATKONV                                            
004700*                     WREVERSE                                            
004800*                     W009REDU                                            
004900*                     WKPSKONV                                            
005000*                     W005INIT                                            
005100     EJECT                                                                
005200 ENVIRONMENT DIVISION.                                                    
005300     SKIP3                                                                
005400 DATA DIVISION.                                                           
005500     SKIP3                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700*    -COPY WY2000W3                                                       
005800     SKIP3                                                                
005900*    -COPY WY2000W2                                                       
006000     SKIP3                                                                
006100*    -COPY WY2000W1                                                       
006200     SKIP3                                                                
006300 77  PROGRAM-NAMN                PIC X(08)   VALUE 'W1011600'.            
006400 77    JA                        PIC X       VALUE 'J'.                   
006500 77    NEJ                       PIC X       VALUE 'N'.                   
006600 01    WS-IDTRANS                PIC X(4).                                
006700    88  EGEN-BILD                VALUE '1116'.                            
006800 01    SW-PROJ-GODK              PIC X(1).                                
006900    88  PROJ-GODK                VALUE 'J'.                               
007000                                                                          
007100 01    SW-PROJK-GODK              PIC X(1).                               
007200    88  PROJK-GODK                VALUE 'J'.                              
007300                                                                          
007400 01    SW-SUPPLIER-OK             PIC X(1).                               
007500    88  SUPPLIER-OK               VALUE 'J'.                              
007600                                                                          
007700 01    SW-TRAEFF                 PIC X(1).                                
007800    88  TRAEFF                   VALUE 'J'.                               
007900                                                                          
008000 77    WS-W009REDU-IN            PIC X(30)   VALUE SPACE.                 
008100 77    WS-W009REDU-UT            PIC X(30)   VALUE SPACE.                 
008200 77    WS-BEART                  PIC X(25)   VALUE SPACE.                 
008300 77    WS-BEART-SVE              PIC X(25)   VALUE SPACE.                 
008400 77    WS-FLRSBEART              PIC X       VALUE SPACE.                 
008500 77    WS-FLGAMART               PIC X       VALUE SPACE.                 
008600 77    WS-FLPISK                 PIC X       VALUE SPACE.                 
008700 77    WS-KVPROG                 PIC 9(7)    VALUE ZERO.                  
008800 77    WS-FLBYTES                PIC X       VALUE SPACE.                 
008900 01    WS-IDPROJ                 PIC X(4)    VALUE SPACE.                 
009000 77    WS-IDRITN                 PIC X(10)   VALUE SPACE.                 
009100 77    WS-IDAO                   PIC X(10)   VALUE SPACE.                 
009200 77    WS-IDBERED                PIC 9(2)    VALUE ZERO.                  
009300 77    WS-IDBERED-2-NUM          PIC 9(2)    VALUE ZERO.                  
009400 77    WS-IDSKYLT                PIC X(3)    VALUE SPACE.                 
009500 77    WS-ERROR-UPDX             PIC X(3)    VALUE SPACE.                 
009600                                                                          
009700 01    WS-IDPROJK.                                                        
009800    05  W-PROJK-POS-1            PIC X(1)    VALUE SPACE.                 
009900    05  W-PROJK-POS-2            PIC X(1)    VALUE SPACE.                 
010000    05  FILLER                   PIC X(2)    VALUE SPACE.                 
010100 77    WS-IDPROJK-GAM            PIC X(4)    VALUE SPACE.                 
010200 77    WS-KVARTVAGN              PIC 9(3)    VALUE ZERO.                  
010300 77    WS-IDFKNGRP               PIC 9(4)    VALUE ZERO.                  
010400 77    WS-KDPRODSL               PIC 9(2)    VALUE ZERO.                  
010500 77    WS-KDSORT                 PIC X(2)    VALUE SPACE.                 
010600 77    WS-KDBPSR                 PIC 9(1)    VALUE ZERO.                  
010700 77    WS-KDFARLIG               PIC 9(1)    VALUE ZERO.                  
010800 77    WS-KDYTBEH                PIC 9(2)    VALUE ZERO.                  
010900                                                                          
011000 01    WS-TEST-IDFKNGRP          PIC 9(4)    VALUE ZERO.                  
011100 01    FILLER REDEFINES WS-TEST-IDFKNGRP.                                 
011200       03 FILLER                 PIC 9(3).                                
011300       03 WS-SISTA-SIFFRAN       PIC 9.                                   
011400                                                                          
011500 77    WS-SPAR-FLPISK            PIC X       VALUE SPACE.                 
011600 77    WS-SPAR-KVPROG            PIC 9(7)    VALUE ZERO.                  
011700 77    WS-SPAR-FLBYTES           PIC X       VALUE SPACE.                 
011800 77    WS-SPAR-IDPROJK           PIC X(4)    VALUE SPACE.                 
011900 77    WS-SPAR-KVARTVAGN         PIC 9(3)    VALUE ZERO.                  
012000 77    WS-SPAR-TEORSAK-1         PIC X(40)   VALUE SPACE.                 
012100 77    W-SPAR-TISERLEV           PIC 9(7)    VALUE ZERO.                  
012200 77    MAX-IX                    PIC S9(3)   VALUE +3.                    
012300 77    MAX-TAB                   PIC S9(3)   VALUE +10.                   
012400 77    MAX-TAB-PLUS-1            PIC S9(3)   VALUE +11.                   
012500 77    MAX-IX-PLUS-1             PIC S9(3)   VALUE +4.                    
012600 77    SPIND                     PIC S9(9)   VALUE +0  COMP SYNC.         
012700 77    SLEV-IX                   PIC S9(9)   VALUE +0  COMP SYNC.         
012800 77    IX                        PIC S9(9)   VALUE +0  COMP SYNC.         
012900 77    RAD-INDX                  PIC S9(9)   VALUE +0  COMP SYNC.         
013000 77    MAX-RAD                   PIC S9(9)   VALUE +13 COMP SYNC.         
013100 77    MAX-RAD-PLUS-1            PIC S9(9)   VALUE +14 COMP SYNC.         
013200 77    MAX-RAD-PLUS-2            PIC S9(9)   VALUE +15 COMP SYNC.         
013300 77    TAB-IX                    PIC S9(9)   VALUE +0  COMP SYNC.         
013400 77    AO-IX                     PIC S9(9)   VALUE +0  COMP SYNC.         
013500 77    LEV05-IX                  PIC S9(9)   VALUE +0  COMP SYNC.         
013600 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +683 COMP SYNC.         
013700 77    WS-IDARTNR-MOTSV          PIC 9(09)  VALUE ZERO.                   
013800 77    WS-SPAR-IDARTNR-MOTSV     PIC 9(09)  VALUE ZERO.                   
013900     SKIP2                                                                
014000*      --- VALID IDDC CODES                                               
014100*                                                                         
014200*01    -COPY WWDC99                                                       
014300       EJECT                                                              
014400                                                                          
014500*   -COPY  WWPRODSL                                                       
014600     SKIP2                                                                
014700                                                                          
014800*   -COPY  WWLEV05                                                        
014900     SKIP2                                                                
015000 01  WS-IDARTNR                       PIC X(9) VALUE SPACE.               
015100 01  IDARTNR-WS REDEFINES WS-IDARTNR  PIC 9(9).                           
015200     SKIP2                                                                
015300 01  WS-NY-IDARTNR                           PIC X(9) VALUE SPACE.        
015400 01  IDARTNR-NY-WS REDEFINES WS-NY-IDARTNR PIC 9(9).                      
015500     SKIP2                                                                
015600 01  WS-IDARTNR-TILLK            PIC S9(9)    COMP-3 VALUE ZERO.          
015700     EJECT                                                                
015800 01  INPUT-RETT                  PIC X        VALUE 'J'.                  
015900 01  KOPIERING                   PIC X        VALUE 'N'.                  
016000 01  UPPDATERING                 PIC X        VALUE 'N'.                  
016100 01  SAMMA-BEN                   PIC X        VALUE 'N'.                  
016200 01  FINNS-REG-PA-NYPON          PIC X        VALUE 'N'.                  
016300 01  FINNS-PA-ARTC               PIC X        VALUE 'N'.                  
016400 01  VAECKNING                   PIC X        VALUE 'N'.                  
016500 01  NYPON-ARTIKEL               PIC X        VALUE 'N'.                  
016600 01  TRANS-TO-1192               PIC X        VALUE 'N'.                  
016700 01  FINNS-RASA                  PIC X        VALUE 'N'.                  
016800     SKIP3                                                                
016900 01  WS-TEHOMONYM.                                                        
017000     03  RS-BM-NAMN              PIC X(7).                                
017100     03  FILLER                  PIC X(53).                               
017200     SKIP3                                                                
017300 01  TABELL                      PIC X(30)                                
017400                       VALUE 'D  E  F  GB I  NL P  S  SF USA'.            
017500 01  TAB REDEFINES TABELL.                                                
017600     03  FILLER OCCURS 10.                                                
017700         05  TAB-IDSKYLT         PIC X(3).                                
017800     SKIP3                                                                
017900 01  WS-TIFINLV-AAMMDD         PIC 9(6)    VALUE ZERO.                    
018000 01  WS-TIFINLV                PIC 9(5)    VALUE ZERO.                    
018100 01  FILLER REDEFINES WS-TIFINLV.                                         
018200     03  WS-AAR                PIC 9(2).                                  
018300     03  WS-VECKA              PIC 9(2).                                  
018400     03  WS-DAG                PIC 9(1).                                  
018500     SKIP2                                                                
018600 01  WS-TISOP-AAMMDD           PIC 9(6)    VALUE ZERO.                    
018700 01  WS-TISOP                  PIC 9(5)    VALUE ZERO.                    
018800 01  FILLER REDEFINES WS-TISOP.                                           
018900     03  WS-AAR-SOP            PIC 9(2).                                  
019000     03  WS-VECKA-SOP          PIC 9(2).                                  
019100     03  WS-DAG-SOP            PIC 9(1).                                  
019200     SKIP2                                                                
019300                                                                          
019400 01  SPAR-TIFINLV-AAVVD        PIC 9(05)  VALUE ZERO.                     
019500                                                                          
019600 01  SPAR-TIFINLV-AAVV.                                                   
019700     10  SPAR-TIFINLV-AA       PIC 9(02)  VALUE ZERO.                     
019800     10  SPAR-TIFINLV-VV       PIC 9(02)  VALUE ZERO.                     
019900 01  SPAR-TIFINLV-AAVV-R  REDEFINES  SPAR-TIFINLV-AAVV                    
020000                               PIC 9(04).                                 
020100                                                                          
020200 01  XX-TIFINLV                PIC X(5)    VALUE SPACE.                   
020300 01  FILLER REDEFINES XX-TIFINLV.                                         
020400     03  XX-AAR                PIC X(2).                                  
020500     03  XX-VECKA              PIC X(2).                                  
020600     03  XX-DAG                PIC X(1).                                  
020700 01  XX-TISOP                  PIC X(5)    VALUE SPACE.                   
020800 01  FILLER REDEFINES XX-TISOP.                                           
020900     03  XX-AAR-SOP            PIC X(2).                                  
021000     03  XX-VECKA-SOP          PIC X(2).                                  
021100     03  XX-DAG-SOP            PIC X(1).                                  
021200     SKIP2                                                                
021300 01  INMATAD-IDPROENH.                                                    
021400     03  FILLER OCCURS 3.                                                 
021500         05  WS-IDPROENH         PIC X(8).                                
021600     SKIP2                                                                
021700 01  INMATAD-IDKAT.                                                       
021800     03  FILLER OCCURS 3.                                                 
021900         05  WS-IDKAT            PIC X(5).                                
022000     SKIP2                                                                
022100 01  DAGENS-AAMMDD               PIC 9(6)  VALUE ZERO.                    
022200 01  DAGENS-AAVV.                                                         
022300     03  DAGENS-AA               PIC 9(2)  VALUE ZERO.                    
022400     03  DAGENS-VV               PIC 9(2)  VALUE ZERO.                    
022500                                                                          
022600 01  SPAR-DAGENS-AAVV.                                                    
022700     03  SPAR-DAGENS-AA           PIC 9(02)  VALUE ZERO.                  
022800     03  SPAR-DAGENS-VV           PIC 9(02)  VALUE ZERO.                  
022900 01  SPAR-DAGENS-AAVV-R  REDEFINES  SPAR-DAGENS-AAVV                      
023000                                  PIC 9(04).                              
023100                                                                          
023200 01  WS-YYWWD                    PIC 9(5).                                
023300 01  FILLER REDEFINES WS-YYWWD.                                           
023400     03  WS-YY                   PIC 9(2).                                
023500     03  WS-WW                   PIC 9(2).                                
023600     03  WS-D                    PIC 9(1).                                
023700     SKIP2                                                                
023800 01  TINEDB-AAVV.                                                         
023900     03  TINEDB-AA               PIC 9(2)  VALUE ZERO.                    
024000     03  TINEDB-VV               PIC 9(2)  VALUE ZERO.                    
024100     SKIP2                                                                
024200 01  VECKOR.                                                              
024300     03  AAVVD                   PIC 9(5).                                
024400     03  FILLER REDEFINES AAVVD.                                          
024500       05  AAVV                  PIC 9(4).                                
024600       05  FILLER REDEFINES AAVV.                                         
024700         07  AA                  PIC 9(2).                                
024800         07  VV                  PIC 9(2).                                
024900       05  D                     PIC 9(1).                                
025000     SKIP2                                                                
025100 01  W-AAR4                      PIC 9(4).                                
025200     SKIP3                                                                
025300 01  C-PRODSL19-DEFAULT-VALUES.                                           
025400     03  C-KDPRODSL-19              PIC 9(2)  VALUE 19.                   
025500     03  C-PS19-DEFAULT-KDSORT      PIC X(2)  VALUE 'ST'.                 
025600     03  C-PS19-DEFAULT-KDYTBEH     PIC 9(2)  VALUE 00.                   
025700     03  C-PS19-DEFAULT-IDPROJ      PIC X(4)  VALUE '1'.                  
025800     03  C-PS19-DEFAULT-KDFARLIG    PIC 9(1)  VALUE 3.                    
025900     03  C-PS19-DEFAULT-KDBPSR      PIC 9(1)  VALUE 1.                    
026000     03  C-PS19-DEFAULT-IDPROJK     PIC X(4)  VALUE 'EMB'.                
026100     03  C-PS19-DEFAULT-FLLSRDEL    PIC X     VALUE 'J'.                  
026200                                                                          
026300 01  DYNAMISKA-SUBPROGRAM.                                                
026400     03  WDATKONV                PIC X(8)     VALUE 'WDATKONV'.           
026500     03  WKPSKONV                PIC X(8)     VALUE 'WKPSKONV'.           
026600     03  WREVERSE                PIC X(8)     VALUE 'WREVERSE'.           
026700     03  W009REDU                PIC X(8)     VALUE 'W009REDU'.           
026800     03  CBLTDLI                 PIC X(8)     VALUE 'CBLTDLI '.           
026900     03  FELLOG                  PIC X(8)     VALUE 'FELLOG  '.           
027000     03  W005INIT                PIC X(8)     VALUE 'W005INIT'.           
027100     EJECT                                                                
027200*01 -COPY WDATAREA                                                        
027300     EJECT                                                                
027400*01 -COPY WKPSAREA                                                        
027500     EJECT                                                                
027600*01 -COPY WREVAREA                                                        
027700     EJECT                                                                
027800*                   ****    PARAMETRAR TILL W005INIT                      
027900*01  -COPY WMSGINIT                                                       
028000     EJECT                                                                
028100 01    MEDDELANDE.                                                        
028200   03    W-FEL-1.                                                         
028300     05   FILLER                 PIC X(34)   VALUE                        
028400             'ARTIKELNUMMER EJ NUMERISKT        '.                        
028500     05   FILLER                 PIC X(34)   VALUE                        
028600             'PARTNUMBER NOT NUMERIC            '.                        
028700   03    FILLER REDEFINES W-FEL-1.                                        
028800     05  FEL-1                   PIC X(34) OCCURS 2.                      
028900                                                                          
029000   03    W-FEL-2.                                                         
029100     05   FILLER                 PIC X(34)   VALUE                        
029200             'ARTIKEL SAKNAS PÅ ARTIKELREGISTRET'.                        
029300     05   FILLER                 PIC X(34)   VALUE                        
029400             'THIS PART IS NOT IN THE DATABASE  '.                        
029500   03    FILLER REDEFINES W-FEL-2.                                        
029600     05  FEL-2                   PIC X(34) OCCURS 2.                      
029700                                                                          
029800   03    W-FEL-3.                                                         
029900     05   FILLER                 PIC X(34)   VALUE                        
030000             'ARTIKEL REDAN REGISTRERAD         '.                        
030100     05   FILLER                 PIC X(34)   VALUE                        
030200             'PART NO ALREADY REGISTRED         '.                        
030300   03    FILLER REDEFINES W-FEL-3.                                        
030400     05  FEL-3                   PIC X(34) OCCURS 2.                      
030500                                                                          
030600   03    W-FEL-4.                                                         
030700     05   FILLER                 PIC X(34)   VALUE                        
030800             'UPPLYSTA FÄLT FEL                 '.                        
030900     05   FILLER                 PIC X(34)   VALUE                        
031000             'CORRECT HIGHLIGHTED FIELDS        '.                        
031100   03    FILLER REDEFINES W-FEL-4.                                        
031200     05  FEL-4                   PIC X(34) OCCURS 2.                      
031300                                                                          
031400   03    W-FEL-5.                                                         
031500     05   FILLER                 PIC X(34)   VALUE                        
031600             'BENÄMNING SAKNAS PÅ BENREG        '.                        
031700     05   FILLER                 PIC X(34)   VALUE                        
031800             'DESCRIPTION IS MISSING            '.                        
031900   03    FILLER REDEFINES W-FEL-5.                                        
032000     05  FEL-5                   PIC X(34) OCCURS 2.                      
032100                                                                          
032200   03    W-FEL-6.                                                         
032300     05   FILLER                 PIC X(32)   VALUE                        
032400             'HOMONYMKOD FINNS-KORRIGERA     '.                           
032500     05   FILLER                 PIC X(32)   VALUE                        
032600             'HOM.CODE EXISTS  VERIFY        '.                           
032700   03    FILLER REDEFINES W-FEL-6.                                        
032800     05  FEL-6                   PIC X(32) OCCURS 2.                      
032900                                                                          
033000   03    W-FEL-7.                                                         
033100     05   FILLER                 PIC X(32)   VALUE                        
033200             'UPPDATERING MED PF11       '.                               
033300     05   FILLER                 PIC X(32)   VALUE                        
033400             'PRESS PF11 FOR UPDATING    '.                               
033500   03    FILLER REDEFINES W-FEL-7.                                        
033600     05  FEL-7                   PIC X(32) OCCURS 2.                      
033700                                                                          
033800   03    W-FEL-8.                                                         
033900     05   FILLER                 PIC X(32)   VALUE                        
034000             'ANGE NYTT ARTIKELNUMMER    '.                               
034100     05   FILLER                 PIC X(32)   VALUE                        
034200             'GIVE A NEW PARTNUMBER      '.                               
034300   03    FILLER REDEFINES W-FEL-8.                                        
034400     05  FEL-8                   PIC X(32) OCCURS 2.                      
034500                                                                          
034600   03    W-FEL-10.                                                        
034700     05   FILLER                 PIC X(36)   VALUE                        
034800             'ARTIKEL REDAN REGISTRERAD PÅ NYPON'.                        
034900     05   FILLER                 PIC X(36)   VALUE                        
035000             'PART NO ALREADY REGISTRED         '.                        
035100   03    FILLER REDEFINES W-FEL-10.                                       
035200     05  FEL-10                  PIC X(36) OCCURS 2.                      
035300                                                                          
035400   03    W-FEL-13.                                                        
035500     05   FILLER                 PIC X(40)   VALUE                        
035600             'UPPDATERING EJ TILLÅTEN               '.                    
035700     05   FILLER                 PIC X(40)   VALUE                        
035800             'UPDATE NOT ALLOWED                '.                        
035900   03    FILLER REDEFINES W-FEL-13.                                       
036000     05  FEL-13                  PIC X(40) OCCURS 2.                      
036100                                                                          
036200   03    W-FEL-14.                                                        
036300     05   FILLER                 PIC X(40)   VALUE                        
036400             'ARTIKEL SAKNAS ARTREG MEN FINNS RASA  '.                    
036500     05   FILLER                 PIC X(40)   VALUE                        
036600             'PART NUMBER REGISTRED IN RASA     '.                        
036700   03    FILLER REDEFINES W-FEL-14.                                       
036800     05  FEL-14                  PIC X(40) OCCURS 2.                      
036900                                                                          
037000   03    W-FEL-15.                                                        
037100     05   FILLER                 PIC X(40)   VALUE                        
037200             'LEV. ARTIKEL KRÄVS FÖR DETTA PRODUKTSLAG'.                  
037300     05   FILLER                 PIC X(40)   VALUE                        
037400             'SUPPL. PART REQUIRED FOR THIS PROD GROUP'.                  
037500   03    FILLER REDEFINES W-FEL-15.                                       
037600     05  FEL-15                  PIC X(40) OCCURS 2.                      
037700                                                                          
037800   03    W-FEL-16.                                                        
037900     05   FILLER                 PIC X(40)   VALUE                        
038000             'LEV. ARTIKEL FINNS REDAN I CROSSINDEX'.                     
038100     05   FILLER                 PIC X(40)   VALUE                        
038200             'SUPPL. PART IN CROSSINDEX ALREADY'.                         
038300   03    FILLER REDEFINES W-FEL-16.                                       
038400     05  FEL-16                  PIC X(40) OCCURS 2.                      
038500                                                                          
038600   03    W-FEL-17.                                                        
038700     05   FILLER                 PIC X(40)   VALUE                        
038800             'LEV. ARTIKEL MAX 10 LÅNG FÖR GIVET PG'.                     
038900     05   FILLER                 PIC X(40)   VALUE                        
039000             'SUPPL. PART MAX 10 LONG FOR GIVEN PG'.                      
039100   03    FILLER REDEFINES W-FEL-17.                                       
039200     05  FEL-17                  PIC X(40) OCCURS 2.                      
039300                                                                          
039400   03    W-MED-1.                                                         
039500     05   FILLER                 PIC X(32)   VALUE                        
039600             'UPPDATERING GJORD          '.                               
039700     05   FILLER                 PIC X(32)   VALUE                        
039800             'UPDATED                    '.                               
039900   03    FILLER REDEFINES W-MED-1.                                        
040000     05  MED-1                   PIC X(32) OCCURS 2.                      
040100                                                                          
040200   03    W-MED-2.                                                         
040300     05   FILLER                 PIC X(32)   VALUE                        
040400             'ARTIKELN AVSLAGEN          '.                               
040500     05   FILLER                 PIC X(32)   VALUE                        
040600             'REJECTED PART              '.                               
040700   03    FILLER REDEFINES W-MED-2.                                        
040800     05  MED-2                   PIC X(32) OCCURS 2.                      
040900                                                                          
041000   03    W-MED-5.                                                         
041100     05   FILLER                 PIC X(40)   VALUE                        
041200             'ARTIKELN FINNS REDAN PÅ CROSSINDEX'.                        
041300     05   FILLER                 PIC X(40)   VALUE                        
041400             'THIS PART ALREADY EXISTS IN CROSS-INDEX'.                   
041500   03    FILLER REDEFINES W-MED-5.                                        
041600     05  MED-5                   PIC X(40) OCCURS 2.                      
041700     EJECT                                                                
041800 01  NYCKLAR-TILL-DLI.                                                    
041900   03  W-WDD2A1KY-MIN.                                                    
042000     05  W-IDBERED-MIN           PIC S9(3)   COMP-3.                      
042100     05  W-IDAO-MIN              PIC X(10).                               
042200     05  W-IDARTNR-MIN           PIC S9(9)   COMP-3.                      
042300   03  W-WDD2A1KY-MAX.                                                    
042400     05  W-IDBERED-MAX           PIC S9(3)   COMP-3.                      
042500     05  W-IDAO-MAX              PIC X(10).                               
042600     05  W-IDARTNR-MAX           PIC S9(9)   COMP-3  VALUE                
042700                                                +999999999.               
042800                                                                          
042900   03  W-WDJ1CSEQ-X.                                                      
043000     05  W-IDLEVNR-S             PIC X(5)    VALUE SPACE.                 
043100     05  W-BELEVART-S            PIC X(30)   VALUE SPACE.                 
043200     05  W-IDARTNR-S             PIC S9(9)   COMP-3  VALUE ZERO.          
043300                                                                          
043400   03  W-IDPROJ-MIN              PIC X(4).                                
043500   03  W-IDPROJ-MAX              PIC X(4).                                
043600   03  W-IDARTNR-X.                                                       
043700     05  W-IDARTNR               PIC S9(9)   COMP-3.                      
043800   03  W-KDNOTTYP-X.                                                      
043900     05  W-KDNOTTYP              PIC S9      COMP-3.                      
044000   03  W-IDARTNR-TILLK-X.                                                 
044100     05  W-IDARTNR-TILLK         PIC S9(9)   COMP-3.                      
044200   03  W-IDARTNR-ERS-LOW-X.                                               
044300     05  W-IDARTNR-ERS-LOW       PIC S9(9)   COMP-3  VALUE ZERO.          
044400   03  W-IDARTNR-ERS-HIGH-X.                                              
044500     05  W-IDARTNR-ERS-HIGH      PIC S9(9)   COMP-3                       
044600                                  VALUE +999999999.                       
044700   03  W-IDKORTNR-LOW-X.                                                  
044800     05  W-IDKORTNR-LOW          PIC S9(3)   COMP-3  VALUE ZERO.          
044900   03  W-IDKORTNR-HIGH-X.                                                 
045000     05  W-IDKORTNR-HIGH         PIC S9(3)   COMP-3  VALUE +999.          
045100                                                                          
045200   03  W-IDSKYLT-X.                                                       
045300     05  W-IDSKYLT               PIC X(3)    VALUE SPACE.                 
045400   03  W-BEART-X.                                                         
045500     05  W-BEART                 PIC X(25)   VALUE SPACE.                 
045600                                                                          
045700   03  W-KDSEGKEY-X.                                                      
045800     05  W-KDSEGKEY              PIC  X(1)   VALUE '1'.                   
045900                                                                          
046000   03  W-WDF5A1KY-MIN.                                                    
046100       05  W-SEQA-IDLEVNR-MIN    PIC X(5)    VALUE LOW-VALUE.             
046200       05  W-SEQA-IDLEVART-MIN   PIC X(30)   VALUE LOW-VALUE.             
046300       05  W-SEQA-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
046400       05  W-SEQA-IDBENR-MIN     PIC S9(1)   VALUE ZERO COMP-3.           
046500   03  W-WDF5A1KY-MAX.                                                    
046600       05  W-SEQA-IDLEVNR-MAX    PIC X(5)    VALUE HIGH-VALUE.            
046700       05  W-SEQA-IDLEVART-MAX   PIC X(30)   VALUE HIGH-VALUE.            
046800       05  W-SEQA-IDARTNR-MAX    PIC S9(9)   VALUE +999999999             
046900                                               COMP-3.                    
047000       05  W-SEQA-IDBENR-MAX     PIC S9(1)   VALUE +9 COMP-3.             
047100                                                                          
047200   03  W-1207-KEY-X.                                                      
047300     05  FILLER                  PIC X(4)    VALUE '1207'.                
047400     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
047500                                                                          
047600   03  W-1131-KEY-X.                                                      
047700     05  FILLER                  PIC X(4)    VALUE '1131'.                
047800     05  W-KDPRODSL              PIC S9(3)   COMP-3 VALUE ZERO.           
047900     05  FILLER                  PIC X(24)   VALUE LOW-VALUE.             
048000                                                                          
048100   03  W-1132-KEY-X.                                                      
048200     05  W-IDPROJK               PIC X(4)    VALUE LOW-VALUE.             
048300     05  W-IDPROJOBJ             PIC X(4)    VALUE LOW-VALUE.             
048400     05  W-IDPROJ                PIC X(4)    VALUE LOW-VALUE.             
048500     05  FILLER                  PIC X(3)    VALUE LOW-VALUE.             
048600                                                                          
048700     EJECT                                                                
048800******************************************************************        
048900*                                                                         
049000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
049100*                                                                         
049200 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
049300     SKIP3                                                                
049400*01    MID -COPY W1I11601.                                                
049500     EJECT                                                                
049600 01  MSG-KOM-MESSAGE-CODES.                                               
049700     03  FEL-ERR-FIELD           PIC X(3)    VALUE '020'.                 
049800     03  FEL-ERR-IDDC            PIC X(3)    VALUE '028'.                 
049900     03  FEL-ERR-IDARTNR         PIC X(3)    VALUE '768'.                 
050000     03  FEL-ERR-IDBERED         PIC X(3)    VALUE '96A'.                 
050100     03  FEL-ERR-KDPRODSL        PIC X(3)    VALUE '96B'.                 
050200     03  FEL-ERR-KDSORT          PIC X(3)    VALUE '96C'.                 
050300     03  FEL-ERR-IDPROENH        PIC X(3)    VALUE '96D'.                 
050400     03  FEL-ERR-KDYTBEH         PIC X(3)    VALUE '96E'.                 
050500     03  FEL-ERR-IDPROJ          PIC X(3)    VALUE '96F'.                 
050600     03  FEL-ERR-KDFARLIG        PIC X(3)    VALUE '96G'.                 
050700     03  FEL-ERR-KDBPSR          PIC X(3)    VALUE '96H'.                 
050800     03  FEL-ERR-KDUART          PIC X(3)    VALUE '96I'.                 
050900     03  FEL-ERR-IDPROJK         PIC X(3)    VALUE '96J'.                 
051000     03  FEL-ERR-FLPISK          PIC X(3)    VALUE '96K'.                 
051100     03  FEL-ERR-IDAO            PIC X(3)    VALUE '96L'.                 
051200     03  FEL-ERR-TISOP           PIC X(3)    VALUE '96M'.                 
051300     03  FEL-ERR-IDSKYLT         PIC X(3)    VALUE '96N'.                 
051400     03  FEL-ERR-FLLSRDEL        PIC X(3)    VALUE '96O'.                 
051500     03  FEL-ERR-IDPROJUP        PIC X(3)    VALUE '96P'.                 
051600     03  FEL-ERR-BEART           PIC X(3)    VALUE '96Q'.                 
051700     03  FEL-ERR-FLRSBEART       PIC X(3)    VALUE '96R'.                 
051800     03  FEL-ERR-IDFKNGRP        PIC X(3)    VALUE '96S'.                 
051900     03  FEL-ERR-TEORSAK-1       PIC X(3)    VALUE '96T'.                 
052000     03  FEL-ERR-TEARTNOT        PIC X(3)    VALUE '96U'.                 
052100     03  FEL-ERR-IDRITN          PIC X(3)    VALUE '96V'.                 
052200     03  FEL-ERR-IDPSN           PIC X(3)    VALUE '96W'.                 
052300     03  FEL-ERR-KDARTHNT        PIC X(3)    VALUE '96X'.                 
052400     03  FEL-ERR-KDEMBKOD-2      PIC X(3)    VALUE '96Z'.                 
052500     03  FEL-ERR-VLFG            PIC X(3)    VALUE '97A'.                 
052600     03  FEL-ERR-KDSORT-VLFG     PIC X(3)    VALUE '97B'.                 
052700     03  FEL-ERR-VKFORSFG        PIC X(3)    VALUE '97C'.                 
052800     03  FEL-ERR-IDCDS           PIC X(3)    VALUE '97D'.                 
052900     03  FEL-ERR-KDARTSYS        PIC X(3)    VALUE '97E'.                 
053000     03  FEL-ERR-FLAGMART        PIC X(3)    VALUE '97F'.                 
053100     03  FEL-ERR-KVPROG          PIC X(3)    VALUE '97G'.                 
053200     03  FEL-ERR-KVARTVAGN       PIC X(3)    VALUE '181'.                 
053300     03  FEL-ERR-BELEV           PIC X(3)    VALUE '092'.                 
053400     03  FEL-ERR-FLBYTES         PIC X(3)    VALUE '358'.                 
053500     03  FEL-ERR-IDFTG           PIC X(3)    VALUE '950'.                 
053600     03  OK-GODKANT-FEL          PIC X(3)    VALUE '114'.                 
053700     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
053800     EJECT                                                                
053900 01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-AREA'.         
054000*    --- GENERELL IO-KOMMUNIKATIONSAREA FÖR DISPATCHER                    
054100*01    -COPY WMSGKOM                                                      
054200     EJECT                                                                
054300 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
054400*01    -COPY WMSGAREA                                                     
054500     EJECT                                                                
054600*  03    MOD -COPY W1O11601 -RED MSG-AREA.                                
054700     EJECT                                                                
054800 01  FILLER          PIC X(16) VALUE 'PROG-TO-PROG-SW'.                   
054900 01  W-PROG-TO-PROG-SW.                                                   
055000     03  P-WS-LL     PIC S9(4)  VALUE +1099 COMP SYNC.                    
055100     03  P-WS-Z1-Z2  PIC X(2)   VALUE LOW-VALUE.                          
055200     03  KDTRANS-WS  PIC X(8)   VALUE 'W1T192X '.                         
055300     03  P-IDTRANS   PIC X(4)   VALUE '1116'.                             
055400     03  P-KDMFSFOR  PIC X(1)   VALUE '1'.                                
055500*    03  MID   -COPY W1I11601     -PRE PROGSW-.                           
055600*    03  MOD   -COPY W1O11601     -PRE PROGSW-.                           
055700     EJECT                                                                
055800*01    -COPY WMFSAREA                                                     
055900     EJECT                                                                
056000******************************************************************        
056100*                                                                         
056200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
056300*                                                                         
056400 01    IMS-WS.                                                            
056500   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
056600     SKIP3                                                                
056700*                        **** STATUS-KOD FRÅN IMS                         
056800   03    STATUS-WS               PIC XX.                                  
056900     88    SEGMENT-FINNS                     VALUE '  '.                  
057000     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
057100     88    BASEN-SLUT                        VALUE 'GB'.                  
057200     SKIP3                                                                
057300   03    GODK-STATUSKODER.                                                
057400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
057500     SKIP3                                                                
057600 01    SSA1                      PIC X(121).                              
057700 01    SSA2                      PIC X(64).                               
057800     EJECT                                                                
057900*                            IMS FUNKTIONSKODER                           
058000*01    -COPY W0003                                                        
058100     EJECT                                                                
058200*                            DLI INPUT-OUTPUT AREA-1                      
058300 01    DLI-IO-AREA-1.                                                     
058400   03    IO-AREA-1               PIC X(900)  VALUE SPACE.                 
058500     SKIP3                                                                
058600*  03  WLARTC01 -COPY WDK601                 -RED IO-AREA-1.              
058700     EJECT                                                                
058800*  03  WLARTC11 -COPY WDK611                 -RED IO-AREA-1.              
058900     EJECT                                                                
059000*  03  WLARTC25 -COPY WDK625                 -RED IO-AREA-1.              
059100     EJECT                                                                
059200*  03  WLERSA01 -COPY WDD701  -PRE ERSA01-   -RED IO-AREA-1.              
059300     EJECT                                                                
059400*  03  WLERSA11 -COPY WDD702  -PRE ERSA11-   -RED IO-AREA-1.              
059500     EJECT                                                                
059600*  03  WLERSB01 -COPY WDD7A1  -PRE TILLK-    -RED IO-AREA-1.              
059700     EJECT                                                                
059800*  03  WLBENA01 -COPY WDD301  -PRE BENA-     -RED IO-AREA-1.              
059900     EJECT                                                                
060000*  03  WLBENA11 -COPY WDD311  -PRE BENA-     -RED IO-AREA-1.              
060100     EJECT                                                                
060200*  03  WLBENA12 -COPY WDD312  -PRE BENA-     -RED IO-AREA-1.              
060300     EJECT                                                                
060400*  03  WLBENA13 -COPY WDD313  -PRE BENA-     -RED IO-AREA-1.              
060500     EJECT                                                                
060600*  03  WLXXAQ11 -COPY WDGX1132   -PRE XXAQ-  -RED IO-AREA-1.              
060700     EJECT                                                                
060800*                            DLI INPUT-OUTPUT AREA-2                      
060900 01    DLI-IO-AREA-2.                                                     
061000   03    IO-AREA-2               PIC X(600)  VALUE SPACE.                 
061100     SKIP3                                                                
061200*  03  WLARTG01 -COPY WDD201  -PRE ARTG01-  -RED IO-AREA-2.               
061300*  03  WLARTH01 -COPY WDD2A1  -PRE ARTH01-  -RED IO-AREA-2.               
061400     EJECT                                                                
061500 01    DLI-IO-AREA-3.                                                     
061600   03    IO-AREA-3               PIC X(240)  VALUE SPACE.                 
061700*  03  WLSATB01 -COPY WDJ101  -PRE SATB-     -RED IO-AREA-3.              
061800     EJECT                                                                
061900*  03  WLSATB11 -COPY WDJ111  -PRE SATB-     -RED IO-AREA-3.              
062000     EJECT                                                                
062100 01    DLI-IO-AREA-4.                                                     
062200   03    IO-AREA-4               PIC X(16)  VALUE SPACE.                  
062300*  03  WDF501 -COPY WDF501                 -RED IO-AREA-4.                
062400     EJECT                                                                
062500 01  DLI-IO-AREA-5.                                                       
062600     03  IO-AREA-5               PIC X(150)  VALUE SPACE.                 
062700     03  WDF5A1 REDEFINES IO-AREA-5.                                      
062800*        05  -COPY WDF5A1                                                 
062900     EJECT                                                                
063000 LINKAGE SECTION.                                                         
063100*01    -COPY W0009     -PRE MSG-                                          
063200     EJECT                                                                
063300*01    -COPY W0009     -PRE ALT-                                          
063400     EJECT                                                                
063500*01    -COPY W0009     -PRE MSGKOM-                                       
063600     EJECT                                                                
063700*01  -COPY W0008     -PRE USEA-                                           
063800         05  FILLER           PIC X.                                      
063900     EJECT                                                                
064000*01    -COPY W0008     -PRE ARTC-                                         
064100     05  FILLER                  PIC X.                                   
064200     EJECT                                                                
064300*01    -COPY W0008     -PRE ERSA-                                         
064400     05  FILLER                  PIC X.                                   
064500     EJECT                                                                
064600*01    -COPY W0008     -PRE ERSB-                                         
064700     05  FILLER                  PIC X.                                   
064800     EJECT                                                                
064900*01    -COPY W0008     -PRE BENB-                                         
065000     05  FILLER                  PIC X.                                   
065100     EJECT                                                                
065200*01    -COPY W0008     -PRE BENC-                                         
065300     05  FILLER                  PIC X.                                   
065400     EJECT                                                                
065500*01    -COPY W0008     -PRE XXAQ-                                         
065600     05  FILLER                  PIC X.                                   
065700     EJECT                                                                
065800*01    -COPY W0008     -PRE ARTG-                                         
065900     05  FILLER                  PIC X.                                   
066000     EJECT                                                                
066100*01    -COPY W0008     -PRE ARTH-                                         
066200     05  FILLER                  PIC X.                                   
066300     EJECT                                                                
066400*01    -COPY W0008     -PRE SATB-                                         
066500     05  FILLER                  PIC X.                                   
066600     EJECT                                                                
066700*01    -COPY W0008     -PRE SATE-                                         
066800     05  FILLER                  PIC X.                                   
066900     EJECT                                                                
067000*01    -COPY W0008     -PRE WDF5-                                         
067100     05  FILLER                  PIC X.                                   
067200     EJECT                                                                
067300*01    -COPY W0008     -PRE WDF5A-                                        
067400     05  FILLER                  PIC X.                                   
067500     EJECT                                                                
067600 PROCEDURE DIVISION USING MSG-PCB ALT-PCB MSGKOM-PCB                      
067700                          USEA-PCB                                        
067800                          ARTC-PCB ERSA-PCB ERSB-PCB BENB-PCB             
067900                          BENC-PCB ARTG-PCB ARTH-PCB XXAQ-PCB             
068000                          SATB-PCB SATE-PCB WDF5-PCB WDF5A-PCB.           
068100                                                                          
068200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB MSGKOM-PCB                     
068300                          USEA-PCB                                        
068400                           ARTC-PCB ERSA-PCB ERSB-PCB BENB-PCB            
068500                           BENC-PCB ARTG-PCB ARTH-PCB XXAQ-PCB            
068600                           SATB-PCB SATE-PCB WDF5-PCB WDF5A-PCB.          
068700     EJECT                                                                
068800     PERFORM IMS-GET-MSG                                                  
068900     IF SEGMENT-FINNS                                                     
069000        PERFORM IMS-GET-WMSGKOM-MSG                                       
069100        PERFORM A-INIT                                                    
069200        IF WS-IDARTNR NUMERIC                                             
069300           MOVE IDARTNR-WS TO W-IDARTNR                                   
069400           PERFORM IMS-GET-ARTC01                                         
069500           IF SEGMENT-FINNS                                               
069600              MOVE ART-KDPRODSL TO WS-KDPRODSL                            
069700              PERFORM S92-CHECK-SUPPLIER                                  
069800           END-IF                                                         
069900        END-IF                                                            
070000        IF WS-IDARTNR NUMERIC AND INPUT-RETT = JA                         
070100           IF MFS-UPDATE OR MFS-UPD-X                                     
070200              PERFORM S13-VISA-BILD-IGEN                                  
070300              IF IDARTNR-WS > 99999999                                    
070400                 MOVE FEL-13 (SPIND) TO MOD-TEMFSFEL                      
070500                 PERFORM L-RENSA-INFAELT                                  
070600                 MOVE FEL-ERR-IDARTNR    TO WS-ERROR-UPDX                 
070700              ELSE                                                        
070800                 MOVE IDARTNR-WS TO W-IDARTNR                             
070900                 PERFORM IMS-GET-ARTC01                                   
071000                 IF SEGMENT-FINNS                                         
071100                    IF ART-KDERS-UTG > 0                                  
071200                       MOVE IDARTNR-WS TO IDARTNR-NY-WS                   
071300                       MOVE JA TO VAECKNING                               
071400                       PERFORM IMS-GET-ARTG01                             
071500                       IF SEGMENT-FINNS                                   
071600                           PERFORM B-REG-ART-ARTREG-UPPDAT-NYPON          
071700                       ELSE                                               
071800                           PERFORM E-VAECKNING-AV-GAMMAL-ARTIKEL          
071900                       END-IF                                             
072000                    ELSE                                                  
072100                       IF WS-NY-IDARTNR = WS-IDARTNR                      
072200                          CONTINUE                                        
072300                       ELSE                                               
072400                          IF WS-NY-IDARTNR > ZERO                         
072500                             IF IDARTNR-NY-WS > 99999999                  
072600                                MOVE FEL-8 (SPIND) TO MOD-TEMFSFEL        
072700                                MOVE MFS-ADD-SAETT-CURSOR TO              
072800                                      MOD-IDARTNR-ATTR                    
072900                             ELSE                                         
073000                                MOVE IDARTNR-NY-WS TO W-IDARTNR           
073100                                PERFORM IMS-GET-ARTC01                    
073200                                IF SEGMENT-FINNS                          
073300                                   MOVE FEL-3 (SPIND) TO                  
073400                                                      MOD-TEMFSFEL        
073500                                   MOVE MFS-ADD-SAETT-CURSOR TO           
073600                                         MOD-IDARTNR-ATTR                 
073700                                ELSE                                      
073800                                   MOVE JA TO KOPIERING                   
073900                                   MOVE IDARTNR-WS TO W-IDARTNR           
074000                                   PERFORM IMS-GET-ARTG01                 
074100                                   IF SEGMENT-FINNS                       
074200                                     PERFORM                              
074300                                     D-KOP-FRAN-ARTREG-EV-NYPON           
074400                                   ELSE                                   
074500                                     PERFORM C-KOP-FRAN-ARTREG            
074600                                   END-IF                                 
074700                                END-IF                                    
074800                             END-IF                                       
074900                          ELSE                                            
075000                             MOVE FEL-8 (SPIND) TO MOD-TEMFSFEL           
075100                             MOVE MFS-ADD-SAETT-CURSOR TO                 
075200                                    MOD-IDARTNR-ATTR                      
075300                          END-IF                                          
075400                       END-IF                                             
075500                    END-IF                                                
075600                 ELSE                                                     
075700                    PERFORM IMS-GET-ARTG01                                
075800                    IF SEGMENT-FINNS                                      
075900                       PERFORM B-REG-ART-ARTREG-UPPDAT-NYPON              
076000                    ELSE                                                  
076100                       PERFORM F-REG-ARTIKEL-ARTREG-EV-NYPON              
076200                    END-IF                                                
076300                 END-IF                                                   
076400              END-IF                                                      
076500           ELSE                                                           
076600              IF UPPDATERING = JA                                         
076700                 PERFORM S13-VISA-BILD-IGEN                               
076800                 MOVE FEL-7 (SPIND) TO MOD-TEMFSFEL                       
076900              ELSE                                                        
077000                 PERFORM L-RENSA-INFAELT                                  
077100                 MOVE IDARTNR-WS TO W-IDARTNR                             
077200                 PERFORM IMS-GET-ARTC01                                   
077300                 IF SEGMENT-FINNS                                         
077400                     MOVE ART-TIFINLV       TO MOD-TIFINLV-UT             
077500                     MOVE ART-TISOP         TO MOD-TISOP-UT               
077600                     MOVE JA                TO FINNS-PA-ARTC              
077700                     IF ART-KDERS-UTG > ZERO                              
077800                        MOVE IDARTNR-WS TO IDARTNR-NY-WS                  
077900                        PERFORM H-LAS-NYPON                               
078000                        IF SEGMENT-FINNS                                  
078100                           PERFORM O-KOLLA-RESBED-ARTUTG                  
078200                           PERFORM M-LAS-BENREG                           
078300                           MOVE MFS-STAENG-FAELT TO                       
078400                                                 MOD-BEART-ATTR           
078500                           MOVE MFS-STAENG-FAELT TO                       
078600                                 MOD-FLRSBEART-ATTR                       
078700                           MOVE MFS-STAENG-FAELT TO                       
078800                                 MOD-IDSKYLT-ATTR                         
078900                           MOVE MFS-STAENG-FAELT TO                       
079000                                 MOD-IDARTNR-ATTR                         
079100                        ELSE                                              
079200                           PERFORM K-LAS-ARTREG-NYPON-BENREG              
079300                        END-IF                                            
079400                     ELSE                                                 
079500                        PERFORM K-LAS-ARTREG-NYPON-BENREG                 
079600                     END-IF                                               
079700                 ELSE                                                     
079800                    PERFORM P-KOLLA-RASA                                  
079900                    PERFORM H-LAS-NYPON                                   
080000                    IF SEGMENT-FINNS                                      
080100                       PERFORM O-KOLLA-RESBED-ARTUTG                      
080200                       IF FINNS-RASA = JA                                 
080300                          MOVE FEL-14(SPIND)        TO                    
080400                                                  MOD-TEMFSINF            
080500                       ELSE                                               
080600                          MOVE FEL-2(SPIND)         TO                    
080700                                                  MOD-TEMFSINF            
080800                       END-IF                                             
080900                       MOVE IDARTNR-WS TO IDARTNR-NY-WS                   
081000                    ELSE                                                  
081100                       IF IDARTNR-WS NOT = ZERO                           
081200                         IF FINNS-RASA = JA                               
081300                            MOVE FEL-14 (SPIND) TO MOD-TEMFSFEL           
081400                         ELSE                                             
081500                            MOVE FEL-2 (SPIND) TO MOD-TEMFSFEL            
081600                         END-IF                                           
081700                         MOVE IDARTNR-WS TO IDARTNR-NY-WS                 
081800                       ELSE                                               
081900                         MOVE FEL-8 (SPIND) TO MOD-TEMFSFEL               
082000                       END-IF                                             
082100                    END-IF                                                
082200                 END-IF                                                   
082300              END-IF                                                      
082400           END-IF                                                         
082500        ELSE                                                              
082600           IF INPUT-RETT = 'J'                                            
082700              MOVE FEL-1 (SPIND) TO MOD-TEMFSFEL                          
082800              PERFORM L-RENSA-INFAELT                                     
082900              MOVE FEL-ERR-IDARTNR TO WS-ERROR-UPDX                       
083000*             MOVE FEL-ERR-IDARTNR TO MSG-KOM-IDMFSMED                    
083100*             MOVE '1'             TO MSG-KOM-KDSVAR                      
083200           END-IF                                                         
083300        END-IF                                                            
083400        MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                 
083500        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
083600        IF IDARTNR-NY-WS NOT = ZERO                                       
083700           MOVE IDARTNR-NY-WS TO MOD-IDARTNR-NY                           
083800           INSPECT MOD-IDARTNR-NY REPLACING LEADING ZERO BY SPACE         
083900           IF EGEN-BILD                                                   
084000           AND NOT MFS-UPD-X                                              
084100              MOVE ALL '+' TO MSGI-WMSGINIT                               
084200              MOVE '001'             TO MSGI-KDCALL                       
084300              MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                       
084400              MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                 
084500              MOVE '1116'            TO MSGI-IDTRANS                      
084600              MOVE IDARTNR-NY-WS     TO MSGI-IDARTNR                      
084700              CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                  
084800           END-IF                                                         
084900        END-IF                                                            
085000     END-IF                                                               
085100                                                                          
085200     IF TRANS-TO-1192 = JA                                                
085300        MOVE MID-W1I11601 TO PROGSW-MID-W1I11601                          
085400        MOVE MOD-W1O11601 TO PROGSW-MOD-W1O11601                          
085500        IF MFS-UPD-X                                                      
085600*          X-TRANS SKALL TILL 1192 HA IDTRANS=111F                        
085700           MOVE '111F' TO P-IDTRANS                                       
085800           MOVE OK-BEHANDLAD TO MSG-KOM-IDMFSMED                          
085900           PERFORM IMS-INSERT-WMSGKOM-MSG                                 
086000        END-IF                                                            
086100        PERFORM IMS-INSERT-ALT                                            
086200     ELSE                                                                 
086300        IF MFS-UPD-X                                                      
086400*          X-TRANS FRÅN DISPATCHERN SKALL INTE SVARA EN SKÄRM             
086500           IF  WS-ERROR-UPDX  = SPACE                                     
086600             MOVE FEL-ERR-FIELD TO MSG-KOM-IDMFSMED                       
086700           ELSE                                                           
086800             MOVE WS-ERROR-UPDX TO MSG-KOM-IDMFSMED                       
086900           END-IF                                                         
087000           MOVE '1'           TO MSG-KOM-KDSVAR                           
087100           PERFORM IMS-INSERT-WMSGKOM-MSG                                 
087200        ELSE                                                              
087300           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
087400           PERFORM IMS-INSERT-MSG                                         
087500        END-IF                                                            
087600     END-IF                                                               
087700                                                                          
087800     MOVE ZERO TO RETURN-CODE                                             
087900     GOBACK                                                               
088000     .                                                                    
088100     EJECT                                                                
088200 A-INIT SECTION.                                                          
088300     SKIP2                                                                
088400     IF MSG-DUBBLA-TRANSKODER                                             
088500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I11601                 
088600       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
088700       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
088800                                               P-KDMFSFOR                 
088900     ELSE                                                                 
089000       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W1I11601                 
089100       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
089200       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
089300     END-IF                                                               
089400     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
089500     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
089600     MOVE MFS-IDTRANS                     TO WS-IDTRANS                   
089700                                                                          
089800     IF MFS-IDTRANS = '1116'                                              
089900     OR MFS-UPD-X                                                         
090000        CONTINUE                                                          
090100*       ******************************************************            
090200*       * X-TRANSEN KOMMER FRÅN RUTIN W100B1 OCH ÄR          *            
090300*       * I FÖRVÄG KONTROLLERAD AV PGM W1116100.             *            
090400*       ******************************************************            
090500     ELSE                                                                 
090600        MOVE SPACE TO MFS-KDTRTYP                                         
090700        MOVE NEJ TO UPPDATERING                                           
090800     END-IF                                                               
090900                                                                          
091000     ACCEPT DAGENS-AAMMDD FROM DATE                                       
091100                                                                          
091200     MOVE DAGENS-AAMMDD         TO DAT-I-TIDATUM                          
091300     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
091400     PERFORM S99-WDATKONV                                                 
091500                                                                          
091600     IF DAT-KDSVAR-OK                                                     
091700        MOVE DAT-TIAA-VECKA     TO SPAR-DAGENS-AA                         
091800        MOVE DAT-TIVV           TO SPAR-DAGENS-VV                         
091900     END-IF                                                               
092000                                                                          
092100     MOVE NEJ TO KOPIERING                                                
092200                 TRANS-TO-1192                                            
092300                 VAECKNING                                                
092400                 FINNS-REG-PA-NYPON                                       
092500                                                                          
092600     MOVE LOW-VALUE TO MSG-AREA                                           
092700     MOVE 'W1O116N1' TO MFS-IDMOD                                         
092800     MOVE '1116' TO MOD-IDTRANS                                           
092900                                                                          
093000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
093100                             MOD-IDARTNR-NY                               
093200                             MOD-TEMFSFEL                                 
093300                             MOD-TEMFSINF                                 
093400                                                                          
093500     IF MFS-UPD-X                                                         
093600****************  DISPATCHANROP                                           
093700                                                                          
093800       IF MID-IDARTNR-IN = ALL '+' OR SPACE                               
093900          INSPECT MID-IDARTNR-UT REPLACING LEADING                        
094000                      SPACE BY ZERO                                       
094100          MOVE MID-IDARTNR-UT TO WS-IDARTNR                               
094200          IF EGEN-BILD                                                    
094300             MOVE JA TO UPPDATERING                                       
094400          END-IF                                                          
094500       ELSE                                                               
094600          MOVE MID-IDARTNR-IN TO WS-IDARTNR                               
094700          IF MFS-UPD-X                                                    
094800             CONTINUE                                                     
094900          ELSE                                                            
095000             MOVE SPACE TO MFS-KDTRTYP                                    
095100          END-IF                                                          
095200          MOVE NEJ TO UPPDATERING                                         
095300       END-IF                                                             
095400                                                                          
095500     ELSE                                                                 
095600       IF MID-IDARTNR-IN = ALL '+'                                        
095700          IF EGEN-BILD                                                    
095800             MOVE JA TO UPPDATERING                                       
095900          END-IF                                                          
096000       ELSE                                                               
096100          MOVE SPACE       TO MFS-KDTRTYP                                 
096200          MOVE NEJ TO UPPDATERING                                         
096300       END-IF                                                             
096400       MOVE ALL '+' TO MSGI-WMSGINIT                                      
096500       MOVE '001'             TO MSGI-KDCALL                              
096600       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
096700       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
096800       MOVE '1116'            TO MSGI-IDTRANS                             
096900       IF EGEN-BILD                                                       
097000         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
097100       END-IF                                                             
097200                                                                          
097300       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
097400       MOVE MSGI-IDDC         TO WS-IDDC                                  
097500       IF MFS-QUERY                                                       
097600         MOVE MSGI-IDARTNR TO WS-IDARTNR                                  
097700       ELSE                                                               
097800         MOVE MID-IDARTNR-UT TO WS-IDARTNR                                
097900       END-IF                                                             
098000       INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                     
098100     END-IF                                                               
098200                                                                          
098300     IF MID-IDARTNR-NY = ALL '+' OR SPACE                                 
098400        MOVE ZERO TO WS-NY-IDARTNR                                        
098500     ELSE                                                                 
098600        INSPECT MID-IDARTNR-NY REPLACING LEADING SPACE BY ZERO            
098700        IF MID-IDARTNR-NY NUMERIC                                         
098800          MOVE MID-IDARTNR-NY TO WS-NY-IDARTNR                            
098900        ELSE                                                              
099000          MOVE ZERO TO WS-NY-IDARTNR                                      
099100        END-IF                                                            
099200     END-IF                                                               
099300                                                                          
099400     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
099500        MOVE +1 TO SPIND                                                  
099600     ELSE                                                                 
099700        MOVE +2 TO SPIND                                                  
099800     END-IF                                                               
099900     .                                                                    
100000     EJECT                                                                
100100 B-REG-ART-ARTREG-UPPDAT-NYPON SECTION.                                   
100200     SKIP2                                                                
100300     IF IDARTNR-WS = IDARTNR-NY-WS                                        
100400        MOVE JA TO INPUT-RETT                                             
100500                                                                          
100600        IF MID-KDPRODSL = C-KDPRODSL-19                                   
100700           IF MID-KDSORT = ALL '+'                                        
100800              MOVE C-PS19-DEFAULT-KDSORT    TO MID-KDSORT                 
100900                                               MOD-KDSORT-IN              
101000           END-IF                                                         
101100           IF MID-KDYTBEH = ALL '+'                                       
101200              MOVE C-PS19-DEFAULT-KDYTBEH   TO MID-KDYTBEH                
101300                                               MOD-KDYTBEH-IN             
101400           END-IF                                                         
101500           IF MID-IDPROJ = ALL '+'                                        
101600              MOVE C-PS19-DEFAULT-IDPROJ    TO MID-IDPROJ                 
101700                                               MOD-IDPROJ-IN              
101800                                                WS-IDPROJ                 
101900           END-IF                                                         
102000           IF MID-KDFARLIG = ALL '+'                                      
102100              MOVE C-PS19-DEFAULT-KDFARLIG  TO MID-KDFARLIG               
102200                                               MOD-KDFARLIG-IN            
102300           END-IF                                                         
102400           IF MID-KDBPSR  = ALL '+'                                       
102500              MOVE C-PS19-DEFAULT-KDBPSR    TO MID-KDBPSR                 
102600                                               MOD-KDBPSR-IN              
102700           END-IF                                                         
102800           IF MID-IDPROJK = ALL '+'                                       
102900              MOVE C-PS19-DEFAULT-IDPROJK   TO MID-IDPROJK                
103000                                               MOD-IDPROJK-IN             
103100                                                WS-IDPROJK                
103200           END-IF                                                         
103300           IF MID-FLLSRDEL = ALL '+'                                      
103400              MOVE C-PS19-DEFAULT-FLLSRDEL  TO MID-FLLSRDEL               
103500                                               MOD-FLLSRDEL-IN            
103600           END-IF                                                         
103700        END-IF                                                            
103800                                                                          
103900        PERFORM S02-NYA-ARTREG-DATAELEMENT                                
104000                                                                          
104100        MOVE IDARTNR-WS TO W-IDARTNR                                      
104200        PERFORM IMS-GET-ARTG01                                            
104300                                                                          
104400        IF MID-KDPRODSL = ALL '+'                                         
104500           MOVE MFS-RENSA-FAELT TO MOD-KDPRODSL-IN                        
104600           IF ARTG01-ART-KDPRODSL = ZERO                                  
104700              MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                 
104800              MOVE NEJ TO INPUT-RETT                                      
104900           END-IF                                                         
105000           MOVE ARTG01-ART-KDPRODSL TO WS-KDPRODSL                        
105100           PERFORM S94-KOLLA-IDDC                                         
105200           IF INPUT-RETT = JA                                             
105300             PERFORM S92-CHECK-SUPPLIER                                   
105400           END-IF                                                         
105500        ELSE                                                              
105600           MOVE ZERO TO WS-KDPRODSL                                       
105700           IF MID-KDPRODSL NUMERIC                                        
105800              MOVE MID-KDPRODSL   TO TEST-KDPRODSL                        
105900*                                    WS-KDPRODSL-LOK-ART                  
106000              IF GOOD-KDPRODSL                                            
106100                 MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-ATTR            
106200                 MOVE MID-KDPRODSL TO WS-KDPRODSL                         
106300                 PERFORM S94-KOLLA-IDDC                                   
106400                 IF INPUT-RETT = JA                                       
106500                   PERFORM S92-CHECK-SUPPLIER                             
106600                 END-IF                                                   
106700              ELSE                                                        
106800                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR              
106900                 MOVE NEJ TO INPUT-RETT                                   
107000              END-IF                                                      
107100           ELSE                                                           
107200              MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                 
107300              MOVE NEJ TO INPUT-RETT                                      
107400           END-IF                                                         
107500        END-IF                                                            
107600                                                                          
107700        IF INPUT-RETT = JA                                                
107800           PERFORM S95-KOLLA-IDFTG                                        
107900        END-IF                                                            
108000                                                                          
108100        PERFORM S06-KOP-NYPON-DATAELEMENT                                 
108200                                                                          
108300        MOVE ZERO TO WS-IDPROENH(1) WS-IDPROENH(2) WS-IDPROENH(3)         
108400                                                                          
108500        IF MID-IDLEVNR = ALL '+'                                          
108600           IF MID-BELEV = ALL '+'                                         
108700              MOVE MFS-RENSA-FAELT TO MOD-BELEV-IN                        
108800                                      MOD-IDLEVNR-IN                      
108900           ELSE                                                           
109000              MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR                   
109100                                         MOD-IDLEVNR-ATTR                 
109200              MOVE NEJ TO INPUT-RETT                                      
109300           END-IF                                                         
109400        ELSE                                                              
109500           IF MID-IDLEVNR(1:1) NOT = ' ' AND '0' AND '+'                  
109600               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-ATTR              
109700           ELSE                                                           
109800              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR                 
109900              MOVE NEJ TO INPUT-RETT                                      
110000           END-IF                                                         
110100                                                                          
110200           IF MID-BELEV = ALL '+' OR SPACE                                
110300              IF MID-IDLEVNR(1:1) NOT = ' ' AND '0' AND '+'               
110400                 IF MID-IDLEVNR = '1002 '                                 
110500                    MOVE MFS-RENSA-FAELT TO MOD-BELEV-IN                  
110600                 ELSE                                                     
110700                    MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR             
110800                    MOVE NEJ TO INPUT-RETT                                
110900                 END-IF                                                   
111000              END-IF                                                      
111100           ELSE                                                           
111200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEV-ATTR                 
111300           END-IF                                                         
111400        END-IF                                                            
111500                                                                          
111600        IF MID-FLGAMART = JA OR NEJ                                       
111700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLGAMART-ATTR                 
111800           MOVE MID-FLGAMART TO WS-FLGAMART                               
111900        ELSE                                                              
112000           IF MID-FLGAMART = ALL '+'                                      
112100              MOVE NEJ   TO WS-FLGAMART  MOD-FLGAMART-IN                  
112200              MOVE MFS-RENSA-FAELT TO    MOD-FLGAMART-IN                  
112300           ELSE                                                           
112400              MOVE MFS-ALFA-FAELT-FEL TO MOD-FLGAMART-ATTR                
112500              MOVE NEJ TO INPUT-RETT                                      
112600           END-IF                                                         
112700        END-IF                                                            
112800                                                                          
112900        IF MID-FLRSBEART = JA OR NEJ                                      
113000           MOVE MID-FLRSBEART TO WS-FLRSBEART                             
113100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRSBEART-ATTR                
113200        ELSE                                                              
113300           IF MID-FLRSBEART = ALL '+'                                     
113400              MOVE NEJ TO WS-FLRSBEART   MOD-FLRSBEART-IN                 
113500           ELSE                                                           
113600              MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRSBEART-ATTR               
113700              MOVE NEJ TO INPUT-RETT                                      
113800           END-IF                                                         
113900        END-IF                                                            
114000                                                                          
114100        IF MID-IDSKYLT = 'GB ' OR 'S  '                                   
114200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-ATTR                  
114300           MOVE MID-IDSKYLT TO WS-IDSKYLT                                 
114400        ELSE                                                              
114500           IF MID-IDSKYLT = ALL '+'                                       
114600              MOVE 'S  ' TO WS-IDSKYLT  MOD-IDSKYLT-IN                    
114700           ELSE                                                           
114800              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-ATTR                 
114900              MOVE NEJ TO INPUT-RETT                                      
115000           END-IF                                                         
115100        END-IF                                                            
115200        INSPECT MID-BEART REPLACING ALL '<' BY SPACE                      
115300        INSPECT MID-BEART REPLACING ALL '>' BY SPACE                      
115400                                                                          
115500        IF MID-BEART = ALL '+' OR SPACE                                   
115600           MOVE MFS-RENSA-FAELT TO MOD-BEART-IN                           
115700           IF VAECKNING = JA                                              
115800              MOVE IDARTNR-WS TO W-IDARTNR                                
115900              MOVE 'S  ' TO W-IDSKYLT                                     
116000              PERFORM IMS-GET-BENA11-CSEQ                                 
116100              MOVE BENA-TEXT-BEART TO WS-BEART  WS-BEART-SVE              
116200           ELSE                                                           
116300              IF WS-IDSKYLT = 'GB '                                       
116400                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-ATTR              
116500                 MOVE NEJ TO INPUT-RETT                                   
116600              END-IF                                                      
116700              IF ARTG01-ART-BEART-SVE = SPACE                             
116800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR                
116900                 MOVE NEJ TO INPUT-RETT                                   
117000              ELSE                                                        
117100                 MOVE ARTG01-ART-BEART-SVE TO WS-BEART                    
117200              END-IF                                                      
117300           END-IF                                                         
117400        ELSE                                                              
117500        IF VAECKNING = JA                                                 
117600           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR                      
117700           MOVE NEJ TO INPUT-RETT                                         
117800        ELSE                                                              
117900           MOVE MID-BEART TO WS-BEART                                     
118000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-ATTR                    
118100        END-IF                                                            
118200     END-IF                                                               
118300                                                                          
118400     IF MID-IDRITN = ALL '+'                                              
118500        MOVE MFS-RENSA-FAELT TO MOD-IDRITN-IN                             
118600        IF ARTG01-ART-IDRITN = SPACE                                      
118700           MOVE NEJ TO INPUT-RETT                                         
118800           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                     
118900        END-IF                                                            
119000     ELSE                                                                 
119100        IF MID-IDRITN = SPACE                                             
119200           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                     
119300           MOVE NEJ TO INPUT-RETT                                         
119400        ELSE                                                              
119500           MOVE MID-IDRITN TO WS-IDRITN                                   
119600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDRITN-ATTR                   
119700        END-IF                                                            
119800     END-IF                                                               
119900                                                                          
120000     IF MID-IDPROJ = ALL '+'                                              
120100        MOVE MFS-RENSA-FAELT TO MOD-IDPROJ-IN                             
120200        MOVE ARTG01-ART-IDPROJ TO WS-IDPROJ                               
120300     ELSE                                                                 
120400        MOVE MID-IDPROJ TO WS-IDPROJ                                      
120500     END-IF                                                               
120600                                                                          
120700     PERFORM S14-GODK-PROJ-MFS-RAETT-FEL                                  
120800                                                                          
120900*    IF KDPRODSL-LOK-ART                                                  
121000*       IF WS-IDPROJ NOT = 'PROD'                                         
121100*          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJ-ATTR                     
121200*          MOVE NEJ TO INPUT-RETT                                         
121300*       END-IF                                                            
121400*    END-IF                                                               
121500                                                                          
121600     IF MID-IDPROENH-1 = ALL '+'                                          
121700        MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-1-IN                         
121800     ELSE                                                                 
121900        IF MID-IDPROENH-1 NUMERIC                                         
122000           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-1-ATTR                
122100        ELSE                                                              
122200           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-1-ATTR                
122300           MOVE NEJ TO INPUT-RETT                                         
122400        END-IF                                                            
122500     END-IF                                                               
122600     IF MID-IDPROENH-2 = ALL '+'                                          
122700        MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-2-IN                         
122800     ELSE                                                                 
122900        IF MID-IDPROENH-2 NUMERIC                                         
123000           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-2-ATTR                
123100        ELSE                                                              
123200           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-2-ATTR                
123300           MOVE NEJ TO INPUT-RETT                                         
123400        END-IF                                                            
123500     END-IF                                                               
123600     IF MID-IDPROENH-3 = ALL '+'                                          
123700        MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-3-IN                         
123800     ELSE                                                                 
123900        IF MID-IDPROENH-3 NUMERIC                                         
124000           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-3-ATTR                
124100        ELSE                                                              
124200           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-3-ATTR                
124300           MOVE NEJ TO INPUT-RETT                                         
124400        END-IF                                                            
124500     END-IF                                                               
124600                                                                          
124700     IF MID-IDBERED = ALL '+'                                             
124800        MOVE MFS-RENSA-FAELT TO MOD-IDBERED-IN                            
124900        IF ARTG01-ART-IDBERED = ZERO                                      
125000           MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                     
125100           MOVE NEJ TO INPUT-RETT                                         
125200        END-IF                                                            
125300     ELSE                                                                 
125400        IF MID-IDBERED NUMERIC                                            
125500           IF MID-IDBERED > ZERO                                          
125600              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDBERED-ATTR                
125700           ELSE                                                           
125800              MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                  
125900              MOVE NEJ TO INPUT-RETT                                      
126000           END-IF                                                         
126100        ELSE                                                              
126200           MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                     
126300           MOVE NEJ TO INPUT-RETT                                         
126400        END-IF                                                            
126500     END-IF                                                               
126600                                                                          
126700        IF MID-KDSORT = ALL '+'                                           
126800           MOVE MFS-RENSA-FAELT TO MOD-KDSORT-IN                          
126900           IF ARTG01-ART-KDSORT = SPACE                                   
127000              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                  
127100              MOVE NEJ TO INPUT-RETT                                      
127200           ELSE                                                           
127300              MOVE ARTG01-ART-KDSORT TO WS-KDSORT                         
127400           END-IF                                                         
127500        ELSE                                                              
127600           IF MID-KDSORT = 'ST' OR 'PA' OR 'SA' OR 'KG' OR 'M '           
127700           OR ' M' OR ' L' OR 'L ' OR 'MM' OR 'G ' OR ' G'                
127800           OR 'C2' OR 'M2' OR 'ML' OR 'SW' OR 'TM' OR 'HW'                
127900              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-ATTR                
128000              MOVE MID-KDSORT TO WS-KDSORT                                
128100           ELSE                                                           
128200              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                  
128300              MOVE NEJ TO INPUT-RETT                                      
128400           END-IF                                                         
128500        END-IF                                                            
128600                                                                          
128700        IF MID-IDAO = ALL '+' OR SPACE                                    
128800           MOVE MFS-RENSA-FAELT TO MOD-IDAO-IN                            
128900           IF ARTG01-ART-IDAO = SPACE                                     
129000              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDAO-ATTR                    
129100              MOVE NEJ TO INPUT-RETT                                      
129200           END-IF                                                         
129300        ELSE                                                              
129400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-ATTR                     
129500        END-IF                                                            
129600                                                                          
129700        PERFORM BA-KOLLA-TISOP                                            
129800        IF INPUT-RETT = JA                                                
129900           PERFORM BB-KOLLA-TIFINLV                                       
130000        END-IF                                                            
130100                                                                          
130200        IF MID-IDFKNGRP = ALL '+'                                         
130300           MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-IN                        
130400           IF ARTG01-ART-IDFKNGRP = ZERO                                  
130500              MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                 
130600              MOVE NEJ TO INPUT-RETT                                      
130700           ELSE                                                           
130800              MOVE ARTG01-ART-IDFKNGRP TO WS-TEST-IDFKNGRP                
130900           END-IF                                                         
131000        ELSE                                                              
131100           IF MID-IDFKNGRP NUMERIC                                        
131200              IF MID-IDFKNGRP > ZERO                                      
131300                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-ATTR            
131400                 MOVE MID-IDFKNGRP TO WS-TEST-IDFKNGRP                    
131500              ELSE                                                        
131600                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR              
131700                 MOVE NEJ TO INPUT-RETT                                   
131800              END-IF                                                      
131900           ELSE                                                           
132000              MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                 
132100              MOVE NEJ TO INPUT-RETT                                      
132200           END-IF                                                         
132300        END-IF                                                            
132400                                                                          
132500        IF MID-TEARTNOT-2 = ALL '+'                                       
132600           MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-2                         
132700        ELSE                                                              
132800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-2-ATTR               
132900        END-IF                                                            
133000                                                                          
133100        IF MID-TEARTNOT-7 = ALL '+'                                       
133200           MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-7                         
133300        ELSE                                                              
133400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-7-ATTR               
133500        END-IF                                                            
133600                                                                          
133700        PERFORM S23-KOLLA-RASA                                            
133800        PERFORM S24-KOLLA-CROSS                                           
133900                                                                          
134000        IF INPUT-RETT = JA                                                
134100           PERFORM S93-KOLLA-SOFTWARE                                     
134200        END-IF                                                            
134300                                                                          
134400        IF VAECKNING = NEJ                                                
134500           IF INPUT-RETT = JA                                             
134600              IF WS-FLGAMART = NEJ                                        
134700                 PERFORM S07-KOLLA-OM-BEN-FINNS                           
134800                 IF INPUT-RETT = JA                                       
134900                    PERFORM BC-UPPDATERA-NYPON                            
135000                    MOVE MED-1(SPIND)TO MOD-TEMFSINF                      
135100                    MOVE JA TO TRANS-TO-1192                              
135200                 ELSE                                                     
135300                    MOVE FEL-4(SPIND) TO MOD-TEMFSFEL                     
135400                 END-IF                                                   
135500              ELSE                                                        
135600                 MOVE NEJ TO INPUT-RETT                                   
135700                 MOVE FEL-2 (SPIND) TO MOD-TEMFSFEL                       
135800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLGAMART-ATTR             
135900                 MOVE FEL-ERR-FLAGMART  TO  WS-ERROR-UPDX                 
136000              END-IF                                                      
136100           ELSE                                                           
136200              IF MOD-TEMFSFEL = MFS-RENSA-FAELT                           
136300                 MOVE FEL-4(SPIND) TO MOD-TEMFSFEL                        
136400              END-IF                                                      
136500           END-IF                                                         
136600        ELSE                                                              
136700           IF INPUT-RETT = JA                                             
136800              IF WS-FLGAMART = JA                                         
136900                 PERFORM BC-UPPDATERA-NYPON                               
137000                 PERFORM EA-RIVNING-AV-EV-ERSATTNING                      
137100                 MOVE JA TO TRANS-TO-1192                                 
137200                 MOVE MED-1(SPIND)TO MOD-TEMFSINF                         
137300              ELSE                                                        
137400                 MOVE FEL-3 (SPIND) TO MOD-TEMFSFEL                       
137500                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLGAMART-ATTR             
137600                 MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-ATTR                
137700                          MOD-BEART-ATTR  MOD-IDSKYLT-ATTR                
137800                                          MOD-FLRSBEART-ATTR              
137900              END-IF                                                      
138000           ELSE                                                           
138100              IF MOD-TEMFSFEL = MFS-RENSA-FAELT                           
138200                 MOVE FEL-4(SPIND) TO MOD-TEMFSFEL                        
138300              END-IF                                                      
138400              MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-ATTR                   
138500                       MOD-BEART-ATTR  MOD-IDSKYLT-ATTR                   
138600                                       MOD-FLRSBEART-ATTR                 
138700           END-IF                                                         
138800        END-IF                                                            
138900     END-IF                                                               
139000     .                                                                    
139100     EJECT                                                                
139200 BA-KOLLA-TISOP     SECTION.                                              
139300     SKIP2                                                                
139400     MOVE MID-TISOP      TO XX-TISOP                                      
139500     MOVE '+' TO XX-DAG-SOP                                               
139600     IF XX-TISOP = ALL '+'                                                
139700        MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                          
139800        MOVE NEJ               TO INPUT-RETT                              
139900*       MOVE MFS-RENSA-FAELT TO MOD-TISOP-IN                              
140000*       PERFORM S97-LAES-XXAQ                                             
140100*       IF SEGMENT-FINNS                                                  
140200*          IF     XXAQ-1132-TIFINLEV = ZERO                               
140300*          AND XXAQ-1132-TIPRODSTA = +111111                              
140400*             PERFORM HA-PLOCKA-NYASTE-TISERLEV                           
140500*             IF W-SPAR-TISERLEV = +9999999                               
140600*                IF ARTG01-ART-KDPRODSL = 15                              
140700*                OR ARTG01-ART-FLUNIKRD = JA                              
140800*                OR ARTG01-ART-IDLEVNR     = '9998 '                      
140900*                   MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR              
141000*                   MOVE NEJ                 TO INPUT-RETT                
141100*                ELSE                                                     
141200*                   MOVE +999999             TO WS-TISOP-AAMMDD           
141300*                   MOVE MFS-NUM-FAELT-RAETT                              
141400*                                         TO MOD-TISOP-ATTR               
141500*                END-IF                                                   
141600*             ELSE                                                        
141700*                MOVE W-SPAR-TISERLEV      TO TMP1-YYMMDD                 
141800*                MOVE DAGENS-AAMMDD        TO TMP2-YYMMDD                 
141900*                PERFORM WY2000P1                                         
142000*                IF TMP1-YYMMDD > TMP2-YYMMDD                             
142100*                   MOVE W-SPAR-TISERLEV     TO WS-TISOP-AAMMDD           
142200*                   MOVE MFS-NUM-FAELT-RAETT                              
142300*                                         TO MOD-TISOP-ATTR               
142400*                ELSE                                                     
142500*                   MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR              
142600*                   MOVE NEJ                  TO INPUT-RETT               
142700*                END-IF                                                   
142800*             END-IF                                                      
142900*          ELSE                                                           
143000*             MOVE XXAQ-1132-TIFINLEV      TO TMP1-YYMMDD                 
143100*             MOVE DAGENS-AAMMDD           TO TMP2-YYMMDD                 
143200*             PERFORM WY2000P1                                            
143300*             IF TMP1-YYMMDD > TMP2-YYMMDD                                
143400*                MOVE XXAQ-1132-TIFINLEV     TO WS-TISOP-AAMMDD           
143500*                MOVE MFS-NUM-FAELT-RAETT TO MOD-TISOP-ATTR               
143600*             ELSE                                                        
143700*                MOVE MFS-NUM-FAELT-FEL      TO MOD-TISOP-ATTR            
143800*                MOVE NEJ                    TO INPUT-RETT                
143900*             END-IF                                                      
144000*          END-IF                                                         
144100*       ELSE                                                              
144200*          PERFORM HA-PLOCKA-NYASTE-TISERLEV                              
144300*          IF W-SPAR-TISERLEV = +9999999                                  
144400*             MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                    
144500*             MOVE NEJ                  TO INPUT-RETT                     
144600*          ELSE                                                           
144700*             MOVE W-SPAR-TISERLEV      TO TMP1-YYMMDD                    
144800*             MOVE DAGENS-AAMMDD        TO TMP2-YYMMDD                    
144900*             PERFORM WY2000P1                                            
145000*             IF TMP1-YYMMDD > TMP2-YYMMDD                                
145100*                MOVE W-SPAR-TISERLEV       TO WS-TISOP-AAMMDD            
145200*                MOVE MFS-NUM-FAELT-RAETT TO MOD-TISOP-ATTR               
145300*             ELSE                                                        
145400*                MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                 
145500*                MOVE NEJ                  TO INPUT-RETT                  
145600*             END-IF                                                      
145700*          END-IF                                                         
145800*       END-IF                                                            
145900     ELSE                                                                 
146000        MOVE '1' TO XX-DAG-SOP                                            
146100                                                                          
146200        IF XX-TISOP NUMERIC                                               
146300           MOVE XX-TISOP      TO WS-TISOP                                 
146400           IF WS-TISOP = 99999                                            
146500              MOVE WS-KDPRODSL   TO TEST-KDPRODSL                         
146600              IF KDPRODSL-PARTS-ACC OR KDPRODSL-SERVICES                  
146700                 MOVE ARTG01-ART-KDPRODSL                                 
146800                                 TO TEST-KDPRODSL                         
146900                 IF KDPRODSL-ACC                                          
147000                 OR ARTG01-ART-FLUNIKRD = JA                              
147100                 OR ARTG01-ART-IDLEVNR     = '9998 '                      
147200                    MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR              
147300                    MOVE NEJ                  TO INPUT-RETT               
147400                 ELSE                                                     
147500                    PERFORM S97-LAES-XXAQ                                 
147600                    IF SEGMENT-FINNS                                      
147700                       IF XXAQ-1132-TIFINLEV     = ZERO                   
147800                       AND XXAQ-1132-TIPRODSTA = +111111                  
147900                          PERFORM HA-PLOCKA-NYASTE-TISERLEV               
148000                          IF W-SPAR-TISERLEV = +9999999                   
148100                             MOVE MFS-NUM-FAELT-RAETT TO                  
148200                                       MOD-TISOP-ATTR                     
148300                             MOVE WS-TISOP TO AAVVD                       
148400                             MOVE +9 TO D                                 
148500                             MOVE AAVVD TO WS-TISOP                       
148600                          ELSE                                            
148700                             MOVE MFS-NUM-FAELT-FEL      TO               
148800                                         MOD-TISOP-ATTR                   
148900                             MOVE NEJ TO INPUT-RETT                       
149000                          END-IF                                          
149100                       ELSE                                               
149200                          MOVE MFS-NUM-FAELT-FEL      TO                  
149300                                         MOD-TISOP-ATTR                   
149400                          MOVE NEJ TO INPUT-RETT                          
149500                       END-IF                                             
149600                    ELSE                                                  
149700                       MOVE MFS-NUM-FAELT-FEL         TO                  
149800                                          MOD-TISOP-ATTR                  
149900                       MOVE NEJ TO INPUT-RETT                             
150000                    END-IF                                                
150100                 END-IF                                                   
150200              ELSE                                                        
150300                 MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                 
150400                 MOVE NEJ TO INPUT-RETT                                   
150500                                                                          
150600              END-IF                                                      
150700           ELSE                                                           
150800              MOVE WS-TISOP       TO DAT-I-TIDATUM                        
150900              MOVE 'AAVVD '       TO DAT-KDDATFORM                        
151000              PERFORM S99-WDATKONV                                        
151100              IF DAT-KDSVAR-OK                                            
151200                PERFORM S98-OM-TVA-AAR                                    
151300                MOVE WS-TISOP     TO TMP1-YYWWD                           
151400                MOVE AAVVD        TO TMP2-YYWWD                           
151500                MOVE DAT-TIAAVVD  TO TMP3-YYWWD                           
151600                PERFORM WY2000Q2                                          
151700                IF TMP1-YYWWD < TMP2-YYWWD                                
151800                   MOVE MFS-NUM-FAELT-RAETT                               
151900                                   TO MOD-TISOP-ATTR                      
152000                ELSE                                                      
152100                   MOVE MFS-NUM-FAELT-FEL                                 
152200                                     TO MOD-TISOP-ATTR                    
152300                   MOVE NEJ TO INPUT-RETT                                 
152400                END-IF                                                    
152500              ELSE                                                        
152600                 MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                 
152700                 MOVE NEJ TO INPUT-RETT                                   
152800              END-IF                                                      
152900           END-IF                                                         
153000        ELSE                                                              
153100           MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                       
153200           MOVE NEJ TO INPUT-RETT                                         
153300        END-IF                                                            
153400     END-IF                                                               
153500     .                                                                    
153600     EJECT                                                                
153700 BB-KOLLA-TIFINLV   SECTION.                                              
153800     SKIP2                                                                
153900     PERFORM S25-TIFINLV-FRAN-SOP                                         
154000     MOVE SPAR-TIFINLV-AAVVD TO XX-TIFINLV                                
154100                                                                          
154200     IF  XX-AAR = '99'                                                    
154300     AND XX-VECKA = '99'                                                  
154400         MOVE '9' TO XX-DAG                                               
154500     ELSE                                                                 
154600         MOVE '1' TO XX-DAG                                               
154700     END-IF                                                               
154800                                                                          
154900     MOVE XX-TIFINLV     TO WS-TIFINLV                                    
155000     MOVE WS-TIFINLV     TO DAT-I-TIDATUM                                 
155100     MOVE 'AAVVD '       TO DAT-KDDATFORM                                 
155200     PERFORM S99-WDATKONV                                                 
155300     IF DAT-KDSVAR-OK                                                     
155400        PERFORM S98-OM-TVA-AAR                                            
155500        MOVE WS-TIFINLV         TO TMP1-YYWWD                             
155600        MOVE AAVVD              TO TMP2-YYWWD                             
155700        MOVE DAT-TIAAVVD        TO TMP3-YYWWD                             
155800        PERFORM WY2000Q2                                                  
155900        IF  TMP1-YYWWD < TMP2-YYWWD                                       
156000        AND TMP1-YYWWD > TMP3-YYWWD                                       
156100           MOVE WS-TIFINLV TO AAVVD                                       
156200           MOVE +1         TO D                                           
156300           MOVE AAVVD      TO WS-TIFINLV                                  
156400        ELSE                                                              
156500           MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                       
156600           MOVE NEJ               TO INPUT-RETT                           
156700        END-IF                                                            
156800     ELSE                                                                 
156900        MOVE MFS-NUM-FAELT-FEL    TO MOD-TISOP-ATTR                       
157000        MOVE NEJ                  TO INPUT-RETT                           
157100     END-IF                                                               
157200     .                                                                    
157300     EJECT                                                                
157400 BC-UPPDATERA-NYPON SECTION.                                              
157500     SKIP2                                                                
157600     MOVE IDARTNR-WS TO W-IDARTNR                                         
157700     PERFORM IMS-GET-ARTG01                                               
157800     IF MID-IDPROJ = ALL '+'                                              
157900        CONTINUE                                                          
158000     ELSE                                                                 
158100        MOVE MID-IDPROJ        TO ARTG01-ART-IDPROJ                       
158200     END-IF                                                               
158300     IF MID-IDAO   = ALL '+'                                              
158400        CONTINUE                                                          
158500     ELSE                                                                 
158600        MOVE MID-IDAO          TO ARTG01-ART-IDAO                         
158700     END-IF                                                               
158800     IF MID-FLBYTES = ALL '+'                                             
158900        CONTINUE                                                          
159000     ELSE                                                                 
159100          MOVE WS-FLBYTES      TO ARTG01-ART-FLBYTES                      
159200     END-IF                                                               
159300     IF MID-FLPISK = ALL '+'                                              
159400        CONTINUE                                                          
159500     ELSE                                                                 
159600          MOVE WS-FLPISK       TO ARTG01-ART-FLPISK                       
159700     END-IF                                                               
159800     IF MID-KVARTVAGN = ALL '+'                                           
159900        CONTINUE                                                          
160000     ELSE                                                                 
160100        MOVE WS-KVARTVAGN    TO ARTG01-ART-KVARTVAGN                      
160200     END-IF                                                               
160300                                                                          
160400     IF MID-IDPROJK = ALL '+'                                             
160500        CONTINUE                                                          
160600     ELSE                                                                 
160700        MOVE WS-IDPROJK      TO ARTG01-ART-IDPROJK                        
160800     END-IF                                                               
160900                                                                          
161000     IF MID-IDARTNR-MOTSV = ALL '+'                                       
161100        CONTINUE                                                          
161200     ELSE                                                                 
161300        MOVE WS-IDARTNR-MOTSV TO ARTG01-ART-IDARTNR-MOTSV                 
161400     END-IF                                                               
161500                                                                          
161600     IF MID-TEORSAK-1 = ALL '+'                                           
161700        CONTINUE                                                          
161800     ELSE                                                                 
161900        MOVE MID-TEORSAK-1 TO ARTG01-ART-TEORSAK                          
162000     END-IF                                                               
162100     IF MID-KVPROG = ALL '+'                                              
162200        CONTINUE                                                          
162300     ELSE                                                                 
162400        MOVE WS-KVPROG       TO ARTG01-ART-KVPROG                         
162500     END-IF                                                               
162600                                                                          
162700     IF MID-TISOP NOT = ALL '+'                                           
162800        PERFORM S25-TIFINLV-FRAN-SOP                                      
162900        MOVE SPAR-TIFINLV-AAVVD TO XX-TIFINLV                             
163000     END-IF                                                               
163100     IF XX-TIFINLV = ALL '+'                                              
163200        MOVE WS-TIFINLV-AAMMDD       TO DAT-I-TIDATUM                     
163300        MOVE 'AAMMDD'                TO DAT-KDDATFORM                     
163400        PERFORM S99-WDATKONV                                              
163500        IF DAT-KDSVAR-OK                                                  
163600           MOVE WS-TIFINLV-AAMMDD  TO ARTG01-ART-DAFINLEV                 
163700           MOVE DAT-TISEKEL        TO ARTG01-ART-DAFINLEV (1:2)           
163800        END-IF                                                            
163900     ELSE                                                                 
164000        IF WS-TIFINLV = 99999                                             
164100           MOVE 99999999             TO ARTG01-ART-DAFINLEV               
164200        ELSE                                                              
164300           MOVE WS-TIFINLV              TO DAT-I-TIDATUM                  
164400           MOVE 'AAVVD '                TO DAT-KDDATFORM                  
164500           PERFORM S99-WDATKONV                                           
164600           IF DAT-KDSVAR-OK                                               
164700              MOVE DAT-TIAAMMDD    TO ARTG01-ART-DAFINLEV                 
164800              MOVE DAT-TISEKEL     TO ARTG01-ART-DAFINLEV (1:2)           
164900           END-IF                                                         
165000        END-IF                                                            
165100     END-IF                                                               
165200                                                                          
165300     MOVE ARTG01-ART-TEORSAK         TO MOD-TEORSAK-1                     
165400     MOVE ARTG01-ART-FLPISK          TO MOD-FLPISK-UT                     
165500     MOVE ARTG01-ART-FLBYTES         TO MOD-FLBYTES-UT                    
165600     MOVE ARTG01-ART-IDPROJK         TO MOD-IDPROJK-UT                    
165700     MOVE ARTG01-ART-KVARTVAGN       TO MOD-KVARTVAGN-UT                  
165800     MOVE ARTG01-ART-IDARTNR-MOTSV   TO MOD-IDARTNR-MOTSV-UT              
165900     MOVE ARTG01-ART-DAFINLEV (3:6)  TO MOD-TIFINLV-UT                    
166000     MOVE WS-BEART                   TO MOD-BEART-UT                      
166100     MOVE ARTG01-ART-KVPROG          TO MOD-KVPROG-UT                     
166200     PERFORM IMS-REPL-ARTG                                                
166300     .                                                                    
166400     EJECT                                                                
166500 C-KOP-FRAN-ARTREG SECTION.                                               
166600     SKIP2                                                                
166700******************************************************************        
166800*                                                                         
166900*  ARTIKEL MAN KOPIERAR IFRÅN FINNS INTE PÅ NYPON-BASEN                   
167000*  OCH DÄRMED SKER KOPIERING BARA FRÅN ARTREG.                            
167100*                                                                         
167200******************************************************************        
167300                                                                          
167400     PERFORM S09-KOLLA-INDATA-KOP-ARTREG                                  
167500     IF INPUT-RETT = JA                                                   
167600        IF WS-FLGAMART = NEJ                                              
167700            PERFORM S07-KOLLA-OM-BEN-FINNS                                
167800            IF INPUT-RETT = JA                                            
167900                IF NYPON-ARTIKEL = JA                                     
168000                   PERFORM CA-REG-UPPDAT-NYPON-KOP                        
168100                END-IF                                                    
168200                MOVE MED-1(SPIND)TO MOD-TEMFSINF                          
168300                MOVE JA TO TRANS-TO-1192                                  
168400            ELSE                                                          
168500                MOVE FEL-4 (SPIND) TO MOD-TEMFSFEL                        
168600            END-IF                                                        
168700        ELSE                                                              
168800            MOVE FEL-2 (SPIND) TO MOD-TEMFSFEL                            
168900            MOVE MFS-ALFA-FAELT-FEL TO                                    
169000                    MOD-FLGAMART-ATTR                                     
169100        END-IF                                                            
169200     ELSE                                                                 
169300        IF MOD-TEMFSFEL = MFS-RENSA-FAELT                                 
169400           MOVE FEL-4(SPIND) TO MOD-TEMFSFEL                              
169500        END-IF                                                            
169600     END-IF                                                               
169700     .                                                                    
169800     EJECT                                                                
169900 CA-REG-UPPDAT-NYPON-KOP SECTION.                                         
170000     SKIP2                                                                
170100     MOVE IDARTNR-NY-WS TO W-IDARTNR                                      
170200     PERFORM IMS-GET-ARTG01                                               
170300                                                                          
170400     IF SEGMENT-FINNS                                                     
170500        MOVE MID-IDPROJ              TO ARTG01-ART-IDPROJ                 
170600        MOVE MID-IDAO                TO ARTG01-ART-IDAO                   
170700        IF MID-TEORSAK-1 = ALL '+'                                        
170800           CONTINUE                                                       
170900        ELSE                                                              
171000           MOVE MID-TEORSAK-1        TO ARTG01-ART-TEORSAK                
171100        END-IF                                                            
171200        IF MID-FLPISK = ALL '+'                                           
171300           CONTINUE                                                       
171400        ELSE                                                              
171500           MOVE WS-FLPISK            TO ARTG01-ART-FLPISK                 
171600        END-IF                                                            
171700        IF MID-FLBYTES = ALL '+'                                          
171800           CONTINUE                                                       
171900        ELSE                                                              
172000           MOVE WS-FLBYTES           TO ARTG01-ART-FLBYTES                
172100        END-IF                                                            
172200        IF MID-IDPROJK = ALL '+'                                          
172300           CONTINUE                                                       
172400        ELSE                                                              
172500           MOVE WS-IDPROJK           TO ARTG01-ART-IDPROJK                
172600        END-IF                                                            
172700        IF MID-KVPROG = ALL '+'                                           
172800           CONTINUE                                                       
172900        ELSE                                                              
173000           MOVE WS-KVPROG            TO ARTG01-ART-KVPROG                 
173100        END-IF                                                            
173200                                                                          
173300        IF MID-KVARTVAGN = ALL '+'                                        
173400           CONTINUE                                                       
173500        ELSE                                                              
173600           MOVE WS-KVARTVAGN         TO ARTG01-ART-KVARTVAGN              
173700        END-IF                                                            
173800                                                                          
173900        IF MID-IDARTNR-MOTSV = ALL '+'                                    
174000           CONTINUE                                                       
174100        ELSE                                                              
174200           MOVE WS-IDARTNR-MOTSV     TO ARTG01-ART-IDARTNR-MOTSV          
174300        END-IF                                                            
174400                                                                          
174500        IF MID-TISOP NOT = ALL '+'                                        
174600           PERFORM S25-TIFINLV-FRAN-SOP                                   
174700           MOVE SPAR-TIFINLV-AAVVD TO XX-TIFINLV                          
174800        END-IF                                                            
174900                                                                          
175000        IF XX-TIFINLV = ALL '+'                                           
175100           IF ART-TIFINLV = 99999                                         
175200              MOVE 99999999          TO ARTG01-ART-DAFINLEV               
175300           ELSE                                                           
175400              MOVE 'AAVVD ' TO DAT-KDDATFORM                              
175500              MOVE ART-TIFINLV TO DAT-I-TIDATUM                           
175600              PERFORM S99-WDATKONV                                        
175700              IF DAT-KDSVAR-OK                                            
175800                 MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV               
175900                 MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)         
176000              END-IF                                                      
176100           END-IF                                                         
176200        ELSE                                                              
176300           IF WS-TIFINLV = 99999                                          
176400              MOVE 99999999          TO ARTG01-ART-DAFINLEV               
176500           ELSE                                                           
176600              MOVE 'AAVVD ' TO DAT-KDDATFORM                              
176700              MOVE WS-TIFINLV TO DAT-I-TIDATUM                            
176800              PERFORM S99-WDATKONV                                        
176900              IF DAT-KDSVAR-OK                                            
177000                 MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV               
177100                 MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)         
177200              END-IF                                                      
177300           END-IF                                                         
177400        END-IF                                                            
177500                                                                          
177600        PERFORM IMS-REPL-ARTG                                             
177700                                                                          
177800        MOVE ARTG01-ART-TEORSAK       TO MOD-TEORSAK-1                    
177900        MOVE ARTG01-ART-FLPISK        TO MOD-FLPISK-UT                    
178000        MOVE ARTG01-ART-FLBYTES       TO MOD-FLBYTES-UT                   
178100        MOVE ARTG01-ART-IDPROJK       TO MOD-IDPROJK-UT                   
178200        MOVE ARTG01-ART-KVARTVAGN     TO MOD-KVARTVAGN-UT                 
178300        MOVE ARTG01-ART-IDARTNR-MOTSV TO MOD-IDARTNR-MOTSV-UT             
178400        MOVE WS-BEART                 TO MOD-BEART-UT                     
178500        MOVE ARTG01-ART-KVPROG        TO MOD-KVPROG-UT                    
178600                                                                          
178700     ELSE                                                                 
178800        MOVE IDARTNR-NY-WS           TO ARTG01-ART-IDARTNR                
178900        MOVE WS-BEART-SVE            TO ARTG01-ART-BEART-SVE              
179000        MOVE WS-FLBYTES              TO ARTG01-ART-FLBYTES                
179100        MOVE WS-FLPISK               TO ARTG01-ART-FLPISK                 
179200        MOVE WS-IDPROJK              TO ARTG01-ART-IDPROJK                
179300        MOVE WS-KVARTVAGN            TO ARTG01-ART-KVARTVAGN              
179400        MOVE WS-KVPROG               TO ARTG01-ART-KVPROG                 
179500        MOVE WS-IDARTNR-MOTSV        TO ARTG01-ART-IDARTNR-MOTSV          
179600                                                                          
179700       IF MID-TEORSAK-1 = ALL '+'                                         
179800           MOVE SPACE                TO ARTG01-ART-TEORSAK                
179900       ELSE                                                               
180000           MOVE MID-TEORSAK-1        TO ARTG01-ART-TEORSAK                
180100       END-IF                                                             
180200                                                                          
180300       MOVE ARTG01-ART-TEORSAK       TO MOD-TEORSAK-1                     
180400       MOVE ARTG01-ART-FLPISK        TO MOD-FLPISK-UT                     
180500       MOVE ARTG01-ART-FLBYTES       TO MOD-FLBYTES-UT                    
180600       MOVE ARTG01-ART-IDPROJK       TO MOD-IDPROJK-UT                    
180700       MOVE ARTG01-ART-KVARTVAGN     TO MOD-KVARTVAGN-UT                  
180800       MOVE ARTG01-ART-IDARTNR-MOTSV TO MOD-IDARTNR-MOTSV-UT              
180900       MOVE ARTG01-ART-KVPROG        TO MOD-KVPROG-UT                     
181000       MOVE WS-BEART                 TO MOD-BEART-UT                      
181100                                                                          
181200       PERFORM S19-KOP-FRAN-ARTREG-TILL-NYPON                             
181300       PERFORM S21-NOLLSTAELL-NYPON                                       
181400                                                                          
181500       PERFORM IMS-ISRT-ARTG01                                            
181600     END-IF                                                               
181700     .                                                                    
181800     EJECT                                                                
181900 D-KOP-FRAN-ARTREG-EV-NYPON SECTION.                                      
182000     SKIP2                                                                
182100******************************************************************        
182200*                                                                         
182300*  ARTIKEL MAN KOPIERAR IFRÅN FINNS PÅ NYPON-BASEN,                       
182400*  OCH DÄRMED SKER KOPIERING FRÅN ARTREG  O C H  EVENTUELLT FRÅN          
182500*  NYPON-BASEN.                                                           
182600******************************************************************        
182700                                                                          
182800     MOVE ARTG01-ART-IDPROJK  TO WS-IDPROJK-GAM                           
182900     PERFORM S10-KOLLA-INDATA-KOP-ARTREG                                  
183000     IF INPUT-RETT = JA                                                   
183100        IF WS-FLGAMART = NEJ                                              
183200           PERFORM S07-KOLLA-OM-BEN-FINNS                                 
183300           IF INPUT-RETT = JA                                             
183400              IF NYPON-ARTIKEL = JA                                       
183500                 PERFORM DA-REG-UPPDAT-NYPON-KOP                          
183600              END-IF                                                      
183700              MOVE JA TO TRANS-TO-1192                                    
183800              MOVE MED-1(SPIND)TO MOD-TEMFSINF                            
183900           ELSE                                                           
184000              MOVE FEL-4 (SPIND) TO MOD-TEMFSFEL                          
184100           END-IF                                                         
184200       ELSE                                                               
184300           MOVE FEL-2 (SPIND) TO MOD-TEMFSFEL                             
184400           MOVE MFS-ALFA-FAELT-FEL TO                                     
184500                   MOD-FLGAMART-ATTR                                      
184600       END-IF                                                             
184700     ELSE                                                                 
184800       IF MOD-TEMFSFEL = MFS-RENSA-FAELT                                  
184900           MOVE FEL-4(SPIND) TO MOD-TEMFSFEL                              
185000       END-IF                                                             
185100     END-IF                                                               
185200     .                                                                    
185300     EJECT                                                                
185400 DA-REG-UPPDAT-NYPON-KOP SECTION.                                         
185500     SKIP2                                                                
185600     MOVE IDARTNR-WS TO W-IDARTNR                                         
185700     PERFORM IMS-GET-ARTG01                                               
185800     MOVE ARTG01-ART-FLPISK             TO WS-SPAR-FLPISK                 
185900     MOVE ARTG01-ART-FLBYTES            TO WS-SPAR-FLBYTES                
186000     MOVE ARTG01-ART-IDPROJK            TO WS-SPAR-IDPROJK                
186100     MOVE ARTG01-ART-KVARTVAGN          TO WS-SPAR-KVARTVAGN              
186200     MOVE ARTG01-ART-KVPROG             TO WS-SPAR-KVPROG                 
186300     MOVE ARTG01-ART-TEORSAK            TO WS-SPAR-TEORSAK-1              
186400     MOVE ARTG01-ART-IDARTNR-MOTSV      TO WS-SPAR-IDARTNR-MOTSV          
186500                                                                          
186600     MOVE IDARTNR-NY-WS TO W-IDARTNR                                      
186700     PERFORM IMS-GET-ARTG01                                               
186800                                                                          
186900     IF SEGMENT-FINNS                                                     
187000        MOVE MID-IDPROJ                 TO ARTG01-ART-IDPROJ              
187100        MOVE MID-IDAO                   TO ARTG01-ART-IDAO                
187200        IF MID-FLPISK = ALL '+'                                           
187300           MOVE WS-SPAR-FLPISK          TO ARTG01-ART-FLPISK              
187400        ELSE                                                              
187500           MOVE WS-FLPISK               TO ARTG01-ART-FLPISK              
187600        END-IF                                                            
187700        IF MID-FLBYTES = ALL '+'                                          
187800           MOVE WS-SPAR-FLBYTES         TO ARTG01-ART-FLBYTES             
187900        ELSE                                                              
188000           MOVE WS-FLBYTES              TO ARTG01-ART-FLBYTES             
188100        END-IF                                                            
188200        IF MID-KVPROG = ALL '+'                                           
188300           MOVE WS-SPAR-KVPROG          TO ARTG01-ART-KVPROG              
188400        ELSE                                                              
188500           MOVE WS-KVPROG               TO ARTG01-ART-KVPROG              
188600        END-IF                                                            
188700                                                                          
188800        IF MID-IDPROJK = ALL '+'                                          
188900           MOVE WS-SPAR-IDPROJK         TO ARTG01-ART-IDPROJK             
189000        ELSE                                                              
189100           MOVE WS-IDPROJK              TO ARTG01-ART-IDPROJK             
189200        END-IF                                                            
189300                                                                          
189400        IF MID-KVARTVAGN = ALL '+'                                        
189500           MOVE WS-SPAR-KVARTVAGN       TO ARTG01-ART-KVARTVAGN           
189600        ELSE                                                              
189700           MOVE WS-KVARTVAGN            TO ARTG01-ART-KVARTVAGN           
189800        END-IF                                                            
189900                                                                          
190000        IF MID-IDARTNR-MOTSV = ALL '+'                                    
190100           MOVE WS-SPAR-IDARTNR-MOTSV                                     
190200                                     TO ARTG01-ART-IDARTNR-MOTSV          
190300        ELSE                                                              
190400           MOVE WS-IDARTNR-MOTSV                                          
190500                                     TO ARTG01-ART-IDARTNR-MOTSV          
190600        END-IF                                                            
190700                                                                          
190800        IF MID-TEORSAK-1 = ALL '+'                                        
190900           MOVE WS-SPAR-TEORSAK-1       TO ARTG01-ART-TEORSAK             
191000        ELSE                                                              
191100           MOVE MID-TEORSAK-1           TO ARTG01-ART-TEORSAK             
191200        END-IF                                                            
191300                                                                          
191400        IF MID-TISOP NOT = ALL '+'                                        
191500           PERFORM S25-TIFINLV-FRAN-SOP                                   
191600           MOVE SPAR-TIFINLV-AAVVD TO XX-TIFINLV                          
191700        END-IF                                                            
191800        IF XX-TIFINLV = ALL '+'                                           
191900           IF ART-TIFINLV = 99999                                         
192000              MOVE 99999999          TO ARTG01-ART-DAFINLEV               
192100           ELSE                                                           
192200              MOVE 'AAVVD ' TO DAT-KDDATFORM                              
192300              MOVE ART-TIFINLV TO DAT-I-TIDATUM                           
192400              PERFORM S99-WDATKONV                                        
192500              IF DAT-KDSVAR-OK                                            
192600                 MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV               
192700                 MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)         
192800              END-IF                                                      
192900           END-IF                                                         
193000        ELSE                                                              
193100           IF WS-TIFINLV = 99999                                          
193200              MOVE 99999999          TO ARTG01-ART-DAFINLEV               
193300           ELSE                                                           
193400              MOVE 'AAVVD ' TO DAT-KDDATFORM                              
193500              MOVE WS-TIFINLV TO DAT-I-TIDATUM                            
193600              PERFORM S99-WDATKONV                                        
193700              IF DAT-KDSVAR-OK                                            
193800                 MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV               
193900                 MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)         
194000              END-IF                                                      
194100           END-IF                                                         
194200        END-IF                                                            
194300                                                                          
194400        PERFORM IMS-REPL-ARTG                                             
194500                                                                          
194600        MOVE ARTG01-ART-TEORSAK         TO MOD-TEORSAK-1                  
194700        MOVE ARTG01-ART-FLPISK          TO MOD-FLPISK-UT                  
194800        MOVE ARTG01-ART-FLBYTES         TO MOD-FLBYTES-UT                 
194900        MOVE ARTG01-ART-IDPROJK         TO MOD-IDPROJK-UT                 
195000        MOVE ARTG01-ART-KVARTVAGN       TO MOD-KVARTVAGN-UT               
195100        MOVE ARTG01-ART-IDARTNR-MOTSV   TO MOD-IDARTNR-MOTSV-UT           
195200        MOVE ARTG01-ART-KVPROG          TO MOD-KVPROG-UT                  
195300        MOVE WS-BEART                   TO MOD-BEART-UT                   
195400     ELSE                                                                 
195500                                                                          
195600        MOVE WS-BEART-SVE               TO ARTG01-ART-BEART-SVE           
195700        MOVE IDARTNR-NY-WS              TO ARTG01-ART-IDARTNR             
195800        IF MID-FLPISK = ALL '+'                                           
195900           MOVE WS-SPAR-FLPISK          TO ARTG01-ART-FLPISK              
196000        ELSE                                                              
196100           MOVE WS-FLPISK               TO ARTG01-ART-FLPISK              
196200        END-IF                                                            
196300        IF MID-FLBYTES = ALL '+'                                          
196400           MOVE WS-SPAR-FLBYTES         TO ARTG01-ART-FLBYTES             
196500        ELSE                                                              
196600           MOVE WS-FLBYTES              TO ARTG01-ART-FLBYTES             
196700        END-IF                                                            
196800        IF MID-IDPROJK = ALL '+'                                          
196900           MOVE WS-SPAR-IDPROJK         TO ARTG01-ART-IDPROJK             
197000        ELSE                                                              
197100           MOVE WS-IDPROJK              TO ARTG01-ART-IDPROJK             
197200        END-IF                                                            
197300        IF MID-KVPROG = ALL '+'                                           
197400           MOVE WS-SPAR-KVPROG          TO ARTG01-ART-KVPROG              
197500        ELSE                                                              
197600           MOVE WS-KVPROG               TO ARTG01-ART-KVPROG              
197700        END-IF                                                            
197800                                                                          
197900        IF MID-KVARTVAGN = ALL '+'                                        
198000           MOVE WS-SPAR-KVARTVAGN       TO ARTG01-ART-KVARTVAGN           
198100        ELSE                                                              
198200           MOVE WS-KVARTVAGN            TO ARTG01-ART-KVARTVAGN           
198300        END-IF                                                            
198400                                                                          
198500        IF MID-IDARTNR-MOTSV = ALL '+'                                    
198600           MOVE WS-SPAR-IDARTNR-MOTSV                                     
198700                                     TO ARTG01-ART-IDARTNR-MOTSV          
198800        ELSE                                                              
198900           MOVE WS-IDARTNR-MOTSV                                          
199000                                     TO ARTG01-ART-IDARTNR-MOTSV          
199100        END-IF                                                            
199200                                                                          
199300        IF MID-TEORSAK-1 = ALL '+'                                        
199400           MOVE WS-SPAR-TEORSAK-1       TO ARTG01-ART-TEORSAK             
199500        ELSE                                                              
199600           MOVE MID-TEORSAK-1           TO ARTG01-ART-TEORSAK             
199700        END-IF                                                            
199800                                                                          
199900        MOVE ARTG01-ART-TEORSAK         TO MOD-TEORSAK-1                  
200000        MOVE ARTG01-ART-FLPISK          TO MOD-FLPISK-UT                  
200100        MOVE ARTG01-ART-FLBYTES         TO MOD-FLBYTES-UT                 
200200        MOVE ARTG01-ART-IDPROJK         TO MOD-IDPROJK-UT                 
200300        MOVE ARTG01-ART-KVARTVAGN       TO MOD-KVARTVAGN-UT               
200400        MOVE ARTG01-ART-IDARTNR-MOTSV   TO MOD-IDARTNR-MOTSV-UT           
200500        MOVE WS-BEART                   TO MOD-BEART-UT                   
200600        MOVE ARTG01-ART-KVPROG          TO MOD-KVPROG-UT                  
200700                                                                          
200800        PERFORM S19-KOP-FRAN-ARTREG-TILL-NYPON                            
200900        PERFORM S21-NOLLSTAELL-NYPON                                      
201000        MOVE IDARTNR-NY-WS TO W-IDARTNR                                   
201100        PERFORM IMS-ISRT-ARTG01                                           
201200                                                                          
201300     END-IF                                                               
201400     .                                                                    
201500     EJECT                                                                
201600 E-VAECKNING-AV-GAMMAL-ARTIKEL SECTION.                                   
201700     SKIP2                                                                
201800     PERFORM S08-KOLLA-INDATA-NYREG                                       
201900     IF INPUT-RETT = JA                                                   
202000        IF WS-FLGAMART = JA                                               
202100           IF NYPON-ARTIKEL = JA                                          
202200              PERFORM S12-REGISTRERA-NYPON                                
202300           END-IF                                                         
202400           PERFORM EA-RIVNING-AV-EV-ERSATTNING                            
202500                                                                          
202600           MOVE JA          TO TRANS-TO-1192                              
202700           MOVE MED-1(SPIND)TO MOD-TEMFSINF                               
202800        ELSE                                                              
202900           MOVE FEL-3 (SPIND)      TO MOD-TEMFSFEL                        
203000           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLGAMART-ATTR                   
203100           MOVE MFS-STAENG-FAELT   TO MOD-IDARTNR-ATTR                    
203200                                      MOD-BEART-ATTR                      
203300                                      MOD-IDSKYLT-ATTR                    
203400                                      MOD-FLRSBEART-ATTR                  
203500        END-IF                                                            
203600     ELSE                                                                 
203700        IF MOD-TEMFSFEL = MFS-RENSA-FAELT                                 
203800           MOVE FEL-4(SPIND) TO MOD-TEMFSFEL                              
203900        END-IF                                                            
204000        MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-ATTR                         
204100                                 MOD-BEART-ATTR                           
204200                                 MOD-IDSKYLT-ATTR                         
204300                                 MOD-FLRSBEART-ATTR                       
204400     END-IF                                                               
204500     .                                                                    
204600     EJECT                                                                
204700 EA-RIVNING-AV-EV-ERSATTNING SECTION.                                     
204800     SKIP2                                                                
204900     MOVE IDARTNR-WS TO W-IDARTNR                                         
205000     PERFORM IMS-GET-ERSA01                                               
205100     IF SEGMENT-FINNS                                                     
205200        PERFORM IMS-GET-ERSA11                                            
205300        PERFORM UNTIL SEGMENT-SAKNAS                                      
205400           IF SEGMENT-FINNS                                               
205500              MOVE ERSA11-IDARTNR-TILLK TO WS-IDARTNR-TILLK               
205600                                            W-IDARTNR-TILLK               
205700              PERFORM IMS-DLET-ERSA                                       
205800              PERFORM IMS-GET-ERSB01                                      
205900              IF SEGMENT-SAKNAS                                           
206000                 MOVE WS-IDARTNR-TILLK  TO W-IDARTNR                      
206100                 PERFORM IMS-GET-ARTC01                                   
206200                 IF SEGMENT-FINNS                                         
206300                    MOVE 'N' TO ART-FLERS                                 
206400                    PERFORM IMS-REPL-ARTC                                 
206500                 END-IF                                                   
206600              END-IF                                                      
206700           END-IF                                                         
206800           PERFORM IMS-GET-ERSA11                                         
206900        END-PERFORM                                                       
207000                                                                          
207100        MOVE IDARTNR-WS TO W-IDARTNR                                      
207200        PERFORM IMS-GET-ERSA01                                            
207300        IF SEGMENT-FINNS                                                  
207400           PERFORM IMS-DLET-ERSA                                          
207500        END-IF                                                            
207600     END-IF                                                               
207700     .                                                                    
207800     EJECT                                                                
207900 F-REG-ARTIKEL-ARTREG-EV-NYPON SECTION.                                   
208000     SKIP2                                                                
208100     IF IDARTNR-NY-WS = IDARTNR-WS                                        
208200        IF IDARTNR-NY-WS > 0 AND IDARTNR-WS > 0                           
208300           IF IDARTNR-NY-WS > 99999999                                    
208400              MOVE FEL-8 (SPIND) TO MOD-TEMFSFEL                          
208500              MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDARTNR-ATTR               
208600              MOVE FEL-ERR-IDARTNR    TO WS-ERROR-UPDX                    
208700           ELSE                                                           
208800              PERFORM S08-KOLLA-INDATA-NYREG                              
208900              IF INPUT-RETT = JA                                          
209000                 IF WS-FLGAMART = NEJ                                     
209100                    PERFORM S07-KOLLA-OM-BEN-FINNS                        
209200                    IF INPUT-RETT = JA                                    
209300                       IF NYPON-ARTIKEL = JA                              
209400                          PERFORM S12-REGISTRERA-NYPON                    
209500                       END-IF                                             
209600                       MOVE MED-1(SPIND)TO MOD-TEMFSINF                   
209700                       MOVE JA TO TRANS-TO-1192                           
209800                    ELSE                                                  
209900                       MOVE FEL-4 (SPIND) TO MOD-TEMFSFEL                 
210000                       MOVE FEL-ERR-FIELD   TO WS-ERROR-UPDX              
210100                    END-IF                                                
210200                 ELSE                                                     
210300                    MOVE FEL-2 (SPIND) TO MOD-TEMFSFEL                    
210400                    MOVE MFS-ALFA-FAELT-FEL TO                            
210500                                        MOD-FLGAMART-ATTR                 
210600                    MOVE FEL-ERR-FLAGMART                                 
210700                                        TO MOD-IDARTNR-ATTR               
210800                 END-IF                                                   
210900              ELSE                                                        
211000                 IF MOD-TEMFSFEL = MFS-RENSA-FAELT                        
211100                    MOVE FEL-4(SPIND) TO MOD-TEMFSFEL                     
211200                    MOVE FEL-ERR-FIELD                                    
211300                                        TO MOD-IDARTNR-ATTR               
211400                 END-IF                                                   
211500              END-IF                                                      
211600           END-IF                                                         
211700        ELSE                                                              
211800           MOVE FEL-8(SPIND)TO MOD-TEMFSFEL                               
211900           MOVE FEL-ERR-IDARTNR                                           
212000                            TO MOD-IDARTNR-ATTR                           
212100        END-IF                                                            
212200     END-IF                                                               
212300     .                                                                    
212400     EJECT                                                                
212500 H-LAS-NYPON SECTION.                                                     
212600     SKIP2                                                                
212700     MOVE ZERO TO WS-NY-IDARTNR                                           
212800     PERFORM IMS-GET-ARTG01                                               
212900     IF SEGMENT-FINNS                                                     
213000        PERFORM S22-FIXA-DOLDA-FAELT                                      
213100        MOVE ARTG01-ART-IDBERED TO WS-IDBERED                             
213200        MOVE WS-IDBERED             TO MOD-IDBERED-UT                     
213300        MOVE ARTG01-ART-KDPRODSL    TO WS-KDPRODSL                        
213400        MOVE WS-KDPRODSL            TO MOD-KDPRODSL-UT                    
213500        MOVE ARTG01-ART-KDSORT      TO MOD-KDSORT-UT                      
213600        MOVE ARTG01-ART-IDPROJ      TO MOD-IDPROJ-UT                      
213700                                       WS-IDPROJ                          
213800        MOVE ARTG01-ART-IDPROENH    TO MOD-IDPROENH-1-UT                  
213900        INSPECT MOD-IDPROENH-1-UT REPLACING LEADING                       
214000                                  ZERO BY SPACE                           
214100        MOVE ARTG01-ART-IDPROJK     TO MOD-IDPROJK-UT                     
214200        MOVE ARTG01-ART-KVARTVAGN   TO MOD-KVARTVAGN-UT                   
214300        MOVE ARTG01-ART-FLPISK      TO MOD-FLPISK-UT                      
214400        MOVE ARTG01-ART-KVPROG      TO MOD-KVPROG-UT                      
214500        MOVE ARTG01-ART-IDAO        TO MOD-IDAO-UT                        
214600        MOVE ARTG01-ART-IDFKNGRP    TO WS-IDFKNGRP                        
214700        MOVE WS-IDFKNGRP            TO MOD-IDFKNGRP-UT                    
214800        MOVE ARTG01-ART-IDRITN      TO MOD-IDRITN-UT                      
214900        MOVE ARTG01-ART-FLBYTES     TO MOD-FLBYTES-UT                     
215000        MOVE ARTG01-ART-TEORSAK     TO MOD-TEORSAK-1                      
215100        MOVE ARTG01-ART-TEARTNOT    TO MOD-TEARTNOT-2                     
215200        MOVE ARTG01-ART-BEART-SVE   TO MOD-BEART-UT                       
215300        MOVE ARTG01-ART-IDARTNR-MOTSV                                     
215400                                    TO MOD-IDARTNR-MOTSV-UT               
215500        MOVE ARTG01-ART-KVPROG      TO MOD-KVPROG-UT                      
215600        IF FINNS-PA-ARTC = JA                                             
215700           CONTINUE                                                       
215800        ELSE                                                              
215900           PERFORM S97-LAES-XXAQ                                          
216000           IF SEGMENT-FINNS                                               
216100              IF  XXAQ-1132-TIPRODSTA = +111111                           
216200              AND XXAQ-1132-TIFINLEV  = ZERO                              
216300                  PERFORM HA-PLOCKA-NYASTE-TISERLEV                       
216400                  IF W-SPAR-TISERLEV = 9999999                            
216500                     MOVE ARTG01-ART-KDPRODSL  TO TEST-KDPRODSL           
216600                     IF KDPRODSL-SPARE-PARTS OR                           
216700                        KDPRODSL-WHEELS      OR                           
216800                        KDPRODSL-SERVICES                                 
216900                        IF ARTG01-ART-FLUNIKRD = JA                       
217000                        OR ARTG01-ART-IDLEVNR = '9998 '                   
217100                           CONTINUE                                       
217200                        ELSE                                              
217300                           MOVE 99999          TO MOD-TIFINLV-UT          
217400                        END-IF                                            
217500                     END-IF                                               
217600                  ELSE                                                    
217700                     MOVE 'AAMMDD'             TO DAT-KDDATFORM           
217800                     MOVE W-SPAR-TISERLEV      TO DAT-I-TIDATUM           
217900                     PERFORM S99-WDATKONV                                 
218000                     IF DAT-KDSVAR-OK                                     
218100                        MOVE DAT-TIAAVVD       TO MOD-TIFINLV-UT          
218200                     END-IF                                               
218300                  END-IF                                                  
218400              ELSE                                                        
218500                 MOVE XXAQ-1132-TIFINLEV       TO DAT-I-TIDATUM           
218600                 MOVE 'AAMMDD'                 TO DAT-KDDATFORM           
218700                 PERFORM S99-WDATKONV                                     
218800                 IF DAT-KDSVAR-OK                                         
218900                    MOVE DAT-TIAAVVD           TO MOD-TIFINLV-UT          
219000                 END-IF                                                   
219100              END-IF                                                      
219200           ELSE                                                           
219300              PERFORM HA-PLOCKA-NYASTE-TISERLEV                           
219400              IF W-SPAR-TISERLEV = 9999999                                
219500                 CONTINUE                                                 
219600              ELSE                                                        
219700                  MOVE 'AAMMDD'              TO DAT-KDDATFORM             
219800                  MOVE W-SPAR-TISERLEV       TO DAT-I-TIDATUM             
219900                  PERFORM S99-WDATKONV                                    
220000                  IF DAT-KDSVAR-OK                                        
220100                     MOVE DAT-TIAAVVD        TO MOD-TIFINLV-UT            
220200                  END-IF                                                  
220300              END-IF                                                      
220400           END-IF                                                         
220500        END-IF                                                            
220600     ELSE                                                                 
220700        MOVE MFS-RENSA-FAELT   TO MOD-IDBERED-UT                          
220800                                  MOD-KDPRODSL-UT                         
220900                                  MOD-KDSORT-UT                           
221000                                  MOD-IDPROENH-1-UT                       
221100                                  MOD-IDPROENH-2-UT                       
221200                                  MOD-IDPROENH-3-UT                       
221300                                  MOD-IDPROJ-UT                           
221400                                  MOD-IDPROJK-UT                          
221500                                  MOD-KVARTVAGN-UT                        
221600                                  MOD-FLPISK-UT                           
221700                                  MOD-KVPROG-UT                           
221800                                  MOD-IDAO-UT                             
221900                                  MOD-TIFINLV-UT                          
222000                                  MOD-TISOP-UT                            
222100                                  MOD-IDFKNGRP-UT                         
222200                                  MOD-IDRITN-UT                           
222300                                  MOD-FLBYTES-UT                          
222400                                  MOD-TEORSAK-1                           
222500                                  MOD-TEARTNOT-2                          
222600                                  MOD-BEART-UT                            
222700                                  MOD-IDARTNR-MOTSV-UT                    
222800     END-IF                                                               
222900                                                                          
223000     MOVE MFS-RENSA-FAELT      TO MOD-KDYTBEH-UT                          
223100                                  MOD-KDFARLIG-UT                         
223200                                  MOD-KDBPSR-UT                           
223300                                  MOD-IDKAT-1-UT                          
223400                                  MOD-IDKAT-2-UT                          
223500                                  MOD-IDKAT-3-UT                          
223600                                  MOD-KDUART-UT                           
223700                                  MOD-FLLSRDEL-UT                         
223800                                  MOD-IDPROJUP-UT                         
223900                                  MOD-TEARTNOT-4                          
224000                                  MOD-TEARTNOT-7                          
224100                                  MOD-IDPROENH-2-UT                       
224200                                  MOD-IDPROENH-3-UT                       
224300                                                                          
224400                                  MOD-BELEV-UT                            
224500                                  MOD-IDLEVNR-UT                          
224600     .                                                                    
224700     EJECT                                                                
224800 HA-PLOCKA-NYASTE-TISERLEV      SECTION.                                  
224900     SKIP2                                                                
225000     MOVE +9999999   TO W-SPAR-TISERLEV                                   
225100     MOVE +1         TO SLEV-IX                                           
225200     PERFORM UNTIL SLEV-IX > 5                                            
225300        IF ARTG01-ART-TISERLEV(SLEV-IX) = ZERO                            
225400           CONTINUE                                                       
225500        ELSE                                                              
225600           MOVE ARTG01-ART-TISERLEV(SLEV-IX)   TO TMP1-YYMMDD             
225700           MOVE W-SPAR-TISERLEV                TO TMP2-YYMMDD             
225800           PERFORM WY2000P1                                               
225900           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
226000              MOVE ARTG01-ART-TISERLEV(SLEV-IX)      TO                   
226100                                                 W-SPAR-TISERLEV          
226200           END-IF                                                         
226300        END-IF                                                            
226400        ADD +1 TO  SLEV-IX                                                
226500     END-PERFORM                                                          
226600     .                                                                    
226700     EJECT                                                                
226800 K-LAS-ARTREG-NYPON-BENREG SECTION.                                       
226900     SKIP2                                                                
227000     MOVE ZERO TO WS-NY-IDARTNR                                           
227100     IF ART-KDERS-UTG > 0                                                 
227200        MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-ATTR                         
227300        MOVE MFS-STAENG-FAELT TO MOD-BEART-ATTR                           
227400                                 MOD-FLRSBEART-ATTR                       
227500                                 MOD-IDSKYLT-ATTR                         
227600     END-IF                                                               
227700                                                                          
227800     MOVE ART-TIFINLV     TO WS-TIFINLV                                   
227900     MOVE WS-TIFINLV      TO MOD-TIFINLV-UT                               
228000     MOVE ART-TISOP       TO WS-TISOP                                     
228100     MOVE WS-TISOP        TO MOD-TISOP-UT                                 
228200     MOVE ART-IDFKNGRP TO WS-IDFKNGRP                                     
228300     MOVE WS-IDFKNGRP     TO MOD-IDFKNGRP-UT                              
228400     MOVE ART-KDPRODSL TO WS-KDPRODSL                                     
228500     MOVE WS-KDPRODSL     TO MOD-KDPRODSL-UT                              
228600     MOVE ART-KDSORT      TO MOD-KDSORT-UT                                
228700                                                                          
228800     IF ART-IDAO(1) = SPACE                                               
228900        MOVE MFS-RENSA-FAELT TO MOD-IDAO-UT                               
229000     ELSE                                                                 
229100        MOVE ART-IDAO(1)     TO MOD-IDAO-UT                               
229200     END-IF                                                               
229300                                                                          
229400     PERFORM IMS-GET-ARTC11                                               
229500     IF SEGMENT-FINNS                                                     
229600        MOVE CLAG-IDBERED    TO WS-IDBERED                                
229700        MOVE WS-IDBERED      TO MOD-IDBERED-UT                            
229800        MOVE CLAG-IDPROJ     TO MOD-IDPROJ-UT                             
229900        MOVE CLAG-IDPROJUP   TO MOD-IDPROJUP-UT                           
230000        MOVE CLAG-IDRITN     TO MOD-IDRITN-UT                             
230100        MOVE CLAG-KDYTBEH    TO WS-KDYTBEH                                
230200        MOVE WS-KDYTBEH      TO MOD-KDYTBEH-UT                            
230300        MOVE CLAG-IDKAT(1)   TO MOD-IDKAT-1-UT                            
230400        MOVE CLAG-IDKAT(2)   TO MOD-IDKAT-2-UT                            
230500        MOVE CLAG-IDKAT(3)   TO MOD-IDKAT-3-UT                            
230600        MOVE CLAG-IDPROENH(1) TO MOD-IDPROENH-1-UT                        
230700        INSPECT MOD-IDPROENH-1-UT REPLACING LEADING                       
230800                                  ZERO BY SPACE                           
230900        MOVE CLAG-IDPROENH(2) TO MOD-IDPROENH-2-UT                        
231000        INSPECT MOD-IDPROENH-2-UT REPLACING LEADING                       
231100                                  ZERO BY SPACE                           
231200        MOVE CLAG-IDPROENH(3) TO MOD-IDPROENH-3-UT                        
231300        INSPECT MOD-IDPROENH-3-UT REPLACING LEADING                       
231400                                  ZERO BY SPACE                           
231500        MOVE CLAG-FLLSRDEL    TO MOD-FLLSRDEL-UT                          
231600        MOVE CLAG-KDUART      TO MOD-KDUART-UT                            
231700        MOVE CLAG-KDFARLIG    TO WS-KDFARLIG                              
231800        MOVE WS-KDFARLIG      TO MOD-KDFARLIG-UT                          
231900        MOVE CLAG-KDBPSR      TO WS-KDBPSR                                
232000        MOVE WS-KDBPSR        TO MOD-KDBPSR-UT                            
232100     ELSE                                                                 
232200        MOVE MFS-RENSA-FAELT TO MOD-IDBERED-UT                            
232300                                MOD-IDPROJ-UT                             
232400                                MOD-IDPROJUP-UT                           
232500                                MOD-IDRITN-UT                             
232600                                MOD-KDYTBEH-UT                            
232700                                MOD-IDKAT-1-UT                            
232800                                MOD-IDKAT-2-UT                            
232900                                MOD-IDKAT-3-UT                            
233000                                MOD-IDPROENH-1-UT                         
233100                                MOD-IDPROENH-2-UT                         
233200                                MOD-IDPROENH-3-UT                         
233300                                MOD-FLLSRDEL-UT                           
233400                                MOD-KDUART-UT                             
233500                                MOD-KDFARLIG-UT                           
233600                                MOD-KDBPSR-UT                             
233700     END-IF                                                               
233800                                                                          
233900                                                                          
234000     MOVE 1 TO W-KDNOTTYP                                                 
234100     PERFORM IMS-GNP-ARTC25                                               
234200     IF SEGMENT-FINNS                                                     
234300        MOVE NOT-TEARTNOT TO MOD-TEARTNOT-4                               
234400     ELSE                                                                 
234500        MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-4                            
234600     END-IF                                                               
234700                                                                          
234800     MOVE 3 TO W-KDNOTTYP                                                 
234900     PERFORM IMS-GNP-ARTC25                                               
235000     IF SEGMENT-FINNS                                                     
235100        MOVE NOT-TEARTNOT TO MOD-TEARTNOT-2                               
235200     ELSE                                                                 
235300        MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-2                            
235400     END-IF                                                               
235500                                                                          
235600     MOVE 7 TO W-KDNOTTYP                                                 
235700     PERFORM IMS-GNP-ARTC25                                               
235800     IF SEGMENT-FINNS                                                     
235900        MOVE NOT-TEARTNOT TO MOD-TEARTNOT-7                               
236000     ELSE                                                                 
236100        MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-7                            
236200     END-IF                                                               
236300                                                                          
236400     IF SWEDISH-TEXT                                                      
236500        MOVE 'S  ' TO W-IDSKYLT                                           
236600     ELSE                                                                 
236700        MOVE 'GB ' TO W-IDSKYLT                                           
236800     END-IF                                                               
236900                                                                          
237000     PERFORM IMS-GET-BENA11-CSEQ                                          
237100     MOVE BENA-TEXT-BEART    TO MOD-BEART-UT                              
237200                                                                          
237300     PERFORM IMS-GET-ARTG01                                               
237400     IF SEGMENT-FINNS                                                     
237500        PERFORM S22-FIXA-DOLDA-FAELT                                      
237600        PERFORM KA-KOLLA-RESBED-ARTUTG                                    
237700        MOVE ARTG01-ART-IDPROJK  TO MOD-IDPROJK-UT                        
237800        MOVE ARTG01-ART-KVARTVAGN TO MOD-KVARTVAGN-UT                     
237900        MOVE ARTG01-ART-FLPISK   TO MOD-FLPISK-UT                         
238000        MOVE ARTG01-ART-KVPROG   TO MOD-KVPROG-UT                         
238100        MOVE ARTG01-ART-FLBYTES  TO MOD-FLBYTES-UT                        
238200        MOVE ARTG01-ART-TEORSAK  TO MOD-TEORSAK-1                         
238300        MOVE ARTG01-ART-IDARTNR-MOTSV                                     
238400                                 TO MOD-IDARTNR-MOTSV-UT                  
238500        MOVE ARTG01-ART-KVPROG   TO MOD-KVPROG-UT                         
238600*       CALL FELLOG                                                       
238700     ELSE                                                                 
238800        MOVE MFS-RENSA-FAELT TO MOD-IDPROJK-UT                            
238900                                MOD-KVARTVAGN-UT                          
239000                                MOD-FLPISK-UT                             
239100                                MOD-KVPROG-UT                             
239200                                MOD-FLBYTES-UT                            
239300                                MOD-TEORSAK-1                             
239400                                MOD-IDARTNR-MOTSV-UT                      
239500     END-IF                                                               
239600     MOVE MFS-RENSA-FAELT    TO MOD-BELEV-UT                              
239700                                MOD-IDLEVNR-UT                            
239800     .                                                                    
239900     EJECT                                                                
240000 KA-KOLLA-RESBED-ARTUTG     SECTION.                                      
240100     IF ARTG01-ART-KDRESBED = '-'                                         
240200        MOVE MED-2(SPIND)         TO                                      
240300                              MOD-TEMFSFEL                                
240400     END-IF                                                               
240500     .                                                                    
240600     EJECT                                                                
240700 L-RENSA-INFAELT SECTION.                                                 
240800     SKIP2                                                                
240900     MOVE MFS-RENSA-FAELT TO MOD-IDBERED-IN                               
241000                             MOD-KDPRODSL-IN                              
241100                             MOD-KDSORT-IN                                
241200                             MOD-IDPROENH-1-IN                            
241300                             MOD-IDPROENH-2-IN                            
241400                             MOD-IDPROENH-3-IN                            
241500                             MOD-KDYTBEH-IN                               
241600                             MOD-IDPROJ-IN                                
241700                             MOD-KDFARLIG-IN                              
241800                             MOD-KDBPSR-IN                                
241900                             MOD-IDKAT-1-IN                               
242000                             MOD-IDKAT-2-IN                               
242100                             MOD-IDKAT-3-IN                               
242200                             MOD-KDUART-IN                                
242300                             MOD-IDPROJK-IN                               
242400                             MOD-KVARTVAGN-IN                             
242500                             MOD-IDARTNR-MOTSV-IN                         
242600                             MOD-FLPISK-IN                                
242700                             MOD-IDAO-IN                                  
242800                             MOD-TISOP-IN                                 
242900                             MOD-IDSKYLT-IN                               
243000                             MOD-FLLSRDEL-IN                              
243100                             MOD-IDPROJUP-IN                              
243200                             MOD-BEART-IN                                 
243300                             MOD-FLRSBEART-IN                             
243400                             MOD-IDFKNGRP-IN                              
243500                             MOD-IDLEVNR-IN                               
243600                             MOD-BELEV-IN                                 
243700                             MOD-IDRITN-IN                                
243800                             MOD-FLBYTES-IN                               
243900                             MOD-FLGAMART-IN                              
244000                             MOD-KVPROG-IN                                
244100     .                                                                    
244200     EJECT                                                                
244300 M-LAS-BENREG SECTION.                                                    
244400     SKIP2                                                                
244500                                                                          
244600     IF SWEDISH-TEXT                                                      
244700        MOVE 'S  ' TO W-IDSKYLT                                           
244800     ELSE                                                                 
244900        MOVE 'GB ' TO W-IDSKYLT                                           
245000     END-IF                                                               
245100                                                                          
245200     PERFORM IMS-GET-BENA11-CSEQ                                          
245300                                                                          
245400     MOVE BENA-TEXT-BEART TO MOD-BEART-UT                                 
245500     .                                                                    
245600     EJECT                                                                
245700 O-KOLLA-RESBED-ARTUTG     SECTION.                                       
245800     IF ARTG01-ART-KDRESBED = '-'                                         
245900        MOVE MED-2(SPIND)         TO                                      
246000                              MOD-TEMFSFEL                                
246100     END-IF                                                               
246200     .                                                                    
246300     EJECT                                                                
246400 P-KOLLA-RASA SECTION.                                                    
246500     SKIP2                                                                
246600     MOVE NEJ TO FINNS-RASA                                               
246700     MOVE IDARTNR-WS TO W-IDARTNR                                         
246800     PERFORM IMS-GET-SATB01                                               
246900     IF SEGMENT-FINNS                                                     
247000        MOVE JA TO FINNS-RASA                                             
247100     ELSE                                                                 
247200        MOVE SPACE TO W-IDLEVNR-S                                         
247300        MOVE SPACE TO W-BELEVART-S                                        
247400        MOVE IDARTNR-WS TO W-IDARTNR-S                                    
247500        PERFORM IMS-GET-SATB11-CSEQ                                       
247600        IF SEGMENT-FINNS                                                  
247700           MOVE JA TO FINNS-RASA                                          
247800        END-IF                                                            
247900     END-IF                                                               
248000     .                                                                    
248100     EJECT                                                                
248200 S01-NYA-GEMENSAMMA-DATAELEMENT SECTION.                                  
248300     SKIP2                                                                
248400     MOVE ZERO TO WS-IDPROENH(1)                                          
248500                  WS-IDPROENH(2)                                          
248600                  WS-IDPROENH(3)                                          
248700                                                                          
248800     IF MID-KDPRODSL = C-KDPRODSL-19                                      
248900        IF MID-KDSORT = ALL '+'                                           
249000           MOVE C-PS19-DEFAULT-KDSORT    TO MID-KDSORT                    
249100                                            MOD-KDSORT-IN                 
249200        END-IF                                                            
249300        IF MID-KDYTBEH = ALL '+'                                          
249400           MOVE C-PS19-DEFAULT-KDYTBEH   TO MID-KDYTBEH                   
249500                                            MOD-KDYTBEH-IN                
249600        END-IF                                                            
249700        IF MID-IDPROJ = ALL '+'                                           
249800           MOVE C-PS19-DEFAULT-IDPROJ    TO MID-IDPROJ                    
249900                                            MOD-IDPROJ-IN                 
250000                                             WS-IDPROJ                    
250100        END-IF                                                            
250200        IF MID-KDFARLIG = ALL '+'                                         
250300           MOVE C-PS19-DEFAULT-KDFARLIG  TO MID-KDFARLIG                  
250400                                            MOD-KDFARLIG-IN               
250500        END-IF                                                            
250600        IF MID-KDBPSR  = ALL '+'                                          
250700           MOVE C-PS19-DEFAULT-KDBPSR    TO MID-KDBPSR                    
250800                                            MOD-KDBPSR-IN                 
250900        END-IF                                                            
251000        IF MID-IDPROJK = ALL '+'                                          
251100           MOVE C-PS19-DEFAULT-IDPROJK   TO MID-IDPROJK                   
251200                                            MOD-IDPROJK-IN                
251300                                             WS-IDPROJK                   
251400        END-IF                                                            
251500        IF MID-FLLSRDEL = ALL '+'                                         
251600           MOVE C-PS19-DEFAULT-FLLSRDEL  TO MID-FLLSRDEL                  
251700                                            MOD-FLLSRDEL-IN               
251800        END-IF                                                            
251900     END-IF                                                               
252000                                                                          
252100     IF MID-IDLEVNR = ALL '+'                                             
252200        IF MID-BELEV = ALL '+'                                            
252300           MOVE MFS-RENSA-FAELT TO MOD-BELEV-IN                           
252400                                   MOD-IDLEVNR-IN                         
252500        ELSE                                                              
252600           MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR                      
252700                                     MOD-IDLEVNR-ATTR                     
252800           MOVE NEJ TO INPUT-RETT                                         
252900           MOVE FEL-ERR-BELEV      TO WS-ERROR-UPDX                       
253000        END-IF                                                            
253100     ELSE                                                                 
253200        IF MID-IDLEVNR(1:1) NOT = ' ' AND '0' AND '+'                     
253300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-ATTR                  
253400        ELSE                                                              
253500           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR                    
253600           MOVE NEJ TO INPUT-RETT                                         
253700           MOVE FEL-ERR-BELEV      TO WS-ERROR-UPDX                       
253800        END-IF                                                            
253900                                                                          
254000        IF MID-BELEV = ALL '+' OR SPACE                                   
254100           IF MID-IDLEVNR(1:1) NOT = ' ' AND '0' AND '+'                  
254200              IF MID-IDLEVNR = '1002 '                                    
254300                 MOVE MFS-RENSA-FAELT TO MOD-BELEV-IN                     
254400              ELSE                                                        
254500                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR                
254600                 MOVE NEJ TO INPUT-RETT                                   
254700                 MOVE FEL-ERR-BELEV   TO WS-ERROR-UPDX                    
254800              END-IF                                                      
254900           END-IF                                                         
255000        ELSE                                                              
255100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEV-ATTR                    
255200        END-IF                                                            
255300     END-IF                                                               
255400                                                                          
255500     IF MID-FLGAMART = JA OR NEJ                                          
255600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLGAMART-ATTR                     
255700       MOVE MID-FLGAMART         TO WS-FLGAMART                           
255800     ELSE                                                                 
255900        IF MID-FLGAMART = ALL '+'                                         
256000           MOVE NEJ             TO WS-FLGAMART                            
256100           MOVE MFS-RENSA-FAELT TO MOD-FLGAMART-IN                        
256200        ELSE                                                              
256300           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLGAMART-ATTR                   
256400           MOVE NEJ TO INPUT-RETT                                         
256500           MOVE FEL-ERR-FLAGMART TO WS-ERROR-UPDX                         
256600        END-IF                                                            
256700     END-IF                                                               
256800                                                                          
256900     IF MID-FLRSBEART = JA OR NEJ                                         
257000        MOVE MID-FLRSBEART        TO WS-FLRSBEART                         
257100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRSBEART-ATTR                   
257200     ELSE                                                                 
257300        IF MID-FLRSBEART = ALL '+'                                        
257400           MOVE NEJ TO WS-FLRSBEART                                       
257500                      MOD-FLRSBEART-IN                                    
257600        ELSE                                                              
257700           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRSBEART-ATTR                  
257800           MOVE NEJ TO INPUT-RETT                                         
257900           MOVE FEL-ERR-FLRSBEART  TO WS-ERROR-UPDX                       
258000        END-IF                                                            
258100     END-IF                                                               
258200                                                                          
258300     INSPECT MID-BEART REPLACING ALL '<' BY SPACE                         
258400     INSPECT MID-BEART REPLACING ALL '>' BY SPACE                         
258500                                                                          
258600     IF MID-BEART = ALL '+' OR SPACE                                      
258700        MOVE MFS-RENSA-FAELT TO MOD-BEART-IN                              
258800        IF VAECKNING = JA                                                 
258900           MOVE IDARTNR-WS      TO W-IDARTNR                              
259000           MOVE 'S  '           TO W-IDSKYLT                              
259100           PERFORM IMS-GET-BENA11-CSEQ                                    
259200           MOVE BENA-TEXT-BEART TO WS-BEART                               
259300                                   WS-BEART-SVE                           
259400        ELSE                                                              
259500           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR                      
259600           MOVE NEJ TO INPUT-RETT                                         
259700           MOVE FEL-ERR-BEART      TO WS-ERROR-UPDX                       
259800        END-IF                                                            
259900     ELSE                                                                 
260000        IF VAECKNING = JA                                                 
260100           MOVE NEJ                TO INPUT-RETT                          
260200           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR                      
260300        ELSE                                                              
260400           MOVE MID-BEART            TO WS-BEART                          
260500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-ATTR                    
260600        END-IF                                                            
260700     END-IF                                                               
260800                                                                          
260900     IF MID-IDSKYLT = 'GB ' OR 'S  '                                      
261000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-ATTR                     
261100        MOVE MID-IDSKYLT          TO WS-IDSKYLT                           
261200     ELSE                                                                 
261300        IF MID-IDSKYLT = ALL '+'                                          
261400           MOVE 'S  '           TO WS-IDSKYLT                             
261500           MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT-IN                         
261600        ELSE                                                              
261700           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-ATTR                    
261800           MOVE NEJ                TO INPUT-RETT                          
261900           MOVE FEL-ERR-IDSKYLT    TO WS-ERROR-UPDX                       
262000        END-IF                                                            
262100     END-IF                                                               
262200                                                                          
262300     IF MID-IDRITN = ALL '+'                                              
262400        MOVE MFS-RENSA-FAELT TO MOD-IDRITN-IN                             
262500        MOVE NEJ TO INPUT-RETT                                            
262600        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                        
262700     ELSE                                                                 
262800        IF MID-IDRITN = SPACE                                             
262900           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                     
263000           MOVE NEJ TO INPUT-RETT                                         
263100           MOVE FEL-ERR-IDRITN    TO WS-ERROR-UPDX                        
263200        ELSE                                                              
263300           MOVE MID-IDRITN TO WS-IDRITN                                   
263400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDRITN-ATTR                   
263500        END-IF                                                            
263600     END-IF                                                               
263700                                                                          
263800     IF MID-IDPROJ = ALL '+' OR SPACE                                     
263900*       IF KDPRODSL-LOK-ART                                               
264000*          MOVE 'PROD' TO MID-IDPROJ                                      
264100*                         WS-IDPROJ                                       
264200*          PERFORM S14-GODK-PROJ-MFS-RAETT-FEL                            
264300*       ELSE                                                              
264400           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJ-ATTR                     
264500           MOVE NEJ TO INPUT-RETT                                         
264600           MOVE MFS-RENSA-FAELT TO MOD-IDPROJ-IN                          
264700           MOVE FEL-ERR-IDPROJ  TO WS-ERROR-UPDX                          
264800*       END IF                                                            
264900     ELSE                                                                 
265000*       IF KDPRODSL-LOK-ART                                               
265100*          IF MID-IDPROJ NOT = 'PROD'                                     
265200*             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJ-ATTR                  
265300*             MOVE NEJ TO INPUT-RETT                                      
265400*          ELSE                                                           
265500*             MOVE MID-IDPROJ TO WS-IDPROJ                                
265600*             PERFORM S14-GODK-PROJ-MFS-RAETT-FEL                         
265700*          END-IF                                                         
265800*       ELSE                                                              
265900           MOVE MID-IDPROJ TO WS-IDPROJ                                   
266000           PERFORM S14-GODK-PROJ-MFS-RAETT-FEL                            
266100*       END-IF                                                            
266200     END-IF                                                               
266300                                                                          
266400     IF MID-IDPROENH-1 = ALL '+'                                          
266500        MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-1-IN                         
266600     ELSE                                                                 
266700        IF MID-IDPROENH-1  NUMERIC                                        
266800           MOVE MID-IDPROENH-1      TO WS-IDPROENH(1)                     
266900           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-1-ATTR                
267000        ELSE                                                              
267100           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-1-ATTR                
267200           MOVE NEJ TO INPUT-RETT                                         
267300           MOVE FEL-ERR-IDPROENH    TO  WS-ERROR-UPDX                     
267400        END-IF                                                            
267500     END-IF                                                               
267600                                                                          
267700     IF MID-IDPROENH-2 = ALL '+'                                          
267800       MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-2-IN                          
267900     ELSE                                                                 
268000       IF MID-IDPROENH-2  NUMERIC                                         
268100          MOVE MID-IDPROENH-2 TO WS-IDPROENH(2)                           
268200          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-2-ATTR                 
268300       ELSE                                                               
268400          MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-2-ATTR                 
268500          MOVE NEJ TO INPUT-RETT                                          
268600          MOVE FEL-ERR-IDPROENH    TO  WS-ERROR-UPDX                      
268700       END-IF                                                             
268800     END-IF                                                               
268900                                                                          
269000     IF MID-IDPROENH-3 = ALL '+'                                          
269100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROENH-3-ATTR                   
269200     ELSE                                                                 
269300       IF MID-IDPROENH-3  NUMERIC                                         
269400          MOVE MID-IDPROENH-3 TO WS-IDPROENH(3)                           
269500          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-3-ATTR                 
269600       ELSE                                                               
269700          MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-3-ATTR                 
269800          MOVE NEJ TO INPUT-RETT                                          
269900          MOVE FEL-ERR-IDPROENH    TO  WS-ERROR-UPDX                      
270000       END-IF                                                             
270100     END-IF                                                               
270200                                                                          
270300     IF MID-IDBERED NUMERIC                                               
270400       IF MID-IDBERED > ZERO                                              
270500          MOVE MID-IDBERED TO WS-IDBERED                                  
270600          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDBERED-ATTR                    
270700       ELSE                                                               
270800          MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                      
270900          MOVE NEJ TO INPUT-RETT                                          
271000          MOVE FEL-ERR-IDBERED     TO  WS-ERROR-UPDX                      
271100       END-IF                                                             
271200     ELSE                                                                 
271300       MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                         
271400       MOVE NEJ TO INPUT-RETT                                             
271500     END-IF                                                               
271600                                                                          
271700     IF MID-KDSORT = 'ST' OR 'SA' OR 'KG' OR 'M ' OR ' M' OR              
271800       ' L' OR 'L ' OR 'MM' OR 'G ' OR ' G' OR 'C2' OR 'M2'               
271900       OR 'ML' OR 'SW' OR 'TM' OR 'HW' OR 'PA'                            
272000       MOVE MID-KDSORT TO WS-KDSORT                                       
272100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-ATTR                       
272200     ELSE                                                                 
272300       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                         
272400       MOVE NEJ TO INPUT-RETT                                             
272500       MOVE FEL-ERR-KDSORT      TO  WS-ERROR-UPDX                         
272600     END-IF                                                               
272700                                                                          
272800     IF MID-IDAO =  ALL '+' OR SPACE                                      
272900        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDAO-ATTR                          
273000        MOVE NEJ TO INPUT-RETT                                            
273100        MOVE FEL-ERR-IDAO      TO  WS-ERROR-UPDX                          
273200     ELSE                                                                 
273300        MOVE MID-IDAO TO WS-IDAO                                          
273400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-ATTR                        
273500     END-IF                                                               
273600                                                                          
273700     MOVE MID-TISOP   TO XX-TISOP                                         
273800     IF MID-TISOP = ALL '+'                                               
273900        MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                          
274000        MOVE NEJ               TO INPUT-RETT                              
274100        MOVE FEL-ERR-TISOP     TO  WS-ERROR-UPDX                          
274200     ELSE                                                                 
274300        MOVE '1'         TO XX-DAG-SOP                                    
274400                                                                          
274500        IF XX-TISOP NUMERIC                                               
274600           MOVE XX-TISOP     TO WS-TISOP                                  
274700           MOVE WS-TISOP     TO DAT-I-TIDATUM                             
274800           MOVE 'AAVVD '     TO DAT-KDDATFORM                             
274900           PERFORM S99-WDATKONV                                           
275000           IF DAT-KDSVAR-OK                                               
275100             PERFORM S98-OM-TVA-AAR                                       
275200             MOVE WS-TISOP       TO TMP1-YYWWD                            
275300             MOVE AAVVD          TO TMP2-YYWWD                            
275400             MOVE DAT-TIAAVVD    TO TMP3-YYWWD                            
275500             PERFORM WY2000P2                                             
275600             IF TMP1-YYWWD < TMP2-YYWWD                                   
275700                MOVE MFS-NUM-FAELT-RAETT TO MOD-TISOP-ATTR                
275800             ELSE                                                         
275900                MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                  
276000                MOVE NEJ TO INPUT-RETT                                    
276100                MOVE FEL-ERR-TISOP TO WS-ERROR-UPDX                       
276200             END-IF                                                       
276300                                                                          
276400           ELSE                                                           
276500              MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                    
276600              MOVE NEJ TO INPUT-RETT                                      
276700              MOVE FEL-ERR-TISOP TO WS-ERROR-UPDX                         
276800           END-IF                                                         
276900        ELSE                                                              
277000           MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                       
277100           MOVE NEJ TO INPUT-RETT                                         
277200           MOVE FEL-ERR-TISOP TO WS-ERROR-UPDX                            
277300        END-IF                                                            
277400     END-IF                                                               
277500                                                                          
277600     IF MID-TISOP NOT = ALL '+'                                           
277700        PERFORM S25-TIFINLV-FRAN-SOP                                      
277800     END-IF                                                               
277900                                                                          
278000     MOVE SPAR-TIFINLV-AAVVD TO XX-TIFINLV                                
278100     IF XX-AAR = '99'                                                     
278200     AND XX-VECKA = '99'                                                  
278300        MOVE '9'         TO XX-DAG                                        
278400     ELSE                                                                 
278500        MOVE '1'         TO XX-DAG                                        
278600     END-IF                                                               
278700                                                                          
278800     MOVE XX-TIFINLV TO WS-TIFINLV                                        
278900     MOVE WS-TIFINLV        TO DAT-I-TIDATUM                              
279000     MOVE 'AAVVD '          TO DAT-KDDATFORM                              
279100     PERFORM S99-WDATKONV                                                 
279200     IF DAT-KDSVAR-OK                                                     
279300        PERFORM S98-OM-TVA-AAR                                            
279400        MOVE WS-TIFINLV TO TMP1-YYWWD                                     
279500        MOVE AAVVD           TO TMP2-YYWWD                                
279600        MOVE DAT-TIAAVVD TO TMP3-YYWWD                                    
279700        PERFORM WY2000Q2                                                  
279800        IF TMP1-YYWWD < TMP2-YYWWD                                        
279900        AND TMP1-YYWWD > TMP3-YYWWD                                       
280000           MOVE WS-TIFINLV TO AAVVD                                       
280100           MOVE +1 TO D                                                   
280200           MOVE AAVVD TO WS-TIFINLV                                       
280300        ELSE                                                              
280400           MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                       
280500           MOVE NEJ TO INPUT-RETT                                         
280600           MOVE FEL-ERR-TISOP       TO  WS-ERROR-UPDX                     
280700        END-IF                                                            
280800                                                                          
280900     ELSE                                                                 
281000        MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                          
281100        MOVE NEJ TO INPUT-RETT                                            
281200        MOVE FEL-ERR-TISOP       TO  WS-ERROR-UPDX                        
281300     END-IF                                                               
281400                                                                          
281500     IF MID-IDFKNGRP NUMERIC                                              
281600       IF MID-IDFKNGRP > ZERO                                             
281700          MOVE MID-IDFKNGRP TO WS-IDFKNGRP                                
281800                               WS-TEST-IDFKNGRP                           
281900          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-ATTR                   
282000       ELSE                                                               
282100          MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                     
282200          MOVE NEJ TO INPUT-RETT                                          
282300          MOVE FEL-ERR-IDFKNGRP    TO  WS-ERROR-UPDX                      
282400       END-IF                                                             
282500     ELSE                                                                 
282600       MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                        
282700       MOVE NEJ TO INPUT-RETT                                             
282800       MOVE FEL-ERR-IDFKNGRP  TO  WS-ERROR-UPDX                           
282900     END-IF                                                               
283000                                                                          
283100     IF MID-TEARTNOT-2 = ALL '+'                                          
283200       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-2                             
283300     ELSE                                                                 
283400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-2-ATTR                   
283500     END-IF                                                               
283600                                                                          
283700     IF MID-TEARTNOT-7 = ALL '+'                                          
283800       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-7                             
283900     ELSE                                                                 
284000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-7-ATTR                   
284100     END-IF                                                               
284200                                                                          
284300     IF INPUT-RETT = JA                                                   
284400        PERFORM S93-KOLLA-SOFTWARE                                        
284500     END-IF                                                               
284600     .                                                                    
284700     EJECT                                                                
284800 S02-NYA-ARTREG-DATAELEMENT SECTION.                                      
284900     SKIP2                                                                
285000     MOVE SPACE TO INMATAD-IDKAT                                          
285100                                                                          
285200     IF MID-TEARTNOT-4 = ALL '+'                                          
285300       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-4                             
285400     ELSE                                                                 
285500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-4-ATTR                   
285600     END-IF                                                               
285700                                                                          
285800     IF MID-IDKAT-1 = ALL '+'                                             
285900       MOVE MFS-RENSA-FAELT TO MOD-IDKAT-1-IN                             
286000     ELSE                                                                 
286100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-1-ATTR                      
286200     END-IF                                                               
286300                                                                          
286400     IF MID-IDKAT-2 = ALL '+'                                             
286500        MOVE MFS-RENSA-FAELT TO MOD-IDKAT-2-IN                            
286600     ELSE                                                                 
286700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-2-ATTR                     
286800     END-IF                                                               
286900                                                                          
287000     IF MID-IDKAT-3 = ALL '+'                                             
287100        MOVE MFS-RENSA-FAELT TO MOD-IDKAT-3-IN                            
287200     ELSE                                                                 
287300        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-3-ATTR                     
287400     END-IF                                                               
287500                                                                          
287600     IF MID-IDPROJUP = ALL '+'                                            
287700       MOVE MFS-RENSA-FAELT TO MOD-IDPROJUP-IN                            
287800     ELSE                                                                 
287900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJUP-ATTR                     
288000     END-IF                                                               
288100                                                                          
288200     IF MID-KDUART = 'A' OR 'M' OR 'S' OR 'P'                             
288300              OR 'K' OR 'B' OR SPACE                                      
288400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDUART-ATTR                       
288500     ELSE                                                                 
288600        IF MID-KDUART = ALL '+'                                           
288700           MOVE MFS-RENSA-FAELT TO MOD-KDUART-ATTR                        
288800        ELSE                                                              
288900           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDUART-ATTR                     
289000           MOVE NEJ TO INPUT-RETT                                         
289100           MOVE FEL-ERR-KDUART     TO WS-ERROR-UPDX                       
289200        END-IF                                                            
289300     END-IF                                                               
289400                                                                          
289500     IF MID-KDYTBEH NUMERIC AND                                           
289600        MID-KDYTBEH < 10                                                  
289700***    NO MORE TESTING IF MID-KDYTBEH = 00 OR 01 OR 02 OR                 
289800***    (93-02-03)                       03 OR 04 OR 05 OR                 
289900***                                     06 OR 07 OR 08                    
290000       MOVE MFS-NUM-FAELT-RAETT TO MOD-KDYTBEH-ATTR                       
290100     ELSE                                                                 
290200       MOVE MFS-NUM-FAELT-FEL TO MOD-KDYTBEH-ATTR                         
290300       MOVE NEJ TO INPUT-RETT                                             
290400       MOVE FEL-ERR-KDYTBEH    TO WS-ERROR-UPDX                           
290500     END-IF                                                               
290600                                                                          
290700     IF MID-KDFARLIG NUMERIC                                              
290800        IF MID-KDFARLIG = 0 OR 3 OR 4 OR 6 OR 7                           
290900           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFARLIG-ATTR                  
291000        ELSE                                                              
291100           MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARLIG-ATTR                    
291200           MOVE NEJ TO INPUT-RETT                                         
291300           MOVE FEL-ERR-KDFARLIG  TO WS-ERROR-UPDX                        
291400        END-IF                                                            
291500     ELSE                                                                 
291600        MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARLIG-ATTR                       
291700        MOVE NEJ TO INPUT-RETT                                            
291800        MOVE FEL-ERR-KDFARLIG  TO WS-ERROR-UPDX                           
291900     END-IF                                                               
292000                                                                          
292100     IF MID-KDBPSR NUMERIC                                                
292200        IF MID-KDBPSR = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8              
292300           MOVE MFS-NUM-FAELT-RAETT       TO MOD-KDBPSR-ATTR              
292400        ELSE                                                              
292500           MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-ATTR                      
292600           MOVE NEJ TO INPUT-RETT                                         
292700           MOVE FEL-ERR-KDBPSR  TO WS-ERROR-UPDX                          
292800        END-IF                                                            
292900     ELSE                                                                 
293000          MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-ATTR                       
293100          MOVE NEJ TO INPUT-RETT                                          
293200          MOVE FEL-ERR-KDBPSR  TO WS-ERROR-UPDX                           
293300     END-IF                                                               
293400     IF MID-FLLSRDEL = JA OR NEJ                                          
293500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLSRDEL-ATTR                     
293600     ELSE                                                                 
293700       MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLSRDEL-ATTR                       
293800       MOVE NEJ TO INPUT-RETT                                             
293900       MOVE FEL-ERR-FLLSRDEL   TO WS-ERROR-UPDX                           
294000     END-IF                                                               
294100                                                                          
294200* BELOW LOGIC IS ONLY FOR TCPLM                                           
294300     IF MFS-UPD-X                                                         
294400       IF MID-IDCDS = ALL '+' OR SPACE                                    
294500*SINCE THERE IS NO MOD-IDCDS HENCE WE MOVE MFS-ALFA                       
294600*         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLSRDEL-ATTR                    
294700          MOVE NEJ                TO INPUT-RETT                           
294800          MOVE FEL-ERR-IDCDS      TO WS-ERROR-UPDX                        
294900       END-IF                                                             
295000       IF MID-KDARTSYS = ALL '+' OR SPACE                                 
295100*         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLSRDEL-ATTR                    
295200          MOVE NEJ                TO INPUT-RETT                           
295300          MOVE FEL-ERR-KDARTSYS   TO WS-ERROR-UPDX                        
295400       END-IF                                                             
295500     ELSE                                                                 
295600       MOVE SPACE                 TO MID-IDCDS                            
295700                                     MID-KDARTSYS                         
295800     END-IF                                                               
295900                                                                          
296000     .                                                                    
296100     EJECT                                                                
296200 S03-NYA-NYPON-DATAELEMENT SECTION.                                       
296300                                                                          
296400     IF MID-TEORSAK-1 = ALL '+'                                           
296500       MOVE MFS-RENSA-FAELT TO MOD-TEORSAK-1                              
296600     ELSE                                                                 
296700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEORSAK-1-ATTR                    
296800     END-IF                                                               
296900     SKIP2                                                                
297000******************************************************************        
297100*    PROJK ÄR OBLIGATORISKT FÖR ARTIKLAR MED PRODUKTSLAG PV-BASL.         
297200*    I DETTA LÄGET SAKNAS NYPON-ART ATT KOPIERA FRÅN,         .           
297300*    PROJK MÅSTE NU ANGES I MID-PROJK.                                    
297400*    DESSA ARTIKLAR MÅSTE HA GODK-PROJK, UPPLAGDA PÅ BILD 1153.           
297500******************************************************************        
297600                                                                          
297700     MOVE WS-KDPRODSL TO TEST-KDPRODSL                                    
297800     IF MID-IDPROJK = ALL '+'                                             
297900        IF KDPRODSL-UTAN-EMB                                              
298000           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJK-ATTR                  
298100           MOVE NEJ TO INPUT-RETT                                         
298200           MOVE MFS-RENSA-FAELT TO MOD-IDPROJK-IN                         
298300           MOVE FEL-ERR-IDPROJK TO WS-ERROR-UPDX                          
298400        ELSE                                                              
298500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-ATTR                  
298600           IF KDPRODSL-LOCAL                                              
298700*             MOVE 'PROD' TO WS-IDPROJK                                   
298800*             PERFORM S15-KTR-GODK-PROJK-PV                               
298900*             IF PROJK-GODK                                               
299000*                MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-ATTR            
299100*             ELSE                                                        
299200                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJK-ATTR            
299300                 MOVE MFS-RENSA-FAELT TO MOD-IDPROJK-IN                   
299400                 MOVE NEJ TO INPUT-RETT                                   
299500                 MOVE FEL-ERR-KDPRODSL TO WS-ERROR-UPDX                   
299600*             END-IF                                                      
299700           END-IF                                                         
299800        END-IF                                                            
299900     ELSE                                                                 
300000                                                                          
300100        MOVE MID-IDPROJK  TO WS-IDPROJK                                   
300200        IF KDPRODSL-UTAN-EMB OR KDPRODSL-LOCAL                            
300300           PERFORM S15-KTR-GODK-PROJK-PV                                  
300400           IF PROJK-GODK                                                  
300500              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-ATTR               
300600*             IF KDPRODSL-LOK-ART                                         
300700*                IF MID-IDPROJK NOT = 'PROD'                              
300800*                   MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJK-ATTR           
300900*                   MOVE NEJ TO INPUT-RETT                                
301000*                END-IF                                                   
301100*             END-IF                                                      
301200           ELSE                                                           
301300              MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJK-ATTR               
301400              MOVE NEJ TO INPUT-RETT                                      
301500              MOVE FEL-ERR-IDPROJK    TO WS-ERROR-UPDX                    
301600           END-IF                                                         
301700        ELSE                                                              
301800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-ATTR                  
301900        END-IF                                                            
302000     END-IF                                                               
302100                                                                          
302200     IF MID-KVARTVAGN = ALL '+'                                           
302300        MOVE MFS-RENSA-FAELT TO MOD-KVARTVAGN-IN                          
302400        MOVE ZERO            TO WS-KVARTVAGN                              
302500     ELSE                                                                 
302600        IF MID-KVARTVAGN NUMERIC                                          
302700           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVARTVAGN-ATTR                 
302800           MOVE MID-KVARTVAGN       TO WS-KVARTVAGN                       
302900        ELSE                                                              
303000           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVARTVAGN-ATTR              
303100           MOVE NEJ                    TO INPUT-RETT                      
303200           MOVE FEL-ERR-KVARTVAGN  TO WS-ERROR-UPDX                       
303300        END-IF                                                            
303400     END-IF                                                               
303500                                                                          
303600     IF MID-IDARTNR-MOTSV = ALL '+'                                       
303700        MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-MOTSV-IN                 
303800        MOVE ZERO                 TO WS-IDARTNR-MOTSV                     
303900     ELSE                                                                 
304000        IF MID-IDARTNR-MOTSV NUMERIC                                      
304100           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-MOTSV-ATTR            
304200           MOVE MID-IDARTNR-MOTSV    TO WS-IDARTNR-MOTSV                  
304300        ELSE                                                              
304400           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-MOTSV-ATTR            
304500           MOVE NEJ                  TO INPUT-RETT                        
304600           MOVE FEL-ERR-IDARTNR      TO WS-ERROR-UPDX                     
304700        END-IF                                                            
304800     END-IF                                                               
304900                                                                          
305000     IF MID-FLPISK = ALL '+'                                              
305100        MOVE NEJ   TO WS-FLPISK   MOD-FLPISK-IN                           
305200     ELSE                                                                 
305300        IF MID-FLPISK = JA OR NEJ                                         
305400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPISK-ATTR                   
305500           MOVE MID-FLPISK TO WS-FLPISK                                   
305600        ELSE                                                              
305700           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLPISK-ATTR                     
305800           MOVE NEJ TO INPUT-RETT                                         
305900           MOVE FEL-ERR-FLPISK     TO WS-ERROR-UPDX                       
306000        END-IF                                                            
306100     END-IF                                                               
306200                                                                          
306300     IF MID-FLBYTES = ALL '+'                                             
306400        MOVE NEJ   TO WS-FLBYTES  MOD-FLBYTES-IN                          
306500     ELSE                                                                 
306600        IF MID-FLBYTES = JA OR NEJ                                        
306700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBYTES-ATTR                  
306800           MOVE MID-FLBYTES TO WS-FLBYTES                                 
306900        ELSE                                                              
307000           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBYTES-ATTR                    
307100           MOVE NEJ TO INPUT-RETT                                         
307200           MOVE FEL-ERR-FLBYTES    TO WS-ERROR-UPDX                       
307300        END-IF                                                            
307400     END-IF                                                               
307500                                                                          
307600     IF MID-KVPROG = ALL '+'                                              
307700        MOVE ZERO TO WS-KVPROG                                            
307800     ELSE                                                                 
307900        IF MID-KVPROG NUMERIC                                             
308000           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPROG-ATTR                    
308100           MOVE MID-KVPROG TO WS-KVPROG                                   
308200        ELSE                                                              
308300           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROG-ATTR                      
308400           MOVE NEJ TO INPUT-RETT                                         
308500           MOVE FEL-ERR-KVPROG    TO WS-ERROR-UPDX                        
308600        END-IF                                                            
308700     END-IF                                                               
308800     .                                                                    
308900     EJECT                                                                
309000 S04-KOP-GEMENSAMMA-DATAELEMENT SECTION.                                  
309100     SKIP2                                                                
309200                                                                          
309300     MOVE ZERO TO WS-IDPROENH(1)                                          
309400                  WS-IDPROENH(2)                                          
309500                  WS-IDPROENH(3)                                          
309600                                                                          
309700     IF MID-KDPRODSL = C-KDPRODSL-19                                      
309800        IF MID-KDSORT = ALL '+'                                           
309900           MOVE C-PS19-DEFAULT-KDSORT    TO MID-KDSORT                    
310000                                            MOD-KDSORT-IN                 
310100        END-IF                                                            
310200        IF MID-KDYTBEH = ALL '+'                                          
310300           MOVE C-PS19-DEFAULT-KDYTBEH   TO MID-KDYTBEH                   
310400                                            MOD-KDYTBEH-IN                
310500        END-IF                                                            
310600        IF MID-IDPROJ = ALL '+'                                           
310700           MOVE C-PS19-DEFAULT-IDPROJ    TO MID-IDPROJ                    
310800                                            MOD-IDPROJ-IN                 
310900                                             WS-IDPROJ                    
311000        END-IF                                                            
311100        IF MID-KDFARLIG = ALL '+'                                         
311200           MOVE C-PS19-DEFAULT-KDFARLIG  TO MID-KDFARLIG                  
311300                                            MOD-KDFARLIG-IN               
311400        END-IF                                                            
311500        IF MID-KDBPSR  = ALL '+'                                          
311600           MOVE C-PS19-DEFAULT-KDBPSR    TO MID-KDBPSR                    
311700                                            MOD-KDBPSR-IN                 
311800        END-IF                                                            
311900        IF MID-IDPROJK = ALL '+'                                          
312000           MOVE C-PS19-DEFAULT-IDPROJK   TO MID-IDPROJK                   
312100                                            MOD-IDPROJK-IN                
312200                                             WS-IDPROJK                   
312300        END-IF                                                            
312400        IF MID-FLLSRDEL = ALL '+'                                         
312500           MOVE C-PS19-DEFAULT-FLLSRDEL  TO MID-FLLSRDEL                  
312600                                            MOD-FLLSRDEL-IN               
312700        END-IF                                                            
312800     END-IF                                                               
312900                                                                          
313000     IF MID-IDLEVNR = ALL '+'                                             
313100        IF MID-BELEV = ALL '+'                                            
313200           MOVE MFS-RENSA-FAELT TO MOD-BELEV-IN                           
313300                                   MOD-IDLEVNR-IN                         
313400        ELSE                                                              
313500           MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR                      
313600                                      MOD-IDLEVNR-ATTR                    
313700           MOVE NEJ TO INPUT-RETT                                         
313800        END-IF                                                            
313900     ELSE                                                                 
314000        IF MID-IDLEVNR(1:1) NOT = ' ' AND '0' AND '+'                     
314100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-ATTR                  
314200        ELSE                                                              
314300           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR                    
314400           MOVE NEJ TO INPUT-RETT                                         
314500        END-IF                                                            
314600                                                                          
314700                                                                          
314800        IF MID-BELEV = ALL '+' OR SPACE                                   
314900           IF MID-IDLEVNR(1:1) NOT = ' ' AND '0' AND '+'                  
315000              IF MID-IDLEVNR = '1002 '                                    
315100                 MOVE MFS-RENSA-FAELT TO MOD-BELEV-IN                     
315200              ELSE                                                        
315300                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR                
315400                 MOVE NEJ TO INPUT-RETT                                   
315500              END-IF                                                      
315600           END-IF                                                         
315700        ELSE                                                              
315800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEV-ATTR                    
315900        END-IF                                                            
316000     END-IF                                                               
316100                                                                          
316200     IF MID-FLGAMART = JA OR NEJ                                          
316300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLGAMART-ATTR                     
316400       MOVE MID-FLGAMART TO WS-FLGAMART                                   
316500     ELSE                                                                 
316600        IF MID-FLGAMART = ALL '+'                                         
316700           MOVE NEJ TO WS-FLGAMART                                        
316800           MOVE MFS-RENSA-FAELT TO MOD-FLGAMART-IN                        
316900        ELSE                                                              
317000           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLGAMART-ATTR                   
317100           MOVE NEJ TO INPUT-RETT                                         
317200        END-IF                                                            
317300     END-IF                                                               
317400                                                                          
317500     IF MID-FLRSBEART = JA OR NEJ                                         
317600        MOVE MID-FLRSBEART TO WS-FLRSBEART                                
317700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRSBEART-ATTR                   
317800     ELSE                                                                 
317900        IF MID-FLRSBEART = ALL '+'                                        
318000           MOVE NEJ TO WS-FLRSBEART                                       
318100                       MOD-FLRSBEART-IN                                   
318200        ELSE                                                              
318300           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRSBEART-ATTR                  
318400           MOVE NEJ TO INPUT-RETT                                         
318500        END-IF                                                            
318600     END-IF                                                               
318700                                                                          
318800     IF MID-IDSKYLT = 'GB ' OR 'S  '                                      
318900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-ATTR                     
319000        MOVE MID-IDSKYLT TO WS-IDSKYLT                                    
319100     ELSE                                                                 
319200        IF MID-IDSKYLT = ALL '+'                                          
319300           MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT-IN                         
319400           MOVE 'S  ' TO WS-IDSKYLT                                       
319500        ELSE                                                              
319600           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-ATTR                    
319700           MOVE NEJ TO INPUT-RETT                                         
319800        END-IF                                                            
319900     END-IF                                                               
320000                                                                          
320100     INSPECT MID-BEART REPLACING ALL '<' BY SPACE                         
320200     INSPECT MID-BEART REPLACING ALL '>' BY SPACE                         
320300                                                                          
320400     IF MID-BEART = ALL '+' OR SPACE                                      
320500       MOVE MFS-RENSA-FAELT TO MOD-BEART-IN                               
320600       IF WS-IDSKYLT = 'GB '                                              
320700          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-ATTR                     
320800          MOVE NEJ TO INPUT-RETT                                          
320900       ELSE                                                               
321000          MOVE IDARTNR-WS TO W-IDARTNR                                    
321100          MOVE 'S  ' TO W-IDSKYLT                                         
321200          PERFORM IMS-GET-BENA11-CSEQ                                     
321300          MOVE BENA-TEXT-BEART TO WS-BEART                                
321400                                  WS-BEART-SVE                            
321500       END-IF                                                             
321600     ELSE                                                                 
321700       MOVE MID-BEART TO WS-BEART                                         
321800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-ATTR                        
321900     END-IF                                                               
322000                                                                          
322100     IF MID-IDRITN = ALL '+'                                              
322200       MOVE MFS-RENSA-FAELT TO MOD-IDRITN-IN                              
322300     ELSE                                                                 
322400       MOVE MID-IDRITN TO WS-IDRITN                                       
322500     END-IF                                                               
322600                                                                          
322700     IF MID-IDPROJ = ALL '+'                                              
322800       MOVE MFS-RENSA-FAELT TO MOD-IDPROJ-IN                              
322900     ELSE                                                                 
323000       MOVE MID-IDPROJ TO WS-IDPROJ                                       
323100     END-IF                                                               
323200                                                                          
323300                                                                          
323400     IF MID-IDPROENH-1 = ALL '+'                                          
323500          MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-1-IN                       
323600     ELSE                                                                 
323700          IF MID-IDPROENH-1  NUMERIC                                      
323800             MOVE MID-IDPROENH-1 TO WS-IDPROENH(1)                        
323900             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-1-ATTR              
324000          ELSE                                                            
324100             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-1-ATTR              
324200             MOVE NEJ TO INPUT-RETT                                       
324300          END-IF                                                          
324400     END-IF                                                               
324500                                                                          
324600     IF MID-IDPROENH-2 = ALL '+'                                          
324700          MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-2-IN                       
324800     ELSE                                                                 
324900          IF MID-IDPROENH-2  NUMERIC                                      
325000             MOVE MID-IDPROENH-2 TO WS-IDPROENH(2)                        
325100             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-2-ATTR              
325200          ELSE                                                            
325300             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-2-ATTR              
325400             MOVE NEJ TO INPUT-RETT                                       
325500          END-IF                                                          
325600     END-IF                                                               
325700                                                                          
325800     IF MID-IDPROENH-3 = ALL '+'                                          
325900          MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-3-IN                       
326000     ELSE                                                                 
326100          IF MID-IDPROENH-3  NUMERIC                                      
326200             MOVE MID-IDPROENH-3 TO WS-IDPROENH(3)                        
326300             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-3-ATTR              
326400          ELSE                                                            
326500             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-3-ATTR              
326600             MOVE NEJ TO INPUT-RETT                                       
326700          END-IF                                                          
326800     END-IF                                                               
326900                                                                          
327000     IF MID-IDBERED = ALL '+'                                             
327100       MOVE MFS-RENSA-FAELT TO MOD-IDBERED-IN                             
327200     ELSE                                                                 
327300       IF MID-IDBERED NUMERIC                                             
327400         IF MID-IDBERED > ZERO                                            
327500            MOVE MID-IDBERED TO WS-IDBERED                                
327600            MOVE MFS-NUM-FAELT-RAETT TO MOD-IDBERED-ATTR                  
327700         ELSE                                                             
327800            MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                    
327900            MOVE NEJ TO INPUT-RETT                                        
328000         END-IF                                                           
328100       ELSE                                                               
328200         MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                       
328300         MOVE NEJ TO INPUT-RETT                                           
328400       END-IF                                                             
328500     END-IF                                                               
328600                                                                          
328700     IF MID-KDSORT = ALL '+'                                              
328800       MOVE MFS-RENSA-FAELT TO MOD-KDSORT-IN                              
328900       MOVE IDARTNR-WS TO W-IDARTNR                                       
329000       PERFORM IMS-GET-ARTC01                                             
329100       IF SEGMENT-FINNS                                                   
329200          MOVE ART-KDSORT TO WS-KDSORT                                    
329300       END-IF                                                             
329400     ELSE                                                                 
329500       IF MID-KDSORT = 'ST' OR 'SA' OR 'KG' OR 'M ' OR ' M' OR            
329600       ' L' OR 'L ' OR 'MM' OR 'G ' OR ' G' OR 'C2' OR 'M2'               
329700       OR 'ML' OR 'SW' OR 'TM' OR 'HW' OR 'PA'                            
329800         MOVE MID-KDSORT TO WS-KDSORT                                     
329900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-ATTR                     
330000       ELSE                                                               
330100         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                       
330200         MOVE NEJ TO INPUT-RETT                                           
330300       END-IF                                                             
330400     END-IF                                                               
330500                                                                          
330600     IF MID-IDAO = ALL '+' OR SPACE                                       
330700       MOVE MFS-RENSA-FAELT TO MOD-IDAO-IN                                
330800     ELSE                                                                 
330900       MOVE MID-IDAO TO WS-IDAO                                           
331000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-ATTR                         
331100     END-IF                                                               
331200                                                                          
331300     MOVE MID-TISOP   TO XX-TISOP                                         
331400     MOVE '+' TO XX-DAG-SOP                                               
331500     IF XX-TISOP = ALL '+'                                                
331600*       MOVE MFS-RENSA-FAELT TO MOD-TISOP-IN                              
331700        MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                          
331800        MOVE NEJ               TO INPUT-RETT                              
331900     ELSE                                                                 
332000                                                                          
332100        MOVE '1' TO XX-DAG-SOP                                            
332200                                                                          
332300        IF XX-TISOP NUMERIC                                               
332400           MOVE XX-TISOP TO WS-TISOP                                      
332500           IF WS-TISOP = 99999                                            
332600              MOVE WS-KDPRODSL   TO TEST-KDPRODSL                         
332700              IF KDPRODSL-PARTS-ACC OR KDPRODSL-SERVICES                  
332800                 IF FINNS-REG-PA-NYPON = JA                               
332900                    MOVE ARTG01-ART-KDPRODSL                              
333000                                 TO TEST-KDPRODSL                         
333100                    IF ARTG01-ART-FLUNIKRD = JA                           
333200                    OR   KDPRODSL-ACC                                     
333300                    OR   ARTG01-ART-IDLEVNR  = '9998 '                    
333400                       MOVE MFS-NUM-FAELT-FEL   TO                        
333500                                          MOD-TISOP-ATTR                  
333600                       MOVE NEJ                 TO                        
333700                                          INPUT-RETT                      
333800                    ELSE                                                  
333900                       PERFORM S96-LAES-XXAQ-KOP                          
334000                       IF SEGMENT-FINNS                                   
334100                          IF XXAQ-1132-TIFINLEV  = ZERO   AND             
334200                             XXAQ-1132-TIPRODSTA = +111111                
334300                             IF FINNS-REG-PA-NYPON = JA                   
334400                                PERFORM                                   
334500                                    HA-PLOCKA-NYASTE-TISERLEV             
334600                                IF W-SPAR-TISERLEV = +9999999             
334700                                   MOVE MFS-NUM-FAELT-RAETT TO            
334800                                               MOD-TISOP-ATTR             
334900                                   MOVE WS-TISOP   TO AAVVD               
335000                                   MOVE +9 TO D                           
335100                                   MOVE AAVVD      TO WS-TISOP            
335200                                ELSE                                      
335300                                   MOVE MFS-NUM-FAELT-FEL   TO            
335400                                                MOD-TISOP-ATTR            
335500                                   MOVE NEJ                 TO            
335600                                                   INPUT-RETT             
335700                                                                          
335800                                END-IF                                    
335900                             ELSE                                         
336000                                MOVE MFS-NUM-FAELT-RAETT TO               
336100                                                MOD-TISOP-ATTR            
336200                                MOVE WS-TISOP   TO AAVVD                  
336300                                MOVE +9 TO D                              
336400                                MOVE AAVVD         TO WS-TISOP            
336500                             END-IF                                       
336600                          ELSE                                            
336700                             MOVE MFS-NUM-FAELT-FEL   TO                  
336800                                               MOD-TISOP-ATTR             
336900                             MOVE NEJ             TO INPUT-RETT           
337000                          END-IF                                          
337100                       ELSE                                               
337200                          MOVE MFS-NUM-FAELT-FEL      TO                  
337300                                            MOD-TISOP-ATTR                
337400                          MOVE NEJ                     TO                 
337500                                                 INPUT-RETT               
337600                       END-IF                                             
337700                    END-IF                                                
337800                 ELSE                                                     
337900                    MOVE MFS-NUM-FAELT-FEL      TO                        
338000                                      MOD-TISOP-ATTR                      
338100                    MOVE NEJ                     TO                       
338200                                           INPUT-RETT                     
338300                 END-IF                                                   
338400                                                                          
338500              ELSE                                                        
338600                 MOVE MFS-NUM-FAELT-FEL      TO                           
338700                                   MOD-TISOP-ATTR                         
338800                    MOVE NEJ                     TO                       
338900                                        INPUT-RETT                        
339000              END-IF                                                      
339100           ELSE                                                           
339200              MOVE WS-TISOP       TO DAT-I-TIDATUM                        
339300              MOVE 'AAVVD '       TO DAT-KDDATFORM                        
339400              PERFORM S99-WDATKONV                                        
339500              IF DAT-KDSVAR-OK                                            
339600                PERFORM S98-OM-TVA-AAR                                    
339700                MOVE WS-KDPRODSL TO TEST-KDPRODSL                         
339800                IF KDPRODSL-VCBV                                          
339900                  MOVE WS-TISOP     TO TMP1-YYWWD                         
340000                  MOVE AAVVD        TO TMP2-YYWWD                         
340100                  PERFORM WY2000P2                                        
340200                  IF TMP1-YYWWD < TMP2-YYWWD                              
340300                     MOVE MFS-NUM-FAELT-RAETT TO MOD-TISOP-ATTR           
340400                  ELSE                                                    
340500                    MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR              
340600                    MOVE NEJ TO INPUT-RETT                                
340700                  END-IF                                                  
340800                ELSE                                                      
340900                  MOVE WS-TISOP     TO TMP1-YYWWD                         
341000                  MOVE AAVVD        TO TMP2-YYWWD                         
341100                  PERFORM WY2000Q2                                        
341200                  IF  TMP1-YYWWD < TMP2-YYWWD                             
341300                     MOVE MFS-NUM-FAELT-RAETT TO                          
341400                                              MOD-TISOP-ATTR              
341500                     MOVE WS-TIFINLV TO AAVVD                             
341600                     MOVE +1 TO D                                         
341700                     MOVE AAVVD TO WS-TIFINLV                             
341800                  ELSE                                                    
341900                   MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR               
342000                   MOVE NEJ TO INPUT-RETT                                 
342100                  END-IF                                                  
342200                END-IF                                                    
342300              ELSE                                                        
342400                 MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                 
342500                 MOVE NEJ TO INPUT-RETT                                   
342600              END-IF                                                      
342700           END-IF                                                         
342800        ELSE                                                              
342900           MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                       
343000           MOVE NEJ TO INPUT-RETT                                         
343100        END-IF                                                            
343200     END-IF                                                               
343300                                                                          
343400     IF MID-TISOP NOT = ALL '+'                                           
343500        PERFORM S25-TIFINLV-FRAN-SOP                                      
343600     END-IF                                                               
343700     MOVE SPAR-TIFINLV-AAVVD TO XX-TIFINLV                                
343800                                                                          
343900     IF  XX-AAR = '99'                                                    
344000     AND XX-VECKA = '99'                                                  
344100         MOVE '9' TO XX-DAG                                               
344200     ELSE                                                                 
344300         MOVE '1' TO XX-DAG                                               
344400     END-IF                                                               
344500                                                                          
344600     MOVE XX-TIFINLV TO WS-TIFINLV                                        
344700     IF WS-TIFINLV = 99999                                                
344800        MOVE WS-KDPRODSL         TO TEST-KDPRODSL                         
344900        IF KDPRODSL-PARTS-ACC OR KDPRODSL-SERVICES                        
345000           IF FINNS-REG-PA-NYPON = JA                                     
345100              MOVE ARTG01-ART-KDPRODSL  TO TEST-KDPRODSL                  
345200              IF ARTG01-ART-FLUNIKRD = JA                                 
345300              OR KDPRODSL-ACC                                             
345400              OR ARTG01-ART-IDLEVNR  = '9998 '                            
345500                 MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                 
345600                 MOVE NEJ               TO INPUT-RETT                     
345700              ELSE                                                        
345800                 PERFORM S96-LAES-XXAQ-KOP                                
345900                 IF SEGMENT-FINNS                                         
346000                    IF XXAQ-1132-TIFINLEV  = ZERO   AND                   
346100                       XXAQ-1132-TIPRODSTA = +111111                      
346200                       IF FINNS-REG-PA-NYPON = JA                         
346300                          PERFORM HA-PLOCKA-NYASTE-TISERLEV               
346400                          IF W-SPAR-TISERLEV = +9999999                   
346500                             MOVE WS-TIFINLV TO AAVVD                     
346600                             MOVE +9 TO D                                 
346700                             MOVE AAVVD      TO WS-TIFINLV                
346800                          ELSE                                            
346900                             MOVE MFS-NUM-FAELT-FEL   TO                  
347000                                                MOD-TISOP-ATTR            
347100                             MOVE NEJ  TO INPUT-RETT                      
347200                                                                          
347300                          END-IF                                          
347400                       ELSE                                               
347500                          MOVE WS-TIFINLV TO AAVVD                        
347600                          MOVE +9 TO D                                    
347700                          MOVE AAVVD         TO WS-TIFINLV                
347800                       END-IF                                             
347900                    ELSE                                                  
348000                       MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR           
348100                       MOVE NEJ               TO INPUT-RETT               
348200                    END-IF                                                
348300                 ELSE                                                     
348400                    MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR              
348500                    MOVE NEJ               TO INPUT-RETT                  
348600                 END-IF                                                   
348700              END-IF                                                      
348800           ELSE                                                           
348900              MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                    
349000              MOVE NEJ               TO INPUT-RETT                        
349100           END-IF                                                         
349200                                                                          
349300        ELSE                                                              
349400           MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                       
349500           MOVE NEJ               TO INPUT-RETT                           
349600        END-IF                                                            
349700     ELSE                                                                 
349800        MOVE WS-TIFINLV     TO DAT-I-TIDATUM                              
349900        MOVE 'AAVVD '       TO DAT-KDDATFORM                              
350000        PERFORM S99-WDATKONV                                              
350100        IF DAT-KDSVAR-OK                                                  
350200           PERFORM S98-OM-TVA-AAR                                         
350300           MOVE WS-KDPRODSL      TO TEST-KDPRODSL                         
350400           IF KDPRODSL-VCBV                                               
350500              MOVE WS-TIFINLV   TO TMP1-YYWWD                             
350600              MOVE AAVVD        TO TMP2-YYWWD                             
350700              PERFORM WY2000P2                                            
350800              IF TMP1-YYWWD < TMP2-YYWWD                                  
350900                 MOVE WS-TIFINLV TO AAVVD                                 
351000                 MOVE +1 TO D                                             
351100                 MOVE AAVVD TO WS-TIFINLV                                 
351200              ELSE                                                        
351300                 MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                 
351400                 MOVE NEJ TO INPUT-RETT                                   
351500              END-IF                                                      
351600           ELSE                                                           
351700              MOVE WS-TIFINLV   TO TMP1-YYWWD                             
351800              MOVE AAVVD        TO TMP2-YYWWD                             
351900              MOVE DAT-TIAAVVD  TO TMP3-YYWWD                             
352000              PERFORM WY2000Q2                                            
352100              IF  TMP1-YYWWD < TMP2-YYWWD                                 
352200              AND TMP1-YYWWD > TMP3-YYWWD                                 
352300                 MOVE WS-TIFINLV TO AAVVD                                 
352400                 MOVE +1 TO D                                             
352500                 MOVE AAVVD TO WS-TIFINLV                                 
352600              ELSE                                                        
352700                 MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                 
352800                 MOVE NEJ TO INPUT-RETT                                   
352900              END-IF                                                      
353000           END-IF                                                         
353100        END-IF                                                            
353200     END-IF                                                               
353300                                                                          
353400     IF MID-IDFKNGRP = ALL '+'                                            
353500       MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-IN                            
353600       MOVE IDARTNR-WS TO W-IDARTNR                                       
353700       PERFORM IMS-GET-ARTC01                                             
353800       IF SEGMENT-FINNS                                                   
353900          MOVE ART-IDFKNGRP TO WS-TEST-IDFKNGRP                           
354000       END-IF                                                             
354100     ELSE                                                                 
354200       IF MID-IDFKNGRP NUMERIC                                            
354300         IF MID-IDFKNGRP > ZERO                                           
354400            MOVE MID-IDFKNGRP TO WS-IDFKNGRP                              
354500                                 WS-TEST-IDFKNGRP                         
354600            MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-ATTR                 
354700         ELSE                                                             
354800            MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                   
354900            MOVE NEJ TO INPUT-RETT                                        
355000         END-IF                                                           
355100       ELSE                                                               
355200         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                      
355300         MOVE NEJ TO INPUT-RETT                                           
355400       END-IF                                                             
355500     END-IF                                                               
355600                                                                          
355700     IF MID-TEARTNOT-2 = ALL '+'                                          
355800       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-2                             
355900     ELSE                                                                 
356000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-2-ATTR                   
356100     END-IF                                                               
356200                                                                          
356300     IF MID-TEARTNOT-7 = ALL '+'                                          
356400       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-7                             
356500     ELSE                                                                 
356600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-7-ATTR                   
356700     END-IF                                                               
356800                                                                          
356900     IF INPUT-RETT = JA                                                   
357000        PERFORM S93-KOLLA-SOFTWARE                                        
357100     END-IF                                                               
357200     .                                                                    
357300     EJECT                                                                
357400 S05-KOP-ARTREG-DATAELEMENT SECTION.                                      
357500     SKIP2                                                                
357600     MOVE SPACE TO INMATAD-IDKAT                                          
357700                                                                          
357800     IF MID-TEARTNOT-4 = ALL '+' OR SPACE                                 
357900        MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-4-ATTR                       
358000     END-IF                                                               
358100                                                                          
358200     IF MID-IDKAT-1 = ALL '+'                                             
358300        MOVE MFS-RENSA-FAELT TO MOD-IDKAT-1-IN                            
358400     END-IF                                                               
358500                                                                          
358600     IF MID-IDKAT-2 = ALL '+'                                             
358700        MOVE MFS-RENSA-FAELT TO MOD-IDKAT-2-IN                            
358800     END-IF                                                               
358900                                                                          
359000     IF MID-IDKAT-3 = ALL '+'                                             
359100        MOVE MFS-RENSA-FAELT TO MOD-IDKAT-3-IN                            
359200     END-IF                                                               
359300                                                                          
359400     IF MID-IDPROJUP = ALL '+'                                            
359500       MOVE MFS-RENSA-FAELT TO MOD-IDPROJUP-IN                            
359600     END-IF                                                               
359700                                                                          
359800     IF MID-KDUART = ALL '+'                                              
359900        MOVE MFS-RENSA-FAELT TO MOD-KDUART-IN                             
360000     ELSE                                                                 
360100        IF MID-KDUART = 'A' OR 'M' OR 'S' OR 'P' OR                       
360200                    'K' OR 'B' OR SPACE                                   
360300            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDUART-ATTR                  
360400        ELSE                                                              
360500            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDUART-ATTR                    
360600            MOVE NEJ TO INPUT-RETT                                        
360700        END-IF                                                            
360800     END-IF                                                               
360900                                                                          
361000     IF MID-KDYTBEH = ALL '+'                                             
361100        MOVE MFS-RENSA-FAELT TO MOD-KDYTBEH-IN                            
361200     ELSE                                                                 
361300***     NO MORE TESTING IF MID-KDYTBEH = 00 OR 01 OR 02 OR                
361400***     (93-02-03)                       03 OR 04 OR 05 OR                
361500***                                      06 OR 07 OR 08                   
361600        IF MID-KDYTBEH NUMERIC AND                                        
361700           MID-KDYTBEH < 10                                               
361800          MOVE MFS-NUM-FAELT-RAETT TO MOD-KDYTBEH-ATTR                    
361900        ELSE                                                              
362000          MOVE MFS-NUM-FAELT-FEL TO MOD-KDYTBEH-ATTR                      
362100          MOVE NEJ TO INPUT-RETT                                          
362200        END-IF                                                            
362300     END-IF                                                               
362400                                                                          
362500     IF MID-KDFARLIG = ALL '+'                                            
362600        MOVE MFS-RENSA-FAELT TO MOD-KDFARLIG-IN                           
362700     ELSE                                                                 
362800        IF MID-KDFARLIG NUMERIC                                           
362900           IF MID-KDFARLIG = 0 OR 3 OR 4 OR 6 OR 7                        
363000              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFARLIG-ATTR               
363100           ELSE                                                           
363200              MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARLIG-ATTR                 
363300              MOVE NEJ TO INPUT-RETT                                      
363400           END-IF                                                         
363500        ELSE                                                              
363600           MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARLIG-ATTR                    
363700           MOVE NEJ TO INPUT-RETT                                         
363800        END-IF                                                            
363900     END-IF                                                               
364000                                                                          
364100     IF MID-KDBPSR = ALL '+'                                              
364200        MOVE MFS-RENSA-FAELT TO MOD-KDBPSR-IN                             
364300     ELSE                                                                 
364400        IF MID-KDBPSR NUMERIC                                             
364500           IF MID-KDBPSR = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8           
364600              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBPSR-ATTR                 
364700           ELSE                                                           
364800              MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-ATTR                   
364900              MOVE NEJ TO INPUT-RETT                                      
365000           END-IF                                                         
365100        ELSE                                                              
365200           MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-ATTR                      
365300           MOVE NEJ TO INPUT-RETT                                         
365400        END-IF                                                            
365500     END-IF                                                               
365600                                                                          
365700     IF MID-FLLSRDEL = ALL '+'                                            
365800        MOVE MFS-RENSA-FAELT TO MOD-FLLSRDEL-IN                           
365900     ELSE                                                                 
366000       IF MID-FLLSRDEL = JA OR NEJ                                        
366100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLSRDEL-ATTR                   
366200       ELSE                                                               
366300         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLSRDEL-ATTR                     
366400         MOVE NEJ TO INPUT-RETT                                           
366500       END-IF                                                             
366600     END-IF                                                               
366700     .                                                                    
366800     EJECT                                                                
366900 S06-KOP-NYPON-DATAELEMENT SECTION.                                       
367000     SKIP2                                                                
367100                                                                          
367200     IF MID-TEORSAK-1 = ALL '+'                                           
367300       MOVE MFS-RENSA-FAELT TO MOD-TEORSAK-1                              
367400     ELSE                                                                 
367500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEORSAK-1-ATTR                    
367600     END-IF                                                               
367700     SKIP2                                                                
367800                                                                          
367900******************************************************************        
368000*    PROJK ÄR OBLIGATORISKT FÖR ARTIKLAR MED PRODUKTSLAG PV-BASL.         
368100*    NYPON-ART FINNS, PROJK KOPIERAS HÄRIFRÅN OM MID-PROJK SAKNAS.        
368200*    DESSA ARTIKLAR MÅSTE HA GODK-PROJK, UPPLAGDA PÅ BILD 1153.           
368300******************************************************************        
368400                                                                          
368500     MOVE WS-KDPRODSL TO TEST-KDPRODSL                                    
368600                                                                          
368700     IF MID-IDPROJK = ALL '+'                                             
368800        MOVE MFS-RENSA-FAELT TO MOD-IDPROJK-IN                            
368900        MOVE ARTG01-ART-IDPROJK TO WS-IDPROJK                             
369000     ELSE                                                                 
369100        MOVE MID-IDPROJK  TO WS-IDPROJK                                   
369200     END-IF                                                               
369300                                                                          
369400*    IF KDPRODSL-LOK-ART                                                  
369500*       IF WS-IDPROJK = SPACE                                             
369600*          MOVE 'PROD' TO WS-IDPROJK                                      
369700*       END-IF                                                            
369800*    END-IF                                                               
369900                                                                          
370000     IF KDPRODSL-UTAN-EMB OR KDPRODSL-LOCAL                               
370100        PERFORM S15-KTR-GODK-PROJK-PV                                     
370200        IF PROJK-GODK                                                     
370300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-ATTR                  
370400        ELSE                                                              
370500           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJK-ATTR                  
370600           MOVE NEJ TO INPUT-RETT                                         
370700        END-IF                                                            
370800     ELSE                                                                 
370900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-ATTR                     
371000     END-IF                                                               
371100                                                                          
371200*    IF KDPRODSL-LOK-ART                                                  
371300*       IF WS-IDPROJK NOT = 'PROD'                                        
371400*           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJK-ATTR                   
371500*           MOVE NEJ TO INPUT-RETT                                        
371600*       END-IF                                                            
371700*    END-IF                                                               
371800                                                                          
371900     IF MID-KVARTVAGN = ALL '+'                                           
372000        MOVE MFS-RENSA-FAELT TO MOD-KVARTVAGN-IN                          
372100        MOVE ARTG01-ART-KVARTVAGN                TO WS-KVARTVAGN          
372200     ELSE                                                                 
372300        IF MID-KVARTVAGN NUMERIC                                          
372400           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVARTVAGN-ATTR                 
372500           MOVE MID-KVARTVAGN       TO WS-KVARTVAGN                       
372600        ELSE                                                              
372700           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVARTVAGN-ATTR              
372800           MOVE NEJ                    TO INPUT-RETT                      
372900        END-IF                                                            
373000     END-IF                                                               
373100                                                                          
373200     IF MID-IDARTNR-MOTSV = ALL '+'                                       
373300        MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-MOTSV-IN                 
373400        MOVE ZERO                 TO WS-IDARTNR-MOTSV                     
373500     ELSE                                                                 
373600        IF MID-IDARTNR-MOTSV NUMERIC                                      
373700           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-MOTSV-ATTR            
373800           MOVE MID-IDARTNR-MOTSV    TO WS-IDARTNR-MOTSV                  
373900        ELSE                                                              
374000           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-MOTSV-ATTR            
374100           MOVE NEJ                  TO INPUT-RETT                        
374200        END-IF                                                            
374300     END-IF                                                               
374400                                                                          
374500     IF MID-FLPISK = ALL '+'                                              
374600        MOVE MFS-RENSA-FAELT TO MOD-FLPISK-IN                             
374700     ELSE                                                                 
374800        IF MID-FLPISK = JA OR NEJ                                         
374900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPISK-ATTR                   
375000           MOVE MID-FLPISK TO WS-FLPISK                                   
375100        ELSE                                                              
375200           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLPISK-ATTR                     
375300           MOVE NEJ TO INPUT-RETT                                         
375400        END-IF                                                            
375500     END-IF                                                               
375600                                                                          
375700     IF MID-FLBYTES = ALL '+'                                             
375800        MOVE MFS-RENSA-FAELT TO MOD-FLBYTES-IN                            
375900     ELSE                                                                 
376000        IF MID-FLBYTES = JA OR NEJ                                        
376100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBYTES-ATTR                  
376200           MOVE MID-FLBYTES TO WS-FLBYTES                                 
376300        ELSE                                                              
376400           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBYTES-ATTR                    
376500           MOVE NEJ TO INPUT-RETT                                         
376600        END-IF                                                            
376700     END-IF                                                               
376800                                                                          
376900     IF MID-KVPROG = ALL '+'                                              
377000        MOVE MFS-RENSA-FAELT TO MOD-KVPROG-IN                             
377100     ELSE                                                                 
377200        IF MID-KVPROG NUMERIC                                             
377300           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPROG-ATTR                    
377400           MOVE MID-KVPROG TO WS-KVPROG                                   
377500        ELSE                                                              
377600           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROG-ATTR                      
377700           MOVE NEJ TO INPUT-RETT                                         
377800        END-IF                                                            
377900     END-IF                                                               
378000                                                                          
378100     .                                                                    
378200     EJECT                                                                
378300 S07-KOLLA-OM-BEN-FINNS SECTION.                                          
378400*****************************************************************         
378500*  ÄT NOV 92 VID IDSKYLT = GB  GODKÄNNES BARA NAMNLEX BENÄMNING *         
378600*             -  GÄLLER INTE VID KOPIERING AV BENÄMNING         *         
378700*****************************************************************         
378800     SKIP2                                                                
378900     MOVE NEJ TO SAMMA-BEN                                                
379000****  FÖR ATT FÅ SAMMA HOMONYMKOD                                         
379100     IF KOPIERING = JA                                                    
379200        MOVE IDARTNR-WS TO W-IDARTNR                                      
379300        PERFORM IMS-GU-BENA01-CSEQ                                        
379400        MOVE WS-IDSKYLT TO W-IDSKYLT                                      
379500        PERFORM IMS-GNP-BENA11-CSEQ                                       
379600        IF BENA-TEXT-BEART = WS-BEART                                     
379700           MOVE JA TO SAMMA-BEN                                           
379800        END-IF                                                            
379900     END-IF                                                               
380000                                                                          
380100     IF SAMMA-BEN = NEJ                                                   
380200        MOVE WS-IDSKYLT TO W-IDSKYLT                                      
380300        MOVE WS-BEART TO W-BEART                                          
380400        PERFORM IMS-GET-BENA01-ASEQ                                       
380500                                                                          
380600        IF WS-IDSKYLT = 'S  '                                             
380700           PERFORM UNTIL SEGMENT-SAKNAS                                   
380800                         OR BENA-BEN-KDHOMONYM = 0                        
380900              PERFORM IMS-GET-BENA01-ASEQ                                 
381000           END-PERFORM                                                    
381100                                                                          
381200           IF SEGMENT-FINNS                                               
381300********     OM BENÄMNING FINNS PÅ BENÄMNINGSREGISTRET                    
381400              IF WS-FLRSBEART = JA                                        
381500                 CONTINUE                                                 
381600              ELSE                                                        
381700                 PERFORM IMS-GET-BENA13-ASEQ                              
381800                                                                          
381900                 IF SEGMENT-FINNS                                         
382000**************     OM HOMONYMKOD FINNS                                    
382100                    MOVE BENA-HOM-TEHOMONYM TO WS-TEHOMONYM               
382200                    IF RS-BM-NAMN = 'BM-NAMN' OR 'RS-NAMN'                
382300                       CONTINUE                                           
382400                    ELSE                                                  
382500                       MOVE FEL-6 (SPIND) TO MOD-TEMFSFEL                 
382600                    END-IF                                                
382700                 ELSE                                                     
382800**************  OM HOMONYMKOD SAKNAS                                      
382900                    CONTINUE                                              
383000                 END-IF                                                   
383100              END-IF                                                      
383200           ELSE                                                           
383300********     OM BENÄMNING SAKNAS PÅ BENÄMNINGSREGISTRET                   
383400              IF WS-FLRSBEART = JA                                        
383500                 CONTINUE                                                 
383600              ELSE                                                        
383700                 MOVE FEL-5 (SPIND) TO MOD-TEMFSINF                       
383800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR                
383900                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRSBEART-ATTR            
384000                 MOVE NEJ TO INPUT-RETT                                   
384100              END-IF                                                      
384200           END-IF                                                         
384300**********  VID IDSKYLT = GB                                              
384400        ELSE                                                              
384500           PERFORM UNTIL SEGMENT-SAKNAS                                   
384600                         OR BENA-BEN-KDBENSTAT < 2                        
384700              PERFORM IMS-GET-BENA01-ASEQ                                 
384800           END-PERFORM                                                    
384900                                                                          
385000           IF SEGMENT-FINNS                                               
385100              CONTINUE                                                    
385200           ELSE                                                           
385300              MOVE FEL-5 (SPIND) TO MOD-TEMFSINF                          
385400              MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR                   
385500              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-ATTR                 
385600              MOVE NEJ TO INPUT-RETT                                      
385700           END-IF                                                         
385800        END-IF                                                            
385900     END-IF                                                               
386000     .                                                                    
386100     EJECT                                                                
386200 S08-KOLLA-INDATA-NYREG SECTION.                                          
386300     SKIP2                                                                
386400     MOVE JA TO INPUT-RETT                                                
386500     MOVE JA TO NYPON-ARTIKEL                                             
386600                                                                          
386700     MOVE ZERO TO WS-KDPRODSL                                             
386800     IF MID-KDPRODSL NUMERIC                                              
386900        MOVE MID-KDPRODSL   TO TEST-KDPRODSL                              
387000*                              WS-KDPRODSL-LOK-ART                        
387100        IF GOOD-KDPRODSL                                                  
387200           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-ATTR                  
387300           MOVE MID-KDPRODSL TO WS-KDPRODSL                               
387400           PERFORM S94-KOLLA-IDDC                                         
387500           IF INPUT-RETT = JA                                             
387600             PERFORM S92-CHECK-SUPPLIER                                   
387700           END-IF                                                         
387800        ELSE                                                              
387900           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                    
388000           MOVE FEL-ERR-KDPRODSL  TO WS-ERROR-UPDX                        
388100           MOVE NEJ TO INPUT-RETT                                         
388200        END-IF                                                            
388300     ELSE                                                                 
388400       MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                        
388500       MOVE NEJ TO INPUT-RETT                                             
388600       MOVE FEL-ERR-KDPRODSL  TO WS-ERROR-UPDX                            
388700     END-IF                                                               
388800                                                                          
388900     IF INPUT-RETT = JA                                                   
389000        PERFORM S95-KOLLA-IDFTG                                           
389100     END-IF                                                               
389200                                                                          
389300     IF NYPON-ARTIKEL = JA                                                
389400        PERFORM S03-NYA-NYPON-DATAELEMENT                                 
389500     END-IF                                                               
389600                                                                          
389700     PERFORM S01-NYA-GEMENSAMMA-DATAELEMENT                               
389800     PERFORM S02-NYA-ARTREG-DATAELEMENT                                   
389900     PERFORM S23-KOLLA-RASA                                               
390000     PERFORM S24-KOLLA-CROSS                                              
390100     .                                                                    
390200     EJECT                                                                
390300 S09-KOLLA-INDATA-KOP-ARTREG SECTION.                                     
390400     SKIP2                                                                
390500     MOVE JA TO INPUT-RETT                                                
390600     MOVE JA TO NYPON-ARTIKEL                                             
390700                                                                          
390800     PERFORM S20-KOLLA-NYPON-ARTIKEL-KOP                                  
390900                                                                          
391000     IF NYPON-ARTIKEL = JA                                                
391100        IF FINNS-REG-PA-NYPON = JA                                        
391200           PERFORM S06-KOP-NYPON-DATAELEMENT                              
391300        ELSE                                                              
391400           PERFORM S03-NYA-NYPON-DATAELEMENT                              
391500        END-IF                                                            
391600     END-IF                                                               
391700     PERFORM S04-KOP-GEMENSAMMA-DATAELEMENT                               
391800     PERFORM S05-KOP-ARTREG-DATAELEMENT                                   
391900     PERFORM S23-KOLLA-RASA                                               
392000     PERFORM S24-KOLLA-CROSS                                              
392100     .                                                                    
392200     EJECT                                                                
392300 S10-KOLLA-INDATA-KOP-ARTREG SECTION.                                     
392400     SKIP2                                                                
392500     MOVE JA TO INPUT-RETT                                                
392600     MOVE JA TO NYPON-ARTIKEL                                             
392700                                                                          
392800     PERFORM S20-KOLLA-NYPON-ARTIKEL-KOP                                  
392900                                                                          
393000     IF NYPON-ARTIKEL = JA                                                
393100        PERFORM S06-KOP-NYPON-DATAELEMENT                                 
393200     END-IF                                                               
393300                                                                          
393400     IF MID-IDPROJK = ALL '+'                                             
393500        MOVE WS-IDPROJK-GAM TO WS-IDPROJK                                 
393600     END-IF                                                               
393700     PERFORM S04-KOP-GEMENSAMMA-DATAELEMENT                               
393800     PERFORM S05-KOP-ARTREG-DATAELEMENT                                   
393900     PERFORM S23-KOLLA-RASA                                               
394000     PERFORM S24-KOLLA-CROSS                                              
394100     .                                                                    
394200     EJECT                                                                
394300 S12-REGISTRERA-NYPON SECTION.                                            
394400     SKIP2                                                                
394500     MOVE IDARTNR-WS      TO ARTG01-ART-IDARTNR                           
394600     MOVE WS-BEART-SVE    TO ARTG01-ART-BEART-SVE                         
394700     MOVE WS-FLBYTES      TO ARTG01-ART-FLBYTES                           
394800     MOVE WS-FLPISK       TO ARTG01-ART-FLPISK                            
394900     MOVE WS-KVPROG       TO ARTG01-ART-KVPROG                            
395000     MOVE WS-IDAO         TO ARTG01-ART-IDAO                              
395100     MOVE WS-IDBERED      TO ARTG01-ART-IDBERED                           
395200     MOVE WS-IDFKNGRP     TO ARTG01-ART-IDFKNGRP                          
395300                                                                          
395400     IF WS-IDPROENH(1)= ZERO                                              
395500        IF WS-IDPROENH(2) = ZERO                                          
395600           MOVE WS-IDPROENH(3) TO ARTG01-ART-IDPROENH                     
395700        ELSE                                                              
395800           MOVE WS-IDPROENH(2) TO ARTG01-ART-IDPROENH                     
395900        END-IF                                                            
396000     ELSE                                                                 
396100        MOVE WS-IDPROENH(1)    TO ARTG01-ART-IDPROENH                     
396200     END-IF                                                               
396300                                                                          
396400     IF WS-IDRITN = '='                                                   
396500*****  KOPIERA ARTIKELNR TILL RITNINGSNR                                  
396600*****  ("KAPA AV" INLEDANDE NOLLOR)                                       
396700                                                                          
396800       MOVE WS-IDARTNR TO WS-W009REDU-IN                                  
396900       INSPECT WS-W009REDU-IN REPLACING LEADING ZERO BY SPACE             
397000       CALL W009REDU USING WS-W009REDU-IN WS-W009REDU-UT                  
397100       MOVE WS-W009REDU-UT TO ARTG01-ART-IDRITN                           
397200     ELSE                                                                 
397300       MOVE WS-IDRITN     TO ARTG01-ART-IDRITN                            
397400     END-IF                                                               
397500                                                                          
397600     MOVE WS-IDPROJ       TO ARTG01-ART-IDPROJ                            
397700     MOVE WS-IDPROJK      TO ARTG01-ART-IDPROJK                           
397800     MOVE WS-KVARTVAGN    TO ARTG01-ART-KVARTVAGN                         
397900     MOVE WS-KDPRODSL     TO ARTG01-ART-KDPRODSL                          
398000     MOVE WS-KDSORT       TO ARTG01-ART-KDSORT                            
398100     MOVE WS-IDARTNR-MOTSV                                                
398200                          TO ARTG01-ART-IDARTNR-MOTSV                     
398300                                                                          
398400     IF WS-TIFINLV = 99999                                                
398500        MOVE 99999999     TO ARTG01-ART-DAFINLEV                          
398600     ELSE                                                                 
398700        MOVE 'AAVVD '     TO DAT-KDDATFORM                                
398800        MOVE WS-TIFINLV   TO DAT-I-TIDATUM                                
398900        PERFORM S99-WDATKONV                                              
399000        IF DAT-KDSVAR-OK                                                  
399100           MOVE DAT-TIAAMMDD TO ARTG01-ART-DAFINLEV                       
399200           MOVE DAT-TISEKEL  TO ARTG01-ART-DAFINLEV (1:2)                 
399300        END-IF                                                            
399400     END-IF                                                               
399500                                                                          
399600     IF MID-TEORSAK-1 = ALL '+'                                           
399700        MOVE SPACE          TO ARTG01-ART-TEORSAK                         
399800     ELSE                                                                 
399900        MOVE MID-TEORSAK-1 TO ARTG01-ART-TEORSAK                          
400000     END-IF                                                               
400100                                                                          
400200     MOVE ARTG01-ART-TEORSAK       TO MOD-TEORSAK-1                       
400300     MOVE ARTG01-ART-FLPISK        TO MOD-FLPISK-UT                       
400400     MOVE ARTG01-ART-FLBYTES       TO MOD-FLBYTES-UT                      
400500     MOVE ARTG01-ART-IDPROJK       TO MOD-IDPROJK-UT                      
400600     MOVE ARTG01-ART-KVARTVAGN     TO MOD-KVARTVAGN-UT                    
400700     MOVE ARTG01-ART-IDARTNR-MOTSV TO MOD-IDARTNR-MOTSV-UT                
400800     MOVE ARTG01-ART-DAFINLEV (3:6) TO MOD-TIFINLV-UT                     
400900     MOVE WS-BEART                 TO MOD-BEART-UT                        
401000     MOVE ARTG01-ART-KVPROG        TO MOD-KVPROG-UT                       
401100                                                                          
401200     PERFORM S21-NOLLSTAELL-NYPON                                         
401300     PERFORM IMS-ISRT-ARTG01                                              
401400     .                                                                    
401500     EJECT                                                                
401600 S13-VISA-BILD-IGEN SECTION.                                              
401700     SKIP2                                                                
401800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDBERED-UT                             
401900                               MOD-IDBERED-IN                             
402000                               MOD-KDPRODSL-UT                            
402100                               MOD-KDPRODSL-IN                            
402200                               MOD-KDSORT-UT                              
402300                               MOD-KDSORT-IN                              
402400                               MOD-IDPROENH-1-UT                          
402500                               MOD-IDPROENH-2-UT                          
402600                               MOD-IDPROENH-3-UT                          
402700                               MOD-IDPROENH-1-IN                          
402800                               MOD-IDPROENH-2-IN                          
402900                               MOD-IDPROENH-3-IN                          
403000                               MOD-KDYTBEH-UT                             
403100                               MOD-KDYTBEH-IN                             
403200                               MOD-IDPROJ-UT                              
403300                               MOD-IDPROJ-IN                              
403400                               MOD-KDFARLIG-UT                            
403500                               MOD-KDFARLIG-IN                            
403600                               MOD-KDBPSR-UT                              
403700                               MOD-KDBPSR-IN                              
403800                               MOD-IDKAT-1-UT                             
403900                               MOD-IDKAT-2-UT                             
404000                               MOD-IDKAT-3-UT                             
404100                               MOD-IDKAT-1-IN                             
404200                               MOD-IDKAT-2-IN                             
404300                               MOD-IDKAT-3-IN                             
404400                               MOD-KDUART-UT                              
404500                               MOD-KDUART-IN                              
404600                               MOD-KVARTVAGN-UT                           
404700                               MOD-KVARTVAGN-IN                           
404800                               MOD-IDPROJK-UT                             
404900                               MOD-IDPROJK-IN                             
405000                               MOD-FLPISK-UT                              
405100                               MOD-FLPISK-IN                              
405200                               MOD-IDAO-UT                                
405300                               MOD-IDAO-IN                                
405400                               MOD-TIFINLV-UT                             
405500                               MOD-TISOP-UT                               
405600                               MOD-TISOP-IN                               
405700                               MOD-FLLSRDEL-UT                            
405800                               MOD-FLLSRDEL-IN                            
405900                               MOD-IDPROJUP-UT                            
406000                               MOD-IDPROJUP-IN                            
406100                               MOD-BEART-UT                               
406200                               MOD-BEART-IN                               
406300                               MOD-IDSKYLT-IN                             
406400                               MOD-FLRSBEART-IN                           
406500                               MOD-IDFKNGRP-UT                            
406600                               MOD-IDLEVNR-UT                             
406700                               MOD-BELEV-UT                               
406800                               MOD-IDFKNGRP-IN                            
406900                               MOD-IDLEVNR-IN                             
407000                               MOD-BELEV-IN                               
407100                               MOD-TEORSAK-1                              
407200                               MOD-TEARTNOT-2                             
407300                               MOD-TEARTNOT-4                             
407400                               MOD-TEARTNOT-7                             
407500                               MOD-IDRITN-UT                              
407600                               MOD-IDRITN-IN                              
407700                               MOD-FLBYTES-UT                             
407800                               MOD-FLBYTES-IN                             
407900                               MOD-FLGAMART-IN                            
408000                               MOD-IDARTNR-MOTSV-UT                       
408100                               MOD-IDARTNR-MOTSV-IN                       
408200                               MOD-KVPROG-IN                              
408300                               MOD-KVPROG-UT                              
408400*DOLDA-FÄLT                                                               
408500                               MOD-IDBERED-LAEST                          
408600                               MOD-IDAO-LAEST                             
408700                               MOD-IDAO-VALD                              
408800                               MOD-IDPROJ-VALD                            
408900     .                                                                    
409000     EJECT                                                                
409100 S14-GODK-PROJ-MFS-RAETT-FEL SECTION.                                     
409200                                                                          
409300******************************************************************        
409400*     PROJEKT ÄR OBLIGATORISKT FÖR ALLA ARTIKLAR MED                      
409500*     PRODUKTSLAG PV-BASL OCH PRODUKTSLAG CARPAC.                         
409600*     DESSA ARTIKLAR MÅSTE HA GODK-PROJ, UPPLAGD PÅ BILD 1153.            
409700*     ARTKLAR MED PRODSL. 19 31 34 35 38  INGEN KONTROLL.                 
409800*     ÖVRIGA ARTIKLAR FÅR EJ HA PROJ BLANK.                               
409900******************************************************************        
410000                                                                          
410100     MOVE WS-KDPRODSL TO W-KDPRODSL                                       
410200                         TEST-KDPRODSL                                    
410300     MOVE NEJ TO SW-PROJ-GODK                                             
410400                                                                          
410500     IF KDPRODSL-VOLVO-UTAN-EMB                                           
410600     OR KDPRODSL-LOCAL                                                    
410700                                                                          
410800        PERFORM IMS-GU-XXAQ01                                             
410900        IF SEGMENT-FINNS                                                  
411000           PERFORM IMS-GNP-XXAQ11                                         
411100           PERFORM UNTIL (PROJ-GODK) OR (SEGMENT-SAKNAS)                  
411200             IF XXAQ-1132-IDPROJ = WS-IDPROJ                              
411300                MOVE JA TO SW-PROJ-GODK                                   
411400             ELSE                                                         
411500                PERFORM IMS-GNP-XXAQ11                                    
411600             END-IF                                                       
411700           END-PERFORM                                                    
411800        END-IF                                                            
411900        IF PROJ-GODK                                                      
412000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-ATTR                   
412100        ELSE                                                              
412200           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJ-ATTR                   
412300           MOVE NEJ TO INPUT-RETT                                         
412400        END-IF                                                            
412500     ELSE                                                                 
412600        IF KDPRODSL-EMB                                                   
412700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-ATTR                   
412800        ELSE                                                              
412900* * * * *  ÖVRIGA PRODUKTSLAG * * * * * * * * * * * * * * * *             
413000           IF WS-IDPROJ = SPACE                                           
413100              MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJ-ATTR                
413200              MOVE NEJ TO INPUT-RETT                                      
413300           ELSE                                                           
413400              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-ATTR                
413500           END-IF                                                         
413600        END-IF                                                            
413700     END-IF                                                               
413800     .                                                                    
413900     EJECT                                                                
414000 S15-KTR-GODK-PROJK-PV SECTION.                                           
414100                                                                          
414200     MOVE NEJ TO SW-PROJK-GODK                                            
414300     MOVE WS-KDPRODSL TO W-KDPRODSL                                       
414400                                                                          
414500     PERFORM IMS-GU-XXAQ01                                                
414600     IF SEGMENT-FINNS                                                     
414700        PERFORM IMS-GNP-XXAQ11                                            
414800        PERFORM UNTIL (PROJK-GODK) OR (SEGMENT-SAKNAS)                    
414900          IF XXAQ-1132-IDPROJK = WS-IDPROJK                               
415000             MOVE JA TO SW-PROJK-GODK                                     
415100          ELSE                                                            
415200             PERFORM IMS-GNP-XXAQ11                                       
415300          END-IF                                                          
415400        END-PERFORM                                                       
415500     END-IF                                                               
415600     .                                                                    
415700     EJECT                                                                
415800 S19-KOP-FRAN-ARTREG-TILL-NYPON SECTION.                                  
415900     SKIP2                                                                
416000     MOVE IDARTNR-WS TO W-IDARTNR                                         
416100     PERFORM IMS-GET-ARTC01                                               
416200                                                                          
416300     IF MID-IDFKNGRP = ALL '+'                                            
416400        MOVE ART-IDFKNGRP            TO ARTG01-ART-IDFKNGRP               
416500     ELSE                                                                 
416600        MOVE WS-IDFKNGRP             TO ARTG01-ART-IDFKNGRP               
416700     END-IF                                                               
416800     IF MID-KDPRODSL = ALL '+'                                            
416900        MOVE ART-KDPRODSL            TO ARTG01-ART-KDPRODSL               
417000     ELSE                                                                 
417100        MOVE WS-KDPRODSL             TO ARTG01-ART-KDPRODSL               
417200     END-IF                                                               
417300     IF MID-KDSORT = ALL '+'                                              
417400        MOVE ART-KDSORT              TO ARTG01-ART-KDSORT                 
417500     ELSE                                                                 
417600        MOVE WS-KDSORT               TO ARTG01-ART-KDSORT                 
417700     END-IF                                                               
417800                                                                          
417900     IF MID-IDAO = ALL '+'                                                
418000        MOVE ART-IDAO(1)             TO ARTG01-ART-IDAO                   
418100     ELSE                                                                 
418200        MOVE WS-IDAO                 TO ARTG01-ART-IDAO                   
418300     END-IF                                                               
418400                                                                          
418500     IF MID-TISOP NOT = ALL '+'                                           
418600        PERFORM S25-TIFINLV-FRAN-SOP                                      
418700        MOVE SPAR-TIFINLV-AAVVD TO XX-TIFINLV                             
418800     END-IF                                                               
418900     IF XX-TIFINLV = ALL '+'                                              
419000        IF ART-TIFINLV = 99999                                            
419100           MOVE 99999999            TO ARTG01-ART-DAFINLEV                
419200        ELSE                                                              
419300           MOVE 'AAVVD ' TO DAT-KDDATFORM                                 
419400           MOVE ART-TIFINLV TO DAT-I-TIDATUM                              
419500           PERFORM S99-WDATKONV                                           
419600           IF DAT-KDSVAR-OK                                               
419700              MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV                  
419800              MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)            
419900           END-IF                                                         
420000        END-IF                                                            
420100     ELSE                                                                 
420200        IF WS-TIFINLV = 99999                                             
420300           MOVE 99999999            TO ARTG01-ART-DAFINLEV                
420400        ELSE                                                              
420500           MOVE 'AAVVD '            TO DAT-KDDATFORM                      
420600           MOVE WS-TIFINLV          TO DAT-I-TIDATUM                      
420700           PERFORM S99-WDATKONV                                           
420800           IF DAT-KDSVAR-OK                                               
420900              MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV                  
421000              MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)            
421100           END-IF                                                         
421200        END-IF                                                            
421300     END-IF                                                               
421400                                                                          
421500     PERFORM IMS-GET-ARTC11                                               
421600     IF MID-IDBERED = ALL '+'                                             
421700        MOVE CLAG-IDBERED            TO ARTG01-ART-IDBERED                
421800     ELSE                                                                 
421900        MOVE WS-IDBERED              TO ARTG01-ART-IDBERED                
422000     END-IF                                                               
422100     IF MID-IDPROJ = ALL '+'                                              
422200        MOVE CLAG-IDPROJ             TO ARTG01-ART-IDPROJ                 
422300     ELSE                                                                 
422400        MOVE WS-IDPROJ               TO ARTG01-ART-IDPROJ                 
422500     END-IF                                                               
422600                                                                          
422700     IF MID-IDRITN = ALL '+'                                              
422800        MOVE CLAG-IDRITN             TO ARTG01-ART-IDRITN                 
422900     ELSE                                                                 
423000       IF WS-IDRITN = '='                                                 
423100*****    KOPIERA ARTIKELNR TILL RITNINGSNR                                
423200*****    ("KAPA AV" INLEDANDE NOLLOR)                                     
423300                                                                          
423400         MOVE WS-NY-IDARTNR TO WS-W009REDU-IN                             
423500         INSPECT WS-W009REDU-IN REPLACING LEADING ZERO BY SPACE           
423600         CALL W009REDU USING WS-W009REDU-IN WS-W009REDU-UT                
423700         MOVE WS-W009REDU-UT         TO ARTG01-ART-IDRITN                 
423800       ELSE                                                               
423900         MOVE WS-IDRITN              TO ARTG01-ART-IDRITN                 
424000       END-IF                                                             
424100     END-IF                                                               
424200                                                                          
424300     IF INMATAD-IDPROENH = ZERO                                           
424400        MOVE CLAG-IDPROENH(1) TO WS-IDPROENH(1)                           
424500        MOVE CLAG-IDPROENH(2) TO WS-IDPROENH(2)                           
424600        MOVE CLAG-IDPROENH(3) TO WS-IDPROENH(3)                           
424700     END-IF                                                               
424800                                                                          
424900     IF WS-IDPROENH(1)= ZERO                                              
425000        IF WS-IDPROENH(2) = ZERO                                          
425100           MOVE WS-IDPROENH(3) TO ARTG01-ART-IDPROENH                     
425200        ELSE                                                              
425300           MOVE WS-IDPROENH(2) TO ARTG01-ART-IDPROENH                     
425400        END-IF                                                            
425500     ELSE                                                                 
425600        MOVE WS-IDPROENH(1)    TO ARTG01-ART-IDPROENH                     
425700     END-IF                                                               
425800                                                                          
425900     .                                                                    
426000     EJECT                                                                
426100 S20-KOLLA-NYPON-ARTIKEL-KOP SECTION.                                     
426200     SKIP2                                                                
426300     IF MID-TISOP   =   ALL '+'                                           
426400        MOVE ART-TISOP             TO MID-TISOP                           
426500     END-IF                                                               
426600     IF MID-IDAO      =   ALL '+'                                         
426700        MOVE ART-IDAO (1)        TO MID-IDAO                              
426800     END-IF                                                               
426900                                                                          
427000     IF MID-KDBPSR   = ALL '+'                                            
427100        MOVE IDARTNR-WS TO W-IDARTNR                                      
427200        PERFORM IMS-GET-ARTC01                                            
427300        PERFORM IMS-GET-ARTC11                                            
427400        MOVE CLAG-KDBPSR TO MID-KDBPSR                                    
427500     END-IF                                                               
427600                                                                          
427700     MOVE ZERO TO WS-KDPRODSL                                             
427800     IF MID-KDPRODSL = ALL '+'                                            
427900        MOVE MFS-RENSA-FAELT TO MOD-KDPRODSL-IN                           
428000        MOVE IDARTNR-WS TO W-IDARTNR                                      
428100        PERFORM IMS-GET-ARTC01                                            
428200        MOVE ART-KDPRODSL TO WS-KDPRODSL                                  
428300        PERFORM S94-KOLLA-IDDC                                            
428400        IF INPUT-RETT = JA                                                
428500          PERFORM S92-CHECK-SUPPLIER                                      
428600        END-IF                                                            
428700     ELSE                                                                 
428800        IF MID-KDPRODSL NUMERIC                                           
428900           MOVE MID-KDPRODSL  TO TEST-KDPRODSL                            
429000*                                WS-KDPRODSL-LOK-ART                      
429100           IF GOOD-KDPRODSL                                               
429200              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-ATTR               
429300              MOVE MID-KDPRODSL TO WS-KDPRODSL                            
429400              PERFORM S94-KOLLA-IDDC                                      
429500              IF INPUT-RETT = JA                                          
429600                PERFORM S92-CHECK-SUPPLIER                                
429700              END-IF                                                      
429800           ELSE                                                           
429900              MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                 
430000              MOVE NEJ TO INPUT-RETT                                      
430100           END-IF                                                         
430200        ELSE                                                              
430300           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                    
430400           MOVE NEJ TO INPUT-RETT                                         
430500        END-IF                                                            
430600     END-IF                                                               
430700                                                                          
430800     IF INPUT-RETT = JA                                                   
430900        PERFORM S95-KOLLA-IDFTG                                           
431000     END-IF                                                               
431100                                                                          
431200     IF MID-IDPROJ = ALL '+'                                              
431300        MOVE IDARTNR-WS TO W-IDARTNR                                      
431400        PERFORM IMS-GET-ARTC01                                            
431500        PERFORM IMS-GET-ARTC11                                            
431600        MOVE CLAG-IDPROJ       TO MID-IDPROJ WS-IDPROJ                    
431700     ELSE                                                                 
431800        MOVE MID-IDPROJ        TO WS-IDPROJ                               
431900     END-IF                                                               
432000                                                                          
432100     PERFORM S14-GODK-PROJ-MFS-RAETT-FEL                                  
432200                                                                          
432300*    IF KDPRODSL-LOK-ART                                                  
432400*       IF WS-IDPROJ NOT = 'PROD'                                         
432500*          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJ-ATTR                     
432600*          MOVE NEJ TO INPUT-RETT                                         
432700*       END-IF                                                            
432800*    END-IF                                                               
432900                                                                          
433000     IF MID-IDRITN = ALL '+'                                              
433100        MOVE IDARTNR-WS TO W-IDARTNR                                      
433200        PERFORM IMS-GET-ARTC01                                            
433300        PERFORM IMS-GET-ARTC11                                            
433400        IF CLAG-IDRITN = SPACE                                            
433500           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                     
433600           MOVE NEJ TO INPUT-RETT                                         
433700        ELSE                                                              
433800           MOVE CLAG-IDRITN        TO MID-IDRITN WS-IDRITN                
433900        END-IF                                                            
434000     ELSE                                                                 
434100        IF MID-IDRITN = SPACE                                             
434200           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                     
434300           MOVE NEJ TO INPUT-RETT                                         
434400        ELSE                                                              
434500           MOVE MID-IDRITN            TO WS-IDRITN                        
434600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDRITN-ATTR                   
434700        END-IF                                                            
434800     END-IF                                                               
434900                                                                          
435000     MOVE IDARTNR-NY-WS TO W-IDARTNR                                      
435100     PERFORM IMS-GET-ARTG01                                               
435200     IF SEGMENT-FINNS                                                     
435300        MOVE JA TO FINNS-REG-PA-NYPON                                     
435400        IF NYPON-ARTIKEL = NEJ                                            
435500           MOVE FEL-10 (SPIND) TO MOD-TEMFSINF                            
435600           MOVE NEJ TO INPUT-RETT                                         
435700        END-IF                                                            
435800     END-IF                                                               
435900     .                                                                    
436000     EJECT                                                                
436100 S21-NOLLSTAELL-NYPON SECTION.                                            
436200     SKIP2                                                                
436300     MOVE SPACE                      TO                                   
436400                                        ARTG01-ART-FLAENDR                
436500                                        ARTG01-ART-FLBASL                 
436600                                        ARTG01-ART-FLBERQ                 
436700                                        ARTG01-ART-FLRITB                 
436800                                        ARTG01-ART-FLRITC                 
436900                                        ARTG01-ART-FLRITP                 
437000                                        ARTG01-ART-KDARTUTG               
437100                                        ARTG01-ART-FLUPG                  
437200                                        ARTG01-ART-KDTPD                  
437300                                        ARTG01-ART-FLUPB                  
437400                                        ARTG01-ART-FLPLAKOP               
437500                                        ARTG01-ART-FLUNIKRD               
437600                                        ARTG01-ART-IDMATKTO               
437700                                        ARTG01-ART-IDPROJOBJ              
437800                                        ARTG01-ART-IDSTEKN                
437900                                        ARTG01-ART-IDRITUTG               
438000                                        ARTG01-ART-KDANSKQ                
438100                                        ARTG01-ART-KDARTTYP               
438200                                        ARTG01-ART-KDRESBED               
438300                                        ARTG01-ART-TETEKNIK               
438400                                        ARTG01-ART-TEANSINK               
438500                                        ARTG01-ART-TEARTNOT-BASL          
438600                                        ARTG01-ART-TEARTNOT               
438700                                        ARTG01-ART-KDKOPTYP               
438800                                        ARTG01-ART-IDLEVNR                
438900                                        ARTG01-ART-IDLEVNR-FORB(1)        
439000                                        ARTG01-ART-IDLEVNR-FORB(2)        
439100                                        ARTG01-ART-IDLEVNR-FORB(3)        
439200                                        ARTG01-ART-IDLEVNR-FORB(4)        
439300                                        ARTG01-ART-IDLEVNR-FORB(5)        
439400                                        ARTG01-ART-IDINK                  
439500     MOVE ZERO                       TO ARTG01-ART-IDANSK                 
439600                                        ARTG01-ART-IDINKTEK               
439700                                        ARTG01-ART-IDAVD                  
439800                                        ARTG01-ART-IDANSK-REG             
439900                                        ARTG01-ART-KDSTAINK               
440000                                        ARTG01-ART-KVARTAR1               
440100                                        ARTG01-ART-KVARTAR2               
440200                                        ARTG01-ART-KVARTAR3               
440300                                        ARTG01-ART-KVBASL                 
440400                                        ARTG01-ART-KVLEVBEG               
440500                                        ARTG01-ART-KVUPB                  
440600                                        ARTG01-ART-PRARTBES               
440700                                        ARTG01-ART-DABASL                 
440800                                        ARTG01-ART-TIINKOP                
440900                                        ARTG01-ART-TILEVBEG               
441000                                        ARTG01-ART-TINEDBRY               
441100                                        ARTG01-ART-TIPLAKOP               
441200                                        ARTG01-ART-TIREGDAT               
441300                                        ARTG01-ART-TIANSKREG              
441400                                        ARTG01-ART-TIRITB                 
441500                                        ARTG01-ART-TIRITC                 
441600                                        ARTG01-ART-TIRITP                 
441700                                        ARTG01-ART-TISERLEV(1)            
441800                                        ARTG01-ART-TISERLEV(2)            
441900                                        ARTG01-ART-TISERLEV(3)            
442000                                        ARTG01-ART-TISERLEV(4)            
442100                                        ARTG01-ART-TISERLEV(5)            
442200                                        ARTG01-ART-TISLUBER               
442300                                        ARTG01-ART-TISTABER               
442400                                        ARTG01-ART-TISTOMREG              
442500                                        ARTG01-ART-TIUPPDAT               
442600                                        ARTG01-ART-TIUPB                  
442700                                        ARTG01-ART-TIUPG                  
442800                                        ARTG01-ART-TITPD                  
442900                                        ARTG01-ART-TIMOTSI                
443000     .                                                                    
443100     EJECT                                                                
443200 S22-FIXA-DOLDA-FAELT   SECTION.                                          
443300     SKIP2                                                                
443400     IF ARTG01-ART-IDAO = MID-IDAO-VALD                                   
443500        MOVE ARTG01-ART-IDAO               TO MOD-IDAO-VALD               
443600     ELSE                                                                 
443700        MOVE SPACE                         TO MOD-IDAO-VALD               
443800     END-IF                                                               
443900     IF ARTG01-ART-IDPROJ = MID-IDPROJ-VALD                               
444000        MOVE ARTG01-ART-IDPROJ             TO MOD-IDPROJ-VALD             
444100     ELSE                                                                 
444200        MOVE SPACE                         TO MOD-IDPROJ-VALD             
444300     END-IF                                                               
444400     MOVE ARTG01-ART-IDBERED               TO WS-IDBERED-2-NUM            
444500     MOVE WS-IDBERED-2-NUM                 TO MOD-IDBERED-LAEST           
444600     MOVE ARTG01-ART-IDAO                  TO MOD-IDAO-LAEST              
444700     .                                                                    
444800     EJECT                                                                
444900 S23-KOLLA-RASA SECTION.                                                  
445000     SKIP2                                                                
445100     IF KOPIERING = NEJ                                                   
445200        MOVE IDARTNR-WS TO W-IDARTNR                                      
445300     ELSE                                                                 
445400        MOVE IDARTNR-NY-WS TO W-IDARTNR                                   
445500     END-IF                                                               
445600     PERFORM IMS-GET-SATB01                                               
445700     IF SEGMENT-FINNS                                                     
445800        IF WS-KDSORT = 'SA' OR 'TM'                                       
445900           IF SATB-STR-IDSTRTYP = 'S'                                     
446000              CONTINUE                                                    
446100           ELSE                                                           
446200              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                  
446300              MOVE NEJ TO INPUT-RETT                                      
446400              MOVE FEL-ERR-KDSORT     TO WS-ERROR-UPDX                    
446500           END-IF                                                         
446600        ELSE                                                              
446700           IF WS-KDSORT = 'ST'                                            
446800              IF SATB-STR-IDSTRTYP = 'R' OR 'K'                           
446900                 CONTINUE                                                 
447000              ELSE                                                        
447100                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR               
447200                 MOVE NEJ TO INPUT-RETT                                   
447300                 MOVE FEL-ERR-KDSORT TO WS-ERROR-UPDX                     
447400              END-IF                                                      
447500           ELSE                                                           
447600              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                  
447700              MOVE NEJ TO INPUT-RETT                                      
447800              MOVE FEL-ERR-KDSORT TO WS-ERROR-UPDX                        
447900           END-IF                                                         
448000        END-IF                                                            
448100      ELSE                                                                
448200       IF VAECKNING = JA                                                  
448300         CONTINUE                                                         
448400       ELSE                                                               
448500         IF KOPIERING = NEJ                                               
448600           MOVE IDARTNR-WS TO W-IDARTNR-S                                 
448700         ELSE                                                             
448800           MOVE IDARTNR-NY-WS TO W-IDARTNR-S                              
448900         END-IF                                                           
449000         MOVE SPACE TO W-IDLEVNR-S                                        
449100                       W-BELEVART-S                                       
449200         PERFORM IMS-GET-SATB11-CSEQ                                      
449300         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                       
449400            IF WS-KDSORT = SATB-RAD-KDSORT                                
449500               CONTINUE                                                   
449600            ELSE                                                          
449700             MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                      
449800             MOVE DAGENS-AAMMDD       TO TMP2-YYMMDD                      
449900             PERFORM WY2000P1                                             
450000             IF TMP1-YYMMDD >= TMP2-YYMMDD                                
450100               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                 
450200               MOVE NEJ TO INPUT-RETT                                     
450300               MOVE FEL-ERR-KDSORT     TO WS-ERROR-UPDX                   
450400             END-IF                                                       
450500            END-IF                                                        
450600         PERFORM IMS-GET-SATB11-CSEQ                                      
450700         END-PERFORM                                                      
450800      END-IF                                                              
450900     END-IF                                                               
451000     .                                                                    
451100     EJECT                                                                
451200 S24-KOLLA-CROSS SECTION.                                                 
451300                                                                          
451400     IF INPUT-RETT = JA                                                   
451500        IF MID-IDLEVNR = ALL '+'                                          
451600        AND MID-BELEV = ALL '+'                                           
451700           IF KDPRODSL-LYNK                                               
451800              MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR                   
451900                                         MOD-IDLEVNR-ATTR                 
452000              MOVE NEJ TO INPUT-RETT                                      
452100              MOVE FEL-15 (SPIND) TO MOD-TEMFSFEL                         
452200              MOVE FEL-ERR-KDPRODSL TO WS-ERROR-UPDX                      
452300           ELSE                                                           
452400              CONTINUE                                                    
452500           END-IF                                                         
452600        ELSE                                                              
452700           IF KOPIERING = NEJ                                             
452800              MOVE IDARTNR-WS TO W-IDARTNR                                
452900           ELSE                                                           
453000              MOVE IDARTNR-NY-WS TO W-IDARTNR                             
453100           END-IF                                                         
453200           PERFORM IMS-GU-WDF501                                          
453300           IF SEGMENT-FINNS                                               
453400              MOVE MED-5(SPIND) TO MOD-TEMFSFEL                           
453500           END-IF                                                         
453600                                                                          
453700           MOVE MID-IDLEVNR     TO W-SEQA-IDLEVNR-MIN                     
453800                                   W-SEQA-IDLEVNR-MAX                     
453900                                                                          
454000           MOVE MID-BELEV       TO WS-W009REDU-IN                         
454100           CALL W009REDU USING WS-W009REDU-IN WS-W009REDU-UT              
454200           MOVE WS-W009REDU-UT  TO W-SEQA-IDLEVART-MIN                    
454300                                   W-SEQA-IDLEVART-MAX                    
454400           PERFORM IMS-GN-WDF5A1                                          
454500           IF SEGMENT-FINNS                                               
454600              MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR                   
454700                                         MOD-IDLEVNR-ATTR                 
454800              MOVE NEJ TO INPUT-RETT                                      
454900              MOVE FEL-16 (SPIND) TO MOD-TEMFSFEL                         
455000              MOVE FEL-ERR-KDPRODSL TO WS-ERROR-UPDX                      
455100           END-IF                                                         
455200        END-IF                                                            
455300     END-IF                                                               
455400     .                                                                    
455500     EJECT                                                                
455600 S25-TIFINLV-FRAN-SOP SECTION.                                            
455700                                                                          
455800*  OM TIFINLV INTE ANGES SKALL DET SÄTTAS = TISOP                         
455900*  MEN OM TISOP INTEÄR STÖRRE ÄN INNEVARANDE VECKA                        
456000*  SKALL TISOP SÄTTAS TILL DAGENS VECKA + 1                               
456100*-- TIFINLV BORTTAGET UR MID JAN. 2015                                    
456200                                                                          
456300     MOVE MID-TISOP            TO SPAR-TIFINLV-AAVV                       
456400     MOVE SPAR-TIFINLV-AAVV-R  TO TMP1-YYWW                               
456500     MOVE SPAR-DAGENS-AAVV-R   TO TMP2-YYWW                               
456600     PERFORM WY2000P3                                                     
456700     IF TMP1-YYWW > TMP2-YYWW                                             
456800        MOVE MID-TISOP         TO SPAR-TIFINLV-AAVVD                      
456900     ELSE                                                                 
457000        MOVE SPAR-DAGENS-AAVV-R(1:2)   TO WS-YY                           
457100        MOVE SPAR-DAGENS-AAVV-R(3:2)   TO WS-WW                           
457200        IF WS-WW < 52                                                     
457300            ADD      1 TO WS-WW                                           
457400        ELSE                                                              
457500            ADD      1 TO WS-YY                                           
457600            MOVE 01 TO WS-WW                                              
457700        END-IF                                                            
457800        MOVE         1 TO WS-D                                            
457900        MOVE WS-YYWWD          TO SPAR-TIFINLV-AAVVD                      
458000     END-IF                                                               
458100     .                                                                    
458200     EJECT                                                                
458300                                                                          
458400 S92-CHECK-SUPPLIER SECTION.                                              
458500     MOVE JA                  TO SW-SUPPLIER-OK                           
458600                                                                          
458700     MOVE 1                   TO LEV05-IX                                 
458800     PERFORM UNTIL LEV05-IX > 2                                           
458900       IF MSGI-KDARBTYP-SEC-IDLEV = TAB-KDARBTYP-LEV (LEV05-IX)           
459000         MOVE NEJ             TO SW-SUPPLIER-OK                           
459100         IF WS-KDPRODSL = TAB-KDPRODSL (LEV05-IX)                         
459200           MOVE JA            TO SW-SUPPLIER-OK                           
459300           MOVE 2             TO LEV05-IX                                 
459400         END-IF                                                           
459500       END-IF                                                             
459600       ADD 1                  TO LEV05-IX                                 
459700     END-PERFORM                                                          
459800                                                                          
459900     IF NOT SUPPLIER-OK                                                   
460000       MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                        
460100       MOVE NEJ               TO INPUT-RETT                               
460200       MOVE FEL-13(SPIND)     TO MOD-TEMFSINF                             
460300       MOVE FEL-ERR-FIELD     TO WS-ERROR-UPDX                            
460400     END-IF                                                               
460500     .                                                                    
460600     EJECT                                                                
460700                                                                          
460800 S93-KOLLA-SOFTWARE SECTION.                                              
460900     IF WS-KDSORT = 'SW'                                                  
461000        IF WS-SISTA-SIFFRAN = 8                                           
461100           CONTINUE                                                       
461200        ELSE                                                              
461300           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                     
461400           MOVE NEJ TO INPUT-RETT                                         
461500           MOVE FEL-ERR-KDSORT     TO WS-ERROR-UPDX                       
461600        END-IF                                                            
461700     END-IF                                                               
461800                                                                          
461900     IF WS-SISTA-SIFFRAN = 8                                              
462000        IF WS-KDSORT = 'SW'                                               
462100           CONTINUE                                                       
462200        ELSE                                                              
462300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                    
462400           MOVE NEJ TO INPUT-RETT                                         
462500           MOVE FEL-ERR-KDSORT     TO WS-ERROR-UPDX                       
462600        END-IF                                                            
462700     END-IF                                                               
462800     .                                                                    
462900     EJECT                                                                
463000                                                                          
463100 S94-KOLLA-IDDC SECTION.                                                  
463200     MOVE WS-KDPRODSL            TO TEST-KDPRODSL                         
463300     IF KDPRODSL-VOLVO-BIMA                                               
463400       IF CDC OR SDC                                                      
463500          CONTINUE                                                        
463600       ELSE                                                               
463700         IF MFS-UPD-X                                                     
463800           CONTINUE                                                       
463900         ELSE                                                             
464000           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                    
464100           MOVE NEJ TO INPUT-RETT                                         
464200           MOVE FEL-13(SPIND) TO MOD-TEMFSINF                             
464300           MOVE FEL-ERR-IDDC  TO WS-ERROR-UPDX                            
464400         END-IF                                                           
464500       END-IF                                                             
464600     END-IF                                                               
464700     .                                                                    
464800     EJECT                                                                
464900                                                                          
465000 S95-KOLLA-IDFTG SECTION.                                                 
465100     SKIP2                                                                
465200     MOVE '002'       TO KPS-KDCALL                                       
465300     MOVE WS-KDPRODSL TO KPS-KDPRODSL                                     
465400     CALL WKPSKONV USING KPS-WKPSAREA                                     
465500     IF KPS-KDSVAR = SPACE                                                
465600        CONTINUE                                                          
465700     ELSE                                                                 
465800        MOVE NEJ TO INPUT-RETT                                            
465900        MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                       
466000        MOVE FEL-ERR-IDFTG     TO WS-ERROR-UPDX                           
466100     END-IF                                                               
466200     .                                                                    
466300     EJECT                                                                
466400 S96-LAES-XXAQ-KOP SECTION.                                               
466500     SKIP2                                                                
466600     MOVE WS-IDPROJ                  TO W-IDPROJ                          
466700     MOVE WS-IDPROJK                 TO W-IDPROJK                         
466800     MOVE SPACE                            TO W-IDPROJOBJ                 
466900     MOVE WS-KDPRODSL                      TO W-KDPRODSL                  
467000     PERFORM IMS-GU-XXAQ11-UNIK                                           
467100     .                                                                    
467200     EJECT                                                                
467300 S97-LAES-XXAQ SECTION.                                                   
467400     SKIP2                                                                
467500     IF MFS-UPDATE                                                        
467600        IF MID-IDPROJK = ALL '+'                                          
467700           MOVE ARTG01-ART-IDPROJK         TO W-IDPROJK                   
467800        ELSE                                                              
467900           MOVE MID-IDPROJK                TO W-IDPROJK                   
468000        END-IF                                                            
468100        IF MID-IDPROJ = ALL '+'                                           
468200           MOVE ARTG01-ART-IDPROJ          TO W-IDPROJ                    
468300        ELSE                                                              
468400           MOVE MID-IDPROJ                 TO W-IDPROJ                    
468500        END-IF                                                            
468600     ELSE                                                                 
468700        MOVE ARTG01-ART-IDPROJK            TO W-IDPROJK                   
468800        MOVE ARTG01-ART-IDPROJ             TO W-IDPROJ                    
468900     END-IF                                                               
469000                                                                          
469100     MOVE SPACE                            TO W-IDPROJOBJ                 
469200     MOVE WS-KDPRODSL                      TO W-KDPRODSL                  
469300     PERFORM IMS-GU-XXAQ11-UNIK                                           
469400     .                                                                    
469500     EJECT                                                                
469600 S98-OM-TVA-AAR SECTION.                                                  
469700     SKIP2                                                                
469800     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
469900     PERFORM S99-WDATKONV                                                 
470000     MOVE DAT-TIAAVVD TO AAVVD                                            
470100                                                                          
470200     MOVE FUNCTION CURRENT-DATE (1:4) TO W-AAR4                           
470300     ADD 2 TO W-AAR4                                                      
470400     MOVE W-AAR4 (3:2) TO AA                                              
470500                                                                          
470600     IF VV = 53                                                           
470700       MOVE 52 TO VV                                                      
470800     END-IF                                                               
470900     MOVE 1 TO D                                                          
471000     .                                                                    
471100     EJECT                                                                
471200 S99-WDATKONV SECTION.                                                    
471300     SKIP2                                                                
471400     CALL WDATKONV USING DAT-KDDATFORM                                    
471500                         DAT-I-TIDATUM                                    
471600                         DAT-O-TIDATUM                                    
471700                         DAT-KDSVAR                                       
471800     .                                                                    
471900     EJECT                                                                
472000* IMS SEKTIONER                                                           
472100     SKIP3                                                                
472200 IMS-GET-MSG SECTION.                                                     
472300     MOVE '  QC' TO GODK-STATUSKODER                                      
472400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
472500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
472600     PERFORM IMS-STATUSKONTROLL                                           
472700     .                                                                    
472800     SKIP3                                                                
472900 IMS-INSERT-MSG SECTION.                                                  
473000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
473100       MOVE 'N' TO MFS-KDHUVOMR                                           
473200     END-IF                                                               
473300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
473400     MOVE SPACE TO GODK-STATUSKODER                                       
473500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
473600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
473700     PERFORM IMS-STATUSKONTROLL                                           
473800     .                                                                    
473900     EJECT                                                                
474000 IMS-GET-WMSGKOM-MSG SECTION.                                             
474100                                                                          
474200     MOVE '  QD'   TO GODK-STATUSKODER                                    
474300     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
474400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
474500     PERFORM IMS-STATUSKONTROLL                                           
474600     .                                                                    
474700     SKIP3                                                                
474800 IMS-INSERT-WMSGKOM-MSG SECTION.                                          
474900                                                                          
475000     MOVE '  '  TO GODK-STATUSKODER                                       
475100     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
475200     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
475300     PERFORM IMS-STATUSKONTROLL                                           
475400     .                                                                    
475500     EJECT                                                                
475600 IMS-GET-ARTG01 SECTION.                                                  
475700     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
475800            DELIMITED BY SIZE INTO SSA1                                   
475900     MOVE '  GE' TO GODK-STATUSKODER                                      
476000     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA-2 SSA1                   
476100     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
476200     PERFORM IMS-STATUSKONTROLL                                           
476300     .                                                                    
476400     SKIP2                                                                
476500 IMS-ISRT-ARTG01 SECTION.                                                 
476600     MOVE 'WLARTG01 ' TO SSA1                                             
476700     MOVE '  ' TO GODK-STATUSKODER                                        
476800     CALL CBLTDLI USING ISRT ARTG-PCB DLI-IO-AREA-2 SSA1                  
476900     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
477000     PERFORM IMS-STATUSKONTROLL                                           
477100     .                                                                    
477200     SKIP2                                                                
477300 IMS-REPL-ARTG SECTION.                                                   
477400     MOVE '  ' TO GODK-STATUSKODER                                        
477500     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA-2                       
477600     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
477700     PERFORM IMS-STATUSKONTROLL                                           
477800     .                                                                    
477900     EJECT                                                                
478000 IMS-GET-ARTC01 SECTION.                                                  
478100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
478200            DELIMITED BY SIZE INTO SSA1                                   
478300     MOVE '  GE' TO GODK-STATUSKODER                                      
478400     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-1 SSA1                   
478500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
478600     PERFORM IMS-STATUSKONTROLL                                           
478700     .                                                                    
478800     SKIP2                                                                
478900 IMS-GET-ARTC11 SECTION.                                                  
479000     MOVE 'WLARTC11 ' TO SSA1                                             
479100     MOVE '  GE' TO GODK-STATUSKODER                                      
479200     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-1 SSA1                  
479300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
479400     PERFORM IMS-STATUSKONTROLL                                           
479500     .                                                                    
479600     SKIP2                                                                
479700 IMS-GNP-ARTC25 SECTION.                                                  
479800     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
479900     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
480000            DELIMITED BY SIZE INTO SSA2                                   
480100     MOVE '  GE' TO GODK-STATUSKODER                                      
480200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-1 SSA1 SSA2              
480300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
480400     PERFORM IMS-STATUSKONTROLL                                           
480500     .                                                                    
480600     SKIP2                                                                
480700 IMS-REPL-ARTC SECTION.                                                   
480800     MOVE '  ' TO GODK-STATUSKODER                                        
480900     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-1                       
481000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
481100     PERFORM IMS-STATUSKONTROLL                                           
481200     .                                                                    
481300     EJECT                                                                
481400 IMS-GET-ERSA01 SECTION.                                                  
481500     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
481600            DELIMITED BY SIZE INTO SSA1                                   
481700     MOVE '  GE' TO GODK-STATUSKODER                                      
481800     CALL CBLTDLI USING GHU ERSA-PCB DLI-IO-AREA-1 SSA1                   
481900     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
482000     PERFORM IMS-STATUSKONTROLL                                           
482100     .                                                                    
482200     SKIP2                                                                
482300 IMS-GET-ERSA11 SECTION.                                                  
482400     STRING 'WLERSA11(FLTEXT   =N)'                                       
482500            DELIMITED BY SIZE INTO SSA1                                   
482600     MOVE '  GE' TO GODK-STATUSKODER                                      
482700     CALL CBLTDLI USING GHNP ERSA-PCB DLI-IO-AREA-1 SSA1                  
482800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
482900     PERFORM IMS-STATUSKONTROLL                                           
483000     .                                                                    
483100     SKIP2                                                                
483200 IMS-GET-ERSB01 SECTION.                                                  
483300     STRING 'WLERSB01(WDD7A1KY >' W-IDARTNR-TILLK-X                       
483400                                  W-IDARTNR-ERS-LOW-X                     
483500                                  W-IDKORTNR-LOW-X                        
483600                    '&WDD7A1KY <' W-IDARTNR-TILLK-X                       
483700                                  W-IDARTNR-ERS-HIGH-X                    
483800                                  W-IDKORTNR-HIGH-X                       
483900                            ')'                                           
484000            DELIMITED BY SIZE INTO SSA1                                   
484100     MOVE '  GE' TO GODK-STATUSKODER                                      
484200     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-AREA-1 SSA1                    
484300     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
484400     PERFORM IMS-STATUSKONTROLL                                           
484500     .                                                                    
484600     EJECT                                                                
484700 IMS-DLET-ERSA SECTION.                                                   
484800     MOVE '  ' TO GODK-STATUSKODER                                        
484900     CALL CBLTDLI USING DLET ERSA-PCB DLI-IO-AREA-1                       
485000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
485100     PERFORM IMS-STATUSKONTROLL                                           
485200     .                                                                    
485300     EJECT                                                                
485400 IMS-GU-XXAQ11-UNIK     SECTION.                                          
485500     STRING 'WLXXAQ01(WDGXKEY  =' W-1131-KEY-X ')'                        
485600            DELIMITED BY SIZE INTO SSA1                                   
485700     STRING 'WLXXAQ11(WDGXKEY  =' W-1132-KEY-X ')'                        
485800            DELIMITED BY SIZE INTO SSA2                                   
485900     MOVE '  GE' TO GODK-STATUSKODER                                      
486000     CALL CBLTDLI USING GU XXAQ-PCB DLI-IO-AREA-1 SSA1 SSA2               
486100     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
486200     PERFORM IMS-STATUSKONTROLL                                           
486300     .                                                                    
486400     SKIP2                                                                
486500 IMS-GU-XXAQ01          SECTION.                                          
486600     STRING 'WLXXAQ01(WDGXKEY  =' W-1131-KEY-X ')'                        
486700            DELIMITED BY SIZE INTO SSA1                                   
486800     MOVE '  GE' TO GODK-STATUSKODER                                      
486900     CALL CBLTDLI USING GU XXAQ-PCB DLI-IO-AREA-1 SSA1                    
487000     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
487100     PERFORM IMS-STATUSKONTROLL                                           
487200     .                                                                    
487300     SKIP2                                                                
487400 IMS-GNP-XXAQ11          SECTION.                                         
487500     MOVE   'WLXXAQ11 ' TO SSA1                                           
487600     MOVE '  GE' TO GODK-STATUSKODER                                      
487700     CALL CBLTDLI USING GNP XXAQ-PCB DLI-IO-AREA-1 SSA1                   
487800     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
487900     PERFORM IMS-STATUSKONTROLL                                           
488000     .                                                                    
488100     EJECT                                                                
488200 IMS-GET-BENA01-ASEQ SECTION.                                             
488300     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
488400                      W-BEART-X  ')'                                      
488500            DELIMITED BY SIZE INTO SSA1                                   
488600     MOVE '  GE' TO GODK-STATUSKODER                                      
488700     CALL CBLTDLI USING GN BENB-PCB DLI-IO-AREA-1 SSA1                    
488800     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
488900     PERFORM IMS-STATUSKONTROLL                                           
489000     .                                                                    
489100     SKIP2                                                                
489200 IMS-GET-BENA13-ASEQ SECTION.                                             
489300     MOVE 'WLBENA13 ' TO SSA1                                             
489400     MOVE '  GE' TO GODK-STATUSKODER                                      
489500     CALL CBLTDLI USING GNP BENB-PCB DLI-IO-AREA-1 SSA1                   
489600     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
489700     PERFORM IMS-STATUSKONTROLL                                           
489800     .                                                                    
489900     EJECT                                                                
490000 IMS-GET-BENA11-CSEQ SECTION.                                             
490100     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
490200            DELIMITED BY SIZE INTO SSA1                                   
490300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
490400            DELIMITED BY SIZE INTO SSA2                                   
490500     MOVE '  ' TO GODK-STATUSKODER                                        
490600     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA-1 SSA1 SSA2               
490700     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
490800     PERFORM IMS-STATUSKONTROLL                                           
490900     .                                                                    
491000     SKIP2                                                                
491100 IMS-GN-ARTH01 SECTION.                                                   
491200     STRING 'WLARTH01(WDD2A1KY> ' W-WDD2A1KY-MIN                          
491300                    '&WDD2A1KY<=' W-WDD2A1KY-MAX                          
491400                    '&IDPROJ  >=' W-IDPROJ-MIN                            
491500                    '&IDPROJ  <=' W-IDPROJ-MAX ')'                        
491600            DELIMITED BY SIZE INTO SSA1                                   
491700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
491800     CALL CBLTDLI USING GN ARTH-PCB DLI-IO-AREA-2 SSA1                    
491900     MOVE ARTH-STATUS-CODE TO STATUS-WS                                   
492000     PERFORM IMS-STATUSKONTROLL                                           
492100     .                                                                    
492200     SKIP2                                                                
492300 IMS-GU-BENA01-CSEQ SECTION.                                              
492400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
492500            DELIMITED BY SIZE INTO SSA1                                   
492600     MOVE '  ' TO GODK-STATUSKODER                                        
492700     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA-1 SSA1                    
492800     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
492900     PERFORM IMS-STATUSKONTROLL                                           
493000     .                                                                    
493100     SKIP2                                                                
493200 IMS-GNP-BENA11-CSEQ SECTION.                                             
493300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
493400            DELIMITED BY SIZE INTO SSA1                                   
493500     MOVE '  ' TO GODK-STATUSKODER                                        
493600     CALL CBLTDLI USING GNP BENC-PCB DLI-IO-AREA-1 SSA1                   
493700     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
493800     PERFORM IMS-STATUSKONTROLL                                           
493900     .                                                                    
494000     EJECT                                                                
494100 IMS-GET-SATB01 SECTION.                                                  
494200     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
494300            DELIMITED BY SIZE INTO SSA1                                   
494400     MOVE '  GE' TO GODK-STATUSKODER                                      
494500     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA-3 SSA1                    
494600     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
494700     PERFORM IMS-STATUSKONTROLL                                           
494800     .                                                                    
494900     SKIP2                                                                
495000 IMS-GET-SATB11-CSEQ SECTION.                                             
495100     STRING 'WLSATB11(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                        
495200            DELIMITED BY SIZE INTO SSA1                                   
495300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
495400     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA-3 SSA1                    
495500     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
495600     PERFORM IMS-STATUSKONTROLL                                           
495700     .                                                                    
495800     EJECT                                                                
495900 IMS-GU-WDF501 SECTION.                                                   
496000     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
496100            DELIMITED BY SIZE INTO SSA1                                   
496200     MOVE '  GE' TO GODK-STATUSKODER                                      
496300     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA-4 SSA1                    
496400     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
496500     PERFORM IMS-STATUSKONTROLL                                           
496600     .                                                                    
496700     SKIP2                                                                
496800 IMS-GN-WDF5A1 SECTION.                                                   
496900     STRING 'WDF5A1  (WDF5A1KY=>' W-WDF5A1KY-MIN                          
497000                    '&WDF5A1KY=<' W-WDF5A1KY-MAX ')'                      
497100          DELIMITED BY SIZE INTO SSA1                                     
497200     MOVE '  GE' TO GODK-STATUSKODER                                      
497300     CALL CBLTDLI USING GN WDF5A-PCB DLI-IO-AREA-5 SSA1                   
497400     MOVE WDF5A-STATUS-CODE TO STATUS-WS                                  
497500     PERFORM IMS-STATUSKONTROLL                                           
497600     .                                                                    
497700     EJECT                                                                
497800 IMS-INSERT-ALT SECTION.                                                  
497900     MOVE SPACE TO GODK-STATUSKODER                                       
498000     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
498100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
498200     PERFORM IMS-STATUSKONTROLL                                           
498300     .                                                                    
498400     SKIP3                                                                
498500 IMS-STATUSKONTROLL SECTION.                                              
498600     SET STATUS-IX TO 1                                                   
498700     SEARCH GODK-STATUS AT END CALL FELLOG                                
498800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
498900       CONTINUE                                                           
499000     END-SEARCH                                                           
499100     .                                                                    
499200     EJECT                                                                
499300*    -COPY WY2000P1                                                       
499400     EJECT                                                                
499500*    -COPY WY2000P2                                                       
499600     EJECT                                                                
499700*    -COPY WY2000Q2                                                       
499800     EJECT                                                                
499900*    -COPY WY2000P3                                                       
