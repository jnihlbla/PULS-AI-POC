000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9041000.                                                
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
002000*       ******************************************************            
002100*       * X-TRANSAR  KAN KOMMA TILL DETTA PGM OCH            *            
002200*       * KOMMER FRÅN DISPATCHERN SOM FÅR INDATA FRÅN RUTIN  *            
002300*       * W100B1A. "MIDDAR" SKAPAS DÄR I PGM W9410100.       *            
002400*       * NÄR KONTROLLREGLER MOT INDATA ÄNDRAS I 9410, V.G.  *            
002500*       * UPPDATERA ÄVEN W9410100.                           *            
002600*       ******************************************************            
002700*                                                                         
002800*        ÄT SOMMAREN 2014  TIFINLV UTBYTT MOT TISOP                       
002900*                                                                         
003000*                                                                         
003100*    INDATA.                                                              
003200*        TRANSAKTION: W90410T                                             
003300*                     W90410U                                             
003400*                     W1T116X                                             
003500*        MID:         W90410I1                                            
003600*                                                                         
003700*    UTDATA.                                                              
003800*        MOD:         W90410O1                                            
003900*        TRANSAKTION: W0T693X                                             
004000*                                                                         
004100*    SUBPROGRAM:      CBLTDLI                                             
004200*                     FELLOG                                              
004300*                     WDATKONV                                            
004400*                     WREVERSE                                            
004500*                     W009REDU                                            
004600*                     WKPSKONV                                            
004700*                     W005INIT                                            
004800     EJECT                                                                
004900 ENVIRONMENT DIVISION.                                                    
005000     SKIP3                                                                
005100 DATA DIVISION.                                                           
005200     SKIP3                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400*    -COPY WY2000W3                                                       
005500     SKIP3                                                                
005600*    -COPY WY2000W2                                                       
005700     SKIP3                                                                
005800*    -COPY WY2000W1                                                       
005900     SKIP3                                                                
006000 77  PROGRAM-NAMN                PIC X(08)   VALUE 'W9041000'.            
006100 77    JA                        PIC X       VALUE 'J'.                   
006200 77    NEJ                       PIC X       VALUE 'N'.                   
006300 01    WS-IDTRANS                PIC X(4).                                
006400    88  EGEN-BILD                VALUE '9410'.                            
006500 01    SW-PROJ-GODK              PIC X(1).                                
006600    88  PROJ-GODK                VALUE 'J'.                               
006700                                                                          
006800 01    SW-PROJK-GODK              PIC X(1).                               
006900    88  PROJK-GODK                VALUE 'J'.                              
007000                                                                          
007100 01    SW-TRAEFF                 PIC X(1).                                
007200    88  TRAEFF                   VALUE 'J'.                               
007300                                                                          
007400 77    WS-W009REDU-IN            PIC X(30)   VALUE SPACE.                 
007500 77    WS-W009REDU-UT            PIC X(30)   VALUE SPACE.                 
007600 77    WS-BEART                  PIC X(25)   VALUE SPACE.                 
007700 77    WS-BEART-SVE              PIC X(25)   VALUE SPACE.                 
007800 77    WS-FLPISK                 PIC X       VALUE SPACE.                 
007900 77    WS-KVPROG                 PIC 9(7)    VALUE ZERO.                  
008000 77    WS-FLBYTES                PIC X       VALUE SPACE.                 
008100 01    WS-IDPROJ                 PIC X(4)    VALUE SPACE.                 
008200 77    WS-IDRITN                 PIC X(10)   VALUE SPACE.                 
008300 77    WS-IDAO                   PIC X(10)   VALUE SPACE.                 
008400 77    WS-IDBERED                PIC 9(2)    VALUE ZERO.                  
008500 77    WS-IDBERED-2-NUM          PIC 9(2)    VALUE ZERO.                  
008600 77    WS-IDSKYLT                PIC X(3)    VALUE 'S  '.                 
008700                                                                          
008800 01    WS-IDPROJK.                                                        
008900    05  W-PROJK-POS-1            PIC X(1)    VALUE SPACE.                 
009000    05  W-PROJK-POS-2            PIC X(1)    VALUE SPACE.                 
009100    05  FILLER                   PIC X(2)    VALUE SPACE.                 
009200 77    WS-IDPROJK-GAM            PIC X(4)    VALUE SPACE.                 
009300 77    WS-KVARTVAGN              PIC 9(3)    VALUE ZERO.                  
009400 77    WS-IDFKNGRP               PIC 9(4)    VALUE ZERO.                  
009500 77    WS-KDPRODSL               PIC 9(2)    VALUE ZERO.                  
009600 77    WS-KDSORT                 PIC X(2)    VALUE SPACE.                 
009700 77    WS-KDBPSR                 PIC 9(1)    VALUE ZERO.                  
009800 77    WS-KDFARLIG               PIC 9(1)    VALUE ZERO.                  
009900 77    WS-KDYTBEH                PIC 9(2)    VALUE ZERO.                  
010000                                                                          
010100 01    WS-TEST-IDFKNGRP          PIC 9(4)    VALUE ZERO.                  
010200 01    FILLER REDEFINES WS-TEST-IDFKNGRP.                                 
010300       03 FILLER                 PIC 9(3).                                
010400       03 WS-SISTA-SIFFRAN       PIC 9.                                   
010500                                                                          
010600 77    WS-SPAR-FLPISK            PIC X       VALUE SPACE.                 
010700 77    WS-SPAR-KVPROG            PIC 9(7)    VALUE ZERO.                  
010800 77    WS-SPAR-FLBYTES           PIC X       VALUE SPACE.                 
010900 77    WS-SPAR-IDPROJK           PIC X(4)    VALUE SPACE.                 
011000 77    WS-SPAR-KVARTVAGN         PIC 9(3)    VALUE ZERO.                  
011100 77    WS-SPAR-TEORSAK-1         PIC X(40)   VALUE SPACE.                 
011200 77    W-SPAR-TISERLEV           PIC 9(7)    VALUE ZERO.                  
011300 77    WS-NYTT-NUMMER            PIC S9(5)   COMP-3 VALUE ZERO.           
011400 77    MAX-IX                    PIC S9(3)   VALUE +3.                    
011500 77    MAX-TAB                   PIC S9(3)   VALUE +10.                   
011600 77    MAX-TAB-PLUS-1            PIC S9(3)   VALUE +11.                   
011700 77    MAX-IX-PLUS-1             PIC S9(3)   VALUE +4.                    
011800 77    SPIND                     PIC S9(9)   VALUE +0  COMP SYNC.         
011900 77    SLEV-IX                   PIC S9(9)   VALUE +0  COMP SYNC.         
012000 77    IX                        PIC S9(9)   VALUE +0  COMP SYNC.         
012100 77    RAD-INDX                  PIC S9(9)   VALUE +0  COMP SYNC.         
012200 77    MAX-RAD                   PIC S9(9)   VALUE +13 COMP SYNC.         
012300 77    MAX-RAD-PLUS-1            PIC S9(9)   VALUE +14 COMP SYNC.         
012400 77    MAX-RAD-PLUS-2            PIC S9(9)   VALUE +15 COMP SYNC.         
012500 77    TAB-IX                    PIC S9(9)   VALUE +0  COMP SYNC.         
012600 77    AO-IX                     PIC S9(9)   VALUE +0  COMP SYNC.         
012700*77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +683 COMP SYNC.         
012800 77    WS-IDARTNR-MOTSV          PIC 9(09)  VALUE ZERO.                   
012900 77    WS-SPAR-IDARTNR-MOTSV     PIC 9(09)  VALUE ZERO.                   
013000     SKIP2                                                                
013100 01 WS-NOLL                  PIC S9(9) VALUE ZERO COMP-3.                 
013200 01 WS-VAR                   PIC X     VALUE SPACE.                       
013300*      --- VALID IDDC CODES                                               
013400*                                                                         
013500*01    -COPY WWDC99                                                       
013600       EJECT                                                              
013700                                                                          
013800*   -COPY  WWPRODSL                                                       
013900     SKIP2                                                                
014000 01  WS-IDARTNR                       PIC X(9) VALUE SPACE.               
014100 01  IDARTNR-WS REDEFINES WS-IDARTNR  PIC 9(9).                           
014200     SKIP2                                                                
014300 01  WS-NY-IDARTNR                           PIC X(9) VALUE SPACE.        
014400 01  IDARTNR-NY-WS REDEFINES WS-NY-IDARTNR PIC 9(9).                      
014500     SKIP2                                                                
014600 01  WS-IDARTNR-TILLK            PIC S9(9)    COMP-3 VALUE ZERO.          
014700     EJECT                                                                
014800 01  INPUT-RETT                  PIC X        VALUE 'J'.                  
014900 01  KOPIERING                   PIC X        VALUE 'N'.                  
015000 01  UPPDATERING                 PIC X        VALUE 'N'.                  
015100 01  SAMMA-BEN                   PIC X        VALUE 'N'.                  
015200 01  FINNS-REG-PA-NYPON          PIC X        VALUE 'N'.                  
015300 01  FINNS-PA-ARTC               PIC X        VALUE 'N'.                  
015400 01  VAECKNING                   PIC X        VALUE 'N'.                  
015500 01  NYPON-ARTIKEL               PIC X        VALUE 'N'.                  
015600 01  TRANS-TO-1192               PIC X        VALUE 'N'.                  
015700 01  FINNS-RASA                  PIC X        VALUE 'N'.                  
015800     SKIP3                                                                
015900 01  WS-TEHOMONYM.                                                        
016000     03  RS-BM-NAMN              PIC X(7).                                
016100     03  FILLER                  PIC X(53).                               
016200     SKIP3                                                                
016300 01  TABELL                      PIC X(30)                                
016400                       VALUE 'D  E  F  GB I  NL P  S  SF USA'.            
016500 01  TAB REDEFINES TABELL.                                                
016600     03  FILLER OCCURS 10.                                                
016700         05  TAB-IDSKYLT         PIC X(3).                                
016800     SKIP3                                                                
016900 01  WS-TISOP-AAMMDD           PIC 9(6)    VALUE ZERO.                    
017000 01  WS-TISOP                  PIC 9(5)    VALUE ZERO.                    
017100 01  FILLER REDEFINES WS-TISOP.                                           
017200     03  WS-AAR                PIC 9(2).                                  
017300     03  WS-VECKA              PIC 9(2).                                  
017400     03  WS-DAG                PIC 9(1).                                  
017500     SKIP2                                                                
017600 01  XX-TISOP                  PIC X(5)    VALUE SPACE.                   
017700 01  FILLER REDEFINES XX-TISOP.                                           
017800     03  XX-AAR                PIC X(2).                                  
017900     03  XX-VECKA              PIC X(2).                                  
018000     03  XX-DAG                PIC X(1).                                  
018100                                                                          
018200 01  WS-YYWWD-VECKA-PLUS-1       PIC 9(5) VALUE ZERO.                     
018300 01  FILLER REDEFINES WS-YYWWD-VECKA-PLUS-1.                              
018400     03  WS-YY                   PIC 9(2).                                
018500     03  WS-WW                   PIC 9(2).                                
018600     03  WS-D                    PIC 9(1).                                
018700     SKIP2                                                                
018800 01  INMATAD-IDPROENH.                                                    
018900     03  FILLER OCCURS 3.                                                 
019000         05  WS-IDPROENH         PIC X(8).                                
019100     SKIP2                                                                
019200 01  INMATAD-IDKAT.                                                       
019300     03  FILLER OCCURS 3.                                                 
019400         05  WS-IDKAT            PIC X(5).                                
019500     SKIP2                                                                
019600 01  DAGENS-AAMMDD               PIC 9(6)  VALUE ZERO.                    
019700 01  DAGENS-AAVV.                                                         
019800     03  DAGENS-AA               PIC 9(2)  VALUE ZERO.                    
019900     03  DAGENS-VV               PIC 9(2)  VALUE ZERO.                    
020000     SKIP2                                                                
020100 01  TINEDB-AAVV.                                                         
020200     03  TINEDB-AA               PIC 9(2)  VALUE ZERO.                    
020300     03  TINEDB-VV               PIC 9(2)  VALUE ZERO.                    
020400     SKIP2                                                                
020500 01  VECKOR.                                                              
020600     03  AAVVD                   PIC 9(5).                                
020700     03  FILLER REDEFINES AAVVD.                                          
020800       05  AAVV                  PIC 9(4).                                
020900       05  FILLER REDEFINES AAVV.                                         
021000         07  AA                  PIC 9(2).                                
021100         07  VV                  PIC 9(2).                                
021200       05  D                     PIC 9(1).                                
021300     SKIP2                                                                
021400 01  W-AAR4                      PIC 9(4).                                
021500     SKIP3                                                                
021600 01  DYNAMISKA-SUBPROGRAM.                                                
021700     03  WDATKONV                PIC X(8)     VALUE 'WDATKONV'.           
021800     03  WKPSKONV                PIC X(8)     VALUE 'WKPSKONV'.           
021900     03  WREVERSE                PIC X(8)     VALUE 'WREVERSE'.           
022000     03  W009REDU                PIC X(8)     VALUE 'W009REDU'.           
022100     03  CBLTDLI                 PIC X(8)     VALUE 'CBLTDLI '.           
022200     03  FELLOG                  PIC X(8)     VALUE 'FELLOG  '.           
022300     03  W005INIT                PIC X(8)     VALUE 'W005INIT'.           
022400     EJECT                                                                
022500*01 -COPY WDATAREA                                                        
022600     EJECT                                                                
022700*01 -COPY WKPSAREA                                                        
022800     EJECT                                                                
022900*01 -COPY WREVAREA                                                        
023000     EJECT                                                                
023100*                   ****    PARAMETRAR TILL W005INIT                      
023200*01  -COPY WMSGINIT                                                       
023300     EJECT                                                                
023400 01    MEDDELANDE.                                                        
023500   03    W-FEL-1.                                                         
023600     05   FILLER                 PIC X(34)   VALUE                        
023700             'ARTIKELNUMMER EJ NUMERISKT        '.                        
023800     05   FILLER                 PIC X(34)   VALUE                        
023900             'PARTNUMBER NOT NUMERIC            '.                        
024000   03    FILLER REDEFINES W-FEL-1.                                        
024100     05  FEL-1                   PIC X(34) OCCURS 2.                      
024200                                                                          
024300   03    W-FEL-2.                                                         
024400     05   FILLER                 PIC X(34)   VALUE                        
024500             'ARTIKEL SAKNAS PÅ ARTIKELREGISTRET'.                        
024600     05   FILLER                 PIC X(34)   VALUE                        
024700             'THIS PART IS NOT IN THE DATABASE  '.                        
024800   03    FILLER REDEFINES W-FEL-2.                                        
024900     05  FEL-2                   PIC X(34) OCCURS 2.                      
025000                                                                          
025100   03    W-FEL-3.                                                         
025200     05   FILLER                 PIC X(34)   VALUE                        
025300             'ARTIKEL REDAN REGISTRERAD         '.                        
025400     05   FILLER                 PIC X(34)   VALUE                        
025500             'PART NO ALREADY REGISTRED         '.                        
025600   03    FILLER REDEFINES W-FEL-3.                                        
025700     05  FEL-3                   PIC X(34) OCCURS 2.                      
025800                                                                          
025900   03    W-FEL-4.                                                         
026000     05   FILLER                 PIC X(34)   VALUE                        
026100             'UPPLYSTA FÄLT FEL                 '.                        
026200     05   FILLER                 PIC X(34)   VALUE                        
026300             'CORRECT HIGHLIGHTED FIELDS        '.                        
026400   03    FILLER REDEFINES W-FEL-4.                                        
026500     05  FEL-4                   PIC X(34) OCCURS 2.                      
026600                                                                          
026700   03    W-FEL-5.                                                         
026800     05   FILLER                 PIC X(34)   VALUE                        
026900             'BENÄMNING SAKNAS PÅ BENREG        '.                        
027000     05   FILLER                 PIC X(34)   VALUE                        
027100             'DESCRIPTION IS MISSING            '.                        
027200   03    FILLER REDEFINES W-FEL-5.                                        
027300     05  FEL-5                   PIC X(34) OCCURS 2.                      
027400                                                                          
027500   03    W-FEL-6.                                                         
027600     05   FILLER                 PIC X(32)   VALUE                        
027700             'HOMONYMKOD FINNS-KORRIGERA     '.                           
027800     05   FILLER                 PIC X(32)   VALUE                        
027900             'HOM.CODE EXISTS  VERIFY        '.                           
028000   03    FILLER REDEFINES W-FEL-6.                                        
028100     05  FEL-6                   PIC X(32) OCCURS 2.                      
028200                                                                          
028300   03    W-FEL-7.                                                         
028400     05   FILLER                 PIC X(32)   VALUE                        
028500             'UPPDATERING MED PF11       '.                               
028600     05   FILLER                 PIC X(32)   VALUE                        
028700             'PRESS PF11 FOR UPDATING    '.                               
028800   03    FILLER REDEFINES W-FEL-7.                                        
028900     05  FEL-7                   PIC X(32) OCCURS 2.                      
029000                                                                          
029100   03    W-FEL-8.                                                         
029200     05   FILLER                 PIC X(32)   VALUE                        
029300             'ANGE NYTT ARTIKELNUMMER    '.                               
029400     05   FILLER                 PIC X(32)   VALUE                        
029500             'GIVE A NEW PARTNUMBER      '.                               
029600   03    FILLER REDEFINES W-FEL-8.                                        
029700     05  FEL-8                   PIC X(32) OCCURS 2.                      
029800                                                                          
029900   03    W-FEL-10.                                                        
030000     05   FILLER                 PIC X(36)   VALUE                        
030100             'ARTIKEL REDAN REGISTRERAD PÅ NYPON'.                        
030200     05   FILLER                 PIC X(36)   VALUE                        
030300             'PART NO ALREADY REGISTRED         '.                        
030400   03    FILLER REDEFINES W-FEL-10.                                       
030500     05  FEL-10                  PIC X(36) OCCURS 2.                      
030600                                                                          
030700   03    W-FEL-12.                                                        
030800     05   FILLER                 PIC X(40)   VALUE                        
030900             'DETTA ÄR SISTA ARTIKELN FRÅN BILD 1142'.                    
031000     05   FILLER                 PIC X(40)   VALUE                        
031100             'LAST PART NO FROM SCREEN 1142     '.                        
031200   03    FILLER REDEFINES W-FEL-12.                                       
031300     05  FEL-12                  PIC X(40) OCCURS 2.                      
031400                                                                          
031500   03    W-FEL-13.                                                        
031600     05   FILLER                 PIC X(40)   VALUE                        
031700             'UPPDATERING EJ TILLÅTET               '.                    
031800     05   FILLER                 PIC X(40)   VALUE                        
031900             'UPDATE NOT ALLOWED                '.                        
032000   03    FILLER REDEFINES W-FEL-13.                                       
032100     05  FEL-13                  PIC X(40) OCCURS 2.                      
032200                                                                          
032300   03    W-FEL-14.                                                        
032400     05   FILLER                 PIC X(40)   VALUE                        
032500             'ARTIKEL SAKNAS ARTREG MEN FINNS RASA  '.                    
032600     05   FILLER                 PIC X(40)   VALUE                        
032700             'PART NO REGISTRED IN RASA         '.                        
032800   03    FILLER REDEFINES W-FEL-14.                                       
032900     05  FEL-14                  PIC X(40) OCCURS 2.                      
033000                                                                          
033100   03    W-MED-1.                                                         
033200     05   FILLER                 PIC X(32)   VALUE                        
033300             'UPPDATERING GJORD          '.                               
033400     05   FILLER                 PIC X(32)   VALUE                        
033500             'UPDATED                    '.                               
033600   03    FILLER REDEFINES W-MED-1.                                        
033700     05  MED-1                   PIC X(32) OCCURS 2.                      
033800                                                                          
033900   03    W-MED-2.                                                         
034000     05   FILLER                 PIC X(32)   VALUE                        
034100             'ARTIKELN AVSLAGEN          '.                               
034200     05   FILLER                 PIC X(32)   VALUE                        
034300             'REJECTED PART              '.                               
034400   03    FILLER REDEFINES W-MED-2.                                        
034500     05  MED-2                   PIC X(32) OCCURS 2.                      
034600                                                                          
034700   03    W-MED-5.                                                         
034800     05   FILLER                 PIC X(40)   VALUE                        
034900             'ARTIKELN FINNS REDAN PÅ CROSSINDEX'.                        
035000     05   FILLER                 PIC X(40)   VALUE                        
035100             'THIS PART ALREADY EXISTS IN CROSS-INDEX'.                   
035200   03    FILLER REDEFINES W-MED-5.                                        
035300     05  MED-5                   PIC X(40) OCCURS 2.                      
035400     EJECT                                                                
035500 01  NYCKLAR-TILL-DLI.                                                    
035600   03  W-WDD2A1KY-MIN.                                                    
035700     05  W-IDBERED-MIN           PIC S9(3)   COMP-3.                      
035800     05  W-IDAO-MIN              PIC X(10).                               
035900     05  W-IDARTNR-MIN           PIC S9(9)   COMP-3.                      
036000   03  W-WDD2A1KY-MAX.                                                    
036100     05  W-IDBERED-MAX           PIC S9(3)   COMP-3.                      
036200     05  W-IDAO-MAX              PIC X(10).                               
036300     05  W-IDARTNR-MAX           PIC S9(9)   COMP-3  VALUE                
036400                                                +999999999.               
036500                                                                          
036600   03  W-WDJ1CSEQ-X.                                                      
036700     05  W-IDLEVNR-S             PIC X(5)    VALUE SPACE.                 
036800     05  W-BELEVART-S            PIC X(30)   VALUE SPACE.                 
036900     05  W-IDARTNR-S             PIC S9(9)   COMP-3  VALUE ZERO.          
037000                                                                          
037100   03  W-IDPROJ-MIN              PIC X(4).                                
037200   03  W-IDPROJ-MAX              PIC X(4).                                
037300   03  W-IDARTNR-XX.                                                      
037400     05  W-IDARTNR-1142          PIC S9(9)   COMP-3.                      
037500   03  W-IDARTNR-X.                                                       
037600     05  W-IDARTNR               PIC S9(9)   COMP-3.                      
037700   03  W-KDNOTTYP-X.                                                      
037800     05  W-KDNOTTYP              PIC S9      COMP-3.                      
037900   03  W-IDARTNR-TILLK-X.                                                 
038000     05  W-IDARTNR-TILLK         PIC S9(9)   COMP-3.                      
038100   03  W-IDARTNR-ERS-LOW-X.                                               
038200     05  W-IDARTNR-ERS-LOW       PIC S9(9)   COMP-3  VALUE ZERO.          
038300   03  W-IDARTNR-ERS-HIGH-X.                                              
038400     05  W-IDARTNR-ERS-HIGH      PIC S9(9)   COMP-3                       
038500                                  VALUE +999999999.                       
038600   03  W-IDKORTNR-LOW-X.                                                  
038700     05  W-IDKORTNR-LOW          PIC S9(3)   COMP-3  VALUE ZERO.          
038800   03  W-IDKORTNR-HIGH-X.                                                 
038900     05  W-IDKORTNR-HIGH         PIC S9(3)   COMP-3  VALUE +999.          
039000                                                                          
039100   03  W-IDSKYLT-X.                                                       
039200     05  W-IDSKYLT               PIC X(3)    VALUE SPACE.                 
039300   03  W-BEART-X.                                                         
039400     05  W-BEART                 PIC X(25)   VALUE SPACE.                 
039500                                                                          
039600   03  W-KDSEGKEY-X.                                                      
039700     05  W-KDSEGKEY              PIC  X(1)   VALUE '1'.                   
039800                                                                          
039900                                                                          
040000   03  W-1207-KEY-X.                                                      
040100     05  FILLER                  PIC X(4)    VALUE '1207'.                
040200     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
040300                                                                          
040400   03  W-1131-KEY-X.                                                      
040500     05  FILLER                  PIC X(4)    VALUE '1131'.                
040600     05  W-KDPRODSL              PIC S9(3)   COMP-3 VALUE ZERO.           
040700     05  FILLER                  PIC X(24)   VALUE LOW-VALUE.             
040800                                                                          
040900   03  W-1132-KEY-X.                                                      
041000     05  W-IDPROJK               PIC X(4)    VALUE LOW-VALUE.             
041100     05  W-IDPROJOBJ             PIC X(4)    VALUE LOW-VALUE.             
041200     05  W-IDPROJ                PIC X(4)    VALUE LOW-VALUE.             
041300     05  FILLER                  PIC X(3)    VALUE LOW-VALUE.             
041400                                                                          
041500     EJECT                                                                
041600******************************************************************        
041700*                                                                         
041800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
041900*                                                                         
042000 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
042100     SKIP3                                                                
042200*01    MID -COPY W90410I1.                                                
042300     EJECT                                                                
042400*01    MID -COPY W1I14201 -PRE 1142-.                                     
042500     EJECT                                                                
042600 01  MSG-KOM-MESSAGE-CODES.                                               
042700     03  FEL-ERR-FIELD           PIC X(3)    VALUE '001'.                 
042800     03  OK-GODKANT-FEL          PIC X(3)    VALUE '114'.                 
042900     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
043000     EJECT                                                                
043100 01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-AREA'.         
043200*    --- GENERELL IO-KOMMUNIKATIONSAREA FÖR DISPATCHER                    
043300*01    -COPY WMSGKOM                                                      
043400     EJECT                                                                
043500 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
043600*01    -COPY WMSGAREA                                                     
043700     EJECT                                                                
043800*  03    MOD -COPY W90410O1 -RED MSG-AREA.                                
043900     EJECT                                                                
044000 01  FILLER          PIC X(16) VALUE 'PROG-TO-PROG-SW'.                   
044100 01  W-PROG-TO-PROG-SW.                                                   
044200     03  P-WS-LL     PIC S9(4)  VALUE +1099 COMP SYNC.                    
044300     03  P-WS-Z1-Z2  PIC X(2)   VALUE LOW-VALUE.                          
044400     03  KDTRANS-WS  PIC X(8)   VALUE 'W1T192X '.                         
044500     03  P-IDTRANS   PIC X(4)   VALUE '9410'.                             
044600     03  P-KDMFSFOR  PIC X(1)   VALUE '1'.                                
044700*    03  MID   -COPY W1I11601     -PRE PROGSW-.                           
044800*    03  MOD   -COPY W1O11601     -PRE PROGSW-.                           
044900     EJECT                                                                
045000*01    -COPY WMFSAREA                                                     
045100     EJECT                                                                
045200******************************************************************        
045300*                                                                         
045400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
045500*                                                                         
045600 01    IMS-WS.                                                            
045700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
045800     SKIP3                                                                
045900*                        **** STATUS-KOD FRÅN IMS                         
046000   03    STATUS-WS               PIC XX.                                  
046100     88    SEGMENT-FINNS                     VALUE '  '.                  
046200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
046300     88    BASEN-SLUT                        VALUE 'GB'.                  
046400     SKIP3                                                                
046500   03    GODK-STATUSKODER.                                                
046600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
046700     SKIP3                                                                
046800 01    SSA1                      PIC X(96).                               
046900 01    SSA2                      PIC X(64).                               
047000     EJECT                                                                
047100*                            IMS FUNKTIONSKODER                           
047200*01    -COPY W0003                                                        
047300     EJECT                                                                
047400*                            DLI INPUT-OUTPUT AREA-1                      
047500 01    DLI-IO-AREA-1.                                                     
047600   03    IO-AREA-1               PIC X(900)  VALUE SPACE.                 
047700     SKIP3                                                                
047800*  03  WLARTC01 -COPY WDK601                 -RED IO-AREA-1.              
047900     EJECT                                                                
048000*  03  WLARTC11 -COPY WDK611                 -RED IO-AREA-1.              
048100     EJECT                                                                
048200*  03  WLARTC25 -COPY WDK625                 -RED IO-AREA-1.              
048300     EJECT                                                                
048400*  03  WLBENA01 -COPY WDD301  -PRE BENA-     -RED IO-AREA-1.              
048500     EJECT                                                                
048600*  03  WLBENA11 -COPY WDD311  -PRE BENA-     -RED IO-AREA-1.              
048700     EJECT                                                                
048800*  03  WLBENA12 -COPY WDD312  -PRE BENA-     -RED IO-AREA-1.              
048900     EJECT                                                                
049000*  03  WLBENA13 -COPY WDD313  -PRE BENA-     -RED IO-AREA-1.              
049100     EJECT                                                                
049200*  03  WLXXAQ11 -COPY WDGX1132   -PRE XXAQ-  -RED IO-AREA-1.              
049300     EJECT                                                                
049400*                            DLI INPUT-OUTPUT AREA-2                      
049500 01    DLI-IO-AREA-2.                                                     
049600   03    IO-AREA-2               PIC X(600)  VALUE SPACE.                 
049700     SKIP3                                                                
049800*  03  WLARTG01 -COPY WDD201  -PRE ARTG01-  -RED IO-AREA-2.               
049900*  03  WLARTH01 -COPY WDD2A1  -PRE ARTH01-  -RED IO-AREA-2.               
050000     EJECT                                                                
050100 01    DLI-IO-AREA-3.                                                     
050200   03    IO-AREA-3               PIC X(240)  VALUE SPACE.                 
050300*  03  WLSATB01 -COPY WDJ101  -PRE SATB-     -RED IO-AREA-3.              
050400     EJECT                                                                
050500*  03  WLSATB11 -COPY WDJ111  -PRE SATB-     -RED IO-AREA-3.              
050600     EJECT                                                                
050700 LINKAGE SECTION.                                                         
050800*01    -COPY W0009     -PRE MSG-                                          
050900     EJECT                                                                
051000*01    -COPY W0009     -PRE ALT-                                          
051100     EJECT                                                                
051200*01    -COPY W0009     -PRE MSGKOM-                                       
051300     EJECT                                                                
051400*01  -COPY W0008     -PRE USEA-                                           
051500         05  FILLER           PIC X.                                      
051600     EJECT                                                                
051700*01    -COPY W0008     -PRE ARTC-                                         
051800     05  FILLER                  PIC X.                                   
051900     EJECT                                                                
052000*01    -COPY W0008     -PRE BENB-                                         
052100     05  FILLER                  PIC X.                                   
052200     EJECT                                                                
052300*01    -COPY W0008     -PRE BENC-                                         
052400     05  FILLER                  PIC X.                                   
052500     EJECT                                                                
052600*01    -COPY W0008     -PRE XXAQ-                                         
052700     05  FILLER                  PIC X.                                   
052800     EJECT                                                                
052900*01    -COPY W0008     -PRE ARTG-                                         
053000     05  FILLER                  PIC X.                                   
053100     EJECT                                                                
053200*01    -COPY W0008     -PRE ARTH-                                         
053300     05  FILLER                  PIC X.                                   
053400     EJECT                                                                
053500*01    -COPY W0008     -PRE SATB-                                         
053600     05  FILLER                  PIC X.                                   
053700     EJECT                                                                
053800*01    -COPY W0008     -PRE SATE-                                         
053900     05  FILLER                  PIC X.                                   
054000     EJECT                                                                
054100 PROCEDURE DIVISION USING MSG-PCB ALT-PCB MSGKOM-PCB                      
054200                          USEA-PCB                                        
054300                          ARTC-PCB                   BENB-PCB             
054400                          BENC-PCB ARTG-PCB ARTH-PCB XXAQ-PCB             
054500                          SATB-PCB SATE-PCB.                              
054600                                                                          
054700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB MSGKOM-PCB                     
054800                          USEA-PCB                                        
054900                           ARTC-PCB                   BENB-PCB            
055000                           BENC-PCB ARTG-PCB ARTH-PCB XXAQ-PCB            
055100                           SATB-PCB SATE-PCB.                             
055200     EJECT                                                                
055300     PERFORM IMS-GET-MSG                                                  
055400     IF SEGMENT-FINNS                                                     
055500        PERFORM IMS-GET-WMSGKOM-MSG                                       
055600        PERFORM A-INIT                                                    
055700        IF WS-IDARTNR NUMERIC                                             
055800           IF MFS-UPDATE OR MFS-UPD-X                                     
055900              PERFORM S13-VISA-BILD-IGEN                                  
056000              IF IDARTNR-WS > 99999999                                    
056100                 MOVE FEL-13 (SPIND) TO MOD-TEMFSFEL                      
056200              ELSE                                                        
056300                 MOVE IDARTNR-WS TO W-IDARTNR                             
056400                 PERFORM IMS-GET-ARTC01                                   
056500                 IF SEGMENT-FINNS                                         
056600                    IF ART-KDERS-UTG > 0                                  
056700                       MOVE IDARTNR-WS TO IDARTNR-NY-WS                   
056800                       MOVE JA TO VAECKNING                               
056900                       PERFORM IMS-GET-ARTG01                             
057000                       IF SEGMENT-FINNS                                   
057100                           PERFORM B-REG-ART-ARTREG-UPPDAT-NYPON          
057200                       ELSE                                               
057300                           PERFORM E-VAECKNING-AV-GAMMAL-ARTIKEL          
057400                       END-IF                                             
057500                    ELSE                                                  
057600                       IF WS-NY-IDARTNR = WS-IDARTNR                      
057700                          CONTINUE                                        
057800                       ELSE                                               
057900                          IF WS-NY-IDARTNR > ZERO                         
058000                             IF IDARTNR-NY-WS > 99999999                  
058100                                MOVE FEL-8 (SPIND) TO MOD-TEMFSFEL        
058200                                MOVE MFS-ADD-SAETT-CURSOR TO              
058300                                      MOD-IDARTNR-ATTR                    
058400                             ELSE                                         
058500                                MOVE IDARTNR-NY-WS TO W-IDARTNR           
058600                                PERFORM IMS-GET-ARTC01                    
058700                                IF SEGMENT-FINNS                          
058800                                   MOVE FEL-3 (SPIND) TO                  
058900                                                      MOD-TEMFSFEL        
059000                                   MOVE MFS-ADD-SAETT-CURSOR TO           
059100                                         MOD-IDARTNR-ATTR                 
059200                                ELSE                                      
059300                                   MOVE JA TO KOPIERING                   
059400                                   MOVE IDARTNR-WS TO W-IDARTNR           
059500                                   PERFORM IMS-GET-ARTG01                 
059600                                   IF SEGMENT-FINNS                       
059700                                     PERFORM                              
059800                                     D-KOP-FRAN-ARTREG-EV-NYPON           
059900                                   ELSE                                   
060000                                     PERFORM C-KOP-FRAN-ARTREG            
060100                                   END-IF                                 
060200                                END-IF                                    
060300                             END-IF                                       
060400                          ELSE                                            
060500                             MOVE FEL-8 (SPIND) TO MOD-TEMFSFEL           
060600                             MOVE MFS-ADD-SAETT-CURSOR TO                 
060700                                    MOD-IDARTNR-ATTR                      
060800                          END-IF                                          
060900                       END-IF                                             
061000                    END-IF                                                
061100                 ELSE                                                     
061200                    PERFORM IMS-GET-ARTG01                                
061300                    IF SEGMENT-FINNS                                      
061400                       PERFORM B-REG-ART-ARTREG-UPPDAT-NYPON              
061500                    ELSE                                                  
061600                       PERFORM F-REG-ARTIKEL-ARTREG-EV-NYPON              
061700                    END-IF                                                
061800                 END-IF                                                   
061900              END-IF                                                      
062000           ELSE                                                           
062100              IF UPPDATERING = JA                                         
062200                 PERFORM S13-VISA-BILD-IGEN                               
062300                 MOVE FEL-7 (SPIND) TO MOD-TEMFSFEL                       
062400              ELSE                                                        
062500                 MOVE IDARTNR-WS TO W-IDARTNR                             
062600                 PERFORM IMS-GET-ARTC01                                   
062700                 IF SEGMENT-FINNS                                         
062800                     MOVE JA                TO FINNS-PA-ARTC              
062900                     IF ART-KDERS-UTG > ZERO                              
063000                        MOVE IDARTNR-WS TO IDARTNR-NY-WS                  
063100                        PERFORM H-LAS-NYPON                               
063200                        IF SEGMENT-FINNS                                  
063300                           PERFORM O-KOLLA-RESBED-ARTUTG                  
063400                           PERFORM M-LAS-BENREG                           
063500                           MOVE MFS-STAENG-FAELT TO                       
063600                                                 MOD-BEART-ATTR           
063700                           MOVE MFS-STAENG-FAELT TO                       
063800                                 MOD-IDARTNR-ATTR                         
063900                        ELSE                                              
064000                           PERFORM K-LAS-ARTREG-NYPON-BENREG              
064100                        END-IF                                            
064200                     ELSE                                                 
064300                        PERFORM K-LAS-ARTREG-NYPON-BENREG                 
064400                     END-IF                                               
064500                 ELSE                                                     
064600                    PERFORM P-KOLLA-RASA                                  
064700                    PERFORM H-LAS-NYPON                                   
064800                    IF SEGMENT-FINNS                                      
064900                       PERFORM O-KOLLA-RESBED-ARTUTG                      
065000                       IF FINNS-RASA = JA                                 
065100                          MOVE FEL-14(SPIND)        TO                    
065200                                                  MOD-TEMFSINF            
065300                       ELSE                                               
065400                          MOVE FEL-2(SPIND)         TO                    
065500                                                  MOD-TEMFSINF            
065600                       END-IF                                             
065700                       MOVE IDARTNR-WS TO IDARTNR-NY-WS                   
065800                    ELSE                                                  
065900                       IF IDARTNR-WS NOT = ZERO                           
066000                         IF FINNS-RASA = JA                               
066100                            MOVE FEL-14 (SPIND) TO MOD-TEMFSFEL           
066200                         ELSE                                             
066300                            MOVE FEL-2 (SPIND) TO MOD-TEMFSFEL            
066400                         END-IF                                           
066500                         MOVE IDARTNR-WS TO IDARTNR-NY-WS                 
066600                       ELSE                                               
066700                         MOVE FEL-8 (SPIND) TO MOD-TEMFSFEL               
066800                       END-IF                                             
066900                    END-IF                                                
067000                 END-IF                                                   
067100              END-IF                                                      
067200           END-IF                                                         
067300        ELSE                                                              
067400           MOVE FEL-1 (SPIND) TO MOD-TEMFSFEL                             
067500           MOVE '020'         TO MSG-KOM-IDMFSMED                         
067600        END-IF                                                            
067700        IF IDARTNR-NY-WS NOT = ZERO                                       
067800           IF EGEN-BILD                                                   
067900           AND NOT MFS-UPD-X                                              
068000              MOVE ALL '+' TO MSGI-WMSGINIT                               
068100              MOVE '001'             TO MSGI-KDCALL                       
068200              MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                       
068300                                        MSGI-IDLTERM-USER                 
068400              MOVE '9410'            TO MSGI-IDTRANS                      
068500              MOVE IDARTNR-NY-WS     TO MSGI-IDARTNR                      
068600              CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                  
068700           END-IF                                                         
068800        END-IF                                                            
068900     END-IF                                                               
069000                                                                          
069100* ABEND PÅ ALLMÄN BEGÄRAN                                                 
069200*    DIVIDE IDARTNR-WS BY WS-NOLL GIVING IDARTNR-WS                       
069300* ABEND PÅ ALLMÄN BEGÄRAN                                                 
069400                                                                          
069500     IF TRANS-TO-1192 = JA                                                
069600        PERFORM Q-FLYTTA-MID-OCH-MOD                                      
069700        IF MFS-UPD-X                                                      
069800*          X-TRANS SKALL TILL 1192 HA IDTRANS=111F                        
069900           MOVE '111F' TO P-IDTRANS                                       
070000                                                                          
070100           MOVE OK-BEHANDLAD TO MSG-KOM-IDMFSMED                          
070200           PERFORM IMS-INSERT-WMSGKOM-MSG                                 
070300        END-IF                                                            
070400        PERFORM IMS-INSERT-ALT                                            
070500     ELSE                                                                 
070600        IF MFS-UPD-X                                                      
070700*          X-TRANS FRÅN DISPATCHERN SKALL INTE SVARA EN SKÄRM             
070800           MOVE FEL-ERR-FIELD     TO MSG-KOM-IDMFSMED                     
070900           PERFORM IMS-INSERT-WMSGKOM-MSG                                 
071000        ELSE                                                              
071100           COMPUTE MSG-KVLL = LENGTH OF MOD-W90410O1 + 4                  
071200*          MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
071300           PERFORM IMS-INSERT-MSG                                         
071400        END-IF                                                            
071500     END-IF                                                               
071600                                                                          
071700     MOVE ZERO TO RETURN-CODE                                             
071800     GOBACK                                                               
071900     .                                                                    
072000     EJECT                                                                
072100 A-INIT SECTION.                                                          
072200     SKIP2                                                                
072300     IF MSG-DUBBLA-TRANSKODER                                             
072400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90410I1                 
072500       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
072600       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
072700                                               P-KDMFSFOR                 
072800     ELSE                                                                 
072900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W90410I1                 
073000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
073100       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
073200     END-IF                                                               
073300     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
073400     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
073500     MOVE MFS-IDTRANS                     TO WS-IDTRANS                   
073600                                                                          
073700     IF MFS-IDTRANS = '9410'                                              
073800     OR MFS-UPD-X                                                         
073900        CONTINUE                                                          
074000*       ******************************************************            
074100*       * X-TRANSEN KOMMER FRÅN RUTIN W100B1 OCH ÄR          *            
074200*       * I FÖRVÄG KONTROLLERAD AV PGM W1116100.             *            
074300*       ******************************************************            
074400     ELSE                                                                 
074500        MOVE SPACE TO MFS-KDTRTYP                                         
074600        MOVE NEJ TO UPPDATERING                                           
074700     END-IF                                                               
074800     IF MFS-IDTRANS = '1142'                                              
074900        MOVE MID-W90410I1  TO 1142-MID-W1I14201                           
075000     END-IF                                                               
075100                                                                          
075200     ACCEPT DAGENS-AAMMDD FROM DATE                                       
075300     MOVE NEJ TO KOPIERING                                                
075400                 TRANS-TO-1192                                            
075500                 VAECKNING                                                
075600                 FINNS-REG-PA-NYPON                                       
075700                                                                          
075800     MOVE LOW-VALUE TO MSG-AREA                                           
075900     MOVE 'W90410O1' TO MFS-IDMOD                                         
076000     MOVE '9410' TO MOD-IDTRANS                                           
076100                                                                          
076200     MOVE MFS-RENSA-FAELT TO                                              
076300                             MOD-TEMFSFEL                                 
076400                             MOD-TEMFSINF                                 
076500     IF MFS-IDPFK = '8'                                                   
076600        MOVE 'IDAG  '                     TO DAT-KDDATFORM                
076700        PERFORM S99-WDATKONV                                              
076800        IF DAT-KDSVAR-OK                                                  
076900           PERFORM AA-BEHANDLA-PFK8                                       
077000        END-IF                                                            
077100     END-IF                                                               
077200     IF MFS-IDTRANS = '1142'                                              
077300        PERFORM AB-BEHANDLA-1142-TRANS                                    
077400     END-IF                                                               
077500                                                                          
077600     IF MFS-UPD-X                                                         
077700****************  DISPATCHANROP                                           
077800                                                                          
077900       IF MID-IDARTNR-IN = ALL '+' OR SPACE                               
078000          INSPECT MID-IDARTNR-UT REPLACING LEADING                        
078100                      SPACE BY ZERO                                       
078200          MOVE MID-IDARTNR-UT TO WS-IDARTNR                               
078300          IF EGEN-BILD                                                    
078400             MOVE JA TO UPPDATERING                                       
078500          END-IF                                                          
078600       ELSE                                                               
078700          MOVE MID-IDARTNR-IN TO WS-IDARTNR                               
078800          MOVE SPACE TO MFS-KDTRTYP                                       
078900          MOVE NEJ TO UPPDATERING                                         
079000       END-IF                                                             
079100                                                                          
079200     ELSE                                                                 
079300       IF MID-IDARTNR-IN = ALL '+'                                        
079400          IF EGEN-BILD                                                    
079500             MOVE JA TO UPPDATERING                                       
079600          END-IF                                                          
079700       ELSE                                                               
079800          MOVE SPACE       TO MFS-KDTRTYP                                 
079900          MOVE NEJ TO UPPDATERING                                         
080000       END-IF                                                             
080100       MOVE ALL '+' TO MSGI-WMSGINIT                                      
080200       MOVE '001'             TO MSGI-KDCALL                              
080300       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
080400                                 MSGI-IDLTERM-USER                        
080500       MOVE '9410'            TO MSGI-IDTRANS                             
080600       IF EGEN-BILD                                                       
080700         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
080800       END-IF                                                             
080900                                                                          
081000       IF MFS-IDTRANS = '1142'                                            
081100          IF MID-IDARTNR-IN NUMERIC                                       
081200             MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                          
081300          END-IF                                                          
081400       END-IF                                                             
081500                                                                          
081600       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
081700       MOVE MSGI-IDDC         TO WS-IDDC                                  
081800       IF MFS-QUERY                                                       
081900         MOVE MSGI-IDARTNR TO WS-IDARTNR                                  
082000       ELSE                                                               
082100         MOVE MID-IDARTNR-UT TO WS-IDARTNR                                
082200       END-IF                                                             
082300       INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                     
082400     END-IF                                                               
082500                                                                          
082600     IF MID-IDARTNR-NY = ALL '+' OR SPACE                                 
082700        MOVE ZERO TO WS-NY-IDARTNR                                        
082800     ELSE                                                                 
082900        INSPECT MID-IDARTNR-NY REPLACING LEADING SPACE BY ZERO            
083000        IF MID-IDARTNR-NY NUMERIC                                         
083100          MOVE MID-IDARTNR-NY TO WS-NY-IDARTNR                            
083200        ELSE                                                              
083300          MOVE ZERO TO WS-NY-IDARTNR                                      
083400        END-IF                                                            
083500     END-IF                                                               
083600                                                                          
083700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
083800        MOVE +1 TO SPIND                                                  
083900     ELSE                                                                 
084000        MOVE +2 TO SPIND                                                  
084100     END-IF                                                               
084200     .                                                                    
084300     EJECT                                                                
084400 AA-BEHANDLA-PFK8 SECTION.                                                
084500     SKIP2                                                                
084600     MOVE DAT-TIAA-VECKA            TO DAGENS-AA                          
084700     MOVE DAT-TIVV                  TO DAGENS-VV                          
084800     INSPECT MID-IDARTNR-UT REPLACING LEADING SPACE BY                    
084900                                                  ZERO                    
085000     IF  MID-IDARTNR-UT    NUMERIC                                        
085100        MOVE MID-IDARTNR-UT                    TO                         
085200                                          W-IDARTNR-MIN                   
085300        PERFORM IMS-GN-ARTH01                                             
085400        MOVE NEJ                               TO SW-TRAEFF               
085500        PERFORM UNTIL SEGMENT-SAKNAS                                      
085600        OR            TRAEFF                                              
085700           MOVE ARTH01-SEQA-IDARTNR            TO W-IDARTNR-1142          
085800           PERFORM IMS-GU-ARTG01                                          
085900           IF ARTG01-ART-TINEDBRY > ZERO                                  
086000              MOVE 'AAMMDD'                     TO DAT-KDDATFORM          
086100              MOVE ARTG01-ART-TINEDBRY          TO DAT-I-TIDATUM          
086200              PERFORM S99-WDATKONV                                        
086300              IF DAT-KDSVAR-OK                                            
086400                 MOVE DAT-TIAA-VECKA            TO TINEDB-AA              
086500                 MOVE DAT-TIVV                  TO TINEDB-VV              
086600                 MOVE DAGENS-AAVV   TO TMP1-YYWW                          
086700                 MOVE TINEDB-AAVV   TO TMP2-YYWW                          
086800                 PERFORM WY2000P3                                         
086900                 IF TMP1-YYWW >= TMP2-YYWW                                
087000                    MOVE JA                     TO SW-TRAEFF              
087100                 ELSE                                                     
087200                    PERFORM IMS-GN-ARTH01                                 
087300                 END-IF                                                   
087400              END-IF                                                      
087500           ELSE                                                           
087600              MOVE JA                     TO SW-TRAEFF                    
087700           END-IF                                                         
087800        END-PERFORM                                                       
087900        IF TRAEFF                                                         
088000           MOVE ARTG01-ART-IDARTNR     TO MID-IDARTNR-IN                  
088100        ELSE                                                              
088200           MOVE MID-IDARTNR-UT                 TO                         
088300                                           MID-IDARTNR-IN                 
088400           MOVE FEL-12 (2)                     TO                         
088500                                             MOD-TEMFSFEL                 
088600        END-IF                                                            
088700     END-IF                                                               
088800     .                                                                    
088900     EJECT                                                                
089000 AB-BEHANDLA-1142-TRANS    SECTION.                                       
089100     SKIP2                                                                
089200     MOVE +1                                TO RAD-INDX                   
089300     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
089400        IF 1142-MID-SELECT-ARTIKEL (RAD-INDX) = ALL '+'                   
089500           ADD +1                           TO RAD-INDX                   
089600        ELSE                                                              
089700           INSPECT 1142-MID-IDARTNR (RAD-INDX) REPLACING                  
089800                                   LEADING SPACE BY ZERO                  
089900           MOVE 1142-MID-IDARTNR (RAD-INDX) TO                            
090000                                        MID-IDARTNR-IN                    
090100           MOVE MAX-RAD-PLUS-2              TO RAD-INDX                   
090200        END-IF                                                            
090300     END-PERFORM                                                          
090400     .                                                                    
090500     EJECT                                                                
090600 B-REG-ART-ARTREG-UPPDAT-NYPON SECTION.                                   
090700     SKIP2                                                                
090800     IF IDARTNR-WS = IDARTNR-NY-WS                                        
090900        MOVE JA TO INPUT-RETT                                             
091000        PERFORM S02-NYA-ARTREG-DATAELEMENT                                
091100                                                                          
091200        MOVE IDARTNR-WS TO W-IDARTNR                                      
091300        PERFORM IMS-GET-ARTG01                                            
091400                                                                          
091500        IF MID-KDPRODSL = ALL '+'                                         
091600           IF ARTG01-ART-KDPRODSL = ZERO                                  
091700              MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                 
091800              MOVE NEJ TO INPUT-RETT                                      
091900           END-IF                                                         
092000           MOVE ARTG01-ART-KDPRODSL TO WS-KDPRODSL                        
092100           PERFORM S94-KOLLA-IDDC                                         
092200        ELSE                                                              
092300           MOVE ZERO TO WS-KDPRODSL                                       
092400           IF MID-KDPRODSL NUMERIC                                        
092500              MOVE MID-KDPRODSL   TO TEST-KDPRODSL                        
092600              IF GOOD-KDPRODSL                                            
092700                 MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-ATTR            
092800                 MOVE MID-KDPRODSL TO WS-KDPRODSL                         
092900                 PERFORM S94-KOLLA-IDDC                                   
093000              ELSE                                                        
093100                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR              
093200                 MOVE NEJ TO INPUT-RETT                                   
093300              END-IF                                                      
093400           ELSE                                                           
093500              MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                 
093600              MOVE NEJ TO INPUT-RETT                                      
093700           END-IF                                                         
093800        END-IF                                                            
093900                                                                          
094000        IF INPUT-RETT = JA                                                
094100           PERFORM S95-KOLLA-IDFTG                                        
094200        END-IF                                                            
094300                                                                          
094400        PERFORM S06-KOP-NYPON-DATAELEMENT                                 
094500                                                                          
094600        MOVE ZERO TO WS-IDPROENH(1) WS-IDPROENH(2) WS-IDPROENH(3)         
094700                                                                          
094800        INSPECT MID-BEART REPLACING ALL '<' BY SPACE                      
094900        INSPECT MID-BEART REPLACING ALL '>' BY SPACE                      
095000                                                                          
095100        IF MID-BEART = ALL '+' OR SPACE                                   
095200           IF VAECKNING = JA                                              
095300              MOVE IDARTNR-WS TO W-IDARTNR                                
095400              MOVE 'S  ' TO W-IDSKYLT                                     
095500              PERFORM IMS-GET-BENA11-CSEQ                                 
095600              MOVE BENA-TEXT-BEART TO WS-BEART  WS-BEART-SVE              
095700           ELSE                                                           
095800              IF ARTG01-ART-BEART-SVE = SPACE                             
095900                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR                
096000                 MOVE NEJ TO INPUT-RETT                                   
096100              ELSE                                                        
096200                 MOVE ARTG01-ART-BEART-SVE TO WS-BEART                    
096300              END-IF                                                      
096400           END-IF                                                         
096500        ELSE                                                              
096600           IF VAECKNING = JA                                              
096700              MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR                   
096800              MOVE NEJ TO INPUT-RETT                                      
096900           ELSE                                                           
097000              MOVE MID-BEART TO WS-BEART                                  
097100              MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-ATTR                 
097200           END-IF                                                         
097300        END-IF                                                            
097400                                                                          
097500        IF MID-IDRITN = ALL '+'                                           
097600           IF ARTG01-ART-IDRITN = SPACE                                   
097700              MOVE NEJ TO INPUT-RETT                                      
097800              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                  
097900           END-IF                                                         
098000        ELSE                                                              
098100           IF MID-IDRITN = SPACE                                          
098200              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                  
098300              MOVE NEJ TO INPUT-RETT                                      
098400           ELSE                                                           
098500              MOVE MID-IDRITN TO WS-IDRITN                                
098600              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDRITN-ATTR                
098700           END-IF                                                         
098800        END-IF                                                            
098900                                                                          
099000        IF MID-IDPROJ = ALL '+'                                           
099100           MOVE ARTG01-ART-IDPROJ TO WS-IDPROJ                            
099200        ELSE                                                              
099300           MOVE MID-IDPROJ TO WS-IDPROJ                                   
099400        END-IF                                                            
099500                                                                          
099600        PERFORM S14-GODK-PROJ-MFS-RAETT-FEL                               
099700                                                                          
099800        IF MID-IDPROENH-1 = ALL '+'                                       
099900           CONTINUE                                                       
100000        ELSE                                                              
100100           IF MID-IDPROENH-1 NUMERIC                                      
100200              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-1-ATTR             
100300           ELSE                                                           
100400              MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-1-ATTR             
100500              MOVE NEJ TO INPUT-RETT                                      
100600           END-IF                                                         
100700        END-IF                                                            
100800        IF MID-IDPROENH-2 = ALL '+'                                       
100900           CONTINUE                                                       
101000        ELSE                                                              
101100           IF MID-IDPROENH-2 NUMERIC                                      
101200              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-2-ATTR             
101300           ELSE                                                           
101400              MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-2-ATTR             
101500              MOVE NEJ TO INPUT-RETT                                      
101600           END-IF                                                         
101700        END-IF                                                            
101800        IF MID-IDPROENH-3 = ALL '+'                                       
101900           CONTINUE                                                       
102000        ELSE                                                              
102100           IF MID-IDPROENH-3 NUMERIC                                      
102200              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-3-ATTR             
102300           ELSE                                                           
102400              MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-3-ATTR             
102500              MOVE NEJ TO INPUT-RETT                                      
102600           END-IF                                                         
102700        END-IF                                                            
102800                                                                          
102900        IF MID-IDBERED = ALL '+'                                          
103000           IF ARTG01-ART-IDBERED = ZERO                                   
103100              MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                  
103200              MOVE NEJ TO INPUT-RETT                                      
103300           END-IF                                                         
103400        ELSE                                                              
103500           IF MID-IDBERED NUMERIC                                         
103600              IF MID-IDBERED > ZERO                                       
103700                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDBERED-ATTR             
103800              ELSE                                                        
103900                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR               
104000                 MOVE NEJ TO INPUT-RETT                                   
104100              END-IF                                                      
104200           ELSE                                                           
104300              MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                  
104400              MOVE NEJ TO INPUT-RETT                                      
104500           END-IF                                                         
104600        END-IF                                                            
104700                                                                          
104800        IF MID-KDSORT = ALL '+'                                           
104900           IF ARTG01-ART-KDSORT = SPACE                                   
105000              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                  
105100              MOVE NEJ TO INPUT-RETT                                      
105200           ELSE                                                           
105300              MOVE ARTG01-ART-KDSORT TO WS-KDSORT                         
105400           END-IF                                                         
105500        ELSE                                                              
105600           IF MID-KDSORT =    'ST' OR 'SA' OR 'KG' OR 'M '                
105700           OR ' M' OR ' L' OR 'L ' OR 'MM' OR 'G ' OR ' G'                
105800           OR 'C2' OR 'M2' OR 'ML' OR 'SW' OR 'TM' OR 'HW'                
105900           OR 'PA'                                                        
106000              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-ATTR                
106100              MOVE MID-KDSORT TO WS-KDSORT                                
106200           ELSE                                                           
106300              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                  
106400              MOVE NEJ TO INPUT-RETT                                      
106500           END-IF                                                         
106600        END-IF                                                            
106700                                                                          
106800        IF MID-IDAO = ALL '+' OR SPACE                                    
106900           IF ARTG01-ART-IDAO = SPACE                                     
107000              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDAO-ATTR                    
107100              MOVE NEJ TO INPUT-RETT                                      
107200           END-IF                                                         
107300        ELSE                                                              
107400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-ATTR                     
107500        END-IF                                                            
107600                                                                          
107700        MOVE MID-TISOP TO XX-TISOP                                        
107800        MOVE '+' TO XX-DAG                                                
107900        IF XX-TISOP = ALL '+'                                             
108000           PERFORM S97-LAES-XXAQ                                          
108100           IF SEGMENT-FINNS                                               
108200              IF  XXAQ-1132-TIFINLEV = ZERO                               
108300              AND XXAQ-1132-TIPRODSTA = +111111                           
108400                 PERFORM HA-PLOCKA-NYASTE-TISERLEV                        
108500                 IF W-SPAR-TISERLEV = +9999999                            
108600                    MOVE ARTG01-ART-KDPRODSL  TO TEST-KDPRODSL            
108700                    IF KDPRODSL-ACC                                       
108800                    OR ARTG01-ART-FLUNIKRD = JA                           
108900                    OR ARTG01-ART-IDLEVNR  = '9998 '                      
109000                       MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR           
109100                       MOVE NEJ               TO INPUT-RETT               
109200                    ELSE                                                  
109300                       MOVE +999999           TO WS-TISOP-AAMMDD          
109400                       MOVE MFS-NUM-FAELT-RAETT                           
109500                                              TO MOD-TISOP-ATTR           
109600                    END-IF                                                
109700                 ELSE                                                     
109800                    MOVE W-SPAR-TISERLEV   TO TMP1-YYMMDD                 
109900                    MOVE DAGENS-AAMMDD     TO TMP2-YYMMDD                 
110000                    PERFORM WY2000P1                                      
110100                    IF TMP1-YYMMDD > TMP2-YYMMDD                          
110200                       MOVE W-SPAR-TISERLEV  TO WS-TISOP-AAMMDD           
110300                       MOVE MFS-NUM-FAELT-RAETT TO MOD-TISOP-ATTR         
110400                    ELSE                                                  
110500                       MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR           
110600                       MOVE NEJ               TO INPUT-RETT               
110700                    END-IF                                                
110800                 END-IF                                                   
110900              ELSE                                                        
111000                 MOVE XXAQ-1132-TIFINLEV   TO TMP1-YYMMDD                 
111100                 MOVE DAGENS-AAMMDD        TO TMP2-YYMMDD                 
111200                 PERFORM WY2000P1                                         
111300                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
111400                    MOVE XXAQ-1132-TIFINLEV  TO WS-TISOP-AAMMDD           
111500                    MOVE MFS-NUM-FAELT-RAETT TO MOD-TISOP-ATTR            
111600                 ELSE                                                     
111700                    MOVE MFS-NUM-FAELT-FEL   TO MOD-TISOP-ATTR            
111800                    MOVE NEJ                 TO INPUT-RETT                
111900                 END-IF                                                   
112000              END-IF                                                      
112100           ELSE                                                           
112200              PERFORM HA-PLOCKA-NYASTE-TISERLEV                           
112300              IF W-SPAR-TISERLEV = +9999999                               
112400                 MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                 
112500                 MOVE NEJ               TO INPUT-RETT                     
112600              ELSE                                                        
112700                 MOVE W-SPAR-TISERLEV   TO TMP1-YYMMDD                    
112800                 MOVE DAGENS-AAMMDD     TO TMP2-YYMMDD                    
112900                 PERFORM WY2000P1                                         
113000                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
113100                    MOVE W-SPAR-TISERLEV    TO WS-TISOP-AAMMDD            
113200                    MOVE MFS-NUM-FAELT-RAETT TO MOD-TISOP-ATTR            
113300                 ELSE                                                     
113400                    MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR              
113500                    MOVE NEJ               TO INPUT-RETT                  
113600                 END-IF                                                   
113700              END-IF                                                      
113800           END-IF                                                         
113900        ELSE                                                              
114000           IF  XX-AAR = '99'                                              
114100           AND XX-VECKA = '99'                                            
114200              MOVE '9' TO XX-DAG                                          
114300           ELSE                                                           
114400              MOVE '1' TO XX-DAG                                          
114500           END-IF                                                         
114600                                                                          
114700           IF XX-TISOP NUMERIC                                            
114800              MOVE XX-TISOP TO WS-TISOP                                   
114900              IF WS-TISOP = 99999                                         
115000                 MOVE WS-KDPRODSL             TO TEST-KDPRODSL            
115100                 IF KDPRODSL-PARTS-ACC OR KDPRODSL-SERVICES               
115200                    MOVE ARTG01-ART-KDPRODSL                              
115300                                 TO TEST-KDPRODSL                         
115400                    IF KDPRODSL-ACC                                       
115500                    OR ARTG01-ART-FLUNIKRD = JA                           
115600                    OR ARTG01-ART-IDLEVNR  = '9998 '                      
115700                       MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR           
115800                       MOVE NEJ               TO INPUT-RETT               
115900                    ELSE                                                  
116000                       PERFORM S97-LAES-XXAQ                              
116100                       IF SEGMENT-FINNS                                   
116200                          IF XXAQ-1132-TIFINLEV  = ZERO                   
116300                          AND XXAQ-1132-TIPRODSTA = +111111               
116400                             PERFORM HA-PLOCKA-NYASTE-TISERLEV            
116500                             IF W-SPAR-TISERLEV = +9999999                
116600                                MOVE MFS-NUM-FAELT-RAETT TO               
116700                                          MOD-TISOP-ATTR                  
116800                                MOVE WS-TISOP TO AAVVD                    
116900                                MOVE +9 TO D                              
117000                                MOVE AAVVD TO WS-TISOP                    
117100                             ELSE                                         
117200                                MOVE MFS-NUM-FAELT-FEL   TO               
117300                                            MOD-TISOP-ATTR                
117400                                MOVE NEJ TO INPUT-RETT                    
117500                             END-IF                                       
117600                          ELSE                                            
117700                             MOVE MFS-NUM-FAELT-FEL   TO                  
117800                                         MOD-TISOP-ATTR                   
117900                             MOVE NEJ TO INPUT-RETT                       
118000                          END-IF                                          
118100                       ELSE                                               
118200                          MOVE MFS-NUM-FAELT-FEL      TO                  
118300                                          MOD-TISOP-ATTR                  
118400                          MOVE NEJ TO INPUT-RETT                          
118500                       END-IF                                             
118600                    END-IF                                                
118700                 ELSE                                                     
118800                    MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR              
118900                    MOVE NEJ TO INPUT-RETT                                
119000                 END-IF                                                   
119100              ELSE                                                        
119200                 MOVE WS-TISOP    TO DAT-I-TIDATUM                        
119300                 MOVE 'AAVVD '    TO DAT-KDDATFORM                        
119400                 PERFORM S99-WDATKONV                                     
119500                 IF DAT-KDSVAR-OK                                         
119600                    PERFORM S98-OM-TVA-AAR                                
119700                    MOVE WS-TISOP     TO TMP1-YYWWD                       
119800                    MOVE AAVVD        TO TMP2-YYWWD                       
119900                    MOVE DAT-TIAAVVD  TO TMP3-YYWWD                       
120000                    PERFORM WY2000Q2                                      
120100                    IF TMP1-YYWWD < TMP2-YYWWD                            
120200                       MOVE MFS-NUM-FAELT-RAETT TO MOD-TISOP-ATTR         
120300                       MOVE WS-TISOP TO AAVVD                             
120400                       MOVE +1 TO D                                       
120500                       MOVE AAVVD TO WS-TISOP                             
120600                       IF TMP1-YYWWD > TMP3-YYWWD                         
120700                         MOVE ZERO     TO WS-YYWWD-VECKA-PLUS-1           
120800                       ELSE                                               
120900*      TIFINLV SKALL SÄTTAS TILL DAGENS VECKA + 1                         
121000*      SPARA DETTA I WS-YYWWD-VECKA-PLUS-1                                
121100                         MOVE DAT-TIAAVVD TO                              
121200                                 WS-YYWWD-VECKA-PLUS-1                    
121300                         IF WS-WW < 52                                    
121400                            ADD   1    TO WS-WW                           
121500                         ELSE                                             
121600                            ADD   1    TO WS-YY                           
121700                            MOVE 01    TO WS-WW                           
121800                         END-IF                                           
121900                         MOVE     1    TO WS-D                            
122000                       END-IF                                             
122100                    END-IF                                                
122200                 ELSE                                                     
122300                    MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR              
122400                    MOVE NEJ TO INPUT-RETT                                
122500                 END-IF                                                   
122600              END-IF                                                      
122700           ELSE                                                           
122800              MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                    
122900              MOVE NEJ TO INPUT-RETT                                      
123000           END-IF                                                         
123100        END-IF                                                            
123200                                                                          
123300        IF MID-IDFKNGRP = ALL '+'                                         
123400           IF ARTG01-ART-IDFKNGRP = ZERO                                  
123500              MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                 
123600              MOVE NEJ TO INPUT-RETT                                      
123700           ELSE                                                           
123800              MOVE ARTG01-ART-IDFKNGRP TO WS-TEST-IDFKNGRP                
123900           END-IF                                                         
124000        ELSE                                                              
124100           IF MID-IDFKNGRP NUMERIC                                        
124200              IF MID-IDFKNGRP > ZERO                                      
124300                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-ATTR            
124400                 MOVE MID-IDFKNGRP TO WS-TEST-IDFKNGRP                    
124500              ELSE                                                        
124600                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR              
124700                 MOVE NEJ TO INPUT-RETT                                   
124800              END-IF                                                      
124900           ELSE                                                           
125000              MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                 
125100              MOVE NEJ TO INPUT-RETT                                      
125200           END-IF                                                         
125300        END-IF                                                            
125400                                                                          
125500        IF MID-TEARTNOT-2 = ALL '+'                                       
125600           CONTINUE                                                       
125700        ELSE                                                              
125800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-2-ATTR               
125900        END-IF                                                            
126000                                                                          
126100        IF MID-TEARTNOT-7 = ALL '+'                                       
126200           CONTINUE                                                       
126300        ELSE                                                              
126400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-7-ATTR               
126500        END-IF                                                            
126600                                                                          
126700        PERFORM S23-KOLLA-RASA                                            
126800                                                                          
126900        IF INPUT-RETT = JA                                                
127000           PERFORM S93-KOLLA-SOFTWARE                                     
127100        END-IF                                                            
127200                                                                          
127300        IF VAECKNING = NEJ                                                
127400           IF INPUT-RETT = JA                                             
127500              PERFORM S07-KOLLA-OM-BEN-FINNS                              
127600              IF INPUT-RETT = JA                                          
127700                 PERFORM BA-UPPDATERA-NYPON                               
127800                 MOVE MED-1(SPIND)TO MOD-TEMFSINF                         
127900                 MOVE JA TO TRANS-TO-1192                                 
128000              ELSE                                                        
128100                 MOVE FEL-4(SPIND) TO MOD-TEMFSFEL                        
128200              END-IF                                                      
128300           ELSE                                                           
128400              MOVE FEL-4(SPIND) TO MOD-TEMFSFEL                           
128500           END-IF                                                         
128600        ELSE                                                              
128700           IF INPUT-RETT = JA                                             
128800              MOVE FEL-3 (SPIND) TO MOD-TEMFSFEL                          
128900              MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-ATTR                   
129000                                       MOD-BEART-ATTR                     
129100           ELSE                                                           
129200              MOVE FEL-4 (SPIND)    TO MOD-TEMFSFEL                       
129300              MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-ATTR                   
129400                                       MOD-BEART-ATTR                     
129500           END-IF                                                         
129600        END-IF                                                            
129700     END-IF                                                               
129800     .                                                                    
129900     EJECT                                                                
130000 BA-UPPDATERA-NYPON SECTION.                                              
130100     SKIP2                                                                
130200     MOVE IDARTNR-WS TO W-IDARTNR                                         
130300     PERFORM IMS-GET-ARTG01                                               
130400     IF MID-IDPROJ = ALL '+'                                              
130500        CONTINUE                                                          
130600     ELSE                                                                 
130700        MOVE MID-IDPROJ        TO ARTG01-ART-IDPROJ                       
130800     END-IF                                                               
130900     IF MID-IDAO   = ALL '+'                                              
131000        CONTINUE                                                          
131100     ELSE                                                                 
131200        MOVE MID-IDAO          TO ARTG01-ART-IDAO                         
131300     END-IF                                                               
131400     IF MID-FLPISK = ALL '+'                                              
131500        CONTINUE                                                          
131600     ELSE                                                                 
131700          MOVE WS-FLPISK       TO ARTG01-ART-FLPISK                       
131800     END-IF                                                               
131900     IF MID-KVARTVAGN = ALL '+'                                           
132000        CONTINUE                                                          
132100     ELSE                                                                 
132200        MOVE WS-KVARTVAGN    TO ARTG01-ART-KVARTVAGN                      
132300     END-IF                                                               
132400                                                                          
132500     IF MID-IDPROJK = ALL '+'                                             
132600        CONTINUE                                                          
132700     ELSE                                                                 
132800        MOVE WS-IDPROJK      TO ARTG01-ART-IDPROJK                        
132900     END-IF                                                               
133000                                                                          
133100     IF MID-IDARTNR-MOTSV = ALL '+'                                       
133200        CONTINUE                                                          
133300     ELSE                                                                 
133400        MOVE WS-IDARTNR-MOTSV TO ARTG01-ART-IDARTNR-MOTSV                 
133500     END-IF                                                               
133600                                                                          
133700     IF MID-TEORSAK-1 = ALL '+'                                           
133800        CONTINUE                                                          
133900     ELSE                                                                 
134000        MOVE MID-TEORSAK-1 TO ARTG01-ART-TEORSAK                          
134100     END-IF                                                               
134200     IF MID-KVPROG = ALL '+'                                              
134300        CONTINUE                                                          
134400     ELSE                                                                 
134500        MOVE WS-KVPROG       TO ARTG01-ART-KVPROG                         
134600     END-IF                                                               
134700                                                                          
134800     IF XX-TISOP = ALL '+'                                                
134900        MOVE WS-TISOP-AAMMDD         TO DAT-I-TIDATUM                     
135000        MOVE 'AAMMDD'                TO DAT-KDDATFORM                     
135100        PERFORM S99-WDATKONV                                              
135200        IF DAT-KDSVAR-OK                                                  
135300           MOVE WS-TISOP-AAMMDD    TO ARTG01-ART-DAFINLEV                 
135400           MOVE DAT-TISEKEL        TO ARTG01-ART-DAFINLEV (1:2)           
135500        END-IF                                                            
135600        IF  WS-TISOP-AAMMDD = 999999                                      
135700           MOVE 99999                   TO MID-TISOP                      
135800        ELSE                                                              
135900           IF DAT-KDSVAR-OK                                               
136000              MOVE DAT-TIAAVVD          TO MID-TISOP                      
136100           END-IF                                                         
136200        END-IF                                                            
136300     ELSE                                                                 
136400        IF WS-TISOP = 99999                                               
136500           MOVE 99999999             TO ARTG01-ART-DAFINLEV               
136600        ELSE                                                              
136700           MOVE WS-TISOP                TO DAT-I-TIDATUM                  
136800           MOVE 'AAVVD '                TO DAT-KDDATFORM                  
136900           PERFORM S99-WDATKONV                                           
137000           IF DAT-KDSVAR-OK                                               
137100              MOVE DAT-TIAAMMDD    TO ARTG01-ART-DAFINLEV                 
137200              MOVE DAT-TISEKEL     TO ARTG01-ART-DAFINLEV (1:2)           
137300           END-IF                                                         
137400        END-IF                                                            
137500     END-IF                                                               
137600                                                                          
137700     MOVE ARTG01-ART-TEORSAK         TO MOD-TEORSAK-1                     
137800     MOVE ARTG01-ART-FLPISK          TO MOD-FLPISK-UT                     
137900     MOVE ARTG01-ART-IDPROJK         TO MOD-IDPROJK-UT                    
138000     MOVE ARTG01-ART-KVARTVAGN       TO MOD-KVARTVAGN-UT                  
138100     MOVE ARTG01-ART-IDARTNR-MOTSV   TO MOD-IDARTNR-MOTSV-UT              
138200     PERFORM IMS-REPL-ARTG                                                
138300     .                                                                    
138400     EJECT                                                                
138500 C-KOP-FRAN-ARTREG SECTION.                                               
138600     SKIP2                                                                
138700******************************************************************        
138800*                                                                         
138900*  ARTIKEL MAN KOPIERAR IFRÅN FINNS INTE PÅ NYPON-BASEN                   
139000*  OCH DÄRMED SKER KOPIERING BARA FRÅN ARTREG.                            
139100*                                                                         
139200******************************************************************        
139300                                                                          
139400     PERFORM S09-KOLLA-INDATA-KOP-ARTREG                                  
139500     IF INPUT-RETT = JA                                                   
139600            PERFORM S07-KOLLA-OM-BEN-FINNS                                
139700            IF INPUT-RETT = JA                                            
139800                IF NYPON-ARTIKEL = JA                                     
139900                   PERFORM CA-REG-UPPDAT-NYPON-KOP                        
140000                END-IF                                                    
140100                MOVE MED-1(SPIND)TO MOD-TEMFSINF                          
140200                MOVE JA TO TRANS-TO-1192                                  
140300            ELSE                                                          
140400                MOVE FEL-4 (SPIND) TO MOD-TEMFSFEL                        
140500            END-IF                                                        
140600     ELSE                                                                 
140700        MOVE FEL-4 (SPIND) TO MOD-TEMFSFEL                                
140800     END-IF                                                               
140900     .                                                                    
141000     EJECT                                                                
141100 CA-REG-UPPDAT-NYPON-KOP SECTION.                                         
141200     SKIP2                                                                
141300     MOVE IDARTNR-NY-WS TO W-IDARTNR                                      
141400     PERFORM IMS-GET-ARTG01                                               
141500                                                                          
141600     IF SEGMENT-FINNS                                                     
141700        MOVE MID-IDPROJ              TO ARTG01-ART-IDPROJ                 
141800        MOVE MID-IDAO                TO ARTG01-ART-IDAO                   
141900        IF MID-TEORSAK-1 = ALL '+'                                        
142000           CONTINUE                                                       
142100        ELSE                                                              
142200           MOVE MID-TEORSAK-1        TO ARTG01-ART-TEORSAK                
142300        END-IF                                                            
142400        IF MID-FLPISK = ALL '+'                                           
142500           CONTINUE                                                       
142600        ELSE                                                              
142700           MOVE WS-FLPISK            TO ARTG01-ART-FLPISK                 
142800        END-IF                                                            
142900        IF MID-IDPROJK = ALL '+'                                          
143000           CONTINUE                                                       
143100        ELSE                                                              
143200           MOVE WS-IDPROJK           TO ARTG01-ART-IDPROJK                
143300        END-IF                                                            
143400        IF MID-KVPROG = ALL '+'                                           
143500           CONTINUE                                                       
143600        ELSE                                                              
143700           MOVE WS-KVPROG            TO ARTG01-ART-KVPROG                 
143800        END-IF                                                            
143900                                                                          
144000        IF MID-KVARTVAGN = ALL '+'                                        
144100           CONTINUE                                                       
144200        ELSE                                                              
144300           MOVE WS-KVARTVAGN         TO ARTG01-ART-KVARTVAGN              
144400        END-IF                                                            
144500                                                                          
144600        IF MID-IDARTNR-MOTSV = ALL '+'                                    
144700           CONTINUE                                                       
144800        ELSE                                                              
144900           MOVE WS-IDARTNR-MOTSV     TO ARTG01-ART-IDARTNR-MOTSV          
145000        END-IF                                                            
145100                                                                          
145200        IF XX-TISOP = ALL '+'                                             
145300           IF ART-TISOP = 99999                                           
145400              MOVE 99999999          TO ARTG01-ART-DAFINLEV               
145500           ELSE                                                           
145600              MOVE 'AAVVD ' TO DAT-KDDATFORM                              
145700              MOVE ART-TISOP TO DAT-I-TIDATUM                             
145800              PERFORM S99-WDATKONV                                        
145900              IF DAT-KDSVAR-OK                                            
146000                 MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV               
146100                 MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)         
146200              END-IF                                                      
146300           END-IF                                                         
146400        ELSE                                                              
146500           IF WS-TISOP = 99999                                            
146600              MOVE 99999999          TO ARTG01-ART-DAFINLEV               
146700           ELSE                                                           
146800              MOVE 'AAVVD ' TO DAT-KDDATFORM                              
146900              MOVE WS-TISOP TO DAT-I-TIDATUM                              
147000              PERFORM S99-WDATKONV                                        
147100              IF DAT-KDSVAR-OK                                            
147200                 MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV               
147300                 MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)         
147400              END-IF                                                      
147500           END-IF                                                         
147600        END-IF                                                            
147700                                                                          
147800        PERFORM IMS-REPL-ARTG                                             
147900                                                                          
148000        MOVE ARTG01-ART-TEORSAK       TO MOD-TEORSAK-1                    
148100        MOVE ARTG01-ART-FLPISK        TO MOD-FLPISK-UT                    
148200        MOVE ARTG01-ART-IDPROJK       TO MOD-IDPROJK-UT                   
148300        MOVE ARTG01-ART-KVARTVAGN     TO MOD-KVARTVAGN-UT                 
148400        MOVE ARTG01-ART-IDARTNR-MOTSV TO MOD-IDARTNR-MOTSV-UT             
148500                                                                          
148600     ELSE                                                                 
148700        MOVE IDARTNR-NY-WS           TO ARTG01-ART-IDARTNR                
148800        MOVE WS-BEART-SVE            TO ARTG01-ART-BEART-SVE              
148900        MOVE WS-FLBYTES              TO ARTG01-ART-FLBYTES                
149000        MOVE WS-FLPISK               TO ARTG01-ART-FLPISK                 
149100        MOVE WS-IDPROJK              TO ARTG01-ART-IDPROJK                
149200        MOVE WS-KVARTVAGN            TO ARTG01-ART-KVARTVAGN              
149300        MOVE WS-KVPROG               TO ARTG01-ART-KVPROG                 
149400        MOVE WS-IDARTNR-MOTSV        TO ARTG01-ART-IDARTNR-MOTSV          
149500                                                                          
149600       IF MID-TEORSAK-1 = ALL '+'                                         
149700           MOVE SPACE                TO ARTG01-ART-TEORSAK                
149800       ELSE                                                               
149900           MOVE MID-TEORSAK-1        TO ARTG01-ART-TEORSAK                
150000       END-IF                                                             
150100                                                                          
150200       MOVE ARTG01-ART-TEORSAK       TO MOD-TEORSAK-1                     
150300       MOVE ARTG01-ART-FLPISK        TO MOD-FLPISK-UT                     
150400       MOVE ARTG01-ART-IDPROJK       TO MOD-IDPROJK-UT                    
150500       MOVE ARTG01-ART-KVARTVAGN     TO MOD-KVARTVAGN-UT                  
150600       MOVE ARTG01-ART-IDARTNR-MOTSV TO MOD-IDARTNR-MOTSV-UT              
150700                                                                          
150800       PERFORM S19-KOP-FRAN-ARTREG-TILL-NYPON                             
150900       PERFORM S21-NOLLSTAELL-NYPON                                       
151000                                                                          
151100       PERFORM IMS-ISRT-ARTG01                                            
151200     END-IF                                                               
151300     .                                                                    
151400     EJECT                                                                
151500 D-KOP-FRAN-ARTREG-EV-NYPON SECTION.                                      
151600     SKIP2                                                                
151700******************************************************************        
151800*                                                                         
151900*  ARTIKEL MAN KOPIERAR IFRÅN FINNS PÅ NYPON-BASEN,                       
152000*  OCH DÄRMED SKER KOPIERING FRÅN ARTREG  O C H  EVENTUELLT FRÅN          
152100*  NYPON-BASEN.                                                           
152200******************************************************************        
152300                                                                          
152400     MOVE ARTG01-ART-IDPROJK  TO WS-IDPROJK-GAM                           
152500     PERFORM S10-KOLLA-INDATA-KOP-ARTREG                                  
152600     IF INPUT-RETT = JA                                                   
152700           PERFORM S07-KOLLA-OM-BEN-FINNS                                 
152800           IF INPUT-RETT = JA                                             
152900              IF NYPON-ARTIKEL = JA                                       
153000                 PERFORM DA-REG-UPPDAT-NYPON-KOP                          
153100              END-IF                                                      
153200              MOVE JA TO TRANS-TO-1192                                    
153300              MOVE MED-1(SPIND)TO MOD-TEMFSINF                            
153400           ELSE                                                           
153500              MOVE FEL-4 (SPIND) TO MOD-TEMFSFEL                          
153600           END-IF                                                         
153700     ELSE                                                                 
153800       MOVE FEL-4 (SPIND) TO MOD-TEMFSFEL                                 
153900     END-IF                                                               
154000     .                                                                    
154100     EJECT                                                                
154200 DA-REG-UPPDAT-NYPON-KOP SECTION.                                         
154300     SKIP2                                                                
154400     MOVE IDARTNR-WS TO W-IDARTNR                                         
154500     PERFORM IMS-GET-ARTG01                                               
154600     MOVE ARTG01-ART-FLPISK             TO WS-SPAR-FLPISK                 
154700     MOVE ARTG01-ART-FLBYTES            TO WS-SPAR-FLBYTES                
154800     MOVE ARTG01-ART-IDPROJK            TO WS-SPAR-IDPROJK                
154900     MOVE ARTG01-ART-KVARTVAGN          TO WS-SPAR-KVARTVAGN              
155000     MOVE ARTG01-ART-KVPROG             TO WS-SPAR-KVPROG                 
155100     MOVE ARTG01-ART-TEORSAK            TO WS-SPAR-TEORSAK-1              
155200     MOVE ARTG01-ART-IDARTNR-MOTSV      TO WS-SPAR-IDARTNR-MOTSV          
155300                                                                          
155400     MOVE IDARTNR-NY-WS TO W-IDARTNR                                      
155500     PERFORM IMS-GET-ARTG01                                               
155600                                                                          
155700     IF SEGMENT-FINNS                                                     
155800        MOVE MID-IDPROJ                 TO ARTG01-ART-IDPROJ              
155900        MOVE MID-IDAO                   TO ARTG01-ART-IDAO                
156000        IF MID-FLPISK = ALL '+'                                           
156100           MOVE WS-SPAR-FLPISK          TO ARTG01-ART-FLPISK              
156200        ELSE                                                              
156300           MOVE WS-FLPISK               TO ARTG01-ART-FLPISK              
156400        END-IF                                                            
156500        IF MID-KVPROG = ALL '+'                                           
156600           MOVE WS-SPAR-KVPROG          TO ARTG01-ART-KVPROG              
156700        ELSE                                                              
156800           MOVE WS-KVPROG               TO ARTG01-ART-KVPROG              
156900        END-IF                                                            
157000                                                                          
157100        IF MID-IDPROJK = ALL '+'                                          
157200           MOVE WS-SPAR-IDPROJK         TO ARTG01-ART-IDPROJK             
157300        ELSE                                                              
157400           MOVE WS-IDPROJK              TO ARTG01-ART-IDPROJK             
157500        END-IF                                                            
157600                                                                          
157700        IF MID-KVARTVAGN = ALL '+'                                        
157800           MOVE WS-SPAR-KVARTVAGN       TO ARTG01-ART-KVARTVAGN           
157900        ELSE                                                              
158000           MOVE WS-KVARTVAGN            TO ARTG01-ART-KVARTVAGN           
158100        END-IF                                                            
158200                                                                          
158300        IF MID-IDARTNR-MOTSV = ALL '+'                                    
158400           MOVE WS-SPAR-IDARTNR-MOTSV                                     
158500                                     TO ARTG01-ART-IDARTNR-MOTSV          
158600        ELSE                                                              
158700           MOVE WS-IDARTNR-MOTSV                                          
158800                                     TO ARTG01-ART-IDARTNR-MOTSV          
158900        END-IF                                                            
159000                                                                          
159100        IF MID-TEORSAK-1 = ALL '+'                                        
159200           MOVE WS-SPAR-TEORSAK-1       TO ARTG01-ART-TEORSAK             
159300        ELSE                                                              
159400           MOVE MID-TEORSAK-1           TO ARTG01-ART-TEORSAK             
159500        END-IF                                                            
159600                                                                          
159700        IF XX-TISOP = ALL '+'                                             
159800           IF ART-TISOP = 99999                                           
159900              MOVE 99999999          TO ARTG01-ART-DAFINLEV               
160000           ELSE                                                           
160100              MOVE 'AAVVD ' TO DAT-KDDATFORM                              
160200              MOVE ART-TISOP TO DAT-I-TIDATUM                             
160300              PERFORM S99-WDATKONV                                        
160400              IF DAT-KDSVAR-OK                                            
160500                 MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV               
160600                 MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)         
160700              END-IF                                                      
160800           END-IF                                                         
160900        ELSE                                                              
161000           IF WS-TISOP = 99999                                            
161100              MOVE 99999999          TO ARTG01-ART-DAFINLEV               
161200           ELSE                                                           
161300              MOVE 'AAVVD ' TO DAT-KDDATFORM                              
161400              MOVE WS-TISOP TO DAT-I-TIDATUM                              
161500              PERFORM S99-WDATKONV                                        
161600              IF DAT-KDSVAR-OK                                            
161700                 MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV               
161800                 MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)         
161900              END-IF                                                      
162000           END-IF                                                         
162100        END-IF                                                            
162200                                                                          
162300        PERFORM IMS-REPL-ARTG                                             
162400                                                                          
162500        MOVE ARTG01-ART-TEORSAK         TO MOD-TEORSAK-1                  
162600        MOVE ARTG01-ART-FLPISK          TO MOD-FLPISK-UT                  
162700        MOVE ARTG01-ART-IDPROJK         TO MOD-IDPROJK-UT                 
162800        MOVE ARTG01-ART-KVARTVAGN       TO MOD-KVARTVAGN-UT               
162900        MOVE ARTG01-ART-IDARTNR-MOTSV   TO MOD-IDARTNR-MOTSV-UT           
163000     ELSE                                                                 
163100                                                                          
163200        MOVE WS-BEART-SVE               TO ARTG01-ART-BEART-SVE           
163300        MOVE IDARTNR-NY-WS              TO ARTG01-ART-IDARTNR             
163400        IF MID-FLPISK = ALL '+'                                           
163500           MOVE WS-SPAR-FLPISK          TO ARTG01-ART-FLPISK              
163600        ELSE                                                              
163700           MOVE WS-FLPISK               TO ARTG01-ART-FLPISK              
163800        END-IF                                                            
163900        IF MID-IDPROJK = ALL '+'                                          
164000           MOVE WS-SPAR-IDPROJK         TO ARTG01-ART-IDPROJK             
164100        ELSE                                                              
164200           MOVE WS-IDPROJK              TO ARTG01-ART-IDPROJK             
164300        END-IF                                                            
164400        IF MID-KVPROG = ALL '+'                                           
164500           MOVE WS-SPAR-KVPROG          TO ARTG01-ART-KVPROG              
164600        ELSE                                                              
164700           MOVE WS-KVPROG               TO ARTG01-ART-KVPROG              
164800        END-IF                                                            
164900                                                                          
165000        IF MID-KVARTVAGN = ALL '+'                                        
165100           MOVE WS-SPAR-KVARTVAGN       TO ARTG01-ART-KVARTVAGN           
165200        ELSE                                                              
165300           MOVE WS-KVARTVAGN            TO ARTG01-ART-KVARTVAGN           
165400        END-IF                                                            
165500                                                                          
165600        IF MID-IDARTNR-MOTSV = ALL '+'                                    
165700           MOVE WS-SPAR-IDARTNR-MOTSV                                     
165800                                     TO ARTG01-ART-IDARTNR-MOTSV          
165900        ELSE                                                              
166000           MOVE WS-IDARTNR-MOTSV                                          
166100                                     TO ARTG01-ART-IDARTNR-MOTSV          
166200        END-IF                                                            
166300                                                                          
166400        IF MID-TEORSAK-1 = ALL '+'                                        
166500           MOVE WS-SPAR-TEORSAK-1       TO ARTG01-ART-TEORSAK             
166600        ELSE                                                              
166700           MOVE MID-TEORSAK-1           TO ARTG01-ART-TEORSAK             
166800        END-IF                                                            
166900                                                                          
167000        MOVE ARTG01-ART-TEORSAK         TO MOD-TEORSAK-1                  
167100        MOVE ARTG01-ART-FLPISK          TO MOD-FLPISK-UT                  
167200        MOVE ARTG01-ART-IDPROJK         TO MOD-IDPROJK-UT                 
167300        MOVE ARTG01-ART-KVARTVAGN       TO MOD-KVARTVAGN-UT               
167400        MOVE ARTG01-ART-IDARTNR-MOTSV   TO MOD-IDARTNR-MOTSV-UT           
167500                                                                          
167600        PERFORM S19-KOP-FRAN-ARTREG-TILL-NYPON                            
167700        PERFORM S21-NOLLSTAELL-NYPON                                      
167800        MOVE IDARTNR-NY-WS TO W-IDARTNR                                   
167900        PERFORM IMS-ISRT-ARTG01                                           
168000                                                                          
168100     END-IF                                                               
168200     .                                                                    
168300     EJECT                                                                
168400 E-VAECKNING-AV-GAMMAL-ARTIKEL SECTION.                                   
168500     SKIP2                                                                
168600     PERFORM S08-KOLLA-INDATA-NYREG                                       
168700     IF INPUT-RETT = JA                                                   
168800           MOVE FEL-3 (SPIND)      TO MOD-TEMFSFEL                        
168900           MOVE MFS-STAENG-FAELT   TO MOD-IDARTNR-ATTR                    
169000                                      MOD-BEART-ATTR                      
169100     ELSE                                                                 
169200        MOVE FEL-4 (SPIND)    TO MOD-TEMFSFEL                             
169300        MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-ATTR                         
169400                                 MOD-BEART-ATTR                           
169500     END-IF                                                               
169600     .                                                                    
169700     EJECT                                                                
169800 F-REG-ARTIKEL-ARTREG-EV-NYPON SECTION.                                   
169900     SKIP2                                                                
170000     IF IDARTNR-NY-WS = IDARTNR-WS                                        
170100        IF IDARTNR-NY-WS > 0 AND IDARTNR-WS > 0                           
170200           IF IDARTNR-NY-WS > 99999999                                    
170300              MOVE FEL-8 (SPIND) TO MOD-TEMFSFEL                          
170400              MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDARTNR-ATTR               
170500           ELSE                                                           
170600              PERFORM S08-KOLLA-INDATA-NYREG                              
170700              IF INPUT-RETT = JA                                          
170800                    PERFORM S07-KOLLA-OM-BEN-FINNS                        
170900                    IF INPUT-RETT = JA                                    
171000                       IF NYPON-ARTIKEL = JA                              
171100                          PERFORM S12-REGISTRERA-NYPON                    
171200                       END-IF                                             
171300                       MOVE MED-1(SPIND)TO MOD-TEMFSINF                   
171400                       MOVE JA TO TRANS-TO-1192                           
171500                    ELSE                                                  
171600                       MOVE FEL-4 (SPIND) TO MOD-TEMFSFEL                 
171700                    END-IF                                                
171800              ELSE                                                        
171900                 MOVE FEL-4 (SPIND) TO MOD-TEMFSFEL                       
172000              END-IF                                                      
172100           END-IF                                                         
172200        ELSE                                                              
172300           MOVE FEL-8(SPIND)TO MOD-TEMFSFEL                               
172400        END-IF                                                            
172500     END-IF                                                               
172600     .                                                                    
172700     EJECT                                                                
172800 H-LAS-NYPON SECTION.                                                     
172900     SKIP2                                                                
173000     MOVE ZERO TO WS-NY-IDARTNR                                           
173100     PERFORM IMS-GET-ARTG01                                               
173200     IF SEGMENT-FINNS                                                     
173300        PERFORM S22-FIXA-DOLDA-FAELT                                      
173400        MOVE ARTG01-ART-IDBERED TO WS-IDBERED                             
173500        MOVE ARTG01-ART-KDPRODSL    TO WS-KDPRODSL                        
173600        MOVE ARTG01-ART-IDPROJ      TO MOD-IDPROJ-UT                      
173700                                       WS-IDPROJ                          
173800        MOVE ARTG01-ART-IDPROENH    TO MOD-IDPROENH-1-UT                  
173900        INSPECT MOD-IDPROENH-1-UT REPLACING LEADING                       
174000                                  ZERO BY SPACE                           
174100        MOVE ARTG01-ART-IDPROJK     TO MOD-IDPROJK-UT                     
174200        MOVE ARTG01-ART-KVARTVAGN   TO MOD-KVARTVAGN-UT                   
174300        MOVE ARTG01-ART-FLPISK      TO MOD-FLPISK-UT                      
174400        MOVE ARTG01-ART-IDFKNGRP    TO WS-IDFKNGRP                        
174500        MOVE ARTG01-ART-TEORSAK     TO MOD-TEORSAK-1                      
174600        MOVE ARTG01-ART-IDARTNR-MOTSV                                     
174700                                    TO MOD-IDARTNR-MOTSV-UT               
174800        IF FINNS-PA-ARTC = JA                                             
174900           CONTINUE                                                       
175000        ELSE                                                              
175100           PERFORM S97-LAES-XXAQ                                          
175200           IF SEGMENT-FINNS                                               
175300              IF  XXAQ-1132-TIPRODSTA = +111111                           
175400              AND XXAQ-1132-TIFINLEV  = ZERO                              
175500                  PERFORM HA-PLOCKA-NYASTE-TISERLEV                       
175600                  IF W-SPAR-TISERLEV NOT = 9999999                        
175700                     MOVE 'AAMMDD'             TO DAT-KDDATFORM           
175800                     MOVE W-SPAR-TISERLEV      TO DAT-I-TIDATUM           
175900                     PERFORM S99-WDATKONV                                 
176000                     IF DAT-KDSVAR-OK                                     
176100                        CONTINUE                                          
176200                     END-IF                                               
176300                  END-IF                                                  
176400              ELSE                                                        
176500                 MOVE XXAQ-1132-TIFINLEV       TO DAT-I-TIDATUM           
176600                 MOVE 'AAMMDD'                 TO DAT-KDDATFORM           
176700                 PERFORM S99-WDATKONV                                     
176800              END-IF                                                      
176900           ELSE                                                           
177000              PERFORM HA-PLOCKA-NYASTE-TISERLEV                           
177100              IF W-SPAR-TISERLEV = 9999999                                
177200                 CONTINUE                                                 
177300              ELSE                                                        
177400                  MOVE 'AAMMDD'              TO DAT-KDDATFORM             
177500                  MOVE W-SPAR-TISERLEV       TO DAT-I-TIDATUM             
177600                  PERFORM S99-WDATKONV                                    
177700              END-IF                                                      
177800           END-IF                                                         
177900        END-IF                                                            
178000     ELSE                                                                 
178100        MOVE MFS-RENSA-FAELT   TO                                         
178200                                  MOD-IDPROENH-1-UT                       
178300                                  MOD-IDPROENH-2-UT                       
178400                                  MOD-IDPROENH-3-UT                       
178500                                  MOD-IDPROJ-UT                           
178600                                  MOD-IDPROJK-UT                          
178700                                  MOD-KVARTVAGN-UT                        
178800                                  MOD-FLPISK-UT                           
178900                                  MOD-TEORSAK-1                           
179000                                  MOD-IDARTNR-MOTSV-UT                    
179100     END-IF                                                               
179200                                                                          
179300     MOVE MFS-RENSA-FAELT      TO                                         
179400                                  MOD-KDUART-UT                           
179500                                  MOD-IDPROENH-2-UT                       
179600                                  MOD-IDPROENH-3-UT                       
179700                                                                          
179800                                  MOD-IDLEVNR-UT                          
179900     .                                                                    
180000     EJECT                                                                
180100 HA-PLOCKA-NYASTE-TISERLEV      SECTION.                                  
180200     SKIP2                                                                
180300     MOVE +9999999   TO W-SPAR-TISERLEV                                   
180400     MOVE +1         TO SLEV-IX                                           
180500     PERFORM UNTIL SLEV-IX > 5                                            
180600        IF ARTG01-ART-TISERLEV(SLEV-IX) = ZERO                            
180700           CONTINUE                                                       
180800        ELSE                                                              
180900           MOVE ARTG01-ART-TISERLEV(SLEV-IX)   TO TMP1-YYMMDD             
181000           MOVE W-SPAR-TISERLEV                TO TMP2-YYMMDD             
181100           PERFORM WY2000P1                                               
181200           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
181300              MOVE ARTG01-ART-TISERLEV(SLEV-IX)      TO                   
181400                                                 W-SPAR-TISERLEV          
181500           END-IF                                                         
181600        END-IF                                                            
181700        ADD +1 TO  SLEV-IX                                                
181800     END-PERFORM                                                          
181900     .                                                                    
182000     EJECT                                                                
182100 K-LAS-ARTREG-NYPON-BENREG SECTION.                                       
182200     SKIP2                                                                
182300     MOVE ZERO TO WS-NY-IDARTNR                                           
182400     IF ART-KDERS-UTG > 0                                                 
182500        MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-ATTR                         
182600        MOVE MFS-STAENG-FAELT TO MOD-BEART-ATTR                           
182700     END-IF                                                               
182800                                                                          
182900     MOVE ART-TISOP       TO WS-TISOP                                     
183000     MOVE ART-IDFKNGRP TO WS-IDFKNGRP                                     
183100     MOVE ART-KDPRODSL TO WS-KDPRODSL                                     
183200                                                                          
183300                                                                          
183400     PERFORM IMS-GET-ARTC11                                               
183500     IF SEGMENT-FINNS                                                     
183600        MOVE CLAG-IDBERED    TO WS-IDBERED                                
183700        MOVE CLAG-IDPROJ     TO MOD-IDPROJ-UT                             
183800        MOVE CLAG-KDYTBEH    TO WS-KDYTBEH                                
183900        MOVE CLAG-IDPROENH(1) TO MOD-IDPROENH-1-UT                        
184000        INSPECT MOD-IDPROENH-1-UT REPLACING LEADING                       
184100                                  ZERO BY SPACE                           
184200        MOVE CLAG-IDPROENH(2) TO MOD-IDPROENH-2-UT                        
184300        INSPECT MOD-IDPROENH-2-UT REPLACING LEADING                       
184400                                  ZERO BY SPACE                           
184500        MOVE CLAG-IDPROENH(3) TO MOD-IDPROENH-3-UT                        
184600        INSPECT MOD-IDPROENH-3-UT REPLACING LEADING                       
184700                                  ZERO BY SPACE                           
184800        MOVE CLAG-KDUART      TO MOD-KDUART-UT                            
184900        MOVE CLAG-KDFARLIG    TO WS-KDFARLIG                              
185000        MOVE CLAG-KDBPSR      TO WS-KDBPSR                                
185100     ELSE                                                                 
185200        MOVE MFS-RENSA-FAELT TO                                           
185300                                MOD-IDPROJ-UT                             
185400                                MOD-IDPROENH-1-UT                         
185500                                MOD-IDPROENH-2-UT                         
185600                                MOD-IDPROENH-3-UT                         
185700                                MOD-KDUART-UT                             
185800     END-IF                                                               
185900                                                                          
186000                                                                          
186100     MOVE 1 TO W-KDNOTTYP                                                 
186200     PERFORM IMS-GNP-ARTC25                                               
186300                                                                          
186400     IF SWEDISH-TEXT                                                      
186500        MOVE 'S  ' TO W-IDSKYLT                                           
186600     ELSE                                                                 
186700        MOVE 'GB ' TO W-IDSKYLT                                           
186800     END-IF                                                               
186900                                                                          
187000     PERFORM IMS-GET-ARTG01                                               
187100     IF SEGMENT-FINNS                                                     
187200        PERFORM S22-FIXA-DOLDA-FAELT                                      
187300        PERFORM KA-KOLLA-RESBED-ARTUTG                                    
187400        MOVE ARTG01-ART-IDPROJK  TO MOD-IDPROJK-UT                        
187500        MOVE ARTG01-ART-KVARTVAGN TO MOD-KVARTVAGN-UT                     
187600        MOVE ARTG01-ART-FLPISK   TO MOD-FLPISK-UT                         
187700        MOVE ARTG01-ART-TEORSAK  TO MOD-TEORSAK-1                         
187800        MOVE ARTG01-ART-IDARTNR-MOTSV                                     
187900                                 TO MOD-IDARTNR-MOTSV-UT                  
188000     ELSE                                                                 
188100        MOVE MFS-RENSA-FAELT TO MOD-IDPROJK-UT                            
188200                                MOD-KVARTVAGN-UT                          
188300                                MOD-FLPISK-UT                             
188400                                MOD-TEORSAK-1                             
188500                                MOD-IDARTNR-MOTSV-UT                      
188600     END-IF                                                               
188700     MOVE MFS-RENSA-FAELT    TO                                           
188800                                MOD-IDLEVNR-UT                            
188900     .                                                                    
189000     EJECT                                                                
189100 KA-KOLLA-RESBED-ARTUTG     SECTION.                                      
189200     IF ARTG01-ART-KDRESBED = '-'                                         
189300        MOVE MED-2(SPIND)         TO                                      
189400                              MOD-TEMFSFEL                                
189500     END-IF                                                               
189600     .                                                                    
189700     EJECT                                                                
189800 M-LAS-BENREG SECTION.                                                    
189900     SKIP2                                                                
190000                                                                          
190100     IF SWEDISH-TEXT                                                      
190200        MOVE 'S  ' TO W-IDSKYLT                                           
190300     ELSE                                                                 
190400        MOVE 'GB ' TO W-IDSKYLT                                           
190500     END-IF                                                               
190600     .                                                                    
190700     EJECT                                                                
190800 O-KOLLA-RESBED-ARTUTG     SECTION.                                       
190900     IF ARTG01-ART-KDRESBED = '-'                                         
191000        MOVE MED-2(SPIND)         TO                                      
191100                              MOD-TEMFSFEL                                
191200     END-IF                                                               
191300     .                                                                    
191400     EJECT                                                                
191500 P-KOLLA-RASA SECTION.                                                    
191600     SKIP2                                                                
191700     MOVE NEJ TO FINNS-RASA                                               
191800     MOVE IDARTNR-WS TO W-IDARTNR                                         
191900     PERFORM IMS-GET-SATB01                                               
192000     IF SEGMENT-FINNS                                                     
192100        MOVE JA TO FINNS-RASA                                             
192200     ELSE                                                                 
192300        MOVE SPACE TO W-IDLEVNR-S                                         
192400        MOVE SPACE TO W-BELEVART-S                                        
192500        MOVE IDARTNR-WS TO W-IDARTNR-S                                    
192600        PERFORM IMS-GET-SATB11-CSEQ                                       
192700        IF SEGMENT-FINNS                                                  
192800           MOVE JA TO FINNS-RASA                                          
192900        END-IF                                                            
193000     END-IF                                                               
193100     .                                                                    
193200     EJECT                                                                
193300 Q-FLYTTA-MID-OCH-MOD   SECTION.                                          
193400     SKIP2                                                                
193500*    1116-MID OCH -MOD HAR FÖRLNGTS MED TISOP DIREKT EFTER                
193600*    TIFINLV.                                                             
193700*    TIDIGARE VAR 11616- OCH 9410-AREORNA IDENTISKA                       
193800*    MEN NÄR NU SÅ INTE LÄNGRE ÄR FALLET                                  
193900*    TVINGAS VI FLYTTA FÄLT FÖR FÄLT.                                     
194000                                                                          
194100     PERFORM QA-FLYTTA-MID                                                
194200     PERFORM QB-FLYTTA-MOD                                                
194300     .                                                                    
194400     EJECT                                                                
194500 QA-FLYTTA-MID SECTION.                                                   
194600     SKIP2                                                                
194700     MOVE SPACE              TO PROGSW-MID-W1I11601                       
194800     MOVE MID-IDARTNR-IN     TO PROGSW-MID-IDARTNR-IN                     
194900     MOVE MID-IDARTNR-UT     TO PROGSW-MID-IDARTNR-UT                     
195000     MOVE MID-IDBERED-LAEST  TO PROGSW-MID-IDBERED-LAEST                  
195100     MOVE MID-IDAO-LAEST     TO PROGSW-MID-IDAO-LAEST                     
195200     MOVE MID-IDAO-VALD      TO PROGSW-MID-IDAO-VALD                      
195300     MOVE MID-IDPROJ-VALD    TO PROGSW-MID-IDPROJ-VALD                    
195400     MOVE MID-IDARTNR-NY     TO PROGSW-MID-IDARTNR-NY                     
195500     MOVE MID-IDBERED        TO PROGSW-MID-IDBERED                        
195600     MOVE MID-KDPRODSL       TO PROGSW-MID-KDPRODSL                       
195700     MOVE MID-KDSORT         TO PROGSW-MID-KDSORT                         
195800     MOVE MID-IDPROENH-1     TO PROGSW-MID-IDPROENH-1                     
195900     MOVE MID-IDPROENH-2     TO PROGSW-MID-IDPROENH-2                     
196000     MOVE MID-IDPROENH-3     TO PROGSW-MID-IDPROENH-3                     
196100     MOVE MID-KDYTBEH        TO PROGSW-MID-KDYTBEH                        
196200     MOVE MID-IDPROJ         TO PROGSW-MID-IDPROJ                         
196300     MOVE MID-KDFARLIG       TO PROGSW-MID-KDFARLIG                       
196400     MOVE MID-KDBPSR         TO PROGSW-MID-KDBPSR                         
196500     MOVE MID-IDKAT-1        TO PROGSW-MID-IDKAT-1                        
196600     MOVE MID-IDKAT-2        TO PROGSW-MID-IDKAT-2                        
196700     MOVE MID-IDKAT-3        TO PROGSW-MID-IDKAT-3                        
196800     MOVE MID-KDUART         TO PROGSW-MID-KDUART                         
196900     MOVE MID-IDPROJK        TO PROGSW-MID-IDPROJK                        
197000     MOVE MID-FLPISK         TO PROGSW-MID-FLPISK                         
197100     MOVE MID-IDAO           TO PROGSW-MID-IDAO                           
197200     MOVE MID-TISOP          TO PROGSW-MID-TISOP                          
197300     MOVE MID-IDSKYLT        TO PROGSW-MID-IDSKYLT                        
197400     MOVE MID-FLLSRDEL       TO PROGSW-MID-FLLSRDEL                       
197500     MOVE MID-IDPROJUP       TO PROGSW-MID-IDPROJUP                       
197600     MOVE MID-BEART          TO PROGSW-MID-BEART                          
197700     MOVE MID-FLRSBEART      TO PROGSW-MID-FLRSBEART                      
197800     MOVE MID-IDFKNGRP       TO PROGSW-MID-IDFKNGRP                       
197900     MOVE MID-IDLEVNR        TO PROGSW-MID-IDLEVNR                        
198000     MOVE MID-BELEV          TO PROGSW-MID-BELEV                          
198100     MOVE MID-KVPROG         TO PROGSW-MID-KVPROG                         
198200     MOVE MID-TEORSAK-1      TO PROGSW-MID-TEORSAK-1                      
198300     MOVE MID-TEARTNOT-2     TO PROGSW-MID-TEARTNOT-2                     
198400     MOVE MID-IDRITN         TO PROGSW-MID-IDRITN                         
198500     MOVE MID-KVARTVAGN      TO PROGSW-MID-KVARTVAGN                      
198600     MOVE MID-TEARTNOT-7     TO PROGSW-MID-TEARTNOT-7                     
198700     MOVE MID-TEARTNOT-4     TO PROGSW-MID-TEARTNOT-4                     
198800     MOVE MID-IDARTNR-MOTSV  TO PROGSW-MID-IDARTNR-MOTSV                  
198900     MOVE MID-FLBYTES        TO PROGSW-MID-FLBYTES                        
199000     MOVE MID-FLGAMART       TO PROGSW-MID-FLGAMART                       
199000     MOVE SPACE              TO PROGSW-MID-IDCDS                          
199000     MOVE SPACE              TO PROGSW-MID-KDARTSYS                       
199100     .                                                                    
199200     EJECT                                                                
199300 QB-FLYTTA-MOD SECTION.                                                   
199400     SKIP2                                                                
199500     MOVE SPACE                  TO PROGSW-MOD-W1O11601                   
199600     MOVE MOD-IDTRANS            TO PROGSW-MOD-IDTRANS                    
199700     MOVE MOD-TEMFSFEL           TO PROGSW-MOD-TEMFSFEL                   
199800     MOVE MOD-IDARTNR-IN         TO PROGSW-MOD-IDARTNR-IN                 
199900     MOVE MOD-IDARTNR-UT         TO PROGSW-MOD-IDARTNR-UT                 
200000     MOVE MOD-IDBERED-LAEST      TO PROGSW-MOD-IDBERED-LAEST              
200100     MOVE MOD-IDAO-LAEST         TO PROGSW-MOD-IDAO-LAEST                 
200200     MOVE MOD-IDAO-VALD          TO PROGSW-MOD-IDAO-VALD                  
200300     MOVE MOD-IDPROJ-VALD        TO PROGSW-MOD-IDPROJ-VALD                
200400     MOVE MOD-IDARTNR-ATTR       TO PROGSW-MOD-IDARTNR-ATTR               
200500     MOVE MOD-IDARTNR-NY         TO PROGSW-MOD-IDARTNR-NY                 
200600     MOVE MOD-IDBERED-UT         TO PROGSW-MOD-IDBERED-UT                 
200700     MOVE MOD-KDPRODSL-UT        TO PROGSW-MOD-KDPRODSL-UT                
200800     MOVE MOD-KDSORT-UT          TO PROGSW-MOD-KDSORT-UT                  
200900     MOVE MOD-IDPROENH-1-UT      TO PROGSW-MOD-IDPROENH-1-UT              
201000     MOVE MOD-IDPROENH-2-UT      TO PROGSW-MOD-IDPROENH-2-UT              
201100     MOVE MOD-IDPROENH-3-UT      TO PROGSW-MOD-IDPROENH-3-UT              
201200     MOVE MOD-IDBERED-ATTR       TO PROGSW-MOD-IDBERED-ATTR               
201300     MOVE MOD-IDBERED-IN         TO PROGSW-MOD-IDBERED-IN                 
201400     MOVE MOD-KDPRODSL-ATTR      TO PROGSW-MOD-KDPRODSL-ATTR              
201500     MOVE MOD-KDPRODSL-IN        TO PROGSW-MOD-KDPRODSL-IN                
201600     MOVE MOD-KDSORT-ATTR        TO PROGSW-MOD-KDSORT-ATTR                
201700     MOVE MOD-KDSORT-IN          TO PROGSW-MOD-KDSORT-IN                  
201800     MOVE MOD-IDPROENH-1-ATTR    TO PROGSW-MOD-IDPROENH-1-ATTR            
201900     MOVE MOD-IDPROENH-1-IN      TO PROGSW-MOD-IDPROENH-1-IN              
202000     MOVE MOD-IDPROENH-2-ATTR    TO PROGSW-MOD-IDPROENH-2-ATTR            
202100     MOVE MOD-IDPROENH-2-IN      TO PROGSW-MOD-IDPROENH-2-IN              
202200     MOVE MOD-IDPROENH-3-ATTR    TO PROGSW-MOD-IDPROENH-3-ATTR            
202300     MOVE MOD-IDPROENH-3-IN      TO PROGSW-MOD-IDPROENH-3-IN              
202400     MOVE MOD-KDYTBEH-UT         TO PROGSW-MOD-KDYTBEH-UT                 
202500     MOVE MOD-IDPROJ-UT          TO PROGSW-MOD-IDPROJ-UT                  
202600     MOVE MOD-KDFARLIG-UT        TO PROGSW-MOD-KDFARLIG-UT                
202700     MOVE MOD-KDBPSR-UT          TO PROGSW-MOD-KDBPSR-UT                  
202800     MOVE MOD-IDKAT-1-UT         TO PROGSW-MOD-IDKAT-1-UT                 
202900     MOVE MOD-IDKAT-2-UT         TO PROGSW-MOD-IDKAT-2-UT                 
203000     MOVE MOD-IDKAT-3-UT         TO PROGSW-MOD-IDKAT-3-UT                 
203100     MOVE MOD-KDYTBEH-ATTR       TO PROGSW-MOD-KDYTBEH-ATTR               
203200     MOVE MOD-KDYTBEH-IN         TO PROGSW-MOD-KDYTBEH-IN                 
203300     MOVE MOD-IDPROJ-ATTR        TO PROGSW-MOD-IDPROJ-ATTR                
203400     MOVE MOD-IDPROJ-IN          TO PROGSW-MOD-IDPROJ-IN                  
203500     MOVE MOD-KDFARLIG-ATTR      TO PROGSW-MOD-KDFARLIG-ATTR              
203600     MOVE MOD-KDFARLIG-IN        TO PROGSW-MOD-KDFARLIG-IN                
203700     MOVE MOD-KDBPSR-ATTR        TO PROGSW-MOD-KDBPSR-ATTR                
203800     MOVE MOD-KDBPSR-IN          TO PROGSW-MOD-KDBPSR-IN                  
203900     MOVE MOD-IDKAT-1-ATTR       TO PROGSW-MOD-IDKAT-1-ATTR               
204000     MOVE MOD-IDKAT-1-IN         TO PROGSW-MOD-IDKAT-1-IN                 
204100     MOVE MOD-IDKAT-2-ATTR       TO PROGSW-MOD-IDKAT-2-ATTR               
204200     MOVE MOD-IDKAT-2-IN         TO PROGSW-MOD-IDKAT-2-IN                 
204300     MOVE MOD-IDKAT-3-ATTR       TO PROGSW-MOD-IDKAT-3-ATTR               
204400     MOVE MOD-IDKAT-3-IN         TO PROGSW-MOD-IDKAT-3-IN                 
204500     MOVE MOD-KDUART-UT          TO PROGSW-MOD-KDUART-UT                  
204600     MOVE MOD-IDPROJK-UT         TO PROGSW-MOD-IDPROJK-UT                 
204700     MOVE MOD-FLPISK-UT          TO PROGSW-MOD-FLPISK-UT                  
204800     MOVE MOD-IDAO-UT            TO PROGSW-MOD-IDAO-UT                    
204900     MOVE MOD-TISOP-UT           TO PROGSW-MOD-TISOP-UT                   
205000     MOVE MOD-KDUART-ATTR        TO PROGSW-MOD-KDUART-ATTR                
205100     MOVE MOD-KDUART-IN          TO PROGSW-MOD-KDUART-IN                  
205200     MOVE MOD-IDPROJK-ATTR       TO PROGSW-MOD-IDPROJK-ATTR               
205300     MOVE MOD-IDPROJK-IN         TO PROGSW-MOD-IDPROJK-IN                 
205400     MOVE MOD-FLPISK-ATTR        TO PROGSW-MOD-FLPISK-ATTR                
205500     MOVE MOD-FLPISK-IN          TO PROGSW-MOD-FLPISK-IN                  
205600     MOVE MOD-IDAO-ATTR          TO PROGSW-MOD-IDAO-ATTR                  
205700     MOVE MOD-IDAO-IN            TO PROGSW-MOD-IDAO-IN                    
205800     MOVE MOD-TISOP-ATTR         TO PROGSW-MOD-TISOP-ATTR                 
205900     MOVE MOD-TISOP-IN           TO PROGSW-MOD-TISOP-IN                   
206000     MOVE MOD-FLLSRDEL-ATTR      TO PROGSW-MOD-FLLSRDEL-ATTR              
206100     MOVE MOD-FLLSRDEL-IN        TO PROGSW-MOD-FLLSRDEL-IN                
206200     MOVE MOD-IDPROJUP-ATTR      TO PROGSW-MOD-IDPROJUP-ATTR              
206300     MOVE MOD-IDPROJUP-IN        TO PROGSW-MOD-IDPROJUP-IN                
206400     MOVE MOD-BEART-ATTR         TO PROGSW-MOD-BEART-ATTR                 
206500     MOVE MOD-BEART-IN           TO PROGSW-MOD-BEART-IN                   
206600     MOVE MOD-FLRSBEART-ATTR     TO PROGSW-MOD-FLRSBEART-ATTR             
206700     MOVE MOD-FLRSBEART-IN       TO PROGSW-MOD-FLRSBEART-IN               
206800     MOVE MOD-IDFKNGRP-UT        TO PROGSW-MOD-IDFKNGRP-UT                
206900     MOVE MOD-IDLEVNR-UT         TO PROGSW-MOD-IDLEVNR-UT                 
207000     MOVE MOD-BELEV-UT           TO PROGSW-MOD-BELEV-UT                   
207100     MOVE MOD-KVPROG-UT          TO PROGSW-MOD-KVPROG-UT                  
207200     MOVE MOD-IDFKNGRP-ATTR      TO PROGSW-MOD-IDFKNGRP-ATTR              
207300     MOVE MOD-IDFKNGRP-IN        TO PROGSW-MOD-IDFKNGRP-IN                
207400     MOVE MOD-IDLEVNR-ATTR       TO PROGSW-MOD-IDLEVNR-ATTR               
207500     MOVE MOD-IDLEVNR-IN         TO PROGSW-MOD-IDLEVNR-IN                 
207600     MOVE MOD-BELEV-ATTR         TO PROGSW-MOD-BELEV-ATTR                 
207700     MOVE MOD-BELEV-IN           TO PROGSW-MOD-BELEV-IN                   
207800     MOVE MOD-KVPROG-ATTR        TO PROGSW-MOD-KVPROG-ATTR                
207900     MOVE MOD-KVPROG-IN          TO PROGSW-MOD-KVPROG-IN                  
208000     MOVE MOD-TEORSAK-1-ATTR     TO PROGSW-MOD-TEORSAK-1-ATTR             
208100     MOVE MOD-TEORSAK-1          TO PROGSW-MOD-TEORSAK-1                  
208200     MOVE MOD-IDRITN-IN          TO PROGSW-MOD-IDRITN-IN                  
208300     MOVE MOD-KVARTVAGN-UT       TO PROGSW-MOD-KVARTVAGN-UT               
208400     MOVE MOD-TEARTNOT-2-ATTR    TO PROGSW-MOD-TEARTNOT-2-ATTR            
208500     MOVE MOD-TEARTNOT-2         TO PROGSW-MOD-TEARTNOT-2                 
208600     MOVE MOD-IDRITN-ATTR        TO PROGSW-MOD-IDRITN-ATTR                
208700     MOVE MOD-IDRITN-IN          TO PROGSW-MOD-IDRITN-IN                  
208800     MOVE MOD-KVARTVAGN-ATTR     TO PROGSW-MOD-KVARTVAGN-ATTR             
208900     MOVE MOD-KVARTVAGN-IN       TO PROGSW-MOD-KVARTVAGN-IN               
209000     MOVE MOD-TEARTNOT-7-ATTR    TO PROGSW-MOD-TEARTNOT-7-ATTR            
209100     MOVE MOD-TEARTNOT-7         TO PROGSW-MOD-TEARTNOT-7                 
209200     MOVE MOD-IDARTNR-MOTSV-UT   TO PROGSW-MOD-IDARTNR-MOTSV-UT           
209300     MOVE MOD-FLBYTES-UT         TO PROGSW-MOD-FLBYTES-UT                 
209400     MOVE MOD-TEARTNOT-4-ATTR    TO PROGSW-MOD-TEARTNOT-4-ATTR            
209500     MOVE MOD-TEARTNOT-4         TO PROGSW-MOD-TEARTNOT-4                 
209600     MOVE MOD-IDARTNR-MOTSV-ATTR TO PROGSW-MOD-IDARTNR-MOTSV-ATTR         
209700     MOVE MOD-IDARTNR-MOTSV-IN   TO PROGSW-MOD-IDARTNR-MOTSV-IN           
209800     MOVE MOD-FLBYTES-ATTR       TO PROGSW-MOD-FLBYTES-ATTR               
209900     MOVE MOD-FLBYTES-IN         TO PROGSW-MOD-FLBYTES-IN                 
210000     MOVE MOD-FLGAMART-ATTR      TO PROGSW-MOD-FLGAMART-ATTR              
210100     MOVE MOD-FLGAMART-IN        TO PROGSW-MOD-FLGAMART-IN                
210200     MOVE MOD-TEMFSINF TO PROGSW-MOD-TEMFSINF                             
210300     .                                                                    
210400     EJECT                                                                
210500 S01-NYA-GEMENSAMMA-DATAELEMENT SECTION.                                  
210600     SKIP2                                                                
210700     MOVE ZERO TO WS-IDPROENH(1)                                          
210800                  WS-IDPROENH(2)                                          
210900                  WS-IDPROENH(3)                                          
211000                                                                          
211100                                                                          
211200     INSPECT MID-BEART REPLACING ALL '<' BY SPACE                         
211300     INSPECT MID-BEART REPLACING ALL '>' BY SPACE                         
211400                                                                          
211500     IF MID-BEART = ALL '+' OR SPACE                                      
211600        IF VAECKNING = JA                                                 
211700           MOVE IDARTNR-WS      TO W-IDARTNR                              
211800           MOVE 'S  '           TO W-IDSKYLT                              
211900           PERFORM IMS-GET-BENA11-CSEQ                                    
212000           MOVE BENA-TEXT-BEART TO WS-BEART                               
212100                                   WS-BEART-SVE                           
212200        ELSE                                                              
212300           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR                      
212400           MOVE NEJ TO INPUT-RETT                                         
212500        END-IF                                                            
212600     ELSE                                                                 
212700        IF VAECKNING = JA                                                 
212800           MOVE NEJ                TO INPUT-RETT                          
212900           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR                      
213000        ELSE                                                              
213100           MOVE MID-BEART            TO WS-BEART                          
213200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-ATTR                    
213300        END-IF                                                            
213400     END-IF                                                               
213500                                                                          
213600     IF MID-IDRITN = ALL '+'                                              
213700        MOVE NEJ TO INPUT-RETT                                            
213800        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                        
213900     ELSE                                                                 
214000        IF MID-IDRITN = SPACE                                             
214100           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                     
214200           MOVE NEJ TO INPUT-RETT                                         
214300        ELSE                                                              
214400           MOVE MID-IDRITN TO WS-IDRITN                                   
214500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDRITN-ATTR                   
214600        END-IF                                                            
214700     END-IF                                                               
214800                                                                          
214900     IF MID-IDPROJ = ALL '+'                                              
215000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJ-ATTR                     
215100           MOVE NEJ TO INPUT-RETT                                         
215200     ELSE                                                                 
215300           MOVE MID-IDPROJ TO WS-IDPROJ                                   
215400           PERFORM S14-GODK-PROJ-MFS-RAETT-FEL                            
215500     END-IF                                                               
215600                                                                          
215700     IF MID-IDPROENH-1 = ALL '+'                                          
215800        CONTINUE                                                          
215900     ELSE                                                                 
216000        IF MID-IDPROENH-1  NUMERIC                                        
216100           MOVE MID-IDPROENH-1      TO WS-IDPROENH(1)                     
216200           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-1-ATTR                
216300        ELSE                                                              
216400           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-1-ATTR                
216500           MOVE NEJ TO INPUT-RETT                                         
216600        END-IF                                                            
216700     END-IF                                                               
216800                                                                          
216900     IF MID-IDPROENH-2 = ALL '+'                                          
217000       CONTINUE                                                           
217100     ELSE                                                                 
217200       IF MID-IDPROENH-2  NUMERIC                                         
217300          MOVE MID-IDPROENH-2 TO WS-IDPROENH(2)                           
217400          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-2-ATTR                 
217500       ELSE                                                               
217600          MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-2-ATTR                 
217700          MOVE NEJ TO INPUT-RETT                                          
217800       END-IF                                                             
217900     END-IF                                                               
218000                                                                          
218100     IF MID-IDPROENH-3 = ALL '+'                                          
218200       CONTINUE                                                           
218300     ELSE                                                                 
218400       IF MID-IDPROENH-3  NUMERIC                                         
218500          MOVE MID-IDPROENH-3 TO WS-IDPROENH(3)                           
218600          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-3-ATTR                 
218700       ELSE                                                               
218800          MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-3-ATTR                 
218900          MOVE NEJ TO INPUT-RETT                                          
219000       END-IF                                                             
219100     END-IF                                                               
219200                                                                          
219300     IF MID-IDBERED NUMERIC                                               
219400       IF MID-IDBERED > ZERO                                              
219500          MOVE MID-IDBERED TO WS-IDBERED                                  
219600          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDBERED-ATTR                    
219700       ELSE                                                               
219800          MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                      
219900          MOVE NEJ TO INPUT-RETT                                          
220000       END-IF                                                             
220100     ELSE                                                                 
220200       MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                         
220300       MOVE NEJ TO INPUT-RETT                                             
220400     END-IF                                                               
220500                                                                          
220600     IF MID-KDSORT = 'ST' OR 'SA' OR 'KG' OR 'M ' OR ' M' OR              
220700       ' L' OR 'L ' OR 'MM' OR 'G ' OR ' G' OR 'C2' OR 'M2'               
220800       OR 'ML' OR 'SW' OR 'TM' OR 'HW' OR 'PA'                            
220900       MOVE MID-KDSORT TO WS-KDSORT                                       
221000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-ATTR                       
221100     ELSE                                                                 
221200       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                         
221300       MOVE NEJ TO INPUT-RETT                                             
221400     END-IF                                                               
221500                                                                          
221600     IF MID-IDAO =  ALL '+' OR SPACE                                      
221700        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDAO-ATTR                          
221800        MOVE NEJ TO INPUT-RETT                                            
221900     ELSE                                                                 
222000        MOVE MID-IDAO TO WS-IDAO                                          
222100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-ATTR                        
222200     END-IF                                                               
222300                                                                          
222400     MOVE MID-TISOP TO XX-TISOP                                           
222500     IF MID-TISOP = ALL '+'                                               
222600        MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                          
222700        MOVE NEJ               TO INPUT-RETT                              
222800     ELSE                                                                 
222900        MOVE '1'         TO XX-DAG                                        
223000                                                                          
223100        IF XX-TISOP NUMERIC                                               
223200           MOVE XX-TISOP TO WS-TISOP                                      
223300           MOVE WS-TISOP       TO DAT-I-TIDATUM                           
223400           MOVE 'AAVVD '       TO DAT-KDDATFORM                           
223500           PERFORM S99-WDATKONV                                           
223600           IF DAT-KDSVAR-OK                                               
223700              PERFORM S98-OM-TVA-AAR                                      
223800              MOVE WS-TISOP     TO TMP1-YYWWD                             
223900              MOVE AAVVD        TO TMP2-YYWWD                             
224000              MOVE DAT-TIAAVVD  TO TMP3-YYWWD                             
224100              PERFORM WY2000Q2                                            
224200              IF TMP1-YYWWD < TMP2-YYWWD                                  
224300                 MOVE MFS-NUM-FAELT-RAETT TO MOD-TISOP-ATTR               
224400                 MOVE WS-TISOP TO AAVVD                                   
224500                 MOVE +1 TO D                                             
224600                 MOVE AAVVD TO WS-TISOP                                   
224700                 IF TMP1-YYWWD > TMP3-YYWWD                               
224800                    MOVE ZERO     TO WS-YYWWD-VECKA-PLUS-1                
224900                 ELSE                                                     
225000*      TIFINLV SKALL SÄTTAS TILL DAGENS VECKA + 1                         
225100*      SPARA DETTA I WS-YYWWD-VECKA-PLUS-1                                
225200                    MOVE DAT-TIAAVVD TO WS-YYWWD-VECKA-PLUS-1             
225300                    IF WS-WW < 52                                         
225400                       ADD   1    TO WS-WW                                
225500                    ELSE                                                  
225600                       ADD   1    TO WS-YY                                
225700                       MOVE 01    TO WS-WW                                
225800                    END-IF                                                
225900                    MOVE     1    TO WS-D                                 
226000                 END-IF                                                   
226100               END-IF                                                     
226200                                                                          
226300           ELSE                                                           
226400              MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                    
226500              MOVE NEJ TO INPUT-RETT                                      
226600           END-IF                                                         
226700        ELSE                                                              
226800           MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                       
226900           MOVE NEJ TO INPUT-RETT                                         
227000        END-IF                                                            
227100     END-IF                                                               
227200                                                                          
227300     IF MID-IDFKNGRP NUMERIC                                              
227400       IF MID-IDFKNGRP > ZERO                                             
227500          MOVE MID-IDFKNGRP TO WS-IDFKNGRP                                
227600                               WS-TEST-IDFKNGRP                           
227700          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-ATTR                   
227800       ELSE                                                               
227900          MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                     
228000          MOVE NEJ TO INPUT-RETT                                          
228100       END-IF                                                             
228200     ELSE                                                                 
228300       MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                        
228400       MOVE NEJ TO INPUT-RETT                                             
228500     END-IF                                                               
228600                                                                          
228700     IF MID-TEARTNOT-2 = ALL '+'                                          
228800       CONTINUE                                                           
228900     ELSE                                                                 
229000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-2-ATTR                   
229100     END-IF                                                               
229200                                                                          
229300     IF MID-TEARTNOT-7 = ALL '+'                                          
229400       CONTINUE                                                           
229500     ELSE                                                                 
229600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-7-ATTR                   
229700     END-IF                                                               
229800                                                                          
229900     IF INPUT-RETT = JA                                                   
230000        PERFORM S93-KOLLA-SOFTWARE                                        
230100     END-IF                                                               
230200     .                                                                    
230300     EJECT                                                                
230400 S02-NYA-ARTREG-DATAELEMENT SECTION.                                      
230500     SKIP2                                                                
230600     MOVE SPACE TO INMATAD-IDKAT                                          
230700                                                                          
230800     IF MID-TEARTNOT-4 = ALL '+'                                          
230900       CONTINUE                                                           
231000     ELSE                                                                 
231100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-4-ATTR                   
231200     END-IF                                                               
231300                                                                          
231400     IF MID-IDKAT-1 = ALL '+'                                             
231500       CONTINUE                                                           
231600     ELSE                                                                 
231700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-1-ATTR                      
231800     END-IF                                                               
231900                                                                          
232000     IF MID-IDKAT-2 = ALL '+'                                             
232100        CONTINUE                                                          
232200     ELSE                                                                 
232300        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-2-ATTR                     
232400     END-IF                                                               
232500                                                                          
232600     IF MID-IDKAT-3 = ALL '+'                                             
232700        CONTINUE                                                          
232800     ELSE                                                                 
232900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-3-ATTR                     
233000     END-IF                                                               
233100                                                                          
233200     IF MID-IDPROJUP = ALL '+'                                            
233300       CONTINUE                                                           
233400     ELSE                                                                 
233500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJUP-ATTR                     
233600     END-IF                                                               
233700                                                                          
233800     IF MID-KDUART = 'A' OR 'M' OR 'S' OR 'P'                             
233900              OR 'K' OR 'B' OR SPACE                                      
234000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDUART-ATTR                       
234100     ELSE                                                                 
234200        IF MID-KDUART = ALL '+'                                           
234300           MOVE MFS-RENSA-FAELT TO MOD-KDUART-ATTR                        
234400        ELSE                                                              
234500           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDUART-ATTR                     
234600           MOVE NEJ TO INPUT-RETT                                         
234700        END-IF                                                            
234800     END-IF                                                               
234900                                                                          
235000     IF MID-KDYTBEH NUMERIC AND                                           
235100        MID-KDYTBEH < 10                                                  
235200***    NO MORE TESTING IF MID-KDYTBEH = 00 OR 01 OR 02 OR                 
235300***    (93-02-03)                       03 OR 04 OR 05 OR                 
235400***                                     06 OR 07 OR 08                    
235500       MOVE MFS-NUM-FAELT-RAETT TO MOD-KDYTBEH-ATTR                       
235600     ELSE                                                                 
235700       MOVE MFS-NUM-FAELT-FEL TO MOD-KDYTBEH-ATTR                         
235800       MOVE NEJ TO INPUT-RETT                                             
235900     END-IF                                                               
236000                                                                          
236100     IF MID-KDFARLIG NUMERIC                                              
236200        IF MID-KDFARLIG = 0 OR 3 OR 4 OR 6 OR 7                           
236300           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFARLIG-ATTR                  
236400        ELSE                                                              
236500           MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARLIG-ATTR                    
236600           MOVE NEJ TO INPUT-RETT                                         
236700        END-IF                                                            
236800     ELSE                                                                 
236900        MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARLIG-ATTR                       
237000        MOVE NEJ TO INPUT-RETT                                            
237100     END-IF                                                               
237200                                                                          
237300     IF MID-KDBPSR NUMERIC                                                
237400        IF MID-KDBPSR = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8              
237500           MOVE MFS-NUM-FAELT-RAETT       TO MOD-KDBPSR-ATTR              
237600        ELSE                                                              
237700           MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-ATTR                      
237800           MOVE NEJ TO INPUT-RETT                                         
237900        END-IF                                                            
238000     ELSE                                                                 
238100          MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-ATTR                       
238200          MOVE NEJ TO INPUT-RETT                                          
238300     END-IF                                                               
238400     IF MID-FLLSRDEL = JA OR NEJ                                          
238500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLSRDEL-ATTR                     
238600     ELSE                                                                 
238700       MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLSRDEL-ATTR                       
238800       MOVE NEJ TO INPUT-RETT                                             
238900     END-IF                                                               
239000     .                                                                    
239100     EJECT                                                                
239200 S03-NYA-NYPON-DATAELEMENT SECTION.                                       
239300                                                                          
239400     IF MID-TEORSAK-1 = ALL '+'                                           
239500       MOVE MFS-RENSA-FAELT TO MOD-TEORSAK-1                              
239600     ELSE                                                                 
239700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEORSAK-1-ATTR                    
239800     END-IF                                                               
239900     SKIP2                                                                
240000******************************************************************        
240100*    PROJK ÄR OBLIGATORISKT FÖR ARTIKLAR MED PRODUKTSLAG PV-BASL.         
240200*    I DETTA LÄGET SAKNAS NYPON-ART ATT KOPIERA FRÅN,         .           
240300*    PROJK MÅSTE NU ANGES I MID-PROJK.                                    
240400*    DESSA ARTIKLAR MÅSTE HA GODK-PROJK, UPPLAGDA PÅ BILD 1153.           
240500******************************************************************        
240600                                                                          
240700     MOVE WS-KDPRODSL TO TEST-KDPRODSL                                    
240800     IF MID-IDPROJK = ALL '+'                                             
240900        IF KDPRODSL-UTAN-EMB                                              
241000           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJK-ATTR                  
241100           MOVE NEJ TO INPUT-RETT                                         
241200        ELSE                                                              
241300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-ATTR                  
241400           IF KDPRODSL-LOCAL                                              
241500                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJK-ATTR            
241600                 MOVE NEJ TO INPUT-RETT                                   
241700           END-IF                                                         
241800        END-IF                                                            
241900     ELSE                                                                 
242000                                                                          
242100        MOVE MID-IDPROJK  TO WS-IDPROJK                                   
242200        IF KDPRODSL-UTAN-EMB OR KDPRODSL-LOCAL                            
242300           PERFORM S15-KTR-GODK-PROJK-PV                                  
242400           IF PROJK-GODK                                                  
242500              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-ATTR               
242600           ELSE                                                           
242700              MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJK-ATTR               
242800              MOVE NEJ TO INPUT-RETT                                      
242900           END-IF                                                         
243000        ELSE                                                              
243100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-ATTR                  
243200        END-IF                                                            
243300     END-IF                                                               
243400                                                                          
243500     IF MID-KVARTVAGN = ALL '+'                                           
243600        MOVE ZERO            TO WS-KVARTVAGN                              
243700     ELSE                                                                 
243800        IF MID-KVARTVAGN NUMERIC                                          
243900           MOVE MID-KVARTVAGN       TO WS-KVARTVAGN                       
244000        ELSE                                                              
244100           MOVE NEJ                    TO INPUT-RETT                      
244200        END-IF                                                            
244300     END-IF                                                               
244400                                                                          
244500     IF MID-IDARTNR-MOTSV = ALL '+'                                       
244600        MOVE ZERO                 TO WS-IDARTNR-MOTSV                     
244700     ELSE                                                                 
244800        IF MID-IDARTNR-MOTSV NUMERIC                                      
244900           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-MOTSV-ATTR            
245000           MOVE MID-IDARTNR-MOTSV    TO WS-IDARTNR-MOTSV                  
245100        ELSE                                                              
245200           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-MOTSV-ATTR            
245300           MOVE NEJ                  TO INPUT-RETT                        
245400        END-IF                                                            
245500     END-IF                                                               
245600                                                                          
245700     IF MID-FLPISK = ALL '+'                                              
245800        CONTINUE                                                          
245900     ELSE                                                                 
246000        IF MID-FLPISK = JA OR NEJ                                         
246100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPISK-ATTR                   
246200           MOVE MID-FLPISK TO WS-FLPISK                                   
246300        ELSE                                                              
246400           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLPISK-ATTR                     
246500           MOVE NEJ TO INPUT-RETT                                         
246600        END-IF                                                            
246700     END-IF                                                               
246800                                                                          
246900     IF MID-KVPROG = ALL '+'                                              
247000        MOVE ZERO TO WS-KVPROG                                            
247100     ELSE                                                                 
247200        IF MID-KVPROG NUMERIC                                             
247300           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPROG-ATTR                    
247400           MOVE MID-KVPROG TO WS-KVPROG                                   
247500        ELSE                                                              
247600           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROG-ATTR                      
247700           MOVE NEJ TO INPUT-RETT                                         
247800        END-IF                                                            
247900     END-IF                                                               
248000     .                                                                    
248100     EJECT                                                                
248200 S04-KOP-GEMENSAMMA-DATAELEMENT SECTION.                                  
248300     SKIP2                                                                
248400                                                                          
248500     MOVE ZERO TO WS-IDPROENH(1)                                          
248600                  WS-IDPROENH(2)                                          
248700                  WS-IDPROENH(3)                                          
248800                                                                          
248900     INSPECT MID-BEART REPLACING ALL '<' BY SPACE                         
249000     INSPECT MID-BEART REPLACING ALL '>' BY SPACE                         
249100                                                                          
249200     IF MID-BEART = ALL '+' OR SPACE                                      
249300        MOVE IDARTNR-WS TO W-IDARTNR                                      
249400        MOVE 'S  ' TO W-IDSKYLT                                           
249500        PERFORM IMS-GET-BENA11-CSEQ                                       
249600        MOVE BENA-TEXT-BEART TO WS-BEART                                  
249700                                WS-BEART-SVE                              
249800     ELSE                                                                 
249900       MOVE MID-BEART TO WS-BEART                                         
250000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-ATTR                        
250100     END-IF                                                               
250200                                                                          
250300     IF MID-IDRITN = ALL '+'                                              
250400       CONTINUE                                                           
250500     ELSE                                                                 
250600       MOVE MID-IDRITN TO WS-IDRITN                                       
250700     END-IF                                                               
250800                                                                          
250900     IF MID-IDPROJ = ALL '+'                                              
251000       CONTINUE                                                           
251100     ELSE                                                                 
251200       MOVE MID-IDPROJ TO WS-IDPROJ                                       
251300     END-IF                                                               
251400                                                                          
251500                                                                          
251600     IF MID-IDPROENH-1 = ALL '+'                                          
251700          CONTINUE                                                        
251800     ELSE                                                                 
251900          IF MID-IDPROENH-1  NUMERIC                                      
252000             MOVE MID-IDPROENH-1 TO WS-IDPROENH(1)                        
252100             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-1-ATTR              
252200          ELSE                                                            
252300             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-1-ATTR              
252400             MOVE NEJ TO INPUT-RETT                                       
252500          END-IF                                                          
252600     END-IF                                                               
252700                                                                          
252800     IF MID-IDPROENH-2 = ALL '+'                                          
252900          CONTINUE                                                        
253000     ELSE                                                                 
253100          IF MID-IDPROENH-2  NUMERIC                                      
253200             MOVE MID-IDPROENH-2 TO WS-IDPROENH(2)                        
253300             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-2-ATTR              
253400          ELSE                                                            
253500             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-2-ATTR              
253600             MOVE NEJ TO INPUT-RETT                                       
253700          END-IF                                                          
253800     END-IF                                                               
253900                                                                          
254000     IF MID-IDPROENH-3 = ALL '+'                                          
254100          CONTINUE                                                        
254200     ELSE                                                                 
254300          IF MID-IDPROENH-3  NUMERIC                                      
254400             MOVE MID-IDPROENH-3 TO WS-IDPROENH(3)                        
254500             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-3-ATTR              
254600          ELSE                                                            
254700             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPROENH-3-ATTR              
254800             MOVE NEJ TO INPUT-RETT                                       
254900          END-IF                                                          
255000     END-IF                                                               
255100                                                                          
255200     IF MID-IDBERED = ALL '+'                                             
255300       CONTINUE                                                           
255400     ELSE                                                                 
255500       IF MID-IDBERED NUMERIC                                             
255600         IF MID-IDBERED > ZERO                                            
255700            MOVE MID-IDBERED TO WS-IDBERED                                
255800            MOVE MFS-NUM-FAELT-RAETT TO MOD-IDBERED-ATTR                  
255900         ELSE                                                             
256000            MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                    
256100            MOVE NEJ TO INPUT-RETT                                        
256200         END-IF                                                           
256300       ELSE                                                               
256400         MOVE MFS-NUM-FAELT-FEL TO MOD-IDBERED-ATTR                       
256500         MOVE NEJ TO INPUT-RETT                                           
256600       END-IF                                                             
256700     END-IF                                                               
256800                                                                          
256900     IF MID-KDSORT = ALL '+'                                              
257000       MOVE IDARTNR-WS TO W-IDARTNR                                       
257100       PERFORM IMS-GET-ARTC01                                             
257200       IF SEGMENT-FINNS                                                   
257300          MOVE ART-KDSORT TO WS-KDSORT                                    
257400       END-IF                                                             
257500     ELSE                                                                 
257600       IF MID-KDSORT = 'ST' OR 'SA' OR 'KG' OR 'M ' OR ' M' OR            
257700       ' L' OR 'L ' OR 'MM' OR 'G ' OR ' G' OR 'C2' OR 'M2'               
257800       OR 'ML' OR 'SW' OR 'TM' OR 'HW' OR 'PA'                            
257900         MOVE MID-KDSORT TO WS-KDSORT                                     
258000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-ATTR                     
258100       ELSE                                                               
258200         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                       
258300         MOVE NEJ TO INPUT-RETT                                           
258400       END-IF                                                             
258500     END-IF                                                               
258600                                                                          
258700     IF MID-IDAO = ALL '+' OR SPACE                                       
258800       CONTINUE                                                           
258900     ELSE                                                                 
259000       MOVE MID-IDAO TO WS-IDAO                                           
259100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-ATTR                         
259200     END-IF                                                               
259300                                                                          
259400     MOVE MID-TISOP TO XX-TISOP                                           
259500     MOVE '+' TO XX-DAG                                                   
259600     IF XX-TISOP = ALL '+'                                                
259700        CONTINUE                                                          
259800     ELSE                                                                 
259900                                                                          
260000        IF  XX-AAR = '99'                                                 
260100        AND XX-VECKA = '99'                                               
260200            MOVE '9' TO XX-DAG                                            
260300        ELSE                                                              
260400            MOVE '1' TO XX-DAG                                            
260500        END-IF                                                            
260600                                                                          
260700        IF XX-TISOP NUMERIC                                               
260800           MOVE XX-TISOP TO WS-TISOP                                      
260900           IF WS-TISOP = 99999                                            
261000              MOVE WS-KDPRODSL   TO TEST-KDPRODSL                         
261100              IF KDPRODSL-PARTS-ACC OR KDPRODSL-SERVICES                  
261200                 IF FINNS-REG-PA-NYPON = JA                               
261300                    MOVE ARTG01-ART-KDPRODSL                              
261400                                 TO TEST-KDPRODSL                         
261500                    IF ARTG01-ART-FLUNIKRD = JA                           
261600                    OR   KDPRODSL-ACC                                     
261700                    OR   ARTG01-ART-IDLEVNR  = '9998 '                    
261800                       MOVE MFS-NUM-FAELT-FEL   TO                        
261900                                          MOD-TISOP-ATTR                  
262000                       MOVE NEJ                 TO                        
262100                                          INPUT-RETT                      
262200                    ELSE                                                  
262300                       PERFORM S96-LAES-XXAQ-KOP                          
262400                       IF SEGMENT-FINNS                                   
262500                          IF XXAQ-1132-TIFINLEV  = ZERO   AND             
262600                             XXAQ-1132-TIPRODSTA = +111111                
262700                             IF FINNS-REG-PA-NYPON = JA                   
262800                                PERFORM                                   
262900                                    HA-PLOCKA-NYASTE-TISERLEV             
263000                                IF W-SPAR-TISERLEV = +9999999             
263100                                   MOVE MFS-NUM-FAELT-RAETT TO            
263200                                               MOD-TISOP-ATTR             
263300                                   MOVE WS-TISOP TO AAVVD                 
263400                                   MOVE +9 TO D                           
263500                                   MOVE AAVVD      TO WS-TISOP            
263600                                ELSE                                      
263700                                   MOVE MFS-NUM-FAELT-FEL   TO            
263800                                                MOD-TISOP-ATTR            
263900                                   MOVE NEJ                 TO            
264000                                                   INPUT-RETT             
264100                                                                          
264200                                END-IF                                    
264300                             ELSE                                         
264400                                MOVE MFS-NUM-FAELT-RAETT TO               
264500                                                MOD-TISOP-ATTR            
264600                                MOVE WS-TISOP TO AAVVD                    
264700                                MOVE +9 TO D                              
264800                                MOVE AAVVD         TO WS-TISOP            
264900                             END-IF                                       
265000                          ELSE                                            
265100                             MOVE MFS-NUM-FAELT-FEL   TO                  
265200                                               MOD-TISOP-ATTR             
265300                             MOVE NEJ             TO INPUT-RETT           
265400                          END-IF                                          
265500                       ELSE                                               
265600                          MOVE MFS-NUM-FAELT-FEL      TO                  
265700                                            MOD-TISOP-ATTR                
265800                          MOVE NEJ                     TO                 
265900                                                 INPUT-RETT               
266000                       END-IF                                             
266100                    END-IF                                                
266200                 ELSE                                                     
266300                    MOVE MFS-NUM-FAELT-FEL      TO                        
266400                                      MOD-TISOP-ATTR                      
266500                    MOVE NEJ                     TO                       
266600                                           INPUT-RETT                     
266700                 END-IF                                                   
266800                                                                          
266900              ELSE                                                        
267000                 MOVE MFS-NUM-FAELT-FEL      TO                           
267100                                   MOD-TISOP-ATTR                         
267200                    MOVE NEJ                     TO                       
267300                                        INPUT-RETT                        
267400              END-IF                                                      
267500           ELSE                                                           
267600              MOVE WS-TISOP       TO DAT-I-TIDATUM                        
267700              MOVE 'AAVVD '       TO DAT-KDDATFORM                        
267800              PERFORM S99-WDATKONV                                        
267900              IF DAT-KDSVAR-OK                                            
268000                 PERFORM S98-OM-TVA-AAR                                   
268100                 MOVE WS-TISOP     TO TMP1-YYWWD                          
268200                 MOVE AAVVD        TO TMP2-YYWWD                          
268300                 MOVE DAT-TIAAVVD  TO TMP3-YYWWD                          
268400                 PERFORM WY2000Q2                                         
268500                 IF TMP1-YYWWD < TMP2-YYWWD                               
268600                    MOVE MFS-NUM-FAELT-RAETT TO                           
268700                                              MOD-TISOP-ATTR              
268800                    MOVE WS-TISOP TO AAVVD                                
268900                    MOVE +1 TO D                                          
269000                    MOVE AAVVD TO WS-TISOP                                
269100                    IF TMP1-YYWWD > TMP3-YYWWD                            
269200                       MOVE ZERO     TO WS-YYWWD-VECKA-PLUS-1             
269300                    ELSE                                                  
269400*      TIFINLV SKALL SÄTTAS TILL DAGENS VECKA + 1                         
269500*      SPARA DETTA I WS-YYWWD-VECKA-PLUS-1                                
269600                       MOVE DAT-TIAAVVD TO WS-YYWWD-VECKA-PLUS-1          
269700                       IF WS-WW < 52                                      
269800                          ADD   1    TO WS-WW                             
269900                       ELSE                                               
270000                          ADD   1    TO WS-YY                             
270100                          MOVE 01    TO WS-WW                             
270200                       END-IF                                             
270300                       MOVE     1    TO WS-D                              
270400                    END-IF                                                
270500                 END-IF                                                   
270600              ELSE                                                        
270700                 MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                 
270800                 MOVE NEJ TO INPUT-RETT                                   
270900              END-IF                                                      
271000           END-IF                                                         
271100        ELSE                                                              
271200           MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-ATTR                       
271300           MOVE NEJ TO INPUT-RETT                                         
271400        END-IF                                                            
271500     END-IF                                                               
271600                                                                          
271700     IF MID-IDFKNGRP = ALL '+'                                            
271800       MOVE IDARTNR-WS TO W-IDARTNR                                       
271900       PERFORM IMS-GET-ARTC01                                             
272000       IF SEGMENT-FINNS                                                   
272100          MOVE ART-IDFKNGRP TO WS-TEST-IDFKNGRP                           
272200       END-IF                                                             
272300     ELSE                                                                 
272400       IF MID-IDFKNGRP NUMERIC                                            
272500         IF MID-IDFKNGRP > ZERO                                           
272600            MOVE MID-IDFKNGRP TO WS-IDFKNGRP                              
272700                                 WS-TEST-IDFKNGRP                         
272800            MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-ATTR                 
272900         ELSE                                                             
273000            MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                   
273100            MOVE NEJ TO INPUT-RETT                                        
273200         END-IF                                                           
273300       ELSE                                                               
273400         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                      
273500         MOVE NEJ TO INPUT-RETT                                           
273600       END-IF                                                             
273700     END-IF                                                               
273800                                                                          
273900     IF MID-TEARTNOT-2 = ALL '+'                                          
274000       CONTINUE                                                           
274100     ELSE                                                                 
274200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-2-ATTR                   
274300     END-IF                                                               
274400                                                                          
274500     IF MID-TEARTNOT-7 = ALL '+'                                          
274600       CONTINUE                                                           
274700     ELSE                                                                 
274800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-7-ATTR                   
274900     END-IF                                                               
275000                                                                          
275100     IF INPUT-RETT = JA                                                   
275200        PERFORM S93-KOLLA-SOFTWARE                                        
275300     END-IF                                                               
275400     .                                                                    
275500     EJECT                                                                
275600 S05-KOP-ARTREG-DATAELEMENT SECTION.                                      
275700     SKIP2                                                                
275800     MOVE SPACE TO INMATAD-IDKAT                                          
275900                                                                          
276000     IF MID-TEARTNOT-4 = ALL '+' OR SPACE                                 
276100        MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-4-ATTR                       
276200     END-IF                                                               
276300                                                                          
276400     IF MID-KDUART = ALL '+'                                              
276500        CONTINUE                                                          
276600     ELSE                                                                 
276700        IF MID-KDUART = 'A' OR 'M' OR 'S' OR 'P' OR                       
276800                    'K' OR 'B' OR SPACE                                   
276900            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDUART-ATTR                  
277000        ELSE                                                              
277100            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDUART-ATTR                    
277200            MOVE NEJ TO INPUT-RETT                                        
277300        END-IF                                                            
277400     END-IF                                                               
277500                                                                          
277600     IF MID-KDYTBEH = ALL '+'                                             
277700        CONTINUE                                                          
277800     ELSE                                                                 
277900***     NO MORE TESTING IF MID-KDYTBEH = 00 OR 01 OR 02 OR                
278000***     (93-02-03)                       03 OR 04 OR 05 OR                
278100***                                      06 OR 07 OR 08                   
278200        IF MID-KDYTBEH NUMERIC AND                                        
278300           MID-KDYTBEH < 10                                               
278400          MOVE MFS-NUM-FAELT-RAETT TO MOD-KDYTBEH-ATTR                    
278500        ELSE                                                              
278600          MOVE MFS-NUM-FAELT-FEL TO MOD-KDYTBEH-ATTR                      
278700          MOVE NEJ TO INPUT-RETT                                          
278800        END-IF                                                            
278900     END-IF                                                               
279000                                                                          
279100     IF MID-KDFARLIG = ALL '+'                                            
279200        CONTINUE                                                          
279300     ELSE                                                                 
279400        IF MID-KDFARLIG NUMERIC                                           
279500           IF MID-KDFARLIG = 0 OR 3 OR 4 OR 6 OR 7                        
279600              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFARLIG-ATTR               
279700           ELSE                                                           
279800              MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARLIG-ATTR                 
279900              MOVE NEJ TO INPUT-RETT                                      
280000           END-IF                                                         
280100        ELSE                                                              
280200           MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARLIG-ATTR                    
280300           MOVE NEJ TO INPUT-RETT                                         
280400        END-IF                                                            
280500     END-IF                                                               
280600                                                                          
280700     IF MID-KDBPSR = ALL '+'                                              
280800        CONTINUE                                                          
280900     ELSE                                                                 
281000        IF MID-KDBPSR NUMERIC                                             
281100           IF MID-KDBPSR = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8           
281200              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBPSR-ATTR                 
281300           ELSE                                                           
281400              MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-ATTR                   
281500              MOVE NEJ TO INPUT-RETT                                      
281600           END-IF                                                         
281700        ELSE                                                              
281800           MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-ATTR                      
281900           MOVE NEJ TO INPUT-RETT                                         
282000        END-IF                                                            
282100     END-IF                                                               
282200                                                                          
282300     IF MID-FLLSRDEL = ALL '+'                                            
282400        CONTINUE                                                          
282500     ELSE                                                                 
282600       IF MID-FLLSRDEL = JA OR NEJ                                        
282700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLSRDEL-ATTR                   
282800       ELSE                                                               
282900         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLSRDEL-ATTR                     
283000         MOVE NEJ TO INPUT-RETT                                           
283100       END-IF                                                             
283200     END-IF                                                               
283300     .                                                                    
283400     EJECT                                                                
283500 S06-KOP-NYPON-DATAELEMENT SECTION.                                       
283600     SKIP2                                                                
283700                                                                          
283800     IF MID-TEORSAK-1 = ALL '+'                                           
283900       MOVE MFS-RENSA-FAELT TO MOD-TEORSAK-1                              
284000     ELSE                                                                 
284100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEORSAK-1-ATTR                    
284200     END-IF                                                               
284300     SKIP2                                                                
284400                                                                          
284500******************************************************************        
284600*    PROJK ÄR OBLIGATORISKT FÖR ARTIKLAR MED PRODUKTSLAG PV-BASL.         
284700*    NYPON-ART FINNS, PROJK KOPIERAS HÄRIFRÅN OM MID-PROJK SAKNAS.        
284800*    DESSA ARTIKLAR MÅSTE HA GODK-PROJK, UPPLAGDA PÅ BILD 1153.           
284900******************************************************************        
285000                                                                          
285100     MOVE WS-KDPRODSL TO TEST-KDPRODSL                                    
285200                                                                          
285300     IF MID-IDPROJK = ALL '+'                                             
285400        MOVE ARTG01-ART-IDPROJK TO WS-IDPROJK                             
285500     ELSE                                                                 
285600        MOVE MID-IDPROJK  TO WS-IDPROJK                                   
285700     END-IF                                                               
285800                                                                          
285900     IF KDPRODSL-UTAN-EMB OR KDPRODSL-LOCAL                               
286000        PERFORM S15-KTR-GODK-PROJK-PV                                     
286100        IF PROJK-GODK                                                     
286200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-ATTR                  
286300        ELSE                                                              
286400           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJK-ATTR                  
286500           MOVE NEJ TO INPUT-RETT                                         
286600        END-IF                                                            
286700     ELSE                                                                 
286800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-ATTR                     
286900     END-IF                                                               
287000                                                                          
287100     IF MID-KVARTVAGN = ALL '+'                                           
287200        MOVE ARTG01-ART-KVARTVAGN                TO WS-KVARTVAGN          
287300     ELSE                                                                 
287400        IF MID-KVARTVAGN NUMERIC                                          
287500           MOVE MID-KVARTVAGN       TO WS-KVARTVAGN                       
287600        ELSE                                                              
287700           MOVE NEJ                    TO INPUT-RETT                      
287800        END-IF                                                            
287900     END-IF                                                               
288000                                                                          
288100     IF MID-IDARTNR-MOTSV = ALL '+'                                       
288200        MOVE ZERO                 TO WS-IDARTNR-MOTSV                     
288300     ELSE                                                                 
288400        IF MID-IDARTNR-MOTSV NUMERIC                                      
288500           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-MOTSV-ATTR            
288600           MOVE MID-IDARTNR-MOTSV    TO WS-IDARTNR-MOTSV                  
288700        ELSE                                                              
288800           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-MOTSV-ATTR            
288900           MOVE NEJ                  TO INPUT-RETT                        
289000        END-IF                                                            
289100     END-IF                                                               
289200                                                                          
289300     IF MID-FLPISK = ALL '+'                                              
289400        CONTINUE                                                          
289500     ELSE                                                                 
289600        IF MID-FLPISK = JA OR NEJ                                         
289700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPISK-ATTR                   
289800           MOVE MID-FLPISK TO WS-FLPISK                                   
289900        ELSE                                                              
290000           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLPISK-ATTR                     
290100           MOVE NEJ TO INPUT-RETT                                         
290200        END-IF                                                            
290300     END-IF                                                               
290400                                                                          
290500     IF MID-KVPROG = ALL '+'                                              
290600        CONTINUE                                                          
290700     ELSE                                                                 
290800        IF MID-KVPROG NUMERIC                                             
290900           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPROG-ATTR                    
291000           MOVE MID-KVPROG TO WS-KVPROG                                   
291100        ELSE                                                              
291200           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROG-ATTR                      
291300           MOVE NEJ TO INPUT-RETT                                         
291400        END-IF                                                            
291500     END-IF                                                               
291600                                                                          
291700     .                                                                    
291800     EJECT                                                                
291900 S07-KOLLA-OM-BEN-FINNS SECTION.                                          
292000*****************************************************************         
292100*  ÄT NOV 92 VID IDSKYLT = GB  GODKÄNNES BARA NAMNLEX BENÄMNING *         
292200*             -  GÄLLER INTE VID KOPIERING AV BENÄMNING         *         
292300*****************************************************************         
292400     SKIP2                                                                
292500     MOVE NEJ TO SAMMA-BEN                                                
292600****  FÖR ATT FÅ SAMMA HOMONYMKOD                                         
292700     IF KOPIERING = JA                                                    
292800        MOVE IDARTNR-WS TO W-IDARTNR                                      
292900        PERFORM IMS-GU-BENA01-CSEQ                                        
293000        MOVE WS-IDSKYLT TO W-IDSKYLT                                      
293100        PERFORM IMS-GNP-BENA11-CSEQ                                       
293200        IF BENA-TEXT-BEART = WS-BEART                                     
293300           MOVE JA TO SAMMA-BEN                                           
293400        END-IF                                                            
293500     END-IF                                                               
293600                                                                          
293700     IF SAMMA-BEN = NEJ                                                   
293800        MOVE WS-IDSKYLT TO W-IDSKYLT                                      
293900        MOVE WS-BEART TO W-BEART                                          
294000        PERFORM IMS-GET-BENA01-ASEQ                                       
294100                                                                          
294200        IF WS-IDSKYLT = 'S  '                                             
294300           PERFORM UNTIL SEGMENT-SAKNAS                                   
294400                         OR BENA-BEN-KDHOMONYM = 0                        
294500              PERFORM IMS-GET-BENA01-ASEQ                                 
294600           END-PERFORM                                                    
294700                                                                          
294800           IF SEGMENT-FINNS                                               
294900********     OM BENÄMNING FINNS PÅ BENÄMNINGSREGISTRET                    
295000                 PERFORM IMS-GET-BENA13-ASEQ                              
295100                                                                          
295200                 IF SEGMENT-FINNS                                         
295300**************     OM HOMONYMKOD FINNS                                    
295400                    MOVE BENA-HOM-TEHOMONYM TO WS-TEHOMONYM               
295500                    IF RS-BM-NAMN = 'BM-NAMN' OR 'RS-NAMN'                
295600                       CONTINUE                                           
295700                    ELSE                                                  
295800                       MOVE FEL-6 (SPIND) TO MOD-TEMFSFEL                 
295900                    END-IF                                                
296000                 ELSE                                                     
296100**************  OM HOMONYMKOD SAKNAS                                      
296200                    CONTINUE                                              
296300                 END-IF                                                   
296400           ELSE                                                           
296500********     OM BENÄMNING SAKNAS PÅ BENÄMNINGSREGISTRET                   
296600                 MOVE 'A' TO WS-VAR                                       
296700                 MOVE FEL-5 (SPIND) TO MOD-TEMFSINF                       
296800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR                
296900                 MOVE NEJ TO INPUT-RETT                                   
297000           END-IF                                                         
297100        END-IF                                                            
297200     END-IF                                                               
297300     .                                                                    
297400     EJECT                                                                
297500 S08-KOLLA-INDATA-NYREG SECTION.                                          
297600     SKIP2                                                                
297700     MOVE JA TO INPUT-RETT                                                
297800     MOVE JA TO NYPON-ARTIKEL                                             
297900                                                                          
298000     MOVE ZERO TO WS-KDPRODSL                                             
298100     IF MID-KDPRODSL NUMERIC                                              
298200        MOVE MID-KDPRODSL   TO TEST-KDPRODSL                              
298300        IF GOOD-KDPRODSL                                                  
298400           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-ATTR                  
298500           MOVE MID-KDPRODSL TO WS-KDPRODSL                               
298600           PERFORM S94-KOLLA-IDDC                                         
298700        ELSE                                                              
298800           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                    
298900           MOVE NEJ TO INPUT-RETT                                         
299000        END-IF                                                            
299100     ELSE                                                                 
299200       MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                        
299300       MOVE NEJ TO INPUT-RETT                                             
299400     END-IF                                                               
299500                                                                          
299600     IF INPUT-RETT = JA                                                   
299700        PERFORM S95-KOLLA-IDFTG                                           
299800     END-IF                                                               
299900                                                                          
300000     IF NYPON-ARTIKEL = JA                                                
300100        PERFORM S03-NYA-NYPON-DATAELEMENT                                 
300200     END-IF                                                               
300300                                                                          
300400     PERFORM S01-NYA-GEMENSAMMA-DATAELEMENT                               
300500     PERFORM S02-NYA-ARTREG-DATAELEMENT                                   
300600     PERFORM S23-KOLLA-RASA                                               
300700     .                                                                    
300800     EJECT                                                                
300900 S09-KOLLA-INDATA-KOP-ARTREG SECTION.                                     
301000     SKIP2                                                                
301100     MOVE JA TO INPUT-RETT                                                
301200     MOVE JA TO NYPON-ARTIKEL                                             
301300                                                                          
301400     PERFORM S20-KOLLA-NYPON-ARTIKEL-KOP                                  
301500                                                                          
301600     IF NYPON-ARTIKEL = JA                                                
301700        IF FINNS-REG-PA-NYPON = JA                                        
301800           PERFORM S06-KOP-NYPON-DATAELEMENT                              
301900        ELSE                                                              
302000           PERFORM S03-NYA-NYPON-DATAELEMENT                              
302100        END-IF                                                            
302200     END-IF                                                               
302300     PERFORM S04-KOP-GEMENSAMMA-DATAELEMENT                               
302400     PERFORM S05-KOP-ARTREG-DATAELEMENT                                   
302500     PERFORM S23-KOLLA-RASA                                               
302600     .                                                                    
302700     EJECT                                                                
302800 S10-KOLLA-INDATA-KOP-ARTREG SECTION.                                     
302900     SKIP2                                                                
303000     MOVE JA TO INPUT-RETT                                                
303100     MOVE JA TO NYPON-ARTIKEL                                             
303200                                                                          
303300     PERFORM S20-KOLLA-NYPON-ARTIKEL-KOP                                  
303400                                                                          
303500     IF NYPON-ARTIKEL = JA                                                
303600        PERFORM S06-KOP-NYPON-DATAELEMENT                                 
303700     END-IF                                                               
303800                                                                          
303900     IF MID-IDPROJK = ALL '+'                                             
304000        MOVE WS-IDPROJK-GAM TO WS-IDPROJK                                 
304100     END-IF                                                               
304200     PERFORM S04-KOP-GEMENSAMMA-DATAELEMENT                               
304300     PERFORM S05-KOP-ARTREG-DATAELEMENT                                   
304400     PERFORM S23-KOLLA-RASA                                               
304500     .                                                                    
304600     EJECT                                                                
304700 S12-REGISTRERA-NYPON SECTION.                                            
304800     SKIP2                                                                
304900     MOVE IDARTNR-WS      TO ARTG01-ART-IDARTNR                           
305000     MOVE WS-BEART-SVE    TO ARTG01-ART-BEART-SVE                         
305100     MOVE WS-FLBYTES      TO ARTG01-ART-FLBYTES                           
305200     MOVE WS-FLPISK       TO ARTG01-ART-FLPISK                            
305300     MOVE WS-KVPROG       TO ARTG01-ART-KVPROG                            
305400     MOVE WS-IDAO         TO ARTG01-ART-IDAO                              
305500     MOVE WS-IDBERED      TO ARTG01-ART-IDBERED                           
305600     MOVE WS-IDFKNGRP     TO ARTG01-ART-IDFKNGRP                          
305700                                                                          
305800     IF WS-IDPROENH(1)= ZERO                                              
305900        IF WS-IDPROENH(2) = ZERO                                          
306000           MOVE WS-IDPROENH(3) TO ARTG01-ART-IDPROENH                     
306100        ELSE                                                              
306200           MOVE WS-IDPROENH(2) TO ARTG01-ART-IDPROENH                     
306300        END-IF                                                            
306400     ELSE                                                                 
306500        MOVE WS-IDPROENH(1)    TO ARTG01-ART-IDPROENH                     
306600     END-IF                                                               
306700                                                                          
306800     IF WS-IDRITN = '='                                                   
306900*****  KOPIERA ARTIKELNR TILL RITNINGSNR                                  
307000*****  ("KAPA AV" INLEDANDE NOLLOR)                                       
307100                                                                          
307200       MOVE WS-IDARTNR TO WS-W009REDU-IN                                  
307300       INSPECT WS-W009REDU-IN REPLACING LEADING ZERO BY SPACE             
307400       CALL W009REDU USING WS-W009REDU-IN WS-W009REDU-UT                  
307500       MOVE WS-W009REDU-UT TO ARTG01-ART-IDRITN                           
307600     ELSE                                                                 
307700       MOVE WS-IDRITN     TO ARTG01-ART-IDRITN                            
307800     END-IF                                                               
307900                                                                          
308000     MOVE WS-IDPROJ       TO ARTG01-ART-IDPROJ                            
308100     MOVE WS-IDPROJK      TO ARTG01-ART-IDPROJK                           
308200     MOVE WS-KVARTVAGN    TO ARTG01-ART-KVARTVAGN                         
308300     MOVE WS-KDPRODSL     TO ARTG01-ART-KDPRODSL                          
308400     MOVE WS-KDSORT       TO ARTG01-ART-KDSORT                            
308500     MOVE WS-IDARTNR-MOTSV                                                
308600                          TO ARTG01-ART-IDARTNR-MOTSV                     
308700                                                                          
308800     IF WS-TISOP = 99999                                                  
308900        MOVE 99999999     TO ARTG01-ART-DAFINLEV                          
309000     ELSE                                                                 
309100        MOVE 'AAVVD '     TO DAT-KDDATFORM                                
309200        MOVE WS-TISOP     TO DAT-I-TIDATUM                                
309300        PERFORM S99-WDATKONV                                              
309400        IF DAT-KDSVAR-OK                                                  
309500           MOVE DAT-TIAAMMDD TO ARTG01-ART-DAFINLEV                       
309600           MOVE DAT-TISEKEL  TO ARTG01-ART-DAFINLEV (1:2)                 
309700        END-IF                                                            
309800     END-IF                                                               
309900                                                                          
310000     IF MID-TEORSAK-1 = ALL '+'                                           
310100        MOVE SPACE          TO ARTG01-ART-TEORSAK                         
310200     ELSE                                                                 
310300        MOVE MID-TEORSAK-1 TO ARTG01-ART-TEORSAK                          
310400     END-IF                                                               
310500                                                                          
310600     MOVE ARTG01-ART-TEORSAK       TO MOD-TEORSAK-1                       
310700     MOVE ARTG01-ART-FLPISK        TO MOD-FLPISK-UT                       
310800     MOVE ARTG01-ART-IDPROJK       TO MOD-IDPROJK-UT                      
310900     MOVE ARTG01-ART-KVARTVAGN     TO MOD-KVARTVAGN-UT                    
311000     MOVE ARTG01-ART-IDARTNR-MOTSV TO MOD-IDARTNR-MOTSV-UT                
311100                                                                          
311200     PERFORM S21-NOLLSTAELL-NYPON                                         
311300     PERFORM IMS-ISRT-ARTG01                                              
311400     .                                                                    
311500     EJECT                                                                
311600 S13-VISA-BILD-IGEN SECTION.                                              
311700     SKIP2                                                                
311800     MOVE MFS-ROER-EJ-FAELT TO                                            
311900                               MOD-IDPROENH-1-UT                          
312000                               MOD-IDPROENH-2-UT                          
312100                               MOD-IDPROENH-3-UT                          
312200                               MOD-IDPROJ-UT                              
312300                               MOD-KDUART-UT                              
312400                               MOD-KVARTVAGN-UT                           
312500                               MOD-IDPROJK-UT                             
312600                               MOD-FLPISK-UT                              
312700                               MOD-IDLEVNR-UT                             
312800                               MOD-TEORSAK-1                              
312900                               MOD-IDARTNR-MOTSV-UT                       
313000     .                                                                    
313100     EJECT                                                                
313200 S14-GODK-PROJ-MFS-RAETT-FEL SECTION.                                     
313300                                                                          
313400******************************************************************        
313500*     PROJEKT ÄR OBLIGATORISKT FÖR ALLA ARTIKLAR MED                      
313600*     PRODUKTSLAG PV-BASL OCH PRODUKTSLAG CARPAC.                         
313700*     DESSA ARTIKLAR MÅSTE HA GODK-PROJ, UPPLAGD PÅ BILD 1153.            
313800*     ARTKLAR MED PRODSL. 19 31 34 35 38  INGEN KONTROLL.                 
313900*     ÖVRIGA ARTIKLAR FÅR EJ HA PROJ BLANK.                               
314000******************************************************************        
314100                                                                          
314200     MOVE WS-KDPRODSL TO W-KDPRODSL                                       
314300                         TEST-KDPRODSL                                    
314400     MOVE NEJ TO SW-PROJ-GODK                                             
314500                                                                          
314600     IF KDPRODSL-VOLVO-UTAN-EMB                                           
314700     OR KDPRODSL-LOCAL                                                    
314800                                                                          
314900        PERFORM IMS-GU-XXAQ01                                             
315000        IF SEGMENT-FINNS                                                  
315100           PERFORM IMS-GNP-XXAQ11                                         
315200           PERFORM UNTIL (PROJ-GODK) OR (SEGMENT-SAKNAS)                  
315300             IF XXAQ-1132-IDPROJ = WS-IDPROJ                              
315400                MOVE JA TO SW-PROJ-GODK                                   
315500             ELSE                                                         
315600                PERFORM IMS-GNP-XXAQ11                                    
315700             END-IF                                                       
315800           END-PERFORM                                                    
315900        END-IF                                                            
316000        IF PROJ-GODK                                                      
316100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-ATTR                   
316200        ELSE                                                              
316300           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJ-ATTR                   
316400           MOVE NEJ TO INPUT-RETT                                         
316500        END-IF                                                            
316600     ELSE                                                                 
316700        IF KDPRODSL-EMB                                                   
316800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-ATTR                   
316900        ELSE                                                              
317000* * * * *  ÖVRIGA PRODUKTSLAG * * * * * * * * * * * * * * * *             
317100           IF WS-IDPROJ = SPACE                                           
317200              MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJ-ATTR                
317300              MOVE NEJ TO INPUT-RETT                                      
317400           ELSE                                                           
317500              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-ATTR                
317600           END-IF                                                         
317700        END-IF                                                            
317800     END-IF                                                               
317900     .                                                                    
318000     EJECT                                                                
318100 S15-KTR-GODK-PROJK-PV SECTION.                                           
318200                                                                          
318300     MOVE NEJ TO SW-PROJK-GODK                                            
318400     MOVE WS-KDPRODSL TO W-KDPRODSL                                       
318500                                                                          
318600     PERFORM IMS-GU-XXAQ01                                                
318700     IF SEGMENT-FINNS                                                     
318800        PERFORM IMS-GNP-XXAQ11                                            
318900        PERFORM UNTIL (PROJK-GODK) OR (SEGMENT-SAKNAS)                    
319000          IF XXAQ-1132-IDPROJK = WS-IDPROJK                               
319100             MOVE JA TO SW-PROJK-GODK                                     
319200          ELSE                                                            
319300             PERFORM IMS-GNP-XXAQ11                                       
319400          END-IF                                                          
319500        END-PERFORM                                                       
319600     END-IF                                                               
319700     .                                                                    
319800     EJECT                                                                
319900 S19-KOP-FRAN-ARTREG-TILL-NYPON SECTION.                                  
320000     SKIP2                                                                
320100     MOVE IDARTNR-WS TO W-IDARTNR                                         
320200     PERFORM IMS-GET-ARTC01                                               
320300                                                                          
320400     IF MID-IDFKNGRP = ALL '+'                                            
320500        MOVE ART-IDFKNGRP            TO ARTG01-ART-IDFKNGRP               
320600     ELSE                                                                 
320700        MOVE WS-IDFKNGRP             TO ARTG01-ART-IDFKNGRP               
320800     END-IF                                                               
320900     IF MID-KDPRODSL = ALL '+'                                            
321000        MOVE ART-KDPRODSL            TO ARTG01-ART-KDPRODSL               
321100     ELSE                                                                 
321200        MOVE WS-KDPRODSL             TO ARTG01-ART-KDPRODSL               
321300     END-IF                                                               
321400     IF MID-KDSORT = ALL '+'                                              
321500        MOVE ART-KDSORT              TO ARTG01-ART-KDSORT                 
321600     ELSE                                                                 
321700        MOVE WS-KDSORT               TO ARTG01-ART-KDSORT                 
321800     END-IF                                                               
321900                                                                          
322000     IF MID-IDAO = ALL '+'                                                
322100        MOVE ART-IDAO(1)             TO ARTG01-ART-IDAO                   
322200     ELSE                                                                 
322300        MOVE WS-IDAO                 TO ARTG01-ART-IDAO                   
322400     END-IF                                                               
322500                                                                          
322600     IF XX-TISOP = ALL '+'                                                
322700        IF ART-TISOP = 99999                                              
322800           MOVE 99999999            TO ARTG01-ART-DAFINLEV                
322900        ELSE                                                              
323000           MOVE 'AAVVD ' TO DAT-KDDATFORM                                 
323100           MOVE ART-TISOP TO DAT-I-TIDATUM                                
323200           PERFORM S99-WDATKONV                                           
323300           IF DAT-KDSVAR-OK                                               
323400              MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV                  
323500              MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)            
323600           END-IF                                                         
323700        END-IF                                                            
323800     ELSE                                                                 
323900        IF WS-TISOP = 99999                                               
324000           MOVE 99999999            TO ARTG01-ART-DAFINLEV                
324100        ELSE                                                              
324200           MOVE 'AAVVD '            TO DAT-KDDATFORM                      
324300           MOVE WS-TISOP            TO DAT-I-TIDATUM                      
324400           PERFORM S99-WDATKONV                                           
324500           IF DAT-KDSVAR-OK                                               
324600              MOVE DAT-TIAAMMDD   TO ARTG01-ART-DAFINLEV                  
324700              MOVE DAT-TISEKEL    TO ARTG01-ART-DAFINLEV (1:2)            
324800           END-IF                                                         
324900        END-IF                                                            
325000     END-IF                                                               
325100                                                                          
325200     PERFORM IMS-GET-ARTC11                                               
325300     IF MID-IDBERED = ALL '+'                                             
325400        MOVE CLAG-IDBERED            TO ARTG01-ART-IDBERED                
325500     ELSE                                                                 
325600        MOVE WS-IDBERED              TO ARTG01-ART-IDBERED                
325700     END-IF                                                               
325800     IF MID-IDPROJ = ALL '+'                                              
325900        MOVE CLAG-IDPROJ             TO ARTG01-ART-IDPROJ                 
326000     ELSE                                                                 
326100        MOVE WS-IDPROJ               TO ARTG01-ART-IDPROJ                 
326200     END-IF                                                               
326300                                                                          
326400     IF MID-IDRITN = ALL '+'                                              
326500        MOVE CLAG-IDRITN             TO ARTG01-ART-IDRITN                 
326600     ELSE                                                                 
326700       IF WS-IDRITN = '='                                                 
326800*****    KOPIERA ARTIKELNR TILL RITNINGSNR                                
326900*****    ("KAPA AV" INLEDANDE NOLLOR)                                     
327000                                                                          
327100         MOVE WS-NY-IDARTNR TO WS-W009REDU-IN                             
327200         INSPECT WS-W009REDU-IN REPLACING LEADING ZERO BY SPACE           
327300         CALL W009REDU USING WS-W009REDU-IN WS-W009REDU-UT                
327400         MOVE WS-W009REDU-UT         TO ARTG01-ART-IDRITN                 
327500       ELSE                                                               
327600         MOVE WS-IDRITN              TO ARTG01-ART-IDRITN                 
327700       END-IF                                                             
327800     END-IF                                                               
327900                                                                          
328000     IF INMATAD-IDPROENH = ZERO                                           
328100        MOVE CLAG-IDPROENH(1) TO WS-IDPROENH(1)                           
328200        MOVE CLAG-IDPROENH(2) TO WS-IDPROENH(2)                           
328300        MOVE CLAG-IDPROENH(3) TO WS-IDPROENH(3)                           
328400     END-IF                                                               
328500                                                                          
328600     IF WS-IDPROENH(1)= ZERO                                              
328700        IF WS-IDPROENH(2) = ZERO                                          
328800           MOVE WS-IDPROENH(3) TO ARTG01-ART-IDPROENH                     
328900        ELSE                                                              
329000           MOVE WS-IDPROENH(2) TO ARTG01-ART-IDPROENH                     
329100        END-IF                                                            
329200     ELSE                                                                 
329300        MOVE WS-IDPROENH(1)    TO ARTG01-ART-IDPROENH                     
329400     END-IF                                                               
329500                                                                          
329600     .                                                                    
329700     EJECT                                                                
329800 S20-KOLLA-NYPON-ARTIKEL-KOP SECTION.                                     
329900     SKIP2                                                                
330000     IF MID-TISOP     =   ALL '+'                                         
330100        MOVE ART-TISOP             TO MID-TISOP                           
330200     END-IF                                                               
330300     IF MID-IDAO      =   ALL '+'                                         
330400        MOVE ART-IDAO (1)        TO MID-IDAO                              
330500     END-IF                                                               
330600                                                                          
330700     IF MID-KDBPSR   = ALL '+'                                            
330800        MOVE IDARTNR-WS TO W-IDARTNR                                      
330900        PERFORM IMS-GET-ARTC01                                            
331000        PERFORM IMS-GET-ARTC11                                            
331100        MOVE CLAG-KDBPSR TO MID-KDBPSR                                    
331200     END-IF                                                               
331300                                                                          
331400     MOVE ZERO TO WS-KDPRODSL                                             
331500     IF MID-KDPRODSL = ALL '+'                                            
331600        MOVE IDARTNR-WS TO W-IDARTNR                                      
331700        PERFORM IMS-GET-ARTC01                                            
331800        MOVE ART-KDPRODSL TO WS-KDPRODSL                                  
331900        PERFORM S94-KOLLA-IDDC                                            
332000     ELSE                                                                 
332100        IF MID-KDPRODSL NUMERIC                                           
332200           MOVE MID-KDPRODSL  TO TEST-KDPRODSL                            
332300           IF GOOD-KDPRODSL                                               
332400              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-ATTR               
332500              MOVE MID-KDPRODSL TO WS-KDPRODSL                            
332600              PERFORM S94-KOLLA-IDDC                                      
332700           ELSE                                                           
332800              MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                 
332900              MOVE NEJ TO INPUT-RETT                                      
333000           END-IF                                                         
333100        ELSE                                                              
333200           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                    
333300           MOVE NEJ TO INPUT-RETT                                         
333400        END-IF                                                            
333500     END-IF                                                               
333600                                                                          
333700     IF INPUT-RETT = JA                                                   
333800        PERFORM S95-KOLLA-IDFTG                                           
333900     END-IF                                                               
334000                                                                          
334100     IF MID-IDPROJ = ALL '+'                                              
334200        MOVE IDARTNR-WS TO W-IDARTNR                                      
334300        PERFORM IMS-GET-ARTC01                                            
334400        PERFORM IMS-GET-ARTC11                                            
334500        MOVE CLAG-IDPROJ       TO MID-IDPROJ WS-IDPROJ                    
334600     ELSE                                                                 
334700        MOVE MID-IDPROJ        TO WS-IDPROJ                               
334800     END-IF                                                               
334900                                                                          
335000     PERFORM S14-GODK-PROJ-MFS-RAETT-FEL                                  
335100                                                                          
335200     IF MID-IDRITN = ALL '+'                                              
335300        MOVE IDARTNR-WS TO W-IDARTNR                                      
335400        PERFORM IMS-GET-ARTC01                                            
335500        PERFORM IMS-GET-ARTC11                                            
335600        IF CLAG-IDRITN = SPACE                                            
335700           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                     
335800           MOVE NEJ TO INPUT-RETT                                         
335900        ELSE                                                              
336000           MOVE CLAG-IDRITN        TO MID-IDRITN WS-IDRITN                
336100        END-IF                                                            
336200     ELSE                                                                 
336300        IF MID-IDRITN = SPACE                                             
336400           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRITN-ATTR                     
336500           MOVE NEJ TO INPUT-RETT                                         
336600        ELSE                                                              
336700           MOVE MID-IDRITN            TO WS-IDRITN                        
336800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDRITN-ATTR                   
336900        END-IF                                                            
337000     END-IF                                                               
337100                                                                          
337200     MOVE IDARTNR-NY-WS TO W-IDARTNR                                      
337300     PERFORM IMS-GET-ARTG01                                               
337400     IF SEGMENT-FINNS                                                     
337500        MOVE JA TO FINNS-REG-PA-NYPON                                     
337600        IF NYPON-ARTIKEL = NEJ                                            
337700           MOVE FEL-10 (SPIND) TO MOD-TEMFSINF                            
337800           MOVE NEJ TO INPUT-RETT                                         
337900        END-IF                                                            
338000     END-IF                                                               
338100     .                                                                    
338200     EJECT                                                                
338300 S21-NOLLSTAELL-NYPON SECTION.                                            
338400     SKIP2                                                                
338500     MOVE SPACE                      TO                                   
338600                                        ARTG01-ART-FLUPB                  
338700                                        ARTG01-ART-FLPLAKOP               
338800                                        ARTG01-ART-FLAENDR                
338900                                        ARTG01-ART-FLBASL                 
339000                                        ARTG01-ART-FLBERQ                 
339100                                        ARTG01-ART-FLRITB                 
339200                                        ARTG01-ART-FLRITC                 
339300                                        ARTG01-ART-FLRITP                 
339400                                        ARTG01-ART-KDARTUTG               
339500                                        ARTG01-ART-FLUPG                  
339600                                        ARTG01-ART-KDTPD                  
339700                                        ARTG01-ART-FLUNIKRD               
339800                                        ARTG01-ART-IDMATKTO               
339900                                        ARTG01-ART-IDPROJOBJ              
340000                                        ARTG01-ART-IDRITUTG               
340100                                        ARTG01-ART-KDANSKQ                
340200                                        ARTG01-ART-KDARTTYP               
340300                                        ARTG01-ART-KDRESBED               
340400                                        ARTG01-ART-TETEKNIK               
340500                                        ARTG01-ART-TEANSINK               
340600                                        ARTG01-ART-TEARTNOT-BASL          
340700                                        ARTG01-ART-TEARTNOT               
340800                                        ARTG01-ART-KDKOPTYP               
340900                                        ARTG01-ART-IDLEVNR                
341000                                        ARTG01-ART-IDSTEKN                
341100                                        ARTG01-ART-IDLEVNR-FORB(1)        
341200                                        ARTG01-ART-IDLEVNR-FORB(2)        
341300                                        ARTG01-ART-IDLEVNR-FORB(3)        
341400                                        ARTG01-ART-IDLEVNR-FORB(4)        
341500                                        ARTG01-ART-IDLEVNR-FORB(5)        
341600     MOVE ZERO                       TO ARTG01-ART-IDANSK                 
341700                                        ARTG01-ART-IDINK                  
341800                                        ARTG01-ART-IDAVD                  
341900                                        ARTG01-ART-IDANSK-REG             
342000                                        ARTG01-ART-KDSTAINK               
342100                                        ARTG01-ART-KVARTAR1               
342200                                        ARTG01-ART-KVARTAR2               
342300                                        ARTG01-ART-KVARTAR3               
342400                                        ARTG01-ART-KVBASL                 
342500                                        ARTG01-ART-KVLEVBEG               
342600                                        ARTG01-ART-KVUPB                  
342700                                        ARTG01-ART-PRARTBES               
342800                                        ARTG01-ART-DABASL                 
342900                                        ARTG01-ART-TIINKOP                
343000                                        ARTG01-ART-TILEVBEG               
343100                                        ARTG01-ART-TINEDBRY               
343200                                        ARTG01-ART-TIPLAKOP               
343300                                        ARTG01-ART-TIREGDAT               
343400                                        ARTG01-ART-TIANSKREG              
343500                                        ARTG01-ART-TIRITB                 
343600                                        ARTG01-ART-TIRITC                 
343700                                        ARTG01-ART-TIRITP                 
343800                                        ARTG01-ART-TISERLEV(1)            
343900                                        ARTG01-ART-TISERLEV(2)            
344000                                        ARTG01-ART-TISERLEV(3)            
344100                                        ARTG01-ART-TISERLEV(4)            
344200                                        ARTG01-ART-TISERLEV(5)            
344300                                        ARTG01-ART-TISLUBER               
344400                                        ARTG01-ART-TISTABER               
344500                                        ARTG01-ART-TISTOMREG              
344600                                        ARTG01-ART-TIUPPDAT               
344700                                        ARTG01-ART-TIUPB                  
344800                                        ARTG01-ART-TIUPG                  
344900                                        ARTG01-ART-IDINKTEK               
345000                                        ARTG01-ART-TITPD                  
345100                                        ARTG01-ART-TIMOTSI                
345200     .                                                                    
345300     EJECT                                                                
345400 S22-FIXA-DOLDA-FAELT   SECTION.                                          
345500     SKIP2                                                                
345600     MOVE ARTG01-ART-IDBERED               TO WS-IDBERED-2-NUM            
345700     .                                                                    
345800     EJECT                                                                
345900 S23-KOLLA-RASA SECTION.                                                  
346000     SKIP2                                                                
346100     IF KOPIERING = NEJ                                                   
346200        MOVE IDARTNR-WS TO W-IDARTNR                                      
346300     ELSE                                                                 
346400        MOVE IDARTNR-NY-WS TO W-IDARTNR                                   
346500     END-IF                                                               
346600     PERFORM IMS-GET-SATB01                                               
346700     IF SEGMENT-FINNS                                                     
346800        IF WS-KDSORT = 'SA' OR 'TM'                                       
346900           IF SATB-STR-IDSTRTYP = 'S'                                     
347000              CONTINUE                                                    
347100           ELSE                                                           
347200              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                  
347300              MOVE NEJ TO INPUT-RETT                                      
347400           END-IF                                                         
347500        ELSE                                                              
347600           IF WS-KDSORT = 'ST'                                            
347700              IF SATB-STR-IDSTRTYP = 'R' OR 'K'                           
347800                 CONTINUE                                                 
347900              ELSE                                                        
348000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR               
348100                 MOVE NEJ TO INPUT-RETT                                   
348200              END-IF                                                      
348300           ELSE                                                           
348400              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                  
348500              MOVE NEJ TO INPUT-RETT                                      
348600           END-IF                                                         
348700        END-IF                                                            
348800      ELSE                                                                
348900       IF VAECKNING = JA                                                  
349000         CONTINUE                                                         
349100       ELSE                                                               
349200         IF KOPIERING = NEJ                                               
349300           MOVE IDARTNR-WS TO W-IDARTNR-S                                 
349400         ELSE                                                             
349500           MOVE IDARTNR-NY-WS TO W-IDARTNR-S                              
349600         END-IF                                                           
349700         MOVE SPACE TO W-IDLEVNR-S                                        
349800                       W-BELEVART-S                                       
349900         PERFORM IMS-GET-SATB11-CSEQ                                      
350000         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                       
350100            IF WS-KDSORT = SATB-RAD-KDSORT                                
350200               CONTINUE                                                   
350300            ELSE                                                          
350400             MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                      
350500             MOVE DAGENS-AAMMDD       TO TMP2-YYMMDD                      
350600             PERFORM WY2000P1                                             
350700             IF TMP1-YYMMDD >= TMP2-YYMMDD                                
350800               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                 
350900               MOVE NEJ TO INPUT-RETT                                     
351000             END-IF                                                       
351100            END-IF                                                        
351200         PERFORM IMS-GET-SATB11-CSEQ                                      
351300         END-PERFORM                                                      
351400      END-IF                                                              
351500     END-IF                                                               
351600     .                                                                    
351700     EJECT                                                                
351800 S93-KOLLA-SOFTWARE SECTION.                                              
351900                                                                          
352000     IF WS-KDSORT = 'SW'                                                  
352100        IF WS-SISTA-SIFFRAN = 8                                           
352200           CONTINUE                                                       
352300        ELSE                                                              
352400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR                     
352500           MOVE NEJ TO INPUT-RETT                                         
352600        END-IF                                                            
352700     END-IF                                                               
352800                                                                          
352900     IF WS-SISTA-SIFFRAN = 8                                              
353000        IF WS-KDSORT = 'SW'                                               
353100           CONTINUE                                                       
353200        ELSE                                                              
353300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR                    
353400           MOVE NEJ TO INPUT-RETT                                         
353500        END-IF                                                            
353600     END-IF                                                               
353700     .                                                                    
353800     EJECT                                                                
353900 S94-KOLLA-IDDC SECTION.                                                  
354000                                                                          
354100     MOVE WS-KDPRODSL            TO TEST-KDPRODSL                         
354200     IF KDPRODSL-VOLVO-BIMA                                               
354300       IF CDC OR SDC                                                      
354400          CONTINUE                                                        
354500       ELSE                                                               
354600         IF MFS-UPD-X                                                     
354700           CONTINUE                                                       
354800         ELSE                                                             
354900           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                    
355000           MOVE NEJ TO INPUT-RETT                                         
355100           MOVE FEL-13(SPIND) TO MOD-TEMFSINF                             
355200         END-IF                                                           
355300       END-IF                                                             
355400     END-IF                                                               
355500     .                                                                    
355600     EJECT                                                                
355700 S95-KOLLA-IDFTG SECTION.                                                 
355800     SKIP2                                                                
355900     MOVE '002'       TO KPS-KDCALL                                       
356000     MOVE WS-KDPRODSL TO KPS-KDPRODSL                                     
356100     CALL WKPSKONV USING KPS-WKPSAREA                                     
356200     IF KPS-KDSVAR = SPACE                                                
356300        CONTINUE                                                          
356400     ELSE                                                                 
356500        MOVE NEJ TO INPUT-RETT                                            
356600        MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                       
356700     END-IF                                                               
356800     .                                                                    
356900     EJECT                                                                
357000 S96-LAES-XXAQ-KOP SECTION.                                               
357100     SKIP2                                                                
357200     MOVE WS-IDPROJ                  TO W-IDPROJ                          
357300     MOVE WS-IDPROJK                 TO W-IDPROJK                         
357400     MOVE SPACE                            TO W-IDPROJOBJ                 
357500     MOVE WS-KDPRODSL                      TO W-KDPRODSL                  
357600     PERFORM IMS-GU-XXAQ11-UNIK                                           
357700     .                                                                    
357800     EJECT                                                                
357900 S97-LAES-XXAQ SECTION.                                                   
358000     SKIP2                                                                
358100     IF MFS-UPDATE                                                        
358200        IF MID-IDPROJK = ALL '+'                                          
358300           MOVE ARTG01-ART-IDPROJK         TO W-IDPROJK                   
358400        ELSE                                                              
358500           MOVE MID-IDPROJK                TO W-IDPROJK                   
358600        END-IF                                                            
358700        IF MID-IDPROJ = ALL '+'                                           
358800           MOVE ARTG01-ART-IDPROJ          TO W-IDPROJ                    
358900        ELSE                                                              
359000           MOVE MID-IDPROJ                 TO W-IDPROJ                    
359100        END-IF                                                            
359200     ELSE                                                                 
359300        MOVE ARTG01-ART-IDPROJK            TO W-IDPROJK                   
359400        MOVE ARTG01-ART-IDPROJ             TO W-IDPROJ                    
359500     END-IF                                                               
359600                                                                          
359700     MOVE SPACE                            TO W-IDPROJOBJ                 
359800     MOVE WS-KDPRODSL                      TO W-KDPRODSL                  
359900     PERFORM IMS-GU-XXAQ11-UNIK                                           
360000     .                                                                    
360100     EJECT                                                                
360200 S98-OM-TVA-AAR SECTION.                                                  
360300     SKIP2                                                                
360400     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
360500     PERFORM S99-WDATKONV                                                 
360600     MOVE DAT-TIAAVVD TO AAVVD                                            
360700                                                                          
360800     MOVE FUNCTION CURRENT-DATE (1:4) TO W-AAR4                           
360900     ADD 2 TO W-AAR4                                                      
361000     MOVE W-AAR4 (3:2) TO AA                                              
361100                                                                          
361200     IF VV = 53                                                           
361300       MOVE 52 TO VV                                                      
361400     END-IF                                                               
361500     MOVE 1 TO D                                                          
361600     .                                                                    
361700     EJECT                                                                
361800 S99-WDATKONV SECTION.                                                    
361900     SKIP2                                                                
362000     CALL WDATKONV USING DAT-KDDATFORM                                    
362100                         DAT-I-TIDATUM                                    
362200                         DAT-O-TIDATUM                                    
362300                         DAT-KDSVAR                                       
362400     .                                                                    
362500     EJECT                                                                
362600* IMS SEKTIONER                                                           
362700     SKIP3                                                                
362800 IMS-GET-MSG SECTION.                                                     
362900     MOVE '  QC' TO GODK-STATUSKODER                                      
363000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
363100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
363200     PERFORM IMS-STATUSKONTROLL                                           
363300     .                                                                    
363400     SKIP3                                                                
363500 IMS-INSERT-MSG SECTION.                                                  
363600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
363700     MOVE SPACE TO GODK-STATUSKODER                                       
363800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
363900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
364000     PERFORM IMS-STATUSKONTROLL                                           
364100     .                                                                    
364200     EJECT                                                                
364300 IMS-GET-WMSGKOM-MSG SECTION.                                             
364400                                                                          
364500     MOVE '  QD'   TO GODK-STATUSKODER                                    
364600     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
364700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
364800     PERFORM IMS-STATUSKONTROLL                                           
364900     .                                                                    
365000     SKIP3                                                                
365100 IMS-INSERT-WMSGKOM-MSG SECTION.                                          
365200                                                                          
365300     MOVE '  '  TO GODK-STATUSKODER                                       
365400     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
365500     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
365600     PERFORM IMS-STATUSKONTROLL                                           
365700     .                                                                    
365800     EJECT                                                                
365900 IMS-GET-ARTG01 SECTION.                                                  
366000     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
366100            DELIMITED BY SIZE INTO SSA1                                   
366200     MOVE '  GE' TO GODK-STATUSKODER                                      
366300     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA-2 SSA1                   
366400     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
366500     PERFORM IMS-STATUSKONTROLL                                           
366600     .                                                                    
366700     SKIP2                                                                
366800 IMS-GU-ARTG01 SECTION.                                                   
366900     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-XX ')'                        
367000            DELIMITED BY SIZE INTO SSA1                                   
367100     MOVE '  GE' TO GODK-STATUSKODER                                      
367200     CALL CBLTDLI USING GU ARTG-PCB DLI-IO-AREA-2 SSA1                    
367300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
367400     PERFORM IMS-STATUSKONTROLL                                           
367500     .                                                                    
367600     SKIP2                                                                
367700 IMS-ISRT-ARTG01 SECTION.                                                 
367800     MOVE 'WLARTG01 ' TO SSA1                                             
367900     MOVE '  ' TO GODK-STATUSKODER                                        
368000     CALL CBLTDLI USING ISRT ARTG-PCB DLI-IO-AREA-2 SSA1                  
368100     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
368200     PERFORM IMS-STATUSKONTROLL                                           
368300     .                                                                    
368400     SKIP2                                                                
368500 IMS-REPL-ARTG SECTION.                                                   
368600     MOVE '  ' TO GODK-STATUSKODER                                        
368700     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA-2                       
368800     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
368900     PERFORM IMS-STATUSKONTROLL                                           
369000     .                                                                    
369100     EJECT                                                                
369200 IMS-GET-ARTC01 SECTION.                                                  
369300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
369400            DELIMITED BY SIZE INTO SSA1                                   
369500     MOVE '  GE' TO GODK-STATUSKODER                                      
369600     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-1 SSA1                   
369700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
369800     PERFORM IMS-STATUSKONTROLL                                           
369900     .                                                                    
370000     SKIP2                                                                
370100 IMS-GET-ARTC11 SECTION.                                                  
370200     MOVE 'WLARTC11 ' TO SSA1                                             
370300     MOVE '  GE' TO GODK-STATUSKODER                                      
370400     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-1 SSA1                  
370500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
370600     PERFORM IMS-STATUSKONTROLL                                           
370700     .                                                                    
370800     SKIP2                                                                
370900 IMS-GNP-ARTC25 SECTION.                                                  
371000     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
371100     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
371200            DELIMITED BY SIZE INTO SSA2                                   
371300     MOVE '  GE' TO GODK-STATUSKODER                                      
371400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-1 SSA1 SSA2              
371500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
371600     PERFORM IMS-STATUSKONTROLL                                           
371700     .                                                                    
371800     SKIP2                                                                
371900 IMS-GU-XXAQ11-UNIK     SECTION.                                          
372000     STRING 'WLXXAQ01(WDGXKEY  =' W-1131-KEY-X ')'                        
372100            DELIMITED BY SIZE INTO SSA1                                   
372200     STRING 'WLXXAQ11(WDGXKEY  =' W-1132-KEY-X ')'                        
372300            DELIMITED BY SIZE INTO SSA2                                   
372400     MOVE '  GE' TO GODK-STATUSKODER                                      
372500     CALL CBLTDLI USING GU XXAQ-PCB DLI-IO-AREA-1 SSA1 SSA2               
372600     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
372700     PERFORM IMS-STATUSKONTROLL                                           
372800     .                                                                    
372900     SKIP2                                                                
373000 IMS-GU-XXAQ01          SECTION.                                          
373100     STRING 'WLXXAQ01(WDGXKEY  =' W-1131-KEY-X ')'                        
373200            DELIMITED BY SIZE INTO SSA1                                   
373300     MOVE '  GE' TO GODK-STATUSKODER                                      
373400     CALL CBLTDLI USING GU XXAQ-PCB DLI-IO-AREA-1 SSA1                    
373500     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
373600     PERFORM IMS-STATUSKONTROLL                                           
373700     .                                                                    
373800     SKIP2                                                                
373900 IMS-GNP-XXAQ11          SECTION.                                         
374000     MOVE   'WLXXAQ11 ' TO SSA1                                           
374100     MOVE '  GE' TO GODK-STATUSKODER                                      
374200     CALL CBLTDLI USING GNP XXAQ-PCB DLI-IO-AREA-1 SSA1                   
374300     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
374400     PERFORM IMS-STATUSKONTROLL                                           
374500     .                                                                    
374600     EJECT                                                                
374700 IMS-GET-BENA01-ASEQ SECTION.                                             
374800     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
374900                      W-BEART-X  ')'                                      
375000            DELIMITED BY SIZE INTO SSA1                                   
375100     MOVE '  GE' TO GODK-STATUSKODER                                      
375200     CALL CBLTDLI USING GN BENB-PCB DLI-IO-AREA-1 SSA1                    
375300     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
375400     PERFORM IMS-STATUSKONTROLL                                           
375500     .                                                                    
375600     SKIP2                                                                
375700 IMS-GET-BENA13-ASEQ SECTION.                                             
375800     MOVE 'WLBENA13 ' TO SSA1                                             
375900     MOVE '  GE' TO GODK-STATUSKODER                                      
376000     CALL CBLTDLI USING GNP BENB-PCB DLI-IO-AREA-1 SSA1                   
376100     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
376200     PERFORM IMS-STATUSKONTROLL                                           
376300     .                                                                    
376400     EJECT                                                                
376500 IMS-GET-BENA11-CSEQ SECTION.                                             
376600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
376700            DELIMITED BY SIZE INTO SSA1                                   
376800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
376900            DELIMITED BY SIZE INTO SSA2                                   
377000     MOVE '  ' TO GODK-STATUSKODER                                        
377100     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA-1 SSA1 SSA2               
377200     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
377300     PERFORM IMS-STATUSKONTROLL                                           
377400     .                                                                    
377500     SKIP2                                                                
377600 IMS-GN-ARTH01 SECTION.                                                   
377700     STRING 'WLARTH01(WDD2A1KY> ' W-WDD2A1KY-MIN                          
377800                    '&WDD2A1KY<=' W-WDD2A1KY-MAX                          
377900                    '&IDPROJ  >=' W-IDPROJ-MIN                            
378000                    '&IDPROJ  <=' W-IDPROJ-MAX ')'                        
378100            DELIMITED BY SIZE INTO SSA1                                   
378200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
378300     CALL CBLTDLI USING GN ARTH-PCB DLI-IO-AREA-2 SSA1                    
378400     MOVE ARTH-STATUS-CODE TO STATUS-WS                                   
378500     PERFORM IMS-STATUSKONTROLL                                           
378600     .                                                                    
378700     SKIP2                                                                
378800 IMS-GU-BENA01-CSEQ SECTION.                                              
378900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
379000            DELIMITED BY SIZE INTO SSA1                                   
379100     MOVE '  ' TO GODK-STATUSKODER                                        
379200     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA-1 SSA1                    
379300     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
379400     PERFORM IMS-STATUSKONTROLL                                           
379500     .                                                                    
379600     SKIP2                                                                
379700 IMS-GNP-BENA11-CSEQ SECTION.                                             
379800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
379900            DELIMITED BY SIZE INTO SSA1                                   
380000     MOVE '  ' TO GODK-STATUSKODER                                        
380100     CALL CBLTDLI USING GNP BENC-PCB DLI-IO-AREA-1 SSA1                   
380200     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
380300     PERFORM IMS-STATUSKONTROLL                                           
380400     .                                                                    
380500     EJECT                                                                
380600 IMS-GET-SATB01 SECTION.                                                  
380700     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
380800            DELIMITED BY SIZE INTO SSA1                                   
380900     MOVE '  GE' TO GODK-STATUSKODER                                      
381000     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA-3 SSA1                    
381100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
381200     PERFORM IMS-STATUSKONTROLL                                           
381300     .                                                                    
381400     SKIP2                                                                
381500 IMS-GET-SATB11-CSEQ SECTION.                                             
381600     STRING 'WLSATB11(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                        
381700            DELIMITED BY SIZE INTO SSA1                                   
381800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
381900     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA-3 SSA1                    
382000     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
382100     PERFORM IMS-STATUSKONTROLL                                           
382200     .                                                                    
382300     EJECT                                                                
382400 IMS-INSERT-ALT SECTION.                                                  
382500     MOVE SPACE TO GODK-STATUSKODER                                       
382600     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
382700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
382800     PERFORM IMS-STATUSKONTROLL                                           
382900     .                                                                    
383000     SKIP3                                                                
383100 IMS-STATUSKONTROLL SECTION.                                              
383200     SET STATUS-IX TO 1                                                   
383300     SEARCH GODK-STATUS AT END CALL FELLOG                                
383400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
383500       CONTINUE                                                           
383600     END-SEARCH                                                           
383700     .                                                                    
383800     EJECT                                                                
383900*    -COPY WY2000P1                                                       
384000     EJECT                                                                
384100*    -COPY WY2000P2                                                       
384200     EJECT                                                                
384300*    -COPY WY2000Q2                                                       
384400     EJECT                                                                
384500*    -COPY WY2000P3                                                       
