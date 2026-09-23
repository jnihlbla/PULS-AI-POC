000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5106100.                                                
000300 AUTHOR.         JONNY SANDSTEN.                                          
000400 DATE-WRITTEN.   98/05/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER                                                 
000900*        1 EKONOMISK HÄNDELSEBAS(WDR9)                                    
001000*        2 EKONOMISK HÄNDELSEFIL                                          
001100*                                                                         
001200*        OCH SKAPAR                                                       
001300*        1 RENSNINGSFIL                                                   
001400*        2 FIL TILL KONTROLL                                              
001500*        3 FIL TILL DIVERSE LISTOR                                        
001600*        4 FIL TILL LEVA1 'FAKTURAFIL N-FAKTUROR'                         
001700*        5 FIL TILL LEVA1 'FAKTURAFIL N-FAKTUROR-KOMPONENTER'             
001800*                                                                         
001900*        PROGRAMMET LÄSER      WLSAPA (WDR9)                              
002000*                                                                         
002100     EJECT                                                                
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- INFIL EKONOMI                                              
003000     SELECT W51064                     ASSIGN TO W51061D1.                
003100     SKIP2                                                                
003200*          --- UTFIL RENS                                                 
003300     SELECT W51060                     ASSIGN TO W51061D2.                
003400     SKIP2                                                                
003500*          --- UTFIL KONTROLL                                             
003600     SELECT W51061A                    ASSIGN TO W51061D3.                
003700     SKIP2                                                                
003800*          --- UTFIL LISTOR                                               
003900     SELECT W51067                     ASSIGN TO W51061D4.                
004000     SKIP2                                                                
004100*          --- UTFIL FAKTURAFIL TILL LEVA1                                
004200     SELECT W51065                     ASSIGN TO W51061D5.                
004300     SKIP2                                                                
004400*          --- UTFIL FAKTURAFIL TILL LEVA1 -> KOMPONENTER                 
004500     SELECT W5106E                     ASSIGN TO W51061D6.                
004600     EJECT                                                                
004700                                                                          
004800*          --- UTFIL FAKTURAFIL TILL LEVA1 -> MAASTRISCHT                 
004900     SELECT W5106H                     ASSIGN TO W51061D7.                
005000     EJECT                                                                
005010*          --- UTFIL KONTROLL                                             
005020     SELECT W51061C                    ASSIGN TO W51061D8.                
005030     SKIP2                                                                
005100                                                                          
005200 DATA DIVISION.                                                           
005300                                                                          
005400 FILE SECTION.                                                            
005500     SKIP3                                                                
005600 FD  W51060                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900*01  POST -COPY WDR901 -PRE  UT1-  -L.                                    
006000                                                                          
006100 FD  W51061A                                                              
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400*01  POST -COPY WDR901 -PRE  UT2-  -L.                                    
006500                                                                          
006600 FD  W51067                                                               
006700     RECORDING       F                                                    
006800     BLOCK CONTAINS  0.                                                   
006900*01  POST -COPY W51060 -PRE  UT3-  -L.                                    
007000                                                                          
007100 FD  W51064                                                               
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS  0.                                                   
007400 01  IN-POST.                                                             
007500*    03   -COPY W51060    -L.                                             
007600                                                                          
007700 FD  W51065                                                               
007800     RECORDING       F                                                    
007900     BLOCK CONTAINS  0.                                                   
008000*01  POST -COPY A432505 -PRE  LRAD-  -L.                                  
008100                                                                          
008200 FD  W5106E                                                               
008300     RECORDING       F                                                    
008400     BLOCK CONTAINS  0.                                                   
008500*01  POST -COPY A432505 -PRE  LRADE-  -L.                                 
008600     EJECT                                                                
008700 FD  W5106H                                                               
008800     RECORDING       F                                                    
008900     BLOCK CONTAINS  0.                                                   
009000*01  POST -COPY A432505 -PRE  LRADH-  -L.                                 
009100     EJECT                                                                
009101                                                                          
009110 FD  W51061C                                                              
009120     RECORDING       F                                                    
009130     BLOCK CONTAINS  0.                                                   
009140*01  POST -COPY WDR901 -PRE  UT4-  -L.                                    
009150                                                                          
009200 WORKING-STORAGE SECTION.                                                 
009300                                                                          
009400 77  IDPGM                       PIC X(8)    VALUE 'W5106100'.            
009500 77  JA                          PIC X       VALUE 'J'.                   
009600 77  NEJ                         PIC X       VALUE 'N'.                   
009700 77  W51064-EOF-SW               PIC X       VALUE 'N'.                   
009800     88  END-OF-W51064                       VALUE 'J'.                   
009900 77  KUND-OK-SW                  PIC X       VALUE 'N'.                   
010000     88 KUND-OK                              VALUE 'J'.                   
010100     88 KUND-NOT-OK                          VALUE 'N'.                   
010200 01  WS-FTAG                     PIC 99      VALUE ZERO.                  
010300                                                                          
010400 01  FILLER                      PIC X(16) VALUE 'LEVA1-TABELL'.          
010500 01  TABELL.                                                              
010600     03 DISTRIKTVARDEN.                                                   
010700       05  FILLER PIC X(21) VALUE '00008 00008   1441 15'.                
010800       05  FILLER PIC X(21) VALUE '00030 00011   1441 22'.                
010900       05  FILLER PIC X(21) VALUE '00030 00040   1441 51'.                
011000       05  FILLER PIC X(21) VALUE '00069 01225   1441 55'.                
011100       05  FILLER PIC X(21) VALUE '00077 78941   1441 07'.                
011110       05  FILLER PIC X(21) VALUE '01627 00000   1441 05'.                
011200                                                                          
011300     03 DISTRIKT REDEFINES DISTRIKTVARDEN OCCURS 6                        
011400                 INDEXED BY IX.                                           
011500        05 TAB-DIST    PIC 9(5).                                          
011600        05 FILLER      PIC X.                                             
011700        05 TAB-KUND    PIC 9(5).                                          
011800        05 FILLER      PIC X.                                             
011900        05 TAB-LEVNR   PIC X(6).                                          
012000        05 FILLER      PIC X.                                             
012100        05 TAB-FTAG    PIC 9(2).                                          
012200                                                                          
012300 01  WAREA.                                                               
012400     03  W-DIST                  PIC 9(5)    VALUE ZERO.                  
012500     03  W-KUND                  PIC 9(7)    VALUE ZERO.                  
012600     03  W-IDARTNR               PIC 9(10)   VALUE ZERO.                  
012700     03  W-IDORDNR6              PIC 9(6)    VALUE ZERO.                  
012800     03  W-ANTAL                 PIC 9(7)    VALUE ZERO.                  
012900     03  W-N                     PIC 9       VALUE ZERO.                  
013000     03  W-65-FIRST              PIC X       VALUE 'J'.                   
013100     03  W-6E-FIRST              PIC X       VALUE 'J'.                   
013200     03  W-6H-FIRST              PIC X       VALUE 'J'.                   
013300                                                                          
013400 01  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
013500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013600     EJECT                                                                
013700 01  DYNAMISKA-SUBPROGRAM.                                                
013800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
014300                                                                          
014400*    --- PARAMETRAR TILL ABEND                                            
014500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014800     SKIP2                                                                
014900 01  FELTEXT.                                                             
015000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
015100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
015200     EJECT                                                                
015300*    --- PARAMETRAR TILL DATKORT                                          
015400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
015500                                                                          
015600*01  -COPY WDATKORT                                                       
015700     EJECT                                                                
015800                                                                          
015900*    --- PARAMETRAR TILL POSTSUM                                          
016000*                                                                         
016100*01  -COPY W0005   -PRE  POSTSUM-                                         
016200     EJECT                                                                
016300                                                                          
016400 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
016500*    --- INAREA EKONOMIPOST                                               
016600*01  AREA -COPY W51060  -PRE IN-                                          
016700     EJECT                                                                
016800                                                                          
016900 01  FILLER                      PIC X(24)   VALUE 'UT1-AREA'.            
017000*01  AREA -COPY WDR901     -PRE UT1-                                      
017100*    05   -COPY W510EKHA   -PRE UT1- -RED UT1-FIL-WDR901-DATA             
017200     EJECT                                                                
017300                                                                          
017400 01  FILLER                      PIC X(24)   VALUE 'UT2-AREA'.            
017500*01  AREA -COPY WDR901     -PRE UT2-                                      
017600*    05   -COPY W510EKHA   -PRE UT2- -RED UT2-FIL-WDR901-DATA             
017700     EJECT                                                                
017800                                                                          
017810 01  FILLER                      PIC X(24)   VALUE 'UT4-AREA'.            
017820*01  AREA -COPY WDR901     -PRE UT4-                                      
017830*    05   -COPY W510EKHA   -PRE UT4- -RED UT4-FIL-WDR901-DATA             
017840     EJECT                                                                
017850                                                                          
017900 01  FILLER                      PIC X(24)   VALUE 'UT3-AREA'.            
018000*01  AREA -COPY W51060     -PRE UT3-                                      
018100     EJECT                                                                
018200                                                                          
018300 01  FILLER                      PIC X(24)   VALUE 'HEAD-AREA'.           
018400*01  AREA -COPY A432504    -PRE HEAD-                                     
018500     EJECT                                                                
018600                                                                          
018700 01  FILLER                      PIC X(24)   VALUE 'LINE-AREA'.           
018800*01  AREA -COPY A432505    -PRE LINE-                                     
018900     EJECT                                                                
019000 01  FILLER                      PIC X(24)   VALUE 'HEAD2-AREA'.          
019100*01  AREA -COPY A432504    -PRE HEAD2-                                    
019200     EJECT                                                                
019300                                                                          
019400 01  FILLER                      PIC X(24)   VALUE 'LINE2-AREA'.          
019500*01  AREA -COPY A432505    -PRE LINE2-                                    
019600     EJECT                                                                
019700                                                                          
019710 01  FILLER                      PIC X(24)   VALUE 'HEAD3-AREA'.          
019720*01  AREA -COPY A432504    -PRE HEAD3-                                    
019730     EJECT                                                                
019740                                                                          
019750 01  FILLER                      PIC X(24)   VALUE 'LINE3-AREA'.          
019760*01  AREA -COPY A432505    -PRE LINE3-                                    
019770     EJECT                                                                
019780                                                                          
019800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019900*                                                                         
020000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020100                                                                          
020200 01  NYCKLAR-TILL-DLI.                                                    
020300     03  W-WDR901KY-X.                                                    
020400         05  W-WDR901KY          PIC X(22)    VALUE SPACE.                
020500                                                                          
020600*    --- STATUS-KOD FRÅN IMS                                              
020700 01  STATUS-WS                   PIC XX.                                  
020800     88  SEGMENT-FINNS                       VALUE '  '.                  
020900     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
021000                                                                          
021100 01  GODK-STATUSKODER.                                                    
021200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021300     SKIP3                                                                
021400 01  SSA1                        PIC X(64).                               
021500 01  SSA2                        PIC X(64).                               
021600                                                                          
021700*    --- IMS FUNKTIONSKODER                                               
021800*01  -COPY W0003                                                          
021900     EJECT                                                                
022000                                                                          
022100*    ---  DLI INPUT-OUTPUT AREA                                           
022200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLSAPA'.                      
022300 01  DLI-IO-WLSAPA.                                                       
022400*    03  -COPY WDR901                                                     
022500*    05   -COPY W510EKHA   -RED FIL-WDR901-DATA                           
022600     EJECT                                                                
022700                                                                          
022800 LINKAGE SECTION.                                                         
022900*01  -COPY W0008  -PRE SAPA-                                              
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200                                                                          
023300 PROCEDURE DIVISION  USING SAPA-PCB.                                      
023400 MAIN SECTION.                                                            
023500     ENTRY 'DLITCBL' USING SAPA-PCB.                                      
023600                                                                          
023700     PERFORM A-INIT                                                       
023800                                                                          
023900     PERFORM IMS-GET-SAPA                                                 
024000     PERFORM UNTIL SEGMENT-SAKNAS                                         
024100       IF FIL-CT-IDSYSTEM = 'W510'                                        
024200          PERFORM B-FLYTTA-POST-TILL-UTAREA1-2                            
024300          PERFORM C-FLYTTA-POST-TILL-UTAREA3                              
024400       END-IF                                                             
024410       IF FIL-CT-IDSYSTEM = 'W570'                                        
024411       OR FIL-CT-IDSYSTEM = 'W561'                                        
024420          PERFORM E-FLYTTA-POST-TILL-UTAREA1-2                            
024440       END-IF                                                             
024500       PERFORM IMS-GET-SAPA                                               
024600     END-PERFORM                                                          
024700                                                                          
024800     PERFORM S01-LAES-W51064                                              
024900     PERFORM UNTIL END-OF-W51064                                          
025000       PERFORM D1-FLYTTA-POST-TILL-UTAREA1                                
025100       PERFORM D2-FLYTTA-POST-TILL-UTAREA2                                
025110       IF IN-EKHT-CT-IDSYSTEM = 'W570'                                    
025111       OR IN-EKHT-CT-IDSYSTEM = 'W561'                                    
025120         CONTINUE                                                         
025130       ELSE                                                               
025200         PERFORM D3-FLYTTA-SKRIV-W51067                                   
025300                                                                          
025400****   SKAPA POSTER SOM SKCKAS VIA LEVA1                                  
025500         IF IN-EKHT-KDEKHHT = '203' AND                                   
025600            IN-EKHT-KDEKSHT = '201' AND                                   
025700            IN-EKHT-KDEKNIVA = 'DET'                                      
025800           PERFORM XA-SEARCH-DIST-KUND                                    
025900           IF IN-EKHT-IDDISTR = 77 AND KUND-OK                            
026000****   SKAPA FAKTURAPOST VIA LEVA1 TILL VOLVO TORSLANDAVERKEN             
026100             PERFORM F-SKAPA-LEVA1-FAKT-POST                              
026200           END-IF                                                         
026300           IF (IN-EKHT-IDDISTR = 8 OR 30) AND KUND-OK                     
026400****   SKAPA FAKTURAPOST VIA LEVA1 TILL VOLVO KOMPONENTER                 
026500             PERFORM G-SKAPA-LEVA1-KOMP-POST                              
026600           END-IF                                                         
026700           IF IN-EKHT-IDDISTR = 69 AND KUND-OK                            
026800****   SKAPA FAKTURAPOST VIA LEVA1 TILL VOLVO PERSONVAGNAR                
026900             PERFORM H-SKAPA-LEVA1-VCC-POST                               
027000           END-IF                                                         
027010           IF IN-EKHT-IDDISTR = 1627 AND KUND-OK                          
027020****   SKAPA FAKTURAPOST VIA LEVA1 TILL VOLVO MAASTRISCHT                 
027030             PERFORM I-SKAPA-LEVA1-MAAS-POST                              
027040           END-IF                                                         
027200         END-IF                                                           
027210       END-IF                                                             
027300                                                                          
027400       PERFORM S01-LAES-W51064                                            
027500     END-PERFORM                                                          
027600                                                                          
027700     PERFORM Z-FINIT                                                      
027800                                                                          
027900     MOVE ZERO TO RETURN-CODE                                             
028000     GOBACK                                                               
028100     .                                                                    
028200     EJECT                                                                
028300                                                                          
028400 A-INIT SECTION.                                                          
028500                                                                          
028600     OPEN INPUT  W51064                                                   
028700     OPEN OUTPUT W51060                                                   
028800                 W51061A                                                  
028900                 W51065                                                   
029000                 W51067                                                   
029100                 W5106E                                                   
029110                 W5106H                                                   
029120                 W51061C                                                  
029200                                                                          
029300     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
029400                                                                          
029500     MOVE FUNCTION CURRENT-DATE(1:4)   TO DAGENS-AAR                      
029600     MOVE FUNCTION CURRENT-DATE(3:6)   TO DAGENS-DATUM                    
029700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029800     .                                                                    
029900     EJECT                                                                
030000                                                                          
030100 B-FLYTTA-POST-TILL-UTAREA1-2 SECTION.                                    
030200                                                                          
030300*---FLYTTAR ALLA POSTER TILL UT1- OCH UT2-AREA                            
030400*---SKRIVER SEDAN POSTER PÅ W51060 OCH W51061                             
030500                                                                          
030600     MOVE SPACE              TO UT1-AREA                                  
030700     MOVE FIL-IDPGM          TO UT1-FIL-IDPGM                             
030800     MOVE FIL-DAREGDAT       TO UT1-FIL-DAREGDAT                          
030900     MOVE FIL-TIKLOCK        TO UT1-FIL-TIKLOCK                           
031000     MOVE FIL-IDSEKVNR       TO UT1-FIL-IDSEKVNR                          
031100     MOVE FIL-IDCPYTXT       TO UT1-FIL-IDCPYTXT                          
031200     PERFORM S10-SKRIV-W51060                                             
031300                                                                          
031400     MOVE DLI-IO-WLSAPA      TO UT2-AREA                                  
031500     MOVE 'Y'                TO UT2-EKH-FLKLAR                            
031600     IF UT2-EKH-FLDCET = JA                                               
031700       MOVE JA               TO UT2-EKH-FLDCET                            
031800     ELSE                                                                 
031900       MOVE NEJ              TO UT2-EKH-FLDCET                            
032000     END-IF                                                               
032100     IF FIL-CT-IDPTYP = 'EKF'                                             
032200       MOVE 'W510EKHA'       TO UT2-FIL-IDCPYTXT                          
032300       MOVE SPACE            TO UT2-EKH-BEFELSAP                          
032400     END-IF                                                               
032500     PERFORM S11-SKRIV-W51061A                                            
032600     .                                                                    
032700     EJECT                                                                
032800                                                                          
032900 C-FLYTTA-POST-TILL-UTAREA3 SECTION.                                      
033000*                                                                         
033100*---FLYTTAR ALLA POSTER TILL UT3-AREA                                     
033200*---SKRIVER SEDAN POST TILL FIL W51067                                    
033300*                                                                         
033400     IF FIL-CT-IDPTYP = 'EKH' AND EKH-FLKLAR NOT = 'Y'                    
033500       MOVE SPACE           TO UT3-AREA                                   
033600       MOVE FIL-IDPGM       TO UT3-EKHT-IDPGM                             
033700       MOVE FIL-DAREGDAT    TO UT3-EKHT-DAREGDAT                          
033800       MOVE FIL-TIKLOCK     TO UT3-EKHT-TIKLOCK                           
033900       MOVE FIL-IDSEKVNR    TO UT3-EKHT-IDSEKVNR                          
034000       MOVE FIL-IDCPYTXT    TO UT3-EKHT-IDCPYTXT                          
034100       MOVE EKH-BEVAT       TO UT3-EKHT-BEVAT                             
034200       MOVE EKH-DAVERDAT    TO UT3-EKHT-DAVERDAT                          
034300       MOVE EKH-FLLSBOK     TO UT3-EKHT-FLLSBOK                           
034400       MOVE EKH-IDANALYS    TO UT3-EKHT-IDANALYS                          
034500       MOVE EKH-IDARTNR     TO UT3-EKHT-IDARTNR                           
034600       MOVE EKH-IDDC-SEND   TO UT3-EKHT-IDDC-SEND                         
034700       MOVE EKH-IDDC-REC    TO UT3-EKHT-IDDC-REC                          
034800       MOVE EKH-IDDISTR     TO UT3-EKHT-IDDISTR                           
034900       MOVE EKH-IDKONTO     TO UT3-EKHT-IDKONTO                           
035000       MOVE EKH-IDKST       TO UT3-EKHT-IDKST                             
035100       MOVE EKH-IDKUNDNR    TO UT3-EKHT-IDKUNDNR                          
035200       MOVE EKH-IDTRANS     TO UT3-EKHT-IDTRANS                           
035300       MOVE EKH-IDVERGL     TO UT3-EKHT-IDVERGL                           
035400       MOVE EKH-KDANMORS    TO UT3-EKHT-KDANMORS                          
035500       MOVE EKH-KDEKHHT     TO UT3-EKHT-KDEKHHT                           
035600       MOVE EKH-KDEKSHT     TO UT3-EKHT-KDEKSHT                           
035700       MOVE EKH-KDEKNIVA    TO UT3-EKHT-KDEKNIVA                          
035800       MOVE EKH-KDPRODSL    TO UT3-EKHT-KDPRODSL                          
035900       MOVE EKH-KDFRAKT     TO UT3-EKHT-KDFRAKT                           
036000       MOVE EKH-KDPSLLOC    TO UT3-EKHT-KDPSLLOC                          
036100       MOVE EKH-KDVALISO    TO UT3-EKHT-KDVALISO                          
036200       MOVE EKH-KVANTAL     TO UT3-EKHT-KVANTAL                           
036300       MOVE EKH-PRARTNTO    TO UT3-EKHT-PRARTNTO                          
036400       MOVE EKH-PRARTSJK    TO UT3-EKHT-PRARTSJK                          
036500       MOVE EKH-PRHEMTAG    TO UT3-EKHT-PRHEMTAG                          
036600       MOVE EKH-PRARTSTD    TO UT3-EKHT-PRARTSTD                          
036700       MOVE EKH-PRDIRLON    TO UT3-EKHT-PRDIRLON                          
036800       MOVE EKH-PRDMTRL     TO UT3-EKHT-PRDMTRL                           
036900       MOVE EKH-PRKURS      TO UT3-EKHT-PRKURS                            
037000       MOVE EKH-PRINK       TO UT3-EKHT-PRINK                             
037100       MOVE EKH-PROVRPAL    TO UT3-EKHT-PROVRPAL                          
037200       MOVE EKH-SUBEL       TO UT3-EKHT-SUBEL                             
037300       MOVE EKH-SUVAT       TO UT3-EKHT-SUVAT                             
037400       MOVE EKH-PRLANDCO    TO UT3-EKHT-PRLANDCO                          
037500       MOVE EKH-DAAVIDAT    TO UT3-EKHT-DAAVIDAT                          
037600       MOVE EKH-IDAVINR     TO UT3-EKHT-IDAVINR                           
037700       MOVE EKH-IDLEVNR     TO UT3-EKHT-IDLEVNR                           
037800       MOVE EKH-KDAVVTYP    TO UT3-EKHT-KDAVVTYP                          
037900       MOVE EKH-KDRT        TO UT3-EKHT-KDRT                              
038000       MOVE EKH-KVANTMOT    TO UT3-EKHT-KVANTMOT                          
038100       MOVE EKH-KVAVIS      TO UT3-EKHT-KVAVIS                            
038200       MOVE EKH-KDSORT      TO UT3-EKHT-KDSORT                            
038300       MOVE EKH-KDTRADP     TO UT3-EKHT-KDTRADP                           
038400       MOVE ' '             TO UT3-EKHT-FLDCET                            
038410       MOVE EKH-IDKUNDRF    TO UT3-EKHT-IDKUNDRF                          
038420       MOVE EKH-IDFAKT-EXP  TO UT3-EKHT-IDFAKT-EXP                        
038500       IF EKH-KDEKHHT = '403' AND EKH-KDEKSHT = '410'                     
038600         MOVE FIL-IDUSER TO UT3-EKHT-IDUSER                               
038700       END-IF                                                             
038800                                                                          
038900       PERFORM S12-SKRIV-W51067                                           
039000     END-IF                                                               
039100     .                                                                    
039200     EJECT                                                                
039300 D1-FLYTTA-POST-TILL-UTAREA1 SECTION.                                     
039400                                                                          
039500     MOVE SPACE              TO UT1-AREA                                  
039600     MOVE IN-EKHT-IDPGM      TO UT1-FIL-IDPGM                             
039700     MOVE IN-EKHT-DAREGDAT   TO UT1-FIL-DAREGDAT                          
039800     MOVE IN-EKHT-TIKLOCK    TO UT1-FIL-TIKLOCK                           
039900     MOVE IN-EKHT-IDSEKVNR   TO UT1-FIL-IDSEKVNR                          
040000     MOVE IN-EKHT-IDCPYTXT   TO UT1-FIL-IDCPYTXT                          
040100                                                                          
040200     PERFORM S10-SKRIV-W51060                                             
040300     .                                                                    
040400     EJECT                                                                
040500 D2-FLYTTA-POST-TILL-UTAREA2 SECTION.                                     
040600                                                                          
040700     MOVE IN-EKHT-IDPGM      TO UT2-FIL-IDPGM                             
040800     MOVE IN-EKHT-DAREGDAT   TO UT2-FIL-DAREGDAT                          
040900     MOVE IN-EKHT-TIKLOCK    TO UT2-FIL-TIKLOCK                           
041000     MOVE IN-EKHT-IDSEKVNR   TO UT2-FIL-IDSEKVNR                          
041200     MOVE IN-EKHT-BEVAT      TO UT2-EKH-BEVAT                             
041300     MOVE IN-EKHT-DAVERDAT   TO UT2-EKH-DAVERDAT                          
041400     MOVE IN-EKHT-FLLSBOK    TO UT2-EKH-FLLSBOK                           
041500     MOVE IN-EKHT-IDANALYS   TO UT2-EKH-IDANALYS                          
041600     MOVE IN-EKHT-IDARTNR    TO UT2-EKH-IDARTNR                           
041700     MOVE IN-EKHT-IDDC-SEND  TO UT2-EKH-IDDC-SEND                         
041800     MOVE IN-EKHT-IDDC-REC   TO UT2-EKH-IDDC-REC                          
041900     MOVE IN-EKHT-IDDISTR    TO UT2-EKH-IDDISTR                           
042000     MOVE IN-EKHT-IDKONTO    TO UT2-EKH-IDKONTO                           
042100     MOVE IN-EKHT-IDKST      TO UT2-EKH-IDKST                             
042200     MOVE IN-EKHT-IDKUNDNR   TO UT2-EKH-IDKUNDNR                          
042300     MOVE IN-EKHT-IDTRANS    TO UT2-EKH-IDTRANS                           
042400     MOVE IN-EKHT-IDVERGL    TO UT2-EKH-IDVERGL                           
042500     MOVE IN-EKHT-KDANMORS   TO UT2-EKH-KDANMORS                          
042600     MOVE IN-EKHT-KDEKHHT    TO UT2-EKH-KDEKHHT                           
042700     MOVE IN-EKHT-KDEKSHT    TO UT2-EKH-KDEKSHT                           
042800     MOVE IN-EKHT-KDEKNIVA   TO UT2-EKH-KDEKNIVA                          
042900     MOVE IN-EKHT-KDFRAKT    TO UT2-EKH-KDFRAKT                           
043000     MOVE IN-EKHT-KDPRODSL   TO UT2-EKH-KDPRODSL                          
043100     MOVE IN-EKHT-KDPSLLOC   TO UT2-EKH-KDPSLLOC                          
043200     MOVE IN-EKHT-KDVALISO   TO UT2-EKH-KDVALISO                          
043300     MOVE IN-EKHT-KVANTAL    TO UT2-EKH-KVANTAL                           
043400     MOVE IN-EKHT-PRARTNTO   TO UT2-EKH-PRARTNTO                          
043500     MOVE IN-EKHT-PRARTSJK   TO UT2-EKH-PRARTSJK                          
043600     MOVE IN-EKHT-PRHEMTAG   TO UT2-EKH-PRHEMTAG                          
043700     MOVE IN-EKHT-PRARTSTD   TO UT2-EKH-PRARTSTD                          
043800     MOVE IN-EKHT-PRDIRLON   TO UT2-EKH-PRDIRLON                          
043900     MOVE IN-EKHT-PRDMTRL    TO UT2-EKH-PRDMTRL                           
044000     MOVE IN-EKHT-PRINK      TO UT2-EKH-PRINK                             
044100     MOVE IN-EKHT-PRKURS     TO UT2-EKH-PRKURS                            
044200     MOVE IN-EKHT-PRLANDCO   TO UT2-EKH-PRLANDCO                          
044300     MOVE IN-EKHT-PROVRPAL   TO UT2-EKH-PROVRPAL                          
044400     MOVE IN-EKHT-SUBEL      TO UT2-EKH-SUBEL                             
044500     MOVE IN-EKHT-SUVAT      TO UT2-EKH-SUVAT                             
044600     MOVE IN-EKHT-DAAVIDAT   TO UT2-EKH-DAAVIDAT                          
044700     MOVE IN-EKHT-IDAVINR    TO UT2-EKH-IDAVINR                           
044800     MOVE IN-EKHT-IDLEVNR    TO UT2-EKH-IDLEVNR                           
044900     MOVE IN-EKHT-KDAVVTYP   TO UT2-EKH-KDAVVTYP                          
045000     MOVE IN-EKHT-KDRT       TO UT2-EKH-KDRT                              
045100     MOVE IN-EKHT-KVANTMOT   TO UT2-EKH-KVANTMOT                          
045200     MOVE IN-EKHT-KVAVIS     TO UT2-EKH-KVAVIS                            
045300     MOVE IN-EKHT-KDSORT     TO UT2-EKH-KDSORT                            
045400     MOVE IN-EKHT-KDTRADP    TO UT2-EKH-KDTRADP                           
045500     MOVE IN-EKHT-FLOVRLEV   TO UT2-EKH-FLOVRLEV                          
045600     IF IN-EKHT-FLDCET = JA                                               
045700       MOVE JA               TO UT2-EKH-FLDCET                            
045800     ELSE                                                                 
045900       MOVE NEJ              TO UT2-EKH-FLDCET                            
046000     END-IF                                                               
046010     MOVE IN-EKHT-IDKUNDRF   TO UT2-EKH-IDKUNDRF                          
046020     MOVE IN-EKHT-IDFAKT-EXP TO UT2-EKH-IDFAKT-EXP                        
046100     IF IN-EKHT-IDORDNR5 NUMERIC                                          
046200       MOVE IN-EKHT-IDORDNR5 TO UT2-EKH-IDORDNR5                          
046300     ELSE                                                                 
046400       MOVE ZERO             TO UT2-EKH-IDORDNR5                          
046500     END-IF                                                               
046600     MOVE IN-EKHT-IDUSER     TO UT2-EKH-IDUSER                            
046700     MOVE SPACE              TO UT2-EKH-IDREF                             
046800                                UT2-EKH-BEFELSAP                          
046900                                UT2-EKH-IDKUNDRF                          
047000     MOVE 'Y'                TO UT2-EKH-FLKLAR                            
047001     IF IN-EKHT-CT-IDSYSTEM = 'W510'                                      
047010       MOVE 'W510EKHA'         TO UT2-FIL-IDCPYTXT                        
047100       PERFORM S11-SKRIV-W51061A                                          
047110     END-IF                                                               
047111     IF IN-EKHT-CT-IDSYSTEM = 'W570'                                      
047112       MOVE 'W570EKHA'         TO UT2-FIL-IDCPYTXT                        
047113       MOVE UT2-AREA           TO UT4-AREA                                
047114       PERFORM S11-SKRIV-W51061C                                          
047120     END-IF                                                               
047130     IF IN-EKHT-CT-IDSYSTEM = 'W561'                                      
047140       MOVE 'W561EKHA'         TO UT2-FIL-IDCPYTXT                        
047150       MOVE UT2-AREA           TO UT4-AREA                                
047160       PERFORM S11-SKRIV-W51061C                                          
047170     END-IF                                                               
047200     .                                                                    
047300     EJECT                                                                
047400                                                                          
047500 D3-FLYTTA-SKRIV-W51067 SECTION.                                          
047600                                                                          
047700     MOVE IN-AREA             TO UT3-AREA                                 
047800     PERFORM S12-SKRIV-W51067                                             
047900     .                                                                    
048000     EJECT                                                                
048010 E-FLYTTA-POST-TILL-UTAREA1-2 SECTION.                                    
048020                                                                          
048030*---FLYTTAR ALLA POSTER TILL UT1- OCH UT2-AREA                            
048040*---SKRIVER SEDAN POSTER PÅ W51060 OCH W51061                             
048050                                                                          
048060     MOVE SPACE              TO UT1-AREA                                  
048070     MOVE FIL-IDPGM          TO UT1-FIL-IDPGM                             
048080     MOVE FIL-DAREGDAT       TO UT1-FIL-DAREGDAT                          
048090     MOVE FIL-TIKLOCK        TO UT1-FIL-TIKLOCK                           
048091     MOVE FIL-IDSEKVNR       TO UT1-FIL-IDSEKVNR                          
048092     MOVE FIL-IDCPYTXT       TO UT1-FIL-IDCPYTXT                          
048093     PERFORM S10-SKRIV-W51060                                             
048094                                                                          
048095     MOVE DLI-IO-WLSAPA      TO UT4-AREA                                  
048096     MOVE 'Y'                TO UT4-EKH-FLKLAR                            
048097     IF UT4-EKH-FLDCET = JA                                               
048098       MOVE JA               TO UT4-EKH-FLDCET                            
048099     ELSE                                                                 
048100       MOVE NEJ              TO UT4-EKH-FLDCET                            
048101     END-IF                                                               
048102     IF FIL-CT-IDPTYP = 'EKF'                                             
048103       MOVE IN-EKHT-IDCPYTXT TO UT4-FIL-IDCPYTXT                          
048104       MOVE SPACE            TO UT4-EKH-BEFELSAP                          
048105     END-IF                                                               
048106     PERFORM S11-SKRIV-W51061C                                            
048107     .                                                                    
048108     EJECT                                                                
048109                                                                          
048150 F-SKAPA-LEVA1-FAKT-POST SECTION.                                         
048200                                                                          
048300     IF W-65-FIRST = JA                                                   
048400       MOVE SPACE          TO HEAD-AREA                                   
048500       MOVE '504'          TO HEAD-PTYP                                   
048700       MOVE WS-FTAG        TO HEAD-FTAG                                   
048800       MOVE DAGENS-AAR     TO HEAD-AR                                     
048900       MOVE D-VECKA        TO HEAD-VA                                     
049000       PERFORM S13-SKRIV-HEAD-W51065                                      
049100       MOVE NEJ            TO W-65-FIRST                                  
049200     END-IF                                                               
049300                                                                          
049400     MOVE SPACE            TO LINE-AREA                                   
049500     MOVE '505'            TO LINE-PTYP                                   
049600     MOVE IN-EKHT-IDARTNR  TO W-IDARTNR                                   
049700     MOVE W-IDARTNR        TO LINE-ARTNR                                  
049800     MOVE '001441'         TO LINE-LEVNR                                  
049900     MOVE IN-EKHT-KVANTAL  TO W-ANTAL                                     
050000     MOVE W-ANTAL          TO LINE-ANTAL-X                                
050100     MOVE '01'             TO LINE-SORT                                   
050200     MOVE IN-EKHT-DAVERDAT TO LINE-DATUM-AVS                              
050300     MOVE IN-EKHT-IDORDNR5 TO W-IDORDNR6                                  
050400     MOVE W-IDORDNR6       TO LINE-PACKNR                                 
050500     MOVE WS-FTAG          TO LINE-FTAG-GODSMOT                           
050600     MOVE IN-EKHT-PRARTNTO TO LINE-PRIS-FR                                
050700     MOVE '1'              TO LINE-ENHET-PRIS                             
050800     COMPUTE LINE-BEL-FR = IN-EKHT-PRARTNTO * IN-EKHT-KVANTAL             
050900                                                                          
051000**** TAG BORT AVSLUTANDE 'SIGN-BOKSTÄVER'                                 
051100     MOVE LINE-PRIS-FR-X(11:1) TO W-N                                     
051200     MOVE W-N              TO LINE-PRIS-FR(11:1)                          
051300     IF IN-EKHT-KVANTAL > ZERO                                            
051400       MOVE LINE-BEL-FR-X(11:1) TO W-N                                    
051500       MOVE W-N              TO LINE-BEL-FR(11:1)                         
051600     END-IF                                                               
051700                                                                          
051800     PERFORM S14-SKRIV-LINE-W51065                                        
051900     .                                                                    
052000     EJECT                                                                
052100 G-SKAPA-LEVA1-KOMP-POST SECTION.                                         
052200                                                                          
052300     IF W-6E-FIRST = JA                                                   
052400       MOVE SPACE          TO HEAD2-AREA                                  
052500       MOVE '504'          TO HEAD2-PTYP                                  
052700       MOVE WS-FTAG        TO HEAD2-FTAG                                  
052800       MOVE DAGENS-AAR     TO HEAD2-AR                                    
052900       MOVE D-VECKA        TO HEAD2-VA                                    
053000       PERFORM S15-SKRIV-HEAD-W5106E                                      
053100       MOVE NEJ            TO W-6E-FIRST                                  
053200     END-IF                                                               
053300                                                                          
053400     MOVE SPACE            TO LINE2-AREA                                  
053500     MOVE '505'            TO LINE2-PTYP                                  
053600     MOVE IN-EKHT-IDARTNR  TO W-IDARTNR                                   
053700     MOVE W-IDARTNR        TO LINE2-ARTNR                                 
053800     MOVE '001441'         TO LINE2-LEVNR                                 
053900     MOVE IN-EKHT-KVANTAL  TO W-ANTAL                                     
054000     MOVE W-ANTAL          TO LINE2-ANTAL-X                               
054100     MOVE '01'             TO LINE2-SORT                                  
054200     MOVE IN-EKHT-DAVERDAT TO LINE2-DATUM-AVS                             
054300     MOVE IN-EKHT-IDORDNR5 TO W-IDORDNR6                                  
054400     MOVE W-IDORDNR6       TO LINE2-PACKNR                                
054500     MOVE WS-FTAG          TO LINE2-FTAG-GODSMOT                          
054600     MOVE IN-EKHT-PRARTNTO TO LINE2-PRIS-FR                               
054700     MOVE '1'              TO LINE2-ENHET-PRIS                            
054800     COMPUTE LINE2-BEL-FR = IN-EKHT-PRARTNTO * IN-EKHT-KVANTAL            
054900                                                                          
055000*****    TAG BORT AVSLUTANDE 'SIGN-BOKSTÄVER'                             
055100     MOVE LINE2-PRIS-FR-X(11:1) TO W-N                                    
055200     MOVE W-N              TO LINE2-PRIS-FR(11:1)                         
055300     IF IN-EKHT-KVANTAL > ZERO                                            
055400       MOVE LINE2-BEL-FR-X(11:1) TO W-N                                   
055500       MOVE W-N            TO LINE2-BEL-FR(11:1)                          
055600     END-IF                                                               
055700                                                                          
055800     PERFORM S16-SKRIV-LINE-W5106E                                        
055900     .                                                                    
056000     EJECT                                                                
056100 H-SKAPA-LEVA1-VCC-POST SECTION.                                          
056200                                                                          
056300     IF W-6E-FIRST = JA                                                   
056400       MOVE SPACE          TO HEAD2-AREA                                  
056500       MOVE '504'          TO HEAD2-PTYP                                  
056700       MOVE WS-FTAG        TO HEAD2-FTAG                                  
056800       MOVE DAGENS-AAR     TO HEAD2-AR                                    
056900       MOVE D-VECKA        TO HEAD2-VA                                    
057000       PERFORM S15-SKRIV-HEAD-W5106E                                      
057100       MOVE NEJ            TO W-6E-FIRST                                  
057200     END-IF                                                               
057300                                                                          
057400     MOVE SPACE            TO LINE2-AREA                                  
057500     MOVE '505'            TO LINE2-PTYP                                  
057600     MOVE IN-EKHT-IDARTNR  TO W-IDARTNR                                   
057700     MOVE W-IDARTNR        TO LINE2-ARTNR                                 
057800     MOVE '001441'         TO LINE2-LEVNR                                 
057900     MOVE IN-EKHT-KVANTAL  TO W-ANTAL                                     
058000     MOVE W-ANTAL          TO LINE2-ANTAL-X                               
058100     MOVE '01'             TO LINE2-SORT                                  
058200     MOVE IN-EKHT-DAVERDAT TO LINE2-DATUM-AVS                             
058300     MOVE IN-EKHT-IDORDNR5 TO W-IDORDNR6                                  
058400     MOVE W-IDORDNR6       TO LINE2-PACKNR                                
058500     MOVE WS-FTAG          TO LINE2-FTAG-GODSMOT                          
058600     MOVE IN-EKHT-PRARTNTO TO LINE2-PRIS-FR                               
058700     MOVE '1'              TO LINE2-ENHET-PRIS                            
058800     COMPUTE LINE2-BEL-FR = IN-EKHT-PRARTNTO * IN-EKHT-KVANTAL            
058900                                                                          
059000***    TAG BORT AVSLUTANDE 'SIGN-BOKSTÄVER'                               
059100     MOVE LINE2-PRIS-FR-X(11:1) TO W-N                                    
059200     MOVE W-N              TO LINE2-PRIS-FR(11:1)                         
059300     IF IN-EKHT-KVANTAL > ZERO                                            
059400       MOVE LINE2-BEL-FR-X(11:1) TO W-N                                   
059500       MOVE W-N            TO LINE2-BEL-FR(11:1)                          
059600     END-IF                                                               
059700                                                                          
059800     PERFORM S16-SKRIV-LINE-W5106E                                        
059900     .                                                                    
060000     EJECT                                                                
060001                                                                          
060010 I-SKAPA-LEVA1-MAAS-POST SECTION.                                         
060030     IF W-6H-FIRST = JA                                                   
060040       MOVE SPACE          TO HEAD3-AREA                                  
060050       MOVE '504'          TO HEAD3-PTYP                                  
060070       MOVE WS-FTAG        TO HEAD3-FTAG                                  
060080       MOVE DAGENS-AAR     TO HEAD3-AR                                    
060090       MOVE D-VECKA        TO HEAD3-VA                                    
060091       PERFORM S15-SKRIV-HEAD-W5106H                                      
060092       MOVE NEJ            TO W-6H-FIRST                                  
060093     END-IF                                                               
060094                                                                          
060095     MOVE SPACE            TO LINE3-AREA                                  
060096     MOVE '505'            TO LINE3-PTYP                                  
060097     MOVE IN-EKHT-IDARTNR  TO W-IDARTNR                                   
060098     MOVE W-IDARTNR        TO LINE3-ARTNR                                 
060099     MOVE '001441'         TO LINE3-LEVNR                                 
060100     MOVE IN-EKHT-KVANTAL  TO W-ANTAL                                     
060101     MOVE W-ANTAL          TO LINE3-ANTAL-X                               
060102     MOVE '01'             TO LINE3-SORT                                  
060103     MOVE IN-EKHT-DAVERDAT TO LINE3-DATUM-AVS                             
060104     MOVE IN-EKHT-IDORDNR5 TO W-IDORDNR6                                  
060105     MOVE W-IDORDNR6       TO LINE3-PACKNR                                
060106     MOVE WS-FTAG          TO LINE3-FTAG-GODSMOT                          
060107     MOVE IN-EKHT-PRARTNTO TO LINE3-PRIS-FR                               
060108     MOVE '1'              TO LINE3-ENHET-PRIS                            
060109     COMPUTE LINE3-BEL-FR = IN-EKHT-PRARTNTO * IN-EKHT-KVANTAL            
060110                                                                          
060111*****    TAG BORT AVSLUTANDE 'SIGN-BOKSTÄVER'                             
060112     MOVE LINE3-PRIS-FR-X(11:1) TO W-N                                    
060113     MOVE W-N              TO LINE3-PRIS-FR(11:1)                         
060114     IF IN-EKHT-KVANTAL > ZERO                                            
060115       MOVE LINE3-BEL-FR-X(11:1) TO W-N                                   
060116       MOVE W-N            TO LINE3-BEL-FR(11:1)                          
060117     END-IF                                                               
060118                                                                          
060119     PERFORM S16-SKRIV-LINE-W5106H                                        
060120     .                                                                    
060121     EJECT                                                                
060122                                                                          
060130 XA-SEARCH-DIST-KUND SECTION.                                             
060300     MOVE IN-EKHT-IDDISTR  TO W-DIST                                      
060400     MOVE IN-EKHT-IDKUNDNR TO W-KUND                                      
060500     MOVE SPACE            TO LINE2-AREA                                  
060510     MOVE SPACE            TO LINE3-AREA                                  
060600     MOVE JA               TO KUND-OK-SW                                  
060700                                                                          
060800     SET IX TO 1                                                          
060900     SEARCH DISTRIKT                                                      
061000       AT END                                                             
061100         MOVE NEJ          TO KUND-OK-SW                                  
061200       WHEN TAB-DIST(IX) = W-DIST AND                                     
061300            TAB-KUND(IX) = W-KUND                                         
061400         MOVE TAB-FTAG(IX) TO   WS-FTAG                                   
061500         MOVE JA           TO   KUND-OK-SW                                
061600     END-SEARCH                                                           
061700                                                                          
061800     .                                                                    
061900     EJECT                                                                
062000                                                                          
062100 Z-FINIT SECTION.                                                         
062200     CLOSE W51064                                                         
062300           W51060                                                         
062400           W51061A                                                        
062500           W51065                                                         
062600           W51067                                                         
062700           W5106E                                                         
062710           W5106H                                                         
062720           W51061C                                                        
062800     SKIP2                                                                
062900     MOVE 'S' TO POSTSUM-OPKOD                                            
063000     CALL POSTSUM USING POSTSUM-PARM                                      
063100     .                                                                    
063200     EJECT                                                                
063300                                                                          
063400 S01-LAES-W51064  SECTION.                                                
063500     READ W51064          INTO IN-AREA                                    
063600     AT END                                                               
063700        MOVE HIGH-VALUE   TO IN-AREA                                      
063800        SET END-OF-W51064 TO TRUE                                         
063900                                                                          
064000     NOT AT END                                                           
064100        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
064200        MOVE 'W51064'     TO POSTSUM-FDNAMN                               
064300        MOVE 'W51060D4'   TO POSTSUM-DDNAMN2                              
064400        CALL POSTSUM USING POSTSUM-PARM                                   
064500                                                                          
064600     END-READ                                                             
064700     .                                                                    
064800                                                                          
064900 S10-SKRIV-W51060 SECTION.                                                
065000     WRITE UT1-POST  FROM UT1-AREA                                        
065100                                                                          
065200     MOVE 'UT1-'     TO POSTSUM-TRANSTYP                                  
065300     MOVE 'W51060'   TO POSTSUM-FDNAMN                                    
065400     MOVE 'W51061D2' TO POSTSUM-DDNAMN2                                   
065500     CALL POSTSUM USING POSTSUM-PARM                                      
065600     .                                                                    
065700                                                                          
065800 S11-SKRIV-W51061A SECTION.                                               
065900     WRITE UT2-POST  FROM UT2-AREA                                        
066000                                                                          
066100     MOVE 'UT2-'     TO POSTSUM-TRANSTYP                                  
066200     MOVE 'W51061'   TO POSTSUM-FDNAMN                                    
066300     MOVE 'W51061D3' TO POSTSUM-DDNAMN2                                   
066400     CALL POSTSUM USING POSTSUM-PARM                                      
066500     .                                                                    
066600                                                                          
066610 S11-SKRIV-W51061C SECTION.                                               
066620     WRITE UT4-POST  FROM UT4-AREA                                        
066630                                                                          
066640     MOVE 'UT4-'     TO POSTSUM-TRANSTYP                                  
066650     MOVE 'W51061'   TO POSTSUM-FDNAMN                                    
066660     MOVE 'W51061D8' TO POSTSUM-DDNAMN2                                   
066670     CALL POSTSUM USING POSTSUM-PARM                                      
066680     .                                                                    
066690                                                                          
066700 S12-SKRIV-W51067 SECTION.                                                
066800     WRITE UT3-POST  FROM UT3-AREA                                        
066900                                                                          
067000     MOVE 'UT3-'     TO POSTSUM-TRANSTYP                                  
067100     MOVE 'W51067'   TO POSTSUM-FDNAMN                                    
067200     MOVE 'W51061D4' TO POSTSUM-DDNAMN2                                   
067300     CALL POSTSUM USING POSTSUM-PARM                                      
067400     .                                                                    
067500                                                                          
067600 S13-SKRIV-HEAD-W51065 SECTION.                                           
067700                                                                          
067800     WRITE LRAD-POST  FROM HEAD-AREA                                      
067900                                                                          
068000     MOVE 'HEAD'     TO POSTSUM-TRANSTYP                                  
068100     MOVE 'W51065'   TO POSTSUM-FDNAMN                                    
068200     MOVE 'W51061D5' TO POSTSUM-DDNAMN2                                   
068300     CALL POSTSUM USING POSTSUM-PARM                                      
068400     .                                                                    
068500                                                                          
068600 S14-SKRIV-LINE-W51065 SECTION.                                           
068700                                                                          
068800     WRITE LRAD-POST  FROM LINE-AREA                                      
068900                                                                          
069000     MOVE 'LINE'     TO POSTSUM-TRANSTYP                                  
069100     MOVE 'W51065'   TO POSTSUM-FDNAMN                                    
069200     MOVE 'W51061D5' TO POSTSUM-DDNAMN2                                   
069300     CALL POSTSUM USING POSTSUM-PARM                                      
069400     .                                                                    
069500                                                                          
069600 S15-SKRIV-HEAD-W5106E SECTION.                                           
069700                                                                          
069800     WRITE LRADE-POST  FROM HEAD2-AREA                                    
069900                                                                          
070000     MOVE 'HEAD'     TO POSTSUM-TRANSTYP                                  
070100     MOVE 'W5106E'   TO POSTSUM-FDNAMN                                    
070200     MOVE 'W51061D6' TO POSTSUM-DDNAMN2                                   
070300     CALL POSTSUM USING POSTSUM-PARM                                      
070400     .                                                                    
070500                                                                          
070600 S16-SKRIV-LINE-W5106E SECTION.                                           
070700                                                                          
070800     WRITE LRADE-POST  FROM LINE2-AREA                                    
070900                                                                          
071000     MOVE 'LINE'     TO POSTSUM-TRANSTYP                                  
071100     MOVE 'W5106E'   TO POSTSUM-FDNAMN                                    
071200     MOVE 'W51061D6' TO POSTSUM-DDNAMN2                                   
071300     CALL POSTSUM USING POSTSUM-PARM                                      
071400     .                                                                    
071500     EJECT                                                                
071510 S15-SKRIV-HEAD-W5106H SECTION.                                           
071520                                                                          
071530     WRITE LRADH-POST  FROM HEAD3-AREA                                    
071540                                                                          
071550     MOVE 'HEAD'     TO POSTSUM-TRANSTYP                                  
071560     MOVE 'W5106H'   TO POSTSUM-FDNAMN                                    
071570     MOVE 'W51061D7' TO POSTSUM-DDNAMN2                                   
071580     CALL POSTSUM USING POSTSUM-PARM                                      
071590     .                                                                    
071591                                                                          
071592 S16-SKRIV-LINE-W5106H SECTION.                                           
071593                                                                          
071594     WRITE LRADH-POST  FROM LINE3-AREA                                    
071595                                                                          
071596     MOVE 'LINE'     TO POSTSUM-TRANSTYP                                  
071597     MOVE 'W5106H'   TO POSTSUM-FDNAMN                                    
071598     MOVE 'W51061D7' TO POSTSUM-DDNAMN2                                   
071599     CALL POSTSUM USING POSTSUM-PARM                                      
071600     .                                                                    
071601     EJECT                                                                
071610* --- IMS SEKTIONER ---                                                   
071700                                                                          
071800 IMS-GET-SAPA   SECTION.                                                  
071900     CALL CBLTDLI USING GN SAPA-PCB DLI-IO-WLSAPA                         
072000     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
072100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
072200     PERFORM IMS-STATUSKONTROLL                                           
072300     .                                                                    
072400                                                                          
072500 IMS-STATUSKONTROLL SECTION.                                              
072600     SET STATUS-IX TO 1                                                   
072700     SEARCH GODK-STATUS                                                   
072800       AT END                                                             
072900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
073000           DELIMITED BY SIZE INTO FELTEXT                                 
073100         DISPLAY FELTEXT                                                  
073200         CALL FELLOG                                                      
073300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
073400         CONTINUE                                                         
073500     END-SEARCH                                                           
073600     .                                                                    
