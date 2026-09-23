000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0079100.                                                
000400 AUTHOR.         JANNE MELANDER                                           
000500 DATE-WRITTEN.   JAN.  88.                                                
000600 DATE-COMPILED.                                                           
000700***************************************************************           
000800*                                                             *           
000900*    FUNKTION.  " BASIC STOCK LISTS"                          *           
001000*                                                             *           
001100*        NYCKLAR: KDBASLM OCH PROJ                            *           
001200*                 BÅDA MÅSTE FYLLAS I.                        *           
001300*        PGM SKAPAR ETT KORT OM 80 TKN.                       *           
001400*            VILKET LÄGGS UPP PÅ EN KORT-DATABAS.             *           
001500*            KORTET ANVÄNDS SEDAN SOM INDATA I ETT BATCH-PGM  *           
001600*        PGM KONTROLLERAR OM MARKNAD + EV DISTRIKT FINNS      *           
001700*            UPPLAGD.                                         *           
001800*            SAMT OM RÄTT SPRÅKKOD ÄR IFYLLD.                 *           
001900*                                                             *           
002000*            ÄR ALLT RÄTT SKAPAS KORTET SOM ÄR NYCKLAR FÖR    *           
002100*            VIDARE UTSÖKNING AV DATA TILL LISTAN.            *           
002200***************************************************************           
002300     INDATA.                                                              
002400         TRANSAKTION: W0T791                                              
002500         MID:         W0I79101                                            
002600                                                                          
002700     UTDATA.                                                              
002800         MOD:         W0O79101                                            
002900         TRANSAKTION  W0T709U                                             
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP3                                                                
003300 DATA DIVISION.                                                           
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(8)    VALUE 'W0079100'.            
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004000 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004100 77  SEQIX                       PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77  DIST-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
004300 77  MAX-IX-PLUS-1               PIC S9(9)   VALUE +15  COMP SYNC.        
004400 77  MIN-MOD-LAENGD              PIC S9(4)   VALUE +49  COMP SYNC.        
004500 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +171 COMP SYNC.        
004600                                                                          
004700 77  WS-DATUM                    PIC 9(7)    VALUE ZERO.                  
004800 77  WS-TIBLREG                  PIC 9(7)    VALUE ZERO.                  
004900 77  SPAR-KDBASLM                PIC X(6)    VALUE SPACE.                 
005000                                                                          
005100 77  WS-KDBASLM                  PIC X(6)    VALUE SPACE.                 
005200 77  WS-KDPRODSL                 PIC S9(3)   VALUE ZERO COMP-3.           
005300 77  WS-IDPROJ                   PIC X(4)    VALUE SPACE.                 
005400 77  WS-IDFKNGRP-1               PIC 9(5)    VALUE ZERO.                  
005500 77  WS-IDFKNGRP-2               PIC 9(5)    VALUE ZERO.                  
005600 77  WS-IDDISTR                  PIC 9(5)    VALUE ZERO.                  
005700 77  WS-KDBPSR-1                 PIC 9(1)    VALUE ZERO.                  
005800 77  WS-KDBPSR-2                 PIC 9(1)    VALUE ZERO.                  
005900 77  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
006000 77  WS-MARK-NOT                 PIC X(1)    VALUE SPACE.                 
006100 77  WS-MARK-QTY                 PIC X(1)    VALUE SPACE.                 
006200 77  WS-KDDEALER                 PIC X(1)    VALUE SPACE.                 
006201*---------------- USER-IDENTIFIKATION I SYSOUTEN                          
006210 01  WS-IDUSER-AREA.                                                      
006220    03 FILLER                   PIC X(41) VALUE                           
006221                     '//******  DETTA JOBB INITIERAT AV USERID '.         
006230    03 WS-IDUSER-USER           PIC X(39) VALUE SPACE.                    
006300*---------------- SORTELEMENT TILL UTPOST                                 
006400 01  SORTELEMENTTAB.                                                      
006500    03  WS-SORTOUT-TAB  OCCURS 4.                                         
006600       05  WS-SORT-SEQUENCE      PIC X(10).                               
006700                                                                          
006800*---------------- SORTELEMENT NAMN-TABELL                                 
006900 01  SORT-MAMNTABELL.                                                     
007000    03 SORT-VALUES               PIC X(40) VALUE                          
007100      'IDFKNGRP  IDARTNR   TISTOMREG KDPRODSL  '.                         
007200                                                                          
007300    03 FILLER REDEFINES SORT-VALUES.                                      
007400       05 SEQ-TEXT-TAB           OCCURS 4.                                
007500          07 SEQ-TEXT            PIC X(10).                               
007600                                                                          
007700*---------------- SORTERINGSORDNINGS-KONTROLL-TABELL                      
007800 01  KONTROLLTABELL.                                                      
007900    03  WS-SEQUENCE-TAB OCCURS 4.                                         
008000       05  WS-SEQUENCE            PIC 9.                                  
008100                                                                          
008200 77  WS-SORTFIELD-KEY             PIC 9(4).                               
008300    88  GODK-SORTFIELD  VALUE                                             
008400        0000  1000  2000  3000  4000                                      
008500        1200  1300  1400                                                  
008600        2100  2300  2400                                                  
008700        3100  3200  3400                                                  
008800        4100  4200  4300                                                  
008900        1230  1240  1320  1340  1420  1430                                
009000        2130  2140  2310  2340  2410  2430                                
009100        3120  3140  3210  3240  3410  3420                                
009200        4120  4130  4210  4230  4310  4320                                
009300        1234  1243  1324  1342  1423  1432                                
009400        2134  2143  2314  2341  2413  2431                                
009500        3124  3142  3214  3241  3412  3421                                
009600        4123  4132  4213  4231  4312  4321.                               
009700                                                                          
009800 01  WS-KDBASLM-KEY                   PIC X(8) VALUE SPACE.               
009900 01  FILLER REDEFINES WS-KDBASLM-KEY.                                     
010000   03  WS-KDBASLM-JOBNAMN             PIC X(6).                           
010100   03  FILLER                         PIC X(2).                           
010200                                                                          
010300*01   -COPY WWKDDEAL        -PRE GODK-                                    
010400     SKIP3                                                                
010500*01   -COPY WWGODKPV                                                      
010600     SKIP3                                                                
010700 77  INDATA-SW                   PIC X.                                   
010800   88  INDATA-OK                             VALUE 'J'.                   
010900     SKIP3                                                                
011000                                                                          
011100 77  UPPDATERING-SW                   PIC X.                              
011200   88  UPPDATERING-OK                        VALUE 'J'.                   
011300     SKIP3                                                                
011400 01  FILLER                      PIC X(16)  VALUE 'DYNAM SUBPGM'.         
011500 01  DYNAM-SUBPGM.                                                        
011600    03 CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI'.              
011700    03 FELLOG                    PIC X(8)   VALUE 'FELLOG '.              
011800                                                                          
011900 01  NYCKLAR-TILL-DLI.                                                    
012000                                                                          
012100   03  W-6021-WDP101KY-X.                                                 
012200     05  FILLER                  PIC X(4)    VALUE '6021'.                
012300     05  W-6021-IDRUTIN          PIC X(8)    VALUE 'W115S1  '.            
012400     05  W-6021-IDJOB            PIC X(8)    VALUE SPACE.                 
012500     05  FILLER                  PIC X(10)   VALUE LOW-VALUE.             
012600                                                                          
012700   03  W-6021-IDJCLRAD-X.                                                 
012800     05  W-6021-IDJCLRAD         PIC S9(5)   COMP-3.                      
012900                                                                          
013000   03  W-6021-IDUSER-X.                                                   
013100     05  W-6021-IDUSER           PIC X(8).                                
013200                                                                          
013300   03  W-1123-KEY-X.                                                      
013400     05  FILLER                  PIC X(4)   VALUE '1123'.                 
013500     05  W-1123-KDPRODSL         PIC S9(3)  COMP-3 VALUE +11.             
013600     05  W-1123-IDPROJ           PIC X(4)   VALUE SPACE.                  
013700     05  FILLER                  PIC X(20)  VALUE LOW-VALUE.              
013800                                                                          
013900   03  W-1124-KEY-X.                                                      
014000     05  W-1124-KDSEGKEY         PIC X(1)   VALUE '1'.                    
014100                                                                          
014200   03  W-1126-KEY-X.                                                      
014300     05  W-1126-KDBASLM          PIC X(6)   VALUE SPACE.                  
014400     05  FILLER                  PIC X(9)   VALUE LOW-VALUE.              
014500                                                                          
014600     EJECT                                                                
014700 01  MEDDELANDE.                                                          
014800   03  FEL1.                                                              
014900     05 FILLER                   PIC X(40)                                
015000          VALUE 'UPPLYSTA FÄLT FEL                      '.                
015100     05 FILLER                   PIC X(40)                                
015200          VALUE 'HIGHLITED FIELDS INCORRECT             '.                
015300   03  FILLER REDEFINES FEL1.                                             
015400     05  FEL-1                   PIC X(40)   OCCURS 2.                    
015500                                                                          
015600   03  FEL2.                                                              
015700     05 FILLER                   PIC X(40)                                
015800          VALUE 'PROD.GRP / PROJ SAKNAS I BASEN         '.                
015900     05 FILLER                   PIC X(40)                                
016000          VALUE 'PROD.GRP / PROJ IS MISSING IN DATABASE '.                
016100   03  FILLER REDEFINES FEL2.                                             
016200     05  FEL-2                   PIC X(40)   OCCURS 2.                    
016300                                                                          
016400   03  FEL3.                                                              
016500     05 FILLER                   PIC X(40)                                
016600          VALUE 'ANGIVEN MARKNAD SAKNAS I BASEN         '.                
016700     05 FILLER                   PIC X(40)                                
016800          VALUE 'MARKET IS MISSING IN DATABASE          '.                
016900   03  FILLER REDEFINES FEL3.                                             
017000     05  FEL-3                   PIC X(40)   OCCURS 2.                    
017100                                                                          
017200   03  FEL4.                                                              
017300     05 FILLER                   PIC X(40)                                
017400          VALUE 'ANGIVET DISTRIKT SAKNAS                '.                
017500     05 FILLER                   PIC X(40)                                
017600          VALUE 'INCORRECT DISTRICT                     '.                
017700   03  FILLER REDEFINES FEL4.                                             
017800     05  FEL-4                   PIC X(40)   OCCURS 2.                    
017900                                                                          
018000   03  FEL5.                                                              
018100     05 FILLER                   PIC X(40)                                
018200          VALUE 'EJ AUKTORISERAD ANVÄNDARE,6021-SECURITY'.                
018300     05 FILLER                   PIC X(40)                                
018400          VALUE 'USER NOT AUTHORIZED,      6021-SECURITY'.                
018500   03  FILLER REDEFINES FEL5.                                             
018600     05  FEL-5                   PIC X(40)   OCCURS 2.                    
018700                                                                          
018800   03  FEL6.                                                              
018900     05 FILLER                   PIC X(40)                                
019000          VALUE 'RUTIN FINNS EJ UPPLAGD PÅ ANG. MARKNAD '.                
019100     05 FILLER                   PIC X(40)                                
019200          VALUE 'NO PRINTING ROUTIN FOR THIS MARKET     '.                
019300   03  FILLER REDEFINES FEL6.                                             
019400     05  FEL-6                   PIC X(40)   OCCURS 2.                    
019500                                                                          
019600   03  FEL8.                                                              
019700     05 FILLER                   PIC X(40)                                
019800          VALUE 'INGA ARTIKLAR REG. FÖR DETTA PROJ      '.                
019900     05 FILLER                   PIC X(40)                                
020000          VALUE 'NO PARTS UPDATED FOR THIS PROJ         '.                
020100   03  FILLER REDEFINES FEL8.                                             
020200     05  FEL-8                   PIC X(40)   OCCURS 2.                    
020300                                                                          
020400   03  MED2.                                                              
020500     05 FILLER                   PIC X(40)                                
020600          VALUE 'KLART FÖR LIST PRINTNING, TRYCK PF11   '.                
020700     05 FILLER                   PIC X(40)                                
020800          VALUE 'READY FOR PRINTING PRESS PF11          '.                
020900   03  FILLER REDEFINES MED2.                                             
021000     05  MED-2                   PIC X(40)   OCCURS 2.                    
021100                                                                          
021200     EJECT                                                                
021300*01  001-AREA   -COPY W1150501C0 -PRE W11505-.                            
021400     EJECT                                                                
021500******************************************************************        
021600*                                                                         
021700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
021800*                                                                         
021900 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
022000     SKIP3                                                                
022100*01  MID -COPY W0I79101                                                   
022200     EJECT                                                                
022300*01  -COPY WMSGAREA                                                       
022400     EJECT                                                                
022500*  03  MOD -COPY W0O79101           -RED MSG-AREA.                        
022600     EJECT                                                                
022700*  03  MID-AREA -COPY W0I70901   -PRE MOD-.                               
022800     EJECT                                                                
022900*01  -COPY WMFSAREA                                                       
023000     EJECT                                                                
023100******************************************************************        
023200*                                                                         
023300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023400*                                                                         
023500 01  IMS-WS.                                                              
023600   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
023700     SKIP3                                                                
023800*                        **** STATUS-KOD FRÅN IMS                         
023900   03  STATUS-WS                 PIC XX.                                  
024000     88  SEGMENT-FINNS                       VALUE '  '.                  
024100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024200     88  BAS-SLUT                            VALUE 'GB'.                  
024300     SKIP3                                                                
024400   03  GODK-STATUSKODER.                                                  
024500     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024600     SKIP3                                                                
024700 01    SSA1                      PIC X(64).                               
024800 01    SSA2                      PIC X(64).                               
024900     EJECT                                                                
025000*                            IMS FUNKTIONSKODER                           
025100*01    -COPY W0003                                                        
025200     EJECT                                                                
025300*                            DLI INPUT-OUTPUT AREA                        
025400 01  DLI-IO-AREA1.                                                        
025500   03  IO-AREA1                  PIC X(200)  VALUE SPACE.                 
025600     SKIP3                                                                
025700     SKIP3                                                                
025800*  03  WLXXAP01  -COPY WDGX1123    -RED IO-AREA1.                         
025900     EJECT                                                                
026000*  03  WLXXAP11  -COPY WDGX1124    -RED IO-AREA1.                         
026100     EJECT                                                                
026200*  03  WLXXAP12  -COPY WDGX1126    -RED IO-AREA1.                         
026300     EJECT                                                                
026400*  03  WLJCLD01  -COPY WDP101      -RED IO-AREA1 -PRE WLJCLD-.            
026500     EJECT                                                                
026600*  03  WLJCLD11  -COPY WDP111      -RED IO-AREA1 -PRE WLJCLD-.            
026700     EJECT                                                                
026800*  03  WLJCLD12  -COPY WDP114      -RED IO-AREA1 -PRE WLJCLD-.            
026900     EJECT                                                                
027000 LINKAGE SECTION.                                                         
027100*01  -COPY W0009     -PRE MSG-                                            
027200     SKIP2                                                                
027300*01  -COPY W0009     -PRE ALT-                                            
027400     SKIP2                                                                
027500*01  -COPY W0008     -PRE WLJCLD-                                         
027600     05  FILLER                  PIC X.                                   
027700     EJECT                                                                
027800*01  -COPY W0008     -PRE XXAP-                                           
027900     05  FILLER                  PIC X.                                   
028000     EJECT                                                                
028100 PROCEDURE DIVISION USING MSG-PCB ALT-PCB WLJCLD-PCB XXAP-PCB.            
028200 MAIN SECTION.                                                            
028300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WLJCLD-PCB XXAP-PCB.           
028400     PERFORM IMS-GET-MSG                                                  
028500     IF SEGMENT-FINNS                                                     
028600        PERFORM A-INIT-SPARA-INPUT                                        
028700        MOVE NEJ TO UPPDATERING-SW                                        
028800        IF MFS-IDTRANS = '0791'                                           
028900           IF MFS-UPDATE                                                  
029000              PERFORM B-KOLLA-INDATA                                      
029100              IF INDATA-OK                                                
029200                 PERFORM C-UPPDATERA                                      
029300                 MOVE JA TO UPPDATERING-SW                                
029400              ELSE                                                        
029500                 PERFORM S1-ROER-EJ-FAELT-VISA                            
029600              END-IF                                                      
029700           ELSE                                                           
029800              PERFORM B-KOLLA-INDATA                                      
029900              IF INDATA-OK                                                
030000                 MOVE MED-2 (SPRAK-IX) TO MOD-TEMFSINF                    
030100                 PERFORM S1-ROER-EJ-FAELT-VISA                            
030200              ELSE                                                        
030300                 PERFORM S1-ROER-EJ-FAELT-VISA                            
030400              END-IF                                                      
030500           END-IF                                                         
030600        ELSE                                                              
030700           PERFORM S3-RENSA-FAELT-MOD                                     
030800        END-IF                                                            
030900*-------------- I PROJ-BAS GÄLLER PRODSL +11 FÖR ALLA ART.                
031000*-------------- I WS-KDPRODSL LIGGER VALT PRODUKTSLAG FÖR LISTAN          
031100*-------------- OM PRODSL EJ HAR VALTS, LIGGER ZERO HÄR.                  
031200*-------------- RÄTTA PRODUKTSLAGET LÄSES FRÅN ARTA01 OCH                 
031300*-------------- DETTA AVSES VID SORTORDNING AV PRODSL, SAMT VID           
031400*-------------- SELEKTERING AV ETT PRODSL, (WS-KDPRODSL > ZERO)           
031500        IF UPPDATERING-OK                                                 
031600           CONTINUE                                                       
031700        ELSE                                                              
031800           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
031900           PERFORM IMS-INSERT-MSG                                         
032000        END-IF                                                            
032100     END-IF                                                               
032200*                                                                         
032300     MOVE ZERO TO RETURN-CODE                                             
032400     GOBACK                                                               
032500     .                                                                    
032600     EJECT                                                                
032700 A-INIT-SPARA-INPUT SECTION.                                              
032800                                                                          
032900     IF MSG-DUBBLA-TRANSKODER                                             
033000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I79101                 
033100       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
033200       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
033300     ELSE                                                                 
033400       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W0I79101                 
033500       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
033600       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
033700     END-IF                                                               
033800     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
033900     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
034000                                                                          
034100     IF MFS-KDMFSFOR = '2'                                                
034200       MOVE +2 TO SPRAK-IX                                                
034300     ELSE                                                                 
034400       MOVE +1 TO SPRAK-IX                                                
034500     END-IF                                                               
034600                                                                          
034700     MOVE LOW-VALUE TO MSG-AREA                                           
034800     MOVE 'W0O79101' TO MFS-IDMOD                                         
034900     MOVE '0791' TO MOD-IDTRANS                                           
035000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
035100                             MOD-KDBASLM  MOD-IDPROJ                      
035200                                                                          
035300                                                                          
035400     ACCEPT WS-DATUM FROM DATE                                            
035500                                                                          
035600     .                                                                    
035700     EJECT                                                                
035800 B-KOLLA-INDATA SECTION.                                                  
035900***********************************************************               
036000* KONTROLLERAR OM INDATAN ÄR KORREKT.                                     
036100* KDBASLM,IDDISTR MÅSTE FINNAS REGISTRERADE PÅ ANGIVET                    
036200* IDPROJ UNDER KDPRODSL=11 PÅ PROJEKTBASEN                                
036300* OM INGA ARTIKLAR FINNS REGISTRERADE PÅ SÖKT KOD KAN MAN INTE            
036400* KÖRA LISTAN.                                                            
036500***********************************************************               
036600     MOVE JA TO INDATA-SW                                                 
036700**   MID-KDBASLM = JOBNAMN                                                
036800**   KONTROLLERA OM JOBBET FINNS UPPLAGT                                  
036900**   BEREDARE     LARS ANDERSSON     KAN KÖRA MARKNADERNAS LISTOR.        
036910**   LAGERSTYRARE LARS-ERIK MAGNUSSON   - " -                             
036920**   LAGERSTYRARE SARA BERGGREN         - " -                             
036930**   LAGERSTYRARE ANDERS LARSSON        - " -                             
036940**   LAGERSTYRARE MIKAEL SKJÖLD         - " -                             
036950**                                                                        
037000**   NÄR DE GÖR DET SÅ KOMMER LISTAN UT PÅ DEN PRINTER SOM                
037010**   ÄR SPECAD FÖR RESPEKTIVE KDBASLM-JOBNAMN I SUBMITPAKETET.            
037011*                                                                         
037020**   ALLA LAGERSTYRARES LISTOR ADRESSERAS TILL L-E MAGNUSSON              
037030*                                                                         
037100**   DENNA FUNKTION STYRS AV JOBNAMN VIA USERID. SE TEST NEDAN            
037200*                                                                         
037300     MOVE MSG-SIGNON-USERID TO W-6021-IDUSER                              
037310*                *                                                        
037320*                * VID ÄNDRING AV USERID, KOLLA ÄVEN C-UPPDATERA          
037330*                *                                                        
037400     EVALUATE MSG-SIGNON-USERID                                           
037500        WHEN 'PC07780 '                                                   
037600*             LARS-ERIK MAGNUSSON, AVD 57250                              
037700              MOVE 'RSBSL1'    TO WS-KDBASLM-JOBNAMN                      
037701        WHEN 'PC31775 '                                                   
037702*             MIKAEL SKJÖLD , AVD 57250                                   
037703              MOVE 'RSBSL1'    TO WS-KDBASLM-JOBNAMN                      
037704        WHEN 'PC54401 '                                                   
037705*             ANDERS LARSSON, AVD 57250                                   
037706              MOVE 'RSBSL1'    TO WS-KDBASLM-JOBNAMN                      
037707        WHEN 'PC55338 '                                                   
037708*             SARA BERGGREN,  AVD 57250                                   
037709              MOVE 'RSBSL1'    TO WS-KDBASLM-JOBNAMN                      
037710        WHEN 'PC55451 '                                                   
037711*             CARINA JANSENIUS AVD 57250                                  
037712              MOVE 'RSBSL1'    TO WS-KDBASLM-JOBNAMN                      
037714        WHEN 'PC01715 '                                                   
037720*             GUNNAR MAGNUSSON, AVD 57713                                 
037730              MOVE 'RSBSL '    TO WS-KDBASLM-JOBNAMN                      
038010*-------------------------------------------------------                  
038100        WHEN 'PC59625 '                                                   
038200*             BODIL LINDAHL SYST.UTV.VCAS                                 
038300              MOVE 'CONNY'     TO WS-KDBASLM-JOBNAMN                      
038400        WHEN 'PC28439 '                                                   
038500*             CONNY EGHOLT  SYST.UTV.VCAS                                 
038600              MOVE 'CONNY'     TO WS-KDBASLM-JOBNAMN                      
038900        WHEN OTHER                                                        
039000              MOVE MID-KDBASLM TO WS-KDBASLM-JOBNAMN                      
039100     END-EVALUATE                                                         
039200     MOVE WS-KDBASLM-KEY TO W-6021-IDJOB                                  
039300     PERFORM IMS-GET-6021-ROT                                             
039400     IF SEGMENT-FINNS                                                     
039500        PERFORM IMS-GET-6021-SECURITY                                     
039600        IF SEGMENT-FINNS                                                  
039700           PERFORM BA-KOLLA-OBL-DATA                                      
039800           PERFORM BB-KOLLA-VALFRIA-DATA                                  
039900           PERFORM BC-KOLLA-SEKVENS-DATA                                  
040000           IF INDATA-OK                                                   
040100              PERFORM BD-KOLLA-MOT-BASEN                                  
040200           END-IF                                                         
040300        ELSE                                                              
040400           MOVE NEJ TO INDATA-SW                                          
040500           MOVE FEL-5 (SPRAK-IX)          TO MOD-TEMFSFEL                 
040600        END-IF                                                            
040700     ELSE                                                                 
040800        MOVE NEJ TO INDATA-SW                                             
040900        MOVE FEL-6 (SPRAK-IX)          TO MOD-TEMFSFEL                    
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 BA-KOLLA-OBL-DATA SECTION.                                               
041400*******************************************                               
041500* ALLA DESSA FÄLT MÅSTE VARA I FYLLDA.                                    
041600*                                                                         
041700*******************************************                               
041800     EVALUATE TRUE ALSO TRUE                                              
041900        WHEN MID-KDBASLM  = ALL '+' ALSO  MID-IDPROJ = ALL '+'            
042000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBASLM-ATTR                    
042100                                     MOD-IDPROJ-ATTR                      
042200           MOVE NEJ               TO INDATA-SW                            
042300           MOVE FEL-1 (SPRAK-IX)  TO MOD-TEMFSFEL                         
042400        WHEN OTHER                                                        
042500           IF MID-KDBASLM NOT = ALL '+'                                   
042600              IF MID-KDBASLM NUMERIC                                      
042700                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBASLM-ATTR              
042800                 MOVE NEJ                TO INDATA-SW                     
042900                 MOVE FEL-1 (SPRAK-IX)   TO  MOD-TEMFSFEL                 
043000              ELSE                                                        
043100                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDBASLM-ATTR            
043200                 MOVE MID-KDBASLM          TO WS-KDBASLM                  
043300              END-IF                                                      
043400           ELSE                                                           
043500              MOVE MFS-ALFA-FAELT-FEL TO  MOD-KDBASLM-ATTR                
043600              MOVE NEJ                TO INDATA-SW                        
043700              MOVE FEL-1 (SPRAK-IX)   TO  MOD-TEMFSFEL                    
043800           END-IF                                                         
043900                                                                          
044000           IF MID-IDPROJ NOT = ALL '+'                                    
044100              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-ATTR                
044200              MOVE MID-IDPROJ           TO WS-IDPROJ                      
044300           ELSE                                                           
044400              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJ-ATTR                  
044500              MOVE NEJ                TO INDATA-SW                        
044600              MOVE FEL-1 (SPRAK-IX)   TO MOD-TEMFSFEL                     
044700           END-IF                                                         
044800     END-EVALUATE                                                         
044900     .                                                                    
045000     EJECT                                                                
045100 BB-KOLLA-VALFRIA-DATA SECTION.                                           
045200********************************************                              
045300*                                                                         
045400*                                                                         
045500********************************************                              
045600     IF MID-KDPRODSL = ALL '+'                                            
045900        MOVE ZERO TO WS-KDPRODSL                                          
046000     ELSE                                                                 
046100        IF MID-KDPRODSL NUMERIC                                           
046200           MOVE MID-KDPRODSL  TO  PRODUKTSLAG-PV                          
046300           IF GODK-PV                                                     
046400             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-ATTR                
046500             MOVE MID-KDPRODSL        TO WS-KDPRODSL                      
046600           ELSE                                                           
046700             MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                  
046800             MOVE NEJ               TO INDATA-SW                          
046900             MOVE FEL-1 (SPRAK-IX)  TO MOD-TEMFSFEL                       
047000           END-IF                                                         
047100        ELSE                                                              
047200           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR                    
047300           MOVE NEJ               TO INDATA-SW                            
047400           MOVE FEL-1 (SPRAK-IX)  TO MOD-TEMFSFEL                         
047500        END-IF                                                            
047600     END-IF                                                               
047700                                                                          
047800     IF MID-IDFKNGRP-1 NOT = ALL '+'                                      
047900        IF MID-IDFKNGRP-1 NUMERIC                                         
048000           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-1-ATTR                
048100           MOVE MID-IDFKNGRP-1      TO WS-IDFKNGRP-1                      
048200        ELSE                                                              
048300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-1-ATTR                  
048400           MOVE NEJ                 TO INDATA-SW                          
048500           MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                         
048600        END-IF                                                            
048700     ELSE                                                                 
048800        MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-1-ATTR                   
048900        MOVE ZERO TO WS-IDFKNGRP-1                                        
049000     END-IF                                                               
049100                                                                          
049200     IF MID-IDFKNGRP-2 NOT = ALL '+'                                      
049300        IF MID-IDFKNGRP-2 NUMERIC                                         
049400           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-2-ATTR                
049500           MOVE MID-IDFKNGRP-2      TO WS-IDFKNGRP-2                      
049600        ELSE                                                              
049700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-2-ATTR                  
049800           MOVE NEJ                 TO INDATA-SW                          
049900           MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                         
050000        END-IF                                                            
050100     ELSE                                                                 
050200        MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-2-ATTR                   
050300        MOVE ZERO  TO WS-IDFKNGRP-2                                       
050400     END-IF                                                               
050500                                                                          
050600     EVALUATE TRUE ALSO TRUE                                              
050700        WHEN WS-IDFKNGRP-1 = ZERO ALSO                                    
050800             WS-IDFKNGRP-2 = ZERO                                         
050900             CONTINUE                                                     
051000        WHEN OTHER                                                        
051100             IF WS-IDFKNGRP-1 < WS-IDFKNGRP-2                             
051200                CONTINUE                                                  
051300             ELSE                                                         
051400                IF WS-IDFKNGRP-2 = ZERO                                   
051500                   CONTINUE                                               
051600                ELSE                                                      
051700                   MOVE MFS-NUM-FAELT-FEL TO                              
051800                                   MOD-IDFKNGRP-2-ATTR                    
051900                   MOVE NEJ TO INDATA-SW                                  
052000                   MOVE FEL-1 (SPRAK-IX) TO                               
052100                                     MOD-TEMFSFEL                         
052200                END-IF                                                    
052300             END-IF                                                       
052400     END-EVALUATE                                                         
052500                                                                          
052600     IF MID-KDBPSR-1 NOT = ALL '+'                                        
052700        IF MID-KDBPSR-1 NUMERIC                                           
052800           IF MID-KDBPSR-1 < 9                                            
052900              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBPSR-1-ATTR               
053000              MOVE MID-KDBPSR-1      TO WS-KDBPSR-1                       
053100           ELSE                                                           
053200              MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-1-ATTR                 
053300              MOVE NEJ                 TO INDATA-SW                       
053400              MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                      
053500           END-IF                                                         
053600        ELSE                                                              
053700           MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-1-ATTR                    
053800           MOVE NEJ                 TO INDATA-SW                          
053900           MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                         
054000        END-IF                                                            
054100     ELSE                                                                 
054200        MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBPSR-1-ATTR                     
054300        MOVE ZERO              TO WS-KDBPSR-1                             
054400     END-IF                                                               
054500                                                                          
054600     IF MID-KDBPSR-2 NOT = ALL '+'                                        
054700        IF MID-KDBPSR-2 NUMERIC                                           
054800           IF MID-KDBPSR-2 < 9                                            
054900              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBPSR-2-ATTR               
055000              MOVE MID-KDBPSR-2      TO WS-KDBPSR-2                       
055100           ELSE                                                           
055200              MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-2-ATTR                 
055300              MOVE NEJ                 TO INDATA-SW                       
055400              MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                      
055500           END-IF                                                         
055600        ELSE                                                              
055700           MOVE MFS-NUM-FAELT-FEL TO MOD-KDBPSR-2-ATTR                    
055800           MOVE NEJ                 TO INDATA-SW                          
055900           MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                         
056000        END-IF                                                            
056100     ELSE                                                                 
056200        MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBPSR-2-ATTR                     
056300        MOVE ZERO              TO WS-KDBPSR-2                             
056400     END-IF                                                               
056500                                                                          
056600     EVALUATE TRUE ALSO TRUE                                              
056700        WHEN WS-KDBPSR-1 = ZERO ALSO                                      
056800             WS-KDBPSR-2 = ZERO                                           
056900             CONTINUE                                                     
057000        WHEN OTHER                                                        
057100             IF WS-KDBPSR-1 < WS-KDBPSR-2                                 
057200                CONTINUE                                                  
057300             ELSE                                                         
057400                IF WS-KDBPSR-2 = ZERO                                     
057500                   CONTINUE                                               
057600                ELSE                                                      
057700                   MOVE MFS-NUM-FAELT-FEL TO                              
057800                                MOD-KDBPSR-2-ATTR                         
057900                   MOVE NEJ TO INDATA-SW                                  
058000                   MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                 
058100                END-IF                                                    
058200             END-IF                                                       
058300     END-EVALUATE                                                         
058400                                                                          
058500                                                                          
058600     IF MID-IDSKYLT NOT = ALL '+'                                         
058700        IF MID-GODK-IDSKYLT                                               
058800           MOVE MID-IDSKYLT TO WS-IDSKYLT                                 
058900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-ATTR                  
059000        ELSE                                                              
059100           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-ATTR                    
059200           MOVE NEJ TO INDATA-SW                                          
059300           MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                         
059400        END-IF                                                            
059500     ELSE                                                                 
059600        MOVE 'GB ' TO WS-IDSKYLT                                          
059700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-ATTR                     
059800     END-IF                                                               
059900                                                                          
060000     IF MID-IDDISTR NOT = ALL '+'                                         
060100        IF MID-IDDISTR NUMERIC                                            
060200           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-ATTR                   
060300           MOVE MID-IDDISTR TO WS-IDDISTR                                 
060400        ELSE                                                              
060500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR                     
060600           MOVE NEJ         TO INDATA-SW                                  
060700           MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                         
060800        END-IF                                                            
060900     ELSE                                                                 
061000        MOVE ZERO TO WS-IDDISTR                                           
061100        MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-ATTR                      
061200     END-IF                                                               
061300                                                                          
061400     IF MID-KDDEALER NOT = ALL '+'                                        
061500        IF MID-KDDEALER NUMERIC                                           
061600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDDEALER-ATTR                   
061700           MOVE NEJ TO INDATA-SW                                          
061800           MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                         
061900        ELSE                                                              
062000           MOVE MID-KDDEALER TO GODK-KDDEALER                             
062100           IF GODK-KDDEALER-VAERDEN                                       
062200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDDEALER-ATTR              
062300              MOVE MID-KDDEALER TO WS-KDDEALER                            
062400           ELSE                                                           
062500              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDDEALER-ATTR                
062600              MOVE NEJ TO INDATA-SW                                       
062700              MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                      
062800           END-IF                                                         
062900        END-IF                                                            
063000     ELSE                                                                 
063100        MOVE ' '   TO WS-KDDEALER                                         
063200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDDEALER-ATTR                    
063300     END-IF                                                               
063400                                                                          
063500     IF MID-MARK-NOT NOT = ALL '+'                                        
063600        IF MID-MARK-NOT NUMERIC                                           
063700           MOVE MFS-ALFA-FAELT-FEL TO MOD-MARK-NOT-ATTR                   
063800           MOVE NEJ TO INDATA-SW                                          
063900           MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                         
064000        ELSE                                                              
064100           IF MID-MARK-NOT = 'Y' OR 'N' OR SPACE                          
064200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-MARK-NOT-ATTR              
064300              MOVE MID-MARK-NOT TO WS-MARK-NOT                            
064400           ELSE                                                           
064500              MOVE MFS-ALFA-FAELT-FEL TO MOD-MARK-NOT-ATTR                
064600              MOVE NEJ TO INDATA-SW                                       
064700              MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                      
064800           END-IF                                                         
064900        END-IF                                                            
065000     ELSE                                                                 
065100        MOVE SPACE TO WS-MARK-NOT                                         
065200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-MARK-NOT-ATTR                    
065300     END-IF                                                               
065400                                                                          
065500     IF MID-MARK-QTY NOT = ALL '+'                                        
065600        IF MID-MARK-QTY NUMERIC                                           
065700           MOVE MFS-ALFA-FAELT-FEL TO MOD-MARK-QTY-ATTR                   
065800           MOVE NEJ TO INDATA-SW                                          
065900           MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                         
066000        ELSE                                                              
066100           IF MID-MARK-QTY = 'Y' OR 'N' OR SPACE                          
066200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-MARK-QTY-ATTR              
066300              MOVE MID-MARK-QTY TO WS-MARK-QTY                            
066400           ELSE                                                           
066500              MOVE MFS-ALFA-FAELT-FEL TO MOD-MARK-QTY-ATTR                
066600              MOVE NEJ TO INDATA-SW                                       
066700              MOVE FEL-1 (SPRAK-IX) TO  MOD-TEMFSFEL                      
066800           END-IF                                                         
066900        END-IF                                                            
067000     ELSE                                                                 
067100        MOVE SPACE TO WS-MARK-QTY                                         
067200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-MARK-QTY-ATTR                    
067300     END-IF                                                               
067400     .                                                                    
067500     EJECT                                                                
067600 BC-KOLLA-SEKVENS-DATA SECTION.                                           
067700**********************************************                            
067800* KONTROLL AV SEKVENSEN  DVS SORTORDNINGEN                                
067900*                                                                         
068000***************************************                                   
068100     MOVE +1 TO SEQIX                                                     
068200     PERFORM UNTIL SEQIX > +4                                             
068300       MOVE ZERO TO WS-SEQUENCE(SEQIX)                                    
068400       ADD +1 TO SEQIX                                                    
068500     END-PERFORM                                                          
068600     EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE                          
068700     WHEN MID-IDFKNGRP-SORT   = ALL '+' ALSO                              
068800          MID-TISTOMREG-SORT  = ALL '+' ALSO                              
068900          MID-IDARTNR-SORT  = ALL '+'   ALSO                              
069000          MID-KDPRODSL-SORT  = ALL '+'                                    
069100          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-SORT-ATTR              
069200                                      MOD-TISTOMREG-SORT-ATTR             
069300                                      MOD-IDARTNR-SORT-ATTR               
069400                                      MOD-KDPRODSL-SORT-ATTR              
069500     WHEN OTHER                                                           
069600       IF MID-IDFKNGRP-SORT NOT = ALL '+'                                 
069700         IF MID-IDFKNGRP-SORT NUMERIC                                     
069800           IF MID-IDFKNGRP-SORT = 1 OR 2 OR 3 OR 4                        
069900             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-SORT-ATTR           
070000             MOVE MID-IDFKNGRP-SORT   TO SEQIX                            
070100             MOVE 1                   TO WS-SEQUENCE(SEQIX)               
070200             MOVE SEQ-TEXT(1)         TO WS-SORT-SEQUENCE(SEQIX)          
070300           ELSE                                                           
070400             MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-SORT-ATTR             
070500             MOVE NEJ               TO INDATA-SW                          
070600             MOVE FEL-1 (SPRAK-IX)  TO  MOD-TEMFSFEL                      
070700           END-IF                                                         
070800         ELSE                                                             
070900           MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-SORT-ATTR               
071000           MOVE NEJ               TO INDATA-SW                            
071100           MOVE FEL-1 (SPRAK-IX)  TO  MOD-TEMFSFEL                        
071200         END-IF                                                           
071300       ELSE                                                               
071400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-SORT-ATTR               
071500       END-IF                                                             
071600                                                                          
071700       IF MID-TISTOMREG-SORT NOT = ALL '+'                                
071800         IF MID-TISTOMREG-SORT NUMERIC                                    
071900           IF MID-TISTOMREG-SORT = 1 OR 2 OR 3 OR 4                       
072000             MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTOMREG-SORT-ATTR          
072100             MOVE MID-TISTOMREG-SORT  TO SEQIX                            
072200             IF WS-SEQUENCE(SEQIX) NOT = ZERO                             
072300               MOVE MFS-NUM-FAELT-FEL TO MOD-TISTOMREG-SORT-ATTR          
072400               MOVE NEJ               TO INDATA-SW                        
072500               MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                      
072600             ELSE                                                         
072700               MOVE 3                 TO WS-SEQUENCE(SEQIX)               
072800               MOVE SEQ-TEXT(3)       TO WS-SORT-SEQUENCE(SEQIX)          
072900             END-IF                                                       
073000           ELSE                                                           
073100             MOVE MFS-NUM-FAELT-FEL TO MOD-TISTOMREG-SORT-ATTR            
073200             MOVE NEJ               TO INDATA-SW                          
073300             MOVE FEL-1 (SPRAK-IX)  TO  MOD-TEMFSFEL                      
073400           END-IF                                                         
073500         ELSE                                                             
073600           MOVE MFS-NUM-FAELT-FEL TO MOD-TISTOMREG-SORT-ATTR              
073700           MOVE NEJ               TO INDATA-SW                            
073800           MOVE FEL-1 (SPRAK-IX)  TO  MOD-TEMFSFEL                        
073900         END-IF                                                           
074000       ELSE                                                               
074100         MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTOMREG-SORT-ATTR              
074200       END-IF                                                             
074300                                                                          
074400       IF MID-IDARTNR-SORT NOT = ALL '+'                                  
074500         IF MID-IDARTNR-SORT NUMERIC                                      
074600           IF MID-IDARTNR-SORT = 1 OR 2 OR 3 OR 4                         
074700             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-SORT-ATTR            
074800             MOVE MID-IDARTNR-SORT    TO SEQIX                            
074900             IF WS-SEQUENCE(SEQIX) NOT = ZERO                             
075000               MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-SORT-ATTR            
075100               MOVE NEJ               TO INDATA-SW                        
075200               MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                      
075300             ELSE                                                         
075400               MOVE 2                 TO WS-SEQUENCE(SEQIX)               
075500               MOVE SEQ-TEXT(2)       TO WS-SORT-SEQUENCE(SEQIX)          
075600             END-IF                                                       
075700           ELSE                                                           
075800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-SORT-ATTR              
075900             MOVE NEJ               TO INDATA-SW                          
076000             MOVE FEL-1 (SPRAK-IX)  TO MOD-TEMFSFEL                       
076100           END-IF                                                         
076200         ELSE                                                             
076300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-SORT-ATTR                
076400           MOVE NEJ               TO INDATA-SW                            
076500           MOVE FEL-1 (SPRAK-IX)  TO MOD-TEMFSFEL                         
076600         END-IF                                                           
076700       ELSE                                                               
076800         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-SORT-ATTR                
076900       END-IF                                                             
077000                                                                          
077100       IF MID-KDPRODSL-SORT NOT = ALL '+'                                 
077300         IF MID-KDPRODSL-SORT NUMERIC                                     
077400           IF MID-KDPRODSL-SORT = 1 OR 2 OR 3 OR 4                        
077500             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-SORT-ATTR           
077600             MOVE MID-KDPRODSL-SORT TO SEQIX                              
077700             IF WS-SEQUENCE(SEQIX) NOT = ZERO                             
077800               MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-SORT-ATTR           
077900               MOVE NEJ               TO INDATA-SW                        
078000               MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                      
078100             ELSE                                                         
078200               MOVE 4                 TO WS-SEQUENCE(SEQIX)               
078300               MOVE SEQ-TEXT(4)       TO WS-SORT-SEQUENCE(SEQIX)          
078400             END-IF                                                       
078500           ELSE                                                           
078600             MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-SORT-ATTR             
078700             MOVE NEJ               TO INDATA-SW                          
078800             MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                        
078900           END-IF                                                         
079000         ELSE                                                             
079100           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-SORT-ATTR               
079200           MOVE NEJ               TO INDATA-SW                            
079300           MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                          
079400         END-IF                                                           
080000       ELSE                                                               
080100         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-SORT-ATTR               
080200       END-IF                                                             
080300     END-EVALUATE                                                         
080400                                                                          
080500     IF INDATA-OK                                                         
080600       MOVE KONTROLLTABELL TO WS-SORTFIELD-KEY                            
080700       IF GODK-SORTFIELD                                                  
080800         CONTINUE                                                         
080900       ELSE                                                               
081000         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-SORT-ATTR                 
081100                                   MOD-IDARTNR-SORT-ATTR                  
081200                                   MOD-TISTOMREG-SORT-ATTR                
081300                                   MOD-KDPRODSL-SORT-ATTR                 
081400         MOVE NEJ             TO INDATA-SW                                
081500         MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                            
081600       END-IF                                                             
081700     END-IF                                                               
081800     .                                                                    
081900     EJECT                                                                
082000 BD-KOLLA-MOT-BASEN     SECTION.                                          
082100***************************************                                   
082200*                                                                         
082300***************************************                                   
082400     MOVE WS-IDPROJ   TO W-1123-IDPROJ                                    
082500     MOVE +11         TO W-1123-KDPRODSL                                  
082600     PERFORM IMS-GET-PROJ-ROT                                             
082700     IF SEGMENT-FINNS                                                     
082800        PERFORM IMS-GET-1124                                              
082900        IF 1124-TIBLREG = ZERO                                            
083000           MOVE NEJ TO INDATA-SW                                          
083100           MOVE FEL-8 (SPRAK-IX) TO MOD-TEMFSFEL                          
083200        ELSE                                                              
083300           MOVE WS-KDBASLM TO W-1126-KDBASLM                              
083400           PERFORM IMS-GET-1126-UNIK-GHNP                                 
083500           IF SEGMENT-FINNS                                               
083600              IF WS-IDDISTR = ZERO                                        
083700                 CONTINUE                                                 
083800              ELSE                                                        
083900                 MOVE +1 TO DIST-INDX                                     
084000                 PERFORM UNTIL DIST-INDX > 6                              
084100                    IF 1126-IDDISTR(DIST-INDX) =                          
084200                                         WS-IDDISTR                       
084300                       MOVE +999999999 TO DIST-INDX                       
084400                    ELSE                                                  
084500                       ADD +1 TO DIST-INDX                                
084600                    END-IF                                                
084700                 END-PERFORM                                              
084800                 IF DIST-INDX = 7                                         
084900                    MOVE NEJ TO INDATA-SW                                 
085000                    MOVE MFS-NUM-FAELT-FEL TO                             
085100                                        MOD-IDDISTR-ATTR                  
085200                    MOVE FEL-4 (SPRAK-IX) TO                              
085300                                        MOD-TEMFSFEL                      
085400                 END-IF                                                   
085500              END-IF                                                      
085600           ELSE                                                           
085700              MOVE NEJ TO INDATA-SW                                       
085800              MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                       
085900           END-IF                                                         
086000        END-IF                                                            
086100     ELSE                                                                 
086200        MOVE NEJ TO INDATA-SW                                             
086300        MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                             
086400     END-IF                                                               
086500     .                                                                    
086600     EJECT                                                                
086700 C-UPPDATERA           SECTION.                                           
086800*************************************************                         
086900*                                                                         
087000*                                                                         
087100*************************************************                         
087200                                                                          
087201* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
087202*    DETTA GÖR ATT MAN KAN SE I SYSOUTEN VEM SOM STARTAT JOBBET           
087203* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
087204     MOVE +0108                 TO W-6021-IDJCLRAD                        
087210     PERFORM IMS-GET-6021-DATA                                            
087220                                                                          
087230     MOVE +0108                 TO WLJCLD-JCL-IDJCLRAD                    
087240     MOVE  MSG-SIGNON-USERID    TO WS-IDUSER-USER                         
087242     MOVE  WS-IDUSER-AREA       TO WLJCLD-JCL-TEJCL                       
087243                                                                          
087250     IF SEGMENT-FINNS                                                     
087260        PERFORM IMS-REPLACE-6021-DATA                                     
087270     ELSE                                                                 
087280        PERFORM IMS-INSERT-6021-DATA                                      
087290     END-IF                                                               
087291* - - - - - - - - - - - - - - - - - - - - - - - -                         
087292*    LÄGGER UPP DATARAD FÖR KÖRNINGEN                                     
087293* - - - - - - - - - - - - - - - - - - - - - - - -                         
087300     MOVE +1000                 TO W-6021-IDJCLRAD                        
087400                                                                          
087500     MOVE '001'                 TO W11505-001-IDPTYP                      
087600     MOVE WS-KDBASLM            TO W11505-001-KDBASLM                     
087700     MOVE WS-IDPROJ             TO W11505-001-IDPROJ                      
087800     MOVE WS-KDPRODSL           TO W11505-001-KDPRODSL                    
087900     MOVE WS-IDSKYLT            TO W11505-001-IDSKYLT                     
088000     MOVE WS-IDDISTR            TO W11505-001-IDDISTR                     
088100     MOVE WS-MARK-NOT           TO W11505-001-MARK-NOT                    
088200     MOVE WS-MARK-QTY           TO W11505-001-MARK-QTY                    
088300     MOVE WS-KDDEALER           TO W11505-001-KDDEALER                    
088400     MOVE WS-KDBPSR-1           TO W11505-001-KDBPSR-1                    
088500     MOVE WS-KDBPSR-2           TO W11505-001-KDBPSR-2                    
088600     MOVE WS-IDFKNGRP-1         TO W11505-001-IDFKNGRP-1                  
088700     MOVE WS-IDFKNGRP-2         TO W11505-001-IDFKNGRP-2                  
088800     MOVE WS-SORT-SEQUENCE(1)   TO W11505-001-SEQUENCE(1)                 
088900     MOVE WS-SORT-SEQUENCE(2)   TO W11505-001-SEQUENCE(2)                 
089000     MOVE WS-SORT-SEQUENCE(3)   TO W11505-001-SEQUENCE(3)                 
089100     MOVE WS-SORT-SEQUENCE(4)   TO W11505-001-SEQUENCE(4)                 
089200                                                                          
089300     PERFORM IMS-GET-6021-DATA                                            
089400                                                                          
089500     MOVE +1000                 TO WLJCLD-JCL-IDJCLRAD                    
089600     MOVE W11505-001-AREA       TO WLJCLD-JCL-TEJCL                       
089700     IF SEGMENT-FINNS                                                     
089800        PERFORM IMS-REPLACE-6021-DATA                                     
089900     ELSE                                                                 
090000        PERFORM IMS-INSERT-6021-DATA                                      
090100     END-IF                                                               
090200     MOVE 'W115S1  '          TO MOD-MID-IDRUTIN-IN                       
090300     EVALUATE MSG-SIGNON-USERID                                           
090310        WHEN 'PC07780 '                                                   
090320*             LARS MAGNUSSON, AVD 57250                                   
090330              MOVE 'RSBSL1'    TO MOD-MID-IDJOB-IN                        
090370        WHEN 'PC31775 '                                                   
090380*             MIKAEL SKJÖLD , AVD 57250                                   
090390              MOVE 'RSBSL1'    TO MOD-MID-IDJOB-IN                        
090391        WHEN 'PC54401 '                                                   
090392*             ANDERS LARSSON, AVD 57250                                   
090393              MOVE 'RSBSL1'    TO MOD-MID-IDJOB-IN                        
090394        WHEN 'PC55338 '                                                   
090395*             SARA BERGGREN,  AVD 57250                                   
090396              MOVE 'RSBSL1'    TO MOD-MID-IDJOB-IN                        
090397        WHEN 'PC55451 '                                                   
090398*             CARINA JANSENIUS AVD 57250                                  
090399              MOVE 'RSBSL1'    TO MOD-MID-IDJOB-IN                        
090400        WHEN 'PC01715 '                                                   
090500*             GUNNAR MAGNUSSON                                            
090600              MOVE 'RSBSL '    TO MOD-MID-IDJOB-IN                        
091300        WHEN 'PC59625 '                                                   
091400*             BODIL LINDAHL SYST.UTV.VCAS                                 
091500              MOVE 'CONNY'    TO MOD-MID-IDJOB-IN                         
091600        WHEN 'PC28439 '                                                   
091700*             CONNY EGHOLT  SYST.UTV.VCAS                                 
091800              MOVE 'CONNY'     TO MOD-MID-IDJOB-IN                        
092100        WHEN OTHER                                                        
092200              MOVE WS-KDBASLM  TO MOD-MID-IDJOB-IN                        
092300     END-EVALUATE                                                         
092400                                                                          
092500     MOVE MIN-MOD-LAENGD      TO MSG-KVLL                                 
092600     MOVE LOW-VALUE           TO MSG-KDZ1                                 
092700                                 MSG-KDZ2                                 
092800     MOVE 'W0T709U '          TO MSG-KDTRANS-1                            
092900     MOVE '0791'              TO MSG-IDTRANS-1                            
093000     MOVE MFS-KDMFSFOR        TO MSG-KDMFSFOR-1                           
093100     MOVE MOD-MID-W0I70901    TO MSG-INDATA-MINUS-1-TRANSKOD              
093200                                                                          
093300     PERFORM IMS-INSERT-ALT-MSG                                           
093400     .                                                                    
093500     EJECT                                                                
093600 S1-ROER-EJ-FAELT-VISA SECTION.                                           
093700*****************************************                                 
093800* GÖR SÅ ATT VISNINGSFÄLTEN BLIR ORÖRDA.                                  
093900*                                                                         
094000*****************************************                                 
094100     MOVE MFS-ROER-EJ-FAELT TO  MOD-KDBASLM                               
094200                                MOD-KDPRODSL                              
094300                                MOD-IDPROJ                                
094400                                MOD-IDSKYLT                               
094500                                MOD-IDDISTR                               
094600                                MOD-MARK-NOT                              
094700                                MOD-MARK-QTY                              
094800                                MOD-KDDEALER                              
094900                                MOD-KDBPSR-1                              
095000                                MOD-KDBPSR-2                              
095100                                MOD-IDFKNGRP-1                            
095200                                MOD-IDFKNGRP-2                            
095300                                MOD-IDFKNGRP-SORT                         
095400                                MOD-TISTOMREG-SORT                        
095500                                MOD-IDARTNR-SORT                          
095600                                MOD-KDPRODSL-SORT                         
095700     .                                                                    
095800     EJECT                                                                
095900 S3-RENSA-FAELT-MOD SECTION.                                              
096000                                                                          
096100*****************************************                                 
096200* RENSAR IN-MATNINGSFÄLTEN.                                               
096300*                                                                         
096400*****************************************                                 
096500     MOVE MFS-RENSA-FAELT TO MOD-KDBASLM                                  
096600                             MOD-KDPRODSL                                 
096700                             MOD-IDPROJ                                   
096800                             MOD-IDSKYLT                                  
096900                             MOD-IDDISTR                                  
097000                             MOD-MARK-NOT                                 
097100                             MOD-MARK-QTY                                 
097200                             MOD-KDDEALER                                 
097300                             MOD-KDBPSR-1                                 
097400                             MOD-KDBPSR-2                                 
097500                             MOD-IDFKNGRP-1                               
097600                             MOD-IDFKNGRP-2                               
097700                             MOD-IDFKNGRP-SORT                            
097800                             MOD-TISTOMREG-SORT                           
097900                             MOD-IDARTNR-SORT                             
098000                             MOD-KDPRODSL-SORT                            
098100                                                                          
098200     .                                                                    
098300     EJECT                                                                
098400* IMS SEKTIONER                                                           
098500     SKIP3                                                                
098600 IMS-GET-MSG SECTION.                                                     
098700                                                                          
098800     MOVE '  QC' TO GODK-STATUSKODER                                      
098900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
099000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
099100     PERFORM IMS-STATUSKONTROLL                                           
099200     .                                                                    
099300     SKIP3                                                                
099400 IMS-INSERT-MSG SECTION.                                                  
099500                                                                          
099600     IF ENGLISH-TEXT                                                      
099700       MOVE 'N' TO MFS-KDHUVOMR                                           
099800     END-IF                                                               
099900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
100000     MOVE SPACE TO GODK-STATUSKODER                                       
100100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
100200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100300     PERFORM IMS-STATUSKONTROLL                                           
100400     .                                                                    
100500     EJECT                                                                
100600 IMS-INSERT-ALT-MSG SECTION.                                              
100700                                                                          
100800     IF ENGLISH-TEXT                                                      
100900       MOVE 'N' TO MFS-KDHUVOMR                                           
101000     END-IF                                                               
101100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
101200     MOVE SPACE TO GODK-STATUSKODER                                       
101300     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
101400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
101500     PERFORM IMS-STATUSKONTROLL                                           
101600     .                                                                    
101700     EJECT                                                                
101800 IMS-GET-PROJ-ROT  SECTION.                                               
101900                                                                          
102000     STRING 'WLXXAP01(WDGXKEY  =' W-1123-KEY-X ')'                        
102100            DELIMITED BY SIZE INTO SSA1                                   
102200     MOVE '  GE' TO GODK-STATUSKODER                                      
102300     CALL CBLTDLI USING GHU XXAP-PCB DLI-IO-AREA1 SSA1                    
102400     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
102500     PERFORM IMS-STATUSKONTROLL                                           
102600     .                                                                    
102700     SKIP3                                                                
102800 IMS-GET-1124  SECTION.                                                   
102900                                                                          
103000     MOVE 'WLXXAP11 ' TO SSA1                                             
103100     MOVE '  ' TO GODK-STATUSKODER                                        
103200     CALL CBLTDLI USING GHNP XXAP-PCB DLI-IO-AREA1 SSA1                   
103300     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
103400     PERFORM IMS-STATUSKONTROLL                                           
103500     .                                                                    
103600     SKIP3                                                                
103700 IMS-GET-1126-UNIK-GHNP SECTION.                                          
103800                                                                          
103900     STRING 'WLXXAP12(WDGXKEY  =' W-1126-KEY-X ')'                        
104000            DELIMITED BY SIZE INTO SSA1                                   
104100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
104200     CALL CBLTDLI USING GHNP XXAP-PCB DLI-IO-AREA1 SSA1                   
104300     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
104400     PERFORM IMS-STATUSKONTROLL                                           
104500     .                                                                    
104600     SKIP3                                                                
104700 IMS-GET-6021-ROT       SECTION.                                          
104800                                                                          
104900     STRING 'WLJCLD01(WDP101KY =' W-6021-WDP101KY-X ')'                   
105000            DELIMITED BY SIZE INTO SSA1                                   
105100     MOVE '  GE' TO GODK-STATUSKODER                                      
105200     CALL CBLTDLI USING GU WLJCLD-PCB DLI-IO-AREA1 SSA1                   
105300     MOVE WLJCLD-STATUS-CODE TO STATUS-WS                                 
105400     PERFORM IMS-STATUSKONTROLL                                           
105500     .                                                                    
105600     SKIP3                                                                
105700 IMS-GET-6021-SECURITY  SECTION.                                          
105800                                                                          
105900     STRING 'WLJCLD01(WDP101KY =' W-6021-WDP101KY-X ')'                   
106000            DELIMITED BY SIZE INTO SSA1                                   
106100     STRING 'WLJCLD11(IDUSER   =' W-6021-IDUSER-X ')'                     
106200            DELIMITED BY SIZE INTO SSA2                                   
106300     MOVE '  GE' TO GODK-STATUSKODER                                      
106400     CALL CBLTDLI USING GU WLJCLD-PCB DLI-IO-AREA1 SSA1 SSA2              
106500     MOVE WLJCLD-STATUS-CODE TO STATUS-WS                                 
106600     PERFORM IMS-STATUSKONTROLL                                           
106700     .                                                                    
106800     SKIP3                                                                
106900 IMS-GET-6021-DATA      SECTION.                                          
107000                                                                          
107100     STRING 'WLJCLD01(WDP101KY =' W-6021-WDP101KY-X ')'                   
107200            DELIMITED BY SIZE INTO SSA1                                   
107300     STRING 'WLJCLD12(IDJCLRAD =' W-6021-IDJCLRAD-X ')'                   
107400            DELIMITED BY SIZE INTO SSA2                                   
107500     MOVE '  GE' TO GODK-STATUSKODER                                      
107600     CALL CBLTDLI USING GHU WLJCLD-PCB DLI-IO-AREA1 SSA1 SSA2             
107700     MOVE WLJCLD-STATUS-CODE TO STATUS-WS                                 
107800     PERFORM IMS-STATUSKONTROLL                                           
107900     .                                                                    
108000     SKIP3                                                                
108100 IMS-INSERT-6021-DATA   SECTION.                                          
108200                                                                          
108300     STRING 'WLJCLD01(WDP101KY =' W-6021-WDP101KY-X ')'                   
108400              DELIMITED BY SIZE INTO SSA1                                 
108500     MOVE   'WLJCLD12 ' TO SSA2                                           
108600     MOVE '  ' TO GODK-STATUSKODER                                        
108700     CALL CBLTDLI USING ISRT WLJCLD-PCB DLI-IO-AREA1 SSA1 SSA2            
108800     MOVE WLJCLD-STATUS-CODE TO STATUS-WS                                 
108900     PERFORM IMS-STATUSKONTROLL                                           
109000     .                                                                    
109100     SKIP3                                                                
109200 IMS-REPLACE-6021-DATA  SECTION.                                          
109300                                                                          
109400     MOVE '  ' TO GODK-STATUSKODER                                        
109500     CALL CBLTDLI USING REPL WLJCLD-PCB DLI-IO-AREA1                      
109600     MOVE WLJCLD-STATUS-CODE TO STATUS-WS                                 
109700     PERFORM IMS-STATUSKONTROLL                                           
109800     .                                                                    
109900     SKIP3                                                                
110000 IMS-STATUSKONTROLL SECTION.                                              
110100                                                                          
110200     SET STATUS-IX TO 1                                                   
110300     SEARCH GODK-STATUS                                                   
110400       AT END                                                             
110500         CALL FELLOG                                                      
110600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
110700         CONTINUE                                                         
110800     END-SEARCH                                                           
110900     .                                                                    
