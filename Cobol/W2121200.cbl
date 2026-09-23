000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2121200.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   13/01/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        TAR EMOT FIL FRÅN EPIC (KINA, USA)                               
000900*        SPARAR POSTER (BUY,BPA) VARS INF TID EJ UPPNÅDD                  
001000*        SÄNDER POSTER TILL W09278(985)                                   
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK7                                       
001300*        PROGRAMMET LÄSER      WDB6                                       
001400*        PROGRAMMET LÄSER      WDF1                                       
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*          --- FIL FRÅN EPIC SI+                                          
002800     SELECT EPICCN                     ASSIGN TO W21212D1.                
002900                                                                          
003000*          ---  SPARADE POSTER                                            
003100*          --- (KONKATENERAS MED EPIC-FILEN)                              
003200*    SELECT W21212I                    ASSIGN TO W21212D1.                
003300                                                                          
003400*          --- SPARADE POSTER UT                                          
003500     SELECT W21212U                    ASSIGN TO W21212D2.                
003600                                                                          
003700*          --- OK POSTER                                                  
003800     SELECT W21213                     ASSIGN TO W21212D3.                
003900                                                                          
004000*          --- PRISER TILL EKONOMI                                        
004100     SELECT W21214                     ASSIGN TO W21212D4.                
004200                                                                          
004300*          --- FELFIL                                                     
004400     SELECT W21215                     ASSIGN TO W21212D5.                
004500                                                                          
004600                                                                          
004700 DATA DIVISION.                                                           
004800 FILE SECTION.                                                            
004900                                                                          
005000 FD  EPICCN                                                               
005100     RECORDING       V                                                    
005200     BLOCK CONTAINS  0.                                                   
005300 01   FILLER            PIC X(82).                                        
005400                                                                          
005500*01  -COPY IS061BPA      -L.                                              
005600                                                                          
005700*01  -COPY IS061BUY      -L.                                              
005800                                                                          
005900*01  -COPY IS061OOP      -L.                                              
006000                                                                          
006100*01  -COPY IS061POB      -L.                                              
006200                                                                          
006300*01  -COPY IS061SSK      -L.                                              
006400                                                                          
006500 FD  W21212U                                                              
006600     RECORDING       V                                                    
006700     BLOCK CONTAINS  0.                                                   
006800*01  POST -COPY IS061BPA -PRE  NEWBPA-  -L.                               
006900*01  POST -COPY IS061BUY -PRE  NEWBUY-  -L.                               
007000                                                                          
007100 FD  W21213                                                               
007200     RECORDING       V                                                    
007300     BLOCK CONTAINS  0.                                                   
007400*01  POST -COPY IS061BPA  -PRE OKBPA- -L.                                 
007500*01  POST -COPY IS061BUY  -PRE OKBUY- -L.                                 
007600                                                                          
007700 FD  W21214                                                               
007800     RECORDING       F                                                    
007900     BLOCK CONTAINS  0.                                                   
008000*01  POST -COPY W55372 -PRE  EKO-  -L.                                    
008100                                                                          
008200 FD  W21215                                                               
008300     RECORDING       F                                                    
008400     BLOCK CONTAINS  0.                                                   
008500*01  ERR-POST -COPY W2121501   -L.                                        
008600                                                                          
008700                                                                          
008800 WORKING-STORAGE SECTION.                                                 
008900                                                                          
009000 77  IDPGM                       PIC X(8)    VALUE 'W2121200'.            
009100 77  JA                          PIC X       VALUE 'J'.                   
009200 77  NEJ                         PIC X       VALUE 'N'.                   
009300 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
009400 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
009500                                                                          
009600 77  EPICCN-EOF-SW               PIC X       VALUE 'N'.                   
009700     88  END-OF-EPICCN                       VALUE 'J'.                   
009800                                                                          
009900                                                                          
010000 77  EPIC-SW                     PIC X       VALUE 'N'.                   
010100     88  EPIC-OK                             VALUE 'J'.                   
010200     88  EPIC-FEL                            VALUE 'N'.                   
010300                                                                          
010400 01  W-IDLEVNR                   PIC X(5)    VALUE SPACE.                 
010500 01  W-IDLEVNR-SHIP              PIC X(5)    VALUE SPACE.                 
010600 01  W-BESTNR12                  PIC 9(12).                               
010700 01  FILLER  REDEFINES W-BESTNR12.                                        
010800     03  W-BESTPREF              PIC 9(3).                                
010900     03  W-BESTLNR               PIC 9(6).                                
011000     03  W-BESTSUFF              PIC 9(3).                                
011100 01  ERROR-TEXT.                                                          
011200     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
011300     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
011400     EJECT                                                                
011500 01  W-DATUM8                    PIC 9(8).                                
011600 01  FILLER  REDEFINES W-DATUM8.                                          
011700     03  FILLER                  PIC 9(2).                                
011800     03  W-DATUM6                PIC 9(6).                                
011900 01  RKOD                        PIC S9(4)  VALUE +0  COMP SYNC.          
012000                                                                          
012100                                                                          
012200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012300 01  FILLER REDEFINES DAGENS-DATUM.                                       
012400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012700 01  NEXTDAY-AAAAMMDD.                                                    
012800     03 FILLER                   PIC 9(2)    VALUE 20.                    
012900     03 NEXTDAY-DATUM            PIC 9(6)    VALUE ZERO.                  
013000                                                                          
013100 01  FILLER              PIC X(16)   VALUE 'IDDC-TABELL'.                 
013200*- - - - - - - - - - - - - TABELL MED ALLA IDDC PÅ WDB601                 
013300*- - - - - - - - - - - - - DC-MAX OCCURS SÄTTS TILL VERKLIGT ANTAL        
013400*- - - - - - - - - - - - - I M- SEKTIONEN.                                
013500 01  IDDC-INDEX-WS.                                                       
013600     03 DC-MAX           PIC S9(3)   VALUE +100 COMP SYNC.                
013700                                                                          
013800     03 WDCIX            PIC S9(3)   VALUE +0  COMP SYNC.                 
013900     03 DCS-TRAEFF       PIC X       VALUE 'J'.                           
014000                                                                          
014100 01  IDDC-TABELL.                                                         
014200     03 DC-TAB  OCCURS 1 TO 100 DEPENDING ON DC-MAX                       
014300                INDEXED BY DCIX.                                          
014400        05 T-DCS.                                                         
014500          07 T-DCS-IDDC           PIC X(2).                               
014600          07 T-DCS-KDDC           PIC X(2).                               
014700          07 T-DCS-IDLEVNR-DC     PIC X(5).                               
014800          07 T-DCS-IDLEVNR-EMB    PIC X(5).                               
014900                                                                          
015000                                                                          
015100 01  FILLER              PIC X(16)   VALUE 'DC-TABELL-SORT'.              
015200*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
015300 01  TABENTRY-PARM.                                                       
015400     03  STEGLANGD               PIC S9(9) COMP.                          
015500     03  ANTAL                   PIC S9(9) COMP.                          
015600     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
015700                                                                          
015800                                                                          
015900 01  DYNAMISKA-SUBPROGRAM.                                                
016000*                                                                         
016100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
016200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
016600     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
016700                                                                          
016800*    --- PARAMETRAR TILL ABEND                                            
016900                                                                          
017000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
017100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017300                                                                          
017400 01  FELTEXT.                                                             
017500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
017600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
017700                                                                          
017800                                                                          
017900*    ---   PARAMETRAR TILL POSTSUM                                        
018000*01  -COPY W0005   -PRE  POSTSUM-                                         
018100                                                                          
018200*    ---   PARAMETRAR TILL WORKDAY                                        
018300*01  -COPY WORKAREA                                                       
018400                                                                          
018500                                                                          
018600 01  EPIC-AREA-START             PIC X(24)   VALUE                        
018700                                 'EPIC-AREA-START  '.                     
018800 01  EPIC-AREA.                                                           
018900     03  EPIC-AREA-0.                                                     
019000       05  EPIC-IDPTYP           PIC X(3).                                
019100       05  EPIC-PLANT            PIC X(5).                                
019200       05  FILLER                PIC X(75).                               
019300*   03  FILLER -COPY IS061BUY  -PRE BUY-  -RED  EPIC-AREA-0               
019400*   03  FILLER -COPY IS061BPA  -PRE BPA-  -RED  EPIC-AREA-0               
019500*   03  FILLER -COPY IS061OOP  -PRE OOP-  -RED  EPIC-AREA-0               
019600                                                                          
019700                                                                          
019800 01  NEW-AREA-BPA                PIC X(24)   VALUE                        
019900                                 'NEW-AREA-BPA   '.                       
020000*01  AREA   -COPY IS061BPA  -PRE NEWBPA-                                  
020100                                                                          
020200 01  NEW-AREA-BUY                PIC X(24)   VALUE                        
020300                                 'NEW-AREA-BUY   '.                       
020400*01  AREA   -COPY IS061BUY  -PRE NEWBUY-                                  
020500                                                                          
020600                                                                          
020700 01  OK-AREA-BPA                 PIC X(24)   VALUE                        
020800                                 'OK-AREA-BPA    '.                       
020900*01  AREA -COPY IS061BPA  -PRE OKBPA-                                     
021000                                                                          
021100                                                                          
021200 01  OK-AREA-BUY                 PIC X(24)   VALUE                        
021300                                 'OK-AREA-BUY    '.                       
021400*01  AREA -COPY IS061BUY  -PRE OKBUY-                                     
021500                                                                          
021600                                                                          
021700 01  EKO-AREA-985                PIC X(24)   VALUE                        
021800                                 'EKO-AREA-985    '.                      
021900*01  AREA -COPY W55372    -PRE EKO985-                                    
022000                                                                          
022100                                                                          
022200 01  FEL-AREA-START              PIC X(24)   VALUE                        
022300                                 'FEL-AREA-START  '.                      
022400*01  ERR-AREA -COPY W2121501                                              
022500                                                                          
022600                                                                          
022700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022800*                                                                         
022900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023000 01  NYCKLAR-TILL-DLI.                                                    
023100     03  W-IDARTNR-X.                                                     
023200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
023300     03  W-IDDC-X.                                                        
023400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
023500     03  W-IDLEVNR-F1-X.                                                  
023600         05  W-IDLEVNR-F1        PIC X(5)    VALUE SPACE.                 
023700     03  W-IDLEVNRDC-X.                                                   
023800         05  W-IDLEVNRDC         PIC X(5)    VALUE SPACE.                 
023900                                                                          
024000*    --- STATUS-KOD FRÅN IMS                                              
024100 01  STATUS-WS                   PIC XX.                                  
024200     88  SEGMENT-FINNS                       VALUE '  '.                  
024300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
024500                                                                          
024600 01  GODK-STATUSKODER.                                                    
024700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024800                                                                          
024900 01  ALL-SSA.                                                             
025000     03 SSA1                     PIC X(64).                               
025100     03 SSA2                     PIC X(64).                               
025200     03 SSA3                     PIC X(64).                               
025300                                                                          
025400                                                                          
025500*    --- IMS FUNKTIONSKODER                                               
025600*01  -COPY W0003                                                          
025700                                                                          
025800*    ---  DLI INPUT-OUTPUT AREA                                           
025900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
026000 01  DLI-IO-WDK711.                                                       
026100*    03  -COPY WDK711                                                     
026200                                                                          
026300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
026400 01  DLI-IO-WDF101.                                                       
026500*    03  -COPY WDF101                                                     
026600                                                                          
026700                                                                          
026800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
026900 01  DLI-IO-WDB601.                                                       
027000*    03  -COPY WDB601                                                     
027100                                                                          
027200                                                                          
027300 LINKAGE SECTION.                                                         
027400                                                                          
027500*01  -COPY W0008  -PRE WDK7-                                              
027600     05  FILLER                  PIC X.                                   
027700                                                                          
027800*01  -COPY W0008  -PRE WDF1-                                              
027900     05  FILLER                  PIC X.                                   
028000                                                                          
028100*01  -COPY W0008  -PRE WDB6-                                              
028200     05  FILLER                  PIC X.                                   
028300                                                                          
028400                                                                          
028500 PROCEDURE DIVISION  USING WDK7-PCB WDF1-PCB WDB6-PCB.                    
028600 MAIN SECTION.                                                            
028700     ENTRY 'DLITCBL' USING WDK7-PCB WDF1-PCB WDB6-PCB.                    
028800                                                                          
028900                                                                          
029000     PERFORM A-INIT                                                       
029100                                                                          
029200     PERFORM S01-LAES-EPICCN                                              
029300     PERFORM UNTIL END-OF-EPICCN                                          
029400        IF EPIC-IDPTYP = 'BPA' OR 'BUY' OR 'OOP'                          
029500           PERFORM B-KOLLA-EPIC-POST                                      
029600           IF EPIC-OK                                                     
029700              PERFORM C-BERAKNA-NASTA-ARBETSDAG                           
029800                                                                          
029900              EVALUATE EPIC-IDPTYP                                        
030000              WHEN 'BPA'                                                  
030100               PERFORM D-HANTERA-BPA                                      
030200                                                                          
030300              WHEN 'BUY'                                                  
030400               PERFORM E-HANTERA-BUY                                      
030500                                                                          
030600              WHEN 'OOP'                                                  
030700               PERFORM F-HANTERA-OOP                                      
030800              END-EVALUATE                                                
030900           ELSE                                                           
031000              PERFORM S100-SKRIV-FELFIL                                   
031100           END-IF                                                         
031200        END-IF                                                            
031300                                                                          
031400        PERFORM S01-LAES-EPICCN                                           
031500     END-PERFORM                                                          
031600                                                                          
031700     PERFORM Z-FINIT                                                      
031800                                                                          
031900     MOVE ZERO TO RETURN-CODE                                             
032000     GOBACK                                                               
032100     .                                                                    
032200                                                                          
032300                                                                          
032400 A-INIT SECTION.                                                          
032500     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
032600                                                                          
032700     OPEN INPUT  EPICCN                                                   
032800                                                                          
032900     OPEN OUTPUT W21212U                                                  
033000                 W21213                                                   
033100                 W21214                                                   
033200                 W21215                                                   
033300                                                                          
033400     ACCEPT DAGENS-DATUM  FROM DATE                                       
033500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
033600     MOVE DAGENS-DATUM  TO NEXTDAY-DATUM                                  
033700     ADD  +1            TO NEXTDAY-DATUM                                  
033800                                                                          
033900     PERFORM AA-SKAPA-DCTABELL                                            
034000     .                                                                    
034100                                                                          
034200                                                                          
034300 AA-SKAPA-DCTABELL SECTION.                                               
034400     MOVE 'AA-SKAPA-DCTABELL'  TO CURRENT-SECTION.                        
034500                                                                          
034600     SET DCIX TO +1                                                       
034700     PERFORM IMS-GN-WDB601                                                
034800                                                                          
034900     PERFORM UNTIL SEGMENT-SLUT                                           
035000       IF DCIX <= DC-MAX                                                  
035100         MOVE DCS-IDDC TO T-DCS-IDDC(DCIX)                                
035200         MOVE DCS-KDDC TO T-DCS-KDDC(DCIX)                                
035300         MOVE DCS-IDLEVNR-DC  TO T-DCS-IDLEVNR-DC(DCIX)                   
035400         MOVE DCS-IDLEVNR-EMB TO T-DCS-IDLEVNR-EMB(DCIX)                  
035500*                                                                         
035600         SET DCIX UP BY +1                                                
035700                                                                          
035800         PERFORM IMS-GN-WDB601                                            
035900       ELSE                                                               
036000                                                                          
036100           MOVE 'DC-TABELL SLUT. ÖKA DC-MAX' TO ERROR-TEXT-STR            
036200           DISPLAY ERROR-TEXT                                             
036300           CALL ABEND USING RKOD                                          
036400       END-IF                                                             
036500     END-PERFORM                                                          
036600                                                                          
036700*    --- SÄTTER TAKET PÅ TABELLEN                                         
036800     SET DCIX   DOWN BY +1                                                
036900     SET DC-MAX TO DCIX                                                   
037000                                                                          
037100*    --- SORTERA TABELLEN PÅ IDDC, FÖR ATT SEARCH SKA FUNKA               
037200     MOVE DC-MAX                  TO ANTAL                                
037300     MOVE LENGTH OF T-DCS(1)      TO STEGLANGD                            
037400     MOVE LENGTH OF T-DCS-IDDC(1) TO NYCKELLANGD                          
037500                                                                          
037600     CALL WINTSOR USING IDDC-TABELL  STEGLANGD  ANTAL                     
037700                  T-DCS-IDDC(1) NYCKELLANGD                               
037800     .                                                                    
037900     EJECT                                                                
038000 B-KOLLA-EPIC-POST SECTION.                                               
038100     MOVE 'B-KOLLA-EPIC    ' TO CURRENT-SECTION                           
038200                                                                          
038300     MOVE NEJ                   TO EPIC-SW                                
038400     MOVE SPACE                 TO W-IDLEVNR                              
038500                                   W-IDLEVNR-SHIP                         
038600                                   W-IDDC                                 
038700     MOVE EPIC-PLANT            TO W-IDLEVNRDC                            
038800                                                                          
038900     IF EPIC-IDPTYP = 'BPA'                                               
039000        MOVE BPA-PARTNO         TO W-IDARTNR                              
039100        MOVE BPA-SUPPLIER-ID    TO W-IDLEVNR                              
039200        MOVE BPA-SUPPLIER-SHIP  TO W-IDLEVNR-SHIP                         
039300     ELSE                                                                 
039400        IF EPIC-IDPTYP = 'BUY'                                            
039500           MOVE BUY-PARTNO      TO W-IDARTNR                              
039600        ELSE                                                              
039700           MOVE OOP-PARTNO      TO W-IDARTNR                              
039800           MOVE OOP-SUPPLIER-ID TO W-IDLEVNR                              
039900        END-IF                                                            
040000     END-IF                                                               
040100                                                                          
040200     PERFORM BA-FETCH-IDDC                                                
040400                                                                          
040410     IF W-IDDC = SPACE                                                    
040420       MOVE NEJ             TO EPIC-SW                                    
040430     ELSE                                                                 
040500       PERFORM IMS-GU-WDK711                                              
040600       IF SEGMENT-FINNS                                                   
040700          MOVE JA           TO EPIC-SW                                    
040800       END-IF                                                             
040810     END-IF                                                               
040900                                                                          
041000                                                                          
041100     IF EPIC-OK                                                           
041200        IF EPIC-IDPTYP = 'BPA'                                            
041300           MOVE BPA-SUPPLIER-ID      TO W-IDLEVNR-F1                      
041400                                                                          
041500           PERFORM IMS-GU-WDF101                                          
041600           IF SEGMENT-SAKNAS                                              
041700              MOVE NEJ               TO EPIC-SW                           
041800           ELSE                                                           
041900              MOVE BPA-SUPPLIER-SHIP TO W-IDLEVNR-F1                      
042000                                                                          
042100              PERFORM IMS-GU-WDF101                                       
042200              IF SEGMENT-SAKNAS                                           
042300                 MOVE NEJ            TO EPIC-SW                           
042400              END-IF                                                      
042500           END-IF                                                         
042600                                                                          
042700           IF BPA-ORDERNO NOT NUMERIC                                     
042800              MOVE NEJ               TO EPIC-SW                           
042900           END-IF                                                         
043000        END-IF                                                            
043100        IF EPIC-IDPTYP = 'OOP'                                            
043200           MOVE OOP-SUPPLIER-ID      TO W-IDLEVNR-F1                      
043300                                                                          
043400           PERFORM IMS-GU-WDF101                                          
043500           IF SEGMENT-SAKNAS                                              
043600              MOVE NEJ               TO EPIC-SW                           
043700           END-IF                                                         
043800        END-IF                                                            
043900     END-IF                                                               
044000     .                                                                    
044100                                                                          
044200                                                                          
044300 BA-FETCH-IDDC SECTION.                                                   
044500     MOVE 'BA-FETCH-IDLEVNR-DC' TO CURRENT-SECTION.                       
044510                                                                          
044600     SET DCIX TO +1                                                       
044700     SEARCH DC-TAB                                                        
044800        AT END                                                            
044900           MOVE NEJ  TO DCS-TRAEFF                                        
045000        WHEN (T-DCS-IDLEVNR-DC(DCIX) = EPIC-PLANT)                        
045010              AND (T-DCS-KDDC(DCIX) = 'NC')                               
045100           MOVE JA   TO DCS-TRAEFF                                        
045210        WHEN (T-DCS-IDLEVNR-EMB(DCIX) = EPIC-PLANT)                       
045220             AND (T-DCS-KDDC(DCIX) = 'NA')                                
045230           MOVE JA   TO DCS-TRAEFF                                        
045300     END-SEARCH                                                           
045400                                                                          
045500     IF DCS-TRAEFF = JA                                                   
045600        MOVE T-DCS-IDDC(DCIX) TO W-IDDC                                   
045700     ELSE                                                                 
045800       DISPLAY 'IDLEVNR-DC' EPIC-PLANT ' EJ REG PÅ WDB6'                  
045900     END-IF                                                               
046000     .                                                                    
046100                                                                          
046200                                                                          
048800 C-BERAKNA-NASTA-ARBETSDAG SECTION.                                       
048900     MOVE 'C-NASTA-ARBDAG  ' TO CURRENT-SECTION                           
049000                                                                          
049100     MOVE 002             TO WORK-KDCALL                                  
049200     MOVE W-IDDC          TO WORK-IDDC                                    
049300     MOVE DAGENS-DATUM    TO WORK-TIAAMMDD-FOM                            
049400     MOVE ZERO            TO WORK-TIAAMMDD-TOM                            
049500     MOVE 1               TO WORK-KVWORKD                                 
049600                                                                          
049700     CALL WORKDAY USING WORK-KDCALL                                       
049800                        WORK-DATE-AREA                                    
049900                        WORK-KDSVAR                                       
050000                                                                          
050100     IF WORK-KDSVAR-OK                                                    
050200        MOVE WORK-TIAAMMDD-NEXT-WORKDAY                                   
050300                          TO NEXTDAY-DATUM                                
050400     ELSE                                                                 
050500*    BORDE INTE INTRÄFFA, MEN DAGENS DATUM KANSKE DUGER ???               
050600        MOVE DAGENS-DATUM TO NEXTDAY-DATUM                                
050700     END-IF                                                               
050800     .                                                                    
050900                                                                          
051000                                                                          
051100 D-HANTERA-BPA   SECTION.                                                 
051200     MOVE 'D-HANTERA-BPA  ' TO CURRENT-SECTION                            
051300                                                                          
051400     IF BPA-ORDERDATE-END > '00000000'                                    
051500        IF BPA-ORDERDATE-END <= NEXTDAY-AAAAMMDD                          
051600           MOVE BPA-IS061BPA     TO OKBPA-IS061BPA                        
051700           PERFORM S13-SKRIV-W21213-BPA                                   
051800        ELSE                                                              
051900           MOVE BPA-IS061BPA  TO NEWBPA-AREA                              
052000           PERFORM S12-SKRIV-W21212-BPA                                   
052100        END-IF                                                            
052200     ELSE                                                                 
052300        IF BPA-ORDERDATE-FROM <= NEXTDAY-AAAAMMDD                         
052400           MOVE BPA-IS061BPA     TO OKBPA-AREA                            
052500           PERFORM S13-SKRIV-W21213-BPA                                   
052600        ELSE                                                              
052700           MOVE BPA-IS061BPA  TO NEWBPA-AREA                              
052800           PERFORM S12-SKRIV-W21212-BPA                                   
052900        END-IF                                                            
053000     END-IF                                                               
053100     .                                                                    
053200                                                                          
053300                                                                          
053400 E-HANTERA-BUY   SECTION.                                                 
053500     MOVE 'E-HANTERA-BUY  ' TO CURRENT-SECTION                            
053600                                                                          
053700     IF BUY-DATE-VALID-FR <= NEXTDAY-AAAAMMDD                             
053800        MOVE BUY-IS061BUY     TO OKBUY-AREA                               
053900        PERFORM S13-SKRIV-W21213-BUY                                      
054000     ELSE                                                                 
054100        MOVE BUY-IS061BUY  TO NEWBUY-AREA                                 
054200        PERFORM S12-SKRIV-W21212-BUY                                      
054300     END-IF                                                               
054400     .                                                                    
054500                                                                          
054600                                                                          
054700 F-HANTERA-OOP   SECTION.                                                 
054800     MOVE 'F-HANTERA-OOP  ' TO CURRENT-SECTION                            
054900                                                                          
055000     MOVE SPACE                 TO EKO985-W55372                          
055100     MOVE '985'                 TO EKO985-IDPTYP                          
055200     MOVE W-IDDC                TO EKO985-IDDC                            
055300     MOVE OOP-PARTNO            TO EKO985-IDARTNR                         
055400     MOVE OOP-SUPPLIER-ID       TO EKO985-IDLEVNR                         
055500     MOVE OOP-ORDER-PRICE-LOC   TO EKO985-PRARTBEL                        
055600     MOVE OOP-PRICE-UNIT        TO EKO985-KDANTENH                        
055700     IF OOP-PACK-TYPE-CODE = 'Y' OR 'J' OR 'N' OR '?'                     
055800        MOVE OOP-PACK-TYPE-CODE TO EKO985-KDFPKPRI                        
055900     END-IF                                                               
056000     MOVE OOP-EFFECTIVE-DATE    TO W-DATUM8                               
056100     MOVE W-DATUM6              TO EKO985-TIPRLIST                        
056200     MOVE OOP-LOC-CURR          TO EKO985-KDVALISO                        
056300     MOVE 'INKOP'               TO EKO985-IDUSER                          
056400     PERFORM S14-SKRIV-W21214                                             
056500     .                                                                    
056600                                                                          
056700                                                                          
056800 Z-FINIT SECTION.                                                         
056900     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
057000                                                                          
057100     CLOSE EPICCN                                                         
057200           W21212U                                                        
057300           W21213                                                         
057400           W21214                                                         
057500           W21215                                                         
057600                                                                          
057700     MOVE 'S' TO POSTSUM-OPKOD                                            
057800     CALL POSTSUM USING POSTSUM-PARM                                      
057900     .                                                                    
058000                                                                          
058100                                                                          
058200 S01-LAES-EPICCN  SECTION.                                                
058300     MOVE 'S01-LAES-EPICCN ' TO CURRENT-SECTION                           
058400                                                                          
058500     READ EPICCN INTO EPIC-AREA                                           
058600     AT END                                                               
058700        MOVE HIGH-VALUE TO EPIC-AREA                                      
058800        SET END-OF-EPICCN TO TRUE                                         
058900                                                                          
059000     NOT AT END                                                           
059100        MOVE 'EPICCN'     TO POSTSUM-FDNAMN                               
059200        MOVE 'W21212D1'   TO POSTSUM-DDNAMN2                              
059300        MOVE EPIC-IDPTYP  TO POSTSUM-TRANSTYP                             
059400        CALL POSTSUM USING POSTSUM-PARM                                   
059500     END-READ                                                             
059600     .                                                                    
059700                                                                          
059800                                                                          
059900 S12-SKRIV-W21212-BPA SECTION.                                            
060000     MOVE 'S12--W21212-BPA ' TO CURRENT-SECTION                           
060100                                                                          
060200     WRITE NEWBPA-POST FROM NEWBPA-AREA                                   
060300                                                                          
060400     MOVE EPIC-IDPTYP  TO POSTSUM-TRANSTYP                                
060500     MOVE 'W21212'     TO POSTSUM-FDNAMN                                  
060600     MOVE 'W21212D2'   TO POSTSUM-DDNAMN2                                 
060700     CALL POSTSUM  USING  POSTSUM-PARM                                    
060800     .                                                                    
060900                                                                          
061000                                                                          
061100 S12-SKRIV-W21212-BUY SECTION.                                            
061200     MOVE 'S12-W21212-BUY  ' TO CURRENT-SECTION                           
061300                                                                          
061400     WRITE NEWBUY-POST FROM NEWBUY-AREA                                   
061500                                                                          
061600     MOVE EPIC-IDPTYP  TO POSTSUM-TRANSTYP                                
061700     MOVE 'W21212'     TO POSTSUM-FDNAMN                                  
061800     MOVE 'W21212D2'   TO POSTSUM-DDNAMN2                                 
061900     CALL POSTSUM  USING  POSTSUM-PARM                                    
062000     .                                                                    
062100                                                                          
062200                                                                          
062300 S13-SKRIV-W21213-BPA SECTION.                                            
062400     MOVE 'S13-W21213-BPA  ' TO CURRENT-SECTION                           
062500                                                                          
062600     WRITE OKBPA-POST FROM OKBPA-AREA                                     
062700                                                                          
062800     MOVE 'BPA'      TO POSTSUM-TRANSTYP                                  
062900     MOVE 'W21213'   TO POSTSUM-FDNAMN                                    
063000     MOVE 'W21212D3' TO POSTSUM-DDNAMN2                                   
063100     CALL POSTSUM USING POSTSUM-PARM                                      
063200     .                                                                    
063300                                                                          
063400                                                                          
063500 S13-SKRIV-W21213-BUY SECTION.                                            
063600     MOVE 'S13-W21213-BUY  ' TO CURRENT-SECTION                           
063700                                                                          
063800     WRITE OKBUY-POST FROM OKBUY-AREA                                     
063900                                                                          
064000     MOVE 'BUY'      TO POSTSUM-TRANSTYP                                  
064100     MOVE 'W21213'   TO POSTSUM-FDNAMN                                    
064200     MOVE 'W21212D3' TO POSTSUM-DDNAMN2                                   
064300     CALL POSTSUM USING POSTSUM-PARM                                      
064400     .                                                                    
064500                                                                          
064600                                                                          
064700 S14-SKRIV-W21214 SECTION.                                                
064800     MOVE 'S14-SKRIV-W21214' TO CURRENT-SECTION                           
064900                                                                          
065000     WRITE EKO-POST FROM EKO985-AREA                                      
065100                                                                          
065200     MOVE '985'      TO POSTSUM-TRANSTYP                                  
065300     MOVE 'W21214'   TO POSTSUM-FDNAMN                                    
065400     MOVE 'W21212D4' TO POSTSUM-DDNAMN2                                   
065500     CALL POSTSUM USING POSTSUM-PARM                                      
065600     .                                                                    
065700                                                                          
065800                                                                          
065900 S15-SKRIV-W21215 SECTION.                                                
066000     MOVE 'S15-SKRIV-W21215' TO CURRENT-SECTION                           
066100                                                                          
066200     WRITE ERR-POST FROM ERR-AREA                                         
066300                                                                          
066400     MOVE ERR-IDPTYP TO POSTSUM-TRANSTYP                                  
066500     MOVE 'W21215' TO POSTSUM-FDNAMN                                      
066600     MOVE 'W21212D5' TO POSTSUM-DDNAMN2                                   
066700     CALL POSTSUM USING POSTSUM-PARM                                      
066800     .                                                                    
066900                                                                          
067000 S100-SKRIV-FELFIL SECTION.                                               
067100     MOVE 'S100-SKRIV-FELFIL ' TO CURRENT-SECTION                         
067200                                                                          
067300     MOVE EPIC-IDPTYP        TO ERR-IDPTYP                                
067400     MOVE W-IDLEVNRDC        TO ERR-IDLEVNR-DC                            
067500     MOVE W-IDARTNR          TO ERR-IDARTNR                               
067600     MOVE W-IDDC             TO ERR-IDDC                                  
067700     MOVE W-IDLEVNR          TO ERR-IDLEVNR                               
067800     MOVE W-IDLEVNR-SHIP     TO ERR-IDLEVNR-SHIP                          
067900     IF EPIC-IDPTYP = 'BPA'                                               
068000        MOVE BPA-ORDERNO     TO ERR-IDAVTAL                               
068100     ELSE                                                                 
068200        MOVE SPACE           TO ERR-IDAVTAL                               
068300     END-IF                                                               
068400                                                                          
068500     PERFORM S15-SKRIV-W21215                                             
068600     .                                                                    
068700                                                                          
068800                                                                          
068900* --- IMS SEKTIONER ---                                                   
069000                                                                          
069100 IMS-GU-WDK711 SECTION.                                                   
069200     MOVE 'IMS-GU-WDK711   ' TO CURRENT-IMS-SECTION                       
069300                                                                          
069400     MOVE SPACE               TO ALL-SSA                                  
069500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
069600          DELIMITED BY SIZE INTO SSA1                                     
069700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
069800          DELIMITED BY SIZE INTO SSA2                                     
069900     MOVE '  GE'              TO GODK-STATUSKODER                         
070000     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
070100     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
070200     PERFORM IMS-STATUSKONTROLL                                           
070300     .                                                                    
070400                                                                          
070500                                                                          
070600 IMS-GU-WDF101 SECTION.                                                   
070700     MOVE 'IMS-GU-WDF101   ' TO CURRENT-IMS-SECTION                       
070800                                                                          
070900     MOVE SPACE               TO ALL-SSA                                  
071000     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-F1-X ')'                      
071100          DELIMITED BY SIZE INTO SSA1                                     
071200     MOVE '  GE'              TO GODK-STATUSKODER                         
071300     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
071400     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
071500     PERFORM IMS-STATUSKONTROLL                                           
071600     .                                                                    
071700                                                                          
071800                                                                          
071900 IMS-GN-WDB601    SECTION.                                                
072000     STRING 'WDB601   '                                                   
072100          DELIMITED BY SIZE INTO SSA1                                     
072200     MOVE '  GB' TO GODK-STATUSKODER                                      
072300     CALL CBLTDLI USING GN WDB6-PCB  DLI-IO-WDB601 SSA1                   
072400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
072500     PERFORM IMS-STATUSKONTROLL                                           
072600     .                                                                    
072700     EJECT                                                                
072800                                                                          
072900                                                                          
073000 IMS-STATUSKONTROLL SECTION.                                              
073100                                                                          
073200     SET STATUS-IX TO 1                                                   
073300     SEARCH GODK-STATUS                                                   
073400       AT END                                                             
073500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
073600           DELIMITED BY SIZE INTO FELTEXT                                 
073700         DISPLAY FELTEXT                                                  
073800         CALL FELLOG                                                      
073900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
074000         CONTINUE                                                         
074100     END-SEARCH                                                           
074200     .                                                                    
