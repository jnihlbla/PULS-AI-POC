000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W6118800.                                        
000400 AUTHOR.                 MARGARETHA BACKLUND.                             
000500 DATE-WRITTEN.           JUNI 1981.                                       
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*            PROGRAMMET SELEKTERAR BORT DEFINITIVT ERSATTA                
001100*            ARTIKLAR ,DOCK SPARAS SENASTE INLEVERANS PÅ                  
001200*            GODSMOTTAGANE SAMT ICKE GODSMOTTAGANDE LAGER.                
001300*            FÖR ÖVRIGA ARTIKLAR BORTSELEKTERAS ALLA SEGMENT              
001400*            MED ÅR < MASKINÅR - 1 ,DOCK SKALL MINST SEX                  
001500*            SEGMENT PER ARTIKELNUMMER SPARAS.                            
001600*                                                                         
001700*    ÄNDRAD DEC 1988 AV MÅNS SAMUELSSON.                                  
001800*            FÖR VARJE ARTIKEL SKALL SPARAS INLEVERANSER                  
001900*            AV REDOVISNINGSTYP 0, 3, 9, 10 SÅ ATT DE TÄCKER              
002000*            SALDONA LS EFR AKS OCH RESS PÅ ARTIKELREGISTRET              
002100*                                                                         
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*- - - - - - - - - - - - - - UTFILER:                                     
003000                                                                          
003100*                            - - BORTSORTERADE SEGMENT                    
003200     SELECT  W61188-DEL               ASSIGN  UT-S-W61188D1.              
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W61188-DEL                                                           
003900     RECORDING      F                                                     
004000     BLOCK CONTAINS 0.                                                    
004100                                                                          
004200*01  W61188-UTPOST -COPY W6118801   -L                                    
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600*    -COPY WY2000W8                                                       
004700                                                                          
004800 01  IDPGM                   PIC X(8)    VALUE 'W6118800'.                
004900                                                                          
005000*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
005100 77  JA                      PIC X       VALUE 'J'.                       
005200 77  NEJ                     PIC X       VALUE 'N'.                       
005300 77  DAINLEV-NIOR            PIC 9(16)  VALUE 9999999999999999.           
005400                                                                          
005500*- - - - - - - - - - - - - -  ARBETSFÄLT                                  
005600                                                                          
005700 01  W.                                                                   
005800     03  W-ANTAL             PIC S9(3)       COMP-3.                      
005900     03  W-DAINLEV-KONV      PIC 9(16).                                   
006000     03  FILLER              REDEFINES W-DAINLEV-KONV.                    
006100         05  FILLER          PIC 9(2).                                    
006200         05  W-DAINLEV-DATUM PIC 9(6).                                    
006300         05  FILLER          PIC 9(8).                                    
006400     03  FILLER              REDEFINES W-DAINLEV-KONV.                    
006500         05  FILLER          PIC 9(2).                                    
006600         05  W-DAINLEV-AA    PIC 9(2).                                    
006700         05  W-DAINLEV-MM    PIC 9(2).                                    
006800         05  FILLER          PIC 9(10).                                   
006900     03  FILLER              REDEFINES W-DAINLEV-KONV.                    
007000         05  FILLER          PIC 9(2).                                    
007100         05  W-DAINLEV-AAMM  PIC 9(4).                                    
007200         05  FILLER          PIC 9(10).                                   
007300     EJECT                                                                
007400     03  W-DATUM             PIC 9(6).                                    
007500     03  FILLER              REDEFINES W-DATUM.                           
007600         05  W-DATUM-AA      PIC 9(2).                                    
007700         05  W-DATUM-MM      PIC 9(2).                                    
007800         05  W-DATUM-DD      PIC 9(2).                                    
007900     03  FILLER              REDEFINES W-DATUM.                           
008000         05  W-DATUM-AAMM    PIC 9(4).                                    
008100         05  W-DATUM-DD      PIC 9(2).                                    
008200                                                                          
008300     03  W-DATUM-MINUS-ETT-AR                                             
008400                             PIC 9(4).                                    
008500     03  FILLER              REDEFINES W-DATUM-MINUS-ETT-AR.              
008600         05  W-DATUM-MINUS-ETT-AR-AA                                      
008700                             PIC 9(2).                                    
008800         05  W-DATUM-MINUS-ETT-AR-MM                                      
008900                             PIC 9(2).                                    
009000                                                                          
009100     03  W-DATUM-MINUS-6-MAN                                              
009200                             PIC 9(4).                                    
009300     03  FILLER              REDEFINES W-DATUM-MINUS-6-MAN.               
009400         05  W-DATUM-MINUS-6-MAN-AA                                       
009500                             PIC 9(2).                                    
009600         05  W-DATUM-MINUS-6-MAN-MM                                       
009700                             PIC 9(2).                                    
009800                                                                          
009900     03  W-TIERSDAT          PIC 9(5).                                    
010000                                                                          
010100     EJECT                                                                
010200     03  WDK6-LS             PIC S9(7).                                   
010300     03  W-LS                PIC S9(7).                                   
010400 01  FLAGGOR.                                                             
010500     03  FL-SPARAT-C1-POST   PIC X(1)    VALUE 'N'.                       
010600     03  FL-SPARAT-C2-POST   PIC X(1)    VALUE 'N'.                       
010700     03  FL-ANTAL-OK         PIC X(1)    VALUE 'N'.                       
010800     SKIP3                                                                
010900 01  DYNAMISKA-SUBPROGRAM.                                                
011000     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
011100     03  ABEND               PIC X(8)    VALUE 'ABEND   '.                
011200     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
011300     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
011400     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
011500     SKIP3                                                                
011600*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
011700                                                                          
011800 01  RETURKODER.                                                          
011900   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16  COMP SYNC.        
012000     SKIP2                                                                
012100*- - - - - - - - - - - - - -  ANTALS BERÄKNANDE REDOVISNINGSTYPER         
012200 01  TEST-KDRT              PIC S9(3)     COMP-3.                         
012300     88  KDRT-OK                          VALUE 0 3 9 10.                 
012400     EJECT                                                                
012500*      --- VALID IDDC CODES                                               
012600*                                                                         
012700*01    -COPY WWDC99                                                       
012800     EJECT                                                                
012900*- - - - - - - - - - - - - - UT POSTAREA                                  
013000*                                                                         
013100 01  FILLER                      PIC X(16)    VALUE                       
013200                                              'UT-POST AREA'.             
013300                                                                          
013400*01  AREA    -COPY W6118801 -PRE W-UT-                                    
013500     EJECT                                                                
013600 01  FILLER                  PIC X(08)   VALUE 'DATUM   '.                
013700                                                                          
013800*01          -COPY WDATAREA                                               
013900     EJECT                                                                
014000 01  FILLER                    PIC X(16) VALUE 'POSTSUM'.                 
014100                                                                          
014200*    -COPY W0005    -PRE POSTSUM-                                         
014300                                                                          
014400 01  W61188-TRANSID.                                                      
014500     03  FILLER                PIC X(6)  VALUE 'W61188'.                  
014600     03  FILLER                PIC X(8)  VALUE 'W61188D1'.                
014700     03  W61188-TRANSTYP       PIC X(4)  VALUE '    '.                    
014800     EJECT                                                                
014900                                                                          
015000*01    NYCKLAR-TILL-DLI.                                                  
015100*                                                                         
015200 01  W-IDARTNR-X.                                                         
015300     03  W-IDARTNR              PIC S9(9)   COMP-3.                       
015400                                                                          
015500 01  W-DAINLEV-X.                                                         
015600     03  W-DAINLEV              PIC 9(16).                                
015700                                                                          
016100     EJECT                                                                
016200*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016300*****                                                                     
016400 01  IMS-WS.                                                              
016500     03  FILLER                 PIC X(16)   VALUE 'IMS-WS '.              
016600     SKIP2                                                                
016700*****                    **** STATUS-KOD FRÅN IMS                         
016800     03  STATUS-WS              PIC XX.                                   
016900         88  SEGMENT-FINNS                  VALUE '  '.                   
017000         88  SEGMENT-SAKNAS                 VALUE 'GE'.                   
017100     SKIP2                                                                
017200     03  GODK-STATUSKODER.                                                
017300       05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).            
017400                                                                          
017500*                                                                         
017600 01  SSA1                       PIC X(64).                                
017700 01  SSA2                       PIC X(64).                                
017800     EJECT                                                                
017900*01   -COPY W0003                                                         
018000     EJECT                                                                
018100 01  DLI-IO-AREA.                                                         
018200     03 IO-AREA          PIC X(900)      VALUE SPACE.                     
018300     SKIP3                                                                
018400*03  WLINLE01  -COPY WDL201   -RED IO-AREA.                               
018500     EJECT                                                                
018600*03  WLINLE11  -COPY WDL211   -RED IO-AREA                                
018700     EJECT                                                                
018800*03  WLINLE21  -COPY WDL221   -RED IO-AREA.                               
018900     EJECT                                                                
019000*03  WLINLE22  -COPY WDL222   -RED IO-AREA.                               
019100     EJECT                                                                
019200*03  WLINLE23  -COPY WDL223   -RED IO-AREA.                               
019300     EJECT                                                                
019400*03  WLARTC01  -COPY WDK601   -PRE WDK601-  -RED IO-AREA.                 
019500     EJECT                                                                
019600*03  WLARTC11  -COPY WDK611   -PRE WDK611-  -RED IO-AREA.                 
019700                                                                          
019800     EJECT                                                                
019900 LINKAGE SECTION.                                                         
020000     SKIP3                                                                
020100*01  -COPY W0008 -PRE WDK6-.                                              
020200     05  FILLER              PIC X(15).                                   
020300     EJECT                                                                
020400*01  -COPY W0008 -PRE WDL2-.                                              
020500     05  FILLER              PIC X(15).                                   
020600     EJECT                                                                
021000 PROCEDURE DIVISION USING WDK6-PCB WDL2-PCB .                             
021100     ENTRY 'DLITCLB' USING WDK6-PCB WDL2-PCB .                            
021200                                                                          
021300     PERFORM A-INIT                                                       
021400                                                                          
021500     PERFORM IMS-GN-WDL201                                                
023300                                                                          
023301     PERFORM UNTIL NOT SEGMENT-FINNS                                      
023302                                                                          
023303        MOVE NEJ TO FL-ANTAL-OK                                           
023304                    FL-SPARAT-C1-POST                                     
023305                    FL-SPARAT-C2-POST                                     
023306        PERFORM E-NOLLA-SALDO-FALT                                        
023307                                                                          
023308        MOVE ART-IDARTNR      TO W-IDARTNR                                
023309        PERFORM IMS-GU-WDK601                                             
023310        MOVE WDK601-ART-TIERSDAT TO W-TIERSDAT                            
023311                                                                          
023312        PERFORM IMS-GNP-WDK611                                            
023313        IF SEGMENT-FINNS                                                  
023314           PERFORM F-FLYTTA-SALDO-FALT                                    
023315        END-IF                                                            
023316        IF W-TIERSDAT = ZERO                                              
023400           PERFORM IMS-GNP-WDL211                                         
023600                                                                          
023700           MOVE +1           TO W-ANTAL                                   
023800                                                                          
023900           PERFORM UNTIL NOT SEGMENT-FINNS                                
024000             OR W-ANTAL > 6                                               
024100                                                                          
024200              MOVE INL-DAINLEV TO W-DAINLEV                               
024300              PERFORM G-LAS-KOLLA-SALDO                                   
024400              ADD +1          TO W-ANTAL                                  
024500              PERFORM IMS-GNP-WDL211                                      
024600           END-PERFORM                                                    
024700                                                                          
024800           PERFORM UNTIL NOT SEGMENT-FINNS                                
024900              MOVE INL-DAINLEV TO W-DAINLEV                               
024910                                  W-UT-DAINLEV                            
025000              SUBTRACT INL-DAINLEV FROM DAINLEV-NIOR                      
025100                         GIVING W-DAINLEV-KONV                            
025200                                                                          
025300              PERFORM IMS-GNP-WDL221                                      
025400                                                                          
025500              IF SEGMENT-FINNS                                            
025600*** R32 (RAPPORTERAD INL, FÖRD, CLEARING)                                 
025700                 MOVE MOT-KDRT TO TEST-KDRT                               
025800                 IF FL-ANTAL-OK = JA OR MOT-KVANTMOT = +0                 
025900                  OR NOT KDRT-OK OR WDK6-LS = +0                          
026000                    IF MOT-IDPTYP = 'R32'                                 
026100                       MOVE W-DAINLEV-AAMM         TO TMP1-YYMM           
026200                       MOVE W-DATUM-MINUS-ETT-AR   TO TMP2-YYMM           
026300                       PERFORM WY2000P8                                   
026400                       IF TMP1-YYMM < TMP2-YYMM                           
026500                          PERFORM B-REDIGERA-SKRIV-UTPOST                 
027000                       END-IF                                             
027100                    END-IF                                                
027200                 ELSE                                                     
027300                    PERFORM H-KOLLA-SALDO                                 
027400                 END-IF                                                   
027500                                                                          
027600              ELSE                                                        
027700                 PERFORM IMS-GNP-WDL222                                   
027800                                                                          
027900                 IF SEGMENT-FINNS                                         
028000*** R33 (DIR.LEV) & R34 (DIR.INL)                                         
028100                    MOVE W-DAINLEV-AAMM         TO TMP1-YYMM              
028200                    MOVE W-DATUM-MINUS-ETT-AR   TO TMP2-YYMM              
028300                    PERFORM WY2000P8                                      
028400                    IF TMP1-YYMM < TMP2-YYMM                              
028500                       PERFORM C-REDIGERA-SKRIV-UTPOST                    
029000                    END-IF                                                
029100                                                                          
029200                                                                          
029300                 ELSE                                                     
029400                    PERFORM IMS-GNP-WDL223                                
029500                                                                          
029600                    IF SEGMENT-FINNS                                      
029700*** R40 (RETURER)                                                         
029800                       MOVE W-DAINLEV-AAMM         TO TMP1-YYMM           
029900                       MOVE W-DATUM-MINUS-ETT-AR   TO TMP2-YYMM           
030000                       PERFORM WY2000P8                                   
030100                       IF TMP1-YYMM < TMP2-YYMM                           
030200                          PERFORM D-REDIGERA-SKRIV-UTPOST                 
030700                       END-IF                                             
030800                                                                          
030900                    ELSE                                                  
031000                       CALL FELLOG                                        
031100                                                                          
031200                    END-IF                                                
031300                 END-IF                                                   
031400              END-IF                                                      
031500                                                                          
031600              PERFORM IMS-GNP-WDL211                                      
031700                                                                          
031800           END-PERFORM                                                    
031900                                                                          
032000        ELSE                                                              
032100           PERFORM IMS-GNP-WDL211                                         
032200                                                                          
032300           PERFORM UNTIL NOT SEGMENT-FINNS                                
032400              MOVE INL-DAINLEV TO W-DAINLEV                               
032410                                  W-UT-DAINLEV                            
032500              SUBTRACT INL-DAINLEV FROM DAINLEV-NIOR                      
032600                         GIVING W-DAINLEV-KONV                            
032700                                                                          
032800              PERFORM IMS-GNP-WDL221                                      
032900                                                                          
033000              IF SEGMENT-FINNS                                            
033100                 MOVE MOT-KDRT TO TEST-KDRT                               
033200                 IF FL-ANTAL-OK = JA OR MOT-KVANTMOT = +0                 
033300                  OR NOT KDRT-OK OR WDK6-LS = +0                          
033400                    MOVE MOT-IDDC TO WS-IDDC                              
033500                    IF NOT CDC-TR                                         
033600                       IF FL-SPARAT-C1-POST = NEJ                         
033700                          MOVE JA TO FL-SPARAT-C1-POST                    
033800                                                                          
033900                       ELSE                                               
034000                          IF MOT-IDPTYP = 'R32'                           
034100                         MOVE W-DAINLEV-AAMM       TO TMP1-YYMM           
034200                         MOVE W-DATUM-MINUS-ETT-AR TO TMP2-YYMM           
034300                         PERFORM WY2000P8                                 
034400                         IF TMP1-YYMM < TMP2-YYMM                         
034500                                PERFORM B-REDIGERA-SKRIV-UTPOST           
035000                             END-IF                                       
035100                          END-IF                                          
035200                       END-IF                                             
035300                                                                          
035400                    ELSE                                                  
035500                       IF FL-SPARAT-C2-POST = NEJ                         
035600                          MOVE JA TO FL-SPARAT-C2-POST                    
035700                                                                          
035800                       ELSE                                               
035900                          IF MOT-IDPTYP = 'R32'                           
036000                         MOVE W-DAINLEV-AAMM       TO TMP1-YYMM           
036100                         MOVE W-DATUM-MINUS-ETT-AR TO TMP2-YYMM           
036200                         PERFORM WY2000P8                                 
036300                         IF TMP1-YYMM < TMP2-YYMM                         
036400                                PERFORM B-REDIGERA-SKRIV-UTPOST           
036900                             END-IF                                       
037000                          END-IF                                          
037100                       END-IF                                             
037200                    END-IF                                                
037300                 ELSE                                                     
037400                    PERFORM H-KOLLA-SALDO                                 
037500                 END-IF                                                   
037600              ELSE                                                        
037700                 PERFORM IMS-GNP-WDL222                                   
037800                                                                          
037900                 IF SEGMENT-FINNS                                         
038000                    MOVE DIR-IDDC TO WS-IDDC                              
038100                    IF NOT CDC-TR                                         
038200                       IF FL-SPARAT-C1-POST = NEJ                         
038300                          MOVE JA TO FL-SPARAT-C1-POST                    
038400                                                                          
038500                       ELSE                                               
038600                         MOVE W-DAINLEV-AAMM       TO TMP1-YYMM           
038700                         MOVE W-DATUM-MINUS-ETT-AR TO TMP2-YYMM           
038800                         PERFORM WY2000P8                                 
038900                         IF TMP1-YYMM < TMP2-YYMM                         
039000                             PERFORM C-REDIGERA-SKRIV-UTPOST              
039500                          END-IF                                          
039600                       END-IF                                             
039700                                                                          
039800                    ELSE                                                  
039900                       IF FL-SPARAT-C2-POST = NEJ                         
040000                          MOVE JA TO FL-SPARAT-C2-POST                    
040100                                                                          
040200                       ELSE                                               
040300                         MOVE W-DAINLEV-AAMM       TO TMP1-YYMM           
040400                         MOVE W-DATUM-MINUS-ETT-AR TO TMP2-YYMM           
040500                         PERFORM WY2000P8                                 
040600                         IF TMP1-YYMM < TMP2-YYMM                         
040700                             PERFORM C-REDIGERA-SKRIV-UTPOST              
041200                          END-IF                                          
041300                       END-IF                                             
041400                    END-IF                                                
041500                                                                          
041600                 ELSE                                                     
041700                    PERFORM IMS-GNP-WDL223                                
041800                                                                          
041900                    IF SEGMENT-FINNS                                      
042000                       MOVE RET-IDDC TO WS-IDDC                           
042100                       IF NOT CDC-TR                                      
042200                          IF FL-SPARAT-C1-POST = NEJ                      
042300                             MOVE JA TO FL-SPARAT-C1-POST                 
042400                                                                          
042500                          ELSE                                            
042600                         MOVE W-DAINLEV-AAMM       TO TMP1-YYMM           
042700                         MOVE W-DATUM-MINUS-ETT-AR TO TMP2-YYMM           
042800                         PERFORM WY2000P8                                 
042900                         IF TMP1-YYMM < TMP2-YYMM                         
043000                                PERFORM D-REDIGERA-SKRIV-UTPOST           
043500                             END-IF                                       
043600                          END-IF                                          
043700                                                                          
043800                       ELSE                                               
043900                          IF FL-SPARAT-C2-POST = NEJ                      
044000                             MOVE JA TO FL-SPARAT-C2-POST                 
044100                                                                          
044200                          ELSE                                            
044300                         MOVE W-DAINLEV-AAMM       TO TMP1-YYMM           
044400                         MOVE W-DATUM-MINUS-ETT-AR TO TMP2-YYMM           
044500                         PERFORM WY2000P8                                 
044600                         IF TMP1-YYMM < TMP2-YYMM                         
044700                                PERFORM D-REDIGERA-SKRIV-UTPOST           
045200                             END-IF                                       
045300                          END-IF                                          
045400                       END-IF                                             
045500                                                                          
045600                    ELSE                                                  
045700                       CALL FELLOG                                        
045800                                                                          
045900                    END-IF                                                
046000                 END-IF                                                   
046100              END-IF                                                      
046200                                                                          
046300              PERFORM IMS-GNP-WDL211                                      
046400                                                                          
046500           END-PERFORM                                                    
046600        END-IF                                                            
046700                                                                          
046800        PERFORM IMS-GN-WDL201                                             
046900                                                                          
047000     END-PERFORM                                                          
047010                                                                          
047100     PERFORM Z-FINIT                                                      
047200     MOVE ZERO TO RETURN-CODE                                             
047300     GOBACK                                                               
047400     .                                                                    
047500     EJECT                                                                
047600 A-INIT SECTION.                                                          
047700                                                                          
047800     OPEN OUTPUT W61188-DEL                                               
047900                                                                          
048000     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
048100                                                                          
048200     ACCEPT W-DATUM      FROM  DATE                                       
048300                                                                          
048400     IF  W-DATUM-MM > 6                                                   
048500       IF W-DATUM-AA = 00                                                 
048600         MOVE 99       TO W-DATUM-MINUS-ETT-AR-AA                         
048700       ELSE                                                               
048800         SUBTRACT +1   FROM W-DATUM-AA                                    
048900                       GIVING W-DATUM-MINUS-ETT-AR-AA                     
049000       END-IF                                                             
049100       MOVE +7         TO W-DATUM-MINUS-ETT-AR-MM                         
049200                                                                          
049300       MOVE W-DATUM-AA TO W-DATUM-MINUS-6-MAN-AA                          
049400       MOVE +1         TO W-DATUM-MINUS-6-MAN-MM                          
049500                                                                          
049600     ELSE                                                                 
049700       IF W-DATUM-AA = 00                                                 
049800         MOVE 99       TO W-DATUM-MINUS-ETT-AR-AA                         
049900                          W-DATUM-MINUS-6-MAN-AA                          
050000       ELSE                                                               
050100         SUBTRACT +1   FROM W-DATUM-AA                                    
050200                       GIVING W-DATUM-MINUS-ETT-AR-AA                     
050300         SUBTRACT +1   FROM W-DATUM-AA                                    
050400                       GIVING W-DATUM-MINUS-6-MAN-AA                      
050500       END-IF                                                             
050600       MOVE +1         TO W-DATUM-MINUS-ETT-AR-MM                         
050700                                                                          
050800       MOVE +7         TO W-DATUM-MINUS-6-MAN-MM                          
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
051200 B-REDIGERA-SKRIV-UTPOST      SECTION.                                    
051300                                                                          
051400     MOVE W-DAINLEV-DATUM TO DAT-I-TIDATUM                                
051500     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
051600                                                                          
051700     CALL WDATKONV        USING DAT-KDDATFORM                             
051800                                DAT-I-TIDATUM                             
051900                                DAT-O-TIDATUM                             
052000                                DAT-KDSVAR                                
052100                                                                          
052200     MOVE MOT-IDPTYP          TO W-UT-IDPTYP                              
052300     MOVE W-IDARTNR           TO W-UT-IDARTNR                             
052400     MOVE DAT-TIAAVVD         TO W-UT-TIAAVVD                             
052500     MOVE MOT-IDLOPNRM        TO W-UT-IDLOPNRM                            
052600     MOVE MOT-IDAVINR         TO W-UT-IDAVINR                             
052700     MOVE MOT-IDKONTO         TO W-UT-IDKONTO                             
052800     MOVE MOT-IDLEVNR         TO W-UT-IDLEVNR                             
052900     MOVE ZERO                TO W-UT-IDORDNR                             
053000     MOVE MOT-ADLAGOMR        TO W-UT-ADLAGOMR                            
053100     MOVE MOT-ADGANG          TO W-UT-ADGANG                              
053200     MOVE MOT-ADPLATS         TO W-UT-ADPLATS                             
053300     MOVE MOT-IDDC            TO W-UT-IDDC                                
053400     MOVE MOT-KDRT            TO W-UT-KDRT                                
053500     MOVE MOT-KDAVVANT        TO W-UT-KDAVVANT                            
053600     MOVE MOT-KDAVVKV         TO W-UT-KDAVVKV                             
053700     MOVE MOT-KVANTMOT        TO W-UT-KVANTMOT                            
053800     MOVE MOT-KVAVIS          TO W-UT-KVAVIS                              
053900     MOVE MOT-KVFORDEL        TO W-UT-KVFORDEL                            
054000     MOVE MOT-KVRETUR         TO W-UT-KVRETUR                             
054100     MOVE MOT-KVFORV          TO W-UT-KVFORV                              
054200     MOVE MOT-TIAVIDAT        TO W-UT-TIAVSDAT                            
054300     MOVE MOT-TIUPPDAT        TO W-UT-TIUPPDAT                            
054400     MOVE ZERO                TO W-UT-IDDISTR                             
054500                                 W-UT-IDKUNDNR                            
054600                                 W-UT-IDFAKT                              
054700                                 W-UT-IDKUNDRF                            
054800                                 W-UT-IDPRODNR                            
054900                                                                          
055000     WRITE W61188-UTPOST FROM W-UT-AREA                                   
055100                                                                          
055200     MOVE W-UT-IDPTYP    TO W61188-TRANSTYP                               
055300     MOVE W61188-TRANSID TO POSTSUM-TRANSID                               
055400     CALL POSTSUM USING POSTSUM-PARM                                      
055500     .                                                                    
055600     EJECT                                                                
055700 C-REDIGERA-SKRIV-UTPOST      SECTION.                                    
055800                                                                          
055900     MOVE W-DAINLEV-DATUM TO DAT-I-TIDATUM                                
056000     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
056100                                                                          
056200     CALL WDATKONV        USING DAT-KDDATFORM                             
056300                                DAT-I-TIDATUM                             
056400                                DAT-O-TIDATUM                             
056500                                DAT-KDSVAR                                
056600                                                                          
056700     MOVE DIR-IDPTYP          TO W-UT-IDPTYP                              
056800     MOVE W-IDARTNR           TO W-UT-IDARTNR                             
056900     MOVE DAT-TIAAVVD         TO W-UT-TIAAVVD                             
057000     MOVE DIR-IDLOPNRM        TO W-UT-IDLOPNRM                            
057100     MOVE DIR-IDAVINR         TO W-UT-IDAVINR                             
057200     MOVE DIR-IDKONTO         TO W-UT-IDKONTO                             
057300     MOVE DIR-IDLEVNR         TO W-UT-IDLEVNR                             
057400     MOVE ZERO                TO W-UT-IDORDNR                             
057500                                 W-UT-ADLAGOMR                            
057600                                 W-UT-ADGANG                              
057700                                 W-UT-ADPLATS                             
057800     MOVE DIR-IDDC            TO W-UT-IDDC                                
057900     MOVE DIR-KDRT            TO W-UT-KDRT                                
058000     MOVE ZERO                TO W-UT-KDAVVANT                            
058100                                 W-UT-KDAVVKV                             
058200                                 W-UT-KVANTMOT                            
058300     MOVE DIR-KVAVIS          TO W-UT-KVAVIS                              
058400     MOVE ZERO                TO W-UT-KVFORDEL                            
058500                                 W-UT-KVRETUR                             
058600                                 W-UT-KVFORV                              
058700     MOVE DIR-TIAVSDAT        TO W-UT-TIAVSDAT                            
058800     MOVE ZERO                TO W-UT-TIUPPDAT                            
058900     MOVE DIR-IDDISTR         TO W-UT-IDDISTR                             
059000     MOVE DIR-IDKUNDNR        TO W-UT-IDKUNDNR                            
059100     MOVE DIR-IDFAKT          TO W-UT-IDFAKT                              
059200     MOVE DIR-IDKUNDRF        TO W-UT-IDKUNDRF                            
059300     MOVE DIR-IDPRODNR        TO W-UT-IDPRODNR                            
059400                                                                          
059500     WRITE W61188-UTPOST FROM W-UT-AREA                                   
059600                                                                          
059700     MOVE W-UT-IDPTYP    TO W61188-TRANSTYP                               
059800     MOVE W61188-TRANSID TO POSTSUM-TRANSID                               
059900     CALL POSTSUM USING POSTSUM-PARM                                      
060000     .                                                                    
060100     EJECT                                                                
060200 D-REDIGERA-SKRIV-UTPOST      SECTION.                                    
060300                                                                          
060400     MOVE W-DAINLEV-DATUM TO DAT-I-TIDATUM                                
060500     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
060600                                                                          
060700     CALL WDATKONV        USING DAT-KDDATFORM                             
060800                                DAT-I-TIDATUM                             
060900                                DAT-O-TIDATUM                             
061000                                DAT-KDSVAR                                
061100                                                                          
061200     MOVE RET-IDPTYP          TO W-UT-IDPTYP                              
061300     MOVE W-IDARTNR           TO W-UT-IDARTNR                             
061400     MOVE DAT-TIAAVVD         TO W-UT-TIAAVVD                             
061500     MOVE RET-IDLOPNRM        TO W-UT-IDLOPNRM                            
061600     MOVE ZERO                TO W-UT-IDAVINR                             
061700                                 W-UT-IDKONTO                             
061800     MOVE RET-IDLEVNR         TO W-UT-IDLEVNR                             
061900     MOVE RET-IDORDNR         TO W-UT-IDORDNR                             
062000     MOVE ZERO                TO W-UT-ADLAGOMR                            
062100                                 W-UT-ADGANG                              
062200                                 W-UT-ADPLATS                             
062300     MOVE RET-IDDC            TO W-UT-IDDC                                
062400     MOVE ZERO                TO W-UT-KDRT                                
062500                                 W-UT-KDAVVANT                            
062600                                 W-UT-KDAVVKV                             
062700                                 W-UT-KVANTMOT                            
062800                                 W-UT-KVAVIS                              
062900                                 W-UT-KVFORDEL                            
063000     MOVE RET-KVRETUR         TO W-UT-KVRETUR                             
063100     MOVE ZERO                TO W-UT-KVFORV                              
063200                                 W-UT-TIAVSDAT                            
063300                                 W-UT-TIUPPDAT                            
063400                                 W-UT-IDDISTR                             
063500                                 W-UT-IDKUNDNR                            
063600                                 W-UT-IDFAKT                              
063700                                 W-UT-IDKUNDRF                            
063800                                 W-UT-IDPRODNR                            
063900                                                                          
064000     WRITE W61188-UTPOST FROM W-UT-AREA                                   
064100                                                                          
064200     MOVE W-UT-IDPTYP    TO W61188-TRANSTYP                               
064300     MOVE W61188-TRANSID TO POSTSUM-TRANSID                               
064400     CALL POSTSUM USING POSTSUM-PARM                                      
064500     .                                                                    
064600     EJECT                                                                
064700 E-NOLLA-SALDO-FALT            SECTION.                                   
064800                                                                          
064900     MOVE ZERO TO                WDK6-LS                                  
065000                                 W-LS                                     
065100     .                                                                    
065200     EJECT                                                                
065300 F-FLYTTA-SALDO-FALT           SECTION.                                   
065400     ADD WDK611-CLAG-KVLS      TO    WDK6-LS                              
065500     ADD WDK611-CLAG-KVEFRS    TO    WDK6-LS                              
065600     ADD WDK611-CLAG-KVAKS-CDC TO    WDK6-LS                              
065700     ADD WDK611-CLAG-KVAKS-PAV TO    WDK6-LS                              
065800     ADD WDK611-CLAG-KVAKS-T   TO    WDK6-LS                              
065900     ADD WDK611-CLAG-KVRESS    TO    WDK6-LS                              
066000     .                                                                    
066100     SKIP3                                                                
066200 G-LAS-KOLLA-SALDO             SECTION.                                   
066300     PERFORM IMS-GNP-WDL221                                               
066400     IF SEGMENT-FINNS                                                     
066500        MOVE MOT-KDRT TO TEST-KDRT                                        
066600        IF KDRT-OK                                                        
066700           MOVE MOT-IDDC TO WS-IDDC                                       
066800           IF CDC-SE                                                      
066900              ADD MOT-KVANTMOT TO W-LS                                    
067000           ELSE                                                           
067100              ADD MOT-KVANTMOT TO W-LS                                    
067200           END-IF                                                         
067300        END-IF                                                            
067400     END-IF                                                               
067500     IF W-LS NOT < WDK6-LS                                                
067600        MOVE JA TO FL-ANTAL-OK                                            
067700     END-IF                                                               
067800     .                                                                    
067900 H-KOLLA-SALDO                 SECTION.                                   
068000     IF SEGMENT-FINNS                                                     
068100        IF KDRT-OK                                                        
068200           MOVE MOT-IDDC TO WS-IDDC                                       
068300           IF NOT CDC-TR                                                  
068400              ADD MOT-KVANTMOT TO W-LS                                    
068500              MOVE JA TO FL-SPARAT-C1-POST                                
068600           ELSE                                                           
068700              ADD MOT-KVANTMOT TO W-LS                                    
068800              MOVE JA TO FL-SPARAT-C2-POST                                
068900           END-IF                                                         
069000        END-IF                                                            
069100     END-IF                                                               
069200     IF W-LS NOT < WDK6-LS                                                
069300        MOVE JA TO FL-ANTAL-OK                                            
069400     END-IF                                                               
069500     .                                                                    
069600 Z-FINIT   SECTION.                                                       
069700                                                                          
069800     CLOSE  W61188-DEL                                                    
069900                                                                          
070000     MOVE 'S' TO POSTSUM-OPKOD                                            
070100     CALL POSTSUM USING POSTSUM-PARM                                      
070200     .                                                                    
070300     EJECT                                                                
070400* IMS SECTIONER                                                           
070500                                                                          
070600 IMS-GN-WDL201           SECTION.                                         
070700     MOVE 'WDL201  '   TO SSA1                                            
070800     MOVE '  GB' TO GODK-STATUSKODER                                      
070900     CALL CBLTDLI USING GN WDL2-PCB DLI-IO-AREA SSA1                      
071000     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
071100     PERFORM IMS-STATUS-KONTROLL                                          
071200     .                                                                    
071300 IMS-GNP-WDL211         SECTION.                                          
071400     MOVE 'WDL211  ' TO SSA1                                              
071500     MOVE '  GE' TO GODK-STATUSKODER                                      
071600     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-AREA SSA1                     
071700                                                                          
071800     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
071900     PERFORM IMS-STATUS-KONTROLL                                          
072000     .                                                                    
072100 IMS-GNP-WDL221         SECTION.                                          
072200     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
072300             DELIMITED BY SIZE INTO SSA1                                  
072400     MOVE 'WDL221  ' TO SSA2                                              
072500                                                                          
072600     MOVE '  GE' TO GODK-STATUSKODER                                      
072700     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-AREA SSA1 SSA2                
072800                                                                          
072900     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
073000     PERFORM IMS-STATUS-KONTROLL                                          
073100     .                                                                    
073200 IMS-GNP-WDL222         SECTION.                                          
073300     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
073400             DELIMITED BY SIZE INTO SSA1                                  
073500     MOVE 'WDL222  ' TO SSA2                                              
073600                                                                          
073700     MOVE '  GE' TO GODK-STATUSKODER                                      
073800     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-AREA SSA1 SSA2                
073900                                                                          
074000     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
074100     PERFORM IMS-STATUS-KONTROLL                                          
074200     .                                                                    
074300 IMS-GNP-WDL223         SECTION.                                          
074400     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
074500             DELIMITED BY SIZE INTO SSA1                                  
074600     MOVE 'WDL223  ' TO SSA2                                              
074700                                                                          
074800     MOVE '  GE' TO GODK-STATUSKODER                                      
074900     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-AREA SSA1 SSA2                
075000                                                                          
075100     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
075200     PERFORM IMS-STATUS-KONTROLL                                          
075300     .                                                                    
077700 IMS-GU-WDK601         SECTION.                                           
077800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
077900             DELIMITED BY SIZE INTO SSA1                                  
078100     MOVE SPACE  TO GODK-STATUSKODER                                      
078110     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-AREA SSA1                     
078200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
078300     PERFORM IMS-STATUS-KONTROLL                                          
078400     .                                                                    
078500 IMS-GNP-WDK611         SECTION.                                          
078600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
078700             DELIMITED BY SIZE INTO SSA1                                  
078800     MOVE 'WDK611  ' TO SSA2                                              
078900     MOVE '  GE' TO GODK-STATUSKODER                                      
079000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA SSA1 SSA2                
079100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
079200     PERFORM IMS-STATUS-KONTROLL                                          
079300     .                                                                    
079400 IMS-STATUS-KONTROLL SECTION.                                             
079500     SET STATUS-IX TO 1                                                   
079600     SEARCH GODK-STATUS AT END CALL FELLOG                                
079700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
079800     END-SEARCH                                                           
079900     .                                                                    
080000*    -COPY WY2000P8                                                       
