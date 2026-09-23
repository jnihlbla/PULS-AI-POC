000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WB010200.                                                
000400 AUTHOR.         CONNY EGHOLT.                                            
000500 DATE-WRITTEN.   03/12/17.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAMN:       WB0102                                                   
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET UPPDATERAR TABELL TB1ACCE                             
001200*                                                                         
001300*        LÄSNING AV ETT RECORD I TB1ACCE MED NYCKEL IDARTNR.              
001400*                   VID KDPGMACT='S'                                      
001500*                                                                         
001600*        REPLACE AV DB2 TB1ACCE MED DE SPECIELLA TILL-                    
001700*                   BEHÖRS DATAELEMENTEN                                  
001800*                   VID KDPGMACT='U'                                      
001900*                                                                         
002000*        BORTTAG AV HELA RECORD I DB2 TB1ACCE.                            
002120*                   VID KDPGMACT='D'.                                     
002130*          BLIR BORTTAGS-TYP = 'P' FÖR ENSKILD ARTIKEL PÅ ETT SU          
002140*          BLIR BORTTAGS-TYP = 'D' FÖR ALLA ARTIKLAR PÅ ETT SU            
002150*          BLIR BORTTAGS-TYP = 'A' FÖR ANNUL-ARTIKLAR PÅ ETT SU           
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: WB0102U                                             
002500*        REQUEST:     WB0102I1                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        RESPONSE:    WB0102O1                                            
002900*                                                                         
003000*    ÄNDRINGAR:                                                           
003100*        VID ÄT 07:7 (ETR=4459048) TAS EN HEL DEL DATAELEMENT             
003200*        BORT OCH NÅGRA KOMMER TILL.                                      
003300*        IDARTNR+IDUPPDSU = NYTT NYCKELBEGREPP                            
003400*        SE ÄNDRINGSNOTERINGAR FÖR WB010100                               
003500*                                                                         
003510*        VID ÄT 07:9 (ETR=5814533) BORTTAGES FÄLTEN FÖR PSW-PLAN          
003520*        OCH 6 NYA NOTERINGSFÄLT TILKOMMER.                               
003530*        OCH TILLÄGG AV FUNKTIONEN BORTTAG AV ANNUL-ARTIKLAR.             
003600*                                                                         
003700*                                                                         
003800                                                                          
003900     SKIP3                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100     SKIP2                                                                
004200 INPUT-OUTPUT SECTION.                                                    
004300                                                                          
004400 FILE-CONTROL.                                                            
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700     SKIP3                                                                
004800 FILE SECTION.                                                            
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100 77  IDPGM                       PIC X(08)   VALUE 'WB010200'.            
005200                                                                          
005300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005500 77  KDRC-DISPLAY                PIC Z(5).                                
005600*    --- GENERELLA KONSTANTER                                             
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  WS-ADDISPABS                PIC X(50)                                
006000                        VALUE 'CARPARTS.ACCE.TABLEUPDATE'.                
006010**** VID DELETE         VALUE 'CARPARTS.ACCE.TABLEDELETE'.                
006100                                                                          
006200*    --- SWITCHAR                                                         
006300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006400     88  NYCKLAR-OK                          VALUE 'J'.                   
006500     88  NYCKLAR-FEL                         VALUE 'N'.                   
006600                                                                          
006700 77  BORTTAG-SW                  PIC X       VALUE ' '.                   
006800     88  BORTTAG-IDARTNR                     VALUE 'P'.                   
006900     88  BORTTAG-IDUPPDSU                    VALUE 'D'.                   
006910     88  BORTTAG-IDUPPDSU-ANNUL              VALUE 'A'.                   
007000                                                                          
007100 77  INDATA-SW                  PIC X       VALUE 'J'.                    
007200     88  INDATA-OK                          VALUE 'J'.                    
007300     88  INDATA-FEL                         VALUE 'N'.                    
007400                                                                          
007500*    --- HJÄLPVARIABLER OCH ARBETSFÄLT                                    
007501*                                                                         
007520 01  WS-IDARTNR                  PIC X(8) VALUE ZERO.                     
007530 01  WS-ANNUL                    PIC X    VALUE 'A'.                      
007600*                                                                         
007700*01  WS-AAVVD-AREA.                                                       
007800*    03  WS-TIAOINF-1.                                                    
007900*        05  WS-TIAOINF-SEK      PIC 9(2).                                
008000*        05  WS-TIAOINF-AAVV     PIC 9(4).                                
008100*        05  WS-TIAOINF-D        PIC 9.                                   
008200*                                                                         
008300*    03  WS-TIAOINF-2  REDEFINES WS-TIAOINF-1.                            
008400*        05  WS-TIAOINF-AAAAVV   PIC 9(6).                                
008500*        05  FILLER              PIC 9(1).                                
008600*                                                                         
008700*    03  WS-TIAOINF-3  REDEFINES WS-TIAOINF-1.                            
008800*        05  FILLER              PIC 9(2).                                
008900*        05  WS-TIAOINF-AAVVD    PIC 9(5).                                
009000                                                                          
009400     EJECT                                                                
009500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009600 01  GENERELLA-SUBPROGRAM.                                                
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010100     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010200     SKIP3                                                                
010300*    --- PARAMETRAR TILL ABEND                                            
010400                                                                          
010500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010800 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
010900     SKIP3                                                                
011000 01  MESSAGE-CODES.                                                       
011100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
011200     03  INF-NOTHING-UPDATED     PIC X(3)    VALUE '004'.                 
011300     03  INF-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
011400     03  ERR-INVALID-KEY-FLD-0   PIC X(3)    VALUE '022'.                 
011500     03  ERR-0-MUST-BE-NUMERIC   PIC X(3)    VALUE '024'.                 
011600     03  ERR-0-NOT-FOUND         PIC X(3)    VALUE '025'.                 
011700     03  ERR-0-MUST-BE-ENTERED   PIC X(3)    VALUE '026'.                 
011800     03  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.                 
011900     03  ERR-0-MISSING           PIC X(3)    VALUE '041'.                 
012000     03  ERR-0-OVERFLOW          PIC X(3)    VALUE '048'.                 
012100     03  ERR-SYSTEM-ERROR        PIC X(3)    VALUE '099'.                 
012200     03  ERR-WRONG-DATE          PIC X(3)    VALUE '102'.                 
012300     03  ERR-2-CONFLICT-FIELDS   PIC X(3)    VALUE '103'.                 
012400     03  ERR-BAD-FORMAT          PIC X(3)    VALUE '104'.                 
012500     03  ERR-CHECK-FORMAT-7-2    PIC X(3)    VALUE '105'.                 
012600     03  ERR-CHECK-FORMAT-3-2    PIC X(3)    VALUE '106'.                 
012700     EJECT                                                                
012800                                                                          
012900 01  FILLER                      PIC X(16) VALUE 'NYCKLAR '.              
013000 01  W-IDARTNR-X.                                                         
013100     03 W-IDARTNR                PIC S9(9) COMP-3 VALUE ZERO.             
013200                                                                          
013300 01  W-IDUPPDSU-X.                                                        
013400     03 W-IDUPPDSU               PIC X(8)        VALUE SPACE.             
013500                                                                          
013600 01  W-IDUPPDSU-D-X.                                                      
013700     03 W-IDUPPDSU-D             PIC X(8)        VALUE SPACE.             
013710                                                                          
013720 01  W-IDUPPDSU-A-X.                                                      
013730     03 W-IDUPPDSU-A             PIC X(8)        VALUE SPACE.             
013800     EJECT                                                                
013900                                                                          
014000*01  -COPY WDATAREA                                                       
014100     EJECT                                                                
014200*01  -COPY WDECAREA                                                       
014300     EJECT                                                                
014400*                                                                         
014500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014600     SKIP3                                                                
014700*01  -COPY WZ01SUB                                                        
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
015000     SKIP3                                                                
015100 01  REQU-AREA.                                                           
015200*    03  -COPY WZ01REQU                                                   
015300*    03  -COPY WB0102I1                                                   
015400     EJECT                                                                
015500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
015600     SKIP3                                                                
015700 01  RESP-AREA.                                                           
015800*    03  -COPY WZ01RESP                                                   
015900*    03  -COPY WB0102O1                                                   
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
016200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
016300                                                                          
016400 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
016500 01  DB2-WS.                                                              
016600     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
016700         88  CURSOR-OK                       VALUE 000.                   
016800         88  RADER-FINNS                     VALUE 000.                   
016900         88  RADER-SAKNAS                    VALUE 100.                   
017000         88  ACCESS-ERROR                    VALUE 904.                   
017100     03  GODK-SQLCODEKODER.                                               
017200         05  GODK-SQLCODE OCCURS 5                                        
017300             INDEXED BY SQLCODE-IX PIC 9(3).                              
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)  VALUE 'TB1ACCE-AREA'.         
017600*01  -COPY TB1ACCE -PRE ACCE-                                             
017700     EJECT                                                                
017800                                                                          
017900                                                                          
018000     EXEC SQL INCLUDE TB1ACCE END-EXEC.                                   
018100     EJECT                                                                
018200                                                                          
018300 LINKAGE SECTION.                                                         
018400 01  MSG-PCB                     PIC X.                                   
018500     EJECT                                                                
018600                                                                          
018700 PROCEDURE DIVISION  USING MSG-PCB.                                       
018800 MAIN SECTION.                                                            
018900     ENTRY 'DLITCBL' USING MSG-PCB.                                       
019000                                                                          
019100     PERFORM S01-HAEMTA-ANROPSDATA                                        
019200     IF SUB-KDRC = 0                                                      
019300       PERFORM A-INIT                                                     
019400       PERFORM B-KOLLA-NYCKLAR                                            
019500       IF NYCKLAR-OK                                                      
019600         IF REQU-KDPGMACT = 'U'                                           
019700           PERFORM C-KOLLA-FLYTTA-INDATA                                  
019800           IF INDATA-OK                                                   
019900             PERFORM D-UPPDATERA                                          
020000           END-IF                                                         
020100         ELSE                                                             
020200           IF REQU-KDPGMACT = 'D'                                         
020300             PERFORM G-BORTTAG                                            
020400             IF CURSOR-OK                                                 
020500               PERFORM S99-RENSA-OUTPUT-RAD                               
020600             END-IF                                                       
020700           END-IF                                                         
020800         END-IF                                                           
020900         IF  REQU-KDPGMACT = 'S' OR 'U' OR 'D'                            
021000           PERFORM F-LAES-VISA-INFO                                       
021100         END-IF                                                           
021200       END-IF                                                             
021300       PERFORM S02-RETURNERA-SVAR                                         
021400     END-IF                                                               
021500                                                                          
021600     MOVE ZERO TO RETURN-CODE                                             
021700     GOBACK                                                               
021800     .                                                                    
021900     EJECT                                                                
022000 A-INIT SECTION.                                                          
022100                                                                          
022200     INITIALIZE GODK-SQLCODEKODER                                         
022300     INITIALIZE      RESP-AREA                                            
022400     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
022500                     RESP-IDMSG-INFO                                      
022600                     RESP-IDELMT-ERROR                                    
022700     MOVE '001'   TO RESP-IDMSGVER                                        
022800     .                                                                    
022900     EJECT                                                                
023000 B-KOLLA-NYCKLAR SECTION.                                                 
023100     SKIP2                                                                
023200     IF REQU-KDPGMACT NOT = 'U' AND 'S' AND 'D'                           
023300       MOVE NEJ TO NYCKLAR-SW                                             
023400       MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                          
023500       MOVE 'KDPGMACT'       TO RESP-IDELMT-ERROR                         
023600     END-IF                                                               
023700                                                                          
023800     IF NYCKLAR-OK                                                        
023900       MOVE ZERO  TO W-IDARTNR                                            
024000       MOVE SPACE TO W-IDUPPDSU                                           
024100                     W-IDUPPDSU-D                                         
024110                     W-IDUPPDSU-A                                         
024200       MOVE JA   TO NYCKLAR-SW                                            
024300                                                                          
024400       IF REQU-KDPGMACT = 'U' OR 'S'                                      
024500*        --- KOLLA IDARTNR-KEY + IDUPPDSU-KEY                             
024600*        --- VID NORMAL FRÅGA OCH UPPDATERING                             
024700         IF REQU-IDARTNR-KEY NOT = ALL '+' AND ZERO                       
024800           INSPECT REQU-IDARTNR-KEY                                       
024900                                 REPLACING LEADING SPACE BY ZERO          
025000           IF REQU-IDARTNR-KEY NUMERIC                                    
025100             MOVE REQU-IDARTNR-KEY TO W-IDARTNR                           
025200           ELSE                                                           
025300             MOVE NEJ   TO NYCKLAR-SW                                     
025400             MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR               
025500             MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                 
025600           END-IF                                                         
025700         ELSE                                                             
025800           MOVE NEJ     TO NYCKLAR-SW                                     
025900           MOVE ERR-0-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                 
026000           MOVE 'IDARTNR'            TO RESP-IDELMT-ERROR                 
026100         END-IF                                                           
026200                                                                          
026300         IF NYCKLAR-OK                                                    
026400*          --- OCH KOLLA IDUPPDSU                                         
026500           IF REQU-IDUPPDSU-KEY = ALL '+'                                 
026600             MOVE NEJ   TO NYCKLAR-SW                                     
026700             MOVE ERR-0-MUST-BE-ENTERED TO RESP-IDMSG-ERROR               
026800             MOVE 'SU'     TO RESP-IDELMT-ERROR                           
026900           ELSE                                                           
027100             INSPECT REQU-IDUPPDSU-KEY                                    
027200                               REPLACING LEADING SPACE BY ZERO            
027300             IF REQU-IDUPPDSU-KEY NUMERIC                                 
027400               MOVE REQU-IDUPPDSU-KEY TO W-IDUPPDSU                       
027500             ELSE                                                         
027600               MOVE NEJ TO NYCKLAR-SW                                     
027700               MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR             
027800               MOVE 'SU          '   TO RESP-IDELMT-ERROR                 
027900             END-IF                                                       
028000           END-IF                                                         
028100         END-IF                                                           
028200       ELSE                                                               
028300*        --- BORTTAGSFALL P: IDARTNR-KEY + IDUPPDSU-KEY                   
028400*        --- BORTTAGSFALL D: IDUPPDSU-D-KEY                               
028410*        --- BORTTAGSFALL A: IDUPPDSU-A-KEY                               
028420                                                                          
028500         IF REQU-IDARTNR-KEY = ALL '+' OR SPACE OR ZERO                   
028600           MOVE ZERO TO REQU-IDARTNR-KEY                                  
028700                        W-IDARTNR                                         
028800         ELSE                                                             
028900           INSPECT REQU-IDARTNR-KEY                                       
029000                          REPLACING LEADING SPACE BY ZERO                 
029100           IF REQU-IDARTNR-KEY NUMERIC                                    
029200             MOVE REQU-IDARTNR-KEY TO W-IDARTNR                           
029300           ELSE                                                           
029400             MOVE NEJ   TO NYCKLAR-SW                                     
029500             MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR               
029600             MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                 
029700           END-IF                                                         
029800         END-IF                                                           
029900                                                                          
030000*        --- KOLLA IDUPPDSU-KEY VID ARTIKEL-BORTTAG                       
030100         IF REQU-IDUPPDSU-KEY = ALL '+'                                   
030101**           -- KONTROLLFÄLT                                              
030102           MOVE SPACE TO W-IDUPPDSU                                       
030300         ELSE                                                             
030400           INSPECT REQU-IDUPPDSU-KEY REPLACING ALL SPACE BY ZERO          
030410                                                                          
030500           IF REQU-IDUPPDSU-KEY NUMERIC                                   
030600             MOVE REQU-IDUPPDSU-KEY TO W-IDUPPDSU                         
030700           ELSE                                                           
030701**           -- KONTROLLFÄLT                                              
030702             MOVE SPACE TO W-IDUPPDSU                                     
030900           END-IF                                                         
031000         END-IF                                                           
031100                                                                          
031200*        --- KOLLA IDUPPDSU-D-KEY                                         
031300         IF REQU-IDUPPDSU-D-KEY = ALL '+' OR SPACE                        
031310**           -- KONTROLLFÄLT                                              
031400           MOVE SPACE TO W-IDUPPDSU-D                                     
031500         ELSE                                                             
031600           INSPECT REQU-IDUPPDSU-D-KEY REPLACING ALL SPACE BY ZERO        
031700           IF REQU-IDUPPDSU-D-KEY NUMERIC                                 
031800             MOVE REQU-IDUPPDSU-D-KEY TO W-IDUPPDSU-D                     
031900           ELSE                                                           
031901**           -- KONTROLLFÄLT                                              
031910             MOVE SPACE TO W-IDUPPDSU-D                                   
032100           END-IF                                                         
032200         END-IF                                                           
032201                                                                          
032210*        --- KOLLA IDUPPDSU-A-KEY                                         
032220         IF REQU-IDUPPDSU-A-KEY = ALL '+' OR SPACE                        
032230**           -- KONTROLLFÄLT                                              
032240           MOVE SPACE TO W-IDUPPDSU-A                                     
032250         ELSE                                                             
032260           INSPECT REQU-IDUPPDSU-A-KEY REPLACING ALL SPACE BY ZERO        
032270           IF REQU-IDUPPDSU-A-KEY NUMERIC                                 
032280             MOVE REQU-IDUPPDSU-A-KEY TO W-IDUPPDSU-A                     
032291           ELSE                                                           
032292**           -- KONTROLLFÄLT                                              
032293             MOVE SPACE TO W-IDUPPDSU-A                                   
032294           END-IF                                                         
032295         END-IF                                                           
032300                                                                          
032400         IF NYCKLAR-OK                                                    
032500*          --- NYCKLAR ÄR FORMELLT RIKTIGA, KOLLA INBÖRDES                
032600           PERFORM BA-KOLLA-NYCKLAR-BORTTAG                               
032700         END-IF                                                           
032800       END-IF                                                             
032900*    -- END-NYCKLAR-OK                                                    
033000     END-IF                                                               
033100                                                                          
033200     .                                                                    
033300     EJECT                                                                
033400 BA-KOLLA-NYCKLAR-BORTTAG    SECTION.                                     
033500     SKIP2                                                                
033550*    CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
033560                                                                          
033600*    --- KOLLA BORTTAGSALTERNATIVEN FÖR ENTYDIGHET                        
033700     IF  W-IDARTNR    = ZERO                                              
033800     AND W-IDUPPDSU   = SPACE                                             
033900     AND W-IDUPPDSU-D   NUMERIC                                           
033910     AND W-IDUPPDSU-A = SPACE                                             
034000**     -- OK, BORTTAG AV ALLA ARTIKLAR PÅ ETT SU                          
034100       SET BORTTAG-IDUPPDSU TO TRUE                                       
034200     ELSE                                                                 
034300       IF W-IDARTNR = ZERO                                                
034400       AND W-IDUPPDSU = SPACE                                             
034500       AND W-IDUPPDSU-D = SPACE                                           
034600       AND W-IDUPPDSU-A NUMERIC                                           
034700**       -- OK, BORTTAG AV ALLA ANNUL-ARTIKLAR PÅ ETT SU                  
034701         SET BORTTAG-IDUPPDSU-ANNUL TO TRUE                               
034800       ELSE                                                               
034801         IF W-IDARTNR > ZERO                                              
034802         AND W-IDUPPDSU   NUMERIC                                         
034803         AND W-IDUPPDSU-D = SPACE                                         
034804         AND W-IDUPPDSU-A = SPACE                                         
034805**         --OK, BORTTAG AV ENSKILT ARTIKELNUMMER PÅ ETT SU               
034806           SET BORTTAG-IDARTNR  TO TRUE                                   
034820         ELSE                                                             
034900           IF W-IDARTNR   = ZERO                                          
035000           AND W-IDUPPDSU = SPACE                                         
035100           AND W-IDUPPDSU-D = SPACE                                       
035110           AND W-IDUPPDSU-A = SPACE                                       
035200*            --- ETT AV ALTERNATIVEN MÅSTE ANGES VID BORTTAG              
035300             MOVE NEJ TO NYCKLAR-SW                                       
035400             MOVE ERR-0-MUST-BE-ENTERED TO RESP-IDMSG-ERROR               
035500             MOVE 'ARTNR+SU/SUD'      TO RESP-IDELMT-ERROR                
035600           ELSE                                                           
035700**           --FEL, KONFLIKTANDE NYCKELKOMBINATION                        
035800             MOVE NEJ TO NYCKLAR-SW                                       
035900             MOVE ERR-2-CONFLICT-FIELDS TO RESP-IDMSG-ERROR               
036000             MOVE 'ARTNR+SU'          TO RESP-IDELMT-ERROR                
036100           END-IF                                                         
036110           MOVE SPACE TO BORTTAG-SW                                       
036200         END-IF                                                           
036210       END-IF                                                             
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600                                                                          
036700 C-KOLLA-FLYTTA-INDATA    SECTION.                                        
036800     SKIP2                                                                
036900     MOVE JA TO INDATA-SW                                                 
037000*    -- LÄSER IN BEFINTLIGA DATA I ACCE-TAB                               
037100     PERFORM DB2-SELECT-TB1ACCE-TAB                                       
037200                                                                          
037300     IF RADER-FINNS                                                       
037400*      -- KOLLA INDATA OCH FYLL PÅ I ACCE-AREA                            
037500*      --- OBS ALLA MID-FÄLT ÄR X-DEKL.                                   
037600*IDPRODGR                                                                 
037700       IF REQU-IDPRODGR NOT = ALL '+'                                     
037800*        IF REQU-IDPRODGR = SPACE                                         
037900*          MOVE '-'            TO ACCE-IDPRODGR                           
038000*        ELSE                                                             
038100           MOVE REQU-IDPRODGR  TO ACCE-IDPRODGR                           
038200*        END-IF                                                           
038300       END-IF                                                             
038400*BEASSTYP                                                                 
038500       IF REQU-BEASSTYP NOT = ALL '+'                                     
038600*        IF REQU-BEASSTYP = SPACE                                         
038700*          MOVE '-'           TO ACCE-BEASSTYP                            
038800*        ELSE                                                             
038900           MOVE REQU-BEASSTYP TO ACCE-BEASSTYP                            
039000*        END-IF                                                           
039100       END-IF                                                             
039200*KDFRPTYP                                                                 
039300       IF REQU-KDFRPTYP NOT = ALL '+'                                     
039400*        IF REQU-KDFRPTYP = SPACE                                         
039500*          MOVE '-'           TO ACCE-KDFRPTYP                            
039600*        ELSE                                                             
039700           MOVE REQU-KDFRPTYP TO ACCE-KDFRPTYP                            
039800*        END-IF                                                           
039900       END-IF                                                             
039910*KVYVOL-INT    "DECIDED LAUNCH VOL" BERÄKNAD INTROD. VOLYM                
039920       IF REQU-KVYVOL-INT   NOT = ALL '+'                                 
039930         IF REQU-KVYVOL-INT  NUMERIC                                      
039940           MOVE REQU-KVYVOL-INT  TO ACCE-KVYVOL-INT                       
039950         ELSE                                                             
039960           MOVE NEJ TO INDATA-SW                                          
039970           MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
039980           MOVE 'KVYVOL-INT'         TO RESP-IDELMT-ERROR                 
039990         END-IF                                                           
039991       END-IF                                                             
040000*KVYVOL-B3    "VOLUME BOARD 3"                                            
040200       IF REQU-KVYVOL-B3  NOT = ALL '+'                                   
040300         IF REQU-KVYVOL-B3  NUMERIC                                       
040400           MOVE REQU-KVYVOL-B3        TO ACCE-KVYVOL-B3                   
040500         ELSE                                                             
040600           MOVE NEJ TO INDATA-SW                                          
040700           MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
040800           MOVE 'KVYVOL-B3'          TO RESP-IDELMT-ERROR                 
040900           CONTINUE                                                       
041000         END-IF                                                           
041100       END-IF                                                             
041101*KVYVOL-B2    "VOLUME BOARD 2"                                            
041102       IF REQU-KVYVOL-B2  NOT = ALL '+'                                   
041103         IF REQU-KVYVOL-B2  NUMERIC                                       
041104           MOVE REQU-KVYVOL-B2        TO ACCE-KVYVOL-B2                   
041105         ELSE                                                             
041106           MOVE NEJ TO INDATA-SW                                          
041107           MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
041108           MOVE 'KVYVOL-B2'          TO RESP-IDELMT-ERROR                 
041109           CONTINUE                                                       
041110         END-IF                                                           
041111       END-IF                                                             
041112*KVYVOL-B1       "VOLUME BOARD 1"                                         
041130       IF REQU-KVYVOL-B1    NOT = ALL '+'                                 
041140         IF REQU-KVYVOL-B1   NUMERIC                                      
041150           MOVE REQU-KVYVOL-B1   TO ACCE-KVYVOL-B1                        
041160         ELSE                                                             
041170           MOVE NEJ TO INDATA-SW                                          
041180           MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
041190           MOVE 'KVYVOL-B1'           TO RESP-IDELMT-ERROR                
041191         END-IF                                                           
041192       END-IF                                                             
041193*KVYVOL-ASS      "ASSIGNMENT VOLUME"  BERÄKNAD ÅRSVOLYM                   
041194       IF REQU-KVYVOL-ASS   NOT = ALL '+'                                 
041195         IF REQU-KVYVOL-ASS  NUMERIC                                      
041196           MOVE REQU-KVYVOL-ASS  TO ACCE-KVYVOL-ASS                       
041197         ELSE                                                             
041198           MOVE NEJ TO INDATA-SW                                          
041199           MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
041200           MOVE 'KVYVOL-ASS'         TO RESP-IDELMT-ERROR                 
041201         END-IF                                                           
041202       END-IF                                                             
041203*BEMAPP              "BINDER"                                             
041204       IF REQU-BEMAPP NOT = ALL '+'                                       
041205*        IF REQU-BEMAPP = SPACE                                           
041206*          MOVE '-'      TO ACCE-BEMAPP                                   
041207*        ELSE                                                             
041208           MOVE REQU-BEMAPP TO ACCE-BEMAPP                                
041209*        END-IF                                                           
041210       END-IF                                                             
041211*KVFOTO          "PHOTO MTRL, QTY"                                        
041212       IF REQU-KVFOTO     NOT = ALL '+'                                   
041213         IF REQU-KVFOTO     NUMERIC                                       
041214           MOVE REQU-KVFOTO            TO ACCE-KVFOTO                     
041215         ELSE                                                             
041216           MOVE NEJ TO INDATA-SW                                          
041217           MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
041218           MOVE 'KVFOTO'              TO RESP-IDELMT-ERROR                
041219           CONTINUE                                                       
041220         END-IF                                                           
041221       END-IF                                                             
041222*TIFOTO          "PHOTO MTRL WEEK"                                        
041223       IF REQU-TIFOTO NOT = ALL '+'                                       
041224         IF REQU-TIFOTO NUMERIC                                           
041225           IF REQU-TIFOTO NOT = ZERO                                      
041226             MOVE 'AAVV'      TO DAT-KDDATFORM                            
041227             MOVE REQU-TIFOTO TO DAT-I-TIDATUM                            
041228             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
041229                                 DAT-O-TIDATUM DAT-KDSVAR                 
041230             IF DAT-KDSVAR-OK                                             
041231               MOVE REQU-TIFOTO TO ACCE-TIFOTO                            
041232             ELSE                                                         
041233               MOVE JA TO INDATA-SW                                       
041234               MOVE ERR-WRONG-DATE TO RESP-IDMSG-ERROR                    
041235               MOVE 'TIFOTO'       TO RESP-IDELMT-ERROR                   
041236             END-IF                                                       
041237           ELSE                                                           
041238             MOVE ZERO          TO ACCE-TIFOTO                            
041239           END-IF                                                         
041240         ELSE                                                             
041241           MOVE NEJ TO INDATA-SW                                          
041242           MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
041243           MOVE 'TIFOTO'              TO RESP-IDELMT-ERROR                
041244           CONTINUE                                                       
041245         END-IF                                                           
041246       END-IF                                                             
041247*TEVERKTYG           "TOOL PURCH INFO"                                    
041248       IF REQU-TEVERKTYG NOT = ALL '+'                                    
041252         MOVE REQU-TEVERKTYG TO ACCE-TEVERKTYG                            
041254       END-IF                                                             
041255                                                                          
041256*TESTATXT            "STA COMMENT"                                        
041257       IF REQU-TESTATXT NOT = ALL '+'                                     
041258         MOVE REQU-TESTATXT TO ACCE-TESTATXT                              
041259       END-IF                                                             
041260                                                                          
041261*TEMATXT            "MA COMMENT"                                          
041262       IF REQU-TEMATXT NOT = ALL '+'                                      
041263         MOVE REQU-TEMATXT TO ACCE-TEMATXT                                
041264       END-IF                                                             
041265                                                                          
041266*TEINKTXT            "INK COMMENT"                                        
041267       IF REQU-TEINKTXT NOT = ALL '+'                                     
041268         MOVE REQU-TEINKTXT TO ACCE-TEINKTXT                              
041269       END-IF                                                             
041270                                                                          
041271*TEANSTXT            "ANS COMMENT"                                        
041272       IF REQU-TEANSTXT NOT = ALL '+'                                     
041273         MOVE REQU-TEANSTXT TO ACCE-TEANSTXT                              
041274       END-IF                                                             
041275                                                                          
041276*TEAUXTXT            "EXTRA COMMENT NOTES FIELD"                          
041277       IF REQU-TEAUXTXT NOT = ALL '+'                                     
041278         MOVE REQU-TEAUXTXT TO ACCE-TEAUXTXT                              
041279       END-IF                                                             
041280                                                                          
041281     ELSE                                                                 
041282       MOVE NEJ TO INDATA-SW                                              
041283       MOVE ERR-0-NOT-FOUND TO RESP-IDMSG-ERROR                           
041284       MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                          
041285     END-IF                                                               
041286*                                                                         
041290* BERÄKNADE FÄLT UTGÅR VID ETRACKER 4459048                               
041300*                                                                         
049300*  KVMAM-N        "LAUNCH, US"        VAR FÖRUT "MAM NAFTA/NORDIC"        
050600*  KVMAM-E        "LAUNCH NORDIC, EU" VAR FÖRUT "MAM EUROPE"              
051900*  KVMAM-O        "LAUNCH, ASIA"     VAR FÖRUT "MAM JAP/VCAP/VCOC"        
053200*PRUNREQ            "TARGET UNIT COST"                                    
055100*PRPUREQ            "TARGET PU COST"                                      
057200*PRTOREQ            "TARGET TOOL COST"                                    
059200*PRTSUGRE               "TARGET SUGGESTED RETAIL PRICE"                   
061200*PRUNOFF              "OFFER UNIT COST"                                   
063100*PRPUOFF              "OFFER PU COST"                                     
065100*PRTOOFF              "OFFER TOOL COST"                                   
071800* KVYVOL-INT                                                              
073000* KVYVOL-LS                                                               
074300* SULSINTR                                                                
077200     .                                                                    
077300     EJECT                                                                
077400 D-UPPDATERA        SECTION.                                              
077500     SKIP2                                                                
077600     PERFORM DB2-UPDATE-TB1ACCE-TAB                                       
077700     IF NOT CURSOR-OK                                                     
077800       MOVE INF-NOTHING-UPDATED TO RESP-IDMSG-INFO                        
077900       MOVE ERR-SYSTEM-ERROR    TO RESP-IDMSG-ERROR                       
078000       MOVE 'TB1ACCE'           TO RESP-IDELMT-ERROR                      
078100     ELSE                                                                 
078200       MOVE INF-UPDATE-DONE     TO RESP-IDMSG-INFO                        
078300     END-IF                                                               
078400     .                                                                    
078500     EJECT                                                                
078600 F-LAES-VISA-INFO   SECTION.                                              
078700     SKIP2                                                                
078800     PERFORM DB2-SELECT-TB1ACCE-TAB                                       
078900     IF RADER-SAKNAS                                                      
079000       MOVE ERR-0-NOT-FOUND     TO RESP-IDMSG-ERROR                       
079100       MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR                      
079200     ELSE                                                                 
079300       PERFORM FA-LAES-GRUNDDATA                                          
079400     END-IF                                                               
079500     .                                                                    
079600     EJECT                                                                
079700 FA-LAES-GRUNDDATA SECTION.                                               
079800     SKIP2                                                                
079900*    --- ENDAST VISAFÄLT                                                  
080000     MOVE   ACCE-IDARTNR     TO RESP-IDARTNR                              
080100     MOVE   ACCE-IDUPPDSU    TO RESP-IDUPPDSU                             
080200     MOVE   ACCE-BEART       TO RESP-BEART                                
080300*    --- UPPDATERBARA FÄLT                                                
080400     MOVE   ACCE-IDPRODGR    TO RESP-IDPRODGR                             
080500     MOVE   ACCE-BEASSTYP    TO RESP-BEASSTYP                             
080600     MOVE   ACCE-KDFRPTYP    TO RESP-KDFRPTYP                             
080700     MOVE   ACCE-KVYVOL-INT  TO RESP-KVYVOL-INT                           
080800     MOVE   ACCE-KVYVOL-B3   TO RESP-KVYVOL-B3                            
080900     MOVE   ACCE-KVYVOL-B2   TO RESP-KVYVOL-B2                            
081000     MOVE   ACCE-KVYVOL-B1   TO RESP-KVYVOL-B1                            
081100     MOVE   ACCE-KVYVOL-ASS  TO RESP-KVYVOL-ASS                           
081200     MOVE   ACCE-BEMAPP      TO RESP-BEMAPP                               
081300     MOVE   ACCE-KVFOTO      TO RESP-KVFOTO                               
081400     MOVE   ACCE-TIFOTO      TO RESP-TIFOTO                               
081410     MOVE   ACCE-TEVERKTYG   TO RESP-TEVERKTYG                            
081420     MOVE   ACCE-TESTATXT    TO RESP-TESTATXT                             
081430     MOVE   ACCE-TEMATXT     TO RESP-TEMATXT                              
081440     MOVE   ACCE-TEINKTXT    TO RESP-TEINKTXT                             
081450     MOVE   ACCE-TEANSTXT    TO RESP-TEANSTXT                             
081460     MOVE   ACCE-TEAUXTXT    TO RESP-TEAUXTXT                             
081500     .                                                                    
081600     EJECT                                                                
081700                                                                          
081800 G-BORTTAG    SECTION.                                                    
081900     SKIP2                                                                
082000     IF BORTTAG-IDARTNR                                                   
082100       PERFORM DB2-DELARTIKEL-TB1ACCE-TAB                                 
082200                                                                          
082300       IF CURSOR-OK                                                       
082400         MOVE INF-UPDATE-DONE     TO RESP-IDMSG-INFO                      
082500       ELSE                                                               
082600         IF RADER-SAKNAS                                                  
082700           MOVE INF-NOTHING-UPDATED TO RESP-IDMSG-INFO                    
082800           MOVE ERR-0-NOT-FOUND   TO RESP-IDMSG-ERROR                     
082900           MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                    
083000         ELSE                                                             
083100           MOVE INF-NOTHING-UPDATED TO RESP-IDMSG-INFO                    
083200           MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                      
083300           MOVE 'TB1ACCE'       TO RESP-IDELMT-ERROR                      
083400         END-IF                                                           
083500       END-IF                                                             
083600     END-IF                                                               
083700                                                                          
083800     IF BORTTAG-IDUPPDSU                                                  
083900       PERFORM DB2-DELUPPDSU-TB1ACCE-TAB                                  
084000       IF CURSOR-OK                                                       
084100         MOVE INF-UPDATE-DONE     TO RESP-IDMSG-INFO                      
084200       ELSE                                                               
084300         IF RADER-SAKNAS                                                  
084400           MOVE INF-NOTHING-UPDATED TO RESP-IDMSG-INFO                    
084500           MOVE ERR-0-NOT-FOUND   TO RESP-IDMSG-ERROR                     
084600           MOVE 'SU      '        TO RESP-IDELMT-ERROR                    
084700         ELSE                                                             
084800           MOVE INF-NOTHING-UPDATED TO RESP-IDMSG-INFO                    
084900           MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                      
085000           MOVE 'TB1ACCE'       TO RESP-IDELMT-ERROR                      
085100         END-IF                                                           
085200       END-IF                                                             
085300     END-IF                                                               
085301                                                                          
085310     IF BORTTAG-IDUPPDSU-ANNUL                                            
085320       PERFORM DB2-DELUPPDSU-ANN-TB1ACCE-TAB                              
085330       IF CURSOR-OK                                                       
085340         MOVE INF-UPDATE-DONE     TO RESP-IDMSG-INFO                      
085350       ELSE                                                               
085360         IF RADER-SAKNAS                                                  
085370           MOVE INF-NOTHING-UPDATED TO RESP-IDMSG-INFO                    
085380           MOVE ERR-0-NOT-FOUND   TO RESP-IDMSG-ERROR                     
085390           MOVE 'SUA     '        TO RESP-IDELMT-ERROR                    
085391         ELSE                                                             
085392           MOVE INF-NOTHING-UPDATED TO RESP-IDMSG-INFO                    
085393           MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                      
085394           MOVE 'TB1ACCE'       TO RESP-IDELMT-ERROR                      
085395         END-IF                                                           
085396       END-IF                                                             
085397     END-IF                                                               
085400     .                                                                    
085500     EJECT                                                                
085600                                                                          
085700                                                                          
085800*    --- DISPATCHER-SEKTIONER                                             
085900 S01-HAEMTA-ANROPSDATA SECTION.                                           
086000                                                                          
086100     MOVE 'GETARG'               TO SUB-KDFUNC                            
086200     MOVE WS-ADDISPABS           TO SUB-ADDISPABS                         
086300     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
086400                                                                          
086500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
086600                                                                          
086700     IF SUB-KDRC > 0                                                      
086800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
086900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
087000       DELIMITED BY SIZE INTO FELTEXT                                     
087100       IF SUB-KDRC = 10                                                   
087200         STRING FELTEXT                   DELIMITED BY '    '             
087300         '. ADDRESS NOT FOUND IN ATAB '   DELIMITED BY SIZE               
087400                                          INTO FELTEXT                    
087500       END-IF                                                             
087600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
087700     END-IF                                                               
087800     .                                                                    
087900     SKIP3                                                                
088000 S02-RETURNERA-SVAR SECTION.                                              
088100                                                                          
088200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
088300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
088400                                                                          
088500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
088600                                                                          
088700     IF SUB-KDRC > 0                                                      
088800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
088900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
089000       DELIMITED BY SIZE INTO FELTEXT                                     
089100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
089200     END-IF                                                               
089300     .                                                                    
089400     EJECT                                                                
089500 S99-RENSA-OUTPUT-RAD SECTION.                                            
089600     SKIP2                                                                
089700     MOVE   ZERO             TO RESP-IDARTNR                              
089800     MOVE   SPACE            TO RESP-BEART                                
089900     MOVE   SPACE            TO RESP-IDPRODGR                             
090000     MOVE   SPACE            TO RESP-BEASSTYP                             
090100     MOVE   SPACE            TO RESP-KDFRPTYP                             
090200     MOVE   ZERO             TO RESP-KVYVOL-INT                           
090300     MOVE   ZERO             TO RESP-KVYVOL-B3                            
090400     MOVE   ZERO             TO RESP-KVYVOL-B2                            
090500     MOVE   ZERO             TO RESP-KVYVOL-B1                            
090600     MOVE   ZERO             TO RESP-KVYVOL-ASS                           
090700     MOVE   SPACE            TO RESP-BEMAPP                               
090800     MOVE   ZERO             TO RESP-KVFOTO                               
090900     MOVE   ZERO             TO RESP-TIFOTO                               
090910     MOVE   SPACE            TO RESP-TEVERKTYG                            
090920     MOVE   SPACE            TO RESP-TESTATXT                             
090930     MOVE   SPACE            TO RESP-TEMATXT                              
090940     MOVE   SPACE            TO RESP-TEINKTXT                             
090950     MOVE   SPACE            TO RESP-TEANSTXT                             
090960     MOVE   SPACE            TO RESP-TEAUXTXT                             
091500     .                                                                    
091600     EJECT                                                                
091700                                                                          
091800                                                                          
091900 DB2-SELECT-TB1ACCE-TAB  SECTION.                                         
092000     SKIP2                                                                
092100     MOVE 000100  TO GODK-SQLCODEKODER                                    
092200     EXEC SQL SELECT                                                      
092300                                                                          
092400          IDARTNR                                                         
092500         ,BEART                                                           
092600         ,IDPRODGR                                                        
092700         ,IDUPPDSU                                                        
092800         ,IDUPPDKU                                                        
092900         ,IDAOT                                                           
093000         ,TIAOINF                                                         
093100         ,BEASSTYP                                                        
093200         ,KDARTTYP                                                        
093300         ,KDMDS                                                           
093400         ,KDFRPTYP                                                        
093500         ,TEARTUTFG                                                       
093600         ,TESTATUPP                                                       
093700         ,IDLEVNR_GSDB                                                    
093800         ,KDTPD_PH1                                                       
093900         ,DATPDPH1                                                        
094200         ,DAPSWQA_1                                                       
094300         ,KDPSWQA_1                                                       
094600         ,DAPSWPA_2                                                       
094700         ,KDPSWPA_2                                                       
095000         ,DAPSWCA_3                                                       
095100         ,KDPSWCA_3                                                       
095200         ,KVYVOL_INT                                                      
095300         ,KVYVOL_B3                                                       
095400         ,KVYVOL_B2                                                       
095500         ,KVYVOL_B1                                                       
095600         ,KVYVOL_ASS                                                      
095700         ,BEMAPP                                                          
095800         ,KVFOTO                                                          
095900         ,TIFOTO                                                          
096000         ,TENOTE                                                          
096100         ,FLANNULL                                                        
096110         ,TEVERKTYG                                                       
096120         ,TESTATXT                                                        
096130         ,TEMATXT                                                         
096140         ,TEINKTXT                                                        
096150         ,TEANSTXT                                                        
096160         ,TEAUXTXT                                                        
096200                                                                          
096300         INTO                                                             
096400                                                                          
096500         :ACCE-IDARTNR                                                    
096600        ,:ACCE-BEART                                                      
096700        ,:ACCE-IDPRODGR                                                   
096800        ,:ACCE-IDUPPDSU                                                   
096900        ,:ACCE-IDUPPDKU                                                   
097000        ,:ACCE-IDAOT                                                      
097100        ,:ACCE-TIAOINF                                                    
097200        ,:ACCE-BEASSTYP                                                   
097300        ,:ACCE-KDARTTYP                                                   
097400        ,:ACCE-KDMDS                                                      
097500        ,:ACCE-KDFRPTYP                                                   
097600        ,:ACCE-TEARTUTFG                                                  
097700        ,:ACCE-TESTATUPP                                                  
097800        ,:ACCE-IDLEVNR-GSDB                                               
097900        ,:ACCE-KDTPD-PH1                                                  
098000        ,:ACCE-DATPDPH1                                                   
098300        ,:ACCE-DAPSWQA-1                                                  
098400        ,:ACCE-KDPSWQA-1                                                  
098700        ,:ACCE-DAPSWPA-2                                                  
098800        ,:ACCE-KDPSWPA-2                                                  
099100        ,:ACCE-DAPSWCA-3                                                  
099200        ,:ACCE-KDPSWCA-3                                                  
099300        ,:ACCE-KVYVOL-INT                                                 
099400        ,:ACCE-KVYVOL-B3                                                  
099500        ,:ACCE-KVYVOL-B2                                                  
099600        ,:ACCE-KVYVOL-B1                                                  
099700        ,:ACCE-KVYVOL-ASS                                                 
099800        ,:ACCE-BEMAPP                                                     
099900        ,:ACCE-KVFOTO                                                     
100000        ,:ACCE-TIFOTO                                                     
100100        ,:ACCE-TENOTE                                                     
100200        ,:ACCE-FLANNULL                                                   
100210        ,:ACCE-TEVERKTYG                                                  
100220        ,:ACCE-TESTATXT                                                   
100230        ,:ACCE-TEMATXT                                                    
100240        ,:ACCE-TEINKTXT                                                   
100250        ,:ACCE-TEANSTXT                                                   
100260        ,:ACCE-TEAUXTXT                                                   
100300                                                                          
100400         FROM    TB1ACCE                                                  
100500         WHERE   IDARTNR = :W-IDARTNR                                     
100600           AND   IDUPPDSU = :W-IDUPPDSU                                   
100700     END-EXEC                                                             
100800                                                                          
100900     MOVE SQLCODE TO SQLCODE-WS                                           
101000     PERFORM DB2-STATUS-KONTROLL                                          
101100     .                                                                    
101200     EJECT                                                                
101300 DB2-UPDATE-TB1ACCE-TAB  SECTION.                                         
101400                                                                          
101500     MOVE 000     TO GODK-SQLCODEKODER                                    
101600     EXEC SQL                                                             
101700         UPDATE TB1ACCE                                                   
101800         SET                                                              
101900         IDPRODGR    =  :ACCE-IDPRODGR,                                   
102000         BEASSTYP    =  :ACCE-BEASSTYP,                                   
102100         KDFRPTYP    =  :ACCE-KDFRPTYP,                                   
102200         KVYVOL_INT  =  :ACCE-KVYVOL-INT,                                 
102300         KVYVOL_B3   =  :ACCE-KVYVOL-B3,                                  
102400         KVYVOL_B2   =  :ACCE-KVYVOL-B2,                                  
102500         KVYVOL_B1   =  :ACCE-KVYVOL-B1,                                  
102600         KVYVOL_ASS  =  :ACCE-KVYVOL-ASS,                                 
102700         BEMAPP      =  :ACCE-BEMAPP,                                     
102800         KVFOTO      =  :ACCE-KVFOTO,                                     
102900         TIFOTO      =  :ACCE-TIFOTO,                                     
102910         TEVERKTYG   =  :ACCE-TEVERKTYG,                                  
102920         TESTATXT    =  :ACCE-TESTATXT,                                   
102930         TEMATXT     =  :ACCE-TEMATXT,                                    
102940         TEINKTXT    =  :ACCE-TEINKTXT,                                   
102950         TEANSTXT    =  :ACCE-TEANSTXT,                                   
102960         TEAUXTXT    =  :ACCE-TEAUXTXT                                    
103000                                                                          
103100         WHERE IDARTNR = :W-IDARTNR                                       
103200           AND IDUPPDSU = :W-IDUPPDSU                                     
103300                                                                          
103400     END-EXEC                                                             
103500                                                                          
103600     MOVE SQLCODE TO SQLCODE-WS                                           
103700     PERFORM DB2-STATUS-KONTROLL                                          
103800     .                                                                    
103900     EJECT                                                                
104000 DB2-DELARTIKEL-TB1ACCE-TAB  SECTION.                                     
104100     SKIP2                                                                
104200     MOVE 000100  TO GODK-SQLCODEKODER                                    
104300     EXEC SQL                                                             
104400         DELETE FROM TB1ACCE                                              
104500         WHERE IDARTNR = :W-IDARTNR                                       
104600           AND IDUPPDSU = :W-IDUPPDSU                                     
104700     END-EXEC                                                             
104800                                                                          
104900     MOVE SQLCODE TO SQLCODE-WS                                           
105000     PERFORM DB2-STATUS-KONTROLL                                          
105100     .                                                                    
105200     EJECT                                                                
105300 DB2-DELUPPDSU-TB1ACCE-TAB   SECTION.                                     
105400     SKIP2                                                                
105500     MOVE 000100  TO GODK-SQLCODEKODER                                    
105600     EXEC SQL                                                             
105700         DELETE FROM TB1ACCE                                              
105800         WHERE IDUPPDSU = :W-IDUPPDSU-D                                   
105900     END-EXEC                                                             
106000                                                                          
106100     MOVE SQLCODE TO SQLCODE-WS                                           
106200     PERFORM DB2-STATUS-KONTROLL                                          
106300     .                                                                    
106400     EJECT                                                                
106410 DB2-DELUPPDSU-ANN-TB1ACCE-TAB  SECTION.                                  
106411     MOVE 000100  TO GODK-SQLCODEKODER                                    
106412     EXEC SQL                                                             
106413         DELETE FROM TB1ACCE                                              
106414         WHERE IDUPPDSU = :W-IDUPPDSU-A                                   
106415           AND FLANNULL = :WS-ANNUL                                       
106416     END-EXEC                                                             
106417                                                                          
106418     MOVE SQLCODE TO SQLCODE-WS                                           
106419     PERFORM DB2-STATUS-KONTROLL                                          
106420     .                                                                    
106421     EJECT                                                                
106430                                                                          
106500 DB2-STATUS-KONTROLL  SECTION.                                            
106600                                                                          
106700     SET SQLCODE-IX TO 1                                                  
106800     SEARCH GODK-SQLCODE                                                  
106900       AT END                                                             
107000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
107100          DELIMITED BY SIZE INTO FELTEXT                                  
107200          CALL ABEND USING RKOD-ABEND-DB2                                 
107300       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
107400     END-SEARCH                                                           
107500     .                                                                    
