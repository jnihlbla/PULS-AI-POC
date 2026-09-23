000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3017310.                                                
000400 AUTHOR.         RONNY S.                                                 
000500 DATE-WRITTEN.   MAJ   92.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*    FUNKTION                                                             
000900*    FRÅGEPROGRAM DB2 OCH DL1                                             
001000*    INMATNINGSFÄLT ÄR BYTESNR (ARTNR)                                    
001100*    PROGRAMMET LÄSER  WLBENA OCH WLARTC-BASEN                            
001200*    SAMT TABELLERNA BYART BYPRO OCH BYLEV                                
001300*                                                                         
001400*    INDATA                                                               
001500*    TRANSAKTION  W3T173                                                  
001600*    MID          W3I17301                                                
001700*    MOD          W3O17301                                                
001800*                                                                         
001900*    CHANGE LOG:                                                          
002000*                                                                         
002100*    ETRACKER 793380 /EÖ                                                  
002200*                                                                         
002300*    ETRACKER 1072007 060815/EÖ                                           
002400*    ADD SCRAP COMMAND                                                    
002500*                                                                         
002600*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
002700*      ----------------------------------------------------------         
002800*      14/11/03 - REDDY RAHUL     - CHANGES FOR CHINA EXCHANGE            
002900*                                   E'TRACKER 10242148                    
003000*                                                                         
003100*      15/02/18 - REDDY RAHUL  - PRINT CORE LABELS IN MAASTRICHT          
003200*                                   E'TRACKER 10206694                    
003300*                                                                         
003400*      16/01/05 - REDDY RAHUL  - INCREASE TEBYTKVA FIELDS                 
003500*                                   E'TRACKER 10251642                    
003600*                                                                         
003700 ENVIRONMENT DIVISION.                                                    
003800     SKIP2                                                                
003900 DATA DIVISION.                                                           
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8) VALUE 'W3017310'.               
004500 01  SPAR-IDARTNR                PIC X(9) VALUE ZERO.                     
004600 01  WS-IDARTNR                  PIC X(9) VALUE ZERO.                     
004700 01  FILLER REDEFINES WS-IDARTNR.                                         
004800   03  FILLER                    PIC 9(5).                                
004900   03  WS-ARTSIFFRA              PIC 9(1).                                
005000     88  ART-0                   VALUE 6.                                 
005100     88  ART-1                   VALUE 4  7.                              
005200     88  ART-2                   VALUE 5  8.                              
005300     88  ART-3                   VALUE 9.                                 
005400 77  WS-IDDISTR                  PIC X(4) VALUE ZERO.                     
005500 77  WS-IDBYTRAD                 PIC X(5) VALUE ZERO.                     
005600 77  WS-IDBYTRAP                 PIC X(7) VALUE ZERO.                     
005700 77  WS-IDTABNR                  PIC X(3) VALUE ZERO.                     
005800 77  W-IDPRODNR                  PIC S9(9) COMP-3 VALUE ZERO.             
005900 77  W-BELEV                     PIC X(30) VALUE SPACE.                   
006000                                                                          
006100 77  KDRC-DISPLAY                PIC Z(5).                                
006200                                                                          
006300 01  WS-CURRENT-DATE-TIME.                                                
006400     03  WS-YEAR                 PIC 9(4).                                
006500     03  WS-MONTH                PIC 9(2).                                
006600     03  WS-DAY                  PIC 9(2).                                
006700     03  WS-HOUR                 PIC 9(2).                                
006800     03  WS-MINUTE               PIC 9(2).                                
006900 01  FILLER REDEFINES WS-CURRENT-DATE-TIME.                               
007000     03  FILLER                  PIC X(2).                                
007100     03  WS-TIYYMMDDHHMM         PIC X(10).                               
007200                                                                          
007300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
007400                                                                          
007500 01  ALL-SPACE.                                                           
007600     03  FILLER                  PIC X(80)  VALUE SPACE.                  
007700 01  ALL-SPACE-UTF8.                                                      
007800     03  FILLER                  PIC X(25)  VALUE ALL X'20'.              
007900 01  ALL-PLUS.                                                            
008000     03  FILLER                  PIC X(80)  VALUE ALL '+'.                
008100 01  ALL-PLUS-UTF8.                                                       
008200     03  FILLER                  PIC X(25)  VALUE ALL X'2B'.              
008300                                                                          
008400 01  WS-IDSKYLT-SE               PIC X(3)   VALUE 'S  '.                  
008500 01  WS-IDSKYLT-GB               PIC X(3)   VALUE 'GB '.                  
008600 01  WS-IDSKYLT-CN               PIC X(3)   VALUE 'RCN'.                  
008700 01  WS-CP-UNICODE               PIC X(4)   VALUE 'UTF8'.                 
008800 01  WS-CP-EBCDIC                PIC X(3)   VALUE '278'.                  
008900                                                                          
009000                                                                          
009100 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
009200 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
009300 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
009400 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
009500 01  FILLER                  PIC X(16)   VALUE 'WS-DB2-SEKTION'.          
009600 01  WS-DB2-SEKTION              PIC X(30)   VALUE SPACE.                 
009700                                                                          
009800 01  WS-IDLEVNR                  PIC 9(5).                                
009900 01  FILLER REDEFINES WS-IDLEVNR.                                         
010000    03  IDLEVNR-WS               PIC X(5).                                
010100 01  WS-IDDISTR-RENOV            PIC 9(5).                                
010200 01  FILLER REDEFINES WS-IDDISTR-RENOV.                                   
010300    03  IDDISTR-RENOV-WS         PIC X(5).                                
010400 01  WS-IDPRODNR                 PIC 9(8).                                
010500 01  FILLER REDEFINES WS-IDPRODNR.                                        
010600    03  IDPRODNR-WS              PIC X(8).                                
010700 77  JA                          PIC X       VALUE 'J'.                   
010800 77  NEJ                         PIC X       VALUE 'N'.                   
010900 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
011000 77  MAX-RAD                     PIC S9(9)   VALUE +3   COMP SYNC.        
011100 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
011200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +783  COMP SYNC.        
011300 77  W-IDARTNR-BYT               PIC S9(9) COMP-3 VALUE ZERO.             
011400*                                                                         
011500*FIXED VALUES FOR INDX                                                    
011600*                                                                         
011700 77  MAX-QTY-BELEV             PIC S9(4)   VALUE +4  COMP.                
011800                                                                          
011900*                                                                         
012000 77  BYT-SW                      PIC X       VALUE 'J'.                   
012100     88  BYT-BILD                            VALUE 'J'.                   
012200     88  BYT-EJ-BILD                         VALUE 'N'.                   
012240*                                                                         
012250 77  WDK7-SW                     PIC X       VALUE 'N'.                   
012260     88  WDK7-OK                             VALUE 'J'.                   
012300*                                                                         
012400 01  SW-NYCKLAR-OK               PIC X.                                   
012500   88  NYCKLAR-OK                          VALUE 'J'.                     
012600 01  SW-UPPDATERINGAR            PIC X.                                   
012700   88  INGA-UPPDATERINGAR                  VALUE 'N'.                     
012800 01  SW-INDATA-OK               PIC X.                                    
012900   88  INDATA-OK                          VALUE 'J'.                      
013000 01  SW-INMATAT                 PIC X.                                    
013100   88  INGET-INMATAT                      VALUE 'N'.                      
013200*                                                                         
013300*                                                                         
013400 01  FILLER                      PIC X(16)   VALUE                        
013500                                            'NYCKLAR-TILL-DLI'.           
013600 01  NYCKLAR-TILL-DLI.                                                    
013700                                                                          
013800   03  W-IDDC-X.                                                          
013900     05 W-IDDC                   PIC X(2)    VALUE SPACE.                 
014000                                                                          
014100   03  W-IDARTNR-X.                                                       
014200     05  W-IDARTNR               PIC S9(9) COMP-3 VALUE ZERO.             
014300   03  W-KDSEGKEY-X.                                                      
014400     05  FILLER                  PIC X(1)    VALUE '1'  .                 
014500   03  W-IDLEVNR-X.                                                       
014600     05  W-IDLEVNR               PIC S9(5) COMP-3 VALUE ZERO.             
014700   03    W-KDNOTTYP-X.                                                    
014800     05    W-KDNOTTYP            PIC S9(1)  VALUE ZERO  COMP-3.           
014900   03  W-IDSKYLT-X.                                                       
015000     05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                     
         03  W-IDDC-B6-X.                                                       
             05 W-IDDC-B6            PIC X(2).                                  
015100     EJECT                                                                
015200 01  DYNAMISKA-SUBPROGRAM.                                                
015300   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
015400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
015500   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
015600   03  WTRAUTF8                  PIC X(8)    VALUE 'WTRAUTF8'.            
015700   03  WZ04CRUL                  PIC X(8)    VALUE 'WZ04CRUL'.            
015800   03  WZ01SEND                  PIC X(8)    VALUE 'WZ01SEND'.            
015900     SKIP3                                                                
016000*    --- PARAMETRAR TILL SUBPROGRAM                                       
016100*                                                                         
016200 01  FILLER                 PIC X(16)   VALUE 'WTRAUTF8-AREA   '.         
016300*01 -COPY WTRAUTF8                                                        
016400     SKIP3                                                                
016500 01  FILLER                      PIC X(16)   VALUE 'WZ04CRUL'.            
016600*01 -COPY WZ04CRUL                                                        
016700                                                                          
016800 01  FILLER                      PIC X(08)   VALUE 'WZ01SEND'.            
016900*01  -COPY WZ01SEND                                                       
017000 01  FILLER                      PIC X(08)  VALUE 'W3017201'.             
017100 01  SEND-AREA-TO-CORE-LABEL.                                             
017200*03  -COPY W3017201                                                       
017300*                                                                         
017400                                                                          
017500 01  HDR-AREA.                                                            
017600*    03  -COPY WZ01REQU -PRE HDR-                                         
017700*    03  -COPY WZ04HDR                                                    
017800                                                                          
017900*    --- VALID IDDC CODES                                                 
018000*                                                                         
018100 01  FILLER                      PIC X(16)   VALUE 'IDDC CODES'.          
018200     SKIP3                                                                
018300*01 -COPY WWDC99                                                          
018400*01 -COPY WWDCKONS                                                        
018500     SKIP3                                                                
018600 01  MESSAGE-CODES.                                                       
018700     03  INF-UPDATE-DONE        PIC X(3)    VALUE '001'.                  
018800     03  INF-FIRST-PAGE         PIC X(3)    VALUE '010'.                  
018900     03  INF-MORE-INFO-EXISTS   PIC X(3)    VALUE '011'.                  
019000     03  INF-PRESS-PF11         PIC X(3)    VALUE '013'.                  
019100     03  INF-PRINT-REQUESTED    PIC X(3)    VALUE '376'.                  
019200     03  ERR-PF11-AND-NO-DATA   PIC X(3)    VALUE '014'.                  
019300     03  ERR-CORR-HILITE-FLDS   PIC X(3)    VALUE '020'.                  
019400     03  ERR-IS-INVALID         PIC X(3)    VALUE '023'.                  
019500     03  ERR-NOT-FOUND          PIC X(3)    VALUE '025'.                  
019600     03  ERR-WRONG-KEY          PIC X(3)    VALUE '043'.                  
019700     03  WRONG-PRINTER          PIC X(3)    VALUE '347'.                  
019800     03  ERR-DATABASE-UNAVAILABLE   PIC X(3)    VALUE '395'.              
019900     03  INF-TRY-LATER-OR-NOTIFY    PIC X(3)    VALUE '396'.              
020000     03  INF-CHECK-MISSING-PARTS    PIC X(3)    VALUE '397'.              
020100                                                                          
020200     EJECT                                                                
020300*- - - - - - - - - - - - - - - - - - - - CORE LABEL LAYOUT                
020400 01  FILLER                      PIC X(16)   VALUE 'CORE LABEL'.          
020500 01  CORE-DATE.                                                           
020600     03  LBL-TIAAAA              PIC X(4).                                
020700     03  FILLER                  PIC X       VALUE '-'.                   
020800     03  LBL-TIMM                PIC X(2).                                
020900     03  FILLER                  PIC X       VALUE '-'.                   
021000     03  LBL-TIDD                PIC X(2).                                
021100                                                                          
021200 01  CORE-TIME.                                                           
021300     03  LBL-TIHH                PIC X(2).                                
021400     03  FILLER                  PIC X       VALUE ':'.                   
021500     03  LBL-TIMIN               PIC X(2).                                
021600                                                                          
021700*- - - - - - - - - - - - - - - - - - - BYTES-ARTIKELTEST                  
021800 01  FILLER                      PIC X(16)   VALUE 'BYTES-ART'.           
021900 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
022000*01  FILLER -COPY WWBYT16   -RED TEST-IDARTNR                             
022100 01  TEST-KDBYTREF               PIC X(3).                                
022200*01  FILLER -COPY WWBYT15   -RED TEST-KDBYTREF                            
022300     EJECT                                                                
022400******************************************************************        
022500*                                                                         
022600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
022700*                                                                         
022800*                                                                         
022900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
023000*01  -COPY WMFSAREA                                                       
023100     EJECT                                                                
023200******************************************************************        
023300*                                                                         
023400*        ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
023500*                                                                         
023600 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
023700 01  FILLER                  PIC X(16)   VALUE 'BYART-COPYTEXTS'.         
023800*01  -COPY BYART -PRE BYART-                                              
023900     EJECT                                                                
024000 01  FILLER                  PIC X(16)   VALUE 'BYPRO-COPY '.             
024100*01  -COPY BYPRO -PRE BYPRO-                                              
024200     EJECT                                                                
024300 01  FILLER                  PIC X(16)   VALUE 'BYLEV-COPY '.             
024400*01  -COPY BYLEV -PRE BYLEV-                                              
024500     EJECT                                                                
024600 01  FILLER                  PIC X(16)   VALUE 'BYART-AREA'.              
024700       EXEC SQL INCLUDE BYART END-EXEC.                                   
024800     SKIP3                                                                
024900 01  FILLER                  PIC X(16) VALUE 'BYPRO-AREA'.                
025000       EXEC SQL INCLUDE BYPRO END-EXEC.                                   
025100     SKIP3                                                                
025200 01  FILLER                  PIC X(16) VALUE 'BYLEV-AREA'.                
025300       EXEC SQL INCLUDE BYLEV END-EXEC.                                   
025400     SKIP3                                                                
025500 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
025600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
025700*                        **** STATUS-KOD FRÅN DB2                         
025800 01  FILLER                     PIC X(16) VALUE 'SQLCODE-WS'.             
025900 01  DB2-WS.                                                              
026000   03  SQLCODE-WS                PIC 9(3) VALUE ZERO.                     
026100     88  CURSOR-OK                           VALUE 000.                   
026200     88  RADER-FINNS                         VALUE 000.                   
026300     88  RADER-SAKNAS                        VALUE 100.                   
026400     88  904-KOD                             VALUE 904.                   
026500     SKIP1                                                                
026600   03  GODK-SQLCODESKODER.                                                
026700     05  GODK-SQLCODE OCCURS 5                                            
026800         INDEXED BY SQLCODE-IX PIC 999.                                   
026900     EJECT                                                                
027000 01  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.           
027100 01  IMS-WS.                                                              
027200     SKIP3                                                                
027300*                        **** STATUS-KOD FRÅN IMS                         
027400   03  STATUS-WS                 PIC XX.                                  
027500     88  SEGMENT-FINNS                       VALUE '  '.                  
027600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027700     88  BASEN-SLUT                          VALUE 'GB'.                  
027800     SKIP3                                                                
027900   03  GODK-STATUSKODER.                                                  
028000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028100     SKIP3                                                                
028200 01    SSA1                      PIC X(64).                               
028300 01    SSA2                      PIC X(64).                               
028400 01    SSA3                      PIC X(64).                               
028500*                            IMS FUNKTIONSKODER                           
028600*01    -COPY W0003                                                        
028700     EJECT                                                                
028800     SKIP3                                                                
028900                                                                          
029000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
029100 01  DLI-IO-WLARTC01.                                                     
029200*    03  -COPY WDK601                                                     
029300     EJECT                                                                
029400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
029500 01  DLI-IO-WLARTC11.                                                     
029600*    03  -COPY WDK611                                                     
029700     EJECT                                                                
029800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
029900 01  DLI-IO-WLBENA11.                                                     
030000*   03 BENA11  -COPY WDD311   -PRE BENA11-                                
030100     EJECT                                                                
030200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711  '.                    
030300 01  DLI-IO-WDK711.                                                       
030400*    03  -COPY WDK711                                                     
030500     EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
       01   DLI-IO-AREA-B601.                                                   
      *     03  -COPY WDB601                                                    
           EJECT                                                                
030600*                                                                         
030700 LINKAGE SECTION.                                                         
030800                                                                          
030900 01  REQU-AREA.                                                           
031000*    03 -COPY WZ01REQU                                                    
031100*    03 -COPY W30173I1                                                    
031200*                                                                         
031300 01  RESP-AREA.                                                           
031400*    03 -COPY WZ01RESP                                                    
031500*    03 -COPY W30173O1                                                    
031600*                                                                         
031700                                                                          
031800 01  MAX-KVRADER                PIC S9(4) COMP.                           
031900                                                                          
032000*01  -COPY W0009     -PRE DISTRDOC-                                       
032100     EJECT                                                                
032200*01  -COPY W0008     -PRE XXCP-                                           
032300     05  FILLER                 PIC X.                                    
032400     EJECT                                                                
032500*01  -COPY W0008     -PRE ARTC-                                           
032600     05  FILLER                 PIC X.                                    
032700     SKIP2                                                                
032800*01  -COPY W0008     -PRE BENA-                                           
032900     05  FILLER                 PIC X.                                    
033000     SKIP2                                                                
033100*01  -COPY W0008     -PRE WDK7-                                           
033200     05  FILLER                 PIC X.                                    
033300     EJECT                                                                
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
033400 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
033500              DISTRDOC-PCB BENA-PCB ARTC-PCB WDK7-PCB WDB6-PCB.           
033600                                                                          
033700 MAIN SECTION.                                                            
033800     PERFORM A-INIT                                                       
033900     PERFORM B-INIT-KEYS                                                  
034000     IF NYCKLAR-OK                                                        
034100*                                                                         
034200       PERFORM C-CHECK-KDPGMACT                                           
034300       PERFORM D-VISA-SIDAN                                               
034400     ELSE                                                                 
034500       PERFORM S05-RENSA-SIDAN                                            
034600     END-IF                                                               
034700                                                                          
034800     MOVE ZERO   TO RETURN-CODE                                           
034900     GOBACK                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 A-INIT               SECTION.                                            
035300     MOVE 'A-INIT               ' TO WS-SEKTION                           
035400     SKIP2                                                                
035500                                                                          
035600     MOVE ALL '+'                 TO RESP-W30173O1                        
035700     MOVE 001                     TO RESP-IDMSGVER                        
035800                                                                          
035900     MOVE ZERO                    TO RESP-KVRADER                         
036000                                                                          
036100     MOVE SPACE                   TO RESP-IDMSG-ERROR                     
036200                                     RESP-IDMSG-INFO                      
036300                                     RESP-IDELMT-ERROR                    
036400                                     RESP-REG-FLAGGA-ATTR                 
036500                                     RESP-REG-FLAGGA                      
036600                                                                          
036700     INITIALIZE GODK-SQLCODESKODER                                        
036800                                                                          
036900     MOVE NEJ TO BYT-SW                                                   
037000     .                                                                    
037100     EJECT                                                                
037200 B-INIT-KEYS      SECTION.                                                
037300     MOVE 'B-INIT-KEYS      ' TO WS-SEKTION                               
037400                                                                          
037500     MOVE REQU-IDDC-KEY              TO W-IDDC                            
037600                                        WS-IDDC                           
                                              W-IDDC-B6                         
037700                                                                          
037800     MOVE JA                         TO SW-NYCKLAR-OK                     
037810     MOVE NEJ                        TO WDK7-SW                           
037900     IF REQU-IDDISTR-KEY = ALL '+'                                        
038000        MOVE ZERO                    TO WS-IDDISTR                        
038100     ELSE                                                                 
038200        MOVE REQU-IDDISTR-KEY        TO WS-IDDISTR                        
038300     END-IF                                                               
038400                                                                          
038500     IF REQU-IDBYTRAD-KEY = ALL '+'                                       
038600        MOVE SPACE                    TO WS-IDBYTRAD                      
038700        INSPECT WS-IDBYTRAD REPLACING LEADING SPACE BY ZERO               
038800     ELSE                                                                 
038900        MOVE REQU-IDBYTRAD-KEY        TO WS-IDBYTRAD                      
039000        INSPECT WS-IDBYTRAD REPLACING LEADING SPACE BY ZERO               
039100     END-IF                                                               
039200                                                                          
039300     IF REQU-IDBYTRAP-KEY = ALL '+'                                       
039400        MOVE ZERO                     TO WS-IDBYTRAP                      
039500     ELSE                                                                 
039600        MOVE REQU-IDBYTRAP-KEY        TO WS-IDBYTRAP                      
039700     END-IF                                                               
039800                                                                          
039900     IF REQU-IDARTNR-KEY = ALL '+'                                        
040000        MOVE ZERO                    TO WS-IDARTNR                        
040100     ELSE                                                                 
040200        MOVE REQU-IDARTNR-KEY        TO WS-IDARTNR                        
040300*       SET REQU-FIRST TO TRUE                                            
040400     END-IF                                                               
040500                                                                          
040600     IF NOT GOOD-DC                                                       
040700        MOVE NEJ                     TO SW-NYCKLAR-OK                     
040800        MOVE 'IDDC'                  TO RESP-IDELMT-ERROR                 
040900        MOVE ERR-IS-INVALID          TO RESP-IDMSG-ERROR                  
041000     END-IF                                                               
041100                                                                          
041200     IF WS-IDARTNR    NUMERIC                                             
041300        MOVE WS-IDARTNR TO TEST-IDARTNR                                   
041400                           W-IDARTNR                                      
041500        IF BYT16-RADIO                                                    
041600           IF BYT16-RADIO-EXTRA                                           
041700             MOVE 0               TO WS-ARTSIFFRA                         
041800           ELSE                                                           
041900             MOVE 3               TO WS-ARTSIFFRA                         
042000           END-IF                                                         
042100        ELSE                                                              
042200           IF ART-0                                                       
042300              MOVE 0              TO WS-ARTSIFFRA                         
042400           ELSE                                                           
042500             IF ART-1                                                     
042600                MOVE 1            TO WS-ARTSIFFRA                         
042700             ELSE                                                         
042800               IF ART-2                                                   
042900                  MOVE 2          TO WS-ARTSIFFRA                         
043000               ELSE                                                       
043100                 IF ART-3                                                 
043200                    MOVE 3        TO WS-ARTSIFFRA                         
043300                 END-IF                                                   
043400               END-IF                                                     
043500             END-IF                                                       
043600           END-IF                                                         
043700        END-IF                                                            
043800        MOVE WS-IDARTNR         TO W-IDARTNR-BYT                          
043900                                                                          
044000        PERFORM IMS-GU-ARTC01                                             
044100        IF SEGMENT-FINNS                                                  
044200           MOVE ART-KDPRODSL         TO RESP-KDPRODSL                     
044300           MOVE ART-IDFKNGRP         TO RESP-IDFKNGRP                     
044400                                                                          
044500           PERFORM IMS-GET-WLARTC11                                       
044600           IF SEGMENT-FINNS                                               
044700           AND CLAG-PRARTSJK > ZERO                                       
044800              IF CDC-SE                                                   
044802                 CONTINUE                                                 
044803              ELSE                                                        
044804                MOVE W-IDARTNR-BYT     TO W-IDARTNR                       
044806                IF BYT16-RADIO                                            
044807                  ADD +1000            TO W-IDARTNR                       
044808                ELSE                                                      
044809                  ADD +6000            TO W-IDARTNR                       
044810                END-IF                                                    
044812                PERFORM IMS-GU-WDK711                                     
044813                IF SEGMENT-FINNS                                          
044814                  MOVE JA TO WDK7-SW                                      
044815                END-IF                                                    
044820              END-IF                                                      
044900           ELSE                                                           
045000              MOVE NEJ               TO SW-NYCKLAR-OK                     
045100              MOVE 'IDARTNR-OBJ'     TO RESP-IDELMT-ERROR                 
045200              MOVE ERR-IS-INVALID    TO RESP-IDMSG-ERROR                  
045300           END-IF                                                         
045400        ELSE                                                              
045500           MOVE NEJ                  TO SW-NYCKLAR-OK                     
045600           MOVE 'IDARTNR-OBJ'        TO RESP-IDELMT-ERROR                 
045700           MOVE ERR-NOT-FOUND        TO RESP-IDMSG-ERROR                  
045800        END-IF                                                            
045900     ELSE                                                                 
046000        MOVE NEJ                     TO SW-NYCKLAR-OK                     
046100        MOVE 'IDARTNR-OBJ'           TO RESP-IDELMT-ERROR                 
046200        MOVE ERR-IS-INVALID          TO RESP-IDMSG-ERROR                  
046300     END-IF                                                               
046400                                                                          
046500     IF REQU-IDTABNR-KEY = ALL '+'                                        
046600        MOVE ZERO                     TO WS-IDTABNR                       
046700     ELSE                                                                 
046800        MOVE REQU-IDTABNR-KEY         TO WS-IDTABNR                       
046900     END-IF                                                               
047000                                                                          
047100     .                                                                    
047200     EJECT                                                                
047300 C-CHECK-KDPGMACT  SECTION.                                               
047400     MOVE 'C-CHECK-KDPGMACT'  TO  WS-SEKTION                              
047500     SKIP2                                                                
047600     IF  REQU-IDPRODNR-LO NUMERIC                                         
047700     AND REQU-IDPRODNR-HI NUMERIC                                         
047800        CONTINUE                                                          
047900     ELSE                                                                 
048000        MOVE ZERO                     TO REQU-IDPRODNR-LO                 
048100                                         REQU-IDPRODNR-HI                 
048200     END-IF                                                               
048300                                                                          
048400*    IF MFS-IDPFK = '8'                                                   
048500     IF REQU-NEXT                                                         
048600        IF  REQU-IDPRODNR-HI = ZERO                                       
048700        AND REQU-BELEV-HI = SPACE                                         
048800*          MOVE '7'                   TO MFS-IDPFK                        
048900          SET REQU-FIRST TO TRUE                                          
049000        END-IF                                                            
049100     END-IF                                                               
049200                                                                          
049300     IF REQU-NEXT                                                         
049400        IF  REQU-IDPRODNR-HI = ZERO                                       
049500           MOVE REQU-IDPRODNR-LO      TO W-IDPRODNR                       
049600        ELSE                                                              
049700           MOVE REQU-IDPRODNR-HI      TO W-IDPRODNR                       
049800                                                                          
049900        END-IF                                                            
050000                                                                          
050100        IF  REQU-BELEV-HI = SPACE                                         
050200           MOVE REQU-BELEV-LO         TO W-BELEV                          
050300        ELSE                                                              
050400           MOVE REQU-BELEV-HI         TO W-BELEV                          
050500        END-IF                                                            
050600     ELSE                                                                 
050700                                                                          
050800        IF REQU-KDPGMACT = ' '                                            
050900           MOVE REQU-BELEV-LO         TO W-BELEV                          
051000           MOVE REQU-IDPRODNR-LO      TO W-IDPRODNR                       
051100        ELSE                                                              
051200            MOVE LOW-VALUE            TO W-BELEV                          
051300            MOVE ZERO                 TO W-IDPRODNR                       
051400            MOVE INF-FIRST-PAGE       TO RESP-IDMSG-INFO                  
051500        END-IF                                                            
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900 D-VISA-SIDAN SECTION.                                                    
052000     MOVE 'D-VISA-SIDAN'  TO  WS-SEKTION                                  
052100     SKIP2                                                                
052200     IF BYT-EJ-BILD                                                       
052300                                                                          
052400       MOVE REQU-KDPRT TO RESP-KDPRT                                      
052500       PERFORM DB2-SELECT-BYART                                           
052600       IF RADER-FINNS                                                     
                PERFORM IMS-GU-WDB601                                           
                                                                                
                MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                         
                IF DCS-UNICODE-IDSKYLT                                          
                   MOVE 'UTF8'             TO TRAUTF8-KDCP                      
                ELSE                                                            
                   MOVE '278 '             TO TRAUTF8-KDCP                      
                END-IF                                                          
054000                                                                          
054100          PERFORM IMS-GU-BENA11                                           
054200          IF SEGMENT-FINNS                                                
054300            MOVE BENA11-TEXT-BEART  TO TRAUTF8-TECONV-FROM                
054400          ELSE                                                            
054500            MOVE SPACE              TO TRAUTF8-TECONV-FROM                
054600                                       BENA11-TEXT-BEART                  
054700            MOVE WS-CP-EBCDIC       TO TRAUTF8-KDCP                       
054800          END-IF                                                          
                IF TRAUTF8-TECONV-FROM = SPACES                                 
                 MOVE 'GB'  TO W-IDSKYLT                                        
                 MOVE '278' TO TRAUTF8-KDCP                                     
                 PERFORM IMS-GU-BENA11                                          
                 MOVE BENA11-TEXT-BEART    TO TRAUTF8-TECONV-FROM               
                END-IF                                                          
054900          IF REQU-IDMSGVER = '101'                                        
055000*           -- USE AS-IS IF CLASSIC CALL                                  
055100            MOVE BENA11-TEXT-BEART  TO RESP-BEART-SVE                     
055200          ELSE                                                            
055300*           -- TANSLATE TO UTF8 IF WEB CALL                               
055400            CALL WTRAUTF8 USING TRAUTF8-AREA                              
055500            MOVE TRAUTF8-TECONV-TO TO RESP-BEART-SVE                      
055600          END-IF                                                          
055700                                                                          
055800          PERFORM DB-FLYTT-BYART-T-BILD                                   
055900          PERFORM DC-FLYTT-BYPRO-T-BILD                                   
056000          PERFORM DD-FLYTT-BYLEV-T-BILD                                   
056100          PERFORM DE-MOVE-CORE-STOCK-WDK711                               
056200          PERFORM DF-RENSA-INMATAT                                        
056300          IF REQU-IDMSGVER = '101' AND                                    
056400             REQU-PRINT AND REQU-IDDC-KEY = WC-SDC-NL-ET                  
056500            IF (REQU-KDPRT = ALL '+' OR                                   
056600                REQU-KDPRT = SPACE)                                       
056700              MOVE MFS-ALFA-FAELT-FEL                                     
056800                                 TO RESP-KDPRT-ATTR                       
056900              MOVE WRONG-PRINTER TO RESP-IDMSG-ERROR                      
057000            ELSE                                                          
057100              PERFORM DG-KONTROLLERA-PRINTER                              
057200              IF CRUL-KDRC = ZERO                                         
057300                MOVE '1        ' TO CORE-IDAFPRCD                         
057400                MOVE REQU-IDARTNR-KEY                                     
057500                                 TO CORE-IDARTNR-OBJ                      
057600                MOVE FUNCTION CURRENT-DATE(1:12)                          
057700                                 TO WS-CURRENT-DATE-TIME                  
057800                MOVE WS-YEAR     TO LBL-TIAAAA                            
057900                MOVE WS-MONTH    TO LBL-TIMM                              
058000                MOVE WS-DAY      TO LBL-TIDD                              
058100                MOVE WS-HOUR     TO LBL-TIHH                              
058200                MOVE WS-MINUTE   TO LBL-TIMIN                             
058300                MOVE CORE-DATE   TO CORE-PRINT-DATE                       
058400                MOVE CORE-TIME   TO CORE-TIHHMM                           
058500                MOVE REQU-IDUSER TO CORE-IDUSER                           
058521                MOVE BENA11-TEXT-BEART                                    
058522                                 TO CORE-BEART                            
058600                MOVE BYART-ADLAGOMR                                       
058700                                 TO CORE-ADLAGOMR                         
058800                MOVE BYART-ADGANG                                         
058900                                 TO CORE-ADGANG                           
059000                MOVE BYART-ADPLATS                                        
059100                                 TO CORE-ADPLATS                          
059200                PERFORM S21-SEND-OPEN                                     
059300                PERFORM S22-PUT-HEADER                                    
059400                PERFORM S25-PUT-LINE                                      
059500                PERFORM S29-SEND-CLOSE                                    
059600                MOVE INF-PRINT-REQUESTED                                  
059700                                 TO RESP-IDMSG-INFO                       
059800              ELSE                                                        
059900                MOVE MFS-ALFA-FAELT-FEL                                   
060000                                 TO RESP-KDPRT-ATTR                       
060100                MOVE WRONG-PRINTER                                        
060200                                 TO RESP-IDMSG-ERROR                      
060300              END-IF                                                      
060400            END-IF                                                        
060500          END-IF                                                          
060600       ELSE                                                               
060700                                                                          
060800          PERFORM S05-RENSA-SIDAN                                         
060900          IF 904-KOD                                                      
061000             MOVE ERR-DATABASE-UNAVAILABLE TO RESP-IDMSG-ERROR            
061100             MOVE INF-TRY-LATER-OR-NOTIFY  TO RESP-IDMSG-INFO             
061200          ELSE                                                            
061300             MOVE 'IDARTNR-OBJ'      TO RESP-IDELMT-ERROR                 
061400             MOVE ERR-NOT-FOUND      TO RESP-IDMSG-ERROR                  
061500          END-IF                                                          
061600       END-IF                                                             
061700     END-IF                                                               
061800     .                                                                    
061900     EJECT                                                                
062000 DB-FLYTT-BYART-T-BILD   SECTION.                                         
062500                                                                          
062600     MOVE 'DB-FLYTT-BYART-T-BILD' TO  WS-SEKTION                          
062700     SKIP2                                                                
062800     MOVE BYART-BETFLEV            TO RESP-BETFLEV                        
062900     MOVE MFS-ADD-LYS-UPP-FAELT    TO RESP-BETFLEV-ATTR                   
063000                                                                          
063100     IF SDC OR CDC                                                        
063200        MOVE BYART-IDDISTR-RENOV      TO RESP-IDDISTR-RENOV               
063300     ELSE                                                                 
063400       IF NDC-NA                                                          
063500         IF NDC-US                                                        
063600            MOVE BYART-IDDISTR-RENOV-NDC  TO RESP-IDDISTR-RENOV           
063700         ELSE                                                             
063800            MOVE BYART-IDDISTR-RENOV-CAN  TO RESP-IDDISTR-RENOV           
063900         END-IF                                                           
064000       ELSE                                                               
064100         IF NDC-JP                                                        
064200          MOVE BYART-IDDISTR-RENOV-PAC  TO RESP-IDDISTR-RENOV             
064300         END-IF                                                           
064400         IF NDC-AU                                                        
064500          MOVE BYART-IDDISTR-RENOV-AUS  TO RESP-IDDISTR-RENOV             
064600         END-IF                                                           
064700         IF NDC-CN                                                        
064800          MOVE BYART-IDDISTR-RENOV-CHN  TO RESP-IDDISTR-RENOV             
064900         END-IF                                                           
064901         IF NDC-KR                                                        
064902          MOVE BYART-IDDISTR-RENOV-KOR  TO RESP-IDDISTR-RENOV             
064903         END-IF                                                           
064904         IF NDC-MY                                                        
064905          MOVE BYART-IDDISTR-RENOV-MY   TO RESP-IDDISTR-RENOV             
064906         END-IF                                                           
064907         IF NDC-TW                                                        
064908          MOVE BYART-IDDISTR-RENOV-TW   TO RESP-IDDISTR-RENOV             
064909         END-IF                                                           
064910         IF NDC-TH                                                        
064920          MOVE BYART-IDDISTR-RENOV-TH   TO RESP-IDDISTR-RENOV             
064930         END-IF                                                           
065000       END-IF                                                             
065100     END-IF                                                               
065210     IF RESP-IDDISTR-RENOV = '  82'                                       
065220        MOVE MFS-ADD-HILIGHT-FIELD    TO RESP-IDDISTR-RENOV-ATTR          
065230     END-IF                                                               
065300*KVALITETSNOTERING BYTESOBJEKT                                            
065400     MOVE BYART-TEBYTKVA1          TO RESP-TEBYTKVA1                      
065500     MOVE BYART-TEBYTKVA2          TO RESP-TEBYTKVA2                      
065600     MOVE BYART-TEBYTKVA3          TO RESP-TEBYTKVA3                      
065700     MOVE BYART-TEBYTKVA4          TO RESP-TEBYTKVA4                      
065800                                                                          
066310     IF CDC-SE                                                            
066311       MOVE CLAG-ADLAGOMR       TO RESP-ADLAGOMR                          
066312       MOVE CLAG-ADGANG         TO RESP-ADGANG                            
066313       MOVE CLAG-ADPLATS        TO RESP-ADPLATS                           
066320     ELSE                                                                 
066330       IF SDC-NL-ET                                                       
066400         MOVE BYART-ADLAGOMR    TO RESP-ADLAGOMR                          
066500         MOVE BYART-ADGANG      TO RESP-ADGANG                            
066600         MOVE BYART-ADPLATS     TO RESP-ADPLATS                           
066610       ELSE                                                               
066611         IF WDK7-OK                                                       
066612           MOVE SLAG-ADLAGOMR   TO RESP-ADLAGOMR                          
066613           MOVE SLAG-ADGANG     TO RESP-ADGANG                            
066614           MOVE SLAG-ADPLATS    TO RESP-ADPLATS                           
066615         END-IF                                                           
066620       END-IF                                                             
066700     END-IF                                                               
066800                                                                          
066900     MOVE BYART-KVBYTPKO           TO RESP-KVBYTPKO                       
066910     IF SDC-NL-ET                                                         
067000       MOVE BYART-KVLS-MAXCORE       TO RESP-KVLS-MAXCORE                 
067010     ELSE                                                                 
067020       MOVE ZERO                     TO RESP-KVLS-MAXCORE                 
067030     END-IF                                                               
067100                                                                          
067200     IF BYART-FLBYTKTL = 'J' OR 'Y'                                       
067300                                                                          
067400       MOVE INF-CHECK-MISSING-PARTS TO RESP-DELAR-SAKNAS                  
067500       MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-DELAR-SAKNAS-ATTR               
067600                                                                          
067610     ELSE                                                                 
067620       MOVE SPACE TO RESP-DELAR-SAKNAS                                    
067630     END-IF                                                               
067640     .                                                                    
067650     EJECT                                                                
067700 DC-FLYTT-BYPRO-T-BILD SECTION.                                           
067800     SKIP1                                                                
067900     PERFORM DB2-DCL-OPN-CRS-BYPRO                                        
068000     PERFORM DB2-FETCH-BYPRO                                              
068100                                                                          
068200     IF RADER-FINNS                                                       
068300        PERFORM DCA-FLYTTA-FRA-BYPRO                                      
068400        PERFORM DB2-CLOSE-BYPRO-CRS                                       
068500     ELSE                                                                 
068600        PERFORM DCB-RENSA-BYPRO                                           
068700     END-IF                                                               
068800     .                                                                    
068900     EJECT                                                                
069000 DCA-FLYTTA-FRA-BYPRO SECTION.                                            
069100     MOVE 'DCA-FLYTT-FRA-BYPRO' TO  WS-SEKTION                            
069200                                                                          
069300     MOVE BYPRO-IDARTNR               TO RESP-IDPRODNR-LO                 
069400     MOVE +1                          TO RAD-INDX                         
069500     MOVE ZERO                        TO RESP-KVRADER                     
069600     PERFORM UNTIL NOT RADER-FINNS                                        
069700     OR RAD-INDX > MAX-KVRADER                                            
069800        MOVE BYPRO-IDARTNR            TO RESP-IDARTNR (RAD-INDX)          
069900        MOVE RAD-INDX                 TO RESP-KVRADER                     
070000        PERFORM DB2-FETCH-BYPRO                                           
070100        ADD +1                        TO RAD-INDX                         
070200     END-PERFORM                                                          
070300                                                                          
070400     IF RADER-FINNS                                                       
070500        IF REQU-UPDATE                                                    
070600           CONTINUE                                                       
070700        ELSE                                                              
070800           MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                   
070900        END-IF                                                            
071000        MOVE BYPRO-IDARTNR            TO RESP-IDPRODNR-HI                 
071100     ELSE                                                                 
071200        MOVE ZERO                     TO RESP-IDPRODNR-HI                 
071300        PERFORM UNTIL RAD-INDX > MAX-KVRADER                              
071400           MOVE ALL-SPACE             TO RESP-IDARTNR (RAD-INDX)          
071500           ADD +1                     TO RAD-INDX                         
071600        END-PERFORM                                                       
071700     END-IF                                                               
071800     .                                                                    
071900     EJECT                                                                
072000 DCB-RENSA-BYPRO SECTION.                                                 
072100     MOVE 'DCB-RENSA-BYPRO' TO  WS-SEKTION                                
072200     SKIP2                                                                
072300     MOVE ZERO                        TO RESP-IDPRODNR-LO                 
072400                                         RESP-IDPRODNR-HI                 
072500     MOVE ZERO                        TO RESP-KVRADER                     
072600     MOVE +1                          TO RAD-INDX                         
072700     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
072800        MOVE ALL-SPACE                TO RESP-IDARTNR (RAD-INDX)          
072900        ADD +1                        TO RAD-INDX                         
073000     END-PERFORM                                                          
073100     .                                                                    
073200     EJECT                                                                
073300 DD-FLYTT-BYLEV-T-BILD SECTION.                                           
073400     MOVE 'DD-FLYTT-BYLEV-T-BILD' TO  WS-SEKTION                          
073500     SKIP1                                                                
073600     PERFORM DB2-DCL-OPN-CRS-BYLEV                                        
073700     PERFORM DB2-FETCH-BYLEV                                              
073800                                                                          
073900     IF RADER-FINNS                                                       
074000        PERFORM DDA-FLYTTA-FRA-BYLEV                                      
074100        PERFORM DB2-CLOSE-BYLEV-CRS                                       
074200     ELSE                                                                 
074300        PERFORM DDB-RENSA-BYLEV                                           
074400     END-IF                                                               
074500     .                                                                    
074600     EJECT                                                                
074700 DDA-FLYTTA-FRA-BYLEV SECTION.                                            
074800     MOVE 'DDA-FLYTT-FRA-BYLEV' TO  WS-SEKTION                            
074900                                                                          
075000     MOVE BYLEV-BELEV                 TO RESP-BELEV-LO                    
075100     MOVE +1                          TO RAD-INDX                         
075200     PERFORM UNTIL NOT RADER-FINNS                                        
075300     OR RAD-INDX > MAX-QTY-BELEV                                          
075400        MOVE BYLEV-BELEV              TO RESP-BELEV (RAD-INDX)            
075500        PERFORM DB2-FETCH-BYLEV                                           
075600        ADD +1                        TO RAD-INDX                         
075700     END-PERFORM                                                          
075800                                                                          
075900     IF RADER-FINNS                                                       
076000        IF REQU-UPDATE                                                    
076100           CONTINUE                                                       
076200        ELSE                                                              
076300           IF REQU-IDMSGVER = '101'                                       
076400             MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                 
076500           END-IF                                                         
076600        END-IF                                                            
076700        MOVE BYLEV-BELEV              TO RESP-BELEV-HI                    
076800     ELSE                                                                 
076900        MOVE SPACE                    TO RESP-BELEV-HI                    
077000        PERFORM UNTIL RAD-INDX > MAX-QTY-BELEV                            
077100           MOVE ALL-SPACE             TO RESP-BELEV (RAD-INDX)            
077200           ADD +1                     TO RAD-INDX                         
077300        END-PERFORM                                                       
077400     END-IF                                                               
077500     .                                                                    
077600     EJECT                                                                
077700 DDB-RENSA-BYLEV SECTION.                                                 
077800     MOVE 'DDB-RENSA-BYLEV' TO  WS-SEKTION                                
077900     SKIP2                                                                
078000     MOVE SPACE                       TO RESP-BELEV-LO                    
078100                                         RESP-BELEV-HI                    
078200     MOVE +1                          TO RAD-INDX                         
078300     PERFORM UNTIL RAD-INDX > MAX-QTY-BELEV                               
078400        MOVE ALL-SPACE                TO RESP-BELEV (RAD-INDX)            
078500        ADD +1                        TO RAD-INDX                         
078600     END-PERFORM                                                          
078700     .                                                                    
078800     EJECT                                                                
078900 DF-RENSA-INMATAT SECTION.                                                
079000     MOVE 'DF-RENSA-INMAT' TO  WS-SEKTION                                 
079100     SKIP2                                                                
079200     MOVE ALL-SPACE                TO RESP-REG-FLAGGA                     
079300     .                                                                    
079400     EJECT                                                                
079500 S05-RENSA-SIDAN SECTION.                                                 
079600     MOVE 'S05-RENSA-SIDAN' TO  WS-SEKTION                                
079700     SKIP2                                                                
079800     MOVE ALL-SPACE                  TO RESP-BEART-SVE                    
079900                                        RESP-BETFLEV                      
080000                                                                          
080100                                        RESP-ADLAGOMR                     
080200                                        RESP-ADGANG                       
080300                                        RESP-ADPLATS                      
080400                                                                          
080500                                        RESP-DELAR-SAKNAS                 
080600                                        RESP-IDDISTR-RENOV                
080700                                        RESP-KVLS                         
080800                                        RESP-KVLS-MAXCORE                 
080900                                                                          
081000                                        RESP-TEBYTKVA1                    
081100                                        RESP-TEBYTKVA2                    
081200                                        RESP-TEBYTKVA3                    
081300                                        RESP-TEBYTKVA4                    
081400                                                                          
081500     IF REQU-IDMSGVER = '001'                                             
081600       MOVE ALL X'20'                TO RESP-BEART-SVE                    
081700     END-IF                                                               
081800     MOVE +1                         TO RAD-INDX                          
081900     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
082000        MOVE ALL-SPACE               TO RESP-IDARTNR (RAD-INDX)           
082100        ADD +1                       TO RAD-INDX                          
082200     END-PERFORM                                                          
082300                                                                          
082400     MOVE +1                         TO RAD-INDX                          
082500     PERFORM UNTIL RAD-INDX > MAX-QTY-BELEV                               
082600        MOVE ALL-SPACE               TO RESP-BELEV  (RAD-INDX)            
082700        ADD +1                       TO RAD-INDX                          
082800     END-PERFORM                                                          
082900     .                                                                    
083000     EJECT                                                                
083100 DE-MOVE-CORE-STOCK-WDK711 SECTION.                                       
083200                                                                          
083210     IF CDC-SE                                                            
083215       MOVE CLAG-KVLS                TO RESP-KVLS                         
083220     ELSE                                                                 
084101       IF WDK7-OK                                                         
084102         MOVE SLAG-KVLS              TO RESP-KVLS                         
084103         IF SDC-NL-ET                                                     
084104           IF BYART-KVLS-MAXCORE NOT =   ZERO AND                         
084105              SLAG-KVLS >= BYART-KVLS-MAXCORE                             
084106             MOVE MFS-ADD-HILIGHT-FIELD  TO RESP-KVLS-ATTR                
084107           END-IF                                                         
084108         END-IF                                                           
084400       END-IF                                                             
084410     END-IF                                                               
084500     .                                                                    
084600     EJECT                                                                
084700 DG-KONTROLLERA-PRINTER SECTION.                                          
084800                                                                          
084900     MOVE 'DG-KONTROLLERA-PRINTER'                                        
085000                                 TO WS-SEKTION                            
085100     MOVE 'CORE-LABEL'           TO CRUL-IDOUTTYPE                        
085200     MOVE REQU-KDPRT             TO CRUL-IDOUTREC                         
085300     CALL WZ04CRUL USING CRUL-WZ04CRUL                                    
085400                                                                          
085500     .                                                                    
085600     EJECT                                                                
085700                                                                          
085800 S21-SEND-OPEN SECTION.                                                   
085900     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
086000     MOVE 'OPEN'                  TO SEND-KDFUNC                          
086100     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
086200                                     SEND-OPEN-AREA                       
086300     IF SEND-KDRC > ZERO                                                  
086400       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
086500       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
086600       DELIMITED BY SIZE INTO FELTEXT                                     
086700       DISPLAY FELTEXT                                                    
086800       CALL FELLOG                                                        
086900     END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200 S22-PUT-HEADER SECTION.                                                  
087300     MOVE 1                       TO HDR-REQU-IDMSGVER                    
087400     MOVE 'R'                     TO HDR-REQU-KDPGMACT                    
087500     MOVE IDPGM                   TO HDR-REQU-IDUSER                      
087600     MOVE 'CORE-LABEL'            TO HDR-IDOUTTYPE                        
087700     MOVE REQU-KDPRT              TO HDR-IDOUTREC                         
087800     MOVE WS-TIYYMMDDHHMM         TO HDR-IDLIST                           
087900     MOVE 'PUT'                   TO SEND-KDFUNC                          
088000     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
088100     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
088200                                     SEND-KVDLEN                          
088300                                     HDR-AREA                             
088400     IF SEND-KDRC > ZERO                                                  
088500       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
088600       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
088700       DELIMITED BY SIZE       INTO FELTEXT                               
088800       DISPLAY FELTEXT                                                    
088900       CALL FELLOG                                                        
089000     END-IF                                                               
089100     .                                                                    
089200     EJECT                                                                
089300 S25-PUT-LINE SECTION.                                                    
089400                                                                          
089500     MOVE 'PUT'                   TO SEND-KDFUNC                          
089600     MOVE LENGTH OF SEND-AREA-TO-CORE-LABEL  TO SEND-KVDLEN               
089700     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
089800                                     SEND-KVDLEN                          
089900                                     SEND-AREA-TO-CORE-LABEL              
090000     IF SEND-KDRC > ZERO                                                  
090100       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
090200       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
090300       DELIMITED BY SIZE       INTO FELTEXT                               
090400       DISPLAY FELTEXT                                                    
090500       CALL FELLOG                                                        
090600     END-IF                                                               
090700     .                                                                    
090800     SKIP2                                                                
090900 S29-SEND-CLOSE SECTION.                                                  
091000     MOVE 'CLOSE'                TO SEND-KDFUNC                           
091100     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
091200     IF SEND-KDRC > 0                                                     
091300       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
091400       STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                    
091500       DELIMITED BY SIZE       INTO FELTEXT                               
091600       DISPLAY FELTEXT                                                    
091700       CALL FELLOG                                                        
091800     END-IF                                                               
091900     .                                                                    
092000     EJECT                                                                
092100* IMS SEKTIONER                                                           
092200     SKIP3                                                                
092300 IMS-GU-ARTC01 SECTION.                                                   
092400     MOVE 'IMS-GU-ARTC01' TO  WS-IMS-SEKTION                              
092500     SKIP1                                                                
092600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
092700            DELIMITED BY SIZE INTO SSA1                                   
092800     MOVE '  GE' TO GODK-STATUSKODER                                      
092900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
093000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
093100     PERFORM IMS-STATUSKONTROLL                                           
093200     SKIP3                                                                
093300     .                                                                    
093400 IMS-GET-WLARTC11 SECTION.                                                
093500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
093600          DELIMITED BY SIZE INTO SSA1                                     
093700     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
093800          DELIMITED BY SIZE INTO SSA2                                     
093900     MOVE '  GE' TO GODK-STATUSKODER                                      
094000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2             
094100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
094200     PERFORM IMS-STATUSKONTROLL                                           
094300     .                                                                    
094400     SKIP3                                                                
094500 IMS-GU-BENA11 SECTION.                                                   
094600     MOVE 'IMS-GU-BENA11' TO  WS-IMS-SEKTION                              
094700     SKIP1                                                                
094800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
094900            DELIMITED BY SIZE INTO SSA1                                   
095000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
095100            DELIMITED BY SIZE INTO SSA2                                   
095200     MOVE '  GE' TO GODK-STATUSKODER                                      
095300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
095400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
095500     PERFORM IMS-STATUSKONTROLL                                           
095600     SKIP3                                                                
095700     .                                                                    
095800 IMS-GU-WDK711 SECTION.                                                   
095900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
096000     DELIMITED BY SIZE INTO SSA1                                          
096100     STRING 'WDK711  (IDDC     =' W-IDDC-X    ')'                         
096200     DELIMITED BY SIZE INTO SSA2                                          
096300     MOVE '  GE' TO GODK-STATUSKODER                                      
096400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711   SSA1 SSA2             
096500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
096600     PERFORM IMS-STATUSKONTROLL                                           
096700     .                                                                    
       IMS-GU-WDB601    SECTION.                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
096800 IMS-STATUSKONTROLL SECTION.                                              
096900     SKIP1                                                                
097000     SET STATUS-IX TO 1                                                   
097100     SEARCH GODK-STATUS                                                   
097200       AT END                                                             
097300         CALL FELLOG                                                      
097400        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
097500           CONTINUE                                                       
097600     END-SEARCH                                                           
097700     .                                                                    
097800     EJECT                                                                
097900 DB2-DCL-OPN-CRS-BYPRO SECTION.                                           
098000     MOVE 'DB2-DCL-OPN-CRS-BYPRO' TO  WS-DB2-SEKTION                      
098100* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
098200     EXEC SQL DECLARE BYPRO-CRS CURSOR FOR                                
098300              SELECT IDARTNR                                              
098400              FROM BYPRO                                                  
098500              WHERE IDARTNR >= :W-IDPRODNR                                
098600              AND IDARTNR_BYT = :W-IDARTNR-BYT                            
098700              ORDER BY IDARTNR                                            
098800     END-EXEC                                                             
098900     MOVE 000               TO GODK-SQLCODESKODER                         
099000     EXEC SQL OPEN BYPRO-CRS END-EXEC                                     
099100     MOVE SQLCODE           TO SQLCODE-WS                                 
099200     PERFORM DB2-STATUSKONTROLL                                           
099300     .                                                                    
099400     EJECT                                                                
099500 DB2-DCL-OPN-CRS-BYLEV SECTION.                                           
099600     MOVE 'DB2-DCL-OPN-CRS-BYLEV' TO  WS-DB2-SEKTION                      
099700     EXEC SQL DECLARE BYLEV-CRS CURSOR FOR                                
099800              SELECT BELEV                                                
099900              FROM BYLEV                                                  
100000              WHERE BELEV >= :W-BELEV                                     
100100              AND IDARTNR_BYT = :W-IDARTNR-BYT                            
100200              ORDER BY BELEV                                              
100300     END-EXEC                                                             
100400     MOVE 000               TO GODK-SQLCODESKODER                         
100500     EXEC SQL OPEN BYLEV-CRS END-EXEC                                     
100600     MOVE SQLCODE           TO SQLCODE-WS                                 
100700     PERFORM DB2-STATUSKONTROLL                                           
100800     .                                                                    
100900     EJECT                                                                
101000 DB2-FETCH-BYPRO SECTION.                                                 
101100     MOVE 'DB2-FETCH-BYPRO' TO  WS-DB2-SEKTION                            
101200     MOVE 000100            TO GODK-SQLCODESKODER                         
101300     EXEC SQL FETCH BYPRO-CRS INTO                                        
101400            :BYPRO-IDARTNR                                                
101500     END-EXEC                                                             
101600     MOVE SQLCODE           TO SQLCODE-WS                                 
101700     PERFORM DB2-STATUSKONTROLL                                           
101800     .                                                                    
101900 DB2-FETCH-BYLEV SECTION.                                                 
102000     MOVE 'DB2-FETCH-BYLEV' TO  WS-DB2-SEKTION                            
102100     MOVE 000100            TO GODK-SQLCODESKODER                         
102200     EXEC SQL FETCH BYLEV-CRS INTO                                        
102300            :BYLEV-BELEV                                                  
102400     END-EXEC                                                             
102500     MOVE SQLCODE           TO SQLCODE-WS                                 
102600     PERFORM DB2-STATUSKONTROLL                                           
102700     .                                                                    
102800     EJECT                                                                
102900 DB2-SELECT-BYART SECTION.                                                
103000     MOVE 'DB2-SELECT-BYART' TO  WS-DB2-SEKTION                           
103100     MOVE 000100904         TO GODK-SQLCODESKODER                         
103200     EXEC SQL SELECT                                                      
103300                  IDARTNR_BYT,                                            
103400                  BETFLEV, TEBYTNOT1,                                     
103500                  TEBYTNOT2, TEBYTNOT3,                                   
103600                  TEBYTNOT4,                                              
103700                  IDDISTR_RENOV,                                          
103800                  ADLAGOMR, ADGANG,                                       
103900                  ADPLATS,                                                
104000                  FLBYTKTL,                                               
104100                  KVBYTPKO, TEBYTKVA1,                                    
104200                  TEBYTKVA2,                                              
104300                  TEBYTKVA3,                                              
104400                  TEBYTKVA4,                                              
104500                  IDDISTR_RENOV_NDC,                                      
104600                  IDDISTR_RENOV_PAC,                                      
104700                  IDDISTR_RENOV_CAN,                                      
104800                  IDDISTR_RENOV_AUS,                                      
104900                  KVLS_MAXCORE,                                           
105000                  IDDISTR_RENOV_CHN,                                      
105010                  IDDISTR_RENOV_KOR,                                      
105020                  IDDISTR_RENOV_MY,                                       
105030                  IDDISTR_RENOV_TW,                                       
105040                  IDDISTR_RENOV_TH                                        
105100              INTO                                                        
105200                  :BYART-IDARTNR-BYT,                                     
105300                  :BYART-BETFLEV, :BYART-TEBYTNOT1,                       
105400                  :BYART-TEBYTNOT2, :BYART-TEBYTNOT3,                     
105500                  :BYART-TEBYTNOT4,                                       
105600                  :BYART-IDDISTR-RENOV,                                   
105700                  :BYART-ADLAGOMR, :BYART-ADGANG,                         
105800                  :BYART-ADPLATS,                                         
105900                  :BYART-FLBYTKTL,                                        
106000                  :BYART-KVBYTPKO, :BYART-TEBYTKVA1,                      
106100                  :BYART-TEBYTKVA2,                                       
106200                  :BYART-TEBYTKVA3,                                       
106300                  :BYART-TEBYTKVA4,                                       
106400                  :BYART-IDDISTR-RENOV-NDC,                               
106500                  :BYART-IDDISTR-RENOV-PAC,                               
106600                  :BYART-IDDISTR-RENOV-CAN,                               
106700                  :BYART-IDDISTR-RENOV-AUS,                               
106800                  :BYART-KVLS-MAXCORE,                                    
106900                  :BYART-IDDISTR-RENOV-CHN,                               
106910                  :BYART-IDDISTR-RENOV-KOR,                               
106920                  :BYART-IDDISTR-RENOV-MY,                                
106930                  :BYART-IDDISTR-RENOV-TW,                                
106940                  :BYART-IDDISTR-RENOV-TH                                 
107000            FROM BYART                                                    
107100            WHERE IDARTNR_BYT = :W-IDARTNR-BYT                            
107200     END-EXEC                                                             
107300     MOVE SQLCODE           TO SQLCODE-WS                                 
107400     PERFORM DB2-STATUSKONTROLL                                           
107500     .                                                                    
107600 DB2-CLOSE-BYLEV-CRS SECTION.                                             
107700     MOVE 'DB2-CLOSE-BYLEV-CRS' TO  WS-DB2-SEKTION                        
107800     SKIP2                                                                
107900     EXEC SQL CLOSE BYLEV-CRS END-EXEC                                    
108000     .                                                                    
108100     EJECT                                                                
108200 DB2-CLOSE-BYPRO-CRS SECTION.                                             
108300     MOVE 'DB2-CLOSE-BYPRO-CRS' TO  WS-DB2-SEKTION                        
108400     SKIP2                                                                
108500     EXEC SQL CLOSE BYPRO-CRS END-EXEC                                    
108600     .                                                                    
108700     EJECT                                                                
108800 DB2-STATUSKONTROLL SECTION.                                              
108900     SKIP2                                                                
109000     SET SQLCODE-IX          TO 1                                         
109100     SEARCH GODK-SQLCODE                                                  
109200       AT END                                                             
109300         CALL FELLOG                                                      
109400        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
109500           CONTINUE                                                       
109600     END-SEARCH                                                           
109700     .                                                                    
