000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3017300.                                                
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
001400*    CORE INSPECTION                                                      
001500*                                                                         
001600*    INDATA                                                               
001700*    TRANSAKTION  W3T173                                                  
001800*    MID          W3I17301                                                
001900*    MOD          W3O17301                                                
002000*                                                                         
002100*    ETRACKER 793380 / EÖ                                                 
002200*                                                                         
002300*    ETRACKER 1072007 060815/EÖ                                           
002400*    ADD SCRAP COMMAND                                                    
002500*                                                                         
002600*      ETRACKER 10206694.  FEB 2015      /RAHUL REDDY                     
002700*      PRINT CORE LABELS IN MAASTRICHT.                                   
002800*                                                                         
002900*      ETRACKER 10251642.  JAN 2016      /RAHUL REDDY                     
003000*      INCREASE TEBYTKVA FIELDS                                           
003100*                                                                         
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3017300'.               
004000                                                                          
004100 01  FILLER                      PIC X(16) VALUE 'WS-SEKTION'.            
004200 01  WS-SEKTION                  PIC X(30) VALUE SPACE.                   
004300 01  FILLER                      PIC X(16) VALUE 'WS-IMS-SEKTION'.        
004400 01  WS-IMS-SEKTION              PIC X(30) VALUE SPACE.                   
004500                                                                          
004600 01  WS-IDARTNR                  PIC X(9) VALUE ZERO.                     
004700 77  WS-IDDISTR                  PIC X(4) VALUE ZERO.                     
004800 77  WS-IDBYTRAD                 PIC X(5) VALUE ZERO.                     
004900 77  WS-IDBYTRAP                 PIC X(7) VALUE ZERO.                     
005000 77  WS-IDTABNR                  PIC X(3) VALUE ZERO.                     
005100                                                                          
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
005500*                                                                         
005600 77  MAX-KVRADER                 PIC S9(4)   VALUE +12  COMP.             
005700*                                                                         
005800 77  BYT-SW                      PIC X       VALUE 'J'.                   
005900     88  BYT-BILD                            VALUE 'J'.                   
006000     88  BYT-EJ-BILD                         VALUE 'N'.                   
006100*                                                                         
006200 77  ANDRAT-ARTNR-SW             PIC X       VALUE 'J'.                   
006300     88  ANDRAT-ARTNR                        VALUE 'J'.                   
006400     88  ANDRAT-EJ-ARTNR                     VALUE 'N'.                   
006500*                                                                         
006600 01  SW-NYCKLAR-OK               PIC X.                                   
006700   88  NYCKLAR-OK                          VALUE 'J'.                     
006800                                                                          
006900 77  W-IDTRANS                   PIC X(4)  VALUE SPACE.                   
007000   88  EGEN-MID                            VALUE '3173'.                  
007100   88  GODK-MID                            VALUE  '3172'                  
007200                                                 '3173' '3174'            
007300                                                 '3175' '3176'            
007400                                                 '3177' '3178'            
007500                                                 '3179'.                  
007600   88  HELP-MID                            VALUE '0551'.                  
007700                                                                          
007800 01  FILLER                      PIC X(16)   VALUE                        
007900                                            'NYCKLAR-TILL-DLI'.           
008000 01  NYCKLAR-TILL-DLI.                                                    
008100                                                                          
008200   03  W-IDDC-X.                                                          
008300     05 W-IDDC                   PIC X(2)    VALUE SPACE.                 
008400                                                                          
008500   03  W-IDARTNR-X.                                                       
008600     05  W-IDARTNR               PIC S9(9) COMP-3 VALUE ZERO.             
008700   03  W-KDSEGKEY-X.                                                      
008800     05  FILLER                  PIC X(1)    VALUE '1'  .                 
008900   03  W-IDLEVNR-X.                                                       
009000     05  W-IDLEVNR               PIC S9(5) COMP-3 VALUE ZERO.             
009100   03    W-KDNOTTYP-X.                                                    
009200     05    W-KDNOTTYP            PIC S9(1)  VALUE ZERO  COMP-3.           
009300   03  W-IDSKYLT-X.                                                       
009400     05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                     
009500   03  W-WDGX3140-KEY-X.                                                  
009600     05   FILLER             PIC X(4)    VALUE '3139'.                    
009700     05   FILLER             PIC X(26)   VALUE LOW-VALUE.                 
009800     EJECT                                                                
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000   03  WMEDKONV                  PIC X(8)    VALUE 'WMEDKONV'.            
010100   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
010200   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
010300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
010400   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
010500   03  W3017310                  PIC X(8)    VALUE 'W3017310'.            
010600   03  WL01MCNV                  PIC X(8)    VALUE 'WL01MCNV'.            
010700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010800*01 -COPY WMEDAREA                                                        
010900     SKIP3                                                                
011000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011300     SKIP3                                                                
011400*01 -COPY WMSGINIT                                                        
011500     SKIP3                                                                
011600*                                                                         
011700 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV'.            
011800     SKIP3                                                                
011900*01 -COPY WL01MCNV                                                        
012000     SKIP3                                                                
012100*    --- VALID IDDC CODES                                                 
012200 01  FILLER                      PIC X(16)   VALUE 'IDDC CODES'.          
012300     SKIP3                                                                
012400*01 -COPY WWDC99                                                          
012500                                                                          
012600*01 -COPY WWDCKONS                                                        
012700     SKIP3                                                                
012800 01  MESSAGE-CODES.                                                       
012900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013500     03  ERR-CORE-NO-CHANGED     PIC X(3)    VALUE '360'.                 
013600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013700*                                                                         
013800     EJECT                                                                
013900******************************************************************        
014000*                                                                         
014100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
014200*                                                                         
014300 01  W-PROG-TO-PROG-SW.                                                   
014400     03  M-SW-LL                 PIC S9(4)   VALUE +367 COMP SYNC.        
014500     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
014600     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W3T172  '.            
014700     03  M-SW-IDTRANS            PIC X(4)    VALUE '3173'.                
014800     03  M-SW-KDMFSTYP           PIC X(1)    VALUE '1'.                   
014900                                                                          
015000*    03  MID -COPY W3I17201 -PRE 3172-                                    
015100     EJECT                                                                
015200                                                                          
015300 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
015400*01  MID -COPY W3I17301                                                   
015500*01  -COPY WMSGAREA                                                       
015600     EJECT                                                                
015700*  03  MOD -COPY W3O17301  -RED MSG-AREA.                                 
015800     EJECT                                                                
015900*01  -COPY WMFSAREA                                                       
016000     SKIP3                                                                
016100******************************************************************        
016200*                                                                         
016300*                AREOR FÖR ANROP TILL W3017310                            
016400*                                                                         
016500 01  FILLER                    PIC X(16) VALUE 'REQU-AREA'.               
016600 01  REQU-AREA.                                                           
016700     03 -COPY WZ01REQU                                                    
016800     03 -COPY W30173I1                                                    
016900     EJECT                                                                
017000                                                                          
017100 01  FILLER                    PIC X(16) VALUE 'RESP-AREA'.               
017200 01  RESP-AREA.                                                           
017300     03 -COPY WZ01RESP                                                    
017400     03 -COPY W30173O1                                                    
017500     EJECT                                                                
017600*                                                                         
017700*FIXED VALUE FOR INDX                                                     
017800 77  MAX-QTY-BELEV             PIC S9(4)   VALUE +4  COMP.                
017900                                                                          
018000 01  IMS-WS.                                                              
018100*                        **** STATUS-KOD FRÅN IMS                         
018200   03  STATUS-WS                 PIC XX.                                  
018300     88  SEGMENT-FINNS                       VALUE '  '.                  
018400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018500                                                                          
018600   03  GODK-STATUSKODER.                                                  
018700     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018800                                                                          
018900*                            IMS FUNKTIONSKODER                           
019000*01    -COPY W0003                                                        
019100     EJECT                                                                
019200                                                                          
019300 LINKAGE SECTION.                                                         
019400*01  -COPY W0009     -PRE MSG-                                            
019500     SKIP2                                                                
019600*01  -COPY W0009     -PRE ALT-                                            
019700     SKIP2                                                                
019800*01  -COPY W0009     -PRE DISTRDOC-                                       
019900     EJECT                                                                
020000*01  -COPY W0008     -PRE USEA-                                           
020100     05  FILLER                 PIC X.                                    
020200     EJECT                                                                
020300*01  -COPY W0008     -PRE ARTC-                                           
020400     05  FILLER                 PIC X.                                    
020500     SKIP2                                                                
020600*01  -COPY W0008     -PRE BENA-                                           
020700     05  FILLER                 PIC X.                                    
020800     SKIP2                                                                
020900*01  -COPY W0008     -PRE WDK7-                                           
021000     05  FILLER                 PIC X.                                    
       01  WDB6-PCB                PIC X.                                       
021100     EJECT                                                                
021200 PROCEDURE DIVISION   USING  MSG-PCB ALT-PCB DISTRDOC-PCB                 
021300                   USEA-PCB  BENA-PCB ARTC-PCB WDK7-PCB WDB6-PCB.         
021400                                                                          
021500      ENTRY 'DLITCBL' USING  MSG-PCB ALT-PCB DISTRDOC-PCB                 
021600                   USEA-PCB  BENA-PCB ARTC-PCB WDK7-PCB WDB6-PCB.         
021700                                                                          
021800     PERFORM IMS-GET-MSG                                                  
021900     IF SEGMENT-FINNS                                                     
022000       PERFORM A-INIT                                                     
022100       PERFORM B-INIT-KEYS                                                
022200       PERFORM C-KOLL-OM-BYT-INMATAT                                      
022400       IF BYT-EJ-BILD                                                     
022500         IF MFS-UPDATE                                                    
022600           SET REQU-UPDATE TO TRUE                                        
022700         ELSE                                                             
022800           IF MFS-PRINT                                                   
022900             SET REQU-PRINT TO TRUE                                       
023000           ELSE                                                           
023100             IF MFS-FIRST                                                 
023200               SET REQU-FIRST TO TRUE                                     
023300               PERFORM D-FIRST-PAGE                                       
023400             ELSE                                                         
023500               IF MFS-NEXT                                                
023600                 SET REQU-NEXT TO TRUE                                    
023700                 PERFORM E-NEXT-PAGE                                      
023800               ELSE                                                       
023900                 SET REQU-QUERY TO TRUE                                   
024000                 PERFORM F-SAME-PAGE                                      
024100               END-IF                                                     
024200             END-IF                                                       
024300           END-IF                                                         
024400         END-IF                                                           
024500                                                                          
024600         IF ANDRAT-ARTNR                                                  
024700            MOVE ERR-CORE-NO-CHANGED TO MED-IDMFSINF                      
024800            CALL WMEDKONV USING MED-WMEDAREA                              
024900            MOVE MED-MFSINF TO MOD-TEMFSFEL                               
025000            MOVE MFS-CLOSE-FIELD TO MOD-REG-FLAGGA-ATTR                   
025100            PERFORM MFS-RENSA-FAELT-UT                                    
025200         ELSE                                                             
025300           PERFORM G-CALL-BIZ-LOGIC-W3017310                              
025400         END-IF                                                           
025500                                                                          
025600         COMPUTE MSG-KVLL = LENGTH OF MOD-W3O17301 + 17                   
025700         PERFORM IMS-INSERT-MSG                                           
025800       END-IF                                                             
025900     END-IF                                                               
026000     MOVE ZERO   TO RETURN-CODE                                           
026100     GOBACK                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 A-INIT               SECTION.                                            
026500     MOVE 'A-INIT               ' TO WS-SEKTION                           
026600     SKIP2                                                                
026700     IF MSG-DUBBLA-TRANSKODER                                             
026800        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I17301                
026900        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
027000        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
027100        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
027200        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
027300     ELSE                                                                 
027400        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I17301                
027500        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
027600        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
027700        MOVE SPACE                         TO MFS-KDTRTYP                 
027800                                              MFS-IDPFK                   
027900     END-IF                                                               
028000                                                                          
028100     MOVE LOW-VALUE                        TO MSG-AREA                    
028200     MOVE 'W3O173N1'                       TO MFS-IDMOD                   
028300     MOVE '3173'                           TO MOD-IDTRANS                 
028400     MOVE MFS-IDTRANS                      TO W-IDTRANS                   
028500     MOVE MFS-RENSA-FAELT                  TO MOD-TEMFSFEL                
028600                                              MOD-TEMFSINF                
028700                                              MOD-IDARTNR-IN              
028800     IF EGEN-MID                                                          
028900        CONTINUE                                                          
029000     ELSE                                                                 
029100       IF GODK-MID                                                        
029200         MOVE SPACE                         TO MFS-KDTRTYP                
029300         MOVE '7'                           TO MFS-IDPFK                  
029400       ELSE                                                               
029500         MOVE ZERO                          TO MID-IDARTNR-IN             
029600                                               MID-IDDISTR-IN             
029700                                               MID-IDBYTRAP-IN            
029800                                               MID-IDBYTRAD-IN            
029900         MOVE SPACE                         TO MFS-KDTRTYP                
030000         MOVE '7'                           TO MFS-IDPFK                  
030100       END-IF                                                             
030200     END-IF                                                               
030300                                                                          
030400     MOVE 'EN'    TO REQU-IDSPRAK                                         
030500     MOVE 'GB '   TO MED-IDSKYLT                                          
030600                                                                          
030700     MOVE '101'   TO REQU-IDMSGVER                                        
030800                                                                          
030900     IF EGEN-MID OR GODK-MID                                              
031000       CONTINUE                                                           
031100     ELSE                                                                 
031200       MOVE NEJ                        TO SW-NYCKLAR-OK                   
031300                                                                          
031400       MOVE ERR-WRONG-KEY  TO MED-IDMFSINF                                
031500       CALL WMEDKONV USING MED-WMEDAREA                                   
031600       MOVE MED-MFSINF TO MOD-TEMFSFEL                                    
031700     END-IF                                                               
031800     MOVE NEJ TO BYT-SW                                                   
031900     MOVE NEJ TO ANDRAT-ARTNR-SW                                          
032000     .                                                                    
032100     EJECT                                                                
032200 B-INIT-KEYS          SECTION.                                            
032300     MOVE 'B-INIT-KEYS      '   TO WS-SEKTION                             
032400                                                                          
032500     IF EGEN-MID OR GODK-MID                                              
032600       MOVE ALL '+'             TO MSGI-WMSGINIT                          
032700       MOVE '001'               TO MSGI-KDCALL                            
032800       MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                            
032900       MOVE '3173'              TO MSGI-IDTRANS                           
033000       MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                      
033100       IF EGEN-MID                                                        
033200          MOVE MID-KDPRT        TO MSGI-KDPRT                             
033300       END-IF                                                             
033400                                                                          
033500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
033600                                                                          
033700       IF MSGI-IDUSER = 'MWGB112 '                                        
033800       OR MSGI-IDUSER = 'MWGB216 '                                        
033900       OR MSGI-IDUSER = 'MWGB219 '                                        
034000          MOVE WC-SDC-NL-ET  TO MSGI-IDDC                                 
034100       END-IF                                                             
034200       MOVE MSGI-IDUSER         TO REQU-IDUSER                            
034300       MOVE MSGI-KDPRT          TO REQU-KDPRT                             
034400       MOVE MSGI-IDDC           TO W-IDDC                                 
034500                                   WS-IDDC                                
034600                                   REQU-IDDC-KEY                          
034700                                                                          
034800       SKIP3                                                              
034900       MOVE JA                       TO SW-NYCKLAR-OK                     
035000       IF MID-IDDISTR-IN = ALL '+'                                        
035100          MOVE MID-IDDISTR-UT        TO WS-IDDISTR                        
035200          INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO              
035300       ELSE                                                               
035400          MOVE MID-IDDISTR-IN        TO WS-IDDISTR                        
035500       END-IF                                                             
035600       MOVE WS-IDDISTR               TO MOD-IDDISTR-UT                    
035700       MOVE WS-IDDISTR               TO REQU-IDDISTR-KEY                  
035800       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
035900                                                                          
036000       IF MID-IDBYTRAD-IN = ALL '+'                                       
036100          MOVE MID-IDBYTRAD-UT        TO WS-IDBYTRAD                      
036200          INSPECT WS-IDBYTRAD REPLACING LEADING SPACE BY ZERO             
036300       ELSE                                                               
036400          MOVE MID-IDBYTRAD-IN        TO WS-IDBYTRAD                      
036500          INSPECT WS-IDBYTRAD REPLACING LEADING SPACE BY ZERO             
036600       END-IF                                                             
036700       MOVE WS-IDBYTRAD              TO MOD-IDBYTRAD-UT                   
036800       MOVE WS-IDBYTRAD              TO REQU-IDBYTRAD-KEY                 
036900                                                                          
037000       IF MID-IDBYTRAP-IN = ALL '+'                                       
037100          MOVE MID-IDBYTRAP-UT        TO WS-IDBYTRAP                      
037200          INSPECT WS-IDBYTRAP REPLACING LEADING SPACE BY ZERO             
037300       ELSE                                                               
037400          MOVE MID-IDBYTRAP-IN        TO WS-IDBYTRAP                      
037500       END-IF                                                             
037600       MOVE WS-IDBYTRAP               TO MOD-IDBYTRAP-UT                  
037700       MOVE WS-IDBYTRAP               TO REQU-IDBYTRAP-KEY                
037800       INSPECT MOD-IDBYTRAP-UT REPLACING LEADING ZERO BY SPACE            
037900                                                                          
038000       IF MID-IDARTNR-IN = ALL '+'                                        
038100          MOVE MID-IDARTNR-UT        TO WS-IDARTNR                        
038200          INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO              
038300       ELSE                                                               
038400          MOVE MID-IDARTNR-IN        TO WS-IDARTNR                        
038500          MOVE '7'                   TO MFS-IDPFK                         
038600          MOVE SPACE                 TO MFS-KDTRTYP                       
038700       END-IF                                                             
038800       MOVE WS-IDARTNR               TO MOD-IDARTNR-UT                    
038900       MOVE WS-IDARTNR               TO REQU-IDARTNR-KEY                  
039000                                                                          
039100       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
039200                                                                          
039300       IF MID-IDTABNR-IN = ALL '+'                                        
039400          MOVE MID-IDTABNR-UT         TO WS-IDTABNR                       
039500          INSPECT WS-IDTABNR REPLACING LEADING SPACE BY ZERO              
039600       ELSE                                                               
039700          MOVE MID-IDTABNR-IN         TO WS-IDTABNR                       
039800       END-IF                                                             
039900       MOVE WS-IDTABNR                TO MOD-IDTABNR-UT                   
040000       MOVE WS-IDTABNR                TO REQU-IDTABNR-KEY                 
040100       INSPECT MOD-IDTABNR-UT REPLACING LEADING ZERO BY SPACE             
040200     END-IF                                                               
040300                                                                          
040300     MOVE MID-IDPRODNR-LO             TO REQU-IDPRODNR-LO                 
040400     MOVE MID-IDPRODNR-HI             TO REQU-IDPRODNR-HI                 
040500     MOVE MID-BELEV-LO                TO REQU-BELEV-LO                    
040600     MOVE MID-BELEV-HI                TO REQU-BELEV-HI                    
040400     .                                                                    
040500     EJECT                                                                
040600 C-KOLL-OM-BYT-INMATAT SECTION.                                           
040700     MOVE 'CA-KOLL-OM-BYT-INMATAT' TO WS-SEKTION                          
040800                                                                          
040900     IF EGEN-MID                                                          
041000       IF MID-REG-FLAGGA NOT = ALL '+'                                    
041100         IF MID-REG-FLAGGA = 'J' OR 'Y'                                   
041200           IF MID-IDARTNR-IN NOT = ALL '+'                                
041300             MOVE JA TO ANDRAT-ARTNR-SW                                   
041400           ELSE                                                           
041500             MOVE JA TO BYT-SW                                            
041600             PERFORM CA-BYT-BILD                                          
041700           END-IF                                                         
041800         END-IF                                                           
041900       END-IF                                                             
042000     END-IF                                                               
042100     SKIP2                                                                
042200     .                                                                    
042300 CA-BYT-BILD SECTION.                                                     
042400     MOVE 'CA-BYT-BILD'  TO  WS-SEKTION                                   
042500                                                                          
042600     MOVE LOW-VALUE TO 3172-MID-W3I17201                                  
042700     MOVE WS-IDDISTR      TO 3172-MID-IDDISTR-IN                          
042800     MOVE WS-IDBYTRAP     TO 3172-MID-IDBYTRAP-IN                         
042900     MOVE ZERO            TO 3172-MID-ANTAL-IN                            
043000     MOVE 'XXX'           TO 3172-MID-ANMARK-IN                           
043100                                                                          
043200     INSPECT WS-IDBYTRAD REPLACING LEADING SPACE BY ZERO                  
043300     MOVE WS-IDBYTRAD     TO  3172-MID-IDBYTRAD(1)                        
043400     MOVE WS-IDBYTRAD     TO 3172-MID-IDBYTRAD-3173                       
043500                                                                          
043600     INSPECT MOD-IDARTNR-UT REPLACING LEADING SPACE BY ZERO               
043700     IF  WS-IDDISTR NUMERIC AND                                           
043800         WS-IDBYTRAP   NUMERIC                                            
043900                                                                          
044000       IF  WS-IDDISTR > ZERO AND                                          
044100           WS-IDBYTRAP  > ZERO                                            
044200         MOVE MOD-IDARTNR-UT TO 3172-MID-OBJNR-SPAERR                     
044300       ELSE                                                               
044400         MOVE ZERO            TO 3172-MID-OBJNR-SPAERR                    
044500       END-IF                                                             
044600     ELSE                                                                 
044700       MOVE ZERO            TO 3172-MID-OBJNR-SPAERR                      
044800     END-IF                                                               
044900                                                                          
045000     MOVE '2' TO M-SW-KDMFSTYP                                            
045100                                                                          
045200     PERFORM IMS-INSERT-ALT-MSG                                           
045300     .                                                                    
045400     EJECT                                                                
050000 D-FIRST-PAGE SECTION.                                                    
050100                                                                          
050200     MOVE INF-FIRST-PAGE       TO MED-IDMFSINF                            
050300     CALL WMEDKONV USING MED-WMEDAREA                                     
050400     MOVE MED-TEMFSINF         TO MOD-TEMFSINF                            
050500                                                                          
050600*    MOVE MFS-RENSA-FAELT      TO MOD-REG-FLAGGA                          
050700     .                                                                    
050800     EJECT                                                                
050900 E-NEXT-PAGE SECTION.                                                     
051000                                                                          
051100     CONTINUE                                                             
051200*    MOVE MFS-RENSA-FAELT      TO MOD-REG-FLAGGA                          
051300     .                                                                    
051400     EJECT                                                                
051500 F-SAME-PAGE SECTION.                                                     
051600                                                                          
051700     CONTINUE                                                             
051800     .                                                                    
051900     EJECT                                                                
052000                                                                          
052100 G-CALL-BIZ-LOGIC-W3017310    SECTION.                                    
052200                                                                          
052300     PERFORM GA-INIT-REQU                                                 
052400                                                                          
052500     CALL W3017310 USING                                                  
052600          REQU-AREA RESP-AREA MAX-KVRADER DISTRDOC-PCB                    
052700          BENA-PCB ARTC-PCB WDK7-PCB WDB6-PCB                             
052800                                                                          
052900     PERFORM GB-SET-MSG-AND-HILIGHT                                       
053000                                                                          
053100     IF RESP-IDMSG-ERROR NOT = ERR-WRONG-KEY                              
053200       PERFORM GC-MOVE-RESP-TO-MOD                                        
053300     END-IF                                                               
053400     .                                                                    
053500     EJECT                                                                
053600                                                                          
053700 GA-INIT-REQU  SECTION.                                                   
053800                                                                          
053900     MOVE MID-REG-FLAGGA      TO REQU-REG-FLAGGA                          
054000     .                                                                    
054100                                                                          
054200 GB-SET-MSG-AND-HILIGHT SECTION.                                          
054300     MOVE 'GB-SET-MSG-AND-HILIGHT' TO WS-SEKTION                          
054400                                                                          
054500     MOVE 'EN'    TO MCNV-IDSPRAK                                         
054600                                                                          
054700     MOVE RESP-IDMSG-ERROR TO MCNV-IDMSG-ERROR                            
054800     MOVE RESP-IDMSG-INFO  TO MCNV-IDMSG-INFO                             
054900     MOVE RESP-IDELMT-ERROR TO MCNV-IDELMT-ERROR                          
055000     MOVE REQU-IDSPRAK     TO MCNV-IDSPRAK                                
055100                                                                          
055200     CALL WL01MCNV USING MCNV-AREA                                        
055300                                                                          
055400     MOVE MCNV-TEMFSINF    TO MOD-TEMFSINF                                
055500                                                                          
055600     MOVE MCNV-TEMFSFEL    TO MOD-TEMFSFEL                                
055700     .                                                                    
055800     EJECT                                                                
055900                                                                          
056000 GC-MOVE-RESP-TO-MOD SECTION.                                             
056100     MOVE 'GC-MOVE-RESP-TO-MOD' TO WS-SEKTION                             
056200                                                                          
056300*    -- IBLAND OM DET BLIR FEL                                            
056400*    -- ENTER OCH SAMTIDIGT RAD-INMATNING?                                
056500*    PERFORM MFS-LAES-IN-IGEN ???                                         
056600                                                                          
056700     MOVE RESP-KDPRT-ATTR            TO MOD-KDPRT-ATTR                    
056800     IF RESP-KDPRT = SPACE                                                
056900       MOVE MFS-ERASE-FIELD          TO MOD-KDPRT                         
057000     ELSE                                                                 
057100       IF RESP-KDPRT = ALL '+'                                            
057200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDPRT                         
057300       ELSE                                                               
057400         MOVE RESP-KDPRT             TO MOD-KDPRT                         
057500       END-IF                                                             
057600     END-IF                                                               
057700                                                                          
057800     IF RESP-IDPRODNR-LO       = SPACE                                    
057900       MOVE MFS-ERASE-FIELD TO MOD-IDPRODNR-LO                            
058000     ELSE                                                                 
058100       IF RESP-IDPRODNR-LO      = ALL '+'                                 
058200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDPRODNR-LO                   
058300      ELSE                                                                
058400         MOVE RESP-IDPRODNR-LO       TO MOD-IDPRODNR-LO                   
058500      END-IF                                                              
058600     END-IF                                                               
058700                                                                          
058800     IF RESP-BELEV-LO       = SPACE                                       
058900       MOVE MFS-ERASE-FIELD TO MOD-BELEV-LO                               
059000     ELSE                                                                 
059100       IF RESP-BELEV-LO         = ALL '+'                                 
059200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BELEV-LO                      
059300      ELSE                                                                
059400         MOVE RESP-BELEV-LO          TO MOD-BELEV-LO                      
059500      END-IF                                                              
059600     END-IF                                                               
059700                                                                          
059800     IF RESP-BEART-SVE      = SPACE                                       
059900       MOVE MFS-ERASE-FIELD TO MOD-BEART-SVE                              
060000     ELSE                                                                 
060100       IF RESP-BEART-SVE        = ALL '+'                                 
060200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEART-SVE                     
060300      ELSE                                                                
060400         MOVE RESP-BEART-SVE         TO MOD-BEART-SVE                     
060500      END-IF                                                              
060600     END-IF                                                               
060700                                                                          
060800     IF RESP-KDPRODSL       = SPACE                                       
060900       MOVE MFS-ERASE-FIELD TO MOD-KDPRODSL                               
061000     ELSE                                                                 
061100       IF RESP-KDPRODSL         = ALL '+'                                 
061200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDPRODSL                      
061300      ELSE                                                                
061400         MOVE RESP-KDPRODSL          TO MOD-KDPRODSL                      
061500      END-IF                                                              
061600     END-IF                                                               
061700                                                                          
061800     IF RESP-IDFKNGRP       = SPACE                                       
061900       MOVE MFS-ERASE-FIELD TO MOD-IDFKNGRP                               
062000     ELSE                                                                 
062100       IF RESP-IDFKNGRP         = ALL '+'                                 
062200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDFKNGRP                      
062300      ELSE                                                                
062400         MOVE RESP-IDFKNGRP          TO MOD-IDFKNGRP                      
062500      END-IF                                                              
062600     END-IF                                                               
           MOVE RESP-IDDISTR-RENOV-ATTR    TO MOD-IDDISTR-RENOV-ATTR            
062700                                                                          
062800     IF RESP-IDDISTR-RENOV  = SPACE                                       
062900       MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-RENOV                          
063000     ELSE                                                                 
063100       IF RESP-IDDISTR-RENOV    = ALL '+'                                 
063200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-RENOV                 
063300      ELSE                                                                
063400         MOVE RESP-IDDISTR-RENOV     TO MOD-IDDISTR-RENOV                 
063500      END-IF                                                              
063600     END-IF                                                               
063700                                                                          
063800     IF RESP-KVBYTPKO      = SPACE                                        
063900       MOVE MFS-ERASE-FIELD TO MOD-KVBYTPKO                               
064000     ELSE                                                                 
064100       IF RESP-KVBYTPKO    = ALL '+'                                      
064200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVBYTPKO                      
064300      ELSE                                                                
064400         MOVE RESP-KVBYTPKO          TO MOD-KVBYTPKO                      
064500      END-IF                                                              
064600     END-IF                                                               
064700                                                                          
064800     IF RESP-ADLAGOMR      = SPACE                                        
064900       MOVE MFS-ERASE-FIELD TO MOD-ADLAGOMR                               
065000     ELSE                                                                 
065100       IF RESP-ADLAGOMR    = ALL '+'                                      
065200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ADLAGOMR                      
065300      ELSE                                                                
065400         MOVE RESP-ADLAGOMR          TO MOD-ADLAGOMR                      
065500      END-IF                                                              
065600     END-IF                                                               
065700                                                                          
065800     IF RESP-ADGANG     = SPACE                                           
065900       MOVE MFS-ERASE-FIELD TO MOD-ADGANG                                 
066000     ELSE                                                                 
066100       IF RESP-ADGANG   = ALL '+'                                         
066200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ADGANG                        
066300      ELSE                                                                
066400         MOVE RESP-ADGANG            TO MOD-ADGANG                        
066500      END-IF                                                              
066600     END-IF                                                               
066700                                                                          
066800     IF RESP-ADPLATS    = SPACE                                           
066900       MOVE MFS-ERASE-FIELD TO MOD-ADPLATS                                
067000     ELSE                                                                 
067100       IF RESP-ADPLATS     = ALL '+'                                      
067200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ADPLATS                       
067300      ELSE                                                                
067400         MOVE RESP-ADPLATS           TO MOD-ADPLATS                       
067500      END-IF                                                              
067600     END-IF                                                               
           MOVE RESP-KVLS-ATTR           TO MOD-KVLS-ATTR                       
                                                                                
067800     IF RESP-KVLS          = SPACE                                        
067900       MOVE MFS-ERASE-FIELD TO MOD-KVLS                                   
068000     ELSE                                                                 
068100       IF RESP-KVLS        = ALL '+'                                      
068200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVLS                          
068300      ELSE                                                                
068400         MOVE RESP-KVLS              TO MOD-KVLS                          
068500      END-IF                                                              
068600     END-IF                                                               
068700                                                                          
068800     IF RESP-KVLS-MAXCORE        = SPACE                                  
068900       MOVE MFS-ERASE-FIELD TO MOD-KVLS-MAXCORE                           
069000     ELSE                                                                 
069100       IF RESP-KVLS-MAXCORE      = ALL '+'                                
069200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVLS-MAXCORE                  
069300      ELSE                                                                
069400         MOVE RESP-KVLS-MAXCORE      TO MOD-KVLS-MAXCORE                  
069500      END-IF                                                              
069600     END-IF                                                               
069700*                                                                         
069800     MOVE RESP-BETFLEV-ATTR       TO MOD-BETFLEV-ATTR                     
069900*                                                                         
070000     IF RESP-BETFLEV             = SPACE                                  
070100       MOVE MFS-ERASE-FIELD TO MOD-BETFLEV                                
070200     ELSE                                                                 
070300       IF RESP-BETFLEV           = ALL '+'                                
070400         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BETFLEV                       
070500      ELSE                                                                
070600         MOVE RESP-BETFLEV           TO MOD-BETFLEV                       
070700      END-IF                                                              
070800     END-IF                                                               
070900*                                                                         
071000     MOVE RESP-DELAR-SAKNAS-ATTR  TO MOD-DELAR-SAKNAS-ATTR                
071100*                                                                         
071200     IF RESP-DELAR-SAKNAS        = SPACE                                  
071300       MOVE MFS-ERASE-FIELD TO MOD-DELAR-SAKNAS                           
071400     ELSE                                                                 
071500       IF RESP-DELAR-SAKNAS      = ALL '+'                                
071600         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-DELAR-SAKNAS                  
071700      ELSE                                                                
071800         MOVE RESP-DELAR-SAKNAS      TO MOD-DELAR-SAKNAS                  
071900       END-IF                                                             
072000     END-IF                                                               
072100                                                                          
072200     IF RESP-TEBYTKVA1           = SPACE                                  
072300       MOVE MFS-ERASE-FIELD TO MOD-TEBYTKVA1                              
072400     ELSE                                                                 
072500       IF RESP-TEBYTKVA1         = ALL '+'                                
072600         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TEBYTKVA1                     
072700      ELSE                                                                
072800         MOVE RESP-TEBYTKVA1         TO MOD-TEBYTKVA1                     
072900      END-IF                                                              
073000     END-IF                                                               
073100                                                                          
073200     IF RESP-TEBYTKVA2           = SPACE                                  
073300       MOVE MFS-ERASE-FIELD TO MOD-TEBYTKVA2                              
073400     ELSE                                                                 
073500       IF RESP-TEBYTKVA2         = ALL '+'                                
073600         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TEBYTKVA2                     
073700      ELSE                                                                
073800         MOVE RESP-TEBYTKVA2         TO MOD-TEBYTKVA2                     
073900      END-IF                                                              
074000     END-IF                                                               
074100                                                                          
074200     IF RESP-TEBYTKVA3           = SPACE                                  
074300       MOVE MFS-ERASE-FIELD TO MOD-TEBYTKVA3                              
074400     ELSE                                                                 
074500       IF RESP-TEBYTKVA3         = ALL '+'                                
074600         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TEBYTKVA3                     
074700      ELSE                                                                
074800         MOVE RESP-TEBYTKVA3         TO MOD-TEBYTKVA3                     
074900      END-IF                                                              
075000     END-IF                                                               
075100                                                                          
075200     IF RESP-TEBYTKVA4           = SPACE                                  
075300       MOVE MFS-ERASE-FIELD TO MOD-TEBYTKVA4                              
075400     ELSE                                                                 
075500       IF RESP-TEBYTKVA4         = ALL '+'                                
075600         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TEBYTKVA4                     
075700      ELSE                                                                
075800         MOVE RESP-TEBYTKVA4         TO MOD-TEBYTKVA4                     
075900      END-IF                                                              
076000     END-IF                                                               
076100                                                                          
076200     IF RESP-BELEV-HI       = SPACE                                       
076300       MOVE MFS-ERASE-FIELD TO MOD-BELEV-HI                               
076400     ELSE                                                                 
076500       IF RESP-BELEV-HI         = ALL '+'                                 
076600         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BELEV-HI                      
076700      ELSE                                                                
076800         MOVE RESP-BELEV-HI          TO MOD-BELEV-HI                      
076900      END-IF                                                              
077000     END-IF                                                               
077100                                                                          
077200     IF RESP-IDPRODNR-HI       = SPACE                                    
077300       MOVE MFS-ERASE-FIELD TO MOD-IDPRODNR-HI                            
077400     ELSE                                                                 
077500       IF RESP-IDPRODNR-HI      = ALL '+'                                 
077600         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDPRODNR-HI                   
077700      ELSE                                                                
077800         MOVE RESP-IDPRODNR-HI       TO MOD-IDPRODNR-HI                   
077900      END-IF                                                              
078000     END-IF                                                               
078100                                                                          
078200*                                                                         
078300*    MOVE RESP-REG-FLAGGA-ATTR TO MOD-REG-FLAGGA-ATTR                     
078400*                                                                         
078500*    IF RESP-REG-FLAGGA          = SPACE                                  
078600*      MOVE SPACE           TO MOD-REG-FLAGGA                             
078700*    ELSE                                                                 
078800*      IF RESP-REG-FLAGGA        = ALL '+'                                
078900*        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-REG-FLAGGA                    
079000*     ELSE                                                                
079100*        MOVE RESP-REG-FLAGGA        TO MOD-REG-FLAGGA                    
079200*        MOVE MID-REG-FLAGGA         TO MOD-REG-FLAGGA                    
079300*     END-IF                                                              
079400*    END-IF                                                               
079500                                                                          
079600*OCCURS 4                                                                 
079700     MOVE +1 TO INDX                                                      
079800     PERFORM UNTIL INDX > MAX-QTY-BELEV                                   
079900                                                                          
080000       IF RESP-BELEV   (INDX) = SPACE                                     
080100         MOVE MFS-ERASE-FIELD TO MOD-BELEV  (INDX)                        
080200       ELSE                                                               
080300         IF RESP-BELEV  (INDX)  = ALL '+'                                 
080400           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BELEV  (INDX)               
080500        ELSE                                                              
080600           MOVE RESP-BELEV  (INDX)                                        
080700                                       TO MOD-BELEV  (INDX)               
080800        END-IF                                                            
080900       END-IF                                                             
081000                                                                          
081100       ADD 1 TO INDX                                                      
081200     END-PERFORM                                                          
081300                                                                          
081400*OCCURS 12 HERE, 300 IN WEB                                               
081500     MOVE +1 TO INDX                                                      
081600     PERFORM UNTIL INDX > MAX-KVRADER                                     
081700                                                                          
081800       IF RESP-IDARTNR (INDX) = SPACE                                     
081900         MOVE MFS-ERASE-FIELD TO MOD-IDARTNR(INDX)                        
082000       ELSE                                                               
082100         IF RESP-IDARTNR(INDX)  = ALL '+'                                 
082200           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR(INDX)               
082300        ELSE                                                              
082400           MOVE RESP-IDARTNR(INDX)                                        
082500                                       TO MOD-IDARTNR(INDX)               
082600        END-IF                                                            
082700       END-IF                                                             
082800                                                                          
082900       ADD 1 TO INDX                                                      
083000     END-PERFORM                                                          
083100                                                                          
083200     IF RESP-IDMSG-ERROR NOT = SPACE                                      
083300        MOVE MFS-CLOSE-FIELD TO MOD-REG-FLAGGA-ATTR                       
083400     END-IF                                                               
083500     .                                                                    
083600                                                                          
083700     EJECT                                                                
083800 MFS-RENSA-FAELT-UT SECTION.                                              
083900                                                                          
084000     MOVE MFS-ERASE-FIELD TO MOD-IDPRODNR-LO                              
084100     MOVE MFS-ERASE-FIELD TO MOD-BELEV-LO                                 
084200     MOVE MFS-ERASE-FIELD TO MOD-BEART-SVE                                
084300     MOVE MFS-ERASE-FIELD TO MOD-KDPRODSL                                 
084400     MOVE MFS-ERASE-FIELD TO MOD-IDFKNGRP                                 
084500     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-RENOV                            
084600     MOVE MFS-ERASE-FIELD TO MOD-KVBYTPKO                                 
084700     MOVE MFS-ERASE-FIELD TO MOD-ADLAGOMR                                 
084800     MOVE MFS-ERASE-FIELD TO MOD-ADGANG                                   
084900     MOVE MFS-ERASE-FIELD TO MOD-ADPLATS                                  
085000     MOVE MFS-ERASE-FIELD TO MOD-KVLS                                     
085100     MOVE MFS-ERASE-FIELD TO MOD-KVLS-MAXCORE                             
085200     MOVE MFS-ERASE-FIELD TO MOD-BETFLEV                                  
085300     MOVE MFS-ERASE-FIELD TO MOD-DELAR-SAKNAS                             
085400     MOVE MFS-ERASE-FIELD TO MOD-TEBYTKVA1                                
085500     MOVE MFS-ERASE-FIELD TO MOD-TEBYTKVA2                                
085600     MOVE MFS-ERASE-FIELD TO MOD-TEBYTKVA3                                
085700     MOVE MFS-ERASE-FIELD TO MOD-TEBYTKVA4                                
085800     MOVE MFS-ERASE-FIELD TO MOD-BELEV-HI                                 
085900     MOVE MFS-ERASE-FIELD TO MOD-IDPRODNR-HI                              
086000*OCCURS 4                                                                 
086100     MOVE +1 TO INDX                                                      
086200     PERFORM UNTIL INDX > MAX-QTY-BELEV                                   
086300       MOVE MFS-ERASE-FIELD TO MOD-BELEV         (INDX)                   
086400       ADD 1 TO INDX                                                      
086500     END-PERFORM                                                          
086600                                                                          
086700*OCCURS 12 HERE, 300 IN WEB                                               
086800     MOVE +1 TO INDX                                                      
086900     PERFORM UNTIL INDX > MAX-KVRADER                                     
087000       MOVE MFS-ERASE-FIELD TO MOD-IDARTNR(INDX)                          
087100       ADD 1 TO INDX                                                      
087200     END-PERFORM                                                          
087300     .                                                                    
087400                                                                          
087500* IMS SEKTIONER                                                           
087600     SKIP3                                                                
087700 IMS-GET-MSG SECTION.                                                     
087800     MOVE 'IMS-GET-MSG' TO  WS-IMS-SEKTION                                
087900     SKIP1                                                                
088000     MOVE '  QC' TO GODK-STATUSKODER                                      
088100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
088200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088300     PERFORM IMS-STATUSKONTROLL                                           
088400     SKIP3                                                                
088500     .                                                                    
088600 IMS-INSERT-MSG SECTION.                                                  
088700     MOVE 'IMS-INSERT-MSG' TO  WS-IMS-SEKTION                             
088800     SKIP1                                                                
088900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
089000     MOVE SPACE TO GODK-STATUSKODER                                       
089100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
089200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089300     PERFORM IMS-STATUSKONTROLL                                           
089400     EJECT                                                                
089500     .                                                                    
089600 IMS-INSERT-ALT-MSG SECTION.                                              
089700     MOVE 'IMS-INSERT-ALT-MSG' TO  WS-IMS-SEKTION                         
089800     MOVE SPACE TO GODK-STATUSKODER                                       
089900     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
090000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
090100     PERFORM IMS-STATUSKONTROLL                                           
090200     .                                                                    
090300     EJECT                                                                
090400 IMS-STATUSKONTROLL SECTION.                                              
090500     SKIP1                                                                
090600     SET STATUS-IX TO 1                                                   
090700     SEARCH GODK-STATUS                                                   
090800       AT END                                                             
090900         CALL FELLOG                                                      
091000        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
091100           CONTINUE                                                       
091200     END-SEARCH                                                           
091300     .                                                                    
091400     EJECT                                                                
