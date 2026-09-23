000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4071600.                                                
000400 AUTHOR.         LASSE C.                                                 
000500 DATE-WRITTEN.   SEPT  94.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET LÄSER KREDIT.REG (WDA2)                               
001100*        VISAR ENDAST VALDA DATAELEMENT SAMT                              
001200*        BERÄKNAR TILLÄGGSKOSTNADER OCH VÄRDEN                            
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T716                                              
001600*        MID:         W4I71601                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O71601                                            
002000*                                                                         
002100*    E'TRACKER: 3968811  2006-10                                          
002200*                                                                         
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800*    -- CHECKED BY WY2000                                                 
002900     SKIP3                                                                
003000 77   PROGRAM-NAMN           VALUE 'W4071600'                             
003100                                 PIC X(8).                                
003200 77  WS-RELANDCO                 PIC S9(3)V9(2)         COMP-3.           
003300 77  WS-PRFOERS                  PIC S9(7)V9(2)         COMP-3.           
003400 77  WS-PRFRAKT                  PIC S9(7)V9(2)         COMP-3.           
003500 77  WS-PRLEGKST                 PIC S9(7)V9(2)         COMP-3.           
003600 77  WS-SUARTOMK-TK              PIC S9(7)V9(2)         COMP-3.           
003700 77  WS-SUARTFSG-RAD             PIC S9(9)V9(2)         COMP-3.           
003800 77  WS-SUACKFSG-KRE-SPAR        PIC 9(9)V9(2) VALUE ZERO.                
003900 77  WS-SUACKFSG-DEB-SPAR        PIC 9(9)V9(2) VALUE ZERO.                
004000 77  WS-IDARTNR                  PIC 9(8).                                
004100 77  WS-KDLEVANM                 PIC X.                                   
004200 77  WS-KVLEVANM                 PIC S9(7)              COMP-3.           
004300 77  IDDISTR-WS                  PIC X(4).                                
004400 77  IDKUNDNR-WS                 PIC X(6).                                
004500 77  IDRAPPNR-WS                 PIC X(7).                                
004600 77  IDARTNR-WS                  PIC 9(8).                                
004700 77  IDRADNR-WS                  PIC 9(4).                                
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77  MAX-RAD                     PIC S9(9)   VALUE +11  COMP SYNC.        
005200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1060 COMP SYNC.        
005300 77  OK-VISA-ALLT                PIC X       VALUE ' '.                   
005400 77  SEC-FELSVAR                 PIC X       VALUE 'F'.                   
006000                                                                          
006400 01  TEST-IDDISTR                PIC 9(5)   VALUE ZERO COMP-3.            
006500*01  FILLER  -COPY WWDIST79      -RED TEST-IDDISTR.                       
006600*                                                                         
006700 01  FILLER                      PIC X(16)   VALUE 'WSIDFTG'.             
006800*01  -COPY WWIDFTG                                                        
006900     EJECT                                                                
007000*                                                                         
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
007600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007700     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
007800     EJECT                                                                
007900*    ---  LÄNKAREA TILL W418OKOD                                          
008000 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
008100                                                                          
008200*01 -COPY W418OKOD           -PRE OKOD-.                                  
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008500 01  FILLER                      PIC X(16)   VALUE 'W005INIT'.            
008600*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008900 01  FILLER                      PIC X(16)   VALUE 'WMEDKONV'.            
009000*01 -COPY WMEDAREA                                                        
009100                                                                          
009200 01  MESSAGE-CODES.                                                       
009300     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '405'.                 
009400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009500     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
009600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010100     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
010200     EJECT                                                                
010300                                                                          
010400 01  NYCKLAR-OK-SW               PIC X.                                   
010500    88  NYCKLAR-OK        VALUE 'J'.                                      
010600 01  GODKAEND-LAGER-SW           PIC X.                                   
010700    88  GODKAEND-LAGER    VALUE '1' '2'.                                  
010800 01  RAEKNA-UPP-SW               PIC X.                                   
010900    88  RAEKNA-UPP        VALUE 'J'.                                      
011000 01  WS-IDTRANS                  PIC X(4).                                
011100    88  GODKAEND-BILD     VALUE '4712' '4713'                             
011200                                '4714' '4715' '4716'.                     
011300    88  EGEN-MID          VALUE '4716'.                                   
011400    88  HELP-MID          VALUE '0551'.                                   
011500    88  4737-MID          VALUE '4737'.                                   
011600                                                                          
011700 01  WS-KDANMORS                 PIC X(2).                                
011800 01  FILLER  REDEFINES WS-KDANMORS.                                       
011900    03  FILLER                   PIC X.                                   
012000    03  WS-KDANMORS-SLUTSIFFR    PIC X.                                   
012100                                                                          
012200 01  NYCKLAR-TILL-DLI.                                                    
012300   03  W-IDLEVANM-X.                                                      
012400     05  W-IDDISTR               PIC S9(5)   VALUE ZERO  COMP-3.          
012500     05  W-IDKUNDNR              PIC S9(7)   VALUE ZERO  COMP-3.          
012600     05  W-IDRAPPNR              PIC  9(7).                               
012700                                                                          
012800   03  W-WDA211KY-MIN-X.                                                  
012900     05  W-IDARTNR               PIC S9(9)   VALUE ZERO  COMP-3.          
013000     05  W-IDRADNR               PIC S9(5)   VALUE ZERO  COMP-3.          
013100     EJECT                                                                
013200*    -NYCKLAR TIL WDB201                                                  
013300     03  W-IDGMT-X.                                                       
013400       05  W-IDDISTR-WDB2        PIC S9(5) VALUE ZERO COMP-3.             
013500       05  W-IDKUNDNR-WDB2       PIC S9(7) VALUE ZERO COMP-3.             
013600     03  W-IDGMT-MIN-X.                                                   
013700       05  W-IDDISTR-WDB2-MIN    PIC S9(5) VALUE ZERO COMP-3.             
013800       05  W-IDKUNDNR-WDB2-MIN   PIC S9(7) VALUE ZERO COMP-3.             
013900     03  W-IDGMT-MAX-X.                                                   
014000       05  W-IDDISTR-WDB2-MAX    PIC S9(5) VALUE ZERO COMP-3.             
014100       05  W-IDKUNDNR-WDB2-MAX   PIC S9(7) VALUE ZERO COMP-3.             
014200                                                                          
014300* TILL WDB101                                                             
014400     03  W-WDB101KY-X.                                                    
014500       05  W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
014600       05  W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
014700                                                                          
014800*01  FILLER -COPY WSECAREA                                                
014900                                                                          
015000     EJECT                                                                
015100******************************************************************        
015200*                                                                         
015300*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
015400*                                                                         
015500 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
015600                                                                          
015700*01  MID -COPY W4I71601                                                   
015800     EJECT                                                                
015900*01  -COPY WMSGAREA                                                       
016000     EJECT                                                                
016100*  03  MOD -COPY W4O71601           -RED MSG-AREA.                        
016200     EJECT                                                                
016300*01  -COPY WMFSAREA                                                       
016400     EJECT                                                                
016500******************************************************************        
016600*                                                                         
016700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016800*                                                                         
016900 01  IMS-WS.                                                              
017000   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
017100                                                                          
017200*                        **** STATUS-KOD FRÅN IMS                         
017300   03  STATUS-WS                 PIC XX.                                  
017400     88  SEGMENT-FINNS                       VALUE '  '.                  
017500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017600                                                                          
017700   03  GODK-STATUSKODER.                                                  
017800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900                                                                          
018000 01    SSA1                      PIC X(64).                               
018100     EJECT                                                                
018200*                            IMS FUNKTIONSKODER                           
018300*01    -COPY W0003                                                        
018400     EJECT                                                                
018500*                            DLI INPUT-OUTPUT AREA                        
018600 01  DLI-IO-AREA.                                                         
018700   03  IO-AREA                   PIC X(300)  VALUE SPACE.                 
018800                                                                          
018900*03  WLKREE01    -COPY WDA201                -RED IO-AREA.                
019000     EJECT                                                                
019100*03  WLKREE11    -COPY WDA211                -RED IO-AREA.                
019200     EJECT                                                                
019300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB201'.           
019400 01  DLI-IO-WDB201.                                                       
019500*     03  -COPY WDB201.                                                   
019600     EJECT                                                                
019700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB101'.           
019800 01  DLI-IO-WDB101.                                                       
019900*     03  -COPY WDB101.                                                   
020000     EJECT                                                                
020100 LINKAGE SECTION.                                                         
020200*01  -COPY W0009     -PRE MSG-                                            
020300                                                                          
020400*01  -COPY W0008     -PRE USEA-                                           
020500     05  FILLER                  PIC X.                                   
020600     EJECT                                                                
020700*01  -COPY W0008     -PRE KREE-                                           
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000*01  -COPY W0008      -PRE WDB1-                                          
021100     05  FILLER                  PIC X.                                   
021200     EJECT                                                                
021300*01  -COPY W0008      -PRE WDB2-                                          
021400     05  FILLER                  PIC X.                                   
021500     EJECT                                                                
021600 PROCEDURE DIVISION USING MSG-PCB  USEA-PCB KREE-PCB                      
021700                                   WDB1-PCB WDB2-PCB.                     
021800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB KREE-PCB                      
021900                                   WDB1-PCB WDB2-PCB.                     
022000                                                                          
022100     PERFORM IMS-GET-MSG                                                  
022200     IF SEGMENT-FINNS                                                     
022300       PERFORM A-INIT                                                     
022400       PERFORM B-KOLLA-NYCKLAR                                            
022500       IF NYCKLAR-OK                                                      
022600         PERFORM G-SECURIT-KONTROLL                                       
022700         IF SEC-KDSVAR NOT = SEC-FELSVAR                                  
022800            IF MFS-FIRST                                                  
022900               PERFORM C-FOERSTA-SIDA                                     
023000            ELSE                                                          
023100               IF MFS-NEXT                                                
023200                  PERFORM D-NAESTA-SIDA                                   
023300               ELSE                                                       
023400                  PERFORM E-SAMMA-SIDA                                    
023500               END-IF                                                     
023600            END-IF                                                        
023700            PERFORM F-LAES-VISA-INFO                                      
023800         ELSE                                                             
023900           MOVE ERR-UNAUTHORIZED TO MED-IDMFSFEL                          
024000           CALL WMEDKONV USING MED-WMEDAREA                               
024100           MOVE MED-MFSFEL      TO MOD-TEMFSFEL                           
024200         END-IF                                                           
024300       END-IF                                                             
024400       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O71601 + 4                      
024500       PERFORM IMS-INSERT-MSG                                             
024600     END-IF                                                               
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300                                                                          
025400     IF MSG-DUBBLA-TRANSKODER                                             
025500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I71601                 
025600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
025700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025800       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
025900       MOVE MSG-IDPFK TO MFS-IDPFK                                        
026000     ELSE                                                                 
026100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I71601                  
026200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
026300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026400       MOVE SPACE TO MFS-KDTRTYP        MFS-IDPFK                         
026500     END-IF                                                               
026600                                                                          
026700     MOVE LOW-VALUE TO MSG-AREA                                           
026800     MOVE 'W4O71601' TO MFS-IDMOD                                         
026900     MOVE '4716' TO MOD-IDTRANS                                           
027000     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
027100     MOVE MFS-RENSA-FAELT              TO MOD-TEMFSFEL                    
027200                                          MOD-TEMFSINF                    
027300                                          MOD-IDDISTR-IN                  
027400                                          MOD-IDKUNDNR-IN                 
027500                                          MOD-IDRAPPNR-IN                 
027600                                          MOD-IDARTNR-IN                  
027700                                          MOD-IDRADNR-IN                  
027800*                                                                         
027900     IF GODKAEND-BILD                                                     
028000       MOVE SPACE TO MFS-KDTRTYP                                          
028100     END-IF                                                               
028200                                                                          
028300     MOVE  LOW-VALUE         TO W-IDGMT-MIN-X                             
028400                                                                          
028500     MOVE HIGH-VALUE         TO W-IDGMT-MAX-X                             
028600     .                                                                    
028700     EJECT                                                                
028800 B-KOLLA-NYCKLAR SECTION.                                                 
028900                                                                          
029000     MOVE ALL '+'               TO MSGI-WMSGINIT                          
029100     MOVE '001'                 TO MSGI-KDCALL                            
029200     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
029300     MOVE '4716'                TO MSGI-IDTRANS                           
029400     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
029500     IF GODKAEND-BILD                                                     
029600        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
029700        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
029800        MOVE MID-IDRAPPNR-IN    TO MSGI-IDRAPPNR                          
029900        IF MID-IDARTNR-IN  = ALL '+'                                      
030000          MOVE '+++++++++'      TO MSGI-IDARTNR                           
030100        ELSE                                                              
030200          MOVE MID-IDARTNR-IN   TO WS-IDARTNR                             
030300          MOVE WS-IDARTNR       TO MSGI-IDARTNR                           
030400        END-IF                                                            
030500        MOVE MID-IDRADNR-IN     TO MSGI-IDRADNR                           
030600     END-IF                                                               
030700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030800                                                                          
030900     IF MSGI-IDLAND-SPR = 'GB'                                            
031000       MOVE 'GB'                TO MED-IDSKYLT                            
031100     ELSE                                                                 
031200       MOVE 'S '                TO MED-IDSKYLT                            
031300     END-IF                                                               
031400                                                                          
031500     MOVE JA TO NYCKLAR-OK-SW                                             
031600                                                                          
031700     MOVE MFS-ERASE-FIELD       TO MOD-IDDISTR-IN                         
031800                                   MOD-IDKUNDNR-IN                        
031900                                   MOD-IDRAPPNR-IN                        
032000                                   MOD-IDARTNR-IN                         
032100                                   MOD-IDRADNR-IN                         
032200                                                                          
032300     IF MID-IDDISTR-IN         NOT = ALL '+'                              
032400        MOVE '7'                TO MFS-IDPFK                              
032500        MOVE SPACE              TO MFS-KDTRTYP                            
032600     END-IF                                                               
032700                                                                          
032800     IF MID-IDKUNDNR-IN        NOT  = ALL '+'                             
032900        MOVE '7'                TO MFS-IDPFK                              
033000        MOVE SPACE              TO MFS-KDTRTYP                            
033100     END-IF                                                               
033200                                                                          
033300     IF MID-IDRAPPNR-IN         NOT = ALL '+'                             
033400        MOVE '7'                TO MFS-IDPFK                              
033500        MOVE SPACE              TO MFS-KDTRTYP                            
033600     END-IF                                                               
033700                                                                          
033800     MOVE MSGI-IDDISTR          TO IDDISTR-WS                             
033900     MOVE MSGI-IDKUNDNR         TO IDKUNDNR-WS                            
034000     MOVE MSGI-IDRAPPNR         TO IDRAPPNR-WS                            
034100     MOVE MSGI-IDRADNR          TO IDRADNR-WS                             
034200     MOVE MSGI-IDARTNR (1:8)    TO IDARTNR-WS                             
034300                                                                          
034400     IF MID-IDARTNR-IN  NOT = ALL '+'                                     
034500       MOVE '7'                  TO MFS-IDPFK                             
034600       MOVE SPACE                TO MFS-KDTRTYP                           
034700       IF 4737-MID                                                        
034800          IF IDARTNR-WS  NOT NUMERIC                                      
034900             MOVE ZERO           TO IDARTNR-WS                            
035000          END-IF                                                          
035100       END-IF                                                             
035200     ELSE                                                                 
035300       IF GODKAEND-BILD                                                   
035400          MOVE MID-IDARTNR-UT    TO IDARTNR-WS                            
035500       END-IF                                                             
035600       IF 4737-MID                                                        
035700          IF IDARTNR-WS  NOT NUMERIC                                      
035800             MOVE ZERO           TO IDARTNR-WS                            
035900          END-IF                                                          
036000       END-IF                                                             
036100     END-IF                                                               
036200                                                                          
036300     IF MID-IDRADNR-IN NOT = ALL '+'                                      
036400       MOVE '7'                  TO MFS-IDPFK                             
036500       MOVE SPACE                TO MFS-KDTRTYP                           
036600       IF 4737-MID                                                        
036700          MOVE ZERO              TO IDRADNR-WS                            
036800       END-IF                                                             
036900     ELSE                                                                 
037000       IF GODKAEND-BILD                                                   
037100          MOVE MID-IDRADNR-UT    TO IDRADNR-WS                            
037200       END-IF                                                             
037300     END-IF                                                               
037400                                                                          
037500     INSPECT IDARTNR-WS  REPLACING LEADING SPACE BY ZERO                  
037600     INSPECT IDRADNR-WS  REPLACING LEADING SPACE BY ZERO                  
037700     INSPECT IDKUNDNR-WS  REPLACING LEADING SPACE BY ZERO                 
037800                                                                          
037900     IF IDARTNR-WS NOT NUMERIC                                            
038000       MOVE NEJ                  TO NYCKLAR-OK-SW                         
038100     ELSE                                                                 
038200       MOVE IDARTNR-WS           TO W-IDARTNR                             
038400     END-IF                                                               
038500                                                                          
038600     IF IDRADNR-WS NOT NUMERIC                                            
038700       MOVE NEJ                  TO NYCKLAR-OK-SW                         
038701     ELSE                                                                 
038710       MOVE IDRADNR-WS           TO W-IDRADNR                             
038800     END-IF                                                               
038900                                                                          
039000     IF MID-IDKUNDNR-IN          = ALL '+' AND                            
039100        MID-IDDISTR-IN NOT       = ALL '+'                                
039200        MOVE ZERO               TO IDKUNDNR-WS                            
039300     END-IF                                                               
039400                                                                          
039500     IF IDDISTR-WS NUMERIC                                                
039600        MOVE IDDISTR-WS         TO W-IDDISTR                              
039700     ELSE                                                                 
039800        MOVE NEJ                TO NYCKLAR-OK-SW                          
039900     END-IF                                                               
040000                                                                          
040100     IF IDKUNDNR-WS NUMERIC                                               
040200        MOVE IDKUNDNR-WS        TO W-IDKUNDNR                             
040210                                   W-IDKUNDNR-WDB2                        
040300     ELSE                                                                 
040400        MOVE NEJ                TO NYCKLAR-OK-SW                          
040410        MOVE IDKUNDNR-WS        TO MOD-IDKUNDNR-UT                        
040500     END-IF                                                               
040600                                                                          
040700     IF IDRAPPNR-WS NUMERIC                                               
040800        MOVE IDRAPPNR-WS        TO W-IDRAPPNR                             
040900     ELSE                                                                 
041000        MOVE NEJ                TO NYCKLAR-OK-SW                          
041010        MOVE IDRAPPNR-WS        TO MOD-IDRAPPNR-UT                        
041100     END-IF                                                               
041200                                                                          
041210     IF IDDISTR-WS NUMERIC                                                
041300       MOVE IDDISTR-WS          TO MOD-IDDISTR-UT                         
041400                                   W-IDDISTR-WDB2                         
041500                                   TEST-IDDISTR                           
041510     ELSE                                                                 
041520       MOVE ZERO                TO MOD-IDDISTR-UT                         
041530                                   W-IDDISTR-WDB2                         
041540                                   TEST-IDDISTR                           
041550     END-IF                                                               
041900                                                                          
042000     IF IDARTNR-WS       NUMERIC                                          
042100       MOVE IDARTNR-WS     TO MOD-IDARTNR-UT                              
042200     END-IF                                                               
042300                                                                          
042400     IF IDRADNR-WS           NUMERIC                                      
042500       MOVE IDRADNR-WS       TO MOD-IDRADNR-UT                            
042600     END-IF                                                               
042700                                                                          
042800     IF NYCKLAR-OK                                                        
042900       MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                        
043000       MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                       
043100       MOVE MSGI-IDRAPPNR        TO MOD-IDRAPPNR-UT                       
043200       MOVE IDARTNR-WS           TO MOD-IDARTNR-UT                        
043300       MOVE IDRADNR-WS           TO MOD-IDRADNR-UT                        
043400     ELSE                                                                 
043500       MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UT                             
043600                               MOD-IDKUNDNR-UT                            
043700                               MOD-IDRAPPNR-UT                            
043800                               MOD-IDARTNR-UT                             
043900                               MOD-IDRADNR-UT                             
044000     END-IF                                                               
044100                                                                          
044200     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
044300     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
044400     INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE              
044500     INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE              
044600     INSPECT MOD-IDRADNR-UT  REPLACING LEADING ZERO BY SPACE              
044700     IF MOD-IDKUNDNR-UT          = SPACE                                  
044800        MOVE '     0'           TO MOD-IDKUNDNR-UT                        
044900     END-IF                                                               
045000                                                                          
045100     IF NOT NYCKLAR-OK                                                    
045200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
045300       CALL WMEDKONV USING MED-WMEDAREA                                   
045400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
045500       PERFORM MFS-ERASE-FIELD-IN                                         
045600       PERFORM MFS-ERASE-FIELD-OUT                                        
045700     END-IF                                                               
045800     .                                                                    
045900     EJECT                                                                
046000 C-FOERSTA-SIDA SECTION.                                                  
046100                                                                          
046200     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
046300     CALL WMEDKONV USING MED-WMEDAREA                                     
046400     MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                  
046500                                                                          
046600*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
046700     .                                                                    
046800     EJECT                                                                
046900 D-NAESTA-SIDA SECTION.                                                   
047000                                                                          
047100     IF EGEN-MID OR HELP-MID                                              
047200        MOVE MID-IDARTNR-NEXT       TO W-IDARTNR                          
047300        MOVE MID-IDRADNR-NEXT       TO W-IDRADNR                          
047400     ELSE                                                                 
047500        MOVE LOW-VALUE              TO W-WDA211KY-MIN-X                   
047600     END-IF                                                               
047700     .                                                                    
047800     EJECT                                                                
047900 E-SAMMA-SIDA SECTION.                                                    
048000                                                                          
048100     IF EGEN-MID OR HELP-MID                                              
048200        MOVE MID-IDARTNR-ENTER      TO W-IDARTNR                          
048300        MOVE MID-IDRADNR-ENTER      TO W-IDRADNR                          
048400     ELSE                                                                 
048500        MOVE LOW-VALUE           TO W-WDA211KY-MIN-X                      
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900 F-LAES-VISA-INFO SECTION.                                                
049000                                                                          
049100                                                                          
049200     PERFORM IMS-GU-WLKREE01                                              
049300     IF SEGMENT-SAKNAS                                                    
049400        MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                           
049500        CALL WMEDKONV USING MED-WMEDAREA                                  
049600        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
049700        PERFORM MFS-RENSA-FAELT-UT                                        
049800     ELSE                                                                 
049900        MOVE ANM-RELANDCO       TO WS-RELANDCO                            
050000        MOVE ANM-PRFOERS        TO WS-PRFOERS                             
050100        MOVE ANM-PRFRAKT        TO WS-PRFRAKT                             
050200        MOVE ANM-PRLEGKST       TO WS-PRLEGKST                            
050300        MOVE ANM-KDLEVANM       TO WS-KDLEVANM                            
050400                                                                          
050500        IF DIST79-DEALER-PRICE OR                                         
050520           DIST79-ECOM-PRICE                                              
050600          MOVE  ANM-KDVALISO    TO MOD-KDVALISO                           
050700          IF ANM-KDVALISO = SPACE                                         
050800            PERFORM S10-HAMTA-KDVALISO                                    
050900          END-IF                                                          
051000        ELSE                                                              
051100*                                                                         
051101*CHINA-PRICE1                                                             
051110*INDIA-PRICE1                                                             
051120*KOREA-PRICE1                                                             
051200          MOVE ANM-IDFTG TO WS-IDFTG                                      
051210          EVALUATE TRUE                                                   
051300          WHEN IDFTG-CN                                                   
051400              MOVE 'CNY'        TO MOD-KDVALISO                           
051510          WHEN IDFTG-IN                                                   
051520              MOVE 'INR'        TO MOD-KDVALISO                           
051540          WHEN IDFTG-KR                                                   
051550              MOVE 'KRW'        TO MOD-KDVALISO                           
051570          WHEN IDFTG-TR                                                   
051580              MOVE 'TRY'        TO MOD-KDVALISO                           
051590          WHEN IDFTG-MX                                                   
051591              MOVE 'MXN'        TO MOD-KDVALISO                           
051592          WHEN IDFTG-BR                                                   
051593              MOVE 'BRL'        TO MOD-KDVALISO                           
051594          WHEN IDFTG-MY                                                   
051595              MOVE 'MYR'        TO MOD-KDVALISO                           
051596          WHEN IDFTG-TH                                                   
051597              MOVE 'THB'        TO MOD-KDVALISO                           
051598          WHEN IDFTG-TW                                                   
051599              MOVE 'TWD'        TO MOD-KDVALISO                           
051600          WHEN IDFTG-ZA                                                   
051601              MOVE 'ZAR'        TO MOD-KDVALISO                           
051610          WHEN OTHER                                                      
051700              MOVE 'SEK'        TO MOD-KDVALISO                           
051710          END-EVALUATE                                                    
051800        END-IF                                                            
051900                                                                          
052000        PERFORM IMS-GNP-WLKREE11-KVAL                                     
052100        PERFORM FA-FIXA-ENTER-KEY                                         
052200                                                                          
052300        MOVE +1                  TO INDX                                  
052400        MOVE ZERO                TO WS-SUARTOMK-TK                        
052500                                    WS-SUARTFSG-RAD                       
052600        IF MFS-FIRST OR MFS-ENTER                                         
052700            MOVE ZERO            TO WS-SUACKFSG-KRE-SPAR                  
052800        ELSE                                                              
052900          IF MID-SUACKFSG-KRE-SPAR NUMERIC                                
053000            MOVE MID-SUACKFSG-KRE-SPAR TO WS-SUACKFSG-KRE-SPAR            
053100          END-IF                                                          
053200        END-IF                                                            
053300                                                                          
053400        IF MFS-FIRST OR MFS-ENTER                                         
053500            MOVE ZERO            TO WS-SUACKFSG-DEB-SPAR                  
053600        ELSE                                                              
053700          IF MID-SUACKFSG-DEB-SPAR NUMERIC                                
053800              MOVE MID-SUACKFSG-DEB-SPAR TO WS-SUACKFSG-DEB-SPAR          
053900          END-IF                                                          
054000        END-IF                                                            
054100        PERFORM UNTIL INDX > MAX-RAD                                      
054200           IF SEGMENT-FINNS                                               
054300             MOVE LEV-KDANMORS  TO OKOD-KDANMORS                          
054400             CALL W418OKOD USING OKOD-W418OKOD                            
054500                                                                          
054600             MOVE LEV-IDORDNR7  TO MOD-IDORDNR             (INDX)         
054700             MOVE LEV-IDARTNR   TO MOD-IDARTNR             (INDX)         
054800             MOVE LEV-KDANMORS  TO MOD-KDANMORS            (INDX)         
054900                                                                          
055000             IF (WS-KDLEVANM < '7') OR OKOD-FL-RETILL = 'N'               
055100                                                                          
055200               MOVE LEV-KVLEVANM-BEKR TO MOD-KVLEVANM      (INDX)         
055300                                          WS-KVLEVANM                     
055400             ELSE                                                         
055500               MOVE LEV-KVRETINL     TO WS-KVLEVANM                       
055600               ADD  LEV-KVRETINL-SKR TO WS-KVLEVANM                       
055700               MOVE WS-KVLEVANM      TO MOD-KVLEVANM       (INDX)         
055800             END-IF                                                       
055900                                                                          
056000             MOVE LEV-TIKNOTA   TO MOD-TIKRENOT            (INDX)         
056100             IF DIST79-DEALER-PRICE OR                                    
056120                DIST79-ECOM-PRICE                                         
056200               MOVE LEV-PRARTBTO-LOC TO MOD-PRARTBTO       (INDX)         
056300             ELSE                                                         
056400*CHINA-PRICE2                                                             
056410*INDIA-PRICE2                                                             
056420*KOREA-PRICE2                                                             
056500               MOVE LEV-IDFTG TO WS-IDFTG                                 
056600               IF IDFTG-CN OR                                             
056610                  IDFTG-IN OR                                             
056620                  IDFTG-KR OR                                             
056630                  IDFTG-TR OR                                             
056631                  IDFTG-MX OR                                             
056632                  IDFTG-BR OR                                             
056640                  IDFTG-MY OR                                             
056650                  IDFTG-TH OR                                             
056660                  IDFTG-TW OR                                             
056670                  IDFTG-ZA                                                
056700                 MOVE LEV-PRARTBTO-LOCINV TO MOD-PRARTBTO  (INDX)         
056800               ELSE                                                       
056900                 MOVE LEV-PRARTBTO TO MOD-PRARTBTO         (INDX)         
057000               END-IF                                                     
057100             END-IF                                                       
057200                                                                          
057300             IF OKOD-FL-KOD-SOM-BAER-TK = 'J'                             
057400               IF DIST79-DEALER-PRICE OR                                  
057420                  DIST79-ECOM-PRICE                                       
057500                 COMPUTE WS-SUARTOMK-TK ROUNDED =                         
057600                 ((LEV-PRARTBTO-LOC * WS-KVLEVANM) *                      
057700                                   WS-RELANDCO) / 100                     
057800               ELSE                                                       
057900*CHINA-PRICE3                                                             
057910*INDIA-PRICE3                                                             
057920*KOREA-PRICE3                                                             
057930*TURKEY-PRICE3                                                            
057940*MEXICO-PRICE3                                                            
057950*BRASIL-PRICE3                                                            
058000                 MOVE LEV-IDFTG TO WS-IDFTG                               
058100                 IF IDFTG-CN OR                                           
058110                    IDFTG-IN OR                                           
058120                    IDFTG-KR OR                                           
058130                    IDFTG-TR OR                                           
058131                    IDFTG-MX OR                                           
058132                    IDFTG-BR OR                                           
058140                    IDFTG-MY OR                                           
058150                    IDFTG-TH OR                                           
058160                    IDFTG-TW OR                                           
058170                    IDFTG-ZA                                              
058200                   COMPUTE WS-SUARTOMK-TK ROUNDED =                       
058300                   ((LEV-PRARTBTO-LOCINV * WS-KVLEVANM) *                 
058400                                     WS-RELANDCO) / 100                   
058500                 ELSE                                                     
058600                   COMPUTE WS-SUARTOMK-TK ROUNDED =                       
058700                   ((LEV-PRARTBTO * WS-KVLEVANM) *                        
058800                                     WS-RELANDCO) / 100                   
058900                 END-IF                                                   
059000               END-IF                                                     
059100             END-IF                                                       
059200             MOVE WS-SUARTOMK-TK TO MOD-SUARTOMK-TK        (INDX)         
059300                                                                          
059400             IF DIST79-DEALER-PRICE OR                                    
059420                DIST79-ECOM-PRICE                                         
059500               COMPUTE WS-SUARTFSG-RAD = (WS-KVLEVANM *                   
059600                       LEV-PRARTBTO-LOC) + WS-SUARTOMK-TK                 
059700             ELSE                                                         
059800*CHINA-PRICE3                                                             
059810*INDIA-PRICE3                                                             
059820*KOREA-PRICE3                                                             
059900               MOVE LEV-IDFTG TO WS-IDFTG                                 
060000               IF IDFTG-CN OR                                             
060010                  IDFTG-IN OR                                             
060020                  IDFTG-KR OR                                             
060030                  IDFTG-TR OR                                             
060031                  IDFTG-MX OR                                             
060032                  IDFTG-BR OR                                             
060040                  IDFTG-MY OR                                             
060050                  IDFTG-TH OR                                             
060060                  IDFTG-TW OR                                             
060070                  IDFTG-ZA                                                
060100                 COMPUTE WS-SUARTFSG-RAD = (WS-KVLEVANM *                 
060200                         LEV-PRARTBTO-LOCINV) + WS-SUARTOMK-TK            
060300               ELSE                                                       
060400                 COMPUTE WS-SUARTFSG-RAD = (WS-KVLEVANM *                 
060500                         LEV-PRARTBTO) + WS-SUARTOMK-TK                   
060600               END-IF                                                     
060700             END-IF                                                       
060800             MOVE WS-SUARTFSG-RAD TO MOD-SUARTFSG-RAD      (INDX)         
060900                                                                          
061000             IF OKOD-FL-TF = 'J'                                          
061100               COMPUTE WS-SUACKFSG-DEB-SPAR ROUNDED =                     
061200                                          WS-SUACKFSG-DEB-SPAR +          
061300                                          WS-SUARTFSG-RAD                 
061400             ELSE                                                         
061500               COMPUTE WS-SUACKFSG-KRE-SPAR ROUNDED =                     
061600                                         WS-SUACKFSG-KRE-SPAR +           
061700                                         WS-SUARTFSG-RAD                  
061800             END-IF                                                       
061900                                                                          
062000             MOVE ZERO          TO WS-SUARTOMK-TK                         
062100                                   WS-SUARTFSG-RAD                        
062200                                                                          
062300             MOVE LEV-IDKNOTNR  TO MOD-IDKNOTNR            (INDX)         
062400                                                                          
062500             PERFORM IMS-GNP-WLKREE11                                     
062600           ELSE                                                           
062700               MOVE MFS-RENSA-FAELT    TO MOD-IDORDNR      (INDX)         
062800                                          MOD-IDARTNR      (INDX)         
062900                                          MOD-KDANMORS     (INDX)         
063000                                          MOD-KVLEVANM     (INDX)         
063100                                          MOD-PRARTBTO     (INDX)         
063200                                          MOD-SUARTOMK-TK  (INDX)         
063300                                          MOD-SUARTFSG-RAD (INDX)         
063400                                          MOD-IDKNOTNR     (INDX)         
063500                                          MOD-TIKRENOT     (INDX)         
063600           END-IF                                                         
063700           ADD +1               TO INDX                                   
063800        END-PERFORM                                                       
063900        IF SEGMENT-FINNS                                                  
064000          MOVE MFS-BLANKA-UT-FAELT TO MOD-SUACKFSG-KRE-TOT                
064100                                      MOD-SUACKFSG-DEB-TOT                
064200        ELSE                                                              
064300          IF WS-SUACKFSG-DEB-SPAR > ZERO                                  
064400              COMPUTE WS-SUACKFSG-DEB-SPAR ROUNDED =                      
064500                                      WS-SUACKFSG-DEB-SPAR +              
064600                                      WS-PRFOERS      +                   
064700                                      WS-PRFRAKT      +                   
064800                                      WS-PRLEGKST                         
064900              MOVE WS-SUACKFSG-DEB-SPAR TO MOD-SUACKFSG-DEB-TOT           
065000          ELSE                                                            
065100            MOVE MFS-BLANKA-UT-FAELT TO MOD-SUACKFSG-DEB-TOT              
065200          END-IF                                                          
065300                                                                          
065400          IF WS-SUACKFSG-KRE-SPAR > ZERO                                  
065500               COMPUTE WS-SUACKFSG-KRE-SPAR ROUNDED =                     
065600                                          WS-SUACKFSG-KRE-SPAR +          
065700                                          WS-PRFOERS      +               
065800                                          WS-PRFRAKT      +               
065900                                          WS-PRLEGKST                     
066000               MOVE WS-SUACKFSG-KRE-SPAR TO MOD-SUACKFSG-KRE-TOT          
066100          ELSE                                                            
066200            MOVE MFS-BLANKA-UT-FAELT TO MOD-SUACKFSG-KRE-TOT              
066300          END-IF                                                          
066400        END-IF                                                            
066500        MOVE WS-SUACKFSG-KRE-SPAR TO MOD-SUACKFSG-KRE-SPAR                
066600        MOVE WS-SUACKFSG-DEB-SPAR TO MOD-SUACKFSG-DEB-SPAR                
066700        PERFORM FB-FIXA-NEXT-KEY                                          
066800     END-IF                                                               
066900     .                                                                    
067000     EJECT                                                                
067100 FA-FIXA-ENTER-KEY        SECTION.                                        
067200                                                                          
067300     IF SEGMENT-FINNS                                                     
067400        MOVE LEV-IDARTNR            TO MOD-IDARTNR-ENTER                  
067500        MOVE LEV-IDRADNR            TO MOD-IDRADNR-ENTER                  
067600     ELSE                                                                 
067700        MOVE ZERO                   TO MOD-IDARTNR-ENTER                  
067800                                       MOD-IDRADNR-ENTER                  
067900     END-IF                                                               
068000     .                                                                    
068100     EJECT                                                                
068200                                                                          
068300 FB-FIXA-NEXT-KEY        SECTION.                                         
068400                                                                          
068500     IF SEGMENT-FINNS                                                     
068600        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
068700        CALL WMEDKONV USING MED-WMEDAREA                                  
068800        MOVE MED-TEMFSINF           TO MOD-TEMFSINF                       
068900                                                                          
069000        MOVE LEV-IDARTNR            TO MOD-IDARTNR-NEXT                   
069100        MOVE LEV-IDRADNR            TO MOD-IDRADNR-NEXT                   
069200     ELSE                                                                 
069300        MOVE ZERO                   TO MOD-IDARTNR-NEXT                   
069400                                       MOD-IDRADNR-NEXT                   
069500                                       MOD-SUACKFSG-KRE-SPAR              
069600                                       MOD-SUACKFSG-DEB-SPAR              
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 G-SECURIT-KONTROLL SECTION.                                              
070100                                                                          
070200     IF MSGI-IDDISTR NUMERIC                                              
070300        MOVE MSG-SIGNON-USERID TO SEC-IDUSER                              
070400        MOVE '4716'            TO SEC-IDTRANS                             
070500        MOVE MSGI-IDDISTR      TO SEC-IDKEY                               
070600        CALL WSECURIT USING       SEC-IDUSER                              
070700                                  SEC-IDTRANS                             
070800                                  SEC-IDKEY                               
070900                                  SEC-KDSVAR                              
071000     END-IF                                                               
071100     .                                                                    
071200     EJECT                                                                
071300 S10-HAMTA-KDVALISO   SECTION.                                            
071400                                                                          
071500     PERFORM IMS-GU-GMTA-WDB201                                           
071600     IF SEGMENT-FINNS                                                     
071700       CONTINUE                                                           
071800     ELSE                                                                 
071900       PERFORM IMS-GET-WDB201                                             
072000     END-IF                                                               
072100     MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                           
072200     MOVE GMT-IDFTG          TO W-WDB1-IDFTG                              
072300     PERFORM IMS-GU-WDB1-WDB101                                           
072400     IF SEGMENT-FINNS                                                     
072500       IF DIST79-DEALER-PRICE                                             
072600         MOVE BET-KDVALISO   TO MOD-KDVALISO                              
072700       ELSE                                                               
072800         IF DIST79-ECOM-PRICE                                             
073100           MOVE SPACE        TO MOD-KDVALISO                              
073210         ELSE                                                             
073220           MOVE 'SEK'        TO MOD-KDVALISO                              
073221         END-IF                                                           
073230       END-IF                                                             
073300     ELSE                                                                 
073400       MOVE SPACE            TO MOD-KDVALISO                              
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
075000 MFS-RENSA-FAELT-UT SECTION.                                              
075100                                                                          
075200     MOVE +1 TO INDX                                                      
075300     PERFORM UNTIL INDX > MAX-RAD                                         
075400        MOVE MFS-RENSA-FAELT  TO MOD-IDORDNR    (INDX)                    
075500                                 MOD-IDARTNR    (INDX)                    
075600                                 MOD-KDANMORS   (INDX)                    
075700                                 MOD-KVLEVANM   (INDX)                    
075800                                 MOD-PRARTBTO   (INDX)                    
075900                                 MOD-SUARTOMK-TK (INDX)                   
076000                                 MOD-SUARTFSG-RAD (INDX)                  
076100                                 MOD-IDKNOTNR   (INDX)                    
076200                                 MOD-TIKRENOT   (INDX)                    
076300        ADD +1 TO INDX                                                    
076400     END-PERFORM                                                          
076500                                                                          
076600     MOVE MFS-RENSA-FAELT     TO MOD-SUACKFSG-KRE-TOT                     
076700                                 MOD-SUACKFSG-DEB-TOT                     
076800     .                                                                    
076900     EJECT                                                                
077000 MFS-ERASE-FIELD-OUT SECTION.                                             
077100                                                                          
077200*    --- ALLA UTDATA-FÄLT                                                 
077300*    --- INCL. SCROLL KEYS                                                
077400     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UT                               
077500                             MOD-IDKUNDNR-UT                              
077600                             MOD-IDRAPPNR-UT                              
077700                             MOD-IDARTNR-UT                               
077800                             MOD-IDRADNR-UT                               
077900                             MOD-IDARTNR-ENTER                            
078000                             MOD-IDARTNR-NEXT                             
078100                             MOD-IDRADNR-ENTER                            
078200                             MOD-IDRADNR-NEXT                             
078300     .                                                                    
078400 MFS-ERASE-FIELD-IN SECTION.                                              
078500                                                                          
078600*    --- ALLA INDATA-FÄLT                                                 
078700     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
078800                             MOD-IDKUNDNR-IN                              
078900                             MOD-IDRAPPNR-IN                              
079000                             MOD-IDARTNR-IN                               
079100                             MOD-IDRADNR-IN                               
079200     .                                                                    
079300     EJECT                                                                
079400* IMS SEKTIONER                                                           
079500                                                                          
079600 IMS-GET-MSG SECTION.                                                     
079700                                                                          
079800     MOVE '  QC' TO GODK-STATUSKODER                                      
079900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
080000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
080100     PERFORM IMS-STATUSKONTROLL                                           
080200                                                                          
080300     .                                                                    
080400 IMS-INSERT-MSG SECTION.                                                  
080500                                                                          
080600     IF MSGI-IDLAND-SPR = 'GB'                                            
080700       MOVE 'N' TO MFS-KDHUVOMR                                           
080800     END-IF                                                               
080900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
081000     MOVE SPACE TO GODK-STATUSKODER                                       
081100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
081200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
081300     PERFORM IMS-STATUSKONTROLL                                           
081400     EJECT                                                                
081500                                                                          
081600     .                                                                    
081700 IMS-GU-WLKREE01 SECTION.                                                 
081800                                                                          
081900     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
082000            DELIMITED BY SIZE INTO SSA1                                   
082100     MOVE '  GE' TO GODK-STATUSKODER                                      
082200     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA SSA1                      
082300     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
082400     PERFORM IMS-STATUSKONTROLL                                           
082500     EJECT                                                                
082600                                                                          
082700     .                                                                    
082800 IMS-GNP-WLKREE11-KVAL SECTION.                                           
082900                                                                          
083000     STRING 'WLKREE11(WDA211KY>=' W-WDA211KY-MIN-X ')'                    
083100            DELIMITED BY SIZE INTO SSA1                                   
083200     MOVE '  GE' TO GODK-STATUSKODER                                      
083300     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
083400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
083500     PERFORM IMS-STATUSKONTROLL                                           
083600                                                                          
083700     .                                                                    
083800 IMS-GNP-WLKREE11     SECTION.                                            
083900                                                                          
084000     MOVE 'WLKREE11 '      TO SSA1                                        
084100     MOVE '  GE' TO GODK-STATUSKODER                                      
084200     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
084300     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
084400     PERFORM IMS-STATUSKONTROLL                                           
084500     .                                                                    
084600     EJECT                                                                
084700 IMS-GU-GMTA-WDB201               SECTION.                                
084800                                                                          
084900     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
085000          DELIMITED BY SIZE INTO SSA1                                     
085100     MOVE '  GE'              TO GODK-STATUSKODER                         
085200                                                                          
085300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
085400     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
085500     PERFORM IMS-STATUSKONTROLL                                           
085600     .                                                                    
085700     EJECT                                                                
085800 IMS-GET-WDB201 SECTION.                                                  
085900                                                                          
086000     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
086100                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
086200          DELIMITED BY SIZE INTO SSA1                                     
086300     MOVE '    '              TO GODK-STATUSKODER                         
086400                                                                          
086500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
086600     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     .                                                                    
086900     EJECT                                                                
087000 IMS-GU-WDB1-WDB101              SECTION.                                 
087100                                                                          
087200     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
087300          DELIMITED BY SIZE INTO SSA1                                     
087400     MOVE '  GE'              TO GODK-STATUSKODER                         
087500                                                                          
087600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
087700     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
087800     PERFORM IMS-STATUSKONTROLL                                           
087900     .                                                                    
088000     EJECT                                                                
088100 IMS-STATUSKONTROLL SECTION.                                              
088200                                                                          
088300     SET STATUS-IX TO 1                                                   
088400     SEARCH GODK-STATUS AT END CALL FELLOG                                
088500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
088600     END-SEARCH.                                                          
