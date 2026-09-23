000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4057400.                                                
000400 AUTHOR.         LENNART LUNDGREN ADB-GRUPPEN.                            
000500 DATE-WRITTEN.   MAJ 1988.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION. DETTA PGM HANTERAR FRÅGEBILDEN 4574 OCH LÄSER              
001000*              RESTORDERREGISTRET FÖRST VIA SEKUNDÄWINDEXET               
001100*              WDA5D1 OCH SEDAN WDA501 OM KDSTARAD = 4.                   
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T574                                              
001400*        MID:         W4I57401                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O57401                                            
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP3                                                                
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002301*    -- CHECKED BY WY2000                                                 
002310     SKIP3                                                                
002400 77  PROGRAM-NAMN              PIC X(8)     VALUE 'W4057400'.             
002500 77  JA                        PIC X(1)     VALUE 'J'.                    
002600 77  NEJ                       PIC X(1)     VALUE 'N'.                    
002700 77  W-NYSIDA                  PIC X(1)     VALUE '8'.                    
002800 77  INPUT-OK                  PIC X(1)     VALUE 'J'.                    
002900 77  W-NEWKEY                  PIC X(1)     VALUE ' '.                    
003000 77  SEGM-SKALL-SKRIVAS        PIC X(1)     VALUE 'N'.                    
003100 77  MINST-1-RAD-SKRIVEN       PIC X(1)     VALUE 'N'.                    
003200 77  IDDISTR-WS                PIC X(4)     VALUE SPACE.                  
003300 77  IDKUNDNR-WS               PIC X(6)     VALUE SPACE.                  
003400 77  IDARTNR-WS                PIC X(9)     VALUE SPACE.                  
003500 77  IDORDNR-WS                PIC X(5)     VALUE SPACE.                  
003600 77  INDX                      PIC S9(9)    VALUE ZERO  COMP SYNC.        
003700 77  SPRAK-IX                  PIC S9(2)    VALUE ZERO  COMP SYNC.        
003800 77  MAX-LINE                  PIC S9(9)    VALUE +14   COMP SYNC.        
003900 77  W-IDDISTR                 PIC 9(4)     VALUE ZERO.                   
004000 77  W-IDKUNDNR                PIC 9(6)     VALUE ZERO.                   
004100 77  W-IDARTNR                 PIC 9(9)     VALUE ZERO.                   
004200 77  W-IDORDNR                 PIC 9(5)     VALUE ZERO.                   
004300 77  WS-IDTRANS                PIC X(4).                                  
004400     88  WS-GODKAEND-BILD                VALUE  '4574'.                   
004500     88  EGEN-MID                        VALUE  '4574'.                   
004600     SKIP3                                                                
004700 01  W-IDKUNDRF.                                                          
004800     03  W-IDKUNDRF-1-5        PIC 9(5).                                  
004900     03  FILLER                PIC X(5) VALUE SPACE.                      
005000                                                                          
005100*                                                                         
005200 01  SUBPROGRAM.                                                          
005300     03  CBLTDLI                  PIC X(8)      VALUE 'CBLTDLI '.         
005400     03  FELLOG                   PIC X(8)      VALUE 'FELLOG  '.         
005500     03  WSECURIT                 PIC X(8)      VALUE 'WSECURIT'.         
005600     03  W005INIT                 PIC X(8)      VALUE 'W005INIT'.         
005700     EJECT                                                                
005800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
005900*01 -COPY WMSGINIT                                                        
006000     EJECT                                                                
006100*                                                                         
006200 01  NYCKLAR-TILL-DLI.                                                    
006300*                                                                         
006400     03  W-SOK-KEY-MIN.                                                   
006500         05  W-IDARTNR-MIN-X.                                             
006600             07  W-IDARTNR-MIN    PIC S9(9)  COMP-3 VALUE ZERO.           
006700         05  W-IDKUNDRF-MIN.                                              
006800             07  W-IDORDNR-MIN    PIC 9(5)          VALUE ZERO.           
006900             07  FILLER           PIC X(5)          VALUE SPACE.          
007000         05  W-KDSTARAD-MIN       PIC  X(1)         VALUE '4'.            
007100*                                                                         
007200     03  W-SOK-KEY-MAX.                                                   
007300         05  W-IDARTNR-MAX-X.                                             
007400             07  W-IDARTNR-MAX    PIC S9(9)  COMP-3  VALUE ZERO.          
007500         05  W-IDKUNDRF-MAX.                                              
007600             07  W-IDORDNR-MAX    PIC 9(5)           VALUE ZERO.          
007700             07  FILLER           PIC X(5)           VALUE SPACE.         
007800         05  W-KDSTARAD-MAX       PIC  X(1)          VALUE '4'.           
007900     EJECT                                                                
008000     03  W-WDA5D1KY-MIN.                                                  
008100         05  W-IDDISTR-N1-MIN     PIC S9(5) COMP-3   VALUE ZERO.          
008200         05  W-IDKUNDNR-N1-MIN    PIC S9(7) COMP-3   VALUE ZERO.          
008300         05  W-IDARTNR-N1-MIN     PIC S9(9) COMP-3   VALUE ZERO.          
008400         05  W-IDKUNDRF-N1-MIN.                                           
008500             07  W-IDORDNR-N1-MIN PIC 9(5)           VALUE ZERO.          
008600             07  FILLER           PIC X(5)           VALUE SPACE.         
008700         05  W-IDLOPNR-N1-MIN     PIC S9(3) COMP-3   VALUE ZERO.          
008800                                                                          
008900     03  W-WDA5D1KY-MAX.                                                  
009000         05  W-IDDISTR-N1-MAX     PIC S9(5) COMP-3   VALUE ZERO.          
009100         05  W-IDKUNDNR-N1-MAX    PIC S9(7) COMP-3   VALUE ZERO.          
009200         05  W-IDARTNR-N1-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
009300         05  W-IDKUNDRF-N1-MAX.                                           
009400             07  W-IDORDNR-N1-MAX PIC 9(5)           VALUE ZERO.          
009500             07  FILLER           PIC X(5)           VALUE SPACE.         
009600         05  W-IDLOPNR-N1-MAX     PIC S9(3) COMP-3   VALUE ZERO.          
009700                                                                          
009800     03  W-WDA501KY.                                                      
009900         05  W-IDDISTR-N2         PIC S9(5) COMP-3   VALUE ZERO.          
010000         05  W-IDKUNDNR-N2        PIC S9(7) COMP-3   VALUE ZERO.          
010100         05  W-IDKUNDRF-N2.                                               
010200             07  W-IDORDNR-N2     PIC 9(5)           VALUE ZERO.          
010300             07  FILLER           PIC X(5)           VALUE SPACE.         
010400         05  W-IDARTNR-N2         PIC S9(9) COMP-3   VALUE ZERO.          
010500         05  W-IDLOPNR-N2         PIC S9(3) COMP-3   VALUE ZERO.          
010600                                                                          
010700***********************************************                           
010800                                                                          
010900     EJECT                                                                
011000*01    -COPY WWTEXT01                                                     
011100     EJECT                                                                
011200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
011300                                                                          
011400 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
011500     SKIP3                                                                
011600*01    MID   -COPY W4I57401.                                              
011700     EJECT                                                                
011800*01    -COPY WMSGAREA                                                     
011900     EJECT                                                                
012000*  03    MOD -COPY W4O57401  -RED MSG-AREA.                               
012100     EJECT                                                                
012200*01    -COPY WMFSAREA                                                     
012300     EJECT                                                                
012400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012500                                                                          
012600 01    IMS-WS.                                                            
012700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
012800     SKIP3                                                                
012900*                        **** STATUS-KOD FRÅN IMS                         
013000   03    STATUS-WS               PIC XX.                                  
013100     88    SEGMENT-FINNS                    VALUE '  '.                   
013200     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
013300     88    SEGMENT-SLUT                     VALUE 'GB'.                   
013400     SKIP3                                                                
013500   03    GODK-STATUSKODER.                                                
013600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
013700     SKIP3                                                                
013800 01    SSA1                      PIC X(320).                              
013900     EJECT                                                                
014000*                            IMS FUNKTIONSKODER                           
014100*01    -COPY W0003                                                        
014200     EJECT                                                                
014300*                            DLI INPUT-OUTPUT AREA                        
014400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WLORDP'.           
014500 01  DLI-IO-WLORDP.                                                       
014600*  03    WDA501 -COPY WDA501                                              
014700     EJECT                                                                
014800                                                                          
014900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WLORDT'.           
015000 01  DLI-IO-WLORDT.                                                       
015100*  03    WDA5D1 -COPY WDA5D1                                              
015200     EJECT                                                                
015300*  03    FILLER -COPY WSECAREA.                                           
015400     EJECT                                                                
015500 LINKAGE SECTION.                                                         
015600*01    -COPY W0009     -PRE MSG-                                          
015700     EJECT                                                                
015800*01    -COPY W0008     -PRE USEA-                                         
015900     05  FILLER                  PIC X.                                   
016000     EJECT                                                                
016100*01    -COPY W0008     -PRE ORDP-                                         
016200     05  FILLER                  PIC X.                                   
016300     EJECT                                                                
016400*01    -COPY W0008     -PRE ORDT-                                         
016500     05  FILLER                  PIC X.                                   
016600     EJECT                                                                
016700 PROCEDURE DIVISION USING MSG-PCB  USEA-PCB ORDP-PCB ORDT-PCB.            
016800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ORDP-PCB ORDT-PCB.            
016900                                                                          
017000     PERFORM IMS-GET-MSG                                                  
017100     IF SEGMENT-FINNS                                                     
017200       PERFORM A-INIT                                                     
017300       IF SEC-KDSVAR = ' ' OR                                             
017400          SEC-KDSVAR = '1' OR                                             
017500          SEC-KDSVAR = '2' OR                                             
017600          SEC-KDSVAR = '3' OR                                             
017700          SEC-KDSVAR = '5' OR                                             
017800          SEC-KDSVAR = '6'                                                
017900         IF INPUT-OK = JA                                                 
018000           EVALUATE TRUE                                                  
018100           WHEN WS-IDTRANS NOT = '4574'                                   
018200             PERFORM D-FLYTTA-GAMMAL-KEY                                  
018300             PERFORM M-LAES                                               
018400           WHEN MFS-IDPFK = 7                                             
018500             PERFORM D-FLYTTA-GAMMAL-KEY                                  
018600             PERFORM M-LAES                                               
018700           WHEN MFS-IDPFK = 8                                             
018800             IF MID-IDDISTR-SPAR NOT = '0000'                             
018900               PERFORM C-FLYTTA-SPARAD-KEY                                
019000             ELSE                                                         
019100               PERFORM E1-FLYTTA-KEY                                      
019200             END-IF                                                       
019300             PERFORM N-LAES                                               
019400           WHEN OTHER                                                     
019500             PERFORM E2-FLYTTA-KEY                                        
019600             IF W-NEWKEY = JA                                             
019700               PERFORM M-LAES                                             
019800             ELSE                                                         
019900               PERFORM N-LAES                                             
020000             END-IF                                                       
020100           END-EVALUATE                                                   
020200         ELSE                                                             
020300           MOVE TEXT-0401 (SPRAK-IX) TO MOD-TEMFSFEL                      
020400         END-IF                                                           
020500       ELSE                                                               
020600         MOVE TEXT-0405 (SPRAK-IX) TO MOD-TEMFSFEL                        
020700       END-IF                                                             
020800     END-IF                                                               
020900     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O57401 + 4                        
021000     PERFORM IMS-INSERT-MSG                                               
021100*                                                                         
021200     MOVE ZERO TO RETURN-CODE                                             
021300     GOBACK.                                                              
021400     EJECT                                                                
021500 A-INIT SECTION.                                                          
021600*                                                                         
021700     IF MSG-DUBBLA-TRANSKODER                                             
021800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I57401                 
021900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
022000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
022100     ELSE                                                                 
022200       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I57401                   
022300       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
022400       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
022500     END-IF                                                               
022600     MOVE MSG-IDPFK TO MFS-IDPFK                                          
022700     MOVE MFS-IDTRANS                     TO WS-IDTRANS                   
022800*                                                                         
022900     MOVE LOW-VALUE TO MSG-AREA                                           
023000     MOVE 'W4O574N1' TO MFS-IDMOD                                         
023100     MOVE '4574' TO MOD-IDTRANS                                           
023200     MOVE ZERO TO MOD-SPARADE-NYCKLAR                                     
023300                  MOD-SPARADE-NYCKLAR-ENT                                 
023400*                                                                         
024000*                                                                         
024100     IF NOT EGEN-MID                                                      
024200       MOVE '7'            TO    MFS-IDPFK                                
024300     END-IF                                                               
024400                                                                          
024500     IF MID-IDDISTR-IN       NOT = ALL '+'                                
024600         MOVE SPACE TO MFS-IDPFK                                          
024700     END-IF                                                               
024800*                                                                         
024900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
025000     MOVE '001'             TO MSGI-KDCALL                                
025100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
025110     MOVE '4574'            TO MSGI-IDTRANS                               
025120     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
025200     IF EGEN-MID                                                          
025300        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
025400        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
025500        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
025600     END-IF                                                               
025700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
025710     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
025720         MOVE +1 TO SPRAK-IX                                              
025730     ELSE                                                                 
025740         MOVE +2 TO SPRAK-IX                                              
025750     END-IF                                                               
025800     MOVE MSGI-IDDISTR       TO IDDISTR-WS                                
025900*                                                                         
026000     PERFORM S30-SECURIT                                                  
026100     IF SEC-KDSVAR = ' ' OR                                               
026200        SEC-KDSVAR = '1' OR                                               
026300        SEC-KDSVAR = '2' OR                                               
026400        SEC-KDSVAR = '3' OR                                               
026500        SEC-KDSVAR = '5' OR                                               
026600        SEC-KDSVAR = '6'                                                  
026700*                                                                         
026800        PERFORM AB-SPARA-INPUT                                            
026900                                                                          
027000        MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                            
027100                                MOD-IDKUNDNR-IN                           
027200                                MOD-IDARTNR-IN                            
027300                                MOD-IDORDNR-IN                            
027400                                MOD-TEMFSFEL                              
027500                                MOD-TEMFSINF                              
027600        MOVE +1 TO INDX                                                   
027700        PERFORM UNTIL INDX > MAX-LINE                                     
027800           MOVE MFS-RENSA-FAELT TO MOD-RAD(INDX)                          
027900           ADD +1 TO INDX                                                 
028000        END-PERFORM                                                       
028100                                                                          
028200        IF MID-SPARADE-NYCKLAR        = ALL '0' AND                       
028300           MID-SPARADE-NYCKLAR-ENT = ALL '0'                              
028400          MOVE SPACE TO MFS-IDPFK                                         
028500        END-IF                                                            
028600                                                                          
028700        IF W-IDDISTR             = 0 AND                                  
028800           MID-IDDISTR-SPAR      = 0 AND                                  
028900           MID-IDDISTR-SPAR-E = 0                                         
029000            MOVE NEJ            TO INPUT-OK                               
029100        END-IF                                                            
029200     END-IF                                                               
029300*                                                                         
029400     .                                                                    
029500     EJECT                                                                
029600 AB-SPARA-INPUT SECTION.                                                  
029700*                                                                         
029800     MOVE JA TO INPUT-OK                                                  
029900*                                                                         
030000     IF MID-IDKUNDNR-IN = ALL '+'                                         
030100         MOVE MID-IDKUNDNR-UT TO IDKUNDNR-WS                              
030200         INSPECT IDKUNDNR-WS REPLACING LEADING SPACE BY ZERO              
030300     ELSE                                                                 
030400         MOVE MID-IDKUNDNR-IN TO IDKUNDNR-WS                              
030500         MOVE SPACE TO MFS-IDPFK                                          
030600     END-IF                                                               
030700*                                                                         
030800     IF MID-IDARTNR-IN = ALL '+'                                          
030900         MOVE MID-IDARTNR-UT TO IDARTNR-WS                                
031000         INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO               
031100     ELSE                                                                 
031200         MOVE MID-IDARTNR-IN TO IDARTNR-WS                                
031300         MOVE SPACE TO MFS-IDPFK                                          
031400     END-IF                                                               
031500*                                                                         
031600     IF MID-IDORDNR-IN = ALL '+'                                          
031700         MOVE MID-IDORDNR-UT TO IDORDNR-WS                                
031800         INSPECT IDORDNR-WS REPLACING LEADING SPACE BY ZERO               
031900     ELSE                                                                 
032000         MOVE MID-IDORDNR-IN TO IDORDNR-WS                                
032100         MOVE SPACE TO MFS-IDPFK                                          
032200     END-IF                                                               
032300*                                                                         
032310     IF WS-GODKAEND-BILD                                                  
032320        CONTINUE                                                          
032330     ELSE                                                                 
032340        MOVE ZERO                       TO IDKUNDNR-WS                    
032350                                           IDARTNR-WS                     
032380                                           IDORDNR-WS                     
032391     END-IF                                                               
032392                                                                          
032400     MOVE MSGI-IDDISTR   TO MOD-IDDISTR-UT                                
032500     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
032600*                                                                         
032700     MOVE IDKUNDNR-WS  TO MOD-IDKUNDNR-UT                                 
032800     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
032900*                                                                         
033000     MOVE IDARTNR-WS   TO MOD-IDARTNR-UT                                  
033100     INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE              
033200*                                                                         
033300     MOVE IDORDNR-WS   TO MOD-IDORDNR-UT                                  
033400     INSPECT MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE              
033500*                                                                         
033600     IF IDDISTR-WS      NOT NUMERIC  OR                                   
033700        IDKUNDNR-WS     NOT NUMERIC  OR                                   
033800        IDARTNR-WS      NOT NUMERIC  OR                                   
033900        IDORDNR-WS      NOT NUMERIC                                       
034000       MOVE NEJ TO INPUT-OK                                               
034100     END-IF                                                               
034200*                                                                         
034300     IF INPUT-OK = JA                                                     
034400        MOVE IDDISTR-WS   TO W-IDDISTR                                    
034500        MOVE IDKUNDNR-WS  TO W-IDKUNDNR                                   
034600        MOVE IDARTNR-WS   TO W-IDARTNR                                    
034700        MOVE IDORDNR-WS   TO W-IDORDNR                                    
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 C-FLYTTA-SPARAD-KEY          SECTION.                                    
035200*                                                                         
035300     MOVE LOW-VALUE  TO W-WDA5D1KY-MIN                                    
035400                        W-SOK-KEY-MIN                                     
035500     MOVE HIGH-VALUE TO W-WDA5D1KY-MAX                                    
035600                        W-SOK-KEY-MAX                                     
035700     MOVE '4'        TO W-KDSTARAD-MAX                                    
035800*                                                                         
035900     IF MID-IDDISTR-SPAR  > ZERO                                          
036000       MOVE MID-IDDISTR-SPAR       TO W-IDDISTR-N1-MIN                    
036100                                      W-IDDISTR-N1-MAX                    
036200     ELSE                                                                 
036300       MOVE W-IDDISTR              TO W-IDDISTR-N1-MIN                    
036400                                      W-IDDISTR-N1-MAX                    
036500     END-IF                                                               
036600*                                                                         
036700     IF MID-IDKUNDNR-SPAR  > ZERO                                         
036800        MOVE MID-IDKUNDNR-SPAR  TO W-IDKUNDNR-N1-MIN                      
036900        IF W-IDKUNDNR      > ZERO                                         
037000           MOVE MID-IDKUNDNR-SPAR  TO W-IDKUNDNR-N1-MAX                   
037100        END-IF                                                            
037200     END-IF                                                               
037300*                                                                         
037400     IF MID-IDARTNR-SPAR  > ZERO                                          
037500       MOVE MID-IDARTNR-SPAR    TO W-IDARTNR-MIN                          
037600                                   W-IDARTNR-N1-MIN                       
037700       IF W-IDARTNR       > ZERO                                          
037800          MOVE MID-IDARTNR-SPAR    TO W-IDARTNR-MAX                       
037900                                      W-IDARTNR-N1-MAX                    
038000       END-IF                                                             
038100     END-IF                                                               
038200*                                                                         
038300     IF MID-IDORDNR-SPAR  > ZERO                                          
038400       MOVE MID-IDORDNR-SPAR    TO W-IDKUNDRF-1-5                         
038500       MOVE W-IDKUNDRF          TO W-IDKUNDRF-MIN                         
038600                                   W-IDKUNDRF-N1-MIN                      
038700       IF W-IDORDNR       > ZERO                                          
038800          MOVE W-IDKUNDRF          TO W-IDKUNDRF-MAX                      
038900                                      W-IDKUNDRF-N1-MAX                   
039000       END-IF                                                             
039100     END-IF                                                               
039200*                                                                         
039300     .                                                                    
039400     EJECT                                                                
039500 D-FLYTTA-GAMMAL-KEY        SECTION.                                      
039600*                                                                         
039700     MOVE LOW-VALUE           TO W-SOK-KEY-MIN                            
039800                                 W-WDA5D1KY-MIN                           
039900     MOVE HIGH-VALUE          TO W-SOK-KEY-MAX                            
040000                                 W-WDA5D1KY-MAX                           
040100     MOVE '4'                 TO W-KDSTARAD-MAX                           
040200*                                                                         
040300     MOVE W-IDDISTR          TO W-IDDISTR-N1-MIN                          
040400                                W-IDDISTR-N1-MAX                          
040500*                                                                         
040600     IF W-IDKUNDNR       > ZERO                                           
040700        MOVE W-IDKUNDNR       TO W-IDKUNDNR-N1-MIN                        
040800                                 W-IDKUNDNR-N1-MAX                        
040900     END-IF                                                               
041000*                                                                         
041100     IF W-IDARTNR       > ZERO                                            
041200       MOVE W-IDARTNR            TO W-IDARTNR-MIN                         
041300                                    W-IDARTNR-MAX                         
041400        IF W-IDKUNDNR     > ZERO                                          
041500           MOVE W-IDARTNR        TO W-IDARTNR-N1-MIN                      
041600                                    W-IDARTNR-N1-MAX                      
041700        END-IF                                                            
041800     END-IF                                                               
041900*                                                                         
042000     IF W-IDORDNR       > ZERO                                            
042100       MOVE W-IDORDNR             TO W-IDKUNDRF-1-5                       
042200       MOVE W-IDKUNDRF            TO W-IDKUNDRF-MIN                       
042300                                     W-IDKUNDRF-MAX                       
042400        IF W-IDKUNDNR     > ZERO AND                                      
042500           W-IDARTNR      > ZERO                                          
042600           MOVE W-IDKUNDRF         TO W-IDKUNDRF-N1-MIN                   
042700                                      W-IDKUNDRF-N1-MAX                   
042800        END-IF                                                            
042900     END-IF                                                               
043000     .                                                                    
043100     EJECT                                                                
043200 E1-FLYTTA-KEY     SECTION.                                               
043300*                                                                         
043400     MOVE LOW-VALUE  TO W-WDA5D1KY-MIN                                    
043500                        W-SOK-KEY-MIN                                     
043600     MOVE HIGH-VALUE TO W-WDA5D1KY-MAX                                    
043700                        W-SOK-KEY-MAX                                     
043800     MOVE '4'        TO W-KDSTARAD-MAX                                    
043900     MOVE MID-IDDISTR-SPAR-E TO W-IDDISTR-N1-MIN                          
044000                                W-IDDISTR-N1-MAX                          
044100     IF W-IDKUNDNR         > ZERO                                         
044200        MOVE W-IDKUNDNR TO W-IDKUNDNR-N1-MAX                              
044300        IF W-IDARTNR        > ZERO                                        
044400           MOVE W-IDARTNR TO W-IDARTNR-N1-MAX                             
044500           IF W-IDORDNR        > ZERO                                     
044600                MOVE W-IDKUNDRF     TO W-IDKUNDRF-N1-MAX                  
044700           END-IF                                                         
044800        END-IF                                                            
044900     END-IF                                                               
045000*                                                                         
045100     IF W-IDARTNR        > ZERO                                           
045200        MOVE W-IDARTNR   TO W-IDARTNR-MAX                                 
045300     END-IF                                                               
045400     IF W-IDORDNR        > ZERO                                           
045500        MOVE W-IDKUNDRF  TO W-IDKUNDRF-MAX                                
045600     END-IF                                                               
045700*                                                                         
045800     IF MID-IDKUNDNR-SPAR-E > ZERO                                        
045900        MOVE MID-IDKUNDNR-SPAR-E TO W-IDKUNDNR-N1-MIN                     
046000        IF MID-IDARTNR-SPAR-E > ZERO                                      
046100           MOVE MID-IDARTNR-SPAR-E TO W-IDARTNR-N1-MIN                    
046200           IF MID-IDORDNR-SPAR-E > ZERO                                   
046300                MOVE MID-IDORDNR-SPAR-E TO W-IDKUNDRF-N1-MIN              
046400           END-IF                                                         
046500        END-IF                                                            
046600     END-IF                                                               
046700*                                                                         
046800     IF MID-IDARTNR-SPAR-E > ZERO                                         
046900       MOVE MID-IDARTNR-SPAR-E   TO W-IDARTNR-MIN                         
047000         IF MID-IDARTNR-UT > ZERO                                         
047100           MOVE MID-IDARTNR-SPAR-E   TO W-IDARTNR-MAX                     
047200         END-IF                                                           
047300     END-IF                                                               
047400     IF MID-IDORDNR-SPAR-E > ZERO                                         
047500       MOVE MID-IDORDNR-SPAR-E    TO W-IDKUNDRF-MIN                       
047600         IF MID-IDORDNR-UT > ZERO                                         
047700           MOVE MID-IDORDNR-SPAR-E    TO W-IDKUNDRF-MAX                   
047800         END-IF                                                           
047900     END-IF                                                               
048000     .                                                                    
048100     EJECT                                                                
048200 E2-FLYTTA-KEY SECTION.                                                   
048300*                                                                         
048400     MOVE LOW-VALUE  TO W-WDA5D1KY-MIN                                    
048500                        W-SOK-KEY-MIN                                     
048600     MOVE HIGH-VALUE TO W-WDA5D1KY-MAX                                    
048700                        W-SOK-KEY-MAX                                     
048800     MOVE '4'        TO W-KDSTARAD-MAX                                    
048900*                                                                         
049000****** KOMMANDE IF-SATS KOLLAR OM DET ÄR FÖRSTA GÅNGEN MAN                
049100****** TRYCKER ENTER, ISÅFALL LÄSER MAN FRÅN INMATNINGSRADEN              
049200****** OM INTE SÅ LÄSER MAN FRÅN SPARADE NYKLAR RAD 20                    
049300*                                                                         
049400     IF MID-IDDISTR-IN  NOT = ALL '+' OR                                  
049500        MID-IDKUNDNR-IN NOT = ALL '+' OR                                  
049600        MID-IDARTNR-IN  NOT = ALL '+' OR                                  
049700        MID-IDORDNR-IN  NOT = ALL '+' OR                                  
049800        MID-IDDISTR-SPAR-E  = ALL '0'                                     
049900        MOVE JA              TO W-NEWKEY                                  
050000        MOVE W-IDDISTR       TO W-IDDISTR-N1-MIN                          
050100                                W-IDDISTR-N1-MAX                          
050200       IF MID-IDDISTR-IN NOT = ALL '+'                                    
050300         IF W-IDKUNDNR         > ZERO                                     
050400           MOVE W-IDKUNDNR   TO W-IDKUNDNR-N1-MIN                         
050500                                W-IDKUNDNR-N1-MAX                         
050600         END-IF                                                           
050700         IF W-IDARTNR          > ZERO                                     
050800            MOVE W-IDARTNR    TO W-IDARTNR-MIN                            
050900                                 W-IDARTNR-MAX                            
051000                                 W-IDARTNR-N1-MIN                         
051100                                 W-IDARTNR-N1-MAX                         
051200         END-IF                                                           
051300         IF W-IDORDNR          > ZERO                                     
051400            MOVE W-IDORDNR    TO W-IDKUNDRF-1-5                           
051500            MOVE W-IDKUNDRF   TO W-IDKUNDRF-MIN                           
051600                                 W-IDKUNDRF-MAX                           
051700                                 W-IDKUNDRF-N1-MIN                        
051800                                 W-IDKUNDRF-N1-MAX                        
051900         END-IF                                                           
052000         IF W-IDARTNR          > ZERO                                     
052100            MOVE W-IDARTNR    TO W-IDARTNR-MIN                            
052200                                 W-IDARTNR-MAX                            
052300                                 W-IDARTNR-N1-MIN                         
052400                                 W-IDARTNR-N1-MAX                         
052500         END-IF                                                           
052600       ELSE                                                               
052700        IF W-IDKUNDNR         > ZERO                                      
052800           MOVE W-IDKUNDNR   TO W-IDKUNDNR-N1-MIN                         
052900                                W-IDKUNDNR-N1-MAX                         
053000        END-IF                                                            
053100        IF W-IDARTNR          > ZERO                                      
053200           MOVE W-IDARTNR    TO W-IDARTNR-MIN                             
053300                                W-IDARTNR-MAX                             
053400                                W-IDARTNR-N1-MIN                          
053500                                W-IDARTNR-N1-MAX                          
053600        END-IF                                                            
053700        IF W-IDORDNR          > ZERO                                      
053800           MOVE W-IDORDNR    TO W-IDKUNDRF-1-5                            
053900           MOVE W-IDKUNDRF   TO W-IDKUNDRF-MIN                            
054000                                W-IDKUNDRF-MAX                            
054100                                W-IDKUNDRF-N1-MIN                         
054200                                W-IDKUNDRF-N1-MAX                         
054300        END-IF                                                            
054400       END-IF                                                             
054500     EJECT                                                                
054600*    VID ENTER UTAN NYA NYKLAR                                            
054700     ELSE                                                                 
054800*                                                                         
054900         MOVE NEJ                TO W-NEWKEY                              
055000         MOVE MID-IDDISTR-SPAR-E TO W-IDDISTR-N1-MIN                      
055100*                                   W-IDDISTR-N1-MAX                      
055200         IF W-IDKUNDNR         > ZERO                                     
055300            MOVE W-IDKUNDNR TO W-IDKUNDNR-N1-MAX                          
055400            IF W-IDARTNR        > ZERO                                    
055500               MOVE W-IDARTNR TO W-IDARTNR-N1-MAX                         
055600               IF W-IDORDNR        > ZERO                                 
055700                    MOVE W-IDKUNDRF     TO W-IDKUNDRF-N1-MAX              
055800               END-IF                                                     
055900            END-IF                                                        
056000         END-IF                                                           
056100*                                                                         
056200         IF W-IDARTNR        > ZERO                                       
056300            MOVE W-IDARTNR   TO W-IDARTNR-MAX                             
056400         END-IF                                                           
056500         IF W-IDORDNR        > ZERO                                       
056600            MOVE W-IDKUNDRF  TO W-IDKUNDRF-MAX                            
056700         END-IF                                                           
056800*                                                                         
056900         IF MID-IDKUNDNR-SPAR-E > ZERO                                    
057000            MOVE MID-IDKUNDNR-SPAR-E TO W-IDKUNDNR-N1-MIN                 
057100            IF MID-IDARTNR-SPAR-E > ZERO                                  
057200               MOVE MID-IDARTNR-SPAR-E TO W-IDARTNR-N1-MIN                
057300               IF MID-IDORDNR-SPAR-E > ZERO                               
057400                    MOVE MID-IDORDNR-SPAR-E TO W-IDKUNDRF-N1-MIN          
057500               END-IF                                                     
057600            END-IF                                                        
057700         END-IF                                                           
057800*                                                                         
057900         IF MID-IDARTNR-SPAR-E > ZERO                                     
058000           MOVE MID-IDARTNR-SPAR-E   TO W-IDARTNR-MIN                     
058100             IF MID-IDARTNR-UT > ZERO                                     
058200               MOVE MID-IDARTNR-SPAR-E   TO W-IDARTNR-MAX                 
058300             END-IF                                                       
058400         END-IF                                                           
058500         IF MID-IDORDNR-SPAR-E > ZERO                                     
058600           MOVE MID-IDORDNR-SPAR-E    TO W-IDKUNDRF-MIN                   
058700             IF MID-IDORDNR-UT > ZERO                                     
058800               MOVE MID-IDORDNR-SPAR-E    TO W-IDKUNDRF-MAX               
058900             END-IF                                                       
059000         END-IF                                                           
059100     END-IF.                                                              
059200     EJECT                                                                
059300 F-LAES-FLYTTA-TILL-MOD SECTION.                                          
059400*                                                                         
059500     IF SEGMENT-FINNS                                                     
059600        PERFORM FB-FLYTTA-TILL-KEY                                        
059700        PERFORM IMS-GU-RO                                                 
059800     END-IF                                                               
059900     MOVE +1 TO INDX                                                      
060000*                                                                         
060100     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
060200        SEGMENT-SLUT              OR                                      
060300        INDX > MAX-LINE                                                   
060400*                                                                         
060500        PERFORM FC-FLYTTA-TILL-MOD                                        
060600        ADD +1 TO INDX                                                    
060700        PERFORM IMS-GN-INDEX-RO                                           
060800        IF SEGMENT-FINNS                                                  
060900           PERFORM FB-FLYTTA-TILL-KEY                                     
061000           PERFORM IMS-GU-RO                                              
061100        END-IF                                                            
061200     END-PERFORM                                                          
061300*                                                                         
061400     IF SEGMENT-FINNS                                                     
061500        PERFORM FD-FLYTTA-TILL-SPAR-MOD                                   
061600     END-IF                                                               
061700     .                                                                    
061800     EJECT                                                                
061900 FB-FLYTTA-TILL-KEY     SECTION.                                          
062000     MOVE SEQD-IDDISTR    TO  W-IDDISTR-N2                                
062100     MOVE SEQD-IDKUNDNR   TO  W-IDKUNDNR-N2                               
062200     MOVE SEQD-IDKUNDRF   TO  W-IDKUNDRF-N2                               
062300     MOVE SEQD-IDARTNR    TO  W-IDARTNR-N2                                
062400     MOVE SEQD-IDLOPNR    TO  W-IDLOPNR-N2                                
062500     .                                                                    
062600     EJECT                                                                
062700 FC-FLYTTA-TILL-MOD SECTION.                                              
062800*                                                                         
062900     MOVE RAD-IDKUNDNR   TO MOD-IDKUNDNR(INDX)                            
063000     MOVE RAD-IDKUNDRF-LEV TO MOD-IDKUNDRF-LEV(INDX)                      
063100     MOVE RAD-IDARTNR    TO MOD-IDARTNR(INDX)                             
063200     MOVE RAD-KVART      TO MOD-KVART(INDX)                               
063300     MOVE RAD-IDKUNDRF   TO W-IDKUNDRF                                    
063400     MOVE W-IDKUNDRF-1-5 TO MOD-IDORDNR(INDX)                             
063410     MOVE RAD-IDDC       TO MOD-IDDC(INDX)                                
063500*                                                                         
063600     IF INDX = 1                                                          
063700       MOVE RAD-IDDISTR    TO MOD-IDDISTR-SPAR-E                          
063800       MOVE RAD-IDKUNDNR   TO MOD-IDKUNDNR-SPAR-E                         
063900       MOVE RAD-IDARTNR    TO MOD-IDARTNR-SPAR-E                          
064000       MOVE RAD-IDKUNDRF   TO W-IDKUNDRF                                  
064100       MOVE W-IDKUNDRF-1-5 TO MOD-IDORDNR-SPAR-E                          
064200     END-IF                                                               
064300     .                                                                    
064400     EJECT                                                                
064500 FD-FLYTTA-TILL-SPAR-MOD SECTION.                                         
064600     MOVE RAD-IDDISTR    TO MOD-IDDISTR-SPAR                              
064700     MOVE RAD-IDKUNDNR   TO MOD-IDKUNDNR-SPAR                             
064800     MOVE RAD-IDARTNR    TO MOD-IDARTNR-SPAR                              
064900     MOVE RAD-IDKUNDRF   TO W-IDKUNDRF                                    
065000     MOVE W-IDKUNDRF-1-5 TO MOD-IDORDNR-SPAR                              
065100     MOVE TEXT-0402 (SPRAK-IX) TO MOD-TEMFSINF                            
065200     .                                                                    
065300     EJECT                                                                
065400 M-LAES            SECTION.                                               
065500*                                                                         
065600     PERFORM IMS-GN-INDEX-RO                                              
065700     IF SEGMENT-FINNS                                                     
065800         PERFORM F-LAES-FLYTTA-TILL-MOD                                   
065900     ELSE                                                                 
066000         MOVE TEXT-0403 (SPRAK-IX) TO MOD-TEMFSFEL                        
066100     END-IF                                                               
066200     .                                                                    
066300     EJECT                                                                
066400 N-LAES            SECTION.                                               
066500*                                                                         
066600     PERFORM IMS-GN-INDEX-RO                                              
066700     IF SEGMENT-FINNS                                                     
066800         MOVE +1 TO INDX                                                  
066900*                                                                         
067000         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
067100            SEGMENT-SLUT OR                                               
067200            INDX > MAX-LINE                                               
067300            IF SEGMENT-FINNS                                              
067400               PERFORM FB-FLYTTA-TILL-KEY                                 
067500               PERFORM IMS-GU-RO                                          
067600               PERFORM FC-FLYTTA-TILL-MOD                                 
067700            END-IF                                                        
067800            ADD +1 TO INDX                                                
067900            IF INDX = +2                                                  
068000               PERFORM D-FLYTTA-GAMMAL-KEY                                
068100            END-IF                                                        
068200            PERFORM IMS-GN-INDEX-RO                                       
068300         END-PERFORM                                                      
068400*                                                                         
068500         IF SEGMENT-FINNS                                                 
068600            PERFORM NA-FLYTTA-TILL-SPAR-MOD                               
068700         END-IF                                                           
068800*                                                                         
068900         PERFORM UNTIL INDX > MAX-LINE                                    
069000            MOVE MFS-RENSA-FAELT TO                                       
069100                                    MOD-IDKUNDNR(INDX)                    
069200                                    MOD-IDARTNR(INDX)                     
069300                                    MOD-KVART(INDX)                       
069400                                    MOD-IDORDNR(INDX)                     
069410                                    MOD-IDDC(INDX)                        
069500            ADD +1 TO INDX                                                
069600         END-PERFORM                                                      
069700     ELSE                                                                 
069800         MOVE TEXT-0403 (SPRAK-IX) TO MOD-TEMFSFEL                        
069900     END-IF                                                               
070000     .                                                                    
070100     EJECT                                                                
070200 NA-FLYTTA-TILL-SPAR-MOD SECTION.                                         
070300                                                                          
070400     MOVE SEQD-IDDISTR  TO MOD-IDDISTR-SPAR                               
070500     MOVE SEQD-IDKUNDNR TO MOD-IDKUNDNR-SPAR                              
070600     MOVE SEQD-IDARTNR  TO MOD-IDARTNR-SPAR                               
070700     MOVE SEQD-IDKUNDRF   TO W-IDKUNDRF                                   
070800     MOVE W-IDKUNDRF-1-5 TO MOD-IDORDNR-SPAR                              
070900     MOVE TEXT-0402 (SPRAK-IX) TO MOD-TEMFSINF                            
071000     .                                                                    
071100     EJECT                                                                
071200 S30-SECURIT SECTION.                                                     
071300*                                                                         
071400     MOVE MSG-SIGNON-USERID TO SEC-IDUSER                                 
071500     MOVE '4574'            TO SEC-IDTRANS                                
071600     MOVE IDDISTR-WS        TO SEC-IDKEY                                  
071700*                                                                         
071800     CALL WSECURIT USING       SEC-IDUSER                                 
071900                               SEC-IDTRANS                                
072000                               SEC-IDKEY                                  
072100                               SEC-KDSVAR                                 
072200     .                                                                    
072300     EJECT                                                                
072400* IMS SEKTIONER                                                           
072500     SKIP3                                                                
072600 IMS-GET-MSG SECTION.                                                     
072700*                                                                         
072800     MOVE    '  QC'          TO    GODK-STATUSKODER                       
072900     CALL    CBLTDLI         USING GU MSG-PCB MSG-IO-AREA                 
073000     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
073100     PERFORM IMS-STATUSKONTROLL                                           
073200     .                                                                    
073300     SKIP3                                                                
073400 IMS-INSERT-MSG SECTION.                                                  
073500                                                                          
073600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
073700       MOVE '0' TO MFS-KDHUVOMR                                           
073800     END-IF                                                               
073900                                                                          
074000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
074100     MOVE SPACE TO GODK-STATUSKODER                                       
074200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
074300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074400     PERFORM IMS-STATUSKONTROLL                                           
074500     .                                                                    
074600     EJECT                                                                
074700 IMS-GN-INDEX-RO SECTION.                                                 
074800*                                                                         
074900     STRING 'WLORDT01(WDA5D1KY>=' W-WDA5D1KY-MIN                          
075000                    '&WDA5D1KY<=' W-WDA5D1KY-MAX                          
075100                    '&IDARTNR >=' W-IDARTNR-MIN-X                         
075200                    '&IDARTNR <=' W-IDARTNR-MAX-X                         
075300                    '&IDKUNDRF>=' W-IDKUNDRF-MIN                          
075400                    '&IDKUNDRF<=' W-IDKUNDRF-MAX                          
075500                    '&KDSTARAD =' W-KDSTARAD-MAX ')'                      
075600            DELIMITED BY SIZE INTO SSA1                                   
075700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
075800     CALL CBLTDLI USING GN ORDT-PCB DLI-IO-WLORDT SSA1                    
075900     MOVE ORDT-STATUS-CODE TO STATUS-WS                                   
076000     PERFORM IMS-STATUSKONTROLL                                           
076100     .                                                                    
076200     EJECT                                                                
076300 IMS-GU-RO    SECTION.                                                    
076400*                                                                         
076500     STRING 'WLORDP01(WDA501KY =' W-WDA501KY ')'                          
076600            DELIMITED BY SIZE INTO SSA1                                   
076700     MOVE '  GE' TO GODK-STATUSKODER                                      
076800     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-WLORDP SSA1                    
076900     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
077000     PERFORM IMS-STATUSKONTROLL                                           
077100     .                                                                    
077200     SKIP3                                                                
077300 IMS-STATUSKONTROLL SECTION.                                              
077400*                                                                         
077500     SET STATUS-IX TO 1                                                   
077600     SEARCH GODK-STATUS AT END CALL FELLOG                                
077700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
077800     END-SEARCH                                                           
077900     .                                                                    
