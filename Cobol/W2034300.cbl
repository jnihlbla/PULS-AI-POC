000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2034300.                                                
000400 AUTHOR.         NIHLBLAD JOHAN.                                          
000500 DATE-WRITTEN.   01/11/26.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET SKALL GENOM KDREFTYP OCH IDDC KUNNA                   
001000*        RELEASA EN REFILLORDER FÖR SDC,NDC OCH LDC                       
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W2T343                                              
001500*        MID:         W2I34301                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W2O34301                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W2034300'.            
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300                                                                          
003400 77  WS-KDARBTYP                 PIC X(8)    VALUE 'ESC'.                 
003500 77  WS-IDDC-REC                 PIC X(2)    VALUE SPACE.                 
003600 77  WS-IDDC-SEND                PIC X(2)    VALUE SPACE.                 
003700 77  WS-KDREFTYP                 PIC X       VALUE SPACE.                 
003800     88  WS-KDREFTYP-VALID                   VALUE 'A' 'B' 'C'            
003900                                                   'L' 'R' 'T'.           
004000                                                                          
004100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004200                                                                          
004300                                                                          
004400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004500     88  NYCKLAR-OK                          VALUE 'J'.                   
004600     88  NYCKLAR-FEL                         VALUE 'N'.                   
004700                                                                          
004800 77  CDC-REFILLED-SW             PIC X       VALUE 'N'.                   
004900     88  CDC-REFILLED                        VALUE 'J'.                   
005000                                                                          
005100 77  INPUT-RETT                  PIC X(1)    VALUE 'J'.                   
005200                                                                          
005300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005400     88  EGEN-MID                            VALUE '2343'.                
005500     88  GODK-MID                            VALUE '2341' '2342'          
005600                                                   '2343' '2344'          
005700                                                   '2345' '2346'          
005800                                                   '2347' '2348'          
005900                                                   '2349'.                
006000     88  HELP-MID                            VALUE '0551'.                
006100     EJECT                                                                
006200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006300 01  GENERELLA-SUBPROGRAM.                                                
006400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006800     EJECT                                                                
006900                                                                          
007000*01 -COPY WWDC99                                                          
007100     SKIP3                                                                
007200*01 -COPY WWDCKONS     -PRE  REFILL-                                      
007300     SKIP3                                                                
007400                                                                          
007500 01  ERR-INF-MESSAGES.                                                    
007600     03  INF-JOB-STARTED         PIC X(40)                                
007700             VALUE 'JOB STARTED'.                                         
007800     03  INF-PF11-TO-RELEASE     PIC X(40)                                
007900             VALUE 'PRESS PF11 TO RELEASE'.                               
008000     03  ERR-INVALID-TRANSFER    PIC X(40)                                
008100             VALUE 'NOT A VALID TRANSFER'.                                
008200     03  ERR-WRONG-CORR-DC       PIC X(40)                                
008300             VALUE 'CORRESPONDING DC WRONG'.                              
008400     03  ERR-SEND-DC-REQUIRED    PIC X(40)                                
008500             VALUE 'SENDING DC REQUIRED'.                                 
008600     03  ERR-NOT-POSIBLE-TO-ORDER PIC X(40)                               
008700             VALUE 'THIS COMBINATION IS NOT POSSIBLE'.                    
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009200     SKIP3                                                                
009300*    --- DATA SOM SKICKAS TILL SOP                                        
009400 01  PARM-TESYMBV.                                                        
009500     03  FILLER                  PIC X(9)    VALUE 'ORDERTYP('.           
009600     03  PARM-KDREFTYP           PIC X(1).                                
009700     03  FILLER                  PIC X(1)    VALUE ')'.                   
009800     03  FILLER                  PIC X(3)    VALUE 'DC('.                 
009900     03  PARM-IDDC               PIC X(2).                                
010000     03  FILLER                  PIC X(1)    VALUE ')'.                   
010100     03  FILLER                  PIC X(5)    VALUE 'DIST('.               
010200     03  PARM-IDDISTR            PIC X(4).                                
010300     03  PARM-IDDISTR-NUM REDEFINES PARM-IDDISTR                          
010400                                 PIC 9(4).                                
010500     03  FILLER                  PIC X(1)    VALUE ')'.                   
010600     03  FILLER                  PIC X(5)    VALUE 'KUND('.               
010700     03  PARM-IDKUNDNR           PIC X(6).                                
010800     03  PARM-IDKUNDNR-NUM REDEFINES PARM-IDKUNDNR                        
010900                                 PIC 9(6).                                
011000     03  FILLER                  PIC X(1)    VALUE ')'.                   
011100     03  FILLER                  PIC X(6)    VALUE 'LEVNR('.              
011200     03  PARM-IDLEVNR            PIC X(5).                                
011300     03  FILLER                  PIC X(1)    VALUE ')'.                   
011400     SKIP3                                                                
011500*01 -COPY WMSGINIT                                                        
011600     EJECT                                                                
011700*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
011800*                                                                         
011900 01  SPAR-AREA.                                                           
012000     03  SPAR-IDTRANS           PIC X(4)    VALUE '2343'.                 
012100     EJECT                                                                
012200*    --- AREA FÖR SOP ANROP                                               
012300 01  W-PROG-TO-PROG-SW.                                                   
012400*03 -COPY WMSGSOP                                                         
012500  SKIP3                                                                   
012600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012900     SKIP3                                                                
013000*01  MID -COPY W2I34301                                                   
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013300     SKIP3                                                                
013400*01  -COPY WMSGAREA                                                       
013500     EJECT                                                                
013600     03  MOD REDEFINES MSG-AREA.                                          
013700*      05  -COPY W2O34301                                                 
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014000     SKIP3                                                                
014100*01  -COPY WMFSAREA                                                       
014200     EJECT                                                                
014300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014400*                                                                         
014500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014600     SKIP3                                                                
014700 01  NYCKLAR-TILL-DLI.                                                    
014800     03  W-IDDC-B6-X.                                                     
014900         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
015000     03  W-IDDC-REF-X.                                                    
015100         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
015200     SKIP3                                                                
015300*    --- STATUS-KOD FRÅN IMS                                              
015400 01  STATUS-WS                   PIC XX.                                  
015500     88  SEGMENT-FINNS                       VALUE '  '.                  
015600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015800     SKIP2                                                                
015900 01  GODK-STATUSKODER.                                                    
016000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016100     SKIP3                                                                
016200 01  SSA1                        PIC X(64).                               
016300 01  SSA2                        PIC X(64).                               
016400 01  SSA3                        PIC X(64).                               
016500     SKIP3                                                                
016600*    --- IMS FUNKTIONSKODER                                               
016700*01  -COPY W0003                                                          
016800     EJECT                                                                
016900 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
017000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
017100                                                                          
017200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
017300 01  DB2-WS.                                                              
017400     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
017500         88  RADER-FINNS                     VALUE 000.                   
017600         88  RADER-SAKNAS                    VALUE 100.                   
017700     03  GODK-SQLCODEKODER.                                               
017800         05  GODK-SQLCODE OCCURS 5                                        
017900             INDEXED BY SQLCODE-IX PIC 9(3).                              
018000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
018100     EJECT                                                                
018200*    ---  DLI INPUT-OUTPUT AREA                                           
018300     EJECT                                                                
018400                                                                          
018500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
018600 01   DLI-IO-AREA-B601.                                                   
018700*     03  -COPY WDB601                                                    
018800     EJECT                                                                
018900                                                                          
019000 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
019100 01   DLI-IO-AREA-B616.                                                   
019200*     03  -COPY WDB616                                                    
019300     EJECT                                                                
019400                                                                          
019500 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
019600                                                                          
019700*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
019800     EJECT                                                                
019900     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
020000     EJECT                                                                
020100                                                                          
020200     EJECT                                                                
020300 LINKAGE SECTION.                                                         
020400*01  -COPY W0009   -PRE MSG-                                              
020500*01  -COPY W0009   -PRE ALT-                                              
020600     05  FILLER                  PIC X.                                   
020700     EJECT                                                                
020800*01  -COPY W0008      -PRE WDB6-                                          
020900     05  FILLER                  PIC X.                                   
021000     EJECT                                                                
021100     EJECT                                                                
021200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDB6-PCB.                      
021300 MAIN SECTION.                                                            
021400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDB6-PCB.                      
021500                                                                          
021600     PERFORM IMS-GET-MSG                                                  
021700     IF SEGMENT-FINNS                                                     
021800       PERFORM A-INIT                                                     
021900       IF EGEN-MID                                                        
022000         IF MFS-UPDATE                                                    
022100           PERFORM C-KONTROLL-AV-KOMBINATIONER                            
022200           IF INPUT-RETT = JA                                             
022300             PERFORM D-STARTA-JOB                                         
022400             MOVE INF-JOB-STARTED       TO MOD-TEMFSINF                   
022500*            MOVE PARM-TESYMBV TO MOD-TEMFSINF                            
022600             PERFORM MFS-RENSA-FAELT-UT                                   
022700           ELSE                                                           
022800             PERFORM MFS-ROER-EJ-FAELT-UT                                 
022900             PERFORM MFS-LAES-IN-IGEN                                     
023000           END-IF                                                         
023100         ELSE                                                             
023200           PERFORM MFS-ROER-EJ-FAELT-UT                                   
023300           PERFORM MFS-LAES-IN-IGEN                                       
023400           MOVE INF-PF11-TO-RELEASE     TO MOD-TEMFSINF                   
023500         END-IF                                                           
023600       ELSE                                                               
023700         PERFORM MFS-RENSA-FAELT-UT                                       
023800       END-IF                                                             
023900       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O34301 + 4                      
024000       PERFORM IMS-INSERT-MSG                                             
024100     END-IF                                                               
024200                                                                          
024300     MOVE ZERO TO RETURN-CODE                                             
024400     GOBACK                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 A-INIT SECTION.                                                          
024800                                                                          
024900     IF MSG-DUBBLA-TRANSKODER                                             
025000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I34301                 
025100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
025200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025300     ELSE                                                                 
025400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I34301                  
025500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
025600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025700     END-IF                                                               
025800                                                                          
025900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026200                                                                          
026300     MOVE LOW-VALUE TO MSG-AREA                                           
026400     MOVE 'W2O343N1' TO MFS-IDMOD                                         
026500     MOVE '2343' TO MOD-IDTRANS                                           
026600     MOVE SPACE           TO MOD-TEMFSFEL MOD-TEMFSINF                    
026700                                                                          
026800     IF EGEN-MID OR HELP-MID                                              
026900       CONTINUE                                                           
027000     ELSE                                                                 
027100       MOVE SPACE TO MFS-KDTRTYP                                          
027200       MOVE '7' TO MFS-IDPFK                                              
027300     END-IF                                                               
027400     .                                                                    
027500     EJECT                                                                
027600*    FÖR KONTROLL AV DC OCH ORDERTYP.                                     
027700 C-KONTROLL-AV-KOMBINATIONER SECTION.                                     
027800                                                                          
027900     MOVE NEJ                    TO INPUT-RETT                            
028000                                    CDC-REFILLED-SW                       
028100                                                                          
028200     MOVE MID-IDDC-REC           TO W-IDDC-B6                             
028300                                    WS-IDDC                               
028400     MOVE MID-KDREFTYP           TO WS-KDREFTYP                           
028500                                                                          
028600     IF MID-IDDC-SEND    = REFILL-WC-CDC-SE                               
028700        MOVE JA                  TO CDC-REFILLED-SW                       
028800     END-IF                                                               
028900                                                                          
029000     PERFORM IMS-GU-WDB601                                                
029100     IF  (SEGMENT-FINNS                                                   
029200     AND (DCS-SDC                                                         
029300     OR   DCS-NDC                                                         
029400     OR   DCS-CDC)                                                        
029500     AND  WS-KDREFTYP-VALID)                                              
030100**     IF  (NDC                                                           
030200**     AND NOT CDC-REFILLED)                                              
030300**     AND  MID-KDREFTYP NOT = 'L'                                        
030320       IF  ((NDC                                                          
030330       AND NOT CDC-REFILLED)                                              
030340       OR CDC)                                                            
030350       AND  MID-KDREFTYP NOT = 'L'                                        
031200*********EXTENDED REFILL                                                  
031300         IF MID-IDDC-SEND = ALL '+'                                       
031400           MOVE ERR-SEND-DC-REQUIRED                                      
031500                                   TO MOD-TEMFSFEL                        
031600           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-SEND-ATTR                  
031700         ELSE                                                             
031800           PERFORM CA-CHECK-FOR-REFILL-DC                                 
032000         END-IF                                                           
032100       ELSE                                                               
032510         IF MID-IDDC-SEND = '11' AND DCS-NDC                              
032600           IF MID-KDREFTYP = 'L' OR 'T'                                   
032700             MOVE ERR-NOT-POSIBLE-TO-ORDER                                
032800                                   TO MOD-TEMFSFEL                        
032900             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDREFTYP-ATTR                 
033000           END-IF                                                         
033100           IF MID-KDREFTYP = 'A' OR 'B' OR 'C'                            
033200             MOVE DCS-IDDISTR-REFILL  TO PARM-IDDISTR-NUM                 
033300             MOVE SPACE               TO PARM-IDKUNDNR                    
033400             MOVE '1441'              TO PARM-IDLEVNR                     
033500             MOVE JA                  TO INPUT-RETT                       
033600           ELSE                                                           
033700             IF MID-KDREFTYP = 'R'                                        
033800               MOVE DCS-IDDISTR-RETUR TO PARM-IDDISTR-NUM                 
033900               MOVE DCS-IDKUNDNR-RETUR TO PARM-IDKUNDNR-NUM               
034000               MOVE SPACE             TO PARM-IDLEVNR                     
034100               MOVE JA                TO INPUT-RETT                       
034200             END-IF                                                       
034300           END-IF                                                         
034400           MOVE MID-KDREFTYP          TO PARM-KDREFTYP                    
034500           MOVE SPACE                 TO PARM-IDDC                        
034600         ELSE                                                             
034700           IF MID-IDDC-SEND = '11'                                        
034800             MOVE JA               TO INPUT-RETT                          
034900             MOVE MID-IDDC-REC     TO PARM-IDDC                           
035000             MOVE SPACE            TO PARM-IDDISTR                        
035100             MOVE SPACE            TO PARM-IDKUNDNR                       
035200             MOVE SPACE            TO PARM-IDLEVNR                        
035300             MOVE MID-KDREFTYP     TO PARM-KDREFTYP                       
035400           ELSE                                                           
035500             IF MID-KDREFTYP = 'L'                                        
035600                IF DCS-FLLPO = NEJ                                        
035700                   MOVE MFS-ALFA-FAELT-FEL                                
035800                                   TO MOD-IDDC-REC-ATTR                   
035900                                      MOD-KDREFTYP-ATTR                   
036000                   MOVE ERR-NOT-POSIBLE-TO-ORDER                          
036100                                   TO MOD-TEMFSFEL                        
036200                ELSE                                                      
036300                  IF DCS-FLLPO = JA                                       
036400                     MOVE JA             TO INPUT-RETT                    
036500                     MOVE MID-IDDC-REC   TO PARM-IDDC                     
036600                     MOVE SPACE          TO PARM-IDDISTR                  
036700                     MOVE SPACE          TO PARM-IDKUNDNR                 
036800                     MOVE SPACE          TO PARM-IDLEVNR                  
036900                     MOVE MID-KDREFTYP   TO PARM-KDREFTYP                 
037000                  END-IF                                                  
037100                END-IF                                                    
037200             ELSE                                                         
037300               MOVE ERR-SEND-DC-REQUIRED                                  
037400                                       TO MOD-TEMFSFEL                    
037500             END-IF                                                       
037600           END-IF                                                         
037700         END-IF                                                           
037800       END-IF                                                             
037900                                                                          
038000     ELSE                                                                 
038100                                                                          
038200       IF DCS-SDC    OR                                                   
038300          DCS-NDC                                                         
038400         CONTINUE                                                         
038500       ELSE                                                               
038600         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-REC-ATTR                     
038700       END-IF                                                             
038800                                                                          
038900       IF WS-KDREFTYP-VALID                                               
039000         CONTINUE                                                         
039100       ELSE                                                               
039200         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDREFTYP-ATTR                     
039300       END-IF                                                             
039400                                                                          
039500       MOVE ERR-NOT-POSIBLE-TO-ORDER                                      
039600                                   TO MOD-TEMFSFEL                        
039700     END-IF                                                               
039800                                                                          
039900     .                                                                    
040000     EJECT                                                                
040100                                                                          
040200 CA-CHECK-FOR-REFILL-DC SECTION.                                          
040300                                                                          
040500     IF MID-KDREFTYP = 'A' OR 'B' OR 'C' OR 'R'                           
040600       MOVE MID-IDDC-SEND             TO W-IDDC-REF                       
040700       PERFORM IMS-GU-WDB616                                              
040800       IF SEGMENT-FINNS                                                   
041700           MOVE JA                    TO INPUT-RETT                       
041800           MOVE SPACE                 TO PARM-IDDC                        
041900           MOVE SPACE                 TO PARM-IDKUNDNR                    
042000           MOVE SPACE                 TO PARM-IDLEVNR                     
042100           MOVE MID-KDREFTYP          TO PARM-KDREFTYP                    
042200           IF MID-KDREFTYP = 'R'                                          
042300             MOVE REF-IDDISTR-RETUR  TO PARM-IDDISTR-NUM                  
042400             MOVE REF-IDKUNDNR-RETUR TO PARM-IDKUNDNR-NUM                 
042500           ELSE                                                           
042600             MOVE MID-IDDC-SEND             TO W-IDDC-B6                  
042700             PERFORM IMS-GU-WDB601                                        
042800             IF SEGMENT-FINNS                                             
042900               MOVE DCS-IDLEVNR-DC    TO PARM-IDLEVNR                     
043000             END-IF                                                       
043100             MOVE REF-IDDISTR-REFILL  TO PARM-IDDISTR-NUM                 
043200           END-IF                                                         
043400       ELSE                                                               
043500         MOVE ERR-NOT-POSIBLE-TO-ORDER                                    
043600                                      TO MOD-TEMFSFEL                     
043700       END-IF                                                             
043800     ELSE                                                                 
043900*    MID-KDREFTYP = T                                                     
044000       MOVE MID-IDDC-REC              TO WS-IDDC-REC                      
044100       MOVE MID-IDDC-SEND             TO WS-IDDC-SEND                     
044200       PERFORM DB2-SELECT-TP4TRAN                                         
044300       IF RADER-FINNS                                                     
044400         MOVE JA                      TO INPUT-RETT                       
044500         MOVE SPACE                   TO PARM-IDDC                        
044600         MOVE TP4TRAN-IDDISTR         TO PARM-IDDISTR-NUM                 
044700         MOVE TP4TRAN-IDKUNDNR        TO PARM-IDKUNDNR-NUM                
044800         MOVE MID-KDREFTYP            TO PARM-KDREFTYP                    
044900         MOVE SPACE                   TO PARM-IDLEVNR                     
045000       ELSE                                                               
045100         MOVE ERR-INVALID-TRANSFER    TO MOD-TEMFSFEL                     
045200       END-IF                                                             
045300     END-IF                                                               
045400                                                                          
045500     .                                                                    
045600     EJECT                                                                
045700                                                                          
045800*    FLYTTAR PARAMETRAR TILL SOPRUTIN OCH STARTAR UPP                     
045900 D-STARTA-JOB SECTION.                                                    
046000     MOVE '2343'       TO MSGSOP-IDTRANS                                  
046100     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
046200     MOVE 'W271B1    ' TO MSGSOP-IDPROCESS                                
046300     MOVE 'O'          TO MSGSOP-KDSOPFUNK                                
046400     MOVE PARM-TESYMBV TO MSGSOP-TESYMBV                                  
046500     PERFORM IMS-INSERT-ALT-MSG                                           
046600                                                                          
046700     SKIP3                                                                
046800     .                                                                    
046900 MFS-RENSA-FAELT-UT SECTION.                                              
047000                                                                          
047100*    --- ALLA UTDATA-FÄLT                                                 
047200     MOVE MFS-RENSA-FAELT     TO MOD-IDDC-REC                             
047300                                 MOD-IDDC-SEND                            
047400                                 MOD-KDREFTYP                             
047500     .                                                                    
047600     SKIP3                                                                
047700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
047800                                                                          
047900*    --- ALLA UTDATA-FÄLT                                                 
048000     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDDC-REC                             
048100                                 MOD-IDDC-SEND                            
048200                                 MOD-KDREFTYP                             
048300     .                                                                    
048400     SKIP3                                                                
048500 MFS-FORM-ATTR SECTION.                                                   
048600                                                                          
048700*    --- ALLA INDATA-FÄLT                                                 
048800     MOVE MFS-FORMATETS-ATTR  TO MOD-IDDC-REC-ATTR                        
048900                                 MOD-IDDC-SEND-ATTR                       
049000                                 MOD-KDREFTYP-ATTR                        
049100     .                                                                    
049200     SKIP2                                                                
049300 MFS-LAES-IN-IGEN SECTION.                                                
049400                                                                          
049500*    --- ALLA INDATA-FÄLT                                                 
049600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-REC-ATTR                      
049700                                   MOD-IDDC-SEND-ATTR                     
049800                                   MOD-KDREFTYP-ATTR                      
049900     .                                                                    
050000     EJECT                                                                
050100* --- IMS SEKTIONER ---                                                   
050200     SKIP3                                                                
050300 IMS-GET-MSG SECTION.                                                     
050400                                                                          
050500     MOVE '  QC' TO GODK-STATUSKODER                                      
050600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
050700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050800     PERFORM IMS-STATUSKONTROLL                                           
050900     .                                                                    
051000     SKIP3                                                                
051100 IMS-INSERT-ALT-MSG SECTION.                                              
051200                                                                          
051300     MOVE SPACE TO GODK-STATUSKODER                                       
051400     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
051500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
051600     PERFORM IMS-STATUSKONTROLL                                           
051700     .                                                                    
051800     EJECT                                                                
051900 IMS-INSERT-MSG SECTION.                                                  
052000                                                                          
052100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
052200     MOVE SPACE TO GODK-STATUSKODER                                       
052300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
052400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
052500     PERFORM IMS-STATUSKONTROLL                                           
052600     .                                                                    
052700     EJECT                                                                
052800                                                                          
052900 IMS-GU-WDB601    SECTION.                                                
053000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
053100          DELIMITED BY SIZE INTO SSA1                                     
053200     MOVE '  GE' TO GODK-STATUSKODER                                      
053300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
053400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
053500     PERFORM IMS-STATUSKONTROLL                                           
053600     .                                                                    
053700     EJECT                                                                
053800 IMS-GU-WDB616    SECTION.                                                
053900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
054000          DELIMITED BY SIZE INTO SSA1                                     
054100     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
054200          DELIMITED BY SIZE INTO SSA2                                     
054300     MOVE '  GE' TO GODK-STATUSKODER                                      
054400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
054500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
054600     PERFORM IMS-STATUSKONTROLL                                           
054700     .                                                                    
054800     EJECT                                                                
054900 IMS-STATUSKONTROLL SECTION.                                              
055000                                                                          
055100     SET STATUS-IX TO 1                                                   
055200     SEARCH GODK-STATUS                                                   
055300       AT END                                                             
055400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055500         DELIMITED BY SIZE INTO FELTEXT                                   
055600         CALL FELLOG                                                      
055700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055800         CONTINUE                                                         
055900     END-SEARCH                                                           
056000     .                                                                    
056100     EJECT                                                                
056200 DB2-SELECT-TP4TRAN SECTION.                                              
056300                                                                          
056400     MOVE 000100 TO GODK-SQLCODEKODER                                     
056500                                                                          
056600     EXEC SQL                                                             
056700           SELECT  KDARBTYP                                               
056800                  ,IDDC_SEND                                              
056900                  ,IDDC_REC                                               
057000                  ,IDDISTR                                                
057100                  ,IDKUNDNR                                               
057200                                                                          
057300           INTO   :TP4TRAN-KDARBTYP                                       
057400                 ,:TP4TRAN-IDDC-SEND                                      
057500                 ,:TP4TRAN-IDDC-REC                                       
057600                 ,:TP4TRAN-IDDISTR                                        
057700                 ,:TP4TRAN-IDKUNDNR                                       
057800                                                                          
057900           FROM    TP4TRAN                                                
058000                                                                          
058100           WHERE KDARBTYP  = :WS-KDARBTYP                                 
058200           AND   IDDC_SEND = :WS-IDDC-SEND                                
058300           AND   IDDC_REC  = :WS-IDDC-REC                                 
058400     END-EXEC                                                             
058500                                                                          
058600     MOVE SQLCODE TO SQLCODE-WS                                           
058700     PERFORM DB2-STATUSKONTROLL                                           
058800     .                                                                    
058900     EJECT                                                                
059000 DB2-STATUSKONTROLL  SECTION.                                             
059100                                                                          
059200     SET SQLCODE-IX TO 1                                                  
059300     SEARCH GODK-SQLCODE                                                  
059400       AT END                                                             
059500         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS                
059600           DELIMITED BY SIZE INTO FELTEXT                                 
059700         CALL ABEND USING RKOD-ABEND-DB2                                  
059800       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
059900         CONTINUE                                                         
060000     END-SEARCH                                                           
060100     .                                                                    
