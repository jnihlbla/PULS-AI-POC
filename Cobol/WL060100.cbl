000100*COMPOPT DB2BIND=YES            -- REMOVE IF PGM USES DB2 DIRECTLY        
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WL060100.                                                
000400 AUTHOR.         ASPFJÄLL MARKUS.                                         
000500 DATE-WRITTEN.   08/04/10.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAMN:       FREELOCATIONUPDATE                                       
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        FREE LOCTION UPDATE                                              
001200*        UPPDATERA OLIKA LAGERPLATSER FÖR LDC                             
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDK7                                       
001500*        PROGRAMMET UPPDATERAR WDJ8                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: WL0601T                                             
001900*        REQUEST:     WL0601I1                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        RESPONSE:    WL0601O1                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'WL060100'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004100 77  KDRC-DISPLAY                PIC Z(5).                                
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  WS-DATUM                    PIC 9(8).                                
004600 77  INDX                        PIC S9(3)  VALUE ZERO COMP-3.            
004700 77  TAB-INDX                    PIC  9(3)  VALUE ZERO.                   
004800 77  MAX-INDX                    PIC S9(3)  VALUE +500 COMP-3.            
004900 01  WS-ADPLATS.                                                          
005000     03 WS-BAY                    PIC 9(2) VALUE ZERO.                    
005100     03 WS-ADLEVEL                PIC 9(2) VALUE ZERO.                    
005200     03 WS-ADSEQ                  PIC 9(1) VALUE ZERO.                    
005300                                                                          
005400                                                                          
005500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005600     88  NYCKLAR-OK                          VALUE 'J'.                   
005700     88  NYCKLAR-FEL                         VALUE 'N'.                   
005800                                                                          
005900 77  INDATA-SW                   PIC X       VALUE 'N'.                   
006000     88  INDATA-OK                           VALUE 'J'.                   
006100     88  INDATA-FEL                          VALUE 'N'.                   
006200                                                                          
006300 77  POST-FINNS-SW               PIC X       VALUE 'N'.                   
006400     88  POST-FINNS                          VALUE 'J'.                   
006500     88  POST-SAKNAS                         VALUE 'N'.                   
006600                                                                          
006700 77  TABELL-1-SW                 PIC X       VALUE 'N'.                   
006800     88  TABELL-1                            VALUE 'J'.                   
006900                                                                          
007000 77  TABELL-2-SW                 PIC X       VALUE 'N'.                   
007100     88  TABELL-2                            VALUE 'J'.                   
007200                                                                          
007300                                                                          
007400     EJECT                                                                
007500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007600 01  GENERELLA-SUBPROGRAM.                                                
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     SKIP3                                                                
008300*    --- PARAMETRAR TILL ABEND                                            
008400                                                                          
008500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008800     SKIP3                                                                
008900 01  MESSAGE-CODES.                                                       
009000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
009100     EJECT                                                                
009200*                                                                         
009300 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009400     SKIP3                                                                
009500*01  -COPY WZ01SUB                                                        
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009800     SKIP3                                                                
009900 01  REQU-AREA.                                                           
010000*    03  -COPY WZ01REQU                                                   
010100*    03  -COPY WL0601I1                                                   
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010400     SKIP3                                                                
010500 01  RESP-AREA.                                                           
010600*    03  -COPY WZ01RESP                                                   
010700*    03  -COPY WL0601O1                                                   
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
011000     SKIP3                                                                
011100*01  -COPY WZ01SEND                                                       
011200     EJECT                                                                
011300 01  NYCKLAR-TILL-DLI.                                                    
011400     03  W-IDARTNR-X.                                                     
011500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011600     03  W-ADLAGOMR-X.                                                    
011700         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO.                  
011800     03  W-ADGANG-X.                                                      
011900         05  W-ADGANG            PIC 9(2)    VALUE ZERO.                  
012000     03  W-BAY-X.                                                         
012100         05  W-BAY               PIC 9(2)    VALUE ZERO.                  
012200     03  W-IDDC-X.                                                        
012300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012400                                                                          
012500     03  W-WDJ8KEY-X.                                                     
012600         05  W-LOC-IDDC          PIC X(2)   VALUE SPACE.                  
012700         05  W-LOC-ADLAGOMR      PIC 9(2)   VALUE ZERO.                   
012800         05  W-LOC-ADGANG        PIC 9(2)   VALUE ZERO.                   
012900         05  W-LOC-ADPLATS.                                               
013000             07 W-LOC-BAY        PIC 9(2) VALUE ZERO.                     
013100             07 W-LOC-ADLEVEL    PIC 9(2) VALUE ZERO.                     
013200             07 W-LOC-ADSEQ      PIC 9(1) VALUE ZERO.                     
013300                                                                          
013400     03  W-WDJ8KEY-MIN-X.                                                 
013500         05  W-LOC-IDDC-MIN       PIC X(2)   VALUE SPACE.                 
013600         05  W-LOC-ADLAGOMR-MIN   PIC 9(2)   VALUE ZERO.                  
013700         05  W-LOC-ADGANG-MIN     PIC 9(2)   VALUE ZERO.                  
013800         05  W-LOC-ADPLATS-MIN.                                           
013900             07 W-LOC-BAY-MIN     PIC 9(2) VALUE ZERO.                    
014000             07 W-LOC-ADLEVEL-MIN PIC 9(2) VALUE ZERO.                    
014100             07 W-LOC-ADSEQ-MIN   PIC 9(1) VALUE ZERO.                    
014200     03  W-WDJ8KEY-MAX-X.                                                 
014300         05  W-LOC-IDDC-MAX       PIC X(2)   VALUE SPACE.                 
014400         05  W-LOC-ADLAGOMR-MAX   PIC 9(2)   VALUE 99.                    
014500         05  W-LOC-ADGANG-MAX     PIC 9(2)   VALUE 99.                    
014600         05  W-LOC-ADPLATS-MAX.                                           
014700             07 W-LOC-BAY-MAX     PIC 9(2) VALUE 99.                      
014800             07 W-LOC-ADLEVEL-MAX PIC 9(2) VALUE 99.                      
014900             07 W-LOC-ADSEQ-MAX   PIC 9(1) VALUE 9.                       
015000                                                                          
015100     EJECT                                                                
015200                                                                          
015300 01    FILLER          PIC X(16)   VALUE '     IMS-WS     '.              
015400 01    STATUS-WS       PIC XX.                                            
015500         88  SEGMENT-FINNS       VALUE '  '.                              
015600         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
015700         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
015800         88  END-OF-DB           VALUE 'GB'.                              
015900                                                                          
016000 01    GODK-STATUSKODER.                                                  
016100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200                                                                          
016300 01      SSA1            PIC X(128) VALUE SPACE.                          
016400 01      SSA2            PIC X(128) VALUE SPACE.                          
016500 01      SSA3            PIC X(128) VALUE SPACE.                          
016600                                                                          
016700*                            IMS FUNKTIONSKODER                           
016800*01      -COPY W0003                                                      
016900                                                                          
017000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ801'.                      
017100 01  DLI-IO-WDJ801.                                                       
017200*    03  -COPY WDJ801                                                     
017300     EJECT                                                                
017400 LINKAGE SECTION.                                                         
017500*01  -COPY W0009  -PRE MSG-     PIC X.                                    
017600                                                                          
017700*01  -COPY W0008  -PRE WDJ8-                                              
017800     05  FILLER                  PIC X.                                   
017900     EJECT                                                                
018000 PROCEDURE DIVISION  USING MSG-PCB WDJ8-PCB.                              
018100 MAIN SECTION.                                                            
018200     ENTRY 'DLITCBL' USING MSG-PCB WDJ8-PCB.                              
018300                                                                          
018400     PERFORM S01-HAEMTA-ANROPSDATA                                        
018500     IF SUB-KDRC = 0                                                      
018600       PERFORM A-INIT                                                     
018700       PERFORM B-KOLLA-NYCKLAR                                            
018800       IF NYCKLAR-OK                                                      
018900         IF REQU-KDPGMACT = 'S'                                           
019000           PERFORM F-LAES-VISA-INFO                                       
019100         ELSE                                                             
019200           IF REQU-KDPGMACT = 'E'                                         
019300             PERFORM G-KOLLA-INDATA                                       
019400             IF INDATA-OK AND POST-FINNS                                  
019500               IF TABELL-1                                                
019600                 PERFORM H-UPPDATERA-TABELL-1                             
019700               END-IF                                                     
019800               IF TABELL-2                                                
019900                 PERFORM I-UPPDATERA-TABELL-2                             
020000               END-IF                                                     
020100             END-IF                                                       
020200             PERFORM F-LAES-VISA-INFO                                     
020300           END-IF                                                         
020400         END-IF                                                           
020500       END-IF                                                             
020600       PERFORM S02-RETURNERA-SVAR                                         
020700*      PERFORM S04-SKICKA-OPEN                                            
020800*      PERFORM S04-SKICKA-MEDDELANDE                                      
020900*      PERFORM S04-SKICKA-CLOSE                                           
021000     END-IF                                                               
021100                                                                          
021200                                                                          
021300     MOVE ZERO TO RETURN-CODE                                             
021400     GOBACK                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 A-INIT SECTION.                                                          
021800*    IF REQU-KDPGMACT = 'S'                                               
021900       MOVE ALL '+'   TO RESP-AREA                                        
022000*    ELSE                                                                 
022100*      IF REQU-KDPGMACT = 'E'                                             
022200*        MOVE ALL SPACE TO RESP-WL0601O1                                  
022300*      END-IF                                                             
022400*    END-IF                                                               
022500                                                                          
022600     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
022700                       RESP-IDMSG-INFO                                    
022800                       RESP-IDELMT-ERROR                                  
022900     MOVE 001       TO RESP-IDMSGVER                                      
023000     MOVE ZERO      TO RESP-KVRADER                                       
023100                                                                          
023200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
023300     MOVE REQU-IDDC-KEY TO W-IDDC                                         
023400                           RESP-IDDC-KEY                                  
023500     MOVE 001           TO RESP-IDMSGVER                                  
023600                                                                          
023700                                                                          
023800     .                                                                    
023900     EJECT                                                                
024000 B-KOLLA-NYCKLAR SECTION.                                                 
024100                                                                          
024200     MOVE JA TO NYCKLAR-SW                                                
024300     IF REQU-IDDC-KEY     NOT = ALL '+'                                   
024400       MOVE REQU-IDDC-KEY TO RESP-IDDC-KEY                                
024500                             W-IDDC                                       
024600                             W-LOC-IDDC                                   
024700                             W-LOC-IDDC-MIN                               
024800                             W-LOC-IDDC-MAX                               
024900     ELSE                                                                 
025000       MOVE NEJ TO INDATA-SW                                              
025100       MOVE 'IDDC'         TO RESP-IDELMT-ERROR                           
025200     END-IF                                                               
025300     IF REQU-ADLAGOMR-KEY NOT = ALL '+'                                   
025400       IF REQU-ADLAGOMR-KEY NUMERIC                                       
025500         MOVE REQU-ADLAGOMR-KEY  TO W-ADLAGOMR                            
025600                                    RESP-ADLAGOMR-KEY                     
025700                                    W-LOC-ADLAGOMR                        
025800                                    W-LOC-ADLAGOMR-MIN                    
025900                                    W-LOC-ADLAGOMR-MAX                    
026000       ELSE                                                               
026100         MOVE NEJ                TO NYCKLAR-SW                            
026200       END-IF                                                             
026300     ELSE                                                                 
026400       MOVE NEJ                  TO NYCKLAR-SW                            
026500       MOVE 'ADLAGOMR'           TO RESP-IDELMT-ERROR                     
026600     END-IF                                                               
026700                                                                          
026800     IF REQU-ADGANG-KEY    NOT = ALL '+'                                  
026900       IF REQU-ADGANG-KEY   NUMERIC                                       
027000         MOVE REQU-ADGANG-KEY    TO W-ADGANG                              
027100                                    RESP-ADGANG-KEY                       
027200                                    W-LOC-ADGANG                          
027300                                    W-LOC-ADGANG-MIN                      
027400                                    W-LOC-ADGANG-MAX                      
027500       ELSE                                                               
027600         MOVE NEJ                TO NYCKLAR-SW                            
027700         MOVE 'ADGANG'           TO RESP-IDELMT-ERROR                     
027800       END-IF                                                             
027900     ELSE                                                                 
028000*      MOVE ZERO                 TO RESP-ADGANG-KEY                       
028100       MOVE ZERO                 TO W-ADGANG                              
028200                                                                          
028300     END-IF                                                               
028400                                                                          
028500     IF REQU-BAY-KEY        NOT = ALL '+'                                 
028600       IF REQU-BAY-KEY      NUMERIC                                       
028700         MOVE REQU-BAY-KEY       TO W-BAY                                 
028800                                    RESP-BAY-KEY                          
028900                                    W-LOC-BAY                             
029000                                    W-LOC-BAY-MIN                         
029100                                    W-LOC-BAY-MAX                         
029200       ELSE                                                               
029300         MOVE NEJ                TO NYCKLAR-SW                            
029400         MOVE 'BAY'              TO RESP-IDELMT-ERROR                     
029500       END-IF                                                             
029600     ELSE                                                                 
029700*      MOVE ZERO                 TO RESP-BAY-KEY                          
029800       MOVE ZERO                 TO W-BAY                                 
029900                                                                          
030000     END-IF                                                               
030100                                                                          
030200                                                                          
030300     IF NYCKLAR-FEL                                                       
030400       IF REQU-KDPGMACT = 'E'                                             
030500         MOVE JA            TO NYCKLAR-SW                                 
030600         MOVE SPACE         TO RESP-IDMSG-ERROR                           
030700                               RESP-IDMSG-INFO                            
030800                               RESP-IDELMT-ERROR                          
030900       ELSE                                                               
031000         MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                           
031100       END-IF                                                             
031200     END-IF                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 F-LAES-VISA-INFO SECTION.                                                
031600                                                                          
031700     PERFORM FA-LAES-GRUNDDATA                                            
031800                                                                          
031900     IF SEGMENT-SAKNAS                                                    
032000        MOVE '027' TO RESP-IDMSG-ERROR                                    
032100     ELSE                                                                 
032200                                                                          
032300        MOVE +0 TO INDX                                                   
032400        PERFORM UNTIL SEGMENT-SAKNAS OR INDX = MAX-INDX                   
032500          ADD +1 TO INDX                                                  
032600          MOVE LOC-ADLAGOMR        TO RESP-ADLAGOMR-UT (INDX)             
032700          MOVE LOC-ADGANG          TO RESP-ADGANG-UT   (INDX)             
032800          MOVE LOC-ADPLATS         TO WS-ADPLATS                          
032900          MOVE WS-BAY              TO RESP-BAY-UT      (INDX)             
033000          MOVE LOC-KVPLATS         TO RESP-KVPLATS-UT  (INDX)             
033100*                                                                         
033200          IF REQU-ADGANG-KEY = ALL '+' OR                                 
033300             REQU-BAY-KEY = ALL '+'                                       
033400            PERFORM IMS-GN-WDJ801-MIN-MAX                                 
033500          ELSE                                                            
033600            PERFORM IMS-GN-WDJ801                                         
033700          END-IF                                                          
033800                                                                          
033900        END-PERFORM                                                       
034000        MOVE INDX TO RESP-KVRADER                                         
034100     END-IF                                                               
034200     IF SEGMENT-FINNS                                                     
034300       MOVE '011'            TO RESP-IDMSG-INFO                           
034400     END-IF                                                               
034500*    IF SEGMENT-SAKNAS AND INDX > +0                                      
034600*      IF INDX > +0                                                       
034700*        CALL ABEND                                                       
034800*        MOVE '012'          TO RESP-IDMSG-INFO                           
034900*      END-IF                                                             
035000*    END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 FA-LAES-GRUNDDATA SECTION.                                               
035400     IF REQU-ADGANG-KEY = ALL '+' OR                                      
035500        REQU-BAY-KEY = ALL '+'                                            
035600       PERFORM IMS-GET-WDJ801-MIN-MAX                                     
035700     ELSE                                                                 
035800       PERFORM IMS-GET-WDJ801                                             
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 G-KOLLA-INDATA SECTION.                                                  
036300*    KOLLA UPPDATERINGS TABELL * 10                                       
036400     MOVE  1 TO TAB-INDX                                                  
036500     MOVE NEJ TO POST-FINNS-SW                                            
036600     MOVE NEJ TO TABELL-1-SW                                              
036700     MOVE JA  TO INDATA-SW                                                
036800     PERFORM UNTIL TAB-INDX > 10                                          
036900      IF REQU-ADLAGOMR-UPD (TAB-INDX) NOT = ALL '+'                       
037000        IF REQU-ADLAGOMR-UPD (TAB-INDX) NUMERIC                           
037100          MOVE JA            TO POST-FINNS-SW                             
037200        ELSE                                                              
037300*         MOVE REQU-ADLAGOMR-UPD (TAB-INDX) TO                            
037400*              RESP-ADLAGOMR-UPD (TAB-INDX)                               
037500          MOVE NEJ           TO INDATA-SW                                 
037600        END-IF                                                            
037700      END-IF                                                              
037800                                                                          
037900      IF REQU-ADGANG-UPD (TAB-INDX) NOT = ALL '+'                         
038000        IF REQU-ADGANG-UPD   (TAB-INDX) NUMERIC                           
038100          MOVE JA            TO POST-FINNS-SW                             
038200        ELSE                                                              
038300*         MOVE REQU-ADGANG-UPD (TAB-INDX) TO                              
038400*              RESP-ADGANG-UPD (TAB-INDX)                                 
038500          MOVE NEJ           TO INDATA-SW                                 
038600        END-IF                                                            
038700      END-IF                                                              
038800                                                                          
038900      IF REQU-BAY-UPD (TAB-INDX) NOT = ALL '+'                            
039000        IF REQU-BAY-UPD (TAB-INDX) NUMERIC                                
039100          MOVE JA            TO POST-FINNS-SW                             
039200        ELSE                                                              
039300*         MOVE REQU-BAY-UPD (TAB-INDX) TO                                 
039400*              RESP-BAY-UPD (TAB-INDX)                                    
039500          MOVE NEJ           TO INDATA-SW                                 
039600        END-IF                                                            
039700      END-IF                                                              
039800                                                                          
039900      IF REQU-KVPLATS-UPD (TAB-INDX) NOT = ALL '+'                        
040000        IF REQU-KVPLATS-UPD (TAB-INDX) NUMERIC                            
040100          MOVE JA            TO POST-FINNS-SW                             
040200        ELSE                                                              
040300*         MOVE REQU-KVPLATS-UPD (TAB-INDX) TO                             
040400*              RESP-KVPLATS-UPD (TAB-INDX)                                
040500          MOVE NEJ           TO INDATA-SW                                 
040600        END-IF                                                            
040700      END-IF                                                              
040800* ALLA FÄLT PÅ RADEN SKA VARA IFYLLDA **                                  
040900      IF REQU-ADLAGOMR-UPD (TAB-INDX) NOT = ALL '+' OR                    
041000         REQU-ADGANG-UPD   (TAB-INDX) NOT = ALL '+' OR                    
041100         REQU-BAY-UPD      (TAB-INDX) NOT = ALL '+' OR                    
041200         REQU-KVPLATS-UPD  (TAB-INDX) NOT = ALL '+'                       
041300         MOVE JA TO TABELL-1-SW                                           
041400        IF REQU-ADLAGOMR-UPD (TAB-INDX) = ALL '+'                         
041500          MOVE NEJ           TO INDATA-SW                                 
041600          MOVE 'ADLAGOMR'         TO RESP-IDELMT-ERROR                    
041700        END-IF                                                            
041800        IF REQU-ADGANG-UPD   (TAB-INDX) = ALL '+'                         
041900          MOVE NEJ           TO INDATA-SW                                 
042000          MOVE 'ADGANG'           TO RESP-IDELMT-ERROR                    
042100        END-IF                                                            
042200        IF REQU-BAY-UPD      (TAB-INDX) = ALL '+'                         
042300          MOVE NEJ           TO INDATA-SW                                 
042400          MOVE 'BAY'              TO RESP-IDELMT-ERROR                    
042500        END-IF                                                            
042600        IF REQU-KVPLATS-UPD  (TAB-INDX) = ALL '+'                         
042700          MOVE NEJ           TO INDATA-SW                                 
042800          MOVE 'KVPLATS'          TO RESP-IDELMT-ERROR                    
042900        END-IF                                                            
043000        IF INDATA-OK                                                      
043100**      KOLLA OM DATA REDAN FINNS PÅ BASEN WDJ8                           
043200          MOVE REQU-ADLAGOMR-UPD (TAB-INDX) TO W-LOC-ADLAGOMR             
043300          MOVE REQU-ADGANG-UPD   (TAB-INDX) TO W-LOC-ADGANG               
043400          MOVE REQU-BAY-UPD      (TAB-INDX) TO WS-BAY                     
043500          MOVE WS-ADPLATS                   TO W-LOC-ADPLATS              
043600          PERFORM IMS-GET-WDJ801                                          
043700          IF SEGMENT-FINNS                                                
043800            MOVE NEJ                        TO INDATA-SW                  
043900            MOVE '030'                      TO RESP-IDMSG-INFO            
044000          END-IF                                                          
044100        ELSE                                                              
044200          MOVE '023'                        TO RESP-IDMSG-ERROR           
044300        END-IF                                                            
044400      END-IF                                                              
044500      ADD  1 TO TAB-INDX                                                  
044600     END-PERFORM                                                          
044700                                                                          
044800     IF INDATA-FEL                                                        
044900       MOVE 1 TO TAB-INDX                                                 
045000       PERFORM UNTIL TAB-INDX > 10                                        
045100         IF REQU-ADLAGOMR-UPD (TAB-INDX) NOT = ALL '+'                    
045200           MOVE REQU-ADLAGOMR-UPD (TAB-INDX)TO                            
045300                RESP-ADLAGOMR-UPD (TAB-INDX)                              
045400         END-IF                                                           
045500         IF REQU-ADGANG-UPD   (TAB-INDX) NOT = ALL '+'                    
045600           MOVE REQU-ADGANG-UPD   (TAB-INDX)TO                            
045700                RESP-ADGANG-UPD   (TAB-INDX)                              
045800         END-IF                                                           
045900         IF REQU-BAY-UPD      (TAB-INDX) NOT = ALL '+'                    
046000           MOVE REQU-BAY-UPD      (TAB-INDX)TO                            
046100                RESP-BAY-UPD      (TAB-INDX)                              
046200         END-IF                                                           
046300         IF REQU-KVPLATS-UPD  (TAB-INDX) NOT = ALL '+'                    
046400           MOVE REQU-KVPLATS-UPD  (TAB-INDX)TO                            
046500                RESP-KVPLATS-UPD  (TAB-INDX)                              
046600         END-IF                                                           
046700         ADD 1 TO TAB-INDX                                                
046800       END-PERFORM                                                        
046900     END-IF                                                               
047000* KOLLA TABELL NR 2 * 500                                                 
047100     MOVE NEJ TO TABELL-2-SW                                              
047200     IF INDATA-OK AND REQU-KVRADER > 0 AND POST-SAKNAS                    
047300       MOVE +1 TO TAB-INDX                                                
047400                                                                          
047500       PERFORM UNTIL TAB-INDX > REQU-KVRADER                              
047600         IF REQU-CMD-IN (TAB-INDX) NOT = ALL '+'                          
047700           MOVE JA            TO TABELL-2-SW                              
047800           MOVE JA            TO POST-FINNS-SW                            
047900           IF REQU-CMD-IN (TAB-INDX) = 'CHA'                              
048000             IF REQU-KVPLATS-IN (TAB-INDX) NOT = ALL '+'                  
048100               IF REQU-KVPLATS-IN (TAB-INDX) NUMERIC                      
048200                 CONTINUE                                                 
048300               ELSE                                                       
048400                 MOVE NEJ TO INDATA-SW                                    
048500                 MOVE '024'   TO RESP-IDMSG-ERROR                         
048600               END-IF                                                     
048700             ELSE                                                         
048800               MOVE NEJ       TO INDATA-SW                                
048900               MOVE '014'     TO RESP-IDMSG-ERROR                         
049000             END-IF                                                       
049100           END-IF                                                         
049200         END-IF                                                           
049300         IF REQU-KVPLATS-IN (TAB-INDX) NOT = ALL '+'                      
049400           MOVE JA            TO TABELL-2-SW                              
049500           MOVE JA            TO POST-FINNS-SW                            
049600           IF REQU-CMD-IN (TAB-INDX) = ALL '+'                            
049700             MOVE NEJ TO INDATA-SW                                        
049800             MOVE '023' TO RESP-IDMSG-ERROR                               
049900             MOVE 'CMD' TO RESP-IDELMT-ERROR                              
050000           END-IF                                                         
050100         END-IF                                                           
050200         ADD +1 TO TAB-INDX                                               
050300       END-PERFORM                                                        
050400       IF INDATA-FEL                                                      
050500         MOVE 1 TO TAB-INDX                                               
050600         PERFORM UNTIL TAB-INDX > MAX-INDX OR                             
050700                       TAB-INDX > REQU-KVRADER                            
050800           IF REQU-KVPLATS-IN (TAB-INDX) NOT = ALL '+'                    
050900              MOVE REQU-KVPLATS-IN (TAB-INDX)  TO                         
051000                   RESP-KVPLATS-IN (TAB-INDX)                             
051100           END-IF                                                         
051200           ADD 1 TO TAB-INDX                                              
051300         END-PERFORM                                                      
051400       END-IF                                                             
051500     END-IF                                                               
051600     IF POST-SAKNAS                                                       
051700       MOVE NEJ TO INDATA-SW                                              
051800       MOVE '014' TO RESP-IDMSG-ERROR                                     
051900     END-IF                                                               
052000     .                                                                    
052100     EJECT                                                                
052200 H-UPPDATERA-TABELL-1 SECTION.                                            
052300                                                                          
052400     MOVE 1 TO TAB-INDX                                                   
052500     PERFORM UNTIL TAB-INDX > 10                                          
052600       IF REQU-ADLAGOMR-UPD (TAB-INDX) NOT = ALL '+'                      
052700         MOVE REQU-IDDC-KEY                 TO LOC-IDDC                   
052800         MOVE REQU-ADLAGOMR-UPD (TAB-INDX)  TO LOC-ADLAGOMR               
052900         MOVE REQU-ADGANG-UPD   (TAB-INDX)  TO LOC-ADGANG                 
053000         MOVE REQU-BAY-UPD      (TAB-INDX)  TO WS-BAY                     
053100         MOVE WS-ADPLATS                    TO LOC-ADPLATS                
053200         MOVE REQU-KVPLATS-UPD  (TAB-INDX)  TO LOC-KVPLATS                
053300         MOVE SPACES                        TO LOC-KDLOC                  
053400         MOVE SPACES                        TO LOC-KDFREQ                 
053500         MOVE SPACES                        TO LOC-KDSTOR                 
053600         MOVE ZEROS                         TO LOC-KVMPART                
053700         MOVE SPACES                        TO LOC-TELOC                  
053800         PERFORM IMS-ISRT-WDJ801                                          
053900         MOVE '001'                         TO RESP-IDMSG-INFO            
054000       END-IF                                                             
054100       ADD 1 TO TAB-INDX                                                  
054200     END-PERFORM                                                          
054300     CONTINUE                                                             
054400     .                                                                    
054500     EJECT                                                                
054600 I-UPPDATERA-TABELL-2 SECTION.                                            
054700     MOVE 1 TO TAB-INDX                                                   
054800     PERFORM UNTIL TAB-INDX > MAX-INDX OR TAB-INDX > REQU-KVRADER         
054900       IF REQU-CMD-IN (TAB-INDX) NOT = ALL '+'                            
055000         IF REQU-CMD-IN (TAB-INDX) = 'CHA'                                
055100           MOVE REQU-ADLAGOMR-UT (TAB-INDX) TO W-LOC-ADLAGOMR             
055200           MOVE REQU-ADGANG-UT (TAB-INDX)   TO W-LOC-ADGANG               
055300           MOVE REQU-BAY-UT      (TAB-INDX) TO WS-BAY                     
055400           MOVE WS-ADPLATS                  TO W-LOC-ADPLATS              
055500           PERFORM IMS-GET-WDJ801                                         
055600           IF SEGMENT-FINNS                                               
055700             MOVE REQU-KVPLATS-IN (TAB-INDX) TO LOC-KVPLATS               
055800             PERFORM IMS-REPL-WDJ801                                      
055900             MOVE '001'                      TO RESP-IDMSG-INFO           
056000           END-IF                                                         
056100         END-IF                                                           
056200       END-IF                                                             
056300       IF REQU-CMD-IN (TAB-INDX) NOT = ALL '+'                            
056400         IF REQU-CMD-IN (TAB-INDX) = 'DEL'                                
056500           MOVE REQU-ADLAGOMR-UT (TAB-INDX) TO W-LOC-ADLAGOMR             
056600           MOVE REQU-ADGANG-UT   (TAB-INDX) TO W-LOC-ADGANG               
056700           MOVE REQU-BAY-UT      (TAB-INDX) TO WS-BAY                     
056800           MOVE WS-ADPLATS                  TO W-LOC-ADPLATS              
056900           PERFORM IMS-GET-WDJ801                                         
057000           IF SEGMENT-FINNS                                               
057100             MOVE REQU-KVPLATS-IN (TAB-INDX) TO LOC-KVPLATS               
057200             PERFORM IMS-DLET-WDJ801                                      
057300             MOVE '001' TO RESP-IDMSG-INFO                                
057400           ELSE                                                           
057500             MOVE '004' TO RESP-IDMSG-INFO                                
057600*          CALL ABEND                                                     
057700           END-IF                                                         
057800         END-IF                                                           
057900       END-IF                                                             
058000     ADD 1 TO TAB-INDX                                                    
058100     END-PERFORM                                                          
058200     CONTINUE                                                             
058300     .                                                                    
058400     EJECT                                                                
058500*    --- DISPATCHER-SEKTIONER                                             
058600 S01-HAEMTA-ANROPSDATA SECTION.                                           
058700                                                                          
058800     MOVE 'GETARG'               TO SUB-KDFUNC                            
058900     MOVE 'CARPARTS.LDC.FREELOCATIONUPDATE'   TO SUB-ADDISPABS            
059000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
059100                                                                          
059200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
059300                                                                          
059400     IF SUB-KDRC > 0                                                      
059500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
059600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
059700       DELIMITED BY SIZE INTO FELTEXT                                     
059800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
059900     END-IF                                                               
060000     .                                                                    
060100     SKIP3                                                                
060200 S02-RETURNERA-SVAR SECTION.                                              
060300                                                                          
060400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
060500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
060600                                                                          
060700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
060800                                                                          
060900     IF SUB-KDRC > 0                                                      
061000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
061100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
061200       DELIMITED BY SIZE INTO FELTEXT                                     
061300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
061400     END-IF                                                               
061500     .                                                                    
061600     EJECT                                                                
061700 IMS-GET-WDJ801 SECTION.                                                  
061800                                                                          
061900     STRING 'WDJ801  (WDJ801KY =' W-WDJ8KEY-X ')'                         
062000          DELIMITED BY SIZE INTO SSA1                                     
062100     MOVE '  GE' TO GODK-STATUSKODER                                      
062200     CALL CBLTDLI USING GHU WDJ8-PCB DLI-IO-WDJ801 SSA1                   
062300     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
062400     PERFORM IMS-STATUSKONTROLL                                           
062500     .                                                                    
062600     SKIP3                                                                
062700 IMS-GN-WDJ801 SECTION.                                                   
062800                                                                          
062900     STRING 'WDJ801  (WDJ801KY =' W-WDJ8KEY-X ')'                         
063000          DELIMITED BY SIZE INTO SSA1                                     
063100     MOVE '  GE' TO GODK-STATUSKODER                                      
063200     CALL CBLTDLI USING GN  WDJ8-PCB DLI-IO-WDJ801 SSA1                   
063300     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
063400     PERFORM IMS-STATUSKONTROLL                                           
063500     .                                                                    
063600     SKIP3                                                                
063700 IMS-GET-WDJ801-MIN-MAX SECTION.                                          
063800                                                                          
063900     STRING 'WDJ801  (WDJ801KY>=' W-WDJ8KEY-MIN-X                         
064000                    '&WDJ801KY<=' W-WDJ8KEY-MAX-X ')'                     
064100          DELIMITED BY SIZE INTO SSA1                                     
064200     MOVE '  GE' TO GODK-STATUSKODER                                      
064300     CALL CBLTDLI USING GHU WDJ8-PCB DLI-IO-WDJ801 SSA1                   
064400     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
064500     PERFORM IMS-STATUSKONTROLL                                           
064600     .                                                                    
064700     SKIP3                                                                
064800 IMS-GN-WDJ801-MIN-MAX SECTION.                                           
064900                                                                          
065000     STRING 'WDJ801  (WDJ801KY>=' W-WDJ8KEY-MIN-X                         
065100                    '&WDJ801KY<=' W-WDJ8KEY-MAX-X ')'                     
065200          DELIMITED BY SIZE INTO SSA1                                     
065300     MOVE '  GE' TO GODK-STATUSKODER                                      
065400     CALL CBLTDLI USING GN WDJ8-PCB DLI-IO-WDJ801 SSA1                    
065500     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
065600     PERFORM IMS-STATUSKONTROLL                                           
065700     .                                                                    
065800     SKIP3                                                                
065900 IMS-ISRT-WDJ801 SECTION.                                                 
066000                                                                          
066100     MOVE 'WDJ801 ' TO SSA1                                               
066200     MOVE '  II' TO GODK-STATUSKODER                                      
066300     CALL CBLTDLI USING ISRT WDJ8-PCB DLI-IO-WDJ801 SSA1                  
066400     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
066500     PERFORM IMS-STATUSKONTROLL                                           
066600     .                                                                    
066700     SKIP3                                                                
066800 IMS-REPL-WDJ801 SECTION.                                                 
066900                                                                          
067000     MOVE '  ' TO GODK-STATUSKODER                                        
067100     CALL CBLTDLI USING REPL WDJ8-PCB DLI-IO-WDJ801                       
067200     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
067300     PERFORM IMS-STATUSKONTROLL                                           
067400     .                                                                    
067500     SKIP3                                                                
067600 IMS-DLET-WDJ801 SECTION.                                                 
067700                                                                          
067800     MOVE '  ' TO GODK-STATUSKODER                                        
067900     CALL CBLTDLI USING DLET WDJ8-PCB DLI-IO-WDJ801                       
068000     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
068100     PERFORM IMS-STATUSKONTROLL                                           
068200     .                                                                    
068300     EJECT                                                                
068400 IMS-STATUSKONTROLL SECTION.                                              
068500                                                                          
068600     SET STATUS-IX TO 1                                                   
068700     SEARCH GODK-STATUS                                                   
068800       AT END                                                             
068900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
069000         DELIMITED BY SIZE INTO ERROR-TEXT                                
069100         CALL FELLOG                                                      
069200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
069300         CONTINUE                                                         
069400     END-SEARCH                                                           
069500     .                                                                    
