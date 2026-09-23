000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL012400.                                                
000300 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000400 DATE-WRITTEN.   04/05/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700* WL012400 PROGRAM IS A REPLICA OF W4035700 PROGRAM                       
000800* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
000900*                                                                         
001000*    ADDRESS:       'CARPARTS.LDC.SPLITOFORDERPRC'                        
001100*                                                                         
001200*    FUNCTION:                                                            
001300*        HERE A SHORT DESCRIPTION OF WHAT THE PROGAMS DOES                
001400*        SHOULD BE INSERTED. DESCRIBE ITS PURPOSE AND                     
001500*        WHICH DATABASES ARE PROCESSED.                                   
001600*                                                                         
001700*        THE PROGRAM IS AN UPDATING NPP                                   
001800*        PROGRAM UPDATES WLORQA (WDQ3)                                    
001900*                                                                         
002000*        PROGRAM READS   WLORQI (WDQ2)                                    
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSACTION: WL0124T                                             
002400*        REQUEST:     WZ01REQU                                            
002500*                     WL0124I1                                            
002600*                                                                         
002700*    OUTDATA.                                                             
002800*        RESPONSE:    WZ01RESP                                            
002900*                     WL0124O1                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300 DATA DIVISION.                                                           
003400                                                                          
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'WL012400'.            
003800                                                                          
003900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004100 77  KDRC-DISPLAY                PIC Z(5).                                
004200                                                                          
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500                                                                          
004600*    --- INDEX FOR BROWSE LINES                                           
004700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
004900                                                                          
005000 77  TOTAL-REC                   PIC S9(4)  VALUE +0    COMP SYNC.        
005100                                                                          
005200                                                                          
005300 77  WS-IDELMT-ERROR             PIC X(16).                               
005400 77  WS-IDMSG-ERROR              PIC X(03).                               
005500 77  WS-IDMSG-INFO               PIC X(03).                               
005600                                                                          
005700*    --- WORKFIELDS FOR ACTUAL KEYVALUES OF SCREENS                       
005800                                                                          
005900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006000     88  INDATA-OK                           VALUE 'J'.                   
006100     88  INDATA-WRONG                        VALUE 'N'.                   
006200                                                                          
006300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006400     88  KEYS-OK                             VALUE 'J'.                   
006500     88  KEYS-WRONG                          VALUE 'N'.                   
006600                                                                          
006700 77  JOINED-ORDER-SW             PIC X       VALUE 'N'.                   
006800     88  JOINED-ORDER                        VALUE 'J'.                   
006900     88  NOT-JOINED-ORDER                    VALUE 'N'.                   
007000                                                                          
007100 77  IDPLKLST-SW                 PIC X       VALUE 'N'.                   
007200     88  IDPLKLST-OK                         VALUE 'J'.                   
007300     88  IDPLKLST-NOT-OK                     VALUE 'N'.                   
007400                                                                          
007500 77  PROD-INGANG-SW              PIC X       VALUE 'J'.                   
007600     88  PROD-INGANG                         VALUE 'J'.                   
007700                                                                          
007800                                                                          
007900     EJECT                                                                
008000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008100 01  GENERAL-SUBPROGRAMS.                                                 
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008600     SKIP3                                                                
008700*    --- PARAMETERS TO ABEND                                              
008800                                                                          
008900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009200     SKIP3                                                                
009300 01  MESSAGE-CODES.                                                       
009400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
009500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND'.             
010400*    --- PARAMETRAR WWOMVAND                                              
010500*01 -COPY WWOMVAND                                                        
010600     EJECT                                                                
010700     SKIP3                                                                
010800*                                                                         
010900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011000     SKIP3                                                                
011100*01  -COPY WZ01SUB                                                        
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011400     SKIP3                                                                
011500 01  REQU-AREA.                                                           
011600*    03  -COPY WZ01REQU                                                   
011700*    03  -COPY WL0124I1                                                   
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012000     SKIP3                                                                
012100 01  RESP-AREA.                                                           
012200*    03  -COPY WZ01RESP                                                   
012300*    03  -COPY WL0124O1                                                   
012400     EJECT                                                                
012500                                                                          
012600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900     SKIP3                                                                
013000 01  KEYS-TILL-DLI.                                                       
013100*                                                                         
013200*    DIRECT KEY TO ORDERPARTSREGISTER                                     
013300*                                                                         
013400*    MIN-MAX KEY TO ORDERPARTSREGISTER                                    
013500*                                                                         
013600     03  W-WDQ301KY-MIN-X.                                                
013700            07 W-IDORDER-MIN     PIC S9(7)         COMP-3.                
013800            07 W-IDDC-MIN        PIC X(2).                                
013900         05 W-IDPRODNR-MIN-X.                                             
014000            07 W-IDPRODNR-MIN    PIC S9(7)         COMP-3.                
014100         05 W-IDPLKLST-MIN-X.                                             
014200            07 W-IDPLKLST-MIN    PIC S9(3)         COMP-3.                
014300                                                                          
014400     03  W-WDQ301KY-MAX-X.                                                
014500            07 W-IDORDER-MAX     PIC S9(7)         COMP-3.                
014600            07 W-IDDC-MAX        PIC X(2).                                
014700            07 W-IDPRODNR-MAX    PIC X(4)  VALUE HIGH-VALUE.              
014800            07 W-IDPLKLST-MAX    PIC X(2)  VALUE HIGH-VALUE.              
014900                                                                          
015000     03  W-IDORDER-X.                                                     
015100            07 W-D-IDORDER       PIC S9(7)         COMP-3.                
015200                                                                          
015300     03  W-IDPLKLST-X.                                                    
015400            07 W-IDPLKLST        PIC S9(3)         COMP-3.                
015500                                                                          
015600     03  W-IDDC-X.                                                        
015700            07 W-IDDC-WDQ2       PIC X(2).                                
015800                                                                          
015900     03  W-WDQ2CSEQ-X.                                                    
016000            07 W-IDDISTR-CSEQ    PIC S9(5)         COMP-3.                
016100            07 W-IDKUNDNR-CSEQ   PIC S9(7)         COMP-3.                
016200            07 W-IDKUNDRF-CSEQ   PIC X(10) VALUE SPACES  .                
016300            07 W-IDKUNDRF-FILLER REDEFINES W-IDKUNDRF-CSEQ.               
016400              09 W-IDORDNR7-CSEQ    PIC  9(7).                            
016500                                                                          
016600     03  W-WDQ3DSEQ-MIN-X.                                                
016700         05  W-Q3DSEQ-IDPRODNR-MIN   PIC S9(7) VALUE ZERO COMP-3.         
016800         05  W-Q3DSEQ-IDPLKLST-MIN   PIC S9(3) VALUE ZERO COMP-3.         
016900     03  W-WDQ3DSEQ-MAX-X.                                                
017000         05  W-Q3DSEQ-IDPRODNR-MAX   PIC S9(7) VALUE ZERO COMP-3.         
017100            05 W-IDPLKLST-MAX        PIC X(2)  VALUE HIGH-VALUE.          
017200                                                                          
017300     03  IDDISTR-WS              PIC 9(4).                                
017400     03  IDKUNDNR-WS             PIC 9(6).                                
017500     03  IDORDNR7-WS             PIC X(7).                                
017600     03  IDPRODNR-WS             PIC X(7).                                
017700     03  IDPRODNR-WS-NUM         PIC 9(7)    VALUE ZERO.                  
017800     03  W-TILST-OD              PIC  9(11).                              
017900     03  WS-ORQA-ODEL-TILST-OD   PIC  9(11).                              
018000     03  WS-ORQA-ODEL-IDDC       PIC  X(02).                              
018100     03  W-DARFS-X.                                                       
018200        05 W-DARFS               PIC 9(12).                               
018300     03  W-DARFS-X2 REDEFINES W-DARFS-X.                                  
018400        05 W-DARFS-DATUM         PIC 9(08).                               
018500        05 W-DARFS-KLOCKA        PIC 9(04).                               
018600     03  WS-TITRPAVG.                                                     
018700        05 WS-TIAAMMDD           PIC  9(6).                               
018800        05 WS-TIHHMM             PIC  9(4).                               
018900     03  W-TIHHMM                PIC  9(4).                               
019000*                                                                         
019100*    --- STATUS-CODE FROM IMS                                             
019200*                                                                         
019300 01  STATUS-WS                   PIC XX.                                  
019400     88  SEGMENT-FOUND                       VALUE '  '.                  
019500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
019600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
019700     88  END-OF-DATA                         VALUE 'GB'.                  
019800     SKIP2                                                                
019900 01  GOOD-STATUSCODES.                                                    
020000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020100     SKIP3                                                                
020200 01  SSA1                        PIC X(90).                               
020300 01  SSA2                        PIC X(90).                               
020400 01  DATUM-TIME.                                                          
020500     03 JJ-MM-DD                 PIC 9(6).                                
020600     03 HH-MM                    PIC 9(4)  VALUE 0.                       
020700     EJECT                                                                
020800*    --- IMS FUNCTIONCODES                                                
020900*01  -COPY W0003                                                          
021000     EJECT                                                                
021100*    ---  DLI INPUT-OUTPUT AREA                                           
021200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA1'.          
021300     SKIP3                                                                
021400 01  DLI-IO-AREA1.                                                        
021500     03  IO-AREA1                PIC X(256)  VALUE SPACE.                 
021600     SKIP3                                                                
021700     03  WLORQA01 REDEFINES IO-AREA1.                                     
021800*        05  -COPY WDQ301     -PRE ORQA-                                  
021900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA2'.          
022000     SKIP3                                                                
022100 01  DLI-IO-AREA2.                                                        
022200     03  IO-AREA2                PIC X(4064)  VALUE SPACE.                
022300     SKIP3                                                                
022400     03  WLORQI01 REDEFINES IO-AREA2.                                     
022500*        05  -COPY WDQ201                                                 
022600     SKIP3                                                                
022700     03  WLORQI12 REDEFINES IO-AREA2.                                     
022800*        05  -COPY WDQ212                                                 
022900     EJECT                                                                
023000 LINKAGE SECTION.                                                         
023100 01  MSG-PCB                     PIC X.                                   
023200                                                                          
023300*01  -COPY W0008      -PRE WDQ2-                                          
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023600*01  -COPY W0008      -PRE ORQA-                                          
023700     05  FILLER                  PIC X.                                   
023800     EJECT                                                                
023900*01  -COPY W0008      -PRE ORQAD-                                         
024000     05  FILLER                  PIC X.                                   
024100     EJECT                                                                
024200 PROCEDURE DIVISION  USING MSG-PCB WDQ2-PCB                               
024300                           ORQA-PCB ORQAD-PCB.                            
024400     ENTRY 'DLITCBL' USING MSG-PCB WDQ2-PCB                               
024500                           ORQA-PCB ORQAD-PCB.                            
024600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
024700     IF SUB-KDRC = 0                                                      
024800       PERFORM A-INIT                                                     
024900       PERFORM B-CONTROL-KEYS                                             
025000       IF KEYS-OK                                                         
025100         IF REQU-KDPGMACT = 'E'                                           
025200           PERFORM G-CONTROL-INPUT                                        
025300           IF INDATA-OK                                                   
025400             PERFORM H-UPDATE                                             
025500           END-IF                                                         
025600         END-IF                                                           
025700         IF INDATA-OK                                                     
025800            PERFORM F-READ-SHOW-INFO                                      
025900         END-IF                                                           
026000       ELSE                                                               
026100         MOVE '022' TO RESP-IDMSG-ERROR                                   
026200       END-IF                                                             
026300                                                                          
026400       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
026500       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
026600       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
026700       IF WS-IDMSG-ERROR NOT = SPACE                                      
026800           MOVE ALL '+' TO RESP-WL0124O1(1:26)                            
026900           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
027000           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
027100           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
027200           MOVE  001             TO RESP-IDMSGVER                         
027300           IF  REQU-KDPGMACT = 'S'                                        
027400              MOVE ZERO             TO RESP-KVRADER                       
027500           ELSE                                                           
027600             IF REQU-KVRADER NUMERIC                                      
027700               MOVE REQU-KVRADER     TO RESP-KVRADER                      
027800             ELSE                                                         
027900               MOVE ZERO             TO RESP-KVRADER                      
028000             END-IF                                                       
028100           END-IF                                                         
028200       END-IF                                                             
028300       PERFORM S02-RETURN-RESPONSE                                        
028400     END-IF                                                               
028500                                                                          
028600     MOVE ZERO TO RETURN-CODE                                             
028700     GOBACK                                                               
028800     .                                                                    
028900     EJECT                                                                
029000 A-INIT SECTION.                                                          
029100                                                                          
029200     ACCEPT JJ-MM-DD FROM DATE.                                           
029300     MOVE ALL '+' TO RESP-AREA                                            
029400     MOVE SPACE   TO RESP-IDMSG-INFO                                      
029500                     RESP-IDMSG-ERROR                                     
029600                     RESP-IDELMT-ERROR                                    
029700     MOVE 001     TO RESP-IDMSGVER                                        
029800     MOVE ZERO    TO RESP-KVRADER                                         
029900                                                                          
030000     MOVE ZERO                 TO W-IDPRODNR-MIN                          
030100     MOVE HIGH-VALUES          TO W-IDPRODNR-MAX                          
030200     MOVE ZERO                 TO W-Q3DSEQ-IDPRODNR-MIN                   
030300     MOVE ZERO                 TO W-Q3DSEQ-IDPRODNR-MAX                   
030400                                                                          
030500     MOVE LOW-VALUES           TO W-WDQ301KY-MIN-X                        
030600     MOVE HIGH-VALUES          TO W-WDQ301KY-MAX-X                        
030700                                                                          
030800     MOVE NOO                  TO PROD-INGANG-SW                          
030900     MOVE SPACE                TO STATUS-WS                               
031000     .                                                                    
031100     EJECT                                                                
031200 B-CONTROL-KEYS SECTION.                                                  
031300                                                                          
031400     MOVE YES TO KEYS-SW                                                  
031500                                                                          
031600     IF REQU-IDDISTR-KEY NOT = ALL '+'   AND                              
031700        REQU-IDDISTR-KEY NUMERIC                                          
031800        MOVE REQU-IDDISTR-KEY TO IDDISTR-WS                               
031900                                 RESP-IDDISTR-KEY                         
032000     ELSE                                                                 
032100        MOVE ZERO             TO IDDISTR-WS                               
032200     END-IF                                                               
032300                                                                          
032400     IF REQU-IDKUNDNR-KEY NOT = ALL '+'    AND                            
032500        REQU-IDKUNDNR-KEY NUMERIC                                         
032600        MOVE REQU-IDKUNDNR-KEY TO IDKUNDNR-WS                             
032700                                  RESP-IDKUNDNR-KEY                       
032800     ELSE                                                                 
032900        MOVE ZERO              TO IDKUNDNR-WS                             
033000     END-IF                                                               
033100                                                                          
033200     IF REQU-IDORDNR7-KEY NOT = ALL '+'  AND                              
033300        REQU-IDORDNR7-KEY NUMERIC                                         
033400        MOVE REQU-IDORDNR7-KEY TO IDORDNR7-WS                             
033500                                  RESP-IDORDNR7-KEY                       
033600     ELSE                                                                 
033700        MOVE ZERO              TO IDORDNR7-WS                             
033800     END-IF                                                               
033900                                                                          
034000     IF REQU-IDPRODNR-KEY NOT = ALL '+'                                   
034100       IF REQU-IDPRODNR-KEY NOT NUMERIC                                   
034200          MOVE NOO             TO KEYS-SW                                 
034300       END-IF                                                             
034400     END-IF                                                               
034500     IF REQU-IDPRODNR-KEY = ALL '+'                                       
034600        MOVE NOO               TO PROD-INGANG-SW                          
034700     ELSE                                                                 
034800        MOVE REQU-IDPRODNR-KEY TO IDPRODNR-WS                             
034900        MOVE YES               TO PROD-INGANG-SW                          
035000     END-IF                                                               
035100                                                                          
035200     IF REQU-IDPRODNR-KEY  NUMERIC                                        
035300        MOVE REQU-IDPRODNR-KEY TO RESP-IDPRODNR-KEY                       
035400       IF IDPRODNR-WS > ZERO                                              
035500         MOVE IDPRODNR-WS     TO IDPRODNR-WS-NUM                          
035600         MOVE IDPRODNR-WS-NUM TO W-Q3DSEQ-IDPRODNR-MIN                    
035700                                 W-Q3DSEQ-IDPRODNR-MAX                    
035800                                 W-IDPRODNR-MIN                           
035900                                 W-IDPRODNR-MAX                           
036000       END-IF                                                             
036100     ELSE                                                                 
036200       MOVE NOO TO PROD-INGANG-SW                                         
036300     END-IF                                                               
036400                                                                          
036500     MOVE REQU-IDDC-KEY TO  RESP-IDDC-KEY                                 
036600                            W-IDDC-MAX                                    
036700                            W-IDDC-MIN                                    
036800                            W-IDDC-WDQ2                                   
036900                                                                          
037000     IF IDDISTR-WS NOT NUMERIC                                            
037100     OR IDKUNDNR-WS NOT NUMERIC                                           
037200     OR IDORDNR7-WS NOT NUMERIC                                           
037300       MOVE NOO TO KEYS-SW                                                
037400     ELSE                                                                 
037500                                                                          
037600       MOVE IDDISTR-WS  TO W-IDDISTR-CSEQ                                 
037700       MOVE IDKUNDNR-WS TO W-IDKUNDNR-CSEQ                                
037800       MOVE IDORDNR7-WS TO W-IDORDNR7-CSEQ                                
037900       INSPECT RESP-IDDISTR-KEY  REPLACING LEADING ZERO BY SPACE          
038000       INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE          
038100       INSPECT RESP-IDORDNR7-KEY REPLACING LEADING ZERO BY SPACE          
038200       INSPECT RESP-IDPRODNR-KEY REPLACING LEADING ZERO BY SPACE          
038300     END-IF                                                               
038400                                                                          
038500     IF IDDISTR-WS  NUMERIC  AND IDDISTR-WS > 0                           
038600     AND IDKUNDNR-WS NUMERIC AND IDKUNDNR-WS > 0                          
038700     AND IDORDNR7-WS NUMERIC AND IDORDNR7-WS > 0                          
038800       MOVE NOO TO PROD-INGANG-SW                                         
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 F-READ-SHOW-INFO SECTION.                                                
039300                                                                          
039400     PERFORM FA-READ-BASICDATA                                            
039500                                                                          
039600     IF SEGMENT-MISSING                                                   
039700       IF REQU-KDPGMACT = 'S'                                             
039800         MOVE '027' TO RESP-IDMSG-ERROR                                   
039900     ELSE                                                                 
040000        MOVE '025'     TO RESP-IDMSG-ERROR                                
040100        MOVE 'IDORDNR' TO RESP-IDELMT-ERROR                               
040200       END-IF                                                             
040300     ELSE                                                                 
040400        IF JOINED-ORDER                                                   
040500            CONTINUE                                                      
040600         ELSE                                                             
040700            MOVE ORQA-ODEL-DATRPAVD (3:6) TO RESP-TITRPAVG-DAT            
040800                                             IN RESP-TITRPAVG             
040900            MOVE ORQA-ODEL-TIHHMM         TO W-TIHHMM                     
041000            MOVE W-TIHHMM TO RESP-TITRPAVG-TID IN RESP-TITRPAVG           
041100            MOVE SPACE                  TO RESP-TITRP-FILLER              
041200                                                                          
041300                                                                          
041400            MOVE ORQA-ODEL-IDTRP  TO RESP-IDTRP-IN                        
041500            MOVE ORQA-ODEL-IDPRODNR TO RESP-IDPRODNR-KEY                  
041600                                       IDPRODNR-WS                        
041700                                       IDPRODNR-WS-NUM                    
041800                                       W-Q3DSEQ-IDPRODNR-MIN              
041900                                       W-Q3DSEQ-IDPRODNR-MAX              
042000                                       W-IDPRODNR-MIN                     
042100                                       W-IDPRODNR-MAX                     
042200                                                                          
042300         INSPECT RESP-IDPRODNR-KEY REPLACING LEADING ZERO BY SPACE        
042400        END-IF                                                            
042500     END-IF                                                               
042600     MOVE +1 TO INDX                                                      
042700     MOVE +0 TO TOTAL-REC                                                 
042800                                                                          
042900     PERFORM UNTIL INDX > MAX-INDX                                        
043000       IF SEGMENT-FOUND AND NOT-JOINED-ORDER                              
043100         MOVE ORQA-ODEL-IDPLKLST     TO RESP-IDPLKLST   (INDX)            
043200         MOVE ORQA-ODEL-IDPRC        TO RESP-IDPRC      (INDX)            
043300         MOVE ORQA-ODEL-IDPRCPLK     TO RESP-IDPRCPLK   (INDX)            
043310         MOVE ORQA-ODEL-IDLOTNR-PLK  TO RESP-IDLOTNR-PLK(INDX)            
043400         MOVE ORQA-ODEL-KDODELSTA    TO RESP-KDODELSTA (INDX)             
043500         MOVE ORQA-ODEL-KVRADER  TO RESP-KVRADER-LINE  (INDX)             
043600         MOVE ORQA-ODEL-KVPACKRAD-OD TO RESP-KVPACKRAD-OD (INDX)          
043700         MOVE ORQA-ODEL-VKORDNTO     TO RESP-VKORDNTO  (INDX)             
043800         MOVE ORQA-ODEL-VLORDNTO     TO RESP-VLORDNTO  (INDX)             
043900         IF ORQA-ODEL-KDODELSTA = 'U' OR 'P' OR 'F' OR 'L'                
044000           MOVE ORQA-ODEL-IDUSER TO RESP-IDUSER-LINE (INDX)               
044100           INSPECT RESP-IDUSER-LINE (INDX)  REPLACING                     
044200                   LEADING ZERO BY SPACE                                  
044300         END-IF                                                           
044400                                                                          
044500         MOVE ORQA-ODEL-SUPTID       TO RESP-SUPTID  (INDX)               
044700         MOVE ORQA-ODEL-TILST-OD     TO W-TILST-OD                        
044800                                                                          
044900         MOVE W-TILST-OD (2:6)       TO RESP-TILST-DAT                    
045000                           IN RESP-TILST-OD (INDX)                        
045100         MOVE W-TILST-OD (8:4)       TO W-TIHHMM                          
045200         MOVE W-TIHHMM TO RESP-TILST-TID IN RESP-TILST-OD (INDX)          
045300         MOVE SPACE  TO RESP-TILST-FILLER (INDX)                          
045400         MOVE ORQA-ODEL-DARFS (3:6)  TO RESP-TIRFS-DAT                    
045500                            IN  RESP-TIRFS-OUT (INDX)                     
045600         MOVE ORQA-ODEL-DARFS (9:4)  TO W-TIHHMM                          
045700         MOVE W-TIHHMM TO RESP-TIRFS-TID IN  RESP-TIRFS-OUT (INDX)        
045800         MOVE SPACE  TO RESP-TIRFS-FILLER (INDX)                          
045900                                                                          
046000         PERFORM FB-READ-LINEDATA                                         
046100         ADD 1 TO TOTAL-REC                                               
046200       END-IF                                                             
046300       ADD 1 TO INDX                                                      
046400     END-PERFORM                                                          
046500     MOVE TOTAL-REC  TO RESP-KVRADER                                      
046600     IF TOTAL-REC = 500                                                   
046700       MOVE MAX-INDX TO RESP-KVRADER                                      
046800       MOVE '028'    TO RESP-IDMSG-ERROR                                  
046900     END-IF                                                               
047000                                                                          
047100     .                                                                    
047200     EJECT                                                                
047300 FA-READ-BASICDATA SECTION.                                               
047400                                                                          
047500     IF PROD-INGANG                                                       
047600       PERFORM IMS-GU-ORQA01-M-Q3DSEQ                                     
047700       IF SEGMENT-FOUND                                                   
047800         MOVE ORQA-ODEL-IDDISTR  TO IDDISTR-WS                            
047900                                    W-IDDISTR-CSEQ                        
048000         MOVE IDDISTR-WS         TO RESP-IDDISTR-KEY                      
048100         MOVE ORQA-ODEL-IDKUNDNR TO IDKUNDNR-WS                           
048200                                    W-IDKUNDNR-CSEQ                       
048300         MOVE IDKUNDNR-WS        TO RESP-IDKUNDNR-KEY                     
048400         MOVE ORQA-ODEL-IDORDNR7 TO IDORDNR7-WS                           
048500                                    W-IDORDNR7-CSEQ                       
048600                                    RESP-IDORDNR7-KEY                     
048700         INSPECT RESP-IDDISTR-KEY  REPLACING LEADING ZERO BY SPACE        
048800         INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE        
048900         INSPECT RESP-IDORDNR7-KEY REPLACING LEADING ZERO BY SPACE        
049000       END-IF                                                             
049100     END-IF                                                               
049200                                                                          
049300     IF SEGMENT-FOUND OR (REQU-KDPGMACT = 'E')                            
049400       PERFORM IMS-GU-CSEQ-WDQ2                                           
049500                                                                          
049600       MOVE NOO                  TO JOINED-ORDER-SW                       
049700       IF SEGMENT-FOUND                                                   
049800                                                                          
049900         MOVE OHUV-IDORDER  TO W-IDORDER-MIN                              
050000                               W-IDORDER-MAX                              
050100         PERFORM IMS-GNP-WDQ2                                             
050200         IF SEGMENT-FOUND                                                 
050300            MOVE ARB-KDFRAKT TO RESP-KDFRAKT                              
050400            IF PROD-INGANG                                                
050500              CONTINUE                                                    
050600            ELSE                                                          
050700              PERFORM IMS-GU-ORQA-WDQ3-KVAL                               
050800            END-IF                                                        
050900         END-IF                                                           
051000       END-IF                                                             
051100     END-IF                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 FB-READ-LINEDATA SECTION.                                                
051500                                                                          
051600     IF PROD-INGANG                                                       
051700       PERFORM IMS-GN-ORQA01-M-Q3DSEQ                                     
051800     ELSE                                                                 
051900       PERFORM IMS-GN-ORQA-WDQ3                                           
052000     END-IF                                                               
052100     .                                                                    
052200     EJECT                                                                
052300 G-CONTROL-INPUT SECTION.                                                 
052400                                                                          
052500     MOVE YES                  TO INDATA-SW                               
052600     IF REQU-KVRADER NOT NUMERIC                                          
052700       MOVE ZERO             TO RESP-KVRADER                              
052800       MOVE NOO              TO INDATA-SW                                 
052900       MOVE 'KVRADER'        TO RESP-IDELMT-ERROR                         
053000       MOVE '024'            TO RESP-IDMSG-ERROR                          
053100     ELSE                                                                 
053200       IF REQU-KVRADER = 0                                                
053300         MOVE NOO              TO INDATA-SW                               
053400         MOVE 'KVRADER'        TO RESP-IDELMT-ERROR                       
053500         MOVE '126'            TO RESP-IDMSG-ERROR                        
053600         MOVE REQU-KVRADER     TO RESP-KVRADER                            
053700       END-IF                                                             
053800     END-IF                                                               
053900                                                                          
054000     IF REQU-FLJANEJ           = '+'     AND                              
054100        REQU-TIAAMMDD-UPD      = ALL '+' AND                              
054200        REQU-TIHHMM-UPD        = ALL '+' AND                              
054300        REQU-IDPLKLST-UPD      = ALL '+'                                  
054400         MOVE 'KEY'  TO RESP-IDELMT-ERROR                                 
054500         MOVE '026'  TO RESP-IDMSG-ERROR                                  
054600         MOVE NOO              TO INDATA-SW                               
054700     ELSE                                                                 
054800         PERFORM GA-CONTROL-FLJANEJ                                       
054900                                                                          
055000         PERFORM IMS-GU-CSEQ-WDQ2                                         
055100         IF SEGMENT-FOUND                                                 
055200           MOVE OHUV-IDORDER TO W-IDORDER-MIN                             
055300                                W-IDORDER-MAX                             
055400                                W-D-IDORDER                               
055500         ELSE                                                             
055600           MOVE NOO          TO INDATA-SW                                 
055700         END-IF                                                           
055800                                                                          
055900         IF REQU-FLJANEJ = 'Y'                                            
056000             PERFORM GB-CONTROL-UPDATE-ALL                                
056100         ELSE                                                             
056200             PERFORM GC-CONTROL-UPDATE-ONE-PLKLST                         
056300         END-IF                                                           
056400     END-IF                                                               
056500                                                                          
056600     .                                                                    
056700     EJECT                                                                
056800 GA-CONTROL-FLJANEJ   SECTION.                                            
056900                                                                          
057000     IF REQU-FLJANEJ                = 'N' OR                              
057100        REQU-FLJANEJ                = 'Y'                                 
057200        CONTINUE                                                          
057300     ELSE                                                                 
057400        MOVE 'FLJANEJ'  TO RESP-IDELMT-ERROR                              
057500        MOVE '023'      TO RESP-IDMSG-ERROR                               
057600        MOVE NOO        TO INDATA-SW                                      
057700     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
058000 GB-CONTROL-UPDATE-ALL  SECTION.                                          
058100                                                                          
058200     MOVE LOW-VALUE                  TO W-IDPLKLST-X                      
058300     IF REQU-IDPLKLST-UPD             = ALL '+'   AND                     
058400        REQU-TIAAMMDD-UPD             NUMERIC     AND                     
058500        REQU-TIHHMM-UPD               NUMERIC                             
058600         MOVE REQU-TIAAMMDD-UPD       TO WS-TIAAMMDD                      
058700                                         RESP-TIAAMMDD-UPD                
058800         MOVE REQU-TIHHMM-UPD         TO WS-TIHHMM                        
058900                                         RESP-TIHHMM-UPD                  
059000         MOVE ZERO                    TO W-DARFS                          
059100         MOVE WS-TIAAMMDD             TO W-DARFS-DATUM                    
059200         IF WS-TIAAMMDD NOT = ZERO                                        
059300           IF WS-TIAAMMDD < 500000                                        
059400             MOVE 20                 TO W-DARFS-DATUM (1:2)               
059500           ELSE                                                           
059600             IF WS-TIAAMMDD < 999999                                      
059700               MOVE 19               TO W-DARFS-DATUM (1:2)               
059800             ELSE                                                         
059900               MOVE 99999999         TO W-DARFS-DATUM                     
060000             END-IF                                                       
060100           END-IF                                                         
060200         END-IF                                                           
060300         MOVE WS-TIHHMM              TO W-DARFS-KLOCKA                    
060400                                                                          
060500         PERFORM GBA-CONTROL-ALL-ORDERPARTS                               
060600      ELSE                                                                
060700         IF REQU-TIAAMMDD-UPD NOT        NUMERIC                          
060800           MOVE 'TIAAMMDD'  TO RESP-IDELMT-ERROR                          
060900           MOVE '024'           TO RESP-IDMSG-ERROR                       
061000         END-IF                                                           
061100                                                                          
061200         IF REQU-TIHHMM-UPD NOT        NUMERIC                            
061300           MOVE 'TIHHMM'    TO RESP-IDELMT-ERROR                          
061400           MOVE '024'           TO RESP-IDMSG-ERROR                       
061500         END-IF                                                           
061600                                                                          
061700         IF REQU-IDPLKLST-UPD NOT      = ALL '+'                          
061800           MOVE '032'             TO RESP-IDMSG-ERROR                     
061900         END-IF                                                           
062000                                                                          
062100         MOVE NOO                    TO INDATA-SW                         
062200     END-IF                                                               
062300     .                                                                    
062400     EJECT                                                                
062500 GBA-CONTROL-ALL-ORDERPARTS  SECTION.                                     
062600                                                                          
062700                                                                          
062800     PERFORM IMS-GU-ORQA-WDQ3                                             
062900     MOVE +1 TO TOTAL-REC                                                 
063000     PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATA OR                      
063100                   INDATA-WRONG                                           
063200        IF ORQA-ODEL-KDODELSTA = 'R' AND                                  
063300           ORQA-ODEL-DARFS     > W-DARFS                                  
063400            CONTINUE                                                      
063500         ELSE                                                             
063600            IF ORQA-ODEL-KDODELSTA = 'R'                                  
063700                MOVE '007' TO RESP-IDMSG-ERROR                            
063800                              RESP-IDMSG-ERROR-LINE (TOTAL-REC)           
063900             ELSE                                                         
064000                MOVE 'KDODELSTA' TO RESP-IDELMT-ERROR                     
064100                MOVE '127' TO RESP-IDMSG-ERROR                            
064200                              RESP-IDMSG-ERROR-LINE (TOTAL-REC)           
064300            END-IF                                                        
064400            MOVE NOO           TO INDATA-SW                               
064500        END-IF                                                            
064600        PERFORM IMS-GN-ORQA-WDQ3                                          
064700        ADD 1 TO TOTAL-REC                                                
064800     END-PERFORM                                                          
064900     .                                                                    
065000     EJECT                                                                
065100 GC-CONTROL-UPDATE-ONE-PLKLST      SECTION.                               
065200                                                                          
065300     IF REQU-IDPLKLST-UPD             NUMERIC AND                         
065400        REQU-TIAAMMDD-UPD             NUMERIC AND                         
065500        REQU-TIHHMM-UPD               NUMERIC                             
065600                                                                          
065700        MOVE REQU-TIAAMMDD-UPD      TO WS-TIAAMMDD                        
065800        MOVE REQU-TIHHMM-UPD        TO WS-TIHHMM                          
065900        MOVE ZERO                   TO W-DARFS                            
066000        MOVE WS-TIAAMMDD            TO W-DARFS-DATUM                      
066100        IF WS-TIAAMMDD NOT = ZERO                                         
066200          IF WS-TIAAMMDD < 500000                                         
066300            MOVE 20                 TO W-DARFS-DATUM (1:2)                
066400          ELSE                                                            
066500            IF WS-TIAAMMDD < 999999                                       
066600              MOVE 19               TO W-DARFS-DATUM (1:2)                
066700            ELSE                                                          
066800              MOVE 99999999         TO W-DARFS-DATUM                      
066900            END-IF                                                        
067000          END-IF                                                          
067100        END-IF                                                            
067200        MOVE WS-TIHHMM              TO W-DARFS-KLOCKA                     
067300                                                                          
067400        MOVE REQU-IDPLKLST-UPD       TO W-IDPLKLST                        
067500        PERFORM GCA-CHECK-IDPLKLST                                        
067600        IF IDPLKLST-NOT-OK                                                
067700            MOVE 'IDPLKLST2'  TO RESP-IDELMT-ERROR                        
067800            MOVE '023'       TO RESP-IDMSG-ERROR                          
067900            MOVE NOO                  TO INDATA-SW                        
068000        ELSE                                                              
068100            PERFORM IMS-GHU-ORQA-WDQ3-KVAL                                
068200            IF ORQA-ODEL-KDODELSTA  = 'R'     AND                         
068300               ORQA-ODEL-DARFS      > W-DARFS                             
068400                MOVE YES            TO INDATA-SW                          
068500            ELSE                                                          
068600             IF ORQA-ODEL-KDODELSTA = 'R'                                 
068700                 MOVE '007'            TO RESP-IDMSG-ERROR                
068800                 MOVE '007' TO RESP-IDMSG-ERROR-LINE (INDX)               
068900             ELSE                                                         
069000                MOVE 'KDODELSTA' TO RESP-IDELMT-ERROR                     
069100                MOVE '127'       TO RESP-IDMSG-ERROR                      
069200                MOVE '127' TO RESP-IDMSG-ERROR-LINE (INDX)                
069300             END-IF                                                       
069400             MOVE NOO              TO INDATA-SW                           
069500            END-IF                                                        
069600        END-IF                                                            
069700     ELSE                                                                 
069800         IF REQU-TIAAMMDD-UPD NOT        NUMERIC                          
069900           MOVE 'TIAAMMDD'  TO RESP-IDELMT-ERROR                          
070000           MOVE '024'           TO RESP-IDMSG-ERROR                       
070100         END-IF                                                           
070200                                                                          
070300         IF REQU-TIHHMM-UPD NOT        NUMERIC                            
070400           MOVE 'TIHHMM'    TO RESP-IDELMT-ERROR                          
070500           MOVE '024'           TO RESP-IDMSG-ERROR                       
070600         END-IF                                                           
070700                                                                          
070800         IF REQU-IDPLKLST-UPD NOT      NUMERIC                            
070900           MOVE 'IDPLKLST2'     TO RESP-IDELMT-ERROR                      
071000           MOVE '024'           TO RESP-IDMSG-ERROR                       
071100         END-IF                                                           
071200                                                                          
071300         MOVE NOO                     TO INDATA-SW                        
071400     END-IF                                                               
071500     .                                                                    
071600     EJECT                                                                
071700 GCA-CHECK-IDPLKLST   SECTION.                                            
071800                                                                          
071900     MOVE +1                   TO INDX                                    
072000     MOVE NOO                  TO IDPLKLST-SW                             
072100     PERFORM UNTIL INDX        >  MAX-INDX OR                             
072200             IDPLKLST-OK                                                  
072300         IF REQU-IDPLKLST-UPD   =  REQU-IDPLKLST-RAD (INDX)               
072400             MOVE YES          TO IDPLKLST-SW                             
072500          ELSE                                                            
072600             ADD +1            TO INDX                                    
072700         END-IF                                                           
072800     END-PERFORM                                                          
072900     .                                                                    
073000     EJECT                                                                
073100 H-UPDATE             SECTION.                                            
073200                                                                          
073300     IF REQU-FLJANEJ            =  'Y'                                    
073400        PERFORM HA-UPDATE-ALL-ORDERPARTS                                  
073500     ELSE                                                                 
073600        PERFORM HB-UPDATE-ONE-PLKLST                                      
073700     END-IF                                                               
073800     .                                                                    
073900     EJECT                                                                
074000 HA-UPDATE-ALL-ORDERPARTS   SECTION.                                      
074100                                                                          
074200     PERFORM IMS-GHU-ORQA-WDQ3-OKVAL                                      
074300     MOVE +1                   TO INDX                                    
074400     PERFORM UNTIL  SEGMENT-MISSING OR END-OF-DATA                        
074500        MOVE W-DARFS                  TO ORQA-ODEL-DARFS                  
074600        MOVE W-DARFS-DATUM            TO ORQA-ODEL-DARFSDAT               
074700        PERFORM IMS-REPL-ORQA                                             
074800        ADD +1                        TO INDX                             
074900        MOVE '001'                    TO RESP-IDMSG-INFO                  
075000        PERFORM IMS-GHN-ORQA-WDQ3                                         
075100     END-PERFORM                                                          
075200     .                                                                    
075300     EJECT                                                                
075400 HB-UPDATE-ONE-PLKLST       SECTION.                                      
075500                                                                          
075600     MOVE REQU-IDPLKLST-UPD        TO W-IDPLKLST                          
075700     PERFORM IMS-GHU-ORQA-WDQ3-KVAL                                       
075800     MOVE W-DARFS                  TO ORQA-ODEL-DARFS                     
075900     MOVE W-DARFS-DATUM            TO ORQA-ODEL-DARFSDAT                  
076000     PERFORM IMS-REPL-ORQA                                                
076100     MOVE '001'                    TO RESP-IDMSG-INFO                     
076200     .                                                                    
076300     EJECT                                                                
076400*    --- DISPATCHER SECTIONS                                              
076500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
076600                                                                          
076700     MOVE 'GETARG'               TO SUB-KDFUNC                            
076800     MOVE 'CARPARTS.LDC.SPLITOFORDERPRC'  TO SUB-ADDISPABS                
076900     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
077000                                                                          
077100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
077200                                                                          
077300     IF SUB-KDRC > 0                                                      
077400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
077500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
077600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
077700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
077800     END-IF                                                               
077900     .                                                                    
078000     SKIP3                                                                
078100 S02-RETURN-RESPONSE SECTION.                                             
078200                                                                          
078300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
078400     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
078500                                                                          
078600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
078700                                                                          
078800     IF SUB-KDRC > 0                                                      
078900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
079000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
079100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
079200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
079300     END-IF                                                               
079400     .                                                                    
079500     EJECT                                                                
079600     SKIP2                                                                
079700 IMS-GU-CSEQ-WDQ2 SECTION.                                                
079800                                                                          
079900     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
080000          DELIMITED BY SIZE INTO SSA1                                     
080100     MOVE '  GE' TO GOOD-STATUSCODES                                      
080200     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA2 SSA1                     
080300     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
080400     PERFORM IMS-STATUSCONTROL                                            
080500     .                                                                    
080600                                                                          
080700 IMS-GNP-WDQ2 SECTION.                                                    
080800                                                                          
080900     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
081000          DELIMITED BY SIZE INTO SSA2                                     
081100     MOVE '  GE' TO GOOD-STATUSCODES                                      
081200     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA2 SSA2                    
081300     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
081400     PERFORM IMS-STATUSCONTROL                                            
081500     .                                                                    
081600                                                                          
081700 IMS-GU-ORQA-WDQ3 SECTION.                                                
081800                                                                          
081900     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
082000                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
082100          DELIMITED BY SIZE INTO SSA1                                     
082200     MOVE '  GE' TO GOOD-STATUSCODES                                      
082300     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA1 SSA1                     
082400     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
082500     PERFORM IMS-STATUSCONTROL                                            
082600     .                                                                    
082700                                                                          
082800                                                                          
082900 IMS-GN-ORQA-WDQ3 SECTION.                                                
083000                                                                          
083100     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
083200                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
083300          DELIMITED BY SIZE INTO SSA2                                     
083400     MOVE '  GE' TO GOOD-STATUSCODES                                      
083500     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA1 SSA2                     
083600     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
083700     PERFORM IMS-STATUSCONTROL                                            
083800     .                                                                    
083900     SKIP2                                                                
084000 IMS-GU-ORQA-WDQ3-KVAL SECTION.                                           
084100                                                                          
084200     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
084300                    '&WDQ301KY<=' W-WDQ301KY-MAX-X                        
084400                    '&IDPLKLST>=' W-IDPLKLST-X ')'                        
084500          DELIMITED BY SIZE INTO SSA2                                     
084600     MOVE '  GE' TO GOOD-STATUSCODES                                      
084700     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA1 SSA2                     
084800     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
084900     PERFORM IMS-STATUSCONTROL                                            
085000     .                                                                    
085100     SKIP2                                                                
085200 IMS-GHU-ORQA-WDQ3-KVAL SECTION.                                          
085300                                                                          
085400     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
085500                    '&WDQ301KY<=' W-WDQ301KY-MAX-X                        
085600                    '&IDPLKLST =' W-IDPLKLST-X ')'                        
085700          DELIMITED BY SIZE INTO SSA1                                     
085800     MOVE '  ' TO GOOD-STATUSCODES                                        
085900     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA1 SSA1                    
086000     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
086100     PERFORM IMS-STATUSCONTROL                                            
086200     .                                                                    
086300     SKIP2                                                                
086400 IMS-GHU-ORQA-WDQ3-OKVAL SECTION.                                         
086500                                                                          
086600     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
086700                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
086800          DELIMITED BY SIZE INTO SSA1                                     
086900     MOVE '  GE' TO GOOD-STATUSCODES                                      
087000     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA1 SSA1                    
087100     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
087200     PERFORM IMS-STATUSCONTROL                                            
087300     .                                                                    
087400     SKIP2                                                                
087500 IMS-GHN-ORQA-WDQ3 SECTION.                                               
087600                                                                          
087700     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
087800                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
087900          DELIMITED BY SIZE INTO SSA2                                     
088000     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
088100     CALL CBLTDLI USING GHN ORQA-PCB DLI-IO-AREA1 SSA2                    
088200     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
088300     PERFORM IMS-STATUSCONTROL                                            
088400     .                                                                    
088500     SKIP2                                                                
088600 IMS-REPL-ORQA SECTION.                                                   
088700                                                                          
088800     MOVE '  ' TO GOOD-STATUSCODES                                        
088900     CALL CBLTDLI USING REPL ORQA-PCB DLI-IO-AREA1                        
089000     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
089100     PERFORM IMS-STATUSCONTROL                                            
089200     .                                                                    
089300     SKIP2                                                                
089400 IMS-GU-ORQA01-M-Q3DSEQ SECTION.                                          
089500     STRING 'WLORQA01(WDQ3DSEQ>=' W-WDQ3DSEQ-MIN-X                        
089600                    '&WDQ3DSEQ<=' W-WDQ3DSEQ-MAX-X ')'                    
089700          DELIMITED BY SIZE INTO SSA1                                     
089800     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
089900     CALL CBLTDLI USING GU ORQAD-PCB DLI-IO-AREA1 SSA1                    
090000     MOVE ORQAD-STATUS-CODE TO STATUS-WS                                  
090100     PERFORM IMS-STATUSCONTROL                                            
090200     .                                                                    
090300     SKIP2                                                                
090400 IMS-GN-ORQA01-M-Q3DSEQ SECTION.                                          
090500     STRING 'WLORQA01(WDQ3DSEQ>=' W-WDQ3DSEQ-MIN-X                        
090600                    '&WDQ3DSEQ<=' W-WDQ3DSEQ-MAX-X ')'                    
090700          DELIMITED BY SIZE INTO SSA1                                     
090800     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
090900     CALL CBLTDLI USING GN ORQAD-PCB DLI-IO-AREA1 SSA1                    
091000     MOVE ORQAD-STATUS-CODE TO STATUS-WS                                  
091100     PERFORM IMS-STATUSCONTROL                                            
091200     .                                                                    
091300     SKIP2                                                                
091400 IMS-STATUSCONTROL SECTION.                                               
091500                                                                          
091600     SET STATUS-IX TO 1                                                   
091700     SEARCH GOOD-STATUS                                                   
091800       AT END CALL FELLOG                                                 
091900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
092000     END-SEARCH                                                           
092100     .                                                                    
