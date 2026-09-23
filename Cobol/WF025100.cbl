000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF025100.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/02/01.                                                
000600 DATE-COMPILED.                                                           
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.FINCUSTLOCATE                                    
001000*    FUNCTION:                                                            
001100*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
001200*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001300*                                                                         
001400*        THE PROGRAM READS   TABLE T01LSEL                                
001500*        THE PROGRAM READS   TABLE T01CUGR                                
001600*        THE PROGRAM READS   TABLE T01COCO                                
001700*        THE PROGRAM READS   TABLE T01FCUS                                
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: WF0251T                                             
002100*        REQUEST:     WZ01REQU                                            
002200*                     WF0251I1                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        RESPONSE:    WZ01RESP                                            
002600*                     WF0251O1                                            
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 DATA DIVISION.                                                           
003100                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003300 77  IDPGM                       PIC X(08)  VALUE 'WF025100'.             
003400                                                                          
003500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003700 77  KDRC-DISPLAY                PIC Z(5).                                
003800                                                                          
003900*    --- CONSTANTS                                                        
004000 77  YES                         PIC X      VALUE 'Y'.                    
004100 77  NOO                         PIC X      VALUE 'N'.                    
004200                                                                          
004300 77  WS-SEARCH                   PIC X      VALUE 'S'.                    
004400 77  WS-CURRENT                  PIC S9(3)  VALUE +001    COMP-3.         
004500 77  WS-COMING                   PIC S9(3)  VALUE +002    COMP-3.         
004600 77  WS-ACTIVE                   PIC X(8)   VALUE '00000000'.             
004700 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
004800 77  WS-ADRESS                   PIC X(50)                                
004900                           VALUE 'CARPARTS.BILLIT.FINCUSTLOCATE'.         
005000 77  WS-PERCENTAGE               PIC X      VALUE '%'.                    
005100 77  WS-KDPARTGR                 PIC X(15)  VALUE SPACE.                  
005200 77  WS-FLRATE                   PIC X(2)   VALUE SPACE.                  
005300 77  WS-KDVALISO                 PIC X(3)   VALUE SPACE.                  
005400                                                                          
005500 77  KEYS-SW                     PIC X      VALUE SPACE.                  
005600     88  KEYS-OK                            VALUE 'Y'.                    
005700     88  KEYS-WRONG                         VALUE 'N'.                    
005800                                                                          
005900*    --- SWITCH WHICH KEY                                                 
006000 01  WHICH-KEY-SW                PIC S9(3)  VALUE ZERO COMP-3.            
006100                                                                          
006200                                                                          
006300*    --- WORK FIELDS                                                      
006400 01  WS-IX-MOD                   PIC S9(4)  VALUE ZERO    BINARY.         
006500 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
006600 01  WS-COUNTER-T01FCUS          PIC S9(7)  VALUE ZERO    COMP-3.         
006700 01  WS-IDALPHA-2                PIC X(2)   VALUE SPACE.                  
006800 01  WS-IDALPHA-3                PIC X(3)   VALUE SPACE.                  
006900 01  WS-IDALPHA-4                PIC X(4)   VALUE SPACE.                  
007000 01  WS-IDALPHA-5                PIC X(5)   VALUE SPACE.                  
007100 01  WS-IDALPHA-6                PIC X(6)   VALUE SPACE.                  
007200 01  WS-IDALPHA-7                PIC X(7)   VALUE SPACE.                  
007300 01  WS-IDALPHA-8                PIC X(8)   VALUE SPACE.                  
007400 01  WS-IDALPHA-9                PIC X(9)   VALUE SPACE.                  
007500 01  WS-IDALPHA-10               PIC X(10)  VALUE SPACE.                  
007600 01  WS-IDALPHA-11               PIC X(11)  VALUE SPACE.                  
007700                                                                          
007800 01  WS-IDALPHA-EDIT.                                                     
007900     03 WS-IDALPHA-OCC11 OCCURS 11 PIC X.                                 
008000                                                                          
008100 01 WS-PARTNR                    PIC S9(3)  VALUE +001 COMP-3.            
008200 01 WS-ALPHA                     PIC S9(3)  VALUE +002 COMP-3.            
008300 01 WS-PREL                      PIC S9(3)  VALUE +003 COMP-3.            
008400 01 WS-LAND                      PIC S9(3)  VALUE +004 COMP-3.            
008500 01 WS-PARTTY                    PIC S9(3)  VALUE +005 COMP-3.            
008600 01 WS-ALPHA-PREL                PIC S9(3)  VALUE +006 COMP-3.            
008700 01 WS-ALPHA-PREL-LAND           PIC S9(3)  VALUE +007 COMP-3.            
008800 01 WS-ALPHA-PREL-LAND-PARTTY    PIC S9(3)  VALUE +008 COMP-3.            
008900 01 WS-ALPHA-PREL-LAND-PARTTY-GR PIC S9(3)  VALUE +009 COMP-3.            
009000 01 WS-ALPHA-PREL-PARTTY         PIC S9(3)  VALUE +010 COMP-3.            
009100 01 WS-ALPHA-PREL-PARTTY-GR      PIC S9(3)  VALUE +011 COMP-3.            
009200 01 WS-ALPHA-LAND                PIC S9(3)  VALUE +012 COMP-3.            
009300 01 WS-ALPHA-LAND-PARTTY         PIC S9(3)  VALUE +013 COMP-3.            
009400 01 WS-ALPHA-LAND-PARTTY-GR      PIC S9(3)  VALUE +014 COMP-3.            
009500 01 WS-ALPHA-PARTTY              PIC S9(3)  VALUE +015 COMP-3.            
009600 01 WS-ALPHA-PARTTY-GR           PIC S9(3)  VALUE +016 COMP-3.            
009700 01 WS-PREL-LAND                 PIC S9(3)  VALUE +017 COMP-3.            
009800 01 WS-PREL-LAND-PARTTY          PIC S9(3)  VALUE +018 COMP-3.            
009900 01 WS-PREL-LAND-PARTTY-GR       PIC S9(3)  VALUE +019 COMP-3.            
010000 01 WS-PREL-PARTTY               PIC S9(3)  VALUE +020 COMP-3.            
010100 01 WS-PREL-PARTTY-GR            PIC S9(3)  VALUE +021 COMP-3.            
010200 01 WS-LAND-PARTTY               PIC S9(3)  VALUE +022 COMP-3.            
010300 01 WS-LAND-PARTTY-GR            PIC S9(3)  VALUE +023 COMP-3.            
010400 01 WS-PARTTY-GR                 PIC S9(3)  VALUE +024 COMP-3.            
010500                                                                          
010600*    --- MAPPING FIELDS                                                   
010700 01  MAP-IDPARTNR-LINE           PIC X(9)   VALUE SPACE.                  
010800 01  MAP-KDSTATUS-LINE           PIC S9(3)  VALUE ZERO    COMP-3.         
010900 01  MAP-BEBET-NAME1-LINE        PIC X(35)  VALUE SPACE.                  
011000 01  MAP-ADBET-STREET-LINE       PIC X(35)  VALUE SPACE.                  
011100 01  MAP-ADBET-BOX-LINE          PIC X(10)  VALUE SPACE.                  
011200 01  MAP-ADBET-PCODE-LINE        PIC X(10)  VALUE SPACE.                  
011300 01  MAP-ADBET-CITY-LINE         PIC X(35)  VALUE SPACE.                  
011400 01  MAP-IDLANDX3-LINE           PIC X(3)   VALUE SPACE.                  
011500                                                                          
011600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
011700 01  GENERAL-SUBPROGRAMS.                                                 
011800     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
011900     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
012000                                                                          
012100*    --- PARAMETERS TO ABEND                                              
012200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012500 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
012600                                                                          
012700 01  MESSAGE-CODES.                                                       
012800     03  ERROR-CODES.                                                     
012900         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
013000         05  ERR-INVALID-FIELD       PIC X(3)   VALUE '023'.              
013100         05  ERR-MUST-BE-NUMERIC     PIC X(3)   VALUE '024'.              
013200         05  ERR-FIELD-NOT-FOUND     PIC X(3)   VALUE '025'.              
013300         05  ERR-MUST-BE-ENTERED     PIC X(3)   VALUE '026'.              
013400         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
013500         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
013600         05  ERR-SYSTEM-ERROR        PIC X(3)   VALUE '099'.              
013700     EJECT                                                                
013800                                                                          
013900*01  -COPY WZ01SUB                                                        
014000     EJECT                                                                
014100*                                                                         
014200 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
014300 01  REQU-AREA.                                                           
014400*    03 -COPY WZ01REQU                                                    
014500*    03 -COPY WF0251I1                                                    
014600     EJECT                                                                
014700                                                                          
014800 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
014900 01  RESP-AREA.                                                           
015000*    03 -COPY WZ01RESP                                                    
015100*    03 -COPY WF0251O1                                                    
015200     EJECT                                                                
015300                                                                          
015400 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
015500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
015600                                                                          
015700 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
015800 01  DB2-WS.                                                              
015900     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
016000         88  CURSOR-OK                      VALUE 000.                    
016100         88  LINES-FOUND                    VALUE 000.                    
016200         88  LINES-MISSING                  VALUE 100.                    
016300         88  RESOURCE-WRONG                 VALUE 904.                    
016400     03  GOOD-SQLCODECODES.                                               
016500         05  GOOD-SQLCODE OCCURS 5                                        
016600             INDEXED BY SQLCODE-IX PIC 9(3).                              
016700                                                                          
016800 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
016900                                                                          
017000*01  -COPY T01LSEL -PRE T01LSEL-                                          
017100     EJECT                                                                
017200                                                                          
017300 01  FILLER                      PIC X(16)   VALUE 'T01CUGR-AREA'.        
017400                                                                          
017500*01  -COPY T01CUGR -PRE T01CUGR-                                          
017600                                                                          
017700 01  FILLER                      PIC X(16)   VALUE 'T01COCO-AREA'.        
017800                                                                          
017900*01  -COPY T01COCO -PRE T01COCO-                                          
018000     EJECT                                                                
018100 01  FILLER                      PIC X(16)   VALUE 'T01FCUS-AREA'.        
018200                                                                          
018300*01  -COPY T01FCUS -PRE T01FCUS-                                          
018400     EJECT                                                                
018500       EXEC SQL INCLUDE T01LSEL END-EXEC.                                 
018600     EJECT                                                                
018700       EXEC SQL INCLUDE T01CUGR END-EXEC.                                 
018800     EJECT                                                                
018900       EXEC SQL INCLUDE T01COCO END-EXEC.                                 
019000     EJECT                                                                
019100       EXEC SQL INCLUDE T01FCUS END-EXEC.                                 
019200     EJECT                                                                
019300 LINKAGE SECTION.                                                         
019400                                                                          
019500 PROCEDURE DIVISION.                                                      
019600 MAIN SECTION.                                                            
019700                                                                          
019800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
019900     IF SUB-KDRC = ZERO                                                   
020000       PERFORM A-INIT                                                     
020100       PERFORM B-CHECK-KEYS                                               
020200       IF KEYS-OK                                                         
020300         PERFORM F-READ-SHOW-INFO                                         
020400       END-IF                                                             
020500       PERFORM S02-RETURN-RESPONSE                                        
020600     END-IF                                                               
020700                                                                          
020800     MOVE ZERO TO RETURN-CODE                                             
020900     GOBACK                                                               
021000     .                                                                    
021100 A-INIT SECTION.                                                          
021200                                                                          
021300     INITIALIZE GOOD-SQLCODECODES                                         
021400     MOVE ALL '+' TO RESP-AREA                                            
021500     MOVE ZERO TO RESP-KVRADER                                            
021600     MOVE SPACE TO RESP-IDMSG-ERROR                                       
021700     MOVE SPACE TO RESP-IDMSG-INFO                                        
021800     MOVE SPACE TO RESP-IDELMT-ERROR                                      
021900     .                                                                    
022000*** - CHECK REQUESTED SEARCHING KEYS AND COMPULSORY FIELDS                
022100 B-CHECK-KEYS SECTION.                                                    
022200                                                                          
022300     MOVE YES TO KEYS-SW                                                  
022400                                                                          
022500     IF REQU-IDMSGVER NUMERIC                                             
022600       IF REQU-KDPGMACT = WS-SEARCH                                       
022700       AND (REQU-IDLEGSEL-KEY > SPACE AND NOT = ALL '+')                  
022800         IF  (REQU-IDPARTNR-KEY = SPACE OR = ALL '+')                     
022900         AND (REQU-IDALPHA-KEY  = SPACE OR = ALL '+')                     
023000         AND (REQU-FLPREL-KEY   = SPACE OR = ALL '+')                     
023100         AND (REQU-KDPARTTY-KEY = SPACE OR = ALL '+')                     
023200         AND (REQU-IDLANDX3-KEY = SPACE OR = ALL '+')                     
023300           MOVE NOO TO KEYS-SW                                            
023400         ELSE                                                             
023500           PERFORM BA-CHECK-KEY-RELATION                                  
023600         END-IF                                                           
023700       ELSE                                                               
023800         MOVE NOO TO KEYS-SW                                              
023900       END-IF                                                             
024000     ELSE                                                                 
024100       MOVE NOO TO KEYS-SW                                                
024200     END-IF                                                               
024300                                                                          
024400     IF KEYS-WRONG                                                        
024500        MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                          
024600                                                                          
024700        IF (REQU-KDPARTGR-KEY > SPACE AND NOT = ALL '+')                  
024800        AND (REQU-KDPARTTY-KEY = SPACE OR = ALL '+')                      
024900        AND (REQU-IDPARTNR-KEY = SPACE OR = ALL '+')                      
025000           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
025100           MOVE 'KDPARTTY'          TO RESP-IDELMT-ERROR                  
025200        END-IF                                                            
025300                                                                          
025400        IF REQU-IDMSGVER NUMERIC                                          
025500           CONTINUE                                                       
025600        ELSE                                                              
025700           MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                      
025800           MOVE 'IDMSGVER'       TO RESP-IDELMT-ERROR                     
025900        END-IF                                                            
026000                                                                          
026100        IF REQU-KDPGMACT = WS-SEARCH                                      
026200           CONTINUE                                                       
026300        ELSE                                                              
026400           MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                      
026500           MOVE 'KDPGMACT'       TO RESP-IDELMT-ERROR                     
026600        END-IF                                                            
026700     ELSE                                                                 
026800        IF (REQU-IDUSER > SPACE AND NOT = ALL '+')                        
026900           PERFORM BB-WHICH-REQU-SEARCH-KEY                               
027000        ELSE                                                              
027100           MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                      
027200           MOVE 'IDUSER'         TO RESP-IDELMT-ERROR                     
027300        END-IF                                                            
027400     END-IF                                                               
027500     .                                                                    
027600*** - CHECK RELATION BETWEEN REQUSTED KEYS                                
027700 BA-CHECK-KEY-RELATION SECTION.                                           
027800                                                                          
027900     IF REQU-IDPARTNR-KEY > SPACE AND NOT = ALL '+'                       
028000       IF  (REQU-IDALPHA-KEY  = SPACE OR = ALL '+')                       
028100       AND (REQU-FLPREL-KEY   = SPACE OR = ALL '+')                       
028200       AND (REQU-KDPARTTY-KEY = SPACE OR = ALL '+')                       
028300       AND (REQU-KDPARTGR-KEY = SPACE OR = ALL '+')                       
028400       AND (REQU-IDLANDX3-KEY = SPACE OR = ALL '+')                       
028500         CONTINUE                                                         
028600       ELSE                                                               
028700         MOVE NOO TO KEYS-SW                                              
028800       END-IF                                                             
028900     END-IF                                                               
029000                                                                          
029100     IF (REQU-KDPARTGR-KEY > SPACE AND NOT = ALL '+')                     
029200       AND (REQU-KDPARTTY-KEY = SPACE OR = ALL '+')                       
029300         MOVE NOO TO KEYS-SW                                              
029400     END-IF                                                               
029500                                                                          
029600     IF (REQU-FLPREL-KEY = YES OR = SPACE OR = ALL '+')                   
029700       CONTINUE                                                           
029800     ELSE                                                                 
029900       MOVE NOO TO KEYS-SW                                                
030000     END-IF                                                               
030100     .                                                                    
030200*** - CHECK WHICH REQUESTED SEARCH KEY                                    
030300 BB-WHICH-REQU-SEARCH-KEY SECTION.                                        
030400                                                                          
030500     IF (REQU-IDPARTNR-KEY > SPACE                                        
030600     AND REQU-IDPARTNR-KEY NOT = ALL '+')                                 
030700       MOVE WS-PARTNR TO WHICH-KEY-SW                                     
030800     ELSE                                                                 
030900       IF (REQU-IDALPHA-KEY > SPACE                                       
031000       AND REQU-IDALPHA-KEY NOT = ALL '+')                                
031100         PERFORM BBA-IDALPHA                                              
031200       ELSE                                                               
031300         IF REQU-FLPREL-KEY = YES                                         
031400           PERFORM BBB-FLPREL                                             
031500         ELSE                                                             
031600           IF (REQU-IDLANDX3-KEY > SPACE                                  
031700           AND REQU-IDLANDX3-KEY NOT = ALL '+')                           
031800             PERFORM BBC-IDLANDX3                                         
031900           ELSE                                                           
032000             IF (REQU-KDPARTGR-KEY > SPACE                                
032100             AND REQU-KDPARTGR-KEY NOT = ALL '+')                         
032200               MOVE WS-PARTTY-GR TO WHICH-KEY-SW                          
032300             ELSE                                                         
032400               MOVE WS-PARTTY TO WHICH-KEY-SW                             
032500             END-IF                                                       
032600           END-IF                                                         
032700         END-IF                                                           
032800       END-IF                                                             
032900     END-IF                                                               
033000     .                                                                    
033100 BBA-IDALPHA SECTION.                                                     
033200                                                                          
033300     IF REQU-FLPREL-KEY = YES                                             
033400       IF (REQU-IDLANDX3-KEY > SPACE                                      
033500       AND REQU-IDLANDX3-KEY NOT = ALL '+')                               
033600         IF (REQU-KDPARTTY-KEY > SPACE AND NOT = ALL '+')                 
033700           IF (REQU-KDPARTGR-KEY > SPACE AND NOT = ALL '+')               
033800             MOVE WS-ALPHA-PREL-LAND-PARTTY-GR TO WHICH-KEY-SW            
033900           ELSE                                                           
034000             MOVE WS-ALPHA-PREL-LAND-PARTTY TO WHICH-KEY-SW               
034100           END-IF                                                         
034200         ELSE                                                             
034300           MOVE WS-ALPHA-PREL-LAND TO WHICH-KEY-SW                        
034400         END-IF                                                           
034500       ELSE                                                               
034600         IF (REQU-KDPARTTY-KEY > SPACE                                    
034700         AND REQU-KDPARTTY-KEY NOT = ALL '+')                             
034800           IF (REQU-KDPARTGR-KEY > SPACE                                  
034900           AND REQU-KDPARTGR-KEY NOT = ALL '+')                           
035000             MOVE WS-ALPHA-PREL-PARTTY-GR TO WHICH-KEY-SW                 
035100           ELSE                                                           
035200             MOVE WS-ALPHA-PREL-PARTTY TO WHICH-KEY-SW                    
035300           END-IF                                                         
035400         ELSE                                                             
035500           MOVE WS-ALPHA-PREL TO WHICH-KEY-SW                             
035600         END-IF                                                           
035700       END-IF                                                             
035800     ELSE                                                                 
035900       IF (REQU-IDLANDX3-KEY > SPACE                                      
036000       AND REQU-IDLANDX3-KEY NOT = ALL '+')                               
036100         IF (REQU-KDPARTTY-KEY > SPACE                                    
036200         AND REQU-KDPARTTY-KEY NOT = ALL '+')                             
036300           IF (REQU-KDPARTGR-KEY > SPACE                                  
036400           AND REQU-KDPARTGR-KEY NOT = ALL '+')                           
036500             MOVE WS-ALPHA-LAND-PARTTY-GR TO WHICH-KEY-SW                 
036600           ELSE                                                           
036700             MOVE WS-ALPHA-LAND-PARTTY TO WHICH-KEY-SW                    
036800           END-IF                                                         
036900         ELSE                                                             
037000           MOVE WS-ALPHA-LAND TO WHICH-KEY-SW                             
037100         END-IF                                                           
037200       ELSE                                                               
037300         IF (REQU-KDPARTTY-KEY > SPACE                                    
037400         AND REQU-KDPARTTY-KEY NOT = ALL '+')                             
037500           IF (REQU-KDPARTGR-KEY > SPACE                                  
037600           AND REQU-KDPARTGR-KEY NOT = ALL '+')                           
037700             MOVE WS-ALPHA-PARTTY-GR TO WHICH-KEY-SW                      
037800           ELSE                                                           
037900             MOVE WS-ALPHA-PARTTY TO WHICH-KEY-SW                         
038000           END-IF                                                         
038100         ELSE                                                             
038200           MOVE WS-ALPHA TO WHICH-KEY-SW                                  
038300         END-IF                                                           
038400       END-IF                                                             
038500     END-IF                                                               
038600     .                                                                    
038700 BBB-FLPREL SECTION.                                                      
038800                                                                          
038900     IF (REQU-IDLANDX3-KEY > SPACE                                        
039000     AND REQU-IDLANDX3-KEY NOT = ALL '+')                                 
039100       IF (REQU-KDPARTTY-KEY > SPACE AND NOT = ALL '+')                   
039200         IF (REQU-KDPARTGR-KEY > SPACE AND NOT = ALL '+')                 
039300           MOVE WS-PREL-LAND-PARTTY-GR TO WHICH-KEY-SW                    
039400         ELSE                                                             
039500           MOVE WS-PREL-LAND-PARTTY TO WHICH-KEY-SW                       
039600         END-IF                                                           
039700       ELSE                                                               
039800         MOVE WS-PREL-LAND TO WHICH-KEY-SW                                
039900       END-IF                                                             
040000     ELSE                                                                 
040100       IF (REQU-KDPARTTY-KEY > SPACE                                      
040200       AND REQU-KDPARTTY-KEY NOT = ALL '+')                               
040300         IF (REQU-KDPARTGR-KEY > SPACE                                    
040400         AND REQU-KDPARTGR-KEY NOT = ALL '+')                             
040500           MOVE WS-PREL-PARTTY-GR TO WHICH-KEY-SW                         
040600         ELSE                                                             
040700           MOVE WS-PREL-PARTTY TO WHICH-KEY-SW                            
040800         END-IF                                                           
040900       ELSE                                                               
041000         MOVE WS-PREL TO WHICH-KEY-SW                                     
041100       END-IF                                                             
041200     END-IF                                                               
041300     .                                                                    
041400 BBC-IDLANDX3 SECTION.                                                    
041500                                                                          
041600     IF (REQU-KDPARTTY-KEY > SPACE                                        
041700     AND REQU-KDPARTTY-KEY NOT = ALL '+')                                 
041800       IF (REQU-KDPARTGR-KEY > SPACE                                      
041900       AND REQU-KDPARTGR-KEY NOT = ALL '+')                               
042000         MOVE WS-LAND-PARTTY-GR TO WHICH-KEY-SW                           
042100       ELSE                                                               
042200         MOVE WS-LAND-PARTTY TO WHICH-KEY-SW                              
042300       END-IF                                                             
042400     ELSE                                                                 
042500       MOVE WS-LAND TO WHICH-KEY-SW                                       
042600     END-IF                                                               
042700     .                                                                    
042800*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
042900 F-READ-SHOW-INFO SECTION.                                                
043000                                                                          
043100     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
043200     MOVE REQU-IDPARTNR-KEY  TO RESP-IDPARTNR-KEY                         
043300     MOVE REQU-IDALPHA-KEY   TO RESP-IDALPHA-KEY                          
043400     MOVE REQU-FLPREL-KEY    TO RESP-FLPREL-KEY                           
043500     MOVE REQU-KDPARTTY-KEY  TO RESP-KDPARTTY-KEY                         
043600     MOVE REQU-KDPARTGR-KEY  TO RESP-KDPARTGR-KEY                         
043700     MOVE REQU-IDLANDX3-KEY  TO RESP-IDLANDX3-KEY                         
043800     MOVE REQU-IDMSGVER      TO RESP-IDMSGVER                             
043900                                                                          
044000     PERFORM DB2-SELECT-T01LSEL-TAB                                       
044100     IF LINES-FOUND                                                       
044200        MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                        
044300     ELSE                                                                 
044400        MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                      
044500        MOVE 'IDLEGSEL'          TO RESP-IDELMT-ERROR                     
044600        MOVE SPACE               TO RESP-BELEGRAD-1                       
044700     END-IF                                                               
044800                                                                          
044900     IF RESP-IDMSG-ERROR = SPACE                                          
045000        IF (REQU-KDPARTGR-KEY > SPACE AND NOT = ALL '+')                  
045100           PERFORM DB2-SELECT-T01CUGR-TAB                                 
045200           IF LINES-FOUND                                                 
045300              CONTINUE                                                    
045400           ELSE                                                           
045500              MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                
045600              MOVE 'KDPARTTYKDPARTGR'  TO RESP-IDELMT-ERROR               
045700           END-IF                                                         
045800        END-IF                                                            
045900     END-IF                                                               
046000                                                                          
046100     IF RESP-IDMSG-ERROR = SPACE                                          
046200        IF (REQU-IDLANDX3-KEY > SPACE AND NOT = ALL '+')                  
046300           PERFORM DB2-SELECT-T01COCO-TAB                                 
046400           IF LINES-FOUND                                                 
046500              CONTINUE                                                    
046600           ELSE                                                           
046700              MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                
046800              MOVE 'IDLANDX2'  TO RESP-IDELMT-ERROR                       
046900           END-IF                                                         
047000        END-IF                                                            
047100     END-IF                                                               
047200                                                                          
047300     IF RESP-IDMSG-ERROR = SPACE                                          
047400        PERFORM FA-READ-BASICDATA                                         
047500     END-IF                                                               
047600     .                                                                    
047700 FA-READ-BASICDATA SECTION.                                               
047800                                                                          
047900     MOVE ZERO TO WS-COUNTER-T01FCUS                                      
048000                                                                          
048100     EVALUATE WHICH-KEY-SW                                                
048200       WHEN WS-PARTNR                                                     
048300         PERFORM FAA-PARTNR                                               
048400       WHEN WS-ALPHA                                                      
048500         PERFORM FAB-ALPHA                                                
048600       WHEN WS-PREL                                                       
048700         PERFORM FAC-PREL                                                 
048800       WHEN WS-LAND                                                       
048900         PERFORM FAD-LAND                                                 
049000       WHEN WS-PARTTY                                                     
049100         PERFORM FAE-PARTTY                                               
049200       WHEN WS-ALPHA-PREL                                                 
049300         PERFORM FAF-ALPHA-PREL                                           
049400       WHEN WS-ALPHA-PREL-LAND                                            
049500         PERFORM FAG-ALPHA-PREL-LAND                                      
049600       WHEN WS-ALPHA-PREL-LAND-PARTTY                                     
049700         PERFORM FAH-ALPHA-PREL-LAND-PARTTY                               
049800       WHEN WS-ALPHA-PREL-LAND-PARTTY-GR                                  
049900         PERFORM FAI-ALPHA-PREL-LAND-PARTTY-GR                            
050000       WHEN WS-ALPHA-PREL-PARTTY                                          
050100         PERFORM FAJ-ALPHA-PREL-PARTTY                                    
050200       WHEN WS-ALPHA-PREL-PARTTY-GR                                       
050300         PERFORM FAK-ALPHA-PREL-PARTTY-GR                                 
050400       WHEN WS-ALPHA-LAND                                                 
050500         PERFORM FAL-ALPHA-LAND                                           
050600       WHEN WS-ALPHA-LAND-PARTTY                                          
050700         PERFORM FAM-ALPHA-LAND-PARTTY                                    
050800       WHEN WS-ALPHA-LAND-PARTTY-GR                                       
050900         PERFORM FAN-ALPHA-LAND-PARTTY-GR                                 
051000       WHEN WS-ALPHA-PARTTY                                               
051100         PERFORM FAO-ALPHA-PARTTY                                         
051200       WHEN WS-ALPHA-PARTTY-GR                                            
051300         PERFORM FAP-ALPHA-PARTTY-GR                                      
051400       WHEN WS-PREL-LAND                                                  
051500         PERFORM FAQ-PREL-LAND                                            
051600       WHEN WS-PREL-LAND-PARTTY                                           
051700         PERFORM FAR-PREL-LAND-PARTTY                                     
051800       WHEN WS-PREL-LAND-PARTTY-GR                                        
051900         PERFORM FAS-PREL-LAND-PARTTY-GR                                  
052000       WHEN WS-PREL-PARTTY                                                
052100         PERFORM FAT-PREL-PARTTY                                          
052200       WHEN WS-PREL-PARTTY-GR                                             
052300         PERFORM FAU-PREL-PARTTY-GR                                       
052400       WHEN WS-LAND-PARTTY                                                
052500         PERFORM FAV-LAND-PARTTY                                          
052600       WHEN WS-LAND-PARTTY-GR                                             
052700         PERFORM FAW-LAND-PARTTY-GR                                       
052800       WHEN WS-PARTTY-GR                                                  
052900         PERFORM FAX-PARTTY-GR                                            
053000     END-EVALUATE                                                         
053100                                                                          
053200     MOVE WS-IX TO RESP-KVRADER                                           
053300     .                                                                    
053400*** - HANDLE SEARCH-KEY IDPARTNR                                          
053500 FAA-PARTNR SECTION.                                                      
053600                                                                          
053700     PERFORM DB2-COUNT-CRS-1                                              
053800                                                                          
053900     IF WS-COUNTER-T01FCUS = ZERO                                         
054000        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
054100     END-IF                                                               
054200                                                                          
054300     IF RESP-IDMSG-ERROR = SPACE                                          
054400        PERFORM DB2-DCL-OPN-T01FCUS-CRS-1                                 
054500        PERFORM DB2-FETCH-T01FCUS-CRS-1                                   
054600        MOVE ZERO TO WS-IX                                                
054700                                                                          
054800        PERFORM UNTIL LINES-MISSING                                       
054900           PERFORM S03-MOVE-TO-RESPOND                                    
055000           PERFORM DB2-FETCH-T01FCUS-CRS-1                                
055100        END-PERFORM                                                       
055200                                                                          
055300        PERFORM DB2-CLOSE-T01FCUS-CRS-1                                   
055400     END-IF                                                               
055500     .                                                                    
055600*** - HANDLE SEARCH-KEY IDALPHA                                           
055700 FAB-ALPHA SECTION.                                                       
055800                                                                          
055900     PERFORM S04-EDIT-IDALPHA-KEY                                         
056000     PERFORM DB2-COUNT-CRS-2                                              
056100                                                                          
056200     IF WS-COUNTER-T01FCUS = ZERO                                         
056300        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
056400     ELSE                                                                 
056500        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
056600           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
056700        END-IF                                                            
056800     END-IF                                                               
056900                                                                          
057000     IF RESP-IDMSG-ERROR = SPACE                                          
057100        PERFORM DB2-DCL-OPN-T01FCUS-CRS-2                                 
057200        PERFORM DB2-FETCH-T01FCUS-CRS-2                                   
057300        MOVE ZERO TO WS-IX                                                
057400                                                                          
057500        PERFORM UNTIL LINES-MISSING                                       
057600           PERFORM S03-MOVE-TO-RESPOND                                    
057700           PERFORM DB2-FETCH-T01FCUS-CRS-2                                
057800        END-PERFORM                                                       
057900                                                                          
058000        PERFORM DB2-CLOSE-T01FCUS-CRS-2                                   
058100     END-IF                                                               
058200     .                                                                    
058300*** - HANDLE SEARCH-KEY FLPREL                                            
058400 FAC-PREL SECTION.                                                        
058500                                                                          
058600     PERFORM DB2-COUNT-CRS-3                                              
058700                                                                          
058800     IF WS-COUNTER-T01FCUS = ZERO                                         
058900        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
059000     ELSE                                                                 
059100        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
059200           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
059300        END-IF                                                            
059400     END-IF                                                               
059500                                                                          
059600     IF RESP-IDMSG-ERROR = SPACE                                          
059700        PERFORM DB2-DCL-OPN-T01FCUS-CRS-3                                 
059800        PERFORM DB2-FETCH-T01FCUS-CRS-3                                   
059900        MOVE ZERO TO WS-IX                                                
060000                                                                          
060100        PERFORM UNTIL LINES-MISSING                                       
060200           PERFORM S03-MOVE-TO-RESPOND                                    
060300           PERFORM DB2-FETCH-T01FCUS-CRS-3                                
060400        END-PERFORM                                                       
060500                                                                          
060600        PERFORM DB2-CLOSE-T01FCUS-CRS-3                                   
060700     END-IF                                                               
060800     .                                                                    
060900*** - HANDLE SEARCH-KEY IDLANDX3                                          
061000 FAD-LAND SECTION.                                                        
061100                                                                          
061200     PERFORM DB2-COUNT-CRS-4                                              
061300                                                                          
061400     IF WS-COUNTER-T01FCUS = ZERO                                         
061500        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
061600     ELSE                                                                 
061700        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
061800           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
061900        END-IF                                                            
062000     END-IF                                                               
062100                                                                          
062200     IF RESP-IDMSG-ERROR = SPACE                                          
062300        PERFORM DB2-DCL-OPN-T01FCUS-CRS-4                                 
062400        PERFORM DB2-FETCH-T01FCUS-CRS-4                                   
062500        MOVE ZERO TO WS-IX                                                
062600                                                                          
062700        PERFORM UNTIL LINES-MISSING                                       
062800           PERFORM S03-MOVE-TO-RESPOND                                    
062900           PERFORM DB2-FETCH-T01FCUS-CRS-4                                
063000        END-PERFORM                                                       
063100                                                                          
063200        PERFORM DB2-CLOSE-T01FCUS-CRS-4                                   
063300     END-IF                                                               
063400     .                                                                    
063500*** - HANDLE SEARCH-KEY KDPARTTY                                          
063600 FAE-PARTTY SECTION.                                                      
063700                                                                          
063800     PERFORM DB2-COUNT-CRS-5                                              
063900                                                                          
064000     IF WS-COUNTER-T01FCUS = ZERO                                         
064100        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
064200     ELSE                                                                 
064300        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
064400           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
064500        END-IF                                                            
064600     END-IF                                                               
064700                                                                          
064800     IF RESP-IDMSG-ERROR = SPACE                                          
064900        PERFORM DB2-DCL-OPN-T01FCUS-CRS-5                                 
065000        PERFORM DB2-FETCH-T01FCUS-CRS-5                                   
065100        MOVE ZERO TO WS-IX                                                
065200                                                                          
065300        PERFORM UNTIL LINES-MISSING                                       
065400           PERFORM S03-MOVE-TO-RESPOND                                    
065500           PERFORM DB2-FETCH-T01FCUS-CRS-5                                
065600        END-PERFORM                                                       
065700                                                                          
065800        PERFORM DB2-CLOSE-T01FCUS-CRS-5                                   
065900     END-IF                                                               
066000     .                                                                    
066100*** - HANDLE SEARCH-KEYS IDALPHA AND FLPREL                               
066200 FAF-ALPHA-PREL SECTION.                                                  
066300                                                                          
066400     PERFORM S04-EDIT-IDALPHA-KEY                                         
066500     PERFORM DB2-COUNT-CRS-6                                              
066600                                                                          
066700     IF WS-COUNTER-T01FCUS = ZERO                                         
066800        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
066900     ELSE                                                                 
067000        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
067100           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
067200        END-IF                                                            
067300     END-IF                                                               
067400                                                                          
067500     IF RESP-IDMSG-ERROR = SPACE                                          
067600        PERFORM DB2-DCL-OPN-T01FCUS-CRS-6                                 
067700        PERFORM DB2-FETCH-T01FCUS-CRS-6                                   
067800        MOVE ZERO TO WS-IX                                                
067900                                                                          
068000        PERFORM UNTIL LINES-MISSING                                       
068100           PERFORM S03-MOVE-TO-RESPOND                                    
068200           PERFORM DB2-FETCH-T01FCUS-CRS-6                                
068300        END-PERFORM                                                       
068400                                                                          
068500        PERFORM DB2-CLOSE-T01FCUS-CRS-6                                   
068600     END-IF                                                               
068700     .                                                                    
068800*** - HANDLE SEARCH-KEYS IDALPHA ,FLPREL AND IDLANDX3                     
068900 FAG-ALPHA-PREL-LAND SECTION.                                             
069000                                                                          
069100     PERFORM S04-EDIT-IDALPHA-KEY                                         
069200     PERFORM DB2-COUNT-CRS-7                                              
069300                                                                          
069400     IF WS-COUNTER-T01FCUS = ZERO                                         
069500        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
069600     ELSE                                                                 
069700        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
069800           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
069900        END-IF                                                            
070000     END-IF                                                               
070100                                                                          
070200     IF RESP-IDMSG-ERROR = SPACE                                          
070300        PERFORM DB2-DCL-OPN-T01FCUS-CRS-7                                 
070400        PERFORM DB2-FETCH-T01FCUS-CRS-7                                   
070500        MOVE ZERO TO WS-IX                                                
070600                                                                          
070700        PERFORM UNTIL LINES-MISSING                                       
070800           PERFORM S03-MOVE-TO-RESPOND                                    
070900           PERFORM DB2-FETCH-T01FCUS-CRS-7                                
071000        END-PERFORM                                                       
071100                                                                          
071200        PERFORM DB2-CLOSE-T01FCUS-CRS-7                                   
071300     END-IF                                                               
071400     .                                                                    
071500*** - HANDLE SEARCH-KEYS IDALPHA ,FLPREL,KDPARTTY AND IDLANDX3            
071600 FAH-ALPHA-PREL-LAND-PARTTY SECTION.                                      
071700                                                                          
071800     PERFORM S04-EDIT-IDALPHA-KEY                                         
071900     PERFORM DB2-COUNT-CRS-8                                              
072000                                                                          
072100     IF WS-COUNTER-T01FCUS = ZERO                                         
072200        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
072300     ELSE                                                                 
072400        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
072500           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
072600        END-IF                                                            
072700     END-IF                                                               
072800                                                                          
072900     IF RESP-IDMSG-ERROR = SPACE                                          
073000        PERFORM DB2-DCL-OPN-T01FCUS-CRS-8                                 
073100        PERFORM DB2-FETCH-T01FCUS-CRS-8                                   
073200        MOVE ZERO TO WS-IX                                                
073300                                                                          
073400        PERFORM UNTIL LINES-MISSING                                       
073500           PERFORM S03-MOVE-TO-RESPOND                                    
073600           PERFORM DB2-FETCH-T01FCUS-CRS-8                                
073700        END-PERFORM                                                       
073800                                                                          
073900        PERFORM DB2-CLOSE-T01FCUS-CRS-8                                   
074000     END-IF                                                               
074100     .                                                                    
074200*** - HANDLE SEARCH-KEYS IDALPHA ,FLPREL,KDPARTTY, KDPARTGR               
074300***   AND IDLANDX3                                                        
074400 FAI-ALPHA-PREL-LAND-PARTTY-GR SECTION.                                   
074500                                                                          
074600     PERFORM S04-EDIT-IDALPHA-KEY                                         
074700     PERFORM DB2-COUNT-CRS-9                                              
074800                                                                          
074900     IF WS-COUNTER-T01FCUS = ZERO                                         
075000        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
075100     ELSE                                                                 
075200        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
075300           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
075400        END-IF                                                            
075500     END-IF                                                               
075600                                                                          
075700     IF RESP-IDMSG-ERROR = SPACE                                          
075800        PERFORM DB2-DCL-OPN-T01FCUS-CRS-9                                 
075900        PERFORM DB2-FETCH-T01FCUS-CRS-9                                   
076000        MOVE ZERO TO WS-IX                                                
076100                                                                          
076200        PERFORM UNTIL LINES-MISSING                                       
076300           PERFORM S03-MOVE-TO-RESPOND                                    
076400           PERFORM DB2-FETCH-T01FCUS-CRS-9                                
076500        END-PERFORM                                                       
076600                                                                          
076700        PERFORM DB2-CLOSE-T01FCUS-CRS-9                                   
076800     END-IF                                                               
076900     .                                                                    
077000*** - HANDLE SEARCH-KEYS IDALPHA ,FLPREL AND KDPARTTY                     
077100 FAJ-ALPHA-PREL-PARTTY SECTION.                                           
077200                                                                          
077300     PERFORM S04-EDIT-IDALPHA-KEY                                         
077400     PERFORM DB2-COUNT-CRS-10                                             
077500                                                                          
077600     IF WS-COUNTER-T01FCUS = ZERO                                         
077700        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
077800     ELSE                                                                 
077900        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
078000           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
078100        END-IF                                                            
078200     END-IF                                                               
078300                                                                          
078400     IF RESP-IDMSG-ERROR = SPACE                                          
078500        PERFORM DB2-DCL-OPN-T01FCUS-CRS-10                                
078600        PERFORM DB2-FETCH-T01FCUS-CRS-10                                  
078700        MOVE ZERO TO WS-IX                                                
078800                                                                          
078900        PERFORM UNTIL LINES-MISSING                                       
079000           PERFORM S03-MOVE-TO-RESPOND                                    
079100           PERFORM DB2-FETCH-T01FCUS-CRS-10                               
079200        END-PERFORM                                                       
079300                                                                          
079400        PERFORM DB2-CLOSE-T01FCUS-CRS-10                                  
079500     END-IF                                                               
079600     .                                                                    
079700*** - HANDLE SEARCH-KEYS IDALPHA, FLPREL, KDPARTTY AND KDPARTGR           
079800 FAK-ALPHA-PREL-PARTTY-GR SECTION.                                        
079900                                                                          
080000     PERFORM S04-EDIT-IDALPHA-KEY                                         
080100     PERFORM DB2-COUNT-CRS-11                                             
080200                                                                          
080300     IF WS-COUNTER-T01FCUS = ZERO                                         
080400        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
080500     ELSE                                                                 
080600        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
080700           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
080800        END-IF                                                            
080900     END-IF                                                               
081000                                                                          
081100     IF RESP-IDMSG-ERROR = SPACE                                          
081200        PERFORM DB2-DCL-OPN-T01FCUS-CRS-11                                
081300        PERFORM DB2-FETCH-T01FCUS-CRS-11                                  
081400        MOVE ZERO TO WS-IX                                                
081500                                                                          
081600        PERFORM UNTIL LINES-MISSING                                       
081700           PERFORM S03-MOVE-TO-RESPOND                                    
081800           PERFORM DB2-FETCH-T01FCUS-CRS-11                               
081900        END-PERFORM                                                       
082000                                                                          
082100        PERFORM DB2-CLOSE-T01FCUS-CRS-11                                  
082200     END-IF                                                               
082300     .                                                                    
082400*** - HANDLE SEARCH-KEYS IDALPHA AND IDLANDX3                             
082500 FAL-ALPHA-LAND SECTION.                                                  
082600                                                                          
082700     PERFORM S04-EDIT-IDALPHA-KEY                                         
082800     PERFORM DB2-COUNT-CRS-12                                             
082900                                                                          
083000     IF WS-COUNTER-T01FCUS = ZERO                                         
083100        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
083200     ELSE                                                                 
083300        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
083400           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
083500        END-IF                                                            
083600     END-IF                                                               
083700                                                                          
083800     IF RESP-IDMSG-ERROR = SPACE                                          
083900        PERFORM DB2-DCL-OPN-T01FCUS-CRS-12                                
084000        PERFORM DB2-FETCH-T01FCUS-CRS-12                                  
084100        MOVE ZERO TO WS-IX                                                
084200                                                                          
084300        PERFORM UNTIL LINES-MISSING                                       
084400           PERFORM S03-MOVE-TO-RESPOND                                    
084500           PERFORM DB2-FETCH-T01FCUS-CRS-12                               
084600        END-PERFORM                                                       
084700                                                                          
084800        PERFORM DB2-CLOSE-T01FCUS-CRS-12                                  
084900     END-IF                                                               
085000     .                                                                    
085100*** - HANDLE SEARCH-KEYS IDALPHA ,KDPARTTY AND IDLANDX3                   
085200 FAM-ALPHA-LAND-PARTTY SECTION.                                           
085300                                                                          
085400     PERFORM S04-EDIT-IDALPHA-KEY                                         
085500     PERFORM DB2-COUNT-CRS-13                                             
085600                                                                          
085700     IF WS-COUNTER-T01FCUS = ZERO                                         
085800        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
085900     ELSE                                                                 
086000        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
086100           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
086200        END-IF                                                            
086300     END-IF                                                               
086400                                                                          
086500     IF RESP-IDMSG-ERROR = SPACE                                          
086600        PERFORM DB2-DCL-OPN-T01FCUS-CRS-13                                
086700        PERFORM DB2-FETCH-T01FCUS-CRS-13                                  
086800        MOVE ZERO TO WS-IX                                                
086900                                                                          
087000        PERFORM UNTIL LINES-MISSING                                       
087100           PERFORM S03-MOVE-TO-RESPOND                                    
087200           PERFORM DB2-FETCH-T01FCUS-CRS-13                               
087300        END-PERFORM                                                       
087400                                                                          
087500        PERFORM DB2-CLOSE-T01FCUS-CRS-13                                  
087600     END-IF                                                               
087700     .                                                                    
087800*** - HANDLE SEARCH-KEYS IDALPHA ,KDPARTTY, KDPARTGR AND IDLANDX3         
087900 FAN-ALPHA-LAND-PARTTY-GR SECTION.                                        
088000                                                                          
088100     PERFORM S04-EDIT-IDALPHA-KEY                                         
088200     PERFORM DB2-COUNT-CRS-14                                             
088300                                                                          
088400     IF WS-COUNTER-T01FCUS = ZERO                                         
088500        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
088600     ELSE                                                                 
088700        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
088800           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
088900        END-IF                                                            
089000     END-IF                                                               
089100                                                                          
089200     IF RESP-IDMSG-ERROR = SPACE                                          
089300        PERFORM DB2-DCL-OPN-T01FCUS-CRS-14                                
089400        PERFORM DB2-FETCH-T01FCUS-CRS-14                                  
089500        MOVE ZERO TO WS-IX                                                
089600                                                                          
089700        PERFORM UNTIL LINES-MISSING                                       
089800           PERFORM S03-MOVE-TO-RESPOND                                    
089900           PERFORM DB2-FETCH-T01FCUS-CRS-14                               
090000        END-PERFORM                                                       
090100                                                                          
090200        PERFORM DB2-CLOSE-T01FCUS-CRS-14                                  
090300     END-IF                                                               
090400     .                                                                    
090500*** - HANDLE SEARCH-KEYS IDALPHA AND KDPARTTY                             
090600 FAO-ALPHA-PARTTY SECTION.                                                
090700                                                                          
090800     PERFORM S04-EDIT-IDALPHA-KEY                                         
090900     PERFORM DB2-COUNT-CRS-15                                             
091000                                                                          
091100     IF WS-COUNTER-T01FCUS = ZERO                                         
091200        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
091300     ELSE                                                                 
091400        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
091500           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
091600        END-IF                                                            
091700     END-IF                                                               
091800                                                                          
091900     IF RESP-IDMSG-ERROR = SPACE                                          
092000        PERFORM DB2-DCL-OPN-T01FCUS-CRS-15                                
092100        PERFORM DB2-FETCH-T01FCUS-CRS-15                                  
092200        MOVE ZERO TO WS-IX                                                
092300                                                                          
092400        PERFORM UNTIL LINES-MISSING                                       
092500           PERFORM S03-MOVE-TO-RESPOND                                    
092600           PERFORM DB2-FETCH-T01FCUS-CRS-15                               
092700        END-PERFORM                                                       
092800                                                                          
092900        PERFORM DB2-CLOSE-T01FCUS-CRS-15                                  
093000     END-IF                                                               
093100     .                                                                    
093200*** - HANDLE SEARCH-KEYS IDALPHA, KDPARTTY AND KDPARTGR                   
093300 FAP-ALPHA-PARTTY-GR SECTION.                                             
093400                                                                          
093500     PERFORM S04-EDIT-IDALPHA-KEY                                         
093600     PERFORM DB2-COUNT-CRS-16                                             
093700                                                                          
093800     IF WS-COUNTER-T01FCUS = ZERO                                         
093900        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
094000     ELSE                                                                 
094100        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
094200           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
094300        END-IF                                                            
094400     END-IF                                                               
094500                                                                          
094600     IF RESP-IDMSG-ERROR = SPACE                                          
094700        PERFORM DB2-DCL-OPN-T01FCUS-CRS-16                                
094800        PERFORM DB2-FETCH-T01FCUS-CRS-16                                  
094900        MOVE ZERO TO WS-IX                                                
095000                                                                          
095100        PERFORM UNTIL LINES-MISSING                                       
095200           PERFORM S03-MOVE-TO-RESPOND                                    
095300           PERFORM DB2-FETCH-T01FCUS-CRS-16                               
095400        END-PERFORM                                                       
095500                                                                          
095600        PERFORM DB2-CLOSE-T01FCUS-CRS-16                                  
095700     END-IF                                                               
095800     .                                                                    
095900*** - HANDLE SEARCH-KEYS FLPREL AND IDLANDX3                              
096000 FAQ-PREL-LAND SECTION.                                                   
096100                                                                          
096200     PERFORM DB2-COUNT-CRS-17                                             
096300                                                                          
096400     IF WS-COUNTER-T01FCUS = ZERO                                         
096500        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
096600     ELSE                                                                 
096700        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
096800           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
096900        END-IF                                                            
097000     END-IF                                                               
097100                                                                          
097200     IF RESP-IDMSG-ERROR = SPACE                                          
097300        PERFORM DB2-DCL-OPN-T01FCUS-CRS-17                                
097400        PERFORM DB2-FETCH-T01FCUS-CRS-17                                  
097500        MOVE ZERO TO WS-IX                                                
097600                                                                          
097700        PERFORM UNTIL LINES-MISSING                                       
097800           PERFORM S03-MOVE-TO-RESPOND                                    
097900           PERFORM DB2-FETCH-T01FCUS-CRS-17                               
098000        END-PERFORM                                                       
098100                                                                          
098200        PERFORM DB2-CLOSE-T01FCUS-CRS-17                                  
098300     END-IF                                                               
098400     .                                                                    
098500*** - HANDLE SEARCH-KEYS FLPREL, KDPARTTY AND IDLANDX3                    
098600 FAR-PREL-LAND-PARTTY SECTION.                                            
098700                                                                          
098800     PERFORM DB2-COUNT-CRS-18                                             
098900                                                                          
099000     IF WS-COUNTER-T01FCUS = ZERO                                         
099100        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
099200     ELSE                                                                 
099300        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
099400           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
099500        END-IF                                                            
099600     END-IF                                                               
099700                                                                          
099800     IF RESP-IDMSG-ERROR = SPACE                                          
099900        PERFORM DB2-DCL-OPN-T01FCUS-CRS-18                                
100000        PERFORM DB2-FETCH-T01FCUS-CRS-18                                  
100100        MOVE ZERO TO WS-IX                                                
100200                                                                          
100300        PERFORM UNTIL LINES-MISSING                                       
100400           PERFORM S03-MOVE-TO-RESPOND                                    
100500           PERFORM DB2-FETCH-T01FCUS-CRS-18                               
100600        END-PERFORM                                                       
100700                                                                          
100800        PERFORM DB2-CLOSE-T01FCUS-CRS-18                                  
100900     END-IF                                                               
101000     .                                                                    
101100*** - HANDLE SEARCH-KEYS FLPREL, KDPARTTY, KDPARTGR AND IDLANDX3          
101200 FAS-PREL-LAND-PARTTY-GR SECTION.                                         
101300                                                                          
101400     PERFORM DB2-COUNT-CRS-19                                             
101500                                                                          
101600     IF WS-COUNTER-T01FCUS = ZERO                                         
101700        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
101800     ELSE                                                                 
101900        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
102000           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
102100        END-IF                                                            
102200     END-IF                                                               
102300                                                                          
102400     IF RESP-IDMSG-ERROR = SPACE                                          
102500        PERFORM DB2-DCL-OPN-T01FCUS-CRS-19                                
102600        PERFORM DB2-FETCH-T01FCUS-CRS-19                                  
102700        MOVE ZERO TO WS-IX                                                
102800                                                                          
102900        PERFORM UNTIL LINES-MISSING                                       
103000           PERFORM S03-MOVE-TO-RESPOND                                    
103100           PERFORM DB2-FETCH-T01FCUS-CRS-19                               
103200        END-PERFORM                                                       
103300                                                                          
103400        PERFORM DB2-CLOSE-T01FCUS-CRS-19                                  
103500     END-IF                                                               
103600     .                                                                    
103700*** - HANDLE SEARCH-KEYS FLPREL AND KDPARTTY                              
103800 FAT-PREL-PARTTY SECTION.                                                 
103900                                                                          
104000     PERFORM DB2-COUNT-CRS-20                                             
104100                                                                          
104200     IF WS-COUNTER-T01FCUS = ZERO                                         
104300        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
104400     ELSE                                                                 
104500        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
104600           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
104700        END-IF                                                            
104800     END-IF                                                               
104900                                                                          
105000     IF RESP-IDMSG-ERROR = SPACE                                          
105100        PERFORM DB2-DCL-OPN-T01FCUS-CRS-20                                
105200        PERFORM DB2-FETCH-T01FCUS-CRS-20                                  
105300        MOVE ZERO TO WS-IX                                                
105400                                                                          
105500        PERFORM UNTIL LINES-MISSING                                       
105600           PERFORM S03-MOVE-TO-RESPOND                                    
105700           PERFORM DB2-FETCH-T01FCUS-CRS-20                               
105800        END-PERFORM                                                       
105900                                                                          
106000        PERFORM DB2-CLOSE-T01FCUS-CRS-20                                  
106100     END-IF                                                               
106200     .                                                                    
106300*** - HANDLE SEARCH-KEYS FLPREL, KDPARTTY AND KDPARTGR                    
106400 FAU-PREL-PARTTY-GR SECTION.                                              
106500                                                                          
106600     PERFORM DB2-COUNT-CRS-21                                             
106700                                                                          
106800     IF WS-COUNTER-T01FCUS = ZERO                                         
106900        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
107000     ELSE                                                                 
107100        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
107200           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
107300        END-IF                                                            
107400     END-IF                                                               
107500                                                                          
107600     IF RESP-IDMSG-ERROR = SPACE                                          
107700        PERFORM DB2-DCL-OPN-T01FCUS-CRS-21                                
107800        PERFORM DB2-FETCH-T01FCUS-CRS-21                                  
107900        MOVE ZERO TO WS-IX                                                
108000                                                                          
108100        PERFORM UNTIL LINES-MISSING                                       
108200           PERFORM S03-MOVE-TO-RESPOND                                    
108300           PERFORM DB2-FETCH-T01FCUS-CRS-21                               
108400        END-PERFORM                                                       
108500                                                                          
108600        PERFORM DB2-CLOSE-T01FCUS-CRS-21                                  
108700     END-IF                                                               
108800     .                                                                    
108900*** - HANDLE SEARCH-KEYS KDPARTTY AND IDLANDX3                            
109000 FAV-LAND-PARTTY SECTION.                                                 
109100                                                                          
109200     PERFORM DB2-COUNT-CRS-22                                             
109300                                                                          
109400     IF WS-COUNTER-T01FCUS = ZERO                                         
109500        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
109600     ELSE                                                                 
109700        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
109800           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
109900        END-IF                                                            
110000     END-IF                                                               
110100                                                                          
110200     IF RESP-IDMSG-ERROR = SPACE                                          
110300        PERFORM DB2-DCL-OPN-T01FCUS-CRS-22                                
110400        PERFORM DB2-FETCH-T01FCUS-CRS-22                                  
110500        MOVE ZERO TO WS-IX                                                
110600                                                                          
110700        PERFORM UNTIL LINES-MISSING                                       
110800           PERFORM S03-MOVE-TO-RESPOND                                    
110900           PERFORM DB2-FETCH-T01FCUS-CRS-22                               
111000        END-PERFORM                                                       
111100                                                                          
111200        PERFORM DB2-CLOSE-T01FCUS-CRS-22                                  
111300     END-IF                                                               
111400     .                                                                    
111500*** - HANDLE SEARCH-KEYS KDPARTTY, KDPARTGR AND IDLANDX3                  
111600 FAW-LAND-PARTTY-GR SECTION.                                              
111700                                                                          
111800     PERFORM DB2-COUNT-CRS-23                                             
111900                                                                          
112000     IF WS-COUNTER-T01FCUS = ZERO                                         
112100        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
112200     ELSE                                                                 
112300        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
112400           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
112500        END-IF                                                            
112600     END-IF                                                               
112700                                                                          
112800     IF RESP-IDMSG-ERROR = SPACE                                          
112900        PERFORM DB2-DCL-OPN-T01FCUS-CRS-23                                
113000        PERFORM DB2-FETCH-T01FCUS-CRS-23                                  
113100        MOVE ZERO TO WS-IX                                                
113200                                                                          
113300        PERFORM UNTIL LINES-MISSING                                       
113400           PERFORM S03-MOVE-TO-RESPOND                                    
113500           PERFORM DB2-FETCH-T01FCUS-CRS-23                               
113600        END-PERFORM                                                       
113700                                                                          
113800        PERFORM DB2-CLOSE-T01FCUS-CRS-23                                  
113900     END-IF                                                               
114000     .                                                                    
114100*** - HANDLE SEARCH-KEY KDPARTTY AND KDPARTGR                             
114200 FAX-PARTTY-GR SECTION.                                                   
114300                                                                          
114400     PERFORM DB2-COUNT-CRS-24                                             
114500                                                                          
114600     IF WS-COUNTER-T01FCUS = ZERO                                         
114700        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
114800     ELSE                                                                 
114900        IF WS-COUNTER-T01FCUS > WS-MAX-LINES                              
115000           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
115100        END-IF                                                            
115200     END-IF                                                               
115300                                                                          
115400     IF RESP-IDMSG-ERROR = SPACE                                          
115500        PERFORM DB2-DCL-OPN-T01FCUS-CRS-24                                
115600        PERFORM DB2-FETCH-T01FCUS-CRS-24                                  
115700        MOVE ZERO TO WS-IX                                                
115800                                                                          
115900        PERFORM UNTIL LINES-MISSING                                       
116000           PERFORM S03-MOVE-TO-RESPOND                                    
116100           PERFORM DB2-FETCH-T01FCUS-CRS-24                               
116200        END-PERFORM                                                       
116300                                                                          
116400        PERFORM DB2-CLOSE-T01FCUS-CRS-24                                  
116500     END-IF                                                               
116600     .                                                                    
116700                                                                          
116800*   --- DISPATCHER SECTION START                                          
116900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
117000                                                                          
117100     MOVE 'GETARG'             TO SUB-KDFUNC                              
117200     MOVE WS-ADRESS            TO SUB-ADDISPABS                           
117300     MOVE LENGTH OF REQU-AREA  TO SUB-KVDLEN                              
117400                                                                          
117500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
117600                                                                          
117700     IF SUB-KDRC > 0                                                      
117800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
117900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
118000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
118100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
118200     END-IF                                                               
118300     .                                                                    
118400 S02-RETURN-RESPONSE SECTION.                                             
118500                                                                          
118600     MOVE 'RETURN'             TO SUB-KDFUNC                              
118700                                                                          
118800     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
118900                              - ((WS-MAX-LINES - WS-IX)                   
119000                              * LENGTH OF RESP-TABELLRAD)                 
119100                                                                          
119200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
119300                                                                          
119400     IF SUB-KDRC > 0                                                      
119500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
119600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
119700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
119800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
119900     END-IF                                                               
120000     .                                                                    
120100*** - MOVE DATA TO RESPOND WHEN CURRENT LINE,                             
120200***   WHEN COMING LINE MODIFY CURRENT LINE.                               
120300 S03-MOVE-TO-RESPOND SECTION.                                             
120400                                                                          
120410     ADD 1 TO WS-IX                                                       
120500     IF MAP-KDSTATUS-LINE = WS-CURRENT                                    
120700       MOVE MAP-IDPARTNR-LINE     TO RESP-IDPARTNR-LINE(WS-IX)            
120800       MOVE MAP-BEBET-NAME1-LINE  TO RESP-BEBET-NAME1-LINE(WS-IX)         
120900       MOVE MAP-ADBET-STREET-LINE TO RESP-ADBET-STREET-LINE(WS-IX)        
121000       MOVE MAP-ADBET-BOX-LINE    TO RESP-ADBET-BOX-LINE(WS-IX)           
121100       MOVE MAP-ADBET-PCODE-LINE  TO RESP-ADBET-PCODE-LINE(WS-IX)         
121200       MOVE MAP-ADBET-CITY-LINE   TO RESP-ADBET-CITY-LINE(WS-IX)          
121300       MOVE MAP-IDLANDX3-LINE     TO RESP-IDLANDX3-LINE(WS-IX)            
121400       MOVE NOO                   TO RESP-FLCOMING-LINE(WS-IX)            
121500     ELSE                                                                 
121600       IF  MAP-KDSTATUS-LINE = WS-COMING                                  
121700       AND MAP-IDPARTNR-LINE =  RESP-IDPARTNR-LINE(WS-IX)                 
121800           MOVE YES TO RESP-FLCOMING-LINE(WS-IX)                          
121900       END-IF                                                             
122000     END-IF                                                               
122100     .                                                                    
122200*** - CHECK NUMBERS OF ENTERED POSITION IN IDALPHA-KEY AND                
122300***   MOVE '%' AFTER ENTERED POSITION.                                    
122400 S04-EDIT-IDALPHA-KEY SECTION.                                            
122500                                                                          
122600     MOVE SPACE TO WS-IDALPHA-2                                           
122700                   WS-IDALPHA-3                                           
122800                   WS-IDALPHA-4                                           
122900                   WS-IDALPHA-5                                           
123000                   WS-IDALPHA-6                                           
123100                   WS-IDALPHA-7                                           
123200                   WS-IDALPHA-8                                           
123300                   WS-IDALPHA-9                                           
123400                   WS-IDALPHA-10                                          
123500                   WS-IDALPHA-11                                          
123600                   WS-IDALPHA-EDIT                                        
123700     MOVE REQU-IDALPHA-KEY TO WS-IDALPHA-EDIT                             
123800     MOVE +10 TO WS-IX-MOD                                                
123900                                                                          
124000     PERFORM UNTIL WS-IDALPHA-OCC11(WS-IX-MOD) > SPACE                    
124100     OR WS-IX-MOD < +1                                                    
124200        SUBTRACT 1 FROM WS-IX-MOD                                         
124300     END-PERFORM                                                          
124400                                                                          
124500     ADD 1 TO WS-IX-MOD                                                   
124600     MOVE WS-PERCENTAGE TO WS-IDALPHA-OCC11(WS-IX-MOD)                    
124700                                                                          
124800     IF WS-IX-MOD = +2                                                    
124900       MOVE WS-IDALPHA-EDIT TO WS-IDALPHA-2                               
125000     ELSE                                                                 
125100       IF WS-IX-MOD = +3                                                  
125200         MOVE WS-IDALPHA-EDIT TO WS-IDALPHA-3                             
125300       ELSE                                                               
125400         IF WS-IX-MOD = +4                                                
125500           MOVE WS-IDALPHA-EDIT TO WS-IDALPHA-4                           
125600         ELSE                                                             
125700           IF WS-IX-MOD = +5                                              
125800             MOVE WS-IDALPHA-EDIT TO WS-IDALPHA-5                         
125900           ELSE                                                           
126000             IF WS-IX-MOD = +6                                            
126100               MOVE WS-IDALPHA-EDIT TO WS-IDALPHA-6                       
126200             ELSE                                                         
126300               IF WS-IX-MOD = +7                                          
126400                 MOVE WS-IDALPHA-EDIT TO WS-IDALPHA-7                     
126500               ELSE                                                       
126600                 IF WS-IX-MOD = +8                                        
126700                   MOVE WS-IDALPHA-EDIT TO WS-IDALPHA-8                   
126800                 ELSE                                                     
126900                   IF WS-IX-MOD = +9                                      
127000                     MOVE WS-IDALPHA-EDIT TO WS-IDALPHA-9                 
127100                   ELSE                                                   
127200                     IF WS-IX-MOD = +10                                   
127300                       MOVE WS-IDALPHA-EDIT TO WS-IDALPHA-10              
127400                     ELSE                                                 
127500                       IF WS-IX-MOD = +11                                 
127600                         MOVE WS-IDALPHA-EDIT TO WS-IDALPHA-11            
127700                       END-IF                                             
127800                     END-IF                                               
127900                   END-IF                                                 
128000                 END-IF                                                   
128100               END-IF                                                     
128200             END-IF                                                       
128300           END-IF                                                         
128400         END-IF                                                           
128500       END-IF                                                             
128600     END-IF                                                               
128700     .                                                                    
128800*   --- DB2 SECTIONS                                                      
128900*** - CHECK THAT REQUESTED LEGAL SELLER EXIST                             
129000 DB2-SELECT-T01LSEL-TAB SECTION.                                          
129100                                                                          
129200     MOVE 000100 TO GOOD-SQLCODECODES                                     
129300                                                                          
129400     EXEC SQL                                                             
129500           SELECT  BELEGRAD_1                                             
129600                                                                          
129700           INTO   :T01LSEL-BELEGRAD-1                                     
129800                                                                          
129900           FROM    T01LSEL                                                
130000                                                                          
130100           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
130200            AND    KDSTATUS = :WS-CURRENT                                 
130300     END-EXEC                                                             
130400                                                                          
130500     MOVE SQLCODE TO SQLCODE-WS                                           
130600     PERFORM DB2-STATUS-CHECK                                             
130700     .                                                                    
130800*** - CHECK THAT REQUESTED PARTNERTYPE/PARTNERGROUP EXISTS                
130900 DB2-SELECT-T01CUGR-TAB SECTION.                                          
131000                                                                          
131100     MOVE 000100 TO GOOD-SQLCODECODES                                     
131200                                                                          
131300     EXEC SQL                                                             
131400           SELECT  KDINVFRQ                                               
131500                                                                          
131600           INTO   :T01CUGR-KDINVFRQ                                       
131700                                                                          
131800           FROM    T01CUGR                                                
131900                                                                          
132000           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
132100            AND    KDPARTTY = :REQU-KDPARTTY-KEY                          
132200            AND    KDPARTGR = :REQU-KDPARTGR-KEY                          
132300            AND    KDSTATUS = :WS-CURRENT                                 
132400            AND    DADELDAT = :WS-ACTIVE                                  
132500     END-EXEC                                                             
132600                                                                          
132700     MOVE SQLCODE TO SQLCODE-WS                                           
132800     PERFORM DB2-STATUS-CHECK                                             
132900     .                                                                    
133000*** - CHECK THAT REQUESTED COUNTRY EXISTS                                 
133100 DB2-SELECT-T01COCO-TAB SECTION.                                          
133200                                                                          
133300     MOVE 000100 TO GOOD-SQLCODECODES                                     
133400                                                                          
133500     EXEC SQL                                                             
133600           SELECT  BELAND                                                 
133700                                                                          
133800           INTO   :T01COCO-BELAND                                         
133900                                                                          
134000           FROM    T01COCO                                                
134100                                                                          
134200           WHERE   IDLANDX3 = :REQU-IDLANDX3-KEY                          
134300     END-EXEC                                                             
134400                                                                          
134500     MOVE SQLCODE TO SQLCODE-WS                                           
134600     PERFORM DB2-STATUS-CHECK                                             
134700     .                                                                    
134800* * * * * * * * * * *   CURSOR-1   * * * * * * * * * * * * * * * *        
134900 DB2-COUNT-CRS-1 SECTION.                                                 
135000                                                                          
135100     EXEC SQL                                                             
135200           SELECT COUNT(*)                                                
135300                                                                          
135400           INTO  :WS-COUNTER-T01FCUS                                      
135500                                                                          
135600           FROM   T01FCUS                                                 
135700                                                                          
135800           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
135900                AND IDPARTNR = :REQU-IDPARTNR-KEY                         
136000                AND KDSTATUS = :WS-CURRENT                                
136100                AND DADELDAT = :WS-ACTIVE                                 
136200     END-EXEC                                                             
136300                                                                          
136400     MOVE 000100  TO GOOD-SQLCODECODES                                    
136500                                                                          
136600     MOVE SQLCODE TO SQLCODE-WS                                           
136700     PERFORM DB2-STATUS-CHECK                                             
136800     .                                                                    
136900 DB2-DCL-OPN-T01FCUS-CRS-1 SECTION.                                       
137000                                                                          
137100     MOVE 000100 TO GOOD-SQLCODECODES                                     
137200                                                                          
137300     EXEC SQL                                                             
137400         DECLARE T01FCUS-CRS-1 CURSOR WITH HOLD FOR                       
137500                                                                          
137600           SELECT  IDPARTNR                                               
137700                 , KDSTATUS                                               
137800                 , BEBET_NAME1                                            
137900                 , ADBET_STREET                                           
138000                 , ADBET_BOX                                              
138100                 , ADBET_PCODE                                            
138200                 , ADBET_CITY                                             
138300                 , IDLANDX3                                               
138400                                                                          
138500           FROM    T01FCUS                                                
138600                                                                          
138700           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
138800                AND IDPARTNR = :REQU-IDPARTNR-KEY                         
138900                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
139000                AND DADELDAT = :WS-ACTIVE                                 
139100                                                                          
139200           ORDER BY IDLEGSEL                                              
139300                  , IDALPHA                                               
139400                  , IDPARTNR                                              
139500                  , KDSTATUS                                              
139600     END-EXEC                                                             
139700                                                                          
139800     MOVE 000100  TO GOOD-SQLCODECODES                                    
139900                                                                          
140000     EXEC SQL                                                             
140100        OPEN T01FCUS-CRS-1                                                
140200     END-EXEC                                                             
140300                                                                          
140400     MOVE SQLCODE TO SQLCODE-WS                                           
140500     PERFORM DB2-STATUS-CHECK                                             
140600     .                                                                    
140700 DB2-FETCH-T01FCUS-CRS-1 SECTION.                                         
140800                                                                          
140900     MOVE 000100  TO GOOD-SQLCODECODES                                    
141000                                                                          
141100     EXEC SQL                                                             
141200         FETCH T01FCUS-CRS-1                                              
141300                                                                          
141400         INTO :MAP-IDPARTNR-LINE                                          
141500            , :MAP-KDSTATUS-LINE                                          
141600            , :MAP-BEBET-NAME1-LINE                                       
141700            , :MAP-ADBET-STREET-LINE                                      
141800            , :MAP-ADBET-BOX-LINE                                         
141900            , :MAP-ADBET-PCODE-LINE                                       
142000            , :MAP-ADBET-CITY-LINE                                        
142100            , :MAP-IDLANDX3-LINE                                          
142200     END-EXEC                                                             
142300                                                                          
142400     MOVE SQLCODE TO SQLCODE-WS                                           
142500     PERFORM DB2-STATUS-CHECK                                             
142600     .                                                                    
142700 DB2-CLOSE-T01FCUS-CRS-1 SECTION.                                         
142800                                                                          
142900     EXEC SQL                                                             
143000        CLOSE T01FCUS-CRS-1                                               
143100     END-EXEC                                                             
143200     .                                                                    
143300* * * * * * * * * * *   CURSOR-2   * * * * * * * * * * * * * * * *        
143400 DB2-COUNT-CRS-2 SECTION.                                                 
143500                                                                          
143600     EXEC SQL                                                             
143700          SELECT COUNT(*)                                                 
143800                                                                          
143900          INTO  :WS-COUNTER-T01FCUS                                       
144000                                                                          
144100          FROM   T01FCUS                                                  
144200                                                                          
144300          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
144400             AND KDSTATUS = :WS-CURRENT                                   
144500             AND  DADELDAT = :WS-ACTIVE                                   
144600             AND (IDALPHA  LIKE :WS-IDALPHA-2                             
144700             OR   IDALPHA  LIKE :WS-IDALPHA-3                             
144800             OR   IDALPHA  LIKE :WS-IDALPHA-4                             
144900             OR   IDALPHA  LIKE :WS-IDALPHA-5                             
145000             OR   IDALPHA  LIKE :WS-IDALPHA-6                             
145100             OR   IDALPHA  LIKE :WS-IDALPHA-7                             
145200             OR   IDALPHA  LIKE :WS-IDALPHA-8                             
145300             OR   IDALPHA  LIKE :WS-IDALPHA-9                             
145400             OR   IDALPHA  LIKE :WS-IDALPHA-10                            
145500             OR   IDALPHA  LIKE :WS-IDALPHA-11)                           
145600     END-EXEC                                                             
145700                                                                          
145800     MOVE 000100  TO GOOD-SQLCODECODES                                    
145900                                                                          
146000     MOVE SQLCODE TO SQLCODE-WS                                           
146100     PERFORM DB2-STATUS-CHECK                                             
146200     .                                                                    
146300 DB2-DCL-OPN-T01FCUS-CRS-2 SECTION.                                       
146400                                                                          
146500     MOVE 000100 TO GOOD-SQLCODECODES                                     
146600                                                                          
146700     EXEC SQL                                                             
146800         DECLARE T01FCUS-CRS-2 CURSOR WITH HOLD FOR                       
146900                                                                          
147000          SELECT   IDPARTNR                                               
147100                 , KDSTATUS                                               
147200                 , BEBET_NAME1                                            
147300                 , ADBET_STREET                                           
147400                 , ADBET_BOX                                              
147500                 , ADBET_PCODE                                            
147600                 , ADBET_CITY                                             
147700                 , IDLANDX3                                               
147800                                                                          
147900          FROM     T01FCUS                                                
148000                                                                          
148100          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
148200             AND  KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
148300             AND  DADELDAT = :WS-ACTIVE                                   
148400             AND (IDALPHA  LIKE :WS-IDALPHA-2                             
148500             OR   IDALPHA  LIKE :WS-IDALPHA-3                             
148600             OR   IDALPHA  LIKE :WS-IDALPHA-4                             
148700             OR   IDALPHA  LIKE :WS-IDALPHA-5                             
148800             OR   IDALPHA  LIKE :WS-IDALPHA-6                             
148900             OR   IDALPHA  LIKE :WS-IDALPHA-7                             
149000             OR   IDALPHA  LIKE :WS-IDALPHA-8                             
149100             OR   IDALPHA  LIKE :WS-IDALPHA-9                             
149200             OR   IDALPHA  LIKE :WS-IDALPHA-10                            
149300             OR   IDALPHA  LIKE :WS-IDALPHA-11)                           
149400                                                                          
149500          ORDER BY IDLEGSEL                                               
149600                 , IDALPHA                                                
149700                 , IDPARTNR                                               
149800                 , KDSTATUS                                               
149900     END-EXEC                                                             
150000                                                                          
150100     MOVE 000100  TO GOOD-SQLCODECODES                                    
150200                                                                          
150300     EXEC SQL                                                             
150400        OPEN T01FCUS-CRS-2                                                
150500     END-EXEC                                                             
150600                                                                          
150700     MOVE SQLCODE TO SQLCODE-WS                                           
150800     PERFORM DB2-STATUS-CHECK                                             
150900     .                                                                    
151000 DB2-FETCH-T01FCUS-CRS-2 SECTION.                                         
151100                                                                          
151200     MOVE 000100  TO GOOD-SQLCODECODES                                    
151300                                                                          
151400     EXEC SQL                                                             
151500         FETCH T01FCUS-CRS-2                                              
151600                                                                          
151700         INTO :MAP-IDPARTNR-LINE                                          
151800            , :MAP-KDSTATUS-LINE                                          
151900            , :MAP-BEBET-NAME1-LINE                                       
152000            , :MAP-ADBET-STREET-LINE                                      
152100            , :MAP-ADBET-BOX-LINE                                         
152200            , :MAP-ADBET-PCODE-LINE                                       
152300            , :MAP-ADBET-CITY-LINE                                        
152400            , :MAP-IDLANDX3-LINE                                          
152500     END-EXEC                                                             
152600                                                                          
152700     MOVE SQLCODE TO SQLCODE-WS                                           
152800     PERFORM DB2-STATUS-CHECK                                             
152900     .                                                                    
153000 DB2-CLOSE-T01FCUS-CRS-2 SECTION.                                         
153100                                                                          
153200     EXEC SQL                                                             
153300        CLOSE T01FCUS-CRS-2                                               
153400     END-EXEC                                                             
153500     .                                                                    
153600* * * * * * * * * * *   CURSOR-3   * * * * * * * * * * * * * * * *        
153700 DB2-COUNT-CRS-3 SECTION.                                                 
153800                                                                          
153900     EXEC SQL                                                             
154000           SELECT COUNT(*)                                                
154100                                                                          
154200           INTO  :WS-COUNTER-T01FCUS                                      
154300                                                                          
154400           FROM   T01FCUS                                                 
154500                                                                          
154600           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
154700           AND    KDSTATUS = :WS-CURRENT                                  
154800           AND    DADELDAT = :WS-ACTIVE                                   
154900           AND   (KDPARTGR = :WS-KDPARTGR                                 
155000           OR     FLRATE   = :WS-FLRATE                                   
155100           OR     KDVALISO = :WS-KDVALISO)                                
155200     END-EXEC                                                             
155300                                                                          
155400     MOVE 000100  TO GOOD-SQLCODECODES                                    
155500                                                                          
155600     MOVE SQLCODE TO SQLCODE-WS                                           
155700     PERFORM DB2-STATUS-CHECK                                             
155800     .                                                                    
155900 DB2-DCL-OPN-T01FCUS-CRS-3 SECTION.                                       
156000                                                                          
156100     MOVE 000100 TO GOOD-SQLCODECODES                                     
156200                                                                          
156300     EXEC SQL                                                             
156400         DECLARE T01FCUS-CRS-3 CURSOR WITH HOLD FOR                       
156500                                                                          
156600           SELECT  IDPARTNR                                               
156700                  , KDSTATUS                                              
156800                  , BEBET_NAME1                                           
156900                  , ADBET_STREET                                          
157000                  , ADBET_BOX                                             
157100                  , ADBET_PCODE                                           
157200                  , ADBET_CITY                                            
157300                  , IDLANDX3                                              
157400                                                                          
157500           FROM     T01FCUS                                               
157600                                                                          
157700           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
157800           AND      KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
157900           AND      DADELDAT = :WS-ACTIVE                                 
158000           AND     (KDPARTGR = :WS-KDPARTGR                               
158100           OR       FLRATE   = :WS-FLRATE                                 
158200           OR       KDVALISO = :WS-KDVALISO)                              
158300                                                                          
158400           ORDER BY IDLEGSEL                                              
158500                  , IDALPHA                                               
158600                  , IDPARTNR                                              
158700                  , KDSTATUS                                              
158800     END-EXEC                                                             
158900                                                                          
159000     MOVE 000100  TO GOOD-SQLCODECODES                                    
159100                                                                          
159200     EXEC SQL                                                             
159300        OPEN T01FCUS-CRS-3                                                
159400     END-EXEC                                                             
159500                                                                          
159600     MOVE SQLCODE TO SQLCODE-WS                                           
159700     PERFORM DB2-STATUS-CHECK                                             
159800     .                                                                    
159900 DB2-FETCH-T01FCUS-CRS-3 SECTION.                                         
160000                                                                          
160100     MOVE 000100  TO GOOD-SQLCODECODES                                    
160200                                                                          
160300     EXEC SQL                                                             
160400         FETCH T01FCUS-CRS-3                                              
160500                                                                          
160600         INTO :MAP-IDPARTNR-LINE                                          
160700            , :MAP-KDSTATUS-LINE                                          
160800            , :MAP-BEBET-NAME1-LINE                                       
160900            , :MAP-ADBET-STREET-LINE                                      
161000            , :MAP-ADBET-BOX-LINE                                         
161100            , :MAP-ADBET-PCODE-LINE                                       
161200            , :MAP-ADBET-CITY-LINE                                        
161300            , :MAP-IDLANDX3-LINE                                          
161400     END-EXEC                                                             
161500                                                                          
161600     MOVE SQLCODE TO SQLCODE-WS                                           
161700     PERFORM DB2-STATUS-CHECK                                             
161800     .                                                                    
161900 DB2-CLOSE-T01FCUS-CRS-3 SECTION.                                         
162000                                                                          
162100     EXEC SQL                                                             
162200        CLOSE T01FCUS-CRS-3                                               
162300     END-EXEC                                                             
162400     .                                                                    
162500* * * * * * * * * * *   CURSOR-4   * * * * * * * * * * * * * * * *        
162600 DB2-COUNT-CRS-4 SECTION.                                                 
162700                                                                          
162800     EXEC SQL                                                             
162900           SELECT COUNT(*)                                                
163000                                                                          
163100           INTO  :WS-COUNTER-T01FCUS                                      
163200                                                                          
163300           FROM   T01FCUS                                                 
163400                                                                          
163500           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
163600              AND KDSTATUS = :WS-CURRENT                                  
163700              AND DADELDAT = :WS-ACTIVE                                   
163800              AND IDLANDX3 = :REQU-IDLANDX3-KEY                           
163900     END-EXEC                                                             
164000                                                                          
164100     MOVE 000100  TO GOOD-SQLCODECODES                                    
164200                                                                          
164300     MOVE SQLCODE TO SQLCODE-WS                                           
164400     PERFORM DB2-STATUS-CHECK                                             
164500     .                                                                    
164600 DB2-DCL-OPN-T01FCUS-CRS-4 SECTION.                                       
164700                                                                          
164800     MOVE 000100 TO GOOD-SQLCODECODES                                     
164900                                                                          
165000     EXEC SQL                                                             
165100         DECLARE T01FCUS-CRS-4 CURSOR WITH HOLD FOR                       
165200                                                                          
165300           SELECT  IDPARTNR                                               
165400                  , KDSTATUS                                              
165500                  , BEBET_NAME1                                           
165600                  , ADBET_STREET                                          
165700                  , ADBET_BOX                                             
165800                  , ADBET_PCODE                                           
165900                  , ADBET_CITY                                            
166000                  , IDLANDX3                                              
166100                                                                          
166200           FROM     T01FCUS                                               
166300                                                                          
166400           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
166500            AND     KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
166600            AND     DADELDAT = :WS-ACTIVE                                 
166700            AND     IDLANDX3 = :REQU-IDLANDX3-KEY                         
166800                                                                          
166900           ORDER BY IDLEGSEL                                              
167000                  , IDALPHA                                               
167100                  , IDPARTNR                                              
167200                  , KDSTATUS                                              
167300     END-EXEC                                                             
167400                                                                          
167500     MOVE 000100  TO GOOD-SQLCODECODES                                    
167600                                                                          
167700     EXEC SQL                                                             
167800        OPEN T01FCUS-CRS-4                                                
167900     END-EXEC                                                             
168000                                                                          
168100     MOVE SQLCODE TO SQLCODE-WS                                           
168200     PERFORM DB2-STATUS-CHECK                                             
168300     .                                                                    
168400 DB2-FETCH-T01FCUS-CRS-4 SECTION.                                         
168500                                                                          
168600     MOVE 000100  TO GOOD-SQLCODECODES                                    
168700                                                                          
168800     EXEC SQL                                                             
168900         FETCH T01FCUS-CRS-4                                              
169000                                                                          
169100         INTO :MAP-IDPARTNR-LINE                                          
169200            , :MAP-KDSTATUS-LINE                                          
169300            , :MAP-BEBET-NAME1-LINE                                       
169400            , :MAP-ADBET-STREET-LINE                                      
169500            , :MAP-ADBET-BOX-LINE                                         
169600            , :MAP-ADBET-PCODE-LINE                                       
169700            , :MAP-ADBET-CITY-LINE                                        
169800            , :MAP-IDLANDX3-LINE                                          
169900     END-EXEC                                                             
170000                                                                          
170100     MOVE SQLCODE TO SQLCODE-WS                                           
170200     PERFORM DB2-STATUS-CHECK                                             
170300     .                                                                    
170400 DB2-CLOSE-T01FCUS-CRS-4 SECTION.                                         
170500                                                                          
170600     EXEC SQL                                                             
170700        CLOSE T01FCUS-CRS-4                                               
170800     END-EXEC                                                             
170900     .                                                                    
171000* * * * * * * * * * *   CURSOR-5   * * * * * * * * * * * * * * * *        
171100 DB2-COUNT-CRS-5 SECTION.                                                 
171200                                                                          
171300     EXEC SQL                                                             
171400           SELECT COUNT(*)                                                
171500                                                                          
171600           INTO  :WS-COUNTER-T01FCUS                                      
171700                                                                          
171800           FROM   T01FCUS                                                 
171900                                                                          
172000           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
172100              AND KDSTATUS = :WS-CURRENT                                  
172200              AND DADELDAT = :WS-ACTIVE                                   
172300              AND KDPARTTY = :REQU-KDPARTTY-KEY                           
172400     END-EXEC                                                             
172500                                                                          
172600     MOVE 000100  TO GOOD-SQLCODECODES                                    
172700                                                                          
172800     MOVE SQLCODE TO SQLCODE-WS                                           
172900     PERFORM DB2-STATUS-CHECK                                             
173000     .                                                                    
173100 DB2-DCL-OPN-T01FCUS-CRS-5 SECTION.                                       
173200                                                                          
173300     MOVE 000100 TO GOOD-SQLCODECODES                                     
173400                                                                          
173500     EXEC SQL                                                             
173600         DECLARE T01FCUS-CRS-5 CURSOR WITH HOLD FOR                       
173700                                                                          
173800           SELECT  IDPARTNR                                               
173900                  , KDSTATUS                                              
174000                  , BEBET_NAME1                                           
174100                  , ADBET_STREET                                          
174200                  , ADBET_BOX                                             
174300                  , ADBET_PCODE                                           
174400                  , ADBET_CITY                                            
174500                  , IDLANDX3                                              
174600                                                                          
174700           FROM     T01FCUS                                               
174800                                                                          
174900           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
175000            AND     KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
175100            AND     DADELDAT = :WS-ACTIVE                                 
175200            AND     KDPARTTY = :REQU-KDPARTTY-KEY                         
175300                                                                          
175400           ORDER BY IDLEGSEL                                              
175500                  , IDALPHA                                               
175600                  , IDPARTNR                                              
175700                  , KDSTATUS                                              
175800     END-EXEC                                                             
175900                                                                          
176000     MOVE 000100  TO GOOD-SQLCODECODES                                    
176100                                                                          
176200     EXEC SQL                                                             
176300        OPEN T01FCUS-CRS-5                                                
176400     END-EXEC                                                             
176500                                                                          
176600     MOVE SQLCODE TO SQLCODE-WS                                           
176700     PERFORM DB2-STATUS-CHECK                                             
176800     .                                                                    
176900 DB2-FETCH-T01FCUS-CRS-5 SECTION.                                         
177000                                                                          
177100     MOVE 000100  TO GOOD-SQLCODECODES                                    
177200                                                                          
177300     EXEC SQL                                                             
177400         FETCH T01FCUS-CRS-5                                              
177500                                                                          
177600         INTO :MAP-IDPARTNR-LINE                                          
177700            , :MAP-KDSTATUS-LINE                                          
177800            , :MAP-BEBET-NAME1-LINE                                       
177900            , :MAP-ADBET-STREET-LINE                                      
178000            , :MAP-ADBET-BOX-LINE                                         
178100            , :MAP-ADBET-PCODE-LINE                                       
178200            , :MAP-ADBET-CITY-LINE                                        
178300            , :MAP-IDLANDX3-LINE                                          
178400     END-EXEC                                                             
178500                                                                          
178600     MOVE SQLCODE TO SQLCODE-WS                                           
178700     PERFORM DB2-STATUS-CHECK                                             
178800     .                                                                    
178900 DB2-CLOSE-T01FCUS-CRS-5 SECTION.                                         
179000                                                                          
179100     EXEC SQL                                                             
179200        CLOSE T01FCUS-CRS-5                                               
179300     END-EXEC                                                             
179400     .                                                                    
179500* * * * * * * * * * *   CURSOR-6   * * * * * * * * * * * * * * * *        
179600 DB2-COUNT-CRS-6 SECTION.                                                 
179700                                                                          
179800     EXEC SQL                                                             
179900          SELECT COUNT(*)                                                 
180000                                                                          
180100          INTO  :WS-COUNTER-T01FCUS                                       
180200                                                                          
180300          FROM   T01FCUS                                                  
180400                                                                          
180500          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
180600           AND    KDSTATUS = :WS-CURRENT                                  
180700           AND    DADELDAT = :WS-ACTIVE                                   
180800           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
180900           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
181000           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
181100           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
181200           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
181300           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
181400           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
181500           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
181600           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
181700           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
181800           AND   (KDPARTGR = :WS-KDPARTGR                                 
181900           OR     FLRATE   = :WS-FLRATE                                   
182000           OR     KDVALISO = :WS-KDVALISO)                                
182100     END-EXEC                                                             
182200                                                                          
182300     MOVE 000100  TO GOOD-SQLCODECODES                                    
182400                                                                          
182500     MOVE SQLCODE TO SQLCODE-WS                                           
182600     PERFORM DB2-STATUS-CHECK                                             
182700     .                                                                    
182800 DB2-DCL-OPN-T01FCUS-CRS-6 SECTION.                                       
182900                                                                          
183000     MOVE 000100 TO GOOD-SQLCODECODES                                     
183100                                                                          
183200     EXEC SQL                                                             
183300         DECLARE T01FCUS-CRS-6 CURSOR WITH HOLD FOR                       
183400                                                                          
183500          SELECT   IDPARTNR                                               
183600                 , KDSTATUS                                               
183700                 , BEBET_NAME1                                            
183800                 , ADBET_STREET                                           
183900                 , ADBET_BOX                                              
184000                 , ADBET_PCODE                                            
184100                 , ADBET_CITY                                             
184200                 , IDLANDX3                                               
184300                                                                          
184400          FROM     T01FCUS                                                
184500                                                                          
184600          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
184700           AND    KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
184800           AND    DADELDAT = :WS-ACTIVE                                   
184900           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
185000           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
185100           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
185200           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
185300           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
185400           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
185500           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
185600           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
185700           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
185800           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
185900           AND   (KDPARTGR = :WS-KDPARTGR                                 
186000           OR     FLRATE   = :WS-FLRATE                                   
186100           OR     KDVALISO = :WS-KDVALISO)                                
186200                                                                          
186300          ORDER BY IDLEGSEL                                               
186400                 , IDALPHA                                                
186500                 , IDPARTNR                                               
186600                 , KDSTATUS                                               
186700     END-EXEC                                                             
186800                                                                          
186900     MOVE 000100  TO GOOD-SQLCODECODES                                    
187000                                                                          
187100     EXEC SQL                                                             
187200        OPEN T01FCUS-CRS-6                                                
187300     END-EXEC                                                             
187400                                                                          
187500     MOVE SQLCODE TO SQLCODE-WS                                           
187600     PERFORM DB2-STATUS-CHECK                                             
187700     .                                                                    
187800 DB2-FETCH-T01FCUS-CRS-6 SECTION.                                         
187900                                                                          
188000     MOVE 000100  TO GOOD-SQLCODECODES                                    
188100                                                                          
188200     EXEC SQL                                                             
188300         FETCH T01FCUS-CRS-6                                              
188400                                                                          
188500         INTO :MAP-IDPARTNR-LINE                                          
188600            , :MAP-KDSTATUS-LINE                                          
188700            , :MAP-BEBET-NAME1-LINE                                       
188800            , :MAP-ADBET-STREET-LINE                                      
188900            , :MAP-ADBET-BOX-LINE                                         
189000            , :MAP-ADBET-PCODE-LINE                                       
189100            , :MAP-ADBET-CITY-LINE                                        
189200            , :MAP-IDLANDX3-LINE                                          
189300     END-EXEC                                                             
189400                                                                          
189500     MOVE SQLCODE TO SQLCODE-WS                                           
189600     PERFORM DB2-STATUS-CHECK                                             
189700     .                                                                    
189800 DB2-CLOSE-T01FCUS-CRS-6 SECTION.                                         
189900                                                                          
190000     EXEC SQL                                                             
190100        CLOSE T01FCUS-CRS-6                                               
190200     END-EXEC                                                             
190300     .                                                                    
190400* * * * * * * * * * *   CURSOR-7 * * * * * * * * * * * * * * * * *        
190500 DB2-COUNT-CRS-7 SECTION.                                                 
190600                                                                          
190700     EXEC SQL                                                             
190800          SELECT COUNT(*)                                                 
190900                                                                          
191000          INTO  :WS-COUNTER-T01FCUS                                       
191100                                                                          
191200          FROM   T01FCUS                                                  
191300                                                                          
191400          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
191500           AND    KDSTATUS = :WS-CURRENT                                  
191600           AND    DADELDAT = :WS-ACTIVE                                   
191700           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
191800           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
191900           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
192000           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
192100           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
192200           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
192300           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
192400           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
192500           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
192600           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
192700           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
192800           AND   (KDPARTGR = :WS-KDPARTGR                                 
192900           OR     FLRATE   = :WS-FLRATE                                   
193000           OR     KDVALISO = :WS-KDVALISO)                                
193100     END-EXEC                                                             
193200                                                                          
193300     MOVE 000100  TO GOOD-SQLCODECODES                                    
193400                                                                          
193500     MOVE SQLCODE TO SQLCODE-WS                                           
193600     PERFORM DB2-STATUS-CHECK                                             
193700     .                                                                    
193800 DB2-DCL-OPN-T01FCUS-CRS-7 SECTION.                                       
193900                                                                          
194000     MOVE 000100 TO GOOD-SQLCODECODES                                     
194100                                                                          
194200     EXEC SQL                                                             
194300         DECLARE T01FCUS-CRS-7 CURSOR WITH HOLD FOR                       
194400                                                                          
194500          SELECT   IDPARTNR                                               
194600                 , KDSTATUS                                               
194700                 , BEBET_NAME1                                            
194800                 , ADBET_STREET                                           
194900                 , ADBET_BOX                                              
195000                 , ADBET_PCODE                                            
195100                 , ADBET_CITY                                             
195200                 , IDLANDX3                                               
195300                                                                          
195400          FROM     T01FCUS                                                
195500                                                                          
195600          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
195700           AND    KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
195800           AND    DADELDAT = :WS-ACTIVE                                   
195900           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
196000           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
196100           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
196200           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
196300           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
196400           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
196500           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
196600           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
196700           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
196800           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
196900           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
197000           AND   (KDPARTGR = :WS-KDPARTGR                                 
197100           OR     FLRATE   = :WS-FLRATE                                   
197200           OR     KDVALISO = :WS-KDVALISO)                                
197300                                                                          
197400          ORDER BY IDLEGSEL                                               
197500                 , IDALPHA                                                
197600                 , IDPARTNR                                               
197700                 , KDSTATUS                                               
197800     END-EXEC                                                             
197900                                                                          
198000     MOVE 000100  TO GOOD-SQLCODECODES                                    
198100                                                                          
198200     EXEC SQL                                                             
198300        OPEN T01FCUS-CRS-7                                                
198400     END-EXEC                                                             
198500                                                                          
198600     MOVE SQLCODE TO SQLCODE-WS                                           
198700     PERFORM DB2-STATUS-CHECK                                             
198800     .                                                                    
198900 DB2-FETCH-T01FCUS-CRS-7 SECTION.                                         
199000                                                                          
199100     MOVE 000100  TO GOOD-SQLCODECODES                                    
199200                                                                          
199300     EXEC SQL                                                             
199400         FETCH T01FCUS-CRS-7                                              
199500                                                                          
199600         INTO :MAP-IDPARTNR-LINE                                          
199700            , :MAP-KDSTATUS-LINE                                          
199800            , :MAP-BEBET-NAME1-LINE                                       
199900            , :MAP-ADBET-STREET-LINE                                      
200000            , :MAP-ADBET-BOX-LINE                                         
200100            , :MAP-ADBET-PCODE-LINE                                       
200200            , :MAP-ADBET-CITY-LINE                                        
200300            , :MAP-IDLANDX3-LINE                                          
200400     END-EXEC                                                             
200500                                                                          
200600     MOVE SQLCODE TO SQLCODE-WS                                           
200700     PERFORM DB2-STATUS-CHECK                                             
200800     .                                                                    
200900 DB2-CLOSE-T01FCUS-CRS-7 SECTION.                                         
201000                                                                          
201100     EXEC SQL                                                             
201200        CLOSE T01FCUS-CRS-7                                               
201300     END-EXEC                                                             
201400     .                                                                    
201500* * * * * * * * * * * * CURSOR-8 * * * * * * * * * * * * * * * * *        
201600 DB2-COUNT-CRS-8 SECTION.                                                 
201700                                                                          
201800     EXEC SQL                                                             
201900          SELECT COUNT(*)                                                 
202000                                                                          
202100          INTO  :WS-COUNTER-T01FCUS                                       
202200                                                                          
202300          FROM   T01FCUS                                                  
202400                                                                          
202500          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
202600           AND    KDSTATUS = :WS-CURRENT                                  
202700           AND    DADELDAT = :WS-ACTIVE                                   
202800           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
202900           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
203000           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
203100           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
203200           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
203300           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
203400           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
203500           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
203600           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
203700           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
203800           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
203900           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
204000           AND   (KDPARTGR = :WS-KDPARTGR                                 
204100           OR     FLRATE   = :WS-FLRATE                                   
204200           OR     KDVALISO = :WS-KDVALISO)                                
204300     END-EXEC                                                             
204400                                                                          
204500     MOVE 000100  TO GOOD-SQLCODECODES                                    
204600                                                                          
204700     MOVE SQLCODE TO SQLCODE-WS                                           
204800     PERFORM DB2-STATUS-CHECK                                             
204900     .                                                                    
205000 DB2-DCL-OPN-T01FCUS-CRS-8 SECTION.                                       
205100                                                                          
205200     MOVE 000100 TO GOOD-SQLCODECODES                                     
205300                                                                          
205400     EXEC SQL                                                             
205500         DECLARE T01FCUS-CRS-8 CURSOR WITH HOLD FOR                       
205600                                                                          
205700          SELECT   IDPARTNR                                               
205800                 , KDSTATUS                                               
205900                 , BEBET_NAME1                                            
206000                 , ADBET_STREET                                           
206100                 , ADBET_BOX                                              
206200                 , ADBET_PCODE                                            
206300                 , ADBET_CITY                                             
206400                 , IDLANDX3                                               
206500                                                                          
206600          FROM     T01FCUS                                                
206700                                                                          
206800          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
206900           AND    KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
207000           AND    DADELDAT = :WS-ACTIVE                                   
207100           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
207200           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
207300           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
207400           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
207500           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
207600           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
207700           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
207800           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
207900           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
208000           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
208100           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
208200           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
208300           AND   (KDPARTGR = :WS-KDPARTGR                                 
208400           OR     FLRATE   = :WS-FLRATE                                   
208500           OR     KDVALISO = :WS-KDVALISO)                                
208600                                                                          
208700          ORDER BY IDLEGSEL                                               
208800                 , IDALPHA                                                
208900                 , IDPARTNR                                               
209000                 , KDSTATUS                                               
209100     END-EXEC                                                             
209200                                                                          
209300     MOVE 000100  TO GOOD-SQLCODECODES                                    
209400                                                                          
209500     EXEC SQL                                                             
209600        OPEN T01FCUS-CRS-8                                                
209700     END-EXEC                                                             
209800                                                                          
209900     MOVE SQLCODE TO SQLCODE-WS                                           
210000     PERFORM DB2-STATUS-CHECK                                             
210100     .                                                                    
210200 DB2-FETCH-T01FCUS-CRS-8 SECTION.                                         
210300                                                                          
210400     MOVE 000100  TO GOOD-SQLCODECODES                                    
210500                                                                          
210600     EXEC SQL                                                             
210700         FETCH T01FCUS-CRS-8                                              
210800                                                                          
210900         INTO :MAP-IDPARTNR-LINE                                          
211000            , :MAP-KDSTATUS-LINE                                          
211100            , :MAP-BEBET-NAME1-LINE                                       
211200            , :MAP-ADBET-STREET-LINE                                      
211300            , :MAP-ADBET-BOX-LINE                                         
211400            , :MAP-ADBET-PCODE-LINE                                       
211500            , :MAP-ADBET-CITY-LINE                                        
211600            , :MAP-IDLANDX3-LINE                                          
211700     END-EXEC                                                             
211800                                                                          
211900     MOVE SQLCODE TO SQLCODE-WS                                           
212000     PERFORM DB2-STATUS-CHECK                                             
212100     .                                                                    
212200 DB2-CLOSE-T01FCUS-CRS-8 SECTION.                                         
212300                                                                          
212400     EXEC SQL                                                             
212500        CLOSE T01FCUS-CRS-8                                               
212600     END-EXEC                                                             
212700     .                                                                    
212800* * * * * * * * * * *   CURSOR-9 * * * * * * * * * * * * * * * * *        
212900 DB2-COUNT-CRS-9 SECTION.                                                 
213000                                                                          
213100     EXEC SQL                                                             
213200          SELECT COUNT(*)                                                 
213300                                                                          
213400          INTO  :WS-COUNTER-T01FCUS                                       
213500                                                                          
213600          FROM   T01FCUS                                                  
213700                                                                          
213800          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
213900           AND    KDSTATUS = :WS-CURRENT                                  
214000           AND    DADELDAT = :WS-ACTIVE                                   
214100           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
214200           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
214300           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
214400           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
214500           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
214600           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
214700           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
214800           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
214900           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
215000           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
215100           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
215200           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
215300           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
215400           AND   (FLRATE   = :WS-FLRATE                                   
215500            OR    KDVALISO = :WS-KDVALISO)                                
215600     END-EXEC                                                             
215700                                                                          
215800     MOVE 000100  TO GOOD-SQLCODECODES                                    
215900                                                                          
216000     MOVE SQLCODE TO SQLCODE-WS                                           
216100     PERFORM DB2-STATUS-CHECK                                             
216200     .                                                                    
216300 DB2-DCL-OPN-T01FCUS-CRS-9 SECTION.                                       
216400                                                                          
216500     MOVE 000100 TO GOOD-SQLCODECODES                                     
216600                                                                          
216700     EXEC SQL                                                             
216800         DECLARE T01FCUS-CRS-9 CURSOR WITH HOLD FOR                       
216900                                                                          
217000          SELECT   IDPARTNR                                               
217100                 , KDSTATUS                                               
217200                 , BEBET_NAME1                                            
217300                 , ADBET_STREET                                           
217400                 , ADBET_BOX                                              
217500                 , ADBET_PCODE                                            
217600                 , ADBET_CITY                                             
217700                 , IDLANDX3                                               
217800                                                                          
217900          FROM     T01FCUS                                                
218000                                                                          
218100          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
218200           AND    KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
218300           AND    DADELDAT = :WS-ACTIVE                                   
218400           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
218500           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
218600           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
218700           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
218800           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
218900           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
219000           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
219100           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
219200           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
219300           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
219400           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
219500           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
219600           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
219700           AND   (FLRATE   = :WS-FLRATE                                   
219800            OR    KDVALISO = :WS-KDVALISO)                                
219900                                                                          
220000          ORDER BY IDLEGSEL                                               
220100                 , IDALPHA                                                
220200                 , IDPARTNR                                               
220300                 , KDSTATUS                                               
220400     END-EXEC                                                             
220500                                                                          
220600     MOVE 000100  TO GOOD-SQLCODECODES                                    
220700                                                                          
220800     EXEC SQL                                                             
220900        OPEN T01FCUS-CRS-9                                                
221000     END-EXEC                                                             
221100                                                                          
221200     MOVE SQLCODE TO SQLCODE-WS                                           
221300     PERFORM DB2-STATUS-CHECK                                             
221400     .                                                                    
221500 DB2-FETCH-T01FCUS-CRS-9 SECTION.                                         
221600                                                                          
221700     MOVE 000100  TO GOOD-SQLCODECODES                                    
221800                                                                          
221900     EXEC SQL                                                             
222000         FETCH T01FCUS-CRS-9                                              
222100                                                                          
222200         INTO :MAP-IDPARTNR-LINE                                          
222300            , :MAP-KDSTATUS-LINE                                          
222400            , :MAP-BEBET-NAME1-LINE                                       
222500            , :MAP-ADBET-STREET-LINE                                      
222600            , :MAP-ADBET-BOX-LINE                                         
222700            , :MAP-ADBET-PCODE-LINE                                       
222800            , :MAP-ADBET-CITY-LINE                                        
222900            , :MAP-IDLANDX3-LINE                                          
223000     END-EXEC                                                             
223100                                                                          
223200     MOVE SQLCODE TO SQLCODE-WS                                           
223300     PERFORM DB2-STATUS-CHECK                                             
223400     .                                                                    
223500 DB2-CLOSE-T01FCUS-CRS-9 SECTION.                                         
223600                                                                          
223700     EXEC SQL                                                             
223800        CLOSE T01FCUS-CRS-9                                               
223900     END-EXEC                                                             
224000     .                                                                    
224100* * * * * * * * * * *   CURSOR-10  * * * * * * * * * * * * * * * *        
224200 DB2-COUNT-CRS-10 SECTION.                                                
224300                                                                          
224400     EXEC SQL                                                             
224500          SELECT COUNT(*)                                                 
224600                                                                          
224700          INTO  :WS-COUNTER-T01FCUS                                       
224800                                                                          
224900          FROM   T01FCUS                                                  
225000                                                                          
225100          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
225200           AND    KDSTATUS = :WS-CURRENT                                  
225300           AND    DADELDAT = :WS-ACTIVE                                   
225400           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
225500           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
225600           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
225700           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
225800           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
225900           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
226000           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
226100           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
226200           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
226300           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
226400           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
226500           AND   (KDPARTGR = :WS-KDPARTGR                                 
226600           OR     FLRATE   = :WS-FLRATE                                   
226700           OR     KDVALISO = :WS-KDVALISO)                                
226800     END-EXEC                                                             
226900                                                                          
227000     MOVE 000100  TO GOOD-SQLCODECODES                                    
227100                                                                          
227200     MOVE SQLCODE TO SQLCODE-WS                                           
227300     PERFORM DB2-STATUS-CHECK                                             
227400     .                                                                    
227500 DB2-DCL-OPN-T01FCUS-CRS-10 SECTION.                                      
227600                                                                          
227700     MOVE 000100 TO GOOD-SQLCODECODES                                     
227800                                                                          
227900     EXEC SQL                                                             
228000         DECLARE T01FCUS-CRS-10 CURSOR WITH HOLD FOR                      
228100                                                                          
228200          SELECT   IDPARTNR                                               
228300                 , KDSTATUS                                               
228400                 , BEBET_NAME1                                            
228500                 , ADBET_STREET                                           
228600                 , ADBET_BOX                                              
228700                 , ADBET_PCODE                                            
228800                 , ADBET_CITY                                             
228900                 , IDLANDX3                                               
229000                                                                          
229100          FROM     T01FCUS                                                
229200                                                                          
229300          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
229400           AND    KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
229500           AND    DADELDAT = :WS-ACTIVE                                   
229600           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
229700           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
229800           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
229900           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
230000           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
230100           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
230200           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
230300           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
230400           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
230500           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
230600           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
230700           AND   (KDPARTGR = :WS-KDPARTGR                                 
230800           OR     FLRATE   = :WS-FLRATE                                   
230900           OR     KDVALISO = :WS-KDVALISO)                                
231000                                                                          
231100          ORDER BY IDLEGSEL                                               
231200                 , IDALPHA                                                
231300                 , IDPARTNR                                               
231400                 , KDSTATUS                                               
231500     END-EXEC                                                             
231600                                                                          
231700     MOVE 000100  TO GOOD-SQLCODECODES                                    
231800                                                                          
231900     EXEC SQL                                                             
232000        OPEN T01FCUS-CRS-10                                               
232100     END-EXEC                                                             
232200                                                                          
232300     MOVE SQLCODE TO SQLCODE-WS                                           
232400     PERFORM DB2-STATUS-CHECK                                             
232500     .                                                                    
232600 DB2-FETCH-T01FCUS-CRS-10 SECTION.                                        
232700                                                                          
232800     MOVE 000100  TO GOOD-SQLCODECODES                                    
232900                                                                          
233000     EXEC SQL                                                             
233100         FETCH T01FCUS-CRS-10                                             
233200                                                                          
233300         INTO :MAP-IDPARTNR-LINE                                          
233400            , :MAP-KDSTATUS-LINE                                          
233500            , :MAP-BEBET-NAME1-LINE                                       
233600            , :MAP-ADBET-STREET-LINE                                      
233700            , :MAP-ADBET-BOX-LINE                                         
233800            , :MAP-ADBET-PCODE-LINE                                       
233900            , :MAP-ADBET-CITY-LINE                                        
234000            , :MAP-IDLANDX3-LINE                                          
234100     END-EXEC                                                             
234200                                                                          
234300     MOVE SQLCODE TO SQLCODE-WS                                           
234400     PERFORM DB2-STATUS-CHECK                                             
234500     .                                                                    
234600 DB2-CLOSE-T01FCUS-CRS-10 SECTION.                                        
234700                                                                          
234800     EXEC SQL                                                             
234900        CLOSE T01FCUS-CRS-10                                              
235000     END-EXEC                                                             
235100     .                                                                    
235200* * * * * * * * * * *   CURSOR-11  * * * * * * * * * * * * * * * *        
235300 DB2-COUNT-CRS-11 SECTION.                                                
235400                                                                          
235500     EXEC SQL                                                             
235600          SELECT COUNT(*)                                                 
235700                                                                          
235800          INTO  :WS-COUNTER-T01FCUS                                       
235900                                                                          
236000          FROM   T01FCUS                                                  
236100                                                                          
236200          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
236300           AND    KDSTATUS = :WS-CURRENT                                  
236400           AND    DADELDAT = :WS-ACTIVE                                   
236500           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
236600           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
236700           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
236800           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
236900           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
237000           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
237100           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
237200           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
237300           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
237400           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
237500           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
237600           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
237700           AND   (FLRATE   = :WS-FLRATE                                   
237800            OR    KDVALISO = :WS-KDVALISO)                                
237900     END-EXEC                                                             
238000                                                                          
238100     MOVE 000100  TO GOOD-SQLCODECODES                                    
238200                                                                          
238300     MOVE SQLCODE TO SQLCODE-WS                                           
238400     PERFORM DB2-STATUS-CHECK                                             
238500     .                                                                    
238600 DB2-DCL-OPN-T01FCUS-CRS-11 SECTION.                                      
238700                                                                          
238800     MOVE 000100 TO GOOD-SQLCODECODES                                     
238900                                                                          
239000     EXEC SQL                                                             
239100         DECLARE T01FCUS-CRS-11 CURSOR WITH HOLD FOR                      
239200                                                                          
239300          SELECT   IDPARTNR                                               
239400                 , KDSTATUS                                               
239500                 , BEBET_NAME1                                            
239600                 , ADBET_STREET                                           
239700                 , ADBET_BOX                                              
239800                 , ADBET_PCODE                                            
239900                 , ADBET_CITY                                             
240000                 , IDLANDX3                                               
240100                                                                          
240200          FROM     T01FCUS                                                
240300                                                                          
240400          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
240500           AND    KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
240600           AND    DADELDAT = :WS-ACTIVE                                   
240700           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
240800           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
240900           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
241000           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
241100           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
241200           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
241300           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
241400           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
241500           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
241600           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
241700           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
241800           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
241900           AND   (FLRATE   = :WS-FLRATE                                   
242000            OR    KDVALISO = :WS-KDVALISO)                                
242100                                                                          
242200          ORDER BY IDLEGSEL                                               
242300                 , IDALPHA                                                
242400                 , IDPARTNR                                               
242500                 , KDSTATUS                                               
242600     END-EXEC                                                             
242700                                                                          
242800     MOVE 000100  TO GOOD-SQLCODECODES                                    
242900                                                                          
243000     EXEC SQL                                                             
243100        OPEN T01FCUS-CRS-11                                               
243200     END-EXEC                                                             
243300                                                                          
243400     MOVE SQLCODE TO SQLCODE-WS                                           
243500     PERFORM DB2-STATUS-CHECK                                             
243600     .                                                                    
243700 DB2-FETCH-T01FCUS-CRS-11 SECTION.                                        
243800                                                                          
243900     MOVE 000100  TO GOOD-SQLCODECODES                                    
244000                                                                          
244100     EXEC SQL                                                             
244200         FETCH T01FCUS-CRS-11                                             
244300                                                                          
244400         INTO :MAP-IDPARTNR-LINE                                          
244500            , :MAP-KDSTATUS-LINE                                          
244600            , :MAP-BEBET-NAME1-LINE                                       
244700            , :MAP-ADBET-STREET-LINE                                      
244800            , :MAP-ADBET-BOX-LINE                                         
244900            , :MAP-ADBET-PCODE-LINE                                       
245000            , :MAP-ADBET-CITY-LINE                                        
245100            , :MAP-IDLANDX3-LINE                                          
245200     END-EXEC                                                             
245300                                                                          
245400     MOVE SQLCODE TO SQLCODE-WS                                           
245500     PERFORM DB2-STATUS-CHECK                                             
245600     .                                                                    
245700 DB2-CLOSE-T01FCUS-CRS-11 SECTION.                                        
245800                                                                          
245900     EXEC SQL                                                             
246000        CLOSE T01FCUS-CRS-11                                              
246100     END-EXEC                                                             
246200     .                                                                    
246300* * * * * * * * * * *   CURSOR-12  * * * * * * * * * * * * * * * *        
246400 DB2-COUNT-CRS-12 SECTION.                                                
246500                                                                          
246600     EXEC SQL                                                             
246700          SELECT COUNT(*)                                                 
246800                                                                          
246900          INTO  :WS-COUNTER-T01FCUS                                       
247000                                                                          
247100          FROM   T01FCUS                                                  
247200                                                                          
247300          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
247400           AND    KDSTATUS = :WS-CURRENT                                  
247500           AND    DADELDAT = :WS-ACTIVE                                   
247600           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
247700           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
247800           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
247900           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
248000           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
248100           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
248200           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
248300           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
248400           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
248500           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
248600           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
248700     END-EXEC                                                             
248800                                                                          
248900     MOVE 000100  TO GOOD-SQLCODECODES                                    
249000                                                                          
249100     MOVE SQLCODE TO SQLCODE-WS                                           
249200     PERFORM DB2-STATUS-CHECK                                             
249300     .                                                                    
249400 DB2-DCL-OPN-T01FCUS-CRS-12 SECTION.                                      
249500                                                                          
249600     MOVE 000100 TO GOOD-SQLCODECODES                                     
249700                                                                          
249800     EXEC SQL                                                             
249900         DECLARE T01FCUS-CRS-12 CURSOR WITH HOLD FOR                      
250000                                                                          
250100          SELECT   IDPARTNR                                               
250200                 , KDSTATUS                                               
250300                 , BEBET_NAME1                                            
250400                 , ADBET_STREET                                           
250500                 , ADBET_BOX                                              
250600                 , ADBET_PCODE                                            
250700                 , ADBET_CITY                                             
250800                 , IDLANDX3                                               
250900                                                                          
251000          FROM     T01FCUS                                                
251100                                                                          
251200          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
251300           AND    KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
251400           AND    DADELDAT = :WS-ACTIVE                                   
251500           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
251600           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
251700           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
251800           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
251900           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
252000           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
252100           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
252200           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
252300           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
252400           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
252500           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
252600                                                                          
252700          ORDER BY IDLEGSEL                                               
252800                 , IDALPHA                                                
252900                 , IDPARTNR                                               
253000                 , KDSTATUS                                               
253100     END-EXEC                                                             
253200                                                                          
253300     MOVE 000100  TO GOOD-SQLCODECODES                                    
253400                                                                          
253500     EXEC SQL                                                             
253600        OPEN T01FCUS-CRS-12                                               
253700     END-EXEC                                                             
253800                                                                          
253900     MOVE SQLCODE TO SQLCODE-WS                                           
254000     PERFORM DB2-STATUS-CHECK                                             
254100     .                                                                    
254200 DB2-FETCH-T01FCUS-CRS-12 SECTION.                                        
254300                                                                          
254400     MOVE 000100  TO GOOD-SQLCODECODES                                    
254500                                                                          
254600     EXEC SQL                                                             
254700         FETCH T01FCUS-CRS-12                                             
254800                                                                          
254900         INTO :MAP-IDPARTNR-LINE                                          
255000            , :MAP-KDSTATUS-LINE                                          
255100            , :MAP-BEBET-NAME1-LINE                                       
255200            , :MAP-ADBET-STREET-LINE                                      
255300            , :MAP-ADBET-BOX-LINE                                         
255400            , :MAP-ADBET-PCODE-LINE                                       
255500            , :MAP-ADBET-CITY-LINE                                        
255600            , :MAP-IDLANDX3-LINE                                          
255700     END-EXEC                                                             
255800                                                                          
255900     MOVE SQLCODE TO SQLCODE-WS                                           
256000     PERFORM DB2-STATUS-CHECK                                             
256100     .                                                                    
256200 DB2-CLOSE-T01FCUS-CRS-12 SECTION.                                        
256300                                                                          
256400     EXEC SQL                                                             
256500        CLOSE T01FCUS-CRS-12                                              
256600     END-EXEC                                                             
256700     .                                                                    
256800* * * * * * * * * * *   CURSOR-13  * * * * * * * * * * * * * * * *        
256900 DB2-COUNT-CRS-13 SECTION.                                                
257000                                                                          
257100     EXEC SQL                                                             
257200          SELECT COUNT(*)                                                 
257300                                                                          
257400          INTO  :WS-COUNTER-T01FCUS                                       
257500                                                                          
257600          FROM   T01FCUS                                                  
257700                                                                          
257800          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
257900           AND    KDSTATUS = :WS-CURRENT                                  
258000           AND    DADELDAT = :WS-ACTIVE                                   
258100           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
258200           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
258300           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
258400           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
258500           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
258600           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
258700           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
258800           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
258900           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
259000           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
259100           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
259200           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
259300     END-EXEC                                                             
259400                                                                          
259500     MOVE 000100  TO GOOD-SQLCODECODES                                    
259600                                                                          
259700     MOVE SQLCODE TO SQLCODE-WS                                           
259800     PERFORM DB2-STATUS-CHECK                                             
259900     .                                                                    
260000 DB2-DCL-OPN-T01FCUS-CRS-13 SECTION.                                      
260100                                                                          
260200     MOVE 000100 TO GOOD-SQLCODECODES                                     
260300                                                                          
260400     EXEC SQL                                                             
260500         DECLARE T01FCUS-CRS-13 CURSOR WITH HOLD FOR                      
260600                                                                          
260700          SELECT   IDPARTNR                                               
260800                 , KDSTATUS                                               
260900                 , BEBET_NAME1                                            
261000                 , ADBET_STREET                                           
261100                 , ADBET_BOX                                              
261200                 , ADBET_PCODE                                            
261300                 , ADBET_CITY                                             
261400                 , IDLANDX3                                               
261500                                                                          
261600          FROM     T01FCUS                                                
261700                                                                          
261800          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
261900           AND    KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
262000           AND    DADELDAT = :WS-ACTIVE                                   
262100           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
262200           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
262300           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
262400           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
262500           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
262600           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
262700           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
262800           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
262900           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
263000           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
263100           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
263200           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
263300                                                                          
263400          ORDER BY IDLEGSEL                                               
263500                 , IDALPHA                                                
263600                 , IDPARTNR                                               
263700                 , KDSTATUS                                               
263800     END-EXEC                                                             
263900                                                                          
264000     MOVE 000100  TO GOOD-SQLCODECODES                                    
264100                                                                          
264200     EXEC SQL                                                             
264300        OPEN T01FCUS-CRS-13                                               
264400     END-EXEC                                                             
264500                                                                          
264600     MOVE SQLCODE TO SQLCODE-WS                                           
264700     PERFORM DB2-STATUS-CHECK                                             
264800     .                                                                    
264900 DB2-FETCH-T01FCUS-CRS-13 SECTION.                                        
265000                                                                          
265100     MOVE 000100  TO GOOD-SQLCODECODES                                    
265200                                                                          
265300     EXEC SQL                                                             
265400         FETCH T01FCUS-CRS-13                                             
265500                                                                          
265600         INTO :MAP-IDPARTNR-LINE                                          
265700            , :MAP-KDSTATUS-LINE                                          
265800            , :MAP-BEBET-NAME1-LINE                                       
265900            , :MAP-ADBET-STREET-LINE                                      
266000            , :MAP-ADBET-BOX-LINE                                         
266100            , :MAP-ADBET-PCODE-LINE                                       
266200            , :MAP-ADBET-CITY-LINE                                        
266300            , :MAP-IDLANDX3-LINE                                          
266400     END-EXEC                                                             
266500                                                                          
266600     MOVE SQLCODE TO SQLCODE-WS                                           
266700     PERFORM DB2-STATUS-CHECK                                             
266800     .                                                                    
266900 DB2-CLOSE-T01FCUS-CRS-13 SECTION.                                        
267000                                                                          
267100     EXEC SQL                                                             
267200        CLOSE T01FCUS-CRS-13                                              
267300     END-EXEC                                                             
267400     .                                                                    
267500* * * * * * * * * * *   CURSOR-14  * * * * * * * * * * * * * * * *        
267600 DB2-COUNT-CRS-14 SECTION.                                                
267700                                                                          
267800     EXEC SQL                                                             
267900          SELECT COUNT(*)                                                 
268000                                                                          
268100          INTO  :WS-COUNTER-T01FCUS                                       
268200                                                                          
268300          FROM   T01FCUS                                                  
268400                                                                          
268500          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
268600           AND    KDSTATUS = :WS-CURRENT                                  
268700           AND    DADELDAT = :WS-ACTIVE                                   
268800           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
268900           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
269000           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
269100           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
269200           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
269300           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
269400           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
269500           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
269600           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
269700           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
269800           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
269900           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
270000           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
270100     END-EXEC                                                             
270200                                                                          
270300     MOVE 000100  TO GOOD-SQLCODECODES                                    
270400                                                                          
270500     MOVE SQLCODE TO SQLCODE-WS                                           
270600     PERFORM DB2-STATUS-CHECK                                             
270700     .                                                                    
270800 DB2-DCL-OPN-T01FCUS-CRS-14 SECTION.                                      
270900                                                                          
271000     MOVE 000100 TO GOOD-SQLCODECODES                                     
271100                                                                          
271200     EXEC SQL                                                             
271300         DECLARE T01FCUS-CRS-14 CURSOR WITH HOLD FOR                      
271400                                                                          
271500          SELECT   IDPARTNR                                               
271600                 , KDSTATUS                                               
271700                 , BEBET_NAME1                                            
271800                 , ADBET_STREET                                           
271900                 , ADBET_BOX                                              
272000                 , ADBET_PCODE                                            
272100                 , ADBET_CITY                                             
272200                 , IDLANDX3                                               
272300                                                                          
272400          FROM     T01FCUS                                                
272500                                                                          
272600          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
272700           AND    KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
272800           AND    DADELDAT = :WS-ACTIVE                                   
272900           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
273000           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
273100           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
273200           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
273300           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
273400           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
273500           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
273600           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
273700           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
273800           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
273900           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
274000           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
274100           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
274200                                                                          
274300          ORDER BY IDLEGSEL                                               
274400                 , IDALPHA                                                
274500                 , IDPARTNR                                               
274600                 , KDSTATUS                                               
274700     END-EXEC                                                             
274800                                                                          
274900     MOVE 000100  TO GOOD-SQLCODECODES                                    
275000                                                                          
275100     EXEC SQL                                                             
275200        OPEN T01FCUS-CRS-14                                               
275300     END-EXEC                                                             
275400                                                                          
275500     MOVE SQLCODE TO SQLCODE-WS                                           
275600     PERFORM DB2-STATUS-CHECK                                             
275700     .                                                                    
275800 DB2-FETCH-T01FCUS-CRS-14 SECTION.                                        
275900                                                                          
276000     MOVE 000100  TO GOOD-SQLCODECODES                                    
276100                                                                          
276200     EXEC SQL                                                             
276300         FETCH T01FCUS-CRS-14                                             
276400                                                                          
276500         INTO :MAP-IDPARTNR-LINE                                          
276600            , :MAP-KDSTATUS-LINE                                          
276700            , :MAP-BEBET-NAME1-LINE                                       
276800            , :MAP-ADBET-STREET-LINE                                      
276900            , :MAP-ADBET-BOX-LINE                                         
277000            , :MAP-ADBET-PCODE-LINE                                       
277100            , :MAP-ADBET-CITY-LINE                                        
277200            , :MAP-IDLANDX3-LINE                                          
277300     END-EXEC                                                             
277400                                                                          
277500     MOVE SQLCODE TO SQLCODE-WS                                           
277600     PERFORM DB2-STATUS-CHECK                                             
277700     .                                                                    
277800 DB2-CLOSE-T01FCUS-CRS-14 SECTION.                                        
277900                                                                          
278000     EXEC SQL                                                             
278100        CLOSE T01FCUS-CRS-14                                              
278200     END-EXEC                                                             
278300     .                                                                    
278400* * * * * * * * * * *   CURSOR-15  * * * * * * * * * * * * * * * *        
278500 DB2-COUNT-CRS-15 SECTION.                                                
278600                                                                          
278700     EXEC SQL                                                             
278800          SELECT COUNT(*)                                                 
278900                                                                          
279000          INTO  :WS-COUNTER-T01FCUS                                       
279100                                                                          
279200          FROM   T01FCUS                                                  
279300                                                                          
279400          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
279500           AND    KDSTATUS = :WS-CURRENT                                  
279600           AND    DADELDAT = :WS-ACTIVE                                   
279700           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
279800           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
279900           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
280000           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
280100           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
280200           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
280300           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
280400           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
280500           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
280600           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
280700           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
280800     END-EXEC                                                             
280900                                                                          
281000     MOVE 000100  TO GOOD-SQLCODECODES                                    
281100                                                                          
281200     MOVE SQLCODE TO SQLCODE-WS                                           
281300     PERFORM DB2-STATUS-CHECK                                             
281400     .                                                                    
281500 DB2-DCL-OPN-T01FCUS-CRS-15 SECTION.                                      
281600                                                                          
281700     MOVE 000100 TO GOOD-SQLCODECODES                                     
281800                                                                          
281900     EXEC SQL                                                             
282000         DECLARE T01FCUS-CRS-15 CURSOR WITH HOLD FOR                      
282100                                                                          
282200          SELECT   IDPARTNR                                               
282300                 , KDSTATUS                                               
282400                 , BEBET_NAME1                                            
282500                 , ADBET_STREET                                           
282600                 , ADBET_BOX                                              
282700                 , ADBET_PCODE                                            
282800                 , ADBET_CITY                                             
282900                 , IDLANDX3                                               
283000                                                                          
283100          FROM     T01FCUS                                                
283200                                                                          
283300          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
283400           AND    KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
283500           AND    DADELDAT = :WS-ACTIVE                                   
283600           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
283700           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
283800           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
283900           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
284000           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
284100           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
284200           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
284300           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
284400           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
284500           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
284600           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
284700                                                                          
284800          ORDER BY IDLEGSEL                                               
284900                 , IDALPHA                                                
285000                 , IDPARTNR                                               
285100                 , KDSTATUS                                               
285200     END-EXEC                                                             
285300                                                                          
285400     MOVE 000100  TO GOOD-SQLCODECODES                                    
285500                                                                          
285600     EXEC SQL                                                             
285700        OPEN T01FCUS-CRS-15                                               
285800     END-EXEC                                                             
285900                                                                          
286000     MOVE SQLCODE TO SQLCODE-WS                                           
286100     PERFORM DB2-STATUS-CHECK                                             
286200     .                                                                    
286300 DB2-FETCH-T01FCUS-CRS-15 SECTION.                                        
286400                                                                          
286500     MOVE 000100  TO GOOD-SQLCODECODES                                    
286600                                                                          
286700     EXEC SQL                                                             
286800         FETCH T01FCUS-CRS-15                                             
286900                                                                          
287000         INTO :MAP-IDPARTNR-LINE                                          
287100            , :MAP-KDSTATUS-LINE                                          
287200            , :MAP-BEBET-NAME1-LINE                                       
287300            , :MAP-ADBET-STREET-LINE                                      
287400            , :MAP-ADBET-BOX-LINE                                         
287500            , :MAP-ADBET-PCODE-LINE                                       
287600            , :MAP-ADBET-CITY-LINE                                        
287700            , :MAP-IDLANDX3-LINE                                          
287800     END-EXEC                                                             
287900                                                                          
288000     MOVE SQLCODE TO SQLCODE-WS                                           
288100     PERFORM DB2-STATUS-CHECK                                             
288200     .                                                                    
288300 DB2-CLOSE-T01FCUS-CRS-15 SECTION.                                        
288400                                                                          
288500     EXEC SQL                                                             
288600        CLOSE T01FCUS-CRS-15                                              
288700     END-EXEC                                                             
288800     .                                                                    
288900* * * * * * * * * * *   CURSOR-16  * * * * * * * * * * * * * * * *        
289000 DB2-COUNT-CRS-16 SECTION.                                                
289100                                                                          
289200     EXEC SQL                                                             
289300          SELECT COUNT(*)                                                 
289400                                                                          
289500          INTO  :WS-COUNTER-T01FCUS                                       
289600                                                                          
289700          FROM   T01FCUS                                                  
289800                                                                          
289900          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
290000           AND    KDSTATUS = :WS-CURRENT                                  
290100           AND    DADELDAT = :WS-ACTIVE                                   
290200           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
290300           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
290400           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
290500           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
290600           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
290700           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
290800           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
290900           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
291000           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
291100           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
291200           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
291300           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
291400     END-EXEC                                                             
291500                                                                          
291600     MOVE 000100  TO GOOD-SQLCODECODES                                    
291700                                                                          
291800     MOVE SQLCODE TO SQLCODE-WS                                           
291900     PERFORM DB2-STATUS-CHECK                                             
292000     .                                                                    
292100 DB2-DCL-OPN-T01FCUS-CRS-16 SECTION.                                      
292200                                                                          
292300     MOVE 000100 TO GOOD-SQLCODECODES                                     
292400                                                                          
292500     EXEC SQL                                                             
292600         DECLARE T01FCUS-CRS-16 CURSOR WITH HOLD FOR                      
292700                                                                          
292800          SELECT   IDPARTNR                                               
292900                 , KDSTATUS                                               
293000                 , BEBET_NAME1                                            
293100                 , ADBET_STREET                                           
293200                 , ADBET_BOX                                              
293300                 , ADBET_PCODE                                            
293400                 , ADBET_CITY                                             
293500                 , IDLANDX3                                               
293600                                                                          
293700          FROM     T01FCUS                                                
293800                                                                          
293900          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
294000           AND    KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING             
294100           AND    DADELDAT = :WS-ACTIVE                                   
294200           AND   (IDALPHA  LIKE :WS-IDALPHA-2                             
294300           OR     IDALPHA  LIKE :WS-IDALPHA-3                             
294400           OR     IDALPHA  LIKE :WS-IDALPHA-4                             
294500           OR     IDALPHA  LIKE :WS-IDALPHA-5                             
294600           OR     IDALPHA  LIKE :WS-IDALPHA-6                             
294700           OR     IDALPHA  LIKE :WS-IDALPHA-7                             
294800           OR     IDALPHA  LIKE :WS-IDALPHA-8                             
294900           OR     IDALPHA  LIKE :WS-IDALPHA-9                             
295000           OR     IDALPHA  LIKE :WS-IDALPHA-10                            
295100           OR     IDALPHA  LIKE :WS-IDALPHA-11)                           
295200           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
295300           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
295400                                                                          
295500          ORDER BY IDLEGSEL                                               
295600                 , IDALPHA                                                
295700                 , IDPARTNR                                               
295800                 , KDSTATUS                                               
295900     END-EXEC                                                             
296000                                                                          
296100     MOVE 000100  TO GOOD-SQLCODECODES                                    
296200                                                                          
296300     EXEC SQL                                                             
296400        OPEN T01FCUS-CRS-16                                               
296500     END-EXEC                                                             
296600                                                                          
296700     MOVE SQLCODE TO SQLCODE-WS                                           
296800     PERFORM DB2-STATUS-CHECK                                             
296900     .                                                                    
297000 DB2-FETCH-T01FCUS-CRS-16 SECTION.                                        
297100                                                                          
297200     MOVE 000100  TO GOOD-SQLCODECODES                                    
297300                                                                          
297400     EXEC SQL                                                             
297500         FETCH T01FCUS-CRS-16                                             
297600                                                                          
297700         INTO :MAP-IDPARTNR-LINE                                          
297800            , :MAP-KDSTATUS-LINE                                          
297900            , :MAP-BEBET-NAME1-LINE                                       
298000            , :MAP-ADBET-STREET-LINE                                      
298100            , :MAP-ADBET-BOX-LINE                                         
298200            , :MAP-ADBET-PCODE-LINE                                       
298300            , :MAP-ADBET-CITY-LINE                                        
298400            , :MAP-IDLANDX3-LINE                                          
298500     END-EXEC                                                             
298600                                                                          
298700     MOVE SQLCODE TO SQLCODE-WS                                           
298800     PERFORM DB2-STATUS-CHECK                                             
298900     .                                                                    
299000 DB2-CLOSE-T01FCUS-CRS-16 SECTION.                                        
299100                                                                          
299200     EXEC SQL                                                             
299300        CLOSE T01FCUS-CRS-16                                              
299400     END-EXEC                                                             
299500     .                                                                    
299600* * * * * * * * * * *   CURSOR-17 * * * * * * * * * * * * * * * *         
299700 DB2-COUNT-CRS-17 SECTION.                                                
299800                                                                          
299900     EXEC SQL                                                             
300000           SELECT COUNT(*)                                                
300100                                                                          
300200           INTO  :WS-COUNTER-T01FCUS                                      
300300                                                                          
300400           FROM   T01FCUS                                                 
300500                                                                          
300600           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
300700           AND    KDSTATUS = :WS-CURRENT                                  
300800           AND    DADELDAT = :WS-ACTIVE                                   
300900           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
301000           AND   (KDPARTGR = :WS-KDPARTGR                                 
301100           OR     FLRATE   = :WS-FLRATE                                   
301200           OR     KDVALISO = :WS-KDVALISO)                                
301300     END-EXEC                                                             
301400                                                                          
301500     MOVE 000100  TO GOOD-SQLCODECODES                                    
301600                                                                          
301700     MOVE SQLCODE TO SQLCODE-WS                                           
301800     PERFORM DB2-STATUS-CHECK                                             
301900     .                                                                    
302000 DB2-DCL-OPN-T01FCUS-CRS-17 SECTION.                                      
302100                                                                          
302200     MOVE 000100 TO GOOD-SQLCODECODES                                     
302300                                                                          
302400     EXEC SQL                                                             
302500         DECLARE T01FCUS-CRS-17 CURSOR WITH HOLD FOR                      
302600                                                                          
302700           SELECT  IDPARTNR                                               
302800                  , KDSTATUS                                              
302900                  , BEBET_NAME1                                           
303000                  , ADBET_STREET                                          
303100                  , ADBET_BOX                                             
303200                  , ADBET_PCODE                                           
303300                  , ADBET_CITY                                            
303400                  , IDLANDX3                                              
303500                                                                          
303600           FROM     T01FCUS                                               
303700                                                                          
303800           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
303900            AND     KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
304000            AND     DADELDAT = :WS-ACTIVE                                 
304100            AND     IDLANDX3 = :REQU-IDLANDX3-KEY                         
304200            AND    (KDPARTGR = :WS-KDPARTGR                               
304300            OR      FLRATE   = :WS-FLRATE                                 
304400            OR      KDVALISO = :WS-KDVALISO)                              
304500                                                                          
304600           ORDER BY IDLEGSEL                                              
304700                  , IDALPHA                                               
304800                  , IDPARTNR                                              
304900                  , KDSTATUS                                              
305000     END-EXEC                                                             
305100                                                                          
305200     MOVE 000100  TO GOOD-SQLCODECODES                                    
305300                                                                          
305400     EXEC SQL                                                             
305500        OPEN T01FCUS-CRS-17                                               
305600     END-EXEC                                                             
305700                                                                          
305800     MOVE SQLCODE TO SQLCODE-WS                                           
305900     PERFORM DB2-STATUS-CHECK                                             
306000     .                                                                    
306100 DB2-FETCH-T01FCUS-CRS-17 SECTION.                                        
306200                                                                          
306300     MOVE 000100  TO GOOD-SQLCODECODES                                    
306400                                                                          
306500     EXEC SQL                                                             
306600         FETCH T01FCUS-CRS-17                                             
306700                                                                          
306800         INTO :MAP-IDPARTNR-LINE                                          
306900            , :MAP-KDSTATUS-LINE                                          
307000            , :MAP-BEBET-NAME1-LINE                                       
307100            , :MAP-ADBET-STREET-LINE                                      
307200            , :MAP-ADBET-BOX-LINE                                         
307300            , :MAP-ADBET-PCODE-LINE                                       
307400            , :MAP-ADBET-CITY-LINE                                        
307500            , :MAP-IDLANDX3-LINE                                          
307600     END-EXEC                                                             
307700                                                                          
307800     MOVE SQLCODE TO SQLCODE-WS                                           
307900     PERFORM DB2-STATUS-CHECK                                             
308000     .                                                                    
308100 DB2-CLOSE-T01FCUS-CRS-17 SECTION.                                        
308200                                                                          
308300     EXEC SQL                                                             
308400        CLOSE T01FCUS-CRS-17                                              
308500     END-EXEC                                                             
308600     .                                                                    
308700* * * * * * * * * * *   CURSOR-18  * * * * * * * * * * * * * * * *        
308800 DB2-COUNT-CRS-18 SECTION.                                                
308900                                                                          
309000     EXEC SQL                                                             
309100           SELECT COUNT(*)                                                
309200                                                                          
309300           INTO  :WS-COUNTER-T01FCUS                                      
309400                                                                          
309500           FROM   T01FCUS                                                 
309600                                                                          
309700           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
309800           AND    KDSTATUS = :WS-CURRENT                                  
309900           AND    DADELDAT = :WS-ACTIVE                                   
310000           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
310100           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
310200           AND   (KDPARTGR = :WS-KDPARTGR                                 
310300           OR     FLRATE   = :WS-FLRATE                                   
310400           OR     KDVALISO = :WS-KDVALISO)                                
310500     END-EXEC                                                             
310600                                                                          
310700     MOVE 000100  TO GOOD-SQLCODECODES                                    
310800                                                                          
310900     MOVE SQLCODE TO SQLCODE-WS                                           
311000     PERFORM DB2-STATUS-CHECK                                             
311100     .                                                                    
311200 DB2-DCL-OPN-T01FCUS-CRS-18 SECTION.                                      
311300                                                                          
311400     MOVE 000100 TO GOOD-SQLCODECODES                                     
311500                                                                          
311600     EXEC SQL                                                             
311700         DECLARE T01FCUS-CRS-18 CURSOR WITH HOLD FOR                      
311800                                                                          
311900           SELECT  IDPARTNR                                               
312000                  , KDSTATUS                                              
312100                  , BEBET_NAME1                                           
312200                  , ADBET_STREET                                          
312300                  , ADBET_BOX                                             
312400                  , ADBET_PCODE                                           
312500                  , ADBET_CITY                                            
312600                  , IDLANDX3                                              
312700                                                                          
312800           FROM     T01FCUS                                               
312900                                                                          
313000           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
313100            AND     KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
313200            AND     DADELDAT = :WS-ACTIVE                                 
313300            AND     KDPARTTY = :REQU-KDPARTTY-KEY                         
313400            AND     IDLANDX3 = :REQU-IDLANDX3-KEY                         
313500            AND    (KDPARTGR = :WS-KDPARTGR                               
313600            OR      FLRATE   = :WS-FLRATE                                 
313700            OR      KDVALISO = :WS-KDVALISO)                              
313800                                                                          
313900           ORDER BY IDLEGSEL                                              
314000                  , IDALPHA                                               
314100                  , IDPARTNR                                              
314200                  , KDSTATUS                                              
314300     END-EXEC                                                             
314400                                                                          
314500     MOVE 000100  TO GOOD-SQLCODECODES                                    
314600                                                                          
314700     EXEC SQL                                                             
314800        OPEN T01FCUS-CRS-18                                               
314900     END-EXEC                                                             
315000                                                                          
315100     MOVE SQLCODE TO SQLCODE-WS                                           
315200     PERFORM DB2-STATUS-CHECK                                             
315300     .                                                                    
315400 DB2-FETCH-T01FCUS-CRS-18 SECTION.                                        
315500                                                                          
315600     MOVE 000100  TO GOOD-SQLCODECODES                                    
315700                                                                          
315800     EXEC SQL                                                             
315900         FETCH T01FCUS-CRS-18                                             
316000                                                                          
316100         INTO :MAP-IDPARTNR-LINE                                          
316200            , :MAP-KDSTATUS-LINE                                          
316300            , :MAP-BEBET-NAME1-LINE                                       
316400            , :MAP-ADBET-STREET-LINE                                      
316500            , :MAP-ADBET-BOX-LINE                                         
316600            , :MAP-ADBET-PCODE-LINE                                       
316700            , :MAP-ADBET-CITY-LINE                                        
316800            , :MAP-IDLANDX3-LINE                                          
316900     END-EXEC                                                             
317000                                                                          
317100     MOVE SQLCODE TO SQLCODE-WS                                           
317200     PERFORM DB2-STATUS-CHECK                                             
317300     .                                                                    
317400 DB2-CLOSE-T01FCUS-CRS-18 SECTION.                                        
317500                                                                          
317600     EXEC SQL                                                             
317700        CLOSE T01FCUS-CRS-18                                              
317800     END-EXEC                                                             
317900     .                                                                    
318000* * * * * * * * * * *   CURSOR-19  * * * * * * * * * * * * * * * *        
318100 DB2-COUNT-CRS-19 SECTION.                                                
318200                                                                          
318300     EXEC SQL                                                             
318400           SELECT COUNT(*)                                                
318500                                                                          
318600           INTO  :WS-COUNTER-T01FCUS                                      
318700                                                                          
318800           FROM   T01FCUS                                                 
318900                                                                          
319000           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
319100           AND    KDSTATUS = :WS-CURRENT                                  
319200           AND    DADELDAT = :WS-ACTIVE                                   
319300           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
319400           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
319500           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
319600           AND   (FLRATE   = :WS-FLRATE                                   
319700            OR    KDVALISO = :WS-KDVALISO)                                
319800     END-EXEC                                                             
319900                                                                          
320000     MOVE 000100  TO GOOD-SQLCODECODES                                    
320100                                                                          
320200     MOVE SQLCODE TO SQLCODE-WS                                           
320300     PERFORM DB2-STATUS-CHECK                                             
320400     .                                                                    
320500 DB2-DCL-OPN-T01FCUS-CRS-19 SECTION.                                      
320600                                                                          
320700     MOVE 000100 TO GOOD-SQLCODECODES                                     
320800                                                                          
320900     EXEC SQL                                                             
321000         DECLARE T01FCUS-CRS-19 CURSOR WITH HOLD FOR                      
321100                                                                          
321200           SELECT  IDPARTNR                                               
321300                  , KDSTATUS                                              
321400                  , BEBET_NAME1                                           
321500                  , ADBET_STREET                                          
321600                  , ADBET_BOX                                             
321700                  , ADBET_PCODE                                           
321800                  , ADBET_CITY                                            
321900                  , IDLANDX3                                              
322000                                                                          
322100           FROM     T01FCUS                                               
322200                                                                          
322300           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
322400            AND     KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
322500            AND     DADELDAT = :WS-ACTIVE                                 
322600            AND     KDPARTTY = :REQU-KDPARTTY-KEY                         
322700            AND     KDPARTGR = :REQU-KDPARTGR-KEY                         
322800            AND     IDLANDX3 = :REQU-IDLANDX3-KEY                         
322900            AND    (FLRATE   = :WS-FLRATE                                 
323000             OR     KDVALISO = :WS-KDVALISO)                              
323100                                                                          
323200           ORDER BY IDLEGSEL                                              
323300                  , IDALPHA                                               
323400                  , IDPARTNR                                              
323500                  , KDSTATUS                                              
323600     END-EXEC                                                             
323700                                                                          
323800     MOVE 000100  TO GOOD-SQLCODECODES                                    
323900                                                                          
324000     EXEC SQL                                                             
324100        OPEN T01FCUS-CRS-19                                               
324200     END-EXEC                                                             
324300                                                                          
324400     MOVE SQLCODE TO SQLCODE-WS                                           
324500     PERFORM DB2-STATUS-CHECK                                             
324600     .                                                                    
324700 DB2-FETCH-T01FCUS-CRS-19 SECTION.                                        
324800                                                                          
324900     MOVE 000100  TO GOOD-SQLCODECODES                                    
325000                                                                          
325100     EXEC SQL                                                             
325200         FETCH T01FCUS-CRS-19                                             
325300                                                                          
325400         INTO :MAP-IDPARTNR-LINE                                          
325500            , :MAP-KDSTATUS-LINE                                          
325600            , :MAP-BEBET-NAME1-LINE                                       
325700            , :MAP-ADBET-STREET-LINE                                      
325800            , :MAP-ADBET-BOX-LINE                                         
325900            , :MAP-ADBET-PCODE-LINE                                       
326000            , :MAP-ADBET-CITY-LINE                                        
326100            , :MAP-IDLANDX3-LINE                                          
326200     END-EXEC                                                             
326300                                                                          
326400     MOVE SQLCODE TO SQLCODE-WS                                           
326500     PERFORM DB2-STATUS-CHECK                                             
326600     .                                                                    
326700 DB2-CLOSE-T01FCUS-CRS-19 SECTION.                                        
326800                                                                          
326900     EXEC SQL                                                             
327000        CLOSE T01FCUS-CRS-19                                              
327100     END-EXEC                                                             
327200     .                                                                    
327300* * * * * * * * * * *   CURSOR-20 * * * * * * * * * * * * * * * *         
327400 DB2-COUNT-CRS-20 SECTION.                                                
327500                                                                          
327600     EXEC SQL                                                             
327700           SELECT COUNT(*)                                                
327800                                                                          
327900           INTO  :WS-COUNTER-T01FCUS                                      
328000                                                                          
328100           FROM   T01FCUS                                                 
328200                                                                          
328300           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
328400           AND    KDSTATUS = :WS-CURRENT                                  
328500           AND    DADELDAT = :WS-ACTIVE                                   
328600           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
328700           AND   (KDPARTGR = :WS-KDPARTGR                                 
328800            OR    FLRATE   = :WS-FLRATE                                   
328900            OR    KDVALISO = :WS-KDVALISO)                                
329000     END-EXEC                                                             
329100                                                                          
329200     MOVE 000100  TO GOOD-SQLCODECODES                                    
329300                                                                          
329400     MOVE SQLCODE TO SQLCODE-WS                                           
329500     PERFORM DB2-STATUS-CHECK                                             
329600     .                                                                    
329700 DB2-DCL-OPN-T01FCUS-CRS-20 SECTION.                                      
329800                                                                          
329900     MOVE 000100 TO GOOD-SQLCODECODES                                     
330000                                                                          
330100     EXEC SQL                                                             
330200         DECLARE T01FCUS-CRS-20 CURSOR WITH HOLD FOR                      
330300                                                                          
330400           SELECT  IDPARTNR                                               
330500                  , KDSTATUS                                              
330600                  , BEBET_NAME1                                           
330700                  , ADBET_STREET                                          
330800                  , ADBET_BOX                                             
330900                  , ADBET_PCODE                                           
331000                  , ADBET_CITY                                            
331100                  , IDLANDX3                                              
331200                                                                          
331300           FROM     T01FCUS                                               
331400                                                                          
331500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
331600            AND     KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
331700            AND     DADELDAT = :WS-ACTIVE                                 
331800            AND     KDPARTTY = :REQU-KDPARTTY-KEY                         
331900            AND    (KDPARTGR = :WS-KDPARTGR                               
332000             OR     FLRATE   = :WS-FLRATE                                 
332100             OR     KDVALISO = :WS-KDVALISO)                              
332200                                                                          
332300           ORDER BY IDLEGSEL                                              
332400                  , IDALPHA                                               
332500                  , IDPARTNR                                              
332600                  , KDSTATUS                                              
332700     END-EXEC                                                             
332800                                                                          
332900     MOVE 000100  TO GOOD-SQLCODECODES                                    
333000                                                                          
333100     EXEC SQL                                                             
333200        OPEN T01FCUS-CRS-20                                               
333300     END-EXEC                                                             
333400                                                                          
333500     MOVE SQLCODE TO SQLCODE-WS                                           
333600     PERFORM DB2-STATUS-CHECK                                             
333700     .                                                                    
333800 DB2-FETCH-T01FCUS-CRS-20 SECTION.                                        
333900                                                                          
334000     MOVE 000100  TO GOOD-SQLCODECODES                                    
334100                                                                          
334200     EXEC SQL                                                             
334300         FETCH T01FCUS-CRS-20                                             
334400                                                                          
334500         INTO :MAP-IDPARTNR-LINE                                          
334600            , :MAP-KDSTATUS-LINE                                          
334700            , :MAP-BEBET-NAME1-LINE                                       
334800            , :MAP-ADBET-STREET-LINE                                      
334900            , :MAP-ADBET-BOX-LINE                                         
335000            , :MAP-ADBET-PCODE-LINE                                       
335100            , :MAP-ADBET-CITY-LINE                                        
335200            , :MAP-IDLANDX3-LINE                                          
335300     END-EXEC                                                             
335400                                                                          
335500     MOVE SQLCODE TO SQLCODE-WS                                           
335600     PERFORM DB2-STATUS-CHECK                                             
335700     .                                                                    
335800 DB2-CLOSE-T01FCUS-CRS-20 SECTION.                                        
335900                                                                          
336000     EXEC SQL                                                             
336100        CLOSE T01FCUS-CRS-20                                              
336200     END-EXEC                                                             
336300     .                                                                    
336400* * * * * * * * * * *   CURSOR-21 * * * * * * * * * * * * * * * *         
336500 DB2-COUNT-CRS-21 SECTION.                                                
336600                                                                          
336700     EXEC SQL                                                             
336800           SELECT COUNT(*)                                                
336900                                                                          
337000           INTO  :WS-COUNTER-T01FCUS                                      
337100                                                                          
337200           FROM   T01FCUS                                                 
337300                                                                          
337400           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
337500           AND    KDSTATUS = :WS-CURRENT                                  
337600           AND    DADELDAT = :WS-ACTIVE                                   
337700           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
337800           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
337900           AND   (FLRATE   = :WS-FLRATE                                   
338000            OR    KDVALISO = :WS-KDVALISO)                                
338100     END-EXEC                                                             
338200                                                                          
338300     MOVE 000100  TO GOOD-SQLCODECODES                                    
338400                                                                          
338500     MOVE SQLCODE TO SQLCODE-WS                                           
338600     PERFORM DB2-STATUS-CHECK                                             
338700     .                                                                    
338800 DB2-DCL-OPN-T01FCUS-CRS-21 SECTION.                                      
338900                                                                          
339000     MOVE 000100 TO GOOD-SQLCODECODES                                     
339100                                                                          
339200     EXEC SQL                                                             
339300         DECLARE T01FCUS-CRS-21 CURSOR WITH HOLD FOR                      
339400                                                                          
339500           SELECT  IDPARTNR                                               
339600                  , KDSTATUS                                              
339700                  , BEBET_NAME1                                           
339800                  , ADBET_STREET                                          
339900                  , ADBET_BOX                                             
340000                  , ADBET_PCODE                                           
340100                  , ADBET_CITY                                            
340200                  , IDLANDX3                                              
340300                                                                          
340400           FROM     T01FCUS                                               
340500                                                                          
340600           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
340700            AND     KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
340800            AND     DADELDAT = :WS-ACTIVE                                 
340900            AND     KDPARTTY = :REQU-KDPARTTY-KEY                         
341000            AND     KDPARTGR = :REQU-KDPARTGR-KEY                         
341100            AND    (FLRATE   = :WS-FLRATE                                 
341200             OR     KDVALISO = :WS-KDVALISO)                              
341300                                                                          
341400           ORDER BY IDLEGSEL                                              
341500                  , IDALPHA                                               
341600                  , IDPARTNR                                              
341700                  , KDSTATUS                                              
341800     END-EXEC                                                             
341900                                                                          
342000     MOVE 000100  TO GOOD-SQLCODECODES                                    
342100                                                                          
342200     EXEC SQL                                                             
342300        OPEN T01FCUS-CRS-21                                               
342400     END-EXEC                                                             
342500                                                                          
342600     MOVE SQLCODE TO SQLCODE-WS                                           
342700     PERFORM DB2-STATUS-CHECK                                             
342800     .                                                                    
342900 DB2-FETCH-T01FCUS-CRS-21 SECTION.                                        
343000                                                                          
343100     MOVE 000100  TO GOOD-SQLCODECODES                                    
343200                                                                          
343300     EXEC SQL                                                             
343400         FETCH T01FCUS-CRS-21                                             
343500                                                                          
343600         INTO :MAP-IDPARTNR-LINE                                          
343700            , :MAP-KDSTATUS-LINE                                          
343800            , :MAP-BEBET-NAME1-LINE                                       
343900            , :MAP-ADBET-STREET-LINE                                      
344000            , :MAP-ADBET-BOX-LINE                                         
344100            , :MAP-ADBET-PCODE-LINE                                       
344200            , :MAP-ADBET-CITY-LINE                                        
344300            , :MAP-IDLANDX3-LINE                                          
344400     END-EXEC                                                             
344500                                                                          
344600     MOVE SQLCODE TO SQLCODE-WS                                           
344700     PERFORM DB2-STATUS-CHECK                                             
344800     .                                                                    
344900 DB2-CLOSE-T01FCUS-CRS-21 SECTION.                                        
345000                                                                          
345100     EXEC SQL                                                             
345200        CLOSE T01FCUS-CRS-21                                              
345300     END-EXEC                                                             
345400     .                                                                    
345500* * * * * * * * * * *   CURSOR-22 * * * * * * * * * * * * * * * *         
345600 DB2-COUNT-CRS-22 SECTION.                                                
345700                                                                          
345800     EXEC SQL                                                             
345900           SELECT COUNT(*)                                                
346000                                                                          
346100           INTO  :WS-COUNTER-T01FCUS                                      
346200                                                                          
346300           FROM   T01FCUS                                                 
346400                                                                          
346500           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
346600           AND    KDSTATUS = :WS-CURRENT                                  
346700           AND    DADELDAT = :WS-ACTIVE                                   
346800           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
346900           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
347000     END-EXEC                                                             
347100                                                                          
347200     MOVE 000100  TO GOOD-SQLCODECODES                                    
347300                                                                          
347400     MOVE SQLCODE TO SQLCODE-WS                                           
347500     PERFORM DB2-STATUS-CHECK                                             
347600     .                                                                    
347700 DB2-DCL-OPN-T01FCUS-CRS-22 SECTION.                                      
347800                                                                          
347900     MOVE 000100 TO GOOD-SQLCODECODES                                     
348000                                                                          
348100     EXEC SQL                                                             
348200         DECLARE T01FCUS-CRS-22 CURSOR WITH HOLD FOR                      
348300                                                                          
348400           SELECT  IDPARTNR                                               
348500                  , KDSTATUS                                              
348600                  , BEBET_NAME1                                           
348700                  , ADBET_STREET                                          
348800                  , ADBET_BOX                                             
348900                  , ADBET_PCODE                                           
349000                  , ADBET_CITY                                            
349100                  , IDLANDX3                                              
349200                                                                          
349300           FROM     T01FCUS                                               
349400                                                                          
349500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
349600            AND     KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
349700            AND     DADELDAT = :WS-ACTIVE                                 
349800            AND     KDPARTTY = :REQU-KDPARTTY-KEY                         
349900            AND     IDLANDX3 = :REQU-IDLANDX3-KEY                         
350000                                                                          
350100           ORDER BY IDLEGSEL                                              
350200                  , IDALPHA                                               
350300                  , IDPARTNR                                              
350400                  , KDSTATUS                                              
350500     END-EXEC                                                             
350600                                                                          
350700     MOVE 000100  TO GOOD-SQLCODECODES                                    
350800                                                                          
350900     EXEC SQL                                                             
351000        OPEN T01FCUS-CRS-22                                               
351100     END-EXEC                                                             
351200                                                                          
351300     MOVE SQLCODE TO SQLCODE-WS                                           
351400     PERFORM DB2-STATUS-CHECK                                             
351500     .                                                                    
351600 DB2-FETCH-T01FCUS-CRS-22 SECTION.                                        
351700                                                                          
351800     MOVE 000100  TO GOOD-SQLCODECODES                                    
351900                                                                          
352000     EXEC SQL                                                             
352100         FETCH T01FCUS-CRS-22                                             
352200                                                                          
352300         INTO :MAP-IDPARTNR-LINE                                          
352400            , :MAP-KDSTATUS-LINE                                          
352500            , :MAP-BEBET-NAME1-LINE                                       
352600            , :MAP-ADBET-STREET-LINE                                      
352700            , :MAP-ADBET-BOX-LINE                                         
352800            , :MAP-ADBET-PCODE-LINE                                       
352900            , :MAP-ADBET-CITY-LINE                                        
353000            , :MAP-IDLANDX3-LINE                                          
353100     END-EXEC                                                             
353200                                                                          
353300     MOVE SQLCODE TO SQLCODE-WS                                           
353400     PERFORM DB2-STATUS-CHECK                                             
353500     .                                                                    
353600 DB2-CLOSE-T01FCUS-CRS-22 SECTION.                                        
353700                                                                          
353800     EXEC SQL                                                             
353900        CLOSE T01FCUS-CRS-22                                              
354000     END-EXEC                                                             
354100     .                                                                    
354200* * * * * * * * * * *   CURSOR-23 * * * * * * * * * * * * * * * *         
354300 DB2-COUNT-CRS-23 SECTION.                                                
354400                                                                          
354500     EXEC SQL                                                             
354600           SELECT COUNT(*)                                                
354700                                                                          
354800           INTO  :WS-COUNTER-T01FCUS                                      
354900                                                                          
355000           FROM   T01FCUS                                                 
355100                                                                          
355200           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
355300           AND    KDSTATUS = :WS-CURRENT                                  
355400           AND    DADELDAT = :WS-ACTIVE                                   
355500           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
355600           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
355700           AND    IDLANDX3 = :REQU-IDLANDX3-KEY                           
355800     END-EXEC                                                             
355900                                                                          
356000     MOVE 000100  TO GOOD-SQLCODECODES                                    
356100                                                                          
356200     MOVE SQLCODE TO SQLCODE-WS                                           
356300     PERFORM DB2-STATUS-CHECK                                             
356400     .                                                                    
356500 DB2-DCL-OPN-T01FCUS-CRS-23 SECTION.                                      
356600                                                                          
356700     MOVE 000100 TO GOOD-SQLCODECODES                                     
356800                                                                          
356900     EXEC SQL                                                             
357000         DECLARE T01FCUS-CRS-23 CURSOR WITH HOLD FOR                      
357100                                                                          
357200           SELECT  IDPARTNR                                               
357300                  , KDSTATUS                                              
357400                  , BEBET_NAME1                                           
357500                  , ADBET_STREET                                          
357600                  , ADBET_BOX                                             
357700                  , ADBET_PCODE                                           
357800                  , ADBET_CITY                                            
357900                  , IDLANDX3                                              
358000                                                                          
358100           FROM     T01FCUS                                               
358200                                                                          
358300           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
358400            AND     KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
358500            AND     DADELDAT = :WS-ACTIVE                                 
358600            AND     KDPARTTY = :REQU-KDPARTTY-KEY                         
358700            AND     KDPARTGR = :REQU-KDPARTGR-KEY                         
358800            AND     IDLANDX3 = :REQU-IDLANDX3-KEY                         
358900                                                                          
359000           ORDER BY IDLEGSEL                                              
359100                  , IDALPHA                                               
359200                  , IDPARTNR                                              
359300                  , KDSTATUS                                              
359400     END-EXEC                                                             
359500                                                                          
359600     MOVE 000100  TO GOOD-SQLCODECODES                                    
359700                                                                          
359800     EXEC SQL                                                             
359900        OPEN T01FCUS-CRS-23                                               
360000     END-EXEC                                                             
360100                                                                          
360200     MOVE SQLCODE TO SQLCODE-WS                                           
360300     PERFORM DB2-STATUS-CHECK                                             
360400     .                                                                    
360500 DB2-FETCH-T01FCUS-CRS-23 SECTION.                                        
360600                                                                          
360700     MOVE 000100  TO GOOD-SQLCODECODES                                    
360800                                                                          
360900     EXEC SQL                                                             
361000         FETCH T01FCUS-CRS-23                                             
361100                                                                          
361200         INTO :MAP-IDPARTNR-LINE                                          
361300            , :MAP-KDSTATUS-LINE                                          
361400            , :MAP-BEBET-NAME1-LINE                                       
361500            , :MAP-ADBET-STREET-LINE                                      
361600            , :MAP-ADBET-BOX-LINE                                         
361700            , :MAP-ADBET-PCODE-LINE                                       
361800            , :MAP-ADBET-CITY-LINE                                        
361900            , :MAP-IDLANDX3-LINE                                          
362000     END-EXEC                                                             
362100                                                                          
362200     MOVE SQLCODE TO SQLCODE-WS                                           
362300     PERFORM DB2-STATUS-CHECK                                             
362400     .                                                                    
362500 DB2-CLOSE-T01FCUS-CRS-23 SECTION.                                        
362600                                                                          
362700     EXEC SQL                                                             
362800        CLOSE T01FCUS-CRS-23                                              
362900     END-EXEC                                                             
363000     .                                                                    
363100* * * * * * * * * * *   CURSOR-24 * * * * * * * * * * * * * * * *         
363200 DB2-COUNT-CRS-24 SECTION.                                                
363300                                                                          
363400     EXEC SQL                                                             
363500           SELECT COUNT(*)                                                
363600                                                                          
363700           INTO  :WS-COUNTER-T01FCUS                                      
363800                                                                          
363900           FROM   T01FCUS                                                 
364000                                                                          
364100           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
364200              AND KDSTATUS = :WS-CURRENT                                  
364300              AND DADELDAT = :WS-ACTIVE                                   
364400              AND KDPARTTY = :REQU-KDPARTTY-KEY                           
364500              AND KDPARTGR = :REQU-KDPARTGR-KEY                           
364600     END-EXEC                                                             
364700                                                                          
364800     MOVE 000100  TO GOOD-SQLCODECODES                                    
364900                                                                          
365000     MOVE SQLCODE TO SQLCODE-WS                                           
365100     PERFORM DB2-STATUS-CHECK                                             
365200     .                                                                    
365300 DB2-DCL-OPN-T01FCUS-CRS-24 SECTION.                                      
365400                                                                          
365500     MOVE 000100 TO GOOD-SQLCODECODES                                     
365600                                                                          
365700     EXEC SQL                                                             
365800         DECLARE T01FCUS-CRS-24 CURSOR WITH HOLD FOR                      
365900                                                                          
366000           SELECT  IDPARTNR                                               
366100                  , KDSTATUS                                              
366200                  , BEBET_NAME1                                           
366300                  , ADBET_STREET                                          
366400                  , ADBET_BOX                                             
366500                  , ADBET_PCODE                                           
366600                  , ADBET_CITY                                            
366700                  , IDLANDX3                                              
366800                                                                          
366900           FROM     T01FCUS                                               
367000                                                                          
367100           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
367200            AND     KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
367300            AND     DADELDAT = :WS-ACTIVE                                 
367400            AND     KDPARTTY = :REQU-KDPARTTY-KEY                         
367500            AND     KDPARTGR = :REQU-KDPARTGR-KEY                         
367600                                                                          
367700           ORDER BY IDLEGSEL                                              
367800                  , IDALPHA                                               
367900                  , IDPARTNR                                              
368000                  , KDSTATUS                                              
368100     END-EXEC                                                             
368200                                                                          
368300     MOVE 000100  TO GOOD-SQLCODECODES                                    
368400                                                                          
368500     EXEC SQL                                                             
368600        OPEN T01FCUS-CRS-24                                               
368700     END-EXEC                                                             
368800                                                                          
368900     MOVE SQLCODE TO SQLCODE-WS                                           
369000     PERFORM DB2-STATUS-CHECK                                             
369100     .                                                                    
369200 DB2-FETCH-T01FCUS-CRS-24 SECTION.                                        
369300                                                                          
369400     MOVE 000100  TO GOOD-SQLCODECODES                                    
369500                                                                          
369600     EXEC SQL                                                             
369700         FETCH T01FCUS-CRS-24                                             
369800                                                                          
369900         INTO :MAP-IDPARTNR-LINE                                          
370000            , :MAP-KDSTATUS-LINE                                          
370100            , :MAP-BEBET-NAME1-LINE                                       
370200            , :MAP-ADBET-STREET-LINE                                      
370300            , :MAP-ADBET-BOX-LINE                                         
370400            , :MAP-ADBET-PCODE-LINE                                       
370500            , :MAP-ADBET-CITY-LINE                                        
370600            , :MAP-IDLANDX3-LINE                                          
370700     END-EXEC                                                             
370800                                                                          
370900     MOVE SQLCODE TO SQLCODE-WS                                           
371000     PERFORM DB2-STATUS-CHECK                                             
371100     .                                                                    
371200 DB2-CLOSE-T01FCUS-CRS-24 SECTION.                                        
371300                                                                          
371400     EXEC SQL                                                             
371500        CLOSE T01FCUS-CRS-24                                              
371600     END-EXEC                                                             
371700     .                                                                    
371800                                                                          
371900 DB2-STATUS-CHECK  SECTION.                                               
372000                                                                          
372100     SET SQLCODE-IX TO 1                                                  
372200     SEARCH GOOD-SQLCODE                                                  
372300       AT END                                                             
372400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
372500          DELIMITED BY SIZE INTO ERROR-TEXT                               
372600          CALL ABEND USING RKOD-ABEND-DB2                                 
372700       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
372800          CONTINUE                                                        
372900     END-SEARCH                                                           
373000     .                                                                    
