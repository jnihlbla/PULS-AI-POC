000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6012A00.                                                
000400*AUTHOR.         UMESH JAIN                                               
000500*DATE-WRITTEN.   01/NOV/2011                                              
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAM TO CREATE UNLOADING LIST FOR LOCAL                       
001100*        DELIVERIES THRU D&P                                              
001200*                                                                         
001300*        PROGRAM READS   W6G2                                             
001400*                        W6D1                                             
001500*                        WDK7                                             
001600*        CPY-TEXT        W6G201 (W6GX01)                                  
001700*                        W6G210 (W6GX6108)                                
001800*                        W6G215 (W6GX6110)                                
001900*                        W6D111                                           
002000*                        W6D121                                           
002100*                        W6D711                                           
002200*                                                                         
002300*    ABENFKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     EJECT                                                                
003500                                                                          
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800                                                                          
003900 FILE SECTION.                                                            
004000     EJECT                                                                
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W6012A00'.            
004700 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900     SKIP2                                                                
005000                                                                          
005100 77  WS-KVKOLLI                  PIC 9(4)    VALUE ZERO.                  
005200 77  WS-KVAVIS                   PIC 9(6)    VALUE ZERO.                  
005210 77  WS-FLKVROS                  PIC X(1)    VALUE SPACE.                 
005300 77  WS-BEFT                     PIC 9(2)    VALUE ZERO.                  
005400 77  WS-ADLAGOMR                 PIC 9(2)    VALUE ZERO.                  
005500 77  WS-IDRADNR-INL              PIC S9(5)   VALUE ZERO COMP-3.           
005600                                                                          
005700 01  WORK-SPAR-AREA.                                                      
005800                                                                          
005900*FÖREGÅENDE RAD (SKRIVEN RAD)                                             
006000     03  WS-RADEN.                                                        
006100         05  WS-PREV-IDLEVNR      PIC  X(5)   VALUE SPACE.                
006200         05  WS-PREV-IDFS         PIC X(8)    VALUE SPACE.                
006300         05  WS-PREV-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.          
006400                                                                          
006500     03  W-RADNR                  PIC S9(3)   VALUE ZERO COMP-3.          
006600                                                                          
006700 01  KDRC-DISPLAY                PIC Z(5).                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007100     EJECT                                                                
007200                                                                          
007300*                                                                         
007400*    --- PARAMETERS TO ABEND                                              
007500                                                                          
007600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007900*                                                                         
008000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008100     SKIP3                                                                
008200*01  -COPY WZ01SUB                                                        
008300     SKIP3                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
008500*01  -COPY WZ01SEND                                                       
008600*                                                                         
008700 01  HDR-AREA.                                                            
008800*    03  -COPY WZ01REQU  -PRE HDR-                                        
008900*    03  -COPY WZ04HDR                                                    
009000     SKIP3                                                                
009100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009200     SKIP3                                                                
009300 01  REQU-AREA.                                                           
009400*    03  -COPY WZ01REQU                                                   
009500*    03  -COPY W6012AI1                                                   
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
009800     SKIP3                                                                
009900 01  SEND-AREA.                                                           
010000*    03  -COPY W6012A1                                                    
010100     EJECT                                                                
010200*01  -COPY WMSGAREA                                                       
010300     EJECT                                                                
010400                                                                          
010500 01  DYNAMISKA-SUBPROGRAM.                                                
010600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
010800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011100     EJECT                                                                
011200*      --- VALID IDDC CODES                                               
011300*                                                                         
011400*01    -COPY WWDC99                                                       
011500       EJECT                                                              
011600                                                                          
011700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011800*01 -COPY WMEDAREA                                                        
011900     SKIP3                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012100     SKIP3                                                                
012200 01  NYCKLAR-TILL-DLI.                                                    
012300*----FYSISK NYCKEL TILL W6G2                                              
012400     03  W-W6G201KY-X.                                                    
012500         05  W-IDHTYP            PIC X(4)    VALUE SPACE.                 
012600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012700         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
012800                                                                          
012900     03  W-W6G210KY-X.                                                    
013000         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
013100         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
013200                                                                          
013300     03  W-W6G215KY-X.                                                    
013400         05  WL-IDLEVNR           PIC X(5)    VALUE SPACE.                
013500         05  WL-IDFS              PIC X(8)    VALUE SPACE.                
013600         05  WL-IDARTNR           PIC S9(9)   COMP-3.                     
013700         05  WL-KDSORT1           PIC S9      COMP-3.                     
013800                                                                          
013900*----FYSISK NYCKEL TILL INLA                                              
014000     03  W-W6D101KY-X.                                                    
014100         05  W-IDDC-D1           PIC X(2)    VALUE SPACE.                 
014200         05  W-IDLEVNR-D1        PIC X(5)    VALUE SPACE.                 
014300         05  W-IDFS-D1           PIC X(8)    VALUE SPACE.                 
014400         05  W-TIAVIDAT-D1       PIC S9(7)   COMP-3.                      
014500                                                                          
014600     03  W-IDRADNR-INL-X.                                                 
014700         05  W-IDRADNR-INL       PIC S9(5)   COMP-3.                      
014800                                                                          
014900     03  W-IDARTNR-X.                                                     
015000         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
015100                                                                          
015200     03  W-IDDC-X.                                                        
015300         05  W-IDDC-K7           PIC X(02).                               
015400                                                                          
015500     03  W-KDSEGKEY-X.                                                    
015600         05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                  
015700                                                                          
015800     EJECT                                                                
015900*    --- STATUS-KOD FRÅN IMS                                              
016000 01  STATUS-WS                   PIC XX.                                  
016100     88  SEGMENT-FINNS                       VALUE '  '.                  
016200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016500     EJECT                                                                
016600                                                                          
016610 01  STATUS-WS-TEMP              PIC XX.                                  
016700     SKIP2                                                                
016800 01  GODK-STATUSKODER.                                                    
016900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017000     EJECT                                                                
017100                                                                          
017200     SKIP3                                                                
017300 01  SSA1                        PIC X(90).                               
017400 01  SSA2                        PIC X(64).                               
017500 01  SSA3                        PIC X(64).                               
017600     EJECT                                                                
017700                                                                          
017800*    --- IMS FUNKTIONSKODER                                               
017900*01  -COPY W0003                                                          
018000     EJECT                                                                
018100                                                                          
018200*    ---  DLI INPUT-OUTPUT AREA                                           
018300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018400     SKIP3                                                                
018500 01  DLI-IO-AREA-1.                                                       
018600     03  IO-AREA-1               PIC X(150)  VALUE SPACE.                 
018700                                                                          
018800     03  W6G201   REDEFINES IO-AREA-1.                                    
018900*        05  -COPY W6GX01                                                 
019000     EJECT                                                                
019100                                                                          
019200     03  W6G210   REDEFINES IO-AREA-1.                                    
019300*        05  -COPY W6GX6108                                               
019400     EJECT                                                                
019500                                                                          
019600     03  W6G215   REDEFINES IO-AREA-1.                                    
019700*        05  -COPY W6GX6110                                               
019800     EJECT                                                                
019900                                                                          
020000 01  DLI-IO-AREA-2.                                                       
020100     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
020200                                                                          
020300     03  W6D111   REDEFINES IO-AREA-2.                                    
020400*        05  -COPY W6D111                                                 
020500     EJECT                                                                
020600                                                                          
020700     03  W6D121   REDEFINES IO-AREA-2.                                    
020800*        05  -COPY W6D121                                                 
020900     EJECT                                                                
021000                                                                          
021100 01  FILLER        PIC X(16) VALUE 'DLI-IO-WDK7'.                         
021200 01  DLI-IO-WDK7.                                                         
021300*    03  -COPY WDK711                                                     
021400     EJECT                                                                
021500                                                                          
021600 LINKAGE SECTION.                                                         
021700                                                                          
021800*01  -COPY W0009  -PRE MSG-                                               
021900     EJECT                                                                
022000                                                                          
022100 01  DISTRWEB-PCB                PIC X.                                   
022200     EJECT                                                                
022300                                                                          
022400*01  -COPY W0008  -PRE W6G2-                                              
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700                                                                          
022800*01  -COPY W0008  -PRE W6D1-                                              
022900     05  FILLER                  PIC X.                                   
023000     EJECT                                                                
023100                                                                          
023200*01  -COPY W0008  -PRE WDK7-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500                                                                          
023600 PROCEDURE DIVISION  USING MSG-PCB DISTRWEB-PCB                           
023700                           W6G2-PCB W6D1-PCB WDK7-PCB.                    
023800                                                                          
023900 MAIN SECTION.                                                            
024000     ENTRY 'DLITCBL' USING MSG-PCB DISTRWEB-PCB                           
024100                           W6G2-PCB W6D1-PCB WDK7-PCB.                    
024200                                                                          
024300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
024400     IF SUB-KDRC = 0                                                      
024500       PERFORM A-INIT                                                     
024600       PERFORM B-BEARB                                                    
024700     END-IF                                                               
024800                                                                          
024900     MOVE ZERO TO RETURN-CODE                                             
025000     GOBACK                                                               
025100     .                                                                    
025200     EJECT                                                                
025300******************************************************************        
025400 A-INIT SECTION.                                                          
025500     MOVE '6107'               TO W-IDHTYP                                
025600     MOVE REQU-IDDC-KEY        TO W-IDDC-K7                               
025610                                  W-IDDC                                  
025700     MOVE REQU-IDLBBET-KEY     TO W-IDLBBET                               
025900     .                                                                    
026000     EJECT                                                                
026100 B-BEARB SECTION.                                                         
026200     PERFORM IMS-GU-W6G210                                                
026300                                                                          
026310     IF SEGMENT-FINNS                                                     
026311       MOVE 6108-ADINLOMR-LPL        TO RR-ADINLOMR-LPL                   
026320       PERFORM S90-OPEN-DAP-SEND-WEB                                      
026330       PERFORM S90-PUT-DAP-HEADER                                         
026400       PERFORM IMS-GNP-W6G215                                             
026500       IF 6110-FLKLAR = NEJ                                               
026700         MOVE STATUS-WS TO STATUS-WS-TEMP                                 
026800         PERFORM BC-SPARA-FRAN-INLA                                       
026810         MOVE STATUS-WS-TEMP TO STATUS-WS                                 
026900       END-IF                                                             
026910                                                                          
027100       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
027200         IF 6110-FLKLAR = NEJ                                             
027300           PERFORM BB-PRINT-LINES                                         
027500           MOVE 6110-IDLEVNR    TO WS-PREV-IDLEVNR                        
027600           MOVE 6110-IDFS       TO WS-PREV-IDFS                           
027700           MOVE 6110-IDARTNR    TO WS-PREV-IDARTNR                        
027800         END-IF                                                           
027900         PERFORM IMS-GNP-W6G215                                           
027920                                                                          
028000         IF 6110-FLKLAR = NEJ                                             
028100           IF 6110-IDARTNR = WS-PREV-IDARTNR AND                          
028200              6110-IDFS    = WS-PREV-IDFS    AND                          
028300              6110-IDLEVNR = WS-PREV-IDLEVNR                              
028400              CONTINUE                                                    
028500           ELSE                                                           
028510             MOVE STATUS-WS TO STATUS-WS-TEMP                             
028600             PERFORM BC-SPARA-FRAN-INLA                                   
028601             MOVE STATUS-WS-TEMP TO STATUS-WS                             
028700           END-IF                                                         
028800         END-IF                                                           
028900       END-PERFORM                                                        
029000       PERFORM S90-CLOSE-DAP-SEND                                         
029010     END-IF                                                               
029100     .                                                                    
029200     EJECT                                                                
029300 BC-SPARA-FRAN-INLA SECTION.                                              
029400     MOVE ZERO TO WS-KVKOLLI                                              
029500     IF SEGMENT-FINNS                                                     
029600     MOVE REQU-IDDC-KEY TO W-IDDC-D1                                      
029700     MOVE 6110-IDLEVNR  TO W-IDLEVNR-D1                                   
029800     MOVE 6110-IDFS     TO W-IDFS-D1                                      
029900     MOVE 6110-TIAVIDAT TO W-TIAVIDAT-D1                                  
030000     MOVE 6110-IDARTNR  TO W-IDARTNR                                      
030100*                                                                         
030200     PERFORM IMS-GU-W6D101                                                
030300     IF SEGMENT-FINNS                                                     
030400       PERFORM IMS-GNP-W6D111-ARTNR                                       
030500       IF SEGMENT-FINNS                                                   
030600         MOVE ART-IDRADNR-INL  TO W-IDRADNR-INL                           
030700         MOVE ART-KVAVIS       TO WS-KVAVIS                               
030800         MOVE ART-BEFT         TO WS-BEFT                                 
030900         MOVE ART-IDRADNR-INL  TO WS-IDRADNR-INL                          
031000         MOVE ART-ADLAGOMR     TO WS-ADLAGOMR                             
031100         MOVE ART-FLKVAKAR     TO RR-FLKVAKAR                             
031200***                                                                       
031300         IF ART-KDFARLIG = 4 OR 7                                         
031400           MOVE YES             TO RR-KDFARLIG                            
031500         ELSE                                                             
031600           MOVE SPACES          TO RR-KDFARLIG                            
031700         END-IF                                                           
031800       END-IF                                                             
031900***                                                                       
032000       PERFORM UNTIL SEGMENT-SAKNAS                                       
032100         PERFORM IMS-GNP-W6D121                                           
032200         PERFORM UNTIL SEGMENT-SAKNAS                                     
032300           IF RAD-IDOKOLLI > ZERO                                         
032400             ADD +1 TO WS-KVKOLLI                                         
032500           END-IF                                                         
032600           PERFORM IMS-GNP-W6D121                                         
032700         END-PERFORM                                                      
032800         PERFORM IMS-GNP-W6D111-ARTNR                                     
032900         IF SEGMENT-FINNS                                                 
033000           MOVE ART-IDRADNR-INL  TO W-IDRADNR-INL                         
033100           ADD  ART-KVAVIS       TO WS-KVAVIS                             
033200           MOVE ART-IDRADNR-INL  TO WS-IDRADNR-INL                        
033300           MOVE ART-ADLAGOMR     TO WS-ADLAGOMR                           
033400         END-IF                                                           
033500       END-PERFORM                                                        
033600     END-IF                                                               
033700     END-IF                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 BB-PRINT-LINES        SECTION.                                           
034200*                                                                         
034210     MOVE REQU-IDLBBET-KEY     TO RR-IDLBBET                              
034230*                                                                         
034300     PERFORM IMS-GU-WDK711                                                
034400     IF SEGMENT-FINNS                                                     
034500       IF SLAG-KVROS-DAG > 0 OR SLAG-KVROS-BULK > 0                       
034600         MOVE YES            TO RR-FLKVROS                                
034610                                WS-FLKVROS                                
034700       END-IF                                                             
034800     END-IF                                                               
034900*                                                                         
035000     IF 6110-IDLEVNR = WS-PREV-IDLEVNR                                    
035100        IF W-RADNR = 1                                                    
035200          MOVE 6110-IDLEVNR TO RR-IDLEVNR                                 
035300        ELSE                                                              
035400          MOVE SPACE        TO RR-IDLEVNR                                 
035500        END-IF                                                            
035600        IF 6110-IDFS = WS-PREV-IDFS                                       
035700          IF W-RADNR = 1                                                  
035800            MOVE 6110-IDFS        TO RR-IDFS                              
035900          ELSE                                                            
036000            MOVE SPACE TO RR-IDFS                                         
036100          END-IF                                                          
036200          IF 6110-IDARTNR = WS-PREV-IDARTNR                               
036300            IF W-RADNR = 1                                                
036400              MOVE 6110-IDARTNR TO RR-IDARTNR                             
036500              MOVE WS-KVAVIS    TO RR-KVANTAL                             
036600              MOVE WS-KVKOLLI   TO RR-KVKOLLI                             
036700              MOVE WS-BEFT      TO RR-BEFT                                
036800            ELSE                                                          
036900              MOVE ZERO  TO RR-IDARTNR                                    
037000              MOVE ZERO  TO RR-KVKOLLI                                    
037100              MOVE ZERO  TO RR-BEFT                                       
037200            END-IF                                                        
037300          ELSE                                                            
037400            MOVE 6110-IDARTNR TO RR-IDARTNR                               
037500            MOVE WS-KVAVIS    TO RR-KVANTAL                               
037600            MOVE WS-KVKOLLI   TO RR-KVKOLLI                               
037700            MOVE WS-BEFT      TO RR-BEFT                                  
037800          END-IF                                                          
037900        ELSE                                                              
038000          MOVE 6110-IDFS     TO RR-IDFS                                   
038100          MOVE 6110-IDARTNR  TO RR-IDARTNR                                
038200          MOVE WS-KVAVIS     TO RR-KVANTAL                                
038300          MOVE WS-KVKOLLI    TO RR-KVKOLLI                                
038400          MOVE WS-BEFT       TO RR-BEFT                                   
038500        END-IF                                                            
038600     ELSE                                                                 
038700        IF W-RADNR > 0                                                    
038710          INITIALIZE RR-W6012A1                                           
038800          PERFORM S90-PUT-LINE                                            
038810        END-IF                                                            
038900        MOVE +1             TO W-RADNR                                    
039000        MOVE 6110-IDLEVNR   TO RR-IDLEVNR                                 
039100        MOVE 6110-IDFS      TO RR-IDFS                                    
039200        MOVE 6110-IDARTNR   TO RR-IDARTNR                                 
039300        MOVE WS-KVAVIS      TO RR-KVANTAL                                 
039400        MOVE WS-KVKOLLI     TO RR-KVKOLLI                                 
039500        MOVE WS-BEFT        TO RR-BEFT                                    
039510        MOVE WS-FLKVROS     TO RR-FLKVROS                                 
039600     END-IF                                                               
039700                                                                          
039800     MOVE 6110-ADINLOMR    TO RR-ADINLOMR                                 
039900     MOVE 6110-KVAVIS      TO RR-KVAVIS                                   
040000     MOVE 6110-KVAVIS-PRIO TO RR-KVAVIS-PRIO                              
040100                                                                          
040200     PERFORM S90-PUT-LINE                                                 
040201     ADD +1                TO W-RADNR                                     
040210     MOVE SPACE            TO RR-W6012A1                                  
040500     .                                                                    
040600     EJECT                                                                
040700*                                                                         
040800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
040900     MOVE 'GETARG'               TO SUB-KDFUNC                            
041000     MOVE 'CARPARTS.NDC.CREATEUNLOADINGREPORT'                            
041100                                 TO SUB-ADDISPABS                         
041200     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
041300                                                                          
041400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
041500                                                                          
041600     IF SUB-KDRC > 0                                                      
041700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
041800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
041900       DELIMITED BY SIZE INTO FELTEXT                                     
042000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 S90-OPEN-DAP-SEND-WEB SECTION.                                           
042500     MOVE 'OPEN'                  TO SEND-KDFUNC                          
042600*    -- WEB RESPONSE SHOULD HAVE LOWER PRIO TO FINISH LAST                
042700*    -- DISTRDOC = WZ0420X HAS LOWER PRIO THAN WZ0420U                    
042800     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
042900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
043000                         SEND-OPEN-AREA                                   
043100     IF SEND-KDRC > ZERO                                                  
043200       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
043300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
043400       DELIMITED BY SIZE INTO FELTEXT                                     
043500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
043600     END-IF                                                               
043700     .                                                                    
043800     SKIP3                                                                
043900 S90-PUT-DAP-HEADER SECTION.                                              
044000     MOVE 1                       TO HDR-REQU-IDMSGVER                    
044100     MOVE REQU-KDPGMACT           TO HDR-REQU-KDPGMACT                    
044200     MOVE REQU-IDUSER             TO HDR-REQU-IDUSER                      
044300     MOVE 'W6012A-001'            TO HDR-IDOUTTYPE                        
044400     MOVE REQU-IDDC-KEY           TO HDR-IDOUTREC (1:2)                   
044500     MOVE REQU-IDUSER             TO HDR-IDOUTREC (3:)                    
044600     MOVE SPACE                   TO HDR-IDLIST                           
044700     MOVE 'PUT'                   TO SEND-KDFUNC                          
044800     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
044900     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
045000                                     SEND-KVDLEN                          
045100                                     HDR-AREA                             
045200     IF SEND-KDRC > ZERO                                                  
045300       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
045400       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
045500       DELIMITED BY SIZE       INTO FELTEXT-STR                           
045600       DISPLAY FELTEXT                                                    
045700       CALL FELLOG                                                        
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 S90-PUT-LINE SECTION.                                                    
046200     MOVE 'PUT'                   TO SEND-KDFUNC                          
046300     MOVE LENGTH OF RR-W6012A1    TO SEND-KVDLEN                          
046400     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
046500                                     SEND-KVDLEN                          
046600                                     RR-W6012A1                           
046700     IF SEND-KDRC > ZERO                                                  
046800       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
046900       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
047000       DELIMITED BY SIZE       INTO FELTEXT-STR                           
047100       DISPLAY FELTEXT                                                    
047200       CALL FELLOG                                                        
047300     END-IF                                                               
047400     .                                                                    
047500     SKIP2                                                                
047600 S90-CLOSE-DAP-SEND SECTION.                                              
047700     MOVE 'CLOSE'                 TO SEND-KDFUNC                          
047800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
047900                                                                          
048000     IF SEND-KDRC > 0                                                     
048100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
048200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
048300       DELIMITED BY SIZE INTO FELTEXT                                     
048400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
048500     END-IF                                                               
048600     .                                                                    
048700     SKIP3                                                                
048800*----------------------------------------------------------------*        
048900 IMS-GU-W6G210    SECTION.                                                
049000     STRING 'W6G201  (W6GXKEY  =' W-W6G201KY-X ')'                        
049100          DELIMITED BY SIZE INTO SSA1                                     
049200     STRING 'W6G210  (W6GXKEY  =' W-W6G210KY-X ')'                        
049300          DELIMITED BY SIZE INTO SSA2                                     
049400     MOVE '  GE'            TO GODK-STATUSKODER                           
049500     CALL CBLTDLI USING GU  W6G2-PCB DLI-IO-AREA-1 SSA1 SSA2              
049600     MOVE W6G2-STATUS-CODE  TO STATUS-WS                                  
049700     PERFORM IMS-STATUSKONTROLL                                           
049800     .                                                                    
049900     EJECT                                                                
050000*----------------------------------------------------------------*        
050100 IMS-GNP-W6G215          SECTION.                                         
050200     STRING 'W6G201  (W6GXKEY  =' W-W6G201KY-X ')'                        
050300          DELIMITED BY SIZE INTO SSA1                                     
050400     STRING 'W6G210  (W6GXKEY  =' W-W6G210KY-X ')'                        
050500          DELIMITED BY SIZE INTO SSA2                                     
050600     MOVE 'W6G215   '            TO SSA3                                  
050700     MOVE '  GE'            TO GODK-STATUSKODER                           
050800     CALL CBLTDLI USING GNP W6G2-PCB DLI-IO-AREA-1 SSA1 SSA2 SSA3         
050900     MOVE W6G2-STATUS-CODE  TO STATUS-WS                                  
051000     PERFORM IMS-STATUSKONTROLL                                           
051100     .                                                                    
051200     EJECT                                                                
051300*----------------------------------------------------------------*        
051400 IMS-GU-W6D101    SECTION.                                                
051500     STRING 'W6D101  (W6D101KY =' W-W6D101KY-X ')'                        
051600          DELIMITED BY SIZE INTO SSA1                                     
051700     MOVE '  '            TO GODK-STATUSKODER                             
051800     CALL CBLTDLI USING GU  W6D1-PCB  DLI-IO-AREA-2 SSA1                  
051900     MOVE W6D1-STATUS-CODE  TO STATUS-WS                                  
052000     PERFORM IMS-STATUSKONTROLL                                           
052100     .                                                                    
052200     EJECT                                                                
052300*----------------------------------------------------------------*        
052400 IMS-GNP-W6D111-ARTNR SECTION.                                            
052500     STRING 'W6D111  (IDARTNR  =' W-IDARTNR-X ')'                         
052600          DELIMITED BY SIZE INTO SSA1                                     
052700     MOVE '  GE'          TO GODK-STATUSKODER                             
052800     CALL CBLTDLI USING GNP W6D1-PCB DLI-IO-AREA-2 SSA1                   
052900     MOVE W6D1-STATUS-CODE  TO STATUS-WS                                  
053000     PERFORM IMS-STATUSKONTROLL                                           
053100     .                                                                    
053200     EJECT                                                                
053300*----------------------------------------------------------------*        
053400 IMS-GNP-W6D121  SECTION.                                                 
053500     STRING 'W6D111  (IDRADNRI =' W-IDRADNR-INL-X ')'                     
053600          DELIMITED BY SIZE INTO SSA1                                     
053700     MOVE 'W6D121  '      TO SSA2                                         
053800     MOVE '  GE'          TO GODK-STATUSKODER                             
053900     CALL CBLTDLI USING GNP W6D1-PCB DLI-IO-AREA-2 SSA1 SSA2              
054000     MOVE W6D1-STATUS-CODE  TO STATUS-WS                                  
054100     PERFORM IMS-STATUSKONTROLL                                           
054200     .                                                                    
054300     EJECT                                                                
054400*----------------------------------------------------------------*        
054500 IMS-GU-WDK711 SECTION.                                                   
054600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
054700          DELIMITED BY SIZE INTO SSA1                                     
054800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
054900          DELIMITED BY SIZE INTO SSA2                                     
055000     MOVE '  GE' TO GODK-STATUSKODER                                      
055100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK7   SSA1 SSA2               
055200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
055300     PERFORM IMS-STATUSKONTROLL                                           
055400     .                                                                    
055500     EJECT                                                                
055600*----------------------------------------------------------------*        
055700 IMS-STATUSKONTROLL SECTION.                                              
055800     SET STATUS-IX TO 1                                                   
055900     SEARCH GODK-STATUS                                                   
056000       AT END                                                             
056100         MOVE 'FEL I W6012A00-PGM - W6INLA' TO FELTEXT-STR                
056200         CALL FELLOG                                                      
056300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
056400         CONTINUE                                                         
056500     END-SEARCH                                                           
056600     .                                                                    
056700     EJECT                                                                
