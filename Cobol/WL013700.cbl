000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL013700.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   07/06/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN:       STOCKCHECKANSWERS                                        
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER DC TRÄD OCH DC:NAS STOCK-CHECK-KOMMENTARER                 
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*        PROGRAMMET UPPDATERAR W6D2                                       
001400*        PROGRAMMET LÄSER      WDK7                                       
001500*        PROGRAMMET LÄSER      WDG2                                       
001600*        PROGRAMMET LÄSER      WDR2-WDGX6331                              
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: WL0137T                                             
002000*        REQUEST:     WL0137I1                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        RESPONSE:    WL0137O1                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'WL013700'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100 77  WS-IMS                      PIC X(80) VALUE SPACE.                   
004200 77  WS-SECTION                  PIC X(80) VALUE SPACE.                   
004300 77  KDRC-DISPLAY                PIC Z(5).                                
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  MAX-INDX                    PIC S9(9)   VALUE +500 COMP SYNC.        
004800 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77  FC-INDX                     PIC S9(9)   VALUE +0   COMP SYNC.        
005000 01  WS-SPAR-IDDC                PIC XX      VALUE SPACE.                 
005100 01  WS-IDDC-6334                PIC XX      VALUE SPACE.                 
005200 77  WS-IDKVAINF                 PIC X(2)    VALUE SPACE.                 
005300 77  WS-TIREGDAT                 PIC X(6)    VALUE SPACE.                 
005400 77  WS-DAREGDAT                 PIC X(6)    VALUE SPACE.                 
005500 77  WS-SHOW-ALL                 PIC X       VALUE SPACE.                 
005600 77  WS-SPAR-IDDC-6332           PIC X(2)  VALUE SPACE.                   
005700 01  W1-DAREGDAT                 PIC 9(08).                               
005800 01  WS-DAGENS-DATUM             PIC S9(16) COMP-3.                       
005900                                                                          
006000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006100     88  NYCKLAR-OK                          VALUE 'J'.                   
006200     88  NYCKLAR-FEL                         VALUE 'N'.                   
006300                                                                          
006400 77  POST-FINNS-SW               PIC X       VALUE 'N'.                   
006500     88  POST-FINNS                          VALUE 'J'.                   
006600     88  POST-SAKNAS                         VALUE 'N'.                   
006700                                                                          
006800 77  LEVEL-SW                    PIC XX      VALUE '00'.                  
006900     88  LEVEL-2                             VALUE '20'.                  
007000     88  LEVEL3-UTAN-USER                    VALUE '30'.                  
007100     88  LEVEL3-MED-USER                     VALUE '31'.                  
007200                                                                          
007300 77  LEVEL2-SW                    PIC X      VALUE 'N'.                   
007400     88  LEVEL-2-IDDC                        VALUE 'J'.                   
007500     88  LEVEL-3-IDDC                        VALUE 'N'.                   
007600     88  LEVEL-MISSING                       VALUE 'M'.                   
007700                                                                          
007800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007900     88  INDATA-OK                           VALUE 'J'.                   
008000     88  INDATA-FEL                          VALUE 'N'.                   
008100                                                                          
008200 77  UPPDAT-SW                   PIC X       VALUE 'N'.                   
008300     88  UPPDAT-OK                           VALUE 'J'.                   
008400     88  UPPDAT-FEL                          VALUE 'N'.                   
008500                                                                          
008600 77  POST-LEVEL-SW               PIC X       VALUE 'N'.                   
008700     88  POST-LEVEL-3                        VALUE 'J'.                   
008800     88  POST-LEVEL-2                        VALUE 'N'.                   
008900                                                                          
009000     EJECT                                                                
009100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009200 01  GENERELLA-SUBPROGRAM.                                                
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     SKIP3                                                                
009800*    --- PARAMETRAR TILL ABEND                                            
009900                                                                          
010000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010300     SKIP3                                                                
010400 01  MESSAGE-CODES.                                                       
010500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
010600     EJECT                                                                
010700*      --- VALID IDDC CODES                                               
010800*                                                                         
010900*01    -COPY WWDCKONS                                                     
011000       EJECT                                                              
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011300     SKIP3                                                                
011400*01  -COPY WZ01SUB                                                        
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011700     SKIP3                                                                
011800 01  REQU-AREA.                                                           
011900*    03  -COPY WZ01REQU                                                   
012000*    03  -COPY WL0137I1                                                   
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012300     SKIP3                                                                
012400 01  RESP-AREA.                                                           
012500*    03  -COPY WZ01RESP                                                   
012600*    03  -COPY WL0137O1                                                   
012700     EJECT                                                                
012800*    --- STATUS-KOD FRÅN IMS                                              
012900 01  STATUS-WS                   PIC XX.                                  
013000     88  SEGMENT-FINNS                       VALUE '  '.                  
013100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013300     88  ROOT-BYTE                           VALUE 'GA'.                  
013400     SKIP2                                                                
013500 01  GODK-STATUSKODER.                                                    
013600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700     SKIP3                                                                
013800 01  SSA1                        PIC X(64).                               
013900 01  SSA2                        PIC X(64).                               
014000 01  SSA3                        PIC X(64).                               
014100     EJECT                                                                
014200*    --- IMS FUNKTIONSKODER                                               
014300*01  -COPY W0003                                                          
014400     EJECT                                                                
014500*    ---  NYCKLAR-TILL-DLI.                                               
014600 01  NYCKLAR-TILL-DLI.                                                    
014700     03  W-IDARTNR-X.                                                     
014800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014900     03  W-IDDC-X.                                                        
015000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015100     03  W-W6D211KY-X.                                                    
015200         05  W-DAREGDAT-9KOMPL   PIC 9(08)   VALUE ZERO.                  
015300         05  W-TIKLOCK-9KOMPL    PIC S9(09)  VALUE ZERO COMP-3.           
015400     03  W-IDKVAINF-X.                                                    
015500         05  W-IDKVAINF          PIC  9(02)  VALUE ZERO.                  
015600     03  W-IDKVAINF-MIN-X.                                                
015700         05  W-IDKVAINF-MIN      PIC  9(02)  VALUE ZERO.                  
015800     03  W-IDKVAINF-MAX-X.                                                
015900         05  W-IDKVAINF-MAX      PIC  9(02)  VALUE ZERO.                  
016000     03  W-IDKVAINF-2.                                                    
016100         05  W-IDKVAINF2         PIC  9(02)  VALUE ZERO.                  
016200     03  W-KDSEGKEY-X.                                                    
016300         05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                  
016400                                                                          
016500     03  W-IDDC-B6-X.                                                     
016600         05 W-IDDC-B6            PIC X(2).                                
016700                                                                          
016800     03  W-WDGXKEY-6331-X.                                                
016900         05  W-IDHTYP            PIC X(4)    VALUE '6331'.                
017000         05  W-6331-IDDC         PIC X(2)    VALUE '11'.                  
017100         05  W-6331-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
017200     03  W-WDGXKEY-6332-X.                                                
017300         05  W-IDDC-6332         PIC X(2)    VALUE SPACE.                 
017400     03  W-WDGXKEY-6334-X.                                                
017500         05  W-IDDC-6334         PIC X(2)    VALUE SPACE.                 
017600     EJECT                                                                
017700                                                                          
017800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
017900 01  DLI-IO-WDK601.                                                       
018000*    03  -COPY WDK601                                                     
018100     EJECT                                                                
018200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
018300 01  DLI-IO-WDK611.                                                       
018400*    03  -COPY WDK611                                                     
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D2'.                        
018600     EJECT                                                                
018700 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D201'.                      
018800 01  DLI-IO-W6D201.                                                       
018900*    03  -COPY W6D201                                                     
019000     EJECT                                                                
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D211'.                      
019200 01  DLI-IO-W6D211.                                                       
019300*    03  -COPY W6D211                                                     
019400     EJECT                                                                
019500 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D221'.                      
019600 01  DLI-IO-W6D221.                                                       
019700*    03  -COPY W6D221                                                     
019800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
019900 01  DLI-IO-WDK701.                                                       
020000*    03  -COPY WDK701                                                     
020100     EJECT                                                                
020200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
020300 01  DLI-IO-WDK711.                                                       
020400*    03  -COPY WDK711                                                     
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6331'.                    
020600 01  DLI-IO-WDGX6331.                                                     
020700*    03  -COPY WDGX6331                                                   
020800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6332'.                    
020900 01  DLI-IO-WDGX6332.                                                     
021000*    03  -COPY WDGX6332                                                   
021100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6334'.                    
021200 01  DLI-IO-WDGX6334.                                                     
021300*    03  -COPY WDGX6334                                                   
021400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
021500 01   DLI-IO-AREA-B601.                                                   
021600*     03  -COPY WDB601                                                    
021700                                                                          
021800     EJECT                                                                
021900 LINKAGE SECTION.                                                         
022000*01  -COPY W0009   -PRE MSG-                                              
022100                                                                          
022200*01  -COPY W0008   -PRE W6D2A-                                            
022300     05  FILLER                  PIC X.                                   
022400                                                                          
022500*01  -COPY W0008   -PRE W6D2B-                                            
022600     05  FILLER                  PIC X.                                   
022700                                                                          
022800*01  -COPY W0008   -PRE WDK7-                                             
022900     05  FILLER                  PIC X.                                   
023000*01  -COPY W0008   -PRE WDK6-                                             
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300*01  -COPY W0008   -PRE WDB6-                                             
023400     05  FILLER                  PIC X.                                   
023500*01  -COPY W0008   -PRE WDR2-                                             
023600     05  FILLER                  PIC X.                                   
023700*01  -COPY W0008   -PRE WDR2ALT-                                          
023800     05  FILLER                  PIC X.                                   
023900     EJECT                                                                
024000 PROCEDURE DIVISION  USING MSG-PCB  W6D2A-PCB W6D2B-PCB                   
024100                           WDK7-PCB WDK6-PCB  WDB6-PCB WDR2-PCB           
024200                           WDR2ALT-PCB.                                   
024300     ENTRY 'DLITCBL' USING MSG-PCB  W6D2A-PCB W6D2B-PCB                   
024400                           WDK7-PCB WDK6-PCB  WDB6-PCB WDR2-PCB           
024500                           WDR2ALT-PCB.                                   
024600                                                                          
024700                                                                          
024800     PERFORM S01-HAEMTA-ANROPSDATA                                        
024900     IF SUB-KDRC = 0                                                      
025000       PERFORM A-INIT                                                     
025100       PERFORM B-KOLLA-NYCKLAR                                            
025200       PERFORM BA-KOLLA-IDDC-TRAD                                         
025300       IF NYCKLAR-OK                                                      
025400         IF REQU-KDPGMACT = 'S' OR 'P' OR 'F'                             
025500           MOVE SPACE TO RESP-IN-RAD                                      
025600           IF REQU-KDPGMACT = 'P'                                         
025700             PERFORM D-NAESTA-SIDA                                        
025800           END-IF                                                         
025900           IF REQU-KDPGMACT = 'F'                                         
026000             PERFORM C-FOERSTA-SIDA                                       
026100           END-IF                                                         
026200           IF LEVEL-MISSING                                               
026300             PERFORM I-SHOW-MISSING-DC                                    
026400           ELSE                                                           
026500             PERFORM F-LAES-VISA-INFO                                     
026600           END-IF                                                         
026700         END-IF                                                           
026800*                                                                         
026900         IF REQU-KDPGMACT = 'E'                                           
027000           PERFORM G-KOLLA-INDATA                                         
027100           IF INDATA-OK                                                   
027200             PERFORM H-UPPDATERA                                          
027300             IF RESP-IDMSG-ERROR = SPACE                                  
027400               MOVE SPACE TO RESP-IN-RAD                                  
027500               PERFORM B-KOLLA-NYCKLAR                                    
027600               PERFORM F-LAES-VISA-INFO                                   
027700             ELSE                                                         
027800               MOVE REQU-KVRADER   TO RESP-KVRADER                        
027900               MOVE ALL '+' TO RESP-WL0137O1                              
028000             END-IF                                                       
028100           ELSE                                                           
028200             IF REQU-KVRADER NUMERIC                                      
028300               MOVE REQU-KVRADER   TO RESP-KVRADER                        
028400               MOVE ALL '+' TO RESP-WL0137O1                              
028500             ELSE                                                         
028600               MOVE ZERO           TO RESP-KVRADER                        
028700             END-IF                                                       
028800           END-IF                                                         
028900         END-IF                                                           
029000       END-IF                                                             
029100*      IF REQU-KDPGMACT = 'P'                                             
029200*        CALL FELLOG                                                      
029300*      END-IF                                                             
029400       PERFORM S02-RETURNERA-SVAR                                         
029500     END-IF                                                               
029600     MOVE ZERO TO RETURN-CODE                                             
029700     GOBACK                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 A-INIT SECTION.                                                          
030100     ACCEPT WS-DAGENS-DATUM  FROM TIME                                    
030200     MOVE ALL '+' TO RESP-AREA                                            
030300     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
030400                     RESP-IDMSG-INFO                                      
030500                     RESP-IDELMT-ERROR                                    
030600     MOVE ZERO    TO RESP-KVRADER                                         
030700     MOVE '001'   TO RESP-IDMSGVER                                        
030800                                                                          
030900     .                                                                    
031000     EJECT                                                                
031100 B-KOLLA-NYCKLAR SECTION.                                                 
031200                                                                          
031300     MOVE JA TO NYCKLAR-SW                                                
031400                                                                          
031500     IF REQU-IDARTNR-KEY NUMERIC                                          
031600       MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                 
031700                                RESP-IDARTNR-KEY                          
031800     ELSE                                                                 
031900       MOVE NEJ TO NYCKLAR-SW                                             
032000       MOVE 'IDARTNR'   TO RESP-IDELMT-ERROR                              
032100     END-IF                                                               
032200                                                                          
032300     IF REQU-IDKVAINF-KEY NOT = ALL '+'                                   
032400       IF REQU-IDKVAINF-KEY NUMERIC                                       
032500         IF REQU-KDPGMACT = 'P' OR 'F'                                    
032600           MOVE ZERO              TO WS-IDKVAINF                          
032700         ELSE                                                             
032800           MOVE REQU-IDKVAINF-KEY TO WS-IDKVAINF                          
032900                                     W-IDKVAINF                           
033000                                     RESP-IDKVAINF-KEY                    
033100         END-IF                                                           
033200       ELSE                                                               
033300         MOVE NEJ TO NYCKLAR-SW                                           
033400         MOVE 'IDKVAINF'  TO RESP-IDELMT-ERROR                            
033500       END-IF                                                             
033600     ELSE                                                                 
033700       MOVE ZERO              TO WS-IDKVAINF                              
033800     END-IF                                                               
033900                                                                          
034000     IF REQU-TIREGDAT-KEY NOT = ALL '+'                                   
034100       IF REQU-TIREGDAT-KEY NUMERIC                                       
034200         MOVE REQU-TIREGDAT-KEY TO WS-TIREGDAT                            
034300                                   RESP-TIREGDAT-KEY                      
034400                                                                          
034500         IF WS-TIREGDAT < 500000                                          
034600           MOVE 20       TO W1-DAREGDAT (1:2)                             
034700         ELSE                                                             
034800           IF WS-TIREGDAT < 999999                                        
034900             MOVE 19     TO W1-DAREGDAT (1:2)                             
035000           ELSE                                                           
035100             MOVE 99999999 TO W1-DAREGDAT                                 
035200           END-IF                                                         
035300         END-IF                                                           
035400         COMPUTE W-DAREGDAT-9KOMPL = 99999999 - W1-DAREGDAT               
035500       ELSE                                                               
035600         MOVE NEJ TO NYCKLAR-SW                                           
035700         MOVE 'TIREGDAT'  TO RESP-IDELMT-ERROR                            
035800       END-IF                                                             
035900     ELSE                                                                 
036000       MOVE ZERO              TO WS-TIREGDAT                              
036100     END-IF                                                               
036200                                                                          
036300     IF REQU-IDDC-KEY NOT = ALL '+'                                       
036400       MOVE REQU-IDDC-KEY TO  WS-SPAR-IDDC                                
036500                              W-IDDC-B6                                   
036600                              W-IDDC                                      
036700                              RESP-IDDC-KEY                               
036800                                                                          
036900       PERFORM IMS-GU-WDB601                                              
037000       IF SEGMENT-SAKNAS                                                  
037100         MOVE NEJ TO NYCKLAR-SW                                           
037200         MOVE 'IDDC'          TO RESP-IDELMT-ERROR                        
037300       END-IF                                                             
037400     ELSE                                                                 
037500       MOVE NEJ TO NYCKLAR-SW                                             
037600       MOVE 'IDDC'          TO RESP-IDELMT-ERROR                          
037700     END-IF                                                               
037800                                                                          
037900     IF REQU-IDDC2-KEY NOT = ALL '+'                                      
038000       MOVE REQU-IDDC2-KEY TO WS-SPAR-IDDC                                
038100                              W-IDDC-B6                                   
038200                              W-IDDC                                      
038300                              RESP-IDDC2-KEY                              
038400                                                                          
038500       PERFORM IMS-GU-WDB601                                              
038600       IF SEGMENT-SAKNAS                                                  
038700         MOVE NEJ TO NYCKLAR-SW                                           
038800         MOVE 'IDDC'          TO RESP-IDELMT-ERROR                        
038900       END-IF                                                             
039000     ELSE                                                                 
039100       MOVE REQU-IDDC-KEY     TO WS-SPAR-IDDC                             
039200                                 W-IDDC-B6                                
039300                                 W-IDDC                                   
039400                                                                          
039500       PERFORM IMS-GU-WDB601                                              
039600       IF SEGMENT-SAKNAS                                                  
039700         MOVE NEJ TO NYCKLAR-SW                                           
039800         MOVE 'IDDC'          TO RESP-IDELMT-ERROR                        
039900       END-IF                                                             
040000                                                                          
040100     END-IF                                                               
040200                                                                          
040300     IF REQU-SHOW-ALL-KEY NOT = ALL '+'                                   
040400       IF REQU-SHOW-ALL-KEY = 'J' OR 'Y' OR 'N'                           
040500         IF REQU-SHOW-ALL-KEY = 'J' OR 'Y'                                
040600           MOVE 'Y'                TO WS-SHOW-ALL                         
040700                                      RESP-SHOW-ALL-KEY                   
040800         ELSE                                                             
040900           MOVE 'N'                TO RESP-SHOW-ALL-KEY                   
041000         END-IF                                                           
041100       ELSE                                                               
041200         MOVE NEJ TO NYCKLAR-SW                                           
041300         MOVE 'SHOWALL'   TO RESP-IDELMT-ERROR                            
041400       END-IF                                                             
041500     ELSE                                                                 
041600       MOVE 'N'           TO RESP-SHOW-ALL-KEY                            
041700     END-IF                                                               
041800                                                                          
041900     IF NYCKLAR-FEL                                                       
042000       MOVE '022'        TO RESP-IDMSG-ERROR                              
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 BA-KOLLA-IDDC-TRAD SECTION.                                              
042500     MOVE NEJ TO LEVEL2-SW                                                
042600**** KOLLA OM PROF-IDDC = EN LEVEL2, FÅR UPPDATERA ALLA LEVEL3            
042700     MOVE REQU-IDDC-KEY TO W-IDDC-6332                                    
042800     PERFORM IMS-GHU-WDGX6332                                             
042900     IF SEGMENT-FINNS                                                     
043000       MOVE JA TO LEVEL2-SW                                               
043100       MOVE W-IDDC-6332 TO WS-SPAR-IDDC-6332                              
043200     END-IF                                                               
043300*** KOLLA W-IDDC SOM KAN VARA PROD ELLER INMATAT DC                       
043400     PERFORM IMS-GET-WDGX6331                                             
043500     IF SEGMENT-FINNS                                                     
043600                                                                          
043700       MOVE W-IDDC        TO W-IDDC-6332                                  
043800       PERFORM IMS-GHU-WDGX6332                                           
043900       IF SEGMENT-FINNS                                                   
044000         MOVE W-IDDC-6332 TO WS-SPAR-IDDC-6332                            
044100         MOVE '20' TO LEVEL-SW                                            
044200*** IDDC INMAMTAT                                                         
044300         IF REQU-IDDC-KEY NOT = W-IDDC                                    
044400           IF 6332-IDUSER(1) NOT = SPACE                                  
044500             IF REQU-IDUSER NOT = 6332-IDUSER(1)                          
044600               MOVE '30' TO LEVEL-SW                                      
044700             ELSE                                                         
044800               MOVE '20' TO LEVEL-SW                                      
044900             END-IF                                                       
045000           ELSE                                                           
045100             MOVE '30' TO LEVEL-SW                                        
045200           END-IF                                                         
045300           IF LEVEL3-UTAN-USER                                            
045400             IF 6332-IDUSER(2) NOT = SPACE                                
045500               IF REQU-IDUSER = 6332-IDUSER(2)                            
045600                 MOVE '20' TO LEVEL-SW                                    
045700               END-IF                                                     
045800             END-IF                                                       
045900           END-IF                                                         
046000           IF LEVEL3-UTAN-USER                                            
046100             IF 6332-IDUSER(3) NOT = SPACE                                
046200               IF REQU-IDUSER = 6332-IDUSER(3)                            
046300                 MOVE '20' TO LEVEL-SW                                    
046400               END-IF                                                     
046500             END-IF                                                       
046600           END-IF                                                         
046700           IF LEVEL3-UTAN-USER                                            
046800             IF 6332-IDUSER(4) NOT = SPACE                                
046900               IF REQU-IDUSER = 6332-IDUSER(4)                            
047000                 MOVE '20' TO LEVEL-SW                                    
047100               END-IF                                                     
047200             END-IF                                                       
047300           END-IF                                                         
047400           IF LEVEL3-UTAN-USER                                            
047500             MOVE NEJ TO NYCKLAR-SW                                       
047600             MOVE 'IDDC'          TO RESP-IDELMT-ERROR                    
047700             MOVE '023'           TO RESP-IDMSG-ERROR                     
047800           END-IF                                                         
047900         END-IF                                                           
048000       ELSE                                                               
048100                                                                          
048200**  REQU-IDDC ÄR EN LEVEL 3 , HITTA VILKET DC SOM ÄR LEVEL2               
048300**      IF LEVEL-3-IDDC                                                   
048400          PERFORM IMS-GET-WDGX6331                                        
048500          PERFORM IMS-GNP-WDGX6332                                        
048600******    MOVE REQU-IDDC-KEY TO W-IDDC-6334                               
048700          MOVE W-IDDC        TO W-IDDC-6334                               
048800                                                                          
048900          PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                      
049000            IF SEGMENT-FINNS                                              
049100              MOVE 6332-IDDC   TO W-IDDC-6332                             
049200              MOVE W-IDDC-6332 TO WS-SPAR-IDDC-6332                       
049300              PERFORM IMS-GET-WDGX6334-UNIK                               
049400              IF SEGMENT-FINNS                                            
049500                 MOVE JA TO POST-FINNS-SW                                 
049600                 IF 6332-IDUSER(1) NOT = SPACE                            
049700                   IF REQU-IDUSER NOT = 6332-IDUSER(1)                    
049800                     MOVE '30' TO LEVEL-SW                                
049900                   ELSE                                                   
050000                     MOVE '31' TO LEVEL-SW                                
050100                   END-IF                                                 
050200                 ELSE                                                     
050300                   MOVE '30' TO LEVEL-SW                                  
050400                 END-IF                                                   
050500                 IF LEVEL3-UTAN-USER                                      
050600                   IF 6332-IDUSER(2) NOT = SPACE                          
050700                     IF REQU-IDUSER = 6332-IDUSER(2)                      
050800                       MOVE '31' TO LEVEL-SW                              
050900                     END-IF                                               
051000                   END-IF                                                 
051100                 END-IF                                                   
051200                 IF LEVEL3-UTAN-USER                                      
051300                   IF 6332-IDUSER(3) NOT = SPACE                          
051400                     IF REQU-IDUSER  = 6332-IDUSER(3)                     
051500                       MOVE '31' TO LEVEL-SW                              
051600                     END-IF                                               
051700                   END-IF                                                 
051800                 END-IF                                                   
051900                 IF LEVEL3-UTAN-USER                                      
052000                   IF 6332-IDUSER(4) NOT = SPACE                          
052100                     IF REQU-IDUSER  = 6332-IDUSER(4)                     
052200                       MOVE '31' TO LEVEL-SW                              
052300                     END-IF                                               
052400                   END-IF                                                 
052500                 END-IF                                                   
052600                                                                          
052700                 IF REQU-IDDC-KEY NOT = W-IDDC-6332                       
052800                   IF W-IDDC-6334 NOT = REQU-IDDC-KEY                     
052900                     IF REQU-IDDC-KEY NOT = W-IDDC-6332                   
053000**                     LEVEL2 DC INTE LIKA MED PROFIL DC                  
053100**                     LEVEL3 HAR INTE RÄTT LEVEL2 IDDC                   
053200                         IF LEVEL3-UTAN-USER                              
053300                           MOVE NEJ TO NYCKLAR-SW                         
053400                           MOVE 'IDDC'       TO RESP-IDELMT-ERROR         
053500                           MOVE '023'        TO RESP-IDMSG-ERROR          
053600                         END-IF                                           
053700                     END-IF                                               
053800                     IF REQU-IDDC-KEY NOT = W-IDDC-6334                   
053900**                   PROF-IDDC INTE LIKA MED LEVEL3 IDDC                  
054000                       IF LEVEL3-UTAN-USER                                
054100                         MOVE NEJ TO NYCKLAR-SW                           
054200                         MOVE 'IDDC'          TO RESP-IDELMT-ERROR        
054300                         MOVE '023'           TO RESP-IDMSG-ERROR         
054400                       END-IF                                             
054500                     END-IF                                               
054600                   END-IF                                                 
054700                 END-IF                                                   
054800              ELSE                                                        
054900                PERFORM IMS-GNP-WDGX6332                                  
055000              END-IF                                                      
055100            END-IF                                                        
055200          END-PERFORM                                                     
055300          IF POST-SAKNAS                                                  
055400            MOVE 'M' TO LEVEL2-SW                                         
055500********    LEVEL MISSING                                                 
055600*           MOVE NEJ  TO NYCKLAR-SW                                       
055700*           MOVE 'IDDC'          TO RESP-IDELMT-ERROR                     
055800*           MOVE '023'           TO RESP-IDMSG-ERROR                      
055900          END-IF                                                          
056000**      END-IF                                                            
056100       END-IF                                                             
056200     END-IF                                                               
056300     .                                                                    
056400     EJECT                                                                
056500 C-FOERSTA-SIDA SECTION.                                                  
056600                                                                          
056700*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
056800     MOVE ZERO                     TO RESP-TIREGDAT-NEXT                  
056900     MOVE ZERO                     TO RESP-TIKLOCK-NEXT                   
057000                                                                          
057100     MOVE ZERO                      TO W-DAREGDAT-9KOMPL                  
057200     MOVE ZERO                      TO W-TIKLOCK-9KOMPL                   
057300     MOVE ZERO                      TO WS-TIREGDAT                        
057400     .                                                                    
057500     EJECT                                                                
057600 D-NAESTA-SIDA SECTION.                                                   
057700                                                                          
057800     MOVE REQU-TIREGDAT-NEXT       TO W-DAREGDAT-9KOMPL                   
057900     IF REQU-TIREGDAT-NEXT (1:1) = 0                                      
058000       MOVE 80                     TO W-DAREGDAT-9KOMPL (1:2)             
058100     ELSE                                                                 
058200       MOVE 79                     TO W-DAREGDAT-9KOMPL (1:2)             
058300     END-IF                                                               
058400     MOVE REQU-TIKLOCK-NEXT        TO W-TIKLOCK-9KOMPL                    
058500                                                                          
058600     .                                                                    
058700     EJECT                                                                
058800 F-LAES-VISA-INFO SECTION.                                                
058900       MOVE 'F-LAES-VISA-INFO   ' TO WS-SECTION                           
059000       PERFORM IMS-GU-W6D201                                              
059100                                                                          
059200       IF SEGMENT-FINNS                                                   
059300         MOVE LOW-VALUE            TO W-IDKVAINF-MIN-X                    
059400         MOVE HIGH-VALUE           TO W-IDKVAINF-MAX-X                    
059500         IF WS-IDKVAINF > ZERO                                            
059600            MOVE W-IDKVAINF        TO W-IDKVAINF-MIN                      
059700                                      W-IDKVAINF-MAX                      
059800            MOVE ZERO              TO W-DAREGDAT-9KOMPL                   
059900         END-IF                                                           
060000                                                                          
060100         PERFORM FA-LAES-W6D211                                           
060200       ELSE                                                               
060300         MOVE '027'           TO  RESP-IDMSG-ERROR                        
060400       END-IF                                                             
060500     .                                                                    
060600     EJECT                                                                
060700                                                                          
060800 FA-LAES-W6D211    SECTION.                                               
060900     MOVE 'FA-LAES-W6D211     ' TO WS-SECTION                             
061000                                                                          
061100     PERFORM IMS-GNP-W6D211                                               
061200                                                                          
061300     IF SEGMENT-FINNS                                                     
061400       IF INFO-IDKVAINF = 99                                              
061500         PERFORM IMS-GNP-W6D211                                           
061600       END-IF                                                             
061700     END-IF                                                               
061800                                                                          
061900     IF SEGMENT-FINNS                                                     
062000       MOVE INFO-DAREGDAT-9KOMPL      TO W-DAREGDAT-9KOMPL                
062100       MOVE INFO-DAREGDAT-9KOMPL(2:7) TO RESP-TIREGDAT-NEXT               
062200       MOVE INFO-TIKLOCK-9KOMPL       TO W-TIKLOCK-9KOMPL                 
062300                                         RESP-TIKLOCK-NEXT                
062400                                                                          
062500       COMPUTE W1-DAREGDAT = 99999999 - INFO-DAREGDAT-9KOMPL              
062600       MOVE W1-DAREGDAT (3:6)   TO RESP-TIREGDAT-KEY                      
062700       MOVE INFO-IDKVAINF       TO RESP-IDKVAINF-KEY                      
062800                                                                          
062900       PERFORM FB-LAES-DCDATA                                             
063000     ELSE                                                                 
063100       MOVE '027'           TO  RESP-IDMSG-ERROR                          
063200     END-IF                                                               
063300                                                                          
063400***  PERFORM FC-KOLLA-INTERN-NOT                                          
063500                                                                          
063600     MOVE LOW-VALUE               TO W-IDKVAINF-MIN-X                     
063700     PERFORM IMS-GNP-W6D211                                               
063800                                                                          
063900                                                                          
064000     IF SEGMENT-FINNS                                                     
064100       MOVE INFO-DAREGDAT-9KOMPL(2:7) TO RESP-TIREGDAT-NEXT               
064200       MOVE INFO-TIKLOCK-9KOMPL       TO RESP-TIKLOCK-NEXT                
064300       IF REQU-KDPGMACT = 'S' OR 'P' OR 'F'                               
064400         MOVE '011'                   TO RESP-IDMSG-INFO                  
064500       END-IF                                                             
064600     END-IF                                                               
064700     .                                                                    
064800     EJECT                                                                
064900 FB-LAES-DCDATA SECTION.                                                  
065000*    LÄS DC TRÄD INFO OM LEVEL 2 ELLER 3                                  
065100*    HÄMTA DC TRÄD INFO FLYTTA DC TILL NYCKEL                             
065200                                                                          
065300     MOVE 'FB-LAES-DCDATA     ' TO WS-SECTION                             
065400     PERFORM FBA-LAES-DC-TRAD                                             
065500     IF POST-LEVEL-2 OR LEVEL-2-IDDC                                      
065600****    (POST-LEVEL-3 AND WS-SHOW-ALL = 'Y')                              
065700       MOVE +1 TO INDX                                                    
065800       MOVE INDX TO  RESP-KVRADER                                         
065900       MOVE 6332-IDDC TO W-IDDC                                           
066000       PERFORM IMS-GU-WDK711                                              
066100       MOVE W-IDDC            TO RESP-IDDC(INDX)                          
066200       MOVE SLAG-KVLS         TO RESP-KVLS(INDX)                          
066300       MOVE SLAG-KDLEVSP      TO RESP-KDLEVSP(INDX)                       
066400       MOVE SLAG-KVSPARR-KVAL TO RESP-KVSPARR-KVAL(INDX)                  
066500                                                                          
066600       PERFORM IMS-GU-W6D221                                              
066700       IF SEGMENT-FINNS                                                   
066800         MOVE DCQ-TISTADAT          TO RESP-TISTADAT(INDX)                
066900         MOVE DCQ-TISTODAT          TO RESP-TISTODAT(INDX)                
067000         MOVE DCQ-KVANTAL           TO RESP-KVANTAL(INDX)                 
067100         MOVE DCQ-KVAVV-KVAL        TO RESP-KVAVV-KVAL(INDX)              
067200         MOVE DCQ-KVART-SKROT       TO RESP-KVART-SKROT(INDX)             
067300         MOVE DCQ-KVART-RET         TO RESP-KVART-RET(INDX)               
067400         MOVE DCQ-KVART-KJUST       TO RESP-KVART-KJUST(INDX)             
067500         MOVE DCQ-BEINIT            TO RESP-BEINIT(INDX)                  
067600       END-IF                                                             
067700       PERFORM FC-KOLLA-INTERN-NOT                                        
067800     END-IF                                                               
067900     IF WS-SHOW-ALL = 'Y' OR POST-LEVEL-3                                 
068000      IF POST-LEVEL-3                                                     
068100*                                                                         
068200*      IF WS-SHOW-ALL = 'Y'                                               
068300*        MOVE 'Y OCH IMS-GET-WDGX6331' TO WS-SECTION                      
068400*        PERFORM IMS-GET-WDGX6331                                         
068500*        MOVE 6332-IDDC TO W-IDDC-6332                                    
068600*        MOVE 'Y OCH IMS-GHU-WDGX6332' TO WS-SECTION                      
068700*        PERFORM IMS-GHU-WDGX6332                                         
068800*        MOVE 'Y OCH IMS-GNP-WDGX6334' TO WS-SECTION                      
068900*        PERFORM IMS-GNP-WDGX6334                                         
069000*        MOVE 6334-IDDC TO W-IDDC                                         
069100*      ELSE                                                               
069200         MOVE 6334-IDDC TO W-IDDC                                         
069300         PERFORM IMS-GET-WDGX6334-UNIK                                    
069400*      END-IF                                                             
069500      ELSE                                                                
069600       PERFORM IMS-GNP-WDGX6334                                           
069700       MOVE 6334-IDDC TO W-IDDC                                           
069800      END-IF                                                              
069900                                                                          
070000*     PERFORM IMS-GU-WDK711                                               
070100      IF SEGMENT-FINNS                                                    
070200       ADD +1 TO INDX                                                     
070300       MOVE INDX TO  RESP-KVRADER                                         
070400      END-IF                                                              
070500      PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                     
070600        PERFORM IMS-GU-WDK711                                             
070700*       IF SEGMENT-FINNS                                                  
070800*        ADD +1 TO INDX                                                   
070900*        MOVE INDX TO  RESP-KVRADER                                       
071000*       END-IF                                                            
071100       IF SEGMENT-FINNS                                                   
071200         MOVE W-IDDC            TO RESP-IDDC(INDX)                        
071300         MOVE SLAG-KVLS         TO RESP-KVLS(INDX)                        
071400         MOVE SLAG-KDLEVSP      TO RESP-KDLEVSP(INDX)                     
071500         MOVE SLAG-KVSPARR-KVAL TO RESP-KVSPARR-KVAL(INDX)                
071600                                                                          
071700*        LÄS MED DEN HÄMTADE DC TRÄD INFO                                 
071800         PERFORM IMS-GU-W6D221                                            
071900         IF SEGMENT-FINNS                                                 
072000           MOVE DCQ-TISTADAT          TO RESP-TISTADAT(INDX)              
072100           MOVE DCQ-TISTODAT          TO RESP-TISTODAT(INDX)              
072200           MOVE DCQ-KVANTAL           TO RESP-KVANTAL(INDX)               
072300           MOVE DCQ-KVAVV-KVAL        TO RESP-KVAVV-KVAL(INDX)            
072400           MOVE DCQ-KVART-SKROT       TO RESP-KVART-SKROT(INDX)           
072500           MOVE DCQ-KVART-RET         TO RESP-KVART-RET(INDX)             
072600           MOVE DCQ-KVART-KJUST       TO RESP-KVART-KJUST(INDX)           
072700           MOVE DCQ-BEINIT            TO RESP-BEINIT(INDX)                
072800         END-IF                                                           
072900       END-IF                                                             
073000       PERFORM FC-KOLLA-INTERN-NOT                                        
073100                                                                          
073200*                                                                         
073300       IF POST-LEVEL-3                                                    
073400*        IF WS-SHOW-ALL = 'Y'                                             
073500*          PERFORM IMS-GNP-WDGX6334                                       
073600*        ELSE                                                             
073700           MOVE 'GE' TO STATUS-WS                                         
073800*        END-IF                                                           
073900       ELSE                                                               
074000         IF WS-SHOW-ALL = 'Y'                                             
074100           PERFORM IMS-GNP-WDGX6334                                       
074200         ELSE                                                             
074300           MOVE 'GE' TO STATUS-WS                                         
074400         END-IF                                                           
074500       END-IF                                                             
074600       IF SEGMENT-FINNS                                                   
074700         MOVE 6334-IDDC TO W-IDDC                                         
074800*        PERFORM IMS-GU-WDK711                                            
074900*        IF SEGMENT-FINNS                                                 
075000           ADD +1 TO INDX                                                 
075100           MOVE W-IDDC TO RESP-IDDC (INDX)                                
075200           MOVE INDX TO  RESP-KVRADER                                     
075300*        END-IF                                                           
075400       END-IF                                                             
075500                                                                          
075600      END-PERFORM                                                         
075700     END-IF                                                               
075800     .                                                                    
075900     EJECT                                                                
076000 FBA-LAES-DC-TRAD SECTION.                                                
076100*                                                                         
076200     MOVE NEJ TO POST-FINNS-SW                                            
076300                 POST-LEVEL-SW                                            
076400     PERFORM IMS-GET-WDGX6331                                             
076500     IF SEGMENT-FINNS                                                     
076600       MOVE WS-SPAR-IDDC TO W-IDDC-6332                                   
076700       PERFORM IMS-GHU-WDGX6332                                           
076800       IF SEGMENT-SAKNAS                                                  
076900         PERFORM IMS-GET-WDGX6331                                         
077000         PERFORM IMS-GNP-WDGX6332                                         
077100         PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                       
077200           MOVE 6332-IDDC TO W-IDDC-6332                                  
077300                             W-IDDC                                       
077400           MOVE WS-SPAR-IDDC TO W-IDDC-6334                               
077500           PERFORM IMS-GET-WDGX6334-UNIK                                  
077600           IF SEGMENT-FINNS                                               
077700             MOVE 6334-IDDC TO W-IDDC-6334                                
077800             MOVE JA TO POST-FINNS-SW                                     
077900                        POST-LEVEL-SW                                     
078000           ELSE                                                           
078100             MOVE ' IMS-GNP-WDGX6332 LOOP  ' TO WS-SECTION                
078200             PERFORM IMS-GNP-WDGX6332                                     
078300           END-IF                                                         
078400         END-PERFORM                                                      
078500                                                                          
078600         MOVE 6332-IDDC TO W-IDDC W-IDDC-6332                             
078700                                                                          
078800       ELSE                                                               
078900         MOVE 6332-IDDC TO W-IDDC W-IDDC-6332                             
079000       END-IF                                                             
079100     ELSE                                                                 
079200       MOVE 'SEG WDGX6331 SAKNAS'    TO RESP-IDELMT-ERROR                 
079300       MOVE '027'                    TO RESP-IDMSG-ERROR                  
079400     END-IF                                                               
079500     .                                                                    
079600     EJECT                                                                
079700 FC-KOLLA-INTERN-NOT SECTION.                                             
079800     MOVE 'FC-KOLLA-INTERN    ' TO WS-SECTION                             
079900     MOVE 99 TO W-IDKVAINF2                                               
080000     MOVE +1 TO FC-INDX                                                   
080100     PERFORM IMS-GU-W6D211-B                                              
080200     IF SEGMENT-FINNS                                                     
080300       PERFORM IMS-GNP-W6D221                                             
080400       PERFORM UNTIL SEGMENT-SAKNAS OR FC-INDX > MAX-INDX                 
080500         IF DCQ-IDDC = W-IDDC                                             
080600           MOVE 'X' TO RESP-FL-DC-INFO(INDX)                              
080700           ADD +501  TO FC-INDX                                           
080800         ELSE                                                             
080900           ADD +1   TO FC-INDX                                            
081000           PERFORM IMS-GNP-W6D221                                         
081100         END-IF                                                           
081200       END-PERFORM                                                        
081300     END-IF                                                               
081400     .                                                                    
081500     EJECT                                                                
081600 G-KOLLA-INDATA SECTION.                                                  
081700     MOVE JA  TO INDATA-SW                                                
081800                                                                          
081900     IF REQU-IDDC-IN        = ALL '+' AND                                 
082000        REQU-TISTADAT-IN    = ALL '+' AND                                 
082100        REQU-TISTODAT-IN    = ALL '+' AND                                 
082200        REQU-KVANTAL-IN     = ALL '+' AND                                 
082300        REQU-KVAVV-KVAL-IN  = ALL '+' AND                                 
082400        REQU-KVART-SKROT-IN = ALL '+' AND                                 
082500        REQU-KVART-RET-IN   = ALL '+' AND                                 
082600        REQU-KVART-KJUST-IN = ALL '+' AND                                 
082700        REQU-BEINIT-IN      = ALL '+'                                     
082800                                                                          
082900       MOVE '014'                TO RESP-IDMSG-ERROR                      
083000       MOVE 'RAD'                TO RESP-IDELMT-ERROR                     
083100       MOVE NEJ TO INDATA-SW                                              
083200     ELSE                                                                 
083300                                                                          
083400       IF REQU-IDDC-IN = ALL '+'                                          
083500         MOVE '023'              TO RESP-IDMSG-ERROR                      
083600         MOVE 'IDDC1'             TO RESP-IDELMT-ERROR                    
083700       ELSE                                                               
083800         MOVE REQU-IDDC-IN TO W-IDDC                                      
083900         IF DCS-IDDC NOT = REQU-IDDC-IN                                   
084000            MOVE REQU-IDDC-IN TO W-IDDC-B6                                
084100            PERFORM IMS-GU-WDB601                                         
084200         END-IF                                                           
084300                                                                          
084400         IF DCS-KDDC = SPACE OR DCS-DDC                                   
084500           MOVE '023'              TO RESP-IDMSG-ERROR                    
084600           MOVE 'IDDC2'             TO RESP-IDELMT-ERROR                  
084700           MOVE NEJ TO INDATA-SW                                          
084800         END-IF                                                           
084900       END-IF                                                             
085000                                                                          
085100       IF INDATA-OK                                                       
085200         IF DCS-CDC                                                       
085300           PERFORM IMS-GU-WDK611                                          
085400         ELSE                                                             
085500           PERFORM IMS-GU-WDK711                                          
085600         END-IF                                                           
085700                                                                          
085800         IF SEGMENT-SAKNAS                                                
085900           MOVE '027'                     TO RESP-IDMSG-ERROR             
086000           MOVE 'IDDCK6K7'                TO RESP-IDELMT-ERROR            
086100           MOVE NEJ                       TO INDATA-SW                    
086200         ELSE                                                             
086300           PERFORM IMS-GU-W6D211                                          
086400           IF SEGMENT-SAKNAS                                              
086500             MOVE '027'                     TO RESP-IDMSG-ERROR           
086600             MOVE 'W6D211'                  TO RESP-IDELMT-ERROR          
086700             MOVE NEJ TO INDATA-SW                                        
086800           END-IF                                                         
086900                                                                          
087000           IF REQU-BEINIT-IN NOT = ALL '+'                                
087100             IF REQU-BEINIT-IN = SPACE                                    
087200               MOVE '023'              TO RESP-IDMSG-ERROR                
087300               MOVE 'BEINIT'           TO RESP-IDELMT-ERROR               
087400               MOVE NEJ TO INDATA-SW                                      
087500             END-IF                                                       
087600           END-IF                                                         
087700                                                                          
087800           IF REQU-TISTADAT-IN NOT = ALL '+'                              
087900             IF REQU-TISTADAT-IN NUMERIC                                  
088000               CONTINUE                                                   
088100             ELSE                                                         
088200               MOVE '023'              TO RESP-IDMSG-ERROR                
088300               MOVE 'TISTADAT'         TO RESP-IDELMT-ERROR               
088400               MOVE NEJ TO INDATA-SW                                      
088500             END-IF                                                       
088600           END-IF                                                         
088700                                                                          
088800           IF REQU-TISTODAT-IN NOT = ALL '+'                              
088900             IF REQU-TISTODAT-IN NUMERIC                                  
089000               CONTINUE                                                   
089100             ELSE                                                         
089200               MOVE '023'              TO RESP-IDMSG-ERROR                
089300               MOVE 'TISTODAT'         TO RESP-IDELMT-ERROR               
089400               MOVE NEJ TO INDATA-SW                                      
089500             END-IF                                                       
089600           END-IF                                                         
089700                                                                          
089800           IF REQU-KVANTAL-IN NOT = ALL '+'                               
089900             IF REQU-KVANTAL-IN NOT NUMERIC                               
090000               MOVE '023'              TO RESP-IDMSG-ERROR                
090100               MOVE 'KVANTAL'          TO RESP-IDELMT-ERROR               
090200               MOVE NEJ TO INDATA-SW                                      
090300             END-IF                                                       
090400           END-IF                                                         
090500                                                                          
090600           IF REQU-KVAVV-KVAL-IN NOT = ALL '+'                            
090700             IF REQU-KVAVV-KVAL-IN NOT NUMERIC                            
090800               MOVE '023'              TO RESP-IDMSG-ERROR                
090900               MOVE 'KVAVV-KVAL'       TO RESP-IDELMT-ERROR               
091000               MOVE NEJ TO INDATA-SW                                      
091100             END-IF                                                       
091200           END-IF                                                         
091300                                                                          
091400           IF REQU-KVART-SKROT-IN NOT = ALL '+'                           
091500             IF REQU-KVART-SKROT-IN NOT NUMERIC                           
091600               MOVE '023'              TO RESP-IDMSG-ERROR                
091700               MOVE 'KVART-SKROT'      TO RESP-IDELMT-ERROR               
091800               MOVE NEJ TO INDATA-SW                                      
091900             END-IF                                                       
092000           END-IF                                                         
092100                                                                          
092200           IF REQU-KVART-RET-IN NOT = ALL '+'                             
092300             IF REQU-KVART-RET-IN NOT NUMERIC                             
092400               MOVE '023'              TO RESP-IDMSG-ERROR                
092500               MOVE 'KVART-RET'        TO RESP-IDELMT-ERROR               
092600               MOVE NEJ TO INDATA-SW                                      
092700             END-IF                                                       
092800           END-IF                                                         
092900                                                                          
093000           IF REQU-KVART-KJUST-IN NOT = ALL '+'                           
093100             IF REQU-KVART-KJUST-IN NOT NUMERIC                           
093200              MOVE '023'              TO RESP-IDMSG-ERROR                 
093300              MOVE 'KVART-KJUST'      TO RESP-IDELMT-ERROR                
093400              MOVE NEJ TO INDATA-SW                                       
093500             END-IF                                                       
093600           END-IF                                                         
093700** KOLLA OM DC FÅR GÖRA UPPDATERING, LÄS DC TRÄD                          
093800           IF INDATA-OK                                                   
093900             IF (WS-SPAR-IDDC  NOT = REQU-IDDC-IN) AND                    
094000                (REQU-IDDC-KEY NOT = REQU-IDDC-IN) AND                    
094100                (REQU-IDDC-KEY NOT = WC-CDC-SE)                           
094200               MOVE REQU-IDDC-IN TO W-IDDC-6332                           
094300               MOVE NEJ TO INDATA-SW                                      
094400               PERFORM GA-KOLLA-DCTRAD                                    
094500             END-IF                                                       
094600           END-IF                                                         
094700* SLUT PÅ DC TRÄD                                                         
094800         END-IF                                                           
094900       END-IF                                                             
095000                                                                          
095100       IF INDATA-FEL                                                      
095200         MOVE '007' TO RESP-IDMSG-ERROR                                   
095300       ELSE                                                               
095400         MOVE NEJ TO UPPDAT-SW                                            
095500         PERFORM IMS-GU-W6D221-2                                          
095600         IF SEGMENT-FINNS                                                 
095700           CONTINUE                                                       
095800         ELSE                                                             
095900           IF DCS-IDDC NOT = REQU-IDDC-KEY                                
096000             IF POST-FINNS AND INDATA-OK                                  
096100               MOVE JA TO UPPDAT-SW                                       
096200             END-IF                                                       
096300           END-IF                                                         
096400           IF DCS-CDC OR UPPDAT-OK                                        
096500             CONTINUE                                                     
096600           ELSE                                                           
096700             MOVE '014'         TO RESP-IDMSG-ERROR                       
096800             MOVE 'IDDC4'       TO RESP-IDELMT-ERROR                      
096900           END-IF                                                         
097000         END-IF                                                           
097100       END-IF                                                             
097200     END-IF                                                               
097300     .                                                                    
097400     EJECT                                                                
097500 GA-KOLLA-DCTRAD SECTION.                                                 
097600     MOVE NEJ TO POST-FINNS-SW                                            
097700     MOVE NEJ TO POST-LEVEL-SW                                            
097800* KOLLA OM IDDC-KEY = LEVEL 2                                             
097900     MOVE REQU-IDDC-KEY TO W-IDDC-6332                                    
098000     PERFORM IMS-GHU-WDGX6332                                             
098100     IF SEGMENT-FINNS                                                     
098200       MOVE JA TO INDATA-SW                                               
098300       MOVE JA TO POST-FINNS-SW                                           
098400     END-IF                                                               
098500*** KOLLA OM INAMATA IDDC ÄR EN LEVEL2 MED RÄTT USER                      
098600     IF POST-SAKNAS                                                       
098700       MOVE REQU-IDDC-IN  TO W-IDDC-6332                                  
098800       PERFORM IMS-GHU-WDGX6332                                           
098900       IF SEGMENT-FINNS                                                   
099000         MOVE JA TO POST-FINNS-SW                                         
099100         IF 6332-IDUSER(1) NOT = SPACE                                    
099200           IF REQU-IDUSER NOT = 6332-IDUSER(1)                            
099300             MOVE NEJ TO INDATA-SW                                        
099400           ELSE                                                           
099500             MOVE JA  TO INDATA-SW                                        
099600           END-IF                                                         
099700         END-IF                                                           
099800         IF INDATA-FEL                                                    
099900           IF 6332-IDUSER(2) NOT = SPACE                                  
100000             IF REQU-IDUSER = 6332-IDUSER(2)                              
100100               MOVE JA  TO INDATA-SW                                      
100200             END-IF                                                       
100300           END-IF                                                         
100400         END-IF                                                           
100500         IF INDATA-FEL                                                    
100600           IF 6332-IDUSER(3) NOT = SPACE                                  
100700             IF REQU-IDUSER  = 6332-IDUSER(3)                             
100800               MOVE JA  TO INDATA-SW                                      
100900             END-IF                                                       
101000           END-IF                                                         
101100         END-IF                                                           
101200         IF INDATA-FEL                                                    
101300           IF 6332-IDUSER(4) NOT = SPACE                                  
101400             IF REQU-IDUSER  = 6332-IDUSER(4)                             
101500               MOVE JA  TO INDATA-SW                                      
101600             END-IF                                                       
101700           END-IF                                                         
101800         END-IF                                                           
101900       END-IF                                                             
102000     END-IF                                                               
102100     IF POST-SAKNAS                                                       
102200** KOLLA OM INMATAT IDDC ÄR LEVEL 3 *****                                 
102300       PERFORM IMS-GET-WDGX6331                                           
102400       IF SEGMENT-FINNS                                                   
102500         MOVE REQU-IDDC-IN TO W-IDDC-6332                                 
102600         PERFORM IMS-GHU-WDGX6332                                         
102700         IF SEGMENT-SAKNAS                                                
102800           PERFORM IMS-GET-WDGX6331                                       
102900           PERFORM IMS-GNP-WDGX6332                                       
103000           PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                     
103100             MOVE 6332-IDDC TO W-IDDC-6332                                
103200                               W-IDDC                                     
103300             MOVE REQU-IDDC-IN TO W-IDDC-6334                             
103400             MOVE ' IMS-GNP-WDGX6334       ' TO WS-SECTION                
103500             PERFORM IMS-GET-WDGX6334-UNIK                                
103600             IF SEGMENT-FINNS                                             
103700               MOVE 6334-IDDC TO W-IDDC-6334                              
103800                                 W-IDDC                                   
103900               MOVE JA TO POST-FINNS-SW                                   
104000                          POST-LEVEL-SW                                   
104100             ELSE                                                         
104200               MOVE ' IMS-GNP-WDGX6332 LOOP  ' TO WS-SECTION              
104300               PERFORM IMS-GNP-WDGX6332                                   
104400             END-IF                                                       
104500           END-PERFORM                                                    
104600                                                                          
104700           MOVE 6332-IDDC TO W-IDDC-6332                                  
104800                                                                          
104900         ELSE                                                             
105000           MOVE 6332-IDDC TO W-IDDC-6332                                  
105100         END-IF                                                           
105200       ELSE                                                               
105300         MOVE 'SEG WDGX6331 SAKNAS'    TO RESP-IDELMT-ERROR               
105400         MOVE '027'                    TO RESP-IDMSG-ERROR                
105500       END-IF                                                             
105600     END-IF                                                               
105700     IF POST-FINNS AND POST-LEVEL-3                                       
105800*     KOLLA OM REQU-IDDC-IN  FINNS PÅ DC-BEN                              
105900      MOVE REQU-IDDC-IN TO W-IDDC-6334                                    
106000      PERFORM IMS-GET-WDGX6334-UNIK                                       
106100      IF SEGMENT-FINNS                                                    
106200*       KOLLA OM IDUSER FÅR UPPDATERA                                     
106300        IF 6332-IDUSER(1) NOT = SPACE                                     
106400          IF REQU-IDUSER NOT = 6332-IDUSER(1)                             
106500            MOVE NEJ TO INDATA-SW                                         
106600          ELSE                                                            
106700            MOVE JA  TO INDATA-SW                                         
106800          END-IF                                                          
106900        END-IF                                                            
107000        IF INDATA-FEL                                                     
107100          IF 6332-IDUSER(2) NOT = SPACE                                   
107200            IF REQU-IDUSER = 6332-IDUSER(2)                               
107300              MOVE JA  TO INDATA-SW                                       
107400            END-IF                                                        
107500          END-IF                                                          
107600        END-IF                                                            
107700        IF INDATA-FEL                                                     
107800          IF 6332-IDUSER(3) NOT = SPACE                                   
107900            IF REQU-IDUSER  = 6332-IDUSER(3)                              
108000              MOVE JA  TO INDATA-SW                                       
108100            END-IF                                                        
108200          END-IF                                                          
108300        END-IF                                                            
108400        IF INDATA-FEL                                                     
108500          IF 6332-IDUSER(4) NOT = SPACE                                   
108600            IF REQU-IDUSER  = 6332-IDUSER(4)                              
108700              MOVE JA  TO INDATA-SW                                       
108800            END-IF                                                        
108900          END-IF                                                          
109000        END-IF                                                            
109100        IF INDATA-FEL                                                     
109200          MOVE '023'     TO RESP-IDMSG-ERROR                              
109300          MOVE 'IDUSER'  TO RESP-IDELMT-ERROR                             
109400        END-IF                                                            
109500                                                                          
109600      ELSE                                                                
109700        MOVE '023'     TO RESP-IDMSG-ERROR                                
109800        MOVE 'IDDC 5'  TO RESP-IDELMT-ERROR                               
109900        MOVE NEJ TO INDATA-SW                                             
110000      END-IF                                                              
110100     END-IF                                                               
110200     .                                                                    
110300     EJECT                                                                
110400 H-UPPDATERA SECTION.                                                     
110500                                                                          
110600     PERFORM IMS-GHU-W6D221                                               
110700                                                                          
110800     IF SEGMENT-FINNS                                                     
110900                                                                          
111000       MOVE INFO-DAREGDAT-9KOMPL TO W-DAREGDAT-9KOMPL                     
111100       MOVE INFO-DAREGDAT-9KOMPL (2:7)                                    
111200                                 TO RESP-TIREGDAT-NEXT                    
111300       MOVE INFO-TIKLOCK-9KOMPL  TO RESP-TIKLOCK-NEXT                     
111400                                    W-TIKLOCK-9KOMPL                      
111500       IF REQU-TISTADAT-IN NOT = ALL '+'                                  
111600         MOVE REQU-TISTADAT-IN TO DCQ-TISTADAT                            
111700       END-IF                                                             
111800                                                                          
111900       IF REQU-TISTODAT-IN NOT = ALL '+'                                  
112000         MOVE REQU-TISTODAT-IN TO DCQ-TISTODAT                            
112100       END-IF                                                             
112200                                                                          
112300       IF REQU-KVANTAL-IN NOT = ALL '+'                                   
112400         MOVE REQU-KVANTAL-IN TO DCQ-KVANTAL                              
112500       END-IF                                                             
112600                                                                          
112700       IF REQU-KVAVV-KVAL-IN NOT = ALL '+'                                
112800         MOVE REQU-KVAVV-KVAL-IN TO DCQ-KVAVV-KVAL                        
112900       END-IF                                                             
113000                                                                          
113100       IF REQU-KVART-SKROT-IN NOT = ALL '+'                               
113200         MOVE REQU-KVART-SKROT-IN TO DCQ-KVART-SKROT                      
113300       END-IF                                                             
113400                                                                          
113500       IF REQU-KVART-RET-IN NOT = ALL '+'                                 
113600         MOVE REQU-KVART-RET-IN TO DCQ-KVART-RET                          
113700       END-IF                                                             
113800                                                                          
113900       IF REQU-KVART-KJUST-IN NOT = ALL '+'                               
114000         MOVE REQU-KVART-KJUST-IN TO DCQ-KVART-KJUST                      
114100       END-IF                                                             
114200                                                                          
114300       IF REQU-BEINIT-IN NOT = ALL '+'                                    
114400         MOVE REQU-BEINIT-IN TO DCQ-BEINIT                                
114500       END-IF                                                             
114600                                                                          
114700       PERFORM IMS-REPL-W6D221                                            
114800       MOVE '001' TO RESP-IDMSG-INFO                                      
114900     ELSE                                                                 
115000       MOVE '027' TO RESP-IDMSG-ERROR                                     
115100     END-IF                                                               
115200                                                                          
115300     .                                                                    
115400     EJECT                                                                
115500 I-SHOW-MISSING-DC  SECTION.                                              
115600       MOVE REQU-IDDC-KEY TO  WS-SPAR-IDDC                                
115700                              W-IDDC                                      
115800                              RESP-IDDC-KEY                               
115900                                                                          
116000       PERFORM IMS-GU-W6D201                                              
116100                                                                          
116200       IF SEGMENT-FINNS                                                   
116300         MOVE LOW-VALUE            TO W-IDKVAINF-MIN-X                    
116400         MOVE HIGH-VALUE           TO W-IDKVAINF-MAX-X                    
116500         IF WS-IDKVAINF > ZERO                                            
116600            MOVE W-IDKVAINF        TO W-IDKVAINF-MIN                      
116700                                      W-IDKVAINF-MAX                      
116800            MOVE ZERO              TO W-DAREGDAT-9KOMPL                   
116900         END-IF                                                           
117000                                                                          
117100         PERFORM IA-LAES-W6D211                                           
117200       ELSE                                                               
117300         MOVE '027'           TO  RESP-IDMSG-ERROR                        
117400       END-IF                                                             
117500     .                                                                    
117600     EJECT                                                                
117700 IA-LAES-W6D211 SECTION.                                                  
117800     MOVE 'IA-LAES-W6D211     ' TO WS-SECTION                             
117900                                                                          
118000     PERFORM IMS-GNP-W6D211                                               
118100                                                                          
118200     IF SEGMENT-FINNS                                                     
118300       IF INFO-IDKVAINF = 99                                              
118400         PERFORM IMS-GNP-W6D211                                           
118500       END-IF                                                             
118600     END-IF                                                               
118700                                                                          
118800     IF SEGMENT-FINNS                                                     
118900       MOVE INFO-DAREGDAT-9KOMPL      TO W-DAREGDAT-9KOMPL                
119000       MOVE INFO-DAREGDAT-9KOMPL(2:7) TO RESP-TIREGDAT-NEXT               
119100       MOVE INFO-TIKLOCK-9KOMPL       TO W-TIKLOCK-9KOMPL                 
119200                                         RESP-TIKLOCK-NEXT                
119300                                                                          
119400       COMPUTE W1-DAREGDAT = 99999999 - INFO-DAREGDAT-9KOMPL              
119500       MOVE W1-DAREGDAT (3:6)   TO RESP-TIREGDAT-KEY                      
119600       MOVE INFO-IDKVAINF       TO RESP-IDKVAINF-KEY                      
119700                                                                          
119800       PERFORM IB-LAES-DCDATA                                             
119900     ELSE                                                                 
120000       MOVE '027'           TO  RESP-IDMSG-ERROR                          
120100     END-IF                                                               
120200                                                                          
120300     MOVE LOW-VALUE               TO W-IDKVAINF-MIN-X                     
120400     PERFORM IMS-GNP-W6D211                                               
120500                                                                          
120600                                                                          
120700     IF SEGMENT-FINNS                                                     
120800       MOVE INFO-DAREGDAT-9KOMPL(2:7) TO RESP-TIREGDAT-NEXT               
120900       MOVE INFO-TIKLOCK-9KOMPL       TO RESP-TIKLOCK-NEXT                
121000       IF REQU-KDPGMACT = 'S' OR 'P' OR 'F'                               
121100         MOVE '011'                   TO RESP-IDMSG-INFO                  
121200       END-IF                                                             
121300     END-IF                                                               
121400     .                                                                    
121500     EJECT                                                                
121600 IB-LAES-DCDATA SECTION.                                                  
121700                                                                          
121800     MOVE +1 TO INDX                                                      
121900     MOVE INDX TO  RESP-KVRADER                                           
122000     MOVE W-IDDC            TO RESP-IDDC(INDX)                            
122100     PERFORM IMS-GU-WDK711                                                
122200     IF SEGMENT-FINNS                                                     
122300       MOVE W-IDDC            TO RESP-IDDC(INDX)                          
122400       MOVE SLAG-KVLS         TO RESP-KVLS(INDX)                          
122500       MOVE SLAG-KDLEVSP      TO RESP-KDLEVSP(INDX)                       
122600       MOVE SLAG-KVSPARR-KVAL TO RESP-KVSPARR-KVAL(INDX)                  
122700     END-IF                                                               
122800     PERFORM IMS-GU-W6D221                                                
122900     IF SEGMENT-FINNS                                                     
123000       MOVE DCQ-TISTADAT          TO RESP-TISTADAT(INDX)                  
123100       MOVE DCQ-TISTODAT          TO RESP-TISTODAT(INDX)                  
123200       MOVE DCQ-KVANTAL           TO RESP-KVANTAL(INDX)                   
123300       MOVE DCQ-KVAVV-KVAL        TO RESP-KVAVV-KVAL(INDX)                
123400       MOVE DCQ-KVART-SKROT       TO RESP-KVART-SKROT(INDX)               
123500       MOVE DCQ-KVART-RET         TO RESP-KVART-RET(INDX)                 
123600       MOVE DCQ-KVART-KJUST       TO RESP-KVART-KJUST(INDX)               
123700       MOVE DCQ-BEINIT            TO RESP-BEINIT(INDX)                    
123800     END-IF                                                               
123900     PERFORM IC-KOLLA-INTERN-NOT                                          
124000     .                                                                    
124100     EJECT                                                                
124200 IC-KOLLA-INTERN-NOT SECTION.                                             
124300     MOVE 99 TO W-IDKVAINF2                                               
124400     MOVE +1 TO FC-INDX                                                   
124500     PERFORM IMS-GU-W6D211-B                                              
124600     IF SEGMENT-FINNS                                                     
124700       PERFORM IMS-GNP-W6D221                                             
124800       PERFORM UNTIL SEGMENT-SAKNAS OR FC-INDX > MAX-INDX                 
124900         IF DCQ-IDDC = W-IDDC                                             
125000           MOVE 'X' TO RESP-FL-DC-INFO(INDX)                              
125100           ADD +501  TO FC-INDX                                           
125200         ELSE                                                             
125300           ADD +1   TO FC-INDX                                            
125400           PERFORM IMS-GNP-W6D221                                         
125500         END-IF                                                           
125600       END-PERFORM                                                        
125700     END-IF                                                               
125800     .                                                                    
125900     EJECT                                                                
126000*    --- DISPATCHER-SEKTIONER                                             
126100 S01-HAEMTA-ANROPSDATA SECTION.                                           
126200                                                                          
126300     MOVE 'GETARG'               TO SUB-KDFUNC                            
126400     MOVE 'CARPARTS.LDC.STOCKCHECKANSWERS'    TO SUB-ADDISPABS            
126500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
126600                                                                          
126700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
126800                                                                          
126900     IF SUB-KDRC > 0                                                      
127000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
127100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
127200       DELIMITED BY SIZE INTO FELTEXT                                     
127300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
127400     END-IF                                                               
127500     .                                                                    
127600     SKIP3                                                                
127700 S02-RETURNERA-SVAR SECTION.                                              
127800                                                                          
127900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
128000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
128100                                                                          
128200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
128300                                                                          
128400     IF SUB-KDRC > 0                                                      
128500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
128600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
128700       DELIMITED BY SIZE INTO FELTEXT                                     
128800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
128900     END-IF                                                               
129000     .                                                                    
129100     EJECT                                                                
129200 IMS-GU-W6D201 SECTION.                                                   
129300                                                                          
129400     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
129500          DELIMITED BY SIZE INTO SSA1                                     
129600     MOVE '  GE' TO GODK-STATUSKODER                                      
129700     CALL CBLTDLI USING GU W6D2A-PCB DLI-IO-W6D201 SSA1                   
129800     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
129900     PERFORM IMS-STATUSKONTROLL                                           
130000     .                                                                    
130100     EJECT                                                                
130200 IMS-GNP-W6D211 SECTION.                                                  
130300                                                                          
130400     STRING 'W6D211  (W6D211KY=>' W-W6D211KY-X                            
130500                    '&IDKVAINF=>' W-IDKVAINF-MIN-X                        
130600                    '&IDKVAINF=<' W-IDKVAINF-MAX-X ')'                    
130700          DELIMITED BY SIZE INTO SSA1                                     
130800     MOVE '  GE' TO GODK-STATUSKODER                                      
130900     CALL CBLTDLI USING GNP W6D2A-PCB DLI-IO-W6D211 SSA1                  
131000     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
131100     PERFORM IMS-STATUSKONTROLL                                           
131200     .                                                                    
131300     SKIP3                                                                
131400 IMS-GU-W6D211 SECTION.                                                   
131500     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
131600          DELIMITED BY SIZE INTO SSA1                                     
131700     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
131800          DELIMITED BY SIZE INTO SSA2                                     
131900     MOVE '  GE' TO GODK-STATUSKODER                                      
132000     CALL CBLTDLI USING GU W6D2A-PCB DLI-IO-W6D211 SSA1 SSA2              
132100     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
132200     PERFORM IMS-STATUSKONTROLL                                           
132300     .                                                                    
132400     SKIP3                                                                
132500 IMS-GU-W6D211-B SECTION.                                                 
132600     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
132700          DELIMITED BY SIZE INTO SSA1                                     
132800     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-2 ')'                        
132900          DELIMITED BY SIZE INTO SSA2                                     
133000     MOVE '  GE' TO GODK-STATUSKODER                                      
133100     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D211 SSA1 SSA2              
133200     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
133300     PERFORM IMS-STATUSKONTROLL                                           
133400     .                                                                    
133500     SKIP3                                                                
133600 IMS-GU-W6D221 SECTION.                                                   
133700                                                                          
133800     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
133900          DELIMITED BY SIZE INTO SSA1                                     
134000     STRING 'W6D211  (W6D211KY =' W-W6D211KY-X ')'                        
134100          DELIMITED BY SIZE INTO SSA2                                     
134200     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
134300          DELIMITED BY SIZE INTO SSA3                                     
134400     MOVE '  GE' TO GODK-STATUSKODER                                      
134500     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3         
134600     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
134700     PERFORM IMS-STATUSKONTROLL                                           
134800     .                                                                    
134900     SKIP3                                                                
135000 IMS-GU-W6D221-2 SECTION.                                                 
135100                                                                          
135200     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
135300          DELIMITED BY SIZE INTO SSA1                                     
135400     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
135500          DELIMITED BY SIZE INTO SSA2                                     
135600     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
135700          DELIMITED BY SIZE INTO SSA3                                     
135800     MOVE '  GE' TO GODK-STATUSKODER                                      
135900     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3         
136000     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
136100     PERFORM IMS-STATUSKONTROLL                                           
136200     .                                                                    
136300     SKIP3                                                                
136400 IMS-GHU-W6D221 SECTION.                                                  
136500                                                                          
136600     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
136700          DELIMITED BY SIZE INTO SSA1                                     
136800     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
136900          DELIMITED BY SIZE INTO SSA2                                     
137000     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
137100          DELIMITED BY SIZE INTO SSA3                                     
137200     MOVE '  GE' TO GODK-STATUSKODER                                      
137300     CALL CBLTDLI USING GHU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3        
137400     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
137500     PERFORM IMS-STATUSKONTROLL                                           
137600     .                                                                    
137700     SKIP3                                                                
137800 IMS-GNP-W6D221 SECTION.                                                  
137900                                                                          
138000     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
138100          DELIMITED BY SIZE INTO SSA1                                     
138200     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-2 ')'                        
138300          DELIMITED BY SIZE INTO SSA2                                     
138400     MOVE 'W6D221 ' TO SSA3                                               
138500     MOVE '  GE' TO GODK-STATUSKODER                                      
138600     CALL CBLTDLI USING GNP W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3        
138700     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
138800     PERFORM IMS-STATUSKONTROLL                                           
138900     .                                                                    
139000     SKIP3                                                                
139100 IMS-REPL-W6D221 SECTION.                                                 
139200                                                                          
139300     MOVE '  ' TO GODK-STATUSKODER                                        
139400     CALL CBLTDLI USING REPL W6D2B-PCB DLI-IO-W6D221                      
139500     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
139600     PERFORM IMS-STATUSKONTROLL                                           
139700     .                                                                    
139800     SKIP3                                                                
139900 IMS-GU-WDK711 SECTION.                                                   
140000                                                                          
140100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
140200          DELIMITED BY SIZE INTO SSA1                                     
140300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
140400          DELIMITED BY SIZE INTO SSA2                                     
140500     MOVE '  GE' TO GODK-STATUSKODER                                      
140600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
140700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
140800     PERFORM IMS-STATUSKONTROLL                                           
140900     .                                                                    
141000     EJECT                                                                
141100 IMS-GU-WDK611 SECTION.                                                   
141200                                                                          
141300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
141400          DELIMITED BY SIZE INTO SSA1                                     
141500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
141600          DELIMITED BY SIZE INTO SSA2                                     
141700     MOVE '  GE' TO GODK-STATUSKODER                                      
141800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
141900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
142000     PERFORM IMS-STATUSKONTROLL                                           
142100     .                                                                    
142200     EJECT                                                                
142300 IMS-GU-WDB601    SECTION.                                                
142400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
142500          DELIMITED BY SIZE INTO SSA1                                     
142600     MOVE '  GE' TO GODK-STATUSKODER                                      
142700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
142800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
142900     PERFORM IMS-STATUSKONTROLL                                           
143000     IF SEGMENT-SAKNAS                                                    
143100         MOVE SPACE TO DCS-KDDC                                           
143200     END-IF                                                               
143300     .                                                                    
143400 IMS-GET-WDGX6331 SECTION.                                                
143500     MOVE 'IMS-GET-WDGX6331        ' TO WS-IMS                            
143600                                                                          
143700     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
143800          DELIMITED BY SIZE INTO SSA1                                     
143900     MOVE '  GE' TO GODK-STATUSKODER                                      
144000     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX6331 SSA1                  
144100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
144200     PERFORM IMS-STATUSKONTROLL                                           
144300     .                                                                    
144400     EJECT                                                                
144500 IMS-GNP-WDGX6331 SECTION.                                                
144600     MOVE 'IMS-GET-WDGX6331        ' TO WS-IMS                            
144700                                                                          
144800*    STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
144900*         DELIMITED BY SIZE INTO SSA1                                     
145000     MOVE '  GE' TO GODK-STATUSKODER                                      
145100     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6332                      
145200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
145300     PERFORM IMS-STATUSKONTROLL                                           
145400     .                                                                    
145500     EJECT                                                                
145600 IMS-GNP-WDGX6332 SECTION.                                                
145700     MOVE 'IMS-GNP-WDGX6332 SECTION' TO WS-IMS                            
145800     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
145900          DELIMITED BY SIZE INTO SSA1                                     
146000                                                                          
146100     MOVE 'WDGX6332   ' TO SSA2                                           
146200     MOVE '  GE' TO GODK-STATUSKODER                                      
146300     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6332 SSA1 SSA2            
146400     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
146500     PERFORM IMS-STATUSKONTROLL                                           
146600     .                                                                    
146700     EJECT                                                                
146800 IMS-GNP-WDGX6332-UNIK SECTION.                                           
146900     MOVE 'IMS-GNP-WDGX6332-UNIK   ' TO WS-IMS                            
147000                                                                          
147100     STRING 'WDR211  (WDGXKEY  =' W-WDGXKEY-6332-X ')'                    
147200          DELIMITED BY SIZE INTO SSA1                                     
147300                                                                          
147400     MOVE 'WDGX6334   ' TO SSA2                                           
147500     MOVE '  GE' TO GODK-STATUSKODER                                      
147600     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6334 SSA1 SSA2            
147700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
147800     PERFORM IMS-STATUSKONTROLL                                           
147900     .                                                                    
148000     EJECT                                                                
148100 IMS-GHU-WDGX6332 SECTION.                                                
148200     MOVE 'IMS-GHU-WDGX6332         ' TO WS-IMS                           
148300                                                                          
148400     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
148500          DELIMITED BY SIZE INTO SSA1                                     
148600     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
148700          DELIMITED BY SIZE INTO SSA2                                     
148800     MOVE '  GE' TO GODK-STATUSKODER                                      
148900     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-WDGX6332 SSA1 SSA2            
149000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
149100     PERFORM IMS-STATUSKONTROLL                                           
149200     .                                                                    
149300     EJECT                                                                
149400 IMS-GET-WDGX6334-UNIK SECTION.                                           
149500     MOVE 'IMS-GET-WDGX6334-UNIK   ' TO WS-IMS                            
149600                                                                          
149700     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
149800          DELIMITED BY SIZE INTO SSA1                                     
149900                                                                          
150000     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
150100          DELIMITED BY SIZE INTO SSA2                                     
150200                                                                          
150300     STRING 'WDGX6334(IDDC     =' W-WDGXKEY-6334-X ')'                    
150400          DELIMITED BY SIZE INTO SSA3                                     
150500                                                                          
150600     MOVE '  GE' TO GODK-STATUSKODER                                      
150700     CALL CBLTDLI USING GU WDR2ALT-PCB DLI-IO-WDGX6334                    
150800                       SSA1 SSA2 SSA3                                     
150900                                                                          
151000     MOVE WDR2ALT-STATUS-CODE TO STATUS-WS                                
151100     PERFORM IMS-STATUSKONTROLL                                           
151200     .                                                                    
151300     EJECT                                                                
151400 IMS-GET-WDGX6334 SECTION.                                                
151500     MOVE 'IMS-GET-WDGX6334        ' TO WS-IMS                            
151600                                                                          
151700     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
151800          DELIMITED BY SIZE INTO SSA1                                     
151900                                                                          
152000     STRING 'WDGX6332(IDDC     =' W-IDDC-X ')'                            
152100          DELIMITED BY SIZE INTO SSA2                                     
152200                                                                          
152300     MOVE 'WDGX6334 ' TO SSA3                                             
152400     MOVE '  GE' TO GODK-STATUSKODER                                      
152500     CALL CBLTDLI USING GU  WDR2-PCB DLI-IO-WDGX6334                      
152600                              SSA1 SSA2 SSA3                              
152700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
152800     PERFORM IMS-STATUSKONTROLL                                           
152900     .                                                                    
153000     EJECT                                                                
153100 IMS-GNP-WDGX6334-UNIK SECTION.                                           
153200     MOVE 'IMS-GNP-WDGX6334-UNIK   ' TO WS-IMS                            
153300                                                                          
153400     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
153500          DELIMITED BY SIZE INTO SSA1                                     
153600     STRING 'WDGX6332(IDDC     =' W-IDDC-X ')'                            
153700          DELIMITED BY SIZE INTO SSA2                                     
153800                                                                          
153900     MOVE 'WDGX6334 ' TO SSA3                                             
154000     MOVE '  GE' TO GODK-STATUSKODER                                      
154100     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6334                      
154200                              SSA1 SSA2 SSA3                              
154300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
154400     PERFORM IMS-STATUSKONTROLL                                           
154500     .                                                                    
154600     EJECT                                                                
154700 IMS-GNP-WDGX6334      SECTION.                                           
154800     MOVE 'IMS-GNP-WDGX6334        ' TO WS-IMS                            
154900                                                                          
155000     MOVE 'WDGX6334 ' TO SSA1                                             
155100     MOVE '  GE' TO GODK-STATUSKODER                                      
155200     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6334 SSA1                 
155300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
155400     PERFORM IMS-STATUSKONTROLL                                           
155500     .                                                                    
155600     EJECT                                                                
155700 IMS-STATUSKONTROLL SECTION.                                              
155800                                                                          
155900     SET STATUS-IX TO 1                                                   
156000     SEARCH GODK-STATUS                                                   
156100       AT END                                                             
156200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
156300         DELIMITED BY SIZE INTO FELTEXT                                   
156400         CALL FELLOG                                                      
156500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
156600         CONTINUE                                                         
156700     END-SEARCH                                                           
156800     .                                                                    
