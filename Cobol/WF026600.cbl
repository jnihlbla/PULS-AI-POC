000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF026600.                                                
000300 AUTHOR.         LUNDH BERNT.                                             
000400 DATE-WRITTEN.   02/03/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000613*    NAME:                                                                
000620*        CARPARTS.BILLIT.SENDCOUNTRYMAINTENANCE                           
000700*    FUNCTION:                                                            
000800*        READ/UPDATE/INSERT/DELETE SENDING COUNTRY TABLE (T01SECO)        
000900*        DEPENDING ON REQUESTED PROGRAMS ACTION CODE (KDPGMACT)           
000910*        KDPGMACT = 'S' READ                                              
000920*        KDPGMACT = 'U' UPDATE                                            
000930*        KDPGMACT = 'I' INSERT                                            
000940*        KDPGMACT = 'D' DELETE                                            
001000*                                                                         
001100*        THE PROGRAM READS   TABLE T01LSEL                                
001110*        THE PROGRAM READS   TABLE T01INRE                                
001120*        THE PROGRAM READS   TABLE T01COCO                                
001130*        THE PROGRAM READS   TABLE T01CURR                                
001200*        THE PROGRAM UPDATES TABLE T01SECO                                
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: WF0266U                                             
001900*        REQUEST:     WF0266I1                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        RESPONSE:    WF0266O1                                            
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
003600 77  IDPGM                       PIC X(08)   VALUE 'WF026600'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003910 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004000 77  KDRC-DISPLAY                PIC Z(5).                                
004100                                                                          
004110*    --- CONSTANT WORK FIELDS                                             
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004311 77  WS-ADRESS                   PIC X(50)                                
004313                   VALUE 'CARPARTS.BILLIT.SENDCOUNTRYMAINTENANCE'.        
004320 77  WS-CURRENT                  PIC S9(3)   VALUE +001    COMP-3.        
004321 77  WS-COMING                   PIC S9(3)   VALUE +002    COMP-3.        
004322 77  WS-ACTIVE                   PIC X(8)    VALUE '00000000'.            
004323 77  WS-DATE-FORMAT              PIC X(8)    VALUE 'YYYYMMDD'.            
004400                                                                          
004600 77  KEYS-SW                     PIC X       VALUE SPACE.                 
004700     88  KEYS-OK                             VALUE 'Y'.                   
004800     88  KEYS-WRONG                          VALUE 'N'.                   
004814                                                                          
004820 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
004830     88  ACT-CODE-VALID                 VALUE 'S', 'U', 'I', 'D'.         
004831     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
004832     88  ACT-CODE-UPDATE                     VALUE 'U'.                   
004833     88  ACT-CODE-INSERT                     VALUE 'I'.                   
004834     88  ACT-CODE-DELETE                     VALUE 'D'.                   
004900                                                                          
004922*    --- OTHER MAPPING-FIELDS THAN COPYTEXT WF0266O1                      
004923 01  MAP-KDSTATUS                PIC S9(3)   VALUE ZERO COMP-3.           
004925 01  MAP-DAREGDAT                PIC X(8)    VALUE SPACE.                 
004926 01  MAP-DAUPPDAT                PIC X(8)    VALUE SPACE.                 
004927 01  MAP-DADELDAT                PIC X(8)    VALUE SPACE.                 
004930                                                                          
004931*    --- WORK-FIELDS                                                      
004935 01  WS-FLCONTROL                PIC X       VALUE SPACE.                 
004937 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
004950                                                                          
005000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005100 01  GENERAL-SUBPROGRAMS.                                                 
005300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005410     03  WZ20DATE                PIC X(8)    VALUE 'WZ20DATE'.            
005500     SKIP3                                                                
005700                                                                          
005710*    --- PARAMETERS TO ABEND                                              
005800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006100 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006200                                                                          
006300 01  MESSAGE-CODES.                                                       
006310     03  ERROR-CODES.                                                     
006330         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
006340         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
006350         05  ERR-INRE-ALREADY-EXISTS PIC X(3)    VALUE '104'.             
006400         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
006410         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
006411         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
006412         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
006413         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
006414         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
006415         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
006416         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
006417     03  INFO-CODES.                                                      
006418         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
006419         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
006420         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
006430         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
006600*                                                                         
006700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006800     SKIP3                                                                
006900 01  -COPY WZ01SUB                                                        
006910     EJECT                                                                
007002 01  FILLER                      PIC X(16)   VALUE 'DATE-CONTROL'.        
007003     SKIP3                                                                
007004 01  -COPY WZ20DATE                                                       
007005     EJECT                                                                
007010*                                                                         
007020 01  FILLER                      PIC X(16)   VALUE 'MAPPING-AREA'.        
007030     SKIP3                                                                
007040 01  -COPY WF0266O1  -PRE MAP-                                            
007050     EJECT                                                                
007100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007200     SKIP3                                                                
007300 01  REQU-AREA.                                                           
007400*    03  -COPY WZ01REQU                                                   
007500*    03  -COPY WF0266I1                                                   
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007800     SKIP3                                                                
007900 01  RESP-AREA.                                                           
008000*    03  -COPY WZ01RESP                                                   
008100*    03  -COPY WF0266O1                                                   
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
008500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
008600                                                                          
008700 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
008800 01  DB2-WS.                                                              
008900     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
009000         88  CURSOR-OK                       VALUE 000.                   
009100         88  LINES-FOUND                     VALUE 000.                   
009200         88  LINES-MISSING                   VALUE 100.                   
009210         88  NULL-VALUE                      VALUE 305.                   
009300         88  RESOURCE-WRONG                  VALUE 904.                   
009400     03  GOOD-SQLCODECODES.                                               
009500         05  GOOD-SQLCODE OCCURS 5                                        
009600             INDEXED BY SQLCODE-IX PIC 9(3).                              
009800                                                                          
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
010200*01  -COPY T01LSEL -PRE T01LSEL-                                          
010210     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'T01SECO-AREA'.        
010500*01  -COPY T01SECO -PRE T01SECO-                                          
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'T01INRE-AREA'.        
010900*01  -COPY T01INRE -PRE T01INRE-                                          
011940     EJECT                                                                
011950 01  FILLER                      PIC X(16)   VALUE 'T01COCO-AREA'.        
011960*01  -COPY T01COCO -PRE T01COCO-                                          
011970     EJECT                                                                
011980 01  FILLER                      PIC X(16)   VALUE 'T01CURR-AREA'.        
011990*01  -COPY T01CURR -PRE T01CURR-                                          
011991     EJECT                                                                
012000     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
012010     EJECT                                                                
012020     EXEC SQL INCLUDE T01SECO END-EXEC.                                   
012030     EJECT                                                                
012040     EXEC SQL INCLUDE T01INRE END-EXEC.                                   
012100     EJECT                                                                
012200     EXEC SQL INCLUDE T01COCO END-EXEC.                                   
012300     EJECT                                                                
012400     EXEC SQL INCLUDE T01CURR END-EXEC.                                   
012500     EJECT                                                                
013000 LINKAGE SECTION.                                                         
013200     EJECT                                                                
013210                                                                          
013300 PROCEDURE DIVISION.                                                      
013400 MAIN SECTION.                                                            
013600                                                                          
013700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
013800     IF SUB-KDRC = 0                                                      
013900       PERFORM A-INIT                                                     
014000       PERFORM B-CHECK-KEYS                                               
014100       IF KEYS-OK                                                         
014200         PERFORM F-READ-SHOW-INFO                                         
014300       END-IF                                                             
014310       IF KEYS-WRONG                                                      
014320         PERFORM S04-MOVE-MISSING-TO-RESPOND                              
014330       END-IF                                                             
014400       PERFORM S02-RETURN-RESPONSE                                        
014500     END-IF                                                               
014700                                                                          
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200                                                                          
015210     EJECT                                                                
015300 A-INIT SECTION.                                                          
015600     INITIALIZE GOOD-SQLCODECODES                                         
015610     MOVE ALL '+' TO RESP-AREA                                            
015620     MOVE SPACE TO RESP-IDMSG-ERROR                                       
015621     MOVE SPACE TO RESP-IDMSG-INFO                                        
015622     MOVE SPACE TO RESP-IDELMT-ERROR                                      
015630     INITIALIZE MAP-RESP-WF0266O1                                         
015640     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
015700     .                                                                    
015801     EJECT                                                                
015802                                                                          
015810*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
015900 B-CHECK-KEYS SECTION.                                                    
016001     MOVE YES TO KEYS-SW                                                  
016007     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
016300                                                                          
016310     IF REQU-KDSTATUS-KEY NUMERIC                                         
016311     AND REQU-IDMSGVER NUMERIC                                            
016312       IF REQU-IDLEGSEL-KEY > SPACE                                       
016313       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
016314       AND REQU-IDLANDX3-KEY > SPACE                                      
016315       AND REQU-IDLANDX3-KEY NOT = ALL '+'                                
016330       AND ACT-CODE-VALID                                                 
016340       AND (REQU-KDSTATUS-KEY = WS-CURRENT OR WS-COMING)                  
016350         CONTINUE                                                         
016360       ELSE                                                               
016361         MOVE NOO TO KEYS-SW                                              
016370       END-IF                                                             
016371     ELSE                                                                 
016372       MOVE NOO TO KEYS-SW                                                
016373     END-IF                                                               
016374                                                                          
016375     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
016376       MOVE NOO TO KEYS-SW                                                
016379     END-IF                                                               
016380                                                                          
016400     IF KEYS-WRONG                                                        
016401       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
016410       IF REQU-IDMSGVER NUMERIC                                           
016420         CONTINUE                                                         
016430       ELSE                                                               
016431         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
016440         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
016450       END-IF                                                             
016460       IF ACT-CODE-VALID                                                  
016470         CONTINUE                                                         
016480       ELSE                                                               
016490         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
016500         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
016510       END-IF                                                             
016520       IF REQU-IDUSER = SPACE OR = ALL '+'                                
016530         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
016540         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
016570       END-IF                                                             
016600     END-IF                                                               
016601                                                                          
016610     IF KEYS-OK                                                           
016620       PERFORM DB2-SELECT-T01COCO-TAB                                     
016630       IF LINES-FOUND                                                     
016640         CONTINUE                                                         
016650       ELSE                                                               
016651         MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                       
016652         MOVE 'IDLANDX3'        TO RESP-IDELMT-ERROR                      
016653         MOVE NOO TO KEYS-SW                                              
016660       END-IF                                                             
016670     END-IF                                                               
016671     IF KEYS-OK                                                           
016680       PERFORM DB2-SELECT-T01LSEL-TAB                                     
016690       IF LINES-FOUND                                                     
016691         CONTINUE                                                         
016692       ELSE                                                               
016693         MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                       
016694         MOVE 'IDLEGSEL'        TO RESP-IDELMT-ERROR                      
016695         MOVE NOO TO KEYS-SW                                              
016696       END-IF                                                             
016697     END-IF                                                               
016698     IF KEYS-OK                                                           
016699       IF REQU-IDVAT    = ALL '+'                                         
016700         MOVE '.'   TO REQU-IDVAT                                         
016701       END-IF                                                             
016702       IF REQU-BETEXT-1 = ALL '+'                                         
016703         MOVE SPACE TO REQU-BETEXT-1                                      
016704       END-IF                                                             
016705       IF REQU-BETEXT-2 = ALL '+'                                         
016706         MOVE SPACE TO REQU-BETEXT-2                                      
016707       END-IF                                                             
016708       IF REQU-BETEXT-3 = ALL '+'                                         
016709         MOVE SPACE TO REQU-BETEXT-3                                      
016710       END-IF                                                             
016711       IF REQU-BETEXT-4 = ALL '+'                                         
016712         MOVE SPACE TO REQU-BETEXT-4                                      
016713       END-IF                                                             
016714     END-IF                                                               
016715     .                                                                    
016716     EJECT                                                                
016720                                                                          
016800*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
016900 F-READ-SHOW-INFO SECTION.                                                
017010     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
017040     MOVE REQU-IDLANDX3-KEY  TO RESP-IDLANDX3-KEY                         
017041     MOVE REQU-KDSTATUS-KEY  TO RESP-KDSTATUS-KEY                         
017042     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
017050                                                                          
017100     PERFORM FA-READ-BASICDATA                                            
017800     .                                                                    
017900     EJECT                                                                
017901                                                                          
017910*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
018000 FA-READ-BASICDATA SECTION.                                               
018200     IF ACT-CODE-SEARCH                                                   
018302       PERFORM FAA-SEARCH-T01SECO                                         
018303     ELSE                                                                 
018310       IF ACT-CODE-UPDATE                                                 
018315         PERFORM FAB-UPDATE-T01SECO                                       
018320       ELSE                                                               
018330         IF ACT-CODE-INSERT                                               
018334           PERFORM FAC-INSERT-T01SECO                                     
018335         ELSE                                                             
018336           IF ACT-CODE-DELETE                                             
018337             PERFORM FAD-DELETE-T01SECO                                   
018338           END-IF                                                         
018339         END-IF                                                           
018340       END-IF                                                             
018353     END-IF                                                               
018600     .                                                                    
018700     EJECT                                                                
018701                                                                          
018702*** - SEARCH FOR RIGHT SENDING COUNTRY AND MARK CURRENT LINE              
018703*** - IF COMING LINE EXIST.                                               
018710 FAA-SEARCH-T01SECO SECTION.                                              
018721     MOVE NOO TO WS-FLCONTROL                                             
018722                                                                          
018723     PERFORM DB2-DCL-OPN-T01SECO-CRS-1                                    
018724     PERFORM DB2-FETCH-T01SECO-CRS-1                                      
018733                                                                          
018734     IF LINES-FOUND                                                       
018737       PERFORM UNTIL LINES-MISSING                                        
018738         IF REQU-KDSTATUS-KEY = WS-CURRENT                                
018739           IF MAP-KDSTATUS = WS-CURRENT                                   
018740             PERFORM S03-MOVE-TO-RESPOND                                  
018741             MOVE YES TO WS-FLCONTROL                                     
018742           ELSE                                                           
018743             IF MAP-KDSTATUS = WS-COMING                                  
018744               IF WS-FLCONTROL = YES                                      
018745                 MOVE YES TO RESP-FLCOMING                                
018746                 MOVE NOO TO WS-FLCONTROL                                 
018747               END-IF                                                     
018748             END-IF                                                       
018749           END-IF                                                         
018750         ELSE                                                             
018751           IF REQU-KDSTATUS-KEY = WS-COMING                               
018752             IF MAP-KDSTATUS = WS-COMING                                  
018753               PERFORM S03-MOVE-TO-RESPOND                                
018754               MOVE SPACE   TO RESP-IDMSG-ERROR                           
018755             ELSE                                                         
018756               MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                        
018757               MOVE 'IDLANDX3' TO RESP-IDELMT-ERROR                       
018758               PERFORM S04-MOVE-MISSING-TO-RESPOND                        
018759             END-IF                                                       
018760           END-IF                                                         
018761         END-IF                                                           
018762         PERFORM DB2-FETCH-T01SECO-CRS-1                                  
018763       END-PERFORM                                                        
018764     ELSE                                                                 
018765       MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                                
018766       MOVE 'IDLANDX3' TO RESP-IDELMT-ERROR                               
018767       MOVE NOO TO KEYS-SW                                                
018768     END-IF                                                               
018769                                                                          
018770     PERFORM DB2-CLOSE-T01SECO-CRS-1                                      
018780     .                                                                    
018790     EJECT                                                                
018800                                                                          
019300*** - CHECK IF UPDATE IS ON CURRENT OR COMING LINE                        
019302 FAB-UPDATE-T01SECO SECTION.                                              
019304     PERFORM FABA-CHECK-UPDATE-DATA                                       
019305                                                                          
019306     IF RESP-IDMSG-ERROR = SPACE                                          
019307       IF REQU-KDSTATUS-KEY = WS-CURRENT                                  
019308         PERFORM FABB-UPD-CURRENT                                         
019310       ELSE                                                               
019312         PERFORM FABC-UPD-COMING                                          
019314       END-IF                                                             
019315     END-IF                                                               
019316     .                                                                    
019317     EJECT                                                                
019318                                                                          
019319*** - VALIDATE REQUESTED FIELDS FOR UPDATE ON SENDING COUNTRY             
019320 FABA-CHECK-UPDATE-DATA SECTION.                                          
019328*    IF RESP-IDMSG-ERROR = SPACE                                          
019329*       IF REQU-IDVAT > SPACE                                             
019330*       AND REQU-IDVAT NOT = ALL '+'                                      
019331*         CONTINUE                                                        
019332*       ELSE                                                              
019333*         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
019334*         MOVE 'IDVAT' TO RESP-IDELMT-ERROR                               
019335*       END-IF                                                            
019336*    END-IF                                                               
019337                                                                          
019338     IF RESP-IDMSG-ERROR = SPACE                                          
019339       IF (REQU-KDVALISO > SPACE AND NOT = ALL '+')                       
019347          CONTINUE                                                        
019352       ELSE                                                               
019353          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
019354          MOVE 'KDVALISO' TO RESP-IDELMT-ERROR                            
019355       END-IF                                                             
019356     END-IF                                                               
019357                                                                          
019358     IF RESP-IDMSG-ERROR = SPACE                                          
019359       PERFORM DB2-SELECT-T01CURR-MAX                                     
019360       IF LINES-FOUND                                                     
019361          CONTINUE                                                        
019370       ELSE                                                               
019380          MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                      
019390          MOVE 'KDVALISO'        TO RESP-IDELMT-ERROR                     
019400       END-IF                                                             
019410     END-IF                                                               
019420                                                                          
019447     IF RESP-IDMSG-ERROR = SPACE                                          
019452       IF REQU-KDSTATUS-KEY = WS-CURRENT                                  
019453         MOVE WS-CURRENT-DATE TO MAP-DAUPPDAT                             
019455       ELSE                                                               
019456         IF REQU-DAUPPDAT NUMERIC                                         
019457           IF REQU-DAUPPDAT > WS-CURRENT-DATE                             
019458             MOVE REQU-DAUPPDAT TO MAP-DAUPPDAT                           
019459                                   DATE-TIDATE                            
019460             MOVE WS-DATE-FORMAT TO DATE-KDDATFMT                         
019461             CALL WZ20DATE USING DATE-WZ20DATE                            
019462             IF DATE-KDRC > ZERO                                          
019463               MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                 
019464               MOVE 'DAUPPDAT'        TO RESP-IDELMT-ERROR                
019465             END-IF                                                       
019466           ELSE                                                           
019467             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
019468             MOVE 'DAUPPDAT' TO RESP-IDELMT-ERROR                         
019469           END-IF                                                         
019470         ELSE                                                             
019471           MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
019472           MOVE 'DAUPPDAT' TO RESP-IDELMT-ERROR                           
019473         END-IF                                                           
019474       END-IF                                                             
019477     END-IF                                                               
019478     .                                                                    
019479     EJECT                                                                
019480                                                                          
019532*** - UPDATE CURRENT LINE ON T01SECO.                                     
019533 FABB-UPD-CURRENT SECTION.                                                
019535     PERFORM DB2-SELECT-T01SECO-TAB-CURR                                  
019536                                                                          
019537     IF LINES-FOUND                                                       
019538        PERFORM DB2-UPDATE-T01SECO-TAB-CURR                               
019539        PERFORM S03-MOVE-TO-RESPOND                                       
019540        IF REQU-FLCOMING = YES                                            
019541          MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                 
019542        ELSE                                                              
019543          MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                           
019544        END-IF                                                            
019545     ELSE                                                                 
019546       MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                                
019547       MOVE 'IDLANDX3' TO RESP-IDELMT-ERROR                               
019548       MOVE NOO TO KEYS-SW                                                
019549     END-IF                                                               
019550     .                                                                    
019551     EJECT                                                                
019552                                                                          
019553*** - UPDATE COMING LINE ON T01SECO. IF NOO COMING LINE EXIST BUT         
019554***   COMING LINE IS CHOOSED FOR UPDATE, PROGRAM WILL INSERT ONE          
019555***   COMING LINE WITH DATA FROM CURRENT LINE BUT USER WILL SEE           
019556***   THIS AS AN UPDATE.                                                  
019557 FABC-UPD-COMING SECTION.                                                 
019559     PERFORM DB2-SELECT-T01SECO-TAB-COM                                   
019560                                                                          
019561     IF LINES-FOUND                                                       
019562       PERFORM DB2-UPDATE-T01SECO-TAB-COM                                 
019563       PERFORM S03-MOVE-TO-RESPOND                                        
019564       MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                    
019565     ELSE                                                                 
019566       PERFORM DB2-SELECT-T01SECO-TAB-CURR                                
019567       IF LINES-FOUND                                                     
019568         MOVE REQU-DAUPPDAT   TO MAP-DAUPPDAT                             
019569         MOVE WS-ACTIVE       TO MAP-DADELDAT                             
019570         PERFORM DB2-INSERT-T01SECO-TAB-COM                               
019571         PERFORM S03-MOVE-TO-RESPOND                                      
019572         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
019573       ELSE                                                               
019574         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
019575         MOVE 'IDLANDX3' TO RESP-IDELMT-ERROR                             
019576         MOVE NOO TO KEYS-SW                                              
019577       END-IF                                                             
019580     END-IF                                                               
019581     .                                                                    
019582     EJECT                                                                
019583                                                                          
019584*** - INSERT NEW CURRENT LINE. COMING LINE COULD NOT BE INSERTED.         
019585*** - IF CURRENT LINE EXIST WITH DELETE DATE, DELETE CURRENT LINE         
019586***   PHYSICAL AND INSERT NEW CURRENT LINE.                               
019587 FAC-INSERT-T01SECO SECTION.                                              
019589     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
019590       PERFORM DB2-SELECT-T01SECO-TAB-CURR-2                              
019591       IF LINES-FOUND                                                     
019592         IF MAP-DADELDAT = WS-ACTIVE                                      
019593           MOVE ERR-ALREADY-EXIST TO RESP-IDMSG-ERROR                     
019594           MOVE 'IDLANDX3' TO RESP-IDELMT-ERROR                           
019595         ELSE                                                             
019596           PERFORM FACA-CHECK-INSERT-DATA                                 
019597           IF RESP-IDMSG-ERROR = SPACE                                    
019598             PERFORM DB2-DELETE-T01SECO-TAB-CURR                          
019599             PERFORM DB2-INSERT-T01SECO-TAB-CURR                          
019600             PERFORM S03-MOVE-TO-RESPOND                                  
019601             MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                        
019603           END-IF                                                         
019604         END-IF                                                           
019605       ELSE                                                               
019606         PERFORM FACA-CHECK-INSERT-DATA                                   
019607         IF RESP-IDMSG-ERROR = SPACE                                      
019608           PERFORM DB2-INSERT-T01SECO-TAB-CURR                            
019609           PERFORM S03-MOVE-TO-RESPOND                                    
019610           MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                          
019611         END-IF                                                           
019612       END-IF                                                             
019613     ELSE                                                                 
019614       MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
019615     END-IF                                                               
019616     .                                                                    
019617     EJECT                                                                
019618                                                                          
019619*** - VALIDATE REQUESTED FIELDS FOR INSERT ON SENDING COUNTRY             
019620 FACA-CHECK-INSERT-DATA SECTION.                                          
019634*    IF RESP-IDMSG-ERROR = SPACE                                          
019635*      IF REQU-IDVAT > SPACE                                              
019636*      AND REQU-IDVAT NOT = ALL '+'                                       
019637*        CONTINUE                                                         
019638*      ELSE                                                               
019639*        MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
019640*        MOVE 'IDVAT' TO RESP-IDELMT-ERROR                                
019641*      END-IF                                                             
019642*    END-IF                                                               
019643                                                                          
019644     IF RESP-IDMSG-ERROR = SPACE                                          
019645       IF (REQU-KDVALISO > SPACE AND NOT = ALL '+')                       
019647         CONTINUE                                                         
019648       ELSE                                                               
019649         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
019650         MOVE 'KDVALISO' TO RESP-IDELMT-ERROR                             
019651       END-IF                                                             
019652     END-IF                                                               
019653                                                                          
019654     IF RESP-IDMSG-ERROR = SPACE                                          
019655       PERFORM DB2-SELECT-T01CURR-MAX                                     
019656       IF LINES-FOUND                                                     
019657          CONTINUE                                                        
019658       ELSE                                                               
019659          MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                      
019660          MOVE 'KDVALISO'        TO RESP-IDELMT-ERROR                     
019661       END-IF                                                             
019662     END-IF                                                               
019663                                                                          
019664     IF RESP-IDMSG-ERROR = SPACE                                          
019665       MOVE WS-CURRENT-DATE TO MAP-DAREGDAT                               
019666       MOVE WS-ACTIVE       TO MAP-DAUPPDAT                               
019667       MOVE WS-ACTIVE       TO MAP-DADELDAT                               
019670     END-IF                                                               
019671     .                                                                    
019672     EJECT                                                                
019673                                                                          
019674*** - DELETE ON CURRENT LINE = UPDATE IN TABLE T01SECO WITH               
019675***   CURRENT DATE AS DELETE DATE.                                        
019676*** - DELETE ON COMING LINE IS A PHYSICAL DELETE FROM TABLE               
019677***   T01SECO.                                                            
019678 FAD-DELETE-T01SECO SECTION.                                              
019680     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
019684       PERFORM DB2-SELECT-T01INRE-TAB                                     
019688       IF LINES-FOUND                                                     
019689         MOVE ERR-INRE-ALREADY-EXISTS TO RESP-IDMSG-ERROR                 
019691       ELSE                                                               
019695         PERFORM DB2-SELECT-T01SECO-TAB-CURR                              
019699         IF LINES-FOUND                                                   
019700           PERFORM DB2-UPDATE-T01SECO-TAB-DEL                             
019701           PERFORM S03-MOVE-TO-RESPOND                                    
019702           MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                          
019703           PERFORM DB2-SELECT-T01SECO-TAB-COM                             
019704           IF LINES-FOUND                                                 
019705             PERFORM DB2-DELETE-T01SECO-TAB-COM                           
019706           END-IF                                                         
019707         ELSE                                                             
019708           MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                            
019709           MOVE 'IDLANDX3' TO RESP-IDELMT-ERROR                           
019710           MOVE NOO TO KEYS-SW                                            
019711         END-IF                                                           
019712       END-IF                                                             
019713     ELSE                                                                 
019714       PERFORM DB2-SELECT-T01SECO-TAB-COM                                 
019715       IF LINES-FOUND                                                     
019716         PERFORM DB2-DELETE-T01SECO-TAB-COM                               
019717         MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                            
019718         PERFORM S03-MOVE-TO-RESPOND                                      
019719       ELSE                                                               
019720         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
019721         MOVE 'IDLANDX3' TO RESP-IDELMT-ERROR                             
019722         MOVE NOO TO KEYS-SW                                              
019723       END-IF                                                             
019724     END-IF                                                               
019725     .                                                                    
019726     EJECT                                                                
019727                                                                          
019728*    --- DISPATCHER SECTIONS                                              
019729 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019730     MOVE 'GETARG'                   TO SUB-KDFUNC                        
019731     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
019740     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
019800                                                                          
019900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
020000                                                                          
020100     IF SUB-KDRC > 0                                                      
020200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
020300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
020400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
020500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020600     END-IF                                                               
020700     .                                                                    
020800                                                                          
020900 S02-RETURN-RESPONSE SECTION.                                             
021100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
021110     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
021300                                                                          
021400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
021500                                                                          
021600     IF SUB-KDRC > 0                                                      
021700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
021900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
022000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022100     END-IF                                                               
022200     .                                                                    
022300     EJECT                                                                
022310                                                                          
022311*    --- MOVE TO OUTPUT SECTIONS                                          
022320 S03-MOVE-TO-RESPOND SECTION.                                             
022340     IF ACT-CODE-SEARCH                                                   
022341       MOVE MAP-RESP-BELAND   TO RESP-BELAND                              
022342       MOVE MAP-RESP-IDVAT    TO RESP-IDVAT                               
022347       MOVE MAP-RESP-KDVALISO TO RESP-KDVALISO                            
022348       MOVE MAP-RESP-BETEXT-1 TO RESP-BETEXT-1                            
022349       MOVE MAP-RESP-BETEXT-2 TO RESP-BETEXT-2                            
022350       MOVE MAP-RESP-BETEXT-3 TO RESP-BETEXT-3                            
022351       MOVE MAP-RESP-BETEXT-4 TO RESP-BETEXT-4                            
022352       MOVE MAP-DAREGDAT      TO RESP-DAREGDAT                            
022353       MOVE MAP-DAUPPDAT      TO RESP-DAUPPDAT                            
022354       MOVE MAP-DADELDAT      TO RESP-DADELDAT                            
022355       MOVE MAP-RESP-IDUSER   TO RESP-IDUSER                              
022356       MOVE NOO               TO RESP-FLCOMING                            
022357     ELSE                                                                 
022358       IF ACT-CODE-UPDATE                                                 
022359         MOVE MAP-RESP-BELAND TO RESP-BELAND                              
022360         MOVE REQU-IDVAT      TO RESP-IDVAT                               
022362         MOVE REQU-KDVALISO   TO RESP-KDVALISO                            
022363         MOVE REQU-BETEXT-1   TO RESP-BETEXT-1                            
022364         MOVE REQU-BETEXT-2   TO RESP-BETEXT-2                            
022365         MOVE REQU-BETEXT-3   TO RESP-BETEXT-3                            
022366         MOVE REQU-BETEXT-4   TO RESP-BETEXT-4                            
022367         MOVE MAP-DAREGDAT    TO RESP-DAREGDAT                            
022368         MOVE MAP-DAUPPDAT    TO RESP-DAUPPDAT                            
022369         MOVE MAP-DADELDAT    TO RESP-DADELDAT                            
022370         MOVE REQU-IDUSER     TO RESP-IDUSER                              
022371         IF REQU-KDSTATUS-KEY = WS-CURRENT                                
022372           MOVE REQU-FLCOMING TO RESP-FLCOMING                            
022373         ELSE                                                             
022374           MOVE REQU-FLCOMING TO RESP-FLCOMING                            
022375* DONT SHOW DAREGDAT WHEN UPDATING ON COMING VERSION                      
022376           MOVE ZERO TO RESP-DAREGDAT                                     
022377         END-IF                                                           
022378       ELSE                                                               
022379         IF ACT-CODE-INSERT                                               
022380           MOVE MAP-RESP-BELAND TO RESP-BELAND                            
022381           MOVE REQU-IDVAT      TO RESP-IDVAT                             
022382           MOVE REQU-KDVALISO   TO RESP-KDVALISO                          
022383           MOVE REQU-BETEXT-1   TO RESP-BETEXT-1                          
022384           MOVE REQU-BETEXT-2   TO RESP-BETEXT-2                          
022385           MOVE REQU-BETEXT-3   TO RESP-BETEXT-3                          
022386           MOVE REQU-BETEXT-4   TO RESP-BETEXT-4                          
022387           MOVE MAP-DAREGDAT    TO RESP-DAREGDAT                          
022388           MOVE MAP-DAUPPDAT    TO RESP-DAUPPDAT                          
022389           MOVE MAP-DADELDAT    TO RESP-DADELDAT                          
022390           MOVE REQU-IDUSER     TO RESP-IDUSER                            
022391           MOVE NOO             TO RESP-FLCOMING                          
022392         ELSE                                                             
022393           IF ACT-CODE-DELETE                                             
022394             MOVE SPACE           TO RESP-BELAND                          
022395                                     RESP-IDVAT                           
022397                                     RESP-KDVALISO                        
022398                                     RESP-BETEXT-1                        
022399                                     RESP-BETEXT-2                        
022400                                     RESP-BETEXT-3                        
022401                                     RESP-BETEXT-4                        
022402             MOVE ZERO            TO RESP-DAREGDAT                        
022403             MOVE ZERO            TO RESP-DAUPPDAT                        
022404             MOVE SPACE           TO RESP-FLCOMING                        
022405             MOVE WS-CURRENT-DATE TO RESP-DADELDAT                        
022406             MOVE REQU-IDUSER     TO RESP-IDUSER                          
022407             MOVE NOO             TO RESP-FLCOMING                        
022408           END-IF                                                         
022409         END-IF                                                           
022410       END-IF                                                             
022411     END-IF                                                               
022416     .                                                                    
022417                                                                          
022418 S04-MOVE-MISSING-TO-RESPOND SECTION.                                     
022419     MOVE SPACE             TO RESP-FLCOMING                              
022420                               RESP-BELAND                                
022421                               RESP-IDVAT                                 
022430                               RESP-KDVALISO                              
022440                               RESP-BETEXT-1                              
022450                               RESP-BETEXT-2                              
022460                               RESP-BETEXT-3                              
022461                               RESP-BETEXT-4                              
022470                               RESP-IDUSER                                
022480     MOVE ZERO              TO RESP-DAREGDAT                              
022490                               RESP-DAUPPDAT                              
022500                               RESP-DADELDAT                              
022510     .                                                                    
022520     EJECT                                                                
022521                                                                          
022530*    --- DB2 SECTIONS                                                     
022540 DB2-SELECT-T01LSEL-TAB   SECTION.                                        
022550                                                                          
022560     MOVE 000100 TO GOOD-SQLCODECODES                                     
022570                                                                          
022580     EXEC SQL                                                             
022590        SELECT  BELEGRAD_1                                                
022600                                                                          
022700        INTO   :T01LSEL-BELEGRAD-1                                        
022800                                                                          
022900        FROM    T01LSEL                                                   
023000                                                                          
023100        WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                             
023200            AND KDSTATUS = :WS-CURRENT                                    
023300     END-EXEC                                                             
023400                                                                          
023500     MOVE SQLCODE TO SQLCODE-WS                                           
023600     PERFORM DB2-STATUS-CHECK                                             
023700     .                                                                    
023701                                                                          
023702 DB2-SELECT-T01COCO-TAB   SECTION.                                        
023704     MOVE 000100 TO GOOD-SQLCODECODES                                     
023705                                                                          
023706     EXEC SQL                                                             
023707        SELECT  BELAND                                                    
023708                                                                          
023709        INTO   :MAP-RESP-BELAND                                           
023710                                                                          
023711        FROM    T01COCO                                                   
023712                                                                          
023713        WHERE   IDLANDX3 = :REQU-IDLANDX3-KEY                             
023714     END-EXEC                                                             
023715                                                                          
023716     MOVE SQLCODE TO SQLCODE-WS                                           
023717     PERFORM DB2-STATUS-CHECK                                             
023718     .                                                                    
023719                                                                          
023720 DB2-SELECT-T01INRE-TAB   SECTION.                                        
023730     MOVE 000100305 TO GOOD-SQLCODECODES                                  
023740                                                                          
023750     EXEC SQL                                                             
023760        SELECT MAX(IDLANDX3_REC)                                          
023770                                                                          
023780        INTO   :T01INRE-IDLANDX3-REC                                      
023790                                                                          
023791        FROM    T01INRE                                                   
023792                                                                          
023793        WHERE IDLEGSEL      = :REQU-IDLEGSEL-KEY                          
023795        AND   IDLANDX3_SEND = :REQU-IDLANDX3-KEY                          
023796        AND   DADELDAT      = :WS-ACTIVE                                  
023797     END-EXEC                                                             
023798                                                                          
023799     MOVE SQLCODE TO SQLCODE-WS                                           
023800     PERFORM DB2-STATUS-CHECK                                             
023810     .                                                                    
023820                                                                          
024000* * * * * * * * * * * * - CURSOR-1 - * * * * * * * * * * * * * * *        
033050 DB2-DCL-OPN-T01SECO-CRS-1 SECTION.                                       
033300     MOVE 000100 TO GOOD-SQLCODECODES                                     
033400                                                                          
033500     EXEC SQL                                                             
033600         DECLARE T01SECO-CRS-1 CURSOR WITH HOLD FOR                       
033700                                                                          
033900           SELECT  KDSTATUS                                               
034010                 , IDVAT                                                  
034020                 , KDVALISO                                               
034030                 , BETEXT_1                                               
034040                 , BETEXT_2                                               
034050                 , BETEXT_3                                               
034060                 , BETEXT_4                                               
034100                 , DAREGDAT                                               
034200                 , DAUPPDAT                                               
034300                 , DADELDAT                                               
034400                 , IDUSER                                                 
034600                                                                          
034610           FROM     T01SECO                                               
034620                                                                          
034630           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
034640                AND IDLANDX3 = :REQU-IDLANDX3-KEY                         
034641                AND DADELDAT = :WS-ACTIVE                                 
034650                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
034660                                                                          
034670           ORDER BY IDLEGSEL                                              
034690                  , IDLANDX3                                              
034700                  , KDSTATUS                                              
034710     END-EXEC                                                             
034711                                                                          
034720     MOVE 000100 TO GOOD-SQLCODECODES                                     
034800                                                                          
034900     EXEC SQL                                                             
035000        OPEN T01SECO-CRS-1                                                
035100     END-EXEC                                                             
035110                                                                          
035200     MOVE SQLCODE TO SQLCODE-WS                                           
035300     PERFORM DB2-STATUS-CHECK                                             
035400     .                                                                    
035500                                                                          
035510 DB2-FETCH-T01SECO-CRS-1 SECTION.                                         
035521     MOVE 000100  TO GOOD-SQLCODECODES                                    
035522                                                                          
035523     EXEC SQL                                                             
035525         FETCH T01SECO-CRS-1                                              
035526                                                                          
035527         INTO :MAP-KDSTATUS                                               
035529            , :MAP-RESP-IDVAT                                             
035530            , :MAP-RESP-KDVALISO                                          
035531            , :MAP-RESP-BETEXT-1                                          
035532            , :MAP-RESP-BETEXT-2                                          
035533            , :MAP-RESP-BETEXT-3                                          
035534            , :MAP-RESP-BETEXT-4                                          
035535            , :MAP-DAREGDAT                                               
035536            , :MAP-DAUPPDAT                                               
035537            , :MAP-DADELDAT                                               
035538            , :MAP-RESP-IDUSER                                            
035556     END-EXEC                                                             
035557                                                                          
035564     MOVE SQLCODE TO SQLCODE-WS                                           
035565     PERFORM DB2-STATUS-CHECK                                             
035566     .                                                                    
035567                                                                          
035590 DB2-CLOSE-T01SECO-CRS-1  SECTION.                                        
035592     EXEC SQL                                                             
035593        CLOSE T01SECO-CRS-1                                               
035594     END-EXEC                                                             
035595     .                                                                    
035596                                                                          
035597 DB2-SELECT-T01SECO-TAB-CURR SECTION.                                     
035599     MOVE 000100  TO GOOD-SQLCODECODES                                    
035600                                                                          
035601     EXEC SQL                                                             
035602         SELECT DAREGDAT                                                  
035654              , DADELDAT                                                  
035656                                                                          
035657         INTO  :MAP-DAREGDAT                                              
035670             , :MAP-DADELDAT                                              
035699                                                                          
035700         FROM  T01SECO                                                    
035703                                                                          
035704         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
035705         AND   IDLANDX3 = :REQU-IDLANDX3-KEY                              
035706         AND   DADELDAT = :WS-ACTIVE                                      
035707         AND   KDSTATUS = :WS-CURRENT                                     
035708     END-EXEC                                                             
035709                                                                          
035710     MOVE SQLCODE TO SQLCODE-WS                                           
035711     PERFORM DB2-STATUS-CHECK                                             
035712     .                                                                    
035713                                                                          
035735 DB2-UPDATE-T01SECO-TAB-CURR SECTION.                                     
035800     MOVE 000     TO GOOD-SQLCODECODES                                    
035810                                                                          
035900     EXEC SQL                                                             
036000        UPDATE T01SECO                                                    
036210           SET                                                            
036212                 IDVAT    = :REQU-IDVAT                                   
036213               , KDVALISO = :REQU-KDVALISO                                
036214               , BETEXT_1 = :REQU-BETEXT-1                                
036215               , BETEXT_2 = :REQU-BETEXT-2                                
036216               , BETEXT_3 = :REQU-BETEXT-3                                
036217               , BETEXT_4 = :REQU-BETEXT-4                                
036218               , DAUPPDAT = :MAP-DAUPPDAT                                 
036219               , IDUSER   = :REQU-IDUSER                                  
036405                                                                          
036410         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
036420         AND     IDLANDX3 = :REQU-IDLANDX3-KEY                            
036421         AND     DADELDAT = :WS-ACTIVE                                    
036430         AND     KDSTATUS = :WS-CURRENT                                   
036500     END-EXEC                                                             
036600                                                                          
036700     MOVE SQLCODE TO SQLCODE-WS                                           
036800     PERFORM DB2-STATUS-CHECK                                             
036900     .                                                                    
036910                                                                          
037000 DB2-UPDATE-T01SECO-TAB-DEL SECTION.                                      
037020     MOVE 000     TO GOOD-SQLCODECODES                                    
037030                                                                          
037040     EXEC SQL                                                             
037041        UPDATE T01SECO                                                    
037042           SET   DADELDAT = :WS-CURRENT-DATE                              
037046                                                                          
037047         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
037048         AND     IDLANDX3 = :REQU-IDLANDX3-KEY                            
037049         AND     DADELDAT = :WS-ACTIVE                                    
037050         AND     KDSTATUS = :WS-CURRENT                                   
037051     END-EXEC                                                             
037052                                                                          
037053     MOVE SQLCODE TO SQLCODE-WS                                           
037054     PERFORM DB2-STATUS-CHECK                                             
037055     .                                                                    
037056                                                                          
037060 DB2-SELECT-T01SECO-TAB-COM SECTION.                                      
037152     MOVE 000100  TO GOOD-SQLCODECODES                                    
037153                                                                          
037154     EXEC SQL                                                             
037155         SELECT DAREGDAT                                                  
037156              , DADELDAT                                                  
037157                                                                          
037158         INTO  :MAP-DAREGDAT                                              
037159             , :MAP-DADELDAT                                              
037160                                                                          
037161         FROM  T01SECO                                                    
037162                                                                          
037163         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
037164         AND   IDLANDX3 = :REQU-IDLANDX3-KEY                              
037166         AND   KDSTATUS = :WS-COMING                                      
037167     END-EXEC                                                             
037210                                                                          
037211     MOVE SQLCODE TO SQLCODE-WS                                           
037212     PERFORM DB2-STATUS-CHECK                                             
037213     .                                                                    
037214                                                                          
037215 DB2-UPDATE-T01SECO-TAB-COM  SECTION.                                     
037217     MOVE 000     TO GOOD-SQLCODECODES                                    
037218                                                                          
037219     EXEC SQL                                                             
037220        UPDATE T01SECO                                                    
037221           SET                                                            
037222                 IDVAT    = :REQU-IDVAT                                   
037223               , KDVALISO = :REQU-KDVALISO                                
037224               , BETEXT_1 = :REQU-BETEXT-1                                
037225               , BETEXT_2 = :REQU-BETEXT-2                                
037226               , BETEXT_3 = :REQU-BETEXT-3                                
037227               , BETEXT_4 = :REQU-BETEXT-4                                
037228               , DAUPPDAT = :MAP-DAUPPDAT                                 
037229               , IDUSER   = :REQU-IDUSER                                  
037230                                                                          
037231         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
037232         AND     IDLANDX3 = :REQU-IDLANDX3-KEY                            
037233         AND     DADELDAT = :WS-ACTIVE                                    
037234         AND     KDSTATUS = :WS-COMING                                    
037240     END-EXEC                                                             
037273                                                                          
037274     MOVE SQLCODE TO SQLCODE-WS                                           
037275     PERFORM DB2-STATUS-CHECK                                             
037276     .                                                                    
037277                                                                          
037278 DB2-SELECT-T01SECO-TAB-CURR-2 SECTION.                                   
037280     MOVE 000100  TO GOOD-SQLCODECODES                                    
037281                                                                          
037290     EXEC SQL                                                             
037291         SELECT DADELDAT                                                  
037293                                                                          
037294         INTO  :MAP-DADELDAT                                              
037296                                                                          
037297         FROM  T01SECO                                                    
037298                                                                          
037299         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
037300         AND   IDLANDX3 = :REQU-IDLANDX3-KEY                              
037302         AND   KDSTATUS = :WS-CURRENT                                     
037303     END-EXEC                                                             
037304                                                                          
037305     MOVE SQLCODE TO SQLCODE-WS                                           
037306     PERFORM DB2-STATUS-CHECK                                             
037307     .                                                                    
037308                                                                          
037309 DB2-INSERT-T01SECO-TAB-CURR  SECTION.                                    
037311     MOVE 000   TO GOOD-SQLCODECODES                                      
037320                                                                          
037400     EXEC SQL                                                             
037500         INSERT INTO T01SECO                                              
037600            (IDLEGSEL,IDLANDX3,KDSTATUS,IDVAT,KDVALISO                    
037700            ,BETEXT_1,BETEXT_2,BETEXT_3,BETEXT_4                          
037730            ,DAREGDAT,DAUPPDAT,DADELDAT,IDUSER)                           
037740         VALUES                                                           
037750            (:REQU-IDLEGSEL-KEY,:REQU-IDLANDX3-KEY,:WS-CURRENT            
037760            ,:REQU-IDVAT,:REQU-KDVALISO                                   
037770            ,:REQU-BETEXT-1,:REQU-BETEXT-2                                
037780            ,:REQU-BETEXT-3,:REQU-BETEXT-4                                
037793            ,:MAP-DAREGDAT,:MAP-DAUPPDAT,:MAP-DADELDAT                    
037794            ,:REQU-IDUSER)                                                
037900     END-EXEC                                                             
038000     MOVE SQLCODE TO SQLCODE-WS                                           
038100     PERFORM DB2-STATUS-CHECK                                             
038200     .                                                                    
038300                                                                          
038320 DB2-INSERT-T01SECO-TAB-COM SECTION.                                      
038340     MOVE 000   TO GOOD-SQLCODECODES                                      
038341                                                                          
038350     EXEC SQL                                                             
038360         INSERT INTO T01SECO                                              
038370            (IDLEGSEL,IDLANDX3,KDSTATUS,IDVAT,KDVALISO                    
038380            ,BETEXT_1,BETEXT_2,BETEXT_3,BETEXT_4                          
038384            ,DAREGDAT,DAUPPDAT,DADELDAT,IDUSER)                           
038385         VALUES                                                           
038386            (:REQU-IDLEGSEL-KEY,:REQU-IDLANDX3-KEY,:WS-COMING             
038388            ,:REQU-IDVAT,:REQU-KDVALISO                                   
038389            ,:REQU-BETEXT-1,:REQU-BETEXT-2                                
038390            ,:REQU-BETEXT-3,:REQU-BETEXT-4                                
038391            ,:MAP-DAREGDAT,:MAP-DAUPPDAT,:MAP-DADELDAT                    
038392            ,:REQU-IDUSER)                                                
038397     END-EXEC                                                             
038398                                                                          
038399     MOVE SQLCODE TO SQLCODE-WS                                           
038400     PERFORM DB2-STATUS-CHECK                                             
038401     .                                                                    
038402                                                                          
038403 DB2-DELETE-T01SECO-TAB-CURR SECTION.                                     
038405     MOVE 000   TO GOOD-SQLCODECODES                                      
038406                                                                          
038407     EXEC SQL                                                             
038408         DELETE FROM T01SECO                                              
038409                                                                          
038410         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
038411            AND IDLANDX3 = :REQU-IDLANDX3-KEY                             
038412            AND KDSTATUS = :WS-CURRENT                                    
038413            AND DADELDAT > :WS-ACTIVE                                     
038414     END-EXEC                                                             
038415                                                                          
038416     MOVE SQLCODE TO SQLCODE-WS                                           
038417     PERFORM DB2-STATUS-CHECK                                             
038418     .                                                                    
038419                                                                          
038420 DB2-DELETE-T01SECO-TAB-COM SECTION.                                      
038422     MOVE 000   TO GOOD-SQLCODECODES                                      
038423                                                                          
038424     EXEC SQL                                                             
038425         DELETE FROM T01SECO                                              
038426                                                                          
038427         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
038428            AND IDLANDX3 = :REQU-IDLANDX3-KEY                             
038429            AND KDSTATUS = :WS-COMING                                     
038430     END-EXEC                                                             
038431                                                                          
038432     MOVE SQLCODE TO SQLCODE-WS                                           
038433     PERFORM DB2-STATUS-CHECK                                             
038434     .                                                                    
038435                                                                          
038436 DB2-SELECT-T01CURR-MAX SECTION.                                          
038437     MOVE 000100305  TO GOOD-SQLCODECODES                                 
038438     EXEC SQL                                                             
038439         SELECT   MAX(KDVALISO)                                           
038440                                                                          
038441         INTO    :T01CURR-KDVALISO                                        
038442                                                                          
038443         FROM     T01CURR                                                 
038444                                                                          
038445         WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                           
038446         AND      KDVALISO = :REQU-KDVALISO                               
038447                                                                          
038448     END-EXEC                                                             
038449                                                                          
038450     MOVE SQLCODE TO SQLCODE-WS                                           
038451     PERFORM DB2-STATUS-CHECK                                             
038452     .                                                                    
038453     EJECT                                                                
038454                                                                          
038460 DB2-STATUS-CHECK  SECTION.                                               
038600     SET SQLCODE-IX TO 1                                                  
038700     SEARCH GOOD-SQLCODE                                                  
038800       AT END                                                             
038900          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
039000          DELIMITED BY SIZE INTO ERROR-TEXT                               
039100          CALL ABEND USING RKOD-ABEND-DB2                                 
039200       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
039300     END-SEARCH                                                           
039400     .                                                                    
