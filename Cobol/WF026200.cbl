000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF026200.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   02/03/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610*    NAME:                                                                
000620*        CARPARTS.BILLIT.APPRPARTGRMAINTENANCE                            
000700*    FUNCTION:                                                            
000800*        READ/UPDATE/INSERT/DELETE PARTNER GROUP                          
000810*        TABLE T01CUGR DEPENDING ON                                       
000900*        REQUESTED PROGRAMS ACTION CODE (KDPGMACT)                        
000910*        KDPGMACT = 'S' READ                                              
000920*        KDPGMACT = 'U' UPDATE                                            
000930*        KDPGMACT = 'I' INSERT                                            
000940*        KDPGMACT = 'D' DELETE                                            
001000*                                                                         
001100*        THE PROGRAM READS   TABLE T01LSEL                                
001400*        THE PROGRAM READS   TABLE T01FCUS                                
001500*        THE PROGRAM UPDATES TABLE T01CUGR                                
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: WF0262U                                             
001900*        REQUEST:     WF0262I1                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        RESPONSE:    WF0262O1                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002700 INPUT-OUTPUT SECTION.                                                    
002900 FILE-CONTROL.                                                            
003100 DATA DIVISION.                                                           
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'WF026200'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003910 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004000 77  KDRC-DISPLAY                PIC Z(5).                                
004100                                                                          
004110*    --- CONSTANT WORK FIELDS                                             
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004311 77  WS-ADRESS                   PIC X(50)                                
004312             VALUE 'CARPARTS.BILLIT.APPRPARTGRMAINTENANCE'.               
004313                                                                          
004320 77  WS-CURRENT                  PIC S9(3)   VALUE +001    COMP-3.        
004321 77  WS-COMING                   PIC S9(3)   VALUE +002    COMP-3.        
004400                                                                          
004600 77  KEYS-SW                     PIC X       VALUE SPACE.                 
004700     88  KEYS-OK                             VALUE 'Y'.                   
004800     88  KEYS-WRONG                          VALUE 'N'.                   
004814                                                                          
004820 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
004830     88  ACT-CODE-VALID              VALUE 'S', 'D', 'U', 'I'.            
004831     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
004832     88  ACT-CODE-UPDATE                     VALUE 'U'.                   
004833     88  ACT-CODE-INSERT                     VALUE 'I'.                   
004834     88  ACT-CODE-DELETE                     VALUE 'D'.                   
004900                                                                          
004931*    --- WORK-FIELDS                                                      
004932 01  WS-IDLEGSEL                 PIC X(4)    VALUE SPACE.                 
004935 01  WS-KDPARTTY                 PIC X(3)    VALUE SPACE.                 
004936 01  WS-KDPARTGR                 PIC X(15)   VALUE SPACE.                 
004937 01  WS-KDSTATUS                 PIC 9(3)    VALUE ZERO.                  
004938 01  WS-BELEGRAD-1               PIC X(35)   VALUE SPACE.                 
004939 01  WS-FLCOMING                 PIC X       VALUE SPACE.                 
004940 01  WS-KDINVFRQ                 PIC X(4)    VALUE SPACE.                 
004941 01  W-KDINVFRQ                  PIC X(4)    VALUE SPACE.                 
004942 01  WS-KDAPPEND                 PIC X(4)    VALUE SPACE.                 
004943 01  W-KDAPPEND                  PIC X(4)    VALUE SPACE.                 
004944 01  WS-FLVAT                    PIC X(1)    VALUE SPACE.                 
004952 01  WS-DAREGDAT                 PIC X(8)    VALUE SPACE.                 
004953 01  WS-DAREGDAT-2               PIC 9(8)    VALUE ZERO.                  
004954 01  WS-DAUPPDAT                 PIC X(8)    VALUE SPACE.                 
004955 01  WS-DAUPPDAT-2               PIC 9(8)    VALUE ZERO.                  
004956 01  WS-DADELDAT                 PIC X(8)    VALUE SPACE.                 
004957 01  WS-DADELDAT-2               PIC 9(8)    VALUE ZERO.                  
004958 01  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
004959 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
004960                                                                          
005000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005100 01  GENERAL-SUBPROGRAMS.                                                 
005300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
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
006320         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
006321         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
006322         05  ERR-DELETE-NOT-ALLOWED  PIC X(3)    VALUE '009'.             
006400         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
006401         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
006402         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
006410         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
006411         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
006412         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
006413         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
006414         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
006416     03  INFO-CODES.                                                      
006417         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
006418         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
006419         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
006420         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
006500     EJECT                                                                
006600                                                                          
006700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006800     SKIP3                                                                
006900 01  -COPY WZ01SUB                                                        
007000     EJECT                                                                
007010                                                                          
007100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007200     SKIP3                                                                
007300 01  REQU-AREA.                                                           
007400*    03  -COPY WZ01REQU                                                   
007500*    03  -COPY WF0262I1                                                   
007600     EJECT                                                                
007610                                                                          
007700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007800     SKIP3                                                                
007900 01  RESP-AREA.                                                           
008000*    03  -COPY WZ01RESP                                                   
008100*    03  -COPY WF0262O1                                                   
008300     EJECT                                                                
008301                                                                          
008302 01  WZ20DATE PIC X(8) VALUE 'WZ20DATE'.                                  
008303     SKIP3                                                                
008304*    -COPY WZ20DATE                                                       
008305     EJECT                                                                
008310                                                                          
008400 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
008500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
008600                                                                          
008700 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
008800 01  DB2-WS.                                                              
008900     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
009000         88  CURSOR-OK                       VALUE 000.                   
009100         88  LINES-FOUND                     VALUE 000.                   
009200         88  LINES-MISSING                   VALUE 100.                   
009300         88  RESOURCE-WRONG                  VALUE 904.                   
009310                                                                          
009400     03  GOOD-SQLCODECODES.                                               
009500         05  GOOD-SQLCODE OCCURS 5                                        
009600             INDEXED BY SQLCODE-IX PIC 9(3).                              
009700     EJECT                                                                
009800                                                                          
010000 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
010200*01  -COPY T01LSEL -PRE T01LSEL-                                          
010300     EJECT                                                                
010310                                                                          
011200 01  FILLER                      PIC X(16)   VALUE 'T01CUGR-AREA'.        
011400*01  -COPY T01CUGR -PRE T01CUGR-                                          
011500     EJECT                                                                
011501                                                                          
011600 01  FILLER                      PIC X(16)   VALUE 'T01FCUS-AREA'.        
011800*01  -COPY T01FCUS -PRE T01FCUS-                                          
011900     EJECT                                                                
011910                                                                          
012000     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
012100     EJECT                                                                
012600     EXEC SQL INCLUDE T01CUGR END-EXEC.                                   
012610     EJECT                                                                
012800     EXEC SQL INCLUDE T01FCUS END-EXEC.                                   
012900     EJECT                                                                
012910                                                                          
013000 LINKAGE SECTION.                                                         
013300 PROCEDURE DIVISION.                                                      
013400 MAIN SECTION.                                                            
013600                                                                          
013700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
013800     IF SUB-KDRC = 0                                                      
013900       PERFORM A-INIT                                                     
014000       PERFORM B-CHECK-KEYS                                               
014001       IF KEYS-OK                                                         
014010         PERFORM D-VALIDATE-REQUEST                                       
014100         IF KEYS-OK                                                       
014120           PERFORM F-READ-SHOW-INFO                                       
014300         END-IF                                                           
014310       END-IF                                                             
014320       IF KEYS-WRONG                                                      
014330         PERFORM S06-MOVE-MISSING-TO-RESPOND                              
014401       END-IF                                                             
014410       PERFORM S02-RETURN-RESPONSE                                        
014500     END-IF                                                               
014700                                                                          
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015210                                                                          
015300 A-INIT SECTION.                                                          
015400     MOVE '00000000' TO WS-DADELDAT                                       
015600     INITIALIZE GOOD-SQLCODECODES                                         
015610     MOVE ALL '+' TO RESP-AREA                                            
015620     MOVE SPACE TO RESP-IDMSG-ERROR                                       
015630     MOVE SPACE TO RESP-IDMSG-INFO                                        
015631     MOVE SPACE TO RESP-IDELMT-ERROR                                      
015640     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
015700     .                                                                    
015710     EJECT                                                                
015800                                                                          
015810*** - CHECK REQUESTED SEARCHING KEYS AND COMPULSORY FIELDS                
015900 B-CHECK-KEYS SECTION.                                                    
016001     MOVE YES TO KEYS-SW                                                  
016007     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
016300                                                                          
016310     IF  REQU-IDLEGSEL-KEY = SPACE OR = ALL '+'                           
016320     OR  REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                           
016323     OR  REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                           
016324       MOVE NOO TO KEYS-SW                                                
016325     END-IF                                                               
016326                                                                          
016327     IF REQU-IDMSGVER NUMERIC                                             
016330     AND ACT-CODE-VALID                                                   
016340     AND (REQU-KDSTATUS-KEY = WS-CURRENT OR WS-COMING)                    
016350      CONTINUE                                                            
016360     ELSE                                                                 
016361       MOVE NOO TO KEYS-SW                                                
016370     END-IF                                                               
016380                                                                          
016390     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
016393       MOVE NOO TO KEYS-SW                                                
016394     END-IF                                                               
016395                                                                          
016620     IF KEYS-WRONG                                                        
016630       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
016640       IF REQU-KDPGMACT = 'S' OR = 'U' OR = 'I' OR = 'D'                  
016650         CONTINUE                                                         
016660       ELSE                                                               
016670         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
016680         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
016690       END-IF                                                             
016697       IF REQU-IDMSGVER NUMERIC                                           
016698         CONTINUE                                                         
016699       ELSE                                                               
016700         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
016701         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
016702       END-IF                                                             
016710       IF REQU-IDUSER = SPACE OR = ALL '+'                                
016711         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
016712         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
016713       ELSE                                                               
016714         CONTINUE                                                         
016715       END-IF                                                             
016716     END-IF                                                               
016717                                                                          
016718     IF KEYS-OK                                                           
016719       PERFORM DB2-SELECT-T01LSEL-TAB                                     
016720       IF LINES-FOUND                                                     
016721         CONTINUE                                                         
016722       ELSE                                                               
016723         MOVE NOT-FOUND    TO RESP-IDMSG-ERROR                            
016724         MOVE 'IDLEGSEL'   TO RESP-IDELMT-ERROR                           
016725         MOVE NOO TO KEYS-SW                                              
016726       END-IF                                                             
016727     END-IF                                                               
016728     .                                                                    
016729     EJECT                                                                
016730                                                                          
016731*** - VALIDATE REQUESTED FIELDS FOR UPDATE/INSERT/DELETE                  
016740 D-VALIDATE-REQUEST SECTION.                                              
016750     IF ACT-CODE-UPDATE OR ACT-CODE-INSERT                                
016760       IF REQU-KDINVFRQ = 'NOW' OR = 'DAY' OR = 'WEEK' OR = ' '           
016761        OR = ALL '+'                                                      
016770         MOVE REQU-KDINVFRQ TO W-KDINVFRQ                                 
016771         IF REQU-KDINVFRQ = ALL '+'                                       
016772           MOVE SPACE TO W-KDINVFRQ                                       
016773         END-IF                                                           
016780       ELSE                                                               
016790         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
016791         MOVE 'KDINVFRQ' TO RESP-IDELMT-ERROR                             
016792*        MOVE NOO TO KEYS-SW                                              
016793       END-IF                                                             
016794                                                                          
016795       IF REQU-KDAPPEND = 'PGRP' OR = 'VAT' OR = 'ORDC' OR                
016796                          ' '    OR = ALL '+'                             
016797         MOVE REQU-KDAPPEND TO W-KDAPPEND                                 
016798         IF REQU-KDAPPEND = ALL '+'                                       
016799           MOVE SPACE TO W-KDAPPEND                                       
016800         END-IF                                                           
016801       ELSE                                                               
016802         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
016803         MOVE 'KDAPPEND' TO RESP-IDELMT-ERROR                             
016804*        MOVE NOO TO KEYS-SW                                              
016805       END-IF                                                             
016806                                                                          
016807       IF REQU-FLVAT = 'Y' OR REQU-FLVAT = 'N'                            
016809         MOVE REQU-FLVAT    TO WS-FLVAT                                   
016813       ELSE                                                               
016814         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
016815         MOVE 'FLVAT'       TO RESP-IDELMT-ERROR                          
016816*        MOVE NOO TO KEYS-SW                                              
016817       END-IF                                                             
016818                                                                          
016819       IF REQU-KDSTATUS-KEY = WS-COMING                                   
016820         IF REQU-DAUPPDAT NUMERIC                                         
016821           IF REQU-DAUPPDAT > WS-CURRENT-DATE                             
016822             MOVE REQU-DAUPPDAT TO DATE-TIDATE                            
016823             MOVE 'YYYYMMDD'  TO DATE-KDDATFMT                            
016824             CALL WZ20DATE USING DATE-WZ20DATE                            
016825             IF DATE-KDRC > ZERO                                          
016826               MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                 
016827               MOVE 'DAUPPDAT'      TO RESP-IDELMT-ERROR                  
016828*              MOVE NOO TO KEYS-SW                                        
016829             ELSE                                                         
016830               CONTINUE                                                   
016831             END-IF                                                       
016832           ELSE                                                           
016833             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
016834             MOVE 'DAUPPDAT'      TO RESP-IDELMT-ERROR                    
016835*            MOVE NOO TO KEYS-SW                                          
016836           END-IF                                                         
016837         ELSE                                                             
016838           MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
016839           MOVE 'DAUPPDAT'          TO RESP-IDELMT-ERROR                  
016840*          MOVE NOO TO KEYS-SW                                            
016841         END-IF                                                           
016850       END-IF                                                             
016867     END-IF                                                               
016868                                                                          
016869     IF REQU-KDPGMACT = 'I'                                               
016870       IF REQU-KDSTATUS-KEY = WS-COMING                                   
016871         MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
016872*        MOVE NOO TO KEYS-SW                                              
016873       END-IF                                                             
016874     END-IF                                                               
016875     .                                                                    
016876     EJECT                                                                
016877                                                                          
016880*** - MOVE KEYS AND COMPULSORY FIELDS TO RESPOND                          
016900 F-READ-SHOW-INFO SECTION.                                                
017010     MOVE REQU-IDLEGSEL-KEY TO RESP-IDLEGSEL-KEY                          
017030     MOVE REQU-KDPARTTY-KEY TO RESP-KDPARTTY-KEY                          
017031     MOVE REQU-KDPARTGR-KEY TO RESP-KDPARTGR-KEY                          
017040     MOVE REQU-KDSTATUS-KEY TO RESP-KDSTATUS-KEY                          
017042     MOVE WS-BELEGRAD-1     TO RESP-BELEGRAD-1                            
017050                                                                          
017100     PERFORM FA-READ-BASICDATA                                            
017800     .                                                                    
017900     EJECT                                                                
017901                                                                          
017910*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
018000 FA-READ-BASICDATA SECTION.                                               
018130     MOVE REQU-KDPGMACT     TO ACTION-CODE-SW                             
018200     IF ACT-CODE-SEARCH                                                   
018302       PERFORM FAA-SEARCH-T01CUGR                                         
018303     END-IF                                                               
018304                                                                          
018310     IF ACT-CODE-UPDATE                                                   
018317       PERFORM FAB-UPDATE-T01CUGR                                         
018320     END-IF                                                               
018321                                                                          
018330     IF ACT-CODE-INSERT                                                   
018331       IF REQU-FLVAT = 'Y'                                                
018332         MOVE 'J'        TO WS-FLVAT                                      
018333       ELSE                                                               
018334         MOVE REQU-FLVAT TO WS-FLVAT                                      
018335       END-IF                                                             
018336       PERFORM FAC-INSERT-T01CUGR                                         
018337     END-IF                                                               
018338                                                                          
018339     IF ACT-CODE-DELETE                                                   
018340       PERFORM FAD-DELETE-T01CUGR                                         
018350     END-IF                                                               
018600     .                                                                    
018700     EJECT                                                                
018701                                                                          
018702*** - SEARCH FOR RIGHT CUSTOMER AND MARK CURRENT LINE IF COMING           
018703*** - LINE EXIST.                                                         
018710 FAA-SEARCH-T01CUGR SECTION.                                              
018721     MOVE NOO TO WS-FLCOMING                                              
018722     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
018723       PERFORM DB2-SELECT-T01CUGR-CURRENT                                 
018734       IF LINES-FOUND                                                     
018739         PERFORM DB2-SELECT-T01CUGR-COMING-FL                             
018740         IF LINES-FOUND                                                   
018741           MOVE YES TO WS-FLCOMING                                        
018742           PERFORM S03-MOVE-SEARCH-TO-RESPOND                             
018744         ELSE                                                             
018745           MOVE NOO TO WS-FLCOMING                                        
018746           PERFORM S03-MOVE-SEARCH-TO-RESPOND                             
018747         END-IF                                                           
018748       ELSE                                                               
018749         MOVE NOT-FOUND    TO RESP-IDMSG-ERROR                            
018750         MOVE 'CUST.TYPE/GROUP'   TO RESP-IDELMT-ERROR                    
018751         MOVE NOO TO KEYS-SW                                              
018753       END-IF                                                             
018754     END-IF                                                               
018755                                                                          
018756     IF REQU-KDSTATUS-KEY = WS-COMING                                     
018757       PERFORM DB2-SELECT-T01CUGR-COMING                                  
018758       IF LINES-FOUND                                                     
018759         MOVE YES TO WS-FLCOMING                                          
018760         PERFORM S03-MOVE-SEARCH-TO-RESPOND                               
018761       ELSE                                                               
018763         MOVE NOO TO WS-FLCOMING                                          
018765         MOVE NOT-FOUND           TO RESP-IDMSG-ERROR                     
018766         MOVE 'CUST.TYPE/GROUP'   TO RESP-IDELMT-ERROR                    
018767         MOVE NOO TO KEYS-SW                                              
018769       END-IF                                                             
018770     END-IF                                                               
018792     .                                                                    
018793     EJECT                                                                
018794                                                                          
019302*** - CHECK IF UPDATE IS ON CURRENT OR COMING LINE                        
019303 FAB-UPDATE-T01CUGR SECTION.                                              
019305     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
019307       PERFORM FABA-UPD-CURRENT                                           
019308     END-IF                                                               
019309                                                                          
019310     IF REQU-KDSTATUS-KEY = WS-COMING                                     
019312       PERFORM FABB-UPD-COMING                                            
019314     END-IF                                                               
019315     .                                                                    
019316     EJECT                                                                
019317                                                                          
019318*** - UPDATE CURRENT LINE ON T01CUGR.                                     
019319 FABA-UPD-CURRENT SECTION.                                                
019325     PERFORM DB2-SELECT-T01CUGR-CURRENT                                   
019326     IF LINES-FOUND                                                       
019327       MOVE WS-CURRENT-DATE TO WS-DAUPPDAT                                
019328       IF REQU-FLVAT = 'Y'                                                
019329         MOVE 'J'        TO WS-FLVAT                                      
019330       ELSE                                                               
019331         MOVE REQU-FLVAT TO WS-FLVAT                                      
019332       END-IF                                                             
019333       PERFORM DB2-UPDATE-T01CUGR-CURRENT                                 
019334       PERFORM S04-MOVE-UPD-INS-TO-RESPOND                                
019335       PERFORM DB2-SELECT-T01CUGR-COMING-FL                               
019336       IF LINES-FOUND                                                     
019337         MOVE YES TO WS-FLCOMING                                          
019338         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
019340       ELSE                                                               
019341         MOVE NOO TO WS-FLCOMING                                          
019342         MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                            
019344       END-IF                                                             
019345     ELSE                                                                 
019346       MOVE NOT-FOUND TO RESP-IDMSG-ERROR                                 
019348     END-IF                                                               
019349     .                                                                    
019350     EJECT                                                                
019351                                                                          
019352*** - UPDATE COMING LINE ON T01CUGR. IF NOO COMING LINE EXIST BUT         
019353***   COMING LINE IS CHOOSED FOR UPDATE, PROGRAM WILL INSERT ONE          
019354***   COMING LINE WITH DATA FROM CURRENT LINE BUT USER WILL SEE           
019355***   THIS AS AN UPDATE.                                                  
019356 FABB-UPD-COMING SECTION.                                                 
019357     PERFORM DB2-SELECT-T01CUGR-COMING                                    
019358     IF LINES-FOUND                                                       
019359       PERFORM DB2-SELECT-T01CUGR-CURRENT                                 
019360       IF LINES-FOUND                                                     
019361         IF REQU-FLVAT = 'Y'                                              
019362           MOVE 'J'        TO WS-FLVAT                                    
019363         ELSE                                                             
019364           MOVE REQU-FLVAT TO WS-FLVAT                                    
019365         END-IF                                                           
019366         PERFORM DB2-UPDATE-T01CUGR-COMING                                
019367*        MOVE '00000000' TO WS-DAREGDAT                                   
019368         MOVE REQU-DAUPPDAT TO WS-DAUPPDAT                                
019369         PERFORM S04-MOVE-UPD-INS-TO-RESPOND                              
019370         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
019371         MOVE YES TO WS-FLCOMING                                          
019372       ELSE                                                               
019373         MOVE ERR-UPDATE-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
019375       END-IF                                                             
019376     ELSE                                                                 
019377       IF REQU-DAUPPDAT NUMERIC                                           
019378         IF REQU-DAUPPDAT > WS-CURRENT-DATE                               
019379           PERFORM DB2-SELECT-T01CUGR-CURRENT                             
019380           IF LINES-FOUND                                                 
019381             IF REQU-FLVAT = 'Y'                                          
019382               MOVE 'J'        TO WS-FLVAT                                
019383             ELSE                                                         
019384               MOVE REQU-FLVAT TO WS-FLVAT                                
019385             END-IF                                                       
019386             PERFORM DB2-INSERT-T01CUGR-COMING                            
019387             MOVE '00000000' TO WS-DAREGDAT                               
019388             MOVE REQU-DAUPPDAT TO WS-DAUPPDAT                            
019389             PERFORM S04-MOVE-UPD-INS-TO-RESPOND                          
019390             MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO              
019391             MOVE YES TO WS-FLCOMING                                      
019392           ELSE                                                           
019393             MOVE ERR-UPDATE-NOT-ALLOWED TO RESP-IDMSG-ERROR              
019395           END-IF                                                         
019396         ELSE                                                             
019397           MOVE ERR-UPDATE-NOT-ALLOWED TO RESP-IDMSG-ERROR                
019398           MOVE 'DAUPPDAT'             TO RESP-IDELMT-ERROR               
019400         END-IF                                                           
019401       ELSE                                                               
019402         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
019403         MOVE 'DAUPPDAT' TO RESP-IDELMT-ERROR                             
019405       END-IF                                                             
019406     END-IF                                                               
019407     .                                                                    
019408     EJECT                                                                
019409                                                                          
019410*** - INSERT NEW CURRENT LINE. COMING LINE COULD NOT BE INSERTED.         
019411 FAC-INSERT-T01CUGR SECTION.                                              
019412     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
019413       PERFORM DB2-SELECT-T01CUGR-INSERT                                  
019414       IF LINES-FOUND                                                     
019415         IF WS-DADELDAT = '00000000'                                      
019416           MOVE ERR-ALREADY-EXIST TO RESP-IDMSG-ERROR                     
019417           MOVE 'KDPARTGR' TO RESP-IDELMT-ERROR                           
019419         ELSE                                                             
019420           MOVE '00000000' TO WS-DADELDAT                                 
019421           IF REQU-FLVAT = 'Y'                                            
019422             MOVE 'J'        TO WS-FLVAT                                  
019423           ELSE                                                           
019424             MOVE REQU-FLVAT TO WS-FLVAT                                  
019425           END-IF                                                         
019426           PERFORM DB2-UPDATE-T01CUGR-CURRENT                             
019427           PERFORM S04-MOVE-UPD-INS-TO-RESPOND                            
019428           MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                          
019429         END-IF                                                           
019430       ELSE                                                               
019431         MOVE WS-CURRENT-DATE TO WS-DAREGDAT                              
019432         IF REQU-FLVAT = 'Y'                                              
019433           MOVE 'J'        TO WS-FLVAT                                    
019434         ELSE                                                             
019435           MOVE REQU-FLVAT TO WS-FLVAT                                    
019436         END-IF                                                           
019437         PERFORM DB2-INSERT-T01CUGR-CURRENT                               
019438         PERFORM S04-MOVE-UPD-INS-TO-RESPOND                              
019439         MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                            
019440       END-IF                                                             
019441     ELSE                                                                 
019442       MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
019444     END-IF                                                               
019445     .                                                                    
019446     EJECT                                                                
019447                                                                          
019448 FAD-DELETE-T01CUGR SECTION.                                              
019449     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
019450       PERFORM DB2-SELECT-T01FCUS                                         
019451       IF LINES-FOUND                                                     
019452         MOVE ERR-DELETE-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
019454       ELSE                                                               
019455         PERFORM DB2-SELECT-T01CUGR-CURRENT                               
019456         IF LINES-FOUND                                                   
019457           MOVE WS-CURRENT-DATE TO WS-DADELDAT                            
019458           PERFORM DB2-UPDATE-T01CUGR-CURRENT                             
019459           PERFORM S05-MOVE-DEL-TO-RESPOND                                
019460           MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                          
019461           PERFORM DB2-SELECT-T01CUGR-COMING                              
019462           IF LINES-FOUND                                                 
019463             PERFORM DB2-DELETE-T01CUGR-COMING                            
019464           END-IF                                                         
019465         ELSE                                                             
019466           MOVE NOT-FOUND TO RESP-IDMSG-ERROR                             
019468         END-IF                                                           
019469       END-IF                                                             
019470     END-IF                                                               
019471                                                                          
019472     IF REQU-KDSTATUS-KEY = WS-COMING                                     
019473       PERFORM DB2-SELECT-T01CUGR-COMING                                  
019474       IF LINES-FOUND                                                     
019475         MOVE WS-CURRENT-DATE TO WS-DADELDAT                              
019476         PERFORM DB2-DELETE-T01CUGR-COMING                                
019477         PERFORM S05-MOVE-DEL-TO-RESPOND                                  
019478         MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                            
019479       ELSE                                                               
019480         MOVE NOT-FOUND           TO RESP-IDMSG-ERROR                     
019481         MOVE 'CUST.TYPE/GROUP'   TO RESP-IDELMT-ERROR                    
019483       END-IF                                                             
019484     END-IF                                                               
019485     .                                                                    
019486     EJECT                                                                
019487                                                                          
019488*    --- DISPATCHER SECTIONS                                              
019490 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019500     MOVE 'GETARG'                   TO SUB-KDFUNC                        
019600     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
019710     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
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
020800     SKIP3                                                                
020810                                                                          
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
022320 S03-MOVE-SEARCH-TO-RESPOND SECTION.                                      
022340     MOVE WS-FLCOMING       TO RESP-FLCOMING                              
022350     MOVE WS-KDINVFRQ       TO RESP-KDINVFRQ                              
022360     MOVE WS-KDAPPEND       TO RESP-KDAPPEND                              
022370     IF WS-FLVAT = 'J'                                                    
022380       MOVE 'Y'             TO RESP-FLVAT                                 
022381     ELSE                                                                 
022390       MOVE WS-FLVAT        TO RESP-FLVAT                                 
022391     END-IF                                                               
022392     MOVE WS-DAREGDAT       TO RESP-DAREGDAT                              
022393     MOVE WS-DAUPPDAT       TO RESP-DAUPPDAT                              
022394     MOVE WS-DADELDAT       TO RESP-DADELDAT                              
022395     MOVE WS-IDUSER         TO RESP-IDUSER                                
022416     .                                                                    
022417     EJECT                                                                
022420                                                                          
022430 S04-MOVE-UPD-INS-TO-RESPOND SECTION.                                     
022450     MOVE REQU-FLCOMING     TO RESP-FLCOMING                              
022460     MOVE REQU-KDINVFRQ     TO RESP-KDINVFRQ                              
022470     MOVE REQU-FLVAT        TO RESP-FLVAT                                 
022493     MOVE WS-DAREGDAT       TO WS-DAREGDAT-2                              
022494     MOVE WS-DAREGDAT-2     TO RESP-DAREGDAT                              
022495     MOVE WS-DAUPPDAT       TO WS-DAUPPDAT-2                              
022496     MOVE WS-DAUPPDAT-2     TO RESP-DAUPPDAT                              
022497     MOVE WS-DADELDAT       TO WS-DADELDAT-2                              
022498     MOVE WS-DADELDAT-2     TO RESP-DADELDAT                              
022499     MOVE REQU-IDUSER       TO RESP-IDUSER                                
022523     .                                                                    
022524     EJECT                                                                
022525                                                                          
022526 S05-MOVE-DEL-TO-RESPOND SECTION.                                         
022527     MOVE SPACE             TO RESP-FLCOMING                              
022528     MOVE SPACE             TO RESP-KDINVFRQ                              
022529     MOVE SPACE             TO RESP-KDAPPEND                              
022530     MOVE SPACE             TO RESP-FLVAT                                 
022534     MOVE ZERO              TO WS-DAREGDAT-2                              
022535     MOVE WS-DAREGDAT-2     TO RESP-DAREGDAT                              
022536     MOVE ZERO              TO WS-DAUPPDAT-2                              
022537     MOVE WS-DAUPPDAT-2     TO RESP-DAUPPDAT                              
022538     MOVE WS-DADELDAT       TO WS-DADELDAT-2                              
022539     MOVE WS-DADELDAT-2     TO RESP-DADELDAT                              
022540     MOVE REQU-IDUSER       TO RESP-IDUSER                                
022541     .                                                                    
022542     EJECT                                                                
022543                                                                          
022544 S06-MOVE-MISSING-TO-RESPOND SECTION.                                     
022545     MOVE SPACE             TO RESP-FLCOMING                              
022546     MOVE SPACE             TO RESP-KDINVFRQ                              
022547     MOVE SPACE             TO RESP-KDAPPEND                              
022548     MOVE SPACE             TO RESP-FLVAT                                 
022549     MOVE SPACE             TO RESP-IDUSER                                
022550     MOVE ZERO              TO RESP-DAREGDAT                              
022552     MOVE ZERO              TO RESP-DAUPPDAT                              
022554     MOVE ZERO              TO RESP-DADELDAT                              
022556     .                                                                    
022557     EJECT                                                                
022558                                                                          
022560*    --- DB2 SECTIONS                                                     
022600 DB2-SELECT-T01LSEL-TAB  SECTION.                                         
022800     MOVE 000100 TO GOOD-SQLCODECODES                                     
023000     EXEC SQL                                                             
023100           SELECT  BELEGRAD_1                                             
023300                                                                          
023400           INTO   :WS-BELEGRAD-1                                          
023600                                                                          
023700           FROM    T01LSEL                                                
023800                                                                          
023900           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
024000               AND KDSTATUS = :WS-CURRENT                                 
024100     END-EXEC                                                             
024800     MOVE SQLCODE TO SQLCODE-WS                                           
024900     PERFORM DB2-STATUS-CHECK                                             
025000     .                                                                    
025100     EJECT                                                                
025200                                                                          
030400 DB2-SELECT-T01CUGR-CURRENT SECTION.                                      
030600     MOVE 000100  TO GOOD-SQLCODECODES                                    
030700     EXEC SQL                                                             
030800         SELECT  KDINVFRQ,                                                
030801                 KDAPPEND,                                                
030802                 FLVAT,                                                   
030806                 DAREGDAT,                                                
030807                 DAUPPDAT,                                                
030808                 DADELDAT,                                                
030809                 IDUSER                                                   
030810                                                                          
030820         INTO    :WS-KDINVFRQ,                                            
030830                 :WS-KDAPPEND,                                            
030840                 :WS-FLVAT,                                               
030880                 :WS-DAREGDAT,                                            
030890                 :WS-DAUPPDAT,                                            
030900                 :WS-DADELDAT,                                            
031000                 :WS-IDUSER                                               
032100                                                                          
032200         FROM    T01CUGR                                                  
032210                                                                          
032300         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
032310             AND KDPARTTY = :REQU-KDPARTTY-KEY                            
032320             AND KDPARTGR = :REQU-KDPARTGR-KEY                            
032330             AND KDSTATUS = :WS-CURRENT                                   
032340             AND DADELDAT = '00000000'                                    
032400     END-EXEC                                                             
032600     MOVE SQLCODE TO SQLCODE-WS                                           
032700     PERFORM DB2-STATUS-CHECK                                             
032800     .                                                                    
032900     EJECT                                                                
032901                                                                          
032902 DB2-SELECT-T01CUGR-COMING SECTION.                                       
032903     MOVE 000100  TO GOOD-SQLCODECODES                                    
032904     EXEC SQL                                                             
032905         SELECT  KDINVFRQ,                                                
032906                 KDAPPEND,                                                
032907                 FLVAT,                                                   
032911                 DAREGDAT,                                                
032912                 DAUPPDAT,                                                
032913                 DADELDAT,                                                
032914                 IDUSER                                                   
032915                                                                          
032916         INTO    :WS-KDINVFRQ,                                            
032917                 :WS-KDAPPEND,                                            
032918                 :WS-FLVAT,                                               
032922                 :WS-DAREGDAT,                                            
032923                 :WS-DAUPPDAT,                                            
032924                 :WS-DADELDAT,                                            
032925                 :WS-IDUSER                                               
032929                                                                          
032930         FROM    T01CUGR                                                  
032931                                                                          
032932         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
032933             AND KDPARTTY = :REQU-KDPARTTY-KEY                            
032940             AND KDPARTGR = :REQU-KDPARTGR-KEY                            
032950             AND KDSTATUS = :WS-COMING                                    
032960             AND DADELDAT = '00000000'                                    
032970     END-EXEC                                                             
032990     MOVE SQLCODE TO SQLCODE-WS                                           
033000     PERFORM DB2-STATUS-CHECK                                             
033010     .                                                                    
033020     EJECT                                                                
033021                                                                          
033022 DB2-SELECT-T01CUGR-COMING-FL SECTION.                                    
033023     MOVE 000100  TO GOOD-SQLCODECODES                                    
033024     EXEC SQL                                                             
033025         SELECT  IDLEGSEL                                                 
033035                                                                          
033036         INTO    :WS-IDLEGSEL                                             
033046                                                                          
033047         FROM    T01CUGR                                                  
033048                                                                          
033049         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
033050             AND KDPARTTY = :REQU-KDPARTTY-KEY                            
033051             AND KDPARTGR = :REQU-KDPARTGR-KEY                            
033052             AND KDSTATUS = :WS-COMING                                    
033053             AND DADELDAT = '00000000'                                    
033054     END-EXEC                                                             
033055     MOVE SQLCODE TO SQLCODE-WS                                           
033056     PERFORM DB2-STATUS-CHECK                                             
033057     .                                                                    
033058     EJECT                                                                
033059                                                                          
033060 DB2-SELECT-T01CUGR-INSERT SECTION.                                       
033061     MOVE 000100  TO GOOD-SQLCODECODES                                    
033062     EXEC SQL                                                             
033063         SELECT  KDINVFRQ,                                                
033064                 KDAPPEND,                                                
033065                 FLVAT,                                                   
033069                 DAREGDAT,                                                
033070                 DAUPPDAT,                                                
033071                 DADELDAT,                                                
033072                 IDUSER                                                   
033073                                                                          
033074         INTO    :WS-KDINVFRQ,                                            
033075                 :WS-KDAPPEND,                                            
033076                 :WS-FLVAT,                                               
033080                 :WS-DAREGDAT,                                            
033081                 :WS-DAUPPDAT,                                            
033082                 :WS-DADELDAT,                                            
033083                 :WS-IDUSER                                               
033084                                                                          
033085         FROM    T01CUGR                                                  
033086                                                                          
033087         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
033088             AND KDPARTTY = :REQU-KDPARTTY-KEY                            
033089             AND KDPARTGR = :REQU-KDPARTGR-KEY                            
033090             AND KDSTATUS = :WS-CURRENT                                   
033091     END-EXEC                                                             
033092     MOVE SQLCODE TO SQLCODE-WS                                           
033093     PERFORM DB2-STATUS-CHECK                                             
033094     .                                                                    
033095     EJECT                                                                
033096                                                                          
033097 DB2-UPDATE-T01CUGR-CURRENT SECTION.                                      
033098     MOVE 000     TO GOOD-SQLCODECODES                                    
033099     EXEC SQL                                                             
033100          UPDATE T01CUGR                                                  
033101          SET DAUPPDAT  = :WS-CURRENT-DATE,                               
033102              DADELDAT  = :WS-DADELDAT,                                   
033103              KDINVFRQ  = :W-KDINVFRQ,                                    
033104              KDAPPEND  = :W-KDAPPEND,                                    
033105              FLVAT     = :WS-FLVAT,                                      
033109              IDUSER    = :REQU-IDUSER                                    
033110                                                                          
033111          WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                             
033112          AND   KDPARTTY = :REQU-KDPARTTY-KEY                             
033113          AND   KDPARTGR = :REQU-KDPARTGR-KEY                             
033114          AND   KDSTATUS = :WS-CURRENT                                    
033115     END-EXEC                                                             
033116     MOVE SQLCODE TO SQLCODE-WS                                           
033117     PERFORM DB2-STATUS-CHECK                                             
033118     .                                                                    
033119     EJECT                                                                
033120                                                                          
033121 DB2-UPDATE-T01CUGR-COMING SECTION.                                       
033122     MOVE 000     TO GOOD-SQLCODECODES                                    
033123     EXEC SQL                                                             
033124          UPDATE T01CUGR                                                  
033125          SET DAUPPDAT  = :REQU-DAUPPDAT,                                 
033126              DADELDAT  = :WS-DADELDAT,                                   
033127              KDINVFRQ  = :W-KDINVFRQ,                                    
033128              KDAPPEND  = :W-KDAPPEND,                                    
033129              FLVAT     = :WS-FLVAT,                                      
033133              IDUSER    = :REQU-IDUSER,                                   
033134              KDSTATUS  = :WS-COMING                                      
033135                                                                          
033136          WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                             
033137          AND   KDPARTTY = :REQU-KDPARTTY-KEY                             
033138          AND   KDPARTGR = :REQU-KDPARTGR-KEY                             
033139          AND   KDSTATUS = :WS-COMING                                     
033140     END-EXEC                                                             
033141     MOVE SQLCODE TO SQLCODE-WS                                           
033142     PERFORM DB2-STATUS-CHECK                                             
033143     .                                                                    
033144     EJECT                                                                
033145                                                                          
033146 DB2-INSERT-T01CUGR-CURRENT SECTION.                                      
033147     MOVE 000     TO GOOD-SQLCODECODES                                    
033148     EXEC SQL                                                             
033149        INSERT INTO T01CUGR                                               
033150          (IDLEGSEL,                                                      
033151           KDPARTTY,                                                      
033152           KDPARTGR,                                                      
033153           KDSTATUS,                                                      
033154           KDINVFRQ,                                                      
033155           KDAPPEND,                                                      
033156           FLVAT,                                                         
033160           DAREGDAT,                                                      
033161           DAUPPDAT,                                                      
033162           DADELDAT,                                                      
033163           IDUSER)                                                        
033164        VALUES(:REQU-IDLEGSEL-KEY,                                        
033165               :REQU-KDPARTTY-KEY,                                        
033166               :REQU-KDPARTGR-KEY,                                        
033167               :WS-CURRENT,                                               
033168               :W-KDINVFRQ,                                               
033169               :W-KDAPPEND,                                               
033170               :WS-FLVAT,                                                 
033174               :WS-CURRENT-DATE,                                          
033175               '00000000',                                                
033176               :WS-DADELDAT,                                              
033177               :REQU-IDUSER)                                              
033178     END-EXEC                                                             
033179     MOVE SQLCODE TO SQLCODE-WS                                           
033180     PERFORM DB2-STATUS-CHECK                                             
033181     .                                                                    
033182     EJECT                                                                
033183                                                                          
033184 DB2-INSERT-T01CUGR-COMING SECTION.                                       
033185     MOVE 000     TO GOOD-SQLCODECODES                                    
033186     EXEC SQL                                                             
033187        INSERT INTO T01CUGR                                               
033188          (IDLEGSEL,                                                      
033189           KDPARTTY,                                                      
033190           KDPARTGR,                                                      
033191           KDSTATUS,                                                      
033192           KDINVFRQ,                                                      
033193           KDAPPEND,                                                      
033194           FLVAT,                                                         
033198           DAREGDAT,                                                      
033199           DAUPPDAT,                                                      
033200           DADELDAT,                                                      
033201           IDUSER)                                                        
033202        VALUES(:REQU-IDLEGSEL-KEY,                                        
033203               :REQU-KDPARTTY-KEY,                                        
033204               :REQU-KDPARTGR-KEY,                                        
033205               :WS-COMING,                                                
033206               :W-KDINVFRQ,                                               
033207               :W-KDAPPEND,                                               
033208               :WS-FLVAT,                                                 
033212               :WS-DAREGDAT,                                              
033213               :REQU-DAUPPDAT,                                            
033214               :WS-DADELDAT,                                              
033215               :REQU-IDUSER)                                              
033216     END-EXEC                                                             
033217     MOVE SQLCODE TO SQLCODE-WS                                           
033218     PERFORM DB2-STATUS-CHECK                                             
033219     .                                                                    
033220     EJECT                                                                
033221                                                                          
033222 DB2-DELETE-T01CUGR-COMING SECTION.                                       
033223     MOVE 000    TO GOOD-SQLCODECODES                                     
033224     EXEC SQL                                                             
033225           DELETE FROM T01CUGR                                            
033226           WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                            
033227           AND   KDPARTTY = :REQU-KDPARTTY-KEY                            
033228           AND   KDPARTGR = :REQU-KDPARTGR-KEY                            
033229           AND   KDSTATUS = :WS-COMING                                    
033230     END-EXEC                                                             
033231     MOVE SQLCODE TO SQLCODE-WS                                           
033232     PERFORM DB2-STATUS-CHECK                                             
033233     .                                                                    
033234     EJECT                                                                
033235                                                                          
033240 DB2-SELECT-T01FCUS SECTION.                                              
033300     MOVE 000100 TO GOOD-SQLCODECODES                                     
033500     EXEC SQL                                                             
034594           SELECT IDLEGSEL                                                
034600                                                                          
034601           INTO :WS-IDLEGSEL                                              
034602                                                                          
034610           FROM     T01FCUS                                               
034620                                                                          
034630           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
034631                AND KDPARTTY = :REQU-KDPARTTY-KEY                         
034640                AND KDPARTGR = :REQU-KDPARTGR-KEY                         
034641                AND DADELDAT = '00000000'                                 
034700                                                                          
034710     END-EXEC                                                             
035200     MOVE SQLCODE TO SQLCODE-WS                                           
035300     PERFORM DB2-STATUS-CHECK                                             
035400     .                                                                    
035410     EJECT                                                                
035500                                                                          
038410 DB2-STATUS-CHECK  SECTION.                                               
038600     SET SQLCODE-IX TO 1                                                  
038700     SEARCH GOOD-SQLCODE                                                  
038800       AT END                                                             
038900          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
039000          DELIMITED BY SIZE INTO ERROR-TEXT                               
039100          CALL ABEND USING RKOD-ABEND-DB2                                 
039200       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
039300     END-SEARCH                                                           
039400     .                                                                    
