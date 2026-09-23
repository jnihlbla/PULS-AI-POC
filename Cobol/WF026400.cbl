000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF026400.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   02/03/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610*    NAME:                                                                
000620*        CARPARTS.BILLIT.APPRDOCMAINTENANCE                               
000700*    FUNCTION:                                                            
000800*        READ/UPDATE/INSERT/DELETE DOCUMENT TYPE                          
000810*        TABLE T01DOTY DEPENDING ON                                       
000900*        REQUESTED PROGRAMS ACTION CODE (KDPGMACT)                        
000910*        KDPGMACT = 'S' READ                                              
000920*        KDPGMACT = 'U' UPDATE                                            
000930*        KDPGMACT = 'I' INSERT                                            
000940*        KDPGMACT = 'D' DELETE                                            
001000*                                                                         
001100*        THE PROGRAM READS   TABLE T01LSEL                                
001400*        THE PROGRAM READS   TABLE T01NSAS                                
001500*        THE PROGRAM UPDATES TABLE T01DOTY                                
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: WF0264U                                             
001900*        REQUEST:     WF0264I1                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        RESPONSE:    WF0264O1                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002700 INPUT-OUTPUT SECTION.                                                    
002900 FILE-CONTROL.                                                            
003100 DATA DIVISION.                                                           
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'WF026400'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003910 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004000 77  KDRC-DISPLAY                PIC Z(5).                                
004100                                                                          
004110*    --- CONSTANT WORK FIELDS                                             
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004311 77  WS-ADRESS                   PIC X(50)                                
004312             VALUE 'CARPARTS.BILLIT.APPRDOCMAINTENANCE'.                  
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
004933 01  WS-KDFINDOC                 PIC X(4)    VALUE SPACE.                 
004934 01  WS-BEFINDOC                 PIC X(35)   VALUE SPACE.                 
004935 01  WS-KDSTATUS                 PIC 9(3)    VALUE ZERO.                  
004936 01  WS-BELEGRAD-1               PIC X(35)   VALUE SPACE.                 
004938 01  WS-FLCOMING                 PIC X       VALUE SPACE.                 
004939 01  WS-FLAP                     PIC X       VALUE SPACE.                 
004940 01  WS-FLAR                     PIC X       VALUE SPACE.                 
004941 01  WS-FLGL                     PIC X       VALUE SPACE.                 
004942 01  WS-FLINTREP                 PIC X       VALUE SPACE.                 
004943 01  WS-FLVATREP                 PIC X       VALUE SPACE.                 
004944 01  WS-FLCUSREP                 PIC X       VALUE SPACE.                 
004945 01  WS-DAREGDAT                 PIC X(8)    VALUE SPACE.                 
004946 01  WS-DAREGDAT-2               PIC 9(8)    VALUE ZERO.                  
004947 01  WS-DAUPPDAT                 PIC X(8)    VALUE SPACE.                 
004948 01  WS-DAUPPDAT-2               PIC 9(8)    VALUE ZERO.                  
004949 01  WS-DADELDAT                 PIC X(8)    VALUE SPACE.                 
004950 01  WS-DADELDAT-2               PIC 9(8)    VALUE ZERO.                  
004951 01  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
004952 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
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
006430         05  INF-OLD-VERSION-DELETED PIC X(3)    VALUE '102'.             
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
007500*    03  -COPY WF0264I1                                                   
007600     EJECT                                                                
007610                                                                          
007700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007800     SKIP3                                                                
007900 01  RESP-AREA.                                                           
008000*    03  -COPY WZ01RESP                                                   
008100*    03  -COPY WF0264O1                                                   
008300     EJECT                                                                
008310                                                                          
008320 01  WZ20DATE PIC X(8) VALUE 'WZ20DATE'.                                  
008330     SKIP3                                                                
008340*    -COPY WZ20DATE                                                       
008350     EJECT                                                                
008360                                                                          
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
011200 01  FILLER                      PIC X(16)   VALUE 'T01DOTY-AREA'.        
011400*01  -COPY T01DOTY -PRE T01DOTY-                                          
011500     EJECT                                                                
011501                                                                          
011600 01  FILLER                      PIC X(16)   VALUE 'T01NSAS-AREA'.        
011800*01  -COPY T01NSAS -PRE T01NSAS-                                          
011900     EJECT                                                                
011910                                                                          
012000     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
012100     EJECT                                                                
012600     EXEC SQL INCLUDE T01DOTY END-EXEC.                                   
012610     EJECT                                                                
012800     EXEC SQL INCLUDE T01NSAS END-EXEC.                                   
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
014130         END-IF                                                           
014300       END-IF                                                             
014310       IF KEYS-WRONG                                                      
014320         PERFORM S06-MOVE-MISSING-TO-RESPOND                              
014330       END-IF                                                             
014400       PERFORM S02-RETURN-RESPONSE                                        
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
016320     AND REQU-KDFINDOC-KEY = SPACE OR = ALL '+'                           
016321     AND REQU-IDMSGVER NOT NUMERIC                                        
016322       MOVE NOO TO KEYS-SW                                                
016323     ELSE                                                                 
016324       IF ACT-CODE-VALID                                                  
016340       AND (REQU-KDSTATUS-KEY = WS-CURRENT OR WS-COMING)                  
016350         CONTINUE                                                         
016357       ELSE                                                               
016358         MOVE NOO TO KEYS-SW                                              
016359       END-IF                                                             
016370     END-IF                                                               
016380                                                                          
016390     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
016394       MOVE NOO TO KEYS-SW                                                
016395     END-IF                                                               
016396                                                                          
016610     IF KEYS-WRONG                                                        
016620       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
016630       IF REQU-KDPGMACT = 'S' OR = 'U' OR = 'I' OR = 'D'                  
016640         CONTINUE                                                         
016650       ELSE                                                               
016660         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
016670         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
016680       END-IF                                                             
016690       IF REQU-IDMSGVER NUMERIC                                           
016691         CONTINUE                                                         
016692       ELSE                                                               
016693         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
016694         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
016695       END-IF                                                             
016696       IF REQU-IDUSER = SPACE OR = ALL '+'                                
016697         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
016698         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
016699       ELSE                                                               
016700         CONTINUE                                                         
016701       END-IF                                                             
016702     END-IF                                                               
016703                                                                          
016704     IF KEYS-OK                                                           
016705       PERFORM DB2-SELECT-T01LSEL-TAB                                     
016706       IF LINES-FOUND                                                     
016707         CONTINUE                                                         
016708       ELSE                                                               
016709         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
016710         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
016711         MOVE NOO TO KEYS-SW                                              
016712       END-IF                                                             
016713     END-IF                                                               
016714     .                                                                    
016715     EJECT                                                                
016720                                                                          
016730*** - VALIDATE REQUESTED FIELDS FOR UPDATE/INSERT/DELETE                  
016740 D-VALIDATE-REQUEST SECTION.                                              
016750     IF REQU-KDPGMACT = 'U' OR = 'I'                                      
016760       IF REQU-BEFINDOC = SPACE OR = ALL '+'                              
016790         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
016791         MOVE 'BEFINDOC' TO RESP-IDELMT-ERROR                             
016792         MOVE NOO TO KEYS-SW                                              
016793       ELSE                                                               
016794         IF REQU-FLAP = 'Y' OR = 'N'                                      
016795           IF REQU-FLAR = 'Y' OR = 'N'                                    
016796             IF REQU-FLGL = 'Y' OR = 'N'                                  
016797               IF REQU-FLINTREP = 'Y' OR = 'N'                            
016798                 IF REQU-FLVATREP = 'Y' OR = 'N'                          
016799                   IF REQU-FLCUSREP = 'Y' OR = 'N'                        
016800                     CONTINUE                                             
016801                   ELSE                                                   
016802                     MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR           
016803                     MOVE 'FLCUSREP' TO RESP-IDELMT-ERROR                 
016805                   END-IF                                                 
016806                 ELSE                                                     
016807                   MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR             
016808                   MOVE 'FLVATREP' TO RESP-IDELMT-ERROR                   
016810                 END-IF                                                   
016811               ELSE                                                       
016812                 MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR               
016813                 MOVE 'FLINTREP' TO RESP-IDELMT-ERROR                     
016815               END-IF                                                     
016816             ELSE                                                         
016817               MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                 
016818               MOVE 'FLGL' TO RESP-IDELMT-ERROR                           
016820             END-IF                                                       
016821           ELSE                                                           
016822             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
016823             MOVE 'FLAR' TO RESP-IDELMT-ERROR                             
016825           END-IF                                                         
016826         ELSE                                                             
016827           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
016828           MOVE 'FLAP' TO RESP-IDELMT-ERROR                               
016830         END-IF                                                           
016831       END-IF                                                             
016832     END-IF                                                               
016833                                                                          
016834     IF REQU-KDPGMACT = 'U' OR = 'I'                                      
016835       IF REQU-KDSTATUS-KEY = WS-COMING                                   
016836         IF REQU-DAUPPDAT NUMERIC                                         
016837           IF REQU-DAUPPDAT > WS-CURRENT-DATE                             
016838             MOVE REQU-DAUPPDAT TO DATE-TIDATE                            
016839             MOVE 'YYYYMMDD'  TO DATE-KDDATFMT                            
016840             CALL WZ20DATE USING DATE-WZ20DATE                            
016841             IF DATE-KDRC > ZERO                                          
016842               MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                 
016843               MOVE 'DAUPPDAT'      TO RESP-IDELMT-ERROR                  
016845             ELSE                                                         
016846               CONTINUE                                                   
016847             END-IF                                                       
016848           ELSE                                                           
016849             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
016850             MOVE 'DAUPPDAT'      TO RESP-IDELMT-ERROR                    
016852           END-IF                                                         
016853         ELSE                                                             
016854           MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
016855           MOVE 'DAUPPDAT'          TO RESP-IDELMT-ERROR                  
016857         END-IF                                                           
016858       END-IF                                                             
016859     END-IF                                                               
016860                                                                          
016861     IF REQU-KDPGMACT = 'I'                                               
016862       IF REQU-KDSTATUS-KEY = WS-COMING                                   
016863         MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
016865       END-IF                                                             
016866     END-IF                                                               
016867     .                                                                    
016868     EJECT                                                                
016869                                                                          
016870*** - MOVE KEYS AND COMPULSORY FIELDS TO RESPOND                          
016900 F-READ-SHOW-INFO SECTION.                                                
017010     MOVE REQU-IDLEGSEL-KEY TO RESP-IDLEGSEL-KEY                          
017030     MOVE REQU-KDFINDOC-KEY TO RESP-KDFINDOC-KEY                          
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
018302       PERFORM FAA-SEARCH-T01DOTY                                         
018303     END-IF                                                               
018304                                                                          
018310     IF ACT-CODE-UPDATE                                                   
018315       PERFORM FAB-UPDATE-T01DOTY                                         
018320     END-IF                                                               
018321                                                                          
018330     IF ACT-CODE-INSERT                                                   
018334       PERFORM FAC-INSERT-T01DOTY                                         
018335     END-IF                                                               
018336                                                                          
018337     IF ACT-CODE-DELETE                                                   
018338       PERFORM FAD-DELETE-T01DOTY                                         
018339     END-IF                                                               
018600     .                                                                    
018700     EJECT                                                                
018701                                                                          
018702*** - SEARCH FOR RIGHT CUSTOMER AND MARK CURRENT LINE IF COMING           
018703*** - LINE EXIST.                                                         
018710 FAA-SEARCH-T01DOTY SECTION.                                              
018721     MOVE NOO TO WS-FLCOMING                                              
018722     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
018723       PERFORM DB2-SELECT-T01DOTY-CURRENT                                 
018734       IF LINES-FOUND                                                     
018739         PERFORM DB2-SELECT-T01DOTY-COMING-FL                             
018740         IF LINES-FOUND                                                   
018741           MOVE YES TO WS-FLCOMING                                        
018742           PERFORM S03-MOVE-SEARCH-TO-RESPOND                             
018744         ELSE                                                             
018745           MOVE NOO TO WS-FLCOMING                                        
018746           PERFORM S03-MOVE-SEARCH-TO-RESPOND                             
018747         END-IF                                                           
018748       ELSE                                                               
018751         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
018752         MOVE 'KDFINDOC' TO RESP-IDELMT-ERROR                             
018754       END-IF                                                             
018755     END-IF                                                               
018756                                                                          
018757     IF REQU-KDSTATUS-KEY = WS-COMING                                     
018758       PERFORM DB2-SELECT-T01DOTY-COMING                                  
018759       IF LINES-FOUND                                                     
018760         MOVE YES TO WS-FLCOMING                                          
018761         PERFORM S03-MOVE-SEARCH-TO-RESPOND                               
018762       ELSE                                                               
018763         MOVE NOO TO WS-FLCOMING                                          
018764         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
018765         MOVE 'KDFINDOC' TO RESP-IDELMT-ERROR                             
018766         PERFORM S06-MOVE-MISSING-TO-RESPOND                              
018768       END-IF                                                             
018769     END-IF                                                               
018792     .                                                                    
018793     EJECT                                                                
018794                                                                          
019302*** - CHECK IF UPDATE IS ON CURRENT OR COMING LINE                        
019303 FAB-UPDATE-T01DOTY SECTION.                                              
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
019318*** - UPDATE CURRENT LINE ON T01DOTY.                                     
019319 FABA-UPD-CURRENT SECTION.                                                
019324     PERFORM DB2-SELECT-T01DOTY-CURRENT                                   
019325     IF LINES-FOUND                                                       
019326       MOVE WS-CURRENT-DATE TO WS-DAUPPDAT                                
019327       IF REQU-FLAP = 'Y'                                                 
019328         MOVE 'J' TO WS-FLAP                                              
019329       END-IF                                                             
019330       IF REQU-FLAR = 'Y'                                                 
019331         MOVE 'J' TO WS-FLAR                                              
019332       END-IF                                                             
019333       IF REQU-FLGL = 'Y'                                                 
019334         MOVE 'J' TO WS-FLGL                                              
019335       END-IF                                                             
019336       IF REQU-FLINTREP = 'Y'                                             
019337         MOVE 'J' TO WS-FLINTREP                                          
019338       END-IF                                                             
019339       IF REQU-FLVATREP = 'Y'                                             
019340         MOVE 'J' TO WS-FLVATREP                                          
019341       END-IF                                                             
019342       IF REQU-FLCUSREP = 'Y'                                             
019343         MOVE 'J' TO WS-FLCUSREP                                          
019344       END-IF                                                             
019345       IF REQU-FLAP = 'N'                                                 
019346         MOVE 'N' TO WS-FLAP                                              
019347       END-IF                                                             
019348       IF REQU-FLAR = 'N'                                                 
019349         MOVE 'N' TO WS-FLAR                                              
019350       END-IF                                                             
019351       IF REQU-FLGL = 'N'                                                 
019352         MOVE 'N' TO WS-FLGL                                              
019353       END-IF                                                             
019354       IF REQU-FLINTREP = 'N'                                             
019355         MOVE 'N' TO WS-FLINTREP                                          
019356       END-IF                                                             
019357       IF REQU-FLVATREP = 'N'                                             
019358         MOVE 'N' TO WS-FLVATREP                                          
019359       END-IF                                                             
019360       IF REQU-FLCUSREP = 'N'                                             
019361         MOVE 'N' TO WS-FLCUSREP                                          
019362       END-IF                                                             
019363       PERFORM DB2-UPDATE-T01DOTY-CURRENT                                 
019364       PERFORM S04-MOVE-UPD-INS-TO-RESPOND                                
019365       PERFORM DB2-SELECT-T01DOTY-COMING-FL                               
019366       IF LINES-FOUND                                                     
019367         MOVE YES TO WS-FLCOMING                                          
019368         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
019369       ELSE                                                               
019370         MOVE NOO TO WS-FLCOMING                                          
019371         MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                            
019372       END-IF                                                             
019373     ELSE                                                                 
019374       MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                                
019375       MOVE 'KDFINDOC' TO RESP-IDELMT-ERROR                               
019376     END-IF                                                               
019377     .                                                                    
019378     EJECT                                                                
019379                                                                          
019380*** - UPDATE COMING LINE ON T01DOTY. IF NO COMING LINE EXIST BUT          
019381***   COMING LINE IS CHOOSED FOR UPDATE, PROGRAM WILL INSERT ONE          
019382***   COMING LINE WITH DATA FROM CURRENT LINE BUT USER WILL SEE           
019383***   THIS AS AN UPDATE.                                                  
019384 FABB-UPD-COMING SECTION.                                                 
019385     PERFORM DB2-SELECT-T01DOTY-COMING                                    
019386     IF LINES-FOUND                                                       
019387       PERFORM DB2-SELECT-T01DOTY-CURRENT                                 
019388       IF LINES-FOUND                                                     
019389         IF REQU-FLAP = 'Y'                                               
019390           MOVE 'J' TO WS-FLAP                                            
019391         END-IF                                                           
019392         IF REQU-FLAR = 'Y'                                               
019393           MOVE 'J' TO WS-FLAR                                            
019394         END-IF                                                           
019395         IF REQU-FLGL = 'Y'                                               
019396           MOVE 'J' TO WS-FLGL                                            
019397         END-IF                                                           
019398         IF REQU-FLINTREP = 'Y'                                           
019399           MOVE 'J' TO WS-FLINTREP                                        
019400         END-IF                                                           
019401         IF REQU-FLVATREP = 'Y'                                           
019402           MOVE 'J' TO WS-FLVATREP                                        
019403         END-IF                                                           
019404         IF REQU-FLCUSREP = 'Y'                                           
019405           MOVE 'J' TO WS-FLCUSREP                                        
019406         END-IF                                                           
019407         IF REQU-FLAP = 'N'                                               
019408           MOVE 'N' TO WS-FLAP                                            
019409         END-IF                                                           
019410         IF REQU-FLAR = 'N'                                               
019411           MOVE 'N' TO WS-FLAR                                            
019412         END-IF                                                           
019413         IF REQU-FLGL = 'N'                                               
019414           MOVE 'N' TO WS-FLGL                                            
019415         END-IF                                                           
019416         IF REQU-FLINTREP = 'N'                                           
019417           MOVE 'N' TO WS-FLINTREP                                        
019418         END-IF                                                           
019419         IF REQU-FLVATREP = 'N'                                           
019420           MOVE 'N' TO WS-FLVATREP                                        
019421         END-IF                                                           
019422         IF REQU-FLCUSREP = 'N'                                           
019423           MOVE 'N' TO WS-FLCUSREP                                        
019424         END-IF                                                           
019425         PERFORM DB2-UPDATE-T01DOTY-COMING                                
019426         MOVE REQU-DAUPPDAT TO WS-DAUPPDAT                                
019427         PERFORM S04-MOVE-UPD-INS-TO-RESPOND                              
019428         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
019429         MOVE YES TO WS-FLCOMING                                          
019430       ELSE                                                               
019431         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
019432         MOVE 'KDFINDOC' TO RESP-IDELMT-ERROR                             
019433       END-IF                                                             
019434     ELSE                                                                 
019435       IF REQU-DAUPPDAT NUMERIC                                           
019436         IF REQU-DAUPPDAT > WS-CURRENT-DATE                               
019437           PERFORM DB2-SELECT-T01DOTY-CURRENT                             
019438           IF LINES-FOUND                                                 
019439             IF REQU-FLAP = 'Y'                                           
019440               MOVE 'J' TO WS-FLAP                                        
019441             END-IF                                                       
019442             IF REQU-FLAR = 'Y'                                           
019443               MOVE 'J' TO WS-FLAR                                        
019444             END-IF                                                       
019445             IF REQU-FLGL = 'Y'                                           
019446               MOVE 'J' TO WS-FLGL                                        
019447             END-IF                                                       
019448             IF REQU-FLINTREP = 'Y'                                       
019449               MOVE 'J' TO WS-FLINTREP                                    
019450             END-IF                                                       
019451             IF REQU-FLVATREP = 'Y'                                       
019452               MOVE 'J' TO WS-FLVATREP                                    
019453             END-IF                                                       
019454             IF REQU-FLCUSREP = 'Y'                                       
019455               MOVE 'J' TO WS-FLCUSREP                                    
019456             END-IF                                                       
019457             IF REQU-FLAP = 'N'                                           
019458               MOVE 'N' TO WS-FLAP                                        
019459             END-IF                                                       
019460             IF REQU-FLAR = 'N'                                           
019461               MOVE 'N' TO WS-FLAR                                        
019462             END-IF                                                       
019463             IF REQU-FLGL = 'N'                                           
019464               MOVE 'N' TO WS-FLGL                                        
019465             END-IF                                                       
019466             IF REQU-FLINTREP = 'N'                                       
019467               MOVE 'N' TO WS-FLINTREP                                    
019468             END-IF                                                       
019469             IF REQU-FLVATREP = 'N'                                       
019470               MOVE 'N' TO WS-FLVATREP                                    
019471             END-IF                                                       
019472             IF REQU-FLCUSREP = 'N'                                       
019473               MOVE 'N' TO WS-FLCUSREP                                    
019474             END-IF                                                       
019475             PERFORM DB2-INSERT-T01DOTY-COMING                            
019476             MOVE '00000000' TO WS-DAREGDAT                               
019477             MOVE REQU-DAUPPDAT TO WS-DAUPPDAT                            
019478             PERFORM S04-MOVE-UPD-INS-TO-RESPOND                          
019479             MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO              
019480             MOVE YES TO WS-FLCOMING                                      
019481           ELSE                                                           
019482             MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                          
019483             MOVE 'KDFINDOC' TO RESP-IDELMT-ERROR                         
019484           END-IF                                                         
019485         ELSE                                                             
019486           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
019487           MOVE 'DAUPPDAT'        TO RESP-IDELMT-ERROR                    
019488         END-IF                                                           
019489       ELSE                                                               
019490         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
019491         MOVE 'DAUPPDAT' TO RESP-IDELMT-ERROR                             
019492       END-IF                                                             
019493     END-IF                                                               
019494     .                                                                    
019495     EJECT                                                                
019496                                                                          
019497*** - INSERT NEW CURRENT LINE. COMING LINE COULD NOT BE INSERTED.         
019498 FAC-INSERT-T01DOTY SECTION.                                              
019499     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
019500       PERFORM DB2-SELECT-T01DOTY-INSERT                                  
019501       IF LINES-FOUND                                                     
019502         IF WS-DADELDAT = '00000000'                                      
019503           MOVE ERR-ALREADY-EXIST TO RESP-IDMSG-ERROR                     
019504           MOVE 'KDFINDOC' TO RESP-IDELMT-ERROR                           
019505         ELSE                                                             
019506           MOVE '00000000' TO WS-DADELDAT                                 
019507           IF REQU-FLAP = 'Y'                                             
019508             MOVE 'J' TO WS-FLAP                                          
019509           END-IF                                                         
019510           IF REQU-FLAR = 'Y'                                             
019511             MOVE 'J' TO WS-FLAR                                          
019512           END-IF                                                         
019513           IF REQU-FLGL = 'Y'                                             
019514             MOVE 'J' TO WS-FLGL                                          
019515           END-IF                                                         
019516           IF REQU-FLINTREP = 'Y'                                         
019517             MOVE 'J' TO WS-FLINTREP                                      
019518           END-IF                                                         
019519           IF REQU-FLVATREP = 'Y'                                         
019520             MOVE 'J' TO WS-FLVATREP                                      
019521           END-IF                                                         
019522           IF REQU-FLCUSREP = 'Y'                                         
019523             MOVE 'J' TO WS-FLCUSREP                                      
019524           END-IF                                                         
019525           IF REQU-FLAP = 'N'                                             
019526             MOVE 'N' TO WS-FLAP                                          
019527           END-IF                                                         
019528           IF REQU-FLAR = 'N'                                             
019529             MOVE 'N' TO WS-FLAR                                          
019530           END-IF                                                         
019531           IF REQU-FLGL = 'N'                                             
019532             MOVE 'N' TO WS-FLGL                                          
019533           END-IF                                                         
019534           IF REQU-FLINTREP = 'N'                                         
019535             MOVE 'N' TO WS-FLINTREP                                      
019536           END-IF                                                         
019537           IF REQU-FLVATREP = 'N'                                         
019538             MOVE 'N' TO WS-FLVATREP                                      
019539           END-IF                                                         
019540           IF REQU-FLCUSREP = 'N'                                         
019541             MOVE 'N' TO WS-FLCUSREP                                      
019542           END-IF                                                         
019543           PERFORM DB2-UPDATE-T01DOTY-CURRENT                             
019544           PERFORM S04-MOVE-UPD-INS-TO-RESPOND                            
019545           MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                          
019546         END-IF                                                           
019547       ELSE                                                               
019548         MOVE WS-CURRENT-DATE TO WS-DAREGDAT                              
019549         IF REQU-FLAP = 'Y'                                               
019550           MOVE 'J' TO WS-FLAP                                            
019551         END-IF                                                           
019552         IF REQU-FLAR = 'Y'                                               
019553           MOVE 'J' TO WS-FLAR                                            
019554         END-IF                                                           
019555         IF REQU-FLGL = 'Y'                                               
019556           MOVE 'J' TO WS-FLGL                                            
019557         END-IF                                                           
019558         IF REQU-FLINTREP = 'Y'                                           
019559           MOVE 'J' TO WS-FLINTREP                                        
019560         END-IF                                                           
019561         IF REQU-FLVATREP = 'Y'                                           
019562           MOVE 'J' TO WS-FLVATREP                                        
019563         END-IF                                                           
019564         IF REQU-FLCUSREP = 'Y'                                           
019565           MOVE 'J' TO WS-FLCUSREP                                        
019566         END-IF                                                           
019567         IF REQU-FLAP = 'N'                                               
019568           MOVE 'N' TO WS-FLAP                                            
019569         END-IF                                                           
019570         IF REQU-FLAR = 'N'                                               
019571           MOVE 'N' TO WS-FLAR                                            
019572         END-IF                                                           
019573         IF REQU-FLGL = 'N'                                               
019574           MOVE 'N' TO WS-FLGL                                            
019575         END-IF                                                           
019576         IF REQU-FLINTREP = 'N'                                           
019577           MOVE 'N' TO WS-FLINTREP                                        
019578         END-IF                                                           
019579         IF REQU-FLVATREP = 'N'                                           
019580           MOVE 'N' TO WS-FLVATREP                                        
019581         END-IF                                                           
019582         IF REQU-FLCUSREP = 'N'                                           
019583           MOVE 'N' TO WS-FLCUSREP                                        
019584         END-IF                                                           
019585         PERFORM DB2-INSERT-T01DOTY-CURRENT                               
019586         PERFORM S04-MOVE-UPD-INS-TO-RESPOND                              
019587         MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                            
019588       END-IF                                                             
019589     ELSE                                                                 
019590       MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
019591     END-IF                                                               
019592     .                                                                    
019593     EJECT                                                                
019594                                                                          
019595 FAD-DELETE-T01DOTY SECTION.                                              
019596     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
019597       PERFORM DB2-OPEN-T01NSAS-CRS                                       
019598       PERFORM DB2-FETCH-T01NSAS-CRS                                      
019599       IF LINES-FOUND                                                     
019600         MOVE ERR-DELETE-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
019601       ELSE                                                               
019602         PERFORM DB2-SELECT-T01DOTY-CURRENT                               
019603         IF LINES-FOUND                                                   
019604           MOVE WS-CURRENT-DATE TO WS-DADELDAT                            
019605           IF REQU-FLAP = 'Y'                                             
019606             MOVE 'J' TO WS-FLAP                                          
019607           END-IF                                                         
019608           IF REQU-FLAR = 'Y'                                             
019609             MOVE 'J' TO WS-FLAR                                          
019610           END-IF                                                         
019611           IF REQU-FLGL = 'Y'                                             
019612             MOVE 'J' TO WS-FLGL                                          
019613           END-IF                                                         
019614           IF REQU-FLINTREP = 'Y'                                         
019615             MOVE 'J' TO WS-FLINTREP                                      
019616           END-IF                                                         
019617           IF REQU-FLVATREP = 'Y'                                         
019618             MOVE 'J' TO WS-FLVATREP                                      
019619           END-IF                                                         
019620           IF REQU-FLCUSREP = 'Y'                                         
019621             MOVE 'J' TO WS-FLCUSREP                                      
019622           END-IF                                                         
019623           IF REQU-FLAP = 'N'                                             
019624             MOVE 'N' TO WS-FLAP                                          
019625           END-IF                                                         
019626           IF REQU-FLAR = 'N'                                             
019627             MOVE 'N' TO WS-FLAR                                          
019628           END-IF                                                         
019629           IF REQU-FLGL = 'N'                                             
019630             MOVE 'N' TO WS-FLGL                                          
019631           END-IF                                                         
019632           IF REQU-FLINTREP = 'N'                                         
019633             MOVE 'N' TO WS-FLINTREP                                      
019634           END-IF                                                         
019635           IF REQU-FLVATREP = 'N'                                         
019636             MOVE 'N' TO WS-FLVATREP                                      
019637           END-IF                                                         
019638           IF REQU-FLCUSREP = 'N'                                         
019639             MOVE 'N' TO WS-FLCUSREP                                      
019640           END-IF                                                         
019641           PERFORM DB2-UPDATE-T01DOTY-CURRENT                             
019642           PERFORM S05-MOVE-DEL-TO-RESPOND                                
019643           MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                          
019644           PERFORM DB2-SELECT-T01DOTY-COMING                              
019645           IF LINES-FOUND                                                 
019646             PERFORM DB2-DELETE-T01DOTY-COMING                            
019647           END-IF                                                         
019648         ELSE                                                             
019649           MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                            
019650           MOVE 'KDFINDOC' TO RESP-IDELMT-ERROR                           
019651         END-IF                                                           
019652       PERFORM DB2-CLOSE-T01NSAS-CRS                                      
019653       END-IF                                                             
019654     END-IF                                                               
019655                                                                          
019656     IF REQU-KDSTATUS-KEY = WS-COMING                                     
019657       PERFORM DB2-SELECT-T01DOTY-COMING                                  
019658       IF LINES-FOUND                                                     
019659         MOVE WS-CURRENT-DATE TO WS-DADELDAT                              
019660         PERFORM DB2-DELETE-T01DOTY-COMING                                
019661         PERFORM S05-MOVE-DEL-TO-RESPOND                                  
019662         MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                            
019663       ELSE                                                               
019664         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
019665         MOVE 'KDFINDOC' TO RESP-IDELMT-ERROR                             
019666       END-IF                                                             
019667     END-IF                                                               
019668     .                                                                    
019669     EJECT                                                                
019670                                                                          
019671*    --- DISPATCHER SECTIONS                                              
019672 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019680     MOVE 'GETARG'                   TO SUB-KDFUNC                        
019700     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
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
022330     IF WS-FLAP = 'J'                                                     
022331       MOVE 'Y' TO WS-FLAP                                                
022332     END-IF                                                               
022333     IF WS-FLAR = 'J'                                                     
022334       MOVE 'Y' TO WS-FLAR                                                
022335     END-IF                                                               
022336     IF WS-FLGL = 'J'                                                     
022337       MOVE 'Y' TO WS-FLGL                                                
022338     END-IF                                                               
022339     IF WS-FLINTREP = 'J'                                                 
022340       MOVE 'Y' TO WS-FLINTREP                                            
022341     END-IF                                                               
022342     IF WS-FLVATREP = 'J'                                                 
022343       MOVE 'Y' TO WS-FLVATREP                                            
022344     END-IF                                                               
022345     IF WS-FLCUSREP = 'J'                                                 
022346       MOVE 'Y' TO WS-FLCUSREP                                            
022347     END-IF                                                               
022348     MOVE WS-FLCOMING       TO RESP-FLCOMING                              
022349     MOVE WS-FLAP           TO RESP-FLAP                                  
022350     MOVE WS-FLAR           TO RESP-FLAR                                  
022351     MOVE WS-FLGL           TO RESP-FLGL                                  
022352     MOVE WS-FLINTREP       TO RESP-FLINTREP                              
022353     MOVE WS-FLVATREP       TO RESP-FLVATREP                              
022354     MOVE WS-FLCUSREP       TO RESP-FLCUSREP                              
022360     MOVE WS-BEFINDOC       TO RESP-BEFINDOC                              
022392     MOVE WS-DAREGDAT       TO RESP-DAREGDAT                              
022393     MOVE WS-DAUPPDAT       TO RESP-DAUPPDAT                              
022394     MOVE WS-DADELDAT       TO RESP-DADELDAT                              
022395     MOVE WS-IDUSER         TO RESP-IDUSER                                
022416     .                                                                    
022417     EJECT                                                                
022420                                                                          
022430 S04-MOVE-UPD-INS-TO-RESPOND SECTION.                                     
022450     MOVE REQU-FLCOMING     TO RESP-FLCOMING                              
022451     MOVE REQU-FLAP         TO RESP-FLAP                                  
022452     MOVE REQU-FLAR         TO RESP-FLAR                                  
022453     MOVE REQU-FLGL         TO RESP-FLGL                                  
022454     MOVE REQU-FLINTREP     TO RESP-FLINTREP                              
022455     MOVE REQU-FLVATREP     TO RESP-FLVATREP                              
022456     MOVE REQU-FLCUSREP     TO RESP-FLCUSREP                              
022460     MOVE REQU-BEFINDOC     TO RESP-BEFINDOC                              
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
022528     MOVE SPACE             TO RESP-FLAP                                  
022529     MOVE SPACE             TO RESP-FLAR                                  
022530     MOVE SPACE             TO RESP-FLGL                                  
022531     MOVE SPACE             TO RESP-FLINTREP                              
022532     MOVE SPACE             TO RESP-FLVATREP                              
022533     MOVE SPACE             TO RESP-FLCUSREP                              
022534     MOVE SPACE             TO RESP-BEFINDOC                              
022535     MOVE ZERO              TO WS-DAREGDAT-2                              
022536     MOVE WS-DAREGDAT-2     TO RESP-DAREGDAT                              
022537     MOVE ZERO              TO WS-DAUPPDAT-2                              
022538     MOVE WS-DAUPPDAT-2     TO RESP-DAUPPDAT                              
022539     MOVE WS-DADELDAT       TO WS-DADELDAT-2                              
022540     MOVE WS-DADELDAT-2     TO RESP-DADELDAT                              
022541     MOVE REQU-IDUSER       TO RESP-IDUSER                                
022542     .                                                                    
022543     EJECT                                                                
022544                                                                          
022545 S06-MOVE-MISSING-TO-RESPOND SECTION.                                     
022546     MOVE SPACE             TO RESP-FLCOMING                              
022547                               RESP-FLAP                                  
022548                               RESP-FLAR                                  
022549                               RESP-FLGL                                  
022550                               RESP-FLINTREP                              
022551                               RESP-FLVATREP                              
022552                               RESP-FLCUSREP                              
022553                               RESP-BEFINDOC                              
022554                               RESP-IDUSER                                
022555     MOVE ZERO              TO RESP-DAREGDAT                              
022556                               RESP-DAUPPDAT                              
022557                               RESP-DADELDAT                              
022558     .                                                                    
022559     EJECT                                                                
022560                                                                          
022570*    --- DB2 SECTIONS                                                     
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
030400 DB2-SELECT-T01DOTY-CURRENT SECTION.                                      
030600     MOVE 000100  TO GOOD-SQLCODECODES                                    
030700     EXEC SQL                                                             
030800         SELECT  BEFINDOC,                                                
030801                 FLAP,                                                    
030802                 FLAR,                                                    
030803                 FLGL,                                                    
030804                 FLINTREP,                                                
030805                 FLVATREP,                                                
030806                 DAREGDAT,                                                
030807                 DAUPPDAT,                                                
030808                 DADELDAT,                                                
030809                 IDUSER,                                                  
030810                 FLCUSREP                                                 
030811                                                                          
030820         INTO    :WS-BEFINDOC,                                            
030830                 :WS-FLAP,                                                
030840                 :WS-FLAR,                                                
030850                 :WS-FLGL,                                                
030860                 :WS-FLINTREP,                                            
030870                 :WS-FLVATREP,                                            
030880                 :WS-DAREGDAT,                                            
030890                 :WS-DAUPPDAT,                                            
030900                 :WS-DADELDAT,                                            
031000                 :WS-IDUSER,                                              
031100                 :WS-FLCUSREP                                             
032100                                                                          
032200         FROM    T01DOTY                                                  
032210                                                                          
032300         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
032310             AND KDFINDOC = :REQU-KDFINDOC-KEY                            
032330             AND KDSTATUS = :WS-CURRENT                                   
032340             AND DADELDAT = '00000000'                                    
032400     END-EXEC                                                             
032600     MOVE SQLCODE TO SQLCODE-WS                                           
032700     PERFORM DB2-STATUS-CHECK                                             
032800     .                                                                    
032900     EJECT                                                                
032901                                                                          
032902 DB2-SELECT-T01DOTY-COMING SECTION.                                       
032903     MOVE 000100  TO GOOD-SQLCODECODES                                    
032904     EXEC SQL                                                             
032905         SELECT  BEFINDOC,                                                
032906                 FLAP,                                                    
032907                 FLAR,                                                    
032908                 FLGL,                                                    
032909                 FLINTREP,                                                
032910                 FLVATREP,                                                
032911                 DAREGDAT,                                                
032912                 DAUPPDAT,                                                
032913                 DADELDAT,                                                
032914                 IDUSER,                                                  
032915                 FLCUSREP                                                 
032916                                                                          
032917         INTO    :WS-BEFINDOC,                                            
032918                 :WS-FLAP,                                                
032919                 :WS-FLAR,                                                
032920                 :WS-FLGL,                                                
032921                 :WS-FLINTREP,                                            
032922                 :WS-FLVATREP,                                            
032923                 :WS-DAREGDAT,                                            
032924                 :WS-DAUPPDAT,                                            
032925                 :WS-DADELDAT,                                            
032926                 :WS-IDUSER,                                              
032927                 :WS-FLCUSREP                                             
032929                                                                          
032930         FROM    T01DOTY                                                  
032931                                                                          
032932         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
032933             AND KDFINDOC = :REQU-KDFINDOC-KEY                            
032950             AND KDSTATUS = :WS-COMING                                    
032960             AND DADELDAT = '00000000'                                    
032970     END-EXEC                                                             
032990     MOVE SQLCODE TO SQLCODE-WS                                           
033000     PERFORM DB2-STATUS-CHECK                                             
033010     .                                                                    
033020     EJECT                                                                
033021                                                                          
033022 DB2-SELECT-T01DOTY-COMING-FL SECTION.                                    
033023     MOVE 000100  TO GOOD-SQLCODECODES                                    
033024     EXEC SQL                                                             
033025         SELECT  IDLEGSEL                                                 
033035                                                                          
033036         INTO    :WS-IDLEGSEL                                             
033046                                                                          
033047         FROM    T01DOTY                                                  
033048                                                                          
033049         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
033050             AND KDFINDOC = :REQU-KDFINDOC-KEY                            
033051             AND KDSTATUS = :WS-COMING                                    
033052             AND DADELDAT = '00000000'                                    
033053     END-EXEC                                                             
033054     MOVE SQLCODE TO SQLCODE-WS                                           
033055     PERFORM DB2-STATUS-CHECK                                             
033056     .                                                                    
033057     EJECT                                                                
033058                                                                          
033059 DB2-SELECT-T01DOTY-INSERT SECTION.                                       
033060     MOVE 000100  TO GOOD-SQLCODECODES                                    
033061     EXEC SQL                                                             
033062         SELECT  BEFINDOC,                                                
033063                 FLAP,                                                    
033064                 FLAR,                                                    
033065                 FLGL,                                                    
033066                 FLINTREP,                                                
033067                 FLVATREP,                                                
033068                 DAREGDAT,                                                
033069                 DAUPPDAT,                                                
033070                 DADELDAT,                                                
033071                 IDUSER,                                                  
033072                 FLCUSREP                                                 
033073                                                                          
033074         INTO    :WS-BEFINDOC,                                            
033075                 :WS-FLAP,                                                
033076                 :WS-FLAR,                                                
033077                 :WS-FLGL,                                                
033078                 :WS-FLINTREP,                                            
033079                 :WS-FLVATREP,                                            
033080                 :WS-DAREGDAT,                                            
033081                 :WS-DAUPPDAT,                                            
033082                 :WS-DADELDAT,                                            
033083                 :WS-IDUSER,                                              
033084                 :WS-FLCUSREP                                             
033085                                                                          
033086         FROM    T01DOTY                                                  
033087                                                                          
033088         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
033089             AND KDFINDOC = :REQU-KDFINDOC-KEY                            
033090             AND KDSTATUS = :WS-CURRENT                                   
033091     END-EXEC                                                             
033092     MOVE SQLCODE TO SQLCODE-WS                                           
033093     PERFORM DB2-STATUS-CHECK                                             
033094     .                                                                    
033095     EJECT                                                                
033096                                                                          
033097 DB2-UPDATE-T01DOTY-CURRENT SECTION.                                      
033098     MOVE 000     TO GOOD-SQLCODECODES                                    
033099     EXEC SQL                                                             
033100          UPDATE T01DOTY                                                  
033101          SET DAUPPDAT  = :WS-CURRENT-DATE,                               
033102              DADELDAT  = :WS-DADELDAT,                                   
033103              FLAP      = :WS-FLAP,                                       
033104              FLAR      = :WS-FLAR,                                       
033105              FLGL      = :WS-FLGL,                                       
033106              FLINTREP  = :WS-FLINTREP,                                   
033107              FLVATREP  = :WS-FLVATREP,                                   
033108              BEFINDOC  = :REQU-BEFINDOC,                                 
033109              IDUSER    = :REQU-IDUSER,                                   
033110              FLCUSREP  = :WS-FLCUSREP                                    
033111                                                                          
033112          WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                             
033113          AND   KDFINDOC = :REQU-KDFINDOC-KEY                             
033114          AND   KDSTATUS = :WS-CURRENT                                    
033115     END-EXEC                                                             
033116     MOVE SQLCODE TO SQLCODE-WS                                           
033117     PERFORM DB2-STATUS-CHECK                                             
033118     .                                                                    
033119     EJECT                                                                
033120                                                                          
033121 DB2-UPDATE-T01DOTY-COMING SECTION.                                       
033122     MOVE 000     TO GOOD-SQLCODECODES                                    
033123     EXEC SQL                                                             
033124          UPDATE T01DOTY                                                  
033125          SET DAUPPDAT  = :REQU-DAUPPDAT,                                 
033126              DADELDAT  = :WS-DADELDAT,                                   
033127              FLAP      = :WS-FLAP,                                       
033128              FLAR      = :WS-FLAR,                                       
033129              FLGL      = :WS-FLGL,                                       
033130              FLINTREP  = :WS-FLINTREP,                                   
033131              FLVATREP  = :WS-FLVATREP,                                   
033132              BEFINDOC  = :REQU-BEFINDOC,                                 
033133              IDUSER    = :REQU-IDUSER,                                   
033134              FLCUSREP  = :WS-FLCUSREP                                    
033135                                                                          
033136          WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                             
033137          AND   KDFINDOC = :REQU-KDFINDOC-KEY                             
033138          AND   KDSTATUS = :WS-COMING                                     
033139     END-EXEC                                                             
033140     MOVE SQLCODE TO SQLCODE-WS                                           
033141     PERFORM DB2-STATUS-CHECK                                             
033142     .                                                                    
033143     EJECT                                                                
033144                                                                          
033145 DB2-INSERT-T01DOTY-CURRENT SECTION.                                      
033146     MOVE 000     TO GOOD-SQLCODECODES                                    
033147     EXEC SQL                                                             
033148        INSERT INTO T01DOTY                                               
033149          (IDLEGSEL,                                                      
033150           KDFINDOC,                                                      
033151           KDSTATUS,                                                      
033152           BEFINDOC,                                                      
033153           FLAP,                                                          
033154           FLAR,                                                          
033155           FLGL,                                                          
033156           FLINTREP,                                                      
033157           FLVATREP,                                                      
033158           DAREGDAT,                                                      
033159           DAUPPDAT,                                                      
033160           DADELDAT,                                                      
033161           IDUSER,                                                        
033162           FLCUSREP)                                                      
033163        VALUES(:REQU-IDLEGSEL-KEY,                                        
033164               :REQU-KDFINDOC-KEY,                                        
033165               :WS-CURRENT,                                               
033166               :REQU-BEFINDOC,                                            
033167               :WS-FLAP,                                                  
033168               :WS-FLAR,                                                  
033169               :WS-FLGL,                                                  
033170               :WS-FLINTREP,                                              
033171               :WS-FLVATREP,                                              
033172               :WS-CURRENT-DATE,                                          
033173               '00000000',                                                
033174               :WS-DADELDAT,                                              
033175               :REQU-IDUSER,                                              
033176               :WS-FLCUSREP)                                              
033177     END-EXEC                                                             
033178     MOVE SQLCODE TO SQLCODE-WS                                           
033179     PERFORM DB2-STATUS-CHECK                                             
033180     .                                                                    
033181     EJECT                                                                
033182                                                                          
033183 DB2-INSERT-T01DOTY-COMING SECTION.                                       
033184     MOVE 000     TO GOOD-SQLCODECODES                                    
033185     EXEC SQL                                                             
033186        INSERT INTO T01DOTY                                               
033187          (IDLEGSEL,                                                      
033188           KDFINDOC,                                                      
033189           KDSTATUS,                                                      
033190           BEFINDOC,                                                      
033191           FLAP,                                                          
033192           FLAR,                                                          
033193           FLGL,                                                          
033194           FLINTREP,                                                      
033195           FLVATREP,                                                      
033196           DAREGDAT,                                                      
033197           DAUPPDAT,                                                      
033198           DADELDAT,                                                      
033199           IDUSER,                                                        
033200           FLCUSREP)                                                      
033201        VALUES(:REQU-IDLEGSEL-KEY,                                        
033202               :REQU-KDFINDOC-KEY,                                        
033204               :WS-COMING,                                                
033205               :REQU-BEFINDOC,                                            
033206               :WS-FLAP,                                                  
033207               :WS-FLAR,                                                  
033208               :WS-FLGL,                                                  
033209               :WS-FLINTREP,                                              
033210               :WS-FLVATREP,                                              
033211               :WS-DAREGDAT,                                              
033212               :REQU-DAUPPDAT,                                            
033213               :WS-DADELDAT,                                              
033214               :REQU-IDUSER,                                              
033215               :WS-FLCUSREP)                                              
033216     END-EXEC                                                             
033217     MOVE SQLCODE TO SQLCODE-WS                                           
033218     PERFORM DB2-STATUS-CHECK                                             
033219     .                                                                    
033220     EJECT                                                                
033221                                                                          
033222 DB2-DELETE-T01DOTY-COMING SECTION.                                       
033223     MOVE 000     TO GOOD-SQLCODECODES                                    
033224     EXEC SQL                                                             
033225           DELETE FROM T01DOTY                                            
033226           WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                            
033227           AND   KDFINDOC = :REQU-KDFINDOC-KEY                            
033228           AND   KDSTATUS = :WS-COMING                                    
033229     END-EXEC                                                             
033230     MOVE SQLCODE TO SQLCODE-WS                                           
033231     PERFORM DB2-STATUS-CHECK                                             
033232     .                                                                    
033233     EJECT                                                                
033234                                                                          
033240 DB2-OPEN-T01NSAS-CRS SECTION.                                            
033300     MOVE 000100 TO GOOD-SQLCODECODES                                     
033500     EXEC SQL                                                             
033600           DECLARE T01NSAS-CRS CURSOR WITH HOLD FOR                       
033700                                                                          
034594           SELECT IDLEGSEL, KDFINDOC                                      
034600                                                                          
034610           FROM     T01NSAS                                               
034620                                                                          
034630           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
034631                AND KDFINDOC = :REQU-KDFINDOC-KEY                         
034710     END-EXEC                                                             
034720                                                                          
034730     MOVE 000100 TO GOOD-SQLCODECODES                                     
034740     EXEC SQL                                                             
034750           OPEN T01NSAS-CRS                                               
034760     END-EXEC                                                             
035200     MOVE SQLCODE TO SQLCODE-WS                                           
035300     PERFORM DB2-STATUS-CHECK                                             
035400     .                                                                    
035410     EJECT                                                                
035500                                                                          
035600 DB2-FETCH-T01NSAS-CRS SECTION.                                           
035700     MOVE 000100 TO GOOD-SQLCODECODES                                     
035710     EXEC SQL                                                             
035720          FETCH T01NSAS-CRS                                               
035730                                                                          
035740          INTO :WS-IDLEGSEL,                                              
035750               :WS-KDFINDOC                                               
035760     END-EXEC                                                             
035770                                                                          
035780     MOVE SQLCODE TO SQLCODE-WS                                           
035790     PERFORM DB2-STATUS-CHECK                                             
035791     .                                                                    
035792     EJECT                                                                
035793                                                                          
035800 DB2-CLOSE-T01NSAS-CRS SECTION.                                           
035810     EXEC SQL                                                             
035820          CLOSE T01NSAS-CRS                                               
035830     END-EXEC                                                             
035840     .                                                                    
035850     EJECT                                                                
035900                                                                          
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
