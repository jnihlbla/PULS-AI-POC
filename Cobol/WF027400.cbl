000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF027400.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   02/04/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610*    NAME:                                                                
000620*        CARPARTS.BILLIT.BUSINRELATMAINTENANCE                            
000700*    FUNCTION:                                                            
000800*        READ/UPDATE/INSERT/DELETE BUSINESS RELATIONS                     
000810*        TABLE T01BURE DEPENDING ON                                       
000900*        REQUESTED PROGRAMS ACTION CODE (KDPGMACT)                        
000910*        KDPGMACT = 'S' READ                                              
000920*        KDPGMACT = 'U' UPDATE                                            
000930*        KDPGMACT = 'I' INSERT                                            
000940*        KDPGMACT = 'D' DELETE                                            
001000*                                                                         
001100*        THE PROGRAM READS   TABLE T01LSEL                                
001400*        THE PROGRAM READS   TABLE T01NSAS                                
001500*        THE PROGRAM READS   TABLE T01DOTY                                
001501*        THE PROGRAM READS   TABLE T01CUGR                                
001510*        THE PROGRAM UPDATES TABLE T01BURE                                
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: WF0274U                                             
001900*        REQUEST:     WF0274I1                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        RESPONSE:    WF0274O1                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002700 INPUT-OUTPUT SECTION.                                                    
002900 FILE-CONTROL.                                                            
003100 DATA DIVISION.                                                           
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'WF027400'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003910 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004000 77  KDRC-DISPLAY                PIC Z(5).                                
004100                                                                          
004110*    --- CONSTANT WORK FIELDS                                             
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004311 77  WS-ADRESS                   PIC X(50)                                
004312             VALUE 'CARPARTS.BILLIT.BUSINRELATMAINTENANCE'.               
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
004934 01  WS-KDPARTTY                 PIC X(3)    VALUE SPACE.                 
004935 01  WS-KDPARTGR                 PIC X(15)   VALUE SPACE.                 
004936 01  WS-KDSTATUS                 PIC 9(3)    VALUE ZERO.                  
004937 01  WS-BELEGRAD-1               PIC X(35)   VALUE SPACE.                 
004938 01  WS-FLCOMING                 PIC X       VALUE SPACE.                 
004939 01  WS-BEFORMS                  PIC X(15)   VALUE SPACE.                 
004940 01  WS-FLGL                     PIC X(1)    VALUE SPACE.                 
004941 01  WS-FLAR                     PIC X(1)    VALUE SPACE.                 
004942 01  WS-FLAP                     PIC X(1)    VALUE SPACE.                 
004943 01  WS-FLVATCHK                 PIC X(1)    VALUE SPACE.                 
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
006430         05  INF-NO-DATA-JOINED      PIC X(3)    VALUE '102'.             
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
007500*    03  -COPY WF0274I1                                                   
007600     EJECT                                                                
007610                                                                          
007700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007800     SKIP3                                                                
007900 01  RESP-AREA.                                                           
008000*    03  -COPY WZ01RESP                                                   
008100*    03  -COPY WF0274O1                                                   
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
011200 01  FILLER                      PIC X(16)   VALUE 'T01DOTY-AREA'.        
011400*01  -COPY T01DOTY -PRE T01DOTY-                                          
011500     EJECT                                                                
011501                                                                          
011502 01  FILLER                      PIC X(16)   VALUE 'T01CUGR-AREA'.        
011503*01  -COPY T01CUGR -PRE T01CUGR-                                          
011504     EJECT                                                                
011505                                                                          
011506 01  FILLER                      PIC X(16)   VALUE 'T01BURE-AREA'.        
011507*01  -COPY T01BURE -PRE T01BURE-                                          
011508     EJECT                                                                
011509                                                                          
011600 01  FILLER                      PIC X(16)   VALUE 'T01NSAS-AREA'.        
011800*01  -COPY T01NSAS -PRE T01NSAS-                                          
011900     EJECT                                                                
011901                                                                          
012000     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
012100     EJECT                                                                
012200     EXEC SQL INCLUDE T01CUGR END-EXEC.                                   
012300     EJECT                                                                
012400     EXEC SQL INCLUDE T01DOTY END-EXEC.                                   
012500     EJECT                                                                
012600     EXEC SQL INCLUDE T01BURE END-EXEC.                                   
012610     EJECT                                                                
012810     EXEC SQL INCLUDE T01NSAS END-EXEC.                                   
012901     EJECT                                                                
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
014020         IF KEYS-OK                                                       
014120           PERFORM F-READ-SHOW-INFO                                       
014130         END-IF                                                           
014140       END-IF                                                             
014150       IF KEYS-WRONG                                                      
014160         PERFORM S06-MOVE-MISSING-TO-RESPOND                              
014170       END-IF                                                             
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
016320     OR  REQU-KDFINDOC-KEY = SPACE OR = ALL '+'                           
016321     OR  REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                           
016322     OR  REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                           
016325       MOVE NOO TO KEYS-SW                                                
016326     END-IF                                                               
016327                                                                          
016328     IF REQU-IDMSGVER NUMERIC                                             
016330     AND ACT-CODE-VALID                                                   
016340     AND (REQU-KDSTATUS-KEY = WS-CURRENT OR WS-COMING)                    
016350       CONTINUE                                                           
016360     ELSE                                                                 
016361       MOVE NOO TO KEYS-SW                                                
016370     END-IF                                                               
016380                                                                          
016390     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
016392       MOVE NOO TO KEYS-SW                                                
016393     END-IF                                                               
016394                                                                          
016610     IF KEYS-WRONG                                                        
016620       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
016630       IF REQU-KDPGMACT = 'S' OR = 'I' OR = 'U' OR = 'D'                  
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
016714                                                                          
016715     IF KEYS-OK                                                           
016716       PERFORM DB2-SELECT-T01DOTY-TAB                                     
016717       IF LINES-FOUND                                                     
016718         CONTINUE                                                         
016719       ELSE                                                               
016720         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
016721         MOVE 'KDFINDOC' TO RESP-IDELMT-ERROR                             
016722         MOVE NOO TO KEYS-SW                                              
016723       END-IF                                                             
016724     END-IF                                                               
016725                                                                          
016726     IF KEYS-OK                                                           
016727       PERFORM DB2-SELECT-T01CUGR-TAB                                     
016728       IF LINES-FOUND                                                     
016729         CONTINUE                                                         
016730       ELSE                                                               
016731         MOVE NOT-FOUND          TO RESP-IDMSG-ERROR                      
016732         MOVE 'CUSTOMER-GROUPING' TO RESP-IDELMT-ERROR                    
016733         MOVE NOO TO KEYS-SW                                              
016734       END-IF                                                             
016735     END-IF                                                               
016736     .                                                                    
016737     EJECT                                                                
016738                                                                          
016739*** - VALIDATE REQUESTED FIELDS FOR UPDATE/INSERT/DELETE                  
016740 D-VALIDATE-REQUEST SECTION.                                              
016750     IF REQU-KDPGMACT = 'U' OR = 'I'                                      
016760       IF REQU-BEFORMS = SPACE OR = ALL '+'                               
016790         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
016791         MOVE 'BEFORMS'           TO RESP-IDELMT-ERROR                    
016793       ELSE                                                               
016794         CONTINUE                                                         
016795       END-IF                                                             
016796                                                                          
016797       IF REQU-KDSTATUS-KEY = WS-COMING                                   
016798         IF REQU-DAUPPDAT NUMERIC                                         
016799           IF REQU-DAUPPDAT > WS-CURRENT-DATE                             
016800             MOVE REQU-DAUPPDAT TO DATE-TIDATE                            
016801             MOVE 'YYYYMMDD'  TO DATE-KDDATFMT                            
016802             CALL WZ20DATE USING DATE-WZ20DATE                            
016803             IF DATE-KDRC > ZERO                                          
016804               MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                 
016805               MOVE 'DAUPPDAT'      TO RESP-IDELMT-ERROR                  
016807             ELSE                                                         
016808               CONTINUE                                                   
016809             END-IF                                                       
016810           ELSE                                                           
016811             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
016812             MOVE 'DAUPPDAT'      TO RESP-IDELMT-ERROR                    
016814           END-IF                                                         
016815         ELSE                                                             
016816           MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
016817           MOVE 'DAUPPDAT'          TO RESP-IDELMT-ERROR                  
016819         END-IF                                                           
016820       END-IF                                                             
016821                                                                          
016822       IF REQU-FLGL = 'Y' OR = 'N'                                        
016823         CONTINUE                                                         
016824       ELSE                                                               
016825         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
016826         MOVE 'FLGL'            TO RESP-IDELMT-ERROR                      
016828       END-IF                                                             
016829                                                                          
016830       IF REQU-FLAR = 'Y' OR = 'N'                                        
016831         CONTINUE                                                         
016832       ELSE                                                               
016833         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
016834         MOVE 'FLAR'            TO RESP-IDELMT-ERROR                      
016836       END-IF                                                             
016837                                                                          
016838       IF REQU-FLAP = 'Y' OR = 'N'                                        
016839         CONTINUE                                                         
016840       ELSE                                                               
016841         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
016842         MOVE 'FLAP'            TO RESP-IDELMT-ERROR                      
016843       END-IF                                                             
016844                                                                          
016853       IF REQU-FLVATCHK = 'Y' OR = 'N'                                    
016854         CONTINUE                                                         
016855       ELSE                                                               
016856         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
016857         MOVE 'FLVATCHK'        TO RESP-IDELMT-ERROR                      
016858       END-IF                                                             
016859     END-IF                                                               
016860                                                                          
016861     IF REQU-KDPGMACT = 'I'                                               
016862       IF REQU-KDSTATUS-KEY = WS-COMING                                   
016863         MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
016864       END-IF                                                             
016865     END-IF                                                               
016866     .                                                                    
016867     EJECT                                                                
016868                                                                          
016870*** - MOVE KEYS AND COMPULSORY FIELDS TO RESPOND                          
016900 F-READ-SHOW-INFO SECTION.                                                
017010     MOVE REQU-IDLEGSEL-KEY TO RESP-IDLEGSEL-KEY                          
017030     MOVE REQU-KDFINDOC-KEY TO RESP-KDFINDOC-KEY                          
017031     MOVE REQU-KDPARTTY-KEY TO RESP-KDPARTTY-KEY                          
017032     MOVE REQU-KDPARTGR-KEY TO RESP-KDPARTGR-KEY                          
017040     MOVE REQU-KDSTATUS-KEY TO RESP-KDSTATUS-KEY                          
017042     MOVE WS-BELEGRAD-1     TO RESP-BELEGRAD-1                            
017050                                                                          
017060     IF KEYS-OK                                                           
017100       PERFORM FA-READ-BASICDATA                                          
017200     END-IF                                                               
017800     .                                                                    
017900     EJECT                                                                
017901                                                                          
017910*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
018000 FA-READ-BASICDATA SECTION.                                               
018130     MOVE REQU-KDPGMACT     TO ACTION-CODE-SW                             
018200     IF ACT-CODE-SEARCH                                                   
018302       PERFORM FAA-SEARCH-T01BURE                                         
018303     END-IF                                                               
018304                                                                          
018310     IF ACT-CODE-UPDATE                                                   
018315       PERFORM FAB-UPDATE-T01BURE                                         
018320     END-IF                                                               
018321                                                                          
018330     IF ACT-CODE-INSERT                                                   
018334       PERFORM FAC-INSERT-T01BURE                                         
018335     END-IF                                                               
018336                                                                          
018337     IF ACT-CODE-DELETE                                                   
018338       PERFORM FAD-DELETE-T01BURE                                         
018339     END-IF                                                               
018600     .                                                                    
018700     EJECT                                                                
018701                                                                          
018702*** - SEARCH FOR RIGHT BUSINESS-RELATION AND MARK CURRENT LINE            
018703*** - IF COMING LINE EXIST.                                               
018710 FAA-SEARCH-T01BURE SECTION.                                              
018721     MOVE NOO TO WS-FLCOMING                                              
018722     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
018723       PERFORM DB2-SELECT-T01BURE-CURRENT                                 
018734       IF LINES-FOUND                                                     
018739         PERFORM DB2-SELECT-T01BURE-COMING-FL                             
018740         IF LINES-FOUND                                                   
018741           MOVE YES TO WS-FLCOMING                                        
018742           PERFORM S03-MOVE-SEARCH-TO-RESPOND                             
018744         ELSE                                                             
018745           MOVE NOO TO WS-FLCOMING                                        
018746           PERFORM S03-MOVE-SEARCH-TO-RESPOND                             
018747         END-IF                                                           
018748       ELSE                                                               
018749         MOVE NOT-FOUND           TO RESP-IDMSG-ERROR                     
018750         MOVE 'BUSINESSRELATION' TO RESP-IDELMT-ERROR                     
018751         MOVE NOO                 TO KEYS-SW                              
018754       END-IF                                                             
018755     END-IF                                                               
018756                                                                          
018757     IF REQU-KDSTATUS-KEY = WS-COMING                                     
018758       PERFORM DB2-SELECT-T01BURE-COMING                                  
018759       IF LINES-FOUND                                                     
018760         MOVE YES TO WS-FLCOMING                                          
018761         PERFORM S03-MOVE-SEARCH-TO-RESPOND                               
018762       ELSE                                                               
018763         MOVE NOO TO WS-FLCOMING                                          
018764         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
018765         PERFORM S06-MOVE-MISSING-TO-RESPOND                              
018768       END-IF                                                             
018769     END-IF                                                               
018792     .                                                                    
018793     EJECT                                                                
018794                                                                          
019302*** - CHECK IF UPDATE IS ON CURRENT OR COMING LINE                        
019303 FAB-UPDATE-T01BURE SECTION.                                              
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
019318*** - UPDATE CURRENT LINE ON T01BURE.                                     
019319 FABA-UPD-CURRENT SECTION.                                                
019324     PERFORM DB2-SELECT-T01BURE-CURRENT                                   
019325     IF LINES-FOUND                                                       
019326       MOVE WS-CURRENT-DATE TO WS-DAUPPDAT                                
019327       IF REQU-FLGL = 'Y'                                                 
019328         MOVE 'J' TO WS-FLGL                                              
019329       END-IF                                                             
019330       IF REQU-FLAR = 'Y'                                                 
019331         MOVE 'J' TO WS-FLAR                                              
019332       END-IF                                                             
019333       IF REQU-FLAP = 'Y'                                                 
019334         MOVE 'J' TO WS-FLAP                                              
019335       END-IF                                                             
019336       IF REQU-FLVATCHK = 'Y'                                             
019337         MOVE 'J' TO WS-FLVATCHK                                          
019338       END-IF                                                             
019339       IF REQU-FLGL = 'N'                                                 
019340         MOVE 'N' TO WS-FLGL                                              
019341       END-IF                                                             
019342       IF REQU-FLAR = 'N'                                                 
019343         MOVE 'N' TO WS-FLAR                                              
019344       END-IF                                                             
019345       IF REQU-FLAP = 'N'                                                 
019346         MOVE 'N' TO WS-FLAP                                              
019347       END-IF                                                             
019348       IF REQU-FLVATCHK = 'N'                                             
019349         MOVE 'N' TO WS-FLVATCHK                                          
019350       END-IF                                                             
019351       PERFORM DB2-UPDATE-T01BURE-CURRENT                                 
019352       PERFORM S04-MOVE-UPD-INS-TO-RESPOND                                
019353       PERFORM DB2-SELECT-T01BURE-COMING-FL                               
019354       IF LINES-FOUND                                                     
019355         MOVE YES TO WS-FLCOMING                                          
019356         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
019357       ELSE                                                               
019358         MOVE NOO TO WS-FLCOMING                                          
019359         MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                            
019360       END-IF                                                             
019361     ELSE                                                                 
019362       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
019363     END-IF                                                               
019364     .                                                                    
019365     EJECT                                                                
019366                                                                          
019367*** - UPDATE COMING LINE ON T01BURE. IF NOO COMING LINE EXIST BUT         
019368***   COMING LINE IS CHOOSED FOR UPDATE, PROGRAM WILL INSERT ONE          
019369***   COMING LINE WITH DATA FROM CURRENT LINE BUT USER WILL SEE           
019370***   THIS AS AN UPDATE.                                                  
019371 FABB-UPD-COMING SECTION.                                                 
019372     PERFORM DB2-SELECT-T01BURE-COMING                                    
019373     IF LINES-FOUND                                                       
019374       PERFORM DB2-SELECT-T01BURE-CURRENT                                 
019375       IF LINES-FOUND                                                     
019376         MOVE '00000000' TO WS-DAREGDAT                                   
019377         MOVE REQU-DAUPPDAT TO WS-DAUPPDAT                                
019378         IF REQU-FLGL = 'Y'                                               
019379           MOVE 'J' TO WS-FLGL                                            
019380         END-IF                                                           
019381         IF REQU-FLAR = 'Y'                                               
019382           MOVE 'J' TO WS-FLAR                                            
019383         END-IF                                                           
019384         IF REQU-FLAP = 'Y'                                               
019385           MOVE 'J' TO WS-FLAP                                            
019386         END-IF                                                           
019387         IF REQU-FLVATCHK = 'Y'                                           
019388           MOVE 'J' TO WS-FLVATCHK                                        
019389         END-IF                                                           
019390         IF REQU-FLGL = 'N'                                               
019391           MOVE 'N' TO WS-FLGL                                            
019392         END-IF                                                           
019393         IF REQU-FLAR = 'N'                                               
019394           MOVE 'N' TO WS-FLAR                                            
019395         END-IF                                                           
019396         IF REQU-FLAP = 'N'                                               
019397           MOVE 'N' TO WS-FLAP                                            
019398         END-IF                                                           
019399         IF REQU-FLVATCHK = 'N'                                           
019400           MOVE 'N' TO WS-FLVATCHK                                        
019401         END-IF                                                           
019402         PERFORM DB2-UPDATE-T01BURE-COMING                                
019403         PERFORM S04-MOVE-UPD-INS-TO-RESPOND                              
019404         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
019405         MOVE YES TO WS-FLCOMING                                          
019406       ELSE                                                               
019407         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
019408       END-IF                                                             
019409     ELSE                                                                 
019410       IF REQU-DAUPPDAT NUMERIC                                           
019411         IF REQU-DAUPPDAT > WS-CURRENT-DATE                               
019412           PERFORM DB2-SELECT-T01BURE-CURRENT                             
019413           IF LINES-FOUND                                                 
019414             IF REQU-FLGL = 'Y'                                           
019415               MOVE 'J' TO WS-FLGL                                        
019416             END-IF                                                       
019417             IF REQU-FLAR = 'Y'                                           
019418               MOVE 'J' TO WS-FLAR                                        
019419             END-IF                                                       
019420             IF REQU-FLAP = 'Y'                                           
019421               MOVE 'J' TO WS-FLAP                                        
019422             END-IF                                                       
019423             IF REQU-FLVATCHK = 'Y'                                       
019424               MOVE 'J' TO WS-FLVATCHK                                    
019425             END-IF                                                       
019426             IF REQU-FLGL = 'N'                                           
019427               MOVE 'N' TO WS-FLGL                                        
019428             END-IF                                                       
019429             IF REQU-FLAR = 'N'                                           
019430               MOVE 'N' TO WS-FLAR                                        
019431             END-IF                                                       
019432             IF REQU-FLAP = 'N'                                           
019433               MOVE 'N' TO WS-FLAP                                        
019434             END-IF                                                       
019435             IF REQU-FLVATCHK = 'N'                                       
019436               MOVE 'N' TO WS-FLVATCHK                                    
019437             END-IF                                                       
019438             PERFORM DB2-INSERT-T01BURE-COMING                            
019439             MOVE '00000000' TO WS-DAREGDAT                               
019440             MOVE REQU-DAUPPDAT TO WS-DAUPPDAT                            
019441             PERFORM S04-MOVE-UPD-INS-TO-RESPOND                          
019442             MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO              
019443             MOVE YES TO WS-FLCOMING                                      
019444           ELSE                                                           
019445             MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                 
019446           END-IF                                                         
019447         ELSE                                                             
019448           MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                   
019449         END-IF                                                           
019450       ELSE                                                               
019451         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
019452         MOVE 'DAUPPDAT' TO RESP-IDELMT-ERROR                             
019453       END-IF                                                             
019454     END-IF                                                               
019455     .                                                                    
019456     EJECT                                                                
019457                                                                          
019458*** - INSERT NEW CURRENT LINE. COMING LINE COULD NOT BE INSERTED.         
019459 FAC-INSERT-T01BURE SECTION.                                              
019460     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
019461       PERFORM DB2-SELECT-T01BURE-INSERT                                  
019462       IF LINES-FOUND                                                     
019463         IF WS-DADELDAT = '00000000'                                      
019464           MOVE ERR-ALREADY-EXIST TO RESP-IDMSG-ERROR                     
019465           MOVE 'KDFINDOC' TO RESP-IDELMT-ERROR                           
019466         ELSE                                                             
019467           MOVE '00000000' TO WS-DADELDAT                                 
019468           IF REQU-FLGL = 'Y'                                             
019469             MOVE 'J' TO WS-FLGL                                          
019470           END-IF                                                         
019471           IF REQU-FLAR = 'Y'                                             
019472             MOVE 'J' TO WS-FLAR                                          
019473           END-IF                                                         
019474           IF REQU-FLAP = 'Y'                                             
019475             MOVE 'J' TO WS-FLAP                                          
019476           END-IF                                                         
019477           IF REQU-FLVATCHK = 'Y'                                         
019478             MOVE 'J' TO WS-FLVATCHK                                      
019479           END-IF                                                         
019480           IF REQU-FLGL = 'N'                                             
019481             MOVE 'N' TO WS-FLGL                                          
019482           END-IF                                                         
019483           IF REQU-FLAR = 'N'                                             
019484             MOVE 'N' TO WS-FLAR                                          
019485           END-IF                                                         
019486           IF REQU-FLAP = 'N'                                             
019487             MOVE 'N' TO WS-FLAP                                          
019488           END-IF                                                         
019489           IF REQU-FLVATCHK = 'N'                                         
019490             MOVE 'N' TO WS-FLVATCHK                                      
019491           END-IF                                                         
019492           PERFORM DB2-UPDATE-T01BURE-CURRENT                             
019493           PERFORM S04-MOVE-UPD-INS-TO-RESPOND                            
019494           MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                          
019495         END-IF                                                           
019496       ELSE                                                               
019497         MOVE WS-CURRENT-DATE TO WS-DAREGDAT                              
019498         IF REQU-FLGL = 'Y'                                               
019499           MOVE 'J' TO WS-FLGL                                            
019500         END-IF                                                           
019501         IF REQU-FLAR = 'Y'                                               
019502           MOVE 'J' TO WS-FLAR                                            
019503         END-IF                                                           
019504         IF REQU-FLAP = 'Y'                                               
019505           MOVE 'J' TO WS-FLAP                                            
019506         END-IF                                                           
019507         IF REQU-FLVATCHK = 'Y'                                           
019508           MOVE 'J' TO WS-FLVATCHK                                        
019509         END-IF                                                           
019510         IF REQU-FLGL = 'N'                                               
019511           MOVE 'N' TO WS-FLGL                                            
019512         END-IF                                                           
019513         IF REQU-FLAR = 'N'                                               
019514           MOVE 'N' TO WS-FLAR                                            
019515         END-IF                                                           
019516         IF REQU-FLAP = 'N'                                               
019517           MOVE 'N' TO WS-FLAP                                            
019518         END-IF                                                           
019519         IF REQU-FLVATCHK = 'N'                                           
019520           MOVE 'N' TO WS-FLVATCHK                                        
019521         END-IF                                                           
019522         PERFORM DB2-INSERT-T01BURE-CURRENT                               
019523         PERFORM S04-MOVE-UPD-INS-TO-RESPOND                              
019524         MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                            
019525       END-IF                                                             
019526     ELSE                                                                 
019527       MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
019528     END-IF                                                               
019529     .                                                                    
019530     EJECT                                                                
019531                                                                          
019532 FAD-DELETE-T01BURE SECTION.                                              
019533     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
019534       PERFORM DB2-SELECT-T01NSAS                                         
019535       IF LINES-FOUND                                                     
019536         MOVE ERR-DELETE-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
019537       ELSE                                                               
019538         PERFORM DB2-SELECT-T01BURE-CURRENT                               
019539         IF LINES-FOUND                                                   
019540           MOVE WS-CURRENT-DATE TO WS-DADELDAT                            
019541           IF REQU-FLGL = 'Y'                                             
019542             MOVE 'J' TO WS-FLGL                                          
019543           END-IF                                                         
019544           IF REQU-FLAR = 'Y'                                             
019545             MOVE 'J' TO WS-FLAR                                          
019546           END-IF                                                         
019547           IF REQU-FLAP = 'Y'                                             
019548             MOVE 'J' TO WS-FLAP                                          
019549           END-IF                                                         
019550           IF REQU-FLVATCHK = 'Y'                                         
019551             MOVE 'J' TO WS-FLVATCHK                                      
019552           END-IF                                                         
019553           IF REQU-FLGL = 'N'                                             
019554             MOVE 'N' TO WS-FLGL                                          
019555           END-IF                                                         
019556           IF REQU-FLAR = 'N'                                             
019557             MOVE 'N' TO WS-FLAR                                          
019558           END-IF                                                         
019559           IF REQU-FLAP = 'N'                                             
019560             MOVE 'N' TO WS-FLAP                                          
019561           END-IF                                                         
019562           IF REQU-FLVATCHK = 'N'                                         
019563             MOVE 'N' TO WS-FLVATCHK                                      
019564           END-IF                                                         
019565           PERFORM DB2-UPDATE-T01BURE-CURRENT                             
019566           PERFORM S05-MOVE-DEL-TO-RESPOND                                
019567           MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                          
019568           PERFORM DB2-SELECT-T01BURE-COMING                              
019569           IF LINES-FOUND                                                 
019570             PERFORM DB2-DELETE-T01BURE-COMING                            
019571           END-IF                                                         
019572         ELSE                                                             
019573           MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                   
019574         END-IF                                                           
019575       END-IF                                                             
019576     END-IF                                                               
019577                                                                          
019578     IF REQU-KDSTATUS-KEY = WS-COMING                                     
019579       PERFORM DB2-SELECT-T01BURE-COMING                                  
019580       IF LINES-FOUND                                                     
019581         MOVE WS-CURRENT-DATE TO WS-DADELDAT                              
019582         PERFORM DB2-DELETE-T01BURE-COMING                                
019583         PERFORM S05-MOVE-DEL-TO-RESPOND                                  
019584         MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                            
019585       ELSE                                                               
019586         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
019587       END-IF                                                             
019588     END-IF                                                               
019589     .                                                                    
019590     EJECT                                                                
019591                                                                          
019592*    --- DISPATCHER SECTIONS                                              
019593 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019594     MOVE 'GETARG'                   TO SUB-KDFUNC                        
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
022330     IF  RESP-IDMSG-ERROR = SPACE                                         
022331     AND RESP-IDMSG-INFO = SPACE                                          
022332       IF WS-FLGL = 'J'                                                   
022333         MOVE 'Y' TO WS-FLGL                                              
022334       END-IF                                                             
022335       IF WS-FLAR = 'J'                                                   
022336         MOVE 'Y' TO WS-FLAR                                              
022337       END-IF                                                             
022338       IF WS-FLAP = 'J'                                                   
022339         MOVE 'Y' TO WS-FLAP                                              
022340       END-IF                                                             
022341       IF WS-FLVATCHK = 'J'                                               
022342         MOVE 'Y' TO WS-FLVATCHK                                          
022343       END-IF                                                             
022344       MOVE WS-FLCOMING     TO RESP-FLCOMING                              
022350       MOVE WS-BEFORMS      TO RESP-BEFORMS                               
022360       MOVE WS-FLGL         TO RESP-FLGL                                  
022370       MOVE WS-FLAR         TO RESP-FLAR                                  
022380       MOVE WS-FLAP         TO RESP-FLAP                                  
022390       MOVE WS-FLVATCHK     TO RESP-FLVATCHK                              
022392       MOVE WS-DAREGDAT     TO RESP-DAREGDAT                              
022393       MOVE WS-DAUPPDAT     TO RESP-DAUPPDAT                              
022394       MOVE WS-DADELDAT     TO RESP-DADELDAT                              
022395       MOVE WS-IDUSER       TO RESP-IDUSER                                
022396     ELSE                                                                 
022397       MOVE SPACE           TO RESP-FLCOMING                              
022398       MOVE SPACE           TO RESP-BEFORMS                               
022399       MOVE SPACE           TO RESP-FLGL                                  
022400       MOVE SPACE           TO RESP-FLAR                                  
022401       MOVE SPACE           TO RESP-FLAP                                  
022402       MOVE SPACE           TO RESP-FLVATCHK                              
022410       MOVE ZERO            TO RESP-DAREGDAT                              
022411       MOVE ZERO            TO RESP-DAUPPDAT                              
022412       MOVE ZERO            TO RESP-DADELDAT                              
022413       MOVE REQU-IDUSER     TO RESP-IDUSER                                
022414     END-IF                                                               
022416     .                                                                    
022417     EJECT                                                                
022420                                                                          
022430 S04-MOVE-UPD-INS-TO-RESPOND SECTION.                                     
022450     MOVE REQU-FLCOMING     TO RESP-FLCOMING                              
022460     MOVE REQU-BEFORMS      TO RESP-BEFORMS                               
022470     MOVE REQU-FLGL         TO RESP-FLGL                                  
022480     MOVE REQU-FLAR         TO RESP-FLAR                                  
022490     MOVE REQU-FLAP         TO RESP-FLAP                                  
022491     MOVE REQU-FLVATCHK     TO RESP-FLVATCHK                              
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
022528     MOVE SPACE             TO RESP-BEFORMS                               
022529     MOVE SPACE             TO RESP-FLGL                                  
022530     MOVE SPACE             TO RESP-FLAR                                  
022531     MOVE SPACE             TO RESP-FLAP                                  
022532     MOVE SPACE             TO RESP-FLVATCHK                              
022533     MOVE ZERO              TO WS-DAREGDAT-2                              
022534     MOVE WS-DAREGDAT-2     TO RESP-DAREGDAT                              
022535     MOVE ZERO              TO WS-DAUPPDAT-2                              
022536     MOVE WS-DAUPPDAT-2     TO RESP-DAUPPDAT                              
022537     MOVE WS-DADELDAT       TO WS-DADELDAT-2                              
022538     MOVE WS-DADELDAT-2     TO RESP-DADELDAT                              
022539     MOVE REQU-IDUSER       TO RESP-IDUSER                                
022540     .                                                                    
022541     EJECT                                                                
022542                                                                          
022543 S06-MOVE-MISSING-TO-RESPOND SECTION.                                     
022544     MOVE SPACE             TO RESP-FLCOMING                              
022545                               RESP-BEFORMS                               
022546                               RESP-FLGL                                  
022547                               RESP-FLAR                                  
022548                               RESP-FLAP                                  
022549                               RESP-FLVATCHK                              
022550                               RESP-IDUSER                                
022551     MOVE ZERO              TO RESP-DAREGDAT                              
022552                               RESP-DAUPPDAT                              
022553                               RESP-DADELDAT                              
022554     .                                                                    
022555     EJECT                                                                
022556                                                                          
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
025300 DB2-SELECT-T01DOTY-TAB  SECTION.                                         
025400     MOVE 000100 TO GOOD-SQLCODECODES                                     
025500     EXEC SQL                                                             
025600           SELECT  BEFINDOC                                               
025700                                                                          
025900           INTO   :T01DOTY-BEFINDOC                                       
025910                                                                          
026000           FROM    T01DOTY                                                
026100                                                                          
026200           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
026300               AND KDFINDOC = :REQU-KDFINDOC-KEY                          
026310               AND KDSTATUS = :WS-CURRENT                                 
026400     END-EXEC                                                             
026500     MOVE SQLCODE TO SQLCODE-WS                                           
026600     PERFORM DB2-STATUS-CHECK                                             
026700     .                                                                    
026800     EJECT                                                                
026900                                                                          
027000 DB2-SELECT-T01CUGR-TAB  SECTION.                                         
027100     MOVE 000100 TO GOOD-SQLCODECODES                                     
027200     EXEC SQL                                                             
027300           SELECT  KDINVFRQ                                               
027400                                                                          
027410           INTO   :T01CUGR-KDINVFRQ                                       
027420                                                                          
027500           FROM    T01CUGR                                                
027600                                                                          
027700           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
027800               AND KDPARTTY = :REQU-KDPARTTY-KEY                          
027810               AND KDPARTGR = :REQU-KDPARTGR-KEY                          
027900               AND KDSTATUS = :WS-CURRENT                                 
028000     END-EXEC                                                             
028100     MOVE SQLCODE TO SQLCODE-WS                                           
028200     PERFORM DB2-STATUS-CHECK                                             
028300     .                                                                    
028400     EJECT                                                                
028500                                                                          
030400 DB2-SELECT-T01BURE-CURRENT SECTION.                                      
030600     MOVE 000100  TO GOOD-SQLCODECODES                                    
030700     EXEC SQL                                                             
030800         SELECT  BEFORMS,                                                 
030801                 FLGL,                                                    
030802                 FLAR,                                                    
030803                 FLAP,                                                    
030804                 FLVATCHK,                                                
030806                 DAREGDAT,                                                
030807                 DAUPPDAT,                                                
030808                 DADELDAT,                                                
030809                 IDUSER                                                   
030810                                                                          
030820         INTO    :WS-BEFORMS,                                             
030830                 :WS-FLGL,                                                
030840                 :WS-FLAR,                                                
030850                 :WS-FLAP,                                                
030860                 :WS-FLVATCHK,                                            
030880                 :WS-DAREGDAT,                                            
030890                 :WS-DAUPPDAT,                                            
030900                 :WS-DADELDAT,                                            
031000                 :WS-IDUSER                                               
032100                                                                          
032200         FROM    T01BURE                                                  
032210                                                                          
032300         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
032310             AND KDFINDOC = :REQU-KDFINDOC-KEY                            
032311             AND KDPARTTY = :REQU-KDPARTTY-KEY                            
032320             AND KDPARTGR = :REQU-KDPARTGR-KEY                            
032330             AND KDSTATUS = :WS-CURRENT                                   
032340             AND DADELDAT = '00000000'                                    
032400     END-EXEC                                                             
032600     MOVE SQLCODE TO SQLCODE-WS                                           
032700     PERFORM DB2-STATUS-CHECK                                             
032800     .                                                                    
032900     EJECT                                                                
032901                                                                          
032902 DB2-SELECT-T01BURE-COMING SECTION.                                       
032903     MOVE 000100  TO GOOD-SQLCODECODES                                    
032904     EXEC SQL                                                             
032905         SELECT  BEFORMS,                                                 
032906                 FLGL,                                                    
032907                 FLAR,                                                    
032908                 FLAP,                                                    
032909                 FLVATCHK,                                                
032911                 DAREGDAT,                                                
032912                 DAUPPDAT,                                                
032913                 DADELDAT,                                                
032914                 IDUSER                                                   
032915                                                                          
032916         INTO    :WS-BEFORMS,                                             
032917                 :WS-FLGL,                                                
032918                 :WS-FLAR,                                                
032919                 :WS-FLAP,                                                
032920                 :WS-FLVATCHK,                                            
032922                 :WS-DAREGDAT,                                            
032923                 :WS-DAUPPDAT,                                            
032924                 :WS-DADELDAT,                                            
032925                 :WS-IDUSER                                               
032929                                                                          
032930         FROM    T01BURE                                                  
032931                                                                          
032932         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
032933             AND KDFINDOC = :REQU-KDFINDOC-KEY                            
032934             AND KDPARTTY = :REQU-KDPARTTY-KEY                            
032940             AND KDPARTGR = :REQU-KDPARTGR-KEY                            
032950             AND KDSTATUS = :WS-COMING                                    
032960             AND DADELDAT = '00000000'                                    
032970     END-EXEC                                                             
032990     MOVE SQLCODE TO SQLCODE-WS                                           
033000     PERFORM DB2-STATUS-CHECK                                             
033010     .                                                                    
033020     EJECT                                                                
033021                                                                          
033022 DB2-SELECT-T01BURE-COMING-FL SECTION.                                    
033023     MOVE 000100  TO GOOD-SQLCODECODES                                    
033024     EXEC SQL                                                             
033025         SELECT  IDLEGSEL                                                 
033033                                                                          
033034         INTO    :WS-IDLEGSEL                                             
033042                                                                          
033043         FROM    T01BURE                                                  
033044                                                                          
033045         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
033046             AND KDFINDOC = :REQU-KDFINDOC-KEY                            
033047             AND KDPARTTY = :REQU-KDPARTTY-KEY                            
033048             AND KDPARTGR = :REQU-KDPARTGR-KEY                            
033049             AND KDSTATUS = :WS-COMING                                    
033050             AND DADELDAT = '00000000'                                    
033051     END-EXEC                                                             
033052     MOVE SQLCODE TO SQLCODE-WS                                           
033053     PERFORM DB2-STATUS-CHECK                                             
033054     .                                                                    
033055     EJECT                                                                
033056                                                                          
033057 DB2-SELECT-T01BURE-INSERT SECTION.                                       
033058     MOVE 000100  TO GOOD-SQLCODECODES                                    
033059     EXEC SQL                                                             
033060         SELECT  BEFORMS,                                                 
033061                 FLGL,                                                    
033062                 FLAR,                                                    
033063                 FLAP,                                                    
033064                 FLVATCHK,                                                
033065                 DAREGDAT,                                                
033066                 DAUPPDAT,                                                
033067                 DADELDAT,                                                
033068                 IDUSER                                                   
033069                                                                          
033070         INTO    :WS-BEFORMS,                                             
033071                 :WS-FLGL,                                                
033072                 :WS-FLAR,                                                
033073                 :WS-FLAP,                                                
033074                 :WS-FLVATCHK,                                            
033075                 :WS-DAREGDAT,                                            
033076                 :WS-DAUPPDAT,                                            
033077                 :WS-DADELDAT,                                            
033078                 :WS-IDUSER                                               
033079                                                                          
033080         FROM    T01BURE                                                  
033081                                                                          
033082         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
033083             AND KDFINDOC = :REQU-KDFINDOC-KEY                            
033084             AND KDPARTTY = :REQU-KDPARTTY-KEY                            
033085             AND KDPARTGR = :REQU-KDPARTGR-KEY                            
033086             AND KDSTATUS = :WS-CURRENT                                   
033087     END-EXEC                                                             
033088     MOVE SQLCODE TO SQLCODE-WS                                           
033089     PERFORM DB2-STATUS-CHECK                                             
033090     .                                                                    
033091     EJECT                                                                
033092                                                                          
033093 DB2-UPDATE-T01BURE-CURRENT SECTION.                                      
033094     MOVE 000     TO GOOD-SQLCODECODES                                    
033095     EXEC SQL                                                             
033096          UPDATE T01BURE                                                  
033097          SET DAUPPDAT  = :WS-CURRENT-DATE,                               
033098              DADELDAT  = :WS-DADELDAT,                                   
033099              BEFORMS   = :REQU-BEFORMS,                                  
033100              FLGL      = :WS-FLGL,                                       
033101              FLAR      = :WS-FLAR,                                       
033102              FLAP      = :WS-FLAP,                                       
033103              FLVATCHK  = :WS-FLVATCHK,                                   
033104              IDUSER    = :REQU-IDUSER                                    
033105                                                                          
033106          WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                             
033107          AND   KDFINDOC = :REQU-KDFINDOC-KEY                             
033108          AND   KDPARTTY = :REQU-KDPARTTY-KEY                             
033109          AND   KDPARTGR = :REQU-KDPARTGR-KEY                             
033110          AND   KDSTATUS = :WS-CURRENT                                    
033111     END-EXEC                                                             
033112     MOVE SQLCODE TO SQLCODE-WS                                           
033113     PERFORM DB2-STATUS-CHECK                                             
033114     .                                                                    
033115     EJECT                                                                
033116                                                                          
033117 DB2-UPDATE-T01BURE-COMING SECTION.                                       
033118     MOVE 000     TO GOOD-SQLCODECODES                                    
033119     EXEC SQL                                                             
033120          UPDATE T01BURE                                                  
033121          SET DAUPPDAT  = :REQU-DAUPPDAT,                                 
033122              DADELDAT  = :WS-DADELDAT,                                   
033123              BEFORMS   = :REQU-BEFORMS,                                  
033124              FLGL      = :WS-FLGL,                                       
033125              FLAR      = :WS-FLAR,                                       
033126              FLAP      = :WS-FLAP,                                       
033127              FLVATCHK  = :WS-FLVATCHK,                                   
033128              IDUSER    = :REQU-IDUSER                                    
033129                                                                          
033130          WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                             
033131          AND   KDFINDOC = :REQU-KDFINDOC-KEY                             
033132          AND   KDPARTTY = :REQU-KDPARTTY-KEY                             
033133          AND   KDPARTGR = :REQU-KDPARTGR-KEY                             
033134          AND   KDSTATUS = :WS-COMING                                     
033135     END-EXEC                                                             
033136     MOVE SQLCODE TO SQLCODE-WS                                           
033137     PERFORM DB2-STATUS-CHECK                                             
033138     .                                                                    
033139     EJECT                                                                
033140                                                                          
033141 DB2-INSERT-T01BURE-CURRENT SECTION.                                      
033142     MOVE 000     TO GOOD-SQLCODECODES                                    
033143     EXEC SQL                                                             
033144        INSERT INTO T01BURE                                               
033145          (IDLEGSEL,                                                      
033146           KDFINDOC,                                                      
033147           KDPARTTY,                                                      
033148           KDPARTGR,                                                      
033149           KDSTATUS,                                                      
033150           BEFORMS,                                                       
033151           FLGL,                                                          
033152           FLAR,                                                          
033153           FLAP,                                                          
033154           FLVATCHK,                                                      
033155           DAREGDAT,                                                      
033156           DAUPPDAT,                                                      
033157           DADELDAT,                                                      
033158           IDUSER)                                                        
033159        VALUES(:REQU-IDLEGSEL-KEY,                                        
033160               :REQU-KDFINDOC-KEY,                                        
033161               :REQU-KDPARTTY-KEY,                                        
033162               :REQU-KDPARTGR-KEY,                                        
033163               :WS-CURRENT,                                               
033164               :REQU-BEFORMS,                                             
033165               :WS-FLGL,                                                  
033166               :WS-FLAR,                                                  
033167               :WS-FLAP,                                                  
033168               :WS-FLVATCHK,                                              
033169               :WS-CURRENT-DATE,                                          
033170               '00000000',                                                
033171               :WS-DADELDAT,                                              
033172               :REQU-IDUSER)                                              
033173     END-EXEC                                                             
033174     MOVE SQLCODE TO SQLCODE-WS                                           
033175     PERFORM DB2-STATUS-CHECK                                             
033176     .                                                                    
033177     EJECT                                                                
033178                                                                          
033179 DB2-INSERT-T01BURE-COMING SECTION.                                       
033180     MOVE 000     TO GOOD-SQLCODECODES                                    
033181     EXEC SQL                                                             
033182        INSERT INTO T01BURE                                               
033183          (IDLEGSEL,                                                      
033184           KDFINDOC,                                                      
033185           KDPARTTY,                                                      
033186           KDPARTGR,                                                      
033190           KDSTATUS,                                                      
033191           BEFORMS,                                                       
033192           FLGL,                                                          
033193           FLAR,                                                          
033194           FLAP,                                                          
033195           FLVATCHK,                                                      
033197           DAREGDAT,                                                      
033198           DAUPPDAT,                                                      
033199           DADELDAT,                                                      
033200           IDUSER)                                                        
033201        VALUES(:REQU-IDLEGSEL-KEY,                                        
033202               :REQU-KDFINDOC-KEY,                                        
033203               :REQU-KDPARTTY-KEY,                                        
033204               :REQU-KDPARTGR-KEY,                                        
033205               :WS-COMING,                                                
033206               :REQU-BEFORMS,                                             
033207               :WS-FLGL,                                                  
033208               :WS-FLAR,                                                  
033209               :WS-FLAP,                                                  
033210               :WS-FLVATCHK,                                              
033211               :WS-DAREGDAT,                                              
033213               :REQU-DAUPPDAT,                                            
033214               :WS-DADELDAT,                                              
033215               :REQU-IDUSER)                                              
033216     END-EXEC                                                             
033217     MOVE SQLCODE TO SQLCODE-WS                                           
033218     PERFORM DB2-STATUS-CHECK                                             
033219     .                                                                    
033220     EJECT                                                                
033221                                                                          
033222 DB2-DELETE-T01BURE-COMING SECTION.                                       
033223     MOVE 000    TO GOOD-SQLCODECODES                                     
033224     EXEC SQL                                                             
033225           DELETE FROM T01BURE                                            
033226           WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                            
033227           AND   KDFINDOC = :REQU-KDFINDOC-KEY                            
033228           AND   KDPARTTY = :REQU-KDPARTTY-KEY                            
033229           AND   KDPARTGR = :REQU-KDPARTGR-KEY                            
033230           AND   KDSTATUS = :WS-COMING                                    
033231     END-EXEC                                                             
033232     MOVE SQLCODE TO SQLCODE-WS                                           
033233     PERFORM DB2-STATUS-CHECK                                             
033234     .                                                                    
033235     EJECT                                                                
033236                                                                          
033240 DB2-SELECT-T01NSAS SECTION.                                              
033300     MOVE 000100 TO GOOD-SQLCODECODES                                     
033500     EXEC SQL                                                             
034594           SELECT IDLEGSEL                                                
034600                                                                          
034601           INTO :WS-IDLEGSEL                                              
034602                                                                          
034610           FROM     T01NSAS                                               
034620                                                                          
034630           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
034631                AND KDFINDOC = :REQU-KDFINDOC-KEY                         
034632                AND KDPARTTY = :REQU-KDPARTTY-KEY                         
034640                AND KDPARTGR = :REQU-KDPARTGR-KEY                         
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
