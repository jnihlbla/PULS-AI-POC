000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W0081500.                                                
000400 AUTHOR.         SJÖBLOM ELAINE.                                          
000500 DATE-WRITTEN.   20/10/19.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        PROGRAM READS AND UPDATES POSTAL CODE DATABASE.                  
001000*        DATABASE WDR5  HTYP 4133.                                        
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W0T815                                              
001400*        MID:         W0I81501                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001700*        MOD:         W0O81501                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W0081500'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND FELLOG         
002800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  YES                         PIC X       VALUE 'J'.                   
003100 77  NOO                         PIC X       VALUE 'N'.                   
003200                                                                          
003300*    --- INDEX FOR SCROLL LINES                                           
003400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003410 77  MFS-INDX                    PIC S9(4)  VALUE +0    COMP SYNC.        
003420 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
003430*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
003440                                                                          
003450 77  CURRENT-IMS-SECTION          PIC X(16)  VALUE SPACE.                 
003460                                                                          
003470 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003480     88  NYCKLAR-OK                          VALUE 'J'.                   
003490     88  NYCKLAR-FEL                         VALUE 'N'.                   
003500                                                                          
003600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003700     88  INDATA-OK                           VALUE 'J'.                   
003800     88  INDATA-FEL                          VALUE 'N'.                   
003900                                                                          
004000 77  INDATA-EXISTS-SW            PIC X       VALUE 'N'.                   
004100     88  INDATA-EXISTS                       VALUE 'J'.                   
004200                                                                          
004300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004400     88  ALLT-OK                             VALUE 'J'.                   
004500                                                                          
004600 77  RAD-VALD-SW                 PIC X       VALUE 'N'.                   
004700     88  RAD-VALD                            VALUE 'J'.                   
004800     88  RAD-EJ-VALD                         VALUE 'N'.                   
004810                                                                          
004820 77  DEFAULT-LINE-SW             PIC X       VALUE 'N'.                   
004830     88  DEFAULT-LINE-MISSING                VALUE 'J'.                   
004840                                                                          
004841 77  COUNTRY-SW                  PIC X       VALUE 'N'.                   
004842     88  COUNTRY-WRONG                       VALUE 'J'.                   
004843                                                                          
004844 77  E-RAD-FEL-SW                PIC X       VALUE 'N'.                   
004845     88  E-RAD-FEL                           VALUE 'J'.                   
004846                                                                          
004847 77  E-LAND-FEL-SW               PIC X       VALUE 'N'.                   
004848     88  E-LAND-FEL                          VALUE 'J'.                   
004849 77  E-ADPOSTNR-FOM-FEL-SW       PIC X       VALUE 'N'.                   
004850     88  E-ADPOSTNR-FOM-FEL                  VALUE 'J'.                   
004860 77  E-ADPOSTNR-TOM-FEL-SW       PIC X       VALUE 'N'.                   
004870     88  E-ADPOSTNR-TOM-FEL                  VALUE 'J'.                   
004880 77  E-DIST-FEL-SW               PIC X       VALUE 'N'.                   
004890     88  E-DIST-FEL                          VALUE 'J'.                   
004891 77  E-KUND-FEL-SW               PIC X       VALUE 'N'.                   
004892     88  E-KUND-FEL                          VALUE 'J'.                   
004893                                                                          
004894 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004895     88  EGEN-MID                            VALUE '0815'.                
004896     88  GOOD-MID                            VALUE '0811' '0812'          
004897                                                   '0813' '0814'          
004898                                                   '0815' '0816'          
004899                                                   '0817' '0818'          
004900                                                   '0819'.                
005000     88  HELP-MID                            VALUE '0551'.                
005100     EJECT                                                                
005200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005300 01  GENERAL-SUBPROGRAMS.                                                 
005400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
005900     03  WISOLAND                PIC X(8)    VALUE 'WISOLAND'.            
006000     EJECT                                                                
006100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
006200*01 -COPY WMEDAREA                                                        
006300     SKIP3                                                                
006400 01  MESSAGE-CODES.                                                       
006500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
006800     03  ERR-FORBIDDEN-INSERT    PIC X(3)    VALUE '007'.                 
006900     03  ERR-MISSING-REG         PIC X(3)    VALUE '010'.                 
007000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007100     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
007200     03  ERR-DIST-CUST-MISSING   PIC X(3)    VALUE '040'.                 
007210     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007220     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007230     03  ERR-LINE-EXISTS         PIC X(3)    VALUE '245'.                 
007240     03  ERR-DELETE-FORBIDDEN    PIC X(3)    VALUE '365'.                 
007250     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007260     03  ERR-DIST-CUST-DB        PIC X(3)    VALUE '412'.                 
007270     03  ERR-WRONG-INTERVAL      PIC X(3)    VALUE '738'.                 
007280     03  ERR-NOTHING-CHANGED     PIC X(3)    VALUE '414'.                 
007290                                                                          
007300     EJECT                                                                
007400*01  -COPY W009REDU                                                       
007500     EJECT                                                                
007501                                                                          
007502*    --- PARAMETERS FOR W009REDU                                          
007503 01  WS-REDUIN                   PIC X(30)  VALUE SPACE.                  
007504 01  WS-REDUUT                   PIC X(30)  VALUE SPACE.                  
007505                                                                          
007506*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
007507 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
007508*01 -COPY WISOLAND                                                        
007509 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007510                                                                          
007520*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
007530*                                                                         
007540     SKIP3                                                                
007550*01 -COPY WMSGINIT                                                        
007560     EJECT                                                                
007570*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
007580*                                                                         
007590 01  SPAR-AREA.                                                           
007600     03  SPAR-IDTRANS              PIC X(4)  VALUE '0815'.                
007700     03  SPAR-IDSYSMOT-KEY        PIC X(10) VALUE SPACE.                  
007800     03  SPAR-IDDISTR-KEY          PIC 9(4)  VALUE ZERO.                  
007900     03  SPAR-IDLANDX2-ENTER       PIC X(2)  VALUE SPACE.                 
008000     03  SPAR-IDLANDX2-NEXT        PIC X(2)  VALUE SPACE.                 
008100     03  SPAR-ADPOSTNR-FOM-ENTER   PIC X(10) VALUE SPACE.                 
008200     03  SPAR-ADPOSTNR-TOM-ENTER   PIC X(10) VALUE SPACE.                 
008300     03  SPAR-ADPOSTNR-FOM-NEXT    PIC X(10) VALUE SPACE.                 
008400     03  SPAR-ADPOSTNR-TOM-NEXT    PIC X(10) VALUE SPACE.                 
008500     03 SPAR-TABELL.                                                      
008600        05 SPAR-WDGX4134 OCCURS 12.                                       
008700           09 SPAR-IDLANDX2        PIC X(02)  VALUE SPACE.                
008800           09 SPAR-ADPOSTNR-FOM    PIC X(10)  VALUE SPACE.                
008900           09 SPAR-ADPOSTNR-TOM    PIC X(10)  VALUE SPACE.                
009000                                                                          
009100 01  WS-IDLANDX2         PIC X(2)    VALUE SPACE.                         
009200 01  WS-ADPOSTNR-FOM     PIC X(10)   VALUE SPACE.                         
009210 01  WS-ADPOSTNR-TOM     PIC X(10)   VALUE SPACE.                         
009220 01  WS-LYNK             PIC X(4)    VALUE 'LYNK'.                        
009230 01  WS-POLESTAR         PIC X(4)    VALUE 'POLE'.                        
009240 01  WS-ECOM             PIC X(4)    VALUE 'ECOM'.                        
009250 01  WS-VOUI             PIC X(4)    VALUE 'VOUI'.                        
009260 01  WS-TACDIS           PIC X(4)    VALUE 'TAD '.                        
009261 01  WS-ACC              PIC X(4)    VALUE 'ACC '.                        
009262 01  WS-APA              PIC X(4)    VALUE 'APA '.                        
009263 01  WS-APB              PIC X(4)    VALUE 'APB '.                        
009264 01  WS-APC              PIC X(4)    VALUE 'APC '.                        
009265 01  WS-APD              PIC X(4)    VALUE 'APD '.                        
009266 01  WS-APE              PIC X(4)    VALUE 'APE '.                        
009267 01  WS-APF              PIC X(4)    VALUE 'APF '.                        
009268 01  WS-APG              PIC X(4)    VALUE 'APG '.                        
009269 01  WS-APH              PIC X(4)    VALUE 'APH '.                        
009270 01  WS-API              PIC X(4)    VALUE 'API '.                        
009271 01  WS-APJ              PIC X(4)    VALUE 'APJ '.                        
009272     EJECT                                                                
009280*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009290*                                                                         
009300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009400     SKIP3                                                                
009500*01  MID -COPY W0I81501                                                   
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009800     SKIP3                                                                
009900*01  -COPY WMSGAREA                                                       
010000     EJECT                                                                
010100     03  MOD REDEFINES MSG-AREA.                                          
010200*      05  -COPY W0O81501                                                 
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010500     SKIP3                                                                
010600*01  -COPY WMFSAREA                                                       
010700     EJECT                                                                
010800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010900*                                                                         
011000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011100     SKIP3                                                                
011200 01  KEYS-FOR-DLI.                                                        
011300*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
011400     03  W-WDGX4133-X.                                                    
011500         05  W-IDHTYP            PIC X(4)    VALUE '4133'.                
011600         05  W-IDSYSMOT          PIC X(10)   VALUE SPACE.                 
011610         05  W-FILLER            PIC X(16)   VALUE LOW-VALUE.             
011620                                                                          
011630     03  W-WDGX4134-X.                                                    
011640         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
011650         05  W-ADPOSTNR-FOM      PIC X(10)   VALUE SPACE.                 
011660         05  W-ADPOSTNR-TOM      PIC X(10)   VALUE SPACE.                 
011670                                                                          
011680     03  W-WDGX4134-MIN-X.                                                
011690         05  W-IDLANDX2-MIN      PIC X(2)    VALUE LOW-VALUE.             
011700         05  W-ADPOSTNR-FOM-MIN  PIC X(10)   VALUE LOW-VALUE.             
011800         05  W-ADPOSTNR-TOM-MIN  PIC X(10)   VALUE LOW-VALUE.             
011900                                                                          
012000     03  W-WDGX4134-MAX-X.                                                
012100         05  W-IDLANDX2-MAX      PIC X(2)    VALUE HIGH-VALUE.            
012200         05  W-ADPOSTNR-FOM-MAX  PIC X(10)   VALUE HIGH-VALUE.            
012300         05  W-ADPOSTNR-TOM-MAX  PIC X(10)   VALUE HIGH-VALUE.            
012400                                                                          
012500     03  W-WDGX4134-M-X.                                                  
012510         05  W-IDLANDX2-M        PIC X(2)    VALUE SPACE.                 
012511         05  W-ADPOSTNR-FOM-M    PIC X(10)   VALUE SPACE.                 
012512         05  W-ADPOSTNR-TOM-M    PIC X(10)   VALUE SPACE.                 
012513                                                                          
012514     03  W-WDGX4134-DEF-X.                                                
012515         05  W-IDLANDX2-DEF      PIC X(2)    VALUE SPACE.                 
012516         05  W-ADPOSTNR-FOM-DEF  PIC X(10)   VALUE '9999999999'.          
012517         05  W-ADPOSTNR-TOM-DEF  PIC X(10)   VALUE '9999999999'.          
012518                                                                          
012519     03  W-IDGMT-X.                                                       
012520         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
012530         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
012540                                                                          
012541     03  W-IDDISTR-X.                                                     
012542         05 W-IDDISTR-SEARCH     PIC S9(5)  VALUE ZERO COMP-3.            
012543                                                                          
012544     SKIP2                                                                
012545*    --- STATUS CODES FROM IMS                                            
012546 01  STATUS-WS                   PIC XX.                                  
012547     88  SEGMENT-FOUND                       VALUE '  '.                  
012548     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012549     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012550     SKIP2                                                                
012560 01  GOOD-STATUSCODES.                                                    
012570     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012580     SKIP3                                                                
012590 01  SSA1                        PIC X(96).                               
012600 01  SSA2                        PIC X(64).                               
012700     EJECT                                                                
012800*    --- IMS FUNCTION CODES                                               
012900*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200                                                                          
013300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR051'.                      
013400 01  DLI-IO-WDR501.                                                       
013500*    03  -COPY WDGX4133                                                   
013600                                                                          
013700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4134'.                    
013800 01  DLI-IO-WDGX4134.                                                     
013900*    03  -COPY WDGX4134                                                   
014000                                                                          
014100                                                                          
014200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
014300 01  DLI-IO-WDB201.                                                       
014310*    03  -COPY WDB201                                                     
014320                                                                          
014330                                                                          
014340     EJECT                                                                
014350 LINKAGE SECTION.                                                         
014360*01  -COPY W0009   -PRE MSG-                                              
014370*01  -COPY W0008   -PRE WDP7-                                             
014380     05  FILLER                  PIC X.                                   
014390                                                                          
014400*01  -COPY W0008  -PRE WDR5-                                              
014500     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014700*01  -COPY W0008  -PRE WDB2-                                              
014800     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015000 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDR5-PCB WDB2-PCB.            
015100 MAIN SECTION.                                                            
015200     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDR5-PCB WDB2-PCB.            
015300                                                                          
015400     PERFORM IMS-GET-MSG                                                  
015500     IF SEGMENT-FOUND                                                     
015600       PERFORM A-INIT                                                     
015700       PERFORM B-CHECK-KEYS                                               
015800       IF NYCKLAR-OK                                                      
015900         IF MFS-UPDATE                                                    
016000           PERFORM G-CHECK-INPUT                                          
016100           IF INDATA-OK                                                   
016200              PERFORM H-UPDATE                                            
016300           END-IF                                                         
016400         ELSE                                                             
016500           IF MFS-FIRST                                                   
016600             PERFORM C-FIRST-PAGE                                         
016700           ELSE                                                           
016800             IF MFS-NEXT                                                  
016900               PERFORM D-NEXT-PAGE                                        
017000             ELSE                                                         
017100               PERFORM E-SAME-PAGE                                        
017200             END-IF                                                       
017300           END-IF                                                         
017400        END-IF                                                            
017500        IF ALLT-OK                                                        
017600          PERFORM F-READ-SHOW-INFO                                        
017700        ELSE                                                              
017800          IF E-RAD-FEL                                                    
017900            PERFORM F-READ-SHOW-INFO                                      
018000            PERFORM S01-E-LINE-TO-MOD                                     
018100          END-IF                                                          
018200        END-IF                                                            
018300       END-IF                                                             
018400*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
018500*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
018600       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O81501 + 4                      
018700       PERFORM IMS-INSERT-MSG                                             
018800     END-IF                                                               
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300     EJECT                                                                
019400 A-INIT SECTION.                                                          
019500                                                                          
019600     IF MSG-DOUBLE-TRANSACTIONS                                           
019700       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W0I81501                 
019800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020000     ELSE                                                                 
020100       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W0I81501                  
020200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020400     END-IF                                                               
020500                                                                          
020600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
020800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020900                                                                          
021000     MOVE LOW-VALUE TO MSG-AREA                                           
021100     MOVE 'W0O815N1' TO MFS-IDMOD                                         
021200     MOVE '0815' TO MOD-IDTRANS                                           
021300     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
021400                                                                          
021500     IF EGEN-MID OR HELP-MID                                              
021600       IF MID-IDSYSMOT-IN NOT = ALL '+'  OR                               
021700          MID-IDDISTR-IN  NOT = ALL '+'                                   
021800         MOVE SPACE TO MFS-KDTRTYP                                        
021900         MOVE '7' TO MFS-IDPFK                                            
022000       ELSE                                                               
022100         CONTINUE                                                         
022200       END-IF                                                             
022300     ELSE                                                                 
022400       MOVE SPACE TO MFS-KDTRTYP                                          
022500       MOVE '7' TO MFS-IDPFK                                              
022600     END-IF                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 B-CHECK-KEYS SECTION.                                                    
023000                                                                          
023100                                                                          
023200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023300     MOVE '001'             TO MSGI-KDCALL                                
023400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023600     MOVE '0815'            TO MSGI-IDTRANS                               
023700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
023800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
023900                                                                          
024000*    - LANGUAGE TO BE USED BY MEDKONV                                     
024100     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
024200                                                                          
024300     MOVE YES TO NYCKLAR-SW                                               
024400     MOVE YES TO ALLT-SW                                                  
024500     MOVE SPACE TO MED-IDMFSFEL MED-IDMFSINF                              
024600                                                                          
024700     IF EGEN-MID                                                          
024800*       *CHECK OF PARTNER                                                 
024900        MOVE MFS-RENSA-FAELT       TO MOD-IDSYSMOT-IN                     
025000        IF MID-IDSYSMOT-IN NOT = ALL '+'                                  
025100           MOVE MID-IDSYSMOT-IN    TO W-IDSYSMOT                          
025200                                      MOD-IDSYSMOT-UT                     
025300                                      SPAR-IDSYSMOT-KEY                   
025400           IF MID-IDSYSMOT-IN = WS-LYNK OR WS-POLESTAR OR                 
025410              WS-ECOM OR WS-VOUI OR WS-TACDIS OR WS-ACC OR                
025411              WS-APA  OR WS-APB OR WS-APC OR WS-APD OR WS-APE OR          
025412              WS-APF  OR WS-APG OR WS-APH OR WS-API OR WS-APJ             
025413                                                                          
025414              CONTINUE                                                    
025415           ELSE                                                           
025416              MOVE NOO             TO NYCKLAR-SW                          
025417           END-IF                                                         
025418        ELSE                                                              
025419         IF MID-IDSYSMOT-UT NOT = ALL '+'                                 
025420            AND NOT = SPACE                                               
025421            MOVE MID-IDSYSMOT-UT TO MOD-IDSYSMOT-UT                       
025422                                       SPAR-IDSYSMOT-KEY                  
025423                                       W-IDSYSMOT                         
025424         ELSE                                                             
025425            MOVE NOO               TO NYCKLAR-SW                          
025426         END-IF                                                           
025427        END-IF                                                            
025428                                                                          
025429*       *CHECK OF DISTRICT                                                
025430        MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                      
025431                                                                          
025432        IF MID-IDDISTR-IN NOT = ALL '+'                                   
025433           INSPECT MID-IDDISTR-IN REPLACING LEADING SPACE BY ZERO         
025434           IF MID-IDDISTR-IN NUMERIC                                      
025435              MOVE MID-IDDISTR-IN  TO W-IDDISTR-SEARCH                    
025436                                      MOD-IDDISTR-UT                      
025437                                      SPAR-IDDISTR-KEY                    
025438              INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO               
025439              BY SPACE                                                    
025440           ELSE                                                           
025441              MOVE MID-IDDISTR-IN  TO MOD-IDDISTR-UT                      
025442              MOVE NOO             TO NYCKLAR-SW                          
025443           END-IF                                                         
025444        ELSE                                                              
025445           IF MID-IDDISTR-UT NOT = ALL '+'                                
025446              MOVE MID-IDDISTR-UT  TO MOD-IDDISTR-UT                      
025447                                        SPAR-IDDISTR-KEY                  
025448                                        W-IDDISTR-SEARCH                  
025449           END-IF                                                         
025450        END-IF                                                            
025451     ELSE                                                                 
025452        MOVE ZERO                  TO SPAR-IDDISTR-KEY                    
025453        MOVE SPACE                 TO SPAR-IDSYSMOT-KEY                   
025454     END-IF                                                               
025455                                                                          
025456     IF NYCKLAR-FEL                                                       
025457       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
025458       CALL WMEDKONV USING MED-WMEDAREA                                   
025459       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025460       PERFORM MFS-ERASE-FIELD-IN                                         
025461       PERFORM MFS-ERASE-FIELD-OUT                                        
025462     END-IF                                                               
025463     .                                                                    
025470     EJECT                                                                
025480 C-FIRST-PAGE SECTION.                                                    
025490                                                                          
025500     IF MID-IDSYSMOT-IN NOT = ALL '+'                                     
025600        MOVE MID-IDSYSMOT-IN  TO SPAR-IDSYSMOT-KEY                        
025700     END-IF                                                               
025800     IF MID-IDDISTR-IN NOT = ALL '+'                                      
025900        INSPECT MID-IDDISTR-IN REPLACING LEADING SPACE BY ZERO            
026000        MOVE MID-IDDISTR-IN TO SPAR-IDDISTR-KEY                           
026100     END-IF                                                               
026200                                                                          
026300     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
026400     CALL WMEDKONV USING MED-WMEDAREA                                     
026500     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
026600                                                                          
026700     PERFORM MFS-ERASE-FIELD-IN                                           
026800     .                                                                    
026900     EJECT                                                                
027000 D-NEXT-PAGE SECTION.                                                     
027100                                                                          
027200     IF SPAR-IDTRANS = '0815'                                             
027300       MOVE SPAR-IDLANDX2-NEXT     TO W-IDLANDX2                          
027400       MOVE SPAR-ADPOSTNR-FOM-NEXT TO W-ADPOSTNR-FOM                      
027500       MOVE SPAR-ADPOSTNR-TOM-NEXT TO W-ADPOSTNR-TOM                      
027600     ELSE                                                                 
027700       PERFORM MFS-ERASE-FIELD-IN                                         
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 E-SAME-PAGE  SECTION.                                                    
028200                                                                          
028300     IF SPAR-IDTRANS = '0815'                                             
028310        MOVE SPAR-IDLANDX2-ENTER     TO W-IDLANDX2                        
028320        MOVE SPAR-ADPOSTNR-FOM-ENTER TO W-ADPOSTNR-FOM                    
028330        MOVE SPAR-ADPOSTNR-TOM-ENTER TO W-ADPOSTNR-TOM                    
028340     END-IF                                                               
028350     MOVE YES  TO INDATA-SW                                               
028360     MOVE NOO TO RAD-VALD-SW                                              
028370                                                                          
028380     MOVE 1 TO INDX                                                       
028390     PERFORM UNTIL INDX > MAX-INDX                                        
028391        IF MID-CMD (INDX) NOT = '+' AND                                   
028392           MID-CMD (INDX) NOT = ' '                                       
028393           IF MID-CMD (INDX) NOT = 'C' OR                                 
028394              RAD-VALD                                                    
028395              IF MID-CMD (INDX) = 'D'                                     
028396                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR (INDX)         
028397              ELSE                                                        
028398                 PERFORM MFS-DONT-TOUCH-FIELD-OUT                         
028399                 PERFORM MFS-DONT-TOUCH-FIELD-IN                          
028400                 PERFORM MFS-DONT-TOUCH-FIELD-IN-E                        
028401                 PERFORM MFS-READ-IN-AGAIN-E                              
028402                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-ATTR (INDX)         
028403                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
028404                 CALL WMEDKONV USING MED-WMEDAREA                         
028405                 MOVE MED-MFSFEL           TO MOD-TEMFSFEL                
028406                 MOVE NOO TO INDATA-SW                                    
028407              END-IF                                                      
028408           ELSE                                                           
028409              PERFORM EA-MOVE-CHOSEN-LINE                                 
028410              MOVE YES TO RAD-VALD-SW                                     
028411           END-IF                                                         
028412        END-IF                                                            
028413        ADD 1 TO INDX                                                     
028414     END-PERFORM                                                          
028415                                                                          
028416     IF INDATA-OK AND                                                     
028417        SPAR-IDTRANS = '0815' OR '0551'                                   
028418        IF RAD-VALD                                                       
028419           PERFORM MFS-READ-IN-AGAIN-E                                    
028420           MOVE MFS-STAENG-FAELT TO MOD-CMD-E-ATTR                        
028430                                    MOD-IDLANDX2-E-ATTR                   
028431                                    MOD-ADPOSTNR-FOM-E-ATTR               
028432                                    MOD-ADPOSTNR-TOM-E-ATTR               
028433           MOVE 1 TO INDX                                                 
028434           PERFORM UNTIL INDX > MAX-INDX                                  
028435              MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR (INDX)                
028436              ADD +1 TO INDX                                              
028437           END-PERFORM                                                    
028438        ELSE                                                              
028439           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
028440           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
028441           PERFORM MFS-DONT-TOUCH-FIELD-IN-E                              
028442           PERFORM MFS-READ-IN-AGAIN-E                                    
028443           IF MID-CMD-E ='C'                                              
028444             MOVE MFS-STAENG-FAELT TO MOD-CMD-E-ATTR                      
028445                                      MOD-IDLANDX2-E-ATTR                 
028446                                      MOD-ADPOSTNR-FOM-E-ATTR             
028447                                      MOD-ADPOSTNR-TOM-E-ATTR             
028448           END-IF                                                         
028449        END-IF                                                            
028450        MOVE NOO TO INDATA-EXISTS-SW                                      
028451        MOVE +1 TO INDX                                                   
028452        PERFORM UNTIL INDX > MAX-INDX                                     
028453          IF MID-CMD (INDX) NOT = ALL '+'                                 
028454             MOVE YES  TO INDATA-EXISTS-SW                                
028455          END-IF                                                          
028456          ADD 1 TO INDX                                                   
028457        END-PERFORM                                                       
028458        IF MID-CMD-E NOT = ALL '+'  OR                                    
028459           MID-IDLANDX2-E NOT = ALL '+' OR                                
028460           MID-ADPOSTNR-FOM-E NOT = ALL '+' OR                            
028461           MID-IDDISTR-E NOT = ALL '+' OR                                 
028462           MID-IDKUNDNR-E NOT = ALL '+'                                   
028463           MOVE YES TO INDATA-EXISTS-SW                                   
028464        END-IF                                                            
028465        IF INDATA-EXISTS                                                  
028466          MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                             
028467          CALL WMEDKONV USING MED-WMEDAREA                                
028468          MOVE MED-MFSFEL     TO MOD-TEMFSFEL                             
028469        END-IF                                                            
028470     END-IF                                                               
028471     .                                                                    
028472                                                                          
028473                                                                          
028474 EA-MOVE-CHOSEN-LINE SECTION.                                             
028475                                                                          
028476     MOVE MFS-ERASE-FIELD    TO MOD-CMD (INDX)                            
028477                                                                          
028478     MOVE SPAR-IDSYSMOT-KEY       TO W-IDSYSMOT                           
028479     MOVE MID-IDLANDX2     (INDX) TO W-IDLANDX2-M                         
028480     MOVE MID-ADPOSTNR-FOM (INDX) TO W-ADPOSTNR-FOM-M                     
028481     MOVE MID-ADPOSTNR-TOM (INDX) TO W-ADPOSTNR-TOM-M                     
028482                                                                          
028483     PERFORM IMS-GU-WDGX4133                                              
028484     PERFORM IMS-GNP-WDGX4134M                                            
028485                                                                          
028486     MOVE 'C'          TO MOD-CMD-E                                       
028487                          MID-CMD-E                                       
028488     MOVE 4134-IDLANDX2     TO MOD-IDLANDX2-E                             
028489     MOVE 4134-ADPOSTNR-FOM TO MOD-ADPOSTNR-FOM-E                         
028490     MOVE 4134-ADPOSTNR-TOM TO MOD-ADPOSTNR-TOM-E                         
028491     MOVE 4134-IDDISTR      TO MOD-IDDISTR-E                              
028492     MOVE 4134-IDKUNDNR     TO MOD-IDKUNDNR-E                             
028493                                                                          
028494     MOVE MFS-STAENG-FAELT  TO MOD-CMD-E-ATTR                             
028495                               MOD-IDLANDX2-E-ATTR                        
028496                               MOD-ADPOSTNR-FOM-E-ATTR                    
028497                               MOD-ADPOSTNR-TOM-E-ATTR                    
028498                               MOD-IDDISTR-E-ATTR                         
028499                               MOD-IDKUNDNR-E-ATTR                        
028500                                                                          
028501     .                                                                    
028502                                                                          
028503                                                                          
028504 EB-MID-INDATA-TO-MOD SECTION.                                            
028505                                                                          
028506     IF RAD-EJ-VALD                                                       
028507        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-E-ATTR                      
028508                                      MOD-IDLANDX2-E-ATTR                 
028509                                      MOD-ADPOSTNR-FOM-E-ATTR             
028510                                      MOD-ADPOSTNR-TOM-E-ATTR             
028511                                      MOD-IDDISTR-E-ATTR                  
028512                                      MOD-IDKUNDNR-E-ATTR                 
028513     END-IF                                                               
028514                                                                          
028515     .                                                                    
028516     EJECT                                                                
028517 F-READ-SHOW-INFO SECTION.                                                
028518                                                                          
028519     IF SPAR-IDDISTR-KEY > 0                                              
028520        PERFORM FA-READ-PARTNER-DISTRICT                                  
028530     ELSE                                                                 
028540        PERFORM FB-READ-PARTNER                                           
028550     END-IF                                                               
028560                                                                          
028570     MOVE '002'      TO MSGI-KDCALL                                       
028580     MOVE '0815'   TO SPAR-IDTRANS                                        
028590     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
028600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
028700     .                                                                    
028800     EJECT                                                                
028900 FA-READ-PARTNER-DISTRICT SECTION.                                        
029000                                                                          
029100     MOVE SPAR-IDSYSMOT-KEY  TO W-IDSYSMOT                                
029200     PERFORM IMS-GU-WDGX4133                                              
029300                                                                          
029400     IF SEGMENT-MISSING                                                   
029500          MOVE ERR-MISSING-REG      TO MED-IDMFSFEL                       
029600          CALL WMEDKONV USING MED-WMEDAREA                                
029700          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
029800          PERFORM MFS-ERASE-FIELD-OUT                                     
029900     ELSE                                                                 
030000       MOVE +1 TO INDX                                                    
030100       MOVE LOW-VALUE           TO W-IDLANDX2-MIN                         
030200       MOVE HIGH-VALUE          TO W-IDLANDX2-MAX                         
030300       PERFORM IMS-GNP-WDGX4134-MIN-MAX-DISTR                             
030400       IF SEGMENT-FOUND                                                   
030500         MOVE 4134-IDLANDX2     TO SPAR-IDLANDX2-ENTER                    
030600         MOVE 4134-ADPOSTNR-FOM TO SPAR-ADPOSTNR-FOM-ENTER                
030700         MOVE 4134-ADPOSTNR-TOM TO SPAR-ADPOSTNR-TOM-ENTER                
030800       ELSE                                                               
030900         MOVE ERR-MISSING-REG      TO MED-IDMFSFEL                        
031000         CALL WMEDKONV USING MED-WMEDAREA                                 
031100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
031200         PERFORM MFS-ERASE-FIELD-OUT                                      
031300         MOVE W-IDLANDX2         TO SPAR-IDLANDX2-ENTER                   
031400         MOVE W-ADPOSTNR-FOM     TO SPAR-ADPOSTNR-FOM-ENTER               
031500         MOVE W-ADPOSTNR-TOM     TO SPAR-ADPOSTNR-TOM-ENTER               
031600       END-IF                                                             
031700                                                                          
031800       IF SEGMENT-FOUND                                                   
031900         PERFORM UNTIL INDX > MAX-INDX                                    
032000           IF SEGMENT-FOUND                                               
032100             MOVE 4134-IDLANDX2     TO MOD-IDLANDX2     (INDX)            
032200             MOVE 4134-ADPOSTNR-FOM TO MOD-ADPOSTNR-FOM (INDX)            
032300             MOVE 4134-ADPOSTNR-TOM TO MOD-ADPOSTNR-TOM (INDX)            
032400             MOVE 4134-IDDISTR      TO MOD-IDDISTR      (INDX)            
032500             MOVE 4134-IDKUNDNR     TO MOD-IDKUNDNR     (INDX)            
032600             MOVE 4134-IDDISTR      TO W-IDDISTR                          
032700             MOVE 4134-IDKUNDNR     TO W-IDKUNDNR                         
032800             PERFORM IMS-GU-WDB201                                        
032900             MOVE GMT-IDDC-DAY (1)  TO MOD-IDDC (INDX)                    
033000             PERFORM IMS-GNP-WDGX4134-MIN-MAX-DISTR                       
033100           ELSE                                                           
033200             MOVE MFS-ERASE-FIELD TO MOD-IDLANDX2     (INDX)              
033300                                     MOD-ADPOSTNR-FOM (INDX)              
033400                                     MOD-ADPOSTNR-TOM (INDX)              
033500                                     MOD-IDDISTR      (INDX)              
033600                                     MOD-IDKUNDNR     (INDX)              
033700                                     MOD-IDDC         (INDX)              
033800           END-IF                                                         
033900           ADD 1 TO INDX                                                  
034000         END-PERFORM                                                      
034100                                                                          
034200         IF SEGMENT-FOUND                                                 
034300           MOVE 4134-IDLANDX2      TO SPAR-IDLANDX2-NEXT                  
034400           MOVE 4134-ADPOSTNR-FOM  TO SPAR-ADPOSTNR-FOM-NEXT              
034500           MOVE 4134-ADPOSTNR-TOM  TO SPAR-ADPOSTNR-TOM-NEXT              
034600           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
034700           CALL WMEDKONV USING MED-WMEDAREA                               
034800           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
034900         ELSE                                                             
035000           MOVE 4134-IDLANDX2      TO SPAR-IDLANDX2-NEXT                  
035100           MOVE 4134-ADPOSTNR-FOM  TO SPAR-ADPOSTNR-FOM-NEXT              
035200           MOVE 4134-ADPOSTNR-TOM  TO SPAR-ADPOSTNR-TOM-NEXT              
035300         END-IF                                                           
035400       END-IF                                                             
035500     END-IF                                                               
035600                                                                          
035700     .                                                                    
035800     EJECT                                                                
035900 FB-READ-PARTNER  SECTION.                                                
036000                                                                          
036100     MOVE SPAR-IDSYSMOT-KEY  TO W-IDSYSMOT                                
036200     PERFORM IMS-GU-WDGX4133                                              
036300                                                                          
036400     IF SEGMENT-MISSING                                                   
036500        IF MID-CMD-E NOT = 'N'                                            
036600          MOVE ERR-MISSING-REG      TO MED-IDMFSFEL                       
036700          CALL WMEDKONV USING MED-WMEDAREA                                
036800          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
036900          PERFORM MFS-ERASE-FIELD-OUT                                     
037000        END-IF                                                            
037100     ELSE                                                                 
037200                                                                          
037300       MOVE +1 TO INDX                                                    
037400       PERFORM IMS-GNP-WDGX4134                                           
037410       IF SEGMENT-FOUND                                                   
037420         MOVE 4134-IDLANDX2     TO SPAR-IDLANDX2-ENTER                    
037430         MOVE 4134-ADPOSTNR-FOM TO SPAR-ADPOSTNR-FOM-ENTER                
037440         MOVE 4134-ADPOSTNR-TOM TO SPAR-ADPOSTNR-TOM-ENTER                
037450       ELSE                                                               
037460         MOVE W-IDLANDX2         TO SPAR-IDLANDX2-ENTER                   
037470         MOVE W-ADPOSTNR-FOM     TO SPAR-ADPOSTNR-FOM-ENTER               
037480         MOVE W-ADPOSTNR-TOM     TO SPAR-ADPOSTNR-TOM-ENTER               
037490       END-IF                                                             
037500                                                                          
037501       PERFORM UNTIL INDX > MAX-INDX                                      
037502         IF SEGMENT-FOUND                                                 
037503           MOVE 4134-IDLANDX2     TO MOD-IDLANDX2     (INDX)              
037504           MOVE 4134-ADPOSTNR-FOM TO MOD-ADPOSTNR-FOM (INDX)              
037505           MOVE 4134-ADPOSTNR-TOM TO MOD-ADPOSTNR-TOM (INDX)              
037506           MOVE 4134-IDDISTR      TO MOD-IDDISTR      (INDX)              
037507           MOVE 4134-IDKUNDNR     TO MOD-IDKUNDNR     (INDX)              
037508           MOVE 4134-IDDISTR      TO W-IDDISTR                            
037509           MOVE 4134-IDKUNDNR     TO W-IDKUNDNR                           
037510           PERFORM IMS-GU-WDB201                                          
037511           MOVE GMT-IDDC-DAY (1)  TO MOD-IDDC (INDX)                      
037512           PERFORM IMS-GNP-WDGX4134                                       
037513         ELSE                                                             
037514           MOVE MFS-ERASE-FIELD TO MOD-IDLANDX2     (INDX)                
037515                                   MOD-ADPOSTNR-FOM (INDX)                
037516                                   MOD-ADPOSTNR-TOM (INDX)                
037517                                   MOD-IDDISTR      (INDX)                
037518                                   MOD-IDKUNDNR     (INDX)                
037519                                   MOD-IDDC         (INDX)                
037520         END-IF                                                           
037521         ADD 1 TO INDX                                                    
037522       END-PERFORM                                                        
037523                                                                          
037524       IF SEGMENT-FOUND                                                   
037525         MOVE 4134-IDLANDX2      TO SPAR-IDLANDX2-NEXT                    
037526         MOVE 4134-ADPOSTNR-FOM  TO SPAR-ADPOSTNR-FOM-NEXT                
037527         MOVE 4134-ADPOSTNR-TOM  TO SPAR-ADPOSTNR-TOM-NEXT                
037528         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
037529         CALL WMEDKONV USING MED-WMEDAREA                                 
037530         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
037531       ELSE                                                               
037532         MOVE 4134-IDLANDX2      TO SPAR-IDLANDX2-NEXT                    
037533         MOVE 4134-ADPOSTNR-FOM  TO SPAR-ADPOSTNR-FOM-NEXT                
037534         MOVE 4134-ADPOSTNR-TOM  TO SPAR-ADPOSTNR-TOM-NEXT                
037535       END-IF                                                             
037536     END-IF                                                               
037537     .                                                                    
037538     EJECT                                                                
037539 G-CHECK-INPUT    SECTION.                                                
037540                                                                          
037550     MOVE YES          TO INDATA-SW                                       
037560     MOVE NOO          TO E-RAD-FEL-SW                                    
037570     MOVE NOO          TO RAD-VALD-SW                                     
037580     MOVE NOO          TO E-ADPOSTNR-FOM-FEL-SW                           
037590     MOVE NOO          TO E-ADPOSTNR-TOM-FEL-SW                           
037600     MOVE NOO          TO E-DIST-FEL-SW                                   
037700     MOVE NOO          TO E-KUND-FEL-SW                                   
037800                                                                          
037900     IF SPAR-IDTRANS = '0815'                                             
038000       MOVE SPAR-IDLANDX2-ENTER       TO W-IDLANDX2                       
038100       MOVE SPAR-ADPOSTNR-FOM-ENTER   TO W-ADPOSTNR-FOM                   
038200       MOVE SPAR-ADPOSTNR-TOM-ENTER   TO W-ADPOSTNR-TOM                   
038300     END-IF                                                               
038400                                                                          
038500     MOVE +1           TO INDX                                            
038600     PERFORM UNTIL INDX > MAX-INDX                                        
038700       IF MID-CMD (INDX) NOT = '+' AND ' '                                
038800          IF MID-CMD (INDX) = 'D' AND                                     
038900             MID-ADPOSTNR-FOM (INDX) NOT = '9999999999' AND               
039000             MID-ADPOSTNR-TOM (INDX) NOT = '9999999999'                   
039100             MOVE YES  TO RAD-VALD-SW                                     
039200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR (INDX)             
039300          ELSE                                                            
039400             MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-ATTR (INDX)             
039500             MOVE ERR-DELETE-FORBIDDEN TO MED-IDMFSFEL                    
039600             CALL WMEDKONV USING MED-WMEDAREA                             
039700             MOVE MED-MFSFEL       TO MOD-TEMFSFEL                        
039800             PERFORM MFS-DONT-TOUCH-FIELD-OUT                             
039900             PERFORM MFS-DONT-TOUCH-FIELD-IN                              
040000             PERFORM MFS-DONT-TOUCH-FIELD-IN-E                            
040100             MOVE NOO TO INDATA-SW                                        
040200          END-IF                                                          
040300       END-IF                                                             
040400       ADD +1          TO INDX                                            
040500     END-PERFORM                                                          
040600                                                                          
040700     IF INDATA-OK                                                         
040800        IF  MID-CMD-E = '+' OR ' '                                        
040900        AND MID-IDLANDX2-E     = ALL '+'                                  
041000        AND MID-ADPOSTNR-FOM-E = ALL '+'                                  
041100        AND MID-ADPOSTNR-TOM-E = ALL '+'                                  
041200        AND MID-IDDISTR-E      = ALL '+'                                  
041300        AND MID-IDKUNDNR-E     = ALL '+'                                  
041400           IF RAD-EJ-VALD                                                 
041500              MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                   
041600              CALL WMEDKONV USING MED-WMEDAREA                            
041700              MOVE MED-MFSFEL        TO MOD-TEMFSFEL                      
041800              MOVE NOO TO INDATA-SW                                       
041900              PERFORM MFS-DONT-TOUCH-FIELD-OUT                            
042000              PERFORM MFS-DONT-TOUCH-FIELD-IN                             
042100              PERFORM MFS-DONT-TOUCH-FIELD-IN-E                           
042200           END-IF                                                         
042300        END-IF                                                            
042400     END-IF                                                               
042500                                                                          
042600     IF INDATA-OK                                                         
042700        IF MID-CMD-E = 'N' OR 'C'                                         
042800           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-CMD-E-ATTR                   
042900           IF MID-CMD-E = 'N'                                             
043000              PERFORM GA-CHECK-NEW-RECORD                                 
043100           ELSE                                                           
043200              PERFORM GB-CHECK-CHANGE                                     
043300           END-IF                                                         
043400           IF INDATA-FEL                                                  
043500              IF DEFAULT-LINE-MISSING OR                                  
043600                 COUNTRY-WRONG                                            
043700                CONTINUE                                                  
043800              ELSE                                                        
043900                CALL WMEDKONV USING MED-WMEDAREA                          
044000                MOVE MED-MFSFEL TO MOD-TEMFSFEL                           
044100              END-IF                                                      
044200              MOVE NOO       TO ALLT-SW                                   
044300              PERFORM MFS-DONT-TOUCH-FIELD-OUT                            
044400              PERFORM MFS-DONT-TOUCH-FIELD-IN                             
044500              PERFORM MFS-DONT-TOUCH-FIELD-IN-E                           
044600           END-IF                                                         
044700        ELSE                                                              
044800           IF RAD-EJ-VALD                                                 
044900              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
045000              CALL WMEDKONV USING MED-WMEDAREA                            
045100              MOVE MED-MFSFEL           TO MOD-TEMFSFEL                   
045200              MOVE YES                  TO E-RAD-FEL-SW                   
045300              MOVE NOO                  TO INDATA-SW                      
045400              MOVE NOO                  TO ALLT-SW                        
045500              MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-E-ATTR                 
045600           END-IF                                                         
045700        END-IF                                                            
045800     END-IF                                                               
045900                                                                          
046000     IF INDATA-OK                                                         
046100       IF MID-CMD-E = 'C'                                                 
046200          MOVE MFS-STAENG-FAELT TO MOD-IDLANDX2-E-ATTR                    
046300                                   MOD-ADPOSTNR-FOM-E-ATTR                
046400                                   MOD-ADPOSTNR-TOM-E-ATTR                
046500       END-IF                                                             
046600     END-IF                                                               
046700     .                                                                    
046800                                                                          
046900 GA-CHECK-NEW-RECORD SECTION.                                             
047000                                                                          
047100     INSPECT MID-IDDISTR-E REPLACING LEADING SPACE BY ZERO                
047200     INSPECT MID-IDKUNDNR-E REPLACING LEADING SPACE BY ZERO               
047300                                                                          
047400                                                                          
047500     IF MID-IDLANDX2-E NOT = ALL '+'                                      
047600        MOVE MID-IDLANDX2-E       TO LAND-IDLANDX2                        
047700        MOVE SPACE                TO LAND-IDLANDX3                        
047800        CALL WISOLAND USING LAND-WISOLAND                                 
047900        IF LAND-KDSVAR = SPACE                                            
048000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLANDX2-E-ATTR               
048100        ELSE                                                              
048200          MOVE NOO                TO INDATA-SW                            
048300          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLANDX2-E-ATTR                  
048400          MOVE YES                TO E-LAND-FEL-SW                        
048500          MOVE YES                TO E-RAD-FEL-SW                         
048600          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
048700        END-IF                                                            
048800     ELSE                                                                 
048900        MOVE NOO                 TO INDATA-SW                             
049000        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLANDX2-E-ATTR                   
049100        MOVE YES                 TO E-LAND-FEL-SW                         
049200        MOVE YES                 TO E-RAD-FEL-SW                          
049300        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
049400     END-IF                                                               
049500                                                                          
049600     IF MID-ADPOSTNR-FOM-E NOT = ALL '+'                                  
049700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPOSTNR-FOM-E-ATTR              
049800     ELSE                                                                 
049810        MOVE NOO                 TO INDATA-SW                             
049820        MOVE MFS-ALFA-FAELT-FEL  TO MOD-ADPOSTNR-FOM-E-ATTR               
049830        MOVE YES                 TO E-ADPOSTNR-FOM-FEL-SW                 
049840        MOVE YES                 TO E-RAD-FEL-SW                          
049850        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
049860     END-IF                                                               
049870                                                                          
049880     IF MID-ADPOSTNR-TOM-E NOT = ALL '+'                                  
049890        MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPOSTNR-TOM-E-ATTR              
049891     ELSE                                                                 
049892        MOVE NOO                 TO INDATA-SW                             
049893        MOVE MFS-ALFA-FAELT-FEL  TO MOD-ADPOSTNR-TOM-E-ATTR               
049894        MOVE YES                 TO E-ADPOSTNR-TOM-FEL-SW                 
049895        MOVE YES                 TO E-RAD-FEL-SW                          
049896        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
049897     END-IF                                                               
049898                                                                          
049899     IF MID-IDDISTR-E NOT = ALL '+'                                       
049900       IF MID-IDDISTR-E NUMERIC                                           
050000         IF MID-IDDISTR-E > 0                                             
050100           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-E-ATTR                 
050200         ELSE                                                             
050300           MOVE NOO                 TO INDATA-SW                          
050400           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-E-ATTR                 
050500           MOVE YES                 TO E-DIST-FEL-SW                      
050600           MOVE YES                 TO E-RAD-FEL-SW                       
050700         END-IF                                                           
050800       ELSE                                                               
050900         MOVE NOO                   TO INDATA-SW                          
051000         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-E-ATTR                 
051100         MOVE ERR-DIST-CUST-MISSING TO MED-IDMFSFEL                       
051200       END-IF                                                             
051300     ELSE                                                                 
051400       MOVE NOO                     TO INDATA-SW                          
051500       MOVE MFS-NUM-FAELT-FEL       TO MOD-IDDISTR-E-ATTR                 
051510       MOVE ERR-DIST-CUST-MISSING   TO MED-IDMFSFEL                       
051520     END-IF                                                               
051530                                                                          
051540     IF MID-IDKUNDNR-E NOT = ALL '+'                                      
051550       IF MID-IDKUNDNR-E NUMERIC                                          
051560         IF MID-IDKUNDNR-E > 0                                            
051570           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-E-ATTR                
051580         ELSE                                                             
051590           MOVE NOO                 TO INDATA-SW                          
051600           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-E-ATTR                
051700           MOVE YES                 TO E-KUND-FEL-SW                      
051710           MOVE YES                 TO E-RAD-FEL-SW                       
051720           MOVE ERR-DIST-CUST-MISSING   TO MED-IDMFSFEL                   
051730         END-IF                                                           
051740       ELSE                                                               
051750         MOVE NOO                   TO INDATA-SW                          
051760         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-E-ATTR                
051770         MOVE ERR-DIST-CUST-MISSING TO MED-IDMFSFEL                       
051780       END-IF                                                             
051790     ELSE                                                                 
051791       MOVE NOO                     TO INDATA-SW                          
051792       MOVE MFS-NUM-FAELT-FEL       TO MOD-IDKUNDNR-E-ATTR                
051793       MOVE ERR-DIST-CUST-MISSING   TO MED-IDMFSFEL                       
051794     END-IF                                                               
051795                                                                          
051796     IF MID-ADPOSTNR-TOM-E NOT = ALL '+'                                  
051797       IF MID-ADPOSTNR-FOM-E NOT = ALL '+'                                
051798           IF MID-ADPOSTNR-TOM-E NOT <  MID-ADPOSTNR-FOM-E                
051799            MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPOSTNR-TOM-E-ATTR          
051800           ELSE                                                           
051900             MOVE NOO TO INDATA-SW                                        
052000             MOVE MFS-NUM-FAELT-FEL   TO MOD-ADPOSTNR-TOM-E-ATTR          
052100             MOVE YES                 TO E-ADPOSTNR-TOM-FEL-SW            
052200             MOVE YES                 TO E-RAD-FEL-SW                     
052300             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
052400           END-IF                                                         
052500       ELSE                                                               
052600         MOVE NOO TO INDATA-SW                                            
052700         MOVE MFS-NUM-FAELT-FEL   TO MOD-ADPOSTNR-FOM-E-ATTR              
052800         MOVE YES                 TO E-ADPOSTNR-FOM-FEL-SW                
052900         MOVE YES                 TO E-RAD-FEL-SW                         
053000         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
053100       END-IF                                                             
053200     ELSE                                                                 
053300       MOVE MFS-NUM-FAELT-FEL   TO MOD-ADPOSTNR-TOM-E-ATTR                
053400       MOVE YES                 TO E-ADPOSTNR-TOM-FEL-SW                  
053500       MOVE YES                 TO E-RAD-FEL-SW                           
053600       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
053700     END-IF                                                               
053800                                                                          
053900     IF INDATA-OK                                                         
054000        MOVE SPAR-IDSYSMOT-KEY       TO W-IDSYSMOT                        
054100        MOVE MID-IDLANDX2-E          TO W-IDLANDX2                        
054200        MOVE MID-ADPOSTNR-FOM-E  TO WS-REDUIN                             
054300        CALL W009REDU USING WS-REDUIN WS-REDUUT                           
054400        MOVE WS-REDUUT           TO W-ADPOSTNR-FOM                        
054500                                                                          
054600        MOVE MID-ADPOSTNR-TOM-E  TO WS-REDUIN                             
054700        CALL W009REDU USING WS-REDUIN WS-REDUUT                           
054800        MOVE WS-REDUUT           TO W-ADPOSTNR-TOM                        
054900                                                                          
055000        PERFORM IMS-GU-WDGX4133                                           
055100        IF SEGMENT-FOUND                                                  
055200           PERFORM IMS-GNP-WDGX4134-KVAL                                  
055300           IF SEGMENT-FOUND                                               
055400             MOVE NOO TO INDATA-SW                                        
055500             MOVE MFS-NUM-FAELT-FEL     TO MOD-CMD-E-ATTR                 
055600             MOVE YES                   TO E-RAD-FEL-SW                   
055700             MOVE ERR-LINE-EXISTS       TO MED-IDMFSFEL                   
055800           END-IF                                                         
055900        END-IF                                                            
056000     END-IF                                                               
056100                                                                          
056200     IF INDATA-OK                                                         
056300       MOVE SPAR-IDSYSMOT-KEY            TO W-IDSYSMOT                    
056400       PERFORM IMS-GU-WDGX4133                                            
056500       IF SEGMENT-FOUND                                                   
056600         MOVE MID-IDLANDX2-E            TO W-IDLANDX2-DEF                 
056700         PERFORM  IMS-GNP-WDGX4134-KVAL-DEF                               
056800         IF SEGMENT-MISSING                                               
056900           IF MID-ADPOSTNR-FOM-E = '9999999999'  AND                      
057000              MID-ADPOSTNR-TOM-E = '9999999999'                           
057100              MOVE NOO                  TO DEFAULT-LINE-SW                
057200           ELSE                                                           
057300             MOVE NOO TO INDATA-SW                                        
057400             MOVE MFS-NUM-FAELT-FEL     TO MOD-ADPOSTNR-FOM-E-ATTR        
057500             MOVE YES                   TO E-ADPOSTNR-FOM-FEL-SW          
057600             MOVE YES                   TO DEFAULT-LINE-SW                
057700           END-IF                                                         
057800         END-IF                                                           
057900       ELSE                                                               
058000         IF MID-ADPOSTNR-FOM-E = '9999999999'  AND                        
058100            MID-ADPOSTNR-TOM-E = '9999999999'                             
058200            MOVE NOO                  TO DEFAULT-LINE-SW                  
058300         ELSE                                                             
058400           MOVE NOO TO INDATA-SW                                          
058500           MOVE YES                   TO E-ADPOSTNR-FOM-FEL-SW            
058600           MOVE MFS-NUM-FAELT-FEL     TO MOD-ADPOSTNR-FOM-E-ATTR          
058700           MOVE YES                   TO DEFAULT-LINE-SW                  
058800         END-IF                                                           
058900       END-IF                                                             
059000     END-IF                                                               
059010                                                                          
059020     IF INDATA-OK                                                         
059030       IF MID-IDDISTR-E   NOT = ALL '+' AND                               
059040          MID-IDKUNDNR-E  NOT = ALL '+'                                   
059050         MOVE MID-IDDISTR-E           TO W-IDDISTR                        
059060         MOVE MID-IDKUNDNR-E          TO W-IDKUNDNR                       
059070         PERFORM IMS-GU-WDB201                                            
059080         IF SEGMENT-MISSING                                               
059090           MOVE NOO TO INDATA-SW                                          
059100           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-E-ATTR               
059200                                         MOD-IDKUNDNR-E-ATTR              
059300           MOVE YES                   TO E-DIST-FEL-SW                    
059400           MOVE YES                   TO E-KUND-FEL-SW                    
059500           MOVE YES                   TO E-RAD-FEL-SW                     
059600           MOVE ERR-DIST-CUST-DB      TO MED-IDMFSFEL                     
059700         ELSE                                                             
059800           IF GMT-IDDC-DAY (1) = SPACE                                    
059900             MOVE NOO TO INDATA-SW                                        
060000             MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-E-ATTR             
060100                                           MOD-IDKUNDNR-E-ATTR            
060200             MOVE YES                   TO E-DIST-FEL-SW                  
060300             MOVE YES                   TO E-KUND-FEL-SW                  
060400             MOVE YES                   TO E-RAD-FEL-SW                   
060500             MOVE ERR-DC-MISSING        TO MED-IDMFSFEL                   
060600           ELSE                                                           
060700             IF GMT-IDLANDX2 NOT = MID-IDLANDX2-E                         
060710               MOVE NOO TO INDATA-SW                                      
060720               MOVE MFS-NUM-FAELT-FEL     TO MOD-IDLANDX2-E-ATTR          
060730               MOVE YES                   TO E-LAND-FEL-SW                
060740               MOVE YES                   TO E-RAD-FEL-SW                 
060750               MOVE YES                   TO COUNTRY-SW                   
060760             ELSE                                                         
060770               MOVE NOO                   TO COUNTRY-SW                   
060780               MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDLANDX2-E-ATTR          
060781             END-IF                                                       
060782           END-IF                                                         
060783         END-IF                                                           
060784       END-IF                                                             
060785     END-IF                                                               
060786                                                                          
060787     IF INDATA-OK                                                         
060788       MOVE SPAR-IDSYSMOT-KEY         TO W-IDSYSMOT                       
060789       PERFORM IMS-GU-WDGX4133                                            
060790       IF SEGMENT-FOUND                                                   
060800         MOVE MID-IDLANDX2-E             TO W-IDLANDX2-MIN                
060900                                            W-IDLANDX2-MAX                
061000         MOVE MID-ADPOSTNR-FOM-E         TO WS-REDUIN                     
061100         CALL W009REDU USING WS-REDUIN WS-REDUUT                          
061200         MOVE WS-REDUUT                  TO WS-ADPOSTNR-FOM               
061300                                                                          
061400         MOVE MID-ADPOSTNR-TOM-E         TO WS-REDUIN                     
061500         CALL W009REDU USING WS-REDUIN WS-REDUUT                          
061600         MOVE WS-REDUUT                  TO WS-ADPOSTNR-TOM               
061700                                                                          
061800         PERFORM  IMS-GNP-WDGX4134-MIN-MAX                                
061900                                                                          
062000         PERFORM UNTIL SEGMENT-MISSING OR                                 
062100                       INDATA-FEL                                         
062200           IF SEGMENT-FOUND                                               
062210             IF MID-ADPOSTNR-FOM-E = '9999999999' AND                     
062220                MID-ADPOSTNR-TOM-E = '9999999999'                         
062230                CONTINUE                                                  
062240             ELSE                                                         
062250                IF MID-ADPOSTNR-FOM-E NOT = MID-ADPOSTNR-TOM-E            
062260                 IF (4134-ADPOSTNR-FOM >= WS-ADPOSTNR-FOM AND             
062270                     4134-ADPOSTNR-FOM <= WS-ADPOSTNR-TOM) OR             
062280                    (4134-ADPOSTNR-TOM >= WS-ADPOSTNR-FOM AND             
062290                     4134-ADPOSTNR-TOM <= WS-ADPOSTNR-TOM)                
062300                   MOVE NOO TO INDATA-SW                                  
062400                   MOVE MFS-NUM-FAELT-FEL TO                              
062500                        MOD-ADPOSTNR-FOM-E-ATTR                           
062600                        MOD-ADPOSTNR-TOM-E-ATTR                           
062700                   MOVE YES               TO E-ADPOSTNR-FOM-FEL-SW        
062800                   MOVE YES               TO E-ADPOSTNR-TOM-FEL-SW        
062900                   MOVE YES                TO E-RAD-FEL-SW                
063000                   MOVE ERR-WRONG-INTERVAL TO MED-IDMFSFEL                
063100                 END-IF                                                   
063200                END-IF                                                    
063300             END-IF                                                       
063400             PERFORM IMS-GNP-WDGX4134-MIN-MAX                             
063500           END-IF                                                         
063600         END-PERFORM                                                      
063700       END-IF                                                             
063800     END-IF                                                               
063900                                                                          
064000     IF INDATA-FEL                                                        
064100        IF COUNTRY-WRONG                                                  
064200          MOVE 'COUNTRY WRONG'            TO MOD-TEMFSFEL                 
064300        ELSE                                                              
064400          IF DEFAULT-LINE-MISSING                                         
064500             MOVE 'DEFAULT LINE MISSING ' TO MOD-TEMFSFEL                 
064600          ELSE                                                            
064700            CALL WMEDKONV USING MED-WMEDAREA                              
064800            MOVE MED-MFSFEL    TO MOD-TEMFSFEL                            
064900          END-IF                                                          
065000        END-IF                                                            
065100        MOVE NOO             TO ALLT-SW                                   
065200     END-IF                                                               
065300     .                                                                    
065400                                                                          
065500                                                                          
065600 GB-CHECK-CHANGE SECTION.                                                 
065700                                                                          
065800                                                                          
065900     IF MID-IDLANDX2-E NOT = ALL '+'                                      
066000       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-IDLANDX2-E-ATTR              
066100     ELSE                                                                 
066200       MOVE NOO                 TO INDATA-SW                              
066300       MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLANDX2-E-ATTR                    
066400       MOVE YES                 TO E-LAND-FEL-SW                          
066500       MOVE YES                 TO E-RAD-FEL-SW                           
066600     END-IF                                                               
066700                                                                          
066800     IF MID-ADPOSTNR-FOM-E NOT = ALL '+'                                  
066900       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-ADPOSTNR-FOM-E-ATTR          
067000     ELSE                                                                 
067100       MOVE NOO                 TO INDATA-SW                              
067200       MOVE MFS-ALFA-FAELT-FEL  TO MOD-ADPOSTNR-FOM-E-ATTR                
067300       MOVE YES                 TO E-ADPOSTNR-FOM-FEL-SW                  
067400       MOVE YES                 TO E-RAD-FEL-SW                           
067500     END-IF                                                               
067600                                                                          
067700     IF MID-ADPOSTNR-TOM-E NOT = ALL '+'                                  
067710       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-ADPOSTNR-TOM-E-ATTR          
067720     ELSE                                                                 
067721       MOVE NOO                 TO INDATA-SW                              
067722       MOVE MFS-ALFA-FAELT-FEL  TO MOD-ADPOSTNR-TOM-E-ATTR                
067723       MOVE YES                 TO E-ADPOSTNR-TOM-FEL-SW                  
067724       MOVE YES                 TO E-RAD-FEL-SW                           
067725     END-IF                                                               
067726                                                                          
067727     IF INDATA-OK                                                         
067728       IF MID-IDDISTR-E NOT = ALL '+'                                     
067729         INSPECT MID-IDDISTR-E REPLACING LEADING SPACE BY ZERO            
067730         IF MID-IDDISTR-E NUMERIC                                         
067740           IF MID-IDDISTR-E > 0                                           
067750             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-E-ATTR               
067760           ELSE                                                           
067770             MOVE NOO                 TO INDATA-SW                        
067780             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-E-ATTR               
067790             MOVE YES                 TO E-DIST-FEL-SW                    
067791             MOVE YES                 TO E-RAD-FEL-SW                     
067792           END-IF                                                         
067793         ELSE                                                             
067794           MOVE NOO                   TO INDATA-SW                        
067795           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-E-ATTR               
067796           MOVE ERR-DIST-CUST-MISSING TO MED-IDMFSFEL                     
067797         END-IF                                                           
067798       ELSE                                                               
067799         MOVE NOO                     TO INDATA-SW                        
067800         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDDISTR-E-ATTR               
067801         MOVE ERR-DIST-CUST-MISSING   TO MED-IDMFSFEL                     
067802       END-IF                                                             
067803     END-IF                                                               
067804                                                                          
067805       IF MID-IDKUNDNR-E NOT = ALL '+'                                    
067806         INSPECT MID-IDKUNDNR-E REPLACING LEADING SPACE BY ZERO           
067807         IF MID-IDKUNDNR-E NUMERIC                                        
067808           IF MID-IDKUNDNR-E > 0                                          
067809             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-E-ATTR              
067810           ELSE                                                           
067811             MOVE NOO                 TO INDATA-SW                        
067812             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-E-ATTR              
067813             MOVE YES                 TO E-KUND-FEL-SW                    
067814             MOVE YES                 TO E-RAD-FEL-SW                     
067815             MOVE ERR-DIST-CUST-MISSING   TO MED-IDMFSFEL                 
067816           END-IF                                                         
067817         ELSE                                                             
067818           MOVE NOO                   TO INDATA-SW                        
067819           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-E-ATTR              
067820           MOVE ERR-DIST-CUST-MISSING TO MED-IDMFSFEL                     
067821         END-IF                                                           
067822       ELSE                                                               
067823         MOVE NOO                     TO INDATA-SW                        
067824         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDKUNDNR-E-ATTR              
067825         MOVE ERR-DIST-CUST-MISSING   TO MED-IDMFSFEL                     
067826       END-IF                                                             
067827                                                                          
067828     IF INDATA-OK                                                         
067829       MOVE SPAR-IDSYSMOT-KEY   TO W-IDSYSMOT                             
067830       MOVE MID-IDLANDX2-E      TO W-IDLANDX2                             
067831       MOVE MID-ADPOSTNR-FOM-E  TO WS-REDUIN                              
067832       CALL W009REDU USING WS-REDUIN WS-REDUUT                            
067833       MOVE WS-REDUUT           TO W-ADPOSTNR-FOM                         
067834                                                                          
067835       MOVE MID-ADPOSTNR-TOM-E  TO WS-REDUIN                              
067836       CALL W009REDU USING WS-REDUIN WS-REDUUT                            
067837       MOVE WS-REDUUT           TO W-ADPOSTNR-TOM                         
067838                                                                          
067839       PERFORM IMS-GU-WDGX4133                                            
067840       IF SEGMENT-FOUND                                                   
067841          PERFORM IMS-GNP-WDGX4134-KVAL                                   
067842          IF SEGMENT-MISSING                                              
067843            MOVE ERR-MISSING-REG  TO MED-IDMFSFEL                         
067844            CALL WMEDKONV USING MED-WMEDAREA                              
067845            MOVE MED-MFSFEL       TO MOD-TEMFSFEL                         
067846            MOVE NOO              TO INDATA-SW                            
067847            MOVE YES              TO E-LAND-FEL-SW                        
067848            PERFORM MFS-ERASE-FIELD-OUT                                   
067849          ELSE                                                            
067850            IF 4134-IDDISTR  = MID-IDDISTR-E AND                          
067851               4134-IDKUNDNR = MID-IDKUNDNR-E                             
067852               MOVE ERR-NOTHING-CHANGED  TO MED-IDMFSFEL                  
067853               CALL WMEDKONV USING MED-WMEDAREA                           
067854               MOVE MED-MFSFEL       TO MOD-TEMFSFEL                      
067855               MOVE YES              TO E-DIST-FEL-SW                     
067856               MOVE NOO              TO INDATA-SW                         
067857               MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-E-ATTR           
067858                                             MOD-IDKUNDNR-E-ATTR          
067859            END-IF                                                        
067860          END-IF                                                          
067861       ELSE                                                               
067862         IF SEGMENT-MISSING                                               
067863            MOVE ERR-MISSING-REG  TO MED-IDMFSFEL                         
067864            CALL WMEDKONV USING MED-WMEDAREA                              
067865            MOVE MED-MFSFEL       TO MOD-TEMFSFEL                         
067866            MOVE NOO              TO INDATA-SW                            
067867            PERFORM MFS-ERASE-FIELD-OUT                                   
067868         END-IF                                                           
067869       END-IF                                                             
067870     END-IF                                                               
067871                                                                          
067872     IF INDATA-OK                                                         
067873       IF MID-IDDISTR-E   NOT = ALL '+' AND                               
067874          MID-IDKUNDNR-E  NOT = ALL '+'                                   
067875         MOVE MID-IDDISTR-E           TO W-IDDISTR                        
067876         MOVE MID-IDKUNDNR-E          TO W-IDKUNDNR                       
067877         PERFORM IMS-GU-WDB201                                            
067878         IF SEGMENT-MISSING                                               
067879           MOVE NOO TO INDATA-SW                                          
067880           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-E-ATTR               
067881                                         MOD-IDKUNDNR-E-ATTR              
067882           MOVE YES                   TO E-DIST-FEL-SW                    
067883           MOVE YES                   TO E-KUND-FEL-SW                    
067884           MOVE YES                   TO E-RAD-FEL-SW                     
067885           MOVE ERR-DIST-CUST-DB      TO MED-IDMFSFEL                     
067886         ELSE                                                             
067887           IF GMT-IDDC-DAY (1) = SPACE                                    
067888             MOVE NOO TO INDATA-SW                                        
067889             MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-E-ATTR             
067890                                           MOD-IDKUNDNR-E-ATTR            
067891             MOVE YES                   TO E-DIST-FEL-SW                  
067892             MOVE YES                   TO E-KUND-FEL-SW                  
067893             MOVE YES                   TO E-RAD-FEL-SW                   
067894             MOVE ERR-DC-MISSING        TO MED-IDMFSFEL                   
067895           ELSE                                                           
067896             IF GMT-IDLANDX2 NOT = MID-IDLANDX2-E                         
067897               MOVE NOO TO INDATA-SW                                      
067898               MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-E-ATTR            
067899                                             MOD-IDKUNDNR-E-ATTR          
067900               MOVE YES                   TO E-DIST-FEL-SW                
067901               MOVE YES                   TO E-KUND-FEL-SW                
067902               MOVE YES                   TO E-RAD-FEL-SW                 
067903               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
067904             ELSE                                                         
067905               MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDDISTR-E-ATTR           
067906                                             MOD-IDKUNDNR-E-ATTR          
067907             END-IF                                                       
067908           END-IF                                                         
067909         END-IF                                                           
067910       END-IF                                                             
067911     END-IF                                                               
067912                                                                          
067913                                                                          
067914     .                                                                    
067915     EJECT                                                                
067916 H-UPDATE  SECTION.                                                       
067917                                                                          
067918     IF INDATA-OK                                                         
067919                                                                          
067920        IF MID-CMD-E = 'N'                                                
067930           PERFORM HA-CREATE-NEW-RECORD                                   
067940        ELSE                                                              
067950           PERFORM HB-UPPDATE-RECORD                                      
067960        END-IF                                                            
067970                                                                          
067980        PERFORM HC-DELETE-RECORD                                          
067990     END-IF                                                               
068000                                                                          
068100*---FÖR ATT POSITIONERA SIG VID LÄSNING AV 1:A POST                       
068200     IF SPAR-IDTRANS = '0815'                                             
068300         MOVE SPAR-IDSYSMOT-KEY   TO W-IDSYSMOT                           
068400         MOVE SPAR-IDLANDX2-ENTER TO W-IDLANDX2                           
068500         MOVE SPAR-ADPOSTNR-FOM-ENTER TO W-ADPOSTNR-FOM                   
068600         MOVE SPAR-ADPOSTNR-TOM-ENTER TO W-ADPOSTNR-TOM                   
068700     END-IF                                                               
068800                                                                          
068900     MOVE INF-UPDATE-DONE         TO MED-IDMFSINF                         
069000     CALL WMEDKONV USING MED-WMEDAREA                                     
069100     MOVE MED-MFSINF              TO MOD-TEMFSINF                         
069200     PERFORM MFS-FORM-ATTR                                                
069300     PERFORM MFS-ERASE-FIELD-IN                                           
069400     .                                                                    
069500                                                                          
069510                                                                          
069520 HA-CREATE-NEW-RECORD SECTION.                                            
069521                                                                          
069522     MOVE SPAR-IDSYSMOT-KEY    TO W-IDSYSMOT                              
069523     PERFORM IMS-GU-WDGX4133                                              
069524     IF SEGMENT-MISSING                                                   
069525       MOVE '4133'             TO 4133-IDHTYP                             
069526       MOVE SPAR-IDSYSMOT-KEY  TO 4133-IDSYSMOT                           
069527       PERFORM IMS-ISRT-WDGX4133                                          
069528     END-IF                                                               
069529                                                                          
069530     MOVE MID-IDLANDX2-E       TO 4134-IDLANDX2                           
069531                                                                          
069532     MOVE MID-ADPOSTNR-FOM-E   TO WS-REDUIN                               
069533     CALL W009REDU USING WS-REDUIN WS-REDUUT                              
069534     MOVE WS-REDUUT            TO 4134-ADPOSTNR-FOM                       
069535                                                                          
069536     MOVE MID-ADPOSTNR-TOM-E   TO WS-REDUIN                               
069537     CALL W009REDU USING WS-REDUIN WS-REDUUT                              
069538     MOVE WS-REDUUT            TO 4134-ADPOSTNR-TOM                       
069539                                                                          
069540     MOVE MID-IDDISTR-E        TO 4134-IDDISTR                            
069541     MOVE MID-IDKUNDNR-E       TO 4134-IDKUNDNR                           
069542                                                                          
069543                                                                          
069544     PERFORM IMS-ISRT-WDGX4134                                            
069545     .                                                                    
069546                                                                          
069547                                                                          
069548 HB-UPPDATE-RECORD SECTION.                                               
069549                                                                          
069550     MOVE SPAR-IDSYSMOT-KEY TO W-IDSYSMOT                                 
069551     PERFORM IMS-GU-WDGX4133                                              
069552     IF SEGMENT-FOUND                                                     
069553        MOVE MID-IDLANDX2-E     TO W-IDLANDX2                             
069554        MOVE MID-ADPOSTNR-FOM-E  TO WS-REDUIN                             
069555        CALL W009REDU USING WS-REDUIN WS-REDUUT                           
069556        MOVE WS-REDUUT           TO W-ADPOSTNR-FOM                        
069557                                                                          
069558       MOVE MID-ADPOSTNR-TOM-E  TO WS-REDUIN                              
069559       CALL W009REDU USING WS-REDUIN WS-REDUUT                            
069560       MOVE WS-REDUUT           TO W-ADPOSTNR-TOM                         
069570        PERFORM IMS-GHNP-WDGX4134                                         
069571        IF SEGMENT-FOUND                                                  
069572           MOVE MID-IDDISTR-E   TO 4134-IDDISTR                           
069573           MOVE MID-IDKUNDNR-E  TO 4134-IDKUNDNR                          
069574           PERFORM IMS-REPL-WDGX4134                                      
069575        END-IF                                                            
069576     END-IF                                                               
069577     .                                                                    
069578                                                                          
069579                                                                          
069580 HC-DELETE-RECORD  SECTION.                                               
069590                                                                          
069591     MOVE +1 TO INDX                                                      
069592     PERFORM UNTIL INDX > MAX-INDX                                        
069593       IF MID-CMD (INDX)  = 'D'                                           
069594         MOVE SPAR-IDSYSMOT-KEY       TO W-IDSYSMOT                       
069595         MOVE MID-IDLANDX2     (INDX) TO W-IDLANDX2                       
069596         MOVE MID-ADPOSTNR-FOM (INDX) TO W-ADPOSTNR-FOM                   
069597         MOVE MID-ADPOSTNR-TOM (INDX) TO W-ADPOSTNR-TOM                   
069598         PERFORM IMS-GU-WDGX4133                                          
069599         IF SEGMENT-FOUND                                                 
069600           PERFORM IMS-GHNP-WDGX4134                                      
069601           IF SEGMENT-FOUND                                               
069602             PERFORM IMS-DLET-WDGX4134                                    
069603           END-IF                                                         
069604         END-IF                                                           
069605       END-IF                                                             
069606       ADD +1 TO INDX                                                     
069607     END-PERFORM                                                          
069608     .                                                                    
069609                                                                          
069610                                                                          
069611 S01-E-LINE-TO-MOD  SECTION.                                              
069612                                                                          
069613     IF MID-CMD-E NOT = ALL '+'                                           
069614       MOVE MID-CMD-E            TO MOD-CMD-E                             
069615     END-IF                                                               
069616     IF MID-IDLANDX2-E NOT = ALL '+'                                      
069617       MOVE MID-IDLANDX2-E       TO MOD-IDLANDX2-E                        
069618     END-IF                                                               
069619     IF MID-ADPOSTNR-FOM-E NOT = ALL '+'                                  
069620       MOVE MID-ADPOSTNR-FOM-E   TO MOD-ADPOSTNR-FOM-E                    
069621     END-IF                                                               
069622     IF MID-ADPOSTNR-TOM-E NOT = ALL '+'                                  
069623       MOVE MID-ADPOSTNR-TOM-E   TO MOD-ADPOSTNR-TOM-E                    
069624     END-IF                                                               
069625                                                                          
069626     IF MID-IDDISTR-E NOT = ALL '+'                                       
069627       INSPECT MID-IDDISTR-E     REPLACING LEADING SPACE BY ZERO          
069628       INSPECT MID-IDDISTR-E     REPLACING LEADING '+'   BY ZERO          
069629       MOVE MID-IDDISTR-E        TO MOD-IDDISTR-E                         
069630     END-IF                                                               
069631                                                                          
069632     IF MID-IDKUNDNR-E NOT = ALL '+'                                      
069633       INSPECT MID-IDKUNDNR-E     REPLACING LEADING SPACE BY ZERO         
069634       INSPECT MID-IDKUNDNR-E     REPLACING LEADING '+'   BY ZERO         
069635       MOVE MID-IDKUNDNR-E       TO MOD-IDKUNDNR-E                        
069636     END-IF                                                               
069637                                                                          
069638                                                                          
069639     IF E-LAND-FEL                                                        
069640        MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDLANDX2-E-ATTR                 
069650     END-IF                                                               
069660     IF E-ADPOSTNR-FOM-FEL                                                
069661        MOVE MFS-ALFA-FAELT-FEL    TO MOD-ADPOSTNR-FOM-E-ATTR             
069662     END-IF                                                               
069663     IF E-ADPOSTNR-TOM-FEL                                                
069664        MOVE MFS-ALFA-FAELT-FEL    TO MOD-ADPOSTNR-TOM-E-ATTR             
069665     END-IF                                                               
069666     IF E-DIST-FEL                                                        
069667        MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-E-ATTR                  
069668     END-IF                                                               
069669     IF E-KUND-FEL                                                        
069670        MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-E-ATTR                 
069680     END-IF                                                               
069681     .                                                                    
069682                                                                          
069683                                                                          
069684 MFS-ERASE-FIELD-OUT SECTION.                                             
069685                                                                          
069686*    --- ALLA UTDATA-FÄLT                                                 
069687*    --- INCL. SCROLL KEYS                                                
069688     MOVE MFS-ERASE-FIELD    TO MOD-CMD-E                                 
069689                                MOD-IDLANDX2-E                            
069690                                MOD-ADPOSTNR-FOM-E                        
069700                                MOD-ADPOSTNR-TOM-E                        
069800                                MOD-IDDISTR-E                             
069900                                MOD-IDKUNDNR-E                            
070000                                                                          
070010     MOVE +1 TO MFS-INDX                                                  
070020     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
070030       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
070040       ADD 1 TO MFS-INDX                                                  
070050     END-PERFORM                                                          
070060     .                                                                    
070070     SKIP3                                                                
070080 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
070090                                                                          
070100*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
070200     MOVE MFS-ERASE-FIELD TO MOD-CMD (MFS-INDX)                           
070300                             MOD-IDLANDX2 (MFS-INDX)                      
070400                             MOD-ADPOSTNR-FOM (MFS-INDX)                  
070500                             MOD-ADPOSTNR-TOM (MFS-INDX)                  
070600                             MOD-IDDISTR  (MFS-INDX)                      
070700                             MOD-IDKUNDNR (MFS-INDX)                      
070800                             MOD-IDDC     (MFS-INDX)                      
070900     .                                                                    
071000     SKIP3                                                                
071100 MFS-ERASE-FIELD-IN SECTION.                                              
071200                                                                          
071300*    --- ALLA INDATA-FÄLT                                                 
071400     MOVE MFS-ERASE-FIELD    TO MOD-CMD-E                                 
071410                                MOD-IDLANDX2-E                            
071420                                MOD-ADPOSTNR-FOM-E                        
071430                                MOD-ADPOSTNR-TOM-E                        
071440                                MOD-IDDISTR-E                             
071450                                MOD-IDKUNDNR-E                            
071460     MOVE +1 TO MFS-INDX                                                  
071470     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
071480       MOVE MFS-ERASE-FIELD TO MOD-CMD (MFS-INDX)                         
071490       ADD +1 TO MFS-INDX                                                 
071500     END-PERFORM                                                          
071600     .                                                                    
071700     EJECT                                                                
071800 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
071900                                                                          
072000     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDSYSMOT-IN                       
072100                                    MOD-IDSYSMOT-UT                       
072200                                    MOD-IDDISTR-IN                        
072201                                    MOD-IDDISTR-UT                        
072202                                    MOD-CMD-E                             
072203                                    MOD-IDLANDX2-E                        
072204                                    MOD-ADPOSTNR-FOM-E                    
072205                                    MOD-ADPOSTNR-TOM-E                    
072206                                    MOD-IDDISTR-E                         
072207                                    MOD-IDKUNDNR-E                        
072208     MOVE +1 TO MFS-INDX                                                  
072209     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
072210       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
072220       ADD +1 TO MFS-INDX                                                 
072230     END-PERFORM                                                          
072240     .                                                                    
072250     SKIP2                                                                
072260 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
072270                                                                          
072280*    --- OUTDATA FIELD ON SCROLL KEYS                                     
072290     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-CMD (MFS-INDX)                    
072300                                    MOD-IDLANDX2 (MFS-INDX)               
072400                                    MOD-ADPOSTNR-FOM (MFS-INDX)           
072500                                    MOD-ADPOSTNR-TOM (MFS-INDX)           
072600                                    MOD-IDDISTR      (MFS-INDX)           
072700                                    MOD-IDKUNDNR     (MFS-INDX)           
072800                                    MOD-IDDC         (MFS-INDX)           
072900                                                                          
073000     .                                                                    
073100     SKIP3                                                                
073200 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
073300                                                                          
073400     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDSYSMOT-IN                       
073500                                    MOD-IDSYSMOT-UT                       
073600                                    MOD-IDDISTR-IN                        
073700                                    MOD-IDDISTR-UT                        
073800                                                                          
073900     MOVE +1 TO MFS-INDX                                                  
074000     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
074010        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-CMD (MFS-INDX)                 
074020        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDLANDX2 (MFS-INDX)            
074030        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ADPOSTNR-FOM (MFS-INDX)        
074031        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ADPOSTNR-TOM (MFS-INDX)        
074032       ADD +1 TO MFS-INDX                                                 
074033     END-PERFORM                                                          
074034     .                                                                    
074035     EJECT                                                                
074036 MFS-DONT-TOUCH-FIELD-IN-E SECTION.                                       
074037     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-CMD-E                             
074038                                    MOD-IDLANDX2-E                        
074039                                    MOD-ADPOSTNR-FOM-E                    
074040                                    MOD-ADPOSTNR-TOM-E                    
074050                                    MOD-IDDISTR-E                         
074060                                    MOD-IDKUNDNR-E                        
074070     .                                                                    
074080     EJECT                                                                
074090 MFS-FORM-ATTR SECTION.                                                   
074100                                                                          
074200*    --- ALL INDATA-FIELDS                                                
074300*    MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-IDSYSMOT-IN                      
074400*    MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-IDSYSMOT-UT                      
074500*    MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-IDDISTR-IN                       
074600*    MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-IDDISTR-UT                       
074700                                                                          
074800     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-CMD-E-ATTR                       
074900                                     MOD-IDLANDX2-E-ATTR                  
075000                                     MOD-ADPOSTNR-FOM-E-ATTR              
075010                                     MOD-ADPOSTNR-TOM-E-ATTR              
075020                                     MOD-IDDISTR-E-ATTR                   
075030                                     MOD-IDKUNDNR-E-ATTR                  
075040                                                                          
075050     MOVE +1 TO MFS-INDX                                                  
075060     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
075070        MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-CMD-ATTR (MFS-INDX)           
075080       ADD +1 TO MFS-INDX                                                 
075090     END-PERFORM                                                          
075100     .                                                                    
075200     SKIP2                                                                
075300 MFS-READ-IN-AGAIN SECTION.                                               
075400                                                                          
075500     MOVE +1 TO MFS-INDX                                                  
075600     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
075700         MOVE MFS-ADD-READ-FIELD     TO MOD-CMD-ATTR (MFS-INDX)           
075710       ADD +1 TO MFS-INDX                                                 
075720     END-PERFORM                                                          
075730     .                                                                    
075740     EJECT                                                                
075750 MFS-READ-IN-AGAIN-E SECTION.                                             
075760                                                                          
075770     MOVE MFS-ADD-READ-FIELD         TO MOD-CMD-E-ATTR                    
075780                                        MOD-IDLANDX2-E-ATTR               
075790                                        MOD-ADPOSTNR-FOM-E-ATTR           
075800                                        MOD-ADPOSTNR-TOM-E-ATTR           
075900                                        MOD-IDDISTR-E-ATTR                
075910                                        MOD-IDKUNDNR-E-ATTR               
075920     .                                                                    
075930     EJECT                                                                
075940* --- IMS SECTIONS ---                                                    
075950     SKIP3                                                                
075960 IMS-GU-WDGX4133 SECTION.                                                 
075970     MOVE 'IMS-GU-WDGX4133'    TO CURRENT-IMS-SECTION                     
075980                                                                          
075990     STRING 'WDR501  (WDGXKEY  =' W-WDGX4133-X ')'                        
076000          DELIMITED BY SIZE INTO SSA1                                     
076100     MOVE '  GE'                TO GOOD-STATUSCODES                       
076110     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDR501 SSA1                    
076120     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
076130     PERFORM IMS-STATUSCHECK                                              
076140     .                                                                    
076150     SKIP2                                                                
076160 IMS-GNP-WDGX4134-OKVAL SECTION.                                          
076170     MOVE 'IMS-GNP-WDGX4134-OK' TO CURRENT-IMS-SECTION                    
076180                                                                          
076190     MOVE 'WDGX4134'            TO SSA1                                   
076191     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
076192     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX4134 SSA1                 
076193     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
076194     PERFORM IMS-STATUSCHECK                                              
076195     .                                                                    
076196     SKIP2                                                                
076197 IMS-GHNP-WDGX4134        SECTION.                                        
076198     MOVE 'IMS-GHNP-WDGX4134'   TO CURRENT-IMS-SECTION                    
076199                                                                          
076200     STRING 'WDGX4134(KY4134   =' W-WDGX4134-X ')'                        
076201          DELIMITED BY SIZE INTO SSA1                                     
076202     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
076203     CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-WDGX4134 SSA1                
076204     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
076205     PERFORM IMS-STATUSCHECK                                              
076206     .                                                                    
076207     SKIP2                                                                
076208 IMS-GNP-WDGX4134-KVAL   SECTION.                                         
076209     MOVE 'IMS-GNP-WDGX4134-K' TO CURRENT-IMS-SECTION                     
076210                                                                          
076211     STRING 'WDGX4134(KY4134   =' W-WDGX4134-X ')'                        
076212          DELIMITED BY SIZE INTO SSA1                                     
076213     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
076214     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX4134 SSA1                 
076215     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
076216     PERFORM IMS-STATUSCHECK                                              
076217     .                                                                    
076218     SKIP2                                                                
076219 IMS-GNP-WDGX4134-KVAL-DEF SECTION.                                       
076220     MOVE 'IMS-GNP-WDGX4134-D' TO CURRENT-IMS-SECTION                     
076221                                                                          
076222     STRING 'WDGX4134(KY4134   =' W-WDGX4134-DEF-X ')'                    
076223          DELIMITED BY SIZE INTO SSA1                                     
076224     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
076225     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX4134 SSA1                 
076226     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
076227     PERFORM IMS-STATUSCHECK                                              
076228     .                                                                    
076229     SKIP2                                                                
076230 IMS-GNP-WDGX4134        SECTION.                                         
076231     MOVE 'IMS-GNP-WDGX4134'   TO CURRENT-IMS-SECTION                     
076232                                                                          
076233     STRING 'WDGX4134(KY4134  >=' W-WDGX4134-X ')'                        
076234          DELIMITED BY SIZE INTO SSA1                                     
076235     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
076236     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX4134 SSA1                 
076237     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
076238     PERFORM IMS-STATUSCHECK                                              
076239     .                                                                    
076240     SKIP2                                                                
076241 IMS-GNP-WDGX4134-MIN-MAX-DISTR SECTION.                                  
076242                                                                          
076243     STRING 'WDGX4134(KY4134  >=' W-WDGX4134-MIN-X                        
076244                    '&KY4134  <=' W-WDGX4134-MAX-X                        
076245                    '&IDDISTR  =' W-IDDISTR-X ')'                         
076246            DELIMITED BY SIZE INTO SSA1                                   
076247     MOVE '  GE'               TO GOOD-STATUSCODES                        
076248     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX4134 SSA1                 
076249     MOVE WDR5-STATUS-CODE     TO STATUS-WS                               
076250     PERFORM IMS-STATUSCHECK                                              
076260     .                                                                    
076261     SKIP2                                                                
076262 IMS-GNP-WDGX4134-MIN-MAX   SECTION.                                      
076263                                                                          
076264     STRING 'WDGX4134(KY4134  >=' W-WDGX4134-MIN-X                        
076265                    '&KY4134  <=' W-WDGX4134-MAX-X ')'                    
076266            DELIMITED BY SIZE INTO SSA1                                   
076267     MOVE '  GE'               TO GOOD-STATUSCODES                        
076268     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX4134 SSA1                 
076269     MOVE WDR5-STATUS-CODE     TO STATUS-WS                               
076270     PERFORM IMS-STATUSCHECK                                              
076271     .                                                                    
076272     SKIP2                                                                
076273 IMS-GNP-WDGX4134M       SECTION.                                         
076274     MOVE 'IMS-GNP-WDGX4134M'  TO CURRENT-IMS-SECTION                     
076275                                                                          
076276     STRING 'WDGX4134(KY4134   =' W-WDGX4134-M-X ')'                      
076277          DELIMITED BY SIZE INTO SSA1                                     
076278     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
076279     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX4134 SSA1                 
076280     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
076281     PERFORM IMS-STATUSCHECK                                              
076282     .                                                                    
076283     SKIP2                                                                
076284 IMS-ISRT-WDGX4133    SECTION.                                            
076285     MOVE 'IMS-ISRT-WDGX4133   '  TO CURRENT-IMS-SECTION                  
076286                                                                          
076287     STRING 'WDR501     '                                                 
076288            DELIMITED BY SIZE INTO SSA1                                   
076289     MOVE '  '                  TO GOOD-STATUSCODES                       
076290     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDR501 SSA1                  
076291     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
076292     PERFORM IMS-STATUSCHECK                                              
076293     .                                                                    
076294     EJECT                                                                
076295 IMS-ISRT-WDGX4134 SECTION.                                               
076296     MOVE 'IMS-ISRT-WDGX4134  '  TO CURRENT-IMS-SECTION                   
076297                                                                          
076298     STRING 'WDR501  (WDGXKEY  =' W-WDGX4133-X ')'                        
076299            DELIMITED BY SIZE INTO SSA1                                   
076300     MOVE 'WDGX4134'            TO SSA2                                   
076301     MOVE '  II'                TO GOOD-STATUSCODES                       
076302     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX4134 SSA1 SSA2           
076303     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
076304     PERFORM IMS-STATUSCHECK                                              
076305     .                                                                    
076306     SKIP3                                                                
076307 IMS-DLET-WDGX4134      SECTION.                                          
076308     MOVE 'IMS-DLET' TO  CURRENT-IMS-SECTION                              
076309                                                                          
076310     MOVE '    '           TO GOOD-STATUSCODES                            
076311     CALL CBLTDLI USING DLET WDR5-PCB DLI-IO-WDGX4134                     
076312     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
076313     PERFORM IMS-STATUSCHECK                                              
076314     .                                                                    
076315 IMS-REPL-WDGX4134      SECTION.                                          
076316     MOVE 'IMS-REPL' TO  CURRENT-IMS-SECTION                              
076317                                                                          
076318     MOVE '    '           TO GOOD-STATUSCODES                            
076319     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDGX4134                     
076320     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
076321     PERFORM IMS-STATUSCHECK                                              
076322     .                                                                    
076323 IMS-GU-WDB201 SECTION.                                                   
076324     MOVE 'IMS-GU-WDB201   ' TO CURRENT-IMS-SECTION                       
076325                                                                          
076326     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
076327         DELIMITED BY SIZE INTO SSA1                                      
076328     MOVE '  GE'             TO GOOD-STATUSCODES                          
076329     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
076330     MOVE WDB2-STATUS-CODE   TO STATUS-WS                                 
076331     PERFORM IMS-STATUSCHECK                                              
076332     .                                                                    
076333                                                                          
076334                                                                          
076335 IMS-GET-MSG SECTION.                                                     
076336                                                                          
076337     MOVE '  QC' TO GOOD-STATUSCODES                                      
076338     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
076339     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076340     PERFORM IMS-STATUSCHECK                                              
076350     .                                                                    
076360     SKIP3                                                                
076370 IMS-INSERT-MSG SECTION.                                                  
076380                                                                          
076390     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
076400     MOVE SPACE TO GOOD-STATUSCODES                                       
076500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
076600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076700     PERFORM IMS-STATUSCHECK                                              
076800     .                                                                    
076900     EJECT                                                                
077000 IMS-STATUSCHECK SECTION.                                                 
077100                                                                          
077200     SET STATUS-IX TO 1                                                   
077300     SEARCH GOOD-STATUS                                                   
077400       AT END                                                             
077500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
077600         DELIMITED BY SIZE INTO ERROR-TEXT                                
077700         CALL FELLOG                                                      
077800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
077900         CONTINUE                                                         
078000     END-SEARCH                                                           
078100     .                                                                    
