000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2045900.                                                
000300 AUTHOR.         MARIE ODSELL.                                            
000400 DATE-WRITTEN.   12/09/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*FUNCTION:       LOCAL SCRAPPING                                          
000800*                                                                         
001000*    THE PROGRAM UPDATES: WDK7                                            
001200*    THE PROGRAM UPDATES: WDR5                                            
001300*    THE PROGRAM READS  : WDN6                                            
001400*    THE PROGRAM READS  : WDB6                                            
001500*    THE PROGRAM READS  : WDD3                                            
001600*    THE PROGRAM READS  : WDK6                                            
001700*    THE PROGRAM READS  : WDP7                                            
001800*                                                                         
001900*    CALL TO            : W411SAP                                         
002000*                       : W1011300                                        
002010*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION    : W2T459                                          
002300*        MID            : W2I45901                                        
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        MOD            : W2O45901                                        
002700*                                                                         
002800*    CCID               : 10143273                                        
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300 DATA DIVISION.                                                           
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003510                                                                          
003520*    -COPY WY2000W1                                                       
003530*                                                                         
003600 77  IDPGM                       PIC X(08)   VALUE 'W2045900'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004400 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004500     88  INDATA-OK                           VALUE 'Y'.                   
004600     88  INDATA-WRONG                        VALUE 'N'.                   
004700                                                                          
004710 77  INGAR-SATS-SW               PIC X.                                   
004720     88  INGAR-I-SATS                        VALUE 'Y'.                   
004721     88  INGAR-EJ-I-SATS                     VALUE 'N'.                   
004730                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
004900     88  KEYS-OK                             VALUE 'Y'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100                                                                          
005200 77  SPR                         PIC S9      VALUE +1 COMP-3.             
005300 77  W-OSPARRAT-ANTAL            PIC S9(7)   COMP-3 VALUE ZERO.           
005400 77  BEEMB-IX                    PIC 9(3)    VALUE ZERO.                  
005500 77  IX                          PIC 9(3)    VALUE ZERO.                  
005600 77  IX2                         PIC 9(3)    VALUE ZERO.                  
005700 77  INDX                        PIC S9(3)   VALUE ZERO.                  
005710 77  PROGSW-IX                   PIC 9(2)    VALUE ZERO.                  
005800 77  W-SPAR-BEANST-GODK          PIC X(25)   VALUE SPACE.                 
005810 77  W-SPAR-IDANSK               PIC S9(3)   COMP-3 VALUE ZERO.           
005900 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 77  DAGENS-DATUM-Y2K            PIC 9(8)    VALUE ZERO.                  
006100 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
006200 77  W-SUSKROT                   PIC S9(6)   VALUE ZERO.                  
006300 77  W-KVAKS                     PIC S9(6)   VALUE ZERO.                  
006400 77  W-KVOKS                     PIC S9(6)   VALUE ZERO.                  
006500 77  W-KVDISP                    PIC S9(7)   VALUE ZERO.                  
006600 77  W-KVDISP1                   PIC S9(6)   VALUE ZERO.                  
006700 77  W-KVDISP2                   PIC S9(6)   VALUE ZERO.                  
006800 77  W-ANTAL-SKROT               PIC S9(7)   COMP-3 VALUE ZERO.           
006900 77  W-KVSKROT-KVAR              PIC S9(7)   COMP-3 VALUE ZERO.           
007000 77  W-KVSKROT                   PIC S9(7)   COMP-3 VALUE ZERO.           
007010 77  W-TISKROT-AUTO              PIC 9(06)          VALUE ZERO.           
007100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007200     88  OWN-MID                             VALUE '2459'.                
007300     88  GOOD-MID                            VALUE '2459'.                
007400     88  HELP-MID                            VALUE '0551'.                
007500     EJECT                                                                
007600 01  GENERAL-SUBPROGRAMS.                                                 
007700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008300     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
008400     03  W411SAP                 PIC X(8)    VALUE 'W411SAP'.             
008500                                                                          
008600*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008800*01 -COPY WMSGINIT                                                        
008900     EJECT                                                                
009000*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
009100 01  FILLER                      PIC X(16)   VALUE 'WDATKONV'.            
009200*01 -COPY WDATAREA                                                        
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'WMEDKONV'.            
009500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009600*01 -COPY WMEDAREA                                                        
009700     EJECT                                                                
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'W411SAP'.             
010000*    --- PARAMETRAR TILL SUBPROGRAM W411SAP                               
010100*01 -COPY W411SAP                                                         
010200     SKIP3                                                                
010600     SKIP3                                                                
010700*01 -COPY WWIDFTG                                                         
010800      EJECT                                                               
010900 01  MESSAGE-CODES.                                                       
011000     03  ERR-CORR-HILITE-FLDS        PIC X(3) VALUE '001'.                
011100     03  INF-PRESS-PF11              PIC X(3) VALUE '003'.                
011200     03  ERR-UPDATE-NOT-ALLOWED      PIC X(3) VALUE '007'.                
011300     03  ERR-PF11-AND-NO-DATA        PIC X(3) VALUE '011'.                
011400     03  INF-UPDATE-DONE             PIC X(3) VALUE '101'.                
011500     03  ERR-WRONG-KEY               PIC X(3) VALUE '401'.                
011600     03  ERR-PART-MISSING            PIC X(3) VALUE '017'.                
011700     03  ERR-ORDER-NOT-AVAILABLE     PIC X(3) VALUE '248'.                
011800     03  ERR-PART-SUPERSEDED         PIC X(3) VALUE '018'.                
011900     03  ERR-DIRECTLY-DELIVERED-PART PIC X(3) VALUE '306'.                
012000     03  ERR-NOT-AUTHORIZED-TO-SCRAP PIC X(3) VALUE '601'.                
012100     03  ERR-FIELDS-ARE-NOT-NUMERIC  PIC X(3) VALUE '020'.                
012200     03  ERR-USER-NOT-AUTHORIZED     PIC X(3) VALUE '405'.                
012300     03  ERR-NO-LOC-SOURCED-PART     PIC X(3) VALUE '433'.                
012400     03  ERR-REFILL-PART             PIC X(3) VALUE '434'.                
012500     03  ERR-ACCOUNT-MISSING-IN-R3   PIC X(3) VALUE '346'.                
012600     03  ERR-ANALYSIS-NO-MISSING-IN-R3 PIC X(3) VALUE '347'.              
012700     03  ERR-COSTCENTER-MISSING-IN-R3  PIC X(3) VALUE '348'.              
012800     03  ERR-REFILL-PART-USE-2359    PIC X(3)   VALUE '367'.              
012810     03  ERR-DC-INVALID              PIC X(3) VALUE '440'.                
012900                                                                          
013000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
013100 01  W-SDC-KVLS                  PIC S9(7)   VALUE ZERO COMP-3.           
013200 01  W-SDC-KVAKS                 PIC S9(7)   VALUE ZERO COMP-3.           
013300 01  W-SDC-KVOKS                 PIC S9(7)   VALUE ZERO COMP-3.           
013301*                                                                         
013310 01  WARTC91-KDERS-C1            PIC S9(3)   COMP-3.                      
013400                                                                          
013500     EJECT                                                                
013510 01  FILLER              PIC X(16)  VALUE 'PROG-TO-PROG-SW'.              
013520                                                                          
013530 01  W-PROG-TO-PROG-SW.                                                   
013540     05  P-WS-LL         PIC S9(4)  VALUE +469 COMP SYNC.                 
013550     05  P-WS-Z1-Z2      PIC  X(2)  VALUE LOW-VALUE.                      
013560     05  KDTRANS-WS      PIC  X(8)  VALUE 'W1T113X '.                     
013570     05  FILLER          PIC  X(5)  VALUE '24593'.                        
013580*    05  MID    -COPY W1I11301     -PRE PROGSW-                           
013590     EJECT                                                                
013600                                                                          
013700 01  DAGENS-TID-I-DELAR.                                                  
013800     03  FILLER                  PIC 9(1).                                
013900     03  DAGENS-TIT              PIC 9(1).                                
014000     03  DAGENS-TIMM             PIC 9(2).                                
014100     03  FILLER                  PIC 9(4).                                
014200                                                                          
014300 01  DAGENS-AAVVD.                                                        
014400     03  FILLER                  PIC 9(3).                                
014500     03  DAGENS-VECKA            PIC 9(1).                                
014600     03  DAGENS-DAG              PIC 9(1).                                
014700                                                                          
014800*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
014900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015000     SKIP3                                                                
015100*01  MID -COPY W2I45901                                                   
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
015400     SKIP3                                                                
015500*01  -COPY WMSGAREA                                                       
015600     EJECT                                                                
015700     03  MOD REDEFINES MSG-AREA.                                          
015800*      05  -COPY W2O45901                                                 
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016100     SKIP3                                                                
016200*01  -COPY WMFSAREA                                                       
016300     EJECT                                                                
016400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
016500*                                                                         
016600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016700     SKIP3                                                                
016800 01  KEYS-FOR-DLI.                                                        
016900     03  W-IDARTNR-X.                                                     
017000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017100                                                                          
017110     03  W-IDARTNR-SS-X.                                                  
017120         05  W-IDARTNR-SS        PIC S9(9)   VALUE ZERO COMP-3.           
017130                                                                          
017200     03  W-IDDC-X.                                                        
017300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017400                                                                          
017500     03  W-KDSEGKEY-X.                                                    
017600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
017700                                                                          
017800     03  W-WDN611KY-X.                                                    
017900         05  W-IDFORDON          PIC S9(2)   VALUE ZERO  COMP-3.          
018000         05  W-TIOMBRYT          PIC S9(7)   VALUE ZERO  COMP-3.          
018100                                                                          
018200     03  W-IDSKYLT-X.                                                     
018300         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
018400                                                                          
018500     03  W-WDGX6321-X.                                                    
018600         05  W-IDHTYP            PIC X(4)    VALUE '6321'.                
018700         05  W-KDARBTYP-6321     PIC X(8)    VALUE 'ANSK    '.            
018800         05  W-FILLER            PIC X(18)   VALUE LOW-VALUE.             
018900                                                                          
019000     03  W-WDGX6322-X.                                                    
019100         05  W-DASKROT9-BEORD    PIC 9(08)   VALUE ZERO.                  
019200                                                                          
019300     03  W-KY6324-X.                                                      
019400         05  W-IDARTNR-6324      PIC S9(9)   VALUE ZERO COMP-3.           
019500         05  W-IDDC-6324         PIC X(2)    VALUE SPACE.                 
019600         05  W-KDSTASKR-6324     PIC S9      VALUE ZERO COMP-3.           
019700                                                                          
019800     03  W-WDGXKEY-X.                                                     
019900         05  W-IDHTYP-6327       PIC X(4)    VALUE '6327'.                
020000         05  W-KDARBTYP-6327     PIC X(8)    VALUE 'ANSK    '.            
020100         05  W-IDDC-6327         PIC X(2)    VALUE SPACE.                 
020200         05  W-FILLER            PIC X(16)   VALUE LOW-VALUE.             
020300                                                                          
020400     03  W-KY6328-X.                                                      
020500         05  W-SUBEL             PIC X(7)    VALUE SPACE.                 
020600         05  W-IDUSER-GODK       PIC X(8)    VALUE SPACE.                 
020700                                                                          
020800     03  W-IDLAND-X.                                                      
020900         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
021000                                                                          
021010 01  W-WDJ1C1KY-MIN.                                                      
021020     03  FILLER               PIC X(5)  VALUE SPACE.                      
021030     03  FILLER               PIC X(30) VALUE SPACE.                      
021040     03  W-RAD-IDARTNR-MIN    PIC S9(9) VALUE ZERO    COMP-3.             
021050     03  FILLER               PIC X(9)  VALUE LOW-VALUE.                  
021060 01  W-WDJ1C1KY-MAX.                                                      
021070     03  FILLER               PIC X(5)  VALUE SPACE.                      
021080     03  FILLER               PIC X(30) VALUE SPACE.                      
021090     03  W-RAD-IDARTNR-MAX    PIC S9(9) VALUE ZERO    COMP-3.             
021091     03  FILLER               PIC X(9)  VALUE HIGH-VALUE.                 
021092 01  W-WDJ111KY-X.                                                        
021093     03  W-KDSTRRAD           PIC X(1).                                   
021094     03  W-IDRADNR            PIC S9(5) COMP-3 VALUE ZERO.                
021095 01  W-IDARTNR-STR-X.                                                     
021096     03  W-IDARTNR-STR        PIC S9(9) COMP-3 VALUE ZERO.                
021100*    --- STATUS CODES FROM IMS                                            
021200 01  STATUS-WS                   PIC XX.                                  
021300     88  SEGMENT-FOUND                       VALUE '  '.                  
021400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
021500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
021600     SKIP2                                                                
021700 01  GOOD-STATUSCODES.                                                    
021800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021900     SKIP3                                                                
022000 01  SSA1                        PIC X(256).                              
022100 01  SSA2                        PIC X(64).                               
022200 01  SSA3                        PIC X(64).                               
022300     EJECT                                                                
022400*    --- IMS FUNCTION CODES                                               
022500*01  -COPY W0003                                                          
022600     EJECT                                                                
022700*    ---  DLI INPUT-OUTPUT AREA                                           
022800                                                                          
022900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN601'.                      
023000 01  DLI-IO-WDN601.                                                       
023100*    03  -COPY WDN601                                                     
023200                                                                          
023300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN611'.                      
023400 01  DLI-IO-WDN611.                                                       
023500*    03  -COPY WDN611                                                     
023600                                                                          
023700                                                                          
023800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
023900 01  DLI-IO-WDB601.                                                       
024000*    03  -COPY WDB601                                                     
024100                                                                          
024600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
024700 01  DLI-IO-WDD311.                                                       
024800*    03  -COPY WDD311                                                     
024900                                                                          
025000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6321'.                 
025100 01  DLI-IO-WDR501-6321.                                                  
025200*    03  -COPY WDGX6321                                                   
025300                                                                          
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
025500 01  DLI-IO-WDGX6322.                                                     
025600*    03  -COPY WDGX6322                                                   
025700                                                                          
025800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
025900 01  DLI-IO-WDGX6324.                                                     
026000*    03  -COPY WDGX6324                                                   
026100                                                                          
026200 01  FILLER         PIC X(24) VALUE  'DLI-IO-WDR501-6327'.                
026300 01  DLI-IO-WDR501-6327.                                                  
026400*    03  -COPY WDGX6327                                                   
026500                                                                          
026600 01  FILLER         PIC X(16) VALUE  'DLI-IO-WDGX6328'.                   
026700 01  DLI-IO-WDGX6328.                                                     
026800*    03  -COPY WDGX6328                                                   
026900                                                                          
027000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
027100 01  DLI-IO-WDK601.                                                       
027200*    03  -COPY WDK601                                                     
027300                                                                          
027400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
027500 01  DLI-IO-WDK611.                                                       
027600*    03  -COPY WDK611                                                     
027700                                                                          
027800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
027900 01  DLI-IO-WDK701.                                                       
028000*    03  -COPY WDK701                                                     
028100                                                                          
028200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
028300 01  DLI-IO-WDK711.                                                       
028400*    03  -COPY WDK711                                                     
028500                                                                          
028600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
028700 01  DLI-IO-WDK712.                                                       
028800*    03  -COPY WDK712                                                     
028900                                                                          
029000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
029100 01  DLI-IO-WDK722.                                                       
029200*    03  -COPY WDK722                                                     
029300                                                                          
029301 01  FILLER         PIC X(16) VALUE 'DLI-IO-SATE01'.                      
029302 01  DLI-IO-SATE01.                                                       
029303*    03  -COPY WDJ1C1                                                     
029304                                                                          
029305 01  FILLER         PIC X(16) VALUE 'DLI-IO-SATB01'.                      
029306 01  DLI-IO-SATB01.                                                       
029307*    03  -COPY WDJ101                                                     
029308                                                                          
029309 01  FILLER         PIC X(16) VALUE 'DLI-IO-SATB11'.                      
029310 01  DLI-IO-SATB11.                                                       
029311*    03  -COPY WDJ111                                                     
029312                                                                          
029313 01  FILLER         PIC X(16) VALUE 'DLI-IO-01'.                          
029314 01  DLI-IO-01.                                                           
029315*    03  -COPY WDK601     -PRE ERS-                                       
029316                                                                          
029317 01  FILLER         PIC X(16) VALUE 'DLI-IO-11'.                          
029318 01  DLI-IO-11.                                                           
029319*    03  -COPY WDK611     -PRE ERS-                                       
029320                                                                          
029330                                                                          
029400 LINKAGE SECTION.                                                         
029500*01  -COPY W0009   -PRE MSG-                                              
029510*01  -COPY W0009   -PRE ALT-                                              
029600*01  -COPY W0008   -PRE WDP7-                                             
029700     05  FILLER                  PIC X.                                   
029800*01  -COPY W0008  -PRE WDN6-                                              
029900     05  FILLER                  PIC X.                                   
030000*01  -COPY W0008  -PRE WDB6-                                              
030100     05  FILLER                  PIC X.                                   
030400*01  -COPY W0008  -PRE WDD3-                                              
030500     05  FILLER                  PIC X.                                   
030600*01  -COPY W0008  -PRE 6321-                                              
030700     05  FILLER                  PIC X.                                   
030800*01  -COPY W0008  -PRE 6327-                                              
030900     05  FILLER                  PIC X.                                   
031000*01  -COPY W0008  -PRE WDK6-                                              
031100     05  FILLER                  PIC X.                                   
031110*01  -COPY W0008  -PRE WDK62-                                             
031120     05  FILLER                  PIC X.                                   
031200 01  SAPC-PCB                    PIC X.                                   
031300*01  -COPY W0008  -PRE WDK7-                                              
031400     05  FILLER                  PIC X.                                   
031410*01  -COPY W0008  -PRE SATE-                                              
031420     05  FILLER                  PIC X.                                   
031440*    -COPY W0008  -PRE SATB-                                              
031450     05    FILLER                PIC X.                                   
031500     EJECT                                                                
031600 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDN6-PCB              
031700                           WDB6-PCB WDD3-PCB                              
031800                           6321-PCB 6327-PCB WDK6-PCB WDK62-PCB           
031900                           SAPC-PCB WDK7-PCB SATE-PCB  SATB-PCB.          
032000 MAIN SECTION.                                                            
032100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDN6-PCB              
032200                           WDB6-PCB WDD3-PCB                              
032300                           6321-PCB 6327-PCB WDK6-PCB WDK62-PCB           
032310                           SAPC-PCB WDK7-PCB SATE-PCB  SATB-PCB.          
032500     PERFORM IMS-GET-MSG                                                  
032600     IF SEGMENT-FOUND                                                     
032700       PERFORM A-INIT                                                     
032800       PERFORM B-CHECK-KEYS                                               
032900       IF KEYS-OK                                                         
033000         IF MFS-UPDATE                                                    
033100           PERFORM   G-CHECK-INPUT                                        
033200           IF INDATA-OK                                                   
033300             PERFORM H-UPDATE                                             
033400           END-IF                                                         
033500         ELSE                                                             
033600           IF MFS-FIRST                                                   
033700             PERFORM C-FIRST-PAGE                                         
033800           ELSE                                                           
033900             PERFORM E-SAME-PAGE                                          
034000           END-IF                                                         
034100         END-IF                                                           
034200         PERFORM F-READ-SHOW-INFO                                         
034310       END-IF                                                             
034400       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O45901-CTX + 4                  
034500       PERFORM IMS-INSERT-MSG                                             
034600     END-IF                                                               
034700                                                                          
034800     MOVE ZERO TO RETURN-CODE                                             
034900     GOBACK                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 A-INIT SECTION.                                                          
035300                                                                          
035400     IF MSG-DOUBLE-TRANSACTIONS                                           
035500       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I45901                 
035600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
035700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
035800     ELSE                                                                 
035900       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I45901                  
036000       MOVE MSG-IDTRANS-1   TO MFS-IDTRANS                                
036100       MOVE MSG-KDMFSFOR-1  TO MFS-KDMFSFOR                               
036200     END-IF                                                               
036300                                                                          
036400     MOVE MSG-KDTRTYP       TO MFS-KDTRTYP                                
036500     MOVE MSG-IDPFK         TO MFS-IDPFK                                  
036600     MOVE MFS-IDTRANS       TO W-IDTRANS                                  
036700                                                                          
036800     MOVE LOW-VALUE TO MSG-AREA                                           
036900     MOVE 'W2O459N1' TO MFS-IDMOD                                         
037000     MOVE '2459' TO MOD-IDTRANS                                           
037100     MOVE MFS-ERASE-FIELD   TO MOD-TEMFSFEL MOD-TEMFSINF                  
037200     MOVE SPACE             TO MED-IDMFSFEL MED-IDMFSINF                  
037300                                                                          
037400     IF OWN-MID OR HELP-MID                                               
037500       CONTINUE                                                           
037600     ELSE                                                                 
037700       MOVE SPACE           TO MFS-KDTRTYP                                
037800       MOVE '7'             TO MFS-IDPFK                                  
037900     END-IF                                                               
038000                                                                          
038100     ACCEPT DAGENS-DATUM FROM DATE                                        
038200     ACCEPT DAGENS-TID FROM TIME                                          
038300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
038400     .                                                                    
038500     EJECT                                                                
038600 B-CHECK-KEYS SECTION.                                                    
038700                                                                          
038800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
038900     MOVE '001'             TO MSGI-KDCALL                                
039000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
039100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039200     MOVE '2459'            TO MSGI-IDTRANS                               
039300     IF OWN-MID                                                           
039400       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
039500       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
039600     END-IF                                                               
039700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
039800                                                                          
039900*    - LANGUAGE TO BE USED BY MEDKONV                                     
040000     IF MSGI-IDLAND-SPR = 'SE'                                            
040100       MOVE +1              TO SPR                                        
040200       MOVE 'S  '           TO MED-IDSKYLT                                
040300       MOVE 'S '            TO W-IDSKYLT                                  
040400     ELSE                                                                 
040500       MOVE +2              TO SPR                                        
040600       MOVE 'GB '           TO MED-IDSKYLT                                
040700       MOVE 'GB'            TO W-IDSKYLT                                  
040800     END-IF                                                               
040900                                                                          
041000     MOVE YES TO KEYS-SW                                                  
041100                                                                          
041200*    -- CHECK IDARTNR                                                     
041300     MOVE MFS-ERASE-FIELD   TO MOD-IDARTNR-IN                             
041400     IF MID-IDARTNR-IN NOT  =  ALL '+'                                    
041500       MOVE '7'             TO MFS-IDPFK                                  
041600       MOVE SPACE           TO MFS-KDTRTYP                                
041700     END-IF                                                               
041800                                                                          
041900     IF MSGI-IDARTNR  NUMERIC AND MSGI-IDARTNR > 0                        
042000       MOVE MSGI-IDARTNR    TO W-IDARTNR                                  
042100     ELSE                                                                 
042200       MOVE NOO TO KEYS-SW                                                
042300       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
042400     END-IF                                                               
042500                                                                          
042600*    -- CHECK IDDC                                                        
042700     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
042800     IF MID-IDDC-IN NOT     =  ALL '+'                                    
042900       MOVE '7'             TO MFS-IDPFK                                  
043000       MOVE SPACE           TO MFS-KDTRTYP                                
043100     END-IF                                                               
043200     MOVE MSGI-IDDC-KEY     TO W-IDDC                                     
043300                                                                          
043310     IF GOOD-MID OR KEYS-OK                                               
043320       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
043330       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
043340       MOVE MSGI-IDDC-KEY       TO MOD-IDDC-UT                            
043350     ELSE                                                                 
043360       MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-UT                             
043370       MOVE MFS-ERASE-FIELD TO MOD-IDDC-UT                                
043380     END-IF                                                               
043390                                                                          
043400     IF KEYS-OK                                                           
043500*    -- CHECK IF IDDC EXIST                                               
043600       PERFORM IMS-GU-WDB601                                              
043700       IF SEGMENT-MISSING                                                 
043800         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
043900         MOVE NOO TO KEYS-SW                                              
044000       ELSE                                                               
044100*    --  CHECK CHINA/NA-USA AND WORKROLE=ANSK                             
044110*    --  SKIPPA WORKROLE=ANSK KOLL ENLIGT ALEXANDER                       
044200         IF (DCS-NDC-CN                                                   
044300         OR (DCS-NDC-NA AND DCS-USA))                                     
044400*        AND MSGI-KDARBTYP-SEC(1:4) = 'ANSK'                              
044401           PERFORM IMS-GU-WDK601                                          
044402           IF SEGMENT-FOUND                                               
044410             IF ART-FLIART = 'J'                                          
044420                 MOVE YES                TO INGAR-SATS-SW                 
044430             ELSE                                                         
044440                 MOVE NOO                TO INGAR-SATS-SW                 
044450             END-IF                                                       
044451             PERFORM IMS-GNP-WDK611                                       
044452             IF SEGMENT-FOUND                                             
044453               MOVE CLAG-KDERS   TO WARTC91-KDERS-C1                      
044454             ELSE                                                         
044455               MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                         
044456               MOVE NOO TO KEYS-SW                                        
044457             END-IF                                                       
044458           ELSE                                                           
044459             MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                           
044460             MOVE NOO TO KEYS-SW                                          
044470           END-IF                                                         
044600         ELSE                                                             
044700           MOVE ERR-DC-INVALID   TO MED-IDMFSFEL                          
044800           MOVE NOO TO KEYS-SW                                            
044900         END-IF                                                           
045000       END-IF                                                             
045100     END-IF                                                               
046100                                                                          
046200     IF KEYS-WRONG                                                        
046300       CALL WMEDKONV USING MED-WMEDAREA                                   
046400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
046500       PERFORM MFS-ERASE-FIELD-IN                                         
046600       PERFORM MFS-ERASE-FIELD-OUT                                        
046700     END-IF                                                               
046800     .                                                                    
046900     EJECT                                                                
047000 C-FIRST-PAGE SECTION.                                                    
047100                                                                          
047200     PERFORM MFS-ERASE-FIELD-IN                                           
047201     MOVE ALL '+'            TO MID-TISKROT-AUTO                          
047300     .                                                                    
047400     EJECT                                                                
047500 E-SAME-PAGE SECTION.                                                     
047600                                                                          
047700     IF OWN-MID OR HELP-MID                                               
047800       IF MID-INPUT             = ALL '+'                                 
047900         PERFORM MFS-ERASE-FIELD-IN                                       
048000       ELSE                                                               
048100         MOVE INF-PRESS-PF11    TO MED-IDMFSINF                           
048200         CALL WMEDKONV USING MED-WMEDAREA                                 
048300         MOVE MED-MFSINF        TO MOD-TEMFSINF                           
048400         PERFORM EA-MID-INDATA-TO-MOD                                     
048500       END-IF                                                             
048600     ELSE                                                                 
048700       PERFORM MFS-ERASE-FIELD-IN                                         
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100                                                                          
049200 EA-MID-INDATA-TO-MOD SECTION.                                            
049300     IF MID-KVSKROT-KVAR NOT = ALL '+'                                    
049400       MOVE MID-KVSKROT-KVAR       TO MOD-KVSKROT-KVAR                    
049500       MOVE MFS-ADD-READ-FIELD     TO MOD-KVSKROT-KVAR-ATTR               
049600     ELSE                                                                 
049700       MOVE MFS-ERASE-FIELD        TO MOD-KVSKROT-KVAR                    
049800     END-IF                                                               
049900                                                                          
050000     IF MID-SKROT-TEXT   NOT = ALL '+'                                    
050100       MOVE MID-SKROT-TEXT         TO MOD-SKROT-TEXT                      
050200       MOVE MFS-ADD-READ-FIELD     TO MOD-SKROT-TEXT-ATTR                 
050300     ELSE                                                                 
050400       MOVE MFS-ERASE-FIELD        TO MOD-SKROT-TEXT                      
050500     END-IF                                                               
050600                                                                          
050700     IF MID-IDKONTO      NOT = ALL '+' AND SPACE                          
050800       INSPECT MID-IDKONTO                                                
050900                         REPLACING LEADING SPACE BY ZERO                  
051000       MOVE MID-IDKONTO            TO MOD-IDKONTO                         
051100       MOVE MFS-ADD-READ-FIELD     TO MOD-IDKONTO-ATTR                    
051200     ELSE                                                                 
051300       MOVE MFS-ERASE-FIELD        TO MOD-IDKONTO                         
051400     END-IF                                                               
051500                                                                          
051600     IF MID-IDANALYS     NOT = ALL '+'                                    
051700       MOVE MID-IDANALYS           TO MOD-IDANALYS                        
051800       MOVE MFS-ADD-READ-FIELD     TO MOD-IDANALYS-ATTR                   
051900     ELSE                                                                 
052000       MOVE MFS-ERASE-FIELD        TO MOD-IDANALYS                        
052100     END-IF                                                               
052101                                                                          
052180     IF MID-TISKROT-AUTO NOT = ALL '+'                                    
052190       MOVE MID-TISKROT-AUTO       TO MOD-TISKROT-AUTO                    
052191       MOVE MFS-ADD-READ-FIELD     TO MOD-TISKROT-AUTO-ATTR               
052192     ELSE                                                                 
052193       MOVE MFS-ERASE-FIELD        TO MOD-TISKROT-AUTO                    
052194     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052400 F-READ-SHOW-INFO SECTION.                                                
052500                                                                          
052600*    --CHECK IDARTNR                                                      
052700     PERFORM IMS-GU-WDK601                                                
052800     IF SEGMENT-MISSING                                                   
052900       MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                         
053000       CALL WMEDKONV USING MED-WMEDAREA                                   
053100       MOVE MED-TEMFSFEL          TO MOD-TEMFSFEL                         
053200       PERFORM MFS-ERASE-FIELD-OUT                                        
053300     ELSE                                                                 
053400       IF ART-KDERS-UTG > +0                                              
053500         MOVE ERR-PART-SUPERSEDED TO MED-IDMFSFEL                         
053600         CALL WMEDKONV USING MED-WMEDAREA                                 
053700         MOVE MED-TEMFSFEL        TO MOD-TEMFSFEL                         
053800         PERFORM MFS-ERASE-FIELD-OUT                                      
053900       ELSE                                                               
054000         PERFORM IMS-GU-WDK701                                            
054100         IF SEGMENT-FOUND                                                 
054200           PERFORM IMS-GNP-WDK711                                         
054300           IF SEGMENT-FOUND                                               
054400             IF SLAG-IDDC-REF = SPACE                                     
054500               PERFORM IMS-GNP-WDK611                                     
054600               PERFORM FA-READ-SHOW-INFO                                  
054700             ELSE                                                         
054800               MOVE ERR-REFILL-PART-USE-2359 TO MED-IDMFSFEL              
054900               CALL WMEDKONV USING MED-WMEDAREA                           
055000               MOVE MED-MFSFEL            TO MOD-TEMFSFEL                 
055100               PERFORM MFS-ERASE-FIELD-OUT                                
055200             END-IF                                                       
055300           ELSE                                                           
055400             MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                   
055500             CALL WMEDKONV USING MED-WMEDAREA                             
055600             MOVE MED-TEMFSFEL          TO MOD-TEMFSFEL                   
055700             PERFORM MFS-ERASE-FIELD-OUT                                  
055800           END-IF                                                         
055900         ELSE                                                             
056000           MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                     
056100           CALL WMEDKONV USING MED-WMEDAREA                               
056200           MOVE MED-TEMFSFEL          TO MOD-TEMFSFEL                     
056300           PERFORM MFS-ERASE-FIELD-OUT                                    
056400         END-IF                                                           
056500       END-IF                                                             
056600     END-IF                                                               
056700     .                                                                    
056800     EJECT                                                                
056900                                                                          
057000 FA-READ-SHOW-INFO SECTION.                                               
057002     IF MID-TISKROT-AUTO = ALL '+'                                        
057010       MOVE SLAG-TISKROT-AUTO  TO W-TISKROT-AUTO                          
057020       MOVE W-TISKROT-AUTO     TO MOD-TISKROT-AUTO                        
057040       INSPECT MOD-TISKROT-AUTO REPLACING LEADING ZERO BY SPACE           
057050     ELSE                                                                 
057051       MOVE MID-TISKROT-AUTO   TO MOD-TISKROT-AUTO                        
057094     END-IF                                                               
057100     MOVE CLAG-KDERS           TO MOD-KDERS                               
057110                                  WARTC91-KDERS-C1                        
057200     MOVE SLAG-KVLS            TO MOD-KVLS                                
057300     MOVE SLAG-KVSKROT         TO MOD-KVSKROT                             
057400     MOVE SLAG-TISKROT         TO MOD-TISKROT                             
057500     MOVE SLAG-TISKROT-BEORD   TO MOD-TISKROT-BEORD                       
057600     COMPUTE W-KVOKS           = SLAG-KVOKS-BULK +                        
057700                                 SLAG-KVOKS-DAG                           
057800     MOVE W-KVOKS              TO MOD-KVOKS                               
057900                                                                          
058000* -- W-KVDISP1= KVLS       - KVRES                                        
058100* -- W-KVDISP2= KVOKS-BULK + KVOKS-DAG                                    
058200* -- KVDISP   = W-KVDISP1  - W-KVDISP2                                    
058300     COMPUTE W-KVDISP1         = SLAG-KVLS      -                         
058400                                 SLAG-KVRESS                              
058500     COMPUTE W-KVDISP2         = SLAG-KVOKS-DAG +                         
058600                                 SLAG-KVOKS-BULK                          
058700     COMPUTE W-KVDISP          = W-KVDISP1 - W-KVDISP2                    
058800     MOVE W-KVDISP             TO MOD-KVDISP                              
058900                                                                          
059000* -- KVAKS    = KVAKS-SDC  + KVAKS-PAV                                    
059100     COMPUTE W-KVAKS           = SLAG-KVAKS-SDC  +                        
059200                                 SLAG-KVAKS-PAV                           
059300     MOVE W-KVAKS              TO MOD-KVAKS                               
059400                                                                          
059500     IF SLAG-FLSKROT-BEORD = JA                                           
059600       MOVE YES                TO MOD-FLSKROT-BEORD                       
059700     ELSE                                                                 
059800       MOVE SLAG-FLSKROT-BEORD TO MOD-FLSKROT-BEORD                       
059900     END-IF                                                               
060000                                                                          
060100     PERFORM IMS-GNP-WDK722                                               
060110     IF SEGMENT-FOUND                                                     
060200       IF XLAG-TISLUTKP = ZERO                                            
060300         MOVE ZERO              TO MOD-TISLUTKP                           
060400       ELSE                                                               
060500         IF XLAG-TISLUTKP > DAGENS-DATUM                                  
060600           MOVE XLAG-TISLUTKP   TO MOD-TISLUTKP                           
060700         ELSE                                                             
060800           MOVE ZERO            TO MOD-TISLUTKP                           
060900         END-IF                                                           
061000       END-IF                                                             
061100       MOVE XLAG-KVSLUTKP       TO MOD-KVSLUTKP                           
061200       MOVE XLAG-KVSPANT        TO MOD-KVSPANT                            
061210     ELSE                                                                 
061211       MOVE ZERO                TO MOD-TISLUTKP                           
061212                                   MOD-KVSLUTKP                           
061213                                   MOD-KVSPANT                            
061220     END-IF                                                               
061300     MOVE SPACE                 TO MOD-FLERSATT                           
061400                                                                          
061500     MOVE DCS-IDLANDX2          TO W-IDLAND                               
061600     PERFORM IMS-GNP-WDK712                                               
061700     IF SEGMENT-FOUND                                                     
061800        IF LART-TIERSDAT-VIPS      = ZERO                                 
061900          MOVE 'N'              TO MOD-FLERSATT                           
062000        ELSE                                                              
062100          IF LART-TIERSDAT-VIPS    > ZERO                                 
062200            MOVE 'Y'            TO MOD-FLERSATT                           
062300          ELSE                                                            
062400            MOVE SPACE          TO MOD-FLERSATT                           
062500          END-IF                                                          
062600        END-IF                                                            
062700     ELSE                                                                 
062800        MOVE SPACE              TO MOD-FLERSATT                           
062900     END-IF                                                               
063000                                                                          
063100     IF MFS-UPDATE AND                                                    
063200        INDATA-OK AND                                                     
063210        MID-KDERS = ALL '+'                                               
063300        COMPUTE W-ANTAL-SKROT   = SLAG-KVLS - W-KVSKROT-KVAR              
063400        END-COMPUTE                                                       
063500        COMPUTE W-SUSKROT       = W-ANTAL-SKROT * SLAG-PRAVCOST           
063600        END-COMPUTE                                                       
063700        MOVE W-SUSKROT          TO MOD-SUSKROT                            
063800     END-IF                                                               
063900                                                                          
064000     PERFORM IMS-GU-WDD311                                                
064100     IF SEGMENT-FOUND                                                     
064200      MOVE TEXT-BEART           TO MOD-BEART-ENG                          
064300     ELSE                                                                 
064400      MOVE SPACE                TO MOD-BEART-ENG                          
064500     END-IF                                                               
064600     .                                                                    
064700     EJECT                                                                
064800 G-CHECK-INPUT SECTION.                                                   
064900                                                                          
065000     MOVE YES  TO INDATA-SW                                               
065100     IF MID-INPUT           = ALL '+'                                     
065200       MOVE ERR-PF11-AND-NO-DATA               TO MED-IDMFSFEL            
065300       CALL WMEDKONV USING MED-WMEDAREA                                   
065400       MOVE MED-MFSFEL                         TO MOD-TEMFSFEL            
065500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
065600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
065700       MOVE NOO                                TO INDATA-SW               
065800     ELSE                                                                 
065801       PERFORM GD-AUTH-USER-CHECK                                         
065802       IF INDATA-OK                                                       
065810         IF MID-KDERS = ALL '+'                                           
065820           MOVE MFS-RENSA-FAELT    TO MOD-KDERS                           
065830         ELSE                                                             
065831          IF MID-KDERS NOT = '09'                                         
065832             MOVE NOO               TO INDATA-SW                          
065833             MOVE MFS-NUM-FAELT-FEL TO MOD-KDERS-ATTR                     
065834          ELSE                                                            
065835             IF INGAR-I-SATS                                              
065836                MOVE W-IDARTNR         TO W-RAD-IDARTNR-MIN               
065837                                            W-RAD-IDARTNR-MAX             
065838                PERFORM IMS-GU-SATE01                                     
065839                PERFORM UNTIL SEGMENT-MISSING OR INDATA-SW = NOO          
065840                  IF SEQC-IDARTNR-STR < +100000000                        
065841                     MOVE SEQC-IDARTNR-STR                                
065842                                               TO W-IDARTNR-STR           
065843                                                  W-IDARTNR-SS            
065844                     MOVE SEQC-IDRADNR                                    
065845                                               TO W-IDRADNR               
065846                     MOVE SEQC-KDSTRRAD                                   
065847                                               TO W-KDSTRRAD              
065848                     PERFORM IMS-GU-SATB01                                
065849                     IF STR-TIBORT = ZERO                                 
065850                     AND STR-IDLEVNR = '1002'                             
065851                        PERFORM IMS-GU-SATB11                             
065852                        MOVE RAD-TISTODAT TO TMP1-YYMMDD                  
065853                        MOVE DAGENS-DATUM        TO TMP2-YYMMDD           
065854                        PERFORM WY2000P1                                  
065855                        IF TMP1-YYMMDD > TMP2-YYMMDD                      
065856                            PERFORM IMS-GET-WDK601-PCB2                   
065857                            PERFORM IMS-GET-WDK611-PCB2                   
065858                            IF ERS-CLAG-KDERS < 09                        
065859*********************    MID-ARTIKEL INGÅR I SATS-ARTIKEL SOM INTE        
065860********************   ÄR ERSÄTTNINGSMÄRKT, DÅ FÅR MAN EJ 09-MÄRKA        
065861********************     MID-ARTIKEL                                      
065862                               MOVE NOO   TO INDATA-SW                    
065863                               MOVE MFS-NUM-FAELT-FEL                     
065864                                                TO MOD-KDERS-ATTR         
065865                            END-IF                                        
065866                        END-IF                                            
065867                     END-IF                                               
065868                  END-IF                                                  
065869                  PERFORM IMS-GN-SATE01                                   
065870                END-PERFORM                                               
065871             ELSE                                                         
065872                 IF WARTC91-KDERS-C1 NOT = 0                              
065873                    MOVE NOO             TO INDATA-SW                     
065874                    MOVE MFS-NUM-FAELT-FEL TO MOD-KDERS-ATTR              
065875                 END-IF                                                   
065876             END-IF                                                       
065877          END-IF                                                          
065880         END-IF                                                           
065881       ELSE                                                               
065882         MOVE ERR-USER-NOT-AUTHORIZED          TO MED-IDMFSFEL            
065883         CALL WMEDKONV USING MED-WMEDAREA                                 
065884         MOVE MED-MFSFEL                       TO MOD-TEMFSFEL            
065885         PERFORM MFS-ROER-EJ-FAELT-UT                                     
065886         PERFORM MFS-ERASE-FIELD-IN                                       
065887       END-IF                                                             
065890       IF MID-KVSKROT-KVAR = ALL '+'                                      
065891       AND MID-SKROT-TEXT         = ALL '+'                               
065892       AND MID-IDKONTO            = ALL '+'                               
065893       AND MID-IDANALYS           = ALL '+'                               
065894         CONTINUE                                                         
065895       ELSE                                                               
065900         PERFORM GD-AUTH-USER-CHECK                                       
066000         IF INDATA-OK                                                     
066100           PERFORM GC-CHECK-IDUSER                                        
066200           IF INDATA-OK                                                   
066300             PERFORM GA-CHECK-INPUT-SCREEN                                
066400             IF INDATA-OK                                                 
066410               PERFORM IMS-GU-WDK722                                      
066420               IF SEGMENT-FOUND                                           
066430                 MOVE XLAG-IDANSK     TO W-SPAR-IDANSK                    
066440               ELSE                                                       
066450                 MOVE ZERO            TO W-SPAR-IDANSK                    
066460               END-IF                                                     
066500               PERFORM IMS-GHU-WDK711                                     
066600               IF SEGMENT-FOUND                                           
066700                 IF SLAG-IDDC-REF > SPACE                                 
066800                   MOVE NOO                      TO INDATA-SW             
066900                   MOVE ERR-REFILL-PART-USE-2359 TO MED-IDMFSFEL          
067000                   CALL WMEDKONV USING MED-WMEDAREA                       
067100                   MOVE MED-MFSFEL               TO MOD-TEMFSFEL          
067200                   PERFORM MFS-ROER-EJ-FAELT-IN                           
067300                   PERFORM MFS-ROER-EJ-FAELT-UT                           
067400                 END-IF                                                   
067500                 IF INDATA-OK                                             
067600                   IF SLAG-FLSKROT-BEORD = JA OR YES                      
067700                     MOVE NOO TO INDATA-SW                                
067800                     MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL          
067900                     CALL WMEDKONV USING MED-WMEDAREA                     
068000                     MOVE MED-MFSFEL TO MOD-TEMFSFEL                      
068100                     PERFORM MFS-ROER-EJ-FAELT-IN                         
068200                     PERFORM MFS-ROER-EJ-FAELT-UT                         
068300                   ELSE                                                   
068400                     PERFORM GB-CHECK-INPUT-WDK711                        
068500                   END-IF                                                 
068600                 END-IF                                                   
068700               ELSE                                                       
068800                 MOVE ERR-UPDATE-NOT-ALLOWED     TO MED-IDMFSFEL          
068900                 CALL WMEDKONV USING MED-WMEDAREA                         
069000                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
069100                 PERFORM MFS-ROER-EJ-FAELT-IN                             
069200                 PERFORM MFS-ROER-EJ-FAELT-UT                             
069300               END-IF                                                     
069400             ELSE                                                         
069500               CALL WMEDKONV USING MED-WMEDAREA                           
069600               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
069700               PERFORM MFS-ROER-EJ-FAELT-IN                               
069800               PERFORM MFS-ROER-EJ-FAELT-UT                               
069910             END-IF                                                       
070000           ELSE                                                           
070100             MOVE ERR-NOT-AUTHORIZED-TO-SCRAP  TO MED-IDMFSINF            
070200             CALL WMEDKONV USING MED-WMEDAREA                             
070300             MOVE MED-MFSINF                   TO MOD-TEMFSINF            
070400             PERFORM MFS-ROER-EJ-FAELT-UT                                 
070500             PERFORM MFS-ERASE-FIELD-IN                                   
070600           END-IF                                                         
070700         ELSE                                                             
070800           MOVE ERR-USER-NOT-AUTHORIZED        TO MED-IDMFSFEL            
070900           CALL WMEDKONV USING MED-WMEDAREA                               
071000           MOVE MED-MFSFEL                     TO MOD-TEMFSFEL            
071100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
071200           PERFORM MFS-ERASE-FIELD-IN                                     
071300         END-IF                                                           
071310       END-IF                                                             
071400     END-IF                                                               
071500     .                                                                    
071600     EJECT                                                                
071700 GA-CHECK-INPUT-SCREEN SECTION.                                           
071800                                                                          
071810     PERFORM GAA-KOLLA-SKROT-AUTO                                         
071820                                                                          
071900     IF MID-KVSKROT-KVAR   = ALL '+'                                      
071910         MOVE MFS-NUM-FIELD-OK       TO MOD-KVSKROT-KVAR-ATTR             
072300     ELSE                                                                 
072400       IF MID-KVSKROT-KVAR NOT NUMERIC                                    
072401         MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVSKROT-KVAR-ATTR             
072402         MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                      
072404         MOVE NOO                    TO INDATA-SW                         
072405       ELSE                                                               
072500         IF MID-KVSKROT-KVAR > +0                                         
072600           MOVE MFS-NUM-FIELD-OK       TO MOD-KVSKROT-KVAR-ATTR           
072700         ELSE                                                             
072800           MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVSKROT-KVAR-ATTR           
072900           MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                    
073000           MOVE NOO                    TO INDATA-SW                       
073010         END-IF                                                           
073100       END-IF                                                             
073200     END-IF                                                               
073300     IF MID-SKROT-TEXT      = ALL '+'                                     
073400       MOVE MFS-ALPHA-FIELD-OK       TO MOD-SKROT-TEXT-ATTR               
073500       MOVE MFS-ERASE-FIELD          TO MOD-SKROT-TEXT                    
073600     ELSE                                                                 
073700       INSPECT MID-SKROT-TEXT                                             
073800                         REPLACING ALL '+' BY SPACE                       
073900       MOVE MFS-ALPHA-FIELD-OK       TO MOD-SKROT-TEXT-ATTR               
074000     END-IF                                                               
074100                                                                          
074200     IF MID-IDKONTO        = ALL '+'                                      
074300       MOVE MFS-NUM-FIELD-OK         TO MOD-IDKONTO-ATTR                  
074400       MOVE MFS-ERASE-FIELD          TO MOD-IDKONTO                       
074500     ELSE                                                                 
074600       INSPECT MID-IDKONTO                                                
074700                         REPLACING LEADING SPACE BY ZERO                  
074800       IF MID-IDKONTO          NUMERIC                                    
074900         MOVE MFS-NUM-FIELD-OK       TO MOD-IDKONTO-ATTR                  
075000       ELSE                                                               
075100         MOVE MFS-NUM-FIELD-WRONG    TO MOD-IDKONTO-ATTR                  
075200         MOVE ERR-FIELDS-ARE-NOT-NUMERIC TO MED-IDMFSFEL                  
075300         MOVE NOO TO INDATA-SW                                            
075400       END-IF                                                             
075500     END-IF                                                               
075600                                                                          
075700     IF MID-IDANALYS        = ALL '+'                                     
075800       MOVE MFS-ALPHA-FIELD-OK       TO MOD-IDANALYS-ATTR                 
075900       MOVE MFS-ERASE-FIELD          TO MOD-IDANALYS                      
076000     ELSE                                                                 
076100       MOVE MFS-ALPHA-FIELD-OK       TO MOD-IDANALYS-ATTR                 
076200     END-IF                                                               
076300                                                                          
076400     IF (MID-IDKONTO      NOT = ALL '+' AND                               
076500         MID-IDANALYS     NOT = ALL '+')                                  
076600       MOVE MSGI-IDFTG TO WS-IDFTG                                        
076700       IF IDFTG-PV                                                        
076800         MOVE 'SEPV'                 TO SAP-KDTRADP                       
076900       ELSE                                                               
077000         IF IDFTG-CN                                                      
077100           MOVE 'CN05'               TO SAP-KDTRADP                       
077200         ELSE                                                             
077300           IF IDFTG-US                                                    
077400             MOVE 'US01'             TO SAP-KDTRADP                       
077500           ELSE                                                           
077600             MOVE 'SEPV'             TO SAP-KDTRADP                       
077700           END-IF                                                         
077800         END-IF                                                           
077900       END-IF                                                             
078000       MOVE SPACE                    TO SAP-IDKST                         
078100       INSPECT MID-IDKONTO                                                
078200                       REPLACING LEADING SPACE BY ZERO                    
078300       MOVE MID-IDKONTO              TO SAP-IDKONTO                       
078400       MOVE MID-IDANALYS             TO SAP-IDANALYS                      
078500       MOVE ZERO                     TO SAP-IDDISTR                       
078600       MOVE SPACE                    TO SAP-KDFAKTYP                      
078700       MOVE ZERO                     TO SAP-IDFTG                         
078800       MOVE SPACE                    TO SAP-IDPROFIT                      
078900       MOVE +2                       TO SAP-KDCALL                        
079000       CALL W411SAP USING SAP-W411SAP SAPC-PCB                            
079100       IF SAP-BEFEL NOT = SPACE                                           
079200*        MOVE SAP-BEFEL TO MOD-TEMFSFEL                                   
079300         MOVE SAP-IDMFSFEL           TO MED-IDMFSFEL                      
079400         MOVE NOO TO INDATA-SW                                            
079500         IF SAP-IDKONTO-OK = NOO                                          
079600           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDKONTO                     
079700           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-IDKONTO-ATTR                
079800         ELSE                                                             
079900           IF SAP-IDANALYS-OK = NOO                                       
080000             MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDKONTO                   
080100             MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-IDANALYS-ATTR             
080200           END-IF                                                         
080300         END-IF                                                           
080400       END-IF                                                             
080500     ELSE                                                                 
080600       IF (MID-IDKONTO       = ALL '+' AND                                
080700           MID-IDANALYS      = ALL '+')                                   
080800         CONTINUE                                                         
080900       ELSE                                                               
081000         MOVE MFS-NUM-FIELD-WRONG    TO MOD-IDKONTO-ATTR                  
081100         MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-IDANALYS-ATTR                 
081200*EJ FYLLT I BÅDA VÄRDEN                                                   
081300         MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                      
081400         MOVE NOO TO INDATA-SW                                            
081500       END-IF                                                             
081600     END-IF                                                               
081700     .                                                                    
081800     EJECT                                                                
081810 GAA-KOLLA-SKROT-AUTO SECTION.                                            
081820     SKIP2                                                                
081830     IF MID-TISKROT-AUTO NOT = ALL '+'                                    
081834       IF MID-TISKROT-AUTO NOT NUMERIC                                    
081860         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TISKROT-AUTO                  
081870         MOVE MFS-NUM-FIELD-WRONG    TO MOD-TISKROT-AUTO-ATTR             
081871         MOVE ERR-FIELDS-ARE-NOT-NUMERIC TO MED-IDMFSFEL                  
081880         MOVE NOO                    TO INDATA-SW                         
081890       ELSE                                                               
081894         MOVE 'AAMMDD'               TO DAT-KDDATFORM                     
081895         MOVE MID-TISKROT-AUTO       TO DAT-I-TIDATUM                     
081897         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
081898                             DAT-O-TIDATUM DAT-KDSVAR                     
081900         IF DAT-KDSVAR-FEL                                                
081902         OR MID-TISKROT-AUTO < DAGENS-DATUM                               
081903            MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TISKROT-AUTO               
081905            MOVE MFS-NUM-FIELD-WRONG TO MOD-TISKROT-AUTO-ATTR             
081906            MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                   
081908            MOVE NOO                 TO INDATA-SW                         
081909         ELSE                                                             
081910            MOVE MFS-NUM-FAELT-RAETT TO MOD-TISKROT-AUTO-ATTR             
081911            MOVE MFS-ERASE-FIELD     TO MOD-TISKROT-AUTO                  
081914         END-IF                                                           
081915       END-IF                                                             
081916     ELSE                                                                 
081917       MOVE MFS-NUM-FAELT-RAETT      TO MOD-TISKROT-AUTO-ATTR             
081918       MOVE MFS-ERASE-FIELD          TO MOD-TISKROT-AUTO                  
081920     END-IF                                                               
081921     .                                                                    
081922     EJECT                                                                
081930 GB-CHECK-INPUT-WDK711 SECTION.                                           
082000                                                                          
082010     IF MID-KVSKROT-KVAR NOT = ALL '+'                                    
082100       MOVE MID-KVSKROT-KVAR           TO W-KVSKROT-KVAR                  
082200       IF SLAG-KDLEVSP > ZERO                                             
082300         MOVE MFS-ALPHA-FIELD-WRONG    TO MOD-KVSKROT-KVAR-ATTR           
082400         MOVE ERR-ORDER-NOT-AVAILABLE  TO MED-IDMFSFEL                    
082500         MOVE NOO TO INDATA-SW                                            
082600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
082700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
082800       ELSE                                                               
082900         IF SLAG-KVSPARR-KVAL        > ZERO                               
083000           COMPUTE W-KVSKROT         = SLAG-KVLS -                        
083100                                       W-KVSKROT-KVAR                     
083200           COMPUTE W-OSPARRAT-ANTAL  = SLAG-KVLS -                        
083300                                       SLAG-KVSPARR-KVAL                  
083400           IF W-KVSKROT              > W-OSPARRAT-ANTAL                   
083500             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KVSKROT-KVAR-ATTR          
083600             MOVE ERR-UPDATE-NOT-ALLOWED    TO MED-IDMFSFEL               
083700             MOVE NOO TO INDATA-SW                                        
083800           END-IF                                                         
083900         ELSE                                                             
084000           IF W-KVSKROT-KVAR         >= SLAG-KVLS                         
084100             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KVSKROT-KVAR-ATTR          
084200             MOVE ERR-UPDATE-NOT-ALLOWED    TO MED-IDMFSFEL               
084300             MOVE NOO TO INDATA-SW                                        
084400           END-IF                                                         
084500         END-IF                                                           
084600         COMPUTE W-KVSKROT            = SLAG-KVLS -                       
084700                                        W-KVSKROT-KVAR                    
084800       END-IF                                                             
084810     END-IF                                                               
084900     IF INDATA-WRONG                                                      
085000       CALL WMEDKONV USING MED-WMEDAREA                                   
085100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
085200     END-IF                                                               
085300     .                                                                    
085400     EJECT                                                                
085500                                                                          
085600 GC-CHECK-IDUSER SECTION.                                                 
085700                                                                          
085800     MOVE W-IDDC                     TO W-IDDC-6327                       
085900     MOVE SPACE                      TO W-SPAR-BEANST-GODK                
086000                                                                          
086100     PERFORM IMS-GU-WDR501-6327                                           
086200     IF SEGMENT-FOUND                                                     
086300        MOVE MSG-SIGNON-USERID       TO W-IDUSER-GODK                     
086400        PERFORM IMS-GNP-WDGX6328                                          
086500        IF SEGMENT-FOUND                                                  
086600           MOVE 6328-BEANST-GODK     TO W-SPAR-BEANST-GODK                
086700        ELSE                                                              
086800           MOVE NOO TO INDATA-SW                                          
086900        END-IF                                                            
087000     ELSE                                                                 
087100        MOVE NOO TO INDATA-SW                                             
087200     END-IF                                                               
087300     .                                                                    
087400     EJECT                                                                
087500                                                                          
087600 GD-AUTH-USER-CHECK SECTION.                                              
087700                                                                          
087800     PERFORM IMS-GU-WDB601                                                
087900     IF SEGMENT-FOUND                                                     
088000       IF DCS-NDC-CN                                                      
088100       OR (DCS-NDC-NA AND DCS-USA)                                        
088200         MOVE MSGI-IDFTG          TO WS-IDFTG                             
088300         IF (DCS-NDC-CN AND IDFTG-CN)                                     
088400         OR (DCS-NDC-NA AND IDFTG-US)                                     
088500         OR MSGI-IDFTG  = WC-IDFTG-PV                                     
088600            CONTINUE                                                      
088700         ELSE                                                             
088800            MOVE NOO              TO INDATA-SW                            
088900         END-IF                                                           
089000       ELSE                                                               
089100         MOVE NOO                 TO INDATA-SW                            
089200       END-IF                                                             
089300     ELSE                                                                 
089400       MOVE NOO                   TO INDATA-SW                            
089500     END-IF                                                               
089600     .                                                                    
089700     EJECT                                                                
089820 H-UPDATE SECTION.                                                        
089900                                                                          
089910     IF MID-TISKROT-AUTO NOT = ALL '+'                                    
089911        PERFORM IMS-GHU-WDK711                                            
089920        MOVE MID-TISKROT-AUTO      TO SLAG-TISKROT-AUTO                   
089924        MOVE MFS-ERASE-FIELD       TO MOD-TISKROT-AUTO                    
089925        PERFORM IMS-REPL-WDK711                                           
089930     END-IF                                                               
089940                                                                          
089950     IF MID-KVSKROT-KVAR NOT = ALL '+'                                    
090000       PERFORM HA-CREATE-IDHTYP-6321                                      
090100                                                                          
090200       MOVE NOO                     TO SLAG-FLSKROT-AUTO                  
090300       MOVE JA                      TO SLAG-FLSKROT-BEORD                 
090400       MOVE DAGENS-DATUM            TO SLAG-TISKROT-BEORD                 
090500       MOVE JA                      TO SLAG-FLREFNYO                      
090600       COMPUTE W-ANTAL-SKROT        = SLAG-KVLS -                         
090700                                      W-KVSKROT-KVAR                      
090800       END-COMPUTE                                                        
090801                                                                          
090900       PERFORM IMS-REPL-WDK711                                            
091000     END-IF                                                               
091200                                                                          
091210     IF  MID-KDERS NOT = ALL '+'                                          
091220         PERFORM HB-SKAPA-TRANS-TILL-1113                                 
091230         MOVE ' SS-CODE CHANGED '   TO MOD-TEMFSINF                       
091240     END-IF                                                               
091250                                                                          
091300* NEDAN EFTER EN CHECK?                                                   
091310     IF  MID-KDERS = ALL '+'                                              
091400       MOVE INF-UPDATE-DONE        TO MED-IDMFSINF                        
091500       CALL WMEDKONV USING MED-WMEDAREA                                   
091600       MOVE MED-MFSINF             TO MOD-TEMFSINF                        
091610     END-IF                                                               
091700     PERFORM MFS-FORM-ATTR                                                
091800     PERFORM MFS-ERASE-FIELD-IN                                           
091900     .                                                                    
092000 HA-CREATE-IDHTYP-6321 SECTION.                                           
092100                                                                          
092200     PERFORM IMS-GU-WDK611                                                
092300* -- WDGX6321 -- WDGX6322                                                 
092400     MOVE '6321'                     TO W-IDHTYP                          
092500*    MOVE MSGI-KDARBTYP-SEC          TO W-KDARBTYP-6321                   
092600     PERFORM IMS-ISRT-WDR501-6321                                         
092700                                                                          
092800     COMPUTE W-DASKROT9-BEORD = 99999999 - DAGENS-DATUM-Y2K               
092900     MOVE W-DASKROT9-BEORD           TO 6322-DASKROT9-BEORD               
093000     PERFORM IMS-ISRT-WDGX6322                                            
093100                                                                          
093200* -- WDGX6324                                                             
093300     MOVE SPACE                      TO 6324-WDGX6324                     
093400     MOVE W-IDARTNR                  TO 6324-IDARTNR                      
093500     MOVE W-IDDC                     TO 6324-IDDC                         
093600     MOVE 1                          TO 6324-KDSTASKR                     
093700     IF MID-SKROT-TEXT NOT    = ALL '+'                                   
093800       MOVE MID-SKROT-TEXT           TO 6324-BELAGINS-DEL                 
093900     ELSE                                                                 
094000       MOVE SPACE                    TO 6324-BELAGINS-DEL                 
094100     END-IF                                                               
094200     MOVE NOO                        TO 6324-FLSKROT-GODK                 
094300     IF (MID-IDANALYS NOT    = ALL '+'                                    
094400     AND MID-IDANALYS > SPACE)                                            
094500        MOVE MID-IDANALYS            TO 6324-IDANALYS                     
094600        MOVE MID-IDKONTO             TO 6324-IDKONTO                      
094700        MOVE SPACE                   TO 6324-IDKST                        
094800     ELSE                                                                 
094900        MOVE ZERO                    TO 6324-IDKONTO                      
095000        MOVE SPACE                   TO 6324-IDANALYS                     
095100                                        6324-IDKST                        
095200     END-IF                                                               
095300                                                                          
095400     MOVE DCS-IDDISTR-SKROT          TO 6324-IDDISTR                      
095500     MOVE DCS-IDKUNDNR-SKROT         TO 6324-IDKUNDNR                     
095610     MOVE W-SPAR-IDANSK              TO 6324-IDPERSON                     
095630                                                                          
095700     MOVE MSG-SIGNON-USERID          TO 6324-IDUSER                       
095800     MOVE ZERO                       TO 6324-KDFRAKT                      
095900     MOVE 1                          TO 6324-KDORDKL                      
096000     MOVE W-KVSKROT                  TO 6324-KVSKROT-BEORD                
096100     MOVE ZERO                       TO 6324-KVSKROT-KVAR                 
096200     MOVE W-KVSKROT-KVAR             TO 6324-KVSKROT-ONDEM                
096300     MOVE W-SPAR-BEANST-GODK         TO 6324-BEANST                       
096400     MOVE CLAG-KDERS                 TO 6324-KDERS-UTG                    
096500                                                                          
096600     MOVE ZERO                       TO W-SDC-KVLS                        
096700                                        W-SDC-KVAKS                       
096800                                        W-SDC-KVOKS                       
096900                                                                          
097000     ADD SLAG-KVLS                   TO W-SDC-KVLS                        
097100     ADD SLAG-KVAKS-SDC              TO W-SDC-KVAKS                       
097200     ADD SLAG-KVAKS-PAV              TO W-SDC-KVAKS                       
097300     ADD SLAG-KVOKS-DAG              TO W-SDC-KVOKS                       
097400     ADD SLAG-KVOKS-BULK             TO W-SDC-KVOKS                       
097500     MOVE ZERO                       TO 6324-SUTPO-TOT                    
097600                                        W-KVOKS                           
097700     COMPUTE 6324-KVTILLG-CDC ROUNDED =                                   
097800     CLAG-KVLS            - CLAG-KVRESS                                   
097900                          - CLAG-KVROS                                    
098000                          - W-KVOKS                                       
098100                                                                          
098200     COMPUTE 6324-KVTILLG-SDC ROUNDED = W-SDC-KVLS -                      
098300                                        W-SDC-KVOKS                       
098400                                                                          
098500     COMPUTE 6324-KVAKS-CDC ROUNDED   =                                   
098600     CLAG-KVAKS-CDC       + CLAG-KVAKS-PAV                                
098700                          + CLAG-KVAKS-T                                  
098800     COMPUTE 6324-KVAKS-SDC ROUNDED   = W-SDC-KVAKS                       
098900                                                                          
099000     MOVE +1 TO BEEMB-IX                                                  
099100     PERFORM UNTIL BEEMB-IX           > 20                                
099200       MOVE SPACE                     TO 6324-BEEMBLEM (BEEMB-IX)         
099300       ADD +1                         TO BEEMB-IX                         
099400     END-PERFORM                                                          
099500     PERFORM IMS-GU-WDN601                                                
099600     IF SEGMENT-FOUND                                                     
099700       PERFORM IMS-GNP-WDN611                                             
099800       MOVE +1                        TO BEEMB-IX                         
099900       PERFORM UNTIL BEEMB-IX         > 20 OR SEGMENT-MISSING             
100000         MOVE KAT-BEEMBLEM            TO 6324-BEEMBLEM (BEEMB-IX)         
100100         ADD +1                       TO BEEMB-IX                         
100200         PERFORM IMS-GNP-WDN611                                           
100300       END-PERFORM                                                        
100400       IF SEGMENT-FOUND                                                   
100500         MOVE 'MORE'                  TO 6324-BEEMBLEM (20)               
100600       END-IF                                                             
100700     END-IF                                                               
100800     PERFORM IMS-ISRT-WDGX6324                                            
100900     .                                                                    
101000     EJECT                                                                
101100 HB-SKAPA-TRANS-TILL-1113 SECTION.                                        
101300                                                                          
101400     MOVE W-IDARTNR               TO PROGSW-MID-IDARTNR-UT                
101500     MOVE 1                       TO PROGSW-MID-DIERS-ERS                 
101600     MOVE 09                      TO PROGSW-MID-KDERS                     
101700     MOVE JA                      TO PROGSW-MID-FLKLAR                    
101800                                                                          
101900     MOVE ALL '+'                 TO PROGSW-MID-IDARTNR-IN                
102000                                     PROGSW-MID-IDAO                      
102100                                     PROGSW-MID-TIERSDAT-PREL             
102200                                     PROGSW-MID-TEARTNOT                  
102210                                                                          
102220     MOVE  1                      TO PROGSW-IX                            
102230     PERFORM UNTIL PROGSW-IX > 09                                         
102240        MOVE ALL '+'              TO PROGSW-MID-IDKORTNR                  
102250                                                     (PROGSW-IX)          
102260                                     PROGSW-MID-IDARTNR-TILLK             
102270                                                     (PROGSW-IX)          
102280                                     PROGSW-MID-DIERS-TILLK               
102290                                                     (PROGSW-IX)          
102291                                     PROGSW-MID-BEERS                     
102292                                                     (PROGSW-IX)          
102293        ADD 1                     TO PROGSW-IX                            
102294     END-PERFORM                                                          
102295                                                                          
102296     PERFORM IMS-ISRT-ALT-PCB                                             
102298     .                                                                    
102299     EJECT                                                                
102300 MFS-ERASE-FIELD-OUT SECTION.                                             
102400*    --- ALLA UTDATA-FÄLT                                                 
102500     MOVE MFS-ERASE-FIELD       TO MOD-BEART-ENG                          
102600                                   MOD-KVLS                               
102700                                   MOD-KVOKS                              
102800                                   MOD-KVDISP                             
102900                                   MOD-KVAKS                              
103000                                   MOD-KVSPANT                            
103100                                   MOD-KVSLUTKP                           
103200                                   MOD-TISLUTKP                           
103300                                   MOD-KDERS                              
103400                                   MOD-FLERSATT                           
103500                                   MOD-FLSKROT-BEORD                      
103600                                   MOD-KVSKROT-KVAR                       
103700                                   MOD-SUSKROT                            
103800                                   MOD-KVSKROT                            
103900                                   MOD-TISKROT                            
104000                                   MOD-TISKROT-BEORD                      
104100     .                                                                    
104200     SKIP3                                                                
104300 MFS-ERASE-FIELD-IN SECTION.                                              
104400*    --- ALLA INDATA-FÄLT                                                 
104500     MOVE MFS-ERASE-FIELD       TO MOD-IDARTNR-IN                         
104600                                   MOD-IDDC-IN                            
104700                                   MOD-KVSKROT-KVAR                       
104800                                   MOD-SKROT-TEXT                         
104900                                   MOD-IDKONTO                            
105000                                   MOD-IDANALYS                           
105010                                   MOD-TISKROT-AUTO                       
105020                                   MOD-KDERS                              
105100     .                                                                    
105200     EJECT                                                                
105300 MFS-ROER-EJ-FAELT-UT SECTION.                                            
105400*    --- ALLA UTDATA-FÄLT                                                 
105500     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEART-ENG                         
105600                                    MOD-KVLS                              
105700                                    MOD-KVOKS                             
105800                                    MOD-KVDISP                            
105900                                    MOD-KVAKS                             
106000                                    MOD-KVSPANT                           
106100                                    MOD-KVSLUTKP                          
106200                                    MOD-TISLUTKP                          
106300                                    MOD-KDERS                             
106400                                    MOD-FLERSATT                          
106500                                    MOD-FLSKROT-BEORD                     
106600                                    MOD-SUSKROT                           
106700                                    MOD-KVSKROT                           
106800                                    MOD-TISKROT                           
106900                                    MOD-TISKROT-BEORD                     
107000                                    MOD-KVSKROT-KVAR                      
107100                                    MOD-SKROT-TEXT                        
107200                                    MOD-IDKONTO                           
107300                                    MOD-IDANALYS                          
107310                                    MOD-TISKROT-AUTO                      
107400     .                                                                    
107500     SKIP3                                                                
107600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
107700*    --- ALLA INDATA-FÄLT                                                 
107800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR-IN                        
107900                                    MOD-IDDC-IN                           
108000                                    MOD-KVSKROT-KVAR                      
108100                                    MOD-SKROT-TEXT                        
108200                                    MOD-IDKONTO                           
108300                                    MOD-IDANALYS                          
108310                                    MOD-TISKROT-AUTO                      
108400     .                                                                    
108500     EJECT                                                                
108600 MFS-FORM-ATTR SECTION.                                                   
108700*    --- ALL INDATA-FIELDS                                                
108800     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KVSKROT-KVAR-ATTR                
108900                                     MOD-SKROT-TEXT-ATTR                  
109000                                     MOD-IDKONTO-ATTR                     
109100                                     MOD-IDANALYS-ATTR                    
109110                                     MOD-TISKROT-AUTO-ATTR                
109200     .                                                                    
109300     SKIP2                                                                
109400 MFS-READ-IN-AGAIN SECTION.                                               
109500*    --- ALL INDATA-FIELDS                                                
109600     MOVE MFS-ADD-READ-FIELD      TO MOD-KVSKROT-KVAR-ATTR                
109700                                     MOD-SKROT-TEXT-ATTR                  
109800                                     MOD-IDKONTO-ATTR                     
109900                                     MOD-IDANALYS-ATTR                    
109910                                     MOD-TISKROT-AUTO-ATTR                
110000     .                                                                    
110100     EJECT                                                                
110200* --- IMS SECTIONS ---                                                    
110300     SKIP3                                                                
110400 IMS-GET-MSG SECTION.                                                     
110500                                                                          
110600     MOVE '  QC' TO GOOD-STATUSCODES                                      
110700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
110800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
110900     PERFORM IMS-STATUSCHECK                                              
111000     .                                                                    
111100     SKIP3                                                                
111200 IMS-INSERT-MSG SECTION.                                                  
111300                                                                          
111400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
111500     MOVE SPACE TO GOOD-STATUSCODES                                       
111600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
111700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
111800     PERFORM IMS-STATUSCHECK                                              
111900     .                                                                    
112000     EJECT                                                                
112100 IMS-GU-WDN601 SECTION.                                                   
112200                                                                          
112300     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
112400          DELIMITED BY SIZE INTO SSA1                                     
112500     MOVE '  GE' TO GOOD-STATUSCODES                                      
112600     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
112700     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
112800     PERFORM IMS-STATUSCHECK                                              
112900     .                                                                    
113000     EJECT                                                                
113100 IMS-GNP-WDN611 SECTION.                                                  
113200                                                                          
113300     STRING 'WDN611  (WDN611KY =' W-WDN611KY-X ')'                        
113400          DELIMITED BY SIZE INTO SSA1                                     
113500     MOVE '  GE' TO GOOD-STATUSCODES                                      
113600     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
113700     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
113800     PERFORM IMS-STATUSCHECK                                              
113900     .                                                                    
114000     EJECT                                                                
114100                                                                          
114200 IMS-GU-WDB601 SECTION.                                                   
114300     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
114400          DELIMITED BY SIZE INTO SSA1                                     
114500     MOVE '  GE' TO GOOD-STATUSCODES                                      
114600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
114700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
114800     PERFORM IMS-STATUSCHECK                                              
114900     .                                                                    
115000     EJECT                                                                
116300 IMS-GU-WDD311 SECTION.                                                   
116400                                                                          
116500     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
116600          DELIMITED BY SIZE INTO SSA1                                     
116700     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
116800          DELIMITED BY SIZE INTO SSA2                                     
116900     MOVE '  GE' TO GOOD-STATUSCODES                                      
117000     CALL CBLTDLI USING GU  WDD3-PCB DLI-IO-WDD311 SSA1 SSA2              
117100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
117200     PERFORM IMS-STATUSCHECK                                              
117300     .                                                                    
117400     EJECT                                                                
117500 IMS-ISRT-WDR501-6321 SECTION.                                            
117600                                                                          
117700     MOVE 'WDR501   '           TO SSA1                                   
117800     MOVE '  II'                TO GOOD-STATUSCODES                       
117900     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDR501-6321 SSA1             
118000     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
118100     PERFORM IMS-STATUSCHECK                                              
118200     .                                                                    
118300     EJECT                                                                
118400 IMS-ISRT-WDGX6322 SECTION.                                               
118500                                                                          
118600     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-X ')'                        
118700            DELIMITED BY SIZE INTO SSA1                                   
118800     MOVE 'WDGX6322 '           TO SSA2                                   
118900     MOVE '  II'                TO GOOD-STATUSCODES                       
119000     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2           
119100     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
119200     PERFORM IMS-STATUSCHECK                                              
119300     SKIP3                                                                
119400     .                                                                    
119500 IMS-ISRT-WDGX6324 SECTION.                                               
119600                                                                          
119700     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-X ')'                        
119800            DELIMITED BY SIZE INTO SSA1                                   
119900     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-X ')'                        
120000            DELIMITED BY SIZE INTO SSA2                                   
120100     MOVE 'WDGX6324'            TO SSA3                                   
120200     MOVE '  '                  TO GOOD-STATUSCODES                       
120300     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6324                     
120400                                      SSA1 SSA2 SSA3                      
120500     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
120600     PERFORM IMS-STATUSCHECK                                              
120700     SKIP3                                                                
120800     .                                                                    
120900     EJECT                                                                
121000 IMS-GU-WDR501-6327 SECTION.                                              
121100                                                                          
121200     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
121300          DELIMITED BY SIZE INTO SSA1                                     
121400     MOVE '  GE' TO GOOD-STATUSCODES                                      
121500     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDR501-6327 SSA1               
121600     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
121700     PERFORM IMS-STATUSCHECK                                              
121800     .                                                                    
121900 IMS-GNP-WDGX6328 SECTION.                                                
122000                                                                          
122100     STRING 'WDGX6328(IDUSERGK= ' W-IDUSER-GODK ')'                       
122200          DELIMITED BY SIZE INTO SSA1                                     
122300     MOVE '  GE' TO GOOD-STATUSCODES                                      
122400     CALL CBLTDLI USING GHNP 6327-PCB DLI-IO-WDGX6328 SSA1                
122500     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
122600     PERFORM IMS-STATUSCHECK                                              
122700     .                                                                    
122800 IMS-GU-WDK601 SECTION.                                                   
122900                                                                          
123000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
123100          DELIMITED BY SIZE INTO SSA1                                     
123200     MOVE '  GE' TO GOOD-STATUSCODES                                      
123300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
123400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
123500     PERFORM IMS-STATUSCHECK                                              
123600     .                                                                    
123700     EJECT                                                                
123800 IMS-GU-WDK611 SECTION.                                                   
123900                                                                          
124000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
124100          DELIMITED BY SIZE INTO SSA1                                     
124200     STRING 'WDK611   '                                                   
124300          DELIMITED BY SIZE INTO SSA2                                     
124400     MOVE '    ' TO GOOD-STATUSCODES                                      
124500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
124600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
124700     PERFORM IMS-STATUSCHECK                                              
124800     .                                                                    
124900     EJECT                                                                
124902 IMS-GNP-WDK611 SECTION.                                                  
124903                                                                          
124904     MOVE 'WDK611   ' TO SSA1                                             
124905     MOVE '  ' TO GOOD-STATUSCODES                                        
124906     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
124907     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
124908     PERFORM IMS-STATUSCHECK                                              
124909     .                                                                    
124910     EJECT                                                                
124911                                                                          
126000 IMS-GU-WDK701 SECTION.                                                   
126100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
126200          DELIMITED BY SIZE INTO SSA1                                     
126300     MOVE '  GE' TO GOOD-STATUSCODES                                      
126400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
126500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
126600     PERFORM IMS-STATUSCHECK                                              
126700     .                                                                    
126800 IMS-GNP-WDK711 SECTION.                                                  
126900                                                                          
127000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
127100          DELIMITED BY SIZE INTO SSA1                                     
127200     MOVE '  GE' TO GOOD-STATUSCODES                                      
127300     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
127400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
127500     PERFORM IMS-STATUSCHECK                                              
127600     .                                                                    
127700     SKIP3                                                                
127800 IMS-GHU-WDK711 SECTION.                                                  
127900                                                                          
128000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
128100          DELIMITED BY SIZE INTO SSA1                                     
128200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
128300          DELIMITED BY SIZE INTO SSA2                                     
128400     MOVE '  GE' TO GOOD-STATUSCODES                                      
128500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
128600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
128700     PERFORM IMS-STATUSCHECK                                              
128800     .                                                                    
128900 IMS-REPL-WDK711 SECTION.                                                 
129000                                                                          
129100     MOVE '  ' TO GOOD-STATUSCODES                                        
129200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
129300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
129400     PERFORM IMS-STATUSCHECK                                              
129500     .                                                                    
129600     EJECT                                                                
129700 IMS-GNP-WDK712 SECTION.                                                  
129800                                                                          
129900     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
130000          DELIMITED BY SIZE INTO SSA1                                     
130100     MOVE '  GE' TO GOOD-STATUSCODES                                      
130200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK712 SSA1                   
130300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
130400     PERFORM IMS-STATUSCHECK                                              
130500     .                                                                    
130600     SKIP3                                                                
130700 IMS-GNP-WDK722 SECTION.                                                  
130800                                                                          
130900     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
131000          DELIMITED BY SIZE INTO SSA1                                     
131100     MOVE '  GE' TO GOOD-STATUSCODES                                      
131200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1                   
131300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
131400     PERFORM IMS-STATUSCHECK                                              
131500     .                                                                    
131501     EJECT                                                                
131510 IMS-GU-WDK722 SECTION.                                                   
131520                                                                          
131522     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
131523            DELIMITED BY SIZE INTO SSA1                                   
131524     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
131525            DELIMITED BY SIZE INTO SSA2                                   
131526     MOVE 'WDK722 '           TO SSA3                                     
131550     MOVE '  GE' TO GOOD-STATUSCODES                                      
131560     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
131570     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
131580     PERFORM IMS-STATUSCHECK                                              
131590     .                                                                    
131591     EJECT                                                                
131592 IMS-ISRT-ALT-PCB SECTION.                                                
131595     MOVE '  ' TO GOOD-STATUSCODES                                        
131596     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
131597     MOVE ALT-STATUS-CODE      TO STATUS-WS                               
131598     PERFORM IMS-STATUSCHECK                                              
131599     .                                                                    
131600     SKIP3                                                                
131601 IMS-GU-SATE01 SECTION.                                                   
131604     STRING 'WLSATE01(WDJ1C1KY>=' W-WDJ1C1KY-MIN                          
131605                    '&WDJ1C1KY<=' W-WDJ1C1KY-MAX ')'                      
131606            DELIMITED BY SIZE INTO SSA1                                   
131607     MOVE '  GE'              TO GOOD-STATUSCODES                         
131608     CALL  CBLTDLI  USING GU   SATE-PCB DLI-IO-SATE01 SSA1                
131609     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
131610     PERFORM IMS-STATUSCHECK                                              
131611     .                                                                    
131612     SKIP3                                                                
131613 IMS-GN-SATE01 SECTION.                                                   
131616     STRING 'WLSATE01(WDJ1C1KY>=' W-WDJ1C1KY-MIN                          
131617                    '&WDJ1C1KY<=' W-WDJ1C1KY-MAX ')'                      
131618            DELIMITED BY SIZE INTO SSA1                                   
131619     MOVE '  GE'              TO GOOD-STATUSCODES                         
131620     CALL  CBLTDLI  USING GN   SATE-PCB DLI-IO-SATE01 SSA1                
131621     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
131622     PERFORM IMS-STATUSCHECK                                              
131623     .                                                                    
131624     SKIP3                                                                
131625 IMS-GU-SATB01 SECTION.                                                   
131628     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-STR-X ')'                     
131629            DELIMITED BY SIZE INTO SSA1                                   
131630     MOVE '  GE'                 TO GOOD-STATUSCODES                      
131631     CALL  CBLTDLI  USING GU SATB-PCB DLI-IO-SATB01 SSA1                  
131632     MOVE SATB-STATUS-CODE       TO STATUS-WS                             
131633     PERFORM IMS-STATUSCHECK                                              
131634     .                                                                    
131635     SKIP3                                                                
131636 IMS-GU-SATB11 SECTION.                                                   
131639     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-STR-X ')'                     
131640            DELIMITED BY SIZE INTO SSA1                                   
131641     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
131642            DELIMITED BY SIZE INTO SSA2                                   
131643     MOVE '  GE'                 TO GOOD-STATUSCODES                      
131644     CALL  CBLTDLI  USING GU SATB-PCB DLI-IO-SATB11 SSA1 SSA2             
131645     MOVE SATB-STATUS-CODE       TO STATUS-WS                             
131646     PERFORM IMS-STATUSCHECK                                              
131647     .                                                                    
131648     EJECT                                                                
131649 IMS-GET-WDK601-PCB2 SECTION.                                             
131652     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-SS-X ')'                      
131653            DELIMITED BY SIZE INTO SSA1                                   
131654     MOVE '  GE'                 TO GOOD-STATUSCODES                      
131655     CALL CBLTDLI USING GU WDK62-PCB DLI-IO-01 SSA1                       
131656     MOVE WDK62-STATUS-CODE       TO STATUS-WS                            
131657     PERFORM IMS-STATUSCHECK                                              
131658     .                                                                    
131659     SKIP3                                                                
131660 IMS-GET-WDK611-PCB2 SECTION.                                             
131663     MOVE 'WDK611  '             TO SSA1                                  
131664     MOVE '  GE'                 TO GOOD-STATUSCODES                      
131665     CALL CBLTDLI USING GNP WDK62-PCB DLI-IO-11 SSA1                      
131666     MOVE WDK62-STATUS-CODE       TO STATUS-WS                            
131667     PERFORM IMS-STATUSCHECK                                              
131668     .                                                                    
131669     SKIP3                                                                
131670 IMS-STATUSCHECK SECTION.                                                 
131700                                                                          
131800     SET STATUS-IX TO 1                                                   
131900     SEARCH GOOD-STATUS                                                   
132000       AT END                                                             
132100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
132200         DELIMITED BY SIZE INTO ERROR-TEXT                                
132300         CALL FELLOG                                                      
132400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
132500         CONTINUE                                                         
132600     END-SEARCH                                                           
132700     .                                                                    
132710     EJECT                                                                
132800*    -COPY WY2000P1                                                       
