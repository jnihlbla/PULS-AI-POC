000100                                                                          
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2039300.                                                
000400 AUTHOR.         BARSHARANI BISHOYE                                       
000500 DATE-WRITTEN.   18/07/13.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        REFILL INFORMATION FOR A SPECIFIC PART NUMBER AND CDC            
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDK6                                       
001200*        THE PROGRAM READS     WDK7                                       
001300*        THE PROGRAM READS     WDB6                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W2T393                                              
001700*        MID:         W2I39301                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W2O39301                                            
002100**CHANGE HISTORY:                                                         
002200* 15-JUL-21 STORY 2223955 ADD PB-PLAN ON 2393 FOR REFILLED PARTS          
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W2039300'.            
003200                                                                          
003300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  YES                         PIC X       VALUE 'J'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
       77  WS-UPD-VOLUME               PIC X       VALUE 'N'.                   
003800                                                                          
003900*    --- INDEX FOR SCROLL LINES                                           
004000*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004100                                                                          
004200 77  WS-IDARTNR                  PIC X(9)    VALUE SPACES.                
004300 77  WS-IDDC-SAVE                PIC X(2)    VALUE SPACE.                 
004600 77  WS-CDC-SE                   PIC X(2)    VALUE '11'.                  
004600 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACES.                
004700*                                                                         
004800 77  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004900*                                                                         
005000 77  DADATTID                    PIC 9(14).                               
005100 77  TODAYS-DATE-WDT5            PIC S9(8)   VALUE ZERO.                  
005200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005300     88  INDATA-OK                           VALUE 'J'.                   
005400     88  INDATA-WRONG                        VALUE 'N'.                   
005500                                                                          
005600 77  SUPPLR-SW                   PIC X       VALUE 'J'.                   
005700     88  SUPPLR-OK                           VALUE 'J'.                   
005800     88  SUPPLR-FEL                          VALUE 'N'.                   
005900                                                                          
006000 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006100     88  KEYS-OK                             VALUE 'J'.                   
006200     88  KEYS-WRONG                          VALUE 'N'.                   
006300                                                                          
006400 77  WS-TEARTNOT1-FL             PIC X       VALUE 'N'.                   
006500     88 TEARTNOT1-JA                         VALUE 'J'.                   
006600     88 TEARTNOT1-NEJ                        VALUE 'N'.                   
006700                                                                          
006800 77  WS-TEARTNOT2-FL             PIC X       VALUE 'N'.                   
006900     88 TEARTNOT2-JA                         VALUE 'J'.                   
007000     88 TEARTNOT2-NEJ                        VALUE 'N'.                   
007100                                                                          
007200 77  SW-FL-SUPPL                 PIC X       VALUE 'N'.                   
007300     88  FL-SUPPL-JA                         VALUE 'J'.                   
007400     88  FL-SUPPL-NEJ                        VALUE 'N'.                   
007500                                                                          
007600 77  SW-LOCAL-SOURCING           PIC X       VALUE 'N'.                   
007700     88  LOCAL-SOURCING-OK                   VALUE 'J'.                   
007800     88  LOCAL-SOURCING-NOK                  VALUE 'N'.                   
007900                                                                          
008000 77  SW-REF-PART                 PIC X       VALUE 'J'.                   
008100     88  REF-PART-OK                         VALUE 'J'.                   
008200     88  REF-PART-NOK                        VALUE 'N'.                   
008300                                                                          
008400 77  SW-EXP-PART                 PIC X       VALUE 'J'.                   
008500     88  EXP-PART-OK                         VALUE 'J'.                   
008600     88  EXP-PART-NOK                        VALUE 'N'.                   
008700*    -COPY WY2000W2                                                       
008800*    -COPY WY2000W3                                                       
008900 77  WS-TIYYWW                   PIC S9(5)   VALUE ZERO COMP-3.           
009000 77  W-ANTAL-VECKOR              PIC 9(3)    VALUE ZERO COMP-3.           
009100 77  W-TIYYWW                    PIC 9(4)    VALUE ZERO.                  
       77  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17  COMP SYNC.        
009200                                                                          
009300 *01 -COPY WWDCKONS                                                       
009400                                                                          
009500 01  WS-IDDC-GROUP.                                                       
009600     03 WS-IDDC1                 PIC X(1)   VALUE SPACES.                 
009700     03 FILLER                   PIC X(1)   VALUE SPACES.                 
009800                                                                          
009900 01  SAVE-TODAYS-DATUM           PIC 9(06)  VALUE ZERO.                   
010000 01  SAVE-TODAYS-YYWW.                                                    
010100     05  SAVE-TODAYS-YY          PIC 9(02)  VALUE ZERO.                   
010200     05  SAVE-TODAYS-WW          PIC 9(02)  VALUE ZERO.                   
010300 01  SAVE-TODAYS-YYWW-R  REDEFINES  SAVE-TODAYS-YYWW                      
010400                                     PIC 9(04).                           
010500 01  SAVE-DAFINLV-MMYYMMDD       PIC 9(08)  VALUE ZERO.                   
010600 01  SAVE-TIFINLV-YYWWD          PIC 9(05)  VALUE ZERO.                   
010700                                                                          
010800 01  SAVE-TIFINLV-YYWW.                                                   
010900     10  SAVE-TIFINLV-YY           PIC 9(02)  VALUE ZERO.                 
011000     10  SAVE-TIFINLV-WW           PIC 9(02)  VALUE ZERO.                 
011100 01  SAVE-TIFINLV-YYWW-R  REDEFINES  SAVE-TIFINLV-YYWW                    
011200                                  PIC 9(04).                              
011300                                                                          
011400 01  XX-TIFINLV                  PIC X(05)  VALUE SPACE.                  
011500 01  FILLER  REDEFINES  XX-TIFINLV.                                       
011600     05  XX-YYR                   PIC X(02).                              
011700     05  XX-WW                    PIC X(02).                              
011800     05  XX-DAG                   PIC X(01).                              
011900                                                                          
012000 01  WEEK-HELP.                                                           
012100     03  YYWWD                   PIC 9(5).                                
012200     03  FILLER REDEFINES YYWWD.                                          
012300       05  YYWW                  PIC 9(4).                                
012400       05  FILLER REDEFINES YYWW.                                         
012500         07  YY                  PIC 9(2).                                
012600         07  WW                  PIC 9(2).                                
012700       05  D                     PIC 9(1).                                
012800 01  W-YYR-HELP                  PIC 9(4).                                
       01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW2'.          
       01  P-TO-P-SW2.                                                          
           02     P-TO-P2-KVLL             PIC S9(4) COMP SYNC.                 
           02     P-TO-P2-KDZ1             PIC X(1)  VALUE LOW-VALUE.           
           02     P-TO-P2-KDZ2             PIC X(1)  VALUE LOW-VALUE.           
           02     P-TO-P2-KDTRANS          PIC X(8).                            
           02     P-TO-P2-IDTRANS          PIC X(4).                            
           02     P-TO-P2-KDMFSFOR         PIC X(1).                            
           02     MID -COPY W4I28901   -PRE P-TO-P2-                            
012900                                                                          
013000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013100     88  OWN-MID                             VALUE '2393'.                
013200     88  GOOD-MID                            VALUE '2391' '2192'.         
013300     88  HELP-MID                            VALUE '0551'.                
013400     EJECT                                                                
013500 01  WS-VARIABLES.                                                        
013600     03  WS-RED-KVPB-PLAN        PIC Z(5)9.9 VALUE ZERO.                  
013700     03  WS-TODAY-DATE           PIC 9(8)    VALUE ZERO.                  
013800     03  WS-KVPB-PLAN            PIC S9(6)V9(1) VALUE ZERO.               
013900     03  WS-DAPBPLAN             PIC X(8) VALUE SPACES.                   
014000     03  FILLER REDEFINES WS-DAPBPLAN.                                    
014100         05 WS-DAPBPLAN-CC       PIC X(2).                                
014200         05 WS-DAPBPLAN-AAMMDD   PIC X(6).                                
014300     03  W-CLAG-KVPB-PLAN        PIC S9(6)V9(1) COMP-3 VALUE 0.           
014400     03  W-CLAG-DAPBPLAN         PIC 9(8) VALUE ZERO.                     
014500     03  W-CLAG-KVMAD-TOT        PIC S9(6)V9(1) COMP-3 VALUE 0.           
014600                                                                          
014700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
014800 01  GENERAL-SUBPROGRAMS.                                                 
014900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
015300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015400     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
           03  W488PRMA                PIC X(8)    VALUE 'W488PRMA'.            
015500     EJECT                                                                
015600*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
015700*01 -COPY WMEDAREA                                                        
015800     SKIP3                                                                
015900 01  MESSAGE-CODES.                                                       
016000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
016100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
016200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
016300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
016500     03  ERR-NOT-REFILL-PART     PIC X(3)    VALUE '957'.                 
016600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
016700     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
016800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016900     03  ERR-PART-EXPIRED        PIC X(3)    VALUE '018'.                 
017000 01  MEDDELANDEN.                                                         
017100     03  MED-1                   PIC X(40)                                
017200         VALUE 'REFILL ORDER/PROPOSAL EXISTS           '.                 
017300     03  MED-2                   PIC X(40)                                
017400         VALUE 'UPD. NOT ALLOWED,PART NOT LOC. SOURCED '.                 
017500     EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'W488PRMA'.            
           SKIP3                                                                
      *01  -COPY W488PRMA                                                       
           EJECT                                                                
017600 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
017700     SKIP3                                                                
017800*01  -COPY WDATAREA                                                       
017900     EJECT                                                                
018000*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
018100*                                                                         
018200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
018300     SKIP3                                                                
018400*01 -COPY WMSGINIT                                                        
018500     EJECT                                                                
018600*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
018700*01  -COPY WDECAREA                                                       
018800     EJECT                                                                
018900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
019000*                                                                         
019100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019200     SKIP3                                                                
019300*01  MID -COPY W2I39301                                                   
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019600     SKIP3                                                                
019700*01  -COPY WMSGAREA                                                       
019800     EJECT                                                                
019900     03  MOD REDEFINES MSG-AREA.                                          
020000*      05  -COPY W2O39301                                                 
020100     EJECT                                                                
020200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020300     SKIP3                                                                
020400*01  -COPY WMFSAREA                                                       
020500     EJECT                                                                
020600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
020700*                                                                         
020800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020900     SKIP3                                                                
021000 01  KEYS-FOR-DLI.                                                        
021100*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
021200                                                                          
021300     03  W-IDARTNR-X.                                                     
021400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
021500     03  W-KDSEGKEY-X.                                                    
021600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
021700     03  W-IDLAND-X.                                                      
021800         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
021900     03  W-KDNOTTYP-X.                                                    
022000         05  W-KDNOTTYP          PIC S9(01)  COMP-3 VALUE ZERO .          
022100     03  W-IDDC-REF-X.                                                    
022200         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
022300     03  W-IDDC-X.                                                        
022400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
022500     03  W-IDDC1-MIN-X.                                                   
022600         05  W-IDDC1-MIN         PIC X(1)    VALUE LOW-VALUES.            
022700     03  W-IDDC1-MAX-X.                                                   
022800         05  W-IDDC1-MAX         PIC X(1)    VALUE HIGH-VALUES.           
022900     03 W-WDE301KY-MIN-X.                                                 
023000         05  W-IDDC-E3MIN        PIC X(2)    VALUE SPACES.                
023100         05  W-IDPERSON-BUY-MIN  PIC S9(3)   VALUE ZERO COMP-3.           
023200         05  W-KDREFTYP-MIN      PIC X       VALUE SPACE.                 
023300         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
023400         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
023500                                                                          
023600     03 W-WDE301KY-MAX-X.                                                 
023700         05  W-IDDC-E3MAX        PIC X(2)    VALUE SPACES.                
023800         05  W-IDPERSON-BUY-MAX  PIC S9(3)   VALUE +999 COMP-3.           
023900         05  W-KDREFTYP-MAX      PIC X       VALUE HIGH-VALUE.            
024000         05  W-IDARTNR-MAX       PIC S9(9)   VALUE                        
024100                                               +999999999 COMP-3.         
024200         05  W-IDDISTR-MAX       PIC S9(5)   VALUE +99999 COMP-3.         
024300     SKIP2                                                                
024400*    --- STATUS CODES FROM IMS                                            
024500 01  STATUS-WS                   PIC XX.                                  
024600     88  SEGMENT-FOUND                       VALUE '  '.                  
024700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
024800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
024900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
025000     SKIP2                                                                
025100 01  GOOD-STATUSCODES.                                                    
025200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025300     SKIP3                                                                
025400 01  SSA1                        PIC X(64).                               
025500 01  SSA2                        PIC X(64).                               
025600 01  SSA3                        PIC X(64).                               
025700     EJECT                                                                
025800*01  -COPY WWDCKONS                                                       
025900     SKIP3                                                                
026000     EJECT                                                                
026100*01  -COPY WWDC99                                                         
026200     EJECT                                                                
026300*    --- IMS FUNCTION CODES                                               
026400*01  -COPY W0003                                                          
026500     EJECT                                                                
026600*    ---  DLI INPUT-OUTPUT AREA                                           
026700                                                                          
026800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
026900 01  DLI-IO-WDK601.                                                       
027000*    03  -COPY WDK601                                                     
027100     EJECT                                                                
027200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
027300 01  DLI-IO-WDK611.                                                       
027400*    03  -COPY WDK611                                                     
027500     EJECT                                                                
027600                                                                          
027700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601-11'.                   
027800 01  DLI-IO-AREA-WDK601-11.                                               
027900*    03  -COPY WDK601 -PRE K6-                                            
028000*    03  -COPY WDK611 -PRE K6-                                            
028100     EJECT                                                                
028200                                                                          
028300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK625'.                      
028400 01  DLI-IO-WDK625.                                                       
028500*    03  -COPY WDK625                                                     
028600     EJECT                                                                
028700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK629'.                      
028800 01  DLI-IO-WDK629.                                                       
028900*    03  -COPY WDK629                                                     
029000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
029100 01  DLI-IO-WDK701.                                                       
029200*    03  -COPY WDK701                                                     
029300     EJECT                                                                
029400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
029500 01  DLI-IO-WDK711.                                                       
029600*    03  -COPY WDK711                                                     
029700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
029800 01  DLI-IO-WDK712.                                                       
029900*    03  -COPY WDK712                                                     
030000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
030100 01  DLI-IO-WDB601.                                                       
030200*    03  -COPY WDB601                                                     
030300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE301'.                      
030400 01  DLI-IO-WDE301.                                                       
030500*    03  -COPY WDE301                                                     
030600     EJECT                                                                
030700 01  FILLER         PIC X(16)   VALUE 'DLI-IO-WDT501'.                    
030800 01  DLI-IO-WDT501.                                                       
030900*    03  -COPY WDT501   -PRE WDT501-                                      
031000     EJECT                                                                
031100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT511'.                      
031200 01  DLI-IO-WDT511.                                                       
031300*    03  -COPY WDT511                                                     
031400     EJECT                                                                
031500 LINKAGE SECTION.                                                         
031600*01  -COPY W0009   -PRE MSG-                                              
      *01  -COPY W0009   -PRE 4289-                                             
      *01  -COPY W0009   -PRE SYNQ-                                             
031700                                                                          
031800*01  -COPY W0008   -PRE WDP7-                                             
031900     05  FILLER                  PIC X.                                   
032000                                                                          
032100*01  -COPY W0008  -PRE WDK6-                                              
032200     05  FILLER                  PIC X.                                   
032300                                                                          
032400*01  -COPY W0008  -PRE WDK7-                                              
032500     05  FILLER                  PIC X.                                   
032600                                                                          
032700*01  -COPY W0008  -PRE WDB6-                                              
032800     05  FILLER                  PIC X.                                   
032900                                                                          
033000*01  -COPY W0008  -PRE WDE3-                                              
033100     05  FILLER                  PIC X.                                   
033200     EJECT                                                                
033300*01  -COPY W0008  -PRE WDT5-                                              
033400      05 FILLER                  PIC X.                                   
      *    PCB'ER FÖR SUBPGM                                                    
       01  SYNQ-ATAB-PCB                       PIC X.                           
       01  SYNQ-WDK6-PCB                       PIC X.                           
       01  SYNQ-WDD3-PCB                       PIC X.                           
033500                                                                          
033600 PROCEDURE DIVISION  USING MSG-PCB 4289-PCB SYNQ-PCB                      
033600                           WDP7-PCB WDK6-PCB WDK7-PCB                     
033700                           WDB6-PCB WDE3-PCB WDT5-PCB                     
                                 SYNQ-ATAB-PCB SYNQ-WDK6-PCB                    
                                 SYNQ-WDD3-PCB                                  
                                 .                                              
033800 MAIN SECTION.                                                            
033900     ENTRY 'DLITCBL' USING MSG-PCB 4289-PCB SYNQ-PCB                      
033900                           WDP7-PCB WDK6-PCB WDK7-PCB                     
034000                           WDB6-PCB WDE3-PCB WDT5-PCB                     
                                 SYNQ-ATAB-PCB SYNQ-WDK6-PCB                    
                                 SYNQ-WDD3-PCB                                  
                                 .                                              
034100                                                                          
034200     PERFORM IMS-GET-MSG                                                  
034300     IF SEGMENT-FOUND                                                     
034400       PERFORM A-INIT                                                     
034500       PERFORM B-CHECK-KEYS                                               
034600       IF KEYS-OK                                                         
034700         IF MFS-UPDATE                                                    
034800           PERFORM G-CHECK-INPUT                                          
034900           IF INDATA-OK                                                   
035000             PERFORM H-UPDATE                                             
035100           END-IF                                                         
035200         ELSE                                                             
035300           IF MFS-FIRST                                                   
035400             PERFORM C-FIRST-PAGE                                         
035500           ELSE                                                           
035600               PERFORM E-SAME-PAGE                                        
035700           END-IF                                                         
035800         END-IF                                                           
035900         PERFORM F-READ-SHOW-INFO                                         
036000       END-IF                                                             
036100       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O39301 + 4                      
036200       PERFORM IMS-INSERT-MSG                                             
036300     END-IF                                                               
036400                                                                          
036500     MOVE ZERO TO RETURN-CODE                                             
036600     GOBACK                                                               
036700     .                                                                    
036800     EJECT                                                                
036900 A-INIT SECTION.                                                          
037000                                                                          
037100     IF MSG-DOUBLE-TRANSACTIONS                                           
037200       MOVE MSG-INDATA-MINUS-2-TRANSACT TO MID-W2I39301                   
037300       MOVE MSG-IDTRANS-2               TO MFS-IDTRANS                    
037400       MOVE MSG-KDMFSFOR-2              TO MFS-KDMFSFOR                   
037500     ELSE                                                                 
037600       MOVE MSG-INDATA-MINUS-1-TRANSACT TO MID-W2I39301                   
037700       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
037800       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
037900     END-IF                                                               
038000                                                                          
038100     MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                    
038200     MOVE MSG-IDPFK                     TO MFS-IDPFK                      
038300     MOVE MFS-IDTRANS                   TO W-IDTRANS                      
038400                                                                          
038500     MOVE LOW-VALUE                     TO MSG-AREA                       
038600     MOVE 'W2O393N1'                    TO MFS-IDMOD                      
038700     MOVE '2393'                        TO MOD-IDTRANS                    
038800     MOVE MFS-ERASE-FIELD               TO MOD-TEMFSFEL                   
038900                                           MOD-TEMFSINF                   
039000     IF OWN-MID OR HELP-MID                                               
039100       CONTINUE                                                           
039200     ELSE                                                                 
039300       MOVE SPACE                       TO MFS-KDTRTYP                    
039400       MOVE '7'                         TO MFS-IDPFK                      
039500     END-IF                                                               
039600                                                                          
039700     ACCEPT TODAYS-DATE FROM DATE                                         
039800                                                                          
039900     MOVE TODAYS-DATE           TO DAT-I-TIDATUM                          
040000     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
040100     PERFORM S99-WDATKONV                                                 
040200                                                                          
040300     IF DAT-KDSVAR-OK                                                     
040400        MOVE DAT-TIAA-VECKA     TO SAVE-TODAYS-YY                         
040500        MOVE DAT-TIVV           TO SAVE-TODAYS-WW                         
040600        MOVE DAT-TIAAMMDD       TO WS-TODAY-DATE                          
040700        MOVE 20                 TO WS-TODAY-DATE(1:2)                     
040800     END-IF                                                               
040900     .                                                                    
041000     EJECT                                                                
041100 B-CHECK-KEYS SECTION.                                                    
041200                                                                          
041300     MOVE ALL '+'                       TO MSGI-WMSGINIT                  
041400     MOVE '001'                         TO MSGI-KDCALL                    
041500     MOVE MSG-LTERM-NAME                TO MSGI-IDLTERM-USER              
041600     MOVE MSG-SIGNON-USERID             TO MSGI-IDUSER                    
041700     MOVE '2393'                        TO MSGI-IDTRANS                   
041800     IF OWN-MID                                                           
041900     OR (MID-IDARTNR-IN NUMERIC                                           
042000     AND MID-IDARTNR-IN > ZERO)                                           
042100         MOVE MID-IDARTNR-IN            TO MSGI-IDARTNR                   
042200     END-IF                                                               
042300     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
042400                                                                          
042500     MOVE 'GB '                         TO MED-IDSKYLT                    
042600                                                                          
042700     MOVE YES                           TO KEYS-SW                        
042800                                                                          
042900*    -- CHECK OF IDARTNR                                                  
043000     MOVE MFS-ERASE-FIELD               TO MOD-IDARTNR-IN                 
043100                                                                          
043200     IF MID-IDARTNR-IN           NOT =  ALL '+'                           
043300       MOVE '7'                         TO MFS-IDPFK                      
043400       MOVE SPACE                       TO MFS-KDTRTYP                    
043500     END-IF                                                               
043600     MOVE MSGI-IDARTNR                  TO WS-IDARTNR                     
043700     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
043800     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
043900       MOVE WS-IDARTNR                  TO W-IDARTNR                      
044000     ELSE                                                                 
044100       MOVE NOO                         TO KEYS-SW                        
044200     END-IF                                                               
044300                                                                          
044400     IF KEYS-OK                                                           
044500       PERFORM BA-CHECK-REFILL-PART                                       
044600       MOVE WS-IDARTNR                  TO MOD-IDARTNR-UT                 
044700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
044800     ELSE                                                                 
044900       MOVE MFS-ERASE-FIELD             TO MOD-IDARTNR-UT                 
045000     END-IF                                                               
045100                                                                          
045200     IF  KEYS-WRONG                                                       
045300     AND REF-PART-OK                                                      
045400     AND EXP-PART-OK                                                      
045500       MOVE ERR-WRONG-KEY               TO MED-IDMFSFEL                   
045600       CALL WMEDKONV USING MED-WMEDAREA                                   
045700       MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                   
045800       PERFORM MFS-ERASE-FIELD-IN                                         
045900       PERFORM MFS-ERASE-FIELD-OUT                                        
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300                                                                          
046400 BA-CHECK-REFILL-PART SECTION.                                            
046500                                                                          
046600     PERFORM IMS-GU-WDK611                                                
046700     IF SEGMENT-FOUND                                                     
046800      IF CLAG-IDDC-REF  = SPACES                                          
046900         MOVE NOO                       TO KEYS-SW                        
047000                                           SW-REF-PART                    
047100         MOVE ERR-NOT-REFILL-PART       TO MED-IDMFSFEL                   
047200         CALL WMEDKONV USING MED-WMEDAREA                                 
047300         MOVE MED-MFSFEL                TO MOD-TEMFSFEL                   
047400         PERFORM MFS-ERASE-FIELD-OUT                                      
047500         PERFORM MFS-CLOSE-FIELD-IN                                       
047600      ELSE                                                                
047700         MOVE CLAG-IDDC-REF             TO W-IDDC                         
047800                                           W-IDDC-REF                     
047900      END-IF                                                              
048000     ELSE                                                                 
048100         PERFORM BAA-CHECK-EXPIRED-PART                                   
048200     END-IF                                                               
048300     .                                                                    
048400     EJECT                                                                
048500                                                                          
048600 BAA-CHECK-EXPIRED-PART SECTION.                                          
048700                                                                          
048800     MOVE NOO                           TO KEYS-SW                        
048900                                           SW-EXP-PART                    
049000     MOVE ERR-PART-EXPIRED              TO MED-IDMFSFEL                   
049100     CALL WMEDKONV USING MED-WMEDAREA                                     
049200     MOVE MED-MFSFEL                    TO MOD-TEMFSFEL                   
049300     PERFORM MFS-ERASE-FIELD-OUT                                          
049400     PERFORM MFS-CLOSE-FIELD-IN                                           
049500     .                                                                    
049600     EJECT                                                                
049700                                                                          
049800 C-FIRST-PAGE SECTION.                                                    
049900                                                                          
050000     PERFORM MFS-ERASE-FIELD-IN                                           
050100     .                                                                    
050200     EJECT                                                                
050300                                                                          
050400 E-SAME-PAGE SECTION.                                                     
050500                                                                          
050600     IF OWN-MID OR HELP-MID                                               
050700       IF  MID-INPUT       = ALL '+'                                      
050800       AND MID-KVPB-PLAN   = ALL '+'                                      
050900       AND MID-TIPBPLAN    = ALL '+'                                      
051000       AND MID-TEARTNOT    = ALL '+'                                      
051100         PERFORM MFS-ERASE-FIELD-IN                                       
051200       ELSE                                                               
051300         MOVE INF-PRESS-PF11            TO MED-IDMFSINF                   
051400         CALL WMEDKONV USING MED-WMEDAREA                                 
051500         MOVE MED-MFSINF                TO MOD-TEMFSFEL                   
051600         PERFORM EA-MID-INDATA-FOR-MOD                                    
051700       END-IF                                                             
051800     ELSE                                                                 
051900      PERFORM MFS-ERASE-FIELD-IN                                          
052000     END-IF                                                               
052100     .                                                                    
052200     EJECT                                                                
052300                                                                          
052400 EA-MID-INDATA-FOR-MOD SECTION.                                           
052500     IF MID-IDLEVNR              NOT =  ALL '+'                           
052600      MOVE MFS-DO-NOT-TOUCH-FIELD       TO MOD-IDLEVNR-IN                 
052700     ELSE                                                                 
052800      MOVE MFS-ERASE-FIELD              TO MOD-IDLEVNR-IN                 
052900     END-IF                                                               
053000     MOVE MFS-ADD-LAES-IN-FAELT         TO MOD-IDLEVNR-IN-ATTR            
053100                                                                          
053200     IF MID-KVSPANT              NOT =  ALL '+'                           
053300      MOVE MFS-DO-NOT-TOUCH-FIELD       TO MOD-KVSPANT-IN                 
053400     ELSE                                                                 
053500      MOVE MFS-ERASE-FIELD              TO MOD-KVSPANT-IN                 
053600     END-IF                                                               
053700     MOVE MFS-ADD-LAES-IN-FAELT         TO MOD-KVSPANT-IN-ATTR            
053800                                                                          
053900     IF MID-IDPERSON-BUY         NOT =  ALL '+'                           
054000      MOVE MFS-DO-NOT-TOUCH-FIELD       TO MOD-IDPERSON-BUY-IN            
054100     ELSE                                                                 
054200      MOVE MFS-ERASE-FIELD              TO MOD-IDPERSON-BUY-IN            
054300     END-IF                                                               
054400     MOVE MFS-ADD-LAES-IN-FAELT         TO                                
054500                                          MOD-IDPERSON-BUY-IN-ATTR        
054600                                                                          
054700     IF MID-TIFINLV              NOT =  ALL '+'                           
054800      MOVE MFS-DO-NOT-TOUCH-FIELD       TO MOD-TIFINLV-IN                 
054900     ELSE                                                                 
055000      MOVE MFS-ERASE-FIELD              TO MOD-TIFINLV-IN                 
055100     END-IF                                                               
055200     MOVE MFS-ADD-LAES-IN-FAELT         TO                                
055300                                          MOD-TIFINLV-IN-ATTR             
055400                                                                          
055500     IF MID-KVPB-PLAN            NOT =  ALL '+'                           
055600       MOVE MFS-DO-NOT-TOUCH-FIELD      TO MOD-KVPB-PLAN-IN               
055700     ELSE                                                                 
055800       MOVE MFS-ERASE-FIELD             TO MOD-KVPB-PLAN-IN               
055900     END-IF                                                               
056000     MOVE MFS-ADD-LAES-IN-FAELT         TO                                
056100                                          MOD-KVPB-PLAN-ATTR              
056200                                                                          
056300     IF MID-TIPBPLAN             NOT =  ALL '+'                           
056400       MOVE MFS-DO-NOT-TOUCH-FIELD      TO MOD-TIPBPLAN-IN                
056500     ELSE                                                                 
056600       MOVE MFS-ERASE-FIELD             TO MOD-TIPBPLAN-IN                
056700     END-IF                                                               
056800     MOVE MFS-ADD-LAES-IN-FAELT         TO                                
056900                                          MOD-TIPBPLAN-ATTR               
057000     IF  MID-TEARTNOT-1          NOT  = ALL '+'                           
057100      IF MID-TEARTNOT-1          NOT  =    MOD-TEARTNOT1-IN               
057200       MOVE MID-TEARTNOT-1              TO MOD-TEARTNOT1-IN               
057300       MOVE MFS-ADD-LAES-IN-FAELT       TO MOD-TEARTNOT1-IN-ATTR          
057400       MOVE YES                         TO WS-TEARTNOT1-FL                
057500      END-IF                                                              
057600     ELSE                                                                 
057700      MOVE MFS-ERASE-FIELD              TO MOD-TEARTNOT1-IN               
057800     END-IF                                                               
057900                                                                          
058000     IF  MID-TEARTNOT-2          NOT  = ALL '+'                           
058100      IF MID-TEARTNOT-2          NOT  =    MOD-TEARTNOT2-IN               
058200       MOVE MID-TEARTNOT-2              TO MOD-TEARTNOT2-IN               
058300       MOVE MFS-ADD-LAES-IN-FAELT       TO MOD-TEARTNOT2-IN-ATTR          
058400       MOVE YES                         TO WS-TEARTNOT2-FL                
058500      END-IF                                                              
058600     ELSE                                                                 
058700      MOVE MFS-ERASE-FIELD              TO MOD-TEARTNOT2-IN               
058800     END-IF                                                               
058900     .                                                                    
059000     EJECT                                                                
059100                                                                          
059200 F-READ-SHOW-INFO SECTION.                                                
059300                                                                          
059400     PERFORM IMS-GU-WDK601                                                
059500     IF SEGMENT-FOUND                                                     
059600      MOVE ART-IDLEVNR                  TO MOD-IDLEVNR                    
059700      MOVE ART-TIFINLV                  TO MOD-TIFINLV                    
059800      PERFORM FA-READ-WDK611                                              
059900      PERFORM FB-READ-WDK625                                              
060000      PERFORM FC-READ-WDK629                                              
060100     ELSE                                                                 
060200      MOVE ERR-PART-MISSING             TO MED-IDMFSFEL                   
060300      CALL WMEDKONV USING MED-WMEDAREA                                    
060400      MOVE MED-TEMFSFEL                 TO MOD-TEMFSFEL                   
060500      PERFORM MFS-ERASE-FIELD-IN                                          
060600      PERFORM MFS-ERASE-FIELD-OUT                                         
060700     END-IF                                                               
060800     .                                                                    
060900     EJECT                                                                
061000                                                                          
061100 FA-READ-WDK611  SECTION.                                                 
061200                                                                          
061300     PERFORM IMS-GU-WDK611                                                
061400     IF SEGMENT-FOUND                                                     
061500      MOVE CLAG-KVSPANT                 TO MOD-KVSPANT                    
061600      MOVE CLAG-IDDC-REF                TO W-IDDC-REF                     
061700         IF CLAG-DAPBPLAN      > WS-TODAY-DATE                            
061800         OR CLAG-DAPBPLAN      = WS-TODAY-DATE                            
061900            MOVE CLAG-DAPBPLAN (3:6)                                      
062000                                        TO MOD-TIPBPLAN-UT                
062100            MOVE CLAG-KVPB-PLAN                                           
062200                                        TO WS-RED-KVPB-PLAN               
062300            MOVE WS-RED-KVPB-PLAN                                         
062400                                        TO MOD-KVPB-PLAN-UT               
062500            INSPECT MOD-KVPB-PLAN-UT                                      
062600                          REPLACING LEADING ZERO BY SPACE                 
062700         ELSE                                                             
062800            MOVE MFS-ERASE-FIELD        TO MOD-KVPB-PLAN-UT               
062900                                           MOD-TIPBPLAN-UT                
063000         END-IF                                                           
063100     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400                                                                          
063500 FB-READ-WDK625 SECTION.                                                  
063600                                                                          
063700     IF TEARTNOT1-NEJ                                                     
063800      MOVE 1                            TO W-KDNOTTYP                     
063900      PERFORM IMS-GU-WDK625                                               
064000      IF SEGMENT-FOUND                                                    
064100        MOVE NOT-TEARTNOT               TO MOD-TEARTNOT1-IN               
064200      ELSE                                                                
064300        MOVE MFS-ERASE-FIELD            TO MOD-TEARTNOT1-IN               
064400      END-IF                                                              
064500     END-IF                                                               
064600                                                                          
064700     IF TEARTNOT2-NEJ                                                     
064800      MOVE 2                            TO W-KDNOTTYP                     
064900      PERFORM IMS-GU-WDK625                                               
065000      IF SEGMENT-FOUND                                                    
065100       MOVE NOT-TEARTNOT                TO MOD-TEARTNOT2-IN               
065200      ELSE                                                                
065300       MOVE MFS-ERASE-FIELD             TO MOD-TEARTNOT2-IN               
065400      END-IF                                                              
065500     END-IF                                                               
065600     .                                                                    
065700     EJECT                                                                
065800                                                                          
065900 FC-READ-WDK629 SECTION.                                                  
066000                                                                          
066100     PERFORM IMS-GU-WDK629                                                
066200     IF SEGMENT-FOUND                                                     
066300      MOVE CREF-IDPERSON-BUY            TO MOD-IDPERSON-BUY               
066400      MOVE CREF-KVPB-PLAN               TO MOD-KVPB-MASK                  
066500     ELSE                                                                 
066600      MOVE MFS-ERASE-FIELD              TO MOD-IDPERSON-BUY               
066700                                           MOD-KVPB-MASK                  
066800     END-IF                                                               
066900     .                                                                    
067000     EJECT                                                                
067100                                                                          
067200 G-CHECK-INPUT SECTION.                                                   
067300                                                                          
067400     MOVE YES                           TO INDATA-SW                      
067500     IF  MID-INPUT       = ALL '+'                                        
067600     AND MID-TEARTNOT    = ALL '+'                                        
067700     AND MID-KVPB-PLAN  = ALL '+'                                         
067800     AND MID-TIPBPLAN    = ALL '+'                                        
067900       MOVE ERR-PF11-AND-NO-DATA        TO MED-IDMFSFEL                   
068000       CALL WMEDKONV USING MED-WMEDAREA                                   
068100       MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                   
068200       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
068300       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
068400       MOVE NOO                         TO INDATA-SW                      
068500     ELSE                                                                 
068600       PERFORM GA-CHECK-INPUT-FIELDS                                      
068700       IF INDATA-WRONG                                                    
068800         MOVE ERR-CORR-HILITE-FLDS      TO MED-IDMFSFEL                   
068900         CALL WMEDKONV USING MED-WMEDAREA                                 
069000         MOVE MED-MFSFEL                TO MOD-TEMFSFEL                   
069100         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
069200         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
069300       ELSE                                                               
069400          PERFORM GB-CHECK-DATABASES                                      
069500          IF INDATA-WRONG                                                 
069600            PERFORM MFS-DONT-TOUCH-FIELD-OUT                              
069700            PERFORM MFS-DONT-TOUCH-FIELD-IN                               
069800          END-IF                                                          
069900       END-IF                                                             
070000     END-IF                                                               
070100     .                                                                    
070200     EJECT                                                                
070300                                                                          
070400 GA-CHECK-INPUT-FIELDS SECTION.                                           
070500*SUPPLIER                                                                 
070600                                                                          
070700     IF MID-IDLEVNR              NOT =  ALL '+'                           
070800      IF MID-IDLEVNR                 =  SPACE                             
070900        MOVE MFS-ALPHA-FIELD-WRONG      TO MOD-IDLEVNR-IN-ATTR            
071000        MOVE NOO                        TO INDATA-SW                      
071100      ELSE                                                                
071200        MOVE MFS-ALPHA-FIELD-OK         TO MOD-IDLEVNR-IN-ATTR            
071300      END-IF                                                              
071400     ELSE                                                                 
071500       MOVE MFS-ALPHA-FIELD-OK          TO MOD-IDLEVNR-IN-ATTR            
071600     END-IF                                                               
071700                                                                          
071800*BLOCK QTY                                                                
071900                                                                          
072000     IF MID-KVSPANT              NOT =  ALL '+'                           
072100      IF MID-KVSPANT             NOT    NUMERIC                           
072200        MOVE MFS-NUM-FIELD-WRONG        TO MOD-KVSPANT-IN-ATTR            
072300        MOVE NOO                        TO INDATA-SW                      
072400      ELSE                                                                
072500        MOVE MFS-NUM-FIELD-OK           TO MOD-KVSPANT-IN-ATTR            
072600      END-IF                                                              
072700     ELSE                                                                 
072800        MOVE MFS-NUM-FIELD-OK           TO MOD-KVSPANT-IN-ATTR            
072900     END-IF                                                               
073000                                                                          
073100*BUYER                                                                    
073200     IF MID-IDPERSON-BUY         NOT =  ALL '+'                           
073300      IF MID-IDPERSON-BUY        NOT    NUMERIC                           
073400        MOVE MFS-NUM-FIELD-WRONG        TO                                
073500                                         MOD-IDPERSON-BUY-IN-ATTR         
073600        MOVE NOO                        TO INDATA-SW                      
073700      ELSE                                                                
073800        MOVE MFS-NUM-FIELD-OK           TO                                
073900                                         MOD-IDPERSON-BUY-IN-ATTR         
074000      END-IF                                                              
074100     ELSE                                                                 
074200      MOVE MFS-NUM-FIELD-OK             TO                                
074300                                         MOD-IDPERSON-BUY-IN-ATTR         
074400     END-IF                                                               
074500                                                                          
074600*PUB WEEK                                                                 
074700     IF MID-TIFINLV              NOT =  ALL '+'                           
074800      IF MID-TIFINLV             NOT    NUMERIC                           
074900        MOVE MFS-NUM-FIELD-WRONG        TO                                
075000                                         MOD-TIFINLV-IN-ATTR              
075100        MOVE NOO                        TO INDATA-SW                      
075200      ELSE                                                                
075300        MOVE MFS-NUM-FIELD-OK           TO                                
075400                                         MOD-TIFINLV-IN-ATTR              
075500      END-IF                                                              
075600     ELSE                                                                 
075700      MOVE MFS-NUM-FIELD-OK             TO                                
075800                                         MOD-TIFINLV-IN-ATTR              
075900     END-IF                                                               
076000                                                                          
076100* KVPB-PLAN                                                               
076200     PERFORM GAA-VALIDATE-KVPB-PLAN                                       
076300* TIPBPLAN                                                                
076400     PERFORM GAB-VALIDATE-TIPB-PLAN                                       
076500*NOTE                                                                     
076600     IF MID-TEARTNOT-1        =  ALL '+'                                  
076700      MOVE MFS-ERASE-FIELD              TO MOD-TEARTNOT1-IN               
076800     ELSE                                                                 
076900      MOVE MID-TEARTNOT-1               TO MOD-TEARTNOT1-IN               
077000      MOVE MFS-ALPHA-FIELD-OK           TO MOD-TEARTNOT1-IN-ATTR          
077100     END-IF                                                               
077200                                                                          
077300     IF MID-TEARTNOT-2        =  ALL '+'                                  
077400      MOVE MFS-ERASE-FIELD              TO MOD-TEARTNOT2-IN               
077500     ELSE                                                                 
077600      MOVE MID-TEARTNOT-2               TO MOD-TEARTNOT2-IN               
077700      MOVE MFS-ALPHA-FIELD-OK           TO MOD-TEARTNOT2-IN-ATTR          
077800     END-IF                                                               
077900     .                                                                    
078000     EJECT                                                                
078100                                                                          
078200 GAA-VALIDATE-KVPB-PLAN SECTION.                                          
078300     MOVE ZERO                           TO WS-KVPB-PLAN                  
078400     IF MID-KVPB-PLAN NOT = ALL '+'                                       
078500         MOVE MID-KVPB-PLAN              TO DEC-IDFRIDATA                 
078600                                                                          
078700         MOVE 6                          TO DEC-KVHELTAL                  
078800         MOVE 1                          TO DEC-KVDECIMAL                 
078900         CALL WDECEDIT USING DEC-WDECAREA                                 
079000         IF DEC-KDSVAR-OK                                                 
079100             MOVE MFS-NUM-FIELD-OK       TO                               
079200                                            MOD-KVPB-PLAN-ATTR            
079300             MOVE DEC-IDEDITDATA         TO WS-RED-KVPB-PLAN              
079400                                            WS-KVPB-PLAN                  
079500             MOVE WS-KVPB-PLAN           TO W-CLAG-KVPB-PLAN              
079600             MOVE WS-RED-KVPB-PLAN       TO MOD-KVPB-PLAN-UT              
079700             INSPECT MOD-KVPB-PLAN-UT REPLACING                           
079800                                         LEADING ZERO BY SPACE            
079900         ELSE                                                             
080000             MOVE NOO                    TO INDATA-SW                     
080100             MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVPB-PLAN-ATTR            
080200             MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                  
080300          END-IF                                                          
080400     ELSE                                                                 
080500         MOVE MFS-NUM-FIELD-OK           TO MOD-KVPB-PLAN-ATTR            
080600     END-IF                                                               
080700     .                                                                    
080800     EJECT                                                                
080900 GAB-VALIDATE-TIPB-PLAN SECTION.                                          
081000     MOVE SPACES                        TO WS-DAPBPLAN                    
081100     IF MID-TIPBPLAN NOT = ALL '+'                                        
081200         MOVE MID-TIPBPLAN              TO WS-DAPBPLAN-AAMMDD             
081300                                                                          
081400         MOVE 'AAMMDD'                  TO DAT-KDDATFORM                  
081500         MOVE WS-DAPBPLAN-AAMMDD        TO DAT-I-TIDATUM                  
081600                                                                          
081700         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
081800                            DAT-O-TIDATUM DAT-KDSVAR                      
081900                                                                          
082000         IF DAT-KDSVAR-OK                                                 
082100***ADD CENTURY*                                                           
082200             MOVE 20                    TO WS-DAPBPLAN-CC                 
082300         ELSE                                                             
082400             MOVE NOO                   TO INDATA-SW                      
082500             MOVE MFS-NUM-FIELD-WRONG   TO MOD-TIPBPLAN-ATTR              
082600             MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                   
082700                                                                          
082800         END-IF                                                           
082900     ELSE                                                                 
083000         MOVE MFS-NUM-FIELD-OK          TO MOD-TIPBPLAN-ATTR              
083100     END-IF                                                               
083200     .                                                                    
083300     EJECT                                                                
083400 GB-CHECK-DATABASES  SECTION.                                             
083500                                                                          
083600     IF INDATA-OK                                                         
083700      IF MID-IDLEVNR             NOT  = ALL '+'                           
083800        MOVE NOO                        TO SW-FL-SUPPL                    
083900        PERFORM IMS-GN-WDB601                                             
084000        PERFORM UNTIL SEGMENT-SLUT                                        
084100          IF DCS-IDLEVNR-DC = MID-IDLEVNR                                 
084200            MOVE DCS-IDDC               TO WS-IDDC                        
084300                                           WS-IDDC-GROUP                  
084400            IF DCS-NDC                                                    
084700***ONLY DC71 HAVE EXPORT LICENS OUT FROM CHINA                            
084800*           OR DCS-CHINA                                                  
084900               MOVE DCS-IDDC            TO WS-IDDC-SAVE                   
085000               MOVE YES                 TO SW-FL-SUPPL                    
085100               PERFORM S01-CHECK-LOCAL-SOURCING                           
085200            ELSE                                                          
085300*            MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                   
085400*            CALL WMEDKONV USING MED-WMEDAREA                             
085500*            MOVE MED-MFSFEL            TO MOD-TEMFSFEL                   
085600             MOVE MED-2                 TO MOD-TEMFSFEL                   
085700             MOVE NOO                   TO INDATA-SW                      
085800            END-IF                                                        
085900            SET SEGMENT-SLUT            TO TRUE                           
086000          ELSE                                                            
086100            PERFORM IMS-GN-WDB601                                         
086200          END-IF                                                          
086300        END-PERFORM                                                       
086400                                                                          
086500        IF FL-SUPPL-NEJ                                                   
086600*          MOVE ERR-WRONG-KEY           TO MED-IDMFSFEL                   
086700*          CALL WMEDKONV USING MED-WMEDAREA                               
086800*          MOVE MED-MFSFEL              TO MOD-TEMFSFEL                   
086900           MOVE MED-2                   TO MOD-TEMFSFEL                   
087000           MOVE NOO                     TO INDATA-SW                      
087100           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-IDLEVNR-IN-ATTR            
087200        END-IF                                                            
087300                                                                          
087400        IF INDATA-OK                                                      
087500           PERFORM IMS-GU-WDK601-11                                       
087600           IF SEGMENT-FOUND                                               
087700              MOVE WS-CDC-SE            TO W-IDDC-E3MIN                   
087800                                           W-IDDC-E3MAX                   
087900              PERFORM IMS-GN-WDE3-MIN-MAX                                 
088000              PERFORM UNTIL SEGMENT-SLUT                                  
088100                      OR SEGMENT-MISSING                                  
088200                      OR SUPPLR-FEL                                       
088300                IF REF-IDARTNR     = W-IDARTNR                            
088400                   IF REF-IDLEVNR  = K6-ART-IDLEVNR                       
088500                      MOVE MFS-ALPHA-FIELD-WRONG                          
088600                                 TO MOD-IDLEVNR-IN-ATTR                   
088700                      MOVE NOO   TO INDATA-SW                             
088800                                  SUPPLR-SW                               
088900                      MOVE MED-1 TO MOD-TEMFSFEL                          
089000                   ELSE                                                   
089100                      MOVE MFS-ALPHA-FIELD-OK                             
089200                                 TO MOD-IDLEVNR-IN-ATTR                   
089300                   END-IF                                                 
089400                ELSE                                                      
089500                   MOVE MFS-ALPHA-FIELD-OK                                
089600                                 TO MOD-IDLEVNR-IN-ATTR                   
089700                END-IF                                                    
089800                PERFORM IMS-GN-WDE3-MIN-MAX                               
089900              END-PERFORM                                                 
090000           END-IF                                                         
090100        END-IF                                                            
090200      END-IF                                                              
090300                                                                          
090400      IF INDATA-OK                                                        
090500         IF MID-KVPB-PLAN NOT = ALL '+' OR                                
090600            MID-TIPBPLAN NOT = ALL '+'                                    
090700            PERFORM IMS-GU-WDK601-11                                      
090800            IF SEGMENT-FOUND                                              
090900               CONTINUE                                                   
091000            ELSE                                                          
091100               MOVE ERR-PART-MISSING    TO MED-IDMFSFEL                   
091200               CALL WMEDKONV USING MED-WMEDAREA                           
091300               MOVE MED-TEMFSFEL        TO MOD-TEMFSFEL                   
091400               MOVE NOO                 TO INDATA-SW                      
091500               MOVE MFS-NUM-FIELD-WRONG TO MOD-KVPB-PLAN-ATTR             
091600                                           MOD-TIPBPLAN-ATTR              
091700            END-IF                                                        
091800         END-IF                                                           
091900      END-IF                                                              
092000      IF MID-TEARTNOT-1          NOT =  ALL '+'                           
092100        PERFORM IMS-GU-WDK625                                             
092200        IF SEGMENT-FOUND                                                  
092300          IF MID-TEARTNOT-1          NOT = NOT-TEARTNOT                   
092400            MOVE YES                    TO WS-TEARTNOT1-FL                
092500            MOVE MID-TEARTNOT-1         TO MOD-TEARTNOT1-IN               
092600          ELSE                                                            
092700            MOVE MFS-ERASE-FIELD        TO MOD-TEARTNOT1-IN               
092800          END-IF                                                          
092900        ELSE                                                              
093000          MOVE YES                      TO WS-TEARTNOT1-FL                
093100        END-IF                                                            
093200      END-IF                                                              
093300                                                                          
093400      IF MID-TEARTNOT-2          NOT =  ALL '+'                           
093500        PERFORM IMS-GU-WDK625                                             
093600        IF SEGMENT-FOUND                                                  
093700         IF MID-TEARTNOT-2           NOT = NOT-TEARTNOT                   
093800           MOVE YES                     TO WS-TEARTNOT2-FL                
093900           MOVE MID-TEARTNOT-2          TO MOD-TEARTNOT2-IN               
094000         ELSE                                                             
094100           MOVE MFS-ERASE-FIELD         TO MOD-TEARTNOT2-IN               
094200         END-IF                                                           
094300        ELSE                                                              
094400         MOVE YES                       TO WS-TEARTNOT2-FL                
094500        END-IF                                                            
094600      END-IF                                                              
094700                                                                          
094800      IF MID-IDPERSON-BUY        NOT =  ALL '+'                           
094900       PERFORM IMS-GU-WDK629                                              
095000         IF SEGMENT-FOUND                                                 
095100           IF CREF-FLBUYUPD = 'J'                                         
095200                                                                          
095300             MOVE MFS-NUM-FIELD-WRONG   TO                                
095400                             MOD-IDPERSON-BUY-IN-ATTR                     
095500             MOVE NOO                   TO INDATA-SW                      
095600           ELSE                                                           
095700             MOVE MFS-NUM-FIELD-OK          TO                            
095800                                         MOD-IDPERSON-BUY-IN-ATTR         
095900           END-IF                                                         
096000         ELSE                                                             
096100           MOVE MFS-NUM-FIELD-WRONG     TO                                
096200                           MOD-IDPERSON-BUY-IN-ATTR                       
096300           MOVE NOO                     TO INDATA-SW                      
096400         END-IF                                                           
096500       END-IF                                                             
096600     END-IF                                                               
096700                                                                          
096800     PERFORM GBA-CHECK-TIFINLV                                            
096900     .                                                                    
097000     EJECT                                                                
097100                                                                          
097200 GBA-CHECK-TIFINLV SECTION.                                               
097300                                                                          
097400     IF MID-TIFINLV NOT = ALL '+'                                         
097500        IF MID-TIFINLV NUMERIC                                            
097600           MOVE MID-TIFINLV             TO XX-TIFINLV                     
097700           MOVE 1                       TO XX-DAG                         
097800           MOVE XX-TIFINLV              TO SAVE-TIFINLV-YYWWD             
097900           IF SAVE-TIFINLV-YYWWD = 99991                                  
098000              MOVE MFS-NUM-FIELD-OK     TO MOD-TIFINLV-IN-ATTR            
098100              MOVE 99999999             TO SAVE-DAFINLV-MMYYMMDD          
098200              MOVE 99999                TO SAVE-TIFINLV-YYWWD             
098300           ELSE                                                           
098400              MOVE 'AAVVD '             TO DAT-KDDATFORM                  
098500              MOVE SAVE-TIFINLV-YYWWD   TO DAT-I-TIDATUM                  
098600              PERFORM S99-WDATKONV                                        
098700              IF DAT-KDSVAR-OK                                            
098800                 MOVE DAT-TIAA-VECKA    TO SAVE-TIFINLV-YY                
098900                 MOVE DAT-TIVV          TO SAVE-TIFINLV-WW                
099000                 MOVE SAVE-TIFINLV-YYWW-R  TO TMP1-YYWW                   
099100                 MOVE SAVE-TODAYS-YYWW-R   TO TMP2-YYWW                   
099200                 MOVE DAT-TIAAMMDD         TO                             
099300                                      SAVE-DAFINLV-MMYYMMDD               
099400                 MOVE DAT-TISEKEL          TO                             
099500                                      SAVE-DAFINLV-MMYYMMDD (1:2)         
099600                 PERFORM WY2000P3                                         
099700                 IF TMP1-YYWW > TMP2-YYWW                                 
099800                    PERFORM S98-WITHIN-TWO-YEARS                          
099900                    MOVE SAVE-TIFINLV-YYWWD TO TMP1-YYWWD                 
100000                    MOVE YYWWD              TO TMP2-YYWWD                 
100100                    IF TMP1-YYWWD NOT < TMP2-YYWWD                        
100200                       MOVE MFS-NUM-FIELD-WRONG                           
100300                                        TO MOD-TIFINLV-IN-ATTR            
100400                       MOVE NOO         TO INDATA-SW                      
100500                    ELSE                                                  
100600                       MOVE MFS-NUM-FIELD-OK                              
100700                                        TO MOD-TIFINLV-IN-ATTR            
100800                    END-IF                                                
100900                 ELSE                                                     
101000                    MOVE MFS-NUM-FIELD-WRONG                              
101100                                        TO MOD-TIFINLV-IN-ATTR            
101200                    MOVE NOO            TO INDATA-SW                      
101300                 END-IF                                                   
101400              ELSE                                                        
101500                 MOVE MFS-NUM-FIELD-WRONG                                 
101600                                        TO MOD-TIFINLV-IN-ATTR            
101700                 MOVE NOO               TO INDATA-SW                      
101800              END-IF                                                      
101900              MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-TIFINLV-IN              
102000           END-IF                                                         
102100        ELSE                                                              
102200           MOVE MFS-NUM-FIELD-WRONG     TO MOD-TIFINLV-IN-ATTR            
102300           MOVE NOO                     TO INDATA-SW                      
102400        END-IF                                                            
102500        MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-TIFINLV-IN                 
102600     ELSE                                                                 
102700        MOVE MFS-ERASE-FIELD            TO MOD-TIFINLV-IN                 
102800     END-IF                                                               
102900     .                                                                    
103000     EJECT                                                                
103100                                                                          
103200 H-UPDATE SECTION.                                                        
103300                                                                          
103400     IF MID-IDLEVNR              NOT =  ALL '+'                           
103500     OR MID-KVSPANT              NOT =  ALL '+'                           
103600     OR MID-IDPERSON-BUY         NOT =  ALL '+'                           
103700     OR MID-TIFINLV              NOT =  ALL '+'                           
103800     OR MID-KVPB-PLAN            NOT =  ALL '+'                           
103900     OR MID-TIPBPLAN             NOT =  ALL '+'                           
104000     OR MID-TEARTNOT             NOT =  ALL '+'                           
104100        PERFORM HA-UPDATE                                                 
104200     END-IF                                                               
104300                                                                          
104400     MOVE INF-UPDATE-DONE               TO MED-IDMFSINF                   
104500     MOVE 'GB '                         TO MED-IDSKYLT                    
104600     CALL WMEDKONV USING MED-WMEDAREA                                     
104700     MOVE MED-MFSINF                    TO MOD-TEMFSINF                   
104800     PERFORM MFS-FORM-ATTR                                                
104900     PERFORM MFS-ERASE-FIELD-IN                                           
105000     .                                                                    
105100     EJECT                                                                
105200                                                                          
105300 HA-UPDATE SECTION.                                                       
105400                                                                          
105500     IF MID-IDLEVNR              NOT =  ALL '+'                           
105600     OR MID-TIFINLV              NOT =  ALL '+'                           
105700       PERFORM IMS-GHU-WDK601                                             
105800       IF SEGMENT-FOUND                                                   
105900          IF MID-IDLEVNR             NOT =  ALL '+'                       
                  MOVE ART-IDLEVNR            TO WS-IDLEVNR                     
106000            MOVE MID-IDLEVNR            TO ART-IDLEVNR                    
106100                                           GLO-IDUSER                     
106200          END-IF                                                          
106300          IF MID-TIFINLV             NOT =  ALL '+'                       
106400            MOVE SAVE-TIFINLV-YYWWD     TO ART-TIFINLV                    
106500          END-IF                                                          
106600          PERFORM IMS-REPL-WDK601                                         
106700       END-IF                                                             
106800                                                                          
106900       IF MID-IDLEVNR            NOT =  ALL '+'                           
107000          PERFORM IMS-GHU-WDK611                                          
107100          MOVE CLAG-KDARTURS TO GLO-KDARTURS                              
107200                                                                          
107300          IF SEGMENT-FOUND                                                
107400             MOVE WS-IDDC-SAVE          TO CLAG-IDDC-REF                  
107500                                             WS-IDDC                      
107600             IF NDC-CN                                                    
107700                MOVE 'CN'               TO W-IDLAND                       
107800             ELSE                                                         
107900                MOVE 'US'               TO W-IDLAND                       
108000             END-IF                                                       
108100*                                                                         
108200             PERFORM IMS-GU-WDK712                                        
108300             IF SEGMENT-FOUND                                             
108400                IF LART-KDARTURS   > SPACE                                
108500                   MOVE LART-KDARTURS   TO CLAG-KDARTURS                  
108600                                           GLO-KDARTURS                   
108700                END-IF                                                    
108800                IF LART-VKART      > ZERO                                 
108900                   MOVE LART-VKART      TO CLAG-VKART                     
109000                   MOVE MSGI-IDUSER     TO CLAG-IDUSER-VUPD               
109100                   MOVE TODAYS-DATE     TO CLAG-TIUPPDAT-VUPD             
109200                   IF CLAG-VKART-NTO = ZERO OR                            
109300                      CLAG-VKART-NTO > CLAG-VKART                         
109400                     MOVE CLAG-VKART    TO CLAG-VKART-NTO                 
109500                     MOVE '4'           TO CLAG-KDUVKNTO                  
109600                   END-IF                                                 
109700                END-IF                                                    
109800                IF LART-VLARTNTO   > ZERO                                 
                         IF CLAG-VLARTNTO = ZERO                                
                           MOVE 002        TO SYNQ-KDCALL                       
                         ELSE                                                   
                           MOVE 003        TO SYNQ-KDCALL                       
                         END-IF                                                 
109900                   MOVE LART-VLARTNTO   TO CLAG-VLARTNTO                  
110000                   MOVE MSGI-IDUSER     TO CLAG-IDUSER-VUPD               
110100                   MOVE TODAYS-DATE     TO CLAG-TIUPPDAT-VUPD             
                         MOVE YES TO WS-UPD-VOLUME                              
110200                END-IF                                                    
110300             END-IF                                                       
110400**WHEN   YOU CHANGE REFILLING DC ALWAYS SPACE IDLEVNR-SHIP                
110500             MOVE SPACE                 TO CLAG-IDLEVNR-SHIP              
110600             PERFORM IMS-REPL-WDK611                                      
                   IF WS-UPD-VOLUME = YES                                       
                     MOVE W-IDARTNR         TO SYNQ-IDARTNR                     
                     PERFORM HC-STARTA-W40289                                   
                     CALL W488PRMA USING  SYNQ-W488PRMA                         
                     SYNQ-ATAB-PCB SYNQ-WDK6-PCB SYNQ-WDD3-PCB                  
                   END-IF                                                       
                   MOVE NOO TO WS-UPD-VOLUME                                    
110700             IF GLO-KDARTURS > ' '                                        
110800               PERFORM HB-UPDATE-WDT5                                     
110900             END-IF                                                       
111000          END-IF                                                          
111100                                                                          
111200          PERFORM IMS-GHU-WDK629                                          
111300          IF SEGMENT-FOUND                                                
111400             PERFORM IMS-DELT-WDK629                                      
111500             MOVE WS-IDDC-SAVE          TO CREF-IDDC-REF                  
111600             PERFORM IMS-ISRT-WDK629                                      
111700          END-IF                                                          
111800       END-IF                                                             
111900     END-IF                                                               
112000                                                                          
112100     IF MID-KVSPANT             NOT      =  ALL '+'                       
112200       PERFORM IMS-GHU-WDK611                                             
112300        IF SEGMENT-FOUND                                                  
112400          MOVE MID-KVSPANT              TO CLAG-KVSPANT                   
112500          PERFORM IMS-REPL-WDK611                                         
112600        END-IF                                                            
112700     END-IF                                                               
112800                                                                          
112900     IF MID-IDPERSON-BUY        NOT      = ALL '+'                        
113000        PERFORM IMS-GHU-WDK629                                            
113100        IF SEGMENT-FOUND                                                  
113200          MOVE MID-IDPERSON-BUY         TO CREF-IDPERSON-BUY              
113300          IF MID-IDPERSON-BUY NOT = ZERO                                  
113400            MOVE 'J'                    TO CREF-FLBUYUPD                  
113500          END-IF                                                          
113600          PERFORM IMS-REPL-WDK629                                         
113700        END-IF                                                            
113800     END-IF                                                               
113900                                                                          
114000     IF MID-KVPB-PLAN = ALL '+'                                           
114100     AND MID-TIPBPLAN = ALL '+'                                           
114200         CONTINUE                                                         
114300     ELSE                                                                 
114400         PERFORM HAA-UPDATE-PB-PLAN                                       
114500     END-IF                                                               
114600                                                                          
114700     IF TEARTNOT1-JA                                                      
114800        MOVE 1                          TO W-KDNOTTYP                     
114900        PERFORM IMS-GHU-WDK625                                            
115000        IF MID-TEARTNOT-1                = SPACE                          
115100         IF SEGMENT-FOUND                                                 
115200          PERFORM IMS-DELT-WDK625                                         
115300          MOVE SPACE                    TO MOD-TEARTNOT1-IN               
115400          MOVE MFS-ADD-HILIGHT-FIELD    TO MOD-TEARTNOT1-IN-ATTR          
115500         ELSE                                                             
115600           CONTINUE                                                       
115700         END-IF                                                           
115800        ELSE                                                              
115900          MOVE MID-TEARTNOT-1           TO NOT-TEARTNOT                   
116000                                           MOD-TEARTNOT1-IN               
116100          MOVE 1                        TO NOT-KDNOTTYP                   
116200          IF SEGMENT-FOUND                                                
116300             PERFORM IMS-REPL-WDK625                                      
116400          ELSE                                                            
116500             PERFORM IMS-ISRT-WDK625                                      
116600          END-IF                                                          
116700          MOVE MFS-ADD-HILIGHT-FIELD    TO MOD-TEARTNOT1-IN-ATTR          
116800        END-IF                                                            
116900        MOVE NOO                        TO WS-TEARTNOT1-FL                
117000     END-IF                                                               
117100                                                                          
117200     IF TEARTNOT2-JA                                                      
117300        MOVE 2                          TO W-KDNOTTYP                     
117400        PERFORM IMS-GHU-WDK625                                            
117500        IF MID-TEARTNOT-2                = SPACE                          
117600           IF SEGMENT-FOUND                                               
117700             PERFORM IMS-DELT-WDK625                                      
117800             MOVE SPACE                 TO MOD-TEARTNOT2-IN               
117900             MOVE MFS-ADD-HILIGHT-FIELD TO                                
118000                                           MOD-TEARTNOT2-IN-ATTR          
118100           ELSE                                                           
118200             CONTINUE                                                     
118300           END-IF                                                         
118400        ELSE                                                              
118500           MOVE MID-TEARTNOT-2          TO NOT-TEARTNOT                   
118600                                           MOD-TEARTNOT2-IN               
118700           MOVE 2                       TO NOT-KDNOTTYP                   
118800           IF SEGMENT-FOUND                                               
118900              PERFORM IMS-REPL-WDK625                                     
119000           ELSE                                                           
119100              PERFORM IMS-ISRT-WDK625                                     
119200           END-IF                                                         
119300           MOVE MFS-ADD-HILIGHT-FIELD   TO MOD-TEARTNOT2-IN-ATTR          
119400        END-IF                                                            
119500        MOVE NOO                        TO WS-TEARTNOT2-FL                
119600     END-IF                                                               
119700     .                                                                    
119800     EJECT                                                                
119900                                                                          
120000 HAA-UPDATE-PB-PLAN SECTION.                                              
120100                                                                          
120200     PERFORM IMS-GU-WDK611                                                
120300                                                                          
120400     IF MID-KVPB-PLAN = ALL '+'                                           
120500        MOVE CLAG-KVPB-PLAN             TO WS-KVPB-PLAN                   
120600     END-IF                                                               
120700                                                                          
120800     IF MID-TIPBPLAN = ALL '+'                                            
120900        MOVE CLAG-DAPBPLAN              TO WS-DAPBPLAN                    
121000     END-IF                                                               
121100                                                                          
121200                                                                          
121300     MOVE CLAG-KVMAD-TOT                TO W-CLAG-KVMAD-TOT               
121400                                                                          
121500     IF WS-DAPBPLAN > WS-TODAY-DATE                                       
121600     OR WS-DAPBPLAN = WS-TODAY-DATE                                       
121700                                                                          
121800       IF WS-KVPB-PLAN NOT = CLAG-KVPB-PLAN                               
121900       AND WS-KVPB-PLAN > ZERO                                            
122000         IF WS-KVPB-PLAN < CLAG-KVPB-PLAN                                 
122100           COMPUTE W-CLAG-KVMAD-TOT ROUNDED =                             
122200             CLAG-KVMAD-TOT * (WS-KVPB-PLAN / CLAG-KVPB-PLAN)             
122300         END-IF                                                           
122400       END-IF                                                             
122500                                                                          
122600       MOVE WS-KVPB-PLAN                TO W-CLAG-KVPB-PLAN               
122700       MOVE WS-DAPBPLAN                 TO W-CLAG-DAPBPLAN                
122800                                                                          
122900     ELSE                                                                 
123000       IF CLAG-IDDC-REF NOT = SPACE                                       
123100         PERFORM IMS-GU-WDK629                                            
123200         IF SEGMENT-FOUND                                                 
123300           IF CREF-KVPB-PLAN > ZERO                                       
123400             IF CREF-KVPB-PLAN < CLAG-KVPB-PLAN                           
123500               COMPUTE W-CLAG-KVMAD-TOT ROUNDED =                         
123600               CLAG-KVMAD-TOT * (CREF-KVPB-PLAN / CLAG-KVPB-PLAN)         
123700             END-IF                                                       
123800           END-IF                                                         
123900           MOVE CREF-KVPB-PLAN          TO W-CLAG-KVPB-PLAN               
124000           MOVE ZERO                    TO W-CLAG-DAPBPLAN                
124100         ELSE                                                             
124200           MOVE ZERO                    TO W-CLAG-KVPB-PLAN               
124300                                           W-CLAG-DAPBPLAN                
124400         END-IF                                                           
124500       END-IF                                                             
124600     END-IF                                                               
124700                                                                          
124800     PERFORM IMS-GHU-WDK611                                               
124900                                                                          
125000     MOVE W-CLAG-KVPB-PLAN              TO CLAG-KVPB-PLAN                 
125100     MOVE W-CLAG-DAPBPLAN               TO CLAG-DAPBPLAN                  
125200     MOVE W-CLAG-KVMAD-TOT              TO CLAG-KVMAD-TOT                 
125300     PERFORM IMS-REPL-WDK611                                              
125400                                                                          
125500     IF CLAG-IDDC-REF NOT = SPACE                                         
125600       PERFORM IMS-GHNP-WDK629                                            
125700       IF SEGMENT-FOUND                                                   
125800         IF CREF-FLREFNYO = YES                                           
125900           MOVE NOO                     TO CREF-FLREFNYO                  
126000           PERFORM IMS-REPL-WDK629                                        
126100         END-IF                                                           
126200       END-IF                                                             
126300     END-IF                                                               
126400     .                                                                    
126500     EJECT                                                                
126600 HB-UPDATE-WDT5 SECTION.                                                  
126700                                                                          
126800     PERFORM IMS-GU-WDT501                                                
           IF SEGMENT-FOUND                                                     
            PERFORM IMS-GHNP-WDT511                                             
            IF WS-IDLEVNR = SPACES                                              
             MOVE ART-IDLEVNR TO GLO-IDLEVNR                                    
            ELSE                                                                
             MOVE WS-IDLEVNR TO GLO-IDLEVNR                                     
            END-IF                                                              
            PERFORM IMS-REPL-WDT511                                             
           END-IF                                                               
126900     IF SEGMENT-MISSING                                                   
127000        MOVE W-IDARTNR TO WDT501-ARTU-IDARTNR                             
127100        PERFORM IMS-ISRT-WDT501                                           
127200     END-IF                                                               
127300                                                                          
127400     MOVE FUNCTION CURRENT-DATE(1:14) TO DADATTID                         
127500     MOVE FUNCTION CURRENT-DATE(1:8)  TO TODAYS-DATE-WDT5                 
           MOVE SPACES TO GLO-IDLEVNR                                           
127600     COMPUTE GLO-DADATTID-9KOMPL =                                        
127700             99999999999999 - DADATTID                                    
127800                                                                          
127900     PERFORM IMS-ISRT-WDT511                                              
128000     .                                                                    
128100     EJECT                                                                
       HC-STARTA-W40289 SECTION.                                                
                                                                                
           MOVE W-IDARTNR            TO P-TO-P2-MID-IDARTNR-IN                  
           MOVE CLAG-VKART           TO P-TO-P2-MID-VKART-IN                    
           MOVE CLAG-VLARTNTO        TO P-TO-P2-MID-VLARTNTO-IN                 
           MOVE ZERO                 TO P-TO-P2-MID-IDDISTR-IN                  
                                        P-TO-P2-MID-IDKUNDNR-IN                 
                                        P-TO-P2-MID-IDORDNR5-IN                 
                                        P-TO-P2-MID-IDORDER-IN                  
                                        P-TO-P2-MID-IDLOPNR-IN                  
                                        P-TO-P2-MID-IDDC-IN                     
                                        P-TO-P2-MID-ADLAGOMR-IN                 
                                        P-TO-P2-MID-ADGANG-IN                   
                                        P-TO-P2-MID-ADPLATS-IN                  
           COMPUTE P-TO-P2-KVLL   =  LENGTH OF P-TO-P2-MID-W4I28901 + 25        
           END-COMPUTE                                                          
                                                                                
           MOVE 'W4T289X '           TO P-TO-P2-KDTRANS                         
           MOVE '2393'               TO P-TO-P2-IDTRANS                         
           MOVE MFS-KDMFSFOR         TO P-TO-P2-KDMFSFOR                        
                                                                                
           PERFORM IMS-PURG-4289                                                
           .                                                                    
128200                                                                          
128300 S01-CHECK-LOCAL-SOURCING SECTION.                                        
128400                                                                          
128500*UPDATE POSSIBLE ONLY IF PART LOCALLY SOURCED IN US/CN                    
128600*AND REFILLED TO CDC                                                      
128700                                                                          
128800     MOVE NOO                           TO SW-LOCAL-SOURCING              
128900     IF NDC                                                               
129000        MOVE WS-IDDC1                   TO W-IDDC1-MIN                    
129100                                           W-IDDC1-MAX                    
129200     END-IF                                                               
130000                                                                          
130100     PERFORM IMS-GN-WDK711                                                
130200*    PERFORM UNTIL SEGMENT-MISSING OR LOCAL-SOURCING-OK                   
130300     PERFORM UNTIL SEGMENT-MISSING                                        
130400       IF SLAG-IDDC-REF              NOT >  SPACES                        
130500          MOVE YES                      TO SW-LOCAL-SOURCING              
130600       END-IF                                                             
130700       IF SLAG-IDDC = WS-IDDC-SAVE                                        
130800         IF SLAG-IDDC-REF = '11'                                          
130900           MOVE NOO                     TO SW-LOCAL-SOURCING              
131000         END-IF                                                           
131100       END-IF                                                             
131200       PERFORM IMS-GN-WDK711                                              
131300     END-PERFORM                                                          
131400                                                                          
131500     IF LOCAL-SOURCING-NOK                                                
131600*       MOVE ERR-WRONG-KEY              TO MED-IDMFSFEL                   
131700*       CALL WMEDKONV USING MED-WMEDAREA                                  
131800*       MOVE MED-MFSFEL                 TO MOD-TEMFSFEL                   
131900        MOVE MED-2                      TO MOD-TEMFSFEL                   
132000        MOVE NOO                        TO INDATA-SW                      
132100                                           SW-FL-SUPPL                    
132200        MOVE MFS-ALPHA-FIELD-WRONG      TO MOD-IDLEVNR-IN-ATTR            
132300     END-IF                                                               
132400     .                                                                    
132500     EJECT                                                                
132600 S98-WITHIN-TWO-YEARS  SECTION.                                           
132700     SKIP2                                                                
132800     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
132900     PERFORM S99-WDATKONV                                                 
133000     MOVE DAT-TIAAVVD TO YYWWD                                            
133100                                                                          
133200     MOVE FUNCTION CURRENT-DATE (1:4) TO W-YYR-HELP                       
133300     ADD 2 TO W-YYR-HELP                                                  
133400     MOVE W-YYR-HELP (3:2) TO YY                                          
133500                                                                          
133600     IF WW = 53                                                           
133700       MOVE 52 TO WW                                                      
133800     END-IF                                                               
133900     MOVE 1 TO D                                                          
134000     .                                                                    
134100     EJECT                                                                
134200 S99-WDATKONV SECTION.                                                    
134300     SKIP2                                                                
134400     CALL WDATKONV USING DAT-KDDATFORM                                    
134500                         DAT-I-TIDATUM                                    
134600                         DAT-O-TIDATUM                                    
134700                         DAT-KDSVAR                                       
134800     .                                                                    
134900     EJECT                                                                
135000                                                                          
135100                                                                          
135200 MFS-ERASE-FIELD-OUT SECTION.                                             
135300                                                                          
135400     MOVE MFS-ERASE-FIELD               TO MOD-IDLEVNR                    
135500                                           MOD-KVSPANT                    
135600                                           MOD-IDPERSON-BUY               
135700                                           MOD-TIFINLV                    
135800                                           MOD-KVPB-PLAN-UT               
135900                                           MOD-TIPBPLAN-UT                
136000                                           MOD-TEARTNOT1-IN               
136100                                           MOD-TEARTNOT2-IN               
136200     .                                                                    
136300     SKIP3                                                                
136400 MFS-ERASE-FIELD-IN SECTION.                                              
136500                                                                          
136600     MOVE MFS-ERASE-FIELD               TO MOD-IDLEVNR-IN                 
136700                                           MOD-KVSPANT-IN                 
136800                                           MOD-IDPERSON-BUY-IN            
136900                                           MOD-TIFINLV-IN                 
137000                                           MOD-KVPB-PLAN-IN               
137100                                           MOD-TIPBPLAN-IN                
137200                                           MOD-TEARTNOT1-IN               
137300                                           MOD-TEARTNOT2-IN               
137400     .                                                                    
137500     EJECT                                                                
137600 MFS-CLOSE-FIELD-IN SECTION.                                              
137700                                                                          
137800     MOVE MFS-CLOSE-FIELD-NOMOD        TO MOD-IDLEVNR-IN-ATTR             
137900                                          MOD-KVSPANT-IN-ATTR             
138000                                          MOD-IDPERSON-BUY-IN-ATTR        
138100                                          MOD-TIFINLV-IN-ATTR             
138200                                          MOD-KVPB-PLAN-ATTR              
138300                                          MOD-TIPBPLAN-ATTR               
138400                                          MOD-TEARTNOT1-IN-ATTR           
138500                                          MOD-TEARTNOT2-IN-ATTR           
138600     .                                                                    
138700     EJECT                                                                
138800 MFS-DONT-TOUCH-FIELD-OUT SECTION.                                        
138900                                                                          
139000     MOVE MFS-DO-NOT-TOUCH-FIELD       TO MOD-IDLEVNR                     
139100                                          MOD-KVSPANT                     
139200                                          MOD-IDPERSON-BUY                
139300                                          MOD-TIFINLV                     
139400                                          MOD-KVPB-PLAN-UT                
139500                                          MOD-TIPBPLAN-UT                 
139600                                          MOD-TEARTNOT1-IN                
139700                                          MOD-TEARTNOT2-IN                
139800     .                                                                    
139900     EJECT                                                                
140000 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
140100                                                                          
140200     MOVE MFS-DO-NOT-TOUCH-FIELD       TO MOD-IDLEVNR-IN                  
140300                                          MOD-KVSPANT-IN                  
140400                                          MOD-IDPERSON-BUY-IN             
140500                                          MOD-TIFINLV-IN                  
140600                                          MOD-KVPB-PLAN-IN                
140700                                          MOD-TIPBPLAN-IN                 
140800                                          MOD-TEARTNOT1-IN                
140900                                          MOD-TEARTNOT2-IN                
141000     .                                                                    
141100     EJECT                                                                
141200 MFS-FORM-ATTR SECTION.                                                   
141300                                                                          
141400*    --- ALL INDATA-FIELDS                                                
141500     MOVE MFS-FORMAT-DEFAULT-ATTR     TO MOD-IDLEVNR-IN-ATTR              
141600                                         MOD-KVSPANT-IN-ATTR              
141700                                         MOD-IDPERSON-BUY-IN-ATTR         
141800                                         MOD-TIFINLV-IN-ATTR              
141900                                         MOD-KVPB-PLAN-ATTR               
142000                                         MOD-TIPBPLAN-ATTR                
142100                                         MOD-TEARTNOT1-IN-ATTR            
142200                                         MOD-TEARTNOT2-IN-ATTR            
142300     .                                                                    
142400     EJECT                                                                
142500* --- IMS SECTIONS ---                                                    
142600     SKIP3                                                                
142700 IMS-GET-MSG SECTION.                                                     
142800                                                                          
142900     MOVE '  QC' TO GOOD-STATUSCODES                                      
143000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
143100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
143200     PERFORM IMS-STATUSCHECK                                              
143300     .                                                                    
143400     EJECT                                                                
143500 IMS-INSERT-MSG SECTION.                                                  
143600                                                                          
143700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
143800     MOVE SPACE TO GOOD-STATUSCODES                                       
143900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
144000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
144100     PERFORM IMS-STATUSCHECK                                              
144200     .                                                                    
144300     EJECT                                                                
144400 IMS-GU-WDK601 SECTION.                                                   
144500                                                                          
144600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
144700          DELIMITED BY SIZE INTO SSA1                                     
144800     MOVE '  GE' TO GOOD-STATUSCODES                                      
144900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
145000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
145100     PERFORM IMS-STATUSCHECK                                              
145200     .                                                                    
145300     EJECT                                                                
145400 IMS-GHU-WDK601 SECTION.                                                  
145500                                                                          
145600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
145700          DELIMITED BY SIZE INTO SSA1                                     
145800     MOVE '  GE'            TO GOOD-STATUSCODES                           
145900     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK601 SSA1                   
146000     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
146100     PERFORM IMS-STATUSCHECK                                              
146200     .                                                                    
146300     EJECT                                                                
146400 IMS-REPL-WDK601 SECTION.                                                 
146500                                                                          
146600     MOVE 'WDK601  '   TO SSA1                                            
146700     MOVE '  '  TO GOOD-STATUSCODES                                       
146800     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK601 SSA1                  
146900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
147000     PERFORM IMS-STATUSCHECK                                              
147100     .                                                                    
147200     EJECT                                                                
147300 IMS-GU-WDK611 SECTION.                                                   
147400                                                                          
147500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
147600          DELIMITED BY SIZE INTO SSA1                                     
147700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X')'                         
147800          DELIMITED BY SIZE INTO SSA2                                     
147900     MOVE '  GE' TO GOOD-STATUSCODES                                      
148000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
148100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
148200     PERFORM IMS-STATUSCHECK                                              
148300     .                                                                    
148400     EJECT                                                                
148500 IMS-GHU-WDK611 SECTION.                                                  
148600                                                                          
148700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
148800          DELIMITED BY SIZE INTO SSA1                                     
148900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X')'                         
149000          DELIMITED BY SIZE INTO SSA2                                     
149100     MOVE '  GE'            TO GOOD-STATUSCODES                           
149200     CALL CBLTDLI USING GHU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2             
149300     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
149400     PERFORM IMS-STATUSCHECK                                              
149500     .                                                                    
149600     EJECT                                                                
149700 IMS-REPL-WDK611 SECTION.                                                 
149800                                                                          
149900     MOVE 'WDK611  '   TO SSA1                                            
150000     MOVE '  '  TO GOOD-STATUSCODES                                       
150100     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611 SSA1                  
150200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
150300     PERFORM IMS-STATUSCHECK                                              
150400     .                                                                    
150500     EJECT                                                                
150600                                                                          
150700 IMS-GU-WDK601-11   SECTION.                                              
150800                                                                          
150900     STRING 'WDK601  *D(IDARTNR  =' W-IDARTNR-X ')'                       
151000          DELIMITED BY SIZE INTO SSA1                                     
151100                                                                          
151200     MOVE 'WDK611  '       TO SSA2                                        
151300     MOVE '  GE' TO GOOD-STATUSCODES                                      
151400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601-11                 
151500                            SSA1 SSA2                                     
151600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
151700     PERFORM IMS-STATUSCHECK                                              
151800     .                                                                    
151900     SKIP3                                                                
152000                                                                          
152100 IMS-GU-WDK625 SECTION.                                                   
152200                                                                          
152300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
152400          DELIMITED BY SIZE INTO SSA1                                     
152500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
152600          DELIMITED BY SIZE INTO SSA2                                     
152700     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
152800          DELIMITED BY SIZE INTO SSA3                                     
152900     MOVE '  GE' TO GOOD-STATUSCODES                                      
153000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK625 SSA1 SSA2 SSA3          
153100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
153200     PERFORM IMS-STATUSCHECK                                              
153300     .                                                                    
153400     EJECT                                                                
153500 IMS-GHU-WDK625 SECTION.                                                  
153600                                                                          
153700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
153800            DELIMITED BY SIZE INTO SSA1                                   
153900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
154000            DELIMITED BY SIZE INTO SSA2                                   
154100     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
154200            DELIMITED BY SIZE INTO SSA3                                   
154300     MOVE '  GE' TO  GOOD-STATUSCODES                                     
154400     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK625 SSA1 SSA2 SSA3         
154500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
154600     PERFORM IMS-STATUSCHECK                                              
154700     .                                                                    
154800     EJECT                                                                
154900 IMS-REPL-WDK625 SECTION.                                                 
155000                                                                          
155100     MOVE '  '   TO GOOD-STATUSCODES                                      
155200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK625                       
155300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
155400     PERFORM IMS-STATUSCHECK                                              
155500     .                                                                    
155600     EJECT                                                                
155700 IMS-ISRT-WDK625 SECTION.                                                 
155800                                                                          
155900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
156000             DELIMITED BY SIZE INTO SSA1                                  
156100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X')'                         
156200             DELIMITED BY SIZE INTO SSA2                                  
156300     MOVE  'WDK625 ' TO SSA3                                              
156400     MOVE '  ' TO GOOD-STATUSCODES                                        
156500     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK625 SSA1 SSA2 SSA3        
156600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
156700     PERFORM IMS-STATUSCHECK                                              
156800     .                                                                    
156900     EJECT                                                                
157000 IMS-DELT-WDK625 SECTION.                                                 
157100                                                                          
157200     MOVE '  '   TO GOOD-STATUSCODES                                      
157300     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK625                       
157400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
157500     PERFORM IMS-STATUSCHECK                                              
157600     .                                                                    
157700     EJECT                                                                
157800 IMS-GU-WDK629 SECTION.                                                   
157900                                                                          
158000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
158100          DELIMITED BY SIZE INTO SSA1                                     
158200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
158300          DELIMITED BY SIZE INTO SSA2                                     
158400     STRING 'WDK629  (IDDCREF  =' W-IDDC-REF-X ')'                        
158500          DELIMITED BY SIZE INTO SSA3                                     
158600     MOVE '  GE' TO GOOD-STATUSCODES                                      
158700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3          
158800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
158900     PERFORM IMS-STATUSCHECK                                              
159000     .                                                                    
159100     EJECT                                                                
159200 IMS-GHU-WDK629 SECTION.                                                  
159300                                                                          
159400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
159500          DELIMITED BY SIZE INTO SSA1                                     
159600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
159700          DELIMITED BY SIZE INTO SSA2                                     
159800     STRING 'WDK629  (IDDCREF = ' W-IDDC-REF-X ')'                        
159900          DELIMITED BY SIZE INTO SSA3                                     
160000     MOVE '  GE' TO GOOD-STATUSCODES                                      
160100     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
160200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
160300     PERFORM IMS-STATUSCHECK                                              
160400     .                                                                    
160500     EJECT                                                                
160600                                                                          
160700 IMS-GHNP-WDK629 SECTION.                                                 
160800                                                                          
160900     MOVE 'WDK629     ' TO SSA1                                           
161000     MOVE '  GE' TO GOOD-STATUSCODES                                      
161100     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK629 SSA1                  
161200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
161300     PERFORM IMS-STATUSCHECK                                              
161400     .                                                                    
161500     EJECT                                                                
161600                                                                          
161700 IMS-REPL-WDK629 SECTION.                                                 
161800                                                                          
161900     MOVE '  ' TO GOOD-STATUSCODES                                        
162000     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
162100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
162200     PERFORM IMS-STATUSCHECK                                              
162300     .                                                                    
162400     EJECT                                                                
162500 IMS-ISRT-WDK629  SECTION.                                                
162600                                                                          
162700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
162800          DELIMITED BY SIZE INTO SSA1                                     
162900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
163000          DELIMITED BY SIZE INTO SSA2                                     
163100     MOVE 'WDK629   ' TO SSA3                                             
163200     MOVE '  ' TO GOOD-STATUSCODES                                        
163300     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3        
163400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
163500     PERFORM IMS-STATUSCHECK                                              
163600     .                                                                    
163700     EJECT                                                                
163800 IMS-DELT-WDK629 SECTION.                                                 
163900                                                                          
164000     MOVE '  '   TO GOOD-STATUSCODES                                      
164100     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK629                       
164200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
164300     PERFORM IMS-STATUSCHECK                                              
164400     .                                                                    
164500     EJECT                                                                
164600 IMS-GN-WDK711 SECTION.                                                   
164700                                                                          
164800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
164900          DELIMITED BY SIZE   INTO SSA1                                   
165000     STRING 'WDK711  (IDDC1   >=' W-IDDC1-MIN-X                           
165100                    '&IDDC1   <=' W-IDDC1-MAX-X ')'                       
165200          DELIMITED BY SIZE INTO SSA2                                     
165300     MOVE '  GE' TO GOOD-STATUSCODES                                      
165400     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
165500     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
165600     PERFORM IMS-STATUSCHECK                                              
165700     .                                                                    
165800     EJECT                                                                
165900 IMS-GU-WDK712   SECTION.                                                 
166000                                                                          
166100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
166200          DELIMITED BY SIZE INTO SSA1                                     
166300     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
166400          DELIMITED BY SIZE INTO SSA2                                     
166500     MOVE '  GE' TO GOOD-STATUSCODES                                      
166600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
166700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
166800     PERFORM IMS-STATUSCHECK                                              
166900     .                                                                    
167000     EJECT                                                                
167100 IMS-GN-WDB601 SECTION.                                                   
167200                                                                          
167300     MOVE   'WDB601   '    TO SSA1                                        
167400     MOVE '  GB' TO GOOD-STATUSCODES                                      
167500     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
167600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
167700     PERFORM IMS-STATUSCHECK                                              
167800     .                                                                    
167900     EJECT                                                                
168000 IMS-GN-WDE3-MIN-MAX SECTION.                                             
168100     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
168200                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
168300          DELIMITED BY SIZE INTO SSA1                                     
168400     MOVE '  GEGB'          TO GOOD-STATUSCODES                           
168500     CALL CBLTDLI        USING GN WDE3-PCB DLI-IO-WDE301 SSA1             
168600     MOVE WDE3-STATUS-CODE  TO STATUS-WS                                  
168700     PERFORM IMS-STATUSCHECK                                              
168800     .                                                                    
168900     EJECT                                                                
169000                                                                          
169100 IMS-GU-WDT501 SECTION.                                                   
169200     STRING 'WDT501  (IDARTNR = ' W-IDARTNR-X ')'                         
169300          DELIMITED BY SIZE INTO SSA1                                     
169400     MOVE '  GE' TO GOOD-STATUSCODES                                      
169500     CALL CBLTDLI USING GU  WDT5-PCB DLI-IO-WDT501 SSA1                   
169600     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
169700     PERFORM IMS-STATUSCHECK                                              
169800     .                                                                    
169900     EJECT                                                                
170000 IMS-ISRT-WDT501 SECTION.                                                 
170100     MOVE 'WDT501 ' TO SSA1                                               
170200     MOVE '  II' TO GOOD-STATUSCODES                                      
170300     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT501 SSA1                  
170400     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
170500     PERFORM IMS-STATUSCHECK                                              
170600     .                                                                    
       IMS-GHNP-WDT511 SECTION.                                                 
           MOVE   'WDT511  *F' TO SSA1                                          
           MOVE '  ' TO GOOD-STATUSCODES                                        
           CALL CBLTDLI USING GHNP  WDT5-PCB DLI-IO-WDT511 SSA1                 
           MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
           EJECT                                                                
       IMS-REPL-WDT511 SECTION.                                                 
           MOVE '  ' TO GOOD-STATUSCODES                                        
           CALL CBLTDLI USING REPL  WDT5-PCB DLI-IO-WDT511                      
           MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
           EJECT                                                                
170700 IMS-ISRT-WDT511 SECTION.                                                 
170800     STRING 'WDT501  (IDARTNR  =' W-IDARTNR-X ')'                         
170900            DELIMITED BY SIZE INTO SSA1                                   
171000     MOVE 'WDT511 ' TO SSA2                                               
171100     MOVE '  ' TO GOOD-STATUSCODES                                        
171200     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT511 SSA1 SSA2             
171300     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
171400     PERFORM IMS-STATUSCHECK                                              
171500     .                                                                    
       IMS-PURG-4289    SECTION.                                                
                                                                                
           MOVE LOW-VALUE TO P-TO-P2-KDZ1 P-TO-P2-KDZ2                          
           MOVE SPACE TO GOOD-STATUSCODES                                       
           CALL  CBLTDLI  USING PURG 4289-PCB P-TO-P-SW2                        
           MOVE 4289-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
171600 IMS-STATUSCHECK SECTION.                                                 
171700                                                                          
171800     SET STATUS-IX TO 1                                                   
171900     SEARCH GOOD-STATUS                                                   
172000       AT END                                                             
172100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
172200         DELIMITED BY SIZE INTO ERROR-TEXT                                
172300         CALL FELLOG                                                      
172400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
172500         CONTINUE                                                         
172600     END-SEARCH                                                           
172700     .                                                                    
172800*    -COPY WY2000P3                                                       
