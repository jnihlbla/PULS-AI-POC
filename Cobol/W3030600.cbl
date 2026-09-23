000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3030600.                                                
000300 AUTHOR.         KARANDE DIGAMBAR.                                        
000400 DATE-WRITTEN.   02/04/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SCREEN USED TO MANUALLY UPDATE THE DEALER PRICE,                 
000900*        WHEN REPLY HAS NOT BEEN RECEIVED FROM THE SALES COMPANY          
001000*        VIPS SYSTEM WITHIN A REASONABLE TIME. LPRQ-PRARTBTO-LOC,         
001100*        LPRQ-PRARTNTO-LOC,LPRQ-KDRAB FIELDS WILL BE UPDATED ONLY         
001200*        IF FIELDS ARE BLANK OR ZERO AND KDPRSTA IS > SPACE.              
001300*        AFTER THE UPDATE KDPRSTA TURNED TO 'M' TO IDENTIFY THE           
001400*        THE MANUAL UPDATE OF PRICE INFORMATION.                          
001500*                                                                         
001600*        THE PROGRAM UPDATES   WDC7                                       
001700*        THE PROGRAM READS     WDC7A WDC7B                                
001800*                                                                         
001810*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W3T306                                              
002100*        MID:         W30306I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        MOD:         W30306O1                                            
002500*                                                                         
002600*    E'TRACKER: 5276159  DATED 2007-07-10 RÄTTAT BLÄDDRINGSFEL OCH        
002700*                                         DYRA TRANSAR.                   
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300 DATA DIVISION.                                                           
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W3030600'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300                                                                          
004400*    --- INDEX FOR SCROLL LINES                                           
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004800                                                                          
004900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005000     88  INDATA-OK                           VALUE 'J'.                   
005100     88  INDATA-WRONG                        VALUE 'N'.                   
005200                                                                          
005300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005400     88  KEYS-OK                             VALUE 'J'.                   
005500     88  KEYS-WRONG                          VALUE 'N'.                   
005600                                                                          
005700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005800     88  OWN-MID                             VALUE '3306'.                
005900     88  GOOD-MID                            VALUE '3301' '3302'          
006000                                                   '3303' '3304'          
006100                                                   '3305' '3306'          
006200                                                   '3307' '3308'          
006300                                                   '3309'.                
006400     88  HELP-MID                            VALUE '0551'.                
006500                                                                          
006600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006700                                                                          
006800 77  W-KDPRSTA-FLAG              PIC X(1)     VALUE 'N'.                  
006900     88 W-KDPRSTA-GIVEN                       VALUE 'J'.                  
007000     88 W-KDPRSTA-NOT                         VALUE 'N'.                  
007100                                                                          
007200 77  W-RECORD-FLAG               PIC X(1)     VALUE 'N'.                  
007300     88 W-RECORD-OK                           VALUE 'J'.                  
007400     88 W-RECORD-WRONG                        VALUE 'N'.                  
007500                                                                          
007600 77  W-CHANGE-FLAG               PIC X(1)     VALUE 'N'.                  
007700     88 W-CHANGE-DATA                         VALUE 'J'.                  
007800     88 W-NOT-CHANGED                         VALUE 'N'.                  
007900                                                                          
008000 77  W-SELECT-FLAG               PIC X(1)     VALUE 'N'.                  
008100     88 W-SELECT-YES                          VALUE 'J'.                  
008200     88 W-SELECT-NO                           VALUE 'N'.                  
008300                                                                          
008400 77  W-SCAN-FLAG                 PIC X(1)     VALUE 'N'.                  
008500     88 W-SCAN-OK                             VALUE 'J'.                  
008600     88 W-STOP-SCAN                           VALUE 'N'.                  
008700                                                                          
008800 77  W-KDPRSTA-VALID             PIC X(1)     VALUE ' '.                  
008900     88 KDPRSTA-VALID                         VALUE 'A',                  
009000                                                    'V',                  
009100                                                    'P',                  
009200                                                    'M',                  
009300                                                    'N',                  
009400                                                    'X',                  
009500                                                    'K',                  
009600                                                    'R',                  
009700                                                    ' '.                  
009800                                                                          
009900 77  W-KDPRSTA                   PIC X(1)    VALUE SPACE.                 
010000 77  W-IDDISTR                   PIC 9(4)    VALUE ZERO.                  
010100 77  W-IDKUNDNR                  PIC 9(7)    VALUE ZERO.                  
010200 77  W-IDORDNR7                  PIC 9(7)    VALUE ZERO.                  
010210 77  W-IDARTNR-KY                PIC 9(9)    VALUE ZERO.                  
010220 77  W-KDPRSTA-KY                PIC X(1)    VALUE SPACE.                 
010230 77  W-IDKUNDNR-6                PIC 9(6)    VALUE ZERO.                  
010300                                                                          
010400 01  W-PRART.                                                             
010500     03  W-PRARTBTO-LOC          PIC S9(7)V9(2) OCCURS 13.                
010600     03  W-PRARTNTO-LOC          PIC S9(7)V9(2) OCCURS 13.                
010700     EJECT                                                                
010800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
010900 01  GENERAL-SUBPROGRAMS.                                                 
011000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011400     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
011500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
011600     EJECT                                                                
011700*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
011800*01 -COPY WMEDAREA                                                        
011900     SKIP3                                                                
012000 01  MESSAGE-CODES.                                                       
012100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012300     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
012400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012800     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
012900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013000     03  ERR-SELECT-LINE         PIC X(3)    VALUE '309'.                 
013100     03  INF-MISSING             PIC X(3)    VALUE '413'.                 
013200     EJECT                                                                
013300******************************                     *************          
013400 01  FILLER                      PIC X(16)   VALUE 'SENDING-AREA'.        
013500                                                                          
013600*01  -COPY WZ01SEND                                                       
013700 01  SEND-DATA.                                                           
013800*03  -COPY WZ01RESP  -PRE MOD-                                            
013900     03  MOD-MID-DATA    PIC X(32).                                       
014000*03  -COPY W40798I1  -PRE MOD-     -RED MOD-MID-DATA                      
014100*03  -COPY W40290I1  -PRE MOD4290- -RED MOD-MID-DATA                      
014200*03  -COPY W40291I1  -PRE MOD4291- -RED MOD-MID-DATA                      
014300 01   KDRC-DISPLAY               PIC X(4).                                
014400**************************************                                    
014500*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
014600*                                                                         
014700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014800     SKIP3                                                                
014900*01 -COPY WMSGINIT                                                        
015000     EJECT                                                                
015100*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
015200*                                                                         
015300 01  SAVE-AREA.                                                           
015400     03  SAVE-IDTRANS            PIC X(4)   VALUE '3306'.                 
015500     03  SAVE-IDDISTR-ENTER      PIC 9(4).                                
015600     03  SAVE-IDDISTR-NEXT       PIC 9(4).                                
015700     03  SAVE-IDKUNDNR-ENTER     PIC 9(7).                                
015800     03  SAVE-IDKUNDNR-NEXT      PIC 9(7).                                
015900     03  SAVE-KDPRSTA-ENTER      PIC X(1).                                
016000     03  SAVE-KDPRSTA-NEXT       PIC X(1).                                
016100     03  SAVE-IDORDNR7-ENTER     PIC 9(7).                                
016200     03  SAVE-IDORDNR7-NEXT      PIC 9(7).                                
016210     03  SAVE-IDARTNR-ENTER      PIC 9(9).                                
016220     03  SAVE-IDARTNR-NEXT       PIC 9(9).                                
016300     03  SAVE-IDPRQUES-ENTER     PIC 9(7).                                
016400     03  SAVE-IDPRQUES-NEXT      PIC 9(7).                                
016500     03  SAVE-W30306I1.                                                   
016600         05  SAVE-KDPRSTA-IN         PIC X(01).                           
016610         05  SAVE-IDDISTR-IN         PIC X(04) VALUE SPACE.               
016620         05  SAVE-IDKUNDNR-IN        PIC X(06) VALUE SPACE.               
016700         05  SAVE-INPUT  OCCURS 13.                                       
016800             07  SAVE-KDCMD          PIC X(1).                            
016900             07  SAVE-IDDISTR        PIC 9(04).                           
017000             07  SAVE-IDKUNDNR       PIC 9(7).                            
017100             07  SAVE-KDPRSTA        PIC X(1).                            
017200             07  SAVE-IDARTNR        PIC 9(9).                            
017300             07  SAVE-IDORDNR7       PIC 9(7).                            
017400             07  SAVE-KDVALISO       PIC X(3).                            
017500             07  SAVE-PRARTBTO-LOC   PIC S9(7)V9(2).                      
017600             07  SAVE-PRARTNTO-LOC   PIC S9(7)V9(2).                      
017700             07  SAVE-KDRAB          PIC X(5).                            
017800             07  SAVE-IDPRQUES       PIC 9(7).                            
017900             07  SAVE-RECORD-FLAG    PIC X(1).                            
018000                                                                          
018100 01 W-INIT-SAVE.                                                          
018200     03  W-INIT-W30306I1.                                                 
018300         05  W-INIT-KDPRSTA-IN         PIC X(01).                         
018400         05  W-INIT-INPUT   OCCURS 13.                                    
018500             07  W-INIT-KDCMD          PIC X(1).                          
018600             07  W-INIT-IDDISTR        PIC 9(04).                         
018700             07  W-INIT-IDKUNDNR       PIC 9(7).                          
018800             07  W-INIT-KDPRSTA        PIC X(1).                          
018900             07  W-INIT-IDARTNR        PIC 9(9).                          
019000             07  W-INIT-IDORDNR7       PIC 9(7).                          
019100             07  W-INIT-KDVALISO       PIC X(3).                          
019200             07  W-INIT-PRARTBTO-LOC   PIC S9(7)V9(2).                    
019300             07  W-INIT-PRARTNTO-LOC   PIC S9(7)V9(2).                    
019400             07  W-INIT-KDRAB          PIC X(5).                          
019500             07  W-INIT-IDPRQUES       PIC 9(7).                          
019600             07  W-INIT-RECORD-FLAG    PIC X(1).                          
019700                                                                          
019800 01 W-ALL-ATTR.                                                           
019900     03 W-ATTR    OCCURS 13.                                              
020000        05 W-KDCMD-ATTR                PIC X(2).                          
020100        05 W-PRARTBTO-LOC-ATTR         PIC X(2).                          
020200        05 W-PRARTNTO-LOC-ATTR         PIC X(2).                          
020300        05 W-KDRAB-ATTR                PIC X(2).                          
020400                                                                          
020500     EJECT                                                                
020600*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
020700*                                                                         
020800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020900     SKIP3                                                                
021000*01  MID -COPY W30306I1                                                   
021100     EJECT                                                                
021200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021300     SKIP3                                                                
021400*01  -COPY WMSGAREA                                                       
021500     EJECT                                                                
021600     03  MOD REDEFINES MSG-AREA.                                          
021700*      05  -COPY W30306O1                                                 
021800     EJECT                                                                
021900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022000     SKIP3                                                                
022100*01  -COPY WMFSAREA                                                       
022200     EJECT                                                                
022300*01    -COPY WDECAREA                                                     
022400     EJECT                                                                
022500*    --- WORK-AREAS FOR IMS-SECTIONS                                      
022600*                                                                         
022700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022800     SKIP3                                                                
022900 01  KEYS-TO-DLI.                                                         
023000*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
023100                                                                          
023200     03  W-WDC7A1KY-MIN-X.                                                
023300         05  W-KDSEGKEY-A1-MIN   PIC  X(1)   VALUE SPACE.                 
023500         05  W-IDDISTR-A1-MIN    PIC 9(4)    VALUE ZERO.                  
023600         05  W-IDKUNDNR-A1-MIN   PIC 9(7)    VALUE ZERO.                  
023700         05  W-IDBUNDLE-A1-MIN.                                           
023710             07 W-IDORDNR7-A1-MIN PIC 9(07)  VALUE ZERO.                  
023720             07 FILLER           PIC X(08)   VALUE SPACE.                 
023730         05  W-IDARTNR-A1-MIN    PIC 9(9)    VALUE ZERO.                  
023740         05  W-IDPRQUES-A1-MIN   PIC 9(7)    VALUE ZERO.                  
023800                                                                          
023810     03  W-WDC7A1KY-MAX-X.                                                
023820         05  W-KDSEGKEY-A1-MAX   PIC  X(1)   VALUE SPACE.                 
023830         05  W-IDDISTR-A1-MAX    PIC 9(4)    VALUE ZERO.                  
023840         05  W-IDKUNDNR-A1-MAX   PIC 9(7)    VALUE ZERO.                  
023850         05  W-IDBUNDLE-A1-MAX.                                           
023860             07 W-IDORDNR7-A1-MAX PIC 9(07)  VALUE ZERO.                  
023861             07 FILLER           PIC X(08)   VALUE SPACE.                 
023862         05  W-IDARTNR-A1-MAX    PIC 9(9)    VALUE ZERO.                  
023863         05  W-IDPRQUES-A1-MAX   PIC 9(7)    VALUE ZERO.                  
023870                                                                          
023880     03  W-WDC7B1KY-MIN-X.                                                
023890         05  W-KDPRSTA-B1-MIN    PIC  X(1)   VALUE SPACE.                 
023891         05  W-IDDISTR-B1-MIN    PIC 9(4)    VALUE ZERO.                  
023892         05  W-IDKUNDNR-B1-MIN   PIC 9(7)    VALUE ZERO.                  
023893         05  W-IDBUNDLE-B1-MIN.                                           
023894             07 W-IDORDNR7-B1-MIN PIC 9(07)  VALUE ZERO.                  
023895             07 FILLER           PIC X(08)   VALUE SPACE.                 
023896         05  W-IDARTNR-B1-MIN    PIC 9(9)    VALUE ZERO.                  
023897         05  W-IDPRQUES-B1-MIN   PIC 9(7)    VALUE ZERO.                  
023898                                                                          
023899     03  W-WDC7B1KY-MAX-X.                                                
023900         05  W-KDPRSTA-B1-MAX    PIC  X(1)   VALUE SPACE.                 
023901         05  W-IDDISTR-B1-MAX    PIC 9(4)    VALUE ZERO.                  
023902         05  W-IDKUNDNR-B1-MAX   PIC 9(7)    VALUE ZERO.                  
023903         05  W-IDBUNDLE-B1-MAX.                                           
023904             07 W-IDORDNR7-B1-MAX PIC 9(07)  VALUE ZERO.                  
023905             07 FILLER           PIC X(08)   VALUE SPACE.                 
023906         05  W-IDARTNR-B1-MAX    PIC 9(9)    VALUE ZERO.                  
023907         05  W-IDPRQUES-B1-MAX   PIC 9(7)    VALUE ZERO.                  
023908                                                                          
023910     03  W-WDC701KY-X.                                                    
024000         05 W-IDDISTR-KY         PIC 9(04)   VALUE ZERO.                  
024100         05 W-IDKUNDNR-KY        PIC 9(07)   VALUE ZERO.                  
024200         05 W-IDBUNDLE-KY.                                                
024300            07 W-IDORDNR7-KY     PIC 9(07)   VALUE ZERO.                  
024400            07 FILLER            PIC X(08)   VALUE SPACES.                
024500                                                                          
024600     03  W-IDPRQUESKY-X.                                                  
024700         05  W-IDPRQUES-KY       PIC 9(7)    VALUE ZERO.                  
024800                                                                          
024900     03  W-WDC701UP-X.                                                    
025000         05 W-IDDISTR-UP         PIC 9(04)   VALUE ZERO.                  
025100         05 W-IDKUNDNR-UP        PIC 9(07)   VALUE ZERO.                  
025200         05 W-IDBUNDLE-UP.                                                
025300            07 W-IDORDNR7-UP     PIC 9(07)   VALUE ZERO.                  
025400            07 FILLER            PIC X(08)   VALUE SPACES.                
025500                                                                          
025600     03  W-IDPRQUESUP-X.                                                  
025700         05  W-IDPRQUES-UP       PIC 9(7)    VALUE ZERO.                  
025800     SKIP2                                                                
025900*    --- STATUS-KOD FRÅN IMS                                              
026000 01  STATUS-WS                   PIC XX.                                  
026100     88  SEGMENT-FOUND                       VALUE '  '.                  
026200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
026300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
026400     SKIP2                                                                
026500 01  GOOD-STATUSCODES.                                                    
026600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026700     SKIP3                                                                
026800 01  SSA1                        PIC X(128).                              
026900 01  SSA2                        PIC X(64).                               
027000     EJECT                                                                
027100*    --- IMS FUNCTION CODES                                               
027200*01  -COPY W0003                                                          
027300     EJECT                                                                
027400*    ---  DLI INPUT-OUTPUT AREA                                           
027500                                                                          
027600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC7'.                        
027700 01  DLI-IO-WDC7.                                                         
027800     03  DLI-IO-WDC701.                                                   
027900*        05  -COPY WDC701                                                 
028000     03  DLI-IO-WDC711.                                                   
028100*        05  -COPY WDC711                                                 
028110                                                                          
028111     EJECT                                                                
028120 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC7A1'.                      
028130 01  DLI-IO-WDC7A1.                                                       
028140*    03  -COPY WDC7A1                                                     
028200     EJECT                                                                
028220 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC7B1'.                      
028230 01  DLI-IO-WDC7B1.                                                       
028240*    03  -COPY WDC7B1                                                     
028250     EJECT                                                                
028300 LINKAGE SECTION.                                                         
028400*01  -COPY W0009   -PRE MSG-                                              
028500     EJECT                                                                
028600*01  -COPY W0009   -PRE CRE-                                              
028700     EJECT                                                                
028800*01  -COPY W0009   -PRE PRO-                                              
028900     EJECT                                                                
029000*01  -COPY W0009   -PRE LTD-                                              
029100     EJECT                                                                
029200*01  -COPY W0008   -PRE USEA-                                             
029300     05  FILLER                  PIC X.                                   
029400*01  -COPY W0008   -PRE WDC7A-                                            
029500     05  FILLER                  PIC X.                                   
029600*01  -COPY W0008   -PRE WDC7B-                                            
029610     05  FILLER                  PIC X.                                   
029700*01  -COPY W0008   -PRE WDC72-                                            
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000 PROCEDURE DIVISION  USING MSG-PCB CRE-PCB PRO-PCB LTD-PCB                
030100                              USEA-PCB WDC7A-PCB WDC7B-PCB                
030110                              WDC72-PCB.                                  
030200 MAIN SECTION.                                                            
030300     ENTRY 'DLITCBL' USING MSG-PCB CRE-PCB PRO-PCB LTD-PCB                
030401                              USEA-PCB WDC7A-PCB WDC7B-PCB                
030410                              WDC72-PCB.                                  
030500                                                                          
030600     PERFORM IMS-GET-MSG                                                  
030700     IF SEGMENT-FOUND                                                     
030800       PERFORM A-INIT                                                     
030900       PERFORM B-CHECK-KEYS                                               
031000       IF KEYS-OK                                                         
031100         IF MFS-UPDATE                                                    
031200           PERFORM G-CHECK-INPUT                                          
031300           IF INDATA-OK                                                   
031400             PERFORM H-UPDATE                                             
031500****IF ALLA PRISVILKOR ÄR OK                                              
031600             IF W-CHANGE-DATA                                             
031700             IF LPRQ-ADDISPABS > SPACE                                    
031800             IF LPRQ-PRARTNTO-LOC > 0                                     
031900               PERFORM S04-SEND-OPEN                                      
032000               MOVE W-IDDISTR-UP TO MOD-MID-IDDISTR                       
032100               MOVE W-IDKUNDNR-UP TO MOD-MID-IDKUNDNR                     
032200               MOVE W-IDBUNDLE-UP TO MOD-MID-IDBUNDLE-GRP                 
032300               MOVE W-IDPRQUES-UP TO MOD-MID-IDPRQUES                     
032400               PERFORM S05-SEND-MESSAGE                                   
032500               PERFORM S06-SEND-CLOSE                                     
032600             END-IF                                                       
032700             END-IF                                                       
032800             END-IF                                                       
032900           END-IF                                                         
033000         ELSE                                                             
033100           IF MFS-FIRST                                                   
033200             PERFORM C-FIRST-PAGE                                         
033300           ELSE                                                           
033400             IF MFS-NEXT                                                  
033500               PERFORM D-NEXT-PAGE                                        
033600             ELSE                                                         
033700               PERFORM E-SAME-PAGE                                        
033800             END-IF                                                       
033900           END-IF                                                         
034000         END-IF                                                           
034100         IF INDATA-OK                                                     
034200            PERFORM F-READ-SHOW-INFO                                      
034300         END-IF                                                           
034400         PERFORM I-OPEN-CLOSE-FIELD                                       
034500                                                                          
034600       END-IF                                                             
034700*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
034800*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
034900       COMPUTE MSG-KVLL = LENGTH OF MOD-W30306O1 + 4                      
035000       PERFORM IMS-INSERT-MSG                                             
035100     END-IF                                                               
035200                                                                          
035300     MOVE ZERO TO RETURN-CODE                                             
035400     GOBACK                                                               
035500     .                                                                    
035600     EJECT                                                                
035700 A-INIT SECTION.                                                          
035800                                                                          
035900     IF MSG-DOUBLE-TRANSACTIONS                                           
036000       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W30306I1                 
036100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
036200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036300     ELSE                                                                 
036400       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W30306I1                  
036500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
036600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
036700     END-IF                                                               
036800                                                                          
036900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
037000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
037100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
037200                                                                          
037300     MOVE LOW-VALUE TO MSG-AREA                                           
037400     MOVE 'W3O306N1' TO MFS-IDMOD                                         
037500     MOVE '3306' TO MOD-IDTRANS                                           
037600     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
037700                                                                          
037800     IF OWN-MID OR HELP-MID                                               
037900       CONTINUE                                                           
038000     ELSE                                                                 
038100       MOVE SPACE TO MFS-KDTRTYP                                          
038200       MOVE '7' TO MFS-IDPFK                                              
038300     END-IF                                                               
038400     MOVE ZEROS        TO W-IDDISTR                                       
038600                          W-IDKUNDNR                                      
038610                          W-IDKUNDNR-6                                    
038700                          W-IDORDNR7                                      
038710                          W-IDDISTR-A1-MIN                                
038800                          W-IDKUNDNR-A1-MIN                               
038810                          W-IDORDNR7-A1-MIN                               
038820                          W-IDARTNR-A1-MIN                                
038830                          W-IDPRQUES-A1-MIN                               
038840                          W-IDDISTR-B1-MIN                                
038850                          W-IDKUNDNR-B1-MIN                               
038860                          W-IDORDNR7-B1-MIN                               
038870                          W-IDARTNR-B1-MIN                                
038880                          W-IDPRQUES-B1-MIN                               
038900     MOVE 9999         TO W-IDDISTR-A1-MAX                                
038910                          W-IDDISTR-B1-MAX                                
039000     MOVE 9999999      TO W-IDKUNDNR-A1-MAX                               
039010                          W-IDKUNDNR-B1-MAX                               
039020                          W-IDORDNR7-A1-MAX                               
039021                          W-IDORDNR7-B1-MAX                               
039022                          W-IDPRQUES-A1-MAX                               
039023                          W-IDPRQUES-B1-MAX                               
039030     MOVE 999999999    TO W-IDARTNR-A1-MAX                                
039031                          W-IDARTNR-B1-MAX                                
039100     MOVE SPACE        TO W-INIT-KDPRSTA-IN                               
039110                                                                          
039200     MOVE 1  TO INDX                                                      
039300                                                                          
039400     PERFORM UNTIL INDX > MAX-INDX                                        
039500                                                                          
039600        MOVE SPACE TO W-INIT-KDCMD (INDX)                                 
039700                      W-INIT-KDPRSTA (INDX)                               
039800                      W-INIT-KDVALISO (INDX)                              
039900                      W-INIT-KDRAB (INDX)                                 
040000        MOVE ZERO  TO W-INIT-IDDISTR (INDX)                               
040100                      W-INIT-IDKUNDNR (INDX)                              
040200                      W-INIT-IDARTNR (INDX)                               
040300                      W-INIT-IDORDNR7 (INDX)                              
040400                      W-INIT-PRARTBTO-LOC (INDX)                          
040500                      W-INIT-PRARTNTO-LOC (INDX)                          
040600                      W-INIT-IDPRQUES (INDX)                              
040700        MOVE NOO   TO W-INIT-RECORD-FLAG (INDX)                           
040800        MOVE MFS-ALPHA-FIELD-OK  TO  W-KDCMD-ATTR (INDX)                  
040900                                     W-PRARTBTO-LOC-ATTR (INDX)           
041000                                     W-PRARTNTO-LOC-ATTR (INDX)           
041100                                     W-KDRAB-ATTR (INDX)                  
041200        ADD  1     TO INDX                                                
041300                                                                          
041400     END-PERFORM                                                          
041500     .                                                                    
041600     EJECT                                                                
041700                                                                          
041800 B-CHECK-KEYS SECTION.                                                    
041900                                                                          
042000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
042100     MOVE '001'             TO MSGI-KDCALL                                
042200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
042300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
042400     MOVE '3306'            TO MSGI-IDTRANS                               
042500                                                                          
042600     IF OWN-MID                                                           
042700         MOVE MID-IDDISTR-IN      TO MSGI-IDDISTR                         
042800         MOVE MID-IDKUNDNR-IN     TO MSGI-IDKUNDNR                        
042900     END-IF                                                               
043000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
043100     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
043200                                                                          
043300     IF OWN-MID                                                           
043400        IF MID-KDPRSTA-IN NOT = ALL '+'                                   
043500           MOVE MID-KDPRSTA-IN  TO W-KDPRSTA                              
043600        ELSE                                                              
043700           IF SAVE-KDPRSTA-IN     > SPACES                                
043800              MOVE SAVE-KDPRSTA-IN      TO W-KDPRSTA                      
043900           ELSE                                                           
044000              MOVE SPACE          TO W-KDPRSTA                            
044100           END-IF                                                         
044200        END-IF                                                            
044300     ELSE                                                                 
044400        MOVE SPACE TO W-KDPRSTA                                           
044500     END-IF                                                               
044600                                                                          
044700     IF W-KDPRSTA    NOT  =  SAVE-KDPRSTA-IN                              
044800       MOVE W-KDPRSTA  TO SAVE-KDPRSTA-IN                                 
044900       MOVE '002'      TO MSGI-KDCALL                                     
045000       MOVE '3306'     TO SAVE-IDTRANS                                    
045100       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
045200       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
045300     END-IF                                                               
045400                                                                          
045500     IF W-KDPRSTA > SPACE                                                 
045600        MOVE YES TO W-KDPRSTA-FLAG                                        
045700     ELSE                                                                 
045800        MOVE NOO TO W-KDPRSTA-FLAG                                        
045900     END-IF                                                               
046000                                                                          
046100*    - LANGUAGE TO BE USED BY MEDKONV                                     
046200     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
046300                                                                          
046400     MOVE YES TO KEYS-SW                                                  
046500                                                                          
046600     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UT                               
046700                             MOD-IDKUNDNR-UT                              
046800                             MOD-KDPRSTA-UT                               
046900                                                                          
047000     IF MID-IDDISTR-IN  NOT = ALL '+'                                     
047100       MOVE '7'         TO MFS-IDPFK                                      
047200       MOVE SPACE       TO MFS-KDTRTYP                                    
047300     END-IF                                                               
047400                                                                          
047500     IF MID-IDKUNDNR-IN  NOT = ALL '+'                                    
047600       MOVE '7'         TO MFS-IDPFK                                      
047700       MOVE SPACE       TO MFS-KDTRTYP                                    
047800     END-IF                                                               
047900                                                                          
048000     IF MID-KDPRSTA-IN  NOT = ALL '+'                                     
048100       MOVE '7'         TO MFS-IDPFK                                      
048200       MOVE SPACE       TO MFS-KDTRTYP                                    
048300     END-IF                                                               
048400                                                                          
048500     IF NOT OWN-MID                                                       
048600       MOVE '7'         TO MFS-IDPFK                                      
048700       MOVE SPACE       TO MFS-KDTRTYP                                    
048800     END-IF                                                               
048900                                                                          
048910*    -- KONTROLL AV IDDISTR                                               
048920                                                                          
048930     IF MID-IDDISTR-IN NOT = ALL '+'                                      
049000       INSPECT MSGI-IDDISTR     REPLACING LEADING SPACE BY ZERO           
049100                                                                          
049200       IF MSGI-IDDISTR NUMERIC                                            
049300         MOVE    MSGI-IDDISTR       TO W-IDDISTR                          
049400       ELSE                                                               
049500         MOVE NOO                   TO KEYS-SW                            
049800       END-IF                                                             
049810     ELSE                                                                 
049820       IF OWN-MID                                                         
049830         IF SAVE-IDTRANS = '3306'  AND                                    
049840            SAVE-IDDISTR-IN     > SPACES                                  
049850                                                                          
049851           IF SAVE-IDDISTR-IN NUMERIC                                     
049860             MOVE SAVE-IDDISTR-IN   TO W-IDDISTR                          
049861           ELSE                                                           
049862             MOVE ZERO              TO W-IDDISTR                          
049863           END-IF                                                         
049870         ELSE                                                             
049880            MOVE ZERO               TO W-IDDISTR                          
049890         END-IF                                                           
049891       ELSE                                                               
049892         MOVE ZERO                  TO W-IDDISTR                          
049893       END-IF                                                             
049894     END-IF                                                               
049900                                                                          
049910*    -- KONTROLL AV IDKUNDNR                                              
049920                                                                          
049930     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
050000       INSPECT MSGI-IDKUNDNR     REPLACING LEADING SPACE BY ZERO          
050100                                                                          
050200       IF MSGI-IDKUNDNR NUMERIC                                           
050300         MOVE  MSGI-IDKUNDNR        TO W-IDKUNDNR                         
050400       ELSE                                                               
050500         MOVE NOO                   TO KEYS-SW                            
050800       END-IF                                                             
050805     ELSE                                                                 
050806       IF OWN-MID                                                         
050807         IF SAVE-IDTRANS = '3306'  AND                                    
050808            SAVE-IDKUNDNR-IN     > SPACES                                 
050809                                                                          
050810           IF SAVE-IDKUNDNR-IN NUMERIC                                    
050811             MOVE SAVE-IDKUNDNR-IN  TO W-IDKUNDNR                         
050812           ELSE                                                           
050813             MOVE ZERO              TO W-IDKUNDNR                         
050814           END-IF                                                         
050815         ELSE                                                             
050816            MOVE ZERO               TO W-IDKUNDNR                         
050817         END-IF                                                           
050818       ELSE                                                               
050819         MOVE ZERO                  TO W-IDKUNDNR                         
050820       END-IF                                                             
050821     END-IF                                                               
050830                                                                          
050840     IF MSGI-IDKUNDNR > ZERO AND MSGI-IDDISTR = ZERO                      
050850       MOVE NOO                   TO KEYS-SW                              
050860     END-IF                                                               
051300                                                                          
051400     MOVE SAVE-KDPRSTA-IN         TO W-KDPRSTA-VALID                      
051500     IF NOT KDPRSTA-VALID                                                 
051600       MOVE  NOO                  TO KEYS-SW                              
051700     END-IF                                                               
051800                                                                          
051900     IF GOOD-MID OR KEYS-OK                                               
052000       MOVE W-IDDISTR             TO MOD-IDDISTR-UT                       
052100       MOVE W-IDKUNDNR            TO MOD-IDKUNDNR-UT                      
052200       MOVE SAVE-KDPRSTA-IN       TO MOD-KDPRSTA-UT                       
052300       IF W-IDDISTR >  ZERO                                               
052400          MOVE MSGI-IDDISTR       TO W-IDDISTR-A1-MIN                     
052500                                     W-IDDISTR-A1-MAX                     
052510                                     W-IDDISTR-B1-MIN                     
052520                                     W-IDDISTR-B1-MAX                     
052600       END-IF                                                             
052700       IF W-IDKUNDNR > ZERO                                               
052800          MOVE MSGI-IDKUNDNR      TO W-IDKUNDNR-A1-MIN                    
052900                                     W-IDKUNDNR-A1-MAX                    
052910                                     W-IDKUNDNR-B1-MIN                    
052920                                     W-IDKUNDNR-B1-MAX                    
053000       END-IF                                                             
053100                                                                          
053200     ELSE                                                                 
053300       MOVE MFS-ERASE-FIELD       TO MOD-IDDISTR-UT                       
053400                                     MOD-IDKUNDNR-UT                      
053500                                     MOD-KDPRSTA-UT                       
053600     END-IF                                                               
053700                                                                          
053800     IF KEYS-WRONG                                                        
053900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
054000       CALL WMEDKONV USING MED-WMEDAREA                                   
054100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
054200       PERFORM MFS-ERASE-FIELD-IN                                         
054300       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
054400     ELSE                                                                 
054500       PERFORM MFS-ERASE-FIELD-IN                                         
054600     END-IF                                                               
054700     .                                                                    
054800     EJECT                                                                
054900                                                                          
055000 C-FIRST-PAGE SECTION.                                                    
055100                                                                          
055200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
055300     CALL WMEDKONV USING MED-WMEDAREA                                     
055400     MOVE MED-MFSINF TO  MOD-TEMFSFEL                                     
055500                                                                          
055600     PERFORM MFS-ERASE-FIELD-IN                                           
055700     .                                                                    
055800     EJECT                                                                
055900                                                                          
056000 D-NEXT-PAGE SECTION.                                                     
056100                                                                          
056200     IF SAVE-IDTRANS = '3306'                                             
056300       IF SAVE-IDDISTR-NEXT > ZERO                                        
056400          MOVE SAVE-IDDISTR-NEXT   TO W-IDDISTR-KY                        
056500       ELSE                                                               
056600          MOVE SAVE-IDDISTR-ENTER  TO W-IDDISTR-KY                        
056700       END-IF                                                             
056800       IF SAVE-IDKUNDNR-NEXT  > ZERO                                      
056900          MOVE SAVE-IDKUNDNR-NEXT  TO W-IDKUNDNR-KY                       
057000       ELSE                                                               
057100          MOVE SAVE-IDKUNDNR-ENTER TO W-IDKUNDNR-KY                       
057200       END-IF                                                             
057300       IF SAVE-IDORDNR7-NEXT  > ZERO                                      
057400          MOVE SAVE-IDORDNR7-NEXT  TO W-IDORDNR7-KY                       
057500       ELSE                                                               
057600          MOVE SAVE-IDORDNR7-ENTER TO W-IDORDNR7-KY                       
057700       END-IF                                                             
057710       IF SAVE-IDARTNR-NEXT  > ZERO                                       
057720          MOVE SAVE-IDARTNR-NEXT   TO W-IDARTNR-KY                        
057730       ELSE                                                               
057740          MOVE SAVE-IDARTNR-ENTER  TO W-IDARTNR-KY                        
057750       END-IF                                                             
057800       IF SAVE-IDPRQUES-NEXT > ZERO                                       
057900          MOVE SAVE-IDPRQUES-NEXT  TO W-IDPRQUES-KY                       
058000       ELSE                                                               
058100          MOVE SAVE-IDPRQUES-ENTER TO W-IDPRQUES-KY                       
058200       END-IF                                                             
058210       IF SAVE-KDPRSTA-NEXT > ZERO                                        
058220          MOVE SAVE-KDPRSTA-NEXT   TO W-KDPRSTA-KY                        
058230       ELSE                                                               
058240          MOVE SAVE-KDPRSTA-ENTER  TO W-KDPRSTA-KY                        
058250       END-IF                                                             
058300     ELSE                                                                 
058400       PERFORM MFS-ERASE-FIELD-IN                                         
058500     END-IF                                                               
058600     .                                                                    
058700     EJECT                                                                
058800 E-SAME-PAGE SECTION.                                                     
058900                                                                          
059000     IF SAVE-IDTRANS = '3306' OR '0551'                                   
059100                                                                          
059200       MOVE SAVE-IDDISTR-ENTER  TO W-IDDISTR-KY                           
059300       MOVE SAVE-IDKUNDNR-ENTER TO W-IDKUNDNR-KY                          
059400       MOVE SAVE-IDORDNR7-ENTER TO W-IDORDNR7-KY                          
059410       MOVE SAVE-IDARTNR-ENTER  TO W-IDARTNR-KY                           
059500       MOVE SAVE-IDPRQUES-ENTER TO W-IDPRQUES-KY                          
059510       MOVE SAVE-KDPRSTA-ENTER  TO W-KDPRSTA-KY                           
059600                                                                          
059700       IF MID-W30306I1 = ALL '+'                                          
059800         PERFORM MFS-ERASE-FIELD-IN                                       
059900       ELSE                                                               
060000         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
060100         CALL WMEDKONV USING MED-WMEDAREA                                 
060200         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
060300       END-IF                                                             
060400       MOVE 1  TO INDX                                                    
060500       PERFORM UNTIL INDX > MAX-INDX                                      
060600         IF MID-KDCMD (INDX ) = 'S'                                       
060700            MOVE INF-PRESS-PF11 TO MED-IDMFSINF                           
060800            CALL WMEDKONV USING MED-WMEDAREA                              
060900            MOVE MED-MFSINF TO MOD-TEMFSFEL                               
061000            MOVE NOO        TO INDATA-SW                                  
061100         END-IF                                                           
061200         ADD  1  TO INDX                                                  
061300       END-PERFORM                                                        
061400     ELSE                                                                 
061500       PERFORM MFS-ERASE-FIELD-IN                                         
061600     END-IF                                                               
061700     IF INDATA-WRONG                                                      
061800       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
061900       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
062000     END-IF                                                               
062100     .                                                                    
062200     EJECT                                                                
062300 F-READ-SHOW-INFO SECTION.                                                
062400                                                                          
062410     IF OWN-MID                                                           
062420       IF W-KDPRSTA-GIVEN                                                 
062500         PERFORM FB-READ-B1-KDPRSTA                                       
062501       ELSE                                                               
062510         PERFORM FA-READ-A1-BASICDATA                                     
062520       END-IF                                                             
062530     ELSE                                                                 
062531       PERFORM FA-READ-A1-BASICDATA                                       
062540     END-IF                                                               
062600                                                                          
062700     IF SEGMENT-MISSING                                                   
062800       MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                              
062900       CALL WMEDKONV USING MED-WMEDAREA                                   
063000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
063100       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
063200       MOVE 1  TO INDX                                                    
063300       PERFORM UNTIL INDX > MAX-INDX                                      
063400          MOVE W-INIT-INPUT (INDX)  TO SAVE-INPUT (INDX)                  
063500          ADD 1  TO INDX                                                  
063600       END-PERFORM                                                        
063700     ELSE                                                                 
063710       IF W-KDPRSTA-GIVEN                                                 
063711         PERFORM FD-MOVE-LINEDATA-B1                                      
063712       ELSE                                                               
063713         PERFORM FC-MOVE-LINEDATA-A1                                      
063720       END-IF                                                             
063730     END-IF                                                               
073400                                                                          
073500     MOVE '002'      TO MSGI-KDCALL                                       
073600     MOVE '3306'     TO SAVE-IDTRANS                                      
073700     MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                    
073800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
074000     .                                                                    
074100     EJECT                                                                
074200                                                                          
074300 FA-READ-A1-BASICDATA SECTION.                                            
074400                                                                          
074410     IF MFS-NEXT OR MFS-ENTER                                             
074420       MOVE W-IDDISTR-KY   TO W-IDDISTR-A1-MIN                            
074430                              W-IDDISTR-A1-MAX                            
074440       MOVE W-IDKUNDNR-KY  TO W-IDKUNDNR-A1-MIN                           
074441                              W-IDKUNDNR-A1-MAX                           
074442       MOVE W-IDORDNR7-KY  TO W-IDORDNR7-A1-MIN                           
074443                              W-IDORDNR7-A1-MAX                           
074444       MOVE W-IDARTNR-KY   TO W-IDARTNR-A1-MIN                            
074445                              W-IDARTNR-A1-MAX                            
074446       MOVE W-IDPRQUES-KY  TO W-IDPRQUES-A1-MIN                           
074447                              W-IDPRQUES-A1-MAX                           
074450                                                                          
074460       PERFORM IMS-GU-WDC7A1                                              
074461                                                                          
074462       IF W-IDDISTR > ZERO                                                
074463         MOVE W-IDDISTR    TO W-IDDISTR-A1-MAX                            
074464       ELSE                                                               
074465         MOVE 9999         TO W-IDDISTR-A1-MAX                            
074466       END-IF                                                             
074467                                                                          
074468       IF W-IDKUNDNR > ZERO                                               
074469         MOVE W-IDKUNDNR     TO W-IDKUNDNR-A1-MAX                         
074470       ELSE                                                               
074471         MOVE 9999999        TO W-IDKUNDNR-A1-MAX                         
074472       END-IF                                                             
074473                                                                          
074474       MOVE 9999999        TO W-IDORDNR7-A1-MAX                           
074475                              W-IDPRQUES-A1-MAX                           
074476       MOVE 999999999      TO W-IDARTNR-A1-MAX                            
074477     ELSE                                                                 
074478       PERFORM IMS-GU-WDC7A1                                              
074480     END-IF                                                               
074500                                                                          
074600     IF SEGMENT-FOUND                                                     
074910        MOVE SEQA-IDDISTR   TO SAVE-IDDISTR-ENTER                         
074920        MOVE SEQA-IDKUNDNR  TO SAVE-IDKUNDNR-ENTER                        
074930        MOVE SEQA-IDORDNR7  TO SAVE-IDORDNR7-ENTER                        
074940        MOVE SEQA-IDPRQUES  TO SAVE-IDPRQUES-ENTER                        
074960        MOVE SEQA-IDARTNR   TO SAVE-IDARTNR-ENTER                         
074970        MOVE W-KDPRSTA      TO SAVE-KDPRSTA-ENTER                         
074971        MOVE W-IDDISTR      TO SAVE-IDDISTR-IN                            
074980        MOVE W-IDKUNDNR     TO W-IDKUNDNR-6                               
074990        MOVE W-IDKUNDNR-6   TO SAVE-IDKUNDNR-IN                           
075000     ELSE                                                                 
075310        MOVE W-IDDISTR-A1-MIN  TO SAVE-IDDISTR-ENTER                      
075320        MOVE W-IDKUNDNR-A1-MIN TO SAVE-IDKUNDNR-ENTER                     
075330        MOVE ZERO           TO SAVE-IDORDNR7-ENTER                        
075340                               SAVE-IDPRQUES-ENTER                        
075350                               SAVE-IDARTNR-ENTER                         
075360        MOVE W-KDPRSTA      TO SAVE-KDPRSTA-ENTER                         
075361        MOVE W-IDDISTR      TO SAVE-IDDISTR-IN                            
075362        MOVE W-IDKUNDNR     TO W-IDKUNDNR-6                               
075370        MOVE W-IDKUNDNR-6   TO SAVE-IDKUNDNR-IN                           
075400     END-IF                                                               
075500     .                                                                    
075600     EJECT                                                                
075700                                                                          
075710 FB-READ-B1-KDPRSTA   SECTION.                                            
075720                                                                          
075730     IF MFS-NEXT OR MFS-ENTER                                             
075740       MOVE W-KDPRSTA      TO W-KDPRSTA-B1-MIN                            
075750                              W-KDPRSTA-B1-MAX                            
075751       MOVE W-IDDISTR-KY   TO W-IDDISTR-B1-MIN                            
075752                              W-IDDISTR-B1-MAX                            
075760       MOVE W-IDKUNDNR-KY  TO W-IDKUNDNR-B1-MIN                           
075770                              W-IDKUNDNR-B1-MAX                           
075780       MOVE W-IDORDNR7-KY  TO W-IDORDNR7-B1-MIN                           
075790                              W-IDORDNR7-B1-MAX                           
075791       MOVE W-IDARTNR-KY   TO W-IDARTNR-B1-MIN                            
075792                              W-IDARTNR-B1-MAX                            
075793       MOVE W-IDPRQUES-KY  TO W-IDPRQUES-B1-MIN                           
075794                              W-IDPRQUES-B1-MAX                           
075795                                                                          
075796       PERFORM IMS-GU-WDC7B1                                              
075797                                                                          
075799       IF W-IDDISTR > ZERO                                                
075800         MOVE W-IDDISTR    TO W-IDDISTR-B1-MAX                            
075801       ELSE                                                               
075802         MOVE 9999         TO W-IDDISTR-B1-MAX                            
075803       END-IF                                                             
075804                                                                          
075805       IF W-IDKUNDNR > ZERO                                               
075806         MOVE W-IDKUNDNR   TO W-IDKUNDNR-B1-MAX                           
075807       ELSE                                                               
075808         MOVE 9999999      TO W-IDKUNDNR-B1-MAX                           
075809       END-IF                                                             
075810                                                                          
075811       MOVE 9999999        TO W-IDORDNR7-B1-MAX                           
075812                              W-IDPRQUES-B1-MAX                           
075813       MOVE 999999999      TO W-IDARTNR-B1-MAX                            
075814     ELSE                                                                 
075815       MOVE W-KDPRSTA      TO W-KDPRSTA-B1-MIN                            
075816                              W-KDPRSTA-B1-MAX                            
075817       PERFORM IMS-GU-WDC7B1                                              
075818     END-IF                                                               
075819                                                                          
075820     IF SEGMENT-FOUND                                                     
075821        MOVE SEQB-KDPRSTA   TO SAVE-KDPRSTA-ENTER                         
075822        MOVE SEQB-IDDISTR   TO SAVE-IDDISTR-ENTER                         
075823        MOVE SEQB-IDKUNDNR  TO SAVE-IDKUNDNR-ENTER                        
075824        MOVE SEQB-IDORDNR7  TO SAVE-IDORDNR7-ENTER                        
075825        MOVE SEQB-IDARTNR   TO SAVE-IDARTNR-ENTER                         
075826        MOVE SEQB-IDPRQUES  TO SAVE-IDPRQUES-ENTER                        
075827        MOVE W-IDDISTR      TO SAVE-IDDISTR-IN                            
075828        MOVE W-IDKUNDNR     TO W-IDKUNDNR-6                               
075829        MOVE W-IDKUNDNR-6   TO SAVE-IDKUNDNR-IN                           
075830     ELSE                                                                 
075832        MOVE W-KDPRSTA      TO SAVE-KDPRSTA-ENTER                         
075833        MOVE W-IDDISTR-B1-MIN  TO SAVE-IDDISTR-ENTER                      
075834        MOVE W-IDKUNDNR-B1-MIN TO SAVE-IDKUNDNR-ENTER                     
075835        MOVE ZERO           TO SAVE-IDORDNR7-ENTER                        
075836                               SAVE-IDPRQUES-ENTER                        
075837                               SAVE-IDARTNR-ENTER                         
075838        MOVE W-IDDISTR      TO SAVE-IDDISTR-IN                            
075839        MOVE W-IDKUNDNR     TO W-IDKUNDNR-6                               
075840        MOVE W-IDKUNDNR-6   TO SAVE-IDKUNDNR-IN                           
075845     END-IF                                                               
075846     .                                                                    
075847     EJECT                                                                
075848                                                                          
075850 FC-MOVE-LINEDATA-A1   SECTION.                                           
075900                                                                          
075901     MOVE +1 TO INDX                                                      
075902                                                                          
075903     PERFORM UNTIL INDX > MAX-INDX                                        
075904       IF SEGMENT-FOUND                                                   
075905                                                                          
075906         MOVE SEQA-IDDISTR  TO MOD-IDDISTR (INDX)                         
075907                               SAVE-IDDISTR (INDX)                        
075908         MOVE SEQA-IDKUNDNR TO MOD-IDKUNDNR (INDX)                        
075909                               SAVE-IDKUNDNR (INDX)                       
075910         MOVE SEQA-KDPRSTA  TO MOD-KDPRSTA (INDX)                         
075911                               SAVE-KDPRSTA (INDX)                        
075912         MOVE SEQA-IDARTNR  TO MOD-IDARTNR (INDX)                         
075913                               SAVE-IDARTNR (INDX)                        
075914         MOVE SEQA-IDORDNR7 TO MOD-IDORDNR7 (INDX)                        
075915                               SAVE-IDORDNR7 (INDX)                       
075916         MOVE SEQA-KDVALISO TO MOD-KDVALISO (INDX)                        
075917                               SAVE-KDVALISO (INDX)                       
075918         MOVE ZERO          TO W-PRARTBTO-LOC (INDX)                      
075919                               W-PRARTNTO-LOC (INDX)                      
075920         MOVE SEQA-PRARTBTO-LOC TO W-PRARTBTO-LOC (INDX)                  
075921         MOVE W-PRARTBTO-LOC (INDX)                                       
075922                                TO MOD-PRARTBTO-LOC (INDX)                
075923                                   SAVE-PRARTBTO-LOC (INDX)               
075924         MOVE SEQA-PRARTNTO-LOC TO W-PRARTNTO-LOC (INDX)                  
075925         MOVE W-PRARTNTO-LOC (INDX)                                       
075926                                TO MOD-PRARTNTO-LOC (INDX)                
075927                                   SAVE-PRARTNTO-LOC (INDX)               
075928         MOVE SEQA-KDRAB        TO MOD-KDRAB (INDX)                       
075929                                   SAVE-KDRAB (INDX)                      
075930         MOVE SEQA-IDPRQUES     TO SAVE-IDPRQUES(INDX)                    
075931         MOVE YES               TO SAVE-RECORD-FLAG (INDX)                
075932                                                                          
075933         MOVE MFS-ERASE-FIELD TO MOD-KDCMD (INDX)                         
075934         ADD 1  TO INDX                                                   
075935         PERFORM IMS-GN-WDC7A1                                            
075936       ELSE                                                               
075937         MOVE MFS-ERASE-FIELD TO MOD-IDDISTR (INDX)                       
075938                                 MOD-IDKUNDNR (INDX)                      
075939                                 MOD-KDPRSTA (INDX)                       
075940                                 MOD-KDCMD (INDX)                         
075941                                 MOD-IDKUNDNR (INDX)                      
075942                                 MOD-KDPRSTA (INDX)                       
075943                                 MOD-IDARTNR (INDX)                       
075944                                 MOD-IDORDNR7 (INDX)                      
075945                                 MOD-KDVALISO (INDX)                      
075946                                 MOD-PRARTBTO-LOC (INDX)                  
075947                                 MOD-PRARTNTO-LOC (INDX)                  
075948                                 MOD-KDRAB (INDX)                         
075949         MOVE W-INIT-INPUT (INDX)  TO SAVE-INPUT (INDX)                   
075950         IF INDX = 1                                                      
075951            MOVE INF-MISSING          TO MED-IDMFSFEL                     
075952            CALL WMEDKONV USING MED-WMEDAREA                              
075953            MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                            
075954         END-IF                                                           
075955         ADD 1  TO INDX                                                   
075956       END-IF                                                             
075957     END-PERFORM                                                          
075958                                                                          
075959*- OM DET FINNS FLERA SEGMENT.                                            
075960                                                                          
075961     IF SEGMENT-FOUND                                                     
075962                                                                          
075963       MOVE SEQA-KDPRSTA     TO  SAVE-KDPRSTA-NEXT                        
075964       MOVE SEQA-IDDISTR     TO  SAVE-IDDISTR-NEXT                        
075965       MOVE SEQA-IDKUNDNR    TO  SAVE-IDKUNDNR-NEXT                       
075966       MOVE SEQA-IDORDNR7    TO  SAVE-IDORDNR7-NEXT                       
075967       MOVE SEQA-IDARTNR     TO  SAVE-IDARTNR-NEXT                        
075968       MOVE SEQA-IDPRQUES    TO  SAVE-IDPRQUES-NEXT                       
075969                                                                          
075970       IF NOT MFS-UPDATE                                                  
075971          MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                       
075972          CALL WMEDKONV USING MED-WMEDAREA                                
075973          MOVE MED-TEMFSINF  TO MOD-TEMFSINF                              
075974       END-IF                                                             
075975     ELSE                                                                 
075976       MOVE SAVE-IDDISTR-ENTER  TO  SAVE-IDDISTR-NEXT                     
075977       MOVE SAVE-IDKUNDNR-ENTER TO  SAVE-IDKUNDNR-NEXT                    
075978       MOVE SAVE-IDPRQUES-ENTER TO  SAVE-IDPRQUES-NEXT                    
075979       MOVE SAVE-IDORDNR7-ENTER TO  SAVE-IDORDNR7-NEXT                    
075980       MOVE SAVE-KDPRSTA-ENTER  TO  SAVE-KDPRSTA-NEXT                     
075981       MOVE SAVE-IDARTNR-ENTER  TO  SAVE-IDARTNR-NEXT                     
075982       IF NOT MFS-UPDATE                                                  
075983          MOVE INF-LAST-PAGE TO MED-IDMFSINF                              
075984          CALL WMEDKONV      USING MED-WMEDAREA                           
075985          MOVE MED-TEMFSINF  TO MOD-TEMFSINF                              
075986       END-IF                                                             
075987     END-IF                                                               
075988     .                                                                    
075989     EJECT                                                                
075990                                                                          
075991 FD-MOVE-LINEDATA-B1   SECTION.                                           
075992                                                                          
075993     MOVE +1 TO INDX                                                      
075994                                                                          
075995     PERFORM UNTIL INDX > MAX-INDX                                        
075996       IF SEGMENT-FOUND                                                   
075997                                                                          
075998         MOVE SEQB-IDDISTR  TO MOD-IDDISTR (INDX)                         
075999                               SAVE-IDDISTR (INDX)                        
076000         MOVE SEQB-IDKUNDNR TO MOD-IDKUNDNR (INDX)                        
076001                               SAVE-IDKUNDNR (INDX)                       
076002         MOVE SEQB-KDPRSTA  TO MOD-KDPRSTA (INDX)                         
076003                               SAVE-KDPRSTA (INDX)                        
076004         MOVE SEQB-IDARTNR  TO MOD-IDARTNR (INDX)                         
076005                               SAVE-IDARTNR (INDX)                        
076006         MOVE SEQB-IDORDNR7 TO MOD-IDORDNR7 (INDX)                        
076007                               SAVE-IDORDNR7 (INDX)                       
076008         MOVE SEQB-KDVALISO TO MOD-KDVALISO (INDX)                        
076009                               SAVE-KDVALISO (INDX)                       
076010         MOVE ZERO          TO W-PRARTBTO-LOC (INDX)                      
076011                               W-PRARTNTO-LOC (INDX)                      
076012         MOVE SEQB-PRARTBTO-LOC TO W-PRARTBTO-LOC (INDX)                  
076013         MOVE W-PRARTBTO-LOC (INDX)                                       
076014                                TO MOD-PRARTBTO-LOC (INDX)                
076015                                   SAVE-PRARTBTO-LOC (INDX)               
076016         MOVE SEQB-PRARTNTO-LOC TO W-PRARTNTO-LOC (INDX)                  
076017         MOVE W-PRARTNTO-LOC (INDX)                                       
076018                                TO MOD-PRARTNTO-LOC (INDX)                
076019                                   SAVE-PRARTNTO-LOC (INDX)               
076020         MOVE SEQB-KDRAB        TO MOD-KDRAB (INDX)                       
076021                                   SAVE-KDRAB (INDX)                      
076022         MOVE SEQB-IDPRQUES     TO SAVE-IDPRQUES(INDX)                    
076023         MOVE YES               TO SAVE-RECORD-FLAG (INDX)                
076024                                                                          
076025         MOVE MFS-ERASE-FIELD TO MOD-KDCMD (INDX)                         
076026         ADD 1  TO INDX                                                   
076027         PERFORM IMS-GN-WDC7B1                                            
076028       ELSE                                                               
076029         MOVE MFS-ERASE-FIELD TO MOD-IDDISTR (INDX)                       
076030                                 MOD-IDKUNDNR (INDX)                      
076031                                 MOD-KDPRSTA (INDX)                       
076032                                 MOD-KDCMD (INDX)                         
076033                                 MOD-IDKUNDNR (INDX)                      
076034                                 MOD-KDPRSTA (INDX)                       
076035                                 MOD-IDARTNR (INDX)                       
076036                                 MOD-IDORDNR7 (INDX)                      
076037                                 MOD-KDVALISO (INDX)                      
076038                                 MOD-PRARTBTO-LOC (INDX)                  
076039                                 MOD-PRARTNTO-LOC (INDX)                  
076040                                 MOD-KDRAB (INDX)                         
076041         MOVE W-INIT-INPUT (INDX)  TO SAVE-INPUT (INDX)                   
076042         IF INDX = 1                                                      
076043            MOVE INF-MISSING          TO MED-IDMFSFEL                     
076044            CALL WMEDKONV USING MED-WMEDAREA                              
076045            MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                            
076046         END-IF                                                           
076047         ADD 1  TO INDX                                                   
076048       END-IF                                                             
076049     END-PERFORM                                                          
076050                                                                          
076051*- OM DET FINNS FLERA SEGMENT.                                            
076052                                                                          
076053     IF SEGMENT-FOUND                                                     
076054                                                                          
076055       MOVE SEQB-KDPRSTA     TO  SAVE-KDPRSTA-NEXT                        
076056       MOVE SEQB-IDDISTR     TO  SAVE-IDDISTR-NEXT                        
076057       MOVE SEQB-IDKUNDNR    TO  SAVE-IDKUNDNR-NEXT                       
076058       MOVE SEQB-IDORDNR7    TO  SAVE-IDORDNR7-NEXT                       
076059       MOVE SEQB-IDARTNR     TO  SAVE-IDARTNR-NEXT                        
076060       MOVE SEQB-IDPRQUES    TO  SAVE-IDPRQUES-NEXT                       
076061                                                                          
076062       IF NOT MFS-UPDATE                                                  
076063          MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                       
076064          CALL WMEDKONV USING MED-WMEDAREA                                
076065          MOVE MED-TEMFSINF  TO MOD-TEMFSINF                              
076066       END-IF                                                             
076067     ELSE                                                                 
076068       MOVE SAVE-IDDISTR-ENTER  TO  SAVE-IDDISTR-NEXT                     
076069       MOVE SAVE-IDKUNDNR-ENTER TO  SAVE-IDKUNDNR-NEXT                    
076070       MOVE SAVE-IDPRQUES-ENTER TO  SAVE-IDPRQUES-NEXT                    
076071       MOVE SAVE-IDORDNR7-ENTER TO  SAVE-IDORDNR7-NEXT                    
076072       MOVE SAVE-KDPRSTA-ENTER  TO  SAVE-KDPRSTA-NEXT                     
076073       MOVE SAVE-IDARTNR-ENTER  TO  SAVE-IDARTNR-NEXT                     
076074       IF NOT MFS-UPDATE                                                  
076075          MOVE INF-LAST-PAGE TO MED-IDMFSINF                              
076076          CALL WMEDKONV      USING MED-WMEDAREA                           
076077          MOVE MED-TEMFSINF  TO MOD-TEMFSINF                              
076078       END-IF                                                             
076079     END-IF                                                               
076080     .                                                                    
076081     EJECT                                                                
076082                                                                          
081500 G-CHECK-INPUT SECTION.                                                   
081600                                                                          
081700     MOVE YES  TO INDATA-SW                                               
081800     IF SAVE-IDTRANS = '3306' OR '0551'                                   
081900        MOVE SAVE-IDDISTR-ENTER  TO W-IDDISTR-KY                          
082000        MOVE SAVE-IDKUNDNR-ENTER TO W-IDKUNDNR-KY                         
082100        MOVE SAVE-IDORDNR7-ENTER TO W-IDORDNR7-KY                         
082200        MOVE SAVE-IDPRQUES-ENTER TO W-IDPRQUES-KY                         
082300     END-IF                                                               
082400                                                                          
082500     IF MID-W30306I1 = ALL '+'                                            
082600       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
082700       CALL WMEDKONV USING MED-WMEDAREA                                   
082800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
082900       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
083000       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
083100       MOVE NOO TO INDATA-SW                                              
083200     ELSE                                                                 
083300                                                                          
083400       MOVE 1   TO INDX                                                   
083500       MOVE NOO TO W-SELECT-FLAG                                          
083600       PERFORM UNTIL INDX > MAX-INDX                                      
083700          MOVE ZERO  TO W-PRARTBTO-LOC (INDX)                             
083800                        W-PRARTNTO-LOC (INDX)                             
083900          IF MID-KDCMD (INDX) NOT = ALL '+'                               
084000             IF MID-KDCMD (INDX) = 'S'                                    
084100               IF W-SELECT-NO                                             
084200                  MOVE YES  TO W-SELECT-FLAG                              
084300                  IF MID-PRARTBTO-LOC (INDX) NOT = ALL '+'  OR            
084400                     MID-PRARTNTO-LOC (INDX) NOT = ALL '+' OR             
084500                     MID-KDRAB (INDX)        NOT = ALL '+'                
084600                                                                          
084700                     IF MID-PRARTBTO-LOC (INDX) NOT = ALL '+'             
084800                        INSPECT MID-PRARTBTO-LOC (INDX)                   
084900                               REPLACING LEADING SPACE BY ZERO            
085000                        MOVE MID-PRARTBTO-LOC(INDX) TO                    
085100                                              DEC-IDFRIDATA               
085200                        PERFORM S02-HANDLE-ALPHA-TO-NUM                   
085300                        IF DEC-KDSVAR-OK                                  
085400                           MOVE DEC-IDEDITDATA  TO                        
085500                                       W-PRARTBTO-LOC (INDX)              
085600                        ELSE                                              
085700                           MOVE MFS-ALPHA-FIELD-WRONG    TO               
085800                                      W-PRARTBTO-LOC-ATTR (INDX)          
085900                           MOVE NOO TO INDATA-SW                          
086000                        END-IF                                            
086100                     END-IF                                               
086200                                                                          
086300                     IF MID-PRARTNTO-LOC (INDX) NOT = ALL '+'             
086400                        INSPECT MID-PRARTNTO-LOC (INDX)                   
086500                                REPLACING LEADING SPACE BY ZERO           
086600                        MOVE MID-PRARTNTO-LOC(INDX)   TO                  
086700                                                 DEC-IDFRIDATA            
086800                        PERFORM S02-HANDLE-ALPHA-TO-NUM                   
086900                        IF DEC-KDSVAR-OK                                  
087000                           MOVE DEC-IDEDITDATA        TO                  
087100                                         W-PRARTNTO-LOC (INDX)            
087200                        ELSE                                              
087300                           MOVE MFS-ALPHA-FIELD-WRONG TO                  
087400                                        W-PRARTNTO-LOC-ATTR (INDX)        
087500                           MOVE NOO  TO INDATA-SW                         
087600                        END-IF                                            
087700                     END-IF                                               
087800                  ELSE                                                    
087900                     MOVE MFS-ALPHA-FIELD-WRONG TO                        
088000                                        W-KDCMD-ATTR (INDX)               
088100                     MOVE NOO TO INDATA-SW                                
088200                  END-IF                                                  
088300               ELSE                                                       
088400                  MOVE MFS-ALPHA-FIELD-WRONG TO                           
088500                                       W-KDCMD-ATTR (INDX)                
088600                  MOVE NOO TO INDATA-SW                                   
088700                  MOVE 'MORE THAN ONE LINES ARE SELECTED ' TO             
088800                                      MOD-TEMFSINF                        
088900               END-IF                                                     
089000             ELSE                                                         
089100               IF MID-KDCMD (INDX) > SPACE                                
089200                  MOVE MFS-ALPHA-FIELD-WRONG TO                           
089300                                        W-KDCMD-ATTR (INDX)               
089400                  MOVE NOO TO INDATA-SW                                   
089500               ELSE                                                       
089600                  MOVE MFS-ERASE-FIELD  TO MOD-KDCMD (INDX)               
089700               END-IF                                                     
089800             END-IF                                                       
089900          END-IF                                                          
090000          ADD 1  TO INDX                                                  
090100       END-PERFORM                                                        
090200                                                                          
090300       IF INDATA-WRONG                                                    
090400         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
090500         CALL WMEDKONV USING MED-WMEDAREA                                 
090600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
090700         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
090800         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
090900       END-IF                                                             
091000       IF W-SELECT-NO                                                     
091100         MOVE ERR-SELECT-LINE      TO MED-IDMFSFEL                        
091200         CALL WMEDKONV USING MED-WMEDAREA                                 
091300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
091400         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
091500         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
091600         MOVE NOO TO INDATA-SW                                            
091700       END-IF                                                             
091800     END-IF                                                               
091900     .                                                                    
092000     EJECT                                                                
092100                                                                          
092200 H-UPDATE SECTION.                                                        
092300                                                                          
092400       MOVE 1  TO INDX                                                    
092500       MOVE  NOO   TO W-CHANGE-FLAG                                       
092600       PERFORM UNTIL INDX > MAX-INDX                                      
092700                                                                          
092800          IF MID-KDCMD (INDX)  = 'S'                                      
092900             IF MID-PRARTBTO-LOC (INDX) NOT = ALL '+' OR                  
093000                MID-PRARTNTO-LOC (INDX) NOT = ALL '+' OR                  
093100                MID-KDRAB (INDX)        NOT = ALL '+'                     
093200                MOVE SAVE-IDDISTR (INDX)  TO  W-IDDISTR-UP                
093300                MOVE SAVE-IDKUNDNR (INDX) TO  W-IDKUNDNR-UP               
093400                MOVE SAVE-IDORDNR7 (INDX) TO  W-IDORDNR7-UP               
093500                MOVE SAVE-IDPRQUES (INDX) TO  W-IDPRQUES-UP               
093600                PERFORM IMS-GHU-WDC711                                    
093700                                                                          
093800                IF SEGMENT-FOUND                                          
093900                   MOVE  NOO   TO W-CHANGE-FLAG                           
094000                  IF MID-PRARTBTO-LOC (INDX) NOT = ALL '+'                
094100                     IF  W-PRARTBTO-LOC (INDX) NOT =                      
094200                                              LPRQ-PRARTBTO-LOC           
094300                         MOVE W-PRARTBTO-LOC (INDX)                       
094400                                          TO  LPRQ-PRARTBTO-LOC           
094500                         MOVE 'M'         TO  LPRQ-KDPRSTA                
094600                         MOVE  YES        TO  W-CHANGE-FLAG               
094700                     END-IF                                               
094800                  END-IF                                                  
094900                  IF MID-PRARTNTO-LOC (INDX)   NOT = ALL '+'              
095000                     IF  W-PRARTNTO-LOC (INDX) NOT =                      
095100                                              LPRQ-PRARTNTO-LOC           
095200                         MOVE W-PRARTNTO-LOC (INDX)                       
095300                                          TO  LPRQ-PRARTNTO-LOC           
095400                         MOVE 'M'         TO  LPRQ-KDPRSTA                
095500                         MOVE  YES        TO  W-CHANGE-FLAG               
095600                     END-IF                                               
095700                  END-IF                                                  
095800                  IF MID-KDRAB (INDX)    NOT = ALL '+'                    
095900                     IF  MID-KDRAB (INDX)  NOT = LPRQ-KDRAB               
096000                         MOVE MID-KDRAB (INDX)  TO LPRQ-KDRAB             
096100                         MOVE 'M'               TO LPRQ-KDPRSTA           
096200                         MOVE  YES              TO  W-CHANGE-FLAG         
096300                     END-IF                                               
096400                  END-IF                                                  
096500                  IF  W-CHANGE-DATA                                       
096600                      PERFORM IMS-REPL-WDC711                             
096700                      MOVE INF-UPDATE-DONE TO MED-IDMFSINF                
096800                      CALL WMEDKONV USING MED-WMEDAREA                    
096900                      MOVE MED-MFSINF TO MOD-TEMFSINF                     
097000                  ELSE                                                    
097100                      MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL           
097200                      MOVE MFS-ALPHA-FIELD-WRONG TO                       
097300                                     W-KDCMD-ATTR (INDX)                  
097400                      CALL WMEDKONV USING MED-WMEDAREA                    
097500                      MOVE MED-MFSFEL TO MOD-TEMFSFEL                     
097600                      PERFORM MFS-DONT-TOUCH-FIELD-IN                     
097700                      PERFORM MFS-DONT-TOUCH-FIELD-OUT                    
097800                      MOVE NOO TO INDATA-SW                               
097900                  END-IF                                                  
098000                END-IF                                                    
098100             END-IF                                                       
098200          END-IF                                                          
098300                                                                          
098400          ADD 1  TO INDX                                                  
098500                                                                          
098600       END-PERFORM                                                        
098700                                                                          
098800     .                                                                    
098900     EJECT                                                                
099000                                                                          
099100 I-OPEN-CLOSE-FIELD SECTION.                                              
099200                                                                          
099300     MOVE 1 TO INDX                                                       
099400     PERFORM UNTIL INDX > MAX-INDX                                        
099500                                                                          
099600       IF SAVE-RECORD-FLAG (INDX) = YES                                   
099700          IF SAVE-KDPRSTA (INDX) = 'M'                                    
099800            MOVE W-PRARTBTO-LOC-ATTR (INDX)  TO                           
099900                             MOD-PRARTBTO-LOC-ATTR (INDX)                 
100000            MOVE W-PRARTNTO-LOC-ATTR (INDX)  TO                           
100100                             MOD-PRARTNTO-LOC-ATTR (INDX)                 
100200            MOVE W-KDRAB-ATTR (INDX)  TO                                  
100300                             MOD-KDRAB-ATTR (INDX)                        
100400            MOVE W-KDCMD-ATTR (INDX)  TO                                  
100500                             MOD-KDCMD-ATTR (INDX)                        
100600          ELSE                                                            
100700            IF SAVE-KDPRSTA (INDX) = 'N' OR 'X'                           
100800              MOVE MFS-CLOSE-FIELD    TO                                  
100900                             MOD-PRARTBTO-LOC-ATTR (INDX)                 
101000                             MOD-PRARTNTO-LOC-ATTR (INDX)                 
101100                             MOD-KDRAB-ATTR (INDX)                        
101200                             MOD-KDCMD-ATTR (INDX)                        
101300            ELSE                                                          
101400              IF SAVE-PRARTBTO-LOC (INDX) = 0                             
101500                MOVE W-PRARTBTO-LOC-ATTR (INDX)  TO                       
101600                                 MOD-PRARTBTO-LOC-ATTR (INDX)             
101700              ELSE                                                        
101800                MOVE MFS-CLOSE-FIELD TO                                   
101900                                 MOD-PRARTBTO-LOC-ATTR (INDX)             
102000              END-IF                                                      
102100              IF SAVE-PRARTNTO-LOC (INDX) = 0                             
102200                MOVE W-PRARTNTO-LOC-ATTR (INDX) TO                        
102300                                 MOD-PRARTNTO-LOC-ATTR (INDX)             
102400                MOVE W-KDRAB-ATTR (INDX)  TO                              
102500                                 MOD-KDRAB-ATTR (INDX)                    
102600              ELSE                                                        
102700                MOVE MFS-CLOSE-FIELD TO                                   
102800                                 MOD-PRARTNTO-LOC-ATTR (INDX)             
102900                                 MOD-KDRAB-ATTR (INDX)                    
103000              END-IF                                                      
103100              IF SAVE-PRARTBTO-LOC (INDX) = 0  OR                         
103200                 SAVE-PRARTNTO-LOC (INDX) = 0                             
103300                MOVE W-KDCMD-ATTR (INDX)  TO                              
103400                                MOD-KDCMD-ATTR (INDX)                     
103500              ELSE                                                        
103600                MOVE MFS-CLOSE-FIELD TO                                   
103700                                MOD-KDCMD-ATTR (INDX)                     
103800              END-IF                                                      
103900            END-IF                                                        
104000          END-IF                                                          
104100       ELSE                                                               
104200          MOVE MFS-CLOSE-FIELD TO                                         
104300                           MOD-PRARTBTO-LOC-ATTR (INDX)                   
104400                           MOD-PRARTNTO-LOC-ATTR (INDX)                   
104500                           MOD-KDRAB-ATTR (INDX)                          
104600                           MOD-KDCMD-ATTR (INDX)                          
104700                                                                          
104800       END-IF                                                             
104900       ADD 1  TO INDX                                                     
105000                                                                          
105100     END-PERFORM                                                          
105200     .                                                                    
105300     EJECT                                                                
105400 S04-SEND-OPEN SECTION.                                                   
105500     MOVE 'OPEN' TO SEND-KDFUNC                                           
105600     MOVE LPRQ-ADDISPABS TO SEND-ADDISPABS                                
105700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
105800                         SEND-OPEN-AREA                                   
105900******** OM FEL                                                           
106000     IF SEND-KDRC  > 0                                                    
106100        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
106200        STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                    
106300        DELIMITED BY SIZE INTO FELTEXT                                    
106400        DISPLAY FELTEXT                                                   
106500        CALL FELLOG                                                       
106600     END-IF                                                               
106700     .                                                                    
106800     EJECT                                                                
106900 S05-SEND-MESSAGE SECTION.                                                
107000                                                                          
107100     MOVE 'PUT' TO SEND-KDFUNC                                            
107200     MOVE LENGTH OF SEND-DATA TO SEND-KVDLEN                              
107300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
107400                         SEND-KVDLEN                                      
107500                         SEND-DATA                                        
107600******** OM FEL                                                           
107700     IF SEND-KDRC  > 0                                                    
107800        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
107900        STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                    
108000        DELIMITED BY SIZE INTO FELTEXT                                    
108100        DISPLAY FELTEXT                                                   
108200        CALL FELLOG                                                       
108300     END-IF                                                               
108400     .                                                                    
108500     EJECT                                                                
108600 S06-SEND-CLOSE SECTION.                                                  
108700                                                                          
108800       MOVE 'CLOSE' TO SEND-KDFUNC                                        
108900       CALL WZ01SEND USING SEND-CONTROL-AREA                              
109000******** OM FEL                                                           
109100     IF SEND-KDRC  > 0                                                    
109200        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
109300        STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                   
109400        DELIMITED BY SIZE INTO FELTEXT                                    
109500        DISPLAY FELTEXT                                                   
109600        CALL FELLOG                                                       
109700     END-IF                                                               
109800     .                                                                    
109900     EJECT                                                                
110000 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
110100                                                                          
110200*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
110300     MOVE  1    TO  INDX                                                  
110400     PERFORM UNTIL INDX > MAX-INDX                                        
110500         MOVE MFS-ERASE-FIELD TO MOD-KDCMD(INDX)                          
110600                                 MOD-IDDISTR(INDX)                        
110700                                 MOD-IDKUNDNR(INDX)                       
110800                                 MOD-KDPRSTA(INDX)                        
110900                                 MOD-IDARTNR(INDX)                        
111000                                 MOD-IDORDNR7(INDX)                       
111100                                 MOD-KDVALISO(INDX)                       
111200                                 MOD-PRARTBTO-LOC(INDX)                   
111300                                 MOD-PRARTNTO-LOC(INDX)                   
111400                                 MOD-KDRAB(INDX)                          
111500         ADD 1  TO  INDX                                                  
111600     END-PERFORM                                                          
111700     .                                                                    
111800     SKIP3                                                                
111900 MFS-ERASE-FIELD-IN SECTION.                                              
112000                                                                          
112100*    --- ALLA INDATA-FÄLT                                                 
112200     MOVE MFS-ERASE-FIELD  TO MOD-IDDISTR-IN                              
112300                              MOD-IDKUNDNR-IN                             
112400                              MOD-KDPRSTA-IN                              
112500     .                                                                    
112600     EJECT                                                                
112700 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
112800                                                                          
112900*    --- ALLA UTDATA-FÄLT                                                 
113000*    --- INCL SCROLL KEYS AND LINEDATA                                    
113100     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-UT                        
113200                                    MOD-IDKUNDNR-UT                       
113300                                    MOD-KDPRSTA-UT                        
113400     MOVE +1 TO INDX                                                      
113500     PERFORM UNTIL INDX > MAX-INDX                                        
113600       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
113700       ADD +1 TO INDX                                                     
113800     END-PERFORM                                                          
113900     .                                                                    
114000     EJECT                                                                
114100     SKIP2                                                                
114200 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
114300                                                                          
114400*    --- OUTDATA FIELD ON SCROLL KEYS                                     
114500     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMD   (INDX)                    
114600                                    MOD-IDDISTR (INDX)                    
114700                                    MOD-IDKUNDNR (INDX)                   
114800                                    MOD-KDPRSTA (INDX)                    
114900                                    MOD-IDARTNR (INDX)                    
115000                                    MOD-IDORDNR7 (INDX)                   
115100                                    MOD-KDVALISO (INDX)                   
115200                                    MOD-PRARTBTO-LOC (INDX)               
115300                                    MOD-PRARTNTO-LOC (INDX)               
115400                                    MOD-KDRAB (INDX)                      
115500     .                                                                    
115600     SKIP3                                                                
115700 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
115800                                                                          
115900*    --- ALLA INDATA-FÄLT                                                 
116000     MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDDISTR-IN                       
116100                                     MOD-IDKUNDNR-IN                      
116200                                     MOD-KDPRSTA-IN                       
116300     .                                                                    
116400     EJECT                                                                
116500                                                                          
119100 S02-HANDLE-ALPHA-TO-NUM SECTION.                                         
119200                                                                          
119300     MOVE 7            TO DEC-KVHELTAL                                    
119400     MOVE 2            TO DEC-KVDECIMAL                                   
119500     CALL WDECEDIT USING DEC-IDFRIDATA                                    
119600                         DEC-IDEDITDATA                                   
119700                         DEC-KVHELTAL                                     
119800                         DEC-KVDECIMAL                                    
119900                         DEC-KDSVAR                                       
120000     .                                                                    
120100     SKIP2                                                                
120200                                                                          
120300* --- IMS SECTIONS ---                                                    
120400                                                                          
120500 IMS-GET-MSG SECTION.                                                     
120600                                                                          
120700     MOVE '  QC' TO GOOD-STATUSCODES                                      
120800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
120900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
121000     PERFORM IMS-STATUSCHECK                                              
121100     .                                                                    
121200     SKIP3                                                                
121300                                                                          
121400 IMS-INSERT-MSG SECTION.                                                  
121500                                                                          
121600     IF MSGI-IDLAND-SPR = 'SE'                                            
121700       MOVE 'N' TO MFS-KDHUVOMR                                           
121800     END-IF                                                               
121900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
122000     MOVE SPACE TO GOOD-STATUSCODES                                       
122100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
122200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
122300     PERFORM IMS-STATUSCHECK                                              
122400     .                                                                    
122500     EJECT                                                                
122600 IMS-GU-WDC7A1   SECTION.                                                 
122700                                                                          
122800     STRING 'WDC7A1  (WDC7A1KY>=' W-WDC7A1KY-MIN-X                        
122900                    '&WDC7A1KY<=' W-WDC7A1KY-MAX-X ')'                    
123100          DELIMITED BY SIZE INTO SSA1                                     
123200     MOVE '  GE' TO GOOD-STATUSCODES                                      
123300     CALL CBLTDLI USING GU WDC7A-PCB DLI-IO-WDC7A1 SSA1                   
123400     MOVE WDC7A-STATUS-CODE TO STATUS-WS                                  
123500     PERFORM IMS-STATUSCHECK                                              
123600     .                                                                    
123700     EJECT                                                                
123800 IMS-GN-WDC7A1   SECTION.                                                 
123900                                                                          
124000     STRING 'WDC7A1  (WDC7A1KY>=' W-WDC7A1KY-MIN-X                        
124100                    '&WDC7A1KY<=' W-WDC7A1KY-MAX-X ')'                    
124300          DELIMITED BY SIZE INTO SSA1                                     
124400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
124500     CALL CBLTDLI USING GN WDC7A-PCB DLI-IO-WDC7A1 SSA1                   
124600     MOVE WDC7A-STATUS-CODE TO STATUS-WS                                  
124700     PERFORM IMS-STATUSCHECK                                              
124800     .                                                                    
124900     EJECT                                                                
124910 IMS-GU-WDC7B1   SECTION.                                                 
124920                                                                          
124930     STRING 'WDC7B1  (WDC7B1KY>=' W-WDC7B1KY-MIN-X                        
124940                    '&WDC7B1KY<=' W-WDC7B1KY-MAX-X ')'                    
124950          DELIMITED BY SIZE INTO SSA1                                     
124960     MOVE '  GE' TO GOOD-STATUSCODES                                      
124970     CALL CBLTDLI USING GU WDC7B-PCB DLI-IO-WDC7B1 SSA1                   
124980     MOVE WDC7B-STATUS-CODE TO STATUS-WS                                  
124990     PERFORM IMS-STATUSCHECK                                              
124991     .                                                                    
124992     EJECT                                                                
124993 IMS-GN-WDC7B1   SECTION.                                                 
124994                                                                          
124995     STRING 'WDC7B1  (WDC7B1KY>=' W-WDC7B1KY-MIN-X                        
124996                    '&WDC7B1KY<=' W-WDC7B1KY-MAX-X ')'                    
124997          DELIMITED BY SIZE INTO SSA1                                     
124998     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
124999     CALL CBLTDLI USING GN WDC7B-PCB DLI-IO-WDC7B1 SSA1                   
125000     MOVE WDC7B-STATUS-CODE TO STATUS-WS                                  
125001     PERFORM IMS-STATUSCHECK                                              
125002     .                                                                    
125003     EJECT                                                                
127100 IMS-GU-WDC711 SECTION.                                                   
127200                                                                          
127300     STRING 'WDC701  *PD(WDC701KY =' W-WDC701KY-X ')'                     
127400          DELIMITED BY SIZE INTO SSA1                                     
127500     STRING 'WDC711  (IDPRQUES =' W-IDPRQUESKY-X ')'                      
127600            DELIMITED BY SIZE INTO SSA2                                   
127700     MOVE '  GE' TO GOOD-STATUSCODES                                      
127800     CALL CBLTDLI USING GU WDC72-PCB DLI-IO-WDC7   SSA1  SSA2             
127900     MOVE WDC72-STATUS-CODE TO STATUS-WS                                  
128000     PERFORM IMS-STATUSCHECK                                              
128100     .                                                                    
128200     SKIP3                                                                
128300                                                                          
128400 IMS-GHU-WDC711 SECTION.                                                  
128500                                                                          
128600     STRING 'WDC701  (WDC701KY =' W-WDC701UP-X ')'                        
128700          DELIMITED BY SIZE INTO SSA1                                     
128800     STRING 'WDC711  (IDPRQUES =' W-IDPRQUESUP-X ')'                      
128900          DELIMITED BY SIZE INTO SSA2                                     
129000     MOVE '  GE' TO GOOD-STATUSCODES                                      
129100     CALL CBLTDLI USING GHU WDC72-PCB DLI-IO-WDC711 SSA1 SSA2             
129200     MOVE WDC72-STATUS-CODE TO STATUS-WS                                  
129300     PERFORM IMS-STATUSCHECK                                              
129400     .                                                                    
129500     SKIP3                                                                
129600                                                                          
129700 IMS-REPL-WDC711 SECTION.                                                 
129800                                                                          
129900     MOVE '  ' TO GOOD-STATUSCODES                                        
130000     CALL CBLTDLI USING REPL WDC72-PCB DLI-IO-WDC711                      
130100     MOVE WDC72-STATUS-CODE TO STATUS-WS                                  
130200     PERFORM IMS-STATUSCHECK                                              
130300     .                                                                    
130400     EJECT                                                                
130500                                                                          
130600 IMS-STATUSCHECK SECTION.                                                 
130700                                                                          
130800     SET STATUS-IX TO 1                                                   
130900     SEARCH GOOD-STATUS                                                   
131000       AT END                                                             
131100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
131200         DELIMITED BY SIZE INTO ERROR-TEXT                                
131300         CALL FELLOG                                                      
131400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
131500         CONTINUE                                                         
131600     END-SEARCH                                                           
131700     .                                                                    
