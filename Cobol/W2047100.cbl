000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2047100.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   12/06/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        LARMKÖ-BILD SOM VISAR ALLA NYA TPO-LARM                          
000900*        PROGRAMMET LÄSER LARMKÖBASEN (WDR5) OCH LISTAR                   
001000*        ALLA 2224-SEGMENT FÖR ANSKAFFARE. ÄR INTE ANSKAFFARE             
001100*        ANGIVEN SOM NYCKEL, HÄMTAS DEN UPPGIFTEN FRÅN WDK7 FÖRST.        
001200*                                                                         
001300*        THE PROGRAM UPDATES   WDR5                                       
001400*        THE PROGRAM READS     WDK6                                       
001500*        THE PROGRAM READS     WDK7                                       
001600*        THE PROGRAM READS     WDB6                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSACTION: W2T471                                              
002000*        MID:         W2I47101                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        MOD:         W2O47101                                            
002400*                                                                         
002500*   ÄNDRINGAR:                                                            
002600*    2016-07-05    E'TRACKER 10273773                                     
002700*                  223-ALARM BACKORDER FROM WHEN                          
002800*                                                                         
002900*    2017-09-18    E'TRACKER 10302687                                     
003000*                  LOCAL SOURCING                                         
003100*                                                                         
003200 ENVIRONMENT DIVISION.                                                    
003300                                                                          
003400 DATA DIVISION.                                                           
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W2047100'.            
003800 77  WS-CURRENT-SECTION          PIC X(30)   VALUE 'START'.               
003900 77  WS-CURRENT-IMS-SECTION      PIC X(30)   VALUE SPACE.                 
004000                                                                          
004100 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
004200     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
004300     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
004400                                                                          
004500 77  S-MARKED-SW                 PIC X(1)    VALUE SPACE.                 
004600     88  S-MARKED                            VALUE 'J'.                   
004700                                                                          
004800 77  W-DC-OK                     PIC X(1)    VALUE SPACE.                 
004900                                                                          
005000 77  TRANS-TO-UPD-PGM-SW         PIC X(1)    VALUE SPACE.                 
005100     88  TRANS-TO-UPD-PGM                    VALUE 'J'.                   
005200                                                                          
005300 77  ENTER-KEY-SW                PIC X(1)    VALUE SPACE.                 
005400     88  ENTER-KEY-IFYLLD                    VALUE 'J'.                   
005500                                                                          
005600     EJECT                                                                
005700 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
005800*01 -COPY WWIDFTG                                                         
005900     EJECT                                                                
006000                                                                          
006100 01  ARBETSFAELT.                                                         
006200     03  WS-INPUT                PIC X(1)    VALUE '+'.                   
006300     03  WS-KDLARM-VAL           PIC S9(3)   VALUE ZERO.                  
006400     03  WS-VALD-RAD             PIC S9(4)   VALUE +0 COMP SYNC.          
006500     03  WS-KDLARM-X.                                                     
006600         05 WS-KDLARM            PIC 9(3)    VALUE ZERO.                  
006700     03  WS-TISENBEK-DAG-YYMMDD  PIC S9(06)  VALUE ZERO.                  
006800                                                                          
006900*01  -COPY WWDCKONS                                                       
007000                                                                          
007100                                                                          
007200 01  WS-LARMTEXTER.                                                       
007300     03  WS-TEORSLRM-200         PIC X(25)                                
007400                                 VALUE 'INVESTIGATION BALANCE   '.        
007500     03  WS-TEORSLRM-210         PIC X(25)                                
007600                                 VALUE 'BACKORDER               '.        
007700     03  WS-TEORSLRM-222         PIC X(25)                                
007800                                 VALUE 'BELOW SAFETY STOCK      '.        
007900     03  WS-TEORSLRM-223         PIC X(25)                                
008000                                 VALUE 'NOT SUFFICIENT CALL OFFS'.        
008100     03  WS-TEORSLRM-500         PIC X(25)                                
008200                                 VALUE 'VOR-QUEUE               '.        
008300     03  WS-TEORSLRM-600         PIC X(25)                                
008400                                 VALUE 'PUBLICATION WEEK CHANGED'.        
008500     03  WS-TEORSLRM-601         PIC X(25)                                
008600                                 VALUE 'EOP UPD FROM KDP - PG15 '.        
008700     03  WS-TEORSLRM-610         PIC X(25)                                
008800                                 VALUE 'SS CODE REMOVED         '.        
008900     03  WS-TEORSLRM-708         PIC X(25)                                
009000                                VALUE 'SI+:PURCH.REQ.REJECTED   '.        
009100     03  WS-TEORSLRM-712         PIC X(30)                                
009200                                 VALUE 'SI+:ALREADY ON ORDER    '.        
009300     03  WS-TEORSLRM-720         PIC X(25)                                
009400                                 VALUE 'SI+:PURCH REJECTED REQ. '.        
009500     03  WS-TEORSLRM-750         PIC X(25)                                
009600                                 VALUE 'MISSING SUPPLIER INFO   '.        
009700     03  WS-TEORSLRM-760         PIC X(25)                                
009800                                 VALUE 'DEL PLAN PARAM. MISSING '.        
009900     03  WS-TEORSLRM-770         PIC X(25)                                
010000                                 VALUE 'NEW SUPP, NO AGREEMENT  '.        
010100     03  WS-TEORSLRM-780         PIC X(25)                                
010200                                 VALUE 'NEW REFILL.OLD CALL OFFS'.        
010300     03  WS-TEORSLRM-790         PIC X(25)                                
010400                                 VALUE 'NEW AGREEMENT           '.        
010500                                                                          
010600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
010700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
010800                                                                          
010900 77  YES                         PIC X       VALUE 'J'.                   
011000 77  NOO                         PIC X       VALUE 'N'.                   
011100                                                                          
011200*    --- INDEX FOR SCROLL LINES                                           
011300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
011400 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
011500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
011600                                                                          
011700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
011800     88  INDATA-OK                           VALUE 'J'.                   
011900     88  INDATA-WRONG                        VALUE 'N'.                   
012000                                                                          
012100 77  KEYS-SW                     PIC X       VALUE 'J'.                   
012200     88  KEYS-OK                             VALUE 'J'.                   
012300     88  KEYS-WRONG                          VALUE 'N'.                   
012400                                                                          
012500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012600     88  OWN-MID                             VALUE '2471'.                
012700     88  GOOD-MID                            VALUE '2471' '2472'          
012800                                                   '2473' '2474'          
012900                                                   '2475' '2476'          
013000                                                   '2477' '2478'          
013100                                                   '2479'.                
013200     88  HELP-MID                            VALUE '0551'.                
013300                                                                          
013400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
013500 01  GENERAL-SUBPROGRAMS.                                                 
013600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014000     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
014100                                                                          
014200*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
014300*01 -COPY WMEDAREA                                                        
014400                                                                          
014500*    --- PARAMETRAR TILL WZ20DAYS                                         
014600*01  -COPY WZ20DAYS                                                       
014700     EJECT                                                                
014800 01  MESSAGE-CODES.                                                       
014900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
015000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
015100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
015200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015300     03  ERR-PARTNO-MISSING      PIC X(3)    VALUE '017'.                 
015400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
015600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015700     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
015800                                                                          
015900     03  ERR-PRESS-F9-FOR-SELECTION PIC X(3) VALUE '127'.                 
016000     03  ERR-ONLY-DELETE-ALLOWED PIC X(3)    VALUE '427'.                 
016100     03  ERR-ALERT-MISSING       PIC X(3)    VALUE '429'.                 
016200     03  ERR-ANSK-MISSING        PIC X(3)    VALUE '331'.                 
016300                                                                          
016400*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
016500*                                                                         
016600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016700                                                                          
016800*01 -COPY WMSGINIT                                                        
016900                                                                          
017000*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
017100*                                                                         
017200 01  SAVE-AREA.                                                           
017300     03  SAVE-IDTRANS            PIC X(4)    VALUE '2471'.                
017400     03  SAVE-TISENBEK-KDLARM OCCURS 14.                                  
017500        05  SAVE-TISENBEK-DAG    PIC S9(7)   VALUE ZERO   COMP-3.         
017600        05  SAVE-TISENBEK-KL     PIC S9(7)   VALUE ZERO   COMP-3.         
017700        05  SAVE-KDLARM          PIC X(3)    VALUE SPACE.                 
017800                                                                          
017900     03  SAVE-TISENBEK-DAG-ENTER PIC S9(7)   VALUE ZERO   COMP-3.         
018000     03  SAVE-TISENBEK-KL-ENTER  PIC S9(7)   VALUE ZERO   COMP-3.         
018100     03  SAVE-KDLARM-ENTER       PIC X(3)    VALUE SPACE.                 
018200                                                                          
018300     03  SAVE-TISENBEK-DAG-NEXT  PIC S9(7)   VALUE ZERO   COMP-3.         
018400     03  SAVE-TISENBEK-KL-NEXT   PIC S9(7)   VALUE ZERO   COMP-3.         
018500     03  SAVE-KDLARM-NEXT        PIC X(3)    VALUE SPACE.                 
018600                                                                          
018700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
018800*                                                                         
018900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019000                                                                          
019100*01  MID -COPY W2I47101                                                   
019200                                                                          
019300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019400                                                                          
019500*01  -COPY WMSGAREA                                                       
019600                                                                          
019700     03  MOD REDEFINES MSG-AREA.                                          
019800*      05  -COPY W2O47101                                                 
019900                                                                          
020000 01  FILLER                PIC X(16) VALUE 'PROG-TO-PROG-SW'.             
020100 01  W-PROG-TO-PROG-SW.                                                   
020200     03  P-WS-LL           PIC S9(4) VALUE +1063 COMP SYNC.               
020300     03  FILLER            PIC X(2)  VALUE LOW-VALUE.                     
020400     03  KDTRANS-WS        PIC X(8)  VALUE 'W2T471  '.                    
020500     03  FILLER            PIC X(5)  VALUE '24712'.                       
020600*    03  MID  -COPY W2I47101    -PRE PRGSW-.                              
020700*    03  MOD  -COPY W2O47101    -PRE PRGSW-.                              
020800                                                                          
020900 01  FILLER                PIC X(16)   VALUE 'MID TILL 2402 '.            
021000 01  ALT1-MSG-AREA.                                                       
021100                                                                          
021200     03  ALT1-LL           PIC S9(4)   VALUE +39 COMP SYNC.               
021300** W2I40201 + 17                                                          
021400     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
021500     03  ALT1-KDTRANS      PIC X(8)    VALUE 'W2T471  '.                  
021600     03  ALT1-IDTRANS      PIC X(4)    VALUE '2471'.                      
021700     03  ALT1-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
021800     03  MID -COPY W2I402N1    -PRE ALT1-                                 
021900                                                                          
022000 01  FILLER                PIC X(16)   VALUE 'MID TILL 2403 '.            
022100 01  ALT2-MSG-AREA.                                                       
022200                                                                          
022300     03  ALT2-LL           PIC S9(4)   VALUE +129 COMP SYNC.              
022400** W2I40301 + 17                                                          
022500     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
022600     03  ALT2-KDTRANS      PIC X(8)    VALUE 'W2T471  '.                  
022700     03  ALT2-IDTRANS      PIC X(4)    VALUE '2471'.                      
022800     03  ALT2-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
022900     03  MID -COPY W2I40301    -PRE ALT2-                                 
023000                                                                          
023100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
023200                                                                          
023300*01  -COPY WMFSAREA                                                       
023400                                                                          
023500*    --- WORK-AREAS FOR IMS-SECTIONS                                      
023600*                                                                         
023700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023800                                                                          
023900 01  KEYS-FOR-DLI.                                                        
024000                                                                          
024100     03  W-IDDC-B6-X.                                                     
024200         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
024300                                                                          
024400     03  W-IDDC-B6-MIN-X.                                                 
024500         05  FILLER              PIC X(1)    VALUE SPACE.                 
024600         05  W-IDDC2-B6-MIN      PIC X(1)    VALUE SPACE.                 
024700     03  W-IDDC-B6-MAX-X.                                                 
024800         05  FILLER              PIC X(1)    VALUE SPACE.                 
024900         05  W-IDDC2-B6-MAX      PIC X(1)    VALUE SPACE.                 
025000                                                                          
025100     03  W-WDGXKEY-R201-X.                                                
025200         05  W-IDHTYP-R201       PIC X(4)    VALUE '2231'.                
025300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
025400     03  W-WDGXKEY-R220-X.                                                
025500         05  W-IDANSK-X.                                                  
025600             07  W-IDANSK-R220   PIC S9(3)   COMP-3.                      
025700         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
025800                                                                          
025900     03  W-WDGXKEY-2223-X.                                                
026000         05  W-IDHTYP-2223       PIC X(4)    VALUE '2223'.                
026100         05  W-IDANSK            PIC S9(3)   VALUE ZERO    COMP-3.        
026200         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
026300     03  W-WDGXKEY-R550-X.                                                
026400         05  W-TISENBEK.                                                  
026500             07  W-TISENBEK-DAG  PIC S9(7)   VALUE ZERO COMP-3.           
026600             07  W-TISENBEK-KL   PIC S9(7)   VALUE ZERO COMP-3.           
026700         05  W-KDLARM-X.                                                  
026800             07 W-KDLARM         PIC S9(3)   COMP-3.                      
026900                                                                          
027000     03  W-IDDC-R5-MIN-X.                                                 
027100         05  W-IDDC-R5-MIN       PIC X(2)    VALUE SPACE.                 
027200     03  W-IDDC-R5-MAX-X.                                                 
027300         05  W-IDDC-R5-MAX       PIC X(2)    VALUE SPACE.                 
027400                                                                          
027500     03  W-IDARTNR-R5-MIN-X.                                              
027600         05  W-IDARTNR-R5-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
027700     03  W-IDARTNR-R5-MAX-X.                                              
027800         05  W-IDARTNR-R5-MAX    PIC S9(9)   VALUE ZERO COMP-3.           
027900                                                                          
028000     03  W-IDLEVNR-R5-MIN-X.                                              
028100         05  W-IDLEVNR-R5-MIN    PIC X(5)    VALUE SPACE.                 
028200     03  W-IDLEVNR-R5-MAX-X.                                              
028300         05  W-IDLEVNR-R5-MAX    PIC X(5)    VALUE SPACE.                 
028400                                                                          
028500     03  W-KDLARM-R5-MIN-X.                                               
028600         05  W-KDLARM-R5-MIN     PIC S9(3)   COMP-3.                      
028700     03  W-KDLARM-R5-MAX-X.                                               
028800         05  W-KDLARM-R5-MAX     PIC S9(3)   COMP-3.                      
028900                                                                          
029000     03  W-IDARTNR-K7-X.                                                  
029100         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
029200     03  W-IDDC-K7-X.                                                     
029300         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
029400     03  W-KDSEGKEY-K722-X.                                               
029500         05  W-KDSEGKEY-K722     PIC X(1)    VALUE '1'.                   
029600                                                                          
029700                                                                          
029800     03  W-IDARTNR-X.                                                     
029900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
030000     03  W-IDLEVNR-X.                                                     
030100         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
030200     03  W-IDDC-X.                                                        
030300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
030400     03  W-KDSEGKEY-X.                                                    
030500         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
030600                                                                          
030700*    --- STATUS CODES FROM IMS                                            
030800 01  STATUS-WS                   PIC XX.                                  
030900     88  SEGMENT-FOUND                       VALUE '  '.                  
031000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
031100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
031200     88  SEGMENT-END                         VALUE 'GB'.                  
031300                                                                          
031400 01  GOOD-STATUSCODES.                                                    
031500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031600                                                                          
031700 01  ALL-SSA.                                                             
031800     03 SSA1                     PIC X(900).                              
031900     03 SSA2                     PIC X(164).                              
032000     03 SSA3                     PIC X(164).                              
032100                                                                          
032200*    --- IMS FUNCTION CODES                                               
032300*01  -COPY W0003                                                          
032400                                                                          
032500*    ---  DLI INPUT-OUTPUT AREA                                           
032600                                                                          
032700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
032800 01  DLI-IO-WDK601.                                                       
032900*    03  -COPY WDK601                                                     
033000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
033100 01  DLI-IO-WDK701.                                                       
033200*    03  -COPY WDK701                                                     
033300                                                                          
033400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
033500 01  DLI-IO-WDK711.                                                       
033600*    03  -COPY WDK711                                                     
033700                                                                          
033800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
033900 01  DLI-IO-WDK722.                                                       
034000*    03  -COPY WDK722                                                     
034100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2223'.                    
034200 01  DLI-IO-WDGX2223.                                                     
034300*    03  -COPY WDGX2223                                                   
034400                                                                          
034500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR550'.                      
034600 01  DLI-IO-WDR550.                                                       
034700*    03  -COPY WDGX2224                                                   
034800                                                                          
034900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
035000 01  DLI-IO-WDB601.                                                       
035100*    03  -COPY WDB601                                                     
035200                                                                          
035300 LINKAGE SECTION.                                                         
035400*01  -COPY W0009   -PRE MSG-                                              
035500                                                                          
035600*    FÖR HOPP TILL BILD 2402                                              
035700*01  -COPY W0009   -PRE ALT1-                                             
035800*    FÖR HOPP TILL BILD 2403                                              
035900*01  -COPY W0009   -PRE ALT2-                                             
036000*01  -COPY W0008   -PRE USEA-                                             
036100     05  FILLER                  PIC X.                                   
036200                                                                          
036300*01  -COPY W0008  -PRE WDR5-                                              
036400     05  FILLER                  PIC X.                                   
036500                                                                          
036600*01  -COPY W0008  -PRE WDK6-                                              
036700     05  FILLER                  PIC X.                                   
036800                                                                          
036900*01  -COPY W0008  -PRE WDK7-                                              
037000     05  FILLER                  PIC X.                                   
037100                                                                          
037200*01  -COPY W0008  -PRE WDB6-                                              
037300     05  FILLER                  PIC X.                                   
037400                                                                          
037500*01  -COPY W0008  -PRE WDB6A-                                             
037600     05  FILLER                  PIC X.                                   
037700                                                                          
037800 PROCEDURE DIVISION  USING MSG-PCB  ALT1-PCB ALT2-PCB                     
037900                           USEA-PCB WDR5-PCB WDK6-PCB                     
038000                           WDK7-PCB WDB6-PCB WDB6A-PCB.                   
038100 MAIN SECTION.                                                            
038200     ENTRY 'DLITCBL' USING MSG-PCB  ALT1-PCB ALT2-PCB                     
038300                           USEA-PCB WDR5-PCB WDK6-PCB                     
038400                           WDK7-PCB WDB6-PCB WDB6A-PCB.                   
038500                                                                          
038600     PERFORM IMS-GET-MSG                                                  
038700     IF SEGMENT-FOUND                                                     
038800        PERFORM A-INIT                                                    
038900        PERFORM B-CHECK-KEYS                                              
039000        IF KEYS-OK                                                        
039100           IF MFS-UPDATE                                                  
039200              PERFORM G-CHECK-INPUT                                       
039300              IF INDATA-OK                                                
039400                 PERFORM H-UPDATE                                         
039500              END-IF                                                      
039600           ELSE                                                           
039700              IF MFS-FIRST                                                
039800                 PERFORM C-FIRST-PAGE                                     
039900              ELSE                                                        
040000                 IF MFS-NEXT                                              
040100                    PERFORM D-NEXT-PAGE                                   
040200                 ELSE                                                     
040300                    PERFORM I-CHECK-S-MARK                                
040400                    IF S-MARKED                                           
040500                       MOVE YES TO TRANS-TO-UPD-PGM-SW                    
040600                    ELSE                                                  
040700                       PERFORM E-SAME-PAGE                                
040800                    END-IF                                                
040900                 END-IF                                                   
041000              END-IF                                                      
041100           END-IF                                                         
041200        END-IF                                                            
041300        IF NOT TRANS-TO-UPD-PGM                                           
041400           IF KEYS-OK                                                     
041500             PERFORM F-READ-SHOW-INFO                                     
041600           END-IF                                                         
041700           COMPUTE MSG-KVLL = LENGTH OF MOD-W2O47101 + 4                  
041800           PERFORM IMS-INSERT-MSG                                         
041900        ELSE                                                              
042000          MOVE MID-W2I47101 TO PRGSW-MID-W2I47101                         
042100          MOVE MOD-W2O47101 TO PRGSW-MOD-W2O47101                         
042200          IF WS-KDLARM-VAL = '200' OR '210' OR '222' OR '500' OR          
042300                             '600' OR '601' OR '610' OR '708' OR          
042400                             '712' OR '720' OR '750' OR '760' OR          
042500                             '770' OR '780' OR '790'                      
042600               PERFORM N-JUMP-TO-SCREEN-2402                              
042700          ELSE                                                            
042800             IF WS-KDLARM-VAL = '223'                                     
042900               PERFORM M-JUMP-TO-SCREEN-2403                              
043000             ELSE                                                         
043100               MOVE ERR-ONLY-DELETE-ALLOWED TO MED-IDMFSFEL               
043200               CALL WMEDKONV USING MED-WMEDAREA                           
043300               MOVE MED-MFSFEL     TO MOD-TEMFSFEL                        
043400               PERFORM MFS-ROER-EJ-FAELT-IN                               
043500               PERFORM MFS-ROER-EJ-FAELT-UT                               
043600               COMPUTE MSG-KVLL = LENGTH OF MOD-W2O47101 + 4              
043700               PERFORM IMS-INSERT-MSG                                     
043800             END-IF                                                       
043900          END-IF                                                          
044000       END-IF                                                             
044100     END-IF                                                               
044200                                                                          
044300     MOVE ZERO TO RETURN-CODE                                             
044400     GOBACK                                                               
044500     .                                                                    
044600                                                                          
044700                                                                          
044800 A-INIT SECTION.                                                          
044900                                                                          
045000     MOVE 'A-INIT' TO WS-CURRENT-SECTION                                  
045100                                                                          
045200     IF MSG-DOUBLE-TRANSACTIONS                                           
045300       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I47101                 
045400       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
045500       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
045600     ELSE                                                                 
045700       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W2I47101                 
045800       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
045900       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
046000     END-IF                                                               
046100                                                                          
046200     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
046300     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
046400     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
046500                                                                          
046600     MOVE LOW-VALUE       TO MSG-AREA                                     
046700     MOVE 'W2O471N1'      TO MFS-IDMOD                                    
046800     MOVE '2471'          TO MOD-IDTRANS                                  
046900     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
047000                                                                          
047100     IF OWN-MID OR HELP-MID                                               
047200       CONTINUE                                                           
047300     ELSE                                                                 
047400       MOVE SPACE   TO MFS-KDTRTYP                                        
047500       MOVE '7'     TO MFS-IDPFK                                          
047600     END-IF                                                               
047700     .                                                                    
047800                                                                          
047900                                                                          
048000 B-CHECK-KEYS SECTION.                                                    
048100                                                                          
048200     MOVE 'B-CHECK-KEYS'      TO WS-CURRENT-SECTION                       
048300                                                                          
048400     MOVE SPACE                  TO MED-IDMFSFEL                          
048500                                                                          
048600     PERFORM BA-CHECK-NEW-KEYS                                            
048700                                                                          
048800     MOVE ALL '+'             TO MSGI-WMSGINIT                            
048900     MOVE '001'               TO MSGI-KDCALL                              
049000     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
049100     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
049200     MOVE '2471'              TO MSGI-IDTRANS                             
049300     IF OWN-MID                                                           
049400        MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                             
049500        MOVE MID-IDDC-IN      TO MSGI-IDDC-KEY                            
049600        MOVE MID-IDANSK-IN    TO MSGI-IDANSK                              
049700        MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                             
049800        MOVE MID-KDLARM-IN    TO MSGI-KDLARM                              
049900     ELSE                                                                 
050000        PERFORM MFS-ERASE-FIELD-IN                                        
050100        MOVE ZERO             TO MSGI-IDARTNR                             
050200        MOVE SPACE            TO MSGI-IDLEVNR                             
050300        MOVE SPACE            TO MSGI-KDLARM                              
050400        MOVE '7'              TO MFS-IDPFK                                
050500        MOVE SPACE            TO MFS-KDTRTYP                              
050600        MOVE ALL '+'          TO MID-IDANSK-IN                            
050700                                 MID-IDANSK-UT                            
050800                                 MID-IDLEVNR-IN                           
050900                                 MID-IDLEVNR-UT                           
051000                                 MID-KDLARM-IN                            
051100                                 MID-KDLARM-UT                            
051200     END-IF                                                               
051300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
051400     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
051500                                                                          
051600*    - LANGUAGE TO BE USED BY MEDKONV                                     
051700     MOVE 'GB'                TO MED-IDSKYLT                              
051800                                                                          
051900     MOVE YES TO KEYS-SW                                                  
052000                                                                          
052100     PERFORM BD-CHECK-IDANSK                                              
052200     IF KEYS-OK                                                           
052300        PERFORM BB-CHECK-IDARTNR                                          
052400        IF KEYS-OK                                                        
052500           PERFORM BC-CHECK-IDDC                                          
052600           IF KEYS-OK                                                     
052700              PERFORM BE-CHECK-IDLEVNR                                    
052800              PERFORM BF-CHECK-KDLARM                                     
052900           END-IF                                                         
053000        END-IF                                                            
053100     END-IF                                                               
053200                                                                          
053300                                                                          
053400     IF GOOD-MID AND KEYS-OK                                              
053500       MOVE MSGI-IDARTNR     TO MOD-IDARTNR-UT                            
053600       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
053700       MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                               
053800       MOVE MSGI-IDANSK      TO MOD-IDANSK-UT                             
053900       MOVE MSGI-IDLEVNR     TO MOD-IDLEVNR-UT                            
054000       INSPECT MOD-IDLEVNR-UT REPLACING LEADING ZERO BY SPACE             
054100       MOVE MSGI-KDLARM      TO MOD-KDLARM-UT                             
054200       INSPECT MOD-KDLARM-UT  REPLACING LEADING ZERO BY SPACE             
054300     ELSE                                                                 
054400       MOVE MFS-ERASE-FIELD  TO MOD-IDARTNR-UT                            
054500       MOVE MFS-ERASE-FIELD  TO MOD-STRECK                                
054600       MOVE MFS-ERASE-FIELD  TO MOD-REKSIFFR                              
054700       MOVE MFS-ERASE-FIELD  TO MOD-IDDC-UT                               
054800       MOVE MFS-ERASE-FIELD  TO MOD-IDANSK-UT                             
054900       MOVE MFS-ERASE-FIELD  TO MOD-IDLEVNR-UT                            
055000       MOVE MFS-ERASE-FIELD  TO MOD-KDLARM-UT                             
055100     END-IF                                                               
055200                                                                          
055300     IF KEYS-WRONG                                                        
055400       IF MED-IDMFSFEL = SPACE                                            
055500          MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                              
055600       END-IF                                                             
055700       CALL WMEDKONV USING MED-WMEDAREA                                   
055800       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
055900       PERFORM MFS-ERASE-FIELD-IN                                         
056000     END-IF                                                               
056100     .                                                                    
056200                                                                          
056300                                                                          
056400 BA-CHECK-NEW-KEYS SECTION.                                               
056500                                                                          
056600     MOVE 'BA-CHECK-NEW-KEYS'  TO WS-CURRENT-SECTION                      
056700                                                                          
056800     IF  MID-IDARTNR-IN = ALL '+'                                         
056900     AND MID-IDDC-IN    = ALL '+'                                         
057000     AND MID-IDLEVNR-IN = ALL '+'                                         
057100     AND MID-IDANSK-IN  = ALL '+'                                         
057200     AND MID-KDLARM-IN  = ALL '+'                                         
057300        CONTINUE                                                          
057400     ELSE                                                                 
057500        MOVE ZERO     TO MID-IDARTNR-UT                                   
057600        MOVE SPACE    TO MID-IDDC-UT                                      
057700        MOVE SPACE    TO MID-IDLEVNR-UT                                   
057800        MOVE ZERO     TO MID-IDANSK-UT                                    
057900        MOVE SPACE    TO MID-KDLARM-UT                                    
058000        MOVE '7'      TO MFS-IDPFK                                        
058100        MOVE SPACE    TO MFS-KDTRTYP                                      
058200     END-IF                                                               
058300     .                                                                    
058400                                                                          
058500                                                                          
058600 BB-CHECK-IDARTNR SECTION.                                                
058700                                                                          
058800     MOVE 'BB-CHECK-IDARTNR'  TO WS-CURRENT-SECTION                       
058900                                                                          
059000     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
059100     IF MSGI-IDARTNR > ZERO                                               
059200*    ARTIKEL GIVEN PÅ BILDEN                                              
059300       IF  MSGI-IDARTNR NUMERIC                                           
059400        MOVE MSGI-IDARTNR             TO W-IDARTNR                        
059500                                         W-IDARTNR-R5-MIN                 
059600                                         W-IDARTNR-R5-MAX                 
059700        PERFORM IMS-GU-WDK701                                             
059800        IF SEGMENT-FOUND                                                  
059900           PERFORM IMS-GU-WDK601                                          
060000           IF SEGMENT-FOUND                                               
060100              MOVE '-'                TO MOD-STRECK                       
060200              MOVE ART-REKSIFFR       TO MOD-REKSIFFR                     
060300           ELSE                                                           
060400              MOVE ERR-PARTNO-MISSING TO MED-IDMFSFEL                     
060500              MOVE NOO                TO KEYS-SW                          
060600           END-IF                                                         
060700        ELSE                                                              
060800           MOVE ERR-PARTNO-MISSING    TO MED-IDMFSFEL                     
060900           MOVE NOO                   TO KEYS-SW                          
061000        END-IF                                                            
061100       ELSE                                                               
061200          MOVE NOO                   TO KEYS-SW                           
061300       END-IF                                                             
061400     ELSE                                                                 
061500        MOVE ZERO                     TO W-IDARTNR                        
061600        MOVE LOW-VALUE                TO W-IDARTNR-R5-MIN-X               
061700        MOVE HIGH-VALUE               TO W-IDARTNR-R5-MAX-X               
061800        MOVE MFS-ERASE-FIELD          TO MOD-IDARTNR-IN                   
061900     END-IF                                                               
062000     .                                                                    
062100                                                                          
062200                                                                          
062300 BC-CHECK-IDDC    SECTION.                                                
062400                                                                          
062500     MOVE 'BC-CHECK-IDDC   '  TO WS-CURRENT-SECTION                       
062600                                                                          
062700     MOVE MFS-ERASE-FIELD     TO MOD-IDDC-IN                              
062800                                                                          
062900     IF MSGI-IDDC-KEY = ALL '+' OR SPACE                                  
063000        MOVE MSGI-IDFTG        TO WS-IDFTG                                
063100        IF IDFTG-US                                                       
063200           MOVE '41'           TO W-IDDC-B6-MIN-X                         
063300           MOVE '49'           TO W-IDDC-B6-MAX-X                         
063400           MOVE '40'           TO MSGI-IDDC-KEY                           
063500        ELSE                                                              
063600           IF IDFTG-CN                                                    
063700              MOVE '71'        TO W-IDDC-B6-MIN-X                         
063800              MOVE '79'        TO W-IDDC-B6-MAX-X                         
063900              MOVE '70'        TO MSGI-IDDC-KEY                           
064000           ELSE                                                           
064100              IF MSGI-IDFTG NOT = WC-IDFTG-PV                             
064200                 MOVE NOO      TO KEYS-SW                                 
064300              END-IF                                                      
064400           END-IF                                                         
064500        END-IF                                                            
064600     ELSE                                                                 
064700        MOVE MSGI-IDDC-KEY     TO W-IDDC-B6-MIN-X                         
064800                                  W-IDDC-B6-MAX-X                         
064900        IF MSGI-IDDC-KEY(2:1) = '0'                                       
065000           MOVE '1'            TO W-IDDC2-B6-MIN                          
065100           MOVE '9'            TO W-IDDC2-B6-MAX                          
065200        END-IF                                                            
065300     END-IF                                                               
065400                                                                          
065500     MOVE NOO                 TO W-DC-OK                                  
065600     PERFORM IMS-GU-WDB601-MIN-MAX                                        
065700     PERFORM UNTIL SEGMENT-MISSING                                        
065800                OR SEGMENT-END                                            
065900                OR W-DC-OK = YES                                          
066000        IF DCS-NDC-CN                                                     
066100        OR (DCS-NDC-NA AND DCS-USA)                                       
066200           MOVE YES           TO W-DC-OK                                  
066300        END-IF                                                            
066400        PERFORM IMS-GN-WDB601-MIN-MAX                                     
066500     END-PERFORM                                                          
066600                                                                          
066700     IF W-DC-OK = NOO                                                     
066800        MOVE NOO              TO KEYS-SW                                  
066900     END-IF                                                               
067000     .                                                                    
067100                                                                          
067200                                                                          
067300 BD-CHECK-IDANSK        SECTION.                                          
067400                                                                          
067500     MOVE 'BD-CHECK-IDANSK '       TO WS-CURRENT-SECTION                  
067600                                                                          
067700     MOVE MFS-ERASE-FIELD          TO MOD-IDANSK-IN                       
067800                                                                          
067900     IF MID-IDANSK-IN NOT = ALL '+'                                       
068000        IF MID-IDANSK-IN NOT NUMERIC                                      
068100           MOVE ERR-ANSK-MISSING   TO MED-IDMFSFEL                        
068200           MOVE NOO                TO KEYS-SW                             
068300        ELSE                                                              
068400           MOVE MID-IDANSK-IN      TO MOD-IDANSK-UT                       
068500        END-IF                                                            
068600     ELSE                                                                 
068700        MOVE MSGI-IDANSK           TO MOD-IDANSK-UT                       
068800     END-IF                                                               
068900                                                                          
069000     IF KEYS-OK                                                           
069100        MOVE MOD-IDANSK-UT         TO W-IDANSK                            
069200        PERFORM IMS-GU-WDR501                                             
069300        IF SEGMENT-MISSING                                                
069400           MOVE ERR-ALERT-MISSING  TO MED-IDMFSFEL                        
069500           MOVE NOO                TO KEYS-SW                             
069600        END-IF                                                            
069700     END-IF                                                               
069800     .                                                                    
069900                                                                          
070000                                                                          
070100 BE-CHECK-IDLEVNR SECTION.                                                
070200                                                                          
070300     MOVE 'BE-CHECK-IDLEVNR'  TO WS-CURRENT-SECTION                       
070400                                                                          
070500     MOVE MFS-ERASE-FIELD     TO MOD-IDLEVNR-IN                           
070600                                                                          
070700     MOVE LOW-VALUE           TO W-IDLEVNR-R5-MIN-X                       
070800     MOVE HIGH-VALUE          TO W-IDLEVNR-R5-MAX-X                       
070900     IF MID-IDLEVNR-IN NOT = ALL '+' AND SPACE                            
071000        MOVE MID-IDLEVNR-IN   TO W-IDLEVNR-R5-MIN                         
071100                                 W-IDLEVNR-R5-MAX                         
071200        MOVE MID-IDLEVNR-IN   TO MOD-IDLEVNR-UT                           
071300     ELSE                                                                 
071400        IF MSGI-IDLEVNR NOT = ALL '+' AND SPACE                           
071500          MOVE MSGI-IDLEVNR   TO W-IDLEVNR-R5-MIN                         
071600                                 W-IDLEVNR-R5-MAX                         
071700          MOVE MSGI-IDLEVNR   TO MOD-IDLEVNR-UT                           
071800        END-IF                                                            
071900     END-IF                                                               
072000     .                                                                    
072100                                                                          
072200                                                                          
072300 BF-CHECK-KDLARM  SECTION.                                                
072400                                                                          
072500     MOVE 'BF-CHECK-KDLARM '  TO WS-CURRENT-SECTION                       
072600                                                                          
072700     MOVE LOW-VALUE              TO W-KDLARM-R5-MIN-X                     
072800     MOVE HIGH-VALUE             TO W-KDLARM-R5-MAX-X                     
072900     IF MSGI-KDLARM > ZERO                                                
073000        IF MID-KDLARM-IN NOT = ALL '+'                                    
073100           MOVE MID-KDLARM-IN    TO W-KDLARM-R5-MIN                       
073200                                    W-KDLARM-R5-MAX                       
073300           MOVE MID-KDLARM-IN    TO MOD-KDLARM-UT                         
073400        ELSE                                                              
073500           MOVE MSGI-KDLARM      TO MOD-KDLARM-UT                         
073600           MOVE MSGI-KDLARM      TO W-KDLARM-R5-MIN                       
073700                                    W-KDLARM-R5-MAX                       
073800           MOVE MFS-ERASE-FIELD  TO MOD-KDLARM-IN                         
073900        END-IF                                                            
074000     END-IF                                                               
074100     .                                                                    
074200                                                                          
074300                                                                          
074400 C-FIRST-PAGE SECTION.                                                    
074500                                                                          
074600     MOVE 'C-FIRST-PAGE'  TO WS-CURRENT-SECTION                           
074700                                                                          
074800     MOVE INF-FIRST-PAGE  TO MED-IDMFSINF                                 
074900     CALL WMEDKONV USING MED-WMEDAREA                                     
075000     MOVE MED-MFSINF      TO MOD-TEMFSFEL                                 
075100                                                                          
075200     PERFORM MFS-ERASE-FIELD-IN                                           
075300     .                                                                    
075400                                                                          
075500                                                                          
075600 D-NEXT-PAGE SECTION.                                                     
075700                                                                          
075800     MOVE 'D-NEXT-PAGE '       TO WS-CURRENT-SECTION                      
075900                                                                          
076000     IF SAVE-IDTRANS = '2471'                                             
076100       MOVE SAVE-TISENBEK-DAG-NEXT  TO W-TISENBEK-DAG                     
076200       MOVE SAVE-TISENBEK-KL-NEXT   TO W-TISENBEK-KL                      
076300       MOVE SAVE-KDLARM-NEXT        TO W-KDLARM                           
076400     ELSE                                                                 
076500       PERFORM MFS-ERASE-FIELD-IN                                         
076600     END-IF                                                               
076700     .                                                                    
076800                                                                          
076900                                                                          
077000 E-SAME-PAGE SECTION.                                                     
077100                                                                          
077200     MOVE 'E-SAME-PAGE '        TO WS-CURRENT-SECTION                     
077300                                                                          
077400     IF SAVE-IDTRANS = '2471' OR '0551'                                   
077500       MOVE SAVE-TISENBEK-DAG-ENTER   TO W-TISENBEK-DAG                   
077600       MOVE SAVE-TISENBEK-KL-ENTER    TO W-TISENBEK-KL                    
077700       MOVE SAVE-KDLARM-ENTER         TO W-KDLARM                         
077800       IF MID-INPUT = ALL '+'                                             
077900         PERFORM MFS-ERASE-FIELD-IN                                       
078000       ELSE                                                               
078100         MOVE INF-PRESS-PF11    TO MED-IDMFSINF                           
078200         CALL WMEDKONV USING MED-WMEDAREA                                 
078300         MOVE MED-MFSINF        TO MOD-TEMFSFEL                           
078400         PERFORM EA-MID-INDATA-FOR-MOD                                    
078500       END-IF                                                             
078600     ELSE                                                                 
078700       PERFORM MFS-ERASE-FIELD-IN                                         
078800     END-IF                                                               
078900     .                                                                    
079000                                                                          
079100                                                                          
079200 EA-MID-INDATA-FOR-MOD SECTION.                                           
079300                                                                          
079400     MOVE 'EA-MID-INDATA-FOR-MOD' TO WS-CURRENT-SECTION                   
079500                                                                          
079600        PERFORM MFS-DONT-TOUCH-FIELD-IN                                   
079700        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
079800     .                                                                    
079900                                                                          
080000                                                                          
080100 F-READ-SHOW-INFO SECTION.                                                
080200                                                                          
080300     MOVE 'F-READ-SHOW-INFO     ' TO WS-CURRENT-SECTION                   
080400                                                                          
080500     PERFORM IMS-GU-WDR501                                                
080600                                                                          
080700     IF SEGMENT-MISSING                                                   
080800        MOVE ERR-ALERT-MISSING  TO MED-IDMFSFEL                           
080900        CALL WMEDKONV USING        MED-WMEDAREA                           
081000        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
081100     ELSE                                                                 
081200                                                                          
081300       MOVE +1 TO INDX                                                    
081400       MOVE W-IDDC-B6-MIN-X     TO W-IDDC-R5-MIN                          
081500       MOVE W-IDDC-B6-MAX-X     TO W-IDDC-R5-MAX                          
081600                                                                          
081700       PERFORM IMS-GNP-WDR550-FIRST                                       
081800       IF SEGMENT-FOUND                                                   
081900         MOVE 2224-TISENBEK-DAG TO SAVE-TISENBEK-DAG-ENTER                
082000         MOVE 2224-TISENBEK-KL  TO SAVE-TISENBEK-KL-ENTER                 
082100         MOVE 2224-KDLARM       TO SAVE-KDLARM-ENTER                      
082200       ELSE                                                               
082300         MOVE +0                TO SAVE-TISENBEK-DAG-ENTER                
082400         MOVE +0                TO SAVE-TISENBEK-KL-ENTER                 
082500         MOVE SPACE             TO SAVE-KDLARM-ENTER                      
082600       END-IF                                                             
082700                                                                          
082800       PERFORM UNTIL INDX > MAX-INDX                                      
082900         IF SEGMENT-FOUND                                                 
083000           PERFORM FB-SECURITY-CHECK-SUPPLIER                             
083100           IF PASSED-SECURITY-CHECK                                       
083200             MOVE 2224-TISENBEK-DAG  TO SAVE-TISENBEK-DAG(INDX)           
083300             MOVE 2224-TISENBEK-KL   TO SAVE-TISENBEK-KL (INDX)           
083400             MOVE 2224-KDLARM        TO SAVE-KDLARM      (INDX)           
083500                                                                          
083600             MOVE 2224-IDDC          TO MOD-IDDC     (INDX)               
083700             IF 2224-FLNYLARM = YES                                       
083800                MOVE '*'             TO MOD-FLNYLARM (INDX)               
083900             ELSE                                                         
084000                MOVE MFS-ERASE-FIELD TO MOD-FLNYLARM (INDX)               
084100             END-IF                                                       
084200             MOVE 2224-KDLARM        TO MOD-KDLARM   (INDX)               
084300             MOVE 2224-IDARTNR       TO MOD-IDARTNR  (INDX)               
084400             MOVE 2224-IDLEVNR       TO MOD-IDLEVNR  (INDX)               
084500                                                                          
084600             IF 2224-KDLARM NOT = +100 AND +110 AND 150 AND 223           
084700               MOVE ZERO             TO MOD-TISENBEK-DAG (INDX)           
084800               INSPECT MOD-TISENBEK-DAG (INDX)                            
084900                       REPLACING LEADING ZERO BY SPACE                    
085000             ELSE                                                         
085100               IF 2224-TISENBEK-DAG > +0                                  
085200                  MOVE 2224-TISENBEK-DAG   TO                             
085300                                            WS-TISENBEK-DAG-YYMMDD        
085400                  MOVE WS-TISENBEK-DAG-YYMMDD TO DAYS-TIDATE1             
085500                  MOVE 'YYMMDD'            TO DAYS-KDDATFMT1              
085600                  MOVE 'YYWWD'             TO DAYS-KDDATFMT2              
085700                  MOVE 0                   TO DAYS-KVDAYS                 
085800                  MOVE SPACE               TO DAYS-TIDATE2                
085900                                              DAYS-IDCALEND               
086000                  CALL WZ20DAYS USING DAYS-WZ20DAYS                       
086100                                                                          
086200                  IF DAYS-KDRC = 8                                        
086300                    MOVE ZERO            TO MOD-TISENBEK-DAG(INDX)        
086400                    INSPECT MOD-TISENBEK-DAG (INDX)                       
086500                            REPLACING LEADING ZERO BY SPACE               
086600                  ELSE                                                    
086700                    MOVE DAYS-TIDATE2(1:5) TO                             
086800                                            MOD-TISENBEK-DAG(INDX)        
086900                  END-IF                                                  
087000               ELSE                                                       
087100                 MOVE ZERO           TO MOD-TISENBEK-DAG (INDX)           
087200                 INSPECT MOD-TISENBEK-DAG (INDX)                          
087300                         REPLACING LEADING ZERO BY SPACE                  
087400               END-IF                                                     
087500             END-IF                                                       
087600                                                                          
087700             MOVE 2224-IDDISTR       TO MOD-IDDISTR  (INDX)               
087800             MOVE 2224-KDLARM        TO MOD-KDLARM   (INDX)               
087900             EVALUATE 2224-KDLARM                                         
088000               WHEN 200                                                   
088100                MOVE WS-TEORSLRM-200 TO MOD-TEORSLRM (INDX)               
088200               WHEN 210                                                   
088300                MOVE WS-TEORSLRM-210 TO MOD-TEORSLRM (INDX)               
088400               WHEN 222                                                   
088500                MOVE WS-TEORSLRM-222 TO MOD-TEORSLRM (INDX)               
088600               WHEN 223                                                   
088700                MOVE WS-TEORSLRM-223 TO MOD-TEORSLRM (INDX)               
088800               WHEN 500                                                   
088900                MOVE WS-TEORSLRM-500 TO MOD-TEORSLRM (INDX)               
089000               WHEN 600                                                   
089100                MOVE WS-TEORSLRM-600 TO MOD-TEORSLRM (INDX)               
089200               WHEN 601                                                   
089300                MOVE WS-TEORSLRM-601 TO MOD-TEORSLRM (INDX)               
089400               WHEN 610                                                   
089500                MOVE WS-TEORSLRM-610 TO MOD-TEORSLRM (INDX)               
089600               WHEN 708                                                   
089700                MOVE WS-TEORSLRM-708 TO MOD-TEORSLRM (INDX)               
089800               WHEN 712                                                   
089900                MOVE WS-TEORSLRM-712 TO MOD-TEORSLRM (INDX)               
090000               WHEN 720                                                   
090100                MOVE WS-TEORSLRM-720 TO MOD-TEORSLRM (INDX)               
090200               WHEN 750                                                   
090300                MOVE WS-TEORSLRM-750 TO MOD-TEORSLRM (INDX)               
090400               WHEN 760                                                   
090500                MOVE WS-TEORSLRM-760 TO MOD-TEORSLRM (INDX)               
090600               WHEN 770                                                   
090700                MOVE WS-TEORSLRM-770 TO MOD-TEORSLRM (INDX)               
090800               WHEN 780                                                   
090900                MOVE WS-TEORSLRM-780 TO MOD-TEORSLRM (INDX)               
091000               WHEN 790                                                   
091100                MOVE WS-TEORSLRM-790 TO MOD-TEORSLRM (INDX)               
091200               WHEN OTHER                                                 
091300                MOVE MFS-RENSA-FAELT TO MOD-TEORSLRM (INDX)               
091400              END-EVALUATE                                                
091500              MOVE 2224-TIREGDAT     TO MOD-TIREGDAT (INDX)               
091600           ELSE                                                           
091700              MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSINF                     
091800              CALL WMEDKONV USING MED-WMEDAREA                            
091900              MOVE MED-MFSINF        TO MOD-TEMFSFEL                      
092000           END-IF                                                         
092100           PERFORM IMS-GNP-WDR550                                         
092200         ELSE                                                             
092300           MOVE MFS-ERASE-FIELD      TO MOD-KDCMD     (INDX)              
092400                                     MOD-IDDC         (INDX)              
092500                                     MOD-FLNYLARM     (INDX)              
092600                                     MOD-KDLARM       (INDX)              
092700                                     MOD-IDARTNR      (INDX)              
092800                                     MOD-IDLEVNR      (INDX)              
092900                                     MOD-TISENBEK-DAG (INDX)              
093000                                     MOD-IDDISTR      (INDX)              
093100                                     MOD-TEORSLRM     (INDX)              
093200                                     MOD-TIREGDAT     (INDX)              
093300         END-IF                                                           
093400         ADD 1 TO INDX                                                    
093500         IF INDX <= MAX-INDX                                              
093600            MOVE MFS-RENSA-FAELT    TO MOD-KDCMD(INDX)                    
093700         END-IF                                                           
093800       END-PERFORM                                                        
093900                                                                          
094000       IF SEGMENT-FOUND                                                   
094100         MOVE 2224-TISENBEK-DAG TO SAVE-TISENBEK-DAG-NEXT                 
094200         MOVE 2224-TISENBEK-KL  TO SAVE-TISENBEK-KL-NEXT                  
094300         MOVE 2224-KDLARM       TO SAVE-KDLARM-NEXT                       
094400         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
094500         CALL WMEDKONV USING MED-WMEDAREA                                 
094600         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
094700       ELSE                                                               
094800         MOVE SAVE-TISENBEK-DAG-ENTER TO SAVE-TISENBEK-DAG-NEXT           
094900         MOVE SAVE-TISENBEK-KL-ENTER  TO SAVE-TISENBEK-KL-NEXT            
095000         MOVE SAVE-KDLARM-ENTER       TO SAVE-KDLARM-NEXT                 
095100       END-IF                                                             
095200                                                                          
095300       MOVE '002'      TO MSGI-KDCALL                                     
095400       MOVE '2471'     TO SAVE-IDTRANS                                    
095500       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
095600       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
095700     END-IF                                                               
095800     .                                                                    
095900                                                                          
096000                                                                          
096100 FB-SECURITY-CHECK-SUPPLIER  SECTION.                                     
096200                                                                          
096300     MOVE 'FB-SECURITY-CHECK-SUPPLIER' TO WS-CURRENT-SECTION              
096400                                                                          
096500*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
096600                                                                          
096700     IF MSGI-KDARBTYP-SEC-IDLEV = ART-IDLEVNR                             
096800     OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                      
096900*      --- BEHÖRIG USER                                                   
097000       SET PASSED-SECURITY-CHECK TO TRUE                                  
097100     ELSE                                                                 
097200       SET BLOCKED-SECURITY-CHECK TO TRUE                                 
097300     END-IF                                                               
097400     .                                                                    
097500                                                                          
097600                                                                          
097700 G-CHECK-INPUT SECTION.                                                   
097800                                                                          
097900     MOVE 'G-CHECK-INPUT'  TO WS-CURRENT-SECTION                          
098000                                                                          
098100     PERFORM S02-CHECK-ENTERED-FIELDS                                     
098200                                                                          
098300     MOVE YES  TO INDATA-SW                                               
098400     IF MID-INPUT = ALL '+'                                               
098500       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
098600       CALL WMEDKONV USING MED-WMEDAREA                                   
098700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
098800       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
098900       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
099000       MOVE NOO TO INDATA-SW                                              
099100     ELSE                                                                 
099200*  -- FORMELL KONTROLL OCH RELATIONSKONTROLL                              
099300       MOVE +1 TO INDX                                                    
099400       PERFORM UNTIL INDX > MAX-INDX                                      
099500                                                                          
099600         IF  (MID-KDCMD   (INDX) NOT = ALL '+')                           
099700         AND (MID-FLNYLARM(INDX) NOT = ALL '+')                           
099800            MOVE MFS-ALPHA-FIELD-WRONG TO                                 
099900                                MOD-KDCMD-ATTR   (INDX)                   
100000                                MOD-FLNYLARM-ATTR(INDX)                   
100100            MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                    
100200            MOVE NOO                   TO INDATA-SW                       
100300         ELSE                                                             
100400                                                                          
100500           IF MID-KDCMD(INDX) = ALL '+'                                   
100600             CONTINUE                                                     
100700           ELSE                                                           
100800              IF MID-KDCMD(INDX) = 'S'                                    
100900                MOVE ERR-PRESS-F9-FOR-SELECTION                           
101000                                       TO MED-IDMFSFEL                    
101100                MOVE MFS-ALPHA-FIELD-WRONG                                
101200                                       TO MOD-KDCMD-ATTR(INDX)            
101300                MOVE NOO TO INDATA-SW                                     
101400              ELSE                                                        
101500                IF MID-KDCMD(INDX) = 'D'                                  
101600                  PERFORM GA-AUTH-USER-CHECK                              
101700                ELSE                                                      
101800                  MOVE MFS-ALPHA-FIELD-WRONG                              
101900                                       TO MOD-KDCMD-ATTR(INDX)            
102000                  MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL              
102100                  MOVE NOO             TO INDATA-SW                       
102200               END-IF                                                     
102300             END-IF                                                       
102400           END-IF                                                         
102500                                                                          
102600           IF MID-FLNYLARM(INDX) = ALL '+'                                
102700              CONTINUE                                                    
102800           ELSE                                                           
102900              IF MID-FLNYLARM(INDX) = SPACE                               
103000                 MOVE MFS-ALPHA-FIELD-OK TO                               
103100                                      MOD-FLNYLARM-ATTR(INDX)             
103200              ELSE                                                        
103300                 MOVE MFS-ALPHA-FIELD-WRONG TO                            
103400                                      MOD-FLNYLARM-ATTR(INDX)             
103500                 MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL               
103600                 MOVE NOO TO INDATA-SW                                    
103700              END-IF                                                      
103800           END-IF                                                         
103900                                                                          
104000         END-IF                                                           
104100         ADD +1 TO INDX                                                   
104200       END-PERFORM                                                        
104300                                                                          
104400       IF INDATA-WRONG                                                    
104500          CALL WMEDKONV USING MED-WMEDAREA                                
104600          MOVE MED-MFSFEL  TO MOD-TEMFSFEL                                
104700       END-IF                                                             
104800                                                                          
104900     END-IF                                                               
105000     .                                                                    
105100                                                                          
105200 GA-AUTH-USER-CHECK SECTION.                                              
105300                                                                          
105400     MOVE MID-IDDC(INDX)              TO W-IDDC-B6                        
105500     PERFORM IMS-GU-WDB601                                                
105600     IF SEGMENT-FOUND                                                     
105700       IF DCS-NDC-CN                                                      
105800       OR (DCS-NDC-NA AND DCS-USA)                                        
105900         MOVE MSGI-IDFTG              TO WS-IDFTG                         
106000         IF (DCS-NDC-CN AND IDFTG-CN)                                     
106100         OR (DCS-NDC-NA AND IDFTG-US)                                     
106200         OR (MSGI-IDFTG   = WC-IDFTG-PV)                                  
106300           MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDCMD-ATTR(INDX)             
106400         ELSE                                                             
106500           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDCMD-ATTR(INDX)             
106600           MOVE ERR-NOT-AUTHORIZED    TO MED-IDMFSFEL                     
106700           MOVE NOO                   TO INDATA-SW                        
106800         END-IF                                                           
106900       ELSE                                                               
107000         MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDCMD-ATTR(INDX)             
107100         MOVE ERR-NOT-AUTHORIZED      TO MED-IDMFSFEL                     
107200         MOVE NOO                     TO INDATA-SW                        
107300       END-IF                                                             
107400     ELSE                                                                 
107500       MOVE ERR-NOT-AUTHORIZED        TO MED-IDMFSFEL                     
107600       MOVE MFS-ALPHA-FIELD-WRONG     TO MOD-KDCMD-ATTR(INDX)             
107700       MOVE NOO                       TO INDATA-SW                        
107800     END-IF                                                               
107900     .                                                                    
108000     EJECT                                                                
108100                                                                          
108200 H-UPDATE SECTION.                                                        
108300                                                                          
108400     MOVE +1  TO INDX                                                     
108500     MOVE NOO TO ENTER-KEY-SW                                             
108600     PERFORM UNTIL INDX > MAX-INDX                                        
108700       IF MID-KDCMD(INDX)    = 'D'                                        
108800       OR MID-FLNYLARM(INDX) = SPACE                                      
108900         MOVE SAVE-TISENBEK-DAG(INDX)  TO W-TISENBEK-DAG                  
109000         MOVE SAVE-TISENBEK-KL (INDX)  TO W-TISENBEK-KL                   
109100         MOVE SAVE-KDLARM      (INDX)  TO W-KDLARM                        
109200         PERFORM IMS-GHU-WDR550                                           
109300         IF SEGMENT-FOUND                                                 
109400           IF NOT ENTER-KEY-IFYLLD                                        
109500             MOVE 2224-TISENBEK-DAG  TO SAVE-TISENBEK-DAG(INDX)           
109600             MOVE 2224-TISENBEK-KL   TO SAVE-TISENBEK-KL (INDX)           
109700             MOVE 2224-KDLARM        TO SAVE-KDLARM      (INDX)           
109800             MOVE YES                TO ENTER-KEY-SW                      
109900           END-IF                                                         
110000           IF MID-KDCMD(INDX)    = 'D'                                    
110100             PERFORM IMS-DLET-WDR550                                      
110200           ELSE                                                           
110300             MOVE 'N'                TO 2224-FLNYLARM                     
110400             PERFORM IMS-REPL-WDR550                                      
110500           END-IF                                                         
110600         END-IF                                                           
110700       END-IF                                                             
110800       ADD +1 TO INDX                                                     
110900     END-PERFORM                                                          
111000                                                                          
111100     MOVE SAVE-TISENBEK-DAG(01) TO W-TISENBEK-DAG                         
111200     MOVE SAVE-TISENBEK-KL (01) TO W-TISENBEK-KL                          
111300     MOVE SAVE-KDLARM      (01) TO W-KDLARM                               
111400                                                                          
111500     MOVE INF-UPDATE-DONE         TO MED-IDMFSINF                         
111600     CALL WMEDKONV             USING MED-WMEDAREA                         
111700     MOVE MED-MFSINF              TO MOD-TEMFSINF                         
111800     .                                                                    
111900                                                                          
112000                                                                          
112100 I-CHECK-S-MARK      SECTION.                                             
112200                                                                          
112300     MOVE 'I-CHECK-S-MARK'  TO WS-CURRENT-SECTION                         
112400                                                                          
112500     MOVE +1 TO INDX                                                      
112600     PERFORM UNTIL INDX > MAX-INDX OR S-MARKED                            
112700       IF MID-KDCMD(INDX) = 'S'                                           
112800        IF MFS-SPLIT                                                      
112900         MOVE YES              TO S-MARKED-SW                             
113000         MOVE MID-KDLARM(INDX) TO WS-KDLARM-VAL                           
113100         MOVE INDX             TO WS-VALD-RAD                             
113200        ELSE                                                              
113300         MOVE ERR-PRESS-F9-FOR-SELECTION                                  
113400                               TO MED-IDMFSFEL                            
113500*        'TRYCK F9 FÖR SELEKTION AV LARM'                                 
113600         CALL WMEDKONV USING MED-WMEDAREA                                 
113700         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
113800         MOVE MFS-ALPHA-FIELD-WRONG                                       
113900                               TO MOD-KDCMD-ATTR(INDX)                    
114000         MOVE NOO TO INDATA-SW                                            
114100         MOVE MAX-INDX TO INDX                                            
114200         ADD  +1       TO INDX                                            
114300        END-IF                                                            
114400       ELSE                                                               
114500         ADD +1 TO INDX                                                   
114600       END-IF                                                             
114700     END-PERFORM                                                          
114800     .                                                                    
114900                                                                          
115000                                                                          
115100 N-JUMP-TO-SCREEN-2402 SECTION.                                           
115200                                                                          
115300     MOVE 'N-JUMP-TO-SCREEN-2402' TO WS-CURRENT-SECTION                   
115400                                                                          
115500     IF SAVE-IDTRANS = '2471'                                             
115600        MOVE SAVE-TISENBEK-DAG(WS-VALD-RAD) TO W-TISENBEK-DAG             
115700        MOVE SAVE-TISENBEK-KL (WS-VALD-RAD) TO W-TISENBEK-KL              
115800        MOVE SAVE-KDLARM      (WS-VALD-RAD) TO W-KDLARM                   
115900        PERFORM IMS-GHU-WDR550                                            
116000        IF SEGMENT-FOUND                                                  
116100           MOVE NOO                         TO 2224-FLNYLARM              
116200           PERFORM IMS-REPL-WDR550                                        
116300                                                                          
116400           MOVE 2224-KDMFSFOR               TO ALT1-KDMFSFOR              
116500           MOVE ALL '+'                     TO ALT1-MID                   
116600           MOVE 2224-IDARTNR                TO ALT1-MID-IDARTNR-IN        
116700           MOVE 2224-IDDC                   TO ALT1-MID-IDDC-IN           
116800           PERFORM IMS-INSERT-ALT1-2402                                   
116900        ELSE                                                              
117000           MOVE ERR-ALERT-MISSING           TO MOD-TEMFSFEL               
117100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
117200           PERFORM MFS-ROER-EJ-FAELT-IN                                   
117300           COMPUTE MSG-KVLL = LENGTH OF MOD-W2O47101 + 4                  
117400           PERFORM IMS-INSERT-MSG                                         
117500        END-IF                                                            
117600     END-IF                                                               
117700                                                                          
117800     .                                                                    
117900                                                                          
118000                                                                          
118100 M-JUMP-TO-SCREEN-2403 SECTION.                                           
118200                                                                          
118300     MOVE 'M-JUMP-TO-SCREEN-2403' TO WS-CURRENT-SECTION                   
118400                                                                          
118500     IF SAVE-IDTRANS = '2471'                                             
118600        MOVE SAVE-TISENBEK-DAG(WS-VALD-RAD) TO W-TISENBEK-DAG             
118700        MOVE SAVE-TISENBEK-KL (WS-VALD-RAD) TO W-TISENBEK-KL              
118800        MOVE SAVE-KDLARM      (WS-VALD-RAD) TO W-KDLARM                   
118900        PERFORM IMS-GHU-WDR550                                            
119000        IF SEGMENT-FOUND                                                  
119100           MOVE NOO                         TO 2224-FLNYLARM              
119200           PERFORM IMS-REPL-WDR550                                        
119300                                                                          
119400           MOVE 2224-KDMFSFOR               TO ALT2-KDMFSFOR              
119500           MOVE ALL '+'                     TO ALT2-MID                   
119600           MOVE 2224-IDARTNR                TO ALT2-IDARTNR-IN            
119700           MOVE 2224-IDDC                   TO ALT2-IDDC-IN               
119800           PERFORM IMS-INSERT-ALT2-2403                                   
119900        ELSE                                                              
120000           MOVE ERR-ALERT-MISSING           TO MOD-TEMFSFEL               
120100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
120200           PERFORM MFS-ROER-EJ-FAELT-IN                                   
120300           COMPUTE MSG-KVLL = LENGTH OF MOD-W2O47101 + 4                  
120400           PERFORM IMS-INSERT-MSG                                         
120500        END-IF                                                            
120600     END-IF                                                               
120700                                                                          
120800     .                                                                    
120900                                                                          
121000                                                                          
121100 S02-CHECK-ENTERED-FIELDS SECTION.                                        
121200                                                                          
121300     MOVE 'S02-CHECK-ENTERED-FIELDS'  TO WS-CURRENT-SECTION               
121400                                                                          
121500     MOVE +1 TO INDX                                                      
121600     PERFORM UNTIL INDX > MAX-INDX OR WS-INPUT = SPACE                    
121700       IF  MID-KDCMD   (INDX) = ALL '+'                                   
121800       AND MID-FLNYLARM(INDX) = ALL '+'                                   
121900         CONTINUE                                                         
122000       ELSE                                                               
122100         MOVE SPACE TO WS-INPUT                                           
122200       END-IF                                                             
122300       ADD +1 TO INDX                                                     
122400     END-PERFORM                                                          
122500                                                                          
122600     .                                                                    
122700                                                                          
122800                                                                          
122900 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
123000                                                                          
123100*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
123200     MOVE MFS-ERASE-FIELD TO MOD-KDCMD       (INDX)                       
123300                             MOD-IDDC        (INDX)                       
123400                             MOD-FLNYLARM    (INDX)                       
123500                             MOD-KDLARM      (INDX)                       
123600                             MOD-IDARTNR     (INDX)                       
123700                             MOD-IDLEVNR     (INDX)                       
123800                             MOD-TISENBEK-DAG(INDX)                       
123900                             MOD-IDDISTR     (INDX)                       
124000                             MOD-TEORSLRM    (INDX)                       
124100                             MOD-TIREGDAT    (INDX)                       
124200     .                                                                    
124300                                                                          
124400 MFS-ERASE-FIELD-IN SECTION.                                              
124500                                                                          
124600*    --- ALLA INDATA-FÄLT                                                 
124700     MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-IN                               
124800                             MOD-IDDC-IN                                  
124900                             MOD-IDANSK-IN                                
125000                             MOD-IDLEVNR-IN                               
125100                             MOD-KDLARM-IN                                
125200     .                                                                    
125300                                                                          
125400                                                                          
125500 MFS-ROER-EJ-FAELT-IN SECTION.                                            
125600                                                                          
125700*    --- ALLA INDATA-FÄLT                                                 
125800     MOVE +1 TO INDX                                                      
125900     PERFORM UNTIL INDX > MAX-INDX                                        
126000       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD(INDX)                          
126100       ADD +1 TO INDX                                                     
126200     END-PERFORM                                                          
126300                                                                          
126400     .                                                                    
126500     EJECT                                                                
126600                                                                          
126700 MFS-ROER-EJ-FAELT-UT   SECTION.                                          
126800                                                                          
126900*    --- ALLA UTDATA-FÄLT                                                 
127000*    --- INCL SCROLL KEYS AND LINEDATA                                    
127100                                                                          
127200     MOVE +1 TO INDX                                                      
127300     PERFORM UNTIL INDX > MAX-INDX                                        
127400       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
127500       ADD +1 TO INDX                                                     
127600     END-PERFORM                                                          
127700     .                                                                    
127800     EJECT                                                                
127900                                                                          
128000 MFS-ROER-EJ-RAD-FAELT-UT   SECTION.                                      
128100                                                                          
128200*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
128300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC        (INDX)                     
128400                               MOD-FLNYLARM    (INDX)                     
128500                               MOD-KDLARM      (INDX)                     
128600                               MOD-IDARTNR     (INDX)                     
128700                               MOD-IDLEVNR     (INDX)                     
128800                               MOD-TISENBEK-DAG(INDX)                     
128900                               MOD-IDDISTR     (INDX)                     
129000                               MOD-TEORSLRM    (INDX)                     
129100                               MOD-TIREGDAT    (INDX)                     
129200                                                                          
129300     .                                                                    
129400     EJECT                                                                
129500                                                                          
129600 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
129700                                                                          
129800*    --- ALLA UTDATA-FÄLT                                                 
129900*    --- INCL SCROLL KEYS AND LINEDATA                                    
130000     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR-IN                        
130100                                    MOD-IDARTNR-UT                        
130200                                    MOD-STRECK                            
130300                                    MOD-REKSIFFR                          
130400                                    MOD-IDDC-IN                           
130500                                    MOD-IDDC-UT                           
130600                                    MOD-IDANSK-IN                         
130700                                    MOD-IDANSK-UT                         
130800                                    MOD-IDLEVNR-IN                        
130900                                    MOD-IDLEVNR-UT                        
131000                                    MOD-KDLARM-IN                         
131100                                    MOD-KDLARM-UT                         
131200     MOVE +1 TO INDX                                                      
131300     PERFORM UNTIL INDX > MAX-INDX                                        
131400       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
131500       ADD +1 TO INDX                                                     
131600     END-PERFORM                                                          
131700     .                                                                    
131800                                                                          
131900 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
132000                                                                          
132100*    --- OUTDATA FIELD ON SCROLL KEYS                                     
132200     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMD    (INDX)                   
132300                                    MOD-FLNYLARM (INDX)                   
132400     .                                                                    
132500                                                                          
132600 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
132700                                                                          
132800*    --- ALLA INDATA-FÄLT                                                 
132900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR-IN                        
133000                                    MOD-IDDC-IN                           
133100                                    MOD-IDANSK-IN                         
133200                                    MOD-IDLEVNR-IN                        
133300                                    MOD-KDLARM-IN                         
133400     .                                                                    
133500                                                                          
133600                                                                          
133700 MFS-FORM-ATTR SECTION.                                                   
133800                                                                          
133900     MOVE +1 TO INDX                                                      
134000     PERFORM UNTIL INDX > MAX-INDX                                        
134100        MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDCMD-ATTR    (INDX)          
134200                                        MOD-FLNYLARM-ATTR (INDX)          
134300        ADD +1 TO INDX                                                    
134400     END-PERFORM                                                          
134500     .                                                                    
134600                                                                          
134700*MFS-READ-IN-AGAIN SECTION.                                               
134800*                                                                         
134900*    --- ALL INDATA-FIELDS                                                
135000*    MOVE MFS-ADD-READ-FIELD TO MOD-XXXXXXXX-ATTR                         
135100*                               MOD-XXXXXXXX-ATTR                         
135200*    .                                                                    
135300*                                                                         
135400* --- IMS SECTIONS ---                                                    
135500                                                                          
135600 IMS-GET-MSG SECTION.                                                     
135700                                                                          
135800     MOVE '  QC' TO GOOD-STATUSCODES                                      
135900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
136000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136100     PERFORM IMS-STATUSCHECK                                              
136200     .                                                                    
136300                                                                          
136400 IMS-INSERT-MSG SECTION.                                                  
136500                                                                          
136600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
136700     MOVE SPACE TO GOOD-STATUSCODES                                       
136800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
136900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
137000     PERFORM IMS-STATUSCHECK                                              
137100     .                                                                    
137200                                                                          
137300                                                                          
137400 IMS-GU-WDR501     SECTION.                                               
137500                                                                          
137600     MOVE 'IMS-GU-WDR501    ' TO WS-CURRENT-IMS-SECTION                   
137700                                                                          
137800     MOVE SPACE               TO ALL-SSA                                  
137900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
138000          DELIMITED BY SIZE INTO SSA1                                     
138100     MOVE '  GE'              TO GOOD-STATUSCODES                         
138200     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDGX2223 SSA1                 
138300     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
138400     PERFORM IMS-STATUSCHECK                                              
138500     .                                                                    
138600                                                                          
138700                                                                          
138800 IMS-GHU-WDR550     SECTION.                                              
138900                                                                          
139000     MOVE 'IMS-GHU-WDR501   ' TO WS-CURRENT-IMS-SECTION                   
139100                                                                          
139200     MOVE SPACE               TO ALL-SSA                                  
139300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
139400          DELIMITED BY SIZE INTO SSA1                                     
139500     STRING 'WDR550  (WDGXKEY  =' W-WDGXKEY-R550-X ')'                    
139600          DELIMITED BY SIZE INTO SSA2                                     
139700     MOVE '  GE'              TO GOOD-STATUSCODES                         
139800     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDR550 SSA1 SSA2              
139900     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
140000     PERFORM IMS-STATUSCHECK                                              
140100     .                                                                    
140200                                                                          
140300                                                                          
140400 IMS-GNP-WDR550-FIRST     SECTION.                                        
140500     MOVE 'IMS-GNP-WDR550-FIRST   ' TO WS-CURRENT-IMS-SECTION             
140600                                                                          
140700     MOVE SPACE               TO ALL-SSA                                  
140800     STRING 'WDR550  *F(WDGXKEY =>' W-WDGXKEY-R550-X                      
140900                    '&KDLARM  >=' W-KDLARM-R5-MIN-X                       
141000                    '&KDLARM  <=' W-KDLARM-R5-MAX-X                       
141100                    '&IDARTNR >=' W-IDARTNR-R5-MIN-X                      
141200                    '&IDARTNR <=' W-IDARTNR-R5-MAX-X                      
141300                    '&IDDC    >=' W-IDDC-R5-MIN-X                         
141400                    '&IDDC    <=' W-IDDC-R5-MAX-X                         
141500                    '&IDLEVNR >=' W-IDLEVNR-R5-MIN-X                      
141600                    '&IDLEVNR <=' W-IDLEVNR-R5-MAX-X  ')'                 
141700          DELIMITED BY SIZE  INTO SSA1                                    
141800     MOVE '  GE'               TO GOOD-STATUSCODES                        
141900     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDR550 SSA1                   
142000     MOVE WDR5-STATUS-CODE     TO STATUS-WS                               
142100     PERFORM IMS-STATUSCHECK                                              
142200     .                                                                    
142300                                                                          
142400 IMS-GNP-WDR550     SECTION.                                              
142500                                                                          
142600     MOVE 'IMS-GNP-WDR550         ' TO WS-CURRENT-IMS-SECTION             
142700                                                                          
142800     MOVE SPACE               TO ALL-SSA                                  
142900     STRING 'WDR550  (WDGXKEY >=' W-WDGXKEY-R550-X                        
143000                    '&KDLARM  >=' W-KDLARM-R5-MIN-X                       
143100                    '&KDLARM  <=' W-KDLARM-R5-MAX-X                       
143200                    '&IDARTNR >=' W-IDARTNR-R5-MIN-X                      
143300                    '&IDARTNR <=' W-IDARTNR-R5-MAX-X                      
143400                    '&IDDC    >=' W-IDDC-R5-MIN-X                         
143500                    '&IDDC    <=' W-IDDC-R5-MAX-X                         
143600                    '&IDLEVNR >=' W-IDLEVNR-R5-MIN-X                      
143700                    '&IDLEVNR <=' W-IDLEVNR-R5-MAX-X  ')'                 
143800          DELIMITED BY SIZE  INTO SSA1                                    
143900     MOVE '  GE'               TO GOOD-STATUSCODES                        
144000     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDR550 SSA1                   
144100     MOVE WDR5-STATUS-CODE     TO STATUS-WS                               
144200     PERFORM IMS-STATUSCHECK                                              
144300     .                                                                    
144400                                                                          
144500                                                                          
144600                                                                          
144700 IMS-REPL-WDR550 SECTION.                                                 
144800                                                                          
144900     MOVE 'IMS-REPL-WDR550         '  TO WS-CURRENT-IMS-SECTION           
145000                                                                          
145100     MOVE SPACE               TO ALL-SSA                                  
145200     MOVE '  ' TO GOOD-STATUSCODES                                        
145300     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDR550                       
145400     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
145500     PERFORM IMS-STATUSCHECK                                              
145600     .                                                                    
145700                                                                          
145800                                                                          
145900 IMS-DLET-WDR550 SECTION.                                                 
146000                                                                          
146100     MOVE 'IMS-DLET-WDR550         '  TO WS-CURRENT-IMS-SECTION           
146200                                                                          
146300     MOVE SPACE               TO ALL-SSA                                  
146400     MOVE '  ' TO GOOD-STATUSCODES                                        
146500     CALL CBLTDLI USING DLET WDR5-PCB DLI-IO-WDR550                       
146600     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
146700     PERFORM IMS-STATUSCHECK                                              
146800     .                                                                    
146900                                                                          
147000                                                                          
147100 IMS-GU-WDK601 SECTION.                                                   
147200                                                                          
147300     MOVE 'IMS-GU-WDK601           '  TO WS-CURRENT-IMS-SECTION           
147400                                                                          
147500     MOVE SPACE               TO ALL-SSA                                  
147600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
147700          DELIMITED BY SIZE INTO SSA1                                     
147800     MOVE '  GE'              TO GOOD-STATUSCODES                         
147900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
148000     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
148100     PERFORM IMS-STATUSCHECK                                              
148200     .                                                                    
148300                                                                          
148400                                                                          
148500 IMS-GU-WDK701 SECTION.                                                   
148600                                                                          
148700     MOVE 'IMS-GU-WDK701'  TO WS-CURRENT-IMS-SECTION                      
148800                                                                          
148900     MOVE SPACE               TO ALL-SSA                                  
149000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
149100          DELIMITED BY SIZE INTO SSA1                                     
149200     MOVE '  GE'              TO GOOD-STATUSCODES                         
149300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
149400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
149500     PERFORM IMS-STATUSCHECK                                              
149600     .                                                                    
149700                                                                          
149800                                                                          
149900 IMS-GU-WDK722 SECTION.                                                   
150000                                                                          
150100     MOVE 'IMS-GU-WDK722'  TO WS-CURRENT-IMS-SECTION                      
150200                                                                          
150300     MOVE SPACE               TO ALL-SSA                                  
150400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
150500          DELIMITED BY SIZE INTO SSA1                                     
150600     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
150700          DELIMITED BY SIZE INTO SSA2                                     
150800     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-K722-X ')'                   
150900          DELIMITED BY SIZE INTO SSA3                                     
151000     MOVE '  GE'              TO GOOD-STATUSCODES                         
151100     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
151200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
151300     PERFORM IMS-STATUSCHECK                                              
151400     .                                                                    
151500                                                                          
151600                                                                          
151700 IMS-GU-WDB601 SECTION.                                                   
151800                                                                          
151900     MOVE 'IMS-GU-WDB601'     TO WS-CURRENT-IMS-SECTION                   
152000                                                                          
152100     MOVE SPACE               TO ALL-SSA                                  
152200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
152300          DELIMITED BY SIZE INTO SSA1                                     
152400     MOVE '  GE'              TO GOOD-STATUSCODES                         
152500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
152600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
152700     PERFORM IMS-STATUSCHECK                                              
152800     .                                                                    
152900                                                                          
153000 IMS-GU-WDB601-MIN-MAX SECTION.                                           
153100                                                                          
153200     MOVE 'IMS-GU-WDB601-MIN-MAX' TO WS-CURRENT-IMS-SECTION               
153300                                                                          
153400     MOVE SPACE            TO ALL-SSA                                     
153500     STRING 'WDB601  (WDB6ASEQ>=' W-IDDC-B6-MIN-X                         
153600                    '&WDB6ASEQ<=' W-IDDC-B6-MAX-X ')'                     
153700       DELIMITED BY SIZE INTO SSA1                                        
153800     MOVE '  GE'            TO GOOD-STATUSCODES                           
153900     CALL CBLTDLI USING GU WDB6A-PCB DLI-IO-WDB601 SSA1                   
154000     MOVE WDB6A-STATUS-CODE TO STATUS-WS                                  
154100     PERFORM IMS-STATUSCHECK                                              
154200     .                                                                    
154300                                                                          
154400 IMS-GN-WDB601-MIN-MAX SECTION.                                           
154500                                                                          
154600     MOVE 'IMS-GN-WDB601-MIN-MAX' TO WS-CURRENT-IMS-SECTION               
154700                                                                          
154800     MOVE SPACE            TO ALL-SSA                                     
154900     STRING 'WDB601  (WDB6ASEQ>=' W-IDDC-B6-MIN-X                         
155000                    '&WDB6ASEQ<=' W-IDDC-B6-MAX-X ')'                     
155100       DELIMITED BY SIZE INTO SSA1                                        
155200     MOVE '  GBGE'          TO GOOD-STATUSCODES                           
155300     CALL CBLTDLI USING GN WDB6A-PCB DLI-IO-WDB601 SSA1                   
155400     MOVE WDB6A-STATUS-CODE TO STATUS-WS                                  
155500     PERFORM IMS-STATUSCHECK                                              
155600     .                                                                    
155700                                                                          
155800                                                                          
155900 IMS-INSERT-ALT1-2402 SECTION.                                            
156000                                                                          
156100     MOVE 'IMS-INSERT-ALT1-2402' TO WS-CURRENT-IMS-SECTION                
156200                                                                          
156300     MOVE SPACE               TO ALL-SSA                                  
156400     MOVE SPACE           TO GOOD-STATUSCODES                             
156500     CALL CBLTDLI USING ISRT ALT1-PCB ALT1-MSG-AREA                       
156600     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
156700     PERFORM IMS-STATUSCHECK                                              
156800     .                                                                    
156900                                                                          
157000 IMS-INSERT-ALT2-2403 SECTION.                                            
157100                                                                          
157200     MOVE 'IMS-INSERT-ALT2-2403' TO WS-CURRENT-IMS-SECTION                
157300                                                                          
157400     MOVE SPACE               TO ALL-SSA                                  
157500     MOVE SPACE           TO GOOD-STATUSCODES                             
157600     CALL CBLTDLI USING ISRT ALT2-PCB ALT2-MSG-AREA                       
157700     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
157800     PERFORM IMS-STATUSCHECK                                              
157900     .                                                                    
158000                                                                          
158100 IMS-STATUSCHECK SECTION.                                                 
158200                                                                          
158300     SET STATUS-IX TO 1                                                   
158400     SEARCH GOOD-STATUS                                                   
158500       AT END                                                             
158600         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
158700         DELIMITED BY SIZE INTO ERROR-TEXT                                
158800         CALL FELLOG                                                      
158900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
159000         CONTINUE                                                         
159100     END-SEARCH                                                           
159200     .                                                                    
