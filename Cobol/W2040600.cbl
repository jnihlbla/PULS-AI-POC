000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2040600.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   12/08/31.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*   FUNCTION:                                                             
000800*       PROCUREMENT.                                                      
000900*       CREATE AND UPDATE DELIVERY PROMISES.                              
001000*                                                                         
001100*       THE PROGRAM READS     WDF1                                        
001200*       THE PROGRAM READS     WDK6                                        
001300*       THE PROGRAM UPDATES   WDD9                                        
001400*       THE PROGRAM READS     WDK7                                        
001500*                                                                         
001600*   INDATA.                                                               
001700*       TRANSACTION: W2T406                                               
001800*       MID:         W2I40601                                             
001900*                                                                         
002000*   OUTDATA.                                                              
002100*       MOD:         W2O40601.                                            
002200*                                                                         
002300            SKIP3                                                         
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W2040600'.            
003000                                                                          
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600                                                                          
003700                                                                          
003800 77  IX                          PIC S9(4)   VALUE +0   COMP SYNC.        
003900 77  MAX-IX                      PIC S9(4)   VALUE +8   COMP SYNC.        
004000 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004100     88  INDATA-OK                           VALUE 'Y'.                   
004200     88  INDATA-WRONG                        VALUE 'N'.                   
004300                                                                          
004400 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
004500     88  KEYS-OK                             VALUE 'Y'.                   
004600     88  KEYS-WRONG                          VALUE 'N'.                   
004700                                                                          
004800 77  MIN-MAX-SW                  PIC X       VALUE 'Y'.                   
004900     88  MIN-MAX-OK                          VALUE 'Y'.                   
005000     88  MIN-MAX-WRONG                       VALUE 'N'.                   
005100                                                                          
005200 77  WS-INSERT-SW                PIC X       VALUE SPACE.                 
005300     88  WS-INSERT-YES                       VALUE 'Y'.                   
005400     88  WS-INSERT-NO                        VALUE 'N'.                   
005500                                                                          
005600 77  WS-723-SW                   PIC X       VALUE SPACE.                 
005700     88  WS-723-YES                          VALUE 'Y'.                   
005800     88  WS-723-NO                           VALUE 'N'.                   
005900                                                                          
006000 77  SECURITY-CHECK              PIC X       VALUE 'Y'.                   
006100     88  SECURITY-CHECK-PASS                 VALUE 'Y'.                   
006200     88  SECURITY-CHECK-FAIL                 VALUE 'N'.                   
006300                                                                          
006400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006500     88  OWN-MID                             VALUE '2406'.                
006600     88  GOOD-MID                            VALUE '2401' '2402'          
006700                                                   '2404'                 
006800                                                   '2405' '2406'          
006900                                                   '2407' '2408'          
007000                                                   '2409'.                
007100     88  HELP-MID                            VALUE '0551'.                
007200                                                                          
007300 01  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
007400 01  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
007500 01  WS-IDLEVNR-SHIP             PIC X(5)    VALUE SPACE.                 
007600 01  WS-KVDAGAR-INLEV            PIC S9(3)   COMP-3.                      
007700                                                                          
007800 01  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
007900 01  FILLER REDEFINES WS-AAAAMMDD.                                        
008000     03 WS-SS                    PIC 9(2).                                
008100     03 WS-AAMMDD                PIC 9(6).                                
008200 01  SYSTEM-DATE                 PIC 9(6)    VALUE ZERO.                  
008300 01  SYSTEM-TIME                 PIC 9(8)    VALUE ZERO.                  
008400 01  FILLER    REDEFINES SYSTEM-TIME.                                     
008500     03  SYSTEM-HHMMSS           PIC 9(6).                                
008600     03  FILLER                  PIC 9(2).                                
008700 01  WS-TIBORT1-SW               PIC X       VALUE 'N'.                   
008800     88 WS-TIBORT1-YES                       VALUE 'Y'.                   
008900     88 WS-TIBORT1-NOO                       VALUE 'N'.                   
009000 01  WS-KDCMD                    PIC X       VALUE SPACE.                 
009100     88 WS-KDCMD-GOOD                        VALUE 'D' 'C' ' '.           
009200     88 WS-KDCMD-DELETE                      VALUE 'D'.                   
009300     88 WS-KDCMD-CHANGE                      VALUE 'C'.                   
009400 01  WS-NDC-KVDAGAR-TT-UPD-GRP.                                           
009500     05  WS-NDC-KVDAGAR-TT-UPD   OCCURS 8 TIMES                           
009600                                 PIC S9(3)   VALUE ZERO COMP-3.           
009700 01  WS-NDC-KVDAGAR-TT-NY        PIC S9(3)   VALUE ZERO COMP-3.           
009800 01  WS-DALEVBSK-AVS-UPD         PIC X(5).                                
009900 01  WS-TILEVBSK-INL-UPD         PIC X(5).                                
010000 01  WS-DALEVBSK-AVS-NY          PIC X(5).                                
010100 01  WS-TILEVBSK-INL-NY          PIC X(5).                                
010200                                                                          
010300 01  WS-UPDATE-GRP.                                                       
010400     03  WS-UPDATE-LINE OCCURS 8 TIMES.                                   
010500         05  WS-DALEVBSK-AVS-AAAAMMDD.                                    
010600             07  WS-DALEVBSK-AVS-SS                                       
010700                                 PIC 9(2).                                
010800             07  WS-DALEVBSK-AVS-AAMMDD                                   
010900                                 PIC 9(6).                                
011000         05  WS-KVAVIS-UPD       PIC X(7).                                
011100         05  WS-TILEVBSK-INL-AAMMDD                                       
011200                                 PIC X(6).                                
011300                                                                          
011400 01  WS-INSERT-GRP.                                                       
011500     03  WS-DALEVBSK-AVS-NY-AAAAMMDD.                                     
011600         05  WS-DALEVBSK-AVS-NY-SS                                        
011700                                 PIC X(2).                                
011800         05  WS-DALEVBSK-AVS-NY-AAMMDD                                    
011900                                 PIC X(6).                                
012000     03  WS-KVAVIS-NY            PIC X(7).                                
012100     03  WS-TILEVBSK-INL-NY-AAMMDD                                        
012200                                 PIC X(6).                                
012300     03  WS-IDLEVNR-NY           PIC X(5).                                
012400     03  WS-IDLEVNR-SHIP-NY      PIC X(5).                                
012500                                                                          
012600     EJECT                                                                
012610 01  FILLER             PIC X(16) VALUE 'WWIDFTG      '.                  
012620*01 -COPY WWIDFTG                                                         
012630                                                                          
012640     EJECT                                                                
012700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
012800 01  GENERAL-SUBPROGRAMS.                                                 
012900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
013500     EJECT                                                                
013600*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
013700*01 -COPY WMEDAREA                                                        
013800     SKIP3                                                                
013900 01  MESSAGE-CODES.                                                       
014000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014110     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014111     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
014120     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014300     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
014400     03  ERR-PART-EXPIRED        PIC X(3)    VALUE '018'.                 
014500     03  ERR-REFILL-PART         PIC X(3)    VALUE '434'.                 
014600     03  ERR-SUPP-NOT-UPD-FOR-DC                                          
014700                                 PIC X(3)    VALUE '437'.                 
014800     03  ERR-USER-NOT-AUTHORIZED                                          
014900                                 PIC X(3)    VALUE '405'.                 
015000     03  ERR-ENTER-SUPP-NO                                                
015100                                 PIC X(3)    VALUE '297'.                 
015200     03  ERR-SUPP-IS-MISSING                                              
015300                                 PIC X(3)    VALUE '273'.                 
015400     03  ERR-MAIN-SUPP-ZERO                                               
015500                                 PIC X(3)    VALUE '426'.                 
015600     03  ERR-DELIVERY-PROMISE-MISS                                        
015700                                 PIC X(3)    VALUE '421'.                 
015800     03  ERR-OTHER-SUPP-EXIST                                             
015900                                 PIC X(3)    VALUE '425'.                 
016000     03  ERR-DATE-SUPP-INFO-OK-CHECK                                      
016100                                 PIC X(3)    VALUE '430'.                 
016200     03  ERR-HIGHLIGHT-FIELDS-WRONG                                       
016300                                 PIC X(3)    VALUE '409'.                 
016400     03  ERR-FIELDS-ARE-NOT-NUMERIC                                       
016500                                 PIC X(3)    VALUE '020'.                 
016600     03  ERR-UPDATE-NOT-ALLOWED                                           
016700                                 PIC X(3)    VALUE '007'.                 
016800     03  ERR-PREADV-D-C-NOT-ALLOWED                                       
016900                                 PIC X(3)    VALUE '296'.                 
017000     03  ERR-CHANGE-DISPDAY-EXIST                                         
017100                                 PIC X(3)    VALUE '431'.                 
017200     03  ERR-DELIVERY-PROMISE-EXIST                                       
017300                                 PIC X(3)    VALUE '424'.                 
017400     03  ERR-MORE-THAN-ONE-FUNCTION                                       
017500                                 PIC X(3)    VALUE '097'.                 
017600     03  INF-PRESS-PF11                                                   
017700                                 PIC X(3)    VALUE '003'.                 
017800                                                                          
017900     EJECT                                                                
018000*01  -COPY WWDCLAND                                                       
018100     EJECT                                                                
018200*    -COPY WY2000W1                                                       
018300     EJECT                                                                
018400*    -COPY WY2000W2                                                       
018500     EJECT                                                                
018600*01  -COPY WDATAREA                                                       
018700     EJECT                                                                
018800*01  -COPY WORKAREA                                                       
018900     EJECT                                                                
019000*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
019100*                                                                         
019200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
019300     SKIP3                                                                
019400*01 -COPY WMSGINIT                                                        
019500     EJECT                                                                
019601*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
019701*                                                                         
019801 01  SAVE-AREA.                                                           
019901     03  SAVE-IDTRANS            PIC X(4)    VALUE '2406'.                
020601     03  SAVE-DALEVBSK-AVS-ENTER PIC 9(08).                               
020701     03  SAVE-DALEVBSK-AVS-NEXT  PIC 9(08).                               
020801     EJECT                                                                
020901*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
021001*                                                                         
021101 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021201     SKIP3                                                                
021301*01  MID -COPY W2I40601                                                   
021401     EJECT                                                                
021501 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021601     SKIP3                                                                
021701*01  -COPY WMSGAREA                                                       
021801     EJECT                                                                
021901     03  MOD REDEFINES MSG-AREA.                                          
022001*      05  -COPY W2O40601                                                 
022101     EJECT                                                                
022201 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022301     SKIP3                                                                
022401*01  -COPY WMFSAREA                                                       
022501     EJECT                                                                
022601*    --- WORK-AREAS FOR IMS-SECTIONS                                      
022701*                                                                         
022801 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022901     SKIP3                                                                
023001 01  KEYS-FOR-DLI.                                                        
023101     03  W-WDD901KY-X.                                                    
023201         05  W-IDARTNR-X.                                                 
023301             07  W-IDARTNR       PIC S9(8)   VALUE ZERO COMP-3.           
023401         05  W-IDDC-X.                                                    
023501             07  W-IDDC          PIC X(2)    VALUE SPACE.                 
023601     03  W-IDLEVNR-X.                                                     
023701         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
023801     03  W-DALEVBSK-X.                                                    
023901         05  W-DALEVBSK          PIC X(8)    VALUE SPACE.                 
024001     03  W-IDLEVBSK-X.                                                    
024101         05  W-IDLEVBSK          PIC S9(1)   VALUE ZERO COMP-3.           
024201     03  W-IDLAND-X.                                                      
024301         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
024401     03  W-KDSEGKEY-X.                                                    
024501         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
024601     03  W-IDAVTAL-X.                                                     
024701         05  W-IDAVTAL           PIC S9(12)   VALUE ZERO COMP-3.          
024801     SKIP2                                                                
024901*    --- STATUS CODES FROM IMS                                            
025001 01  STATUS-WS                   PIC XX.                                  
025101     88  SEGMENT-FOUND                       VALUE '  '.                  
025201     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
025301     88  SEGMENT-MISSING                     VALUE 'GE'.                  
025401     SKIP2                                                                
025501 01  GOOD-STATUSCODES.                                                    
025601     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025701     SKIP3                                                                
025801 01  SSA1                        PIC X(64).                               
025901 01  SSA2                        PIC X(64).                               
026001 01  SSA3                        PIC X(64).                               
026101     EJECT                                                                
026201*    --- IMS FUNCTION CODES                                               
026301*01  -COPY W0003                                                          
026401     EJECT                                                                
026501*    ---  DLI INPUT-OUTPUT AREA                                           
026601                                                                          
026701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
026801 01  DLI-IO-WDB601.                                                       
026901*    03  -COPY WDB601 -PRE WDB601-                                        
027001     EJECT                                                                
027101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
027201 01  DLI-IO-WDF101.                                                       
027301*    03  -COPY WDF101 -PRE WDF101-                                        
027401     EJECT                                                                
027501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
027601 01  DLI-IO-WDF116.                                                       
027701*    03  -COPY WDF116 -PRE WDF116-                                        
027801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
027901 01  DLI-IO-WDK601.                                                       
028001*    03  -COPY WDK601 -PRE WDK601-                                        
028101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
028201 01  DLI-IO-WDD901.                                                       
028301*    03  -COPY WDD901 -PRE WDD901-                                        
028401     EJECT                                                                
028501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
028601 01  DLI-IO-WDD902.                                                       
028701*    03  -COPY WDD902 -PRE WDD902-                                        
028801     EJECT                                                                
028901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
029001 01  DLI-IO-WDD924.                                                       
029101*    03  -COPY WDD924 -PRE WDD924-                                        
029201     EJECT                                                                
029301 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD925'.                      
029401 01  DLI-IO-WDD925.                                                       
029501*    03  -COPY WDD925 -PRE WDD925-                                        
029601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
029701 01  DLI-IO-WDK701.                                                       
029801*    03  -COPY WDK701 -PRE WDK701-                                        
029901     EJECT                                                                
030001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
030101 01  DLI-IO-WDK711.                                                       
030201*    03  -COPY WDK711 -PRE WDK711-                                        
030301     EJECT                                                                
030401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
030501 01  DLI-IO-WDK712.                                                       
030601*    03  -COPY WDK712 -PRE WDK712-                                        
030701     EJECT                                                                
030801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
030901 01  DLI-IO-WDK722.                                                       
031001*    03  -COPY WDK722 -PRE WDK722-                                        
031101     EJECT                                                                
031201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK723'.                      
031301 01  DLI-IO-WDK723.                                                       
031401*    03  -COPY WDK723 -PRE WDK723-                                        
031501     EJECT                                                                
031601 LINKAGE SECTION.                                                         
031701*01  -COPY W0009   -PRE MSG-                                              
031801*01  -COPY W0008   -PRE WDP7-                                             
031901     05  FILLER                  PIC X.                                   
032001                                                                          
032101*01  -COPY W0008  -PRE WDF1-                                              
032201     05  FILLER                  PIC X.                                   
032301                                                                          
032401*01  -COPY W0008  -PRE WDK6-                                              
032501     05  FILLER                  PIC X.                                   
032601                                                                          
032701*01  -COPY W0008  -PRE WDD9-                                              
032801     05  KFBA-IDARTNR            PIC S9(9) COMP-3.                        
032901     05  KFBA-IDDC               PIC X(2).                                
033001     05  KFBA-IDLEVNR            PIC X(5).                                
033101                                                                          
033201*01  -COPY W0008  -PRE WDK7-                                              
033301     05  FILLER                  PIC X.                                   
033401                                                                          
033501*01  -COPY W0008  -PRE WDB6-                                              
033601     05  FILLER                  PIC X.                                   
033701     EJECT                                                                
033801 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDF1-PCB WDK6-PCB             
033901     WDD9-PCB WDK7-PCB WDB6-PCB.                                          
034001 MAIN SECTION.                                                            
034101     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDF1-PCB WDK6-PCB             
034201     WDD9-PCB WDK7-PCB WDB6-PCB.                                          
034202                                                                          
034401     PERFORM IMS-GET-MSG                                                  
034501     IF SEGMENT-FOUND                                                     
034601       PERFORM A-INIT                                                     
034701       PERFORM B-CHECK-KEYS                                               
034801       IF SECURITY-CHECK-PASS                                             
034901         IF KEYS-OK                                                       
035001           IF MFS-UPDATE                                                  
035101             PERFORM G-CHECK-INPUT                                        
035201             IF INDATA-OK                                                 
035301               PERFORM H-UPDATE                                           
035401             END-IF                                                       
035501           ELSE                                                           
035601             IF MFS-FIRST                                                 
035701               PERFORM C-FIRST-PAGE                                       
035801             ELSE                                                         
035901               IF MFS-NEXT                                                
036001                 PERFORM D-NEXT-PAGE                                      
036101               ELSE                                                       
036201                 PERFORM E-SAME-PAGE                                      
036301               END-IF                                                     
036401             END-IF                                                       
036501           END-IF                                                         
036601           IF INDATA-OK                                                   
036701             PERFORM F-READ-SHOW-INFO                                     
036801           END-IF                                                         
038101         END-IF                                                           
038201       END-IF                                                             
038301       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O40601 + 4                      
038401       PERFORM IMS-INSERT-MSG                                             
038501     END-IF                                                               
038601                                                                          
038701     MOVE ZERO                   TO RETURN-CODE                           
038801     GOBACK                                                               
038901     .                                                                    
039001     EJECT                                                                
039101 A-INIT SECTION.                                                          
039201                                                                          
039301     IF MSG-DOUBLE-TRANSACTIONS                                           
039401       MOVE MSG-INDATA-MINUS-2-TRANSACT                                   
039501                                 TO MID-W2I40601                          
039601       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
039701       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
039801     ELSE                                                                 
039901       MOVE MSG-INDATA-MINUS-1-TRANSACT                                   
040001                                 TO MID-W2I40601                          
040101       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
040201       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
040301     END-IF                                                               
040401                                                                          
040501     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
040601     MOVE MSG-IDPFK              TO MFS-IDPFK                             
040701     MOVE MFS-IDTRANS            TO W-IDTRANS                             
040801                                                                          
040901     MOVE LOW-VALUE              TO MSG-AREA                              
041001     MOVE 'W2O406N1'             TO MFS-IDMOD                             
041101     MOVE '2406'                 TO MOD-IDTRANS                           
041201     MOVE SPACE                  TO MOD-TEMFSFEL                          
041301                                    MOD-TEMFSINF                          
041401                                                                          
041501     IF OWN-MID OR HELP-MID                                               
041601       CONTINUE                                                           
041701     ELSE                                                                 
041801       MOVE SPACE                TO MFS-KDTRTYP                           
041901       MOVE '7'                  TO MFS-IDPFK                             
042001     END-IF                                                               
042101                                                                          
042201     ACCEPT SYSTEM-DATE        FROM DATE                                  
042301     ACCEPT SYSTEM-TIME        FROM TIME                                  
042601     .                                                                    
042701     EJECT                                                                
042801 B-CHECK-KEYS SECTION.                                                    
042901                                                                          
043001     MOVE ALL '+'                TO MSGI-WMSGINIT                         
043101     MOVE '001'                  TO MSGI-KDCALL                           
043201     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
043301     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
043401     MOVE '2406'                 TO MSGI-IDTRANS                          
043601     IF OWN-MID                                                           
043701       MOVE MID-IDARTNR-IN       TO MSGI-IDARTNR                          
043801       MOVE MID-IDDC-IN          TO MSGI-IDDC-KEY                         
043901       MOVE MID-IDLEVNR-IN       TO MSGI-IDLEVNR                          
044201     END-IF                                                               
044301     CALL W005INIT            USING MSGI-WMSGINIT                         
044401                                    WDP7-PCB                              
044602     MOVE MSGI-SPAR-AREA         TO SAVE-AREA                             
044801                                                                          
044901*    - LANGUAGE TO BE USED BY MEDKONV                                     
045001     MOVE MSGI-IDLAND-SPR        TO MED-IDSKYLT                           
045101                                                                          
045201     MOVE YES                    TO KEYS-SW                               
045503                                                                          
045504*    -- CHECK OF IDARTNR                                                  
045601     MOVE MFS-ERASE-FIELD        TO MOD-IDARTNR-IN                        
045701                                                                          
045801     IF MID-IDARTNR-IN NOT = ALL '+'                                      
045901       MOVE '7'                  TO MFS-IDPFK                             
046001       MOVE SPACE                TO MFS-KDTRTYP                           
046101     END-IF                                                               
046201     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
046301     IF MSGI-IDARTNR NUMERIC                                              
046401       MOVE MSGI-IDARTNR         TO W-IDARTNR                             
046501     ELSE                                                                 
046601       MOVE NOO                  TO KEYS-SW                               
046701       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
046801     END-IF                                                               
046901                                                                          
047001*    -- CHECK OF IDDC                                                     
047101     MOVE MFS-ERASE-FIELD        TO MOD-IDDC-IN                           
047201                                                                          
047301     IF MID-IDDC-IN NOT = ALL '+'                                         
047401       MOVE '7'                  TO MFS-IDPFK                             
047501       MOVE SPACE                TO MFS-KDTRTYP                           
047601     END-IF                                                               
047701                                                                          
047702     MOVE MSGI-IDDC-KEY          TO W-IDDC                                
047703     PERFORM IMS-GU-WDB601                                                
047704     IF SEGMENT-FOUND AND                                                 
047705        (WDB601-DCS-NDC-CN OR                                             
047706        (WDB601-DCS-NDC-NA AND WDB601-DCS-USA))                           
047707        CONTINUE                                                          
047708     ELSE                                                                 
047709        MOVE NOO                 TO KEYS-SW                               
047710        MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                          
047730     END-IF                                                               
057446                                                                          
057447*    -- CHECK OF IDLEVNR                                                  
057448     MOVE MFS-ERASE-FIELD        TO MOD-IDLEVNR-IN                        
057449                                                                          
057450     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
057451       MOVE '7'                  TO MFS-IDPFK                             
057452       MOVE SPACE                TO MFS-KDTRTYP                           
057453     END-IF                                                               
057454     MOVE MSGI-IDLEVNR           TO W-IDLEVNR                             
057456                                                                          
057457     IF KEYS-OK                                                           
057458       PERFORM BA-CHECK-IDARTNR-IDLEVNR                                   
057459     END-IF                                                               
057460                                                                          
057461                                                                          
057462     IF GOOD-MID OR KEYS-OK                                               
057463       MOVE MSGI-IDARTNR         TO MOD-IDARTNR-UT                        
057464       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
057465       MOVE MSGI-IDDC-KEY        TO MOD-IDDC-UT                           
057466       MOVE WS-IDLEVNR           TO MOD-IDLEVNR-UT                        
057467       MOVE WS-IDLEVNR-SHIP      TO MOD-IDLEVNR-SHIP-UT                   
057472     ELSE                                                                 
057473       MOVE MFS-ERASE-FIELD      TO MOD-IDARTNR                           
057474                                    MOD-IDDC-UT                           
057475                                    MOD-IDLEVNR-UT                        
057476                                    MOD-IDLEVNR-SHIP-UT                   
057477     END-IF                                                               
057478                                                                          
057479     IF KEYS-WRONG                                                        
057480       IF GOOD-MID                                                        
057481         CONTINUE                                                         
057482       ELSE                                                               
057483         MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                          
057484       END-IF                                                             
057485       CALL WMEDKONV          USING MED-WMEDAREA                          
057486       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
057487       PERFORM MFS-ERASE-FIELD-IN                                         
057488       PERFORM MFS-ERASE-FIELD-OUT                                        
057489     END-IF                                                               
057490     .                                                                    
057491     EJECT                                                                
057492 BA-CHECK-IDARTNR-IDLEVNR SECTION.                                        
057493                                                                          
057494     PERFORM IMS-GU-WDK601                                                
057495     IF SEGMENT-FOUND                                                     
057496       MOVE WDK601-ART-REKSIFFR  TO MOD-REKSIFFR                          
057497       MOVE '-'                  TO MOD-DASH                              
057498       IF WDK601-ART-KDERS-UTG > 0                                        
057499         MOVE ERR-PART-EXPIRED   TO MED-IDMFSFEL                          
057500         MOVE NOO                TO KEYS-SW                               
057501       END-IF                                                             
057502     ELSE                                                                 
057503       MOVE ERR-PART-MISSING     TO MED-IDMFSFEL                          
057504       MOVE NOO                  TO KEYS-SW                               
057505     END-IF                                                               
057506     IF KEYS-OK                                                           
057507       PERFORM IMS-GU-WDK701                                              
057508       IF SEGMENT-MISSING                                                 
057509         MOVE ERR-PART-MISSING   TO MED-IDMFSFEL                          
057510         MOVE NOO                TO KEYS-SW                               
057511       ELSE                                                               
057512         PERFORM IMS-GU-WDK711                                            
057513         IF SEGMENT-FOUND                                                 
057514           IF MID-IDLEVNR-IN = ALL '+'                                    
057515             MOVE WDK711-SLAG-IDLEVNR                                     
057516                                 TO W-IDLEVNR                             
057517                                    WS-IDLEVNR                            
057520                                                                          
057521           ELSE                                                           
057522             MOVE MSGI-IDLEVNR   TO W-IDLEVNR                             
057523                                    WS-IDLEVNR                            
057526           END-IF                                                         
057527           IF WDK711-SLAG-IDDC-REF NOT = SPACE                            
057528             MOVE ERR-REFILL-PART                                         
057529                                 TO MED-IDMFSFEL                          
057530             MOVE NOO            TO KEYS-SW                               
057531           END-IF                                                         
057532           MOVE WDK711-SLAG-IDLEVNR                                       
057533                                 TO WS-IDLEVNR-8                          
057535           IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8 OR                   
057536              MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                
057537             SET SECURITY-CHECK-PASS                                      
057538                                 TO TRUE                                  
057539           ELSE                                                           
057540             SET SECURITY-CHECK-FAIL                                      
057541                                 TO TRUE                                  
057542             MOVE NOO            TO KEYS-SW                               
057550             MOVE ERR-USER-NOT-AUTHORIZED                                 
057601                                 TO MED-IDMFSFEL                          
057701           END-IF                                                         
057801         ELSE                                                             
057901           MOVE ERR-PART-MISSING TO MED-IDMFSFEL                          
058001           MOVE NOO              TO KEYS-SW                               
058101         END-IF                                                           
058201       END-IF                                                             
058301     END-IF                                                               
058401     IF KEYS-OK                                                           
058501       IF WDK711-SLAG-IDLEVNR = WS-IDLEVNR                                
058601         PERFORM IMS-GNP-WDK722                                           
058701         IF SEGMENT-FOUND                                                 
058801           MOVE WDK722-XLAG-IDLEVNR-SHIP                                  
058901                                 TO WS-IDLEVNR-SHIP                       
059001         ELSE                                                             
059101           PERFORM BAA-FETCH-WDK723                                       
059201         END-IF                                                           
059301       ELSE                                                               
059401         PERFORM BAA-FETCH-WDK723                                         
059501       END-IF                                                             
059601     END-IF                                                               
059701     IF KEYS-OK                                                           
059801       PERFORM IMS-GU-WDF101                                              
059901       IF SEGMENT-MISSING                                                 
060001         MOVE ERR-SUPP-IS-MISSING                                         
060101                                 TO MED-IDMFSFEL                          
060201         MOVE NOO                TO KEYS-SW                               
060301       END-IF                                                             
060401     END-IF                                                               
060501     .                                                                    
060601     EJECT                                                                
060701 BAA-FETCH-WDK723 SECTION.                                                
060801                                                                          
060901     MOVE WS-IDLEVNR             TO WS-IDLEVNR-SHIP                       
061001     PERFORM IMS-GNP-WDK723                                               
061101     MOVE NOO                    TO WS-723-SW                             
061201     PERFORM                                                              
061301       UNTIL SEGMENT-MISSING OR WS-723-YES                                
061401       IF WDK723-SAVT-IDLEVNR-AVT = WS-IDLEVNR                            
061501         MOVE WDK723-SAVT-IDLEVNR-SHIP                                    
061601                                 TO WS-IDLEVNR-SHIP                       
061701         MOVE YES                TO WS-723-SW                             
061801       END-IF                                                             
061901       PERFORM IMS-GNP-WDK723                                             
062001     END-PERFORM                                                          
062101     .                                                                    
062201     EJECT                                                                
062301 C-FIRST-PAGE SECTION.                                                    
062401                                                                          
062901     PERFORM MFS-ERASE-FIELD-IN                                           
063001     .                                                                    
063101     EJECT                                                                
063201 D-NEXT-PAGE SECTION.                                                     
063301                                                                          
063401     IF SAVE-IDTRANS = '2406'                                             
063902        MOVE SAVE-DALEVBSK-AVS-NEXT  TO W-DALEVBSK                        
064202        PERFORM MFS-ERASE-FIELD-IN                                        
064302     END-IF                                                               
064402     .                                                                    
064502     EJECT                                                                
064602 E-SAME-PAGE SECTION.                                                     
064702                                                                          
064802     IF OWN-MID OR HELP-MID                                               
065302        MOVE SAVE-DALEVBSK-AVS-ENTER TO W-DALEVBSK                        
065303                                                                          
065404       IF MID-UPDATE-LINE-GRP = ALL '+' AND                               
065602          MID-INSERT-LINE     = ALL '+' AND                               
065603          MID-TEXT            = ALL '+'                                   
065702         PERFORM MFS-ERASE-FIELD-IN                                       
065802       ELSE                                                               
065902         MOVE INF-PRESS-PF11     TO MED-IDMFSFEL                          
066002         CALL WMEDKONV        USING MED-WMEDAREA                          
066102         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
066202         PERFORM EA-MID-INDATA-TO-MOD                                     
066302       END-IF                                                             
066402     ELSE                                                                 
066502       PERFORM MFS-ERASE-FIELD-IN                                         
066602     END-IF                                                               
066702     .                                                                    
066802     EJECT                                                                
066902 EA-MID-INDATA-TO-MOD SECTION.                                            
067002                                                                          
067102     PERFORM                                                              
067202     VARYING IX FROM +1 BY +1                                             
067302       UNTIL IX > MAX-IX                                                  
067402                                                                          
067502       IF MID-KDCMD-UPD (IX) NOT = ALL '+'                                
067602         MOVE MID-KDCMD-UPD (IX) TO MOD-KDCMD-UPD        (IX)             
067702         MOVE MFS-ADD-READ-FIELD TO MOD-KDCMD-UPD-ATTR   (IX)             
067802       ELSE                                                               
067902         MOVE MFS-ERASE-FIELD    TO MOD-KDCMD-UPD        (IX)             
068002       END-IF                                                             
068102                                                                          
068202       IF MID-DALEVBSK-AVS-UPD (IX) NOT = ALL '+'                         
068302         MOVE MID-DALEVBSK-AVS-UPD (IX)                                   
068402                                 TO MOD-DALEVBSK-AVS-UPD (IX)             
068502         MOVE MFS-ADD-READ-FIELD TO MOD-DALEVBSK-AVS-UPD-ATTR(IX)         
068602       ELSE                                                               
068702         MOVE MFS-ERASE-FIELD    TO MOD-DALEVBSK-AVS-UPD (IX)             
068802       END-IF                                                             
068902                                                                          
069002       IF MID-KVAVIS-UPD (IX) NOT = ALL '+'                               
069102         MOVE MID-KVAVIS-UPD (IX)                                         
069202                                 TO MOD-KVAVIS-UPD       (IX)             
069302         MOVE MFS-ADD-READ-FIELD TO MOD-KVAVIS-UPD-ATTR  (IX)             
069402       ELSE                                                               
069502         MOVE MFS-ERASE-FIELD    TO MOD-KVAVIS-UPD       (IX)             
069602       END-IF                                                             
069702                                                                          
069802       IF MID-TILEVBSK-INL-UPD (IX) NOT = ALL '+'                         
069902         MOVE MID-TILEVBSK-INL-UPD (IX)                                   
070002                                 TO MOD-TILEVBSK-INL-UPD (IX)             
070102         MOVE MFS-ADD-READ-FIELD TO MOD-TILEVBSK-INL-UPD-ATTR(IX)         
070202       ELSE                                                               
070302         MOVE MFS-ERASE-FIELD    TO MOD-TILEVBSK-INL-UPD (IX)             
070402       END-IF                                                             
070502                                                                          
070602     END-PERFORM                                                          
070702                                                                          
070802     IF MID-DALEVBSK-AVS-NY  NOT = ALL '+'                                
070902       MOVE MID-DALEVBSK-AVS-NY  TO MOD-DALEVBSK-AVS-NY                   
071002       MOVE MFS-ADD-READ-FIELD   TO MOD-DALEVBSK-AVS-NY-ATTR              
071102     ELSE                                                                 
071202       MOVE MFS-ERASE-FIELD      TO MOD-DALEVBSK-AVS-NY                   
071302     END-IF                                                               
071402                                                                          
071502     IF MID-KVAVIS-NY NOT = ALL '+'                                       
071602       MOVE MID-KVAVIS-NY        TO MOD-KVAVIS-NY                         
071702       MOVE MFS-ADD-READ-FIELD   TO MOD-KVAVIS-NY-ATTR                    
071802     ELSE                                                                 
071902       MOVE MFS-ERASE-FIELD      TO MOD-KVAVIS-NY                         
072002     END-IF                                                               
072102                                                                          
072202     IF MID-TILEVBSK-INL-NY NOT = ALL '+'                                 
072302       MOVE MID-TILEVBSK-INL-NY  TO MOD-TILEVBSK-INL-NY                   
072402       MOVE MFS-ADD-READ-FIELD   TO MOD-TILEVBSK-INL-NY-ATTR              
072502     ELSE                                                                 
072602       MOVE MFS-ERASE-FIELD      TO MOD-TILEVBSK-INL-NY                   
072702     END-IF                                                               
072802                                                                          
072902     IF MID-IDLEVNR-NY NOT = ALL '+'                                      
073002       MOVE MID-IDLEVNR-NY       TO MOD-IDLEVNR-NY                        
073102       MOVE MFS-ADD-READ-FIELD   TO MOD-IDLEVNR-NY-ATTR                   
073202     ELSE                                                                 
073302       MOVE MFS-ERASE-FIELD      TO MOD-IDLEVNR-NY                        
073402     END-IF                                                               
073502                                                                          
073602     IF MID-IDLEVNR-SHIP-NY NOT = ALL '+'                                 
073702       MOVE MID-IDLEVNR-SHIP-NY  TO MOD-IDLEVNR-SHIP-NY                   
073802       MOVE MFS-ADD-READ-FIELD   TO MOD-IDLEVNR-SHIP-NY-ATTR              
073902     ELSE                                                                 
074002       MOVE MFS-ERASE-FIELD      TO MOD-IDLEVNR-SHIP-NY                   
074102     END-IF                                                               
074202                                                                          
074302     IF MID-TELEVBSK-TEXT1 NOT = ALL '+'                                  
074402       MOVE MID-TELEVBSK-TEXT1   TO MOD-TELEVBSK-TEXT1                    
074502       MOVE MFS-ADD-READ-FIELD   TO MOD-TELEVBSK-TEXT1-ATTR               
074602     ELSE                                                                 
074702       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
074802                                 TO MOD-TELEVBSK-TEXT1                    
074902     END-IF                                                               
075002                                                                          
075102     IF MID-TELEVBSK-TEXT2 NOT = ALL '+'                                  
075202       MOVE MID-TELEVBSK-TEXT2   TO MOD-TELEVBSK-TEXT2                    
075302       MOVE MFS-ADD-READ-FIELD   TO MOD-TELEVBSK-TEXT2-ATTR               
075402     ELSE                                                                 
075502       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
075602                                 TO MOD-TELEVBSK-TEXT2                    
075702     END-IF                                                               
075802                                                                          
075902     IF MID-TELEVBSK-TEXT3 NOT = ALL '+'                                  
076002       MOVE MID-TELEVBSK-TEXT3   TO MOD-TELEVBSK-TEXT3                    
076102       MOVE MFS-ADD-READ-FIELD   TO MOD-TELEVBSK-TEXT3-ATTR               
076202     ELSE                                                                 
076302       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
076402                                 TO MOD-TELEVBSK-TEXT3                    
076502     END-IF                                                               
076602                                                                          
076702     IF MID-TELEVBSK-TEXT4 NOT = ALL '+'                                  
076802       MOVE MID-TELEVBSK-TEXT4   TO MOD-TELEVBSK-TEXT4                    
076902       MOVE MFS-ADD-READ-FIELD   TO MOD-TELEVBSK-TEXT4-ATTR               
077002     ELSE                                                                 
077102       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
077202                                 TO MOD-TELEVBSK-TEXT4                    
077302     END-IF                                                               
077402                                                                          
077502     IF MID-TIBORT NOT = ALL '+'                                          
077602       MOVE MID-TIBORT           TO MOD-TIBORT                            
077702       MOVE MFS-ADD-READ-FIELD   TO MOD-TIBORT-ATTR                       
077802     ELSE                                                                 
077902       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
078002                                 TO MOD-TIBORT                            
078102     END-IF                                                               
078202                                                                          
078302     .                                                                    
078402     EJECT                                                                
078502 F-READ-SHOW-INFO SECTION.                                                
078607                                                                          
078702     PERFORM IMS-GU-WDD901                                                
078802     IF SEGMENT-MISSING                                                   
078902       MOVE ERR-DELIVERY-PROMISE-MISS                                     
079002                                 TO MED-IDMFSINF                          
079102       CALL WMEDKONV          USING MED-WMEDAREA                          
079202       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
079302       PERFORM MFS-ERASE-FIELD-OUT                                        
079502     ELSE                                                                 
079602       PERFORM IMS-GNP-WDD924                                             
079702       IF SEGMENT-FOUND                                                   
079802          MOVE WDD924-LEV-DALEVBSK-AVS TO SAVE-DALEVBSK-AVS-ENTER         
079902          MOVE WDD924-LEV-DALEVBSK-AVS TO SAVE-DALEVBSK-AVS-NEXT          
080002       END-IF                                                             
080102       PERFORM                                                            
080202       VARYING IX FROM +1 BY +1                                           
080302         UNTIL IX > MAX-IX                                                
080402         IF SEGMENT-FOUND                                                 
080502*          DISPATCH DAY                                                   
080602           MOVE WDD924-LEV-DALEVBSK-AVS                                   
080702                                 TO WS-AAAAMMDD                           
080802           MOVE WS-AAMMDD        TO DAT-I-TIDATUM                         
080902           MOVE 'AAMMDD'         TO DAT-KDDATFORM                         
081002           PERFORM S01-CALL-WDATKONV                                      
081102           MOVE DAT-TIAAVVD      TO MOD-DALEVBSK-AVS (IX)                 
081202                                                                          
081302*          QUANTITY                                                       
081402           MOVE WDD924-LEV-KVAVIS-BSKURS                                  
081502                                 TO MOD-KVAVIS (IX)                       
081602                                                                          
081702*          PLANNED RECEIVE DAY                                            
081802           MOVE WDD924-LEV-TILEVBSK-INL                                   
081902                                 TO DAT-I-TIDATUM                         
082002           MOVE 'AAMMDD'         TO DAT-KDDATFORM                         
082102           PERFORM S01-CALL-WDATKONV                                      
082202           MOVE DAT-TIAAVVD      TO MOD-TILEVBSK-INL (IX)                 
082302                                                                          
082402*          AVAILABLE DAY                                                  
082502           MOVE WDD924-LEV-TILEVBSK-DISP                                  
082602                                 TO DAT-I-TIDATUM                         
082702           MOVE 'AAMMDD'         TO DAT-KDDATFORM                         
082802           PERFORM S01-CALL-WDATKONV                                      
082902           MOVE DAT-TIAAVVD      TO MOD-TILEVBSK-DISP (IX)                
083002                                                                          
083102*          PRE ADVISED ?                                                  
083202           MOVE WDD924-LEV-FLFORAVI                                       
083302                                 TO MOD-FLFORAVI (IX)                     
083402*          MFG SUPPLIER                                                   
083502           MOVE KFBA-IDLEVNR     TO MOD-IDLEVNR (IX)                      
083602           PERFORM IMS-GNP-WDD924                                         
083603         ELSE                                                             
083604*          ERASE AND CLOSE UNUSED ROWS                                    
083610           MOVE MFS-CLOSE-FIELD  TO MOD-KDCMD-UPD-ATTR       (IX)         
083611                                    MOD-DALEVBSK-AVS-UPD-ATTR(IX)         
083612                                    MOD-KVAVIS-UPD-ATTR      (IX)         
083613                                    MOD-TILEVBSK-INL-UPD-ATTR(IX)         
083614           MOVE MFS-ERASE-FIELD  TO MOD-KDCMD-UPD            (IX)         
083615                                    MOD-DALEVBSK-AVS         (IX)         
083616                                    MOD-KVAVIS               (IX)         
083617                                    MOD-TILEVBSK-INL         (IX)         
083618                                    MOD-TILEVBSK-DISP        (IX)         
083620                                    MOD-FLFORAVI             (IX)         
083630                                    MOD-IDLEVNR              (IX)         
085102         END-IF                                                           
085202       END-PERFORM                                                        
085204                                                                          
085402       IF SEGMENT-FOUND                                                   
085502          MOVE WDD924-LEV-DALEVBSK-AVS TO SAVE-DALEVBSK-AVS-NEXT          
085602          MOVE INF-MORE-INFO-EXISTS    TO MED-IDMFSINF                    
085702          CALL WMEDKONV             USING MED-WMEDAREA                    
085803          MOVE MED-MFSINF              TO MOD-TEMFSINF                    
085902       ELSE                                                               
085903          MOVE INF-LAST-PAGE           TO MED-IDMFSINF                    
085904          CALL WMEDKONV             USING MED-WMEDAREA                    
085906          MOVE MED-MFSINF              TO MOD-TEMFSINF                    
085907       END-IF                                                             
086002                                                                          
086003       MOVE '002'                      TO MSGI-KDCALL                     
086004       MOVE '2406'                     TO SAVE-IDTRANS                    
086005       MOVE SAVE-AREA                  TO MSGI-SPAR-AREA                  
086006       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
086010                                                                          
086102       IF MFS-QUERY AND                                                   
086202          MFS-ENTER AND                                                   
086302          MID-TEXT NOT = ALL '+'                                          
086402         CONTINUE                                                         
086502       ELSE                                                               
086602         PERFORM FA-FETCH-TEXT-ROWS                                       
086702       END-IF                                                             
086802*      CHECK IF DELIVERY PROMISE EXIST FOR ANOTHER SUPPLIER               
086902       PERFORM FB-CHECK-ANOTHER-LEVNR                                     
087002     END-IF                                                               
087102                                                                          
087202     .                                                                    
087302     EJECT                                                                
087402 FA-FETCH-TEXT-ROWS SECTION.                                              
087502                                                                          
087602     SET WS-TIBORT1-NOO          TO TRUE                                  
087702     MOVE +2                     TO W-IDLEVBSK                            
087802     PERFORM IMS-GHU-WDD925                                               
087902     IF SEGMENT-FOUND                                                     
088002       PERFORM FAA-CHECK-DATE                                             
088102       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
088202         PERFORM IMS-DLET-WDD925                                          
088302         MOVE +4                 TO W-IDLEVBSK                            
088402         PERFORM IMS-GHU-WDD925                                           
088502         IF SEGMENT-FOUND                                                 
088602           PERFORM IMS-DLET-WDD925                                        
088702         END-IF                                                           
088802       ELSE                                                               
088902         MOVE WDD925-INFO-TELEVBSK                                        
089002                                 TO MOD-TELEVBSK-TEXT1                    
089102         PERFORM FAB-CONVERT-DATE                                         
089202         SET WS-TIBORT1-YES      TO TRUE                                  
089302       END-IF                                                             
089402     ELSE                                                                 
089502       MOVE MFS-ERASE-FIELD      TO MOD-TELEVBSK-TEXT1                    
089602                                    MOD-TIBORT                            
089702     END-IF                                                               
089802                                                                          
089902     MOVE +4                     TO W-IDLEVBSK                            
090002     PERFORM IMS-GHU-WDD925                                               
090102     IF SEGMENT-FOUND                                                     
090202       PERFORM FAA-CHECK-DATE                                             
090302       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
090402         PERFORM IMS-DLET-WDD925                                          
090502       ELSE                                                               
090602         MOVE WDD925-INFO-TELEVBSK                                        
090702                                 TO MOD-TELEVBSK-TEXT2                    
090802         PERFORM FAB-CONVERT-DATE                                         
090902       END-IF                                                             
091002     ELSE                                                                 
091102       MOVE MFS-ERASE-FIELD      TO MOD-TELEVBSK-TEXT2                    
091202       IF WS-TIBORT1-NOO                                                  
091302         MOVE MFS-ERASE-FIELD    TO MOD-TIBORT                            
091402       END-IF                                                             
091502     END-IF                                                               
091602                                                                          
091702     MOVE +5                     TO W-IDLEVBSK                            
091802     PERFORM IMS-GHU-WDD925                                               
091902     IF SEGMENT-FOUND                                                     
092002       PERFORM FAA-CHECK-DATE                                             
092102       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
092202         PERFORM IMS-DLET-WDD925                                          
092302       ELSE                                                               
092402         MOVE WDD925-INFO-TELEVBSK                                        
092502                                 TO MOD-TELEVBSK-TEXT3                    
092602         PERFORM FAB-CONVERT-DATE                                         
092702       END-IF                                                             
092802     ELSE                                                                 
092902       MOVE MFS-ERASE-FIELD      TO MOD-TELEVBSK-TEXT3                    
093002       IF WS-TIBORT1-NOO                                                  
093102         MOVE MFS-ERASE-FIELD    TO MOD-TIBORT                            
093202       END-IF                                                             
093302     END-IF                                                               
093402                                                                          
093502     MOVE +6                     TO W-IDLEVBSK                            
093602     PERFORM IMS-GHU-WDD925                                               
093702     IF SEGMENT-FOUND                                                     
093802       PERFORM FAA-CHECK-DATE                                             
093902       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
094002         PERFORM IMS-DLET-WDD925                                          
094102       ELSE                                                               
094202         MOVE WDD925-INFO-TELEVBSK                                        
094302                                 TO MOD-TELEVBSK-TEXT4                    
094402         PERFORM FAB-CONVERT-DATE                                         
094502       END-IF                                                             
094602     ELSE                                                                 
094702       MOVE MFS-ERASE-FIELD      TO MOD-TELEVBSK-TEXT4                    
094802       IF WS-TIBORT1-NOO                                                  
094902         MOVE MFS-ERASE-FIELD    TO MOD-TIBORT                            
095002       END-IF                                                             
095102     END-IF                                                               
095202                                                                          
095302     .                                                                    
095402     EJECT                                                                
095502 FAA-CHECK-DATE SECTION.                                                  
095602                                                                          
095702     MOVE SYSTEM-DATE            TO TMP1-YYMMDD                           
095802     MOVE WDD925-INFO-TIBORT     TO TMP2-YYMMDD                           
095902     PERFORM WY2000P1                                                     
096002                                                                          
096102     .                                                                    
096202     EJECT                                                                
096302 FAB-CONVERT-DATE SECTION.                                                
096402                                                                          
096502     MOVE WDD925-INFO-TIBORT     TO DAT-I-TIDATUM                         
096602     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
096702     PERFORM S01-CALL-WDATKONV                                            
096802     MOVE DAT-TIAAVVD            TO MOD-TIBORT                            
096902                                                                          
097002     .                                                                    
097102     EJECT                                                                
097202 FB-CHECK-ANOTHER-LEVNR SECTION.                                          
097302                                                                          
097402     PERFORM IMS-GU-WDD901                                                
097502     PERFORM IMS-GNP-WDD902                                               
097602     IF SEGMENT-FOUND                                                     
097702       IF WDD902-IDLEVNR = W-IDLEVNR                                      
097802         PERFORM IMS-GNP-WDD902                                           
097902         IF SEGMENT-FOUND                                                 
098002           IF MOD-TEMFSFEL = SPACE                                        
098102             MOVE ERR-OTHER-SUPP-EXIST                                    
098202                                 TO MED-IDMFSFEL                          
098302             CALL WMEDKONV    USING MED-WMEDAREA                          
098402             MOVE MED-MFSFEL     TO MOD-TEMFSFEL                          
098502           END-IF                                                         
098602         END-IF                                                           
098702       ELSE                                                               
098802         IF MOD-TEMFSFEL = SPACE                                          
098902           MOVE ERR-OTHER-SUPP-EXIST                                      
099002                               TO MED-IDMFSFEL                            
099102           CALL WMEDKONV    USING MED-WMEDAREA                            
099202           MOVE MED-MFSFEL     TO MOD-TEMFSFEL                            
099302         END-IF                                                           
099402       END-IF                                                             
099502     END-IF                                                               
099602                                                                          
099702     .                                                                    
099802     EJECT                                                                
099902 G-CHECK-INPUT SECTION.                                                   
100002                                                                          
100102     MOVE YES                    TO INDATA-SW                             
100202                                                                          
100302     IF MID-UPDATE-LINE-GRP = ALL '+' AND                                 
100402        MID-INSERT-LINE     = ALL '+' AND                                 
100502        MID-TEXT            = ALL '+'                                     
100602       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
100603       CALL WMEDKONV USING MED-WMEDAREA                                   
100604       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
100703       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
100704       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
100705       MOVE NOO                  TO INDATA-SW                             
100902     ELSE                                                                 
100904       PERFORM GE-CHECK-DC-FTG-USER                                       
100905       IF INDATA-OK                                                       
101002          EVALUATE TRUE                                                   
101102            WHEN MID-UPDATE-LINE-GRP NOT = ALL '+' AND                    
101202                (MID-INSERT-LINE     NOT = ALL '+' OR                     
101302                 MID-TEXT            NOT = ALL '+')                       
101402            WHEN MID-INSERT-LINE     NOT = ALL '+' AND                    
101502                (MID-UPDATE-LINE-GRP NOT = ALL '+' OR                     
101602                 MID-TEXT            NOT = ALL '+')                       
101702              MOVE ERR-MORE-THAN-ONE-FUNCTION                             
101802                                    TO MED-IDMFSFEL                       
101902              MOVE NOO              TO INDATA-SW                          
102002              PERFORM GD-CLOSE-UNUSED-FIELDS                              
102102            WHEN MID-UPDATE-LINE-GRP NOT = ALL '+'                        
102202              PERFORM GA-CHECK-UPDATE                                     
102302            WHEN MID-INSERT-LINE     NOT = ALL '+'                        
102402              PERFORM GB-CHECK-INSERT                                     
102502            WHEN MID-TEXT            NOT = ALL '+'                        
102602              PERFORM GC-CHECK-TEXT                                       
102702          END-EVALUATE                                                    
102802        END-IF                                                            
103002                                                                          
103102        IF INDATA-WRONG                                                   
103202          PERFORM MFS-DONT-TOUCH-FIELD-IN                                 
103302          PERFORM MFS-DONT-TOUCH-FIELD-OUT                                
103402          CALL WMEDKONV          USING MED-WMEDAREA                       
103502          MOVE MED-MFSFEL           TO MOD-TEMFSFEL                       
103602          MOVE MED-MFSINF           TO MOD-TEMFSINF                       
103702        END-IF                                                            
103703     END-IF                                                               
103802     .                                                                    
103902     EJECT                                                                
104020 GA-CHECK-UPDATE SECTION.                                                 
104102                                                                          
104202     PERFORM                                                              
104302     VARYING IX FROM +1 BY +1                                             
104402       UNTIL IX > MAX-IX OR                                               
104502             INDATA-WRONG                                                 
104602       IF MID-UPDATE-LINE (IX) NOT = ALL '+'                              
104702         MOVE MID-KDCMD-UPD (IX) TO WS-KDCMD                              
104802         IF WS-KDCMD-GOOD                                                 
104902           MOVE MFS-ALPHA-FIELD-OK                                        
105002                                 TO MOD-KDCMD-UPD-ATTR (IX)               
105102         ELSE                                                             
105202           MOVE MFS-ALPHA-FIELD-WRONG                                     
105302                                 TO MOD-KDCMD-UPD-ATTR (IX)               
105402           MOVE NOO              TO INDATA-SW                             
105502           MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                
105602                                 TO MED-IDMFSFEL                          
105702         END-IF                                                           
105802         IF INDATA-OK                                                     
105902           IF MID-KDCMD-UPD (IX) = '+' OR 'D' OR ' '                      
106002             MOVE MFS-ERASE-FIELD                                         
106102                                 TO MOD-DALEVBSK-AVS-UPD (IX)             
106202                                    MOD-KVAVIS-UPD-ATTR (IX)              
106302                                    MOD-TILEVBSK-INL-UPD (IX)             
106402           END-IF                                                         
106502           IF MID-KDCMD-UPD (IX) = 'C'                                    
106602             IF MID-FLFORAVI (IX) = 'Y' OR 'J'                            
106702               MOVE MFS-ALPHA-FIELD-WRONG                                 
106802                                 TO MOD-KDCMD-UPD-ATTR (IX)               
106902               MOVE NOO          TO INDATA-SW                             
107002               MOVE ERR-PREADV-D-C-NOT-ALLOWED                            
107102                                 TO MED-IDMFSFEL                          
107202             END-IF                                                       
107302           END-IF                                                         
107402         END-IF                                                           
107502         IF INDATA-OK                                                     
107602           IF MID-KDCMD-UPD (IX) = 'C'                                    
107702             PERFORM GAA-CHECK-CHANGE-DATA                                
107802           END-IF                                                         
107902         END-IF                                                           
108002       ELSE                                                               
108102         MOVE MFS-CLOSE-FIELD  TO MOD-KDCMD-UPD-ATTR        (IX)          
108202                                  MOD-DALEVBSK-AVS-UPD-ATTR (IX)          
108302                                  MOD-KVAVIS-UPD-ATTR       (IX)          
108402                                  MOD-TILEVBSK-INL-UPD-ATTR (IX)          
108502       END-IF                                                             
108602     END-PERFORM                                                          
108702                                                                          
108802     .                                                                    
108902     EJECT                                                                
109002 GAA-CHECK-CHANGE-DATA SECTION.                                           
109102                                                                          
109202     IF MID-DALEVBSK-AVS-UPD (IX) = ALL '+' AND                           
109302        MID-KVAVIS-UPD (IX)       = ALL '+' AND                           
109402        MID-TILEVBSK-INL-UPD (IX) = ALL '+'                               
109502       MOVE MFS-ERASE-FIELD      TO MOD-DALEVBSK-AVS-UPD (IX)             
109602                                    MOD-KVAVIS-UPD-ATTR (IX)              
109702                                    MOD-TILEVBSK-INL-UPD (IX)             
109802       MOVE MFS-NUM-FIELD-WRONG  TO MOD-DALEVBSK-AVS-UPD-ATTR (IX)        
109902                                    MOD-KVAVIS-UPD-ATTR (IX)              
110002                                    MOD-TILEVBSK-INL-UPD-ATTR (IX)        
110102       MOVE NOO                  TO INDATA-SW                             
110202       MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                    
110302                                 TO MED-IDMFSFEL                          
110402     ELSE                                                                 
110502       IF MID-DALEVBSK-AVS-UPD (IX) = ALL '+'                             
110602         MOVE MFS-ERASE-FIELD    TO MOD-DALEVBSK-AVS-UPD (IX)             
110702       ELSE                                                               
110802         MOVE MID-DALEVBSK-AVS-UPD (IX)                                   
110902                                 TO WS-DALEVBSK-AVS-UPD                   
111002         INSPECT WS-DALEVBSK-AVS-UPD REPLACING LEADING SPACE              
111102                                                    BY ZERO               
111202         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
111302                                 TO MOD-DALEVBSK-AVS-UPD (IX)             
111402         IF WS-DALEVBSK-AVS-UPD NUMERIC                                   
111502           MOVE MFS-NUM-FIELD-OK TO MOD-DALEVBSK-AVS-UPD-ATTR (IX)        
111602         ELSE                                                             
111702           MOVE MFS-NUM-FIELD-WRONG                                       
111802                                 TO MOD-DALEVBSK-AVS-UPD-ATTR (IX)        
111902           MOVE NOO              TO INDATA-SW                             
112002         END-IF                                                           
112102       END-IF                                                             
112202                                                                          
112302       IF MID-KVAVIS-UPD (IX) = ALL '+'                                   
112402         MOVE MFS-ERASE-FIELD    TO MOD-KVAVIS-UPD       (IX)             
112502       ELSE                                                               
112602         MOVE MID-KVAVIS-UPD (IX)                                         
112702                                 TO WS-KVAVIS-UPD        (IX)             
112802         INSPECT WS-KVAVIS-UPD (IX) REPLACING LEADING SPACE               
112902                                                   BY ZERO                
113002         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
113102                                 TO MOD-KVAVIS-UPD       (IX)             
113202         IF WS-KVAVIS-UPD (IX) NUMERIC                                    
113302           MOVE MFS-NUM-FIELD-OK TO MOD-KVAVIS-UPD-ATTR  (IX)             
113402         ELSE                                                             
113502           MOVE MFS-NUM-FIELD-WRONG                                       
113602                                 TO MOD-KVAVIS-UPD-ATTR  (IX)             
113702           MOVE NOO              TO INDATA-SW                             
113802         END-IF                                                           
113902       END-IF                                                             
114002                                                                          
114102       IF MID-TILEVBSK-INL-UPD (IX) = ALL '+'                             
114202         MOVE MFS-ERASE-FIELD    TO MOD-TILEVBSK-INL-UPD (IX)             
114302       ELSE                                                               
114402         MOVE MID-TILEVBSK-INL-UPD (IX)                                   
114502                                 TO WS-TILEVBSK-INL-UPD                   
114602         INSPECT WS-TILEVBSK-INL-UPD REPLACING LEADING SPACE              
114702                                                    BY ZERO               
114802         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
114902                                 TO MOD-TILEVBSK-INL-UPD (IX)             
115002         IF WS-TILEVBSK-INL-UPD NUMERIC                                   
115102           MOVE MFS-NUM-FIELD-OK TO MOD-TILEVBSK-INL-UPD-ATTR (IX)        
115202         ELSE                                                             
115302           MOVE MFS-NUM-FIELD-WRONG                                       
115402                                 TO MOD-TILEVBSK-INL-UPD-ATTR (IX)        
115502           MOVE NOO              TO INDATA-SW                             
115602         END-IF                                                           
115702       END-IF                                                             
115802                                                                          
115902       IF INDATA-WRONG                                                    
116002         MOVE ERR-FIELDS-ARE-NOT-NUMERIC                                  
116102                                 TO MED-IDMFSFEL                          
116202       END-IF                                                             
116302                                                                          
116402       IF INDATA-OK                                                       
116502         IF MID-DALEVBSK-AVS-UPD (IX) NOT = ALL '+'                       
116602           MOVE WS-DALEVBSK-AVS-UPD                                       
116702                                 TO DAT-I-TIDATUM                         
116802           MOVE 'AAVVD'          TO DAT-KDDATFORM                         
116902           PERFORM S01-CALL-WDATKONV                                      
117002           IF DAT-KDSVAR-OK                                               
117102             MOVE DAT-TIAAMMDD   TO TMP1-YYMMDD                           
117202             PERFORM GAAA-CHECK-MIN-MAX                                   
117302             IF MIN-MAX-OK                                                
117402               MOVE DAT-TIAAMMDD TO WS-DALEVBSK-AVS-AAMMDD (IX)           
117502               IF DAT-TIAAMMDD > 500000                                   
117602                 MOVE 19         TO WS-DALEVBSK-AVS-SS (IX)               
117702               ELSE                                                       
117802                 MOVE 20         TO WS-DALEVBSK-AVS-SS (IX)               
117902               END-IF                                                     
118002             ELSE                                                         
118102               MOVE MFS-NUM-FIELD-WRONG                                   
118202                                 TO MOD-DALEVBSK-AVS-UPD-ATTR (IX)        
118302               MOVE ERR-HIGHLIGHT-FIELDS-WRONG                            
118402                                 TO MED-IDMFSFEL                          
118502               MOVE NOO          TO INDATA-SW                             
118602             END-IF                                                       
118702           ELSE                                                           
118802             MOVE MFS-NUM-FIELD-WRONG                                     
118902                                 TO MOD-DALEVBSK-AVS-UPD-ATTR (IX)        
119002             MOVE ERR-HIGHLIGHT-FIELDS-WRONG                              
119102                                 TO MED-IDMFSFEL                          
119202             MOVE NOO            TO INDATA-SW                             
119302           END-IF                                                         
119402         ELSE                                                             
119502           MOVE ZERO             TO WS-DALEVBSK-AVS-AAAAMMDD (IX)         
119602         END-IF                                                           
119702                                                                          
119802         IF MID-KVAVIS-UPD (IX) NOT = ALL '+'                             
119902           IF WS-KVAVIS-UPD (IX) > ZERO                                   
120002             MOVE MFS-NUM-FIELD-OK                                        
120102                                 TO MOD-KVAVIS-UPD-ATTR (IX)              
120202           ELSE                                                           
120302             MOVE MFS-NUM-FIELD-WRONG                                     
120402                                 TO MOD-KVAVIS-UPD-ATTR (IX)              
120502             MOVE ERR-HIGHLIGHT-FIELDS-WRONG                              
120602                                 TO MED-IDMFSFEL                          
120702             MOVE NOO            TO INDATA-SW                             
120802           END-IF                                                         
120902         END-IF                                                           
121002                                                                          
121102         IF MID-TILEVBSK-INL-UPD (IX) NOT = ALL '+'                       
121202           MOVE WS-TILEVBSK-INL-UPD                                       
121302                                 TO DAT-I-TIDATUM                         
121402           MOVE 'AAVVD'          TO DAT-KDDATFORM                         
121502           PERFORM S01-CALL-WDATKONV                                      
121602           IF DAT-KDSVAR-OK                                               
121702             MOVE DAT-TIAAMMDD   TO TMP1-YYMMDD                           
121802             PERFORM GAAA-CHECK-MIN-MAX                                   
121902             IF MIN-MAX-OK                                                
122002               MOVE DAT-TIAAMMDD TO WS-TILEVBSK-INL-AAMMDD (IX)           
122102             ELSE                                                         
122202               MOVE MFS-NUM-FIELD-WRONG                                   
122302                                 TO MOD-TILEVBSK-INL-UPD-ATTR (IX)        
122402               MOVE ERR-HIGHLIGHT-FIELDS-WRONG                            
122502                                 TO MED-IDMFSFEL                          
122602               MOVE NOO          TO INDATA-SW                             
122702             END-IF                                                       
122802           ELSE                                                           
122902             MOVE MFS-NUM-FIELD-WRONG                                     
123002                                 TO MOD-TILEVBSK-INL-UPD-ATTR (IX)        
123102             MOVE ERR-HIGHLIGHT-FIELDS-WRONG                              
123202                                 TO MED-IDMFSFEL                          
123302             MOVE NOO            TO INDATA-SW                             
123402           END-IF                                                         
123502         ELSE                                                             
123602           MOVE ZERO             TO WS-TILEVBSK-INL-AAMMDD (IX)           
123702         END-IF                                                           
123802       END-IF                                                             
123902                                                                          
124002       IF INDATA-OK                                                       
124102         IF MID-TILEVBSK-INL-UPD (IX) NOT = ALL '+'                       
124202           MOVE MID-TILEVBSK-INL-UPD (IX)                                 
124302                                 TO TMP1-YYWWD                            
124402           IF MID-DALEVBSK-AVS-UPD (IX) = ALL '+'                         
124502             MOVE MID-DALEVBSK-AVS (IX)                                   
124602                                 TO TMP2-YYWWD                            
124702           ELSE                                                           
124802             MOVE MID-DALEVBSK-AVS-UPD (IX)                               
124902                                 TO TMP2-YYWWD                            
125002           END-IF                                                         
125102           PERFORM WY2000P2                                               
125202           IF TMP1-YYWWD > TMP2-YYWWD                                     
125302             CONTINUE                                                     
125402           ELSE                                                           
125502             MOVE MFS-NUM-FIELD-WRONG                                     
125602                                 TO MOD-TILEVBSK-INL-UPD-ATTR (IX)        
125702             MOVE ERR-HIGHLIGHT-FIELDS-WRONG                              
125802                                 TO MED-IDMFSFEL                          
125902             MOVE NOO            TO INDATA-SW                             
126002           END-IF                                                         
126102         END-IF                                                           
126202       END-IF                                                             
126302                                                                          
126402       IF INDATA-OK                                                       
126502         IF MID-DALEVBSK-AVS-UPD (IX) NOT = ALL '+' AND                   
126602            MID-TILEVBSK-INL-UPD (IX) = ALL '+'                           
126702           MOVE MID-IDLEVNR (IX) TO W-IDLEVNR                             
126802           PERFORM IMS-GU-WDK711                                          
126902           IF SEGMENT-FOUND                                               
127002             IF WDK711-SLAG-IDLEVNR = MID-IDLEVNR (IX)                    
127102               PERFORM IMS-GNP-WDK722                                     
127202               IF SEGMENT-FOUND                                           
127302                 MOVE WDK722-XLAG-IDLEVNR-SHIP                            
127402                                 TO W-IDLEVNR                             
127502               END-IF                                                     
127602             ELSE                                                         
127702               PERFORM IMS-GNP-WDK723                                     
127802               MOVE NOO          TO WS-723-SW                             
127902               PERFORM                                                    
128002                 UNTIL SEGMENT-MISSING OR WS-723-YES                      
128102                 IF MID-IDLEVNR (IX) = WDK723-SAVT-IDLEVNR-AVT            
128202                   MOVE WDK723-SAVT-IDLEVNR-SHIP                          
128302                                 TO W-IDLEVNR                             
128402                   MOVE YES      TO WS-723-SW                             
128502                 END-IF                                                   
128602                 PERFORM IMS-GNP-WDK723                                   
128702               END-PERFORM                                                
128802             END-IF                                                       
128902           END-IF                                                         
129002           PERFORM IMS-GU-WDF116                                          
129102           IF SEGMENT-FOUND                                               
129202             CONTINUE                                                     
129302           ELSE                                                           
129402             MOVE ERR-SUPP-NOT-UPD-FOR-DC                                 
129502                                 TO MED-IDMFSFEL                          
129602             CALL WMEDKONV    USING MED-WMEDAREA                          
129702             MOVE MED-MFSFEL     TO MOD-TEMFSFEL                          
129802             MOVE ZERO           TO WDF116-NDC-KVDAGAR-TT                 
129902           END-IF                                                         
130002           COMPUTE WS-NDC-KVDAGAR-TT-UPD (IX)                             
130102                                  = WDF116-NDC-KVDAGAR-TT + 1             
130202         END-IF                                                           
130302       END-IF                                                             
130402                                                                          
130502       IF INDATA-OK                                                       
130602         IF MID-DALEVBSK-AVS-UPD (IX) NOT = ALL '+'                       
130702           MOVE WS-DALEVBSK-AVS-AAAAMMDD (IX)                             
130802                                 TO W-DALEVBSK                            
130902           MOVE MID-IDLEVNR (IX) TO W-IDLEVNR                             
131002           PERFORM IMS-GHU-WDD924                                         
131102           IF SEGMENT-FOUND                                               
131202             MOVE MFS-NUM-FIELD-WRONG                                     
131302                                 TO MOD-DALEVBSK-AVS-UPD-ATTR (IX)        
131402             MOVE ERR-CHANGE-DISPDAY-EXIST                                
131502                                 TO MED-IDMFSFEL                          
131602             MOVE NOO            TO INDATA-SW                             
131702           END-IF                                                         
131802         END-IF                                                           
131902       END-IF                                                             
132002     END-IF                                                               
132102                                                                          
132202     .                                                                    
132302     EJECT                                                                
132402 GAAA-CHECK-MIN-MAX SECTION.                                              
132502                                                                          
132602     SET MIN-MAX-OK              TO TRUE                                  
132702     MOVE 3                      TO WORK-KDCALL                           
132802     MOVE W-IDDC                 TO WORK-IDDC                             
132902     MOVE SYSTEM-DATE            TO WORK-TIAAMMDD-TOM                     
133002     MOVE 25                     TO WORK-KVWORKD                          
133102     PERFORM S02-CALL-WORKDAY                                             
133202                                                                          
133302     IF WORK-KDSVAR-OK                                                    
133402       MOVE WORK-TIAAMMDD-FOM    TO TMP2-YYMMDD                           
133502       PERFORM WY2000P1                                                   
133602       IF TMP1-YYMMDD < TMP2-YYMMDD                                       
133702         SET MIN-MAX-WRONG       TO TRUE                                  
133802       END-IF                                                             
133902     ELSE                                                                 
134002       SET MIN-MAX-WRONG         TO TRUE                                  
134102     END-IF                                                               
134202                                                                          
134302     MOVE SYSTEM-DATE            TO TMP2-YYMMDD                           
134402     ADD 30000                   TO TMP2-YYMMDD                           
134502     PERFORM WY2000P1                                                     
134602     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
134702       SET MIN-MAX-WRONG         TO TRUE                                  
134802     END-IF                                                               
134902     .                                                                    
135002     EJECT                                                                
135102 GB-CHECK-INSERT SECTION.                                                 
135202                                                                          
135302     IF MID-DALEVBSK-AVS-NY = ALL '+'                                     
135402       MOVE MFS-ERASE-FIELD      TO MOD-DALEVBSK-AVS-NY                   
135502       MOVE MFS-NUM-FIELD-WRONG  TO MOD-DALEVBSK-AVS-NY-ATTR              
135602       MOVE NOO                  TO INDATA-SW                             
135702       MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                    
135802                                 TO MED-IDMFSFEL                          
135902     ELSE                                                                 
136002       MOVE MID-DALEVBSK-AVS-NY  TO WS-DALEVBSK-AVS-NY                    
136102                                                                          
136202       INSPECT WS-DALEVBSK-AVS-NY REPLACING LEADING SPACE                 
136302                                                 BY ZERO                  
136402       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
136502                                 TO MOD-DALEVBSK-AVS-NY                   
136602       IF WS-DALEVBSK-AVS-NY NUMERIC                                      
136702         MOVE MFS-NUM-FIELD-OK   TO MOD-DALEVBSK-AVS-NY-ATTR              
136802       ELSE                                                               
136902         MOVE MFS-NUM-FIELD-WRONG                                         
137002                                 TO MOD-DALEVBSK-AVS-NY-ATTR              
137102         MOVE NOO                TO INDATA-SW                             
137202         MOVE ERR-FIELDS-ARE-NOT-NUMERIC                                  
137302                                 TO MED-IDMFSFEL                          
137402       END-IF                                                             
137502     END-IF                                                               
137602                                                                          
137702     IF MID-KVAVIS-NY = ALL '+'                                           
137802       MOVE MFS-ERASE-FIELD      TO MOD-KVAVIS-NY                         
137902       MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVAVIS-NY-ATTR                    
138002       MOVE NOO                  TO INDATA-SW                             
138102       MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                    
138202                                 TO MED-IDMFSFEL                          
138302     ELSE                                                                 
138402       MOVE MID-KVAVIS-NY        TO WS-KVAVIS-NY                          
138502       INSPECT WS-KVAVIS-NY REPLACING LEADING SPACE BY ZERO               
138602       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
138702                                 TO MOD-KVAVIS-NY                         
138802       IF WS-KVAVIS-NY NUMERIC                                            
138902         IF WS-KVAVIS-NY > ZERO                                           
139002           MOVE MFS-NUM-FIELD-OK TO MOD-KVAVIS-NY-ATTR                    
139102         ELSE                                                             
139202           MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                
139302                                 TO MOD-KVAVIS-NY-ATTR                    
139402           MOVE NOO              TO INDATA-SW                             
139502           MOVE ERR-FIELDS-ARE-NOT-NUMERIC                                
139602                                 TO MED-IDMFSFEL                          
139702         END-IF                                                           
139802       ELSE                                                               
139902         MOVE MFS-NUM-FIELD-WRONG                                         
140002                                 TO MOD-KVAVIS-NY-ATTR                    
140102         MOVE NOO                TO INDATA-SW                             
140202         MOVE ERR-FIELDS-ARE-NOT-NUMERIC                                  
140302                                 TO MED-IDMFSFEL                          
140402       END-IF                                                             
140502     END-IF                                                               
140602                                                                          
140702     IF MID-TILEVBSK-INL-NY = ALL '+'                                     
140802       MOVE MFS-ERASE-FIELD      TO MOD-TILEVBSK-INL-NY                   
140902     ELSE                                                                 
141002       MOVE MID-TILEVBSK-INL-NY  TO WS-TILEVBSK-INL-NY                    
141102       INSPECT WS-TILEVBSK-INL-NY REPLACING LEADING SPACE                 
141202                                                 BY ZERO                  
141302       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
141402                                 TO MOD-TILEVBSK-INL-NY                   
141502       IF WS-TILEVBSK-INL-NY NUMERIC                                      
141602         MOVE MFS-NUM-FIELD-OK   TO MOD-TILEVBSK-INL-NY-ATTR              
141702       ELSE                                                               
141802         MOVE MFS-NUM-FIELD-WRONG                                         
141902                                 TO MOD-TILEVBSK-INL-NY-ATTR              
142002         MOVE NOO                TO INDATA-SW                             
142102         MOVE ERR-FIELDS-ARE-NOT-NUMERIC                                  
142202                                 TO MED-IDMFSFEL                          
142302       END-IF                                                             
142402     END-IF                                                               
142502                                                                          
142602     IF INDATA-OK                                                         
142702       MOVE WS-DALEVBSK-AVS-NY   TO DAT-I-TIDATUM                         
142802       MOVE 'AAVVD'              TO DAT-KDDATFORM                         
142902       PERFORM S01-CALL-WDATKONV                                          
143002       IF DAT-KDSVAR-OK                                                   
143102         MOVE DAT-TIAAMMDD       TO TMP1-YYMMDD                           
143202         PERFORM GAAA-CHECK-MIN-MAX                                       
143302         IF MIN-MAX-OK                                                    
143402           MOVE DAT-TIAAMMDD     TO WS-DALEVBSK-AVS-NY-AAMMDD             
143502           IF DAT-TIAAMMDD > 500000                                       
143602             MOVE 19             TO WS-DALEVBSK-AVS-NY-SS                 
143702           ELSE                                                           
143802             MOVE 20             TO WS-DALEVBSK-AVS-NY-SS                 
143902           END-IF                                                         
144002         ELSE                                                             
144102           MOVE MFS-NUM-FIELD-WRONG                                       
144202                                 TO MOD-DALEVBSK-AVS-NY-ATTR              
144302           MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                
144402                                 TO MED-IDMFSFEL                          
144502           MOVE NOO              TO INDATA-SW                             
144602         END-IF                                                           
144702       ELSE                                                               
144802         MOVE MFS-NUM-FIELD-WRONG                                         
144902                                 TO MOD-DALEVBSK-AVS-NY-ATTR              
145002         MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                  
145102                                 TO MED-IDMFSFEL                          
145202         MOVE NOO                TO INDATA-SW                             
145302       END-IF                                                             
145402                                                                          
145502       IF MID-TILEVBSK-INL-NY NOT = ALL '+'                               
145602         MOVE WS-TILEVBSK-INL-NY TO DAT-I-TIDATUM                         
145702         MOVE 'AAVVD'            TO DAT-KDDATFORM                         
145802         PERFORM S01-CALL-WDATKONV                                        
145902         IF DAT-KDSVAR-OK                                                 
146002           MOVE DAT-TIAAMMDD     TO TMP1-YYMMDD                           
146102           PERFORM GAAA-CHECK-MIN-MAX                                     
146202           IF MIN-MAX-OK                                                  
146302             MOVE DAT-TIAAMMDD   TO WS-TILEVBSK-INL-NY-AAMMDD             
146402           ELSE                                                           
146502             MOVE MFS-NUM-FIELD-WRONG                                     
146602                                 TO MOD-TILEVBSK-INL-NY-ATTR              
146702             MOVE ERR-HIGHLIGHT-FIELDS-WRONG                              
146802                                 TO MED-IDMFSFEL                          
146902             MOVE NOO            TO INDATA-SW                             
147002           END-IF                                                         
147102         ELSE                                                             
147202           MOVE MFS-NUM-FIELD-WRONG                                       
147302                                 TO MOD-TILEVBSK-INL-NY-ATTR              
147402           MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                
147502                                 TO MED-IDMFSFEL                          
147602           MOVE NOO              TO INDATA-SW                             
147702         END-IF                                                           
147802       ELSE                                                               
147902         MOVE ZERO               TO WS-TILEVBSK-INL-NY-AAMMDD             
148002       END-IF                                                             
148102                                                                          
148202       IF MID-IDLEVNR-NY = ALL '+'                                        
148302         PERFORM IMS-GU-WDK701                                            
148402         IF SEGMENT-MISSING                                               
148502           MOVE ERR-PART-MISSING TO MED-IDMFSFEL                          
148602           MOVE NOO              TO INDATA-SW                             
148702         ELSE                                                             
148802           PERFORM IMS-GU-WDK711                                          
148902           IF SEGMENT-FOUND AND                                           
149002              WDK711-SLAG-IDLEVNR > SPACE                                 
149102             MOVE WDK711-SLAG-IDLEVNR                                     
149202                                 TO WS-IDLEVNR-NY                         
149302                                    W-IDLEVNR                             
149402             PERFORM IMS-GU-WDF101                                        
149502             IF SEGMENT-MISSING                                           
149602               MOVE ERR-SUPP-IS-MISSING                                   
149702                                 TO MED-IDMFSFEL                          
149802               MOVE NOO          TO INDATA-SW                             
149902             END-IF                                                       
150002             IF MID-IDLEVNR-SHIP-NY = ALL '+'                             
150102               PERFORM IMS-GNP-WDK722                                     
150202               IF SEGMENT-FOUND                                           
150302                 IF WDK722-XLAG-IDLEVNR-SHIP = SPACE                      
150402                   MOVE WDK711-SLAG-IDLEVNR                               
150502                                 TO WS-IDLEVNR-SHIP-NY                    
150602                                    W-IDLEVNR                             
150702                 ELSE                                                     
150802                   MOVE WDK722-XLAG-IDLEVNR-SHIP                          
150902                                 TO WS-IDLEVNR-SHIP-NY                    
151002                                    W-IDLEVNR                             
151102                 END-IF                                                   
151202               ELSE                                                       
151302                 MOVE WDK711-SLAG-IDLEVNR                                 
151402                                 TO WS-IDLEVNR-SHIP-NY                    
151502                                    W-IDLEVNR                             
151602               END-IF                                                     
151702             ELSE                                                         
151802               MOVE MID-IDLEVNR-SHIP-NY                                   
151902                                 TO WS-IDLEVNR-SHIP-NY                    
152002                                    W-IDLEVNR                             
152102             END-IF                                                       
152202             PERFORM IMS-GU-WDF101                                        
152302             IF SEGMENT-MISSING                                           
152402               MOVE ERR-SUPP-IS-MISSING                                   
152502                                 TO MED-IDMFSFEL                          
152602               MOVE NOO          TO INDATA-SW                             
152702             END-IF                                                       
152802           ELSE                                                           
152902             MOVE ERR-SUPP-IS-MISSING                                     
153002                                 TO MED-IDMFSFEL                          
153102             MOVE NOO            TO INDATA-SW                             
153202           END-IF                                                         
153302         END-IF                                                           
153402       ELSE                                                               
153502         MOVE MID-IDLEVNR-NY     TO WS-IDLEVNR-NY                         
153602                                    W-IDLEVNR                             
153702         PERFORM IMS-GU-WDF101                                            
153802         IF SEGMENT-MISSING                                               
153902           MOVE ERR-SUPP-IS-MISSING                                       
154002                                 TO MED-IDMFSFEL                          
154102           MOVE NOO              TO INDATA-SW                             
154202         END-IF                                                           
154302         IF MID-IDLEVNR-SHIP-NY NOT = ALL '+'                             
154402           MOVE MID-IDLEVNR-SHIP-NY                                       
154502                                 TO WS-IDLEVNR-SHIP-NY                    
154602                                    W-IDLEVNR                             
154702           PERFORM IMS-GU-WDF101                                          
154802           IF SEGMENT-MISSING                                             
154902             MOVE ERR-SUPP-IS-MISSING                                     
155002                                 TO MED-IDMFSFEL                          
155102             MOVE NOO            TO INDATA-SW                             
155202           END-IF                                                         
155302         ELSE                                                             
155402           PERFORM IMS-GU-WDK701                                          
155502           IF SEGMENT-MISSING                                             
155602             MOVE ERR-PART-MISSING                                        
155702                                 TO MED-IDMFSFEL                          
155802             MOVE NOO            TO INDATA-SW                             
155902           ELSE                                                           
156002             PERFORM IMS-GU-WDK711                                        
156102             IF SEGMENT-MISSING                                           
156202               MOVE ERR-SUPP-IS-MISSING                                   
156302                                 TO MED-IDMFSFEL                          
156402               MOVE NOO          TO INDATA-SW                             
156502             ELSE                                                         
156602               IF MID-IDLEVNR-NY = WDK711-SLAG-IDLEVNR                    
156702                 PERFORM IMS-GNP-WDK722                                   
156802                 IF SEGMENT-FOUND                                         
156902                   MOVE WDK722-XLAG-IDLEVNR-SHIP                          
157002                                 TO WS-IDLEVNR-SHIP-NY                    
157102                 ELSE                                                     
157202                   MOVE MID-IDLEVNR-NY                                    
157302                                 TO WS-IDLEVNR-SHIP-NY                    
157402                 END-IF                                                   
157502               ELSE                                                       
157602                 MOVE MID-IDLEVNR-NY                                      
157702                                 TO WS-IDLEVNR-SHIP-NY                    
157802                 PERFORM IMS-GNP-WDK723                                   
157902                 MOVE NOO        TO WS-723-SW                             
158002                 PERFORM                                                  
158102                   UNTIL SEGMENT-MISSING OR WS-723-YES                    
158202                   IF MID-IDLEVNR-NY = WDK723-SAVT-IDLEVNR-AVT            
158302                     MOVE WDK723-SAVT-IDLEVNR-SHIP                        
158402                                 TO WS-IDLEVNR-SHIP-NY                    
158502                     MOVE YES    TO WS-723-SW                             
158602                   END-IF                                                 
158702                   PERFORM IMS-GNP-WDK723                                 
158802                 END-PERFORM                                              
158902               END-IF                                                     
159002             END-IF                                                       
159102           END-IF                                                         
159202         END-IF                                                           
159302       END-IF                                                             
159402                                                                          
159502     END-IF                                                               
159602                                                                          
159702     IF INDATA-OK                                                         
159802       IF MID-TILEVBSK-INL-NY NOT = ALL '+'                               
159902         MOVE MID-TILEVBSK-INL-NY                                         
160002                                 TO TMP1-YYWWD                            
160102         MOVE MID-DALEVBSK-AVS-NY                                         
160202                                 TO TMP2-YYWWD                            
160302         PERFORM WY2000P2                                                 
160402         IF TMP1-YYWWD > TMP2-YYWWD                                       
160502           CONTINUE                                                       
160602         ELSE                                                             
160702           MOVE MFS-NUM-FIELD-WRONG                                       
160802                                 TO MOD-TILEVBSK-INL-NY-ATTR              
160902           MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                
161002                                 TO MED-IDMFSFEL                          
161102           MOVE NOO              TO INDATA-SW                             
161202         END-IF                                                           
161302       ELSE                                                               
161402         MOVE WS-IDLEVNR-SHIP-NY TO W-IDLEVNR                             
161502         PERFORM IMS-GU-WDF116                                            
161602         IF SEGMENT-FOUND                                                 
161702           CONTINUE                                                       
161802         ELSE                                                             
161902           MOVE ERR-SUPP-NOT-UPD-FOR-DC                                   
162002                                 TO MED-IDMFSFEL                          
162102           CALL WMEDKONV      USING MED-WMEDAREA                          
162202           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
162302           MOVE ZERO             TO WDF116-NDC-KVDAGAR-TT                 
162402         END-IF                                                           
162502         COMPUTE WS-NDC-KVDAGAR-TT-NY                                     
162602                                  = WDF116-NDC-KVDAGAR-TT + 1             
162702       END-IF                                                             
162802     END-IF                                                               
162902                                                                          
163002     IF INDATA-OK                                                         
163102       MOVE WS-IDLEVNR-NY        TO W-IDLEVNR                             
163202       MOVE WS-DALEVBSK-AVS-NY-AAAAMMDD                                   
163302                                 TO W-DALEVBSK                            
163402       PERFORM IMS-GHU-WDD924                                             
163502       IF SEGMENT-FOUND                                                   
163602         MOVE MFS-NUM-FIELD-WRONG                                         
163702                                 TO MOD-DALEVBSK-AVS-NY-ATTR              
163802         MOVE ERR-DELIVERY-PROMISE-EXIST                                  
163902                                 TO MED-IDMFSFEL                          
164002         MOVE NOO                TO INDATA-SW                             
164102       END-IF                                                             
164202     END-IF                                                               
164302                                                                          
164402     IF INDATA-OK                                                         
164502       SET WS-INSERT-YES         TO TRUE                                  
164602     ELSE                                                                 
164702       SET WS-INSERT-NO          TO TRUE                                  
164802     END-IF                                                               
164902     .                                                                    
165002     EJECT                                                                
165102 GC-CHECK-TEXT SECTION.                                                   
165202                                                                          
165302     IF MID-TIBORT = ALL '+' AND                                          
165402        (MID-TELEVBSK-TEXT1 NOT = ALL '+' OR                              
165502         MID-TELEVBSK-TEXT2 NOT = ALL '+' OR                              
165602         MID-TELEVBSK-TEXT3 NOT = ALL '+' OR                              
165702         MID-TELEVBSK-TEXT4 NOT = ALL '+')                                
165802       MOVE NOO                  TO INDATA-SW                             
165902       MOVE MFS-NUM-FIELD-WRONG  TO MOD-TIBORT-ATTR                       
166002                                                                          
166102       MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                    
166202                                 TO MED-IDMFSFEL                          
166302       MOVE ERR-DATE-SUPP-INFO-OK-CHECK                                   
166402                                 TO MED-IDMFSINF                          
166502       IF MID-TELEVBSK-TEXT1 NOT = ALL '+'                                
166602         MOVE MFS-ADD-READ-FIELD TO MOD-TELEVBSK-TEXT1-ATTR               
166702       END-IF                                                             
166802       IF MID-TELEVBSK-TEXT2 NOT = ALL '+'                                
166902         MOVE MFS-ADD-READ-FIELD TO MOD-TELEVBSK-TEXT2-ATTR               
167002       END-IF                                                             
167102       IF MID-TELEVBSK-TEXT3 NOT = ALL '+'                                
167202         MOVE MFS-ADD-READ-FIELD TO MOD-TELEVBSK-TEXT3-ATTR               
167302       END-IF                                                             
167402       IF MID-TELEVBSK-TEXT4 NOT = ALL '+'                                
167502         MOVE MFS-ADD-READ-FIELD TO MOD-TELEVBSK-TEXT4-ATTR               
167602       END-IF                                                             
167702     ELSE                                                                 
167802       MOVE MID-TIBORT           TO DAT-I-TIDATUM                         
167902       MOVE 'AAVVD'              TO DAT-KDDATFORM                         
168002       PERFORM S01-CALL-WDATKONV                                          
168102       IF DAT-KDSVAR-FEL                                                  
168202         MOVE NOO                TO INDATA-SW                             
168302         MOVE MFS-NUM-FIELD-WRONG                                         
168402                                 TO MOD-TIBORT-ATTR                       
168502         MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                  
168602                                 TO MED-IDMFSFEL                          
168702       ELSE                                                               
168802         MOVE DAT-TIAAMMDD       TO TMP1-YYMMDD                           
168902         MOVE SYSTEM-DATE        TO TMP2-YYMMDD                           
169002         PERFORM WY2000P1                                                 
169102         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
169202           MOVE NOO              TO INDATA-SW                             
169302           MOVE MFS-NUM-FIELD-WRONG                                       
169402                                 TO MOD-TIBORT-ATTR                       
169502           MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                
169602                                 TO MED-IDMFSFEL                          
169702         END-IF                                                           
169802       END-IF                                                             
169902     END-IF                                                               
170002                                                                          
170102     .                                                                    
170202     EJECT                                                                
170302 GD-CLOSE-UNUSED-FIELDS SECTION.                                          
170402                                                                          
170502     PERFORM                                                              
170602     VARYING IX FROM +1 BY +1                                             
170702       UNTIL IX > MAX-IX                                                  
170802                                                                          
170902       IF MID-UPDATE-LINE (IX) = ALL '+'                                  
171002         MOVE MFS-CLOSE-FIELD    TO MOD-KDCMD-UPD-ATTR (IX)               
171102                                    MOD-DALEVBSK-AVS-UPD-ATTR (IX)        
171202                                    MOD-KVAVIS-UPD-ATTR (IX)              
171302                                    MOD-TILEVBSK-INL-UPD-ATTR (IX)        
171402       ELSE                                                               
171502         MOVE MFS-OPEN-ALPHA-FIELD                                        
171602                                 TO MOD-KDCMD-UPD-ATTR (IX)               
171702         MOVE MFS-OPEN-NUM-FIELD TO MOD-DALEVBSK-AVS-UPD-ATTR (IX)        
171802                                    MOD-KVAVIS-UPD-ATTR (IX)              
171902                                    MOD-TILEVBSK-INL-UPD-ATTR (IX)        
172002       END-IF                                                             
172102                                                                          
172202     END-PERFORM                                                          
172302                                                                          
172402     MOVE MFS-OPEN-NUM-FIELD     TO MOD-DALEVBSK-AVS-NY-ATTR              
172502                                    MOD-KVAVIS-NY-ATTR                    
172602                                    MOD-TILEVBSK-INL-NY-ATTR              
172702     MOVE MFS-OPEN-ALPHA-FIELD   TO MOD-IDLEVNR-NY-ATTR                   
172802                                    MOD-IDLEVNR-SHIP-NY-ATTR              
172902                                                                          
173002     IF MID-TIBORT NOT = ALL '+'                                          
173102       MOVE MFS-OPEN-NUM-FIELD   TO MOD-TIBORT-ATTR                       
173202     END-IF                                                               
173302                                                                          
173402     IF MID-TELEVBSK-TEXT1 NOT = ALL '+'                                  
173502       MOVE MFS-OPEN-ALPHA-FIELD TO MOD-TELEVBSK-TEXT1-ATTR               
173602     END-IF                                                               
173702     IF MID-TELEVBSK-TEXT2 NOT = ALL '+'                                  
173802       MOVE MFS-OPEN-ALPHA-FIELD TO MOD-TELEVBSK-TEXT2-ATTR               
173902     END-IF                                                               
174002     IF MID-TELEVBSK-TEXT3 NOT = ALL '+'                                  
174102       MOVE MFS-OPEN-ALPHA-FIELD TO MOD-TELEVBSK-TEXT3-ATTR               
174202     END-IF                                                               
174302     IF MID-TELEVBSK-TEXT4 NOT = ALL '+'                                  
174402       MOVE MFS-OPEN-ALPHA-FIELD TO MOD-TELEVBSK-TEXT4-ATTR               
174502     END-IF                                                               
174602     .                                                                    
174702     EJECT                                                                
174703 GE-CHECK-DC-FTG-USER SECTION.                                            
174705                                                                          
174706     MOVE MSGI-IDDC-KEY               TO W-IDDC                           
174707     PERFORM IMS-GU-WDB601                                                
174708     IF SEGMENT-FOUND                                                     
174720        IF WDB601-DCS-NDC-CN                                              
174730        OR (WDB601-DCS-NDC-NA AND WDB601-DCS-USA)                         
174740           MOVE MSGI-IDFTG            TO WS-IDFTG                         
174750           IF (WDB601-DCS-NDC-CN AND IDFTG-CN)                            
174751           OR (WDB601-DCS-NDC-NA AND IDFTG-US)                            
174752           OR MSGI-IDFTG  = WC-IDFTG-PV                                   
174760              CONTINUE                                                    
174780           ELSE                                                           
174792              MOVE NOO                TO INDATA-SW                        
174802           END-IF                                                         
174803        ELSE                                                              
174806           MOVE NOO                   TO INDATA-SW                        
174809        END-IF                                                            
174810     ELSE                                                                 
174811        MOVE NOO                      TO INDATA-SW                        
174812     END-IF                                                               
174813                                                                          
174814     IF INDATA-WRONG                                                      
174815       PERFORM MFS-ERASE-FIELD-IN                                         
174816       MOVE ERR-USER-NOT-AUTHORIZED  TO MED-IDMFSFEL                      
174817       CALL WMEDKONV              USING MED-WMEDAREA                      
174818       MOVE MED-MFSFEL               TO MOD-TEMFSFEL                      
174819     END-IF                                                               
174820                                                                          
174821     .                                                                    
174822     EJECT                                                                
174830 H-UPDATE SECTION.                                                        
174902                                                                          
175002     PERFORM                                                              
175102     VARYING IX FROM +1 BY +1                                             
175202       UNTIL IX > MAX-IX                                                  
175302       MOVE MID-KDCMD-UPD (IX)   TO WS-KDCMD                              
175402       IF WS-KDCMD-DELETE                                                 
175502         PERFORM HA-DELETE                                                
175602       END-IF                                                             
175702       IF WS-KDCMD-CHANGE                                                 
175802         PERFORM HB-CHANGE                                                
175902       END-IF                                                             
176002     END-PERFORM                                                          
176102                                                                          
176202     IF WS-INSERT-YES                                                     
176302       PERFORM HC-INSERT                                                  
176402     END-IF                                                               
176502     IF MID-TEXT NOT = ALL '+'                                            
176602       PERFORM HD-TEXT-UPDATE                                             
176702     END-IF                                                               
176802                                                                          
176803     PERFORM MFS-FORM-ATTR                                                
176902     PERFORM MFS-ERASE-FIELD-IN                                           
177002     MOVE INF-UPDATE-DONE        TO MED-IDMFSINF                          
177102     CALL WMEDKONV            USING MED-WMEDAREA                          
177202     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
177302                                                                          
177402     .                                                                    
177502     EJECT                                                                
177602 HA-DELETE SECTION.                                                       
177702                                                                          
177802     MOVE MID-IDLEVNR (IX)       TO W-IDLEVNR                             
177902     MOVE MID-DALEVBSK-AVS (IX)  TO DAT-I-TIDATUM                         
178002     MOVE 'AAVVD'                TO DAT-KDDATFORM                         
178102     PERFORM S01-CALL-WDATKONV                                            
178202     MOVE DAT-TIAAMMDD           TO WS-AAMMDD                             
178302     IF DAT-TIAAMMDD > 500000                                             
178402       MOVE 19                   TO WS-SS                                 
178502     ELSE                                                                 
178602       MOVE 20                   TO WS-SS                                 
178702     END-IF                                                               
178802     MOVE WS-AAAAMMDD            TO W-DALEVBSK                            
178902     PERFORM IMS-GHU-WDD924                                               
179002     IF SEGMENT-FOUND                                                     
179102       PERFORM IMS-DLET-WDD924                                            
179202     END-IF                                                               
179302                                                                          
179402     .                                                                    
179502     EJECT                                                                
179602 HB-CHANGE SECTION.                                                       
179702                                                                          
179802     MOVE MID-IDLEVNR (IX)       TO W-IDLEVNR                             
179902     MOVE MID-DALEVBSK-AVS (IX)  TO DAT-I-TIDATUM                         
180002     MOVE 'AAVVD'                TO DAT-KDDATFORM                         
180102     PERFORM S01-CALL-WDATKONV                                            
180202     MOVE DAT-TIAAMMDD           TO WS-AAMMDD                             
180302     IF DAT-TIAAMMDD > 500000                                             
180402       MOVE 19                   TO WS-SS                                 
180502     ELSE                                                                 
180602       MOVE 20                   TO WS-SS                                 
180702     END-IF                                                               
180802     MOVE WS-AAAAMMDD            TO W-DALEVBSK                            
180902                                                                          
181002     IF WS-DALEVBSK-AVS-AAAAMMDD (IX) > ZERO                              
181102       PERFORM IMS-GHU-WDD924                                             
181202       IF SEGMENT-FOUND                                                   
181302         PERFORM IMS-DLET-WDD924                                          
181402         MOVE WS-DALEVBSK-AVS-AAAAMMDD (IX)                               
181502                                 TO WDD924-LEV-DALEVBSK-AVS               
181602         IF MID-KVAVIS-UPD (IX) NOT = ALL '+'                             
181702           MOVE WS-KVAVIS-UPD (IX)                                        
181802                                 TO WDD924-LEV-KVAVIS-BSKKVAR             
181902                                    WDD924-LEV-KVAVIS-BSKURS              
182002         END-IF                                                           
182102         IF MID-TILEVBSK-INL-UPD (IX) NOT = ALL '+'                       
182202           MOVE WS-TILEVBSK-INL-AAMMDD (IX)                               
182302                                 TO WDD924-LEV-TILEVBSK-INL               
182402           PERFORM S04-CALC-AVAIL-DAY                                     
182502         ELSE                                                             
182602           MOVE WS-NDC-KVDAGAR-TT-UPD (IX)                                
182702                                 TO WORK-KVWORKD                          
182802           PERFORM S05-CALC-RECV-AVAIL-DAY                                
182902         END-IF                                                           
183002         PERFORM IMS-ISRT-WDD924                                          
183102       END-IF                                                             
183202     ELSE                                                                 
183302       PERFORM IMS-GHU-WDD924                                             
183402       IF SEGMENT-FOUND                                                   
183502         IF MID-KVAVIS-UPD (IX) NOT = ALL '+'                             
183602           MOVE WS-KVAVIS-UPD (IX)                                        
183702                                 TO WDD924-LEV-KVAVIS-BSKKVAR             
183802                                    WDD924-LEV-KVAVIS-BSKURS              
183902         END-IF                                                           
184002         IF MID-TILEVBSK-INL-UPD (IX) NOT = ALL '+'                       
184102           MOVE WS-TILEVBSK-INL-AAMMDD (IX)                               
184202                                 TO WDD924-LEV-TILEVBSK-INL               
184302           PERFORM S04-CALC-AVAIL-DAY                                     
184402         END-IF                                                           
184502         PERFORM IMS-REPL-WDD924                                          
184602       END-IF                                                             
184702     END-IF                                                               
184802                                                                          
184902     .                                                                    
185002     EJECT                                                                
185102 HC-INSERT SECTION.                                                       
185202                                                                          
185302     PERFORM IMS-GHU-WDD901                                               
185402     IF SEGMENT-MISSING                                                   
185502       MOVE W-IDARTNR            TO WDD901-IDARTNR                        
185602       MOVE W-IDDC               TO WDD901-IDDC                           
185702       PERFORM IMS-ISRT-WDD901                                            
185802     END-IF                                                               
185902     MOVE WS-IDLEVNR-SHIP-NY     TO W-IDLEVNR                             
186002     PERFORM IMS-GU-WDD902                                                
186102     IF SEGMENT-MISSING                                                   
186202       MOVE WS-IDLEVNR-SHIP-NY   TO WDD902-IDLEVNR                        
186302       MOVE ZERO                 TO WDD902-KVBR                           
186402                                    WDD902-TILEVPL                        
186502       PERFORM IMS-ISRT-WDD902                                            
186602     END-IF                                                               
186702     MOVE WS-IDLEVNR-NY          TO W-IDLEVNR                             
186802     PERFORM IMS-GHU-WDD902                                               
186902     IF SEGMENT-MISSING                                                   
187002       MOVE WS-IDLEVNR-NY        TO WDD902-IDLEVNR                        
187102       MOVE ZERO                 TO WDD902-KVBR                           
187202                                    WDD902-TILEVPL                        
187302       PERFORM IMS-ISRT-WDD902                                            
187402     END-IF                                                               
187502*                                                                         
187602     MOVE ZERO                   TO WDD924-LEV-TILEVBSK-INL               
187702                                    WDD924-LEV-TILEVBSK-DISP              
187802                                    WDD924-LEV-KVAVIS-BSKURS              
187902                                    WDD924-LEV-KVAVIS-BSKKVAR             
188002*                                                                         
188102     MOVE WS-DALEVBSK-AVS-NY-AAAAMMDD                                     
188202                                 TO WDD924-LEV-DALEVBSK-AVS               
188302     IF MID-TILEVBSK-INL-NY = ALL '+'                                     
188402       MOVE WS-NDC-KVDAGAR-TT-NY TO WORK-KVWORKD                          
188502       PERFORM S05-CALC-RECV-AVAIL-DAY                                    
188602       IF WDD924-LEV-TILEVBSK-INL > ZERO                                  
188702         MOVE WS-KVAVIS-NY       TO WDD924-LEV-KVAVIS-BSKURS              
188802                                    WDD924-LEV-KVAVIS-BSKKVAR             
188902       ELSE                                                               
189002         MOVE ZERO               TO WDD924-LEV-KVAVIS-BSKURS              
189102                                    WDD924-LEV-KVAVIS-BSKKVAR             
189202       END-IF                                                             
189302     ELSE                                                                 
189402       MOVE WS-TILEVBSK-INL-NY-AAMMDD                                     
189502                                 TO WDD924-LEV-TILEVBSK-INL               
189602       PERFORM S04-CALC-AVAIL-DAY                                         
189702       MOVE WS-KVAVIS-NY         TO WDD924-LEV-KVAVIS-BSKURS              
189802                                    WDD924-LEV-KVAVIS-BSKKVAR             
189902     END-IF                                                               
190002                                                                          
190102     MOVE 'N'                    TO WDD924-LEV-FLSENLEV                   
190202                                    WDD924-LEV-FLFORAVI                   
190203     MOVE SYSTEM-DATE           TO WDD924-LEV-TIREGDAT                    
190204     MOVE SYSTEM-HHMMSS         TO  WDD924-LEV-TIREGTID                   
190302     PERFORM IMS-ISRT-WDD924                                              
190402                                                                          
190502     .                                                                    
190602     EJECT                                                                
190702 HD-TEXT-UPDATE SECTION.                                                  
190802                                                                          
190902     PERFORM IMS-GHU-WDD901                                               
191002     IF SEGMENT-MISSING                                                   
191102       MOVE W-IDARTNR            TO WDD901-IDARTNR                        
191202       MOVE W-IDDC               TO WDD901-IDDC                           
191302       PERFORM IMS-ISRT-WDD901                                            
191402     END-IF                                                               
191502     MOVE WS-IDLEVNR-SHIP        TO W-IDLEVNR                             
191602     PERFORM IMS-GU-WDD902                                                
191702     IF SEGMENT-MISSING                                                   
191802       MOVE WS-IDLEVNR-SHIP      TO WDD902-IDLEVNR                        
191902       MOVE ZERO                 TO WDD902-KVBR                           
192002                                    WDD902-TILEVPL                        
192102       PERFORM IMS-ISRT-WDD902                                            
192202     END-IF                                                               
192302     MOVE WS-IDLEVNR             TO W-IDLEVNR                             
192402     PERFORM IMS-GHU-WDD902                                               
192502     IF SEGMENT-MISSING                                                   
192602       MOVE WS-IDLEVNR           TO WDD902-IDLEVNR                        
192702       MOVE ZERO                 TO WDD902-KVBR                           
192802                                    WDD902-TILEVPL                        
192902       PERFORM IMS-ISRT-WDD902                                            
193002     END-IF                                                               
193102                                                                          
193202     IF MID-TELEVBSK-TEXT1 NOT = ALL '+'                                  
193302       MOVE +2                   TO W-IDLEVBSK                            
193402       PERFORM IMS-GHU-WDD925                                             
193502       IF SEGMENT-FOUND                                                   
193602         IF MID-TELEVBSK-TEXT1 = SPACE                                    
193702           PERFORM IMS-DLET-WDD925                                        
193802           MOVE +4               TO W-IDLEVBSK                            
193902           PERFORM IMS-GHU-WDD925                                         
194002           IF SEGMENT-FOUND                                               
194102             PERFORM IMS-DLET-WDD925                                      
194202           END-IF                                                         
194302           MOVE +5               TO W-IDLEVBSK                            
194402           PERFORM IMS-GHU-WDD925                                         
194502           IF SEGMENT-FOUND                                               
194602             PERFORM IMS-DLET-WDD925                                      
194702           END-IF                                                         
194802           MOVE +6               TO W-IDLEVBSK                            
194902           PERFORM IMS-GHU-WDD925                                         
195002           IF SEGMENT-FOUND                                               
195102             PERFORM IMS-DLET-WDD925                                      
195202           END-IF                                                         
195302         ELSE                                                             
195402           MOVE MID-TELEVBSK-TEXT1                                        
195502                                 TO WDD925-INFO-TELEVBSK                  
195602           MOVE SYSTEM-DATE      TO WDD925-INFO-TIREGDAT                  
195702           MOVE SYSTEM-HHMMSS    TO WDD925-INFO-TIREGTID                  
195802           PERFORM HDA-CONVERT-DATE                                       
195902           PERFORM IMS-REPL-WDD925                                        
196002         END-IF                                                           
196102       ELSE                                                               
196202         MOVE W-IDLEVBSK         TO WDD925-INFO-IDLEVBSK                  
196302         MOVE MID-TELEVBSK-TEXT1 TO WDD925-INFO-TELEVBSK                  
196402         MOVE SYSTEM-DATE        TO WDD925-INFO-TIREGDAT                  
196502         MOVE SYSTEM-HHMMSS      TO WDD925-INFO-TIREGTID                  
196602         PERFORM HDA-CONVERT-DATE                                         
196702         PERFORM IMS-ISRT-WDD925                                          
196802       END-IF                                                             
196902     ELSE                                                                 
197002       IF MID-TIBORT NOT = ALL '+'                                        
197102         MOVE +2                 TO W-IDLEVBSK                            
197202         PERFORM IMS-GHU-WDD925                                           
197302         IF SEGMENT-FOUND                                                 
197402           PERFORM HDA-CONVERT-DATE                                       
197502           PERFORM IMS-REPL-WDD925                                        
197602         END-IF                                                           
197702       END-IF                                                             
197802     END-IF                                                               
197902                                                                          
198002     IF MID-TELEVBSK-TEXT2 NOT = ALL '+'                                  
198102       MOVE +4                   TO W-IDLEVBSK                            
198202       PERFORM IMS-GHU-WDD925                                             
198302       IF SEGMENT-FOUND                                                   
198402         IF MID-TELEVBSK-TEXT2 = SPACE                                    
198502           PERFORM IMS-DLET-WDD925                                        
198602         ELSE                                                             
198702           MOVE MID-TELEVBSK-TEXT2                                        
198802                                 TO WDD925-INFO-TELEVBSK                  
198902           MOVE SYSTEM-DATE      TO WDD925-INFO-TIREGDAT                  
199002           MOVE SYSTEM-HHMMSS    TO WDD925-INFO-TIREGTID                  
199102           PERFORM HDA-CONVERT-DATE                                       
199202           PERFORM IMS-REPL-WDD925                                        
199302         END-IF                                                           
199402       ELSE                                                               
199502         MOVE W-IDLEVBSK         TO WDD925-INFO-IDLEVBSK                  
199602         MOVE MID-TELEVBSK-TEXT2 TO WDD925-INFO-TELEVBSK                  
199702         MOVE SYSTEM-DATE        TO WDD925-INFO-TIREGDAT                  
199802         MOVE SYSTEM-HHMMSS      TO WDD925-INFO-TIREGTID                  
199902         PERFORM HDA-CONVERT-DATE                                         
200002         PERFORM IMS-ISRT-WDD925                                          
200102       END-IF                                                             
200202     ELSE                                                                 
200302       IF MID-TIBORT NOT = ALL '+'                                        
200402         MOVE +4                 TO W-IDLEVBSK                            
200502         PERFORM IMS-GHU-WDD925                                           
200602         IF SEGMENT-FOUND                                                 
200702           PERFORM HDA-CONVERT-DATE                                       
200802           PERFORM IMS-REPL-WDD925                                        
200902         END-IF                                                           
201002       END-IF                                                             
201102     END-IF                                                               
201202                                                                          
201302     IF MID-TELEVBSK-TEXT3 NOT = ALL '+'                                  
201402       MOVE +5                   TO W-IDLEVBSK                            
201502       PERFORM IMS-GHU-WDD925                                             
201602       IF SEGMENT-FOUND                                                   
201702         IF MID-TELEVBSK-TEXT3 = SPACE                                    
201802           PERFORM IMS-DLET-WDD925                                        
201902         ELSE                                                             
202002           MOVE MID-TELEVBSK-TEXT3                                        
202102                                 TO WDD925-INFO-TELEVBSK                  
202202           MOVE SYSTEM-DATE      TO WDD925-INFO-TIREGDAT                  
202302           MOVE SYSTEM-HHMMSS    TO WDD925-INFO-TIREGTID                  
202402           PERFORM HDA-CONVERT-DATE                                       
202502           PERFORM IMS-REPL-WDD925                                        
202602         END-IF                                                           
202702       ELSE                                                               
202802         MOVE W-IDLEVBSK         TO WDD925-INFO-IDLEVBSK                  
202902         MOVE MID-TELEVBSK-TEXT3 TO WDD925-INFO-TELEVBSK                  
203002         MOVE SYSTEM-DATE        TO WDD925-INFO-TIREGDAT                  
203102         MOVE SYSTEM-HHMMSS      TO WDD925-INFO-TIREGTID                  
203202         PERFORM HDA-CONVERT-DATE                                         
203302         PERFORM IMS-ISRT-WDD925                                          
203402       END-IF                                                             
203502     ELSE                                                                 
203602       IF MID-TIBORT NOT = ALL '+'                                        
203702         MOVE +5                 TO W-IDLEVBSK                            
203802         PERFORM IMS-GHU-WDD925                                           
203902         IF SEGMENT-FOUND                                                 
204002           PERFORM HDA-CONVERT-DATE                                       
204102           PERFORM IMS-REPL-WDD925                                        
204202         END-IF                                                           
204302       END-IF                                                             
204402     END-IF                                                               
204502                                                                          
204602     IF MID-TELEVBSK-TEXT4 NOT = ALL '+'                                  
204702       MOVE +6                   TO W-IDLEVBSK                            
204802       PERFORM IMS-GHU-WDD925                                             
204902       IF SEGMENT-FOUND                                                   
205002         IF MID-TELEVBSK-TEXT4 = SPACE                                    
205102           PERFORM IMS-DLET-WDD925                                        
205202         ELSE                                                             
205302           MOVE MID-TELEVBSK-TEXT4                                        
205402                                 TO WDD925-INFO-TELEVBSK                  
205502           MOVE SYSTEM-DATE      TO WDD925-INFO-TIREGDAT                  
205602           MOVE SYSTEM-HHMMSS    TO WDD925-INFO-TIREGTID                  
205702           PERFORM HDA-CONVERT-DATE                                       
205802           PERFORM IMS-REPL-WDD925                                        
205902         END-IF                                                           
206002       ELSE                                                               
206102         MOVE W-IDLEVBSK         TO WDD925-INFO-IDLEVBSK                  
206202         MOVE MID-TELEVBSK-TEXT4 TO WDD925-INFO-TELEVBSK                  
206302         MOVE SYSTEM-DATE        TO WDD925-INFO-TIREGDAT                  
206402         MOVE SYSTEM-HHMMSS      TO WDD925-INFO-TIREGTID                  
206502         PERFORM HDA-CONVERT-DATE                                         
206602         PERFORM IMS-ISRT-WDD925                                          
206702       END-IF                                                             
206802     ELSE                                                                 
206902       IF MID-TIBORT NOT = ALL '+'                                        
207002         MOVE +6                 TO W-IDLEVBSK                            
207102         PERFORM IMS-GHU-WDD925                                           
207202         IF SEGMENT-FOUND                                                 
207302           PERFORM HDA-CONVERT-DATE                                       
207402           PERFORM IMS-REPL-WDD925                                        
207502         END-IF                                                           
207602       END-IF                                                             
207702     END-IF                                                               
207802                                                                          
207902     .                                                                    
208002     EJECT                                                                
208102 HDA-CONVERT-DATE SECTION.                                                
208202                                                                          
208302     MOVE MID-TIBORT             TO DAT-I-TIDATUM                         
208402     MOVE 'AAVVD'                TO DAT-KDDATFORM                         
208502     PERFORM S01-CALL-WDATKONV                                            
208602     MOVE DAT-TIAAMMDD           TO WDD925-INFO-TIBORT                    
208702                                                                          
208802     .                                                                    
208902     EJECT                                                                
209002 S01-CALL-WDATKONV SECTION.                                               
209102                                                                          
209202     CALL WDATKONV            USING DAT-KDDATFORM                         
209302                                    DAT-I-TIDATUM                         
209402                                    DAT-O-TIDATUM                         
209502                                    DAT-KDSVAR                            
209602                                                                          
209702     .                                                                    
209802     EJECT                                                                
209902 S02-CALL-WORKDAY SECTION.                                                
210002                                                                          
210102     CALL WORKDAY             USING WORK-KDCALL                           
210202                                    WORK-DATE-AREA                        
210302                                    WORK-KDSVAR                           
210402                                                                          
210502                                                                          
210602     .                                                                    
210702     EJECT                                                                
210802 S03-SEARCH-IDLAND SECTION.                                               
210902                                                                          
211002     SEARCH ALL DC-LAND                                                   
211102       AT END                                                             
211202         MOVE 'NO MATCH FOUND IN WWDCLAND' TO ERROR-TEXT                  
211302         CALL FELLOG                                                      
211402       WHEN DCLAND-IDDC (DCLAND-IX) = W-IDDC                              
211502         MOVE DCLAND-IDLANDX2(DCLAND-IX) TO W-IDLAND                      
211602     END-SEARCH                                                           
211702     .                                                                    
211802     EJECT                                                                
211902                                                                          
212002 S04-CALC-AVAIL-DAY SECTION.                                              
212102                                                                          
212202     PERFORM S03-SEARCH-IDLAND                                            
212302     PERFORM IMS-GU-WDK712                                                
212402     IF SEGMENT-FOUND                                                     
212502       COMPUTE WS-KVDAGAR-INLEV = WDK712-LART-KVDAGAR-INLEV + 1           
212602       MOVE 2                    TO WORK-KDCALL                           
212702       MOVE W-IDDC               TO WORK-IDDC                             
212802       MOVE WDD924-LEV-TILEVBSK-INL                                       
212902                                 TO WORK-TIAAMMDD-FOM                     
213002       MOVE WS-KVDAGAR-INLEV     TO WORK-KVWORKD                          
213102       PERFORM S02-CALL-WORKDAY                                           
213202       IF WORK-KDSVAR-OK                                                  
213302         MOVE WORK-TIAAMMDD-TOM  TO WDD924-LEV-TILEVBSK-DISP              
213402       END-IF                                                             
213502     END-IF                                                               
213602                                                                          
213702     .                                                                    
213802     EJECT                                                                
213902 S05-CALC-RECV-AVAIL-DAY SECTION.                                         
214002                                                                          
214102     MOVE 2                      TO WORK-KDCALL                           
214202     MOVE W-IDDC                 TO WORK-IDDC                             
214302     MOVE WDD924-LEV-DALEVBSK-AVS                                         
214402                                 TO WORK-TIAAMMDD-FOM                     
214502     PERFORM S02-CALL-WORKDAY                                             
214602     IF WORK-KDSVAR-OK                                                    
214702       MOVE WORK-TIAAMMDD-TOM    TO WDD924-LEV-TILEVBSK-INL               
214802       PERFORM S04-CALC-AVAIL-DAY                                         
214902     END-IF                                                               
215002     .                                                                    
215102     EJECT                                                                
215202 MFS-ERASE-FIELD-IN SECTION.                                              
215302                                                                          
215303     PERFORM MFS-ERASE-LINE-FIELD-IN                                      
215304                                                                          
215305     MOVE MFS-ERASE-FIELD        TO MOD-DALEVBSK-AVS-NY                   
215306                                    MOD-KVAVIS-NY                         
215307                                    MOD-TILEVBSK-INL-NY                   
215308                                    MOD-IDLEVNR-NY                        
215309                                    MOD-IDLEVNR-SHIP-NY                   
215310                                    MOD-TELEVBSK-TEXT1                    
215311                                    MOD-TELEVBSK-TEXT2                    
215312                                    MOD-TELEVBSK-TEXT3                    
215320                                    MOD-TELEVBSK-TEXT4                    
215330                                    MOD-TIBORT                            
216902     .                                                                    
217002     EJECT                                                                
217102 MFS-ERASE-LINE-FIELD-IN SECTION.                                         
217202                                                                          
217203     PERFORM                                                              
217204     VARYING IX FROM +1 BY +1                                             
217205       UNTIL IX > MAX-IX                                                  
217206     MOVE MFS-ERASE-FIELD        TO MOD-KDCMD-UPD        (IX)             
217207                                    MOD-DALEVBSK-AVS-UPD (IX)             
217208                                    MOD-KVAVIS-UPD       (IX)             
217209                                    MOD-TILEVBSK-INL-UPD (IX)             
217210     END-PERFORM                                                          
217702     .                                                                    
217802     EJECT                                                                
217902 MFS-ERASE-FIELD-OUT SECTION.                                             
218002                                                                          
218102     PERFORM                                                              
218202     VARYING IX FROM +1 BY +1                                             
218302       UNTIL IX > MAX-IX                                                  
218402       MOVE MFS-ERASE-FIELD      TO MOD-DALEVBSK-AVS (IX)                 
218502                                    MOD-KVAVIS       (IX)                 
218602                                    MOD-TILEVBSK-INL (IX)                 
218702                                    MOD-TILEVBSK-DISP(IX)                 
218802                                    MOD-FLFORAVI     (IX)                 
218902                                    MOD-IDLEVNR      (IX)                 
219002     END-PERFORM                                                          
219102                                                                          
219202     .                                                                    
219302     SKIP3                                                                
219402 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
219502                                                                          
219503     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-DALEVBSK-AVS-NY                   
219504                                    MOD-KVAVIS-NY                         
219505                                    MOD-TILEVBSK-INL-NY                   
219506                                    MOD-IDLEVNR-NY                        
219507                                    MOD-IDLEVNR-SHIP-NY                   
219508                                                                          
219903     PERFORM MFS-DONT-TOUCH-LINE-FIELD-IN                                 
220602     .                                                                    
220702     EJECT                                                                
220802 MFS-DONT-TOUCH-LINE-FIELD-IN  SECTION.                                   
220902                                                                          
220903     PERFORM                                                              
220904     VARYING IX FROM +1 BY +1                                             
220905       UNTIL IX > MAX-IX                                                  
221002       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMD-UPD        (IX)           
221102                                      MOD-DALEVBSK-AVS-UPD (IX)           
221202                                      MOD-KVAVIS-UPD       (IX)           
221302                                      MOD-TILEVBSK-INL-UPD (IX)           
221303     END-PERFORM                                                          
221402     .                                                                    
221502     EJECT                                                                
221602                                                                          
221702 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
221802                                                                          
221902     PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                                
222302                                                                          
222402     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TELEVBSK-TEXT1                    
222502                                    MOD-TELEVBSK-TEXT2                    
222602                                    MOD-TELEVBSK-TEXT3                    
222702                                    MOD-TELEVBSK-TEXT4                    
222802                                    MOD-TIBORT                            
223002     .                                                                    
223102     SKIP3                                                                
223202 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
223302                                                                          
223303     PERFORM                                                              
223304     VARYING IX FROM +1 BY +1                                             
223305       UNTIL IX > MAX-IX                                                  
223402     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-DALEVBSK-AVS  (IX)                
223502                                    MOD-KVAVIS        (IX)                
223602                                    MOD-TILEVBSK-INL  (IX)                
223702                                    MOD-TILEVBSK-DISP (IX)                
223802                                    MOD-FLFORAVI      (IX)                
223902                                    MOD-IDLEVNR       (IX)                
223903     END-PERFORM                                                          
224002     .                                                                    
224102     EJECT                                                                
224202                                                                          
224302 MFS-CLOSE-FIELD-IN  SECTION.                                             
224402                                                                          
224502     PERFORM                                                              
224602     VARYING IX FROM +1 BY +1                                             
224702       UNTIL IX > MAX-IX                                                  
224802                                                                          
224902       MOVE MFS-CLOSE-FIELD      TO MOD-KDCMD-UPD-ATTR        (IX)        
225002                                    MOD-DALEVBSK-AVS-UPD-ATTR (IX)        
225102                                    MOD-KVAVIS-UPD-ATTR       (IX)        
225202                                    MOD-TILEVBSK-INL-UPD-ATTR (IX)        
225302     END-PERFORM                                                          
225402     .                                                                    
225502     EJECT                                                                
225602                                                                          
225702 MFS-FORM-ATTR SECTION.                                                   
225802                                                                          
225902     PERFORM MFS-LINE-FORM-ATTR                                           
225903                                                                          
226302     MOVE MFS-FORMAT-DEFAULT-ATTR                                         
226402                                 TO MOD-DALEVBSK-AVS-NY-ATTR              
226502                                    MOD-KVAVIS-NY-ATTR                    
226602                                    MOD-TILEVBSK-INL-NY-ATTR              
226702                                    MOD-IDLEVNR-NY-ATTR                   
226802                                    MOD-IDLEVNR-SHIP-NY-ATTR              
227002     .                                                                    
227102     SKIP2                                                                
227202 MFS-LINE-FORM-ATTR SECTION.                                              
227302                                                                          
227303     PERFORM                                                              
227304     VARYING IX FROM +1 BY +1                                             
227305       UNTIL IX > MAX-IX                                                  
227402     MOVE MFS-FORMAT-DEFAULT-ATTR                                         
227502                                 TO MOD-KDCMD-UPD-ATTR        (IX)        
227602                                    MOD-DALEVBSK-AVS-UPD-ATTR (IX)        
227702                                    MOD-KVAVIS-UPD-ATTR       (IX)        
227802                                    MOD-TILEVBSK-INL-UPD-ATTR (IX)        
227803     END-PERFORM                                                          
227902     .                                                                    
228002     SKIP2                                                                
228102* --- IMS SECTIONS ---                                                    
228202     SKIP3                                                                
228302 IMS-GET-MSG SECTION.                                                     
228402                                                                          
228502     MOVE '  QC'                 TO GOOD-STATUSCODES                      
228602     CALL CBLTDLI             USING GU                                    
228702                                    MSG-PCB                               
228802                                    MSG-IO-AREA                           
228902     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
229002     PERFORM IMS-STATUSCHECK                                              
229102     .                                                                    
229202     SKIP3                                                                
229302 IMS-INSERT-MSG SECTION.                                                  
229402                                                                          
229502**   IF MSGI-IDLAND-SPR = 'SE'                                            
229602**     MOVE '0'                  TO MFS-KDHUVOMR                          
229702**   END-IF                                                               
229802     MOVE LOW-VALUE              TO MSG-KDZ1                              
229902                                    MSG-KDZ2                              
230002     MOVE SPACE                  TO GOOD-STATUSCODES                      
230102     CALL CBLTDLI             USING ISRT                                  
230202                                    MSG-PCB                               
230302                                    MSG-IO-AREA                           
230402                                    MFS-IDMOD                             
230502     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
230602     PERFORM IMS-STATUSCHECK                                              
230702     .                                                                    
230802     EJECT                                                                
230902 IMS-GU-WDB601 SECTION.                                                   
231002                                                                          
231102     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
231202          DELIMITED BY SIZE INTO SSA1                                     
231302     MOVE '  GE'                 TO GOOD-STATUSCODES                      
231402     CALL CBLTDLI             USING GU                                    
231502                                    WDB6-PCB                              
231602                                    DLI-IO-WDB601                         
231702                                    SSA1                                  
231802     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
231902     PERFORM IMS-STATUSCHECK                                              
232002     .                                                                    
232102     EJECT                                                                
232202 IMS-GU-WDF101 SECTION.                                                   
232302                                                                          
232402     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
232502          DELIMITED BY SIZE INTO SSA1                                     
232602     MOVE '  GE'                 TO GOOD-STATUSCODES                      
232702     CALL CBLTDLI             USING GU                                    
232802                                    WDF1-PCB                              
232902                                    DLI-IO-WDF101                         
233002                                    SSA1                                  
233102     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
233202     PERFORM IMS-STATUSCHECK                                              
233302     .                                                                    
233402     EJECT                                                                
233502 IMS-GU-WDF116 SECTION.                                                   
233602                                                                          
233702     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
233802          DELIMITED BY SIZE INTO SSA1                                     
233902     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
234002          DELIMITED BY SIZE INTO SSA2                                     
234102     MOVE '  GE'                 TO GOOD-STATUSCODES                      
234202     CALL CBLTDLI             USING GU                                    
234302                                    WDF1-PCB                              
234402                                    DLI-IO-WDF116                         
234502                                    SSA1 SSA2                             
234602     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
234702     PERFORM IMS-STATUSCHECK                                              
234802     .                                                                    
234902     EJECT                                                                
235002 IMS-GU-WDK601 SECTION.                                                   
235102                                                                          
235202     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
235302          DELIMITED BY SIZE INTO SSA1                                     
235402     MOVE '  GE'                 TO GOOD-STATUSCODES                      
235502     CALL CBLTDLI             USING GU                                    
235602                                    WDK6-PCB                              
235702                                    DLI-IO-WDK601                         
235802                                    SSA1                                  
235902     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
236002     PERFORM IMS-STATUSCHECK                                              
236102     .                                                                    
236202     EJECT                                                                
236302 IMS-GU-WDK701 SECTION.                                                   
236402                                                                          
236502     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
236602          DELIMITED BY SIZE INTO SSA1                                     
236702     MOVE '  GE'                 TO GOOD-STATUSCODES                      
236802     CALL CBLTDLI             USING GU                                    
236902                                    WDK7-PCB                              
237002                                    DLI-IO-WDK701                         
237102                                    SSA1                                  
237202     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
237302     PERFORM IMS-STATUSCHECK                                              
237402     .                                                                    
237502     EJECT                                                                
237602 IMS-GU-WDK711 SECTION.                                                   
237702                                                                          
237802     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
237902          DELIMITED BY SIZE INTO SSA1                                     
238002     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
238102          DELIMITED BY SIZE INTO SSA2                                     
238202     MOVE '  GE'                 TO GOOD-STATUSCODES                      
238302     CALL CBLTDLI             USING GU                                    
238402                                    WDK7-PCB                              
238502                                    DLI-IO-WDK711                         
238602                                    SSA1 SSA2                             
238702     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
238802     PERFORM IMS-STATUSCHECK                                              
238902     .                                                                    
239002     EJECT                                                                
239102 IMS-GU-WDK712 SECTION.                                                   
239202                                                                          
239302     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
239402          DELIMITED BY SIZE INTO SSA1                                     
239502     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
239602          DELIMITED BY SIZE INTO SSA2                                     
239702     MOVE '  GE'                 TO GOOD-STATUSCODES                      
239802     CALL CBLTDLI             USING GU                                    
239902                                    WDK7-PCB                              
240002                                    DLI-IO-WDK712                         
240102                                    SSA1 SSA2                             
240202     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
240302     PERFORM IMS-STATUSCHECK                                              
240402     .                                                                    
240502     EJECT                                                                
240602 IMS-GNP-WDK722 SECTION.                                                  
240702                                                                          
240802     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
240902          DELIMITED BY SIZE INTO SSA1                                     
241002     MOVE '  GE'                 TO GOOD-STATUSCODES                      
241102     CALL CBLTDLI             USING GNP                                   
241202                                    WDK7-PCB                              
241302                                    DLI-IO-WDK722                         
241402                                    SSA1                                  
241502     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
241602     PERFORM IMS-STATUSCHECK                                              
241702     .                                                                    
241802     EJECT                                                                
241902 IMS-GNP-WDK723 SECTION.                                                  
242002                                                                          
242102     MOVE 'WDK723  '             TO SSA1                                  
242202     MOVE '  GE'                 TO GOOD-STATUSCODES                      
242302     CALL CBLTDLI             USING GNP                                   
242402                                    WDK7-PCB                              
242502                                    DLI-IO-WDK723                         
242602                                    SSA1                                  
242702     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
242802     PERFORM IMS-STATUSCHECK                                              
242902     .                                                                    
243002     EJECT                                                                
243102 IMS-GU-WDD901 SECTION.                                                   
243202                                                                          
243302     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
243402          DELIMITED BY SIZE INTO SSA1                                     
243502     MOVE '  GE'                 TO GOOD-STATUSCODES                      
243602     CALL CBLTDLI             USING GU                                    
243702                                    WDD9-PCB                              
243802                                    DLI-IO-WDD901                         
243902                                    SSA1                                  
244002     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
244102     PERFORM IMS-STATUSCHECK                                              
244202     .                                                                    
244302     SKIP3                                                                
244402 IMS-GHU-WDD901 SECTION.                                                  
244502                                                                          
244602     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
244702          DELIMITED BY SIZE INTO SSA1                                     
244802     MOVE '  GE'                 TO GOOD-STATUSCODES                      
244902     CALL CBLTDLI             USING GHU                                   
245002                                    WDD9-PCB                              
245102                                    DLI-IO-WDD901                         
245202                                    SSA1                                  
245302     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
245402     PERFORM IMS-STATUSCHECK                                              
245502     .                                                                    
245602     SKIP3                                                                
245702 IMS-ISRT-WDD901 SECTION.                                                 
245802                                                                          
245902     MOVE 'WDD901 ' TO SSA1                                               
246002     MOVE '  II'                 TO GOOD-STATUSCODES                      
246102     CALL CBLTDLI             USING ISRT                                  
246202                                    WDD9-PCB                              
246302                                    DLI-IO-WDD901                         
246402                                    SSA1                                  
246502     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
246602     PERFORM IMS-STATUSCHECK                                              
246702     .                                                                    
246802     SKIP3                                                                
246902 IMS-GU-WDD902 SECTION.                                                   
247002                                                                          
247102     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
247202          DELIMITED BY SIZE INTO SSA1                                     
247302     MOVE '  GE'                 TO GOOD-STATUSCODES                      
247402     CALL CBLTDLI             USING GU                                    
247502                                    WDD9-PCB                              
247602                                    DLI-IO-WDD902                         
247702                                    SSA1                                  
247802     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
247902     PERFORM IMS-STATUSCHECK                                              
248002     .                                                                    
248102     SKIP3                                                                
248202 IMS-GHU-WDD902 SECTION.                                                  
248302                                                                          
248402     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
248502          DELIMITED BY SIZE INTO SSA1                                     
248602     MOVE '  GE'                 TO GOOD-STATUSCODES                      
248702     CALL CBLTDLI             USING GHU                                   
248802                                    WDD9-PCB                              
248902                                    DLI-IO-WDD902                         
249002                                    SSA1                                  
249102     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
249202     PERFORM IMS-STATUSCHECK                                              
249302     .                                                                    
249402     SKIP3                                                                
249502 IMS-GNP-WDD902 SECTION.                                                  
249602                                                                          
249702     MOVE 'WDD902  '             TO SSA1                                  
249802     MOVE '  GE'                 TO GOOD-STATUSCODES                      
249902     CALL CBLTDLI             USING GNP                                   
250002                                    WDD9-PCB                              
250102                                    DLI-IO-WDD902                         
250202                                    SSA1                                  
250302     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
250402     PERFORM IMS-STATUSCHECK                                              
250502     .                                                                    
250602     SKIP3                                                                
250702 IMS-ISRT-WDD902 SECTION.                                                 
250802                                                                          
250902     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
251002          DELIMITED BY SIZE INTO SSA1                                     
251102     MOVE 'WDD902 '              TO SSA2                                  
251202     MOVE '  II'                 TO GOOD-STATUSCODES                      
251302     CALL CBLTDLI             USING ISRT                                  
251402                                    WDD9-PCB                              
251502                                    DLI-IO-WDD902                         
251602                                    SSA1 SSA2                             
251702     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
251802     PERFORM IMS-STATUSCHECK                                              
251902     .                                                                    
252002     SKIP3                                                                
252102 IMS-GNP-WDD924 SECTION.                                                  
252202                                                                          
252303     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
252402             DELIMITED BY SIZE INTO SSA1                                  
252403     STRING 'WDD924  (DALEVBSK=>' W-DALEVBSK-X ')'                        
252404             DELIMITED BY SIZE INTO SSA2                                  
252502     MOVE '  GE'                 TO GOOD-STATUSCODES                      
252602     CALL CBLTDLI             USING GNP                                   
252702                                    WDD9-PCB                              
252802                                    DLI-IO-WDD924                         
252902                                    SSA1 SSA2                             
253002     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
253102     PERFORM IMS-STATUSCHECK                                              
253202     .                                                                    
253302     SKIP3                                                                
254602 IMS-GHU-WDD924 SECTION.                                                  
254702                                                                          
254802     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
254902          DELIMITED BY SIZE INTO SSA1                                     
255002     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
255102          DELIMITED BY SIZE INTO SSA2                                     
255202     STRING 'WDD924  (DALEVBSK =' W-DALEVBSK-X ')'                        
255302          DELIMITED BY SIZE INTO SSA3                                     
255402     MOVE '  GE'                 TO GOOD-STATUSCODES                      
255502     CALL CBLTDLI             USING GHU                                   
255602                                    WDD9-PCB                              
255702                                    DLI-IO-WDD924                         
255802                                    SSA1 SSA2 SSA3                        
255902     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
256002     PERFORM IMS-STATUSCHECK                                              
256102     .                                                                    
256202     SKIP3                                                                
256302 IMS-ISRT-WDD924 SECTION.                                                 
256402                                                                          
256502     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
256602          DELIMITED BY SIZE INTO SSA1                                     
256702     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
256802          DELIMITED BY SIZE INTO SSA2                                     
256902     MOVE 'WDD924 '              TO SSA3                                  
257002     MOVE '  II'                 TO GOOD-STATUSCODES                      
257102     CALL CBLTDLI             USING ISRT                                  
257202                                    WDD9-PCB                              
257302                                    DLI-IO-WDD924                         
257402                                    SSA1 SSA2 SSA3                        
257502     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
257602     PERFORM IMS-STATUSCHECK                                              
257702     .                                                                    
257802     SKIP3                                                                
257902 IMS-REPL-WDD924 SECTION.                                                 
258002                                                                          
258102     MOVE '  '                   TO GOOD-STATUSCODES                      
258202     CALL CBLTDLI             USING REPL                                  
258302                                    WDD9-PCB                              
258402                                    DLI-IO-WDD924                         
258502     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
258602     PERFORM IMS-STATUSCHECK                                              
258702     .                                                                    
258802     SKIP3                                                                
258902 IMS-DLET-WDD924 SECTION.                                                 
259002                                                                          
259102     MOVE '  '                   TO GOOD-STATUSCODES                      
259202     CALL CBLTDLI             USING DLET                                  
259302                                    WDD9-PCB                              
259402                                    DLI-IO-WDD924                         
259502     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
259602     PERFORM IMS-STATUSCHECK                                              
259702     .                                                                    
259802     EJECT                                                                
259902 IMS-GHU-WDD925 SECTION.                                                  
260002                                                                          
260102     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
260202          DELIMITED BY SIZE INTO SSA1                                     
260302     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
260402          DELIMITED BY SIZE INTO SSA2                                     
260502     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
260602          DELIMITED BY SIZE INTO SSA3                                     
260702     MOVE '  GE'                 TO GOOD-STATUSCODES                      
260802     CALL CBLTDLI             USING GHU                                   
260902                                    WDD9-PCB                              
261002                                    DLI-IO-WDD925                         
261102                                    SSA1 SSA2 SSA3                        
261202     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
261302     PERFORM IMS-STATUSCHECK                                              
261402     .                                                                    
261502     SKIP3                                                                
261602 IMS-ISRT-WDD925 SECTION.                                                 
261702                                                                          
261802     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
261902          DELIMITED BY SIZE INTO SSA1                                     
262002     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
262102          DELIMITED BY SIZE INTO SSA2                                     
262202     MOVE 'WDD925 '              TO SSA3                                  
262302     MOVE '  II'                 TO GOOD-STATUSCODES                      
262402     CALL CBLTDLI             USING ISRT                                  
262502                                    WDD9-PCB                              
262602                                    DLI-IO-WDD925                         
262702                                    SSA1 SSA2 SSA3                        
262802     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
262902     PERFORM IMS-STATUSCHECK                                              
263002     .                                                                    
263102     SKIP3                                                                
263202 IMS-REPL-WDD925 SECTION.                                                 
263302                                                                          
263402     MOVE '  '                   TO GOOD-STATUSCODES                      
263502     CALL CBLTDLI             USING REPL                                  
263602                                    WDD9-PCB                              
263702                                    DLI-IO-WDD925                         
263802     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
263902     PERFORM IMS-STATUSCHECK                                              
264002     .                                                                    
264102     SKIP3                                                                
264202 IMS-DLET-WDD925 SECTION.                                                 
264302                                                                          
264402     MOVE '  '                   TO GOOD-STATUSCODES                      
264502     CALL CBLTDLI             USING DLET                                  
264602                                    WDD9-PCB                              
264702                                    DLI-IO-WDD925                         
264802     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
264902     PERFORM IMS-STATUSCHECK                                              
265002     .                                                                    
265102     EJECT                                                                
265202 IMS-STATUSCHECK SECTION.                                                 
265302                                                                          
265402     SET STATUS-IX               TO 1                                     
265502     SEARCH GOOD-STATUS                                                   
265602       AT END                                                             
265702         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
265802         DELIMITED BY SIZE INTO ERROR-TEXT                                
265902         CALL FELLOG                                                      
266002       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
266102         CONTINUE                                                         
266202     END-SEARCH                                                           
266302     .                                                                    
266402     EJECT                                                                
266502*    -COPY WY2000P1                                                       
266602     EJECT                                                                
266702*    -COPY WY2000P2                                                       
267001     EJECT                                                                
