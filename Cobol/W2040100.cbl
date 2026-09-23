000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2040100.                                                
000300 AUTHOR.         RAHUL JAIN.                                              
000400 DATE-WRITTEN.   SEPTEMBER 2012.                                          
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        PARTS PLANNER STOCK INFORMATION FOR CHINA.                       
000900*                                                                         
001000*        THE PROGRAM READS     WDK6                                       
001100*                              WDK7                                       
001200*                              WDR2                                       
001300*                              WDD3                                       
001400*                              WDN6                                       
001500*                              WDF1                                       
001600*                              WDB6                                       
001700*                              WDP7                                       
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W2T401                                              
002100*        MID:         W2I40101                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        MOD:         W2O401N1                                            
002500                                                                          
002600                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W2040100'.            
003200                                                                          
003300 01  WS.                                                                  
003400     03  WS-IDLEVNR-8            PIC X(8)  VALUE SPACE.                   
003500     03  IDLEVNR-WS              PIC X(5)  VALUE SPACE.                   
003600     03  WS-INDATE.                                                       
003700         05 FILLER               PIC 9(2)  VALUE ZERO.                    
003800         05 WS-INDATE-6          PIC 9(6)  VALUE ZERO.                    
003900     03  IDDC-WS                 PIC X(2)  VALUE SPACE.                   
004000     03  IX                      PIC S9(9) COMP SYNC.                     
004100     03  WS-WDK711-EXIST         PIC X(01) VALUE SPACE.                   
004200                                                                          
004300                                                                          
004400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004600                                                                          
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  YES                         PIC X       VALUE 'Y'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000 77  HYPHEN                      PIC X       VALUE '-'.                   
005100                                                                          
005200 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005300 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005400                                                                          
005500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005600                                                                          
005700                                                                          
005800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005900     88  KEYS-OK                             VALUE 'J'.                   
006000     88  KEYS-WRONG                          VALUE 'N'.                   
006100                                                                          
006200 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
006300     88  USER-AUTHORISED                     VALUE 'J'.                   
006400     88  USER-UNAUTHORISED                   VALUE 'N'.                   
006500                                                                          
006600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006700     88  OWN-MID                             VALUE '2401'.                
006800     88  GOOD-MID                            VALUE '2401' '2402'          
006900                                                   '2403' '2404'          
007000                                                   '2405' '2406'          
007100                                                   '2407' '2408'          
007200                                                   '2409'.                
007300     88  HELP-MID                            VALUE '0551'.                
007400                                                                          
007500 77  MAX-ANT-AO-NUMMER           PIC S9(9)   VALUE +5 COMP SYNC.          
007600 77  MAX-ANT-PROENH              PIC S9(9)   VALUE +3 COMP SYNC.          
007700                                                                          
007800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007900 01  GENERAL-SUBPROGRAMS.                                                 
008000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008500     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
008600     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
008700                                                                          
008800                                                                          
008900*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
009000*01  -COPY WDATAREA                                                       
009100                                                                          
009200*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009300*01 -COPY WMEDAREA                                                        
009400                                                                          
009500*    --- PARAMETRAR TILL W271REFL                                         
009600*01 -COPY W271REFL                                                        
009700                                                                          
009800*    --- PARAMETRAR TILL W271UTUP                                         
009900*01 -COPY W271UTUP                                                        
010000                                                                          
010100                                                                          
010200 01  MESSAGE-CODES.                                                       
010300     03  ERR-PART-NOT-NUMERIC    PIC X(3)    VALUE '020'.                 
010400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010500     03  ERR-USER-NOT-AUTHORISED PIC X(3)    VALUE '405'.                 
010600     03  ERR-INVALID-DC          PIC X(3)    VALUE '440'.                 
010700     03  ERR-PART-NOT-FOUND      PIC X(3)    VALUE '769'.                 
010800     03  INF-PRESS-ENTER         PIC X(3)    VALUE '169'.                 
010900     03  INF-PART-SUPERSEDED     PIC X(3)    VALUE '220'.                 
011000     03  INF-PART-EXPIRED        PIC X(3)    VALUE '258'.                 
011100     03  INF-REPLACING-PART      PIC X(3)    VALUE '259'.                 
011200                                                                          
011300                                                                          
011400*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011700*01 -COPY WMSGINIT                                                        
011800                                                                          
011900                                                                          
012000*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
012100*                                                                         
012200 01  SAVE-AREA.                                                           
012300     03  SAVE-IDTRANS           PIC X(4)    VALUE '2401'.                 
012400                                                                          
012500                                                                          
012600*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012900*01  MID -COPY W2I40101                                                   
013000                                                                          
013100                                                                          
013200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013300*01  -COPY WMSGAREA                                                       
013400                                                                          
013500                                                                          
013600     03  MOD REDEFINES MSG-AREA.                                          
013700*      05  -COPY W2O40101                                                 
013800                                                                          
013900                                                                          
014000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014100*01  -COPY WMFSAREA                                                       
014200                                                                          
014300                                                                          
014400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700                                                                          
014800 01  SCROLLING-FIELDS.                                                    
014900     03  LAST-SEG                PIC 9(3)    VALUE ZERO.                  
015000                                                                          
015100 01  KEYS-FOR-DLI.                                                        
015200     03  W-IDARTNR-X.                                                     
015300         05  W-IDARTNR           PIC S9(9) COMP-3                         
015400                                             VALUE ZERO.                  
015500     03  W-WDGXKEY-2261-X.                                                
015600          05 W-IDHTYP            PIC X(4)    VALUE '2261'.                
015700          05 W-IDDC-2261         PIC X(2)    VALUE SPACE.                 
015800          05 FILLER              PIC X(24)   VALUE LOW-VALUE.             
015900                                                                          
016000     03  W-IDLEVNR-X.                                                     
016100         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
016200     03  W-KDSEGKEY-X.                                                    
016300         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016400     03  W-KDNOTTYP-X.                                                    
016500         05  W-KDNOTTYP          PIC S9      COMP-3.                      
016600     03  W-IDDC-X.                                                        
016700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016800     03  W-IDLAND-X.                                                      
016900         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
017000     03  W-IDSKYLT-X.                                                     
017100         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
017200                                                                          
017300     03  W-WDGXKEY-1143-X.                                                
017400         05 W-IDHTYP-1143        PIC X(4)    VALUE '1143'.                
017500         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
017600                                                                          
017700     03  W-IDFKNGRP-FOM-X.                                                
017800         05 W-IDFKNGRP-FOM       PIC S9(5) COMP-3 VALUE +0.               
017900                                                                          
018000     03  W-IDFKNGRP-TOM-X.                                                
018100         05 W-IDFKNGRP-TOM       PIC S9(5) COMP-3 VALUE +0.               
018200                                                                          
018300                                                                          
018400*    --- STATUS CODES FROM IMS                                            
018500 01  STATUS-WS                   PIC XX.                                  
018600     88  SEGMENT-FOUND                       VALUE '  '.                  
018700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
018800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018900                                                                          
019000 01  GOOD-STATUSCODES.                                                    
019100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019200                                                                          
019300 01  ALL-SSA.                                                             
019400     03 SSA1                     PIC X(64).                               
019500     03 SSA2                     PIC X(64).                               
019600     03 SSA3                     PIC X(64).                               
019700                                                                          
019800                                                                          
019900*    --- IMS FUNCTION CODES                                               
020000*01  -COPY W0003                                                          
020100                                                                          
020200                                                                          
020300*    ---  DLI INPUT-OUTPUT AREA                                           
020400                                                                          
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
020600 01  DLI-IO-WDK601.                                                       
020700*    03  -COPY WDK601                                                     
020800                                                                          
020900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
021000 01  DLI-IO-WDK611.                                                       
021100*    03  -COPY WDK611                                                     
021200                                                                          
021300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK625'.                      
021400 01  DLI-IO-WDK625.                                                       
021500*    03  -COPY WDK625                                                     
021600                                                                          
021700                                                                          
021800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
021900 01  DLI-IO-WDK711.                                                       
022000*    03  -COPY WDK711                                                     
022100                                                                          
022200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
022300 01  DLI-IO-WDK722.                                                       
022400*    03  -COPY WDK722                                                     
022500                                                                          
022600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
022700 01  DLI-IO-WDK712.                                                       
022800*    03  -COPY WDK712                                                     
022900                                                                          
023000                                                                          
023100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2262'.                    
023200 01  DLI-IO-WDGX2262.                                                     
023300*    03  -COPY WDGX2262                                                   
023400                                                                          
023500                                                                          
023600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
023700 01  DLI-IO-WDD311.                                                       
023800*    03  -COPY WDD311                                                     
023900                                                                          
024000                                                                          
024100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN601'.                      
024200 01  DLI-IO-WDN601.                                                       
024300*    03  -COPY WDN601                                                     
024400                                                                          
024500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN611'.                      
024600 01  DLI-IO-WDN611.                                                       
024700*    03  -COPY WDN611                                                     
024800                                                                          
024900                                                                          
025000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
025100 01  DLI-IO-WDF116.                                                       
025200*    03  -COPY WDF116                                                     
025300                                                                          
025400                                                                          
025500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
025600 01  DLI-IO-WDB601.                                                       
025700*    03  -COPY WDB601                                                     
025800                                                                          
025900 01  FILLER              PIC X(16) VALUE 'DLI-IO-WDGX1144'.               
026000 01  DLI-IO-WDGX1144.                                                     
026100*    03  -COPY WDGX1144                                                   
026200                                                                          
026300 LINKAGE SECTION.                                                         
026400*01  -COPY W0009   -PRE MSG-                                              
026500*01  -COPY W0008   -PRE USEA-                                             
026600     05  FILLER                  PIC X.                                   
026700                                                                          
026800*01  -COPY W0008  -PRE WDK6-                                              
026900     05  FILLER                  PIC X.                                   
027000                                                                          
027100*01  -COPY W0008  -PRE WDK7-                                              
027200     05  FILLER                  PIC X.                                   
027300                                                                          
027400*01  -COPY W0008  -PRE 2261-                                              
027500     05  FILLER                  PIC X.                                   
027600                                                                          
027700*01  -COPY W0008  -PRE WDD3-                                              
027800     05  FILLER                  PIC X.                                   
027900                                                                          
028000*01  -COPY W0008  -PRE WDN6-                                              
028100     05  FILLER                  PIC X.                                   
028200                                                                          
028300*01  -COPY W0008  -PRE WDF1-                                              
028400     05  FILLER                  PIC X.                                   
028500                                                                          
028600*01  -COPY W0008  -PRE WDB6-                                              
028700     05  FILLER                  PIC X.                                   
028800                                                                          
028900*01  -COPY W0008  -PRE WDG2-                                              
029000     05  FILLER                  PIC X.                                   
029100                                                                          
029200*01  -COPY W0008  -PRE REFL-2501-                                         
029300     05  FILLER                  PIC X.                                   
029400                                                                          
029500*01  -COPY W0008  -PRE REFL-WDB6-                                         
029600     05  FILLER                  PIC X.                                   
029700                                                                          
029800*01  -COPY W0008  -PRE REFL-WDK7-                                         
029900     05  FILLER                  PIC X.                                   
030000 01  REFL-UTIL-WDK6-PCB          PIC X.                                   
030100 01  REFL-UTIL-WDK7-PCB          PIC X.                                   
030200 01  REFL-UTIL-WDB6-PCB          PIC X.                                   
030300     EJECT                                                                
030400                                                                          
030500*****W271UTUP**********                                                   
030600 01  UTUP1-WDK7-PCB              PIC X.                                   
030700 01  UTUP1-WDB6-PCB              PIC X.                                   
030800 01  UTUP1-UTIL-WDK6-PCB         PIC X.                                   
030900 01  UTUP1-UTIL-WDK7-PCB         PIC X.                                   
031000 01  UTUP1-UTIL-WDB6-PCB         PIC X.                                   
031100     EJECT                                                                
031200                                                                          
031300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK6-PCB WDK7-PCB             
031400     2261-PCB WDD3-PCB WDN6-PCB WDF1-PCB WDB6-PCB WDG2-PCB                
031500     REFL-2501-PCB REFL-WDB6-PCB REFL-WDK7-PCB                            
031600     REFL-UTIL-WDK6-PCB                                                   
031700     REFL-UTIL-WDK7-PCB                                                   
031800     REFL-UTIL-WDB6-PCB                                                   
031900     UTUP1-WDK7-PCB                                                       
032000     UTUP1-WDB6-PCB                                                       
032100     UTUP1-UTIL-WDK6-PCB                                                  
032200     UTUP1-UTIL-WDK7-PCB                                                  
032300     UTUP1-UTIL-WDB6-PCB.                                                 
032400                                                                          
032500 MAIN SECTION.                                                            
032600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK6-PCB WDK7-PCB             
032700     2261-PCB WDD3-PCB WDN6-PCB WDF1-PCB WDB6-PCB WDG2-PCB                
032800     REFL-2501-PCB REFL-WDB6-PCB REFL-WDK7-PCB                            
032900     REFL-UTIL-WDK6-PCB                                                   
033000     REFL-UTIL-WDK7-PCB                                                   
033100     REFL-UTIL-WDB6-PCB                                                   
033200     UTUP1-WDK7-PCB                                                       
033300     UTUP1-WDB6-PCB                                                       
033400     UTUP1-UTIL-WDK6-PCB                                                  
033500     UTUP1-UTIL-WDK7-PCB                                                  
033600     UTUP1-UTIL-WDB6-PCB.                                                 
033700                                                                          
033800     PERFORM IMS-GET-MSG                                                  
033900     IF SEGMENT-FOUND                                                     
034000       PERFORM A-INIT                                                     
034100       PERFORM B-CHECK-KEYS                                               
034200       PERFORM SEC-URITY                                                  
034300       IF KEYS-OK AND USER-AUTHORISED                                     
034400         PERFORM F-READ-SHOW-INFO                                         
034500*---                                                                      
034600*---     SAVE MFG SUPPLIER USING INIT-IO-AREA                             
034700         IF IDLEVNR-WS > SPACES                                           
034800            MOVE ALL '+'           TO MSGI-WMSGINIT                       
034900            MOVE '001'             TO MSGI-KDCALL                         
035000            MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                   
035100            MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                         
035200            MOVE '2401'            TO MSGI-IDTRANS                        
035300            MOVE IDLEVNR-WS        TO MSGI-IDLEVNR                        
035400                                                                          
035500            CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                    
035600         END-IF                                                           
035700                                                                          
035800       END-IF                                                             
035900       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O40101 + 4                      
036000       PERFORM IMS-INSERT-MSG                                             
036100     END-IF                                                               
036200                                                                          
036300     MOVE ZERO              TO RETURN-CODE                                
036400     GOBACK                                                               
036500     .                                                                    
036600                                                                          
036700                                                                          
036800 A-INIT SECTION.                                                          
036900     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
037000                                                                          
037100     IF MSG-DOUBLE-TRANSACTIONS                                           
037200       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I40101                 
037300       MOVE MSG-IDTRANS-2   TO MFS-IDTRANS                                
037400       MOVE MSG-KDMFSFOR-2  TO MFS-KDMFSFOR                               
037500     ELSE                                                                 
037600       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I40101                  
037700       MOVE MSG-IDTRANS-1   TO MFS-IDTRANS                                
037800       MOVE MSG-KDMFSFOR-1  TO MFS-KDMFSFOR                               
037900     END-IF                                                               
038000                                                                          
038100     MOVE MSG-KDTRTYP       TO MFS-KDTRTYP                                
038200     MOVE MSG-IDPFK         TO MFS-IDPFK                                  
038300     MOVE MFS-IDTRANS       TO W-IDTRANS                                  
038400                                                                          
038500     MOVE LOW-VALUE         TO MSG-AREA                                   
038600     MOVE 'W2O401N1'        TO MFS-IDMOD                                  
038700     MOVE '2401'            TO MOD-IDTRANS                                
038800     MOVE MFS-ERASE-FIELD   TO MOD-TEMFSFEL MOD-TEMFSINF                  
038900                                                                          
039000     IF OWN-MID OR HELP-MID                                               
039100       CONTINUE                                                           
039200     ELSE                                                                 
039300       MOVE SPACE           TO MFS-KDTRTYP                                
039400       MOVE '7'             TO MFS-IDPFK                                  
039500     END-IF                                                               
039600     .                                                                    
039700                                                                          
039800                                                                          
039900 B-CHECK-KEYS SECTION.                                                    
040000     MOVE 'B-CHECK-KEYS    ' TO CURRENT-SECTION                           
040100                                                                          
040200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
040300     MOVE '001'             TO MSGI-KDCALL                                
040400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
040500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
040600     MOVE '2401'            TO MSGI-IDTRANS                               
040700                                                                          
040800     MOVE JA                TO KEYS-SW                                    
040900                                                                          
041000     IF OWN-MID                                                           
041100       IF MID-IDARTNR-IN = ALL '+'                                        
041200         IF MID-IDARTNR-UT NUMERIC                                        
041300         AND MID-IDARTNR-UT > ZERO                                        
041400           MOVE MID-IDARTNR-UT                                            
041500                            TO MSGI-IDARTNR                               
041600         END-IF                                                           
041700       ELSE                                                               
041800         IF MID-IDARTNR-IN NUMERIC                                        
041900         AND MID-IDARTNR-IN > ZERO                                        
042000           MOVE MID-IDARTNR-IN                                            
042100                            TO MSGI-IDARTNR                               
042200         ELSE                                                             
042300           MOVE ERR-PART-NOT-NUMERIC                                      
042400                            TO MED-IDMFSFEL                               
042500           MOVE NOO         TO KEYS-SW                                    
042600         END-IF                                                           
042700       END-IF                                                             
042800       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
042900     ELSE                                                                 
043000       IF MID-IDARTNR-IN NUMERIC                                          
043100       AND MID-IDARTNR-IN > ZERO                                          
043200         MOVE MID-IDARTNR-IN                                              
043300                            TO MSGI-IDARTNR                               
043400       END-IF                                                             
043500     END-IF                                                               
043600                                                                          
043700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
043800     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
043900                                                                          
044000*    -- LANGUAGE TO BE USED BY MEDKONV                                    
044100     MOVE 'GB'              TO MED-IDSKYLT                                
044200                               W-IDSKYLT                                  
044300                                                                          
044400*    -- CHECK OF IDARTNR                                                  
044500     MOVE MFS-ERASE-FIELD   TO MOD-IDARTNR-IN                             
044600                                                                          
044700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
044800       MOVE '7'             TO MFS-IDPFK                                  
044900       MOVE SPACE           TO MFS-KDTRTYP                                
045000     END-IF                                                               
045001                                                                          
045010     IF MSGI-IDARTNR NUMERIC AND MSGI-IDARTNR > ZERO                      
045100        MOVE MSGI-IDARTNR      TO W-IDARTNR                               
045200                                  MOD-IDARTNR-UT                          
045210     END-IF                                                               
045220                                                                          
045300     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
045400                                                                          
045500*    -- CHECK OF IDDC                                                     
045600     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
045700                                                                          
045800     IF MID-IDDC-IN NOT = ALL '+'                                         
045900       MOVE '7'             TO MFS-IDPFK                                  
046000       MOVE SPACE           TO MFS-KDTRTYP                                
046100     END-IF                                                               
046200     MOVE MSGI-IDDC-KEY     TO W-IDDC                                     
046300                               MOD-IDDC-UT                                
046400                                                                          
046500     PERFORM IMS-GU-WDB601                                                
046600                                                                          
046700     IF SEGMENT-MISSING                                                   
046800        MOVE ERR-INVALID-DC TO MED-IDMFSFEL                               
046900        MOVE NOO            TO KEYS-SW                                    
047000     ELSE                                                                 
047100        IF DCS-NDC-CN                                                     
047200        OR (DCS-NDC-NA AND DCS-USA)                                       
047300          MOVE DCS-IDLANDX2    TO W-IDLAND                                
047400        ELSE                                                              
047500          MOVE NOO             TO KEYS-SW                                 
047600          MOVE ERR-INVALID-DC  TO MED-IDMFSFEL                            
047700        END-IF                                                            
047800     END-IF                                                               
047900                                                                          
048000     IF KEYS-WRONG                                                        
048100       CALL WMEDKONV USING MED-WMEDAREA                                   
048200       MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                               
048300       PERFORM MFS-ERASE-FIELD-OUT                                        
048400     END-IF                                                               
048500                                                                          
048600     IF MID-BLADDRING-ANT NOT NUMERIC                                     
048700     OR (NOT OWN-MID)                                                     
048800        MOVE ZERO           TO MID-BLADDRING-ANT                          
048900     END-IF                                                               
049000     .                                                                    
049100                                                                          
049200                                                                          
049300 F-READ-SHOW-INFO SECTION.                                                
049400     MOVE 'F-READ-SHOW-INFO' TO CURRENT-SECTION                           
049500                                                                          
049600     IF ART-KDERS-UTG > ZERO                                              
049700       IF ART-KDERS-UTG = +29 OR +52                                      
049800         MOVE INF-PART-EXPIRED                                            
049900                            TO MED-IDMFSINF                               
050000       ELSE                                                               
050100         MOVE INF-PART-SUPERSEDED                                         
050200                            TO MED-IDMFSINF                               
050300       END-IF                                                             
050400       CALL WMEDKONV USING MED-WMEDAREA                                   
050500       MOVE MED-TEMFSINF    TO MOD-TEMFSINF                               
050600     ELSE                                                                 
050700       IF ART-FLERS = JA                                                  
050800         MOVE INF-REPLACING-PART                                          
050900                            TO MED-IDMFSINF                               
051000         CALL WMEDKONV USING MED-WMEDAREA                                 
051100         MOVE MED-TEMFSINF  TO MOD-TEMFSINF                               
051200       END-IF                                                             
051300       PERFORM FA-READ-SHOW-WDK6                                          
051400       PERFORM FB-READ-SHOW-WDK7                                          
051500       PERFORM FC-READ-SHOW-WDD3                                          
051600       PERFORM FD-READ-SHOW-WDR2                                          
051700       PERFORM FE-READ-SHOW-WDN6                                          
051800       IF W-IDLEVNR NOT = SPACE                                           
051900          PERFORM FF-READ-SHOW-WDF1                                       
052000       END-IF                                                             
052100     END-IF                                                               
052200     .                                                                    
052300                                                                          
052400                                                                          
052500 FA-READ-SHOW-WDK6 SECTION.                                               
052600     MOVE 'FA-READ-SHOW-K6 ' TO CURRENT-SECTION                           
052700                                                                          
052800     MOVE ART-KDERS-UTG     TO MOD-KDERS-UTG                              
052900     MOVE ART-REKSIFFR      TO MOD-REKSIFFR                               
053000     MOVE HYPHEN            TO MOD-STRECK                                 
053100     MOVE ART-IDFKNGRP      TO MOD-IDFKNGRP                               
053200     MOVE ART-KDSORT        TO MOD-KDSORT                                 
053300     MOVE ART-TIURPROD      TO MOD-TIURPROD                               
053400     MOVE ART-TIFINLV       TO MOD-DAPUBLW                                
053500                                                                          
053600     MOVE +1                TO IX                                         
053700     SET MOD-IX             TO 1                                          
053800     PERFORM UNTIL IX > MAX-ANT-AO-NUMMER                                 
053900         MOVE ART-IDAO(IX)  TO MOD-IDAO(MOD-IX)                           
054000         ADD 1              TO IX                                         
054100         SET MOD-IX UP BY 1                                               
054200     END-PERFORM                                                          
054300                                                                          
054400     PERFORM IMS-GU-WDK711                                                
054500     IF SEGMENT-FOUND                                                     
054600* -- ONLY SHOW CDC-INFO IF PART EXIST FOR GIVEN DC                        
054700        PERFORM IMS-GU-WDK611                                             
054800        IF SEGMENT-FOUND                                                  
054900           PERFORM FAA-SHOW-WDK611                                        
055000           PERFORM FAB-SHOW-WDK625                                        
055100        ELSE                                                              
055200           MOVE ERR-PART-NOT-FOUND                                        
055300                               TO MED-IDMFSFEL                            
055400           CALL WMEDKONV USING MED-WMEDAREA                               
055500           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
055600           PERFORM MFS-ERASE-FIELD-OUT                                    
055700        END-IF                                                            
055800     ELSE                                                                 
055900        MOVE ERR-PART-NOT-FOUND                                           
056000                            TO MED-IDMFSFEL                               
056100        CALL WMEDKONV USING MED-WMEDAREA                                  
056200        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
056300        PERFORM MFS-ERASE-FIELD-OUT                                       
056400     END-IF                                                               
056500     .                                                                    
056600                                                                          
056700                                                                          
056800 FAA-SHOW-WDK611 SECTION.                                                 
056900     MOVE 'FAA-SHOW-WDK611 ' TO CURRENT-SECTION                           
057000                                                                          
057100     MOVE CLAG-IDBERED      TO MOD-IDBERED                                
057200     MOVE CLAG-IDKAT(1)     TO MOD-IDKAT-1                                
057300     MOVE CLAG-IDKAT(2)     TO MOD-IDKAT-2                                
057400     MOVE CLAG-IDKAT(3)     TO MOD-IDKAT-3                                
057500     MOVE CLAG-IDPROJ       TO MOD-IDPROJ                                 
057600     IF CLAG-FLLSRDEL = JA                                                
057700       MOVE YES             TO MOD-FLLSRDEL                               
057800     ELSE                                                                 
057900       MOVE CLAG-FLLSRDEL   TO MOD-FLLSRDEL                               
058000     END-IF                                                               
058100     MOVE CLAG-KDFARLIG     TO MOD-KDFARLIG                               
058200                                                                          
058300     PERFORM FAC-RED-MOD-KVAARLF                                          
058400                                                                          
058500     MOVE 1                 TO IX                                         
058600     PERFORM UNTIL (IX > MAX-ANT-PROENH)                                  
058700         MOVE CLAG-IDPROENH (IX)                                          
058800                            TO MOD-IDPROENH (IX)                          
058900         INSPECT MOD-IDPROENH (IX) REPLACING LEADING ZERO BY SPACE        
059000         ADD 1              TO IX                                         
059100     END-PERFORM                                                          
059200     .                                                                    
059300                                                                          
059400                                                                          
059500 FAB-SHOW-WDK625 SECTION.                                                 
059600     MOVE 'FAB-SHOW-WDK625 ' TO CURRENT-SECTION                           
059700                                                                          
059800     MOVE +1                 TO W-KDNOTTYP                                
059900     PERFORM IMS-GNP-WDK625                                               
060000     IF SEGMENT-FOUND                                                     
060100        MOVE NOT-TEARTNOT    TO MOD-PLANOT                                
060200     END-IF                                                               
060300                                                                          
060400     MOVE +6                 TO W-KDNOTTYP                                
060500     PERFORM IMS-GNP-WDK625                                               
060600     IF SEGMENT-FOUND                                                     
060700        MOVE NOT-TEARTNOT    TO MOD-VARNOT                                
060800     END-IF                                                               
060900     .                                                                    
061000                                                                          
061100 FAC-RED-MOD-KVAARLF SECTION.                                             
061200                                                                          
061300     MOVE ART-IDFKNGRP            TO W-IDFKNGRP-FOM                       
061400                                     W-IDFKNGRP-TOM                       
061500     PERFORM IMS-GU-WDGX1144                                              
061600     IF SEGMENT-FOUND                                                     
061700        MOVE 1144-KVAARLF         TO MOD-KVAARLF                          
061800     ELSE                                                                 
061900        MOVE 15                   TO MOD-KVAARLF                          
062000     END-IF                                                               
062100     .                                                                    
062200                                                                          
062300                                                                          
062400 FB-READ-SHOW-WDK7  SECTION.                                              
062500     MOVE 'FB-READ-SHOW-K7 ' TO CURRENT-SECTION                           
062600                                                                          
062700     MOVE NOO                  TO WS-WDK711-EXIST                         
062800                                                                          
062900     PERFORM IMS-GU-WDK711                                                
063000     IF SEGMENT-FOUND                                                     
063100        MOVE SLAG-IDLEVNR      TO MOD-IDLEVNR                             
063200                                  IDLEVNR-WS                              
063300        MOVE SLAG-KVREFBER     TO MOD-KVREFBER                            
063400        MOVE SLAG-TIREFPAF     TO MOD-TIREFPAF                            
063500        IF SLAG-FLWILSON = JA                                             
063600          MOVE YES             TO MOD-FLWILSON                            
063700        ELSE                                                              
063800          MOVE SLAG-FLWILSON   TO MOD-FLWILSON                            
063900        END-IF                                                            
064000        MOVE SLAG-IDREFTAB     TO MOD-IDREFTAB                            
064100        MOVE JA                TO WS-WDK711-EXIST                         
064200     END-IF                                                               
064300                                                                          
064400     PERFORM IMS-GU-WDK712                                                
064500     IF SEGMENT-FOUND                                                     
064600       MOVE LART-KDARTURS      TO MOD-KDARTURS                            
064700       MOVE LART-DAPUBL        TO WS-INDATE                               
064800       MOVE LART-KVDAGAR-INLEV TO MOD-KVDAGAR-INL                         
064900       IF WS-WDK711-EXIST = JA                                            
065000          PERFORM FBA-READ-SHOW-W271REFL                                  
065100       END-IF                                                             
065200     ELSE                                                                 
065300       MOVE SPACE              TO MOD-KDARTURS                            
065400       MOVE ZERO               TO MOD-KVDAGAR-INL                         
065500       MOVE ZERO               TO WS-INDATE                               
065600     END-IF                                                               
065700                                                                          
065800                                                                          
065900     IF WS-INDATE-6 > ZERO                                                
066000        MOVE 'AAMMDD'          TO DAT-KDDATFORM                           
066100        MOVE WS-INDATE-6       TO DAT-I-TIDATUM                           
066200                                                                          
066300        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
066400                            DAT-O-TIDATUM DAT-KDSVAR                      
066500                                                                          
066600        IF DAT-KDSVAR-OK                                                  
066700           MOVE DAT-TIAAVVD    TO MOD-DALPUBLW                            
066800        ELSE                                                              
066900           STRING ' ERROR FROM DATE ROUTINE WDATKONV '                    
067000           DELIMITED BY SIZE INTO ERROR-TEXT                              
067100           CALL FELLOG                                                    
067200        END-IF                                                            
067300     ELSE                                                                 
067400        MOVE ZERO              TO MOD-DALPUBLW                            
067500     END-IF                                                               
067600                                                                          
067700     PERFORM IMS-GU-WDK722                                                
067800     IF SEGMENT-FOUND                                                     
067900        MOVE XLAG-IDINK        TO MOD-IDINK                               
068000        MOVE XLAG-KDAVT        TO MOD-KDAVT                               
068100        MOVE XLAG-KVSLAGER     TO MOD-KVSLAGER                            
068200        MOVE XLAG-TIMANSEC     TO MOD-TIMANSEC                            
068300        MOVE XLAG-KVEOQ        TO MOD-KVEOQ                               
068400        IF XLAG-FLJIT = JA                                                
068500          MOVE YES             TO MOD-FLJIT                               
068600        ELSE                                                              
068700          MOVE XLAG-FLJIT      TO MOD-FLJIT                               
068800        END-IF                                                            
068900        MOVE XLAG-KVVECKOR-LT  TO MOD-KVVECKOR-LT                         
069000        MOVE XLAG-TIMANLED     TO MOD-TIMANLED                            
069100        MOVE XLAG-KVPALL       TO MOD-KVPALL                              
069200        MOVE XLAG-KVULOAD      TO MOD-KVULOAD                             
069300        MOVE XLAG-IDANSK       TO MOD-IDANSK                              
069400        MOVE XLAG-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP                        
069500                                  W-IDLEVNR                               
069600        MOVE XLAG-IDPLANGR-AG  TO MOD-IDPLANGR-AG                         
069700        MOVE XLAG-TIREFSTO-LOC TO MOD-TIREFSTO                            
069800     ELSE                                                                 
069900        MOVE SPACE             TO MOD-IDINK                               
070000                                  MOD-FLJIT                               
070100                                  MOD-IDLEVNR-SHIP                        
070200                                  W-IDLEVNR                               
070300        MOVE ZERO              TO MOD-KDAVT                               
070400                                  MOD-KVSLAGER                            
070500                                  MOD-TIMANSEC                            
070600                                  MOD-KVEOQ                               
070700                                  MOD-KVVECKOR-LT                         
070800                                  MOD-TIMANLED                            
070900                                  MOD-KVPALL                              
071000                                  MOD-KVULOAD                             
071100                                  MOD-IDANSK                              
071200                                  MOD-IDPLANGR-AG                         
071300                                  MOD-KVDAGAR-INL                         
071400                                  MOD-TIREFSTO                            
071500     END-IF                                                               
071600     .                                                                    
071700                                                                          
071800                                                                          
071900 FBA-READ-SHOW-W271REFL SECTION.                                          
072000     MOVE 'FBA-SHOW-W271REF' TO CURRENT-SECTION                           
072100                                                                          
072200     INITIALIZE REFL-W271REFL                                             
072300                                                                          
072400     MOVE W-IDDC                     TO REFL-IDDC                         
072500     MOVE W-IDARTNR                  TO REFL-IDARTNR                      
072600     MOVE SLAG-IDDC-REF              TO REFL-IDDC-REF                     
072700     MOVE SLAG-IDREFTAB              TO REFL-IDREFTAB                     
072800     MOVE LART-PRMATRL               TO REFL-PRARTBES                     
072900                                                                          
073000     INITIALIZE UTUP-W271UTUP                                             
073100     MOVE 004                        TO UTUP-KDCALL                       
073200     MOVE W-IDARTNR                  TO UTUP-IDARTNR                      
073300     MOVE W-IDDC                     TO UTUP-IDDC                         
073400     MOVE SLAG-IDDC-REF              TO UTUP-IDDC-REF                     
073500                                                                          
073600     CALL W271UTUP USING UTUP-W271UTUP                                    
073700                         UTUP1-WDK7-PCB                                   
073800                         UTUP1-WDB6-PCB                                   
073900                         UTUP1-UTIL-WDK6-PCB                              
074000                         UTUP1-UTIL-WDK7-PCB                              
074100                         UTUP1-UTIL-WDB6-PCB                              
074200     IF UTUP-KDSVAR-OK                                                    
074300        MOVE UTUP-LEADTID-BEHOV      TO REFL-IN-LEADTID-BEHOV             
074400     ELSE                                                                 
074500        DISPLAY 'W271UTUP-ERROR :' UTUP-TEXT                              
074600        CALL FELLOG                                                       
074700     END-IF                                                               
074800                                                                          
074900     CALL W271REFL USING REFL-W271REFL REFL-2501-PCB                      
075000                                       REFL-WDB6-PCB                      
075100                                       REFL-WDK7-PCB                      
075200                                       REFL-UTIL-WDK6-PCB                 
075300                                       REFL-UTIL-WDK7-PCB                 
075400                                       REFL-UTIL-WDB6-PCB                 
075500     MOVE REFL-KLASS (2:1)           TO MOD-KDPRISKL                      
075600     MOVE REFL-KLASS (3:1)           TO MOD-KDFREKKL                      
075700     .                                                                    
075800                                                                          
075900                                                                          
076000 FC-READ-SHOW-WDD3  SECTION.                                              
076100     MOVE 'FC-READ-SHOW-D3 ' TO CURRENT-SECTION                           
076200                                                                          
076300     PERFORM IMS-GU-WDD311                                                
076400     IF SEGMENT-FOUND                                                     
076500        MOVE TEXT-BEART     TO MOD-BEART-SVE                              
076600     ELSE                                                                 
076700        MOVE 'UNKNOWN'      TO MOD-BEART-SVE                              
076800     END-IF                                                               
076900     .                                                                    
077000                                                                          
077100                                                                          
077200 FD-READ-SHOW-WDR2  SECTION.                                              
077300     MOVE 'FD-READ-SHOW-R2 ' TO CURRENT-SECTION                           
077400                                                                          
077500     MOVE W-IDDC             TO W-IDDC-2261                               
077600     PERFORM IMS-GU-WDGX2262                                              
077700     IF SEGMENT-FOUND                                                     
077800        MOVE 2262-TEREFMED (1:36)                                         
077900                             TO MOD-TEARTNOT (1)                          
078000        MOVE 2262-TEREFMED (37:36)                                        
078100                             TO MOD-TEARTNOT (2)                          
078200     ELSE                                                                 
078300        MOVE MFS-ERASE-FIELD TO MOD-TEARTNOT (1)                          
078400                                MOD-TEARTNOT (2)                          
078500     END-IF                                                               
078600     .                                                                    
078700                                                                          
078800                                                                          
078900 FE-READ-SHOW-WDN6  SECTION.                                              
079000     MOVE 'FE-READ-SHOW-N6 ' TO CURRENT-SECTION                           
079100                                                                          
079200     PERFORM IMS-GU-WDN601                                                
079300     IF SEGMENT-FOUND                                                     
079400        PERFORM IMS-GNP-WDN611                                            
079500        IF MID-BLADDRING-ANT = ZERO                                       
079600           CONTINUE                                                       
079700        ELSE                                                              
079800           SET MOD-IZ       TO +1                                         
079900           MOVE MID-BLADDRING-ANT                                         
080000                            TO LAST-SEG                                   
080100           PERFORM UNTIL MOD-IZ > LAST-SEG OR SEGMENT-MISSING             
080200              PERFORM IMS-GNP-WDN611                                      
080300              SET MOD-IZ  UP BY +1                                        
080400           END-PERFORM                                                    
080500        END-IF                                                            
080600        SET MOD-IZ          TO +1                                         
080700        PERFORM UNTIL MOD-IZ > 11 OR SEGMENT-MISSING                      
080800           MOVE KAT-BEEMBLEM                                              
080900                            TO MOD-IDKAT (MOD-IZ)                         
081000           ADD +1           TO LAST-SEG                                   
081100           PERFORM IMS-GNP-WDN611                                         
081200           SET MOD-IZ UP BY +1                                            
081300        END-PERFORM                                                       
081400        IF SEGMENT-FOUND                                                  
081500           MOVE LAST-SEG    TO MOD-BLADDRING-ANT                          
081600           MOVE INF-PRESS-ENTER                                           
081700                            TO MED-IDMFSINF                               
081800           CALL WMEDKONV USING MED-WMEDAREA                               
081900           MOVE MED-TEMFSINF                                              
082000                            TO MOD-TEMFSINF                               
082100        ELSE                                                              
082200           MOVE ZERO        TO MOD-BLADDRING-ANT                          
082300        END-IF                                                            
082400     END-IF                                                               
082500     .                                                                    
082600                                                                          
082700                                                                          
082800 FF-READ-SHOW-WDF1  SECTION.                                              
082900     MOVE 'FF-READ-SHOW-F1 ' TO CURRENT-SECTION                           
083000                                                                          
083100     PERFORM IMS-GU-WDF116                                                
083200     IF SEGMENT-FOUND                                                     
083300        MOVE NDC-KVDAGAR-TT TO MOD-KVDAGAR-TT                             
083400     ELSE                                                                 
083500        MOVE ZERO           TO MOD-KVDAGAR-TT                             
083600     END-IF                                                               
083700     .                                                                    
083800                                                                          
083900                                                                          
084000 SEC-URITY SECTION.                                                       
084100     MOVE 'SEC-URITY       ' TO CURRENT-SECTION                           
084200                                                                          
084300     MOVE NOO               TO SECURITY-SW                                
084400                                                                          
084500     PERFORM IMS-GU-WDK601                                                
084600     IF SEGMENT-FOUND                                                     
084700        MOVE ART-IDLEVNR    TO WS-IDLEVNR-8                               
084800        IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                         
084900        OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                   
085000           MOVE JA          TO SECURITY-SW                                
085100        ELSE                                                              
085200           MOVE ERR-USER-NOT-AUTHORISED                                   
085300                            TO MED-IDMFSFEL                               
085400           CALL WMEDKONV USING MED-WMEDAREA                               
085500           MOVE MED-TEMFSFEL                                              
085600                            TO MOD-TEMFSFEL                               
085700           PERFORM MFS-ERASE-FIELD-OUT                                    
085800        END-IF                                                            
085900     ELSE                                                                 
086000        MOVE ERR-PART-NOT-FOUND                                           
086100                            TO MED-IDMFSFEL                               
086200        CALL WMEDKONV USING MED-WMEDAREA                                  
086300        MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                               
086400        PERFORM MFS-ERASE-FIELD-OUT                                       
086500     END-IF                                                               
086600     .                                                                    
086700                                                                          
086800                                                                          
086900 MFS-ERASE-FIELD-OUT SECTION.                                             
087000                                                                          
087100*    --- ALL OUTPUT FIELDS                                                
087200     MOVE MFS-ERASE-FIELD   TO MOD-IDANSK                                 
087300                               MOD-IDLEVNR-SHIP                           
087400                               MOD-IDFKNGRP                               
087500                               MOD-IDBERED                                
087600                               MOD-KVAARLF                                
087700                               MOD-IDPLANGR-AG                            
087800                               MOD-IDLEVNR                                
087900                               MOD-IDPROJ                                 
088000                               MOD-IDINK                                  
088100                               MOD-DAPUBLW                                
088200                               MOD-KDSORT                                 
088300                               MOD-KDAVT                                  
088400                               MOD-KVSLAGER                               
088500                               MOD-TIMANSEC                               
088600                               MOD-DALPUBLW                               
088700                               MOD-KDERS-UTG                              
088800                               MOD-KDARTURS                               
088900                               MOD-KVEOQ                                  
089000                               MOD-TIURPROD                               
089100                               MOD-FLLSRDEL                               
089200                               MOD-FLJIT                                  
089300                               MOD-KVREFBER                               
089400                               MOD-TIREFPAF                               
089500                               MOD-KVVECKOR-LT                            
089600                               MOD-TIMANLED                               
089700                               MOD-KDFARLIG                               
089800                               MOD-FLWILSON                               
089900                               MOD-KVPALL                                 
090000                               MOD-KVDAGAR-INL                            
090100                               MOD-KDFREKKL                               
090200                               MOD-KDPRISKL                               
090300                               MOD-IDREFTAB                               
090400                               MOD-KVULOAD                                
090500                               MOD-KVDAGAR-TT                             
090600                               MOD-TIREFSTO                               
090700                               MOD-IDKAT-1                                
090800                               MOD-IDKAT-2                                
090900                               MOD-IDKAT-3                                
091000                               MOD-IDPROENH (1)                           
091100                               MOD-IDPROENH (2)                           
091200                               MOD-IDPROENH (3)                           
091300                               MOD-IDKAT (1)                              
091400                               MOD-IDKAT (2)                              
091500                               MOD-IDKAT (3)                              
091600                               MOD-IDKAT (4)                              
091700                               MOD-IDKAT (5)                              
091800                               MOD-IDKAT (6)                              
091900                               MOD-IDKAT (7)                              
092000                               MOD-IDKAT (8)                              
092100                               MOD-IDKAT (9)                              
092200                               MOD-IDKAT (10)                             
092300                               MOD-IDKAT (11)                             
092400                               MOD-IDAO (1)                               
092500                               MOD-IDAO (2)                               
092600                               MOD-IDAO (3)                               
092700                               MOD-IDAO (4)                               
092800                               MOD-IDAO (5)                               
092900                               MOD-VARNOT                                 
093000                               MOD-PLANOT                                 
093100                               MOD-TEARTNOT (1)                           
093200                               MOD-TEARTNOT (2)                           
093300     .                                                                    
093400                                                                          
093500                                                                          
093600* --- IMS SECTIONS ---                                                    
093700                                                                          
093800 IMS-GET-MSG SECTION.                                                     
093900                                                                          
094000     MOVE '  QC' TO GOOD-STATUSCODES                                      
094100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
094200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094300     PERFORM IMS-STATUSCHECK                                              
094400     .                                                                    
094500                                                                          
094600                                                                          
094700 IMS-INSERT-MSG SECTION.                                                  
094800                                                                          
094900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
095000     MOVE SPACE TO GOOD-STATUSCODES                                       
095100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
095200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
095300     PERFORM IMS-STATUSCHECK                                              
095400     .                                                                    
095500                                                                          
095600                                                                          
095700 IMS-GU-WDK601 SECTION.                                                   
095800     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
095900                                                                          
096000     MOVE SPACE                 TO ALL-SSA                                
096100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
096200            DELIMITED BY SIZE INTO SSA1                                   
096300     MOVE '  GE'                TO GOOD-STATUSCODES                       
096400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
096500     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
096600     PERFORM IMS-STATUSCHECK                                              
096700     .                                                                    
096800                                                                          
096900                                                                          
097000 IMS-GU-WDK611 SECTION.                                                   
097100     MOVE 'IMS-GU-WDK611   ' TO CURRENT-IMS-SECTION                       
097200                                                                          
097300     MOVE SPACE                 TO ALL-SSA                                
097400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
097500            DELIMITED BY SIZE INTO SSA1                                   
097600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
097700          DELIMITED BY SIZE INTO SSA2                                     
097800     MOVE '  GE'              TO GOOD-STATUSCODES                         
097900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
098000     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
098100     PERFORM IMS-STATUSCHECK                                              
098200     .                                                                    
098300                                                                          
098400                                                                          
098500 IMS-GNP-WDK625 SECTION.                                                  
098600     MOVE 'IMS-GNP-WDK625  ' TO CURRENT-IMS-SECTION                       
098700                                                                          
098800     MOVE SPACE                 TO ALL-SSA                                
098900     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
099000            DELIMITED BY SIZE INTO SSA1                                   
099100     MOVE '  GE'                TO GOOD-STATUSCODES                       
099200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK625 SSA1                   
099300     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
099400     PERFORM IMS-STATUSCHECK                                              
099500     .                                                                    
099600                                                                          
099700                                                                          
099800 IMS-GU-WDK711 SECTION.                                                   
099900     MOVE 'IMS-GU-WDK711   ' TO CURRENT-IMS-SECTION                       
100000                                                                          
100100     MOVE SPACE                 TO ALL-SSA                                
100200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
100300            DELIMITED BY SIZE INTO SSA1                                   
100400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
100500            DELIMITED BY SIZE INTO SSA2                                   
100600     MOVE '  GE'                TO GOOD-STATUSCODES                       
100700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
100800     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
100900     PERFORM IMS-STATUSCHECK                                              
101000     .                                                                    
101100                                                                          
101200                                                                          
101300 IMS-GU-WDK722 SECTION.                                                   
101400     MOVE 'IMS-GU-WDK722   ' TO CURRENT-IMS-SECTION                       
101500                                                                          
101600     MOVE SPACE                 TO ALL-SSA                                
101700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
101800            DELIMITED BY SIZE INTO SSA1                                   
101900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
102000            DELIMITED BY SIZE INTO SSA2                                   
102100     MOVE 'WDK722 '           TO SSA3                                     
102200     MOVE '  GE'              TO GOOD-STATUSCODES                         
102300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
102400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
102500     PERFORM IMS-STATUSCHECK                                              
102600     .                                                                    
102700                                                                          
102800                                                                          
102900 IMS-GU-WDK712 SECTION.                                                   
103000     MOVE 'IMS-GU-WDK712   ' TO CURRENT-IMS-SECTION                       
103100                                                                          
103200     MOVE SPACE                 TO ALL-SSA                                
103300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
103400            DELIMITED BY SIZE INTO SSA1                                   
103500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
103600            DELIMITED BY SIZE INTO SSA2                                   
103700     MOVE '  GE'                TO GOOD-STATUSCODES                       
103800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
103900     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
104000     PERFORM IMS-STATUSCHECK                                              
104100     .                                                                    
104200                                                                          
104300                                                                          
104400 IMS-GU-WDGX2262 SECTION.                                                 
104500                                                                          
104600     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2261-X ')'                    
104700          DELIMITED BY SIZE INTO SSA1                                     
104800     STRING 'WDGX2262(IDARTNR  =' W-IDARTNR-X ')'                         
104900          DELIMITED BY SIZE INTO SSA2                                     
105000     MOVE '  GE' TO GOOD-STATUSCODES                                      
105100     CALL CBLTDLI USING GU 2261-PCB DLI-IO-WDGX2262 SSA1 SSA2             
105200     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
105300     PERFORM IMS-STATUSCHECK                                              
105400     .                                                                    
105500     SKIP3                                                                
105600                                                                          
105700 IMS-GU-WDD311 SECTION.                                                   
105800     MOVE 'IMS-GU-WDD311   ' TO CURRENT-IMS-SECTION                       
105900                                                                          
106000     MOVE SPACE                 TO ALL-SSA                                
106100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
106200            DELIMITED BY SIZE INTO SSA1                                   
106300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
106400            DELIMITED BY SIZE INTO SSA2                                   
106500     MOVE '  GE'                TO GOOD-STATUSCODES                       
106600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
106700     MOVE WDD3-STATUS-CODE      TO STATUS-WS                              
106800     PERFORM IMS-STATUSCHECK                                              
106900     .                                                                    
107000                                                                          
107100                                                                          
107200 IMS-GU-WDN601 SECTION.                                                   
107300     MOVE 'IMS-GU-WDN601   ' TO CURRENT-IMS-SECTION                       
107400                                                                          
107500     MOVE SPACE                 TO ALL-SSA                                
107600     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
107700            DELIMITED BY SIZE INTO SSA1                                   
107800     MOVE '  GE'                TO GOOD-STATUSCODES                       
107900     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
108000     MOVE WDN6-STATUS-CODE      TO STATUS-WS                              
108100     PERFORM IMS-STATUSCHECK                                              
108200     .                                                                    
108300                                                                          
108400                                                                          
108500 IMS-GNP-WDN611 SECTION.                                                  
108600     MOVE 'IMS-GNP-WDN611  ' TO CURRENT-IMS-SECTION                       
108700                                                                          
108800     MOVE SPACE             TO ALL-SSA                                    
108900     MOVE 'WDN611   '       TO SSA1                                       
109000     MOVE '  GE' TO GOOD-STATUSCODES                                      
109100     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
109200     MOVE WDN6-STATUS-CODE  TO STATUS-WS                                  
109300     PERFORM IMS-STATUSCHECK                                              
109400     .                                                                    
109500                                                                          
109600                                                                          
109700 IMS-GU-WDF116 SECTION.                                                   
109800     MOVE 'IMS-GU-WDF116   ' TO CURRENT-IMS-SECTION                       
109900                                                                          
110000     MOVE SPACE                 TO ALL-SSA                                
110100     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
110200            DELIMITED BY SIZE INTO SSA1                                   
110300     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
110400            DELIMITED BY SIZE INTO SSA2                                   
110500     MOVE '  GE'                TO GOOD-STATUSCODES                       
110600     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
110700     MOVE WDF1-STATUS-CODE      TO STATUS-WS                              
110800     PERFORM IMS-STATUSCHECK                                              
110900     .                                                                    
111000                                                                          
111100                                                                          
111200 IMS-GU-WDB601 SECTION.                                                   
111300     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
111400                                                                          
111500     MOVE SPACE                 TO ALL-SSA                                
111600     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
111700            DELIMITED BY SIZE INTO SSA1                                   
111800     MOVE '  GE'                TO GOOD-STATUSCODES                       
111900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
112000     MOVE WDB6-STATUS-CODE      TO STATUS-WS                              
112100     PERFORM IMS-STATUSCHECK                                              
112200     .                                                                    
112300                                                                          
112400 IMS-GU-WDGX1144 SECTION.                                                 
112500                                                                          
112600     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-1143-X ')'                    
112700            DELIMITED BY SIZE INTO SSA1                                   
112800     STRING 'WDGX1144(IDFKNGRF=<' W-IDFKNGRP-FOM-X                        
112900                    '&IDFKNGRT=>' W-IDFKNGRP-TOM-X ')'                    
113000          DELIMITED BY SIZE  INTO SSA2                                    
113100     MOVE '  GE'               TO GOOD-STATUSCODES                        
113200     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDGX1144 SSA1 SSA2             
113300     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
113400     PERFORM IMS-STATUSCHECK                                              
113500     .                                                                    
113600                                                                          
113700                                                                          
113800 IMS-STATUSCHECK SECTION.                                                 
113900                                                                          
114000     SET STATUS-IX TO 1                                                   
114100     SEARCH GOOD-STATUS                                                   
114200       AT END                                                             
114300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
114400         DELIMITED BY SIZE INTO ERROR-TEXT                                
114500         CALL FELLOG                                                      
114600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
114700         CONTINUE                                                         
114800     END-SEARCH                                                           
114900     .                                                                    
