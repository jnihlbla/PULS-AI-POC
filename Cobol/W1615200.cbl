000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1615200.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   10/05/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        TO UPDATE PART NUMERS FOR BIMA                                   
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDK6                                       
001200*        THE PROGRAM READS     WDF1                                       
001300*                                                                         
001400*        PROGRAM DOES PROG-TO-PROG SWITCH W0T107X, FOR UPDATING           
001500*        OF CROSS INDEX (WDF5)                                            
001600*                                                                         
001700*        PROGRAM CREATES 985 POSTER FILE FOR UPDATING OF PRICE            
001800*        IN PULS                                                          
001900*                                                                         
002000*        PROGRAMMET SKICKAR DISPATCHER-TRANS TILL 1112-BILDEN             
002100*        VIA WDP8 FÖR FÖR UPPDATERING AV BENÄMNING                        
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000*          --- INPUT FILE FROM BIMA                                       
003100     SELECT W16152                     ASSIGN TO W16152D1.                
003200*          --- OUTPUT FILE TO VCOM                                        
003300     SELECT W16153                     ASSIGN TO W16152D2.                
003400*          --- 905 POSTER FILE TO UPDATE PRICE                            
003500     SELECT W16154                     ASSIGN TO W16152D3.                
003600*                                                                         
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W16152                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  -COPY W1615001                                                       
004600     SKIP3                                                                
004700 FD  W16153                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  POST -COPY W55310    -PRE  UT-   -L.                                 
005200     SKIP3                                                                
005300 FD  W16154                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700 01  WS-REC           PIC X(100).                                         
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000                                                                          
006100 77  IDPGM                       PIC X(8)    VALUE 'W1615200'.            
006200 01  CHKP-VAR.                                                            
006300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006800     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
006900 77  YES                         PIC X       VALUE 'J'.                   
007000 77  NOO                         PIC X       VALUE 'N'.                   
007010 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
007100     SKIP2                                                                
007200 01  ERROR-TEXT.                                                          
007300     03  FILLER                  PIC X(8)    VALUE 'ERR-TXT'.             
007400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007500                                                                          
007600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
007700 77  W16152-EOF-SW               PIC X       VALUE 'N'.                   
007800     88  END-OF-W16152                       VALUE 'Y'.                   
007900     EJECT                                                                
008000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
008100 01  FILLER REDEFINES TODAYS-DATE.                                        
008200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
008300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
008400     03  TODAYS-DATE-DAY         PIC 9(2).                                
008500     EJECT                                                                
008600*                                                                         
008700 01 W-ERR-DESC.                                                           
008800   03 ERR-IDARTNR                PIC X(10) VALUE SPACES.                  
008900   03 ERR-STATUS                 PIC X(8)  VALUE SPACES.                  
009000   03 ERR-TXT                    PIC X(30) VALUE SPACES.                  
009100   03 ERR-VALUE-X                PIC X(15) VALUE SPACES.                  
009200   03  FILLER REDEFINES ERR-VALUE-X.                                      
009300     05  ERR-VALUE-N             PIC 9(15).                               
009400*                                                                         
009500*01  -COPY WWPRODSL                                                       
009600*                                                                         
009700*    --- PARAMETRAR TILL ABEND                                            
009800                                                                          
009900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010200     SKIP3                                                                
010300*                                                                         
010400 77  INREC-SW                    PIC X       VALUE 'J'.                   
010500     88  INREC-OK                            VALUE 'J'.                   
010600     88  INREC-ERROR                         VALUE 'N'.                   
010700*                                                                         
010800 01  GENERAL-SUBPROGRAMS.                                                 
010900*                                                                         
011000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
011400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011410     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
011500     EJECT                                                                
011600*    --- PARAMETRAR TILL POSTSUM                                          
011700*                                                                         
011800*01  -COPY W0005   -PRE  POSTSUM-                                         
011900     EJECT                                                                
011910*01  -COPY W510CURR                                                       
011920     EJECT                                                                
012000 01  W16152-AREA-START           PIC X(24)   VALUE                        
012100                                             'W16152-AREA-START'.         
012200*01  AREA -COPY W1615001     -PRE IN-                                     
012300                                                                          
012400 01  W16153-AREA-START           PIC X(24)   VALUE                        
012500                                             'W16153-AREA-START'.         
012600*01  AREA -COPY W55310       -PRE W16153-                                 
012700                                                                          
012800*    ---  MSG INPUT-OUTPUT AREA                                           
012900 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
013000*01  -COPY WMSGAREA                                                       
013100                                                                          
013200*    ---  AREA FÖR W006KOM SUBMODUL                                       
013300 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
013400 01  KOM-IO-AREA.                                                         
013500*    03  -COPY WMSGKOM                                                    
013600     EJECT                                                                
013700*    MID-AREA FÖR W1I11201                                       *        
013800*01  -COPY W1I11201 -PRE 1112-                                            
013900                                                                          
014000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014100                                                                          
014200 01  KEYS-TILL-DLI.                                                       
014300   03  W-IDARTNR-X.                                                       
014400     05  W-IDARTNR               PIC S9(9)   COMP-3  VALUE ZERO.          
014500                                                                          
014600   03  W-KDSEGKEY-X.                                                      
014700     05  W-KDSEGKEY              PIC X       VALUE '1'.                   
014800                                                                          
014900   03    W-IDLEVNR-X.                                                     
015000     05  W-IDLEVNR               PIC X(5)    VALUE SPACE.                 
015100                                                                          
015200     03  W-KDEMBAL-X.                                                     
015300         05  W-KDEMBAL           PIC X(3)    VALUE SPACE.                 
015400                                                                          
016600     SKIP2                                                                
016700 01    W-PROG-TO-PROG-SW-1.                                               
016800       03 M-SW-LL-1              PIC S9(4) VALUE +290 COMP SYNC.          
016900       03 M-SW-Z1-Z2-1           PIC  X(2) VALUE LOW-VALUE.               
017000       03 M-SW-KDTRANS-1         PIC  X(8) VALUE 'W0T107X'.               
017100       03 M-SW-IDTRANS-1         PIC  X(4) VALUE '1615'.                  
017200       03 M-SW-KDMFSTYP-1        PIC  X(1) VALUE '1'.                     
017300                                                                          
017400       03  MID -COPY W0I10701 -PRE PROG1-                                 
017500     SKIP2                                                                
017600*    --- STATUS-KOD FRÅN IMS                                              
017700 01  STATUS-WS                   PIC XX.                                  
017800     88  SEGMENT-EXISTS                      VALUE '  '.                  
017900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
018000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018100     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
018200     88  IMS-NOT-OK                          VALUE 'XD'.                  
018300     SKIP2                                                                
018400 01  GOOD-STATUSCODES.                                                    
018500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018600     SKIP3                                                                
018700 01  SSA1                        PIC X(64).                               
018800 01  SSA2                        PIC X(64).                               
018900     EJECT                                                                
019000*    --- IMS FUNCTION CODES                                               
019100*01  -COPY W0003                                                          
019200     EJECT                                                                
019300*    ---  DLI INPUT-OUTPUT AREA                                           
019400                                                                          
019500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
019600 01  DLI-IO-WDK601.                                                       
019700*    03  -COPY WDK601                                                     
019800     EJECT                                                                
019900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
020000 01  DLI-IO-WDK611.                                                       
020100*    03  -COPY WDK611                                                     
020200     EJECT                                                                
020300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK613'.                      
020400 01  DLI-IO-WDK613.                                                       
020500*    03  -COPY WDK613                                                     
020600                                                                          
020700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDF101'.                      
020800 01  DLI-IO-WDF101.                                                       
020900*    03  -COPY WDF101                                                     
021000     EJECT                                                                
021100                                                                          
021700 LINKAGE SECTION.                                                         
021800                                                                          
021900*01  -COPY W0009   -PRE MSG-                                              
022000                                                                          
022100*01  -COPY W0009  -PRE ALT1-                                              
022200                                                                          
022300*01  -COPY W0009  -PRE DISP-                                              
022400                                                                          
022500*01  -COPY W0008  -PRE WDK6-                                              
022600     05  FILLER                  PIC X.                                   
022700                                                                          
022800*01  -COPY W0008  -PRE WDF1-                                              
022900     05  FILLER                  PIC X.                                   
023000                                                                          
023100*01  -COPY W0008  -PRE 9305-                                              
023200     05  FILLER                  PIC X.                                   
023300                                                                          
023400 01  WDP8-PCB                    PIC X.                                   
023500     EJECT                                                                
023600 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB DISP-PCB WDK6-PCB             
023700                           WDF1-PCB 9305-PCB WDP8-PCB.                    
023800 MAIN SECTION.                                                            
023900     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB DISP-PCB WDK6-PCB             
024000                           WDF1-PCB 9305-PCB WDP8-PCB.                    
024100                                                                          
024200     SKIP2                                                                
024300     PERFORM A-INIT                                                       
024400     PERFORM S01-READ-W16152                                              
024500                                                                          
024600     PERFORM UNTIL END-OF-W16152                                          
024700       IF CHKP-ANT > CHKP-MAX                                             
024800         PERFORM X-TAKE-CHECKPOINT                                        
024900       END-IF                                                             
025000                                                                          
025100       PERFORM B-VALIDATE-INREC                                           
025200       IF INREC-OK                                                        
025300         PERFORM C-UPDATE-WDK6                                            
025400         PERFORM D-UPDATE-CROSS-INDEX                                     
025500         PERFORM E-CREATE-985-POSTER                                      
025600         PERFORM F-UPDATE-BENAMNING                                       
025700         MOVE SPACES     TO WS-REC                                        
025800         MOVE IN-IDARTNR TO ERR-IDARTNR                                   
025900         MOVE 'OK'       TO ERR-STATUS                                    
026000         MOVE SPACES     TO ERR-TXT ERR-VALUE-X                           
026100         MOVE W-ERR-DESC TO WS-REC                                        
026200         PERFORM S02-WRITE-W16154                                         
026300       ELSE                                                               
026400         MOVE SPACES     TO WS-REC                                        
026500         MOVE 'ERROR'    TO ERR-STATUS                                    
026600         MOVE W-ERR-DESC TO WS-REC                                        
026700         PERFORM S02-WRITE-W16154                                         
026800       END-IF                                                             
026900                                                                          
027000       PERFORM S01-READ-W16152                                            
027100     END-PERFORM                                                          
027200                                                                          
027300                                                                          
027400     PERFORM Z-FINIT                                                      
027500                                                                          
027600     MOVE ZERO TO RETURN-CODE                                             
027700     GOBACK                                                               
027800     .                                                                    
027900     EJECT                                                                
028000 A-INIT SECTION.                                                          
028100     SKIP2                                                                
028200                                                                          
028300     ACCEPT TODAYS-DATE FROM DATE                                         
028400     MOVE TODAYS-DATE (1:2) TO W-DATE-AAMM(1:2)                           
028410     MOVE 01                TO W-DATE-AAMM(3:2)                           
028500     PERFORM IMS-RESTART                                                  
028600                                                                          
028700     OPEN INPUT W16152                                                    
028800                                                                          
028900     OPEN OUTPUT W16153                                                   
029000                 W16154                                                   
029100     .                                                                    
029200     EJECT                                                                
029300 B-VALIDATE-INREC SECTION.                                                
029400     MOVE 'J'                             TO INREC-SW                     
029500     MOVE SPACE                           TO ERR-IDARTNR                  
029600     MOVE SPACE                           TO ERR-TXT                      
029700     MOVE SPACE                           TO ERR-VALUE-X                  
029800* VALIDATE BIMA PART NUMBER                                               
029900     IF INREC-OK                                                          
030000       IF IN-IDARTNR IS NUMERIC                                           
030100         MOVE IN-IDARTNR                  TO W-IDARTNR                    
030200         PERFORM IMS-GU-WDK601                                            
030300         IF SEGMENT-EXISTS                                                
030400           CONTINUE                                                       
030500         ELSE                                                             
030600           MOVE 'N'                           TO INREC-SW                 
030700           MOVE IN-IDARTNR                    TO ERR-IDARTNR              
030800           MOVE 'PART NUMBER DOES NOT EXISTS:' TO ERR-TXT                 
030900           MOVE IN-IDARTNR                    TO ERR-VALUE-X              
031000         END-IF                                                           
031100       ELSE                                                               
031200         MOVE 'N'                           TO INREC-SW                   
031300         MOVE IN-IDARTNR                    TO ERR-IDARTNR                
031400         MOVE 'PART NUMBER IS NOT NUMERIC:' TO ERR-TXT                    
031500         MOVE IN-IDARTNR                    TO ERR-VALUE-X                
031600       END-IF                                                             
031700     END-IF                                                               
031800                                                                          
031900* VALIDATE PRODUCT GROUP                                                  
032000     IF INREC-OK                                                          
032100       MOVE ART-KDPRODSL                      TO TEST-KDPRODSL            
032200       IF KDPRODSL-BIMA                                                   
032300         CONTINUE                                                         
032400       ELSE                                                               
032500         MOVE 'N'                             TO INREC-SW                 
032600         MOVE IN-IDARTNR                      TO ERR-IDARTNR              
032700         MOVE 'PRODUCT GROUP IS NOT NUMERIC:' TO ERR-TXT                  
032800         MOVE IN-KDPRODSL                     TO ERR-VALUE-X              
032900       END-IF                                                             
033000     END-IF                                                               
033100                                                                          
033200* VALIDATION FOR KVPALL UPDATE                                            
033300     IF INREC-OK                                                          
033400       IF IN-KVPALL >= 0                                                  
033500         CONTINUE                                                         
033600       ELSE                                                               
033700         MOVE 'N'                      TO INREC-SW                        
033800         MOVE IN-IDARTNR               TO ERR-IDARTNR                     
033900         MOVE 'KVPALL IS NOT NUMERIC:' TO ERR-TXT                         
034000         MOVE IN-KVPALL                TO ERR-VALUE-X                     
034100       END-IF                                                             
034200     END-IF                                                               
034300                                                                          
034400* VALIDATION FOR KVQPACK-1 UPDATE                                         
034500     IF INREC-OK                                                          
034600       IF IN-KVQPACK-1 >= 0                                               
034700         CONTINUE                                                         
034800       ELSE                                                               
034900         MOVE 'N'                         TO INREC-SW                     
035000         MOVE IN-IDARTNR                  TO ERR-IDARTNR                  
035100         MOVE 'KVQPACK-1 IS NOT NUMERIC:' TO ERR-TXT                      
035200         MOVE IN-KVQPACK-1                TO ERR-VALUE-X                  
035300       END-IF                                                             
035400     END-IF                                                               
035500                                                                          
035600* VALIDATION FOR UPDATE OF PRICES                                         
035700* VALIDATE ORDER PRICE                                                    
035800     IF INREC-OK                                                          
035900       IF IN-PRARTBEL IS NUMERIC                                          
036000         IF IN-PRARTBEL > 0                                               
036100*          VALIDATE CURRENCY CODE                                         
036200           MOVE IN-KDVALISO                TO CURR-KDVALISO-ROW           
036210           MOVE 'SEK'                      TO CURR-KDVALISO-HUV           
036220           MOVE W-DATE-AAMM                TO CURR-TIAAMM                 
036230           MOVE 'A'                        TO CURR-KDVALTYP               
036240           CALL W510CURR USING CURR-W510CURR 9305-PCB                     
036250           IF CURR-KDSVAR = ' '                                           
036500*            VALIDATE TIPRLIST                                            
036600             IF IN-TIPRLIST IS NOT NUMERIC                                
036700               MOVE 'N'                      TO INREC-SW                  
036800               MOVE IN-IDARTNR               TO ERR-IDARTNR               
036900               MOVE 'TIPRLIST NOT NUMERIC:' TO ERR-TXT                    
037000               MOVE IN-TIPRLIST              TO ERR-VALUE-X               
037100             ELSE                                                         
037200               IF IN-TIPRLIST > TODAYS-DATE OR IN-TIPRLIST = 0            
037300                 CONTINUE                                                 
037400               ELSE                                                       
037500                 MOVE 'N'                      TO INREC-SW                
037600                 MOVE IN-IDARTNR               TO ERR-IDARTNR             
037700                 MOVE 'INVALID TIPRLIST    :'  TO ERR-TXT                 
037800                 MOVE IN-TIPRLIST              TO ERR-VALUE-X             
037900               END-IF                                                     
038000             END-IF                                                       
038100           ELSE                                                           
038200             MOVE 'N'                       TO INREC-SW                   
038300             MOVE IN-IDARTNR                TO ERR-IDARTNR                
038400             MOVE 'INVALID CURRENCY CODE:'  TO ERR-TXT                    
038500             MOVE IN-KDVALISO               TO ERR-VALUE-X                
038600           END-IF                                                         
038700         END-IF                                                           
038800       ELSE                                                               
038900         MOVE 'N'                       TO INREC-SW                       
039000         MOVE IN-IDARTNR                TO ERR-IDARTNR                    
039100         MOVE 'INVALID ORDER PROCE:'    TO ERR-TXT                        
039200         MOVE IN-PRARTBEL               TO ERR-VALUE-N                    
039300       END-IF                                                             
039400     END-IF                                                               
039500                                                                          
039600*    VALIDATE SUPLLIER NUMBER                                             
039700     IF INREC-OK                                                          
039800       IF IN-PRARTBEL > 0 OR IN-BELEV > SPACE                             
039900         IF IN-IDLEVNR > SPACES                                           
040000           MOVE IN-IDLEVNR                 TO W-IDLEVNR                   
040100           PERFORM IMS-GU-WDF101                                          
040200           IF SEGMENT-EXISTS                                              
040300             CONTINUE                                                     
040400           ELSE                                                           
040500             MOVE 'N'                      TO INREC-SW                    
040600             MOVE IN-IDARTNR               TO ERR-IDARTNR                 
040700             MOVE 'INVALID SUPPLIER NAME:' TO ERR-TXT                     
040800             MOVE IN-IDLEVNR               TO ERR-VALUE-X                 
040900           END-IF                                                         
041000         END-IF                                                           
041100       END-IF                                                             
041200     END-IF                                                               
041300     .                                                                    
041400 C-UPDATE-WDK6        SECTION.                                            
041500     IF IN-KVPALL > 0 OR IN-KVQPACK-1 > 0                                 
041600       MOVE IN-IDARTNR       TO W-IDARTNR                                 
041700       PERFORM IMS-GHNP-WDK611                                            
041800       IF SEGMENT-EXISTS                                                  
041900         IF IN-KVPALL    = CLAG-KVPALL    AND                             
042000            IN-KVQPACK-1 = CLAG-KVQPACK-1                                 
042100           CONTINUE                                                       
042200         ELSE                                                             
042300           IF IN-KVPALL > 0                                               
042400             MOVE IN-KVPALL      TO CLAG-KVPALL                           
042500           END-IF                                                         
042600           IF IN-KVQPACK-1 > 0                                            
042700             MOVE IN-KVQPACK-1   TO CLAG-KVQPACK-1                        
042800           END-IF                                                         
042900           IF IN-KVPALL > 0 OR IN-KVQPACK-1 > 0                           
043000             PERFORM IMS-REPL-WDK611                                      
043100           END-IF                                                         
043200           IF IN-KVQPACK-1 > 0                                            
043300             MOVE 'Q1 '                  TO W-KDEMBAL                     
043400             PERFORM IMS-GET-WDK613-EMB                                   
043500             IF SEGMENT-EXISTS                                            
043600                MOVE CLAG-IDARTNR-EMBQ0  TO EMB-IDARTNR-EMB               
043700                MOVE CLAG-KDEMBKOD-0     TO EMB-KDEMBKOD                  
043800                MOVE CLAG-KVQPACK-0      TO EMB-KVQPACK-EMB               
043900                PERFORM IMS-REPL-WDK613-EMB                               
044000             ELSE                                                         
044100                MOVE 'Q1 '               TO EMB-KDEMBKEY                  
044200                MOVE CLAG-IDARTNR-EMBQ1  TO EMB-IDARTNR-EMB               
044300                MOVE CLAG-KDEMBKOD-1     TO EMB-KDEMBKOD                  
044400                MOVE CLAG-KVQPACK-1      TO EMB-KVQPACK-EMB               
044500                PERFORM IMS-ISRT-WDK613-EMB                               
044600             END-IF                                                       
044700           END-IF                                                         
044800         END-IF                                                           
044900       END-IF                                                             
045000     END-IF                                                               
045100     .                                                                    
045200 D-UPDATE-CROSS-INDEX SECTION.                                            
045300     IF IN-BELEV > SPACE AND IN-IDLEVNR > SPACE                           
045400       MOVE ALL '+' TO PROG1-MID                                          
045500       MOVE IN-IDLEVNR     TO PROG1-MID-IDLEVNR-UP                        
045600       MOVE IN-BELEV       TO PROG1-MID-BELEVART-UP                       
045700                                                                          
045800       MOVE SPACE          TO PROG1-MID-IDARTNR-UT                        
045900                              PROG1-MID-IDLEVNR-UT                        
046000                              PROG1-MID-BELEVART-UT                       
046100                                                                          
046200       MOVE W-IDARTNR      TO PROG1-MID-IDARTNR-IN                        
046300                              PROG1-MID-IDARTNR-UP                        
046400                                                                          
046500       MOVE +9             TO PROG1-MID-KDFTAG-UP                         
046600       MOVE +1             TO PROG1-MID-IDBENR-UP                         
046700       MOVE 'J'            TO PROG1-MID-FLTLVM-UP                         
046800                                                                          
046900       PERFORM IMS-INSERT-ALT1MSG                                         
047000     END-IF                                                               
047100     .                                                                    
047200     EJECT                                                                
047300 F-UPDATE-BENAMNING   SECTION.                                            
047400*** DISPATCHERTRANS TILL 1112-BILDEN FÖR UPPD AV BENÄMNING                
047500*** INITIERA WMSGKOM                                                      
047600     IF IN-BEART > SPACE                                                  
047700       MOVE SPACE              TO MSG-KOM-WMSGKOM                         
047800       ACCEPT MSG-KOM-TIREGDAT FROM DATE                                  
047900       ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                  
048000       MOVE +54                TO MSG-KOM-KVLL                            
048100       MOVE LOW-VALUE          TO MSG-KOM-KDZ1                            
048200       MOVE LOW-VALUE          TO MSG-KOM-KDZ2                            
048300       MOVE '1'                TO MSG-KDMFSFOR-1                          
048400       MOVE SPACE              TO MSG-KOM-KDTRANS                         
048500       MOVE 'W1615200'         TO MSG-KOM-IDSNDJOB                        
048600       MOVE SPACE              TO MSG-KOM-IDMFSMED                        
048700                                  MSG-KOM-KDSVAR                          
048800       ADD +1                  TO MSG-KOM-TIKLOCK                         
048900                                                                          
049000       COMPUTE MSG-KVLL = LENGTH OF 1112-MID-W1I11201 + 17                
049100       MOVE 'W1T112X'          TO MSG-KDTRANS-1                           
049200       MOVE '1112'             TO MSG-IDTRANS-1                           
049300       MOVE 'W1I11201'         TO MSG-KOM-IDCPYTXT                        
049400       MOVE 'BIMA'             TO MSG-KOM-IDSNDNOD                        
049500                                                                          
049600       MOVE W-IDARTNR          TO 1112-MID-IDARTNR-IN                     
049700       MOVE IN-BEART           TO 1112-MID-BEART-NY                       
049800       MOVE 'J'                TO 1112-MID-FLRSBEART                      
049900       MOVE SPACE              TO 1112-MID-KDHOMONYM-NY                   
050000                                                                          
050100       MOVE 1112-MID-W1I11201  TO MSG-INDATA-MINUS-1-TRANSKOD             
050200                                                                          
050300       CALL W006KOM USING MSG-PCB                                         
050400                          DISP-PCB                                        
050500                          WDP8-PCB                                        
050600                          MSG-KOM-WMSGKOM                                 
050700                          MSG-IO-AREA                                     
050800                                                                          
050900       IF MSG-KOM-IDMFSMED NOT = SPACE                                    
051000          STRING ' ERROR FROM W006KOM: '  MSG-KOM-IDMFSMED                
051100            DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
051200          DISPLAY  ERROR-TEXT-STR                                         
051300          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
051400       END-IF                                                             
051500                                                                          
051600       MOVE SPACE       TO KOM-IO-AREA                                    
051700     END-IF                                                               
051800     .                                                                    
051900     EJECT                                                                
052000 E-CREATE-985-POSTER SECTION.                                             
052100     IF IN-PRARTBEL > 0                                                   
052200       MOVE '985'            TO W16153-IDPTYP                             
052300       MOVE IN-IDARTNR       TO W16153-IDARTNR                            
052400       MOVE IN-IDLEVNR       TO W16153-IDLEVNR                            
052500       MOVE 'BIMA    '       TO W16153-IDUSER                             
052600*                                                                         
052700* THE PRICE VALUE IS CHANGED ACCORDINGLY TO BE HANDLED IN W55312          
052800       IF IN-KDVALISO = 'GBP'                                             
052900         COMPUTE W16153-PRARTBEL = IN-PRARTBEL * 1000                     
053000       ELSE                                                               
053100         COMPUTE W16153-PRARTBEL = IN-PRARTBEL * 100                      
053200       END-IF                                                             
053300*                                                                         
053400       MOVE '1'              TO W16153-KDANTENH                           
053500       MOVE SPACE            TO W16153-KDFPKPRI                           
053600       IF IN-TIPRLIST = 0                                                 
053700         MOVE TODAYS-DATE    TO W16153-TIPRLIST                           
053800       ELSE                                                               
053900         MOVE IN-TIPRLIST    TO W16153-TIPRLIST                           
054000       END-IF                                                             
054100       MOVE IN-KDVALISO      TO W16153-KDVALISO                           
054200       PERFORM S03-WRITE-W16153                                           
054300     END-IF                                                               
054400     .                                                                    
054500     EJECT                                                                
054600 Z-FINIT SECTION.                                                         
054700     CLOSE W16152                                                         
054800           W16153                                                         
054900           W16154                                                         
055000     SKIP2                                                                
055100     MOVE 'S' TO POSTSUM-OPKOD                                            
055200     CALL POSTSUM USING POSTSUM-PARM                                      
055300     .                                                                    
055400     EJECT                                                                
055500 S01-READ-W16152  SECTION.                                                
055600     SKIP2                                                                
055700     READ W16152 INTO IN-AREA                                             
055800     AT END                                                               
055900        SET END-OF-W16152 TO TRUE                                         
056000                                                                          
056100     NOT AT END                                                           
056200        MOVE 'W16152' TO POSTSUM-FDNAMN                                   
056300        MOVE 'W16152D1' TO POSTSUM-DDNAMN2                                
056400        CALL POSTSUM USING POSTSUM-PARM                                   
056500                                                                          
056600     END-READ                                                             
056700     .                                                                    
056800     EJECT                                                                
056900 S02-WRITE-W16154 SECTION.                                                
057000     WRITE WS-REC                                                         
057100                                                                          
057200     MOVE SPACE TO POSTSUM-TRANSTYP                                       
057300     MOVE 'W16154 '  TO POSTSUM-FDNAMN                                    
057400     MOVE 'W16152D2' TO POSTSUM-DDNAMN2                                   
057500     CALL POSTSUM USING POSTSUM-PARM                                      
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 S03-WRITE-W16153 SECTION.                                                
058000     WRITE UT-POST FROM W16153-AREA                                       
058100                                                                          
058200     MOVE W16153-IDPTYP TO POSTSUM-TRANSTYP                               
058300     MOVE 'W16153 ' TO POSTSUM-FDNAMN                                     
058400     MOVE 'W16152D2' TO POSTSUM-DDNAMN2                                   
058500     CALL POSTSUM USING POSTSUM-PARM                                      
058600     .                                                                    
058700     EJECT                                                                
058800 X-TAKE-CHECKPOINT   SECTION.                                             
058900                                                                          
059000* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
059100* --- SAVE DATABASE KEYS IF NECESSARY                                     
059200     PERFORM IMS-CHECKPOINT                                               
059300     MOVE ZERO TO CHKP-ANT                                                
059400* --- REREAD DATABASE IF NECESSARY                                        
059500     .                                                                    
059600     EJECT                                                                
059700* --- IMS SECTIONS  ---                                                   
059800                                                                          
059900 IMS-INSERT-ALT1MSG  SECTION.                                             
060000     MOVE SPACE TO GOOD-STATUSCODES                                       
060100     CALL CBLTDLI USING PURG ALT1-PCB W-PROG-TO-PROG-SW-1                 
060200     MOVE  ALT1-STATUS-CODE TO STATUS-WS                                  
060300     PERFORM IMS-STATUSCHECK                                              
060400     .                                                                    
060500     SKIP2                                                                
060600 IMS-GU-WDF101 SECTION.                                                   
060700     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
060800            DELIMITED BY SIZE INTO SSA1                                   
060900     MOVE '  GE' TO GOOD-STATUSCODES                                      
061000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
061100     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
061200     PERFORM IMS-STATUSCHECK                                              
061300     .                                                                    
061400     SKIP3                                                                
061500 IMS-GU-WDK601 SECTION.                                                   
061600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
061700          DELIMITED BY SIZE INTO SSA1                                     
061800     MOVE '  GE' TO GOOD-STATUSCODES                                      
061900     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK601 SSA1                   
062000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
062100     PERFORM IMS-STATUSCHECK                                              
062200     .                                                                    
062300     SKIP3                                                                
062400 IMS-GHNP-WDK611 SECTION.                                                 
062500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
062600          DELIMITED BY SIZE INTO SSA1                                     
062700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
062800          DELIMITED BY SIZE INTO SSA2                                     
062900     MOVE '  GE' TO GOOD-STATUSCODES                                      
063000     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1 SSA2             
063100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
063200     PERFORM IMS-STATUSCHECK                                              
063300     .                                                                    
063400     EJECT                                                                
063500 IMS-REPL-WDK611 SECTION.                                                 
063600     MOVE '  ' TO GOOD-STATUSCODES                                        
063700     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
063800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
063900     PERFORM IMS-STATUSCHECK                                              
064000     .                                                                    
064100     EJECT                                                                
064200 IMS-GET-WDK613-EMB  SECTION.                                             
064300                                                                          
064400     STRING 'WDK613  (KDEMBAL  =' W-KDEMBAL-X ')'                         
064500          DELIMITED BY SIZE INTO SSA1                                     
064600     MOVE '  GE' TO GOOD-STATUSCODES                                      
064700     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK613 SSA1                  
064800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
064900     PERFORM IMS-STATUSCHECK                                              
065000     .                                                                    
065100     SKIP3                                                                
065200 IMS-REPL-WDK613-EMB SECTION.                                             
065300                                                                          
065400     MOVE '  ' TO GOOD-STATUSCODES                                        
065500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK613                       
065600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
065700     PERFORM IMS-STATUSCHECK                                              
065800     .                                                                    
065900     SKIP3                                                                
066000 IMS-ISRT-WDK613-EMB SECTION.                                             
066100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
066200          DELIMITED BY SIZE INTO SSA1                                     
066300     MOVE 'WDK613   ' TO SSA2                                             
066400     MOVE '  ' TO GOOD-STATUSCODES                                        
066500     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK613 SSA1 SSA2             
066600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
066700     PERFORM IMS-STATUSCHECK                                              
066800     .                                                                    
067900 IMS-RESTART SECTION.                                                     
068000     SKIP2                                                                
068100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
068200     MOVE '  ' TO GOOD-STATUSCODES                                        
068300     CALL CBLTDLI USING XRST MSG-PCB                                      
068400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
068500                        CHKP-AREA-LENGTH CHKP-AREA                        
068600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
068700     PERFORM IMS-STATUSCHECK                                              
068800     .                                                                    
068900     SKIP3                                                                
069000 IMS-CHECKPOINT SECTION.                                                  
069100     SKIP2                                                                
069200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
069300     MOVE '  XD' TO GOOD-STATUSCODES                                      
069400     CALL CBLTDLI USING CHKP MSG-PCB                                      
069500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
069600                        CHKP-AREA-LENGTH CHKP-AREA                        
069700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
069800     PERFORM IMS-STATUSCHECK                                              
069900                                                                          
070000     IF IMS-NOT-OK                                                        
070100       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO                     
070200                                           ERROR-TEXT-STR                 
070300       DISPLAY ERROR-TEXT                                                 
070400       CALL FELLOG                                                        
070500     END-IF                                                               
070600     .                                                                    
070700     EJECT                                                                
070800 IMS-STATUSCHECK SECTION.                                                 
070900     SKIP2                                                                
071000     SET STATUS-IX TO 1                                                   
071100     SEARCH GOOD-STATUS                                                   
071200       AT END                                                             
071300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
071400           DELIMITED BY SIZE INTO ERROR-TEXT                              
071500         DISPLAY ERROR-TEXT                                               
071600         CALL FELLOG                                                      
071700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
071800         CONTINUE                                                         
071900     END-SEARCH                                                           
072000     .                                                                    
