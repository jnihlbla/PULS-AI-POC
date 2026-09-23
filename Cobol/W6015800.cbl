000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6015800.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   09/12/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700**   FUNCTION:                                                            
000800*        THIS PROGRAM HANDLES THE GATE STEERING                           
000900*                                                                         
001000*        THE PROGRAM UPDATES   WDT2                                       
001100*        THE PROGRAM READS     WDK6                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W6T158                                              
001500*        MID:         W6I15801                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W6O15801                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W6015800'.            
002700                                                                          
002800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002900 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
003000                                                                          
003100 77  YES                         PIC X      VALUE 'Y'.                    
003200 77  NOO                         PIC X      VALUE 'N'.                    
003300 77  W-KDSORT1                   PIC X      VALUE SPACE.                  
003400 77  W-CMD                       PIC 9(2)   VALUE ZERO.                   
003500 77  W-CMD-CNT                   PIC 9(2)   VALUE ZERO.                   
003600*01 -COPY WWDCKONS                                                        
004800                                                                          
004900*    --- INDEX FOR SCROLL LINES                                           
005000 77  W-INDX                      PIC S9(4)  VALUE +0    COMP SYNC.        
005100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
005300*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005400                                                                          
005500 77  UPDATE-SW                   PIC X       VALUE 'N'.                   
005600     88  UPDATE-SUCCESS                      VALUE 'Y'.                   
005700     88  NO-UPDATE                           VALUE 'N'.                   
005800                                                                          
005900 77  DELETE-SW                   PIC X       VALUE 'N'.                   
006000     88  DELETE-YES                          VALUE 'Y'.                   
006100     88  NO-DELETE                           VALUE 'N'.                   
006200                                                                          
006900 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
007000     88  INDATA-OK                           VALUE 'Y'.                   
007100     88  INDATA-WRONG                        VALUE 'N'.                   
007200                                                                          
007300 77  CHK-CMD-SW                  PIC X       VALUE 'Y'.                   
007400     88  CMD-CHK-OK                          VALUE 'Y'.                   
007500     88  CMD-CHK-WRONG                       VALUE 'N'.                   
007600                                                                          
007700 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
007800     88  KEYS-OK                             VALUE 'Y'.                   
007900     88  KEYS-WRONG                          VALUE 'N'.                   
008000                                                                          
008100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008200     88  OWN-MID                             VALUE '6158'.                
008300     88  GOOD-MID                            VALUE '6151' '6152'          
008400                                                   '6153' '6154'          
008500                                                   '6155' '6156'          
008600                                                   '6157' '6158'          
008700                                                   '6159'.                
008800     88  HELP-MID                            VALUE '0551'.                
008900     EJECT                                                                
009000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009100 01  GENERAL-SUBPROGRAMS.                                                 
009200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009700     EJECT                                                                
009800*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009900*01 -COPY WMEDAREA                                                        
010000     SKIP3                                                                
010100 01  MESSAGE-CODES.                                                       
010200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010400     03  ERR-LAST-PAGE           PIC X(3)    VALUE '106'.                 
010500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010900     03  INF-NO-RECORDS          PIC X(3)    VALUE '010'.                 
011000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011100     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
011300     03  ERR-UPDATE-NOT-POSS     PIC X(3)    VALUE '007'.                 
011500     EJECT                                                                
011600*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012000*01 -COPY WMSGINIT                                                        
012100*                                                                         
012200*    --- PARAMETRAR TILL WDATKONV                                         
012400*01  -COPY WDATAREA                                                       
012500*                                                                         
012600*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
012700*                                                                         
012800 01  SAVE-AREA.                                                           
012900     03  SAVE-IDTRANS            PIC X(4)    VALUE '6158'.                
012910*                                                                         
012920     03  SAVE-BEFT               PIC 9(2)    VALUE ZERO.                  
012930*                                                                         
013000     03  SAVE-ADLAGOMR-ENTER     PIC 9(2)    VALUE ZERO.                  
013100     03  SAVE-ADLAGOMR-NEXT      PIC 9(2)    VALUE ZERO.                  
013200     03  SAVE-ADLAGOMR-PREV      PIC 9(2)    VALUE ZERO.                  
013300     03  SAVE-ADLAGOMR-PREV2     PIC 9(2)    VALUE ZERO.                  
013301*                                                                         
013310     03  SAVE-BEFT-ENTER         PIC 9(2)    VALUE ZERO.                  
013320     03  SAVE-BEFT-NEXT          PIC 9(2)    VALUE ZERO.                  
013330     03  SAVE-BEFT-PREV          PIC 9(2)    VALUE ZERO.                  
013340     03  SAVE-BEFT-PREV2         PIC 9(2)    VALUE ZERO.                  
013400*                                                                         
013500     03  SAVE-IDARTNR-ENTER      PIC S9(9)   COMP-3 VALUE ZERO.           
013600     03  SAVE-IDARTNR-NEXT       PIC S9(9)   COMP-3 VALUE ZERO.           
013700     03  SAVE-IDARTNR-PREV       PIC S9(9)   COMP-3 VALUE ZERO.           
013800     03  SAVE-IDARTNR-PREV2      PIC S9(9)   COMP-3 VALUE ZERO.           
013900*                                                                         
014000     03  SAVE-TIAAVV-ENTER       PIC 9(4)    VALUE ZERO.                  
014100     03  SAVE-TIAAVV-NEXT        PIC 9(4)    VALUE ZERO.                  
014200     03  SAVE-TIAAVV-PREV        PIC 9(4)    VALUE ZERO.                  
014300     03  SAVE-TIAAVV-PREV2       PIC 9(4)    VALUE ZERO.                  
014900*                                                                         
015000     03  SAVE-KEY                PIC X(6)    VALUE SPACES.                
015100     03  SAVE-KEY-ENTER          PIC X(6)    VALUE SPACES.                
015200     03  SAVE-KEY-NEXT           PIC X(6)    VALUE SPACES.                
015300     03  SAVE-KEY-PREV           PIC X(6)    VALUE SPACES.                
015400     03  SAVE-KEY-PREV2          PIC X(6)    VALUE SPACES.                
015500     EJECT                                                                
015600*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015900     SKIP3                                                                
016000*01  MID -COPY W6I15801                                                   
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016300     SKIP3                                                                
016400*01  -COPY WMSGAREA                                                       
016500     EJECT                                                                
016600     03  MOD REDEFINES MSG-AREA.                                          
016700*      05  -COPY W6O15801                                                 
016800     EJECT                                                                
016900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017000     SKIP3                                                                
017100*01  -COPY WMFSAREA                                                       
017200     EJECT                                                                
017300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
017400*                                                                         
017500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017600     SKIP3                                                                
017700 01  KEYS-FOR-DLI.                                                        
017710     03  W-IDDC-X.                                                        
017720         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017721     03  W-WDT211KY-X.                                                    
017722         05  W-ADLAGOMR-GLO      PIC S9(3)   VALUE ZERO COMP-3.           
017723         05  W-TIAAVV-GLO        PIC S9(5)   VALUE ZERO COMP-3.           
017724     03  W-WDT211KY-MIN-X.                                                
017725         05  W-ADLAGOMR-MIN      PIC S9(3)   VALUE ZERO COMP-3.           
017726         05  W-TIAAVV-LO-MIN     PIC S9(5)   VALUE ZERO COMP-3.           
017728     03  W-WDT212KY-X.                                                    
017729         05  W-BEFT-GFT          PIC 9(2)    VALUE ZERO.                  
017730         05  W-TIAAVV-GFT        PIC S9(5)   VALUE ZERO COMP-3.           
017731     03  W-WDT212KY-MIN-X.                                                
017732         05  W-BEFT-MIN          PIC 9(2)    VALUE ZERO.                  
017733         05  W-TIAAVV-FT-MIN     PIC S9(5)   VALUE ZERO COMP-3.           
017734     03  W-WDT213KY-X.                                                    
017735         05  W-IDARTNR-GART      PIC S9(9)   VALUE ZERO COMP-3.           
017736         05  W-TIAAVV-GART       PIC S9(5)   VALUE ZERO COMP-3.           
017737     03  W-WDT213KY-MIN-X.                                                
017738         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
017739         05  W-TIAAVV-ART-MIN    PIC S9(5)   VALUE ZERO COMP-3.           
017740                                                                          
017800*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
018500     03  W-BEFT-X.                                                        
018600         05  W-BEFT              PIC 9(02)   VALUE ZERO.                  
018700*                                                                         
018800     03  W-ADLAGOMR-X.                                                    
018900         05  W-ADLAGOMR          PIC S9(03)  VALUE ZERO COMP-3.           
019000*                                                                         
019400     03  W-IDARTNR-X.                                                     
019500         05  W-IDARTNR           PIC S9(09)  VALUE ZERO COMP-3.           
019600*                                                                         
021100     SKIP2                                                                
021200*    --- STATUS CODES FROM IMS                                            
021300 01  STATUS-WS                   PIC XX.                                  
021400     88  SEGMENT-FOUND                       VALUE '  '.                  
021500     88  SEGMENT-VALID                       VALUE 'GK'.                  
021600     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
021700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
021800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
021900     SKIP2                                                                
022000 01  GOOD-STATUSCODES.                                                    
022100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022200     SKIP3                                                                
022300 01  SSA1                        PIC X(64).                               
022400 01  SSA2                        PIC X(128).                              
022500                                                                          
022600 01  TODAYS-DATE                  PIC 9(6)    VALUE ZERO.                 
022700 01  FILLER REDEFINES TODAYS-DATE.                                        
022800     03  W-CURR-YEAR              PIC 9(2).                               
022900     03  TODAYS-TIME-HD           PIC 9(4).                               
023002 01  W-CURR-TIAAVV                PIC 9(4).                               
023003 01  FILLER REDEFINES W-CURR-TIAAVV.                                      
023004     03 W-CURR-TIAA               PIC 9(2).                               
023005     03 W-CURR-TIVV               PIC 9(2).                               
023100     EJECT                                                                
023200*    --- IMS FUNCTION CODES                                               
023300*01  -COPY W0003                                                          
023400     EJECT                                                                
023500*    ---  DLI INPUT-OUTPUT AREA                                           
023600                                                                          
023700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT201'.                      
023800 01  DLI-IO-WDT201.                                                       
023900*    03  -COPY WDT201                                                     
024000     EJECT                                                                
024100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT211'.                      
024200 01  DLI-IO-WDT211.                                                       
024300*    03  -COPY WDT211                                                     
024400     EJECT                                                                
024500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT212'.                      
024600 01  DLI-IO-WDT212.                                                       
024700*    03  -COPY WDT212                                                     
024710 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT213'.                      
024720 01  DLI-IO-WDT213.                                                       
024730*    03  -COPY WDT213                                                     
024800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
024900 01  DLI-IO-WDK601.                                                       
025000*    03  -COPY WDK601  -PRE WDK601-                                       
025100                                                                          
025200 01  DLI-IO-WDT2.                                                         
025300     03  IO-AREA-WDT2            PIC X(28)  VALUE SPACE.                  
025400     SKIP3                                                                
025500     03  WDT201 REDEFINES IO-AREA-WDT2.                                   
025600*        05 -COPY WDT201 -PRE R-                                          
025700     03  WDT211 REDEFINES IO-AREA-WDT2.                                   
025800*        05 -COPY WDT211 -PRE R-                                          
025900     03  WDT212 REDEFINES IO-AREA-WDT2.                                   
026000*        05 -COPY WDT212 -PRE R-                                          
026010     03  WDT213 REDEFINES IO-AREA-WDT2.                                   
026020*        05 -COPY WDT213 -PRE R-                                          
026100                                                                          
026200     EJECT                                                                
026300 LINKAGE SECTION.                                                         
026400*01  -COPY W0009   -PRE MSG-                                              
026500*01  -COPY W0008   -PRE WDP7-                                             
026600     05  FILLER                  PIC X.                                   
026700                                                                          
026800*01  -COPY W0008  -PRE WDT2-                                              
026900     05  FILLER                  PIC X.                                   
027000                                                                          
028000*01  -COPY W0008  -PRE WDK6-                                              
028100     05  FILLER                  PIC X.                                   
028200     EJECT                                                                
028300 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDT2-PCB                      
028500                           WDK6-PCB.                                      
028600 MAIN SECTION.                                                            
028700     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDT2-PCB                      
028900                           WDK6-PCB.                                      
029000                                                                          
029100     PERFORM IMS-GET-MSG                                                  
029200     IF SEGMENT-FOUND                                                     
029300       PERFORM A-INIT                                                     
029400       PERFORM B-CHECK-KEYS                                               
029500       IF KEYS-OK                                                         
029600         IF MFS-UPDATE                                                    
029700           PERFORM G-CHECK-INPUT                                          
029800           IF INDATA-OK                                                   
029900             PERFORM H-UPDATE                                             
030000           END-IF                                                         
030100         ELSE                                                             
030200           IF MFS-FIRST                                                   
030300             PERFORM C-FIRST-PAGE                                         
030400           ELSE                                                           
030500             IF MFS-NEXT                                                  
030600               PERFORM D-NEXT-PAGE                                        
030700             ELSE                                                         
030800               IF MFS-PREVIOUS                                            
030900                 PERFORM I-PREVIOUS-PAGE                                  
031000               ELSE                                                       
031100                 PERFORM S01-CHECK-INPUT                                  
031200                 IF CMD-CHK-OK                                            
031300                   PERFORM E-SAME-PAGE                                    
031400                 END-IF                                                   
031500               END-IF                                                     
031600             END-IF                                                       
031700           END-IF                                                         
031800         END-IF                                                           
031900         IF INDATA-OK                                                     
032000           PERFORM F-READ-SHOW-INFO                                       
032100         END-IF                                                           
032200       END-IF                                                             
032300*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
032400*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
032500       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O15801 + 4                      
032600       PERFORM IMS-INSERT-MSG                                             
032700     END-IF                                                               
032800                                                                          
032900     MOVE ZERO TO RETURN-CODE                                             
033000     GOBACK                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 A-INIT SECTION.                                                          
033410     IF MSG-DOUBLE-TRANSACTIONS                                           
033500       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I15801                 
033600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
033700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
033800     ELSE                                                                 
033900       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I15801                  
034000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
034100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
034200     END-IF                                                               
034300                                                                          
034400     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
034500     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
034600     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
034700                                                                          
034800     MOVE LOW-VALUE        TO MSG-AREA                                    
034900     MOVE 'W6O15801'       TO MFS-IDMOD                                   
035000     MOVE '6158'           TO MOD-IDTRANS                                 
035100     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL MOD-TEMFSINF                   
035200                                                                          
035300     IF OWN-MID OR HELP-MID                                               
035400       CONTINUE                                                           
035500     ELSE                                                                 
035600       MOVE SPACE TO MFS-KDTRTYP                                          
035700       MOVE '7' TO MFS-IDPFK                                              
035710       INITIALIZE SAVE-AREA                                               
035800     END-IF                                                               
035900                                                                          
036000     ACCEPT TODAYS-DATE FROM DATE                                         
036100*                                                                         
036200     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
036300     MOVE TODAYS-DATE  TO DAT-I-TIDATUM                                   
036500                                                                          
036600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
036700                         DAT-O-TIDATUM DAT-KDSVAR                         
036800                                                                          
036900     IF DAT-KDSVAR-OK                                                     
037200       MOVE W-CURR-YEAR    TO W-CURR-TIAA                                 
037300       MOVE DAT-TIVV       TO W-CURR-TIVV                                 
037500     ELSE                                                                 
037600       MOVE ' INVALID RETURN CODE FROM WDATKONV ' TO ERROR-TEXT           
037800       CALL FELLOG                                                        
038300     END-IF                                                               
038310                                                                          
038320     MOVE WC-CDC-SE        TO W-IDDC                                      
038400     .                                                                    
038500     EJECT                                                                
038600 B-CHECK-KEYS SECTION.                                                    
038700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
038800     MOVE '001'             TO MSGI-KDCALL                                
038900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
039000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039100     MOVE '6158'            TO MSGI-IDTRANS                               
039110     CALL W005INIT       USING MSGI-WMSGINIT WDP7-PCB                     
039120     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
039130     PERFORM BA-VALIDATE-SAVE-AREA                                        
039200                                                                          
039300*    - LANGUAGE TO BE USED BY MEDKONV                                     
039400     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
039500     MOVE YES             TO KEYS-SW                                      
039600                                                                          
039700     MOVE MFS-ERASE-FIELD TO MOD-BEFT-IN                                  
039800     IF MID-BEFT-IN NOT = ALL '++'                                        
039900       MOVE '7'         TO MFS-IDPFK                                      
040000       MOVE SPACE       TO MFS-KDTRTYP                                    
040001       MOVE MID-BEFT-IN TO MOD-BEFT-UT                                    
040002                           SAVE-BEFT                                      
040010     ELSE                                                                 
040020       MOVE SAVE-BEFT   TO MOD-BEFT-UT                                    
040030                           MID-BEFT-IN                                    
040100     END-IF                                                               
040200*                                                                         
040300     MOVE MFS-ERASE-FIELD TO MOD-KDSORT1-IN                               
040400     IF MID-KDSORT1-IN = SPACE                                            
040401        MOVE 'A'        TO MID-KDSORT1-IN                                 
040402     END-IF                                                               
040410     IF MID-KDSORT1-IN NOT = ALL '+'                                      
040500       MOVE '7'            TO MFS-IDPFK                                   
040600       MOVE SPACE          TO MFS-KDTRTYP                                 
040610       MOVE MID-KDSORT1-IN TO MOD-KDSORT1-UT                              
040611       IF MID-KDSORT1-IN = 'L'                                            
040612          MOVE 'WDT211'    TO SAVE-KEY                                    
040613                              SAVE-KEY-ENTER                              
040614       ELSE                                                               
040615          IF MID-KDSORT1-IN = 'F'                                         
040616             MOVE 'WDT212' TO SAVE-KEY                                    
040617                              SAVE-KEY-ENTER                              
040618          ELSE                                                            
040619             MOVE 'WDT213' TO SAVE-KEY                                    
040620                              SAVE-KEY-ENTER                              
040621          END-IF                                                          
040622       END-IF                                                             
040623     ELSE                                                                 
040630       IF SAVE-KEY = 'WDT211'                                             
040640          MOVE 'L'      TO MOD-KDSORT1-UT                                 
040650       ELSE                                                               
040660          IF SAVE-KEY = 'WDT212'                                          
040661             MOVE 'F'   TO MOD-KDSORT1-UT                                 
040670          ELSE                                                            
040671             MOVE 'A'   TO MOD-KDSORT1-UT                                 
040672             MOVE ZERO  TO MID-BEFT-IN                                    
040673                           MOD-BEFT-UT                                    
040674                           SAVE-BEFT                                      
040680          END-IF                                                          
040690       END-IF                                                             
040700     END-IF                                                               
040800                                                                          
040900*    -- CHECK OF MID-VALUES                                               
040910     IF OWN-MID OR HELP-MID                                               
041020       IF MID-KDSORT1-IN > SPACES AND NOT = '+'                           
041100         IF MID-KDSORT1-IN = 'L' OR 'F' OR 'A'                            
041200           MOVE MID-KDSORT1-IN TO W-KDSORT1                               
041300                                  MOD-KDSORT1-UT MSGI-KDURVAL             
041310           IF MID-KDSORT1-IN NOT = MSGI-BEFT                              
041311             MOVE ZERO  TO MID-BEFT-IN                                    
041312                           MOD-BEFT-UT                                    
041313                           SAVE-BEFT                                      
041320           END-IF                                                         
041400*          MOVE SPACE          TO MOD-BEFT-UT MSGI-BEFT                   
041500*          CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                     
041600         ELSE                                                             
041700            MOVE NOO            TO KEYS-SW                                
041800         END-IF                                                           
041900       ELSE                                                               
042000         IF MID-BEFT-IN > SPACES AND NOT = '++'                           
042100           INSPECT MID-BEFT-IN REPLACING LEADING SPACE BY ZERO            
042200           IF MID-BEFT-IN IS NUMERIC                                      
042210              IF SAVE-KEY = 'WDT211'                                      
042211                 MOVE MID-BEFT-IN  TO W-ADLAGOMR  W-ADLAGOMR-MIN          
042212                                        MOD-BEFT-UT MSGI-BEFT             
042213                                        SAVE-BEFT                         
042220              ELSE                                                        
042230                IF SAVE-KEY = 'WDT212'                                    
042231                   MOVE MID-BEFT-IN  TO W-BEFT  W-BEFT-MIN                
042232                                        MOD-BEFT-UT MSGI-BEFT             
042240                END-IF                                                    
042250              END-IF                                                      
042500*             MOVE SPACE        TO MOD-KDSORT1-UT MSGI-KDURVAL            
042600*             CALL W005INIT     USING MSGI-WMSGINIT WDP7-PCB              
042700           ELSE                                                           
042800              MOVE NOO          TO KEYS-SW                                
042900           END-IF                                                         
043000*        ELSE                                                             
043010*          CALL W005INIT     USING MSGI-WMSGINIT WDP7-PCB                 
043020*          MOVE MSGI-SPAR-AREA TO SAVE-AREA                               
043030*          MOVE MSGI-BEFT      TO MOD-BEFT-UT                             
043040*          MOVE MSGI-KDURVAL   TO W-KDSORT1 MOD-KDSORT1-UT                
043050*          CALL W005INIT     USING MSGI-WMSGINIT WDP7-PCB                 
043060*          INSPECT MSGI-BEFT REPLACING LEADING SPACE BY ZERO              
043070*          MOVE MSGI-BEFT TO W-BEFT W-BEFT-MIN                            
043100         END-IF                                                           
043900       END-IF                                                             
043910*    ELSE                                                                 
043920*      MOVE ZERO TO W-BEFT W-BEFT-MIN                                     
043940*    END-IF                                                               
044000*                                                                         
044100     IF KEYS-WRONG                                                        
044200       MOVE ERR-WRONG-KEY    TO    MED-IDMFSFEL                           
044300       CALL WMEDKONV         USING MED-WMEDAREA                           
044400       MOVE MED-MFSFEL       TO    MOD-TEMFSFEL                           
044500       PERFORM MFS-ERASE-FIELD-IN                                         
044600       PERFORM MFS-ERASE-FIELD-OUT                                        
044700     ELSE                                                                 
046610       PERFORM MFS-RENSA-FAELT-UP                                         
046700     END-IF                                                               
046800     .                                                                    
046900     EJECT                                                                
047000 C-FIRST-PAGE SECTION.                                                    
047100     MOVE INF-FIRST-PAGE     TO    MED-IDMFSINF                           
047200     CALL WMEDKONV           USING MED-WMEDAREA                           
047300     MOVE MED-MFSINF         TO    MOD-TEMFSFEL                           
047400     MOVE SAVE-KEY-ENTER     TO    SAVE-KEY                               
047500                                   SAVE-KEY-PREV                          
047600*    IF SAVE-KEY = 'WDT211'                                               
047700     IF W-KDSORT1 = 'L'                                                   
047800       MOVE SAVE-ADLAGOMR-ENTER   TO SAVE-ADLAGOMR-PREV                   
047900     ELSE                                                                 
048000*      IF SAVE-KEY = 'WDT212'                                             
048010       IF W-KDSORT1 = 'F'                                                 
048100         MOVE SAVE-BEFT-ENTER       TO SAVE-BEFT-PREV                     
048400       ELSE                                                               
048500*        IF SAVE-KEY = 'WDT213'                                           
048510         IF W-KDSORT1 = 'A'                                               
048600           MOVE SAVE-IDARTNR-ENTER  TO SAVE-IDARTNR-PREV                  
048700         END-IF                                                           
048800       END-IF                                                             
048900     END-IF                                                               
048910     MOVE SAVE-TIAAVV-ENTER         TO SAVE-TIAAVV-PREV                   
049000     .                                                                    
049100     EJECT                                                                
049200 BA-VALIDATE-SAVE-AREA SECTION.                                           
049210     IF SAVE-BEFT NOT NUMERIC                                             
049211       MOVE ZERO TO SAVE-BEFT                                             
049212     END-IF                                                               
049213     IF SAVE-ADLAGOMR-ENTER NOT NUMERIC                                   
049214       MOVE ZERO TO SAVE-ADLAGOMR-ENTER                                   
049215     END-IF                                                               
049216     IF SAVE-ADLAGOMR-NEXT NOT NUMERIC                                    
049217       MOVE ZERO TO SAVE-ADLAGOMR-NEXT                                    
049218     END-IF                                                               
049219     IF SAVE-ADLAGOMR-PREV NOT NUMERIC                                    
049220       MOVE ZERO TO SAVE-ADLAGOMR-PREV                                    
049221     END-IF                                                               
049222     IF SAVE-ADLAGOMR-PREV2 NOT NUMERIC                                   
049223       MOVE ZERO TO SAVE-ADLAGOMR-PREV2                                   
049224     END-IF                                                               
049225     IF SAVE-BEFT-ENTER NOT NUMERIC                                       
049226       MOVE ZERO TO SAVE-BEFT-ENTER                                       
049227     END-IF                                                               
049228     IF SAVE-BEFT-NEXT NOT NUMERIC                                        
049229       MOVE ZERO TO SAVE-BEFT-NEXT                                        
049230     END-IF                                                               
049231     IF SAVE-BEFT-PREV NOT NUMERIC                                        
049232       MOVE ZERO TO SAVE-BEFT-PREV                                        
049233     END-IF                                                               
049234     IF SAVE-BEFT-PREV NOT NUMERIC                                        
049235       MOVE ZERO TO SAVE-BEFT-PREV2                                       
049236     END-IF                                                               
049237     IF SAVE-IDARTNR-ENTER NOT NUMERIC                                    
049238       MOVE ZERO TO SAVE-IDARTNR-ENTER                                    
049239     END-IF                                                               
049240     IF SAVE-IDARTNR-NEXT NOT NUMERIC                                     
049241       MOVE ZERO TO SAVE-IDARTNR-NEXT                                     
049242     END-IF                                                               
049243     IF SAVE-IDARTNR-PREV NOT NUMERIC                                     
049244       MOVE ZERO TO SAVE-IDARTNR-PREV                                     
049245     END-IF                                                               
049246     IF SAVE-IDARTNR-PREV2 NOT NUMERIC                                    
049247       MOVE ZERO TO SAVE-IDARTNR-PREV2                                    
049248     END-IF                                                               
049249     IF SAVE-TIAAVV-ENTER NOT NUMERIC                                     
049250       MOVE ZERO TO SAVE-TIAAVV-ENTER                                     
049251     END-IF                                                               
049252     IF SAVE-TIAAVV-NEXT NOT NUMERIC                                      
049253       MOVE ZERO TO SAVE-TIAAVV-NEXT                                      
049254     END-IF                                                               
049255     IF SAVE-TIAAVV-PREV NOT NUMERIC                                      
049256       MOVE ZERO TO SAVE-TIAAVV-PREV                                      
049257     END-IF                                                               
049258     IF SAVE-TIAAVV-PREV2 NOT NUMERIC                                     
049259       MOVE ZERO TO SAVE-TIAAVV-PREV2                                     
049260     END-IF                                                               
049261     .                                                                    
049262     EJECT                                                                
049270 D-NEXT-PAGE SECTION.                                                     
049300     IF SAVE-IDTRANS = '6158'                                             
049400       IF MID-KDSORT1-IN   = '+'                                          
049600         MOVE SAVE-KEY-NEXT           TO SAVE-KEY                         
049700         IF SAVE-KEY = 'WDT211'                                           
049800           MOVE SAVE-ADLAGOMR-NEXT    TO W-ADLAGOMR-MIN                   
049810           MOVE SAVE-TIAAVV-NEXT      TO W-TIAAVV-LO-MIN                  
049900         ELSE                                                             
050000           IF SAVE-KEY = 'WDT212'                                         
050100             MOVE SAVE-BEFT-NEXT      TO W-BEFT-MIN                       
050200             MOVE SAVE-TIAAVV-NEXT    TO W-TIAAVV-FT-MIN                  
050300           ELSE                                                           
050400             IF SAVE-KEY = 'WDT213'                                       
050500               MOVE SAVE-IDARTNR-NEXT TO W-IDARTNR-MIN                    
050600               MOVE SAVE-TIAAVV-NEXT  TO W-TIAAVV-ART-MIN                 
052100             END-IF                                                       
052200           END-IF                                                         
052300         END-IF                                                           
052400       ELSE                                                               
052500         MOVE INF-FIRST-PAGE TO MED-IDMFSINF                              
052600         CALL WMEDKONV USING MED-WMEDAREA                                 
052700         MOVE MED-MFSINF     TO MOD-TEMFSFEL                              
052800       END-IF                                                             
052900     ELSE                                                                 
053000       PERFORM MFS-ERASE-FIELD-IN                                         
053100     END-IF                                                               
053200     .                                                                    
053300     EJECT                                                                
053400 I-PREVIOUS-PAGE SECTION.                                                 
053500     IF SAVE-IDTRANS = '6158'                                             
053600       IF MID-KDSORT1-IN   = '+'                                          
053800         MOVE SAVE-KEY-PREV           TO SAVE-KEY                         
053900         IF SAVE-KEY = 'WDT211'                                           
054000           MOVE SAVE-ADLAGOMR-PREV    TO W-ADLAGOMR-MIN                   
054010           MOVE SAVE-TIAAVV-PREV      TO W-TIAAVV-LO-MIN                  
054100         ELSE                                                             
054200           IF SAVE-KEY = 'WDT212'                                         
054300             MOVE SAVE-BEFT-PREV      TO W-BEFT-MIN                       
054400             MOVE SAVE-TIAAVV-PREV    TO W-TIAAVV-FT-MIN                  
054500           ELSE                                                           
054600             IF SAVE-KEY = 'WDT213'                                       
054700               MOVE SAVE-IDARTNR-PREV TO W-IDARTNR-MIN                    
054800               MOVE SAVE-TIAAVV-PREV  TO W-TIAAVV-ART-MIN                 
056600             END-IF                                                       
056700           END-IF                                                         
056800         END-IF                                                           
056900       ELSE                                                               
057000         MOVE INF-FIRST-PAGE TO MED-IDMFSINF                              
057100         CALL WMEDKONV USING MED-WMEDAREA                                 
057200         MOVE MED-MFSINF     TO MOD-TEMFSFEL                              
057300       END-IF                                                             
057400     ELSE                                                                 
057500       PERFORM MFS-ERASE-FIELD-IN                                         
057600     END-IF                                                               
057700     .                                                                    
057800     EJECT                                                                
057900 E-SAME-PAGE SECTION.                                                     
058000                                                                          
059100     IF MFS-ENTER                                                         
059200       MOVE SAVE-KEY-ENTER TO SAVE-KEY                                    
059300     END-IF                                                               
059400     IF SAVE-IDTRANS = '6158' OR '0551'                                   
059500       IF MID-KDSORT1-IN   = '+'                                          
059700         IF SAVE-KEY = 'WDT211'                                           
059800           MOVE SAVE-ADLAGOMR-ENTER    TO W-ADLAGOMR-MIN                  
059900           MOVE SAVE-TIAAVV-ENTER      TO W-TIAAVV-LO-MIN                 
060000         ELSE                                                             
060100           IF SAVE-KEY = 'WDT212'                                         
060200             MOVE SAVE-BEFT-ENTER      TO W-BEFT-MIN                      
060300             MOVE SAVE-TIAAVV-ENTER    TO W-TIAAVV-FT-MIN                 
060500           ELSE                                                           
060600             IF SAVE-KEY = 'WDT213'                                       
060700               MOVE SAVE-IDARTNR-ENTER TO W-IDARTNR-MIN                   
060800               MOVE SAVE-TIAAVV-ENTER  TO W-TIAAVV-ART-MIN                
062300             END-IF                                                       
062400           END-IF                                                         
062500         END-IF                                                           
062600       ELSE                                                               
062700         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
062800         CALL WMEDKONV       USING MED-WMEDAREA                           
062900         MOVE MED-MFSINF     TO MOD-TEMFSFEL                              
063000         PERFORM EA-MID-INDATA-FOR-MOD                                    
063100       END-IF                                                             
063200     ELSE                                                                 
063300       PERFORM MFS-ERASE-FIELD-IN                                         
063400     END-IF                                                               
063500     .                                                                    
063600     EJECT                                                                
063700 EA-MID-INDATA-FOR-MOD SECTION.                                           
063800       MOVE 1 TO INDX                                                     
063900       PERFORM UNTIL INDX > MAX-INDX                                      
064000         IF MID-BEFT (INDX) = ALL '+' OR SPACE                            
064100            MOVE MFS-ERASE-FIELD   TO MOD-BEFT (INDX)                     
064200         ELSE                                                             
064300            MOVE MID-BEFT (INDX)   TO MOD-BEFT (INDX)                     
064400         END-IF                                                           
064500                                                                          
064600         IF MID-IDARTNR (INDX) = ALL '+' OR SPACE                         
064700            MOVE MFS-ERASE-FIELD      TO MOD-IDARTNR (INDX)               
064800         ELSE                                                             
064900            MOVE MID-IDARTNR (INDX)   TO MOD-IDARTNR (INDX)               
065000         END-IF                                                           
065100                                                                          
066470                                                                          
066500         IF MID-ADINPORT (INDX) = ALL '+' OR SPACE                        
066600            MOVE MFS-ERASE-FIELD       TO MOD-ADINPORT (INDX)             
066700         ELSE                                                             
066800            MOVE MID-ADINPORT (INDX)   TO MOD-ADINPORT (INDX)             
066900         END-IF                                                           
067000                                                                          
067100         IF MID-TIAAVV-FOM (INDX) = ALL '+' OR SPACE                      
067200            MOVE MFS-ERASE-FIELD       TO MOD-TIAAVV-FOM (INDX)           
067300         ELSE                                                             
067400            MOVE MID-TIAAVV-FOM (INDX) TO MOD-TIAAVV-FOM (INDX)           
067500         END-IF                                                           
067600                                                                          
067700         IF MID-IDUSER (INDX) = ALL '+' OR SPACE                          
067800            MOVE MFS-ERASE-FIELD       TO MOD-IDUSER (INDX)               
067900         ELSE                                                             
068000            MOVE MID-IDUSER (INDX)     TO MOD-IDUSER (INDX)               
068100         END-IF                                                           
068200                                                                          
068300         IF MID-TIUPPDAT (INDX) = ALL '+' OR SPACE                        
068400            MOVE MFS-ERASE-FIELD       TO MOD-TIUPPDAT (INDX)             
068500         ELSE                                                             
068600            MOVE MID-TIUPPDAT (INDX)   TO MOD-TIUPPDAT (INDX)             
068700         END-IF                                                           
068800         ADD 1 TO INDX                                                    
068900       END-PERFORM                                                        
069000     .                                                                    
069100                                                                          
069110                                                                          
069200 F-READ-SHOW-INFO SECTION.                                                
069210                                                                          
069220     PERFORM IMS-GU-WDT201                                                
069221     MOVE GDC-IDDC     TO MOD-IDDC                                        
069222     MOVE GDC-ADINPORT TO MOD-ADINPORT-CDC                                
069230                                                                          
069310     IF W-KDSORT1 = 'L' OR SAVE-KEY = 'WDT211'                            
069400       PERFORM FA-READ-WDT211                                             
069410       MOVE 'WDT211'        TO SAVE-KEY                                   
069420       MOVE MFS-CLOSE-FIELD TO MOD-BEFT-UP-ATTR                           
069430                               MOD-IDARTNR-UP-ATTR                        
069500     ELSE                                                                 
069610       IF W-KDSORT1 = 'F' OR SAVE-KEY = 'WDT212'                          
069700         PERFORM FB-READ-WDT212                                           
069710           MOVE 'WDT212'  TO SAVE-KEY                                     
069711       MOVE MFS-CLOSE-FIELD TO MOD-ADLAGOMR-UP-ATTR                       
069712                               MOD-IDARTNR-UP-ATTR                        
069800       ELSE                                                               
069910         IF W-KDSORT1 = 'A' OR SAVE-KEY = 'WDT213'                        
070000           PERFORM FC-READ-WDT213                                         
070100             MOVE 'WDT213' TO SAVE-KEY                                    
070200       MOVE MFS-CLOSE-FIELD TO MOD-ADLAGOMR-UP-ATTR                       
070300                               MOD-BEFT-UP-ATTR                           
070700         END-IF                                                           
070800       END-IF                                                             
070900     END-IF                                                               
071000     MOVE '002'      TO MSGI-KDCALL                                       
071100     MOVE '6158'     TO SAVE-IDTRANS                                      
071200     MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                    
071300     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
071400     .                                                                    
071500     EJECT                                                                
079300 FA-READ-WDT211   SECTION.                                                
079400*----------------------------------------------------*                    
079500** FETCH ALL THE AREAS WITH EXCEPTION GATE                                
079600*----------------------------------------------------*                    
079610                                                                          
079700     PERFORM IMS-GNP-WDT211                                               
079800     IF SEGMENT-MISSING                                                   
079900       MOVE INF-NO-RECORDS      TO MED-IDMFSINF                           
080000       CALL WMEDKONV USING MED-WMEDAREA                                   
080100       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
080200     ELSE                                                                 
080300       IF MFS-PREVIOUS                                                    
080400         MOVE SAVE-ADLAGOMR-PREV2  TO SAVE-ADLAGOMR-PREV                  
080500         MOVE SAVE-TIAAVV-PREV2    TO SAVE-TIAAVV-PREV                    
080600       ELSE                                                               
082700         IF MFS-IDPFK = '7'                                               
083200           MOVE 'WDT211'         TO SAVE-KEY-PREV                         
083300                                    SAVE-KEY-PREV2                        
083400         END-IF                                                           
083410       END-IF                                                             
083500                                                                          
083600       MOVE GLO-ADLAGOMR        TO SAVE-ADLAGOMR-ENTER                    
083700       MOVE GLO-TIAAVV-FOM      TO SAVE-TIAAVV-ENTER                      
083800       MOVE 'WDT211'            TO SAVE-KEY-ENTER                         
083900       MOVE +1 TO INDX                                                    
084000       PERFORM UNTIL INDX > MAX-INDX                                      
084100         IF SEGMENT-FOUND                                                 
084200           MOVE MFS-ERASE-FIELD   TO MOD-IDARTNR      (INDX)              
084300                                     MOD-BEFT         (INDX)              
084400           MOVE GLO-ADLAGOMR      TO MOD-ADLAGOMR     (INDX)              
084500           MOVE GLO-TIAAVV-FOM    TO MOD-TIAAVV-FOM   (INDX)              
084510           MOVE GLO-ADINPORT-LO   TO MOD-ADINPORT     (INDX)              
084600           MOVE GLO-IDUSER        TO MOD-IDUSER       (INDX)              
084700           MOVE GLO-TIUPPDAT      TO MOD-TIUPPDAT     (INDX)              
084800           PERFORM IMS-GNP-WDT211                                         
085300         ELSE                                                             
085400           PERFORM XX-CLOSE-ERASE-FIELD                                   
085500         END-IF                                                           
085600         ADD +1 TO INDX                                                   
085700       END-PERFORM                                                        
085800                                                                          
085900       IF SEGMENT-FOUND                                                   
086200         MOVE 'WDT211'            TO SAVE-KEY-NEXT                        
086300         IF MFS-UPDATE                                                    
086400           CONTINUE                                                       
086500         ELSE                                                             
086510           MOVE GLO-ADLAGOMR    TO SAVE-ADLAGOMR-NEXT                     
086520           MOVE GLO-TIAAVV-FOM  TO SAVE-TIAAVV-NEXT                       
086600           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
086700           CALL WMEDKONV USING MED-WMEDAREA                               
086800           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
086900         END-IF                                                           
087000       ELSE                                                               
087100         MOVE ERR-LAST-PAGE    TO MED-IDMFSFEL                            
087200         CALL WMEDKONV USING MED-WMEDAREA                                 
087300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
087600         MOVE 'WDT211'            TO SAVE-KEY-NEXT                        
087700       END-IF                                                             
087800     END-IF                                                               
087900     .                                                                    
088000     EJECT                                                                
088100 FB-READ-WDT212       SECTION.                                            
088200*----------------------------------------------------*                    
088300** FETCH ALL THE PACKAGING TYPES FROM WDT212                              
088400*----------------------------------------------------*                    
088410                                                                          
088500     PERFORM IMS-GNP-WDT212                                               
088600     IF SEGMENT-MISSING                                                   
088700       MOVE INF-NO-RECORDS      TO MED-IDMFSINF                           
088800       CALL WMEDKONV USING MED-WMEDAREA                                   
088900       MOVE MED-TEMFSINF        TO MOD-TEMFSINF                           
089000     ELSE                                                                 
089100       IF MFS-PREVIOUS                                                    
089200         MOVE SAVE-BEFT-PREV2   TO SAVE-BEFT-PREV                         
089210         MOVE SAVE-TIAAVV-PREV2 TO SAVE-TIAAVV-PREV                       
089300       ELSE                                                               
089400         IF SAVE-BEFT-ENTER > 0 AND SAVE-BEFT-NEXT > 0                    
089500            AND (SAVE-BEFT-ENTER = SAVE-BEFT-NEXT)                        
089600           CONTINUE                                                       
089700         ELSE                                                             
089800           IF SAVE-BEFT-PREV  IS NUMERIC                                  
089900             MOVE SAVE-BEFT-PREV   TO SAVE-BEFT-PREV2                     
089910             MOVE SAVE-TIAAVV-PREV TO SAVE-TIAAVV-PREV2                   
090000           END-IF                                                         
090100           IF SAVE-BEFT-ENTER IS NUMERIC                                  
090200             MOVE SAVE-BEFT-ENTER   TO SAVE-BEFT-PREV                     
090210             MOVE SAVE-TIAAVV-ENTER TO SAVE-TIAAVV-PREV                   
090300             MOVE SAVE-KEY-ENTER    TO SAVE-KEY-PREV                      
090400           END-IF                                                         
090500         END-IF                                                           
090600       END-IF                                                             
090700                                                                          
090800       IF MFS-IDPFK = '7'                                                 
090900         MOVE GFT-BEFT          TO SAVE-BEFT-PREV                         
091000                                   SAVE-BEFT-PREV2                        
091010         MOVE GFT-TIAAVV-FOM    TO SAVE-TIAAVV-PREV                       
091020                                   SAVE-TIAAVV-PREV2                      
091100         MOVE 'WDT212'          TO SAVE-KEY-PREV                          
091200                                   SAVE-KEY-PREV2                         
091300       END-IF                                                             
091400                                                                          
091500       MOVE GFT-BEFT            TO SAVE-BEFT-ENTER                        
091510       MOVE GFT-TIAAVV-FOM      TO SAVE-TIAAVV-ENTER                      
091600       MOVE 'WDT212'            TO SAVE-KEY-ENTER                         
091700       MOVE +1 TO INDX                                                    
091800       PERFORM UNTIL INDX > MAX-INDX                                      
091900         IF SEGMENT-FOUND                                                 
091910           MOVE MFS-ERASE-FIELD TO MOD-IDARTNR      (INDX)                
091920                                   MOD-ADLAGOMR     (INDX)                
092000           MOVE GFT-BEFT        TO MOD-BEFT         (INDX)                
092010           MOVE GFT-TIAAVV-FOM    TO MOD-TIAAVV-FOM (INDX)                
092200           MOVE GFT-ADINPORT-FT TO MOD-ADINPORT     (INDX)                
092500           MOVE GFT-IDUSER      TO MOD-IDUSER       (INDX)                
092600           MOVE GFT-TIUPPDAT    TO MOD-TIUPPDAT     (INDX)                
092700           PERFORM IMS-GNP-WDT212                                         
092800         ELSE                                                             
092900           PERFORM XX-CLOSE-ERASE-FIELD                                   
093000         END-IF                                                           
093100         ADD +1 TO INDX                                                   
093200       END-PERFORM                                                        
093300                                                                          
093400       IF SEGMENT-FOUND                                                   
093500         MOVE GFT-BEFT            TO SAVE-BEFT-NEXT                       
093510         MOVE GFT-TIAAVV-FOM      TO SAVE-TIAAVV-NEXT                     
093600         MOVE 'WDT212'            TO SAVE-KEY-NEXT                        
093700         IF MFS-UPDATE                                                    
093800           CONTINUE                                                       
093900         ELSE                                                             
094000           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
094100           CALL WMEDKONV USING MED-WMEDAREA                               
094200           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
094300         END-IF                                                           
094400       ELSE                                                               
094500         MOVE ERR-LAST-PAGE    TO MED-IDMFSFEL                            
094600         CALL WMEDKONV USING MED-WMEDAREA                                 
094700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
094800         MOVE SAVE-BEFT-ENTER      TO SAVE-BEFT-NEXT                      
094900         MOVE 'WDT212'             TO SAVE-KEY-NEXT                       
095000       END-IF                                                             
095100     END-IF                                                               
095200     .                                                                    
095300     EJECT                                                                
095310 FC-READ-WDT213       SECTION.                                            
095320*----------------------------------------------------*                    
095330** FETCH ALL THE PART NUMBERS WITH EXCEPTION GATE                         
095340*----------------------------------------------------*                    
095341                                                                          
095350       PERFORM IMS-GNP-WDT213                                             
095360       IF SEGMENT-MISSING                                                 
095370         MOVE INF-NO-RECORDS TO MED-IDMFSINF                              
095380         CALL WMEDKONV USING MED-WMEDAREA                                 
095390         MOVE MED-TEMFSINF   TO MOD-TEMFSINF                              
095391       ELSE                                                               
095392         IF MFS-PREVIOUS                                                  
095393           MOVE SAVE-IDARTNR-PREV2   TO SAVE-IDARTNR-PREV                 
095394           MOVE SAVE-TIAAVV-PREV2    TO SAVE-TIAAVV-PREV                  
095395         ELSE                                                             
095396           IF SAVE-IDARTNR-ENTER > 0 AND SAVE-IDARTNR-NEXT > 0            
095397              AND (SAVE-IDARTNR-ENTER = SAVE-IDARTNR-ENTER)               
095398             CONTINUE                                                     
095399           ELSE                                                           
095400             IF SAVE-IDARTNR-PREV IS NUMERIC                              
095401               MOVE SAVE-IDARTNR-PREV  TO SAVE-IDARTNR-PREV2              
095402               MOVE SAVE-TIAAVV-PREV   TO SAVE-TIAAVV-PREV2               
095403             END-IF                                                       
095404             IF SAVE-IDARTNR-ENTER IS NUMERIC                             
095405               MOVE SAVE-IDARTNR-ENTER TO SAVE-IDARTNR-PREV               
095406               MOVE SAVE-TIAAVV-ENTER  TO SAVE-TIAAVV-PREV                
095407               MOVE SAVE-KEY-ENTER     TO SAVE-KEY-PREV                   
095408             END-IF                                                       
095409           END-IF                                                         
095410         END-IF                                                           
095411                                                                          
095412         IF MFS-IDPFK = '7'                                               
095413           MOVE GART-IDARTNR     TO SAVE-IDARTNR-PREV                     
095414                                    SAVE-IDARTNR-PREV2                    
095415           MOVE GART-TIAAVV-FOM  TO SAVE-TIAAVV-PREV                      
095416                                    SAVE-TIAAVV-PREV2                     
095417           MOVE 'WDT213'         TO SAVE-KEY-PREV                         
095418                                    SAVE-KEY-PREV2                        
095419         END-IF                                                           
095420                                                                          
095421         MOVE GART-IDARTNR          TO SAVE-IDARTNR-ENTER                 
095422         MOVE GART-TIAAVV-FOM       TO SAVE-TIAAVV-ENTER                  
095423         MOVE 'WDT213'              TO SAVE-KEY-ENTER                     
095424         MOVE +1 TO INDX                                                  
095425         PERFORM UNTIL INDX > MAX-INDX                                    
095426           IF SEGMENT-FOUND                                               
095427             MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR    (INDX)             
095428                                       MOD-BEFT        (INDX)             
095429             MOVE GART-IDARTNR      TO MOD-IDARTNR     (INDX)             
095430             MOVE GART-ADINPORT-ART TO MOD-ADINPORT    (INDX)             
095431             MOVE GART-IDUSER       TO MOD-IDUSER      (INDX)             
095432             MOVE GART-TIAAVV-FOM   TO MOD-TIAAVV-FOM  (INDX)             
095433             MOVE GART-TIUPPDAT     TO MOD-TIUPPDAT    (INDX)             
095434             PERFORM IMS-GNP-WDT213                                       
095435           ELSE                                                           
095436             PERFORM XX-CLOSE-ERASE-FIELD                                 
095437           END-IF                                                         
095438           ADD +1 TO INDX                                                 
095439         END-PERFORM                                                      
095440                                                                          
095441         IF SEGMENT-FOUND                                                 
095442           MOVE GART-IDARTNR      TO SAVE-IDARTNR-NEXT                    
095443           MOVE GART-TIAAVV-FOM   TO SAVE-TIAAVV-NEXT                     
095444           MOVE 'WDT213'          TO SAVE-KEY-NEXT                        
095445           IF MFS-UPDATE                                                  
095446             CONTINUE                                                     
095447           ELSE                                                           
095448             MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                    
095449             CALL WMEDKONV USING MED-WMEDAREA                             
095450             MOVE MED-TEMFSINF TO MOD-TEMFSINF                            
095451           END-IF                                                         
095452         ELSE                                                             
095453           MOVE ERR-LAST-PAGE    TO MED-IDMFSFEL                          
095454           CALL WMEDKONV USING MED-WMEDAREA                               
095455           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
095456           MOVE SAVE-IDARTNR-ENTER TO SAVE-IDARTNR-NEXT                   
095457           MOVE 'WDT213'           TO SAVE-KEY-NEXT                       
095458         END-IF                                                           
095459       END-IF                                                             
095460     .                                                                    
095470     EJECT                                                                
116700 XX-CLOSE-ERASE-FIELD SECTION.                                            
116800     MOVE MFS-CLOSE-FIELD TO MOD-KDCMDVAL-ATTR (INDX)                     
116900     MOVE MFS-ERASE-FIELD TO   MOD-BEFT        (INDX)                     
117000                               MOD-ADLAGOMR    (INDX)                     
117100                               MOD-IDARTNR     (INDX)                     
117300                               MOD-ADINPORT    (INDX)                     
117400                               MOD-TIAAVV-FOM  (INDX)                     
117500                               MOD-IDUSER      (INDX)                     
117600                               MOD-TIUPPDAT    (INDX)                     
117700                                                                          
117800     .                                                                    
117900     EJECT                                                                
118000 G-CHECK-INPUT SECTION.                                                   
118100     MOVE YES  TO INDATA-SW                                               
118200     IF (MID-KDCMDVAL (1)     = ALL '+' OR SPACE) AND                     
118300        (MID-KDCMDVAL (2)     = ALL '+' OR SPACE) AND                     
118400        (MID-KDCMDVAL (3)     = ALL '+' OR SPACE) AND                     
118500        (MID-KDCMDVAL (4)     = ALL '+' OR SPACE) AND                     
118600        (MID-KDCMDVAL (5)     = ALL '+' OR SPACE) AND                     
118700        (MID-KDCMDVAL (6)     = ALL '+' OR SPACE) AND                     
118800        (MID-KDCMDVAL (7)     = ALL '+' OR SPACE) AND                     
118900        (MID-KDCMDVAL (8)     = ALL '+' OR SPACE) AND                     
119000        (MID-KDCMDVAL (9)     = ALL '+' OR SPACE) AND                     
119100        (MID-KDCMDVAL (10)    = ALL '+' OR SPACE) AND                     
119200        (MID-KDCMDVAL (11)    = ALL '+' OR SPACE) AND                     
119300        (MID-KDCMDVAL (12)    = ALL '+' OR SPACE) AND                     
119400        (MID-ADLAGOMR-UP      = ALL '+' OR SPACE) AND                     
119410        (MID-BEFT-UP          = ALL '+' OR SPACE) AND                     
119500        (MID-IDARTNR-UP       = ALL '+' OR SPACE) AND                     
119600        (MID-ADINPORT-UP      = ALL '+' OR SPACE) AND                     
119900        (MID-TIAAVV-FOM-UP    = ALL '+' OR SPACE)                         
120000       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
120100       CALL WMEDKONV USING MED-WMEDAREA                                   
120200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
120300       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
120400       MOVE NOO        TO INDATA-SW                                       
120500     ELSE                                                                 
120600       PERFORM S03-CHECK-FOR-DELETE                                       
120700       IF NO-DELETE AND INDATA-OK                                         
120800*      VALIDATE COMBINATIONE                                              
120801       IF (MID-ADLAGOMR-UP NOT = '++' AND                                 
120802          (MID-BEFT-UP NOT = '++'  OR                                     
120803           MID-IDARTNR-UP NOT = ALL '+'))                                 
120804       OR                                                                 
120805          (MID-BEFT-UP NOT = '++' AND                                     
120806          (MID-ADLAGOMR-UP NOT = '++' OR                                  
120808           MID-IDARTNR-UP NOT = ALL '+'))                                 
120809       OR                                                                 
120810          (MID-IDARTNR-UP NOT = ALL '+' AND                               
120811          (MID-BEFT-UP NOT = '++' OR                                      
120812           MID-ADLAGOMR-UP NOT = '++'))                                   
120814          MOVE NOO TO INDATA-SW                                           
120815          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-ADLAGOMR-UP-ATTR              
120816                                        MOD-BEFT-UP-ATTR                  
120817                                        MOD-IDARTNR-UP-ATTR               
120818          PERFORM MFS-DONT-TOUCH-FAELT-UP                                 
120819       END-IF                                                             
120820*                                                                         
120821*      VALIDATE TIAAVV-FOM-UP                                             
120822         IF MID-TIAAVV-FOM-UP = SPACES OR ALL '+'                         
120823            MOVE W-CURR-TIAAVV TO MID-TIAAVV-FOM-UP                       
120824         END-IF                                                           
120825                                                                          
120826         MOVE MID-TIAAVV-FOM-UP TO MOD-TIAAVV-FOM-UP                      
120827         IF MID-TIAAVV-FOM-UP IS NOT NUMERIC                              
120828            MOVE MFS-ALPHA-FIELD-WRONG TO MOD-TIAAVV-FOM-UP-ATTR          
120829            MOVE NOO TO INDATA-SW                                         
120830            PERFORM MFS-DONT-TOUCH-FAELT-UP                               
120831         ELSE                                                             
120832            IF MID-TIAAVV-FOM-UP < W-CURR-TIAAVV                          
120833              MOVE MFS-ALPHA-FIELD-WRONG TO MOD-TIAAVV-FOM-UP-ATTR        
120834              MOVE NOO TO INDATA-SW                                       
120835              PERFORM MFS-DONT-TOUCH-FAELT-UP                             
120836            ELSE                                                          
120837              MOVE 'AAVV'            TO DAT-KDDATFORM                     
120838              MOVE MID-TIAAVV-FOM-UP TO DAT-I-TIDATUM                     
120839                                                                          
120840              CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM             
120841                                  DAT-O-TIDATUM DAT-KDSVAR                
120842              IF NOT DAT-KDSVAR-OK                                        
120843              MOVE MFS-ALPHA-FIELD-WRONG TO MOD-TIAAVV-FOM-UP-ATTR        
120844                 MOVE NOO TO INDATA-SW                                    
120845                 PERFORM MFS-DONT-TOUCH-FAELT-UP                          
120849              END-IF                                                      
120850            END-IF                                                        
120851         END-IF                                                           
120852                                                                          
120853       IF INDATA-OK                                                       
120860*      VALIDATE ADLAGOMR                                                  
120900         IF MID-ADLAGOMR-UP > SPACES AND                                  
121000            MID-ADLAGOMR-UP NOT = '++'                                    
121100           INSPECT MID-ADLAGOMR-UP REPLACING LEADING SPACE BY ZERO        
121200           MOVE MID-ADLAGOMR-UP  TO MOD-ADLAGOMR-UP                       
121300           IF MID-ADLAGOMR-UP IS NOT NUMERIC                              
121400             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-ADLAGOMR-UP-ATTR           
121500             MOVE NOO TO INDATA-SW                                        
121510           ELSE                                                           
121520             MOVE MID-ADLAGOMR-UP TO W-ADLAGOMR-GLO                       
121530             MOVE MID-TIAAVV-FOM-UP TO W-TIAAVV-GLO                       
121540             PERFORM IMS-GHU-WDT211                                       
121550             IF SEGMENT-FOUND                                             
121560               MOVE MFS-ALPHA-FIELD-WRONG TO MOD-ADLAGOMR-UP-ATTR         
121570                                            MOD-TIAAVV-FOM-UP-ATTR        
121580               MOVE NOO TO INDATA-SW                                      
121581             ELSE                                                         
121584               MOVE 'L'           TO MID-KDSORT1-IN                       
121593             END-IF                                                       
121600           END-IF                                                         
121700         END-IF                                                           
121800*                                                                         
121810*      VALIDATE BEFT                                                      
121820         IF MID-BEFT-UP > SPACES AND                                      
121830            MID-BEFT-UP NOT = '++'                                        
121840           INSPECT MID-BEFT-UP REPLACING LEADING SPACE BY ZERO            
121850           MOVE MID-BEFT-UP  TO MOD-BEFT-UP                               
121860           IF MID-BEFT-UP IS NOT NUMERIC                                  
121870             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-BEFT-UP-ATTR               
121880             MOVE NOO TO INDATA-SW                                        
121881           ELSE                                                           
121882             MOVE MID-BEFT-UP     TO W-BEFT-GFT                           
121883             MOVE MID-TIAAVV-FOM-UP TO W-TIAAVV-GFT                       
121884             PERFORM IMS-GHU-WDT212                                       
121885             IF SEGMENT-FOUND                                             
121886               MOVE MFS-ALPHA-FIELD-WRONG TO MOD-BEFT-UP-ATTR             
121887                                            MOD-TIAAVV-FOM-UP-ATTR        
121888               MOVE NOO TO INDATA-SW                                      
121889             ELSE                                                         
121890                MOVE 'F'          TO MID-KDSORT1-IN                       
121892             END-IF                                                       
121893           END-IF                                                         
121894         END-IF                                                           
121895*                                                                         
121900*      VALIDATE ADINPORT                                                  
122000         IF MID-ADINPORT-UP > SPACES                                      
122100           MOVE MID-ADINPORT-UP TO MOD-ADINPORT-UP                        
122200         ELSE                                                             
122300           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-ADINPORT-UP-ATTR             
122400           MOVE NOO TO INDATA-SW                                          
122500         END-IF                                                           
122600*                                                                         
122700**     VALIDATE PART NUMBER                                               
122800         IF MID-IDARTNR-UP > SPACES AND                                   
122900           MID-IDARTNR-UP   NOT = '+++++++++'                             
123000           INSPECT MID-IDARTNR-UP REPLACING LEADING SPACE BY ZERO         
123100           MOVE MID-IDARTNR-UP TO MOD-IDARTNR-UP                          
123200           IF MID-IDARTNR-UP IS NOT NUMERIC                               
123300             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDARTNR-UP-ATTR            
123400             MOVE NOO TO INDATA-SW                                        
123401           ELSE                                                           
123410             MOVE MID-IDARTNR-UP  TO W-IDARTNR-GART                       
123420             MOVE MID-TIAAVV-FOM-UP TO W-TIAAVV-GART                      
123430             PERFORM IMS-GHU-WDT213                                       
123440             IF SEGMENT-FOUND                                             
123450               MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDARTNR-UP-ATTR          
123451                                            MOD-TIAAVV-FOM-UP-ATTR        
123460               MOVE NOO TO INDATA-SW                                      
123470             ELSE                                                         
123480               MOVE 'A'           TO MID-KDSORT1-IN                       
123500             END-IF                                                       
123510           END-IF                                                         
123600         END-IF                                                           
123700*                                                                         
127100*                                                                         
127200         IF INDATA-WRONG                                                  
127300           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
127400           CALL WMEDKONV USING MED-WMEDAREA                               
127500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
127600           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
127700           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
127800         END-IF                                                           
127900*                                                                         
128000*      IF MID-BEFT-UP       IS NOT NUMERIC       OR                       
128100*        (MID-ADINPORT-UP =   ALL '+' OR SPACES) OR                       
128700*         MID-TIAAVV-FOM-UP   IS NUMERIC)        OR                       
129000*        (MID-IDARTNR-UP      IS NUMERIC AND                              
129100*         MID-TIAAVV-FOM-UP   NOT NUMERIC)       OR                       
129600*        (MID-TIAAVV-FOM-UP   IS NUMERIC AND                              
129700*         MID-IDARTNR-UP      IS NOT NUMERIC))                            
129800*        MOVE NOO                   TO INDATA-SW                          
129900*        MOVE ERR-UPDATE-NOT-POSS   TO MED-IDMFSFEL                       
130000*        CALL WMEDKONV USING MED-WMEDAREA                                 
130100*        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
130200*        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
130300**       PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
130400*      END-IF                                                             
130500                                                                          
132100**     CHECK IF PART NUMBER EXISTS IN WDK601                              
132200         IF INDATA-OK                                                     
132300           IF MID-IDARTNR-UP NOT = ALL '+'                                
132400             MOVE MID-IDARTNR-UP          TO W-IDARTNR                    
132500             PERFORM IMS-GU-WDK601                                        
132600             IF SEGMENT-MISSING                                           
132700               MOVE NOO                   TO INDATA-SW                    
132800               MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDARTNR-UP-ATTR          
132900               MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                 
133000               CALL WMEDKONV USING MED-WMEDAREA                           
133100               MOVE MED-MFSFEL            TO MOD-TEMFSFEL                 
133200             END-IF                                                       
133300           END-IF                                                         
133400         END-IF                                                           
133500                                                                          
135100**     CHECK IF DATE ENTER IS VALID                                       
135200         IF INDATA-OK                                                     
135300           IF MID-TIAAVV-FOM-UP = '++++' OR SPACE                         
135301              MOVE W-CURR-TIAAVV TO MID-TIAAVV-FOM-UP                     
135310           IF MID-TIAAVV-FOM-UP > SPACE                                   
135400             IF MID-TIAAVV-FOM-UP = W-CURR-TIAAVV                         
135500               CONTINUE                                                   
135600             ELSE                                                         
136201               MOVE NOO TO INDATA-SW                                      
136301               MOVE MFS-ALPHA-FIELD-WRONG TO                              
136401                                         MOD-TIAAVV-FOM-UP-ATTR           
136501               MOVE ERR-UPDATE-NOT-POSS TO MED-IDMFSFEL                   
136601               CALL WMEDKONV USING MED-WMEDAREA                           
136701               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
136801             END-IF                                                       
137001           END-IF                                                         
137101         END-IF                                                           
137201       END-IF                                                             
137202       END-IF                                                             
137301*                                                                         
137401       IF INDATA-WRONG                                                    
137501         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
137601         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
137701       END-IF                                                             
137801     END-IF                                                               
137901     .                                                                    
138001     EJECT                                                                
138101 H-UPDATE SECTION.                                                        
138201     MOVE NOO TO UPDATE-SW                                                
138301     IF DELETE-YES                                                        
138401       MOVE +1 TO INDX                                                    
138501       PERFORM UNTIL INDX > MAX-INDX                                      
138601         IF MID-KDCMDVAL (INDX) = 'B' OR 'D'                              
138602                                                                          
138603           IF MID-ADLAGOMR (INDX) > SPACES                                
138604             INSPECT MID-ADLAGOMR (INDX) REPLACING LEADING                
138605                     SPACE BY ZERO                                        
138606             MOVE MID-ADLAGOMR (INDX)   TO W-ADLAGOMR-GLO                 
138607             MOVE MID-TIAAVV-FOM (INDX) TO W-TIAAVV-GLO                   
138608             PERFORM IMS-GHU-WDT211                                       
138609             IF SEGMENT-FOUND                                             
138610               PERFORM IMS-DLET-WDT211                                    
138611               MOVE YES              TO UPDATE-SW                         
138620             END-IF                                                       
138630           END-IF                                                         
138640                                                                          
138701           IF MID-BEFT (INDX) > SPACES                                    
138801             INSPECT MID-BEFT (INDX)    REPLACING LEADING                 
138901                     SPACE BY ZERO                                        
139301             MOVE MID-BEFT (INDX)       TO W-BEFT-GFT                     
139302             MOVE MID-TIAAVV-FOM (INDX) TO W-TIAAVV-GFT                   
139401             PERFORM IMS-GHU-WDT212                                       
139501             IF SEGMENT-FOUND                                             
139601               PERFORM IMS-DLET-WDT212                                    
139701               MOVE YES              TO UPDATE-SW                         
139801             END-IF                                                       
141701           END-IF                                                         
141702                                                                          
141760           IF MID-IDARTNR (INDX) > SPACES                                 
141790             INSPECT MID-IDARTNR (INDX) REPLACING LEADING                 
141800                     SPACE BY ZERO                                        
141802             MOVE MID-IDARTNR (INDX)    TO W-IDARTNR-GART                 
141803             MOVE MID-TIAAVV-FOM (INDX) TO W-TIAAVV-GART                  
141804             PERFORM IMS-GHU-WDT213                                       
141805             IF SEGMENT-FOUND                                             
141806               PERFORM IMS-DLET-WDT213                                    
141807               MOVE YES              TO UPDATE-SW                         
141808             END-IF                                                       
141809           END-IF                                                         
141810         END-IF                                                           
141901         ADD +1 TO INDX                                                   
142001       END-PERFORM                                                        
142101     END-IF                                                               
142102                                                                          
142201     IF MID-IDARTNR-UP NOT = ALL '+' AND > 0                              
142501                                                                          
142701        MOVE W-IDARTNR-GART     TO GART-IDARTNR                           
142702        MOVE MID-TIAAVV-FOM-UP  TO GART-TIAAVV-FOM                        
142801        MOVE MID-ADINPORT-UP    TO GART-ADINPORT-ART                      
142901        MOVE MSGI-IDUSER        TO GART-IDUSER                            
143101        MOVE TODAYS-DATE        TO GART-TIUPPDAT                          
144501        PERFORM IMS-ISRT-WDT213                                           
144502        MOVE YES                TO UPDATE-SW                              
150701     END-IF                                                               
150702                                                                          
150703     IF MID-BEFT-UP NOT = ALL '+'                                         
150704                                                                          
150705        MOVE MID-BEFT-UP        TO GFT-BEFT                               
150706        MOVE MID-TIAAVV-FOM-UP  TO GFT-TIAAVV-FOM                         
150707        MOVE MID-ADINPORT-UP    TO GFT-ADINPORT-FT                        
150708        MOVE MSGI-IDUSER        TO GFT-IDUSER                             
150709        MOVE TODAYS-DATE        TO GFT-TIUPPDAT                           
150710        PERFORM IMS-ISRT-WDT212                                           
150711        MOVE YES                TO UPDATE-SW                              
150712     END-IF                                                               
150713                                                                          
150714     IF MID-ADLAGOMR-UP NOT = ALL '+'                                     
150715                                                                          
150716        MOVE MID-ADLAGOMR-UP    TO GLO-ADLAGOMR                           
150717        MOVE MID-TIAAVV-FOM-UP  TO GLO-TIAAVV-FOM                         
150718        MOVE MID-ADINPORT-UP    TO GLO-ADINPORT-LO                        
150719        MOVE MSGI-IDUSER        TO GLO-IDUSER                             
150720        MOVE TODAYS-DATE        TO GLO-TIUPPDAT                           
150721        PERFORM IMS-ISRT-WDT211                                           
150722        MOVE YES                TO UPDATE-SW                              
150730     END-IF                                                               
150801*                                                                         
150901     IF UPDATE-SUCCESS                                                    
151001       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
151101       CALL WMEDKONV        USING MED-WMEDAREA                            
151201       MOVE MED-MFSINF      TO MOD-TEMFSINF                               
151202       PERFORM MFS-RENSA-FAELT-UP                                         
151301     END-IF                                                               
151401     .                                                                    
151501     EJECT                                                                
151601 S01-CHECK-INPUT SECTION.                                                 
151701     MOVE +1   TO INDX                                                    
151801     MOVE +0   TO W-CMD-CNT                                               
151901*                                                                         
152001     PERFORM UNTIL INDX > MAX-INDX                                        
152101       IF MID-KDCMDVAL (INDX) = 'B' OR 'D'                                
152201         ADD 1  TO W-CMD-CNT                                              
152301         MOVE MID-KDCMDVAL (INDX) TO MOD-KDCMDVAL (INDX)                  
152401         IF W-CMD-CNT > 1                                                 
152501          MOVE NOO                TO CHK-CMD-SW INDATA-SW                 
152601          MOVE MFS-ADD-READ-HILIGHT-FIELD                                 
152701                                 TO MOD-KDCMDVAL-ATTR (INDX)              
152801         END-IF                                                           
152901       END-IF                                                             
153001*                                                                         
153101       IF MID-KDCMDVAL  (INDX) NOT = ALL '+'                              
153201       AND MID-KDCMDVAL (INDX) NOT = SPACE                                
153301         IF  MID-KDCMDVAL (INDX) NOT = 'B'                                
153601         AND MID-KDCMDVAL (INDX) NOT = 'D'                                
153701           MOVE MID-KDCMDVAL (INDX) TO MOD-KDCMDVAL (INDX)                
153801           MOVE MFS-ALPHA-FIELD-WRONG                                     
153901                                    TO MOD-KDCMDVAL-ATTR (INDX)           
154001           MOVE NOO                 TO INDATA-SW CHK-CMD-SW               
154101         END-IF                                                           
154201       END-IF                                                             
154301       ADD 1 TO INDX                                                      
154401     END-PERFORM                                                          
154501                                                                          
154601     IF CMD-CHK-WRONG                                                     
154701       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
154801       CALL WMEDKONV USING MED-WMEDAREA                                   
154901       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
155001       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
155101     END-IF                                                               
155201     .                                                                    
155301     EJECT                                                                
155401 S02-SET-CURSOR   SECTION.                                                
155501     MOVE MFS-ADD-SET-CURSOR      TO MOD-BEFT-UP-ATTR                     
155602     MOVE MFS-ERASE-FIELD         TO MOD-KDCMDVAL (INDX)                  
155701     MOVE MID-BEFT         (INDX) TO MOD-BEFT-UP                          
155702                                                                          
155703     IF MID-IDARTNR (INDX) > SPACES                                       
155801       MOVE MID-IDARTNR    (INDX) TO MOD-IDARTNR-UP                       
155812     ELSE                                                                 
155813       MOVE MFS-ERASE-FIELD       TO MOD-IDARTNR-UP                       
155814     END-IF                                                               
155815                                                                          
156101     MOVE MID-ADINPORT     (INDX) TO MOD-ADINPORT-UP                      
156201     MOVE MID-TIAAVV-FOM   (INDX) TO MOD-TIAAVV-FOM-UP                    
156301     .                                                                    
156401     EJECT                                                                
156501 S03-CHECK-FOR-DELETE SECTION.                                            
156601     MOVE NOO TO DELETE-SW                                                
156701     MOVE +1 TO INDX                                                      
156801     PERFORM UNTIL INDX > MAX-INDX OR DELETE-YES                          
156901      IF MID-KDCMDVAL (INDX) = 'B' OR 'D'                                 
157001*       IF MID-IDARTNR (INDX) = 0 OR SPACE                                
157002        IF MID-TIAAVV-FOM (INDX) = 0 OR SPACE                             
157301          MOVE NOO TO INDATA-SW                                           
157401          MOVE MID-KDCMDVAL (INDX) TO MOD-KDCMDVAL (INDX)                 
157501          MOVE MFS-ALPHA-FIELD-WRONG                                      
157601                                   TO MOD-KDCMDVAL-ATTR (INDX)            
157701*         MOVE 'DELETE NOT POSS' TO MOD-TEMFSFEL                          
157801          MOVE ERR-UPDATE-NOT-POSS TO MED-IDMFSFEL                        
157901          CALL WMEDKONV USING MED-WMEDAREA                                
158001          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
158101          PERFORM MFS-DONT-TOUCH-FIELD-OUT                                
158201        ELSE                                                              
158301          MOVE YES TO DELETE-SW                                           
158401        END-IF                                                            
158501      END-IF                                                              
158601      ADD +1 TO INDX                                                      
158701     END-PERFORM                                                          
158801     .                                                                    
158901     EJECT                                                                
176801 MFS-ERASE-FIELD-OUT SECTION.                                             
176901     MOVE MFS-ERASE-FIELD TO MOD-BEFT-UT                                  
177001                             MOD-KDSORT1-UT                               
177101     .                                                                    
177201     SKIP3                                                                
177301 MFS-ERASE-FIELD-IN SECTION.                                              
177401     MOVE MFS-ERASE-FIELD TO MID-BEFT-IN                                  
177501                             MID-KDSORT1-IN                               
177507     .                                                                    
177702 MFS-RENSA-FAELT-UP  SECTION.                                             
177708     MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-UP                               
177709                             MOD-ADLAGOMR-UP                              
177710                             MOD-BEFT-UP                                  
177712                             MOD-ADINPORT-UP                              
177713                             MOD-TIAAVV-FOM-UP                            
177714     .                                                                    
177720     EJECT                                                                
177730 MFS-DONT-TOUCH-FAELT-UP  SECTION.                                        
177740     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR-UP                        
177750                                    MOD-ADLAGOMR-UP                       
177760                                    MOD-BEFT-UP                           
177770                                    MOD-ADINPORT-UP                       
177780                                    MOD-TIAAVV-FOM-UP                     
177790     .                                                                    
177800     EJECT                                                                
177804 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
177901     MOVE +1 TO INDX                                                      
178001     PERFORM UNTIL INDX > MAX-INDX                                        
178101       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
178201       ADD +1 TO INDX                                                     
178301     END-PERFORM                                                          
178401     .                                                                    
178501     SKIP2                                                                
178601 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
178701     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ADLAGOMR     (INDX)               
178702                                    MOD-BEFT         (INDX)               
178801                                    MOD-IDARTNR      (INDX)               
179101                                    MOD-ADINPORT     (INDX)               
179201                                    MOD-TIAAVV-FOM   (INDX)               
179301                                    MOD-IDUSER       (INDX)               
179401                                    MOD-TIUPPDAT     (INDX)               
179501     .                                                                    
179601     SKIP3                                                                
179701 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
179801     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEFT-IN                           
179901                                    MOD-KDSORT1-IN                        
180001     .                                                                    
180101     EJECT                                                                
180201* --- IMS SECTIONS ---                                                    
180301     SKIP3                                                                
180401 IMS-GET-MSG SECTION.                                                     
180501     MOVE '  QC' TO GOOD-STATUSCODES                                      
180601     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
180701     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
180801     PERFORM IMS-STATUSCHECK                                              
180901     .                                                                    
181001     SKIP3                                                                
181101 IMS-INSERT-MSG SECTION.                                                  
181201     IF MSGI-IDLAND-SPR = 'SE'                                            
181301       MOVE '0' TO MFS-KDHUVOMR                                           
181401     END-IF                                                               
181501     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
181601     MOVE SPACE TO GOOD-STATUSCODES                                       
181701     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
181801     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
181901     PERFORM IMS-STATUSCHECK                                              
182001     .                                                                    
182101     EJECT                                                                
182102 IMS-GU-WDT201 SECTION.                                                   
182103     STRING 'WDT201  (IDDC     =' W-IDDC-X ')'                            
182104          DELIMITED BY SIZE INTO SSA1                                     
182105     MOVE '    ' TO GOOD-STATUSCODES                                      
182106     CALL CBLTDLI USING GU WDT2-PCB DLI-IO-WDT201 SSA1                    
182107     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
182108     PERFORM IMS-STATUSCHECK                                              
182109     .                                                                    
182110                                                                          
182120                                                                          
182131 IMS-GNP-WDT211 SECTION.                                                  
182140     STRING 'WDT211  (WDT211KY>=' W-WDT211KY-MIN-X ')'                    
182160          DELIMITED BY SIZE INTO SSA1                                     
182170     MOVE '  GE' TO GOOD-STATUSCODES                                      
182180     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT211 SSA1                   
182190     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
182200     PERFORM IMS-STATUSCHECK                                              
182201     .                                                                    
182202 IMS-GHU-WDT211 SECTION.                                                  
182203     STRING 'WDT201  (IDDC     =' W-IDDC-X ')'                            
182204          DELIMITED BY SIZE INTO SSA1                                     
182205     STRING 'WDT211  (WDT211KY =' W-WDT211KY-X ')'                        
182206          DELIMITED BY SIZE INTO SSA2                                     
182207     MOVE '  GE' TO GOOD-STATUSCODES                                      
182208     CALL CBLTDLI USING GHU WDT2-PCB DLI-IO-WDT211 SSA1 SSA2              
182209     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
182210     PERFORM IMS-STATUSCHECK                                              
182211     .                                                                    
182212                                                                          
182213                                                                          
182214 IMS-ISRT-WDT211 SECTION.                                                 
182215     STRING 'WDT201  (IDDC     =' W-IDDC-X ')'                            
182216          DELIMITED BY SIZE INTO SSA1                                     
182217     MOVE 'WDT211 ' TO SSA2                                               
182218     MOVE '    ' TO GOOD-STATUSCODES                                      
182219     CALL CBLTDLI USING ISRT WDT2-PCB DLI-IO-WDT211 SSA1 SSA2             
182220     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
182221     PERFORM IMS-STATUSCHECK                                              
182222     .                                                                    
182223                                                                          
182224                                                                          
182225                                                                          
182226                                                                          
182227                                                                          
182228                                                                          
182229 IMS-GNP-WDT212 SECTION.                                                  
182230     STRING 'WDT212  (WDT212KY>=' W-WDT212KY-MIN-X ')'                    
182231          DELIMITED BY SIZE INTO SSA1                                     
182232     MOVE '  GE' TO GOOD-STATUSCODES                                      
182233     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT212 SSA1                   
182234     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
182235     PERFORM IMS-STATUSCHECK                                              
182236     .                                                                    
182237                                                                          
182238                                                                          
182239                                                                          
182240 IMS-GHU-WDT212 SECTION.                                                  
182241     STRING 'WDT201  (IDDC     =' W-IDDC-X ')'                            
182242          DELIMITED BY SIZE INTO SSA1                                     
182243     STRING 'WDT212  (WDT212KY =' W-WDT212KY-X ')'                        
182244          DELIMITED BY SIZE INTO SSA2                                     
182245     MOVE '  GE' TO GOOD-STATUSCODES                                      
182246     CALL CBLTDLI USING GHU WDT2-PCB DLI-IO-WDT212 SSA1 SSA2              
182247     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
182248     PERFORM IMS-STATUSCHECK                                              
182249     .                                                                    
182250                                                                          
182251                                                                          
182252                                                                          
182253 IMS-ISRT-WDT212 SECTION.                                                 
182254     STRING 'WDT201  (IDDC     =' W-IDDC-X ')'                            
182255          DELIMITED BY SIZE INTO SSA1                                     
182256     MOVE 'WDT212 ' TO SSA2                                               
182257     MOVE '    ' TO GOOD-STATUSCODES                                      
182258     CALL CBLTDLI USING ISRT WDT2-PCB DLI-IO-WDT212 SSA1 SSA2             
182259     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
182260     PERFORM IMS-STATUSCHECK                                              
182261     .                                                                    
182262                                                                          
182263                                                                          
182264                                                                          
182265 IMS-GNP-WDT213 SECTION.                                                  
182266     STRING 'WDT213  (WDT213KY>=' W-WDT213KY-MIN-X ')'                    
182267          DELIMITED BY SIZE INTO SSA1                                     
182268     MOVE '  GE' TO GOOD-STATUSCODES                                      
182269     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT213 SSA1                   
182270     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
182271     PERFORM IMS-STATUSCHECK                                              
182272     .                                                                    
182273                                                                          
182274                                                                          
182275                                                                          
182276 IMS-GHU-WDT213 SECTION.                                                  
182277     STRING 'WDT201  (IDDC     =' W-IDDC-X ')'                            
182278          DELIMITED BY SIZE INTO SSA1                                     
182279     STRING 'WDT213  (WDT213KY =' W-WDT213KY-X ')'                        
182280          DELIMITED BY SIZE INTO SSA2                                     
182281     MOVE '  GE' TO GOOD-STATUSCODES                                      
182282     CALL CBLTDLI USING GHU WDT2-PCB DLI-IO-WDT213 SSA1 SSA2              
182283     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
182284     PERFORM IMS-STATUSCHECK                                              
182285     .                                                                    
182286                                                                          
182287                                                                          
182288                                                                          
182289 IMS-ISRT-WDT213 SECTION.                                                 
182290     STRING 'WDT201  (IDDC     =' W-IDDC-X ')'                            
182291          DELIMITED BY SIZE INTO SSA1                                     
182292     MOVE 'WDT213 ' TO SSA2                                               
182293     MOVE '    ' TO GOOD-STATUSCODES                                      
182294     CALL CBLTDLI USING ISRT WDT2-PCB DLI-IO-WDT213 SSA1 SSA2             
182295     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
182296     PERFORM IMS-STATUSCHECK                                              
182297     .                                                                    
182300 IMS-GU-WDK601 SECTION.                                                   
182301     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
182401          DELIMITED BY SIZE INTO SSA1                                     
182501     MOVE '  GE' TO GOOD-STATUSCODES                                      
182601     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
182701     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
182801     PERFORM IMS-STATUSCHECK                                              
182901     .                                                                    
183001     EJECT                                                                
199101 IMS-DLET-WDT211 SECTION.                                                 
199201     MOVE '  ' TO GOOD-STATUSCODES                                        
199301     CALL CBLTDLI USING DLET WDT2-PCB DLI-IO-WDT211                       
199401     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
199501     PERFORM IMS-STATUSCHECK                                              
199601     .                                                                    
199701     EJECT                                                                
202701 IMS-DLET-WDT212 SECTION.                                                 
202801     MOVE '  ' TO GOOD-STATUSCODES                                        
202901     CALL CBLTDLI USING DLET WDT2-PCB DLI-IO-WDT212                       
203001     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
203101     PERFORM IMS-STATUSCHECK                                              
203201     .                                                                    
203202 IMS-DLET-WDT213 SECTION.                                                 
203203     MOVE '  ' TO GOOD-STATUSCODES                                        
203204     CALL CBLTDLI USING DLET WDT2-PCB DLI-IO-WDT213                       
203205     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
203206     PERFORM IMS-STATUSCHECK                                              
203207     .                                                                    
203301     EJECT                                                                
203401 IMS-STATUSCHECK SECTION.                                                 
203501     SET STATUS-IX TO 1                                                   
203601     SEARCH GOOD-STATUS                                                   
203701       AT END                                                             
203801         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
203901         DELIMITED BY SIZE INTO ERROR-TEXT                                
204001         CALL FELLOG                                                      
204101       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
204201         CONTINUE                                                         
204301     END-SEARCH                                                           
205000     .                                                                    
